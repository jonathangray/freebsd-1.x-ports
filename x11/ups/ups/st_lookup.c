/* st_lookup.c - look up things by name or address in the symbol tables */

/*  Copyright 1992 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_st_lookup_c_sccsid[] = "@(#)st_lookup.c	1.2 15/9/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <sys/types.h>
#include <string.h>
#include <a.out.h>		/* needed for st_priv.h */
#include <stdlib.h>

#include <local/ukcprog.h>
#include <local/arg.h>
#include <mtrprog/strcache.h>
#include <mtrprog/alloc.h>
#include <mtrprog/genmergesort.h>

#include "ups.h"
#include "symtab.h"
#include "ci.h"
#ifdef ARCH_MIPS
#include "mips_frame.h"
#endif
#include "st_priv.h"

/*  List of globals added to by find_matching_funcs() and
 *  find_matching_globals().
 */
typedef struct global_list_s {
	const char *gl_name;
	fil_t *gl_fil;
	func_t *gl_func;
	var_t *gl_var;
	taddr_t gl_addr;
	struct global_list_s *gl_next;
} global_list_t;

static bool filename_matches PROTO((fil_t *fil, fil_t *matchfil,
					const char *name, int namelen));
static void find_matching_funcs PROTO((func_t *funclist,
				       fil_t *matchfil, const char *filename,
				       int flen, const char *funcname,
				       global_list_t **p_glhead,
				       bool *p_found_file));
static void find_matching_globals PROTO((symtab_t *st, fil_t *matchfil, const char *filename, int filename_len, const char *pat, global_list_t **p_glhead));
static void scan_symtabs PROTO((fil_t *fil, const char *filename, int filename_len, const char *funcpat, bool want_vars, global_list_t **p_glhead, bool *p_found_file));
static void add_var_to_global_list PROTO((global_list_t **p_glhead,
					  var_t *v, fil_t *fil));
static int glcmp PROTO((global_list_t *f1, global_list_t *f2));
static void add_common_block_globals PROTO((const char *name, func_t *f,
					    common_block_id_t *p_cblock,
					    global_list_t **p_glhead));
static void dupf_mesg PROTO((const char *name,
			     global_list_t *gl1, global_list_t *gl2));

GENERIC_MERGE_SORT(static,sort_global_list,global_list_t,gl_next)

ALLOC_NEW_FREELIST(static,global_list_t,global_list,gl_next)

static bool
filename_matches(fil, matchfil, name, namelen)
fil_t *fil, *matchfil;
const char *name;
int namelen;
{
	int len;

	if (matchfil != NULL)
		return fil == matchfil;

	if (name == NULL)
		return TRUE;
	if (fil == NULL)
		return FALSE;
	if (namelen == 0)
		return TRUE;

	len = strlen(fil->fi_name);

	if (len < namelen ||
	    memcmp(&fil->fi_name[len - namelen], name, namelen) != 0)
		return FALSE;
	
	return len == namelen || fil->fi_name[len - namelen - 1] == '/';
}

static void
find_matching_funcs(funclist, matchfil, filename, flen, funcpat,
							p_glhead, p_found_file)
func_t *funclist;
fil_t *matchfil;
const char *filename;
int flen;
const char *funcpat;
global_list_t **p_glhead;
bool *p_found_file;
{
	func_t *f;

	for (f = funclist; f != NULL; f = f->fu_next) {

		if (!filename_matches(f->fu_fil, matchfil, filename, flen))
			continue;

		*p_found_file = TRUE;

		if (arg_match(f->fu_name, funcpat)) {
			global_list_t *gl;

			gl = new_global_list();
			gl->gl_name = f->fu_name;
			gl->gl_fil = f->fu_fil;
			gl->gl_func = f;
			gl->gl_var = NULL;
			gl->gl_addr = f->fu_addr;
			gl->gl_next = *p_glhead;
			*p_glhead = gl;
		}
	}
}

fil_t *
name_to_fil(name)
const char *name;
{
	symtab_t *st;

	for (st = get_main_st(); st != NULL; st = st->st_next) {
		fil_t *fil;

		for (fil = st->st_sfiles; fil != NULL; fil = fil->fi_next) {
			if (strcmp(fil->fi_name, name) == 0)
				return fil;
		}
	}

	return NULL;
}

func_t *
name_and_fil_to_func(name, fil)
const char *name;
fil_t *fil;
{
	funclist_t *fl;

	for (fl = fil->fi_funclist_head; fl != NULL; fl = fl->fl_next) {
		if (strcmp(fl->fl_func->fu_name, name) == 0)
			return fl->fl_func;
	}

	return NULL;
}

int
resolve_untyped_name(name, p_v)
const char *name;
var_t **p_v;
{
	global_list_t *glhead;

	glhead = NULL;
	find_matching_globals(get_main_st(), (fil_t *)NULL,
			      (const char *)NULL, 0, name, &glhead);
	
	if (glhead != NULL && glhead->gl_next == NULL)
		*p_v = glhead->gl_var;
	else
		*p_v = NULL;
	
	free_global_list_list(glhead);

	return (*p_v != NULL) ? 0 : -1;
}

static void
find_matching_globals(st, matchfil, filename, filename_len, pat, p_glhead)
symtab_t *st;
fil_t *matchfil;
const char *filename;
int filename_len;
const char *pat;
global_list_t **p_glhead;
{
	fil_t *fil;
	addrlist_t *al;

	for (fil = st->st_sfiles; fil != NULL; fil = fil->fi_next) {
#ifndef ST_TE
		snlist_t *sn;
#endif
		var_t *v;

		if (!filename_matches(fil, matchfil, filename, filename_len))
			continue;

#ifndef ST_TE
		sn = ((stf_t *)fil->fi_stf)->stf_sn;
		for (; sn != NULL; sn = sn->sn_next) {
			if (arg_match(sn->sn_name, pat))
				break;
		}
		if (sn == NULL)
			continue;
#endif

		for (v = FI_VARS(fil); v != NULL; v = v->va_next) {
			if (arg_match(v->va_name, pat))
				add_var_to_global_list(p_glhead, v, fil);
		}
	}

	 if (matchfil != NULL || filename != NULL)
		return;
	 
	 for (st = get_main_st(); st != NULL; st = st->st_next) {
		for (al = st->st_addrlist; al != NULL; al = al->al_next) {
			if (!arg_match(al->al_name, pat))
				continue;

			if (al->al_var == NULL) {
				var_t *v;

				v = ci_make_var(st->st_alloc_id,
						al->al_name, CL_EXT,
						ci_code_to_type(TY_INT_ASSUMED),
						al->al_addr);
				v->va_language = LANG_UNKNOWN;
				al->al_var = v;
			}

			add_var_to_global_list(p_glhead, al->al_var,
								(fil_t *)NULL);
		}
	}
}

static void
add_var_to_global_list(p_glhead, v, fil)
global_list_t **p_glhead;
var_t *v;
fil_t *fil;
{
	global_list_t *gl;

	gl = new_global_list();
	gl->gl_name = v->va_name;
	gl->gl_fil = fil;
	gl->gl_var = v;
	gl->gl_func = NULL;
	gl->gl_addr = v->va_addr;
	gl->gl_next = *p_glhead;
	*p_glhead = gl;
}

/*  Search for the common block where the variable called name
 *  is defined.  We loop over all the symbol tables, searching
 *  the common block lists of each.  (At present there are no
 *  shared library FORTRAN executables, so the only symbol
 *  table with a common block list will be the main one).
 */
static void
add_common_block_globals(name, f, p_cblock, p_glhead)
const char *name;
func_t *f;
common_block_id_t *p_cblock;
global_list_t **p_glhead;
{
	symtab_t *st;

	for (st = get_main_st(); st != NULL; st = st->st_next) {
		var_t *v;
		fil_t *fil;

		global_and_cblist_to_var(st->st_cblist_id, name, f,
							 p_cblock, &fil, &v);
		if (v != NULL) {
			add_var_to_global_list(p_glhead, v, fil);
			return;
		}
	}
}

static int
glcmp(gl1, gl2)
global_list_t *gl1, *gl2;
{
	int res;

	if ((res = strcmp(gl1->gl_name,  gl2->gl_name)) != 0)
		return res;
	
	if ((gl1->gl_func != NULL) != (gl2->gl_func != NULL))
		return (gl1->gl_func == NULL) ? -1 : 1;
	
	if (gl1->gl_func == NULL)
		return 0;

	return addrcmp(gl1->gl_func, gl2->gl_func);
}

static void
scan_symtabs(matchfil, filename, filename_len, funcpat, want_vars,
							p_glhead, p_found_file)
fil_t *matchfil;
const char *filename;
int filename_len;
const char *funcpat;
bool want_vars;
global_list_t **p_glhead;
bool *p_found_file;
{
	symtab_t *st;

	for (st = get_main_st(); st != NULL; st = st->st_next) {
		find_matching_funcs(st->st_funclist,
				    matchfil, filename, filename_len,
			 	    funcpat, p_glhead, p_found_file);
	}

	for (st = get_symtab_cache_list(); st != NULL; st = st->st_next) {
		find_matching_funcs(st->st_funclist,
				    matchfil, filename, filename_len,
			 	    funcpat, p_glhead, p_found_file);
	}

	if (want_vars) {
		find_matching_globals(get_main_st(), matchfil, filename,
				      filename_len, funcpat, p_glhead);
	}
}
					
int
find_func_by_name(name, p_f)
const char *name;
func_t **p_f;
{
	return find_global_by_name(name, (fil_t *)NULL, (func_t *)NULL, FALSE,
				   p_f, (var_t **)NULL,
				   (common_block_id_t *)NULL, (fil_t **)NULL);
				
}

/*  We search all the symbol tables including those in the cache.
 *  This is so we can match names in shared libraries (like printf)
 *  before the target has been started.
 */
int
find_global_by_name(name, fil, f, exact, p_f, p_v, p_cblock, p_fil)
const char *name;
fil_t *fil;
func_t *f;
bool exact;
func_t **p_f;
var_t **p_v;
common_block_id_t *p_cblock;
fil_t **p_fil;
{
	global_list_t *gl, *glhead, *prev;
	global_list_t *exact_gls[2], *partial_gls[2], *exact_extern_gl;
	int matches, exact_matches, exact_extern_matches, partial_matches;
	int filename_len;
	const char *filename, *funcname, *funcpat;
	char *infile;
	bool found_file;

	if ((funcname = strchr(name, ':')) != NULL && funcname[1] != ':') {
		if (funcname[1] == '\0') {
			errf("Missing function name after source file");
			return -1;
		}
		
		filename = name;
		filename_len = funcname - name; 
		++funcname;
	}
	else {
		filename = NULL;
		filename_len = 0;	/* to satisfy gcc */
		funcname = name;
	}

	if (exact) {
		funcpat = funcname;
	}
	else {
		static const char globchars[] = "*?[]";
		const char *s, *globname;
		char *namecopy;

		for (s = funcname; s != '\0'; ++s) {
			if (strchr(globchars, *s) != NULL)
				break;
		}
		
		if (*s == '\0') {
			namecopy = strf("%s*", funcname);
			globname = namecopy;
		}
		else {
			namecopy = NULL;
			globname = funcname;
		}

		funcpat = arg_do_quoting(&globname, globchars);

		if (namecopy != NULL)
			free(namecopy);

		if (funcpat == NULL)
			return -1;
	}

	glhead = NULL;
	found_file = FALSE;

	if (p_v != NULL)
		*p_cblock = NULL;

	if (fil != NULL && filename == NULL)
		scan_symtabs(fil, (const char *)NULL, 0, funcpat,
					p_v != NULL, &glhead, &found_file);

	if (glhead == NULL) {
		scan_symtabs((fil_t *)NULL, filename, filename_len, funcpat,
					p_v != NULL, &glhead, &found_file);

		if (p_v != NULL)
			add_common_block_globals(name, f, p_cblock, &glhead);
	}

	if (!found_file) {
		errf("No source file name `%.*s'", filename_len, filename);
		return -1;
	}

	if (glhead == NULL) {
		const char *orvar;

		orvar = (p_v != NULL) ? " or variable" : "";

		if (filename != NULL)
			errf("No function%s matching `%s' in %.*s",
				     orvar, funcname, filename_len, filename);
		else
			errf("No function%s `%s'", orvar, funcname);

		return -1;
	}

	matches = exact_matches = exact_extern_matches = partial_matches = 0;
	exact_extern_gl = NULL;		/* to satisfy gcc */

	for (gl = glhead; gl != NULL; gl = gl->gl_next)
		++matches;
	glhead = sort_global_list(glhead, matches, glcmp);

	prev = NULL;

	for (gl = glhead; gl != NULL; gl = gl->gl_next) {

		/*  We can get duplicate symbol table entries for a
		 *  function or variable - ignore these.
		 */
		if (prev != NULL && gl->gl_addr == prev->gl_addr &&
		    strcmp(prev->gl_name, gl->gl_name) == 0 &&
		    (prev->gl_func != NULL) == (gl->gl_func != NULL))
			continue;
		
		if (strcmp(gl->gl_name, funcname) == 0) {
			if ((gl->gl_func != NULL) ?
				      (gl->gl_func->fu_flags & FU_STATIC) == 0 :
				      gl->gl_var->va_class == CL_EXT) {
				++exact_extern_matches;
				exact_extern_gl = gl;
			}

			if (exact_matches < 2)
				exact_gls[exact_matches] = gl;
			++exact_matches;
		}
		else {
			if (partial_matches < 2)
				partial_gls[partial_matches] = gl;
			++partial_matches;
		}

		prev = gl;
	}
	free_global_list_list(glhead);

	if (exact_matches == 1)
		gl = exact_gls[0];
	else if (partial_matches == 1 && exact_matches == 0)
		gl = partial_gls[0];
	else if (exact_extern_matches == 1)
		gl = exact_extern_gl;
	else
		gl = NULL;

	if (gl != NULL) {
		*p_f = gl->gl_func;
		if (p_v != NULL) {
			*p_v = gl->gl_var;
			*p_fil = gl->gl_fil;
		}
		return 0;
	}

	if (exact_matches > 1) {
		dupf_mesg(funcname, exact_gls[0], exact_gls[1]);
		return -1;
	}

	if (partial_matches <= 1)
		panic("pmatch botch in ffbn");

	if (strcmp(partial_gls[0]->gl_name, partial_gls[1]->gl_name) == 0) {
		dupf_mesg(partial_gls[0]->gl_name,
				partial_gls[0], partial_gls[1]);
		return -1;
	}

	infile = (filename != NULL && *filename != '\0')
				? strf(" in %.*s", filename_len, filename)
				: strsave("");

	if (partial_matches == 2) {
		errf("`%s' matches both `%s' and `%s'%s",
				funcname, partial_gls[0]->gl_name,
				partial_gls[1]->gl_name, infile);
	}
	else {
		errf("`%s' matches `%s', `%s' (and %d more)%s",
				funcname, partial_gls[0]->gl_name,
				partial_gls[1]->gl_name, 
				partial_matches - 2, infile);
	}
	
	free(infile);
	
	return -1;
}

static void
dupf_mesg(funcname, gl1, gl2)
const char *funcname;
global_list_t *gl1, *gl2;
{
	const char *what;
	fil_t *fil1, *fil2;

	if ((gl1->gl_func != NULL) != (gl2->gl_func != NULL))
		what = "Name";
	else
		what = (gl1->gl_func != NULL) ? "Function" : "Variable";

	fil1 = gl1->gl_fil;
	fil2 = gl2->gl_fil;

	if (fil1 != NULL && fil2 != NULL)
		errf("%s `%s' appears in both %s and %s",
				what, funcname, fil1->fi_name, fil2->fi_name);
	else if (fil1 != NULL && fil2 == NULL)
		errf("%s `%s' appears in both %s and elsewhere",
				what, funcname, fil1->fi_name);
	else if (fil2 != NULL && fil1 == NULL)
		errf("%s `%s' appears in both %s and elsewhere",
				what, funcname, fil2->fi_name);
	else
		errf("%s `%s' appears more than once!", what, funcname);
}

/*  Return a pointer to the func_t block of the function wherein
 *  lies address addr, or NULL if addr does not lie within any
 *  known function.  We search all the symbol tables.
 */
func_t *
addr_to_func(addr)
taddr_t addr;
{
	symtab_t *st;
	func_t *f;

	for (st = get_main_st(); st != NULL; st = st->st_next) {
		f = addr_and_functab_to_func(st->st_functab_id, addr);
		if (f != NULL)
			return f;
	}

	return NULL;
}

/*  Call (*func)() for each source file with the source file as an argument.
 *
 *  Do the source files in the cache as well.
 */
void
iterate_over_source_files(func)
void (*func)PROTO((fil_t *fil));
{
	symtab_t *st;
	fil_t *fil;

	for (st = get_main_st(); st != NULL; st = st->st_next)
		for (fil = st->st_sfiles; fil != NULL; fil = fil->fi_next)
			if ((((stf_t *)fil->fi_stf)->stf_flags & STF_HIDE) == 0)
				(*func)(fil);
	for (st = get_symtab_cache_list(); st != NULL; st = st->st_next)
		for (fil = st->st_sfiles; fil != NULL; fil = fil->fi_next)
			if ((((stf_t *)fil->fi_stf)->stf_flags & STF_HIDE) == 0)
				(*func)(fil);
}

/*  Call (*func)(cblock) for each common block.
 */
void
iterate_over_common_blocks(func)
void (*func)PROTO((common_block_id_t cblock));
{
	symtab_t *st;

	for (st = get_main_st(); st != NULL; st = st->st_next)
		iterate_over_cblist(st->st_cblist_id, func);
}
