/* st_skim.c - do the initial pass over the symbol table */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_st_skim_c_sccsid[] = "@(#)st_skim.c	1.39 15/9/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <a.out.h>
#ifndef OS_RISCOS
#include <stab.h>
#endif
#include <errno.h>
extern int errno;

#include <local/ukcprog.h>
#include <mtrprog/strcache.h>

#include "ups.h"
#include "symtab.h"
#ifdef ARCH_MIPS
#include "mips_frame.h"
#endif
#include "st_priv.h"
#include "text.h"
#include "ci.h"		/* for ci_make_block prototype */

/*  Function prototypes.
 */
#ifndef ST_TE
static int backup_lnum_to_curly PROTO((func_t *f, int orig_lnum));
static void get_lno_symrange PROTO((func_t *f, int *p_start, int *p_lim));
static int get_def PROTO((stf_t *stf, int tnum, int min, int max,
						int *p_symno, const char **p_s));
static type_t *get_type PROTO((stf_t *stf, int tnum, int min, int max));
static void build_symset PROTO((char *symset));
static language_t srctype PROTO((const char *name));
static void wrapup_stf PROTO((stf_t *stf, hf_t **fmap, int mapsize));
static void get_fi_types PROTO((fil_t *fil, bool is_header));
static aggr_or_enum_def_t *match_tag PROTO((aggr_or_enum_def_t *ae,
								const char *tag));
static aggr_or_enum_def_t *fix_undef_aggr PROTO((aggr_or_enum_def_t *uae,
								const char *arg));
#endif

#ifdef OS_SUNOS
static hf_t *lookup_hf PROTO((hf_t *headers, int id));
static stf_t *id_to_hdrstf PROTO((stf_t *stf, taddr_t id, int symno));
#endif

static bool Use_srcpath_only = FALSE;

void
set_use_srcpath_only_flag()
{
	Use_srcpath_only = TRUE;
}

/*  Open the source file for fil if not already open.
 *
 *  Return 0 and set fil->fi_so if successful, otherwise return -1 and
 *  leave fi_so alone 
 */
int
open_source_file(fil, want_error_messages)
fil_t *fil;
bool want_error_messages;
{
	const char *name, *path_hint;
	so_id_t so;

	so = fil->fi_so;
	if (so != 0)
		return 0;

	name = fil->fi_name;
	path_hint = fil->fi_path_hint;

	if (*name != '/' && path_hint != NULL && !Use_srcpath_only) {
		char *fullname;

		fullname = strf("%s/%s", path_hint, name);
		so = so_open_file(fullname, (so_line_callback_t)NULL);
		if (so == 0 && errno != ENOENT) {
			if (want_error_messages)
				errf("Can't open source file %s (%m)",
								     fullname);
			free(fullname);
			return -1;
		}
		free(fullname);
	}

	if (so == 0) {
		so = so_open_file(name, (so_line_callback_t)NULL);

		/*  If the file doesn't exist try knocking any directory
		 *  components off it.
		 */
		if (so == 0 && errno == ENOENT) {
			const char *basename;

			basename = strrchr(name, '/');
			if (basename != NULL && basename[1] != '\0') {
				so = so_open_file(basename + 1,
						      (so_line_callback_t)NULL);

				/*  We want the error for the original
				 *  file if we don't find the basename.
				 */
				if (so == 0)
					errno = ENOENT;
			}
		}
				
		if (so == 0 && want_error_messages)
			errf("Can't open source file %s (%m)", name);
	}

	fil->fi_so = so;

	return (so != 0) ? 0 : -1;
}

#ifndef ST_TE
/*  Deduce language of source file from name.
 *  *.c is C, *.f is FORTRAN, anything else is unknown.
 *
 *  BUG: dubious way of getting language.
 *
 *  NOTE: this routine is not used on the DECstation 3100.
 */
static language_t
srctype(name)
const char *name;
{
	char *suf;

	if ((suf = strrchr(name, '.')) == NULL)
		return LANG_UNKNOWN;
	else
		++suf;
	if (strcmp(suf, "c") == 0)
		return LANG_C;
	if (strcmp(suf, "f") == 0)
		return LANG_FORTRAN;
	return LANG_UNKNOWN;
}

/*  Called when we have finished with an stf structure.  Create space
 *  for the map and copy the stuff to it.
 *
 *  NOTE: this routine is not used on the DECstation 3100.
 */
static void
wrapup_stf(stf, fmap, mapsize)
stf_t *stf;
hf_t **fmap;
int mapsize;
{
	funclist_t *fl, *nextfl;

	if (mapsize == 0)
		panic("mapsize 0 in wrapup_stf");
	stf->stf_mapsize = mapsize;
	stf->stf_fmap = (hf_t **) alloc(stf->stf_symtab->st_alloc_id,
					       mapsize * sizeof(hf_t *));
	memcpy((char *)stf->stf_fmap, (char *)fmap, mapsize * sizeof(hf_t *));

	/*  Build the forwards links in the function list.
	 *  See the comment on the flist_t structure in symtab.h.
	 */
	nextfl = NULL;
	for (fl = stf->stf_fil->fi_funclist_tail; fl != NULL; fl = fl->fl_prev) {
		fl->fl_next = nextfl;
		nextfl = fl;
	}
	stf->stf_fil->fi_funclist_head = nextfl;
}

#ifdef OS_SUNOS
/*  Look up the header file entry with id id in the headers list headers.
 *
 *  Used when we find a N_EXCL symbol meaning a header file excluded
 *  because it has already been encountered.
 */
static hf_t *
lookup_hf(headers, id)
hf_t *headers;
int id;
{
	hf_t *hf;

	for (hf = headers; hf != NULL; hf = hf->hf_next)
		if (hf->hf_id == id)
			return hf;
	panic("id not found in lookup_hf");
	/* NOTREACHED */
	return NULL;	/* to keep gcc happy */

}
#endif /* OS_SUNOS */

/*  Return a pointer to a munged saved copy of fname.  Ext is true
 *  if name is an external symbol generated by the linker.  These
 *  symbols are expected to have an underscore prepended - if it
 *  is there it is stripped, otherwise the returned name is
 *  enclosed in brackets (e.g. "[name]").
 *
 *  We use alloc() to get the space.
 *
 *  NOTE: this routine is not used on the DECstation 3100.
 */
const char *
save_fname(st, name, ext)
symtab_t *st;
const char *name;
int ext;
{
	register const char *cptr;
	register char *ptr;
	int len;

#ifdef COFF_SUN386
	/*  On the 386i, external symbols don't seem to
	 *  have underscores prepended, so we force ext
	 *  to false.
	 */
	ext = FALSE;
#endif
	if (ext && *name != '_') {
		ptr = alloc(st->st_alloc_id, strlen(name) + 3);
		(void) sprintf(ptr, "[%s]", name);
	}
	else {
		if (ext)
			name++;
		for (cptr = name; *cptr != '\0' && *cptr != ':'; cptr++)
			;
		len = cptr - name;
		ptr = alloc(st->st_alloc_id, len + 1);
		memcpy(ptr, name, len);
		ptr[len] = '\0';
	}
	return ptr;
}
#endif /* !ST_TE */

fsyminfo_t *
make_fsyminfo(alloc_id, symno)
alloc_id_t alloc_id;
int symno;
{
	fsyminfo_t *fs;

	fs = (fsyminfo_t *)alloc(alloc_id, sizeof(fsyminfo_t));
	fs->fs_symno = symno;
	fs->fs_cblist = 0;
#if defined(ARCH_SUN386) && defined(OS_SUNOS)
	fs->fs_coff_lno_start = 0;
	fs->fs_coff_lno_lim = 0;
#endif
	return fs;
}

/*  Allocate a new stf_t structure with alloc() and fill in the fields.
 */
stf_t *
make_stf(name, st, symno, language, addr)
const char *name;
symtab_t *st;
int symno;
language_t language;
taddr_t addr;
{
	register stf_t *stf;

	stf = (stf_t *) alloc(st->st_alloc_id, sizeof(stf_t));
	stf->stf_symtab = st;
	stf->stf_symno = symno;
	stf->stf_name = name;
	stf->stf_language = language;
	stf->stf_addr = addr;
	stf->stf_flags = 0;
#ifdef ARCH_CLIPPER
	stf->stf_addrlist = NULL;
#endif
#ifndef ST_TE
	stf->stf_sn = NULL;
	stf->stf_types = NULL;
#endif
	return stf;
}

/*  Allocate a new fil_t structure with alloc() and fill in the fields.
 */
fil_t *
make_fil(stf, parblock, path_hint, next)
stf_t *stf;
block_t *parblock;
const char *path_hint;
fil_t *next;
{
	fil_t *fil;

	fil = (fil_t *) alloc(stf->stf_symtab->st_alloc_id, sizeof(fil_t));
	fil->fi_name = stf->stf_name;
	fil->fi_path_hint = path_hint;
	fil->fi_language = stf->stf_language;
	fil->fi_flags = 0;
	fil->fi_stf = (long) stf;
	fil->fi_so = 0;
	fil->fi_editblocks_id = 0;
	fil->fi_block = ci_make_block(stf->stf_symtab->st_alloc_id, parblock);
	fil->fi_funclist_head = NULL;
	fil->fi_funclist_tail = NULL;
	fil->fi_next = next;
	return fil;
}

void
push_typedefs_and_aggrs(alloc_id, hdrbl, bl)
alloc_id_t alloc_id;
block_t *hdrbl, *bl;
{
	if (hdrbl->bl_aggr_or_enum_defs != NULL) {
		aggr_or_enum_def_t *ae;

		ae = (aggr_or_enum_def_t *)alloc(alloc_id,
						 sizeof(aggr_or_enum_def_t));
		ae->ae_type = NULL;
		ae->ae_sublist = hdrbl->bl_aggr_or_enum_defs;
		ae->ae_next = bl->bl_aggr_or_enum_defs;
		bl->bl_aggr_or_enum_defs = ae;
	}

	if (hdrbl->bl_typedefs != NULL) {
		typedef_t *td;

		td = (typedef_t *)alloc(alloc_id, sizeof(typedef_t));
		td->td_type = NULL;
		td->td_sublist = hdrbl->bl_typedefs;
		td->td_next = bl->bl_typedefs;
		bl->bl_typedefs = td;
	}
}

#ifndef ST_TE
#define SYMSET_SIZE	256

/*  Build the map of interesting symbols for skim_symtab.  We could do this
 *  at compile time with some effort, but it's much less hassle to do it at
 *  run time like this.
 *
 *  NOTE: this routine is not used on the DECstation 3100.
 */
static void
build_symset(symset)
char *symset;
{
	static int wanted_syms[] = {
		N_SO, N_SOL, N_BCOMM, N_STSYM, N_GSYM, N_LCSYM, N_FUN,
		N_TEXT, N_TEXT | N_EXT, N_BSS | N_EXT, N_DATA | N_EXT,
#if defined(OS_ULTRIX) || defined(ARCH_CLIPPER)
		N_DATA,
#endif
#ifdef OS_SUNOS
		N_BINCL, N_EXCL, N_EINCL,
#endif
	};
	int i;

	memset(symset, '\0', SYMSET_SIZE);
	for (i = 0; i < sizeof(wanted_syms) / sizeof(wanted_syms[0]); ++i)
		symset[wanted_syms[i]] = TRUE;
}

/*  BUG: this is gross, and wrong to boot.  The number of basic types
 *       varies between f77 compilers.  See the comment about this
 *       in get_fi_vars().
 */
#define N_FIXED_FORTRAN_TYPES	9

/*  Do a prescan of a symbol table.  We don't load in all of the symbol
 *  table on startup as that costs a lot in startup time for big symbol
 *  tables.  Instead we load enough to get us going (basically function
 *  and global variable names and addresses) and pull other bits in as
 *  needed.  This is a big win in normal use because the average debugger
 *  run touches only a small number of functions and global variables.
 *  Most of the symbol table is never read in at all.
 *
 *  The Sun C compiler has a system for reducing symbol table size by
 *  only including header file symbol table information once.  We have
 *  to do a fair amount of bookkeeping to resolve the header file
 *  references.
 *
 *  For most things that we load in this pass (like functions, global
 *  variables etc) we record the symbol number range of the object.
 *  This is so we can pull in more information when needed (e.g. the
 *  local variables of a function, the globals of a source file).
 *
 *  NOTE: this routine is not used on the DECstation 3100.
 */
void
skim_symtab(st, first_addr, last_addr, rootblock,
				p_have_common_blocks,
				p_sfiles,
				p_functab_id,
				p_cblist_id)
symtab_t *st;
taddr_t first_addr, last_addr;
block_t *rootblock;
int *p_have_common_blocks;
fil_t **p_sfiles;
functab_id_t *p_functab_id;
cblist_id_t *p_cblist_id;
{
	static int first_call = TRUE;
	static char symset[SYMSET_SIZE];
	nlist_t nm;
	snlist_t *sn;
#ifdef OS_SUNOS
	hf_t *headers, *hf;
#endif
	hf_t **fmap, **istack;
	stf_t *stf;
	fil_t *sfiles;
	func_t *f, *flist;
	funclist_t *fl;
	const char *name, *path_hint, *cptr;
	int flist_len, symno;
	int isp, istack_size, mapsize, max_mapsize, nsyms;
	cblist_id_t cblist_id;
	functab_id_t functab_id;
	alloc_id_t alloc_id;
	int seen_func;
#ifdef OS_ULTRIX
	int last_data_symno, have_unused_datasym;
	nlist_t data_nm;
	const char *lastname;
#endif

	if (first_call) {
		build_symset(symset);
		first_call = FALSE;
	}

	stf = NULL;
	flist = NULL;
	f = NULL;
	flist_len = 0;
	sfiles = NULL;
	cblist_id = create_empty_cblist();
#ifdef OS_SUNOS
	headers = NULL;
#endif
#ifdef OS_ULTRIX
	/*  FRAGILE CODE
	 *
	 *  The Ultrix C compiler has a charming habit of correcting
	 *  itself over symbol addresses.  A symbol table frequently
	 *  has an N_DATA symbol with one address, followed soon
	 *  after by an N_STSYM for the same symbol name with a slightly
	 *  different address.  The N_DATA address is the correct one.
	 *
	 *  To cope with this, we remember the symbol number of N_DATA
	 *  symbols, and correct the N_STSYM address if necessary.
	 *
	 *  The compiler also tends to put out N_DATA symbols immediately
	 *  after N_STSYM symbols, but in these cases the addresses
	 *  seem to always be the same.
	 */
	have_unused_datasym = FALSE;
	last_data_symno = 0; /* to satisfy gcc */
#endif

	mapsize = 0; /* for lint */
	max_mapsize = 32;
	fmap = (hf_t **) e_malloc(max_mapsize * sizeof(hf_t *));

	isp = 0;
	istack_size = 8;
	istack = (hf_t **) e_malloc(istack_size * sizeof(hf_t *));

	symno = -1;
	nsyms = get_symio_nsyms(st->st_symio_id);
	path_hint = NULL;
	seen_func = 0; /* to satisfy gcc */
	alloc_id = st->st_alloc_id;
	while ((symno = findsym(st->st_symio_id, symno+1, &nm, symset)) != nsyms) {
		switch(nm.n_type) {
		case N_SO:
#ifdef COFF_SUN386
			/*  The Sun 386i C compiler seems to put out
			 *  spurious N_SO symbols for functions.
			 *  The bad ones all seem to have a non zero
			 *  n_dbx_desc field, so skip these.
			 */
			if (nm.n_dbx_desc != 0)
				break;
#endif /* COFF_SUN386 */
			if (f != NULL) {
				FU_SYMLIM(f) = symno;
				f = NULL;
			}
			if (stf != NULL)
				stf->stf_symlim = symno;

			name = symstring(st->st_symio_id, symno);

			/*  4.0 cc puts paths ending in '/' just before
			 *  the source files that follow.
			 */
			if (name[strlen(name) - 1] == '/') {
				path_hint = name;
				break;
			}

			if (stf != NULL)
				wrapup_stf(stf, fmap, mapsize);

			stf = make_stf(name, st, symno,
						    srctype(name), nm.n_value);
			sfiles = make_fil(stf, rootblock, path_hint, sfiles);
			stf->stf_fil = sfiles;

			if (isp != 0)
				panic("unmatched N_BINCL");

			stf->stf_fnum = 0;
			fmap[0] = (hf_t *) alloc(alloc_id, sizeof(hf_t));
			fmap[0]->hf_stf = stf;
			fmap[0]->hf_id = -1;
			fmap[0]->hf_next = NULL;
			mapsize = 1;
			path_hint = NULL;
			seen_func = FALSE;
			symset[N_SLINE] = TRUE;
#ifdef OS_ULTRIX
			have_unused_datasym = FALSE;
#endif
			break;
		case N_SLINE:
			if (!seen_func && stf != NULL)
				stf->stf_flags |= STF_LNOS_PRECEDE_FUNCS;
			symset[N_SLINE] = FALSE;
			break;
		case N_SOL:
			/*  Basic support for #line construct.  Really just
			 *  supports yacc.
			 */
			if (stf != NULL) {
				name = symstring(st->st_symio_id, symno);
				stf->stf_name = name;

				/*  We don't change the filename for .h files.
				 *  DJ Delorie <dj@ctron.com> says this makes
				 *  ups work better with cfront (c++).
				 */
				if (name != NULL &&
				    strcmp(name + strlen(name) - 2, ".h") != 0)
					sfiles->fi_name = stf->stf_name;
			}
			break;

#ifdef OS_SUNOS
		case N_BINCL:
		case N_EXCL:
			if (sfiles == NULL)
				panic("header outside source file in st_skim");
			if (mapsize == max_mapsize) {
				max_mapsize *= 2;
				fmap = (hf_t **) e_realloc((char *)fmap,
						max_mapsize * sizeof(hf_t *));
			}
			if (nm.n_type == N_EXCL) {
				hf = lookup_hf(headers, (int)nm.n_value);
				fmap[mapsize++] = hf;
				break;
			}
			if (isp == istack_size) {
				istack_size *= 2;
				istack = (hf_t **) e_realloc((char *)istack,
						istack_size * sizeof(hf_t *));
			}
			istack[isp] = (hf_t *) alloc(alloc_id, sizeof(hf_t));
			istack[isp]->hf_next = headers;
			headers = istack[isp++];
			headers->hf_stf = make_stf(symstring(st->st_symio_id, symno),
						   st, symno, sfiles->fi_language,
						   nm.n_value);
			if (nm.n_type == N_EXCL)
				headers->hf_stf->stf_fil = NULL;
			else {
				headers->hf_stf->stf_fil =
						make_fil(headers->hf_stf,
							 (block_t *)NULL,
							 (const char *)NULL,
							 (fil_t *)NULL);
			}

			headers->hf_stf->stf_fnum = mapsize;
			headers->hf_id = nm.n_value;
			fmap[mapsize++] = headers;
			break;
		case N_EINCL:
			if (isp == 0)
				panic("unmatched N_EINCL");
			wrapup_stf(istack[--isp]->hf_stf, fmap, mapsize);
			istack[isp]->hf_stf->stf_symlim = symno;
			break;
#endif /* OS_SUNOS */
		case N_STSYM:
		case N_GSYM:
		case N_LCSYM:
			if (stf->stf_language == LANG_FORTRAN &&
				     symno - stf->stf_symno <= N_FIXED_FORTRAN_TYPES)
				break;
			name = symstring(st->st_symio_id, symno);
			sn = (snlist_t *)alloc(st->st_alloc_id, sizeof(snlist_t));
			sn->sn_symno = symno;
			sn->sn_name = parse_name(&name, alloc_id);
#ifdef OS_ULTRIX
			sn->sn_addr = 0;
			if (have_unused_datasym) {
				lastname = symstring(st->st_symio_id,
								last_data_symno);
				if (*lastname == '_' &&
					   strcmp(lastname + 1, sn->sn_name) == 0) {
					getsym(st->st_symio_id, last_data_symno,
									&data_nm);
					sn->sn_addr = data_nm.n_value;
				}
				have_unused_datasym = FALSE;
						
			}
#endif
			sn->sn_next = stf->stf_sn;
			stf->stf_sn = sn;
			break;
#ifdef OS_ULTRIX
		case N_DATA:
			last_data_symno = symno;
			have_unused_datasym = TRUE;
			break;
#endif
		case N_FUN:
			name = symstring(st->st_symio_id, symno);

			/*  Some compilers (e.g. gcc) put read only strings
			 *  in the text segment, and generate N_FUN symbols
			 *  for them.  We're not interested in these here.
			 */
			if ((cptr = strchr(name, ':')) == NULL ||
						(cptr[1] != 'F' && cptr[1] != 'f'))
				break;

			if (f != NULL) {
				FU_SYMLIM(f) = symno;
				f = NULL;
			}

			name = save_fname(st, name, FALSE);

			fl = (funclist_t *)alloc(alloc_id, sizeof(funclist_t));
			flist = ci_make_func(alloc_id, name, nm.n_value,
					     (symtab_id_t)st, sfiles, flist, fl);
			flist->fu_fsyminfo_id = (fsyminfo_id_t)make_fsyminfo(alloc_id,symno);
			flist->fu_preamble_id = NULL;
			flist->fu_flags |= FU_STATIC;
			++flist_len;

			fl->fl_func = flist;
			fl->fl_prev = sfiles->fi_funclist_tail;
			sfiles->fi_funclist_tail = fl;

			f = flist;
			seen_func = TRUE;
			break;
		case N_TEXT:
		case N_TEXT | N_EXT:
			name = symstring(st->st_symio_id, symno);

			/*  Some compilers put N_TEXT symbols out with object
			 *  file names, often with the same addresses as real
			 *  functions.  We don't want these.
			 *
			 *  We also don't want N_TEXT symbols which immediately
			 *  follow an N_FUN symbol with the same address.
			 */
			if (nm.n_type == N_TEXT) {
				if (*name != '_') {
					/*  A .o file symbol is definitely
					 *  the end of the current function,
					 *  if any.
					 */
					if (f != NULL) {
						FU_SYMLIM(f) = symno;
						f = NULL;
					}
					break;
				}
				if (f != NULL && f->fu_addr == nm.n_value)
					break;
			}
			
			/*  You'd expect that we'd close down the current
			 *  function here, but some C compilers put out
			 *  N_TEXT symbols in the middle of the N_SLINE
			 *  symbols for a function.
			 *
			 *  Thus we leave f alone, and rely on an N_SO or
			 *  a .o file N_TEXT to terminate the current
			 *  function.
			 */

			name = save_fname(st, name, TRUE);
			flist = ci_make_func(alloc_id, name, nm.n_value,
					     (symtab_id_t)st, (fil_t *)NULL,
					     flist, (funclist_t *)NULL);
			flist->fu_fsyminfo_id = (fsyminfo_id_t)make_fsyminfo(alloc_id,symno);
			flist->fu_preamble_id = NULL;
			flist->fu_flags |= FU_NOSYM | FU_DONE_BLOCKS |
								FU_DONE_LNOS;
			if ((nm.n_type & N_EXT) == 0)
				flist->fu_flags |= FU_STATIC;
			++flist_len;
			break;
#ifdef ARCH_CLIPPER
		case N_DATA:
			/*  The Clipper C compiler puts out an N_LCSYM symbol
			 *  with the wrong address for static globals, then
			 *  later puts out an N_DATA with the right address.
			 *  This we keep a list of N_DATA symbols for each
			 *  file so we can check the addresses later.
			 *
			 *  Note that we can't just stick the symbol in the
			 *  global addresses list, as we may have static
			 *  globals with the same name in different files.
			 */
			name = symstring(st->st_symio_id, symno);
			if (*name != '_')
				break;
			stf->stf_addrlist = insert_global_addr(alloc_id,
							stf->stf_addrlist, name + 1,
							(taddr_t)nm.n_value);
			break;
#endif
		case N_BSS | N_EXT:
		case N_DATA | N_EXT:
			name = symstring(st->st_symio_id, symno);
#ifndef COFF_SUN386
			if (*name != '_')
				break;
			++name;
#endif
			st->st_addrlist = insert_global_addr(alloc_id,
				      st->st_addrlist, name,
				      st->st_text_addr_offset + (taddr_t)nm.n_value);
			break;
		case N_BCOMM:
			cblist_id = add_common_block(cblist_id, stf, f,
							 st, &symno);
			break;
		default:
			panic("unexpected symbol in init_syms");
		}
	}

	if (f != NULL) {
		FU_SYMLIM(f) = symno;
		f = NULL;
	}
	if (stf != NULL) {
		wrapup_stf(stf, fmap, mapsize);
		stf->stf_symlim = symno;
	}

	free((char *)fmap);
	free((char *)istack);

	functab_id = make_functab(st, flist, flist_len, first_addr, last_addr);
	finish_common_blocks(cblist_id, st, p_have_common_blocks);

#ifdef COFF_SUN386
	get_func_coff_lno_offsets(st->st_symio_id, sfiles);
#endif

	*p_sfiles = sfiles;
	*p_functab_id = functab_id;
	*p_cblist_id = cblist_id;
}

/*  NOTE: this routine is not used on the DECstation 3100.
 */
type_t *
tnum_to_type(stf, tnum)
stf_t *stf;
int tnum;
{
	type_t *type;
	ftype_t *ft, *last;

	last = NULL;
	for (ft = stf->stf_types; ft != NULL; ft = ft->ft_next) {
		if (ft->ft_tnum > tnum)
			break;
		last = ft;
	}
	if (last != NULL && last->ft_tnum == tnum)
		type = &last->ft_type;
	else
		type = get_type(stf, tnum, stf->stf_symno, stf->stf_symlim-1);
	return type;
}

/*  NOTE: this routine is not used on the DECstation 3100.
 */
static type_t *
get_type(stf, tnum, min, max)
stf_t *stf;
int tnum, min, max;
{
	const char *s;
	int symno;

	if (get_def(stf, tnum, min, max, &symno, &s) == -1)
		return NULL;
	return TypeId(stf, &symno, &s, TRUE);
}

/*  NOTE: this routine is not used on the DECstation 3100.
 */
type_t *
lookup_tnum(stf, tnum)
stf_t *stf;
int tnum;
{
	ftype_t *ft;

	for (ft = stf->stf_types; ft != NULL; ft = ft->ft_next)
		if (ft->ft_tnum == tnum)
			return &ft->ft_type;
	return NULL;
}

/*  NOTE: this routine is not used on the DECstation 3100.
 */
ftype_t *
insert_ftype(stf, tnum)
stf_t *stf;
int tnum;
{
	ftype_t *last, *ft, *new;

	last = ft = NULL;
	for (ft = stf->stf_types; ft != NULL; ft = ft->ft_next) {
		if (ft->ft_tnum > tnum)
			break;
		last = ft;
	}
	if (last != NULL && last->ft_tnum == tnum)
		panic("type already exists in insert_type");

	new = (ftype_t *) alloc(stf->stf_symtab->st_alloc_id, sizeof(ftype_t));
	new->ft_tnum = tnum;
	ci_init_type(&new->ft_type, TY_NOTYPE);

	if (last == NULL)
		stf->stf_types = new;
	else
		last->ft_next = new;

	new->ft_next = ft;
	return new;
}

/*  Return the symbol number for the definition of type number tnum
 *  in file stf.
 *  Search the symbols between min and lim.
 *
 *  NOTE: this routine is not used on the DECstation 3100.
 */
static int
get_def(stf, tnum, min, max, p_symno, p_s)
stf_t *stf;
int tnum, min, max;
int *p_symno;
const char **p_s;
{
	nlist_t nm;
	const char *s, *base, *pos;
	int symno, match;
	int f, t;

	for (symno = min; symno <= max; symno++) {
		getsym(stf->stf_symtab->st_symio_id, symno, &nm);
		base = s = symstring(stf->stf_symtab->st_symio_id, symno);
		if (base == NULL)
			continue;
		while ((pos = strchr(s, '=')) != NULL) {

			/*  Definitions of the form "=x" are unresolved tag
			 *  definitions.  We don't want these - we want the
			 *  real definition.
			 */
			if (pos[1] == 'x') {
				s = pos + 1;
				continue;
			}

			if (pos[-1] == ')') {
				while (pos >= base && *pos != '(')
					--pos;
				*p_s = pos;
				Typenum(&pos, &f, &t);
				scheck(&pos, '=');
				match = f == stf->stf_fnum && t == tnum &&
									*pos != 'x';
			}
			else {
				while (pos >= base && isdigit(pos[-1]))
					--pos;
				*p_s = pos;
				match = parse_num(&pos) == tnum;
			}
			if (match) {
				*p_symno = symno;
				return 0;
			}
			s = pos + 1;
		}
	}
	return -1;
}

#ifdef OS_SUNOS
static stf_t *
id_to_hdrstf(stf, id, symno)
stf_t *stf;
taddr_t id;
int symno;
{
	hf_t **p_hf, **maxhf;
	stf_t *hdrstf;

	maxhf = stf->stf_fmap + stf->stf_mapsize;
	hdrstf = NULL;
	for (p_hf = stf->stf_fmap; p_hf < maxhf; ++p_hf) {
		if ((*p_hf)->hf_id == id) {
			if (symno != -1 && (*p_hf)->hf_stf->stf_symno != symno)
				continue;
			hdrstf = (*p_hf)->hf_stf;
			break;
		}
	}
	
	if (hdrstf == NULL)
		panic("hdr botch in ght");

	return hdrstf;
}
#endif

static void
get_fi_types(fil, is_header)
fil_t *fil;
bool is_header;
{
	stf_t *stf;
	symtab_t *st;
	int symno, func_symno;
	bool push_aggr;
	funclist_t *fl;
	block_t *bl;

	if (fil->fi_flags & FI_DOING_TYPES)
		panic("dup fil in gft");
	if (fil->fi_flags & FI_DONE_TYPES)
		return;
	fil->fi_flags |= FI_DOING_TYPES;
	
	bl = fil->fi_block;
	stf = (stf_t *)fil->fi_stf;
	st = stf->stf_symtab;
	fl = fil->fi_funclist_head;
	func_symno = (fl != NULL) ? FU_SYMNO(fl->fl_func) : -1;

	/*  Note that we skip the 0th symbol in the source file, which
	 *  is the N_SO or N_BINCL symbol.  This is essential for N_BINCL -
	 *  if we didn't skip it we would recurse forever.
	 */
	for (symno = stf->stf_symno + 1; ; ++symno) {
		nlist_t nm;
		const char *name, *s;
		class_t class;
		type_t *type;

		/*  Skip over the symbols of functions - we don't
		 *  want type information from functions.
		 */
		while (symno == func_symno) {
			symno = FU_SYMLIM(fl->fl_func);
			fl = fl->fl_next;
			func_symno = (fl != NULL) ? FU_SYMNO(fl->fl_func) : -1;
		}

		if (symno >= stf->stf_symlim)
			break;

		getsym(st->st_symio_id, symno, &nm);
#ifdef OS_SUNOS
		if ((nm.n_type == N_BINCL || nm.n_type == N_EXCL)
							   && nm.n_value != 0) {
			stf_t *hdrstf;
			block_t *hdrbl;
			int match_symno;

			/*  If the symbol is N_BINCL we insist on the current
			 *  symbol number for hdrstf->stf_symno for a match
			 *  in id_to_hdrstf().  This is because you can have
			 *  different header files with the same id, and if
			 *  two such header files are #included in the same
			 *  source file we could pick up the wrong one if
			 *  we went just by the id.  Picking the wrong one
			 *  is disastrous for an N_BINCL because we set
			 *  symno below to hdrstf->stf_symlim.
			 */
			match_symno = (nm.n_type == N_BINCL) ? symno : -1;
			hdrstf = id_to_hdrstf(stf, nm.n_value, match_symno);

			get_fi_types(hdrstf->stf_fil, TRUE);
			hdrbl = hdrstf->stf_fil->fi_block;

			push_typedefs_and_aggrs(st->st_alloc_id, hdrbl, bl);

			if (nm.n_type == N_BINCL)
				symno = hdrstf->stf_symlim - 1;
		}
#endif
		if (nm.n_type != N_LSYM)
			continue;

		name = symstring(stf->stf_symtab->st_symio_id, symno);

		/*  Skip junk symbols, and ones that aren't TypeDefs (see st_parse.c)
		 */
		for (s = name; isalnum(*s) || *s == '_' || *s == '$'; ++s)
			;
		if (*s != ':' || (s[1] != 'T' && s[1] != 't'))
			continue;

		++s;
		type = Class(stf, &symno, &s, &class);

		switch(class) {
		case CL_TYPEDEF:
			type->ty_typedef->td_next = bl->bl_typedefs;
			bl->bl_typedefs = type->ty_typedef;

			/*  Unnamed aggregate types don't get a CL_TAGNAME
			 *  entry, so if we get one that hasn't already been
			 *  typedefed, fall through to the aggregate pushing code.
			 */  
			switch (type->ty_code) {
			case TY_STRUCT:
			case TY_UNION:
			case TY_ENUM:
				push_aggr = type->ty_typedef == NULL &&
					    type->ty_aggr_or_enum->ae_tag == NULL;
				break;
			default:
				push_aggr = FALSE;
				break;
			}
			if (!push_aggr)
				break;
			/*  fall though ... */
		case CL_TAGNAME:
			type->ty_aggr_or_enum->ae_next = bl->bl_aggr_or_enum_defs;
			bl->bl_aggr_or_enum_defs = type->ty_aggr_or_enum;
			break;
		case CL_NOCLASS:
			break;
		default:
			panic("bad class in gft");
		}
	}
	
	if (!is_header)
		ci_apply_to_aelist(bl->bl_aggr_or_enum_defs, fix_undef_aggr,
						(char *)bl->bl_aggr_or_enum_defs);

	fil->fi_flags &= ~FI_DOING_TYPES;
	fil->fi_flags |= FI_DONE_TYPES;
}

static aggr_or_enum_def_t *
match_tag(ae, tag)
aggr_or_enum_def_t *ae;
const char *tag;
{
	if (ae->ae_is_complete == AE_COMPLETE && ae->ae_tag != NULL &&
						      strcmp(ae->ae_tag, tag) == 0)
		return ae;
	
	return NULL;
}

static aggr_or_enum_def_t *
fix_undef_aggr(uae, arg)
aggr_or_enum_def_t *uae;
const char *arg;
{
	aggr_or_enum_def_t *aelist;
	type_t *utype, *type;
	aggr_or_enum_def_t *ae;

	aelist = (aggr_or_enum_def_t *)arg;

	switch (uae->ae_type->ty_code) {
	case TY_U_STRUCT:
	case TY_U_UNION:
	case TY_U_ENUM:
		if (uae->ae_tag == NULL)
			ae = NULL;
		else
			ae = ci_apply_to_aelist(aelist, match_tag, uae->ae_tag);
		break;
	default:
		ae = NULL;
		break;
	}

	if (ae == NULL)
		return NULL;

	utype = uae->ae_type;
	type = ae->ae_type;

	utype->ty_code = type->ty_code;
	utype->ty_aggr_or_enum = ae;

	if (utype->ty_typedef != NULL && type->ty_typedef == NULL) {
		type->ty_typedef = utype->ty_typedef;
		type->ty_typedef->td_type = type;
	}

	return NULL;
}

/*  NOTE: this routine is not used on the DECstation 3100.
 */
var_t *
get_fi_vars(fil)
fil_t *fil;
{
	nlist_t nm;
	snlist_t *sn;
	int symno;
	class_t class;
	type_t *type;
	const char *s;
	symtab_t *st;
	stf_t *stf;
	var_t *v, *last;
	taddr_t addr;

	if (fil->fi_flags & FI_DONE_VARS)
		return fil->fi_block->bl_vars;
	
	/*  Needed for typedefs etc, as with get_fu_blocks().
	 */
	get_fi_types(fil, FALSE);

	stf = (stf_t *) fil->fi_stf;
	fil->fi_block->bl_vars = last = NULL;
	st = stf->stf_symtab;
	for (sn = stf->stf_sn; sn != NULL; sn = sn->sn_next) {
		symno = sn->sn_symno;
		getsym(st->st_symio_id, symno, &nm);
#ifdef OS_ULTRIX
		if (sn->sn_addr != 0)
			nm.n_value = sn->sn_addr;
#endif
#ifdef ARCH_CLIPPER
		addr = find_addr_in_addrlist(stf->stf_addrlist, sn->sn_name);
		if (addr != NULL)
			nm.n_value = addr;
#endif
		s = symstring(st->st_symio_id, symno);
		(void) parse_name(&s, (alloc_id_t)0);

		/*  Ignore symbols with funny characters in their names.
		 *  For normally named symbols, parse_name will have
		 *  left us at the ':' that follows the name, so if
		 *  *s is not ':' then we have an oddly named symbol.
		 *
		 *  BUG: the reason for adding this code is that we
		 *  tripped over the basic type definition of
		 *  "logical*4" in an a.out produced by the SunOS 4.0
		 *  f77 compiler.  This is because we assume a fixed
		 *  number (N_FORTRAN_BASIC_TYPES) of basic types, and
		 *  the 4.0 f77 added a new one.  We need a better way
		 *  of deciding how many basic types there are, based
		 *  on the symbol table of the file.
		 */
		if (*s != ':')
			continue;
		++s;

		type = Class(stf, &symno, &s, &class);
		if (nm.n_value != 0)
			addr = (taddr_t)nm.n_value + st->st_text_addr_offset;
		else
			addr = lookup_global_addr(st, sn->sn_name);
		if (addr != 0 && (class == CL_EXT || class == CL_STAT)) {
			v = ci_make_var(st->st_alloc_id, sn->sn_name,
								class, type, addr);
			v->va_language = fil->fi_language;
			v->va_next = fil->fi_block->bl_vars;
			if (last != NULL)
				last->va_next = v;
			else
				fil->fi_block->bl_vars = v;
			last = v;
		}
	}
	if (last != NULL)
		last->va_next = NULL;
	fil->fi_flags |= FI_DONE_VARS;
	return fil->fi_block->bl_vars;
}
#endif /* !ST_TE */

/*  Many compilers give you two seperate symbol table entries for a
 *  register parameter - one for the stack copy of the parameter, one
 *  for the register.  Go through the vars of f zapping the stack
 *  symbols of any such registers.
 *
 *  We use an n^2 algorithm as we don't expect functions to have more than
 *  about ten arguments.
 */
void
fix_register_params(f)
func_t *f;
{
	register var_t *vreg, *varg;
	var_t *prev, *next, *head;
	int zapped_vreg;

	/*  We are only interested in the vars at the outermost scope.
	 */
	if (f->fu__blocks == NULL)
		return;

	head = f->fu__blocks->bl_vars;

	prev = NULL;
	for (vreg = head; vreg != NULL; vreg = next) {
		next = vreg->va_next;
		zapped_vreg = FALSE;
		if (vreg->va_class == CL_REG) {
			for (varg = head; varg != NULL; varg = varg->va_next){
				if (varg->va_class == CL_ARG &&
				    strcmp(vreg->va_name, varg->va_name) == 0) {
					vreg->va_next = varg->va_next;
					*varg = *vreg;
					if (prev != NULL)
						prev->va_next = next;
					else
						head = next;
					
					/*  Would free vreg at this point but
					 *  it was allocated by alloc() so
					 *  we can't - forget it.
					 */

					zapped_vreg = TRUE;
					break;
				}
			}
		}
		if (!zapped_vreg)
			prev = vreg;
	}

	f->fu__blocks->bl_vars = head;
}

void
iterate_over_vars_of_block(block, func, args)
block_t *block;
void (*func)PROTO((var_t *v, char *c_args));
char *args;
{
	block_t *bl;
	var_t *v;

	if (block != NULL) {
		for (bl = block->bl_blocks; bl != NULL; bl = bl->bl_next)
			iterate_over_vars_of_block(bl, func, args);
		for (v = block->bl_vars; v != NULL; v = v->va_next)
			(*func)(v, args);
	}
}

#ifndef ST_TE
/*  This function is an attempt to improve the accuracy of block start line
 *  numbers.  Most compilers make the LBRAC for a block point to after the
 *  declarations of variables in the block, but we want the declarations
 *  included in the block.
 *
 *  The way we do this is to search back through the source lines, looking
 *  for the '{' character that starts a block.  We only go back a smallish
 *  number of lines - if we don't find anything by then we just return orig_lnum.
 *
 *  Note that we make no attempt to deal with #ifdefs, comments, strings etc.
 *  To do this right in general would require too much knowledge of C.
 *  It doesn't matter if we get this wrong - the result is just that our
 *  block numbers will be slightly off.
 *
 *  The so package numbers lines from zero up, whereas the symbol table
 *  numbers them from one up, so we have to adjust.
 *
 *  NOTE: this routine is not used on the DECstation 3100.
 */
static int
backup_lnum_to_curly(f, orig_lnum)
func_t *f;
int orig_lnum;
{
	const int lbrace = '{'; /* to avoid upsetting vi's '}' matching */
	so_id_t so;
	const char *cptr;
	int lnum, min_lnum;

	if (f->fu_fil == NULL || open_source_file(f->fu_fil, FALSE) != 0)
		return orig_lnum;
	so = f->fu_fil->fi_so;

	if (orig_lnum > so_get_nlines(so))
		return orig_lnum;
	
	/*  Make sure that we only go back a reasonable distance, and that we
	 *  don't go out of range.
	 */
	min_lnum = orig_lnum - 50;
	if (min_lnum < 1)
		min_lnum = 1;
	if (orig_lnum < 1)
		orig_lnum = 1;

	lnum = orig_lnum;

	/*  If the opening line doesn't have an lbrace, or has an
	 *  lbrace with nothing but whitespace after it, back up
	 *  a line.
	 *
	 *  Again, this code is not bulletproof (we'll get confused by
	 *  if the lbrace is inside a comment, for example), but it doesn't
	 *  matter much if it gets things wrong.
	 */
	cptr = strrchr(so_getline(so, lnum - 1), lbrace);
	if (cptr != NULL) {
		int lastch;
		bool incomment;

		incomment = FALSE;
		lastch = *cptr++;
		for (; ; ++cptr) {
			if (incomment && lastch == '*' && *cptr == '/')
				incomment = FALSE;
			else if (!incomment && lastch == '/' && *cptr == '*')
				incomment = TRUE;
			else {
				if (!incomment && isalnum(*cptr))
					break;
				lastch = *cptr;
			}
			if (*cptr == '\0') {
				--lnum;
				break;
			}
		}
	}
	if (cptr == NULL || *cptr == '\0')
		--lnum;

	for (; lnum >= min_lnum; --lnum)
		if (strchr(so_getline(so, lnum - 1), lbrace) != NULL)
			return lnum;
	return orig_lnum;
}

/*  NOTE: this routine is not used on the DECstation 3100.
 */
block_t *
get_fu_blocks(f)
func_t *f;
{
	nlist_t nm, extra_nm;
	int symno;
	class_t class;
	type_t *type;
	alloc_id_t alloc_id;
	const char *name, *s;
	stf_t *stf;
	var_t *v;
	var_t *varlists[MAX_BLOCK_LEVEL];
	aggr_or_enum_def_t *aelists[MAX_BLOCK_LEVEL];
	typedef_t *tdlists[MAX_BLOCK_LEVEL];
	block_t *blocklists_tab[MAX_BLOCK_LEVEL + 1], **blocklists, *bl;
	bool skipthis;
	int symlim, extra_sym, level, lnum, next_lnum;

	if (f->fu_flags & FU_DONE_BLOCKS)
		return f->fu__blocks;
	
	/*  We want to use typedef names if possible for structures and
	 *  enums (see ci_basetype_name), so we need any type information
	 *  in the file.  Thus we load the file types.
	 */
	get_fi_types(f->fu_fil, FALSE);

	stf = (stf_t *) f->fu_fil->fi_stf;
	alloc_id = stf->stf_symtab->st_alloc_id;

	blocklists = blocklists_tab + 1;
	blocklists[-1] = f->fu_fil->fi_block;

	level = 0;
	varlists[0] = NULL;
	aelists[0] = NULL;
	tdlists[0] = NULL;
	blocklists[0] = NULL;
	symlim = FU_SYMLIM(f);
	extra_sym = 0;

	for (symno = FU_SYMNO(f); symno < symlim + extra_sym; symno++) {
		if (symno == symlim)
			nm = extra_nm;
		else
			getsym(stf->stf_symtab->st_symio_id, symno, &nm);
		switch(nm.n_type) {
		case N_LBRAC:
			if (level == MAX_BLOCK_LEVEL)
				panic("block nesting too deep");

#ifdef ARCH_SUN386
			lnum = addr_to_lnum(f, (taddr_t)nm.n_value);
#else
			lnum = addr_to_lnum(f, stf->stf_addr + (taddr_t)nm.n_value);
#endif

			bl = ci_make_block(alloc_id, blocklists[level - 1]);
			bl->bl_start_lnum = backup_lnum_to_curly(f, lnum);

			bl->bl_vars = varlists[level];
			varlists[level] = NULL;
			bl->bl_typedefs = tdlists[level];
			tdlists[level] = NULL;
			bl->bl_aggr_or_enum_defs = aelists[level];
			aelists[level] = NULL;

			bl->bl_next = blocklists[level];
			blocklists[level] = bl;

			++level;
			blocklists[level] = NULL;
			varlists[level] = NULL;
			aelists[level] = NULL;
			tdlists[level] = NULL;

			break;

		case N_RBRAC:
			if (level <= 0)
				panic("missing LBRAC");
			
			/*  The logically last RBRAC symbol of a function
			 *  (the one that takes us from level 1 to level 0)
			 *  sometimes appears earlier than we'd expect.
			 *  We arrange that we always process this symbol
			 *  last.
			 */
			if (level == 1 && symno < symlim - 1) {
				extra_nm = nm;
				extra_sym = 1;
				break;
			}

			blocklists[level - 1]->bl_blocks = blocklists[level];
			--level;

			/*  FRAGILE CODE:
			 *
			 *  We subtract one from the return value of addr_to_lnum()
			 *  here.  This is based only on looking at the results
			 *  on some code - not on any documentation.
			 */
			lnum = rbrac_addr_to_lnum(f, stf->stf_addr + nm.n_value);
			blocklists[level]->bl_end_lnum = lnum - 1;

			/*  FRAGILE CODE:
			 *
			 *  More heuristics to try and cope with bogus block
			 *  start lines.  If a block starts on the same line
			 *  as the following block at the same level, back up
			 *  one then go back to the next previous curly.
			 *
			 *  Note that bl_next points the lexically *previous*
			 *  block, because we push blocks as we find them.
			 */
			bl = blocklists[level]->bl_blocks; 
			if (bl == NULL)
				break;

			next_lnum = bl->bl_start_lnum + 1;
			for (; bl != NULL; bl = bl->bl_next) {
				if (next_lnum != 0 && bl->bl_start_lnum >= next_lnum)
					bl->bl_start_lnum =
					  backup_lnum_to_curly(f, next_lnum - 1);
				next_lnum = bl->bl_start_lnum;
			}
			if (blocklists[level]->bl_start_lnum >= next_lnum)
				blocklists[level]->bl_start_lnum =
					  backup_lnum_to_curly(f, next_lnum - 1);

			break;
		case N_LSYM:
		case N_PSYM:
		case N_STSYM:
		case N_RSYM:
		case N_FUN:
		case N_LCSYM:
			s = symstring(stf->stf_symtab->st_symio_id, symno);

			/*  The Sun 4 C compiler emits symbols for
			 *  what look like compiler-created temporary
			 *  variables.  These have names like '#tmp0'.
			 *  So skip any names that start with '#'.
			 */
			if (*s == '#')
				break;

			name = parse_name(&s, alloc_id);
			scheck(&s, ':');
			type = Class(stf, &symno, &s, &class);

			if (class == CL_FUNC && symno == FU_SYMNO(f) &&
					       strcmp(name, f->fu_name) == 0) {
				f->fu_type = type;
				break;
			}
					
			switch (class) {
			case CL_ARG:
			case CL_AUTO:
				if (nm.n_type == N_RSYM)
					class = CL_REG;
				skipthis = FALSE;
				break;
			case CL_REG:
			case CL_LSTAT:
			case CL_REF:
				skipthis = FALSE;
				break;
			case CL_TYPEDEF:
				type->ty_typedef->td_next = tdlists[level];
				tdlists[level] = type->ty_typedef;
				skipthis = TRUE;
				break;
			case CL_TAGNAME:
				type->ty_aggr_or_enum->ae_next = aelists[level];
				aelists[level] = type->ty_aggr_or_enum;
				skipthis = TRUE;
				break;
			default:
				skipthis = TRUE;
				break;
			}
			
			/*  Gcc without the -O flag emits duplicate symbols
			 *  for parameters.  So if we are doing the outermost
			 *  block (where parameters occur) discard this variable
			 *  if we already have one with same name and address.
			 */
			if (level == 0 && class == CL_AUTO) {
				for (v = varlists[level]; v != NULL;
							      v = v->va_next) {
					if (v->va_addr == nm.n_value &&
					      (v->va_class == CL_ARG ||
					       v->va_class == CL_REG) &&
					    strcmp(v->va_name, name) == 0) {
						skipthis = TRUE;
						break;
					}
				}
			}

			if (skipthis)
				break;

#ifdef ARCH_CLIPPER
			if (class == CL_REG && nm.n_value >= 16)
				nm.n_value = (nm.n_value - 16) * 2 + 16;
#endif
			v = ci_make_var(alloc_id, name, class, type,nm.n_value);
			v->va_language = stf->stf_language;
			if (v->va_class == CL_LSTAT)
				v->va_addr += stf->stf_symtab->st_text_addr_offset;
			v->va_next = varlists[level];
			varlists[level] = v;

#if defined(IS_BIG_ENDIAN) && defined(FIX_SHORT_PARAMS)
			if (v->va_class == CL_ARG) {
				switch (v->va_type->ty_code) {
				case TY_USHORT:
				case TY_SHORT:
					v->va_addr += sizeof(int)-sizeof(short);
					break;
				case TY_UCHAR:
				case TY_CHAR:
					v->va_addr += sizeof(int)-sizeof(char);
					break;
				default:
					/* No adjustment */
					break;
				}
			}
#endif
#ifdef ARCH_SUN4
			/*  Special case code for structures passed by value.
			 *  The Sun 4 C compiler copies these to the stack and
			 *  passes a pointer to this copy to the called
			 *  function.
			 *
			 *  In the symbol table for the called function we get
			 *  a normal symbol for the variable, followed by a
			 *  register symbol for it.  We spot the situation by
			 *  looking for register vars that are too big to go
			 *  in a register.
			 *
			 *  We push an extra level of indirection on to the
			 *  variable's type, and lose the symbol table entry
			 *  before this one if it refers to a variable of the
			 *  same name.
			 *
			 *  BUG: What if the passed pointer doesn't go in a
			 *       register?
			 */
			if (class == CL_REG &&
			    type->ty_code != TY_FLOAT &&
			    type->ty_code != TY_DOUBLE &&
		            typesize(type) > sizeof(long)) {
				type = ci_make_type(alloc_id, DT_PTR_TO);
				type->ty_qualifiers = 0;
				type->ty_base = v->va_type;
				v->va_type = type;
				v->va_flags |= VA_HIDE_PTR;

				if (v->va_next != NULL &&
				    strcmp(name, v->va_next->va_name) == 0)
					v->va_next = v->va_next->va_next;
			}
#endif
			break;
		}
	}
	if (level != 0)
		panic("missing RBRAC");

	/*  With some compilers, functions with parameters but no local
	 *  variables don't get an LBRAC symbol.  If this is the case
	 *  for this function, build a level 0 block for it.
	 */
	if (blocklists[0] == NULL && varlists[0] != NULL) {
		bl = ci_make_block(alloc_id, f->fu_fil->fi_block);
		bl->bl_start_lnum = (FU_LNOS(f) == NULL) ? 1 : f->fu__lnos->ln_num;
		bl->bl_end_lnum = f->fu_max_lnum;
		bl->bl_vars = varlists[0];
		bl->bl_typedefs = tdlists[0];
		bl->bl_aggr_or_enum_defs = aelists[0];
		blocklists[0] = bl;
	}

	f->fu__blocks = blocklists[0];
	f->fu_flags |= FU_DONE_BLOCKS;

	/*  If a function has parameters, no outer level locals but some
	 *  inner locals, we can get a situation where the inner locals
	 *  are noted at one level too low.  Fix this if necessary.
	 */
	if (f->fu__blocks != NULL && f->fu__blocks->bl_blocks == NULL &&
						f->fu__blocks->bl_next != NULL) {
		f->fu__blocks->bl_blocks = f->fu__blocks->bl_next;
		f->fu__blocks->bl_next = NULL;
	}
	
	fix_register_params(f);
	return f->fu__blocks;
}

/*  NOTE: this routine is not used on the DECstation 3100.
 */
static void
get_lno_symrange(f, p_start, p_lim)
func_t *f;
int *p_start, *p_lim;
{
	stf_t *stf;

	if (f->fu_fil == NULL)
		panic("NULL fil in gls");
	stf = (stf_t *)f->fu_fil->fi_stf;

	if (stf->stf_flags & STF_LNOS_PRECEDE_FUNCS) {
		register funclist_t *prevfl;
		
		prevfl = f->fu_fl->fl_prev;
		if (prevfl != NULL)
			*p_start = FU_SYMNO(prevfl->fl_func);
		else
			*p_start = stf->stf_symno;
		*p_lim = FU_SYMNO(f);
	}
	else {
		*p_start = FU_SYMNO(f);
		*p_lim = FU_SYMLIM(f);
	}
}

/*  NOTE: this routine is not used on the DECstation 3100.
 */
lno_t *
get_fu_lnos(f)
func_t *f;
{
	lno_t *last;
	symtab_t *st;
#ifndef COFF_SUN386
	lno_t *lno, *first;
	int symno, symno_start, symno_lim;
#endif

	if (f->fu_flags & FU_DONE_LNOS)
		return f->fu__lnos;
	
	st = (symtab_t *)f->fu_symtab_id;

	/*  First grab all the N_SLINE symbols in the function - we possibly
	 *  discard some of them later.  On the 386i the lnos are not stored
	 *  with other symbols, so we use a seperate function to get them.
	 */
#ifdef COFF_SUN386
	f->fu__lnos = read_func_coff_lnos(st->st_symio_id, f, &last);
#else
	last = NULL;
	get_lno_symrange(f, &symno_start, &symno_lim);
	for (symno = symno_start; symno < symno_lim; ++symno) {
		nlist_t nm;

		getsym(st->st_symio_id, symno, &nm);
		if (nm.n_type == N_SLINE) {
			lno = (lno_t *) alloc(((symtab_t *)f->fu_symtab_id)->st_alloc_id,
						     sizeof(lno_t));
			lno->ln_num = nm.n_desc;
			lno->ln_addr = nm.n_value + st->st_text_addr_offset;
			if (last != NULL)
				last->ln_next = lno;
			else
				f->fu__lnos = lno;
			last = lno;
		}
	}
	if (last == NULL)
		f->fu__lnos = NULL;
	else
		last->ln_next = NULL;
#endif
	
	/*  FRAGILE CODE
	 *
	 *  Some compilers helpfully put out junk symbols at the start
	 *  of functions.  We try to get rid of these so that add bpt
	 *  puts the bpt at the line the user expects.
	 *
	 *  The rules for how many symbol to junk were guessed at by
	 *  examining symbol tables.  To put it mildly, this is not
	 *  a desirable way to do things.  This stuff is almost the
	 *  most fragile code in the debugger - it is liable to break
	 *  with every new release.
	 *
	 *  The current code should work with FORTRAN for SunOS 3.X and
	 *  Ultrix 2.2, and with C for SunOS 3.X and 4.0 and Ultrix 2.2.
	 *
	 *  Of course, it's too much to ask that the first SLINE symbol
	 *  for a function should correspond to the first line of the
	 *  function, or that any of this stuff should be documented ...
	 *
	 *  Notice that we don't free lnos as we discard them.  This is
	 *  because they were allocated via alloc().
	 *
	 *  The Sun 386i doesn't seem to put out junk symbols, so don't
	 *  do anything for it.
	 */
#ifndef COFF_SUN386
	switch(f->fu_language) {
	case LANG_C:
		lno = f->fu__lnos;
#ifdef OS_ULTRIX
		if (lno != NULL)
			lno = lno->ln_next;
#else
		/*  Skip any symbols whose address is the same as the
		 *  function address.
		 */
		for (; lno != NULL && lno->ln_addr == f->fu_addr; lno = lno->ln_next)
			;
#endif

		/*  Skip all but the last of a set of lnos with the same
		 *  address.
		 */
		if (lno != NULL) {
			taddr_t base_addr;
			lno_t *next;

			base_addr = lno->ln_addr;
			for (;;) {
				next = lno->ln_next;
				if (next == NULL || next->ln_addr != base_addr)
					break;
				lno = next;
			}
		}

		f->fu__lnos = lno;
		break;
	case LANG_FORTRAN:
		/*  The f77 compiler on Ultrix 2.2 and 4.3BSD generates
		 *  lots of N_SLINE symbols with the function address as
		 *  the address.  Try rejecting all but the last of these
		 *  symbols.
		 */
		first = NULL;
		for (lno = f->fu__lnos; lno != NULL; lno = lno->ln_next)
			if (lno->ln_addr == f->fu_addr)
				first = lno;
		if (first != NULL) {
			for (lno = f->fu__lnos; lno != first; lno = lno->ln_next)
				;
			f->fu__lnos = first;
		}
		break;
	case LANG_UNKNOWN:
		break;
	default:
		panic("unknown language in gfl");
	}
#endif /* !COFF_SUN386 */

	if (f->fu__lnos != NULL && last != NULL)
		f->fu_max_lnum = last->ln_num;
	f->fu_flags |= FU_DONE_LNOS;
	return f->fu__lnos;
}
#endif /* !ST_TE */
