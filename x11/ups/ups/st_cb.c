/* st_cb.c - FORTRAN common block routines */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_st_cb_c_sccsid[] = "@(#)st_cb.c	1.19 15/9/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <sys/types.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <a.out.h>
#ifndef OS_RISCOS
#include <stab.h>
#endif

#include <local/ukcprog.h>
#include <mtrprog/strcache.h>

#include "ups.h"
#include "symtab.h"
#ifdef ARCH_MIPS
#include "mips_frame.h"
#endif
#include "st_priv.h"
#include "ci.h"

typedef struct cbstringst {
	const char *cbs_string;
	int cbs_symno;
} cbstring_t;

/*  FORTRAN common block. 
 */
typedef struct cblockst {
	const char *cb_name;
	symtab_t *cb_symtab;
	taddr_t cb_addr;
	stf_t *cb_stf;
	int cb_id;
	func_t *cb_deffunc;
	int cb_onedef;
	int cb_symno;
	int cb_nstrings;
	cbstring_t *cb_strings;
	int cb_havevars;
	var_t *cb__vars;
} cblock_t;

#define CB_VARS(cb)	((cb)->cb_havevars ? (cb)->cb__vars \
					   : get_cblock_vars((common_block_id_t)(cb)))

/*  List of common blocks that hangs off a func_t.
 */
typedef struct cblistst {
	cblock_t *cl_cblock;
	struct cblistst *cl_next;
} cblist_t;

var_t *get_cblock_vars PROTO((common_block_id_t cblock));

static cblock_t *search_cblist PROTO((cblist_t *cl, const char *name));
#ifndef ST_TE
static void add_cb_strings PROTO((cblock_t *cb, cbstring_t *strings, int nstrings));
static int get_cb_strings PROTO((symtab_t *st, int symno,
					cbstring_t **p_strings, int *p_nstrings));
#endif

void
free_cblist_strings(cblist_id)
cblist_id_t cblist_id;
{
	cblist_t *cl;

	for (cl = (cblist_t *)cblist_id; cl != NULL; cl = cl->cl_next)
		free((char *)cl->cl_cblock->cb_strings);
}

cblist_id_t
create_empty_cblist()
{
	return (cblist_id_t)(cblist_t *)NULL;
}

void
finish_common_blocks(cblist_id, st, p_have_common_blocks)
cblist_id_t cblist_id;
symtab_t *st;
int *p_have_common_blocks;
{
	cblist_t *cblist, *cl;
	cblock_t *cb, *lastcb;
	int same_count, id;

	same_count = 0;	/* just to keep lint happy */

	lastcb = NULL;
	id = 0;
	cblist = (cblist_t *)cblist_id;

	for (cl = cblist; cl != NULL; cl = cl->cl_next) {
		cb = cl->cl_cblock;
		if (cb->cb_strings == NULL)
			panic("undefined common");
		cb->cb_onedef = FALSE;
		if (cb->cb_id != id) {
			if (lastcb != NULL && same_count == 1)
				lastcb->cb_onedef = TRUE;
			same_count = 1;
			cb->cb_addr = lookup_global_addr(st, cb->cb_name);
			id = cb->cb_id;
		}
		else {
			++same_count;
			cb->cb_addr = lastcb->cb_addr;
		}
		lastcb = cb;
	}
	if (lastcb != NULL && same_count == 1)
		lastcb->cb_onedef = TRUE;
	*p_have_common_blocks = cblist != NULL;
}

#ifndef ST_TE
/*  Copy the string pointers from the static vector returned by get_cb_strings.
 *
 *  If cb->cb_strings not NULL, the strings are added to the end of the
 *  existing strings.
 */
static void
add_cb_strings(cb, strings, nstrings)
cblock_t *cb;
cbstring_t *strings;
int nstrings;
{
	cbstring_t *dst, *lim;
	int new_nstrings;

	new_nstrings = cb->cb_nstrings + nstrings;
	if (cb->cb_strings == NULL)
		cb->cb_strings = (cbstring_t *) e_malloc((new_nstrings + 1) *
								sizeof(cbstring_t));
	else
		cb->cb_strings = (cbstring_t *) e_realloc((char *)cb->cb_strings,
					    (new_nstrings + 1) * sizeof(cbstring_t));

	dst = cb->cb_strings + cb->cb_nstrings;
	lim = strings + nstrings + 1;
	while (strings < lim)
		*dst++ = *strings++;
	cb->cb_nstrings = new_nstrings;
}

cblist_id_t
add_common_block(cblist_id, stf, f, st, p_symno)
cblist_id_t cblist_id;
stf_t *stf;
func_t *f;
symtab_t *st;
int *p_symno;
{
	static int id = 0;
	cblist_t *cblist, *cl, *first_cl, *same_cl, *prev_cl;
	cblock_t *cb;
	cbstring_t *s1ptr, *s2ptr, *strings;
	const char *name;
	int empty, nstrings, start_symno;
	int this_id, symno;
	nlist_t nm;

	symno = *p_symno;
	cblist = (cblist_t *)cblist_id;

	name = symstring(st->st_symio_id, symno++);
	start_symno = symno;

	getsym(st->st_symio_id, symno, &nm);
	empty = nm.n_type == N_ECOMM;

	/*  Seen this one already?
	 */
	first_cl = prev_cl = NULL;
	for (cl = cblist; cl != NULL; cl = cl->cl_next) {
		if (strcmp(cl->cl_cblock->cb_name, name) == 0) {
			first_cl = cl;
			break;
		}
		prev_cl = cl;
	}
	this_id = (first_cl != NULL) ? first_cl->cl_cblock->cb_id : ++id;

	if (empty && first_cl != NULL)
		cb = first_cl->cl_cblock;
	else {
		if (empty) {
			strings = NULL;
			nstrings = 0;
		}
		else
			symno = get_cb_strings(st, symno, &strings, &nstrings);

		same_cl = NULL;
		for (cl = first_cl; cl != NULL; cl = cl->cl_next) {
			cb = cl->cl_cblock;
			if (cb->cb_id != this_id)
				break;
			if (cb->cb_strings == NULL || cb->cb_deffunc == f) {
				same_cl = cl;
				break;
			}
			if (cb->cb_nstrings == nstrings) {
				s1ptr = cb->cb_strings;
				s2ptr = strings;
				for (; s1ptr->cbs_string != NULL; ++s1ptr, ++s2ptr)
					if (strcmp(s1ptr->cbs_string,
							s2ptr->cbs_string) != 0)
						break;
				if (s1ptr->cbs_string == NULL) {
					same_cl = cl;
					break;
				}
			}
		}

		if (same_cl != NULL) {
			cb = same_cl->cl_cblock;
			if (cb->cb_deffunc == f)
				add_cb_strings(cb, strings, nstrings);
			else {
				if (cb->cb_strings == NULL)
					add_cb_strings(cb, strings, nstrings);
				cb->cb_symno = start_symno;
			}
		}
		else {
			/*  same_cl == NULL => this is a new common block
			 *  definition (first_cl == NULL), or a common block
			 *  definition which doesn't agree with an existing
			 *  one (legal in FORTRAN).
			 */
			cb = (cblock_t *) alloc(st->st_alloc_id, sizeof(cblock_t));
			cb->cb_name = strsave(name);
			cb->cb_symtab = st;
			cb->cb_addr = 0;
			cb->cb_stf = stf;
			if (empty) {
				cb->cb_symno = -10000; /* for safety */
				cb->cb_strings = NULL;
			}
			else {
				cb->cb_symno = start_symno;
				cb->cb_strings = NULL;
				cb->cb_nstrings = 0;
				add_cb_strings(cb, strings, nstrings);
			}
			cb->cb_nstrings = nstrings;
			cb->cb_havevars = FALSE;
			cb->cb_id = this_id;
			cb->cb_deffunc = f;
			cb->cb__vars = NULL;

			/*  Insert the new block in the global list.
			 */
			cl = (cblist_t *) alloc(st->st_alloc_id, sizeof(cblist_t));
			cl->cl_cblock = cb;
			if (prev_cl == NULL) {
				cl->cl_next = cblist;
				cblist = cl;
			}
			else {
				cl->cl_next = prev_cl->cl_next;
				prev_cl->cl_next = cl;
			}
		}

	}
	
	/*  Add the common block to the list for this function.
	 */
	if (f != NULL) {
		cl = (cblist_t *) alloc(st->st_alloc_id, sizeof(cblist_t));
		cl->cl_cblock = cb;
		cl->cl_next = (cblist_t *)FU_CBLIST(f);
		FU_CBLIST(f) = (long) cl;
	}

	*p_symno = symno;
	return (cblist_id_t)cblist;
}

static int
get_cb_strings(st, symno, p_strings, p_nstrings)
symtab_t *st;
int symno;
cbstring_t **p_strings;
int *p_nstrings;
{
	static cbstring_t *strings;
	static int strings_size = 0;
	nlist_t nm;
	int nstrings;
	const char *string;

	if (strings_size == 0) {
		strings_size = 32;
		strings = (cbstring_t *) e_malloc((strings_size + 1) *
								sizeof(cbstring_t));
	}

	nstrings = 0;
	for (;;) {
		getsym(st->st_symio_id, symno, &nm);
		if (nm.n_type == N_ECOMM)
			break;
		if (nstrings >= strings_size) {
			strings_size *= 2;
			strings = (cbstring_t *) e_realloc((char *)strings,
					    strings_size * sizeof(cbstring_t));
		}
		string = symstring(st->st_symio_id, symno);
		if (strchr(string, '=') == NULL) {
			strings[nstrings].cbs_string = string;
			strings[nstrings].cbs_symno = symno;
			++nstrings;
		}
		++symno;
	}
	strings[nstrings].cbs_string = NULL;

	*p_strings = strings;
	*p_nstrings = nstrings;
	return symno;
}
#endif /* !ST_TE */
	
static cblock_t *
search_cblist(cl, name)
cblist_t *cl;
const char *name;
{
	register cbstring_t *cbs, *lim;
	register int len;

	for (; cl != NULL; cl = cl->cl_next) {
		cbs = cl->cl_cblock->cb_strings;
		lim = cbs + cl->cl_cblock->cb_nstrings;
		for (; cbs < lim; ++cbs) {
			len = strchr(cbs->cbs_string, ':') - cbs->cbs_string;
			if (strlen(name) == len &&
				     strncmp(cbs->cbs_string, name, len) == 0)
				return cl->cl_cblock;
		}
	}
	return NULL;
}

void
iterate_over_cblist(cblist_id, func)
cblist_id_t cblist_id;
void (*func)PROTO((common_block_id_t cblock));
{
	cblist_t *cl;

	for (cl = (cblist_t *)cblist_id; cl != NULL; cl = cl->cl_next)
		(*func)((common_block_id_t)cl->cl_cblock);
}

const char *
get_cblock_funcname(cblock)
common_block_id_t cblock;
{
	cblock_t *cb;

	cb = (cblock_t *)cblock;

	return (cb->cb_onedef || cb->cb_deffunc == NULL) ? NULL
							 : cb->cb_deffunc->fu_name;
}

const char *
get_cblock_name(cblock)
common_block_id_t cblock;
{
	return ((cblock_t *)cblock)->cb_name;
}

var_t *
get_cblock_vars(cblock)
common_block_id_t cblock;
{
#ifdef ST_TE
	panic("gcv NYI under ST_TE");
	return NULL;
#else
	register cblock_t *cb;
	var_t *v;
	int symno, i;
	const char *s, *name;
	type_t *type;
	class_t class;
	symtab_t *st;
	nlist_t nm;

	cb = (cblock_t *)cblock;
	if (!cb->cb_havevars) {
		st = cb->cb_symtab;
		for (i = 0; i < cb->cb_nstrings; ++i) {
			symno = cb->cb_strings[i].cbs_symno;
			getsym(st->st_symio_id, symno, &nm);

			s = cb->cb_strings[i].cbs_string;
			name = parse_name(&s, st->st_alloc_id);
			scheck(&s, ':');
			type = Class(cb->cb_stf, &symno, &s, &class);

			if (class != CL_NOCLASS) {
				v = ci_make_var(st->st_alloc_id, name, class, type,
						cb->cb_addr + (taddr_t) nm.n_value);
				v->va_language = cb->cb_stf->stf_language;
				v->va_next = cb->cb__vars;
				cb->cb__vars = v;
			}
		}
		cb->cb_havevars = TRUE;
	}
	return cb->cb__vars;
#endif /* !ST_TE */
}

void
global_and_cblist_to_var(cblist_id, name, f, p_cblock, p_fil, p_var)
cblist_id_t cblist_id;
const char *name;
func_t *f;
common_block_id_t *p_cblock;
fil_t **p_fil;
var_t **p_var;
{
	cblock_t *cb;
	var_t *v;

	if (f==NULL || (cb = search_cblist((cblist_t *)FU_CBLIST(f), name)) == NULL)
		cb = search_cblist((cblist_t *)cblist_id, name);
	if (cb == NULL) {
		*p_var = NULL;
		return;
	}
	for (v = CB_VARS(cb); v != NULL; v = v->va_next) {
		if (strcmp(name, v->va_name) == 0) {
			*p_fil = cb->cb_stf->stf_fil;
			*p_var = v;
			break;
		}
	}
	*p_cblock = (common_block_id_t) cb;
}
