/* st_fmap.c - functions for mapping names and addresses to func_t pointers */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_st_fmap_c_sccsid[] = "@(#)st_fmap.c	1.17 12/9/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <sys/types.h>
#include <string.h>
#include <a.out.h>		/* needed for st_priv.h */

#include <local/ukcprog.h>
#include <mtrprog/genmergesort.h>
#include <mtrprog/alloc.h>
#include <mtrprog/strcache.h>

#include "ups.h"
#include "symtab.h"
#ifdef ARCH_MIPS
#include "mips_frame.h"
#endif
#include "st_priv.h"

/*  The sorted array of func_t pointers.
 *  Used for binary chop search for function by address.
 */
typedef struct functabst {
	taddr_t ft_first_addr;		/* First valid address of segment */
	taddr_t ft_last_addr;		/* Last ditto */

	func_t **ft_tab;		/* Sorted table of functions */
	int ft_nfuncs;			/* #Funcs in this table */
} functab_t;

static void adjust_lstat_addr PROTO((var_t *v, char *c_delta));
static func_t *sort_functab PROTO((alloc_id_t alloc_id, functab_t *functab,
						func_t *flist, int flist_len));

GENERIC_MERGE_SORT(static,sortflist,func_t,fu_next)

ALLOC_NEW_FREELIST(extern,funclist_t,flist,fl_next)


/*  Comparison function for the sort by address.  The list may contain
 *  duplicate entries for a given function (this comes from the dbx
 *  type entry in the symbol table and the linker N_TEXT entry).
 *  Do the comparison so that entries with symbol table information
 *  appear before entries without).
 */
int
addrcmp(f1, f2)
register func_t *f1, *f2;
{
	if (f1->fu_addr == f2->fu_addr)
		return (f1->fu_flags & FU_NOSYM) - (f2->fu_flags & FU_NOSYM);
	return (f1->fu_addr < f2->fu_addr) ? -1 : 1;
}

/*  Create a functab structure representing the list of functions flist.
 *  Flist is unsorted (actually in symbol table order).
 *
 *  We just save away the parameters in the structure.  We don't sort
 *  the functions by address until the first addr_to_func() call.
 */
functab_id_t
make_functab(st, flist, flist_len, first_addr, last_addr)
symtab_t *st;
register func_t *flist;
int flist_len;
taddr_t first_addr, last_addr;
{
	functab_t *functab;
	taddr_t addr_offset;
	func_t *f;

	addr_offset = first_addr;
	for (f = flist; f != NULL; f = f->fu_next)
		f->fu_addr += addr_offset;
	
	functab = (functab_t *) alloc(st->st_alloc_id, sizeof(functab_t));

	functab->ft_first_addr = first_addr;
	functab->ft_last_addr = last_addr;

	st->st_funclist = sort_functab(st->st_alloc_id, functab,
							flist, flist_len);

	return (functab_id_t)functab;
}

/*  Create a table of functions sorted by address in functab->ft_tab.
 *  We eliminate duplicate function entries.
 */
static func_t *
sort_functab(alloc_id, functab, flist, flist_len)
alloc_id_t alloc_id;
functab_t *functab;
func_t *flist;
int flist_len;
{
	func_t *f, *prev, *next, *high_func;
	func_t **ft, **ftab;
	int nfuncs;

	f = sortflist(flist, flist_len, addrcmp);

	nfuncs = 0;
	flist = NULL;
	prev = NULL;

	for (; f != NULL; f = next) {
		next = f->fu_next;

		if (prev != NULL && f->fu_addr == prev->fu_addr) {
			if ((f->fu_flags & FU_STATIC) == 0)
				prev->fu_flags &= ~FU_STATIC;
		}
		else {
			f->fu_next = flist;
			flist = f;
			++nfuncs;
		}

		prev = f;
	}

	ftab = (func_t **)alloc(alloc_id, (nfuncs + 1) * sizeof(func_t *));
	ft = ftab + nfuncs;

	for (f = flist; f != NULL; f = f->fu_next)
		*--ft = f;

	high_func = (func_t *)alloc(alloc_id, sizeof(func_t));
	high_func->fu_addr = functab->ft_last_addr + 1;
	high_func->fu_name = "[end text marker]";
	ftab[nfuncs] = high_func;

	functab->ft_tab = ftab;
	functab->ft_nfuncs = nfuncs;

	return flist;
}

static void
adjust_lstat_addr(v, c_delta)
var_t *v;
char *c_delta;
{
	if (v->va_class == CL_LSTAT)
		v->va_addr += (long)c_delta;
}

/*  Adjust the segment base functab by delta.  This involves changing the
 *  function and line number addresses for all functions in the table.
 *  We also have to change the offsets of any static local variables.
 *
 *  This is called when a SunOS 4.X shared library's base address changes -
 *  see change_text_addr_offset() in st_stab.c.
 */
void
adjust_functab_text_addr_base(functab_id, funclist, delta)
functab_id_t functab_id;
func_t *funclist;
long delta;
{
	functab_t *functab;
	register func_t *f;
	lno_t *lno;

	for (f = funclist; f != NULL; f = f->fu_next) {
		f->fu_addr += delta;

		if (f->fu_flags & FU_DONE_LNOS) {
			for (lno = FU_LNOS(f); lno != NULL; lno = lno->ln_next)
				lno->ln_addr += delta;
		}

		if (f->fu_flags & FU_DONE_BLOCKS) {
			iterate_over_vars_of_block(FU_BLOCKS(f),
						   adjust_lstat_addr,
						   (char *)delta);
		}
	}

	functab = (functab_t *)functab_id;
	functab->ft_first_addr += delta;
	functab->ft_last_addr += delta;
}

/*  Return the highest text address that is still within function f.
 *  Used for getting the last text line of a function. See trun_j.c.
 *
 *  BUG: this returns a way too high value for the last function in
 *       a text segment, where all the padding to the next 8k or whatever
 *       boundary is included.  Possibly peek at the text for an rts?
 */
taddr_t
get_addr_lim(f)
func_t *f;
{
	functab_t *functab;
	register func_t **ft;

	functab = (functab_t *)((symtab_t *)f->fu_symtab_id)->st_functab_id;

	for (ft = functab->ft_tab; *ft != f; ++ft)
		;
	
	return ft[1]->fu_addr;
}

/*  Call (*func)(f, addrlim(f), arg1, arg2) for all functions in
 *  the function table of symtab_id.
 */
int
iterate_over_functions(symtab_id, func, arg1, arg2)
symtab_id_t symtab_id;
iof_func_t func;
char *arg1, *arg2;
{
	functab_t *functab;
	register func_t **ft, **lim;
	int res;

	functab = (functab_t *)((symtab_t *)symtab_id)->st_functab_id;

	ft = functab->ft_tab;
	lim = ft + functab->ft_nfuncs;
	for (; ft < lim; ++ft)
		if ((res = (*func)(*ft, ft[1]->fu_addr, arg1, arg2)) != 0)
			return res;
	return 0;
}

/*  Look up the function by address and return the function with address
 *  greatest but <= addr.
 *
 *  On the first call of this for a given functab we must create the
 *  sorted table.
 */
func_t *
addr_and_functab_to_func(functab_id, addr)
functab_id_t functab_id;
taddr_t addr;
{
	register func_t **mid;
	functab_t *functab;
	func_t **vec, **low, **max, **high;
	int vec_len;
	taddr_t maddr;

	functab = (functab_t *)functab_id;

	/*  Don't bother with the search if the address is out of range.
	 */
	if (addr < functab->ft_first_addr || addr >= functab->ft_last_addr)
		return NULL;

	vec = functab->ft_tab;
	vec_len = functab->ft_nfuncs;

	low = vec;
	high = max = vec + vec_len;
	while (low <= high) {
		mid = low + (high - low) / 2;
		maddr = (*mid)->fu_addr;
		if (addr >= maddr && (mid == max || addr < mid[1]->fu_addr))
			return (mid == vec + vec_len) ? NULL : *mid;
		else if (maddr > addr)
			high = mid - 1;
		else
			low = mid + 1;
	}
	return NULL;
}
