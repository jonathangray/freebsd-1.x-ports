/* va_type - type handling routines */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_va_type_c_sccsid[] = "@(#)va_type.c	1.17 20/5/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <local/ukcprog.h>

#include "ups.h"
#include "symtab.h"
#include "data.h"
#include "va.h"
#include "va_priv.h"
#include "ci.h"

int
typesize(type)
type_t *type;
{
	return dynamic_type_size(type, (ilist_t *)NULL);
}

/*  Return the size in bytes of type.
 *
 *  This routine just calls ci_typesize, except for arrays, where it
 *  checks for variable size arrays.
 */
int
dynamic_type_size(type, il)
type_t *type;
ilist_t *il;
{
	int size;

	if (type->ty_size != -1)
		size = type->ty_size;
	else if (type->ty_code == DT_ARRAY_OF) {
		dim_t *dim;
		int basesize, high, low, size_is_variable;

		dim = type->ty_dim;
		size_is_variable = dim->di_ldynamic || dim->di_hdynamic;
		if (size_is_variable) {
			if (il == NULL || !il->il_low_known || !il->il_high_known)
				return UNKNOWN_SIZE;
			low = il->il_low;
			high = il->il_high;
		}
		else {
			low = dim->di_low;
			high = dim->di_high;
		}
		basesize = dynamic_type_size(type->ty_base,
				    (il != NULL) ? il->il_next
						 : (ilist_t *)NULL);

		/*  The VAX Ultrix C compiler has a bug in it's symbol
		 *  table code (surprise!) - it emits arrays with a size
		 *  of zero if the first thing it sees is a pointer to
		 *  the array.
		 */
		if (high <= low) {
			errf("%s array size - assumed one",
					(low < high) ? "Negative" : "Zero");
			high = low + 1;
		}

		if (basesize == UNKNOWN_SIZE)
			size = UNKNOWN_SIZE;
		else
			size = (high - low) * basesize;
		if (!size_is_variable)
			type->ty_size = size;
	}
	else
		size = ci_typesize((lexinfo_t *)NULL, type);

	if (size <= 0 && size != UNKNOWN_SIZE)
		panic("non positive type size in typesize");

	return size;
}

int
fix_if_fortran_dynamic_char(type, addr, ilist)
type_t *type;
taddr_t addr;
ilist_t *ilist;
{
	type_t *prev, *dtype;
	int size;

	prev = NULL;
	for (dtype = type; ISDERIV(dtype->ty_code); dtype = dtype->ty_base)
		prev = dtype;
	if (dtype->ty_code == TY_CHARACTER && prev != NULL &&
					prev->ty_code == DT_ARRAY_OF &&
					prev->ty_dim->di_hdynamic) {

		for (; type != prev; type = type->ty_base)
			ilist = ilist->il_next;

		if (dread(addr + prev->ty_dim->di_high - 1,
						(char *)&size, sizeof(int)) == 0) {
			ilist->il_high_known = TRUE;
			ilist->il_high = size + 1;
		}
		else
			ilist->il_high_known = FALSE;
		return TRUE;
	}
	else
		return FALSE;
}

type_t *
get_basetype(type)
type_t *type;
{
	while (ISDERIV(type->ty_code))
		type = type->ty_base;
	return type;
}

type_t *
get_type_at_level(v, level)
var_t *v;
int level;
{
	type_t *type;

	for (type = v->va_type; type != NULL && --level >= 0; type = type->ty_base)
		;
	if (type == NULL)
		panic("level too high in get_type_at_level");
	return type;
}
