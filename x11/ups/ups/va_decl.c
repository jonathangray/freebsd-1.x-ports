/* va_decl.c - variable declaration construction */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_va_decl_c_sccsid[] = "@(#)va_decl.c	1.22 20/5/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include <local/ukcprog.h>
#include <mtrprog/alloc.h>

#include "ups.h"
#include "symtab.h"
#include "ci.h"
#include "data.h"
#include "va.h"
#include "va_priv.h"

ilist_t *new_ilist PROTO((void));
static const char *derivname PROTO((typecode_t deriv, typecode_t last,
								int fake_array));
static const char *c_mkdecl PROTO((dvar_t *dv));
static void dim_to_s PROTO((int val, int known, char *buf));
static const char *fortran_mkdecl PROTO((dvar_t *dv));
static void set_il_indices PROTO((ilist_t *il, type_t *type));

ALLOC_NEW_FREELIST(extern,ilist_t,ilist,il_next)

/*  Calculate the address of the bracketed part of the variable declaration.
 */
taddr_t
dvar_addr(dv)
dvar_t *dv;
{
	ilist_t *ilist;
	type_t *type;
	typecode_t deriv;
	int i, basesize, offset;
	taddr_t addr;

	ilist = dv->dv_ilist;
	addr = dv->dv_addr;
	type = dv->dv_var->va_type;

	for (i = dv->dv_ilevel; i > 0; --i) {
		deriv = type->ty_code;

		if (deriv != DT_ARRAY_OF && deriv != DT_PTR_TO)
			panic("bad deriv in da");

		if (deriv == DT_PTR_TO) {
			if (addr == 0)
				return 0;
			else
				if (dread(addr, (char *)&addr, 4) != 0)
					return BAD_ADDR;
		}

		if (deriv == DT_ARRAY_OF && !ilist->il_low_known)
			return BAD_ADDR;

		basesize = dynamic_type_size(type->ty_base, ilist->il_next);
		if (basesize == UNKNOWN_SIZE)
			return BAD_ADDR;

		offset = ilist->il_index;
		if (deriv == DT_ARRAY_OF)
			offset -= ilist->il_low;
		addr += offset * basesize;

		ilist = ilist->il_next;
		type = type->ty_base;
	}

	return addr;
}

ilist_t *
dup_ilist(old)
ilist_t *old;
{
	ilist_t *last, *newlist, *il;

	newlist = last = NULL;
	for (; old != NULL; old = old->il_next) {
		il = new_ilist();
		*il = *old;
		if (last != NULL)
			last->il_next = il;
		else
			newlist = il;
		last = il;
	}
	if (last != NULL)
		last->il_next = NULL;
	return newlist;
}

static const char *
derivname(deriv, last, fake_array)
typecode_t deriv, last;
int fake_array;
{
	int needb;

	needb = (deriv == DT_FUNC_RETURNING || deriv == DT_ARRAY_OF) && last == DT_PTR_TO;
	switch(deriv) {
	case DT_PTR_TO:
		if (fake_array)
			return needb ? "(%s){%s}" : "%s{%s}";
		else
			return needb ? "*(%s)" : "*%s";
	case DT_FUNC_RETURNING:
		return needb ? "(%s)()" : "%s()";
	case DT_ARRAY_OF:
		return needb ? "(%s)[%s]" : "%s[%s]";
	default:
		panic("bad deriv in derivname");
	}
	/* NOTREACHED */
	return NULL;	/* to keep gcc happy */
}

/*  Marker for start of var name.  Any character that can't occur in a
 *  declaration.
 */
#define MARKER_CH	'@'

static const char *
c_mkdecl(dv)
dvar_t *dv;
{
	static char bufs[2][256];
	class_t cl;
	typecode_t deriv, last, bt;
	int cur, level, fake_array;
	char ibuf[12];
	bitfield_t *bf;
	var_t *v;
	ilist_t *ilist;
	type_t *type;
	bool hide_ptr;
	
	v = dv->dv_var;
	ilist = dv->dv_ilist;
	cur = 0;
	(void) strcpy(bufs[cur], v->va_name);
	bufs[cur][0] = MARKER_CH;
	last = TY_NOTYPE;
	hide_ptr = v->va_flags & VA_HIDE_PTR;

	level = 0;
	type = v->va_type;
	for (;;) {
		const char *fmt;

		if (level == dv->dv_ilevel) {
			(void) sprintf(bufs[1-cur], "%c%s%c",
						C_AOPEN, bufs[cur], C_ACLOSE);
			cur = 1 - cur;
		}

		deriv = type->ty_code;
		if (!ISDERIV(deriv))
			break;

		fake_array = deriv == DT_PTR_TO && level < dv->dv_ilevel;
		if (deriv == DT_ARRAY_OF || fake_array) {
			if (level < dv->dv_ilevel) {
				(void) sprintf(ibuf, "%d", ilist->il_index);
				ilist = ilist->il_next;
			}
			else
				*ibuf = '\0';
		}

		fmt = derivname(deriv, last, fake_array);
		fmt = hide_ptr ? "%s" : derivname(deriv, last, fake_array);
		(void) sprintf(bufs[1-cur], fmt, bufs[cur], ibuf);
		cur = 1 - cur;
		last = deriv;

		type = type->ty_base;
		++level;
	}

	bt = type->ty_code;
	if (bt == TY_BITFIELD) {
		bf = type->ty_bitfield;
		(void) sprintf(bufs[1 - cur], "%s %s:%d", "int",
							bufs[cur],
							bf->bf_width);
	}
	else {
		const char *btname;

		if ((dv->dv_flags & DVF_NO_TYPEDEFS) == 0 &&
				type->ty_typedef != NULL &&
				(bt == TY_STRUCT || bt == TY_UNION || bt == TY_ENUM))
			btname = type->ty_typedef->td_name;
		else
			btname = ci_basetype_name(type);

		(void) sprintf(bufs[1 - cur], "%s %s", btname, bufs[cur]);
	}
	cur = 1 - cur;

	cl = v->va_class;

	if (cl == CL_REG && (v->va_flags & VA_HIDE_PTR) != 0)
		cl = CL_AUTO;

	if (cl == CL_STAT || cl == CL_LSTAT || cl == CL_REG) {
		(void) sprintf(bufs[1 - cur], "%s %s",
					(cl == CL_REG) ? "register" : "static",
					bufs[cur]);
		cur = 1 - cur;
	}
	dv->dv_ul_start = strchr(bufs[cur], MARKER_CH) - bufs[cur];
	bufs[cur][dv->dv_ul_start] = *v->va_name;
	return bufs[cur];
}

static void
dim_to_s(val, known, buf)
int val, known;
char *buf;
{
	if (known)
		(void) sprintf(buf, "%d", val);
	else
		(void) strcpy(buf, "[?]");
}

static const char *
fortran_mkdecl(dv)
dvar_t *dv;
{
	static char bufs[2][256];
	char ldim[20], hdim[20];
	int cur, need_brac, ch;
	var_t *v;
	dim_t *dim;
	type_t *type;
	ilist_t *il;
	
	v = dv->dv_var;
	cur = 0;
	bufs[cur][0] = '\0';

	need_brac = FALSE;
	il = dv->dv_ilist;
	for (type = v->va_type; ISDERIV(type->ty_code); type = type->ty_base) {

		if (type->ty_code != DT_ARRAY_OF)
			panic("unknown type derivation in fortran_mkdecl");
		if (type->ty_base->ty_code == TY_CHARACTER)
			break;

		ch = (type == v->va_type) ? FORTRAN_ACLOSE : ',';
		(void) sprintf(bufs[1 - cur], "%d%c%s", il->il_index, ch, bufs[cur]);
		cur = 1 - cur;
		need_brac = TRUE;
		il = il->il_next;
	}
	if (need_brac)
		(void) sprintf(bufs[1 - cur], " %c%s", FORTRAN_AOPEN, bufs[cur]);
	else
		(void) sprintf(bufs[1 - cur], " %s", bufs[cur]);
	cur = 1 - cur;

	il = dv->dv_ilist;
	need_brac = FALSE;
	for (type = v->va_type; ISDERIV(type->ty_code); type = type->ty_base) {
		if (type->ty_base->ty_code == TY_CHARACTER)
			break;

		dim = type->ty_dim;
		ch = (type == v->va_type) ? ')' : ',';

		dim_to_s(il->il_high - 1, il->il_high_known, hdim);
		if (!dim->di_ldynamic && dim->di_low == 1)
			(void) sprintf(bufs[1-cur], "%s%c%s", hdim, ch, bufs[cur]);
		else {
			dim_to_s(il->il_low, il->il_low_known, ldim);
			(void) sprintf(bufs[1-cur], "%s:%s%c%s",
							ldim, hdim, ch, bufs[cur]);
		}
		need_brac = TRUE;
		cur = 1 - cur;
		il = il->il_next;
	}

	(void) sprintf(bufs[1 - cur], need_brac ? "%s(%s" : "%s %s",
							v->va_name, bufs[cur]);
	cur = 1 - cur;

	if (ISDERIV(type->ty_code)) {
		if (type->ty_base->ty_code != TY_CHARACTER)
			panic("botch in fortran_mkdecl");
		dim_to_s(il->il_high - 1, il->il_high_known, hdim);
		(void) sprintf(bufs[1 - cur], "character*%s %s", hdim, bufs[cur]);
	}
	else
		(void) sprintf(bufs[1 - cur], "%s %s",
						ci_basetype_name(type), bufs[cur]);
	cur = 1 - cur;

	return bufs[cur];
}

const char *
mkdecl(dv)
dvar_t *dv;
{
	switch(dv->dv_var->va_language) {
	case LANG_C:
	case LANG_UNKNOWN:
		return c_mkdecl(dv);
	case LANG_FORTRAN:
		return fortran_mkdecl(dv);
	default:
		panic("unknown language in mkdecl");
	}
	/* NOTREACHED */
	return NULL;	/* to keep gcc happy */
}

/*  return the default format for the variable value.
 */
vformat_t
default_format(vtype, type)
type_t *vtype, *type;
{
	typecode_t typecode;

	typecode = type->ty_code;
	if (typecode == TY_BITFIELD)
		typecode = type->ty_bitfield->bf_code;

	switch(typecode) {

	/*  The C interpreter preserves array types (rather than
	 *  having them turn into pointers) to make the copyin/copyout
	 *  pointer stuff work in breakpoint code.  Thus we can get
	 *  arrays types for display expressions.
	 */
	case DT_ARRAY_OF:
	case DT_PTR_TO:
		return DF_UHEX;

	case TY_CHAR:
	case TY_UCHAR:
		while (vtype != type && vtype->ty_base != type)
			vtype = vtype->ty_base;
		if (vtype->ty_code == DT_ARRAY_OF || vtype->ty_code == DT_PTR_TO)
			return DF_STRING;
		else
			return DF_ASCII;

	case TY_USHORT:
	case TY_UINT:
	case TY_ULONG:
		return DF_UDEC;
	
	default:
		return DF_SDEC;
	}
}

/*  Return the default level for displaying a variable of type type.
 */
int
default_level(type)
type_t *type;
{
	int level;

	if (type->ty_code == DT_PTR_TO && type->ty_base->ty_code == TY_CHAR)
		level = 1;
	else {
		for (level = 0; type->ty_code == DT_ARRAY_OF; type = type->ty_base)
			++level;
	}

	return level;
}

/*  Set the ilist indices for an array.  Note that we set il_high and il_low
 *  even for a dynamic array, because di_{low,high} contain the stack offset
 *  of the place where the current array dimensions can be found for dynamic
 *  arrays.
 */
static void
set_il_indices(il, type)
ilist_t *il;
type_t *type;
{
	dim_t *dim;

	il->il_index = 0;
	if (type->ty_code == DT_ARRAY_OF) {
		dim = type->ty_dim;
		il->il_low = dim->di_low;
		il->il_high = dim->di_high;
		il->il_low_known = !dim->di_ldynamic;
		il->il_high_known = !dim->di_hdynamic;
		il->il_index = dim->di_low;
	}

}

void
change_dv_level(dv, dir)
dvar_t *dv;
enum leveldir dir;
{
	ilist_t *il, *last;
	type_t *type;
	int i;

	if (dir == CL_DOWN) {
		--dv->dv_ilevel;
		return;
	}

	il = dv->dv_ilist;
	last = NULL;
	type = dv->dv_var->va_type;
	for (i = 0; i < dv->dv_ilevel; ++i) {
		type = type->ty_base;
		last = il;
		il = il->il_next;
	}
	if (il == NULL) {
		il = new_ilist();
		set_il_indices(il, type);
		if (last != NULL)
			last->il_next = il;
		else
			dv->dv_ilist = il;
		il->il_next = NULL;
	}
	++dv->dv_ilevel;
}

ilist_t *
make_ilist(orig_type, level)
type_t *orig_type;
int level;
{
	type_t *type;
	ilist_t *ilist, *il, *last;
	int i;

	ilist = last = NULL;
	type = orig_type;
	for (i = 0; i < level; i++) {
		while (type->ty_code != DT_ARRAY_OF && type->ty_code != DT_PTR_TO)
			type = type->ty_base;

		il = new_ilist();
		set_il_indices(il, type);

		if (last != NULL)
			last->il_next = il;
		else
			ilist = il;
		last = il;
		type = type->ty_base;
	}
	if (last != NULL)
		last->il_next = NULL;
	return ilist;
}
