/* va_menu.c - code handling actions initiated from the va menu */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_va_menu_c_sccsid[] = "@(#)va_menu.c	1.24 26/7/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <local/wn.h>
#include <local/obj/obj.h>
#include <local/ukcprog.h>
#include <mtrprog/alloc.h>

#include "objtypes.h"
#include "ups.h"
#include "symtab.h"
#include "data.h"
#include "va.h"
#include "menudata.h"
#include "va_priv.h"
#include "ui.h"
#include "obj_stack.h"
#include "tdr.h"

static dvar_t *new_dvar PROTO((void));
static void free_dvar PROTO((dvar_t *dv));
static taddr_t get_dv_addr PROTO((objid_t par, var_t *v, ilist_t *ilist, int *p_decl_may_have_changed));

ALLOC_NEW_FREE(static,dvar_t,dvar,dv_nextfree)

static taddr_t
get_dv_addr(par, v, ilist, p_decl_may_have_changed)
objid_t par;
var_t *v;
ilist_t *ilist;
int *p_decl_may_have_changed;
{
	taddr_t addr, fp, ap;

	switch (v->va_class) {
	case CL_REF:
		(void) get_stack_func(par, &fp, &ap);
		if (dread(ap + v->va_addr, (char *)&addr, sizeof(addr)) != 0)
			addr = BAD_ADDR;
		if (v->va_language == LANG_FORTRAN) {
			*p_decl_may_have_changed =
				fix_if_fortran_dynamic_char(v->va_type, ap, ilist);
		}
		break;
	case CL_ARG:
		(void) get_stack_func(par, &fp, &ap);
		addr = ap + v->va_addr;
		break;
	case CL_AUTO:
		(void) get_stack_func(par, &fp, &ap);
		addr = fp + v->va_addr;
		break;
	case CL_MOS:
	case CL_MOU:
		addr = var_or_expr_addr(par) + v->va_addr;
		break;
	case CL_EXT:
	case CL_STAT:
	case CL_LSTAT:
		addr = v->va_addr;
		break;
	case CL_REG:
		addr = get_reg_addr(par, v->va_addr);
		break;
	default:
		panic("unknown class in gda");
	}
	return addr;
}

/*  Create an object describing the variable, and insert it in the display
 *  tree as a child of par.  Poscode must be OBJ_FIRST_CHILD or OBJ_LAST_CHILD.
 *  par is also needed to obtain the values of register variables.
 *
 *  If pos is OBJ_LAST_CHILD, we read this as "add this object after the
 *  last child of par which is an OT_VAR object".  This is to maintain
 *  the rule that variables always come before other things like block
 *  headers.
 */
objid_t
add_var_object(par, v, poscode)
objid_t par;
var_t *v;
int poscode;
{
	static bool first_call = TRUE;
	static long initial_dv_flags;
	objid_t code;
	dvar_t *dv;
	fval_t fields[FN_VAR_LAST + 1];
	int junk;

	if (first_call) {
		const char *tdstr;

		tdstr = wn_get_default("WantTypedefs");
		if (tdstr != NULL && strcmp(tdstr, "no") == 0)
			initial_dv_flags = DVF_NO_TYPEDEFS;
		first_call = FALSE;
	}

	dv = new_dvar();
	dv->dv_var = v;
	dv->dv_ilevel = default_level(v->va_type);
	dv->dv_ilist = make_ilist(v->va_type, dv->dv_ilevel);
	dv->dv_addr = get_dv_addr(par, v, dv->dv_ilist, &junk);
	dv->dv_flags = initial_dv_flags;
	dv->dv_format = default_format(v->va_type,
				       get_type_at_level(v, dv->dv_ilevel));
		
	switch (poscode) {
	case OBJ_FIRST_CHILD:
		code = par;
		break;
	case OBJ_LAST_CHILD:
		code = get_code(par, OBJ_CHILD);
		while (code != NULL && get_object_type(code) == OT_VAR)
			code = get_code(code, OBJ_NEXT);
		if (code != NULL)
			poscode = OBJ_BEFORE;
		else {
			code = par;
			poscode = OBJ_LAST_CHILD;
		}
		break;
	default:
		panic("bad poscode in add_var_object");
		code = 0; /* to satisfy gcc */
	}
	new_object((objid_t)dv, OT_VAR, code, poscode);

	fields[FN_VAR_DECL] = (fval_t) strsave(mkdecl(dv));
	fields[FN_VAR_VALUE] = (fval_t) strsave(mkval(dv));
	fields[FN_VAR_LAST] = (fval_t) NULL;
	set_all_fields((objid_t)dv, fields, (fval_t)NULL);
	return (objid_t)dv;
}

/*  Fully dereference var dv if it is an aggregate (struct or union).
 */
type_t *
deref_aggr(dv)
dvar_t *dv;
{
	var_t *v;
	type_t *type, *btype;
	int ilevel;

	v = dv->dv_var;
	btype = get_basetype(v->va_type);
	type = get_type_at_level(v, dv->dv_ilevel);
	ilevel = dv->dv_ilevel;
	if (btype->ty_code == TY_STRUCT || btype->ty_code == TY_UNION)
		for (; type->ty_code == DT_PTR_TO || type->ty_code == DT_ARRAY_OF;
								type = type->ty_base)
			change_dv_level(dv, CL_UP);
	if (dv->dv_ilevel != ilevel)
		redo_decl(dv);
	return type;
}

void
free_displayed_var(obj)
objid_t obj;
{
	free((char *)get_field_value(obj, FN_VAR_DECL));
	free((char *)get_field_value(obj, FN_VAR_VALUE));
	free_ilist_list(((dvar_t *)obj)->dv_ilist);
	free_dvar((dvar_t *)obj);
}

/*  Update the displayed declaration and value after a change.
 */
void
redo_decl(dv)
dvar_t *dv;
{
	fval_t fields[FN_VAR_LAST + 1];
	char *oldval, *olddecl;

	olddecl = (char *) get_field_value((objid_t)dv, FN_VAR_VALUE);
	oldval = (char *) get_field_value((objid_t)dv, FN_VAR_DECL);
	fields[FN_VAR_DECL] = (fval_t) strsave(mkdecl(dv));
	fields[FN_VAR_VALUE] = (fval_t) strsave(mkval(dv));
	fields[FN_VAR_LAST] = (fval_t) NULL;
	set_all_fields((objid_t)dv, fields, (fval_t)NULL);
	free(oldval);
	free(olddecl);
}

vformat_t
mval_to_vformat(cmd)
int cmd;
{
	switch (cmd) {
	case MR_VAR_STRING:		return DF_STRING;
	case MR_VAR_SIGNED_DECIMAL:	return DF_SDEC;
	case MR_VAR_UNSIGNED_DECIMAL:	return DF_UDEC;
	case MR_VAR_SIGNED_OCTAL:	return DF_SOCT;
	case MR_VAR_UNSIGNED_OCTAL:	return DF_UOCT;
	case MR_VAR_SIGNED_HEX:		return DF_SHEX;
	case MR_VAR_UNSIGNED_HEX:	return DF_UHEX;
	case MR_VAR_UNSIGNED_BINARY:	return DF_UBIN;
	case MR_VAR_ASCII_BYTE:		return DF_ASCII;
	default:
		panic("unknown format char in mtf");
		return DF_SDEC;		/* to satisfy gcc */
	}
}

const char *
deriv_to_string(typecode)
typecode_t typecode;
{
	switch(typecode) {
	case DT_ARRAY_OF:
		return "an array";
	case DT_PTR_TO:
		return "a pointer";
	case DT_FUNC_RETURNING:
		return "a function";
	default:
		panic("bad deriv code in dts");
		return NULL;	/* to keep gcc happy */
	}
	/* NOTREACHED */
}

const char *
var_getobjname(obj)
objid_t obj;
{
	static char *last = NULL;
	dvar_t *dv;
	const char *name;
	int count;

	dv = (dvar_t *)obj;

	name = dv->dv_var->va_name;

	count = 1;
	obj = get_code(get_code(obj, OBJ_PARENT), OBJ_CHILD);

	for (; obj != NULL; obj = get_code(obj, OBJ_NEXT)) {
		if (obj == (objid_t)dv)
			break;
		if (get_object_type(obj) != OT_VAR)
			continue;
		if (strcmp(name, ((dvar_t *)obj)->dv_var->va_name) == 0)
			++count;
	}

	if (last != NULL)
		free(last);

	if (count == 1) {
		last = NULL;
		return name;
	}

	last = strf("%d-%s", count, name);
	return last;
}

int
var_dumpobj(arg, code, level)
char *arg;
objid_t code;
int level;
{
	const char *decl, *value;
	
	decl = get_field_value(code, FN_VAR_DECL);
	value = get_field_value(code, FN_VAR_VALUE);

	return td_outf(level, "%-30s %s", decl, value);
}

/*  Process the return from a var menu.
 */
void
do_vars(obj, cmd)
objid_t obj;
int cmd;
{
	dvar_t *dv, *dv2;
	type_t *type;
	const char *mesg;
	var_t *v;
	bool hide_ptr;
	int oldstate, is_ptr_to_func;
	fval_t fields[FN_VAR_LAST + 1];

	dv = (dvar_t *)obj;
	v = dv->dv_var;

	hide_ptr = (v->va_flags & VA_HIDE_PTR) != 0;
	type = get_type_at_level(v, dv->dv_ilevel);

	switch(cmd) {
	case MR_VAR_STRING:
	case MR_VAR_SIGNED_DECIMAL:
	case MR_VAR_UNSIGNED_DECIMAL:
	case MR_VAR_SIGNED_OCTAL:
	case MR_VAR_UNSIGNED_OCTAL:
	case MR_VAR_SIGNED_HEX:
	case MR_VAR_UNSIGNED_HEX:
	case MR_VAR_UNSIGNED_BINARY:
	case MR_VAR_ASCII_BYTE:
		dv->dv_format = mval_to_vformat(cmd);
		oldstate = td_set_obj_updating(OBJ_UPDATING_OFF);
		redo_decl(dv);
		td_set_obj_updating(oldstate);
		break;
	case MR_VAR_DEREF:
		if (hide_ptr || !ISDERIV(type->ty_code)) {
			errf("Can't indirect through %s", v->va_name);
			break;
		}

		if (type->ty_code == DT_PTR_TO && type->ty_base->ty_code == DT_FUNC_RETURNING) {
			/*  Function address
			 */
			errf("Can't indirect through a pointer to a function");
			break;
		}

		if (type->ty_code == DT_PTR_TO && type->ty_base->ty_code == TY_VOID) {
			errf("Can't indirect through a void *");
			break;
		}

		if (type->ty_code == DT_PTR_TO &&
				(type->ty_base->ty_code == TY_U_STRUCT ||
				 type->ty_base->ty_code == TY_U_UNION)) {
			errf("Can't indirect through a pointer to an undefined type");
			break;
		}

		do {
			change_dv_level(dv, CL_UP);
			type = type->ty_base;
		} while (type->ty_code == DT_ARRAY_OF);


		dv->dv_format = default_format(v->va_type, type);
		oldstate = td_set_obj_updating(OBJ_UPDATING_OFF);
		redo_decl(dv);
		td_set_obj_updating(oldstate);
		break;
	case MR_VAR_ADDRESS:
		if (hide_ptr || dv->dv_ilevel == 0) {
			errf("Can't decrease the indirection level of %s",
								v->va_name);
			break;
		}

		do {
			change_dv_level(dv, CL_DOWN);
			type = get_type_at_level(v, dv->dv_ilevel);
		} while (type->ty_code == DT_ARRAY_OF && dv->dv_ilevel > 0);

		for (; type->ty_code == DT_ARRAY_OF; type = type->ty_base)
			change_dv_level(dv, CL_UP);

		dv->dv_format = default_format(v->va_type, type);
		oldstate = td_set_obj_updating(OBJ_UPDATING_OFF);
		redo_decl(dv);
		td_set_obj_updating(oldstate);

		break;
	case MR_VAR_EXPAND:
		is_ptr_to_func = FALSE;
		for (type = v->va_type; type != NULL; type = type->ty_base)
			if (type->ty_code == DT_FUNC_RETURNING)
				is_ptr_to_func = TRUE;

		if (is_ptr_to_func) {
			errf("Can't expand a pointer to a function");
			break;
		}
		type = deref_aggr(dv);

		switch(type->ty_code) {
		case TY_STRUCT:
		case TY_UNION:
			mesg = NULL;
			break;
		case TY_U_STRUCT:
			mesg = "Can't expand undefined structure %s";
			break;
		case TY_U_UNION:
			mesg = "Can't expand undefined union %s";
			break;
		default:
			mesg = "%s is not a structure or union";
			break;
		}
		if (mesg != NULL) {
			errf(mesg, v->va_name);
			break;
		}

		for (v = type->ty_aggr_or_enum->ae_aggr_members; v != NULL; v = v->va_next)
			if (find_var((objid_t)dv, v) == NULL)
				add_var_object((objid_t)dv, v, OBJ_FIRST_CHILD);
		break;
	case MR_VAR_DUP:
		dv2 = new_dvar();
		*dv2 = *dv;
		dv2->dv_ilist = dup_ilist(dv->dv_ilist);
		fields[FN_VAR_VALUE] = (fval_t) strsave(mkval(dv2));
		fields[FN_VAR_DECL] = (fval_t) strsave(mkdecl(dv2));
		fields[FN_VAR_LAST] = (fval_t) NULL;
		new_object((objid_t)dv2, OT_VAR, (objid_t)dv, OBJ_AFTER);
		set_all_fields((objid_t)dv2, fields, (fval_t)NULL);
		add_to_new_selection((objid_t)dv2);
		break;
	case MR_VAR_DELETE:
		remove_object((objid_t)dv, OBJ_DESCENDENTS);
		remove_object((objid_t)dv, OBJ_SELF);
		break;
	case MR_VAR_COLLAPSE:
		remove_object((objid_t)dv, OBJ_CHILDREN);
		break;
	case MR_VAR_COLLAPSE_COMPLETELY:
		remove_object((objid_t)dv, OBJ_DESCENDENTS);
		break;
	case MR_VAR_WANT_TYPEDEFS:
		dv->dv_flags &= ~DVF_NO_TYPEDEFS;
		redo_decl(dv);
		break;
	case MR_VAR_NO_TYPEDEFS:
		dv->dv_flags |= DVF_NO_TYPEDEFS;
		redo_decl(dv);
		break;
	default:
		panic("bad cmd in dv");
	}
}

void
update_struct(par, change_caused_by_edit)
objid_t par;
int change_caused_by_edit;
{
	objid_t obj;

	obj = get_code(par, OBJ_CHILD);
	for (; obj != NULL; obj = get_code(obj, OBJ_NEXT))
		update_var(obj, change_caused_by_edit);
}

/*  Update the value of the variable obj.  If it is a structure, update
 *  any displayed members.
 *
 *  The change_caused_by_edit argument is non zero if the reason the address
 *  has changed is simply that the user has changed the display. This happens,
 *  for example when the user edits the index in an array of structures.
 *  We normally flag variable values that have changed, but we don't want
 *  to flag ones that have changed simply because of a user edit.
 *
 *  We return 0 and use fval_t for the type of change_caused_by_edit as we
 *  are called from visit_objects().
 */
/* ARGSUSED */
void
update_var(obj, change_caused_by_edit)
objid_t obj;
int change_caused_by_edit;
{
	int decl_may_have_changed, decl_changed, val_changed;
	dvar_t *dv;
	typecode_t btype;

	dv = (dvar_t *) obj;

	decl_may_have_changed = FALSE;
	dv->dv_addr = get_dv_addr(get_code(obj, OBJ_PARENT),
				  dv->dv_var,
				  dv->dv_ilist,
				  &decl_may_have_changed);

	if (decl_may_have_changed)
		decl_changed = change_field(obj, FN_VAR_DECL, mkdecl(dv));
	else
		decl_changed = FALSE;
	val_changed = change_field(obj, FN_VAR_VALUE, mkval(dv));

	if ((decl_changed || val_changed) && !change_caused_by_edit)
		dv->dv_flags |= DVF_VAL_CHANGED;
	else
		dv->dv_flags &= ~DVF_VAL_CHANGED;

	btype = get_basetype(dv->dv_var->va_type)->ty_code;
	if (btype == TY_STRUCT || btype == TY_UNION)
		update_struct(obj, change_caused_by_edit);
}

/*  Update the values of the variables which are the children of par.
 *  See update_var for the meanings of the other arguments.
 */
void
update_vars_of(par, change_caused_by_edit)
objid_t par;
int change_caused_by_edit;
{
	objid_t obj;

	obj = get_code(par, OBJ_CHILD);
	for (; obj != NULL; obj = get_code(obj, OBJ_NEXT)) {
		switch(get_object_type(obj)) {
		case OT_EXPR:
			update_expr(obj, change_caused_by_edit);
			break;
		case OT_VAR:
			update_var(obj, change_caused_by_edit);
			break;
		case OT_BLOCK:
			update_vars_of(obj, change_caused_by_edit);
			break;
		}
	}
}
