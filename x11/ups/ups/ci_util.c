/* ci_util.c - utility routines for the C interpreter */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_ci_util_c_sccsid[] = "%W 26/7/92 (UKC)";

#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#include <setjmp.h>
#include <stdlib.h>
#include <string.h>
#include <local/ukcprog.h>

#include "ups.h"
#include "symtab.h"
#include "expr.h"
#include "ci.h"
#include "ci_parse.h"
#include "ci_expr.h"
#include "ci_constexpr.h"
#include "ci_util.h"

/*  Alloc pool identifier for parse tree allocation.
 *  Declared extern in ci_alloc.h.
 */
alloc_id_t Parse_alloc_id;

jmp_buf ci_Catch_panic_env;
bool ci_Catching_panics = FALSE;

static diag_handler_func_t Diag_handler_func;
static char *Diag_handler_arg;

#ifdef __STDC__
void
diagf(errtype_t errtype, lexinfo_t *lx, const char *fmt, ...)
{
#else /* !__STDC__ */
void
diagf(va_alist)
va_dcl
{
	errtype_t errtype;
	lexinfo_t *lx;
	char *fmt;
#endif /* !__STDC__ */
	va_list args;
	char buffer[150];
	char *s;

#ifdef __STDC__
	va_start(args, fmt);
#else
	va_start(args);
	errtype = va_arg(args, errtype_t);
	lx = va_arg(args, lexinfo_t *);
	fmt = va_arg(args, char *);
#endif

	s = formf(buffer, sizeof(buffer), fmt, args);

	va_end(args);

	if (Diag_handler_func == NULL)
		panic("no diag func");

	(*Diag_handler_func)(Diag_handler_arg, errtype, lx, s);

	if (s != buffer)
		free(s);
}

void
ci_set_diag_handler(func, arg)
diag_handler_func_t func;
char *arg;
{
	Diag_handler_func = func;
	Diag_handler_arg = arg;
}

void
ci_panic(s)
const char *s;
{
	if (ci_Catching_panics) {
		errf("Compiler error: %s (please tell mtr@ukc.ac.uk)", s);
		longjmp(ci_Catch_panic_env, 1);
	}
	else {
		panic(s);
	}
}

/*  Create an aggregate type for an undefined structure, union or enum.
 *  If type is NULL space is allocated.
 *
 *  The created aggregate has no members.
 */
type_t *
ci_make_undef_type(alloc_id, tag, typecode, type)
alloc_id_t alloc_id;
const char *tag;
typecode_t typecode;
type_t *type;
{
	aggr_or_enum_def_t *ae;

	switch(typecode) {
	case TY_U_STRUCT:
	case TY_U_UNION:
		ae = ci_make_aggr_or_enum_def(alloc_id, tag, typecode, type);
		ae->ae_aggr_members = NULL;
		type = ae->ae_type;
		break;
	case TY_U_ENUM:
		ae = ci_make_aggr_or_enum_def(alloc_id, tag, typecode, type);
		ae->ae_enum_members = NULL;
		type = ae->ae_type;
		break;
	case TY_INT_ASSUMED:
		if (type == NULL)
			type = ci_code_to_type(typecode);
		else
			*type = *ci_code_to_type(typecode);
		break;
	default:
		ci_panic("unknown typecode in mut");
	}
	return type;
}

const char *
ci_exec_result_to_string(res)
ci_exec_result_t res;
{
	switch (res) {
		case CI_ER_CONTINUE:		return "continue";
		case CI_ER_TRAP:		return "trap";
		case CI_ER_READDATA_FAILED:	return "data read failed";
		case CI_ER_WRITEDATA_FAILED:	return "data write failed";
		case CI_ER_INDIRECT_CALL_FAILED:return "indirect call failed";
		case CI_ER_STACK_OVERFLOW:	return "stack overflow";
		case CI_ER_DIVISION_BY_ZERO:	return "division by zero";
		case CI_ER_ARITHMETIC_EXCEPTION:return "arithmetic exception";
		case CI_ER_BAD_MA_COUNT:	return "bad ->[] index";
		case CI_ER_USER1:		return "user1";
		case CI_ER_USER2:		return "user2";
		case CI_ER_USER3:		return "user3";
		default:			return "unknown result";
	}
}

aggr_or_enum_def_t *
ci_make_aggr_or_enum_def(alloc_id, tag, typecode, type)
alloc_id_t alloc_id;
const char *tag;
typecode_t typecode;
type_t *type;
{
	aggr_or_enum_def_t *ae;

	ae = (aggr_or_enum_def_t *)alloc(alloc_id, sizeof(aggr_or_enum_def_t));

	if (type != NULL)
		ci_init_type(type, typecode);
	else
		type = ci_make_type(alloc_id, typecode);
	type->ty_aggr_or_enum = ae;

	ae->ae_tag = tag;
	ae->ae_is_complete = AE_INCOMPLETE;
	ae->ae_size = -1;
	/* We leave ae_alignment uninitialised */
	ae->ae_type = type;
	ae->ae_next = NULL;
	/* We leave ae_aggr_members/ae_enum_members uninitialised. */
	ae->ae_lexinfo = NULL;

	return ae;
}

aggr_or_enum_def_t *
ci_apply_to_aelist(aelist, func, arg)
aggr_or_enum_def_t *aelist;
aggr_or_enum_def_t *(*func)PROTO((aggr_or_enum_def_t *ae, const char *farg));
const char *arg;
{
	aggr_or_enum_def_t *ae, *res;

	for (ae = aelist, res = NULL; ae != NULL && res == NULL; ae = ae->ae_next) {
		if (ae->ae_type != NULL)
			res = (*func)(ae, arg);
		else
			res = ci_apply_to_aelist(ae->ae_sublist, func, arg);
	}

	return res;
}

block_t *
ci_make_block(alloc_id, parent)
alloc_id_t alloc_id;
block_t *parent;
{
	block_t *bl = (block_t *)alloc(alloc_id, sizeof(block_t));

	bl->bl_start_lnum = 0;
	bl->bl_vars = NULL;
	bl->bl_typedefs = NULL;
	bl->bl_aggr_or_enum_defs = NULL;
	bl->bl_initlist = NULL;
	bl->bl_parent = parent;
	bl->bl_blocks = NULL;
	bl->bl_next = NULL;
	return bl;
}

func_t *
ci_make_func(alloc_id, name, addr, symtab_id, fil, next, fl)
alloc_id_t alloc_id;
const char *name;
taddr_t addr;
symtab_id_t symtab_id;
fil_t *fil;
func_t *next;
funclist_t *fl;
{
	func_t *f;

	f = (func_t *) alloc(alloc_id, sizeof(func_t));

	f->fu_flags = 0;
	f->fu_name = name;
	f->fu_type = NULL;
	f->fu_lexinfo = NULL;
	f->fu_addr = addr;
	f->fu_fil = fil;
	f->fu_language = (fil != NULL) ? fil->fi_language : LANG_UNKNOWN;
	f->fu_symtab_id = symtab_id;
	f->fu_next = next;
	f->fu_fl = fl;
	f->fu_statement_id = NULL;
	f->fu__blocks = NULL;
	f->fu__lnos = NULL;

	return f;
}

var_t *
ci_make_var(alloc_id, name, class, type, addr)
alloc_id_t alloc_id;
const char *name;
class_t class;
type_t *type;
taddr_t addr;
{
	var_t *v;

	v = (var_t *)alloc(alloc_id, sizeof(var_t));
	v->va_name = name;
	v->va_class = class;
	v->va_language = LANG_C;
	v->va_flags = 0;
	v->va_type = type;
	v->va_addr = addr;
	v->va_next = NULL;
	v->va_lexinfo = NULL;
	return v;
}

initlist_t *
ci_push_initlist(newlist, list)
initlist_t *newlist, *list;
{
	initlist_t *next;

	for (; newlist != NULL; newlist = next) {
		next = newlist->il_next;
		newlist->il_next = list;
		list = newlist;
	}
	return list;
}

type_t *
ci_push_types(typelist, types)
type_t *typelist, *types;
{
	type_t *base;

	for (; types != NULL; types = base) {
		base = types->ty_base;
		types->ty_base = typelist;
		typelist = types;
	}
	return typelist;
}

identifier_list_t *
ci_reverse_idlist(idl)
identifier_list_t *idl;
{
	identifier_list_t *list, *next;

	list = NULL;
	for (; idl != NULL; idl = next) {
		next = idl->idl_next;
		idl->idl_next = list;
		list = idl;
	}
	return list;
}

expr_list_t *
ci_reverse_expr_list(el)
expr_list_t *el;
{
	expr_list_t *list, *next;

	list = NULL;
	for (; el != NULL; el = next) {
		next = el->el_next;
		el->el_next = list;
		list = el;
	}
	return list;
}

/*  Construct a bitfield type given an offset and a width.
 */
type_t *
ci_make_bitfield_type(alloc_id, typecode, bit_offset, bit_width)
alloc_id_t alloc_id;
typecode_t typecode;
int bit_offset, bit_width;
{
	bitfield_t *bf;
	type_t *type;

	bf = (bitfield_t *) alloc(alloc_id, sizeof(bitfield_t));
	bf->bf_code = typecode;
	bf->bf_offset = bit_offset;
	bf->bf_width = bit_width;
	bf->bf_expr_id = NULL;

	type = ci_make_type(alloc_id, TY_BITFIELD);
	type->ty_bitfield = bf;

	return type;
}

type_t *
ci_make_expr_bitfield_type(base, expr)
type_t *base;
expr_t *expr;
{
	type_t *type;
	constval_t val;

	ci_evaluate_constant_expression(expr, "bitfield width", TRUE, &val);
	type = ci_make_bitfield_type(Parse_alloc_id, TY_INT, 0, val.cv_int);
	type->ty_base = base;
	type->ty_bitfield->bf_expr_id = (expr_id_t)expr;
	return type;
}

type_t *
ci_make_pointer(base, qualifiers)
type_t *base;
qualifiers_t qualifiers;
{
	type_t *type;

	type = ci_make_type(Parse_alloc_id, DT_PTR_TO);
	type->ty_qualifiers = qualifiers;
	type->ty_base = base;
	return type;
}

var_t *
ci_push_vars(v, list)
var_t *v, *list;
{
	var_t *next;

	for (; v != NULL; v = next) {
		next = v->va_next;
		v->va_next = list;
		list = v;
	}
	return list;
}

type_t *
ci_make_array_type(base, expr)
type_t *base;
expr_t *expr;
{
	type_t *type;

	type = ci_make_type(Parse_alloc_id, DT_ARRAY_OF);
	type->ty_base = base;
	type->ty_dim = NEW(dim_t);
	type->ty_dim->di_ldynamic = FALSE;
	type->ty_dim->di_hdynamic = expr == NULL;
	type->ty_dim->di_low = 0;
	type->ty_dim->di_type = ci_code_to_type(TY_INT);
	if (expr != NULL) {
		constval_t val;

		if (ci_evaluate_constant_expression(expr, "array size",
								      TRUE, &val)) {
			if (val.cv_int <= 0)
				diagf(ET_ERROR, expr->ex_lexinfo,
					"Illegal %s array dimension %d",
					(val.cv_int == 0) ? "zero"
							  : "negative",
					val.cv_int);
			type->ty_dim->di_high = val.cv_int;
		}
	}
	return type;
}

enum_member_t *
ci_build_enum_member(id, expr)
identifier_t *id;
expr_t *expr;
{
	enum_member_t *em;
	constval_t val;

	if (expr != NULL)
		ci_evaluate_constant_expression(expr, "enum constant value", TRUE,
									      &val);
	else
		val.cv_int = 0;
	em = ci_make_enum_member(Parse_alloc_id, id->id_name, val.cv_int);
	em->em_expr_id = (expr_id_t)expr;
	em->em_lexinfo = id->id_lexinfo;
	return em;
}

enum_member_t *
ci_make_enum_member(alloc_id, name, val)
alloc_id_t alloc_id;
const char *name;
long val;
{
	enum_member_t *em;

	em = (enum_member_t *)alloc(alloc_id, sizeof(enum_member_t));
	em->em_expr_id = NULL;
	em->em_name = name;
	em->em_val = val;
	em->em_lexinfo = NULL;
	return em;
}

declaration_t *
ci_make_declaration(class, basetype, qualifiers, declarators)
class_t class;
type_t *basetype;
qualifiers_t qualifiers;
declarator_t *declarators;
{
	declaration_t *dn = NEW(declaration_t);

	dn->dn_class = class;
	dn->dn_basetype = basetype;
	dn->dn_qualifiers = qualifiers;
	dn->dn_declarators = declarators;
	dn->dn_next = NULL;

	return dn;
}

declarator_t *
ci_make_declarator(type)
type_t *type;
{
	declarator_t *dr = NEW(declarator_t);

	dr->dr_type = type;
	dr->dr_initialiser = NULL;
	dr->dr_next = NULL;

	return dr;
}

initexpr_t *
ci_make_initexpr(is_list, expr, list)
bool is_list;
expr_t *expr;
initexpr_t *list;
{
	initexpr_t *in = NEW(initexpr_t);

	in->ie_is_list = is_list;
	if (is_list) {
		initexpr_t *newlist, *next;

		for (newlist = NULL; list != NULL; list = next) {
			next = list->ie_next;
			list->ie_next = newlist;
			newlist = list;
		}
		in->ie_list = newlist;
	}
	else
		in->ie_expr = expr;
	in->ie_next = NULL;

	return in;
}

type_t *
ci_code_to_type(typecode)
typecode_t typecode;
{
	static type_t typetab[TY_MAXTYPE];

	if ((int)typecode < 0 || (int)typecode >= (int)TY_MAXTYPE)
		ci_panic("bad typecode in ctt");

	if (typetab[(int)typecode].ty_name == NULL) {
		const char *name;
		typecode_t typetab_code;

		typetab_code = typecode;
		switch(typecode) {
			case TY_VOID:	name = "void";			break;
			case TY_CHAR:	name = "char";			break;
			case TY_UCHAR:	name = "unsigned char";		break;
			case TY_SHORT:	name = "short";			break;
			case TY_USHORT:	name = "unsigned short";	break;
			case TY_INT:	name = "int";			break;
			case TY_UINT:	name = "unsigned int";		break;
			case TY_LONG:	name = "long";			break;
			case TY_ULONG:	name = "unsigned long";		break;
			case TY_FLOAT:	name = "float";			break;
			case TY_DOUBLE:	name = "double";		break;
			case TY_INT_ASSUMED:
				name = "(int assumed)";
				typetab_code = TY_INT;
				break;
			case TY_ELLIPSIS:
			case TY_SIGNED:
			case TY_UNSIGNED:
				name = "<internal type>";
				break;
			default:
				ci_panic("bad typecode in ctt");
				name = 0; /* to satisfy gcc */
				break;
		
		}
		typetab[(int)typecode].ty_code = typetab_code;
		typetab[(int)typecode].ty_size = -1;
		typetab[(int)typecode].ty_base = NULL;
		typetab[(int)typecode].ty_typedef = NULL;
		typetab[(int)typecode].ty_name = name;
	}
	return &typetab[(int)typecode];
}

type_t *
ci_make_type(alloc_id, typecode)
alloc_id_t alloc_id;
typecode_t typecode;
{
	type_t *type;

	type = (type_t *)alloc(alloc_id, sizeof(type_t));
	ci_init_type(type, typecode);
	return type;
}

void
ci_init_type(type, typecode)
type_t *type;
typecode_t typecode;
{
	type->ty_code = typecode;
	type->ty_size = -1;	/* i.e., not yet set */
	type->ty_base = NULL;
	type->ty_typedef = NULL;
}

const char *
ci_lexinfo_to_string(lx, prev_lx)
lexinfo_t *lx, *prev_lx;
{
	static char buf[512];

	if (strcmp(prev_lx->lx_filename, lx->lx_filename) == 0)
		strnf(buf, sizeof(buf), "line %d", lx->lx_lnum);
	else
		strnf(buf, sizeof(buf), "line %d of %s", 
						lx->lx_lnum, lx->lx_filename);
	return buf;
}
