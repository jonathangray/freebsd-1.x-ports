/* ci_expr.c - routines used by ci_parse.y to build expression parse trees */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_ci_expr_c_sccsid[] = "@(#)ci_expr.c	1.18 26/7/92 (UKC)";

#include <stdlib.h>
#include <string.h>

#include <local/ukcprog.h>

#include "ups.h"
#include "symtab.h"
#include "ci.h"
#include "ci_parse.h"
#include "ci_util.h"
#include "ci_expr.h"
#include "ci_func.h"
#include "ci_types.h"
#include "ci_decl.h"
#include "ci_showexpr.h"
#include "ci_constexpr.h"

static bool int_is_bigger_than_char PROTO((void));
static bool int_is_bigger_than_short PROTO((void));
static bool long_is_bigger_than_unsigned PROTO((void));

static bool var_is_lvalue PROTO((var_t *v));
static void make_implicit_func_declaration PROTO((expr_t *func_expr));

static type_t *do_usual_arithmetic_conversions PROTO((expr_t *left, expr_t *right,
								const char *opname));
static bool is_ptr_to_void PROTO((type_t *type));
static bool is_null_pointer PROTO((expr_t *expr));
static expr_t *make_unary_expr PROTO((optype_t op, expr_t *expr, type_t *type,
					bool is_lvalue));

static void complain PROTO((expr_t *expr, const char *which_operand,
					const char *opname, const char *mesg));
static bool complain_if_not_integral PROTO((expr_t *expr,
					    const char *which_operand,
					    const char *opname));
static bool complain_if_not_arithmetic PROTO((expr_t *expr,
					      const char *which_operand,
					      const char *opname));
static bool complain_if_not_arithmetic_or_ptr PROTO((expr_t *expr,
						     const char *which_operand,
						     const char *opname));
static expr_t *make_binary_expr PROTO((optype_t op, expr_t *left,
						expr_t *right, type_t *type));
static expr_t *make_scale_expr PROTO((expr_t *expr, optype_t op, type_t *type));
static optype_t typecode_to_cvt_op PROTO((typecode_t typecode));

#define IS_PTR(code)	(((code) == DT_PTR_TO) || ((code) == DT_ARRAY_OF))

static bool
int_is_bigger_than_char()
{
	return sizeof(int) > sizeof(char);
}

static bool
int_is_bigger_than_short()
{
	return sizeof(int) > sizeof(short);
}

static bool
long_is_bigger_than_unsigned()
{
	return sizeof(long) > sizeof(unsigned);
}

static expr_t *
make_unary_expr(op, expr, type, is_lvalue)
optype_t op;
expr_t *expr;
type_t *type;
bool is_lvalue;
{
	expr_t *ex = NEW(expr_t);

	ex->ex_exprtype = ET_UNARY;
	ex->ex_type = type;
	ex->ex_is_lvalue = is_lvalue;
	ex->ex_is_constant = op != OP_DEREF && expr->ex_is_constant;
	ex->ex_lexinfo = expr->ex_lexinfo;

	ex->ex_unary_expr = NEW(unary_expr_t);
	ex->ex_unary_expr->ue_op = op;
	ex->ex_unary_expr->ue_expr = expr;
	return ex;
}

static expr_t *
make_binary_expr(op, left, right, type)
optype_t op;
expr_t *left;
expr_t *right;
type_t *type;
{
	expr_t *ex = NEW(expr_t);

	ex->ex_exprtype = ET_BINARY;
	ex->ex_type = type;
	ex->ex_is_lvalue = FALSE;
	ex->ex_is_constant = left->ex_is_constant && right->ex_is_constant;
	ex->ex_lexinfo = left->ex_lexinfo;

	ex->ex_binary_expr = NEW(binary_expr_t);
	ex->ex_binary_expr->be_left = left;
	ex->ex_binary_expr->be_op = op;
	ex->ex_binary_expr->be_right = right;
	return ex;
}

expr_t *
ci_make_comma_expr(left, right)
expr_t *left, *right;
{
	return make_binary_expr(OP_COMMA, left, right, right->ex_type);
}

static optype_t
typecode_to_cvt_op(typecode)
typecode_t typecode;
{
	switch (typecode) {
		case TY_CHAR:	return OP_CVT_TO_CHAR;
		case TY_UCHAR:	return OP_CVT_TO_UCHAR;
		case TY_SHORT:	return OP_CVT_TO_SHORT;
		case TY_USHORT:	return OP_CVT_TO_USHORT;
		case TY_INT:	return OP_CVT_TO_INT;
		case TY_U_ENUM:	return OP_CVT_TO_INT;
		case TY_ENUM:	return OP_CVT_TO_INT;
		case TY_UINT:	return OP_CVT_TO_UINT;
		case TY_LONG:	return OP_CVT_TO_LONG;
		case TY_ULONG:	return OP_CVT_TO_ULONG;
		case TY_FLOAT:	return OP_CVT_TO_FLOAT;
		case TY_VOID:	return OP_CVT_TO_VOID;
		case TY_DOUBLE:	return OP_CVT_TO_DOUBLE;
		case DT_PTR_TO:	return OP_CVT_TO_PTR;
		default:
			ci_panic("bad type in pc");
			return OP_LAST; /* to satisfy gcc */
	}
}

type_t *
ci_push_conversion(expr, typecode, exprtype)
expr_t *expr;
typecode_t typecode;
expr_type_t exprtype;
{

	if (expr->ex_type->ty_code != typecode) {
		expr_t *new = NEW(expr_t);
		*new = *expr;

		expr->ex_exprtype = exprtype;
		expr->ex_type = ci_code_to_type(typecode);
		expr->ex_lexinfo = new->ex_lexinfo;
		expr->ex_is_lvalue = FALSE;
		expr->ex_is_constant = new->ex_is_constant;

		expr->ex_unary_expr = NEW(unary_expr_t);
		expr->ex_unary_expr->ue_op = typecode_to_cvt_op(typecode);
		expr->ex_unary_expr->ue_expr = new;
	}
	
	return expr->ex_type;
}

/*  This used also to convert "array of foo" to "pointer to foo".
 */
void
ci_do_pointer_generation(expr)
expr_t *expr;
{
	if (expr->ex_type != NULL && expr->ex_type->ty_code == DT_FUNC_RETURNING) {
		expr_t *new;

		new = NEW(expr_t);
		*new = *expr;

		expr->ex_exprtype = ET_PROMOTION;
		expr->ex_type = ci_make_pointer(expr->ex_type, 0);
		expr->ex_lexinfo = new->ex_lexinfo;
		expr->ex_is_lvalue = FALSE;
		expr->ex_is_constant = FALSE;

		expr->ex_unary_expr = NEW(unary_expr_t);
		expr->ex_unary_expr->ue_op = OP_CVT_TO_PTR;
		expr->ex_unary_expr->ue_expr = new;
	}
}

static type_t *
do_usual_arithmetic_conversions(left, right, opname)
expr_t *left, *right;
const char *opname;
{
	bool leftbad, rightbad;
	typecode_t leftcode, rightcode;

	leftbad = complain_if_not_arithmetic(left, "Left ", opname);
	rightbad = complain_if_not_arithmetic(right, "Right ", opname);
	if (leftbad || rightbad)
		return NULL;

	ci_do_integral_promotions(left);
	ci_do_integral_promotions(right);
	leftcode = left->ex_type->ty_code;
	rightcode = right->ex_type->ty_code;

	/* TODO: check for long double. */

	if (leftcode == TY_DOUBLE || rightcode == TY_DOUBLE) {
		(void) ci_push_conversion(left, TY_DOUBLE, ET_PROMOTION);
		return ci_push_conversion(right, TY_DOUBLE, ET_PROMOTION);
	}
	if (leftcode == TY_FLOAT || rightcode == TY_FLOAT) {
		(void) ci_push_conversion(left, TY_FLOAT, ET_PROMOTION);
		return ci_push_conversion(right, TY_FLOAT, ET_PROMOTION);
	}
	
	if (leftcode == TY_ULONG || rightcode == TY_ULONG) {
		(void) ci_push_conversion(left, TY_ULONG, ET_PROMOTION);
		return ci_push_conversion(right, TY_ULONG, ET_PROMOTION);
	}

	if ((leftcode == TY_LONG && rightcode == TY_UINT) ||
	    (leftcode == TY_UINT && rightcode == TY_LONG)) {
		if (long_is_bigger_than_unsigned()) {
			(void) ci_push_conversion(left, TY_LONG, ET_PROMOTION);
			return ci_push_conversion(right, TY_LONG, ET_PROMOTION);
		}
		else {
			(void) ci_push_conversion(left, TY_ULONG, ET_PROMOTION);
			return ci_push_conversion(right, TY_ULONG, ET_PROMOTION);
		}
	}

	if (leftcode == TY_LONG || rightcode == TY_LONG) {
		(void) ci_push_conversion(left, TY_LONG, ET_PROMOTION);
		return ci_push_conversion(right, TY_LONG, ET_PROMOTION);
	}
	if (leftcode == TY_UINT || rightcode == TY_UINT) {
		(void) ci_push_conversion(left, TY_UINT, ET_PROMOTION);
		return ci_push_conversion(right, TY_UINT, ET_PROMOTION);
	}

	/*  If we get here, the types should both be int.
	 */
	if (leftcode != TY_INT || rightcode != TY_INT)
		ci_panic("type botch in do_usual_arithmetic_conversions");
	
	return left->ex_type;
}

void
ci_do_old_style_argument_promotions(expr)
expr_t *expr;
{
	if (expr->ex_type != NULL) {
		ci_do_integral_promotions(expr);
		if (expr->ex_type->ty_code == TY_FLOAT)
			ci_push_conversion(expr, TY_DOUBLE, ET_PROMOTION);
	}
}


static bool
complain_if_not_integral(expr, which_operand, opname)
expr_t *expr;
const char *which_operand, *opname;
{
	if (expr->ex_type == NULL)
		return TRUE;
	if (!ci_is_integral(expr->ex_type->ty_code)) {
		complain(expr, which_operand, opname, "is not of integral type");
		return TRUE;
	}
	return FALSE;
}

static bool
complain_if_not_arithmetic(expr, which_operand, opname)
expr_t *expr;
const char *which_operand, *opname;
{
	if (expr->ex_type == NULL)
		return TRUE;
	if (!IS_ARITHMETIC_TYPE(expr->ex_type->ty_code)) {
		complain(expr, which_operand, opname, "is not of arithmetic type");
		return TRUE;
	}
	return FALSE;
}

static bool
complain_if_not_arithmetic_or_ptr(expr, which_operand, opname)
expr_t *expr;
const char *which_operand, *opname;
{
	if (expr->ex_type == NULL)
		return TRUE;
	if (!IS_ARITHMETIC_OR_PTR_TYPE(expr->ex_type->ty_code)) {
		complain(expr, which_operand, opname,
					"is not of arithmetic type or a pointer");
		return TRUE;
	}
	return FALSE;
}

static void
complain(expr, which_operand, opname, mesg)
expr_t *expr;
const char *which_operand, *opname, *mesg;
{
	diagf(ET_ERROR, expr->ex_lexinfo, "%soperand of %s %s",
						which_operand, opname, mesg);
	ci_show_expr_and_type(expr, expr->ex_type);
}

static bool
var_is_lvalue(v)
var_t *v;
{
	typecode_t code;

	code = v->va_type->ty_code;
	return code != DT_FUNC_RETURNING && code != DT_ARRAY_OF;
}

static bool
is_null_pointer(expr)
expr_t *expr;
{
	constval_t val;

	if (expr->ex_type != NULL && ci_is_integral(expr->ex_type->ty_code) &&
							 expr->ex_is_constant) {
		ci_evaluate_constant_expression(expr, "<null>", TRUE, &val);
		return val.cv_int == 0;
	}
	return FALSE;
}

static bool
is_ptr_to_void(type)
type_t *type;
{
	return type->ty_code == DT_PTR_TO && type->ty_base->ty_code == TY_VOID;
}

/*  Check that expr is of the right type to assign to t1.  Say OK if
 *  either type is NULL, as we've already given an error.  If the
 *  types are compatible but different, push a conversion onto
 *  the expression.  Return 0 for OK, -1 otherwise.
 */
int
ci_do_assignment_conversions(t1, expr)
type_t *t1;
expr_t *expr;
{
	typecode_t code1, code2;
	type_t *t2;

	t2 = expr->ex_type;

	if (t1 == NULL || t2 == NULL)
		return 0;

	code1 = t1->ty_code;
	code2 = t2->ty_code;

	if (IS_ARITHMETIC_TYPE(code1) && IS_ARITHMETIC_TYPE(code2)) {
		if (t1->ty_code == TY_BITFIELD)
			t1 = ci_code_to_type(TY_INT);
		if (!ci_types_same(t1, t2))
			ci_push_conversion(expr, code1, ET_ASSIGNMENT_CONVERSION);
		return 0;
	}

	if ((IS_PTR(code1) && (is_ptr_to_void(t2) || is_null_pointer(expr))) ||
	    (IS_PTR(code2) && is_ptr_to_void(t1)))
		return 0;

	if (code1 == DT_PTR_TO && code2 == DT_ARRAY_OF)
		return ci_types_same(t1->ty_base, t2->ty_base) ? 0 : -1;

	return ci_types_same(t1, t2) ? 0 : -1;
}
		
/* TODO: various integer types for constants.
 */
expr_t *
ci_make_integer_constant_expr(exprtype, lx, ival)
expr_type_t exprtype;
lexinfo_t *lx;
long ival;
{
	expr_t *ex = NEW(expr_t);

	ex->ex_exprtype = exprtype;
	ex->ex_type = ci_code_to_type(TY_INT);
	ex->ex_lexinfo = lx;
	ex->ex_is_lvalue = FALSE;
	ex->ex_is_constant = TRUE;
	ex->ex_integer_constant_val = ival;
	return ex;
}

expr_t *
ci_make_floating_constant_expr(lx, fval)
lexinfo_t *lx;
double fval;
{
	expr_t *ex = NEW(expr_t);

	ex->ex_exprtype = ET_FLOATING_CONST;
	ex->ex_type = ci_code_to_type(TY_DOUBLE);
	ex->ex_lexinfo = lx;
	ex->ex_is_lvalue = FALSE;
	ex->ex_is_constant = TRUE;
	ex->ex_floating_constant_val = fval;
	return ex;
}

expr_t *
ci_make_string_constant_expr(lx, sc)
lexinfo_t *lx;
string_const_t *sc;
{
	expr_t *ex = NEW(expr_t);

	ex->ex_exprtype = ET_STRING_CONST;
	ex->ex_type = ci_make_array_type(ci_code_to_type(TY_CHAR), (expr_t *)NULL);
	ex->ex_type->ty_dim->di_high = sc->sc_size;
	ex->ex_type->ty_dim->di_hdynamic = FALSE;
	ex->ex_lexinfo = lx;
	ex->ex_is_lvalue = FALSE;
	ex->ex_is_constant = FALSE;
	ex->ex_string_constant_val = NEW(string_const_t);
	*ex->ex_string_constant_val = *sc;

	return ex;
}

expr_t *
ci_make_identifier_expr(id)
identifier_t *id;
{
	expr_t *ex = NEW(expr_t);
	expr_type_t exprtype;
	type_t *type;
	bool is_lvalue, is_constant;
	namedesc_t *nd;

	if ((nd = ci_lookup_name(id->id_name)) == NULL) {
		if (!id->id_lparen_follows)
			diagf(ET_ERROR, id->id_lexinfo,
			      "Undefined variable %s", id->id_name);
		exprtype = ET_UNDEF_VAR;
		ex->ex_undef_name = id->id_name;
		type = NULL;
		is_lvalue = FALSE;
		is_constant = FALSE;
	}
	else {
		switch (nd->nd_nametype) {
		case NT_VARNAME:
		case NT_FUNCNAME:
			exprtype = (nd->nd_nametype == NT_VARNAME) ? ET_VAR
								   : ET_FUNCNAME;
			type = nd->nd_var->va_type;
			is_lvalue = var_is_lvalue(nd->nd_var);
			is_constant = FALSE;
			ex->ex_var = nd->nd_var;
			break;
		case NT_ENUM_CONST:
			exprtype = ET_ENUM_CONST;
			type = nd->nd_enum_member->em_enum->ae_type;
			is_lvalue = FALSE;
			is_constant = TRUE;
			ex->ex_enum_member = nd->nd_enum_member;
			break;
		default:
			ci_panic("unknown name type in mie");
			type = NULL;		/* this and below for gcc */
			is_lvalue = FALSE;
			is_constant = FALSE;
			exprtype = ET_VAR;
		}
	}

	ex->ex_exprtype = exprtype;
	ex->ex_type = type;
	ex->ex_is_lvalue = is_lvalue;
	ex->ex_is_constant = is_constant;
	ex->ex_lexinfo = id->id_lexinfo;

	return ex;
}

expr_t *
ci_make_multi_arrow_expr(aggr_expr, index_expr, id)
expr_t *aggr_expr, *index_expr;
identifier_t *id;
{
	expr_t *ex = NEW(expr_t);
	var_t *v;

	if (aggr_expr->ex_type == NULL) {
		v = NULL;
	}
	else if (aggr_expr->ex_type->ty_code != DT_PTR_TO ||
		 aggr_expr->ex_type->ty_base->ty_code != TY_STRUCT) {
		complain(aggr_expr, "", "->[]",
					"is not a pointer to a structure");
		v = NULL;
	}
	else {
		type_t *type;

		type = aggr_expr->ex_type->ty_base;

		v = type->ty_aggr_or_enum->ae_aggr_members;
		for (; v != NULL; v = v->va_next)
			if (strcmp(v->va_name, id->id_name) == 0)
				break;

		if (v == NULL) {
			diagf(ET_ERROR, id->id_lexinfo,
			      "%s is not a member of %s", id->id_name,
			      ci_basetype_name(type));
		}
		else if (!ci_types_same(v->va_type, aggr_expr->ex_type)) {
			diagf(ET_ERROR, id->id_lexinfo,
			      "type clash in operands of ->[]");
			ci_show_type(v->va_type, "LHS");
			ci_show_type(aggr_expr->ex_type, "RHS");
		}
	}

	if (complain_if_not_integral(index_expr, "index", "->[]")) {
		v = NULL;
	}

	ex->ex_multi_arrow_expr = NEW(multi_arrow_expr_t);
	ex->ex_multi_arrow_expr->ma_aggr = aggr_expr;
	ex->ex_multi_arrow_expr->ma_index = index_expr;
	ex->ex_multi_arrow_expr->ma_member = v;

	ex->ex_exprtype = ET_MULTI_ARROW;
	ex->ex_type = (v != NULL) ? v->va_type : NULL;
	ex->ex_is_lvalue = FALSE;
	ex->ex_is_constant = FALSE;
	ex->ex_lexinfo = id->id_lexinfo;

	return ex;
}

expr_t *
ci_make_dot_expr(aggr_expr, id, opname)
expr_t *aggr_expr;
identifier_t *id;
const char *opname;
{
	expr_t *ex = NEW(expr_t);
	var_t *v;

	v = NULL;

	if (aggr_expr->ex_type != NULL) {
		typecode_t code;

		code = aggr_expr->ex_type->ty_code;
		if (code == TY_STRUCT || code == TY_UNION) {
			v = aggr_expr->ex_type->ty_aggr_or_enum->ae_aggr_members;
			for (; v != NULL; v = v->va_next)
				if (strcmp(v->va_name, id->id_name) == 0)
					break;
			if (v == NULL)
				diagf(ET_ERROR, id->id_lexinfo,
				      "%s is not a member of %s", id->id_name,
				      ci_basetype_name(aggr_expr->ex_type));
		}
		else {
			if (code == TY_U_STRUCT || code == TY_U_UNION) {
				diagf(ET_ERROR, id->id_lexinfo,
				      "Request for member (%s) of %s",
					id->id_name,
					ci_basetype_name(aggr_expr->ex_type));
			}
			else {
				diagf(ET_ERROR, id->id_lexinfo,
				      "%s used on non struct or union", opname);
				ci_show_expr_and_type(aggr_expr, aggr_expr->ex_type);
			}
		}

	}

	ex->ex_dot_expr = NEW(dot_expr_t);
	ex->ex_dot_expr->de_aggr = aggr_expr;
	ex->ex_dot_expr->de_member = v;

	ex->ex_exprtype = ET_DOT;
	ex->ex_type = (v != NULL) ? v->va_type : NULL;
	ex->ex_is_lvalue = aggr_expr->ex_is_lvalue;
	ex->ex_is_constant = FALSE;
	ex->ex_lexinfo = id->id_lexinfo;

	return ex;
}

expr_t *
ci_make_address_of_expr(expr)
expr_t *expr;
{
	type_t *type;

	if (expr->ex_type != NULL) {
		typecode_t code = expr->ex_type->ty_code;

		if (code == TY_BITFIELD) {
			diagf(ET_ERROR, expr->ex_lexinfo,
					"Can't take the address of a bitfield");
			ci_show_expr_and_type(expr, expr->ex_type);
			type = NULL;
		}
		else if (!expr->ex_is_lvalue && code != DT_FUNC_RETURNING
					     && code != DT_ARRAY_OF) {
			complain(expr, "", "&", "is not an lvalue");
			type = NULL;
		}
		else
			type = ci_make_pointer(expr->ex_type, 0);
	}
	else
		type = NULL;

	return make_unary_expr(OP_ADDRESS_OF, expr, type, FALSE);
}

expr_t *
ci_make_deref_expr(expr)
expr_t *expr;
{
	type_t *type;
	bool is_lvalue;

	ci_do_pointer_generation(expr);

	type = expr->ex_type;
	if (type == NULL) {
		is_lvalue = FALSE;
	}
	else if (!IS_PTR(type->ty_code)) {
		type = NULL;
		is_lvalue = FALSE;
		complain(expr, "", "*", "is not a pointer or array");
	}
	else if (type->ty_base->ty_code == TY_VOID) {
		type = NULL;
		is_lvalue = FALSE;
		diagf(ET_ERROR, expr->ex_lexinfo,
			            "Illegal dereference of pointer to void");
		ci_show_expr_and_type(expr, expr->ex_type);
	}
	else {
		typecode_t code;

		type = type->ty_base;

		code = type->ty_code;
		is_lvalue = IS_ARITHMETIC_OR_PTR_TYPE(code) ||
						 code == TY_STRUCT ||
						 code == TY_UNION ||
						 code == TY_ENUM;
	}
		
	return make_unary_expr(OP_DEREF, expr, type, is_lvalue);
}

expr_t *
ci_make_inc_or_dec_expr(op, expr, opname)
optype_t op;
expr_t *expr;
const char *opname;
{
	type_t *type;

	type = expr->ex_type;
	if (type != NULL) {
		if (complain_if_not_arithmetic_or_ptr(expr, "", opname))
			type = NULL;
		else if (!expr->ex_is_lvalue) {
			complain(expr, "", opname, "is not an lvalue");
			type = NULL;
		}

		/* BUG: can't face doing f++ or d++ in the code generator right now.
		 */
		if (type != NULL && (type->ty_code == TY_FLOAT ||
							type->ty_code == TY_DOUBLE))
			diagf(ET_ERROR, expr->ex_lexinfo,
		"Sorry, ++ and -- on floating expressions not yet implemented");
							
	}

	return make_unary_expr(op, expr, type, FALSE);
}

expr_t *
ci_make_unary_plus_expr(expr)
expr_t *expr;
{
	type_t *type;
	expr_t *new;

	type = expr->ex_type;
	if (type != NULL && complain_if_not_arithmetic_or_ptr(expr, "", "unary +"))
		type = NULL;
			
	new = make_unary_expr(OP_UNARY_PLUS, expr, type, FALSE);
	ci_do_integral_promotions(new);
	return new;
}

expr_t *
ci_make_unary_minus_expr(expr)
expr_t *expr;
{
	type_t *type;
	expr_t *new;

	type = expr->ex_type;
	if (type != NULL && complain_if_not_arithmetic(expr, "", "unary -"))
		type = NULL;
			
	new = make_unary_expr(OP_UNARY_MINUS, expr, type, FALSE);
	ci_do_integral_promotions(new);
	return new;
}

expr_t *
ci_make_bitwise_not_expr(expr)
expr_t *expr;
{
	type_t *type;
	expr_t *new;

	type = complain_if_not_integral(expr, "", "bitwise not") ? NULL
								 : expr->ex_type;
	new = make_unary_expr(OP_BITWISE_NOT, expr, type, FALSE);
	ci_do_integral_promotions(new);
	return new;
}

expr_t *
ci_make_logical_not_expr(expr)
expr_t *expr;
{
	type_t *type;

	if (complain_if_not_arithmetic_or_ptr(expr, "", "logical not"))
		type = NULL;
	else
		type = ci_code_to_type(TY_INT);
			
	return make_unary_expr(OP_LOGICAL_NOT, expr, type, FALSE);
}

expr_t *
ci_make_conditional_expression(cond, if_true, if_false)
expr_t *cond, *if_true, *if_false;
{
	expr_t *ex;
	type_t *type;

	ci_do_pointer_generation(cond);
	ci_do_pointer_generation(if_true);
	ci_do_pointer_generation(if_false);

	complain_if_not_arithmetic_or_ptr(cond, "First ", "conditional expression");

	if (if_true->ex_type == NULL || if_false->ex_type == NULL)
		type = NULL;
	else {
		type_t *truetype, *falsetype;
		typecode_t tcode, fcode;

		truetype = if_true->ex_type;
		falsetype = if_false->ex_type;
		tcode = truetype->ty_code;
		fcode = falsetype->ty_code;

		if (IS_ARITHMETIC_TYPE(tcode) && IS_ARITHMETIC_TYPE(fcode))
			type = do_usual_arithmetic_conversions(if_true, if_false,
										"?");
		else if (tcode == TY_VOID && fcode == TY_VOID)
			type = ci_code_to_type(TY_VOID);
		else if ((tcode==TY_STRUCT || tcode==TY_UNION || tcode==TY_ENUM) &&
			 (fcode==TY_STRUCT || fcode==TY_UNION || fcode==TY_ENUM) &&
			 truetype->ty_aggr_or_enum == falsetype->ty_aggr_or_enum)
			type = truetype;
		else if (IS_PTR(tcode) || IS_PTR(fcode)) {
			if (is_null_pointer(if_true))
				type = falsetype;
			else if (is_null_pointer(if_false))
				type = truetype;
			else if (is_ptr_to_void(truetype))
				type = truetype;
			else if (is_ptr_to_void(falsetype))
				type = falsetype;
			else if (tcode == DT_PTR_TO && fcode == DT_ARRAY_OF &&
					      ci_types_same(truetype->ty_base,
							    falsetype->ty_base))
				type = truetype;
			else if (tcode == DT_ARRAY_OF && fcode == DT_PTR_TO &&
					      ci_types_same(truetype->ty_base,
							    falsetype->ty_base))
				type = falsetype;
			else if (tcode == DT_ARRAY_OF && fcode == DT_ARRAY_OF &&
					      ci_types_same(truetype->ty_base,
						            falsetype->ty_base))
				type = ci_make_pointer(truetype->ty_base, 0);
			else if (ci_types_same(truetype, falsetype))
				type = truetype;
			else {
				diagf(ET_ERROR, cond->ex_lexinfo,
		"Pointer type clash in conditional expression");
				ci_show_type(if_true->ex_type,
							"Second operand");
				ci_show_type(if_false->ex_type,
							" Third operand");
				type = NULL;
			}
		}
		else {
			diagf(ET_ERROR, cond->ex_lexinfo,
					"Type clash in conditional expression");
			ci_show_type(if_true->ex_type,  "Second operand");
			ci_show_type(if_false->ex_type, " Third operand");
			type = NULL;
		}
	}
			
	ex = NEW(expr_t);
	ex->ex_exprtype = ET_CONDITIONAL;
	ex->ex_type = type;
	ex->ex_is_lvalue = FALSE;
	ex->ex_is_constant = cond->ex_is_constant && if_true->ex_is_constant &&
						     if_false->ex_is_constant;
	ex->ex_lexinfo = cond->ex_lexinfo;

	ex->ex_cond_expr = NEW(cond_expr_t);
	ex->ex_cond_expr->co_cond = cond;
	ex->ex_cond_expr->co_if_true = if_true;
	ex->ex_cond_expr->co_if_false = if_false;
	return ex;
}

expr_t *
ci_make_assignment_expr(op, left, right)
optype_t op;
expr_t *left, *right;
{
	expr_t *(*make_expr_func)PROTO((optype_t mop, const char *opname,
						expr_t *mleft, expr_t *mright));
	const char *opname;
	expr_t *expr;

	ci_do_pointer_generation(right);

	switch (op) {
	case OP_ASSIGN:
		make_expr_func = NULL;
		opname = "=";
		break;
	case OP_MUL_ASSIGN:
		make_expr_func = ci_make_mul_or_div_expr;
		opname = "*=";
		break;
	case OP_DIV_ASSIGN:
		make_expr_func = ci_make_mul_or_div_expr;
		opname = "*=";
		break;
	case OP_MOD_ASSIGN:
		make_expr_func = ci_make_mod_expr;
		opname = "%=";
		break;
	case OP_PLUS_ASSIGN:
		make_expr_func = ci_make_add_or_subtract_expr;
		opname = "+=";
		break;
	case OP_MINUS_ASSIGN:
		make_expr_func = ci_make_add_or_subtract_expr;
		opname = "-=";
		break;
	case OP_LSHIFT_ASSIGN:
		make_expr_func = ci_make_shift_expr;
		opname = ">>=";
		break;
	case OP_RSHIFT_ASSIGN:
		make_expr_func = ci_make_shift_expr;
		opname = "<<=";
		break;
	case OP_BITWISE_AND_ASSIGN:
		make_expr_func = ci_make_bitwise_expr;
		opname = "&=";
		break;
	case OP_BITWISE_XOR_ASSIGN:
		make_expr_func = ci_make_bitwise_expr;
		opname = "^=";
		break;
	case OP_BITWISE_OR_ASSIGN:
		make_expr_func = ci_make_bitwise_expr;
		opname = "|=";
		break;
	default:
		ci_panic("bad op in mae");
		make_expr_func = NULL; /* to satisfy gcc */
		opname = NULL; /* to satisfy gcc */
	}

	if (left->ex_type != NULL) {
		/* TODO: check for const qualifiers */
		const char *what;

		switch(left->ex_type->ty_code) {
		case DT_FUNC_RETURNING:
			what = "is a function";
			break;
		case TY_U_STRUCT:
			what = "is an undefined structure";
			break;
		case TY_U_UNION:
			what = "is an undefined union";
			break;
		case TY_U_ENUM:
			what = "is an undefined enum";
			break;
		case DT_ARRAY_OF:
			what = "is an array";
			break;
		default:
			what = NULL;
		}
		if (what == NULL && !left->ex_is_lvalue)
			what = "is not an lvalue";
		if (what != NULL)
			complain(left, "Left ", opname, what);
	}
	if (op == OP_ASSIGN) {
		if (ci_do_assignment_conversions(left->ex_type, right) != 0) {
			diagf(ET_ERROR, left->ex_lexinfo,
						"Type clash in assignment");
			ci_show_type(left->ex_type,  " Left operand");
			ci_show_type(right->ex_type, "Right operand");
		}
		expr = make_binary_expr(op, left, right, left->ex_type);
	}
	else {
		type_t *type;

		type = left->ex_type;
		expr = (*make_expr_func)(op, opname, left, right);
		expr->ex_type = type;
	}
			
	return expr;
}

static expr_t *
make_scale_expr(expr, op, type)
expr_t *expr;
optype_t op;
type_t *type;
{
	expr_t *ex = NEW(expr_t);

	ex->ex_exprtype = ET_SCALE;
	ex->ex_type = expr->ex_type;
	ex->ex_is_lvalue = FALSE;
	ex->ex_is_constant = expr->ex_is_constant;
	ex->ex_lexinfo = expr->ex_lexinfo;

	ex->ex_scale_expr = NEW(scale_expr_t);
	ex->ex_scale_expr->sc_expr = expr;
	ex->ex_scale_expr->sc_factor = ci_typesize(expr->ex_lexinfo, type);
	ex->ex_scale_expr->sc_op = op;

	return ex;
}

expr_t *
ci_make_cast_expr(type, expr)
type_t *type;
expr_t *expr;
{
	typecode_t typecode;
	expr_t *ex = NEW(expr_t);

	ci_do_pointer_generation(expr);

	if (expr->ex_type != NULL &&
	    expr->ex_type->ty_code != TY_ENUM &&
	    expr->ex_type->ty_code != TY_U_ENUM &&
	    expr->ex_type->ty_code != DT_ARRAY_OF &&
	    !(type != NULL && type->ty_code == TY_VOID) &&
	    !(expr->ex_type->ty_code == TY_VOID && type->ty_code == TY_VOID)) {
		if (complain_if_not_arithmetic_or_ptr(expr, "", "cast"))
			type = NULL;
	}
	if (type != NULL &&
	    type->ty_code != TY_ENUM && type->ty_code != TY_U_ENUM &&
	    type->ty_code != TY_VOID &&
	    !IS_ARITHMETIC_OR_PTR_TYPE(type->ty_code)) {
		diagf(ET_ERROR, expr->ex_lexinfo,
			    "Illegal cast to non arithmetic or pointer type");
		ci_show_type(type, "type");
		type = NULL;
	}


	ex->ex_exprtype = ET_CAST;
	ex->ex_type = type;
	ex->ex_is_lvalue = FALSE;
	ex->ex_is_constant = expr->ex_is_constant;
	ex->ex_lexinfo = expr->ex_lexinfo;

	ex->ex_unary_expr = NEW(unary_expr_t);
	typecode = (type != NULL) ? type->ty_code : TY_INT;
	ex->ex_unary_expr->ue_op = typecode_to_cvt_op(typecode);
	ex->ex_unary_expr->ue_expr = expr;
	return ex;
}

expr_t *
ci_make_mod_expr(op, opname, left, right)
optype_t op;
const char *opname;
expr_t *left, *right;
{
	type_t *type;
	bool leftbad, rightbad;

	leftbad = complain_if_not_integral(left, "Left ", opname);
	rightbad = complain_if_not_integral(right, "Right ", opname);
	
	if (leftbad || rightbad)
		type = NULL;
	else
		type = do_usual_arithmetic_conversions(left, right, opname);
	return make_binary_expr(op, left, right, type);
}

expr_t *
ci_make_add_or_subtract_expr(real_op, opname, left, right)
optype_t real_op;
const char *opname;
expr_t *left, *right;
{
	typecode_t leftcode, rightcode;
	type_t *type;
	expr_t *res;
	optype_t op;

	ci_do_pointer_generation(left);
	ci_do_pointer_generation(right);

	if (left->ex_type == NULL || right->ex_type == NULL)
		return make_binary_expr(real_op, left, right, (type_t *)NULL);
	
	leftcode = left->ex_type->ty_code;
	rightcode = right->ex_type->ty_code;

	switch (real_op) {
	case OP_PLUS:
	case OP_MINUS:
		op = real_op;
		break;
	case OP_PLUS_ASSIGN:
		op = OP_PLUS;
		break;
	case OP_MINUS_ASSIGN:
		op = OP_MINUS;
		break;
	default:
		ci_panic("illegal op in mase");
		op = OP_LAST;			/* to satisfy gcc */
	}
	
	if (IS_PTR(leftcode) || IS_PTR(rightcode)) {
		if (IS_PTR(leftcode) && IS_PTR(rightcode)) {
			if (op == OP_PLUS) {
				diagf(ET_ERROR, left->ex_lexinfo,
					    "Illegal addition of two pointers");
				ci_show_expr_and_type(left, left->ex_type);
				ci_show_expr_and_type(right, right->ex_type);
				type = NULL;
			}
			else {
				if (ci_types_same(left->ex_type->ty_base,
						  right->ex_type->ty_base))
					type = ci_code_to_type(TY_LONG);
				else {
					diagf(ET_ERROR, left->ex_lexinfo,
		      "Illegal subtraction of pointers of different types");
					ci_show_type(left->ex_type, " Left operand");
					ci_show_type(right->ex_type,"Right operand");
					type = NULL;
				}
			}

		}
		else if ((IS_PTR(leftcode) && !ci_is_integral(rightcode)) ||
		         (IS_PTR(rightcode) && !ci_is_integral(leftcode))) {
			diagf(ET_ERROR, right->ex_lexinfo,
				"Illegal %s of non integer to pointer",
				(op == OP_PLUS) ? "addition" : "subtraction");
			ci_show_expr_and_type(right, right->ex_type);
			type = NULL;
		}
		else if (op == OP_MINUS && IS_PTR(rightcode)) {
			diagf(ET_ERROR, right->ex_lexinfo,
		      "Illegal subtraction of a pointer from an integer");
			type = NULL;
		}
		else
			type = (IS_PTR(leftcode) ? left : right)->ex_type;
	}
	else
		type = do_usual_arithmetic_conversions(left, right, opname);

	if (type != NULL) {
		if (IS_PTR(leftcode) && ci_is_integral(rightcode)) {
			ci_do_integral_promotions(right);
			right = make_scale_expr(right, OP_MUL,
						left->ex_type->ty_base);
		}
		else if (IS_PTR(rightcode) && ci_is_integral(leftcode)) {
			ci_do_integral_promotions(left);
			left = make_scale_expr(left, OP_MUL,
						right->ex_type->ty_base);
		}
	}

	res = make_binary_expr(real_op, left, right, type);

	if (type != NULL && IS_PTR(leftcode) && IS_PTR(rightcode))
		res = make_scale_expr(res, OP_DIV, left->ex_type->ty_base);
	
	return res;
}

expr_t *
ci_make_shift_expr(op, opname, left, right)
optype_t op;
const char *opname;
expr_t *left, *right;
{
	bool leftbad, rightbad;
	type_t *type;

	leftbad = complain_if_not_integral(left, "Left ", opname);
	rightbad = complain_if_not_integral(right, "Right ", opname);
	if (leftbad || rightbad)
		type = NULL;
	else {
		ci_do_integral_promotions(left);
		ci_do_integral_promotions(right);
		type = left->ex_type;
	}
	return make_binary_expr(op, left, right, type);
}

expr_t *
ci_make_comparison_expr(op, opname, left, right)
optype_t op;
const char *opname;
expr_t *left, *right;
{
	type_t *type;
	bool ok;

	ci_do_pointer_generation(left);
	ci_do_pointer_generation(right);

	if (left->ex_type == NULL || right->ex_type == NULL)
		ok = FALSE;
	else { 
		typecode_t leftcode, rightcode;

		leftcode = left->ex_type->ty_code;
		rightcode = right->ex_type->ty_code;
		if (IS_PTR(leftcode) || IS_PTR(rightcode)) {
			if (IS_PTR(leftcode) && IS_PTR(rightcode)) {
				if (ci_types_same(left->ex_type->ty_base,
						  right->ex_type->ty_base) ||
				    ((op == OP_IS_EQUAL || op == OP_NOT_EQUAL) &&
				     (is_ptr_to_void(left->ex_type) ||
				      is_ptr_to_void(right->ex_type)))) {
					ok = TRUE;
				}
				else {
					diagf(ET_ERROR, left->ex_lexinfo,
			   "Illegal comparison of different pointer types");
					ci_show_type(left->ex_type, " Left operand");
					ci_show_type(right->ex_type,"Right operand");
					ok = FALSE;
				}
			}
			else if ((op == OP_IS_EQUAL || op == OP_NOT_EQUAL) &&
				 (is_null_pointer(left) || is_null_pointer(right))) {
					ok = TRUE;
			}
			else {
				diagf(ET_ERROR, left->ex_lexinfo,
			"Illegal comparison of pointer and integer");
				ci_show_type(left->ex_type,  " Left operand");
				ci_show_type(right->ex_type, "Right operand");
				ok = FALSE;
			}
		}
		else if (leftcode == TY_ENUM && rightcode == TY_ENUM) {
			if (left->ex_type->ty_aggr_or_enum ==
						right->ex_type->ty_aggr_or_enum)
				ok = TRUE;
			else {
				diagf(ET_WARNING, left->ex_lexinfo,
			"Illegal comparison of different enum types");
				ci_show_type(left->ex_type,  " Left operand");
				ci_show_type(right->ex_type, "Right operand");
				ok = FALSE;
			}
		}
		else {
			ok = do_usual_arithmetic_conversions(left, right, opname)
									   != NULL;
		}
	}
	type = ok ? ci_code_to_type(TY_INT) : NULL;
	return make_binary_expr(op, left, right, type);
}

expr_t *
ci_make_bitwise_expr(op, opname, left, right)
optype_t op;
const char *opname;
expr_t *left, *right;
{
	bool leftbad, rightbad;
	type_t *type;

	leftbad = complain_if_not_integral(left, "Left ", opname);
	rightbad = complain_if_not_integral(right, "Right ", opname);
	if (leftbad || rightbad)
		type = NULL;
	else
		type = do_usual_arithmetic_conversions(left, right, opname);
	return make_binary_expr(op, left, right, type);
}

expr_t *
ci_make_logical_expr(op, opname, left, right)
optype_t op;
const char *opname;
expr_t *left, *right;
{
	bool leftbad, rightbad;
	type_t *type;

	leftbad = complain_if_not_arithmetic_or_ptr(left, "Left ", opname);
	rightbad = complain_if_not_arithmetic_or_ptr(right, "Right ", opname);

	type = (leftbad || rightbad) ? NULL : ci_code_to_type(TY_INT);
	return make_binary_expr(op, left, right, type);
}

expr_t *
ci_make_mul_or_div_expr(op, opname, left, right)
optype_t op;
const char *opname;
expr_t *left, *right;
{
	type_t *type;

	type = do_usual_arithmetic_conversions(left, right, opname);
	return make_binary_expr(op, left, right, type);
}

expr_t *
ci_make_sizeof_expr(expr, type)
expr_t *expr;
type_t *type;
{
	expr_t *ex;
	lexinfo_t *lexinfo;
	taddr_t size;
	
	if (expr != NULL) {
		type = expr->ex_type;
		lexinfo = expr->ex_lexinfo;
	}
	else
		lexinfo = NULL;

	if (type == NULL)
		size = 0;
	else {
		const char *badstr;

		switch(type->ty_code) {
		case DT_FUNC_RETURNING:
			badstr = "a function";
			break;
		case TY_U_STRUCT:
		case TY_U_UNION:
		case TY_U_ENUM:
			badstr = "an incomplete type";
			break;
		case DT_ARRAY_OF:
			badstr = type->ty_dim->di_hdynamic ? "an incomplete type"
							   : NULL;
			break;
		case TY_BITFIELD:
			badstr = "a bitfield";
			break;
		default:
			badstr = NULL;
		}
		if (badstr != NULL) {
			diagf(ET_ERROR, lexinfo,
			      "Sizeof may not be applied to %s", badstr);
			ci_show_expr_and_type(expr, type);
			size = 0;
		}
		else
			size = ci_typesize(lexinfo, type);	
	}

	ex = NEW(expr_t);
	ex->ex_exprtype = ET_SIZEOF;
	ex->ex_type = ci_code_to_type(TY_ULONG);
	ex->ex_is_lvalue = FALSE;
	ex->ex_is_constant = TRUE;
	ex->ex_lexinfo = lexinfo;

	ex->ex_sizeof_expr = NEW(sizeof_expr_t);
	ex->ex_sizeof_expr->sz_expr = expr;
	ex->ex_sizeof_expr->sz_type = type;
	ex->ex_sizeof_expr->sz_size = size;
	return ex;
}

static void
make_implicit_func_declaration(func_expr)
expr_t *func_expr;
{
	namedesc_t *nd;
	declaration_t *dn;
	type_t *ftype, *idtype;

	idtype = ci_make_type(Parse_alloc_id, TY_IDENTIFIER);
	idtype->ty_identifier = NEW(identifier_t);
	idtype->ty_identifier->id_name = func_expr->ex_undef_name;
	idtype->ty_identifier->id_lexinfo = func_expr->ex_lexinfo;

	ftype = ci_make_funcret_type(idtype,
				     FDT_IDLIST,
				     (declaration_t *)NULL,
				     (identifier_list_t *)NULL);
	dn = ci_make_declaration(CL_DECL, ci_code_to_type(TY_INT), 0,
						ci_make_declarator(ftype));
	ci_handle_declaration(dn);

	nd = ci_lookup_name(func_expr->ex_undef_name);
	if (nd == NULL || nd->nd_nametype != NT_FUNCNAME)
		ci_panic("implicit func decl botch in mifd");

	func_expr->ex_exprtype = ET_FUNCNAME;
	func_expr->ex_type = nd->nd_var->va_type;
	func_expr->ex_var = nd->nd_var;
}

expr_t *
ci_make_func_call_expr(func_expr, arglist)
expr_t *func_expr;
expr_list_t *arglist;
{
	expr_t *ex = NEW(expr_t);
	type_t *ftype, *type;
	bool is_implicit_declaration;

	is_implicit_declaration = func_expr->ex_exprtype == ET_UNDEF_VAR;
	if (is_implicit_declaration) {
		diagf(ET_WARNING, func_expr->ex_lexinfo,
		      "Implicit declaration of function %s",
						func_expr->ex_undef_name);
		make_implicit_func_declaration(func_expr);
	}
	if (func_expr->ex_type != NULL) {
		if (func_expr->ex_type->ty_code == DT_FUNC_RETURNING)
			func_expr->ex_type = ci_make_pointer(func_expr->ex_type, 0);
		ftype = func_expr->ex_type;
		if (ftype->ty_code != DT_PTR_TO || ftype->ty_base->ty_code !=
								DT_FUNC_RETURNING) {
			diagf(ET_ERROR, func_expr->ex_lexinfo,
			    "Function designator expression is not a function");
			ci_show_expr_and_type(func_expr, func_expr->ex_type);
			type = NULL;
		}
		else {
			char *exprstr;
			expr_list_t *el;

			exprstr = ci_expr_to_english(func_expr);
			type = ftype->ty_base->ty_base;
			for (el = arglist; el != NULL; el = el->el_next)
				ci_do_pointer_generation(el->el_expr);
			arglist = ci_check_func_call(func_expr, exprstr, arglist,
							is_implicit_declaration);
			free(exprstr);
		}
	}
	else
		type = NULL;
	
	ex->ex_exprtype = ET_FUNC_CALL;
	ex->ex_type = type;
	ex->ex_is_lvalue = FALSE;
	ex->ex_is_constant = FALSE;
	ex->ex_lexinfo = func_expr->ex_lexinfo;

	ex->ex_func_call_expr = NEW(func_call_expr_t);
	ex->ex_func_call_expr->fce_func = func_expr;
	ex->ex_func_call_expr->fce_expr_list = arglist;
	return ex;
}

void
ci_do_integral_promotions(expr)
expr_t *expr;
{
	typecode_t typecode;

	if (expr->ex_type == NULL)
		return;

	switch(expr->ex_type->ty_code) {
	case TY_BITFIELD:
		ci_push_conversion(expr,
				   expr->ex_type->ty_bitfield->bf_code,
				   ET_PROMOTION);
		break;
	case TY_CHAR:
	case TY_UCHAR:
		typecode = int_is_bigger_than_char() ? TY_INT : TY_UINT;
		ci_push_conversion(expr, typecode, ET_PROMOTION);
		break;
	case TY_SHORT:
	case TY_USHORT:
		typecode = int_is_bigger_than_short() ? TY_INT : TY_UINT;
		ci_push_conversion(expr, typecode, ET_PROMOTION);
		break;
	default:
		break;
	}
}

