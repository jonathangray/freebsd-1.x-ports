/* ci_showexpr.c - routines to convert an expression tree to text */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_ci_showexpr_c_sccsid[] = "@(#)ci_showexpr.c	1.12 26/7/92 (UKC)";

#include <stdlib.h>

#include <local/ukcprog.h>

#include "ups.h"
#include "symtab.h"
#include "ci.h"
#include "ci_parse.h"
#include "ci_showexpr.h"
#include "ci_types.h"
#include "ci_util.h"

static char *binary_expr_to_english PROTO((binary_expr_t *binary_expr,
								bool want_brackets));
static char *unary_expr_to_english PROTO((unary_expr_t *unary_expr,
								bool want_brackets));
static char *cast_expr_to_english PROTO((unary_expr_t *unary_expr, type_t *type));
static char *func_call_to_english PROTO((func_call_expr_t *func_call));
static char *dot_expr_to_english PROTO((dot_expr_t *dot_expr));
static char *multi_arrow_expr_to_english PROTO((multi_arrow_expr_t *ma));
static char *sizeof_expr_to_english PROTO((sizeof_expr_t *sizeof_expr));
static char *cond_expr_to_english PROTO((cond_expr_t *cond_expr));
static char *promotion_to_english PROTO((expr_t *expr));

static char *
binary_expr_to_english(binary_expr, want_brackets)
binary_expr_t *binary_expr;
bool want_brackets;
{
	const char *opstr, *fmt;
	char *leftstr, *rightstr, *s;

	switch(binary_expr->be_op) {
		case OP_ASSIGN:			opstr = "=";		break;
		case OP_MUL_ASSIGN:		opstr = "*=";		break;
		case OP_DIV_ASSIGN:		opstr = "/=";		break;
		case OP_MOD_ASSIGN:		opstr = "%=";		break;
		case OP_PLUS_ASSIGN:		opstr = "+=";		break;
		case OP_MINUS_ASSIGN:		opstr = "-=";		break;
		case OP_LSHIFT_ASSIGN:		opstr = "<<=";		break;
		case OP_RSHIFT_ASSIGN:		opstr = ">>=";		break;
		case OP_BITWISE_AND_ASSIGN:	opstr = "&=";		break;
		case OP_BITWISE_XOR_ASSIGN:	opstr = "^=";		break;
		case OP_BITWISE_OR_ASSIGN:	opstr = "|=";		break;
		case OP_LOGICAL_OR:		opstr = "||";		break;
		case OP_LOGICAL_AND:		opstr = "&&";		break;
		case OP_BITWISE_OR:		opstr = "|";		break;
		case OP_BITWISE_XOR:		opstr = "^";		break;
		case OP_BITWISE_AND:		opstr = "&";		break;
		case OP_BITWISE_NOT:		opstr = "~";		break;
		case OP_IS_EQUAL:		opstr = "==";		break;
		case OP_NOT_EQUAL:		opstr = "!=";		break;
		case OP_LESS:			opstr = "<";		break;
		case OP_GREATER:		opstr = ">";		break;
		case OP_LESS_OR_EQUAL:		opstr = "<=";		break;
		case OP_GREATER_OR_EQUAL:	opstr = ">=";		break;
		case OP_LSHIFT:			opstr = "<<";		break;
		case OP_RSHIFT:			opstr = ">>";		break;
		case OP_PLUS:			opstr = "+";		break;
		case OP_MINUS:			opstr = "-";		break;
		case OP_MUL:			opstr = "*";		break;
		case OP_DIV:			opstr = "/";		break;
		case OP_MOD:			opstr = "%";		break;
		case OP_COMMA:			opstr = ",";		break;

		default:ci_panic("bad op in bte");opstr = NULL;		break;
	}
	fmt = want_brackets ? "(%s %s %s)" : "%s %s %s";
	leftstr = ci_expr_to_english(binary_expr->be_left);
	rightstr = ci_expr_to_english(binary_expr->be_right);
	s = strf(fmt, leftstr, opstr, rightstr);
	free(leftstr);
	free(rightstr);
	return s;
}

static char *
unary_expr_to_english(unary_expr, want_brackets)
unary_expr_t *unary_expr;
bool want_brackets;
{
	const char *preop, *postop, *fmt;
	char *exprstr, *s;

	preop = postop = NULL;
	switch(unary_expr->ue_op) {
		case OP_PREINC:			preop = "++";		break;
		case OP_PREDEC:			preop = "--";		break;
		case OP_POSTINC:		postop = "++";		break;
		case OP_POSTDEC:		postop = "--";		break;
		case OP_LOGICAL_NOT:		preop = "!";		break;
		case OP_ADDRESS_OF:		preop = "&";		break;
		case OP_DEREF:			preop = "*";		break;
		case OP_UNARY_PLUS:		preop = "+";		break;
		case OP_UNARY_MINUS:		preop = "-";		break;
		default:			ci_panic("bad op in ute");break;
	}
	fmt = want_brackets ? "(%s%s)" : "%s%s";
	exprstr = ci_expr_to_english(unary_expr->ue_expr);
	if (preop != NULL)
		s = strf(fmt, preop, exprstr);
	else
		s = strf(fmt, exprstr, postop);
	free(exprstr);
	return s;
}

static char *
cast_expr_to_english(unary_expr, type)
unary_expr_t *unary_expr;
type_t *type;
{
	char *typestr, *exprstr, *s;

	typestr = ci_type_to_english(type, FALSE);
	exprstr = ci_expr_to_english(unary_expr->ue_expr);
	s = strf("(%s)(%s)", typestr, exprstr);
	free(typestr);
	free(exprstr);
	return s;
}

static char *
func_call_to_english(func_call)
func_call_expr_t *func_call;
{
	expr_list_t *el;
	char *args, *res, *funcstr;
	const char *sep;

	args = strsave("");
	sep = "";
	for (el = func_call->fce_expr_list; el != NULL; el = el->el_next) {
		char *argstr, *new;

		argstr = ci_expr_to_english(el->el_expr);
		new = strf("%s%s%s", args, sep, argstr);
		free(argstr);
		free(args);
		args = new;
		sep = ", ";
	}
	funcstr = ci_expr_to_english(func_call->fce_func);
	res = strf("%s(%s)", funcstr, args);
	free(funcstr);
	free(args);
	return res;
}

static char *
multi_arrow_expr_to_english(ma)
multi_arrow_expr_t *ma;
{
	char *aggrstr, *indexstr, *s;

	aggrstr = ci_expr_to_english(ma->ma_aggr);
	indexstr = ci_expr_to_english(ma->ma_index);

	s = strf("%s->[%s]%s", aggrstr, indexstr, ma->ma_member->va_name);

	free(aggrstr);
	free(indexstr);
	return s;
}

static char *
dot_expr_to_english(dot_expr)
dot_expr_t *dot_expr;
{
	char *aggrstr, *s;

	aggrstr = ci_expr_to_english(dot_expr->de_aggr);
	s = strf("%s.%s", aggrstr, dot_expr->de_member->va_name);
	free(aggrstr);
	return s;
}

static char *
sizeof_expr_to_english(sizeof_expr)
sizeof_expr_t *sizeof_expr;
{
	if (sizeof_expr->sz_type != NULL)
		return ci_type_to_english(sizeof_expr->sz_type, FALSE);
	else
		return ci_expr_to_english(sizeof_expr->sz_expr);
}

static char *
promotion_to_english(expr)
expr_t *expr;
{
	char *s, *oldtypestr, *newtypestr, *exprstr;

	oldtypestr = ci_type_to_decl(expr->ex_unary_expr->ue_expr->ex_type, FALSE);
	newtypestr = ci_type_to_decl(expr->ex_type, FALSE);
	exprstr = ci_expr_to_english(expr->ex_unary_expr->ue_expr);
	s = strf("({%s -> %s} %s)", oldtypestr, newtypestr, exprstr);
	free(oldtypestr);
	free(newtypestr);
	free(exprstr);
	return s;
}

static char *
cond_expr_to_english(cond_expr)
cond_expr_t *cond_expr;
{
	char *s, *cond_str, *if_true_str, *if_false_str;

	cond_str = ci_expr_to_english(cond_expr->co_cond);
	if_true_str = ci_expr_to_english(cond_expr->co_if_true);
	if_false_str = ci_expr_to_english(cond_expr->co_if_false);
	s = strf("%s ? %s : %s", cond_str, if_true_str, if_false_str);
	free(cond_str);
	free(if_true_str);
	free(if_false_str);
	return s;
}

char *
ci_expr_to_english(expr)
expr_t *expr;
{
	char *s;

	switch(expr->ex_exprtype) {
	case ET_SCALE:
		return ci_expr_to_english(expr->ex_scale_expr->sc_expr);
	case ET_ASSIGNMENT_CONVERSION:
		return ci_expr_to_english(expr->ex_unary_expr->ue_expr);
	case ET_PROMOTION:
		return promotion_to_english(expr);
	case ET_ENUM_CONST:
		return strsave(expr->ex_enum_member->em_name);
	case ET_VAR:
	case ET_FUNCNAME:
		return strsave(expr->ex_var->va_name);
	case ET_UNDEF_VAR:
		return strsave(expr->ex_undef_name);
	case ET_FUNC_CALL:
		return func_call_to_english(expr->ex_func_call_expr);
	case ET_MULTI_ARROW:
		return multi_arrow_expr_to_english(expr->ex_multi_arrow_expr);
	case ET_DOT:
		return dot_expr_to_english(expr->ex_dot_expr);
	case ET_CHAR_CONST:
		s = strf("'%c'", expr->ex_integer_constant_val);
		return s;
	case ET_INT_CONST:
		s = strf("%d", expr->ex_integer_constant_val);
		return s;
	case ET_FLOATING_CONST:
		return strsave("{floating constant}");
	case ET_STRING_CONST:
		s = strf("\"%s\"", expr->ex_string_constant_val->sc_val);
		return s;
	case ET_BINARY:
		return binary_expr_to_english(expr->ex_binary_expr, TRUE);
	case ET_UNARY:
		return unary_expr_to_english(expr->ex_unary_expr, TRUE);
	case ET_CAST:
		return cast_expr_to_english(expr->ex_unary_expr, expr->ex_type);
	case ET_SIZEOF:
		return sizeof_expr_to_english(expr->ex_sizeof_expr);
	case ET_CONDITIONAL:
		return cond_expr_to_english(expr->ex_cond_expr);
	default:
		ci_panic("bad expr type in ete");
		return NULL; /* to satisfy gcc */
	}

}

void
ci_show_expr_and_type(expr, type)
expr_t *expr;
type_t *type;
{
	if (expr != NULL) {
		char *exprstr;

		exprstr = ci_expr_to_english(expr);
		diagf(ET_MORE, (lexinfo_t *)NULL, "Expression: %s", exprstr);
		free(exprstr);
	}
	ci_show_type(type, "      type");
}
