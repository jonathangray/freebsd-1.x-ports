/* ci_stm.c - routines used by ci_parse.y to build statement parse trees */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_ci_stm_c_sccsid[] = "@(#)ci_stm.c	1.12 20/5/92 (UKC)";

#include <stdlib.h>

#include <local/ukcprog.h>
#include <mtrprog/genmergesort.h>

#include "ups.h"
#include "symtab.h"
#include "ci.h"
#include "ci_parse.h"
#include "ci_util.h"
#include "ci_stm.h"
#include "ci_func.h"
#include "ci_types.h"
#include "ci_expr.h"
#include "ci_constexpr.h"
#include "ci_showexpr.h"

typedef struct case_statement_listst {
	case_labeled_stm_t *csl_case;
	struct case_statement_listst *csl_next;
} case_statement_list_t;

static void check_iteration_expr_is_arithmetic_or_ptr PROTO((expr_t *expr,
							     const char *what));
static case_statement_list_t *build_case_stm_list PROTO((statement_t *st,
					            case_statement_list_t *cslist));
static void build_cstab PROTO((type_t *switchtype, case_statement_list_t *cslist,
			       case_labeled_stm_t ***p_cstab,
			       case_labeled_stm_t **p_default_cs, int *p_ncase));
static int casecmp PROTO((case_statement_list_t *csl1, case_statement_list_t *csl2));

GENERIC_MERGE_SORT(static,sortcslist,case_statement_list_t,csl_next)

static void
check_iteration_expr_is_arithmetic_or_ptr(expr, what)
expr_t *expr;
const char *what;
{
	if (expr->ex_type != NULL &&
	    !IS_ARITHMETIC_OR_PTR_TYPE(expr->ex_type->ty_code)) {
		diagf(ET_ERROR, expr->ex_lexinfo,
		      "`%s' expression is not of arithmetic type or a pointer",
									what);
		ci_show_type(expr->ex_type, "type");
	}
}

statement_t *
ci_make_labeled_statement(id, statement)
identifier_t *id;
statement_t *statement;
{
	statement_t *st = NEW(statement_t);
	
	st->st_type = STT_LABELED;
	st->st_lexinfo = NULL;
	st->st_labeled = NEW(labeled_stm_t);
	st->st_labeled->ls_goto_label = ci_name_to_label(id, TRUE);
	st->st_labeled->ls_stm = statement;
	return st;
}

statement_t *
ci_make_goto_statement(id)
identifier_t *id;
{
	statement_t *st = NEW(statement_t);
	
	st->st_type = STT_GOTO;
	st->st_lexinfo = id->id_lexinfo;
	st->st_goto_label = ci_name_to_label(id, FALSE);
	return st;
}

statement_t *
ci_make_case_labeled_statement(in_switch, expr, statement)
bool in_switch;
expr_t *expr;
statement_t *statement;
{
	statement_t *st = NEW(statement_t);
	
	if (!in_switch) {
		diagf(ET_ERROR, (lexinfo_t *)NULL,
		      "`%s' not in switch statement",
					(expr != NULL) ? "case" : "default");
	}

	st->st_type = STT_CASE_LABELED;
	st->st_lexinfo = NULL;
	st->st_case = NEW(case_labeled_stm_t);
	st->st_case->cs_expr = expr;
	st->st_case->cs_stm = statement;
	st->st_case->cs_labeldesc = NO_LABELDESC;

	if (expr != NULL) {
		static const char what[] = "Case label constant expression";

		if (!ci_is_integral(expr->ex_type->ty_code) &&
						expr->ex_type->ty_code != TY_ENUM) {
			diagf(ET_ERROR, expr->ex_lexinfo,
						"%s must be integral", what);
			ci_show_expr_and_type(expr, expr->ex_type);
			expr->ex_type = NULL;
		}
		else {
			constval_t val;

			ci_evaluate_constant_expression(expr, what, FALSE, &val);
			st->st_case->cs_val = val.cv_int;
		}
	}

	return st;
}

statement_t *
ci_make_expression_statement(expr)
expr_t *expr;
{
	statement_t *st = NEW(statement_t);
	
	st->st_type = STT_EXPR;
	st->st_lexinfo = (expr == NULL) ? NULL : expr->ex_lexinfo;
	st->st_expr = expr;
	return st;
}

statement_t *
ci_make_if_statement(expr, ifpart, elsepart)
expr_t *expr;
statement_t *ifpart, *elsepart;
{
	statement_t *st = NEW(statement_t);
	
	check_iteration_expr_is_arithmetic_or_ptr(expr, "if");

	st->st_type = STT_IF;
	st->st_lexinfo = expr->ex_lexinfo;
	st->st_if = NEW(if_stm_t);
	st->st_if->is_expr = expr;
	st->st_if->is_ifpart = ifpart;
	st->st_if->is_elsepart = elsepart;
	return st;
}

static case_statement_list_t *
build_case_stm_list(st, cslist)
statement_t *st;
case_statement_list_t *cslist;
{
	statement_t *childst;

	if (st == NULL)
		return cslist;

	switch (st->st_type) {
	case STT_LABELED:
		cslist = build_case_stm_list(st->st_labeled->ls_stm, cslist);
		break;
	case STT_CASE_LABELED:
		{
			case_statement_list_t *csl;

			csl = NEW(case_statement_list_t);
			csl->csl_case = st->st_case;
			csl->csl_next = cslist;
			cslist = build_case_stm_list(st->st_case->cs_stm, csl);
		}
		break;
	case STT_COMPOUND:
		childst = st->st_compound->co_statements;
		for (; childst != NULL; childst = childst->st_next)
			cslist = build_case_stm_list(childst, cslist);
		break;
        case STT_IF:
		cslist = build_case_stm_list(st->st_if->is_ifpart, cslist);
		cslist = build_case_stm_list(st->st_if->is_ifpart, cslist);
		break;
        case STT_WHILE:
        case STT_DO:
		cslist = build_case_stm_list(st->st_while->ws_stm, cslist);
		break;
        case STT_FOR:
		cslist = build_case_stm_list(st->st_for->fs_stm, cslist);
		break;
        case STT_SWITCH:
        case STT_EXPR:
        case STT_GOTO:
        case STT_CONTINUE:
        case STT_BREAK:
        case STT_RETURN:
		break;
	default:
		ci_panic("bad stm in mcsl");
		break;
	}
	return cslist;
}
		
static int
casecmp(csl1, csl2)
case_statement_list_t *csl1, *csl2;
{
	return csl1->csl_case->cs_val - csl2->csl_case->cs_val;
}

static void
build_cstab(switchtype, cslist, p_cstab, p_default_cs, p_ncase)
type_t *switchtype;
case_statement_list_t *cslist;
case_labeled_stm_t ***p_cstab;
case_labeled_stm_t **p_default_cs;
int *p_ncase;
{
	case_statement_list_t *csl, *newlist, *next;
	int ncase;
	case_labeled_stm_t **p_cs, **cstab, *default_cs;
	typecode_t switchcode;

	switchcode = (switchtype == NULL) ? TY_NOTYPE : switchtype->ty_code;

	default_cs = NULL;
	newlist = NULL;
	ncase = 0;
	for (csl = cslist; csl != NULL; csl = next) {
		case_labeled_stm_t *cs;
		expr_t *expr;
		bool want_case;

		next = csl->csl_next;

		cs = csl->csl_case;
		expr = cs->cs_expr;

		if (expr == NULL) {
			if (default_cs != NULL)
				diagf(ET_ERROR, (lexinfo_t *)NULL,
						"Duplicate defaults in switch");
			default_cs = cs;
			want_case = FALSE;
		}
		else if (expr->ex_type == NULL)
			want_case = FALSE;
		else {
			type_t *casetype;
			
			casetype = expr->ex_type;
			if (casetype->ty_code == TY_ENUM || switchcode == TY_ENUM) {
				if (ci_types_same(casetype, switchtype))
					want_case = TRUE;
				else {
					diagf(ET_ERROR, expr->ex_lexinfo,
		"Case expression type conflicts with switch expression type");
					ci_show_type(switchtype,
							"Switch expression type");
					ci_show_type(casetype,
							"  Case expression type");
					want_case = FALSE;
				}
			}
			else if (switchcode != TY_NOTYPE) {
				ci_push_conversion(expr, switchcode, ET_PROMOTION);
				want_case = TRUE;
			}
			else
				want_case = FALSE;
		}
		if (want_case) {
			csl->csl_next = newlist;
			newlist = csl;
			++ncase;
		}
	}

	if (ncase == 0)
		cstab = NULL;
	else {
		newlist = sortcslist(newlist, ncase, casecmp);

		p_cs = cstab = (case_labeled_stm_t **)alloc(Parse_alloc_id,
					     ncase * sizeof(case_labeled_stm_t *));
		for (csl = newlist; csl != NULL; csl = csl->csl_next)
			*p_cs++ = csl->csl_case;
		
		for (p_cs = cstab + 1; p_cs < cstab + ncase; ++p_cs) {
			if ((*p_cs)->cs_val == p_cs[-1]->cs_val) {
				diagf(ET_ERROR, (*p_cs)->cs_expr->ex_lexinfo,
				"Case label has same value as label at %s",
				       ci_lexinfo_to_string(
						 p_cs[-1]->cs_expr->ex_lexinfo,
						 (*p_cs)->cs_expr->ex_lexinfo));
			}
		}
	}
		
	*p_default_cs = default_cs;
	*p_ncase = ncase;
	*p_cstab = cstab;
}

statement_t *
ci_make_switch_statement(expr, statement)
expr_t *expr;
statement_t *statement;
{
	statement_t *st = NEW(statement_t);
	int ncase;
	case_statement_list_t *csl;
	case_labeled_stm_t **cstab, *default_cs;
	
	if (expr->ex_type != NULL && expr->ex_type->ty_code != TY_ENUM &&
					!ci_is_integral(expr->ex_type->ty_code)) {
		diagf(ET_ERROR, expr->ex_lexinfo,
					"Non integral switch expression");
		ci_show_type(expr->ex_type, "Type");
	}

	ci_do_integral_promotions(expr);

	csl = build_case_stm_list(statement, (case_statement_list_t *)NULL);
	
	if (csl == NULL)
		diagf(ET_WARNING, expr->ex_lexinfo,
				"Switch statement with no case labels");
	else if (csl->csl_case->cs_expr == NULL && csl->csl_next == NULL)
		diagf(ET_WARNING, expr->ex_lexinfo,
				"Switch statement with only a default label");

	build_cstab(expr->ex_type, csl, &cstab, &default_cs, &ncase);

	st->st_type = STT_SWITCH;
	st->st_lexinfo = expr->ex_lexinfo;
	st->st_switch = NEW(switch_stm_t);
	st->st_switch->ss_expr = expr;
	st->st_switch->ss_stm = statement;
	st->st_switch->ss_cstab = cstab;
	st->st_switch->ss_ncase = ncase;
	st->st_switch->ss_default_cs = default_cs;
	return st;
}

statement_t *
ci_make_while_statement(statement_type, expr, statement)
statement_type_t statement_type;
expr_t *expr;
statement_t *statement;
{
	statement_t *st = NEW(statement_t);
	
	check_iteration_expr_is_arithmetic_or_ptr(expr, "while");

	st->st_type = statement_type;
	st->st_lexinfo = expr->ex_lexinfo;
	st->st_while = NEW(while_stm_t);
	st->st_while->ws_expr = expr;
	st->st_while->ws_stm = statement;
	return st;
}

statement_t *
ci_make_for_statement(init, test, reinit, statement, lx)
expr_t *init, *test, *reinit;
statement_t *statement;
lexinfo_t *lx;
{
	statement_t *st = NEW(statement_t);
	
	if (test != NULL)
		check_iteration_expr_is_arithmetic_or_ptr(test, "for");

	st->st_type = STT_FOR;

	if (init != NULL)
		st->st_lexinfo = init->ex_lexinfo;
	else if (test != NULL)
		st->st_lexinfo = test->ex_lexinfo;
	else if (reinit != NULL)
		st->st_lexinfo = reinit->ex_lexinfo;
	else
		st->st_lexinfo = lx;

	st->st_for = NEW(for_stm_t);
	st->st_for->fs_init = init;
	st->st_for->fs_test = test;
	st->st_for->fs_reinit = reinit;
	st->st_for->fs_stm = statement;
	return st;
}

statement_t *
ci_make_continue_statement(in_loop, lx)
bool in_loop;
lexinfo_t *lx;
{
	statement_t *st = NEW(statement_t);
	
	if (!in_loop)
		diagf(ET_ERROR, (lexinfo_t *)NULL,
						"`continue' outside any loop");
	st->st_type = STT_CONTINUE;
	st->st_lexinfo = lx;
	return st;
}

statement_t *
ci_make_break_statement(in_loop_or_switch, lx)
bool in_loop_or_switch;
lexinfo_t *lx;
{
	statement_t *st = NEW(statement_t);
	
	if (!in_loop_or_switch)
		diagf(ET_ERROR, (lexinfo_t *)NULL,
					"`break' not in loop or switch");
	st->st_type = STT_BREAK;
	st->st_lexinfo = lx;
	return st;
}

statement_t *
ci_make_return_statement(expr, lx)
expr_t *expr;
lexinfo_t *lx;
{
	statement_t *st = NEW(statement_t);
	
	ci_check_return_expr(expr);

	st->st_type = STT_RETURN;
	st->st_lexinfo = (expr == NULL) ? lx : expr->ex_lexinfo;
	st->st_expr = expr;
	return st;
}
