/* ci_init.c - C interpreter routines handling initialisation */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_ci_init_c_sccsid[] = "@(#)ci_init.c	1.14 20/5/92 (UKC)";

#include <string.h>

#include <stdlib.h>

#include <local/ukcprog.h>

#include "ups.h"
#include "symtab.h"
#include "ci.h"
#include "ci_parse.h"
#include "ci_init.h"
#include "ci_expr.h"
#include "ci_constexpr.h"
#include "ci_util.h"
#include "ci_types.h"

static initialiser_t *make_initialiser PROTO((const char *name,
					   type_t *type,
					   initexpr_t *ie,
					   const char *what,
					   bool is_local,
					   initexpr_t **p_excess));
static initialiser_t *make_scalar_initialiser PROTO((const char *name,
						  type_t *type,
						  initexpr_t *ie,
						  const char *what,
						  bool is_local));
static initialiser_t *make_array_initialiser PROTO((const char *name,
						 type_t *arraytype,
						 initexpr_t *ie,
						 bool is_local,
						 initexpr_t **p_excess));
static initialiser_t *make_struct_or_union_initialiser PROTO((type_t *type,
							   initexpr_t *ie,
							   bool is_local,
							  initexpr_t **p_excess));
static lexinfo_t *initexpr_to_lexinfo PROTO((initexpr_t *ie));
static bool is_address_expr PROTO((expr_t *expr));

static lexinfo_t *
initexpr_to_lexinfo(ie)
initexpr_t *ie;
{
	while (ie->ie_is_list)
		ie = ie->ie_list;
	return ie->ie_expr->ex_lexinfo;
}

static initialiser_t *
make_struct_or_union_initialiser(type, ie, is_local, p_excess)
type_t *type;
initexpr_t *ie;
bool is_local;
initexpr_t **p_excess;
{
	const char *what;
	initialiser_t *first, *last, *initialiser;
	var_t *v;

	what = (type->ty_code == TY_STRUCT) ? "structure member" : "union member";

	first = last = NULL;
	for (v = type->ty_aggr_or_enum->ae_aggr_members; v != NULL; v = v->va_next) {
		initialiser_t *in;

		in = make_initialiser(v->va_name, v->va_type, ie, what, is_local,
									       &ie);
		if (in != NULL) {
			if (last != NULL)
				last->in_next = in;
			else
				first = in;
			last = in;
		}
		if (ie == NULL || type->ty_code == TY_UNION)
			break;
	}
	if (last != NULL)
		last->in_next = NULL;
	*p_excess = ie;

	initialiser = NEW(initialiser_t);
	initialiser->in_inittype = IT_LIST;
	initialiser->in_list = first;
	return initialiser;
}

static initialiser_t *
make_array_initialiser(name, arraytype, ie, is_local, p_excess)
const char *name;
type_t *arraytype;
initexpr_t *ie;
bool is_local;
initexpr_t **p_excess;
{
	dim_t *dim;
	long count;
	initialiser_t *initialiser;

	dim = arraytype->ty_dim;

	if (!ie->ie_is_list && ie->ie_expr->ex_exprtype == ET_STRING_CONST
					&& arraytype->ty_base->ty_code == TY_CHAR) {
		count = ie->ie_expr->ex_string_constant_val->sc_size;
		initialiser = NULL;
		if (!dim->di_hdynamic && count > dim->di_high) {
			diagf(ET_ERROR, ie->ie_expr->ex_lexinfo,
			"%d character initialiser for %d character array `%s'",
				      		count, dim->di_high, name);
		}
		else {
			initialiser = NEW(initialiser_t);
			initialiser->in_inittype = IT_STRINGVAL;
			initialiser->in_stringval =
						ie->ie_expr->ex_string_constant_val;
		}
		ie = ie->ie_next;
	}
	else {
		initialiser_t *first, *last;

		first = last = NULL;
		for (count = 0; ie != NULL; ++count) {
			initialiser_t *in;

			if (!dim->di_hdynamic && count == dim->di_high)
				break;

			in = make_initialiser(name, arraytype->ty_base, ie,
					      "element of array", is_local, &ie);
			
			if (in != NULL) {
				if (last != NULL)
					last->in_next = in;
				else
					first = in;
				last = in;
			}
		}
		if (last != NULL)
			last->in_next = NULL;
		initialiser = NEW(initialiser_t);
		initialiser->in_inittype = IT_LIST;
		initialiser->in_list = first;
	}

	if (dim->di_hdynamic) {
		dim->di_high = count;
		dim->di_hdynamic = FALSE;
	}

	*p_excess = ie;
	
	return initialiser;
}

static bool
is_address_expr(expr)
expr_t *expr;
{
	switch (expr->ex_exprtype) {
	case ET_STRING_CONST:
		return TRUE;
	case ET_UNARY:
		return expr->ex_unary_expr->ue_op == OP_ADDRESS_OF &&
				expr->ex_unary_expr->ue_expr->ex_exprtype == ET_VAR;
	case ET_VAR:
		return expr->ex_var->va_type->ty_code == DT_ARRAY_OF;
	case ET_FUNCNAME:
		return TRUE;
	default:
		return FALSE;
	}
}

static initialiser_t *
make_scalar_initialiser(name, type, ie, what, is_local)
const char *name;
type_t *type;
initexpr_t *ie;
const char *what;
bool is_local;
{
	initialiser_t *in;
	expr_t *expr, *addr_expr;
	long offset;

	if (ie->ie_is_list) {
		if (ie->ie_list->ie_is_list || ie->ie_list->ie_next != NULL){
			diagf(ET_ERROR, initexpr_to_lexinfo(ie),
			      "Aggregate initialiser for scalar %s `%s'",
								what, name);
			return NULL;
		}
		expr = ie->ie_list->ie_expr;
	}
	else
		expr = ie->ie_expr;

	if (expr->ex_type == NULL)
		return NULL;

	ci_do_pointer_generation(expr);
	if (ci_do_assignment_conversions(type, expr) != 0) {
		diagf(ET_ERROR, expr->ex_lexinfo,
		      "Type clash in initialisation of %s `%s'", what, name);
		ci_show_type(type,          "   Expected type");
		ci_show_type(expr->ex_type, "Initialiser type");
		return NULL;
	}

	in = NEW(initialiser_t);

	if (is_local) {
		in->in_inittype = IT_EXPR;
		in->in_expr = ie->ie_expr;
		return in;
	}

	/*  BUG: we assume (wrongly) that all casts and promotions are nops.
	 */
	while (expr->ex_exprtype == ET_CAST || expr->ex_exprtype == ET_PROMOTION)
		expr = expr->ex_unary_expr->ue_expr;

	if (expr->ex_is_constant) {
		constval_t val;

		ci_evaluate_constant_expression(expr, "<initialiser>", FALSE, &val);
		switch (type->ty_code) {
		case TY_DOUBLE:
			in->in_floatval = val.cv_double;
			in->in_inittype = IT_FLOATVAL;
			break;
		case TY_FLOAT:
			in->in_floatval = val.cv_float;
			in->in_inittype = IT_FLOATVAL;
			break;
		default:
			in->in_inittype = IT_INTVAL;
			in->in_intval = val.cv_int;
			break;
		}
		return in;
	}

	/*  OK, it's not just a constant expression.  First zap matching
	 *  &* pairs.  This turns "&iob[2]" via "&*(iob + 2)" to "iob + 2".
	 */
	while (expr->ex_exprtype == ET_UNARY &&
	       expr->ex_unary_expr->ue_op == OP_ADDRESS_OF &&
	       expr->ex_unary_expr->ue_expr->ex_exprtype == ET_UNARY &&
	       expr->ex_unary_expr->ue_expr->ex_unary_expr->ue_op == OP_DEREF)
		expr = expr->ex_unary_expr->ue_expr->ex_unary_expr->ue_expr;
	
	addr_expr = NULL;

	offset = 0;
	if (is_address_expr(expr))
		addr_expr = expr;
	else if (expr->ex_exprtype == ET_BINARY) {
		binary_expr_t *be;
		expr_t *offset_expr;
		
		offset_expr = NULL; /* to satisfy gcc */
		be = expr->ex_binary_expr;
		if (be->be_op == OP_PLUS || be->be_op == OP_MINUS) {
			if (is_address_expr(be->be_left)) {
				addr_expr = be->be_left;
				offset_expr = be->be_right;
			}
			else if (is_address_expr(be->be_right)) {
				addr_expr = be->be_right;
				offset_expr = be->be_left;
			}
		}
		if (addr_expr != NULL) {
			constval_t val;

			if (!ci_evaluate_constant_expression(offset_expr,
						   "initialiser address offset",
						   TRUE, &val))
				return NULL;
			offset = val.cv_int;
		}
	}

	if (addr_expr != NULL) {
		switch (addr_expr->ex_exprtype) {
		case ET_VAR:
			if (addr_expr->ex_var->va_type->ty_code != DT_ARRAY_OF)
				ci_panic("var botch in msi");
			in->in_inittype = IT_VARADDR;
			in->in_addrvar = addr_expr->ex_var;
			in->in_offset = offset;
			break;
		case ET_FUNCNAME:
			in->in_inittype = IT_FUNCADDR;
			in->in_func_lexinfo = addr_expr->ex_lexinfo;
			in->in_func_name = addr_expr->ex_var->va_name;
			break;
		case ET_UNARY:
			if (addr_expr->ex_unary_expr->ue_op != OP_ADDRESS_OF &&
			    addr_expr->ex_unary_expr->ue_expr->ex_exprtype != ET_VAR)
				ci_panic("addr botch in msi");
			in->in_inittype = IT_VARADDR;
			in->in_addrvar = addr_expr->ex_unary_expr->ue_expr->ex_var;
			in->in_offset = offset;
			break;
		case ET_STRING_CONST:
			in->in_inittype = IT_STRINGVAL;
			in->in_stringval = addr_expr->ex_string_constant_val;
			in->in_offset = offset;
			break;
		default:
			ci_panic("exprtype botch in msi");
		}
		return in;
	}

	/*  Give an error message
	 */
	{
		char *s;
		constval_t junk;

		s = strf("Initialiser for %s %s", what, name);
		ci_evaluate_constant_expression(expr, s, FALSE, &junk);
		free(s);
	}

	return NULL;
}

static initialiser_t *
make_initialiser(name, type, ie, what, is_local, p_excess)
const char *name;
type_t *type;
initexpr_t *ie;
const char *what;
bool is_local;
initexpr_t **p_excess;
{
	initexpr_t *excess;
	initialiser_t *in;
	typecode_t code;

	code = type->ty_code;

	if (code == TY_U_STRUCT || code == TY_U_UNION || code == TY_U_ENUM) {
		diagf(ET_ERROR, initexpr_to_lexinfo(ie),
			"Can't initialise %s `%s'",
					ci_basetype_name(type), name);
		return NULL;
	}

	if (code == DT_ARRAY_OF || code == TY_STRUCT || code == TY_UNION) {
		initexpr_t *list;

		list = (ie->ie_is_list) ? ie->ie_list : ie;

		if (code == DT_ARRAY_OF)
			in = make_array_initialiser(name, type, list, is_local,
									  &excess);
		else
			in = make_struct_or_union_initialiser(type, list, is_local,
									  &excess);

		if (ie->ie_is_list) {
			if (excess != NULL) {
				diagf(ET_WARNING, initexpr_to_lexinfo(excess),
				   "Excess initialisers for aggregate %s `%s'",
								what, name);
			}
			excess = ie->ie_next;
		}
	}
	else {
		in = make_scalar_initialiser(name, type, ie, what, is_local);
		excess = ie->ie_next;
	}

	*p_excess = excess;

	return in;
}

initlist_t *
ci_make_var_initialiser(v, ie)
var_t *v;
initexpr_t *ie;
{
	typecode_t code;
	initexpr_t *junk;
	initialiser_t *in;

	code = v->va_type->ty_code;

	if (((code == TY_STRUCT || code == TY_UNION) && !ie->ie_is_list) ||
	    (code == DT_ARRAY_OF && !ie->ie_is_list &&
				 ie->ie_expr->ex_exprtype != ET_STRING_CONST)) {
		diagf(ET_ERROR, ie->ie_expr->ex_lexinfo,
			"Scalar initialiser for aggregate variable `%s'",
								v->va_name);
		return NULL;
	}

	in =  make_initialiser(v->va_name, v->va_type, ie, "variable",
						v->va_class == CL_AUTO, &junk);
	if (in != NULL) {
		initlist_t *il;

		v->va_flags |= VA_HAS_INITIALISER;
		il = NEW(initlist_t);
		il->il_var = v;
		il->il_fil = NULL;	/* filled in later by ci_end_parse_tree */
		il->il_initialiser_id = (initialiser_id_t)in;
		il->il_next = NULL;
		return il;
	}
	return NULL;
}
