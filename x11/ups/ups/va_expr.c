/* va_expr.c - C interpreter expressions in the stack trace */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_va_expr_c_sccsid[] = "@(#)va_expr.c	1.15 15/9/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <local/ukcprog.h>
#include <mtrprog/alloc.h>
#include <local/wn.h>
#include <local/obj/obj.h>
#include <local/obj/fed.h>

#include "objtypes.h"
#include "ups.h"
#include "symtab.h"
#include "ci.h"
#include "ui.h"
#include "va.h"
#include "data.h"
#include "exec.h"	/* for SET_EXPR_RESULT_ADDR */
#include "obj_stack.h"
#include "menudata.h"
#include "va_priv.h"
#include "tdr.h"

/*  Structure representing a displayed expression
 */
typedef struct dexprst {
	parse_id_t de_parse_id;
	code_id_t de_code_id;
	block_t *de_block;
	type_t *de_type;
	int de_typelevel;
	typecode_t de_basecode;
	type_t *de_aggrtype;
	value_t de_value;
	vformat_t de_format;
	bool de_val_changed;
	struct dexprst *de_nextfree;	/* for free list management only */
} dexpr_t;

static void expr_keyfunc PROTO((struct edescst *edesc));
static int expr_quitfunc PROTO((struct edescst *edesc, int n_tries));
static void expr_edit PROTO((struct drawst dets));
static ci_exec_result_t set_expr_result PROTO((code_id_t code_id, taddr_t addr,
					       taddr_t *args,
					       int nwords, taddr_t *p_res));
static const char *exprval_to_string PROTO((dexpr_t *de, ci_exec_result_t res));
static void expr_draw PROTO((draw_t *dets));
static dexpr_t *dup_dexpr PROTO((dexpr_t *de));
static dexpr_t *make_dexpr PROTO((block_t *bl));
static int compile_dexpr_code PROTO((dexpr_t *de, const char *estr, int *p_cnum));
static int nowrite PROTO((taddr_t unused_addr, const char *unused_buf,
								int unused_nbytes));
static bool is_hex_digit PROTO((int c));

char Expr_format[] = "%[-]60cx%[-]30cs\n";

/*  These are used to pass back the result of an expression from
 *  the builtin function $set_expr_result() to update_var().
 */
static type_t *Expr_type;
static value_t Expr_value;

#define FN_EXPR_STR	0
#define FN_EXPR_VAL	1

#define MAX_EXPR_LEN	250

fdef_t Expr_fdefs[] = {
	'x', expr_draw, expr_edit, var_or_expr_getwidth,
	'\0', NULL, NULL, NULL,
};

fnamemap_t Expr_fnamemap[] = {
	"expression",	FN_EXPR_STR,
	NULL,		0,
};

ALLOC_NEW_FREE(static,dexpr_t,dexpr,de_nextfree)

taddr_t
var_or_expr_addr(obj)
objid_t obj;
{
	dexpr_t *de;

	switch (get_object_type(obj)) {
	case OT_VAR:
		return dvar_addr((dvar_t *)obj);
	case OT_EXPR:
		break;
	default:
		panic("objtype botch in vea");
	}

	de = (dexpr_t *)obj;

	if (de->de_type->ty_code != DT_PTR_TO)
		panic("vea on non ptr");

	return de->de_value.vl_addr;
}

/*  Return TRUE iff par is a structure and name is the name of a member
 *  of the structure.
 *
 *  If this is a the case, point *p_v at the var representing the structure
 *  member, and *p_addr to the address of the parent structure.
 *  Also fully dereference the parent.
 */
int
get_member_of_aggr(par, name, p_v, p_addr)
objid_t par;
const char *name;
var_t **p_v;
taddr_t *p_addr;
{
	var_t *v;
	type_t *btype;
	int objtype;

	switch (objtype = get_object_type(par)) {
	case OT_VAR:
		btype = get_basetype(((dvar_t *)par)->dv_var->va_type);
		break;
	case OT_EXPR:
		btype = ((dexpr_t *)par)->de_type;
		if (btype->ty_code == DT_PTR_TO)
			btype = btype->ty_base;
		break;
	default:
		panic("bad objtype in gma");
		btype = NULL;	/* to satisfy gcc */
	}

	if (btype->ty_code == TY_STRUCT || btype->ty_code == TY_UNION) {
		for (v = btype->ty_aggr_or_enum->ae_aggr_members; v != NULL; v = v->va_next) {
			if (strcmp(v->va_name, name) == 0) {
				*p_v = v;
				if (objtype == OT_VAR)
					deref_aggr((dvar_t *)par);
				*p_addr = var_or_expr_addr(par);
				return TRUE;
			}
		}
	}
	return FALSE;
}

void
expr_getcolor(obj, p_fg, p_bg)
objid_t obj;
short *p_fg, *p_bg;
{
	get_value_colors(((dexpr_t *)obj)->de_val_changed, p_fg, p_bg);
}

static void
expr_draw(dets)
draw_t *dets;
{
	const char *s;

	s = (const char *)dets->dr_fval;
	wn_ttext(dets->dr_wn, s, dets->dr_x, dets->dr_y, dets->dr_fg, dets->dr_bg);
	dets->dr_width = get_decl_len(strlen(s)) * wn_get_sysfont()->ft_width;
}

static bool
is_hex_digit(c)
int c;
{
	return c != '\0' && (isdigit(c) || strchr("abcdefABCDEF", c) != NULL);
}

static void
expr_keyfunc(edesc)
edesc_t *edesc;
{
	const char *pos, *endnum, *fmt;
	bool up, seen_hex_digit, non_octal;
	int radix, inc, dig_offset, i, junk_cnum;
	long origval;
	char buf[MAX_EXPR_LEN];

	if (edesc->ed_meaning != EDM_INSERT_CHAR &&
					edesc->ed_meaning != EDM_ILLEGAL_CHAR)
		return;
	
	switch (edesc->ed_char) {
	case CONTROL('K'):
		up = FALSE;
		break;
	case CONTROL('J'):
		up = TRUE;
		break;
	default:
		return;
	}

	pos = edesc->ed_copy + edesc->ed_curpos;

	seen_hex_digit = non_octal = FALSE;
	for (dig_offset = 0; is_hex_digit(pos[dig_offset]); ++dig_offset) {
		if (pos[dig_offset] == '8' || pos[dig_offset] == '9')
			non_octal = TRUE;
		else if (!isdigit(pos[dig_offset]))
			seen_hex_digit = TRUE;
	}
	endnum = pos + dig_offset;

	while (pos > edesc->ed_copy && is_hex_digit(pos[-1])) {
		--pos;
		if (*pos == '8' || *pos == '9')
			non_octal = TRUE;
		if (!isdigit(*pos))
			seen_hex_digit = TRUE;
	}
	
	/*  Determine the radix of the number.
	 */
	if (pos > edesc->ed_copy + 1 && strncmp(pos - 2, "0x", 2) == 0) {
		pos -= 2;
		radix = 16;
		fmt = "0x%lx";
	}
	else if (*pos == '0' && isdigit(pos[1])) {
		radix = 8;
		fmt = "0%lo";
	}
	else {
		radix = 10;
		fmt = "%ld";
	}

	if (isalpha(*endnum) || !is_hex_digit(*pos) ||
		     (seen_hex_digit && radix != 16) || (non_octal && radix == 8)) {
		errf("Not pointing at a number");
		edesc->ed_meaning = EDM_CONT;
		return;
	}

	if (pos > edesc->ed_copy && pos[-1] == '-')
		--pos;
	
	inc = 1;
	for (i = 0; i < dig_offset; ++i)
		inc *= radix;	

	origval = strtol(pos, (char **)NULL, 0);

	sprintf(buf, "%.*s", pos - edesc->ed_copy, edesc->ed_copy);
	sprintf(buf + strlen(buf), fmt, origval + (up ? inc : -inc));
	strcat(buf, endnum);
	
	/*  Point pos at the end of the number in the new string.
	 */
	pos = buf + (pos - edesc->ed_copy);
	if (*pos == '-')
		++pos;
	if (radix == 16)
		pos += 2;
	while (is_hex_digit(*pos))
		++pos;

	/*  Try to maintain the same number of digits as before after
	 *  the marker bar.
	 */
	while (pos > buf && is_hex_digit(pos[-1]) && dig_offset > 0) {
		--dig_offset;
		--pos;
	}

	edesc->ed_curpos = pos - buf;
	ed_newstring(edesc, buf);

	wn_updating_off(edesc->ed_wn);
	ed_cursor(edesc, EDC_CURSOR_OFF);
	change_field((objid_t)edesc->ed_user, FN_EXPR_STR, edesc->ed_copy);
	ed_new_orig(edesc, get_field_value((objid_t)edesc->ed_user, FN_EXPR_STR));

	compile_dexpr_code((dexpr_t *)edesc->ed_user, edesc->ed_copy, &junk_cnum);
	update_expr((objid_t)edesc->ed_user, TRUE);

	ed_cursor(edesc, EDC_CURSOR_ON);
	wn_updating_on(edesc->ed_wn);

	edesc->ed_meaning = EDM_CONT;
}

static int
expr_quitfunc(edesc, n_tries)
struct edescst *edesc;
int n_tries;
{
	dexpr_t *de;
	int cnum, res;

	if (strcmp(edesc->ed_orig, edesc->ed_copy) == 0)
		return EDR_CONFIRM_NO_CHANGE;

	if (message_on_null_name(edesc, "expression") == -1)
		return force_quit(n_tries);

	de = (dexpr_t *)edesc->ed_user;
	if (compile_dexpr_code(de, edesc->ed_copy, &cnum) != 0) {
		res = force_quit(n_tries);
		if (cnum != -1 && res != EDR_CANCEL_AND_QUIT) {
			edesc->ed_meaning = EDM_SETCURSOR;
			edesc->ed_newpos = cnum;
			do_edit(edesc, EDM_SETCURSOR);
		}
		return res;
	}

	td_set_obj_updating(OBJ_UPDATING_OFF);
	change_field((objid_t)edesc->ed_user, FN_EXPR_STR, edesc->ed_copy);
	update_expr((objid_t)de, TRUE);
	td_set_obj_updating(OBJ_UPDATING_ON);

	return EDR_CONFIRM_CHANGE;
}

static int
compile_dexpr_code(de, estr, p_cnum)
dexpr_t *de;
const char *estr;
int *p_cnum;
{
	compile_res_t *cr;
	lexinfo_t lxbuf;

	cr = compile_code(&estr, 1, de->de_block, (char *)de, &lxbuf,
			  "void $start(void) { $set_expr_value((", ")); }",
			  (const char *)NULL, (const char *)NULL);

	if (cr->cr_code_has_func_calls) {
		errf("Can't call target functions in expressions");
		free_parse_and_code_ids(cr->cr_parse_id, cr->cr_code_id);
		*p_cnum = -1;
		return -1;
	}

	if (cr->cr_code_id == NULL || cr->cr_parse_id == NULL) {
		free_parse_and_code_ids(cr->cr_parse_id, cr->cr_code_id);
		*p_cnum = lxbuf.lx_cnum;
		return -1;
	}

	free_parse_and_code_ids(de->de_parse_id, de->de_code_id);

	de->de_code_id = cr->cr_code_id;
	de->de_parse_id = cr->cr_parse_id;

	return 0;
}

static void
expr_edit(fdets)
struct drawst fdets;
{
	dexpr_t *de;
	edesc_t edescbuf;

	de = (dexpr_t *)fdets.dr_code;

	clear_selection();
	make_edesc(&edescbuf, fdets.dr_wn, (const char *)fdets.dr_fval,
						MAX_EXPR_LEN,
						fdets.dr_fg, fdets.dr_bg);

	/*  BUG: must think of a better way to do this.
	 */
	if (fdets.dr_fval[0] == '\0' && fdets.dr_wn != -1) {
		edescbuf.ed_meaning = EDM_SETCURSOR;
		edescbuf.ed_newpos = 0;
		do_edit(&edescbuf, EDM_SETCURSOR);
	}

	edescbuf.ed_user = (int)de;
	edescbuf.ed_quitfunc = (ed_quitfunc_t)expr_quitfunc;
	edescbuf.ed_keyfunc = (ed_keyfunc_t)expr_keyfunc;
	suppress_ta_cursor_then_edit_field(&edescbuf, "expression");
}

const char *
expr_getobjname(obj)
objid_t obj;
{
	static char *last = NULL;
	const char *name;
	objid_t obj2;
	int count;

	name = get_field_value(obj, FN_EXPR_STR);

	count = 1;
	obj2 = get_code(get_code(obj, OBJ_PARENT), OBJ_CHILD);

	for (; obj2 != NULL; obj2 = get_code(obj2, OBJ_NEXT)) {
		if (obj2 == obj)
			break;
		if (get_object_type(obj2) != OT_EXPR)
			continue;
		if (strcmp(name, get_field_value(obj2, FN_EXPR_STR)) == 0)
			++count;
	}

	if (last != NULL)
		free(last);

	if (count == 1) {
		last = NULL;
		return name;
	}

	last = strf("@%d:%s", count, name);
	return last;
}

int
expr_dumpobj(arg, code, level)
char *arg;
objid_t code;
int level;
{
	return td_outf(level, "%20s %s", get_field_value(code, FN_EXPR_STR),
					 get_field_value(code, FN_EXPR_VAL));
}

/*  Process the return from a var menu.
 */
void
do_expr(obj, cmd)
objid_t obj;
int cmd;
{
	dexpr_t *de;

	de = (dexpr_t *)obj;

	switch(cmd) {
	case MR_VAR_STRING:
	    {
		int oldstate;
		typecode_t typecode;

		if (de->de_type == NULL) {
			errf("Can't display illegal expression as string");
			break;
		}

		if (de->de_type->ty_code != DT_PTR_TO)
			typecode = TY_NOTYPE;
		else
			typecode = de->de_type->ty_base->ty_code;

		if (typecode != TY_CHAR && typecode != TY_UCHAR) {
			errf("Can't display expression as a string");
			break;
		}

		de->de_format = DF_STRING;
		oldstate = td_set_obj_updating(OBJ_UPDATING_OFF);
		update_expr(obj, TRUE);
		td_set_obj_updating(oldstate);
		break;
	    }
		break;
	case MR_VAR_SIGNED_DECIMAL:
	case MR_VAR_UNSIGNED_DECIMAL:
	case MR_VAR_SIGNED_OCTAL:
	case MR_VAR_UNSIGNED_OCTAL:
	case MR_VAR_SIGNED_HEX:
	case MR_VAR_UNSIGNED_HEX:
	case MR_VAR_UNSIGNED_BINARY:
	case MR_VAR_ASCII_BYTE:
	    {
		int oldstate;

		de->de_format = mval_to_vformat(cmd);
		oldstate = td_set_obj_updating(OBJ_UPDATING_OFF);
		update_expr(obj, TRUE);
		td_set_obj_updating(oldstate);
		break;
	    }
	case MR_VAR_DEREF:
		errf("Can't increase the indirection level of an expression");
		break;
	case MR_VAR_ADDRESS:
		errf("Can't decrease the indirection level of an expression");
		break;
	case MR_VAR_EXPAND:
		if (de->de_type == NULL) {
			errf("Can't expand illegal expression");
			break;
		}
		if (de->de_type->ty_code != DT_PTR_TO) {
			errf("Expression is not a struct or union pointer");
			break;
		}

		switch(de->de_type->ty_base->ty_code) {
		case TY_STRUCT:
		case TY_UNION:
		    {
			var_t *v;
			v = de->de_type->ty_base->ty_aggr_or_enum->ae_aggr_members;
			for (; v != NULL; v = v->va_next) {
				if (find_var((objid_t)de, v) == NULL)
					add_var_object(obj, v, OBJ_FIRST_CHILD);
			}
			break;
		    }
		case TY_U_STRUCT:
			errf("Can't expand undefined structure");
			break;
		case TY_U_UNION:
			errf("Can't expand undefined structure");
			break;
		default:
			errf("Expression is not a struct or union pointer");
			break;
		}

		break;
	case MR_VAR_DUP:
		add_to_new_selection((objid_t)dup_dexpr(de));
		break;
	case MR_VAR_DELETE:
		remove_object(obj, OBJ_DESCENDENTS);
		remove_object(obj, OBJ_SELF);
		break;
	case MR_VAR_COLLAPSE:
		remove_object(obj, OBJ_CHILDREN);
		break;
	case MR_VAR_COLLAPSE_COMPLETELY:
		remove_object(obj, OBJ_DESCENDENTS);
		break;
	case MR_VAR_WANT_TYPEDEFS:
	case MR_VAR_NO_TYPEDEFS:
		/*  Typedefs don't apply to expressions, as the declaration
		 *  is supplied by the user.
		 */
		break;
	default:
		panic("bad cmd in de");
	}
}

static dexpr_t *
dup_dexpr(de)
dexpr_t *de;
{
	dexpr_t *newde;
	char *estr;
	int junk_cnum;

	newde = make_dexpr(de->de_block);

	/*  Note that we must create the new object before calling
	 *  compile_dexpr_code because that eventually calls
	 *  get_regaddr_for_ci() which needs the object to exist.
	 */
	new_object((objid_t)newde, OT_EXPR, (objid_t)de, OBJ_AFTER);

	estr = (char *)get_field_value((objid_t)de, FN_EXPR_STR);
	if (compile_dexpr_code(newde, estr, &junk_cnum) != 0)
		panic("cdc botch in dd");

	set_field_value((objid_t)newde, FN_EXPR_STR, (fval_t)strsave(estr));
	set_field_value((objid_t)newde, FN_EXPR_VAL, (fval_t)strsave("0"));

	/*  We want the same format for the new expression as for the old.
	 *  update_expr() will reset the format to the default if it sees
	 *  a change in basecode or typelevel so copy these as well.
	 */
	newde->de_basecode = de->de_basecode;
	newde->de_typelevel = de->de_typelevel;
	newde->de_format = de->de_format;

	update_expr((objid_t)newde, TRUE);

	return newde;
}

static dexpr_t *
make_dexpr(bl)
block_t *bl;
{
	dexpr_t *de;

	de = new_dexpr();

	de->de_parse_id = NULL;
	de->de_code_id = NULL;
	de->de_block = bl;
	de->de_type = NULL;
	de->de_basecode = TY_NOTYPE;
	de->de_typelevel = 0;
	de->de_aggrtype = NULL;
	/* Don't set de_format as we don't have a type yet */
	de->de_val_changed = FALSE;
	de->de_nextfree = NULL;		/* for safety */

	return de;
}

void
add_expr_object(par, bl, language)
objid_t par;
block_t *bl;
language_t language;
{
	dexpr_t *de;
	objid_t obj;

	if (language != LANG_C) {
		errf("Sorry, can only add expressions in C");
		return;
	}

	de = make_dexpr(bl);
	obj = (objid_t)de;

	td_set_obj_updating(OBJ_UPDATING_OFF);
	new_object(obj, OT_EXPR, par, OBJ_FIRST_CHILD);
	set_field_value(obj, FN_EXPR_STR, (fval_t)strsave(""));
	set_field_value(obj, FN_EXPR_VAL, (fval_t)strsave("0"));
	ensure_visible(obj);
	td_set_obj_updating(OBJ_UPDATING_ON);

	td_obj_edit_field(obj, FN_EXPR_STR, 0, 0);
	if (de->de_code_id == NULL)
		remove_object(obj, OBJ_SELF);
}

void
free_displayed_expr(obj)
objid_t obj;
{
	dexpr_t *de;

	de = (dexpr_t *)obj;
	free_parse_and_code_ids(de->de_parse_id, de->de_code_id);
	free_dexpr((dexpr_t *)obj);
}

static int
nowrite(unused_addr, unused_buf, unused_nbytes)
taddr_t unused_addr;
const char *unused_buf;
int unused_nbytes;
{
	return -1;
}

void
update_expr(obj, change_caused_by_edit)
objid_t obj;
bool change_caused_by_edit;
{
	taddr_t fp, ap;
	dexpr_t *de;
	bool lose_descendents;
	type_t *type;
	typecode_t basecode;
	ci_exec_result_t res;
	int typelevel;
	objid_t par;
	const char *newval;

	de = (dexpr_t *)obj;

	if (de->de_code_id == NULL) {
		change_field(obj, FN_EXPR_VAL, "<syntax error>");

		/*  This change is always caused by an edit.
		 */
		de->de_val_changed = FALSE;

		return;
	}

	par = get_code(obj, OBJ_PARENT);
	if (ups_get_object_type(par) == OT_SFILE)
		fp = ap = 0;
	else
		get_stack_func(par, &fp, &ap);

	Expr_type = NULL;
	ci_initialise_code(de->de_code_id, TRUE);

	res = ci_execute_code(de->de_code_id, fp, ap,
					dread, nowrite, set_expr_result);

	if (res != CI_ER_TRAP) {
		switch (res) {
		case CI_ER_READDATA_FAILED:
		case CI_ER_DIVISION_BY_ZERO:
			break;
		default:
			errf("%s in expression", ci_exec_result_to_string(res));
			break;
		}
		Expr_type = NULL;
	}

	basecode = TY_NOTYPE;

	typelevel = 0;
	for (type = Expr_type; type != NULL; type = type->ty_base) {
		basecode = type->ty_code;
		++typelevel;
	}

	if (basecode != de->de_basecode || de->de_typelevel != typelevel) {
		if (Expr_type == NULL)
			de->de_format = DF_NONE;
		else if (Expr_type->ty_code == DT_PTR_TO &&
		         Expr_type->ty_base->ty_code == TY_CHAR)
			de->de_format = DF_STRING;
		else
			de->de_format = default_format(Expr_type, Expr_type);
	}

	de->de_type = Expr_type;
	de->de_typelevel = typelevel;
	de->de_basecode = basecode;
	de->de_value = Expr_value;

	lose_descendents = TRUE;
	if (de->de_type == NULL)
		de->de_aggrtype = NULL;
	else if (de->de_type->ty_code == DT_PTR_TO) {
		type_t *aggrtype;

		aggrtype = de->de_type->ty_base;
		if (aggrtype->ty_code == TY_STRUCT ||
						aggrtype->ty_code == TY_UNION) {
			if (de->de_aggrtype != aggrtype)
				de->de_aggrtype = aggrtype;
			else
				lose_descendents = FALSE;
		}
		else
			de->de_aggrtype = NULL;
	}
			
	if (lose_descendents)
		remove_object(obj, OBJ_DESCENDENTS);

	newval = exprval_to_string(de, res);
	de->de_val_changed = change_field(obj, FN_EXPR_VAL, newval) &&
							!change_caused_by_edit;
	
	if (de->de_aggrtype != NULL)
		update_struct(obj, change_caused_by_edit);
}

	
/*  Construct a value string for the displayed expression de.
 */
static const char *
exprval_to_string(de, res)
dexpr_t *de;
ci_exec_result_t res;
{
	value_t vl;
	type_t *type;
	static char buf[128];

	type = de->de_type;
	vl = de->de_value;

	switch (res) {
	case CI_ER_TRAP:
		break;
	case CI_ER_WRITEDATA_FAILED:
		return "<illegal data modification>";
		break;
	case CI_ER_READDATA_FAILED:
		return "<bad address>";
		break;
	default:
		strnf(buf, sizeof(buf), "<%s>", ci_exec_result_to_string(res));
		return buf;
	}

	if (type == NULL)
		return "<illegal expression type>";

	switch (type->ty_code) {
	case DT_ARRAY_OF:
	case DT_PTR_TO:
		addr_to_string(buf, sizeof(buf) - 1, vl.vl_addr,
								type, de->de_format);
		break;
	case TY_INT:
	case TY_LONG:
		int_to_string(buf, sizeof(buf), vl.vl_int, de->de_format);
		break;
	case TY_UINT:
	case TY_ULONG:
		int_to_string(buf, sizeof(buf), vl.vl_int, de->de_format);
		break;
	case TY_FLOAT:
		strcpy(buf, get_real(vl, FALSE, TRUE));
		break;
	case TY_DOUBLE:
		strcpy(buf, get_real(vl, FALSE, FALSE));
		break;
	case TY_ENUM:
	case TY_U_ENUM:
		enumval_to_string(buf, sizeof(buf), vl.vl_int, type);
		break;
	default:
		panic("unknown type in es");
	}

	return buf;
}

static ci_exec_result_t
set_expr_result(code_id, addr, args, nwords, p_res)
code_id_t code_id;
taddr_t addr;
taddr_t *args;
int nwords;
taddr_t *p_res;
{
	int argslots;

	if (addr != SET_EXPR_RESULT_ADDR)
		panic("bad func in ser");
	if (Expr_type != NULL)
		panic("dup call of ser");
	if (nwords < 1)
		panic("bad nwords in ser");

	Expr_type = (type_t *)args[nwords];

	switch (Expr_type->ty_code) {
	case TY_FLOAT:
		argslots = sizeof(float) / sizeof(long);
		Expr_value.vl_double = *(float *)args;
		break;
	case TY_DOUBLE:
		argslots = sizeof(double) / sizeof(long);
		Expr_value.vl_double = *(double *)args;
		break;
	case DT_ARRAY_OF:
	case DT_PTR_TO:
		argslots = sizeof(char *) / sizeof(long);
		Expr_value.vl_addr = args[0];
		break;
	case TY_INT:
	case TY_UINT:
	case TY_LONG:
	case TY_ULONG:
	case TY_ENUM:
	case TY_U_ENUM:
		argslots = 1;
		Expr_value.vl_int = args[0];
		break;
	case TY_STRUCT:
	case TY_UNION:
	case TY_U_UNION:
	case TY_U_STRUCT:
		errf("Can't display struct/union types");
		argslots = typesize(Expr_type) /sizeof(int);
		Expr_type = NULL;
		break;
	default:
		panic("unknown typecode in ser");
		argslots = 0;	/* to satisfy gcc */
		break;
	}

	if (argslots != nwords)
		panic("argslots botch in ser");
	
	return CI_ER_CONTINUE;
}
