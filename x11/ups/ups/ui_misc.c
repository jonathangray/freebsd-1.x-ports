/* ui_misc.c - miscellaneous user interface type support functions */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_ui_misc_c_sccsid[] = "@(#)ui_misc.c	1.11 15/9/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <local/wn.h>
#include <local/obj/obj.h>

#include <local/ukcprog.h>
#include <local/obj/newtb.h>
#include <local/obj/fed.h>

#include "ups.h"
#include "debug.h"
#include "objtypes.h"
#include "symtab.h"
#include "ci.h"
#include "ui.h"
#include "ui_priv.h"
#include "exec.h"
#include "data.h"
#include "obj_stack.h"
#include "state.h"
#include "tdr.h"

typedef enum {
	GS_BEFORE_LINES,
	GS_IN_LINES,
	GS_AT_END_OF_LINES,
	GS_AFTER_LINES,
	GS_EOF
} getline_state_t;

static const char *getline_from_nlines PROTO((char *arg));
static bool report_error PROTO((const char *filename, int lnum, int cnum,
								const char *mesg));
static bool checkarg PROTO((type_t *type, int nargs, int argn, long *p_val));
static ci_nametype_t getaddr PROTO((const char *name, taddr_t *p_addr));
static unsigned long get_regaddr_for_ci PROTO((char *arg, int reg));

/*  Arguments for getline_from_nlines.
 */
typedef struct getline_from_nlines_argst {
	getline_state_t ge_state;
	const char **ge_lines;
	int ge_nlines;
	int ge_lnum;
	const char *ge_firstline;
	const char *ge_lastline;
	const char *ge_repl_text;
	const char *ge_repl_val;
} getline_from_nlines_arg_t;

static bool Func_addr_requested;
static lexinfo_t *Error_lexinfo;

const char Stop_funcname[] = "$st";

static const char *
getline_from_nlines(arg)
char *arg;
{
	getline_from_nlines_arg_t *ge;
	const char *line, *cptr;
	bool need_copy;
	int len;

	ge = (getline_from_nlines_arg_t *)arg;

	switch (ge->ge_state) {
	case GS_BEFORE_LINES:
		line = ge->ge_firstline;
		ge->ge_state = GS_IN_LINES;
		break;
	case GS_IN_LINES:
		line = ge->ge_lines[ge->ge_lnum];
		if (++ge->ge_lnum == ge->ge_nlines)
			ge->ge_state = GS_AT_END_OF_LINES;
		break;
	case GS_AT_END_OF_LINES:
		line = ge->ge_lastline;
		ge->ge_state = GS_AFTER_LINES;
		break;
	case GS_AFTER_LINES:
		line = NULL;
		ge->ge_state = GS_EOF;
		break;
	case GS_EOF:
		panic("getline_from_lines called after EOF");
		line = NULL; /* to satisfy gcc */
		break;
	default:
		panic("bad state in getline_from_nlines");
		line = NULL; /* to satisfy gcc */
		break;
	}

	if (line == NULL || ge->ge_repl_text == NULL)
		return line;

	need_copy = FALSE;
	len = strlen(ge->ge_repl_text);
	for (cptr = line; *cptr != '\0'; ++cptr) {
		if (strncmp(cptr, ge->ge_repl_text, len) == 0) {
			need_copy = TRUE;
			break;
		}
	}

	if (need_copy) {
		static char *buf;
		static int bufsize = 0;
		const char *src;
		char *dst;

		if (bufsize <= strlen(line)) {
			if (bufsize != 0)
				free(buf);
			bufsize = strlen(line) + 1;
			buf = e_malloc(bufsize);
		}

		src = line;
		dst = buf;
		while (*src != '\0') {
			if (strncmp(src, ge->ge_repl_text, len) == 0 &&
			    !isalnum(src[len]) &&
			    src[len] != '_' && src[len] != '$') {
				sprintf(dst, "%s()", ge->ge_repl_val);
				dst += len;
				src += len;
			}
			else
				*dst++ = *src++;
		}
		*dst = '\0';
		
		line = buf;
	}

	return line;
}

static bool
report_error(filename, lnum, cnum, mesg)
const char *filename;
int lnum, cnum;
const char *mesg;
{
	if (strcmp(mesg, ci_Illegal_return_from_start_message) == 0)
		mesg = "Illegal return statement in interpreted code";
	errf("%s", mesg);

	/*  We subtract one because error messages are numbered 1.. and we
	 *  and we want to number 0.. when reporting.  We subtract another 1
	 *  to adjust for the extra line we insert in front of the code
	 *  (see ge_firstline in getline_from_nlines()).
	 */
	lnum -= 2;

	Error_lexinfo->lx_filename = filename;
	Error_lexinfo->lx_lnum = lnum;
	Error_lexinfo->lx_cnum = cnum;

	return TRUE;
}

code_id_t
recompile_code(parse_id, code_id, grp_arg)
parse_id_t parse_id;
code_id_t code_id;
char *grp_arg;
{
	ci_get_regaddr_proc_t get_regaddr_proc;

	get_regaddr_proc = grp_arg != NULL ? get_regaddr_for_ci : NULL;

	ci_free_code_id(code_id);
	code_id = ci_compile_program(parse_id,
				    (ci_report_error_func_t)NULL,
				    getaddr, checkarg,
				    regno_to_addr,
				    get_regaddr_proc, grp_arg,
				    CI_CP_DONT_PANIC | CI_CP_CHECK_DIV,
				    0, (const char **)NULL);

	if (Debug_flags & DBFLAG_SHOW_CI_CODE)
		ci_disassemble(parse_id, code_id);
	
	return code_id;
}

void
free_parse_and_code_ids(parse_id, code_id)
parse_id_t parse_id;
code_id_t code_id;
{
	if (code_id != NULL)
		ci_free_code_id(code_id);
	if (parse_id != NULL)
		ci_free_parse_id(parse_id);
}

compile_res_t *
compile_code(lines, nlines, block, grp_arg, error_lx,
					firstline, lastline, repl_text, repl_val)
const char **lines;
int nlines;
block_t *block;
char *grp_arg;
lexinfo_t *error_lx;
const char *firstline, *lastline;
const char *repl_text, *repl_val;
{
	static bool first_call = TRUE;
	static compile_res_t crbuf;
	getline_from_nlines_arg_t gebuf;

	if (first_call) {
		ci_add_message_action((char *)NULL, MA_WARNING_ONLY);
		ci_add_message_action("No prototype in scope", MA_IGNORE);
		ci_add_message_action("Implicit declaration of", MA_IGNORE);
		first_call = FALSE;
	}
	
	gebuf.ge_state = GS_BEFORE_LINES;
	gebuf.ge_firstline = firstline;
	gebuf.ge_lastline = lastline;
	gebuf.ge_repl_text = repl_text;
	gebuf.ge_repl_val = repl_val;
	gebuf.ge_lines = lines;
	gebuf.ge_nlines = nlines;
	gebuf.ge_lnum = 0;
	
	error_lx->lx_lnum = -1;
	Error_lexinfo = error_lx;

	crbuf.cr_parse_id = NULL;
	crbuf.cr_code_id = NULL;

	crbuf.cr_parse_id = ci_parse_file((parse_id_t)NULL, "<internal file>",
						block, CI_DONT_PANIC,
						report_error,
						resolve_untyped_name,
						getline_from_nlines,
						(char *)&gebuf);
	if (crbuf.cr_parse_id != NULL) {
		ci_get_regaddr_proc_t get_regaddr_proc;

		get_regaddr_proc = grp_arg != NULL ? get_regaddr_for_ci : NULL;
		Func_addr_requested = FALSE;
		crbuf.cr_code_id = ci_compile_program(crbuf.cr_parse_id,
					      report_error, getaddr,
					      checkarg, regno_to_addr,
					      get_regaddr_proc, grp_arg,
					      CI_CP_DONT_PANIC | CI_CP_CHECK_DIV,
					      0, (const char **)NULL);
		crbuf.cr_code_has_func_calls = Func_addr_requested;

		if (crbuf.cr_code_id != NULL &&
					(Debug_flags & DBFLAG_SHOW_CI_CODE) != 0)
			ci_disassemble(crbuf.cr_parse_id, crbuf.cr_code_id);
	}

	return &crbuf;
}

static bool
checkarg(type, nargs, argn, p_val)
type_t *type;
int nargs, argn;
long *p_val;
{
	*p_val = (long)type;
	return TRUE;
}

static unsigned long
get_regaddr_for_ci(arg, reg)
char *arg;
int reg;
{
	return get_reg_addr(get_code((objid_t)arg, OBJ_PARENT), reg);
}

static ci_nametype_t
getaddr(name, p_addr)
const char *name;
taddr_t *p_addr;
{
	fil_t *junk_fil;
	common_block_id_t junk_cblock;
	func_t *f;
	var_t *v;

	if (strcmp(name, "$set_expr_value") == 0) {
		*p_addr = (taddr_t)SET_EXPR_RESULT_ADDR;
		return CI_INDIRECT_LIBFUNC;
	}
	if (strcmp(name, "$st") == 0) {
		*p_addr = (taddr_t)STOP_ADDR;
		return CI_INDIRECT_LIBFUNC;
	}
	if (strcmp(name, "$printf") == 0) {
		*p_addr = (taddr_t)PRINTF_ADDR;
		return CI_INDIRECT_LIBFUNC;
	}

	if (find_global_by_name(name, get_displayed_fil(), (func_t *)NULL, TRUE,
					&f, &v, &junk_cblock, &junk_fil) != 0)
		return CI_UNDEFINED;

	if (f != NULL) {
		*p_addr = f->fu_addr;
		Func_addr_requested = TRUE;
		return CI_INDIRECT_LIBFUNC;
	}

	*p_addr = v->va_addr;
	return CI_DATA;
}

/*  Get the type of an object.  This is a wrapper round get_object_type()
 *  which maps OT_SFILE_EX to OT_SFILE, so source file objects all
 *  appear to have the same type regardless of whether they are
 *  expanded or not.
 */
int
ups_get_object_type(obj)
objid_t obj;
{
	int type;

	if ((type = get_object_type(obj)) == OT_SFILE_EX)
		type = OT_SFILE;
	return type;
}

/*  If obj is not on the display, move the display so that it is central.
 */
void
ensure_visible(obj)
objid_t obj;
{
	int junk, obj_y;
	
	if (td_have_window() && !visible(obj)) {
		get_position(obj, &junk, &obj_y, &junk, &junk);
		obj_y -= wn_get_height(get_display_area_wn()) / 2;
		display_from(0, (obj_y < 0) ? 0 : obj_y);
	}
}

bool
change_field(obj, fnum, new)
objid_t obj;
int fnum;
const char *new;
{
	char *old;

	old = (char *) get_field_value(obj, fnum);
	if (strcmp(old, new) != 0) {
		set_field_value(obj, fnum, (fval_t)strsave(new));
		free(old);
		return TRUE;
	}
	return FALSE;
}

/*  Scroll the object display by dist pixels and update the thumb bar.
 */
void
display_area_scroll(dist)
int dist;
{
	tbar_id_t tbar_id;

	tbar_id = 0;
	panic("can't get obj tb");
	tb_scroll(tbar_id, v_scroll(dist), FALSE);
}

/*  Move the object display.  Y is an offset from the start of the
 *  window containing the object scroll bar - the object display
 *  and thumb bar are updated to reflect this.
 */
void
display_area_goto(y)
int y;
{
	int disp_y, disp_wn;
	tbar_id_t tbar_id;

	tbar_id = 0;
	panic("can't get obj tb");
	disp_wn = get_display_area_wn();
	disp_y = tb_tbpos_to_unit(tbar_id, disp_wn, y, TRUE);
	display_from(0, disp_y);
	tb_goto(tbar_id, disp_y, FALSE);
}

/*  Called by all the *_quitfunc() functions. Allow the user to
 *  quit (thus cancelling the edit) if this is the second consecutive
 *  unsuccessful attempt to confirm the edit.
 */
int
force_quit(n_tries)
int n_tries;
{
	if (n_tries > 0) {
		/* message("+ (edit cancelled)"); */
		return EDR_CANCEL_AND_QUIT;
	}
	else
		return EDR_CANT_QUIT;
}

/*  Complain and return -1 of the ed_copy field of edesc is the empty
 *  string, otherwise return 0. field_type should be the name of the
 *  field - it is used in the error message.
 */
int
message_on_null_name(edesc, field_type)
struct edescst *edesc;
const char *field_type;
{
	if (*edesc->ed_copy == '\0') {
		errf("Zero length %s illegal", field_type);
		return -1;
	}
	return 0;
}

/*  Display a number.
 */
void
n_draw(dets)
struct drawst *dets;
{
	char buf[12];

	(void) sprintf(buf, "%d", (int) dets->dr_fval);
	wn_ttext(dets->dr_wn, buf, dets->dr_x, dets->dr_y, dets->dr_fg, dets->dr_bg);
}

/*  ed_keyfunc for unsigned numeric fields - map non digits to illegal
 *  characters.
 */
void
num_filter(edesc)
struct edescst *edesc;
{
	if (edesc->ed_meaning == EDM_INSERT_CHAR && !isdigit(edesc->ed_char))
		edesc->ed_meaning = EDM_ILLEGAL_CHAR;
}
