/* obj_bpt.c - breakpoint object handling */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_obj_bpt_c_sccsid[] = "@(#)obj_bpt.c	1.31 15/9/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <local/wn.h>
#include <local/obj/obj.h>

#include <local/ukcprog.h>
#include <local/obj/fed.h>

#include "objtypes.h"
#include "ups.h"
#include "symtab.h"
#include "breakpoint.h"
#include "ci.h"
#include "src.h"
#include "ui.h"
#include "text.h"
#include "exec.h"
#include "data.h"
#include "obj_bpt.h"
#include "menudata.h"
#include "state.h"
#include "debug.h"
#include "tdr.h"

typedef struct bpdescst {
	breakpoint_t bd_breakpoint;
	editblock_id_t bd_editblock_id;
	func_t *bd_func;
	int bd_lnum;
	bool bd_code_bad;
	bool bd_data_needs_initialising;
	bool bd_code_has_func_calls;
	parse_id_t bd_parse_id;
	code_id_t bd_code_id;
} bpdesc_t;

#define HAS_CODE(bd)	((bd)->bd_parse_id != NULL || (bd)->bd_code_bad)

static void bpfunc_draw PROTO((struct drawst *dets));
static int move_bpt PROTO((objid_t obj, const char *newstring, int edit_lnum));
static void show_bpt_source PROTO((bpdesc_t *bd, bool beep_on_failure));
static int bpfunc_quitfunc PROTO((struct edescst *edesc, int n_tries));
static void bpfunc_edit PROTO((struct drawst fdets));
static void bplnum_edit PROTO((struct drawst fdets));
static void build_source_lines PROTO((fil_t *fil, int lnum, const char *text,
				      const char ***p_lines, int *p_nlines));
static void select_bd PROTO((editblock_id_t editblock_id,
			     editblock_action_t action));
static void update_editblock PROTO((bpdesc_t *bd));
static bool change_bd PROTO((editblock_id_t editblock_id,
			     const char **lines, int nlines,
			     callback_arg_t callback_arg));
static int set_initialise_data_flag PROTO((objid_t obj, fval_t unused_arg));

#ifdef OS_SUNOS_4
static taddr_t get_bpt_addr PROTO((objid_t obj));
#endif

/*  Maximum length of a function name. If you change this you must also
 *  change the Bpt_format string to match.
 */
#define LEN_BP_FNAME 	32

const char Bpt_format[] = "function:%[-]32cb line:%[B]5cn\n";

/*  Field numbers for breakpoints
 */
#define FN_BPT_FNAME	0
#define FN_BPT_LNUM	1
#define FN_BPT_LAST	2

/*  Flist terminator for breakpoint fields for set_all_fields().
 *  Can't use NULL as one of the fields is an int which can legitimately
 *  have the value 0.
 */
#define BPT_FLIST_TERMINATOR	((fval_t)-1)

fnamemap_t Bpt_fnamemap[] = {
	"function-name",	FN_BPT_FNAME,
	"lnum",			FN_BPT_LNUM,
	NULL,			0,
};

const char Bphead_format[] = "Breakpoints\n";

#define BPHEAD_OBJCODE	((objid_t)Bphead_format)

static const char Stop_keyword[] = "#stop";

fdef_t Bpt_fdefs[] = {
	'b', bpfunc_draw, bpfunc_edit, NULL,
	'n', n_draw, bplnum_edit, NULL,
	0, NULL, NULL, NULL,
};

const char *
bpt_getobjname(obj)
objid_t obj;
{
	static char *last = NULL;
	bpdesc_t *bd;

	bd = (bpdesc_t *)obj;

	if (last != NULL)
		free(last);

	last = strf("%s:%d", bd->bd_func->fu_name, bd->bd_lnum);

	return last;
}

static void
bpfunc_draw(dets)
register struct drawst *dets;
{
	wn_ttext(dets->dr_wn, (char *)dets->dr_fval, dets->dr_x, dets->dr_y,
						    dets->dr_fg, dets->dr_bg);
}

#ifdef OS_SUNOS_4
static taddr_t
get_bpt_addr(obj)
objid_t obj;
{
	bpdesc_t *bd;

	bd = (bpdesc_t *)obj;
	return (bd->bd_lnum != 0) ? lnum_to_addr(bd->bd_func, bd->bd_lnum)
				  : min_bpt_addr(bd->bd_func);
}
#endif

/*  Build a source line for a breakpoint.  We duplicate any whitespace
 *  at the beginning of the source line following the breakpoint.
 */
static void
build_source_lines(fil, lnum, text, p_lines, p_nlines)
fil_t *fil;
int lnum;
const char *text;
const char ***p_lines;
int *p_nlines;
{
	static const char *linestab[1];

	if (lnum < 1 || open_source_file(fil, FALSE) != 0 ||
					   lnum > so_get_nlines(fil->fi_so))
		linestab[0] = text;
	else {
		static char buf[80];
		const char *line, *cptr;
		int len;
		
		/*  We don't check for tabs in the loop below because
		 *  so_getline turns them into spaces for us.
		 */
		line = so_getline(fil->fi_so, lnum - 1);
		for (cptr = line; *cptr == ' '; ++cptr)
			;
		len = cptr - line;

		if (td_have_window()) {
			font_t *srcfont, *editfont;

			src_get_fonts(get_current_srcwin(), &srcfont,&editfont);
			if (editfont->ft_width != 0)
				len = (len * srcfont->ft_width) /
							editfont->ft_width;
		}

		if (len > sizeof(buf) - strlen(text))
			len = sizeof(buf) - strlen(text);
		memset(buf, ' ', len);
		strcpy(buf + len, text);
		linestab[0] = buf;
	}

	*p_lines = linestab;
	*p_nlines = 1;
}

#ifdef OS_SUNOS_4
void
recalculate_bpt_addrs()
{
	objid_t obj;
	bpdesc_t *bd;

	obj = get_code(BPHEAD_OBJCODE, OBJ_CHILD);
	for (; obj != NULL; obj = get_code(obj, OBJ_NEXT)) {
		bd = (bpdesc_t *)obj;
		if (bd->bd_breakpoint != NULL) {
			remove_breakpoint(bd->bd_breakpoint);
			bd->bd_breakpoint = add_breakpoint(get_bpt_addr(obj));
			set_breakpoint_data(bd->bd_breakpoint, (long)obj);
		}
		if (bd->bd_code_has_func_calls) {
			bd->bd_code_id = recompile_code(bd->bd_parse_id,
							bd->bd_code_id,
							(char *)NULL);
		}
	}
}
#endif /* OS_SUNOS_4 */

void
bpt_select(wn, obj, x, y, width, height, flags)
int wn;
objid_t obj;
int x, y, width, height, flags;
{
	bpdesc_t *bd;

	bd = (bpdesc_t *)obj;

	if (flags & SEL_CHANGING && bd->bd_editblock_id != NULL) {
		editblock_action_t action;

		action = (flags & SEL_ON) ? EDL_SELECT : EDL_DESELECT;
		select_editblock(bd->bd_editblock_id, action,
							DONT_CALL_CALLBACK);
	}
	gen_select(wn, obj, x, y, width, height, flags);
}

int
change_bpt_lines(obj, lines, nlines)
objid_t obj;
const char **lines;
int nlines;
{
	bpdesc_t *bd;
	lexinfo_t lxbuf;

	bd = (bpdesc_t *)obj;

	return change_editblock(bd->bd_editblock_id, lines, nlines,
			 	DO_CALL_CALLBACK, (callback_arg_t)&lxbuf);
	
}

/*  Handle a change to the text of an editblock block for a breakpoint.
 *
 *  This function is registered as a callback for breakpoint editblock objects.
 *
 *  We never return FALSE (which indicates an error) because we always let
 *  edits complete.  We set a flag in the breakpoint if the code is bad,
 *  and complain whenever execution hits the breakpoint.
 */
static bool
change_bd(editblock_id, lines, nlines, callback_arg)
editblock_id_t editblock_id;
const char **lines;
int nlines;
callback_arg_t callback_arg;
{
	compile_res_t *cr;
	bpdesc_t *bd;
	block_t *block;

	bd = (bpdesc_t *)get_editblock_data(editblock_id);

	td_record_bpt_code_edit((objid_t)bd, lines, nlines);

	block = FU_BLOCKS(bd->bd_func);
	while (block != NULL) {
		block_t *child;

		for (child = block->bl_blocks; child != NULL; child = child->bl_next)
			if (child->bl_start_lnum <= bd->bd_lnum &&
						bd->bd_lnum <= child->bl_end_lnum)
				break;
		if (child == NULL)
			break;
		block = child;
	}

	if (block == NULL)
		block = bd->bd_func->fu_fil->fi_block;
	
	/*  Make sure block vars and type information of fil are loaded.
	 */
	FI_VARS(bd->bd_func->fu_fil);
	
	if (bd->bd_parse_id != NULL)
		ci_free_parse_id(bd->bd_parse_id);
	if (bd->bd_code_id != NULL) {
		ci_free_code_id(bd->bd_code_id);
		bd->bd_code_id = NULL;
	}

	cr = compile_code(lines, nlines, block, (char *)NULL,
					(lexinfo_t *)callback_arg,
					"void $start(void) {", "}",
					Stop_keyword, Stop_funcname);

	bd->bd_parse_id = cr->cr_parse_id;
	bd->bd_code_id = cr->cr_code_id;
	bd->bd_code_bad = cr->cr_code_id == NULL || cr->cr_parse_id == NULL;
	bd->bd_code_has_func_calls = cr->cr_code_has_func_calls;

	return TRUE;
}

/*  Select or deselect a breakpoint object.  This function is registered
 *  as a callback for breakpoint editblock objects.
 */
static void
select_bd(editblock_id, action)
editblock_id_t editblock_id;
editblock_action_t action;
{
	bpdesc_t *bd;

	bd = (bpdesc_t *)get_editblock_data(editblock_id);
	switch (action) {
	case EDL_SELECT:
		select_object((objid_t)bd, TRUE, OBJ_SELF);
		break;
	case EDL_DESELECT:
		select_object((objid_t)bd, FALSE, OBJ_SELF);
		break;
	case EDL_REMOVE:
		bd->bd_editblock_id = NULL;
		break;
	default:
		panic("unknown action in sb");
	}
}

/*  Make a new breakpoint object.
 */
objid_t
add_breakpoint_object(f, lnum)
func_t *f;
int lnum;
{
	fval_t fields[FN_BPT_LAST + 1];
	bpdesc_t *bd;
	const char *fname;
	taddr_t addr;

	if (f == NULL)
		fname = "";
	else {
		if (lnum == -1)
			addr = min_bpt_addr(f);
		else {
			if (map_lnum_to_addr(f, lnum, &addr) != 0)
				return NULL;
		}

		if (addr_to_breakpoint(addr) != 0) {
			errf("There is already a breakpoint at line %d of %s",
							      lnum, f->fu_name);
			return NULL;
		}

		lnum = addr_to_lnum(f, addr);
		fname = f->fu_name;
	}

	fields[FN_BPT_FNAME] = (fval_t)fname;
	fields[FN_BPT_LNUM] = (fval_t)lnum;
	fields[FN_BPT_LAST] = BPT_FLIST_TERMINATOR;

	/*  Add an object for the breakpoint.
	 */
	td_set_obj_updating(OBJ_UPDATING_OFF);
	bd = (bpdesc_t *) e_malloc(sizeof(bpdesc_t));
	bd->bd_breakpoint = NULL;
	bd->bd_func = f;
	bd->bd_lnum = lnum;
	bd->bd_editblock_id = NULL;
	bd->bd_parse_id = NULL;
	bd->bd_code_id = NULL;
	bd->bd_code_bad = FALSE;
	bd->bd_data_needs_initialising = FALSE;
	bd->bd_code_has_func_calls = FALSE;

	new_object((objid_t)bd, OT_BPT, BPHEAD_OBJCODE, OBJ_LAST_CHILD);
	set_all_fields((objid_t)bd, fields, BPT_FLIST_TERMINATOR);
	td_set_obj_updating(OBJ_UPDATING_ON);
	ensure_visible((objid_t)bd);

	if (bd->bd_func != NULL) {
		bd->bd_breakpoint = add_breakpoint(addr);
		set_breakpoint_data(bd->bd_breakpoint, (long)bd);
		update_editblock(bd);
	}
	else {
		td_obj_edit_field((objid_t)bd, FN_BPT_FNAME, 0, 0);
		if (bd->bd_func == NULL) {
			remove_object((objid_t)bd, OBJ_SELF);
			bd = NULL;
		}
	}

	return (objid_t)bd;
}

/*  Update the editblock for a breakpoint after a change of status
 *  or position.  If necessary, create the editblock block.
 */
static void
update_editblock(bd)
bpdesc_t *bd;
{
	const char **lines;
	int nlines;
	editblock_id_t editblock_id;
	char textbuf[sizeof(Stop_keyword) + 1];	/* space for added semicolon */

	editblock_id = bd->bd_editblock_id;

	if (bd->bd_func->fu_fil == NULL) {
		if (editblock_id != NULL) {
			remove_editblock(editblock_id, DONT_CALL_CALLBACK);
			bd->bd_editblock_id = NULL;
		}
		return;
	}

	strcpy(textbuf, Stop_keyword);
	strcat(textbuf, ";");
	build_source_lines(bd->bd_func->fu_fil, bd->bd_lnum, textbuf,
							&lines, &nlines);

	if (editblock_id != NULL) {
		editblockinfo_t eibuf;

		get_editblock_info(editblock_id, &eibuf);
		if (bd->bd_func->fu_fil != eibuf.ei_fil ||
					   bd->bd_lnum != eibuf.ei_fil_lnum) {
			remove_editblock(editblock_id, DONT_CALL_CALLBACK);
			editblock_id = NULL;
		}
	}

	if (editblock_id != NULL)
		change_editblock(editblock_id, lines, nlines,
				 DONT_CALL_CALLBACK, (callback_arg_t)NULL);
	else {
		editblock_id = add_editblock(bd->bd_func->fu_fil, bd->bd_lnum,
								lines, nlines);
		set_editblock_data(editblock_id, (char *)bd);
		register_editblock_callback(editblock_id, select_bd);
		register_editblock_edit_callback(editblock_id, change_bd);
	}

	bd->bd_editblock_id = editblock_id;
}

/*  Attempt to move a breakpoint after editing the function name
 *  or line number.
 */
static int
move_bpt(obj, newstring, edit_lnum)
objid_t obj;
const char *newstring;
int edit_lnum;
{
	fval_t fields[FN_BPT_LAST + 1];
	func_t *f;
	bpdesc_t *bd;
	const char *fname;
	int lnum;
	taddr_t addr;


	bd = (bpdesc_t *)obj;
	f = bd->bd_func;

	if (edit_lnum) {
		lnum = atoi(newstring);
		fname = f->fu_name;
	}
	else {
		lnum = bd->bd_lnum;
		fname = newstring;
	}

	get_all_fields(obj, fields);
	if (f == NULL || strcmp(fname, f->fu_name) != 0) {
		if (find_func_by_name(fname, &f) != 0)
			return -1;

		addr = min_bpt_addr(f);
		lnum = addr_to_lnum(f, addr);
	}
	else {
		if (map_lnum_to_addr(f, lnum, &addr) != 0)
			return -1;
		lnum = addr_to_lnum(f, addr);
	}

	/*  We don't allow more than one breakpoint at a given address.
	 */
	if (addr_to_breakpoint(addr) != 0) {
		errf("There is already a breakpoint at line %d of %s",
							lnum, f->fu_name);
		return -1;
	}

	fields[FN_BPT_FNAME] = f->fu_name;
	fields[FN_BPT_LNUM] = (fval_t)lnum;
	fields[FN_BPT_LAST] = BPT_FLIST_TERMINATOR;
	set_all_fields(obj, fields, BPT_FLIST_TERMINATOR);

	bd->bd_func = f;
	bd->bd_lnum = lnum;

	/*  Move the actual breakpoint in the code to the new position.
	 */
	if (bd->bd_breakpoint != NULL)
		remove_breakpoint(bd->bd_breakpoint);
	bd->bd_breakpoint = add_breakpoint(addr);
	set_breakpoint_data(bd->bd_breakpoint, (long)bd);

	update_editblock(bd);

	return 0;
}

bool
execute_bp_code(bp, fp, ap)
breakpoint_t bp;
taddr_t fp, ap;
{
	objid_t obj;
	bpdesc_t *bd;
	ci_exec_result_t res;

	if ((obj = (objid_t)get_breakpoint_data(bp)) == NULL)
		return TRUE;

	bd = (bpdesc_t *)obj;
	
	if (bd->bd_code_bad) {
		errf("Bad breakpoint code");
		return TRUE;
	}
	if (bd->bd_code_id == NULL)
		return TRUE;
	
	if (bd->bd_data_needs_initialising) {
		ci_initialise_code(bd->bd_code_id, TRUE);
		bd->bd_data_needs_initialising = FALSE;
	}
	else
		ci_initialise_code(bd->bd_code_id, FALSE);

	res = ci_execute_code(bd->bd_code_id, fp, ap, dread, dwrite,
							call_target_function);

	if (res != CI_ER_TRAP && res != STOP)
		errf("%s in breakpoint code", ci_exec_result_to_string(res));

	return res != CI_ER_TRAP;
}

static int
set_initialise_data_flag(obj, unused_arg)
objid_t obj;
fval_t unused_arg;
{
	((bpdesc_t *)obj)->bd_data_needs_initialising = TRUE;
	return 0;
}

void
reinitialise_bpt_code_data()
{
	visit_objects(BPHEAD_OBJCODE, OBJ_CHILDREN, set_initialise_data_flag,
						(fval_t)0, (objid_t *)NULL);
}

void
remove_breakpoint_object(obj)
objid_t obj;
{
	bpdesc_t *bd;

	bd = (bpdesc_t *)obj;

	if (bd->bd_editblock_id != NULL)
		remove_editblock(bd->bd_editblock_id, DONT_CALL_CALLBACK);

	if (bd->bd_breakpoint != NULL)
		remove_breakpoint(bd->bd_breakpoint);

	if (bd->bd_parse_id != NULL)
		ci_free_parse_id(bd->bd_parse_id);
	if (bd->bd_code_id != NULL)
		ci_free_code_id(bd->bd_code_id);
	
	free((char *)bd);
}

static void
show_bpt_source(bd, beep_on_failure)
bpdesc_t *bd;
bool beep_on_failure;
{
	if (bd->bd_lnum != 0 && bd->bd_func->fu_fil != NULL)
		show_source(bd->bd_func->fu_fil, bd->bd_lnum, TRUE);
	else {
		errf("%s`%s' was not compiled with the -g flag",
					beep_on_failure ? "" : "\bNote: ",
					bd->bd_func->fu_name);
	}
}

int
bpt_dumpobj(arg, code, level)
char *arg;
objid_t code;
int level;
{
	const char *fname;
	int lnum;

	fname = get_field_value(code, FN_BPT_FNAME);
	lnum = (int)get_field_value(code, FN_BPT_LNUM);

	return td_outf(level, "%s %d", fname, lnum);
}

/*  Process the return from the breakpoint menu enabling and disabling
 *  ACTIVE, RESTART etc. and displaying source.
 */
void
do_bpt(obj, command)
objid_t obj;
int command;
{
	int oldstate;

	oldstate = td_set_obj_updating(OBJ_UPDATING_OFF);
	switch (command) {
	case MR_BPTMEN_SOURCE:
		show_bpt_source((bpdesc_t *)obj, TRUE);
		break;
	case MR_BPTMEN_REMOVE:
		remove_object(obj, OBJ_SELF);
		break;
	default:
		panic("bad rv in db");
	}
	td_set_obj_updating(oldstate);
}

void
remove_symtab_breakpoints(symtab_id)
symtab_id_t symtab_id;
{
	objid_t obj, next;
	int oldstate;

	oldstate = td_set_obj_updating(OBJ_UPDATING_OFF);
	for (obj = get_code(BPHEAD_OBJCODE, OBJ_CHILD); obj != NULL; obj = next) {
		next = get_code(obj, OBJ_NEXT);
		if ((((bpdesc_t *)obj)->bd_func)->fu_symtab_id == symtab_id)
			remove_object(obj, OBJ_SELF);
	}
	td_set_obj_updating(oldstate);
}

/*  Add the breakpoint header to the display.  Called on startup from main().
 */
void
add_breakpoint_header(par)
objid_t par;
{
	new_object(BPHEAD_OBJCODE, OT_BPHEAD, par, OBJ_CHILD);
}

/*  BUG - Main_func is used directly in stack.c
 */
func_t *Main_func;

void
set_main_func(f)
func_t *f;
{
	Main_func = f;
}

/*  Process the return from the breakpoint header menu, creating new
 *  breakpoints and such.
 */
/* ARGSUSED */
void
do_bps(unused_obj, command)
objid_t unused_obj;
int command;
{
	switch(command) {
	case MR_ADD_BREAKPOINT:
		add_breakpoint_object((func_t *)NULL, 0);
		break;
	case MR_REMOVE_ALL_BREAKPOINTS:
		remove_object(BPHEAD_OBJCODE, OBJ_CHILDREN);
		break;
	default:
		panic("bad cmd in db");
	}
}

#define LNUM_EDIT	EDF_USER1

static int
bpfunc_quitfunc(edesc, n_tries)
struct edescst *edesc;
int n_tries;
{
	objid_t obj;
	bool edit_lnum;
	
	edit_lnum = (edesc->ed_flags & LNUM_EDIT) != 0;

	if (message_on_null_name(edesc, edit_lnum ? "line number"
						  : "function name") == -1)
		return force_quit(n_tries);

	if (strcmp(edesc->ed_copy, edesc->ed_orig) == 0)
		return EDR_CONFIRM_NO_CHANGE;
	obj = (objid_t) edesc->ed_user;
	if (move_bpt(obj, edesc->ed_copy, edit_lnum) != 0)
		return force_quit(n_tries);
	return EDR_CONFIRM_CHANGE;
}

static void
bplnum_edit(fdets)
struct drawst fdets;
{
	char buf[20];
	struct edescst edescbuf;
	int res;

	bpdesc_t *bd;

	bd = (bpdesc_t *)fdets.dr_code;

	if (HAS_CODE(bd)) {
		errf("Can't move a breakpoint that has interpreted code");
		wn_wait_for_release_of(fdets.dr_wn, B_ANY);
		return;
	}

	clear_selection();
	(void) sprintf(buf, "%d", (int)fdets.dr_fval);

	make_edesc(&edescbuf, fdets.dr_wn, buf, 5, fdets.dr_fg, fdets.dr_bg);
	edescbuf.ed_user = (int)bd;
	edescbuf.ed_keyfunc = (ed_keyfunc_t)num_filter;
	edescbuf.ed_quitfunc = (ed_quitfunc_t)bpfunc_quitfunc;
	edescbuf.ed_flags |= LNUM_EDIT;
	res = suppress_ta_cursor_then_edit_field(&edescbuf, "lnum");

	if (res == EDR_CONFIRM_CHANGE)
		show_bpt_source(bd, FALSE);
}

static void
bpfunc_edit(fdets)
struct drawst fdets;
{
	struct edescst edescbuf;
	bpdesc_t *bd;
	int res;

	bd = (bpdesc_t *)fdets.dr_code;

	if (HAS_CODE(bd)) {
		errf("Can't move a breakpoint that has interpreted code");
		wn_wait_for_release_of(fdets.dr_wn, B_ANY);
		return;
	}

	clear_selection();
	make_edesc(&edescbuf, fdets.dr_wn, fdets.dr_fval,
					LEN_BP_FNAME, fdets.dr_fg, fdets.dr_bg);

	edescbuf.ed_user = (int)bd;
	edescbuf.ed_quitfunc = (ed_quitfunc_t)bpfunc_quitfunc;

	/*  BUG: must think of a better way to do this.
	 *  BUG: must document what is going on here.
	 */
	if (td_have_window() && fdets.dr_fval[0] == '\0') {
		edescbuf.ed_meaning = EDM_SETCURSOR;
		edescbuf.ed_newpos = 0;
		do_edit(&edescbuf, EDM_SETCURSOR);
	}

	res = suppress_ta_cursor_then_edit_field(&edescbuf, "function-name");
	if (res == EDR_CONFIRM_CHANGE)
		show_bpt_source(bd, FALSE);
}
