/* obj_buildf.c - miscellaneous stuff - will be broken up soon */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_obj_buildf_c_sccsid[] = "@(#)obj_buildf.c	1.23 13/8/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <stdlib.h>
#include <stdio.h>

#include <local/wn.h>
#include <local/obj/obj.h>
#include <local/menu3.h>
#include <local/ukcprog.h>
#include <mtrprog/utils.h>

#include "ups.h"
#include "symtab.h"
#include "objtypes.h"

#include "obj_buildf.h"
#include "obj_misc.h"
#include "obj_target.h"
#include "obj_bpt.h"
#include "obj_signal.h"
#include "obj_env.h"
#include "obj_stack.h"
#include "va.h"
#include "ui.h"
#include "exec.h"
#include "core.h"
#include "expr.h"
#include "state.h"

static const char *gen_getobjname PROTO((objid_t code));

/*  Flag to say if we have common blocks.  Set by initialise_display_area(),
 *  used by rebuild_display().
 */
static int Have_common_blocks;

#define BUILTIN_MENUS	/* define for compiled in menu files */

/* Menus
 */
#ifdef BUILTIN_MENUS
extern MENU core_men;		/* target menu */
extern MENU bph_men;		/* breakpoint header menu */
extern MENU bpt_men;		/* breakpoint menu */
extern MENU fun_men;		/* function menu */
extern MENU fil_men;		/* source file menu */
extern MENU sh_men;		/* source file header menu */
extern MENU var_men;		/* C variable menu */
extern MENU fvr_men;		/* Fortran 77 variable menu */
extern MENU sgh_men;		/* signal header menu */
extern MENU envhead_men;	/* signal header menu */
extern MENU sig_men;		/* signal menu */
extern MENU env_men;		/* environment menu */
extern MENU gbl_men;		/* globals menu */
extern MENU cbh_men;		/* common block header menu */
extern MENU cblock_men;		/* common block menu */
#endif

static const char Sfile_format[] = "%[-]s";

ot_t Objtab[] = {
#define OT_COM		0
		&core_men, "target", "menus/tgtmen.c", -1,
		do_target, Com_format, Com_fdefs, Com_fnamemap,
		gen_select, can_select, free_com, NULL, NULL,
		gen_getobjname, target_dumpobj,

#define OT_GLOBALS	1
		&gbl_men, "globals", "menus/gblmen.c", -1,
		do_globals, Globals_format, NULL, NULL,
		gen_select, can_select, NULL, NULL, NULL,
		gen_getobjname, header_dumpobj,
	
#define OT_SRCHEAD	2
		&sh_men, "srchead", "menus/shmen.c", -1,
		do_srchead, Srchead_format, NULL, NULL,
		gen_select, can_select, NULL, NULL, NULL,
		gen_getobjname, header_dumpobj,
	
#define OT_SFILE	3
		&fil_men, "source", "menus/filmen.c", -1,
		do_file, Sfile_format, NULL, NULL,
		gen_select, can_select, NULL, srcfile_getsize, NULL,
		srcfile_getobjname, file_dumpobj,
	
#define OT_FHEAD	4
		NULL, "stack", NULL, -1,
		NULL, Fhead_format, NULL, NULL,
		NULL, cannot_select, NULL, NULL, NULL,
		gen_getobjname, header_dumpobj,

#define OT_FUNC		5
		NULL, "function", NULL, OT_SFILE,
		do_func, Func_format, Func_fdefs, NULL,
		gen_select, can_select, free_func, NULL, NULL,
		func_getobjname, func_dumpobj,

#define OT_BLOCK	6
		NULL, "block", NULL, OT_SFILE,
		do_block, Block_format, NULL, NULL,
		gen_select, can_select, free_block, NULL, NULL,
		block_getobjname, block_dumpobj,

#define OT_FSIG		7
		NULL, "*fsig", NULL, -1,
		NULL, Fsig_format, Fsig_fdefs, NULL,
		NULL, cannot_select, free_fsig, NULL, NULL,
		gen_getobjname, fsig_dumpobj,
	
#define OT_BADFUNC	8
		NULL, "*badfunc", NULL, -1,
		NULL, Badfunc_format, NULL, NULL,
		NULL, cannot_select, free_badfunc, NULL, NULL,
		gen_getobjname, badstack_dumpobj,
	
#define OT_BADFRAME	9
		NULL, "*badframe", NULL, -1,
		NULL, Badfunc_format, NULL, NULL,
		NULL, cannot_select, free_badfunc, NULL, NULL,
		gen_getobjname, badstack_dumpobj,
	
#define OT_VAR		10
		&var_men, "var", "menus/varmen.c", -1,
		do_vars, Var_format, Var_fdefs, Var_fnamemap,
		gen_select, can_select, free_displayed_var, NULL, var_getcolor,
		var_getobjname, var_dumpobj,
	
#define OT_EXPR		11
		NULL, "expr", NULL, OT_VAR,
		do_expr, Expr_format, Expr_fdefs, Expr_fnamemap,
		gen_select, can_select, free_displayed_expr, NULL, expr_getcolor,
		expr_getobjname, expr_dumpobj,

#define OT_BPHEAD	12
		&bph_men, "bphead", "menus/bphmen.c", -1,
		do_bps, Bphead_format, NULL, NULL,
		gen_select, can_select, NULL, NULL, NULL,
		gen_getobjname, header_dumpobj,
	
#define OT_BPT		13
		&bpt_men, "bpt", "menus/bptmen.c", -1,
		do_bpt, Bpt_format, Bpt_fdefs, Bpt_fnamemap,
		bpt_select, can_select, remove_breakpoint_object, NULL, NULL,
		bpt_getobjname, bpt_dumpobj,

#define OT_SGHEAD	14
		&sgh_men, "sghead", "menus/sghmen.c", -1,
		do_sgh, Sghead_format, NULL, NULL,
		gen_select, can_select, NULL, NULL, NULL,
		gen_getobjname, header_dumpobj,
	
#define OT_SIG		15
		&sig_men, "signal", "menus/sigmen.c", -1,
		do_sig, Sig_format, NULL, NULL,
		gen_select, can_select, free_sig, sig_getsize, NULL,
		sig_getobjname, sig_dumpobj,

#define OT_CBHEAD	16
		&cbh_men, "cbhead", "menus/cbhmen.c", -1,
		do_cbhead, Cbhead_format, NULL, NULL,
		gen_select, can_select, NULL, NULL, NULL,
		gen_getobjname, header_dumpobj,
	
#define OT_CBLOCK	17
		&cblock_men, "cblock", "menus/cblockmen.c", -1,
		do_cblock, Cblock_format, NULL, NULL,
		gen_select, can_select, free_common_block_object, NULL, NULL,
		cblock_getobjname, cblock_dumpobj,

#define OT_ENVHEAD	18
		&envhead_men, "envhead", "menus/envheadmen.c", -1,
		do_envhead, Envhead_format, NULL, NULL,
		gen_select, can_select, NULL, NULL, NULL,
		gen_getobjname, header_dumpobj,
	
#define OT_ENV		19
		&env_men, "env", "menus/envmen.c", -1,
		do_env, Env_format, Env_fdefs, Env_fnamemap,
		gen_select, can_select, free_env, env_getsize, NULL,
		env_getobjname, env_dumpobj,
};

#define OT_MAXTYPE	19
#define N_OBJTYPES (sizeof(Objtab) / sizeof(Objtab[0]))

#define CHILD_INDENT	20

static const char *
gen_getobjname(code)
objid_t code;
{
	int objtype;

	objtype = ups_get_object_type(code);

	if (objtype < 0 || objtype >= N_OBJTYPES)
		panic("ot botch in ggn");

	return Objtab[objtype].ot_menuname;
}

void
do_formats(have_window)
bool have_window;
{
	fdef_t *fdef;
	register ot_t *ot;

	init_objects();

	if (have_window) {
		font_t *sysfont;

		sysfont = wn_get_sysfont();
		set_field_scale_factors(sysfont->ft_width, sysfont->ft_height);
	}

	for (ot = Objtab; ot < Objtab + N_OBJTYPES; ot++) {
		fdef = ot->ot_fdefs;
		if (fdef != NULL) {
			for (; fdef->fd_char != '\0'; fdef++)
				define_vsformat(fdef->fd_char,
						fdef->fd_edit,
						fdef->fd_draw,
						fdef->fd_getwidth);
		}
	}

	for (ot = Objtab; ot < Objtab + N_OBJTYPES; ot++) {
		if (have_window && ot->ot_men != NULL) {
#ifdef BUILTIN_MENUS
			ot->ot_md = Minsert((MENU *)ot->ot_men);
#else
			ot->ot_md = Mopen(ot->ot_menupath);
#endif
			if (ot->ot_md == -1) {
				Mperror("Mopen");
				exit(1);
			}
			Mnobox(ot->ot_md);
		}

		define_objtype(ot - Objtab, ot->ot_format,
					CHILD_INDENT,
					ot->ot_select, ot->ot_can_select,
					ot->ot_free,
					ot->ot_get_size, ot->ot_get_color);
		set_objtype_get_name_func(ot - Objtab, ot->ot_get_name);
		set_objtype_dump_func(ot - Objtab, ot->ot_dump);
	}

	/*  An objtype with ot_men NULL and ot_md != -1 means that the
	 *  objtype shares a menu with objtype ot_md.
	 */
	for (ot = Objtab; ot < Objtab + N_OBJTYPES; ++ot) {
		if (ot->ot_men == NULL && ot->ot_md != -1)
			ot->ot_md = Objtab[ot->ot_md].ot_md;
	}

	/*  Do the OT_SFILE_EX special case.
	 */
	ot = Objtab + OT_SFILE;
	define_objtype(OT_SFILE_EX, Sfile_ex_format,
				CHILD_INDENT,
				ot->ot_select, ot->ot_can_select,
				ot->ot_free,
				ot->ot_get_size, ot->ot_get_color);
	set_objtype_get_name_func(OT_SFILE_EX, ot->ot_get_name);
	set_objtype_dump_func(OT_SFILE_EX, ot->ot_dump);
}

void
close_target_display()
{
	delete_functions(FHEAD_OBJCODE);
	remove_object(GLOBALS_OBJCODE, OBJ_DESCENDENTS);
	if (Have_common_blocks)
		remove_object(CBHEAD_OBJCODE, OBJ_DESCENDENTS);
	hide_source_vars();
}

objid_t
rebuild_display(proc)
proc_t proc;
{
	objid_t obj_to_make_visible;

	clear_selection();
	obj_to_make_visible = add_functions(proc, FHEAD_OBJCODE);
	update_variable_values();

	return obj_to_make_visible;
}

void
update_variable_values()
{
	objid_t obj;

	update_local_variable_values();

	update_vars_of(GLOBALS_OBJCODE, FALSE);

	if (Have_common_blocks) {
		obj = get_code(CBHEAD_OBJCODE, OBJ_CHILD);
		for (; obj != NULL; obj = get_code(obj, OBJ_NEXT))
			update_vars_of(obj, FALSE);
	}

	obj = get_code(SRCHEAD_OBJCODE, OBJ_CHILD);
	for (; obj != NULL; obj = get_code(obj, OBJ_NEXT))
		update_vars_of(obj, FALSE);
}

void
initialise_display_area(textname, corename, args, have_common_blocks)
const char *textname, *corename, *args;
int have_common_blocks;
{
	add_breakpoint_header((objid_t)NULL);
	new_object(FHEAD_OBJCODE, OT_FHEAD, (objid_t)NULL, OBJ_CHILD);
	if (have_common_blocks)
		new_object(CBHEAD_OBJCODE, OT_CBHEAD, (objid_t)NULL, OBJ_CHILD);
	new_object(SRCHEAD_OBJCODE, OT_SRCHEAD, (objid_t)NULL, OBJ_CHILD);
	add_globals_header((objid_t)NULL);
	add_env_header((objid_t)NULL);
	add_signals_header((objid_t)NULL);

	if (corename != NULL) {
		if (is_number(corename)) {
			if (attach_to_process(textname, atoi(corename)) == 0)
				set_target_state(TS_STOPPED_AND_CAN_CONTINUE);
			else
				set_target_state(TS_NOTR);
		}
		else {
			set_target_state(TS_CORE);
			add_functions((proc_t)NULL, FHEAD_OBJCODE);
		}
	}
	else
		set_target_state(TS_NOTR);

	add_target_object((objid_t)NULL, textname, args); /* add command */
	/*  Stash Have_common_blocks for rebuild_display().
	 */
	Have_common_blocks = have_common_blocks;
}
