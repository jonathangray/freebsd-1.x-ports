/* obj_target.c - target argv and envp munging */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_obj_target_c_sccsid[] = "@(#)obj_target.c	1.19 26/7/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <sys/types.h>
#include <sys/param.h>	/* for NCARGS */
#include <sys/file.h>
#include <errno.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <local/wn.h>
#include <local/obj/obj.h>
#include <local/arg.h>
#include <local/ukcprog.h>
#include <local/obj/fed.h>

#include "ups.h"
#include "objtypes.h"
#include "obj_target.h"
#include "obj_env.h"
#include "exec.h"
#include "ui.h"
#include "obj_stack.h"
#include "state.h"
#include "tdr.h"

static int args_quitfunc PROTO((struct edescst *edesc, int unused_n_tries));
static void args_edit PROTO((struct drawst fdets));

const char Com_format[] = "Target %[-]40ca\n        %[-]120ce\n";

static const char *Execfile_name;


fdef_t Com_fdefs[] = {
	'e', s_draw, args_edit, NULL,
	'a', s_draw, no_edit, NULL,
	'\0', NULL, NULL, NULL,
};

static objid_t Com_objcode = NULL;

#define FN_COM_EFILE	0
#define FN_COM_ARGS	1
#define FN_COM_LAST	2

fnamemap_t Com_fnamemap[] = {
	"arguments",	FN_COM_ARGS,
	NULL,		0,
};

#define MAX_ARG_LEN	500
#define MAX_EFILE_LEN	40

int
setup_args(p_path, p_argv, p_envp, p_rdlist)
const char **p_path, ***p_argv, ***p_envp;
long *p_rdlist;
{
	static dvec_t dv = 0;
	const char *cmdline;
	const char **av;
	int len;

	if (dv != 0)
		free_dvec_and_strings(dv);

	*p_path = Execfile_name;
	*p_envp = (const char **)get_environment();
	cmdline = (char *) get_field_value(Com_objcode, FN_COM_ARGS);

	if (arg_lparse(cmdline, &dv, p_rdlist) == -1)
		return -1;
	*p_argv = (const char **)get_dvec_vec(dv);

	/*  Check that the arg list is not too long.
	 */
	len = 0;
	av = *p_argv; 
	while (*av != NULL)
		len += strlen(*av++);
	if (len > NCARGS) {
		errf("Argument list too long for exec");
		return -1;
	}

	return 0;
}

bool
target_process_exists()
{
	tstate_t state;

	state = get_target_state();
	return state == TS_STOPPED_AND_CAN_CONTINUE ||
					state == TS_STOPPED_AND_CANT_CONTINUE;
}

bool
can_get_target_vars()
{
	tstate_t state;

	state = get_target_state();
	return state == TS_CORE || state == TS_STOPPED_AND_CAN_CONTINUE ||
				   state == TS_STOPPED_AND_CANT_CONTINUE;
}

/* ARGSUSED */
static int
args_quitfunc(edesc, unused_n_tries)
struct edescst *edesc;
int unused_n_tries;
{
	change_field((objid_t)edesc->ed_user, FN_COM_ARGS, (fval_t)edesc->ed_copy);
	return EDR_CONFIRM_CHANGE;
}

static void
args_edit(fdets)
struct drawst fdets;
{
	struct edescst edescbuf;
	clear_selection();
	make_edesc(&edescbuf, fdets.dr_wn, (char *)fdets.dr_fval, MAX_ARG_LEN,
								fdets.dr_fg, fdets.dr_bg);
	edescbuf.ed_user = (int) fdets.dr_code;
	edescbuf.ed_quitfunc = (ed_quitfunc_t)args_quitfunc;
	suppress_ta_cursor_then_edit_field(&edescbuf, "arguments");
}

/*  Create an object for the command and enviroment
 */
void
add_target_object(par, efile, minus_a_cmdline)
objid_t par;
const char *efile, *minus_a_cmdline;
{
	char *command_line, *pos, *cptr;
	fval_t fields[FN_COM_LAST + 1];
	int efile_len;

	/*  BUG: find a better place for this.
	 */
	Execfile_name = efile;
	efile_len = strlen(efile);

	cptr = e_malloc(MAX_EFILE_LEN + 1);
	if (efile_len <= MAX_EFILE_LEN)
		(void) strcpy(cptr, efile);
	else {
		const char *s;

		s = efile + (efile_len - MAX_EFILE_LEN) + 3; /* 3 for ... */
		while (*s == '/' && *s != '\0')
			++s;
		(void) sprintf(cptr, "...%s", s);
	}
	fields[FN_COM_EFILE] = (fval_t) cptr;

	if ((pos = strrchr(efile, '/')) != NULL)
		efile = pos + 1;
	efile_len = strlen(efile);

	command_line = e_malloc(MAX_ARG_LEN + 1);
	command_line[MAX_ARG_LEN] = '\0';

	if (minus_a_cmdline != NULL) {
		if (strlen(minus_a_cmdline) > MAX_ARG_LEN - efile_len)
			errf("-a args too long - ignored");
		else
			(void) sprintf(command_line, "%s %s",
							efile, minus_a_cmdline);
	}
	else if (can_get_target_vars()) {
		if (get_command_line_from_stack(command_line, MAX_ARG_LEN) != 0)
			(void) strncpy(command_line, efile, MAX_ARG_LEN);
	}
	else
		(void) strncpy(command_line, efile, MAX_ARG_LEN);

	fields[FN_COM_ARGS] = (fval_t) command_line;
	fields[FN_COM_LAST] = (fval_t) NULL;

	Com_objcode = (objid_t) Com_format;
	new_object(Com_objcode, OT_COM, par, OBJ_CHILD);
	set_all_fields(Com_objcode, fields, (fval_t)NULL);
}

/*  Return values from the target menu.
 */
#define TGT_CMD_CORE	'c'

int
target_dumpobj(arg, code, level)
char *arg;
objid_t code;
int level;
{
	if (td_outf(level, "%s", get_field_value(code, FN_COM_EFILE)) != 0) 
		return -1;

	return td_outf(level, "%s", get_field_value(code, FN_COM_ARGS));
}

/*  Menu function for the target menu (this is the menu that appears
 *  when the target object is selected, not the target control menu).
 */
/* ARGSUSED */
void
do_target(par, command)
objid_t par;
int command;
{
	switch(command) {
	case TGT_CMD_CORE:
		write_target_core();
		break;
	default:
		panic("bad cmd in dt");
	}
}

void
free_com(obj)
objid_t obj;
{
	free((char *)get_field_value(obj, FN_COM_EFILE));
	free((char *)get_field_value(obj, FN_COM_ARGS));
}
