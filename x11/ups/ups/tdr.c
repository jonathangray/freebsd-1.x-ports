/* tdr.c - code to record and replay text based test driver files */

/*  Copyright 1992 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_tdr_c_sccsid[] = "@(#)tdr.c	1.6 18/9/92 (UKC)";

#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#ifdef __STDC__
#include <unistd.h>
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <local/ukcprog.h>

#include <local/wn.h>
#include <local/obj/obj.h>
#include <local/obj/fed.h>
#include <mtrprog/ifdefs.h>

#include "ui.h"
#include "cursors.h"
#include "reg.h"
#include "ups.h"
#include "symtab.h"
#include "exec.h"
#include "objtypes.h"
#include "obj_misc.h"
#include "obj_target.h"
#include "obj_bpt.h"
#include "obj_signal.h"
#include "obj_env.h"
#include "obj_stack.h"
#include "va.h"
#include "menudata.h"
#include "src.h"
#include "expr.h"
#include "output.h"
#include "state.h"
#include "tdr.h"
#include "sccsdata.h"

/*  Avoid messing up `%' in vi with literal braces.
 */
#define OPEN_BRACE	'{'
#define CLOSE_BRACE	'}'

#define errf	DONT_CALL_ERRF__USE_SYSF_INSTEAD

/*  Version number for the actions file.  We complain (but continue anyway)
 *  if we are fed a file with a version number higher than we expect.
 */
#define CURRENT_VERSION		1

static FILE *Record_fp = NULL;
static const char *Record_file_name;

static FILE *Replay_fp = NULL;
static const char *Replay_file_name;
int Replay_lnum;
static bool Check_message_failed;

static bool Interactive;
static bool Doing_record_edit = FALSE;
static bool Want_select_recording = TRUE;
static bool Have_window = TRUE;

static bool Runcmds = TRUE;
static bool Want_cmd_echo = FALSE;
static bool Want_wait_for_user = FALSE;
static bool Want_wait_on_check = FALSE;
static bool Want_verbose_test = FALSE;
static bool Want_displayed_source_check = TRUE;

static const char *Set_replay_mode_mode = NULL;
static int Set_replay_mode_lnum;

static objid_t Obj_to_make_visible;
static objid_t Default_obj;

static fil_t *Current_displayed_fil = NULL;

static void (*Error_func)PROTO((const char *s));

#define NEL(a)	(sizeof(a) / sizeof*(a))

typedef void (*mfunc_t)PROTO((objid_t objid, int cmd));

typedef int (*cmdfunc_t)PROTO((const char *cmd, int argc, char **args));

#ifndef THIS_ARCH
#define THIS_ARCH	"unknown"
#endif

#ifndef THIS_OS
#define THIS_OS		"unknown"
#endif

typedef struct {
	int am_rv;
	const char *am_name;
} actmap_t;

typedef struct {
	const char *mc_name;
	mfunc_t mc_func;
	bool mc_apply_to_objs;
	actmap_t *mc_actmap;
	int mc_actmap_size;
} mcmds_t;

static void do_permanent_menu PROTO((objid_t unused_objid, int cmd));
static void do_target_menu PROTO((objid_t unused_objid, int cmd));
static void do_source_window_menu PROTO((objid_t unused_objid, int cmd));
static void do_output_window_menu PROTO((objid_t unused_objid, int cmd));

static cmdfunc_t cmd_to_func PROTO((const char *cmd,
				    bool *p_splitargs, bool *p_alwaysexec));
static int process_command PROTO((char *line));

static mcmds_t *mname_to_menu PROTO((const char *name));
static void list_mnames PROTO(( FILE *fp));

static const char *rv_to_name PROTO((mcmds_t *mc, int rv));
static int name_to_rv PROTO((mcmds_t *mc, const char *name, int *p_rv));
static void list_rvs PROTO((FILE *fp, mcmds_t *mc));

static int replay_edit PROTO((edesc_t *ed, const char *what));
static void record_edit PROTO((int res, const char *what, const char *value));
static void record_start_edit PROTO((draw_t *dr));

static void show_error_on_tty PROTO((const char *s));
static void wait_for_user PROTO((const char *line));

static void show_obj_path PROTO((const char *op, objid_t obj,
							int endc, FILE *fp));
static objid_t path_to_obj PROTO((const char *path));
static int checkline PROTO((const char *line));
static void record_obj_dump PROTO((objid_t obj));
static int r_flush PROTO((void));
static int getline PROTO((bool lose_leading_whitespace, char **p_line));

static int cf_refresh PROTO((const char *cmd, int argc, char **unused_args));
static int cf_menu PROTO((const char *cmd, int argc, char **args));
static int cf_replaymode PROTO((const char *cmd, int argc, char **unused_args));
static int cf_select PROTO((const char *cmd, int argc, char **args));
static int cf_show_var PROTO((const char *cmd, int argc, char **args));
static int cf_to_lnum PROTO((const char *cmd, int argc, char **args));
static int cf_debug PROTO((const char *cmd, int argc, char **args));
static int cf_show PROTO((const char *cmd, int argc, char **args));
static int cf_edit PROTO((const char *cmd, int argc, char **args));
static int cf_bptcode PROTO((const char *cmd, int argc, char **args));
static int cf_help PROTO((const char *cmd, int argc, char **unused_args));
static int cf_version PROTO((const char *cmd, int argc, char **unused_args));
static int cf_check PROTO((const char *cmd, int argc, char **args));
static int cf_cond PROTO((const char *cmd, int argc, char **args));
static int cf_echo PROTO((const char *cmd, int unused_argc, char **args));
static int cf_shellcmd PROTO((const char *cmd, int argc, char **args));
static int cf_message PROTO((const char *cmd, int argc, char **args));
static int cf_checksrc PROTO((const char *cmd, int argc, char **args));

static bool lines_match PROTO((const char *fline, const char *uline));
static bool mf_spaces PROTO((const char *unused_fline, const char **p_uline));
static bool mf_addr PROTO((const char *unused_fline, const char **p_uline));
static bool mf_choice PROTO((const char *fline, const char **p_uline));
static bool mf_value PROTO((const char *unused_fline, const char **p_uline));
static bool mf_junkaddr PROTO((const char *unused_fline, const char **p_uline));
static bool mf_junkstring PROTO((const char *unused_fline, const char **p_uline));
static bool mf_junkfuncptr PROTO((const char *unused_fline, const char **p_uline));

static int cond_exists PROTO((const char *cond, int len));
static int check_cond PROTO((const char *cond, bool *p_cond_holds));
static const char *not PROTO((const char *cond));
static int handle_cond PROTO((const char *cmd, const char *cond));
static int skip_to PROTO((const char *word));
static void sysf PROTO((const char *fmt, ...)) FORMF_ARGS(1, 2);
static int check_message PROTO((const char *what, const char *mesg));
static int cmp_lines PROTO((const char *what,
			     const char *expected, const char *got));
static int check_shell_line PROTO((const char *line));

static struct {
	const char *name;
	cmdfunc_t func;
	bool splitargs;
	bool alwaysexec;
} Cmdtab[] = {
	"menu",		cf_menu,	TRUE,	FALSE,
	"select",	cf_select,	FALSE,	FALSE,
	"addselect",	cf_select,	FALSE,	FALSE,
	"deselect",	cf_select,	FALSE,	FALSE,
	"replaymode",	cf_replaymode,	TRUE,	FALSE,
	"checksrc",	cf_checksrc,	TRUE,	FALSE,
	"refresh",	cf_refresh,	TRUE,	FALSE,
	"execto",	cf_to_lnum,	TRUE,	FALSE,
	"showvar",	cf_show_var,	TRUE,	FALSE,
	"show",		cf_show,	FALSE,	FALSE,
	"breakpoint",	cf_to_lnum,	TRUE,	FALSE,
	"debug",	cf_debug,	FALSE,	FALSE,
	"help",		cf_help,	TRUE,	FALSE,
	"echo",		cf_echo,	FALSE,	FALSE,
	"warn",		cf_echo,	FALSE,	FALSE,
	"message",	cf_message,	FALSE,	FALSE,
	"output",	cf_message,	FALSE,	FALSE,
	"shellcmd",	cf_shellcmd,	FALSE,	FALSE,

	"shell",	cf_shellcmd,	FALSE,	TRUE,
	"edit",		cf_edit,	FALSE,	TRUE,
	"bptcode",	cf_bptcode,	FALSE,	TRUE,
	"version",	cf_version,	TRUE,	TRUE,
	"check",	cf_check,	FALSE,	TRUE,

	"if",		cf_cond,	TRUE,	TRUE,
	"else",		cf_cond,	TRUE,	TRUE,
	"endif",	cf_cond,	TRUE,	TRUE,
};

static actmap_t Am_var[] = {
	MR_VAR_STRING,			"string",
	MR_VAR_SIGNED_DECIMAL,		"signed_decimal",
	MR_VAR_UNSIGNED_DECIMAL,	"unsigned_decimal",
	MR_VAR_SIGNED_OCTAL,		"signed_octal",
	MR_VAR_UNSIGNED_OCTAL,		"unsigned_octal",
	MR_VAR_SIGNED_HEX,		"signed_hex",
	MR_VAR_UNSIGNED_HEX,		"unsigned_hex",
	MR_VAR_UNSIGNED_BINARY,		"unsigned_binary",
	MR_VAR_ASCII_BYTE,		"ascii_byte",
	MR_VAR_DEREF,			"deref",
	MR_VAR_ADDRESS,			"address",
	MR_VAR_EXPAND,			"expand",
	MR_VAR_DUP,			"dup",
	MR_VAR_DELETE,			"delete",
	MR_VAR_COLLAPSE,		"collapse",
	MR_VAR_COLLAPSE_COMPLETELY,	"collapse_completely",
	MR_VAR_WANT_TYPEDEFS,		"want_typedefs",
	MR_VAR_NO_TYPEDEFS,		"no_typedefs",
};

static actmap_t Am_srcwin[] = {
	MR_SRCWIN_BACK,			"back",
	MR_SRCWIN_SEARCH_FORWARDS,	"search_forwards",
	MR_SRCWIN_SEARCH_BACKWARDS,	"search_backwards",
	MR_SRCWIN_PAGE_UP,		"page_up",
	MR_SRCWIN_PAGE_DOWN,		"page_down",
};

static actmap_t Am_outwin[] = {
	MR_OUTWIN_PAGE_UP,		"page_up",
	MR_OUTWIN_PAGE_DOWN,		"page_down",
	MR_OUTWIN_CLEAR,		"clear",
	MR_OUTWIN_SEARCH_FORWARDS,	"search_forwards",
	MR_OUTWIN_SEARCH_BACKWARDS,	"search_backwards",
};

static actmap_t Am_bphead[] = {
	MR_ADD_BREAKPOINT,		"add_breakpoint",
	MR_REMOVE_ALL_BREAKPOINTS,	"remove_all_breakpoints",
};

static actmap_t Am_bpt[] = {
	MR_BPTMEN_REMOVE,		"remove",
	MR_BPTMEN_SOURCE,		"source",
};

static actmap_t Am_globals[] = {
	MR_SHOW_UNTYPED_VARS,		"show_untyped_vars",
	MR_HIDE_UNTYPED_VARS,		"hide_untyped_vars",
};

static actmap_t Am_cbhead[] = {
	MR_SHOW_COMMON_BLOCKS,		"show_common_blocks",
	MR_HIDE_ALL_COMMON_BLOCKS,	"hide_all_common_blocks",
};

static actmap_t Am_cblock[] = {
	MR_EXPAND_COMMON_BLOCK,		"expand_common_block",
	MR_COLLAPSE_COMMON_BLOCK,	"collapse_common_block",
	MR_HIDE_COMMON_BLOCK,		"hide_common_block",
};

static actmap_t Am_srchead[] = {
	MR_SHOW_SOURCE_FILES,		"show_source_files",
	MR_HIDE_SOURCE_FILES,		"hide_source_files",
};

static actmap_t Am_source[] = {
	MR_ADD_VARS,			"add_vars",
	MR_HIDE_VARS,			"hide_vars",
	MR_DISPLAY_SOURCE,		"display_source",
	MR_ADD_EXPRESSION,		"add_expression",
};

static actmap_t Am_sighead[] = {
	MR_SHOW_ALL_SIGNALS,		"show_all_signals",
	MR_HIDE_ALL_SIGNALS,		"hide_all_signals",
};

static actmap_t Am_signal[] = {
	MR_SIG_TOGGLE_STOP_CONT,	"toggle_stop_cont",
	MR_SIG_TOGGLE_ACCEPT_IGNORE,	"toggle_accept_ignore",
	MR_SIG_TOGGLE_REDRAW,		"toggle_redraw",
	MR_HIDE_SIGNAL,			"hide_signal",
};

static actmap_t Am_pmenu[] = {
	MR_QUIT_UPS,			"quit_ups",
	MR_DONT_QUIT,			"dont_quit",
};

static actmap_t Am_target[] = {
	MR_WRITE_CORE,			"write_core",
};

static actmap_t Am_tmenu[] = {
	MR_TGT_RUN,			"run",
	MR_TGT_CONT,			"cont",
	MR_TGT_STEP,			"step",
	MR_TGT_NEXT,			"next",
	MR_TGT_STOP,			"stop",
	MR_TGT_KILL,			"kill",
};

static actmap_t Am_envhead[] = {
	MR_RESET_ENVTAB,		"reset_envtab",
	MR_SHOW_ALL_ENV_ENTRIES,	"show_all_env_entries",
	MR_HIDE_ALL_ENV_ENTRIES,	"hide_all_env_entries",
	MR_ADD_ENV_ENTRY,		"add_env_entry",
};

static actmap_t Am_env[] = {
	MR_DELETE_ENV_ENTRY,		"delete_env_entry",
	MR_HIDE_ENV_ENTRY,		"hide_env_entry",
	MR_APPEND_ENV_ENTRY,		"append_env_entry",
};

#define AM(actmap)	actmap,	NEL(actmap)

static mcmds_t Mnames[] = {
	"target",	do_target,		TRUE,	AM(Am_target),
	"globals",	do_globals,		TRUE,	AM(Am_globals),
	"srchead",	do_srchead,		TRUE,	AM(Am_srchead),
	"source",	do_file,		TRUE,	AM(Am_source),
	"function",	do_func,		TRUE,	AM(Am_source),
	"block",	do_block,		TRUE,	AM(Am_source),
	"var",		do_vars,		TRUE,	AM(Am_var),
	"expr",		do_expr,		TRUE,	AM(Am_var),
	"bphead",	do_bps,			TRUE,	AM(Am_bphead),
	"bpt",		do_bpt,			TRUE,	AM(Am_bpt),
	"sghead",	do_sgh,			TRUE,	AM(Am_sighead),
	"signal",	do_sig,			TRUE,	AM(Am_signal),
	"cbhead",	do_cbhead,		TRUE,	AM(Am_cbhead),
	"cblock",	do_cblock,		TRUE,	AM(Am_cblock),
	"envhead",	do_envhead,		TRUE,	AM(Am_envhead),
	"env",		do_env,			TRUE,	AM(Am_env),
	"pmenu",	do_permanent_menu,	FALSE,	AM(Am_pmenu),
	"tmenu",	do_target_menu,		FALSE,	AM(Am_tmenu),
	"outwin",	do_output_window_menu,	FALSE,	AM(Am_outwin),
	"srcwin",	do_source_window_menu,	FALSE,	AM(Am_srcwin),
};

static int
r_flush()
{
	if (fflush(Record_fp) == EOF || ferror(Record_fp)) {
		sysf("Error writing to %s (%m)", Record_file_name);
		return -1;
	}

	return 0;
}

static void
do_permanent_menu(unused_objid, cmd)
objid_t unused_objid;
int cmd;
{
	permanent_menu_func((char *)NULL, -1, cmd);
}

static void
do_target_menu(unused_objid, cmd)
objid_t unused_objid;
int cmd;
{
	if (Have_window) {
		target_menu_index_t tmi;
		target_menu_info_t *tm;
		int md;

		switch (cmd) {
			case MR_TGT_RUN:  tmi = TM_START;	break;
			case MR_TGT_CONT: tmi = TM_CONT;	break;
			case MR_TGT_STEP: tmi = TM_STEP;	break;
			case MR_TGT_NEXT: tmi = TM_NEXT;	break;
			case MR_TGT_STOP: tmi = TM_STOP;	break;
			case MR_TGT_KILL: tmi = TM_EXIT;	break;
			default: panic("bad cmd"); tmi = TM_EXIT;
		}
		tm = get_target_menu_info();
		md = tm->tm_mdtab[(int)tmi].md;

		target_menu_func((char *)NULL, md, cmd);
	}
	else {
		do_menu_target_command(cmd);
	}
}

static void
do_source_window_menu(unused_objid, cmd)
objid_t unused_objid;
int cmd;
{
	panic("dswm NYI");
}

static void
do_output_window_menu(unused_objid, cmd)
objid_t unused_objid;
int cmd;
{
	panic("dowm NYI");
}

static const char *
rv_to_name(mc, rv)
mcmds_t *mc;
int rv;
{
	static char buf[20];
	int i;

	for (i = 0; i < mc->mc_actmap_size; ++i)
		if (rv == mc->mc_actmap[i].am_rv)
			return mc->mc_actmap[i].am_name;
	
	strnf(buf, sizeof(buf), "<%d>", rv);
	return buf;
}

static int
name_to_rv(mc, name, p_rv)
mcmds_t *mc;
const char *name;
int *p_rv;
{
	int i;
	char junkc;

	for (i = 0; i < mc->mc_actmap_size; ++i) {
		if (strcmp(name, mc->mc_actmap[i].am_name) == 0) {
			*p_rv = mc->mc_actmap[i].am_rv;
			return 0;
		}
	}
	
	if (sscanf(name, "<%d>%c", p_rv, &junkc) == 1)
		return 0;

	return -1;
}

static void
list_rvs(fp, mc)
FILE *fp;
mcmds_t *mc;
{
	int i;

	fprintf(fp, "Return value names for menu `%s':", mc->mc_name);

	for (i = 0; i < mc->mc_actmap_size; ++i)
		fprintf(fp, " %s", mc->mc_actmap[i].am_name);
	
	fputc('\n', fp);
	fflush(fp);
}

static mcmds_t *
mname_to_menu(name)
const char *name;
{
	int i;

	for (i = 0; i < NEL(Mnames); ++i)
		if (strcmp(Mnames[i].mc_name, name) == 0)
			return &Mnames[i];
	
	return NULL;
}

static void
list_mnames(fp)
FILE *fp;
{
	int i;

	fputs("Menu names:", fp);

	for (i = 0; i < NEL(Mnames); ++i)
		fprintf(fp, " %s", Mnames[i].mc_name);
	
	fputc('\n', fp);
	fflush(fp);
}

static bool
mf_spaces(unused_fline, p_uline)
const char *unused_fline, **p_uline;
{
	const char *s;

	for (s = *p_uline; isspace(*s); ++s)
		;
	
	if (s == *p_uline)
		return FALSE;

	*p_uline = s;
	return TRUE;
}

static bool
mf_value(fline, p_uline)
const char *fline, **p_uline;
{
	char *endstr;
	long val;

	val = strtol(*p_uline, &endstr, (fline[1] == 'h') ? 16 : 10);

	if (endstr == *p_uline)
		return FALSE;
	
	*p_uline = endstr;
	return TRUE;
}

static bool
mf_junkfuncptr(unused_fline, p_uline)
const char *unused_fline, **p_uline;
{
	const char *s;
	bool need_close;

	s = *p_uline;

	if (need_close = *s == '(')
		++s;
	
	while (isalnum(*s) || (*s != '\0' && strchr("_$+[]", *s) != NULL))
		++s;
	
	if (need_close) {
		if (*s++ != ')')
			return FALSE;
	}

	if (s[0] != '(' || s[1] != ')')
		return FALSE;

	*p_uline = s + 2;
	return TRUE;
}

static bool
mf_junkstring(fline, p_uline)
const char *fline, **p_uline;
{
	static const char nullstr[] = "*NULL";
	const char *s;

	if (mf_junkaddr(fline, p_uline))
		return TRUE;
	
	if (strncmp(*p_uline, nullstr, sizeof(nullstr) - 1) == 0) {
		*p_uline += sizeof(nullstr) - 1;
		return TRUE;
	}

	if (**p_uline != '"')
		return FALSE;
	
	for (s = *p_uline + 1; *s != '"' && *s != '\0'; ++s)
		if (*s == '\\' && s[1] == '"')
			++s;
	
	if (*s == '"')
		++s;
	
	*p_uline = s;
	return TRUE;
}

static bool
mf_junkaddr(unused_fline, p_uline)
const char *unused_fline, **p_uline;
{
	static const char badaddr[] = "Bad address ";
	char *endstr;

	if (strncmp(*p_uline, "0x", 2) == 0) {
		strtol(*p_uline, &endstr, 16);

		if (endstr > *p_uline) {
			*p_uline = endstr;
			return TRUE;
		}
	}

	if (strncmp(*p_uline, badaddr, sizeof(badaddr) - 1) == 0) {
		strtol(*p_uline + sizeof(badaddr) - 1, &endstr, 0);

		if (endstr > *p_uline) {
			*p_uline = endstr;
			return TRUE;
		}
	}
	
	return FALSE;
}

static bool
mf_addr(unused_fline, p_uline)
const char *unused_fline, **p_uline;
{
	char *endstr;
	long val;

	if (strncmp(*p_uline, "0x", 2) != 0)
		return FALSE;

	val = strtol(*p_uline, &endstr, 16);

	if (val == 0 || endstr == *p_uline)
		return FALSE;
	
	*p_uline = endstr;
	return TRUE;
}

static bool
mf_choice(fline, p_uline)
const char *fline, **p_uline;
{
	const char *s;

	fline += 2;
	s = fline;

	for (;;) {
		if (*s == '\0')
			panic("val botch in mfc");

		if (*s == '|' || *s == CLOSE_BRACE) {
			if (strncmp(*p_uline, fline, s - fline) == 0) {
				*p_uline += s - fline;
				return TRUE;
			}

			if (*s == CLOSE_BRACE)
				return FALSE;
			
			fline = s + 1;
		}

		++s;
	}
}

static bool
lines_match(fline, uline)
const char *fline, *uline;
{
	static struct {
		const char *name;
		bool (*func)PROTO((const char *ffline, const char **p_uline));
	} mf[] = {
		"{addr}",		mf_addr,
		"{value}",		mf_value,
		"{hex}",		mf_value,
		"{junkaddr}",		mf_junkaddr,
		"{junkfuncptr}",	mf_junkfuncptr,
		"{junkstring}",		mf_junkstring,
		"{spaces}",		mf_spaces,
		"{?",			mf_choice,
	};

	for (;;) {
		int i;
		const char *endf;

		if (*fline == *uline) {
			if (*fline == '\0')
				return TRUE;
			++fline;
			++uline;
			continue;
		}

		if ((endf = strchr(fline, CLOSE_BRACE)) == NULL)
			return FALSE;

		for (i = 0; i < NEL(mf); ++i) {
			if (strncmp(mf[i].name, fline, strlen(mf[i].name)) == 0)
				break;
		}
		
		if (i == NEL(mf) || !(*mf[i].func)(fline, &uline))
			return FALSE;
		
		fline = endf + 1;
	}
}

static int
checkline(upsline)
const char *upsline;
{
	char *line;

	for (;;) {
		char *cond;
		bool cond_holds;

		if (getline(FALSE, &line) != 0)
			return -1;
		
		if (*line == '\t')
			break;

		if (strcmp(line, "endcheck") == 0) {
			if (upsline == NULL)
				return 0;
			sysf("Unexpected extra ups output line `%s'", upsline);
			return -1;
		}

	 	if (strncmp(line, "if ", 3) != 0) {
			sysf("Unexpected extra line `%s'", line);
			return -1;
		}
		
		cond = line + 3;
		for (line = cond; !isspace(*line) && *line != '\0'; ++line)
			;
		if (*line != '\0')
			*line++ = '\0';
		
		if (check_cond(cond, &cond_holds) != 0)
			return -1;
		
		if (cond_holds)
			break;
	}

	if (upsline == NULL) {
		sysf("Unexpected end of ups output\n\tExpected: `%s'", line);
		return -1;
	}

	while (isspace(*line))
		++line;
	while (isspace(*upsline))
		++upsline;

	return cmp_lines("line", line, upsline);
}

static void
record_obj_dump(obj)
objid_t obj;
{
	show_obj_path("check", obj, '\n', Record_fp);

	dump_object(obj, (char *)NULL, OBJ_SELF);
	dump_object(obj, (char *)NULL, OBJ_DESCENDENTS);

	fputs("endcheck\n", Record_fp);
	r_flush();
}

static int
skip_to(word)
const char *word;
{
	char *line;

	while (getline(TRUE, &line) == 0) {
		if (strcmp(line, word) == 0)
			return 0;
	}
	
	return -1;
}

static int
check_shell_line(line)
const char *line;
{
	int res;
	char *shell_line, *linecopy, *file_line;

	/*  The line to be checked came from fpgetline() and getline()
	 *  also calls fpgetline(), so stash the line.
	 */
	linecopy = shell_line = strsave(line);

	if (getline(TRUE, &file_line) != 0)
		return -1;
	
	while (isspace(*shell_line))
		++shell_line;
	
	res = cmp_lines("Shell output", file_line, shell_line);

	free(linecopy);
	return res;
}

static int
cf_shellcmd(cmd, argc, args)
const char *cmd;
int argc;
char **args;
{
	char *line, *shellcmd;
	const char *usercmd;
	int res, status;
	bool expect_output;
	FILE *fp;

	if (!Runcmds)
		return skip_to("endshell");

	expect_output = strcmp(cmd, "shell") == 0;

	shellcmd = strf("2>&1 %s", args[0]);
	usercmd = shellcmd + 5;		/* Don't show redir in error messages */

	if ((fp = popen(shellcmd, "r")) == NULL) {
		sysf("Can't run `%s'", usercmd);
		free(shellcmd);
		return -1;
	}

	res = 0;
	while ((line = fpgetline(fp)) != NULL) {
		if (expect_output) {
			if (check_shell_line(line) != 0) {
				res = -1;
				break;
			}
		}
		else {
			if (res == 0) {
				sysf("Unexpected output from `%s':", usercmd);
				res = -1;
			}
			fprintf(stderr, "\t%s\n", line);
		}
	}

	if (ferror(fp)) {
		sysf("Error reading output from `%s'", usercmd);
		res = -1;
	}

	if ((status = pclose(fp)) != 0) {
		if ((status & 0x7f) != 0) {
			sysf("`%s' died of signal %d%s", usercmd,
				status & 0x7f,
				(status & 0x80) ? " (and dumped core)" : "");
		}
		else {
			sysf("`%s' exited with status %d",
						usercmd, (status >> 8) & 0xff);
		}
		res = -1;
	}

	if (expect_output && res == 0) {
		if (getline(TRUE, &line) != 0)
			res = -1;
		else if (strcmp(line, "endshell") != 0) {
			sysf("Expected `endshell'");
			res = -1;
		}
	}

	free(shellcmd);
	return res;
}

static int
cf_check(cmd, argc, args)
const char *cmd;
int argc;
char **args;
{
	objid_t obj;

	if (!Runcmds)
		return skip_to("endcheck");

	if (argc != 1) {
		sysf("Usage: %s objpath", cmd);
		return -1;
	}

	if ((obj = path_to_obj(args[0])) == NULL)
		return -1;
	
	if (dump_object(obj, (char *)NULL, OBJ_SELF) != 0 ||
	    dump_object(obj, (char *)NULL, OBJ_DESCENDENTS) != 0)
		return -1;
	
	return checkline((const char *)NULL);
}

void
td_set_default_obj_to_selection()
{
	sel_t *sel;

	sel = get_selection();
	
	if (sel == NULL)
		Default_obj = NULL;
	else if (sel->se_next != NULL)
		sysf("More than one object selected");
	else
		Default_obj = sel->se_code;
}

void
td_check(path)
const char *path;
{
	if (Record_fp == NULL) {
		sysf("Not recording");
		return;
	}

	while (isspace(*path) && *path != '\0')
		++path;

	if (*path == '\0') {
		if (Default_obj == NULL) {
			sysf("No default object");
			return;
		}
		record_obj_dump(Default_obj);
	}
	else if (strcmp(path, "<sel>") == 0) {
		sel_t *sel;

		for (sel = get_selection(); sel != NULL; sel = sel->se_next)
			record_obj_dump(sel->se_code);
	}
	else {
		objid_t obj;

		if ((obj = path_to_obj(path)) != NULL)
			record_obj_dump(obj);
	}
}

#ifdef __STDC__
static void
sysf(const char *fmt, ...)
{
#else
static void
sysf(va_alist)
va_dcl
{
	const char *fmt;
#endif
	va_list args;
	char buf[100];
	char *line;

#ifdef __STDC__
	va_start(args, fmt);
#else
	va_start(args);
	fmt = va_arg(args, const char *);
#endif
	line = formf(buf, sizeof(buf), fmt, args);
	va_end(args);

	if (!Interactive && Replay_file_name != NULL && Replay_lnum != 0)
		fprintf(stderr, "%s,%d: ", Replay_file_name, Replay_lnum);
	

	fputs(line, stderr);
	fputc('\n', stderr);
}

#ifdef __STDC__
int
td_outf(int level, const char *fmt, ...)
{
#else
int
td_outf(va_alist)
va_dcl
{
	const char *fmt;
	int level;
#endif
	va_list args;
	char buf[100];
	char *line;

#ifdef __STDC__
	va_start(args, fmt);
#else
	va_start(args);
	level = va_arg(args, int);
	fmt = va_arg(args, const char *);
#endif

	line = formf(buf, sizeof(buf), fmt, args);
	va_end(args);

	if (Record_fp != NULL) {
		fprintf(Record_fp, "\t%*s%s\n", level * 4, "", line);
		if (r_flush() != 0)
			return -1;
	}

	if (Replay_fp != NULL && checkline(line) != 0)
		return -1;
	
	if (Record_fp == NULL && Replay_fp == NULL)
		printf("%*s%s\n", level * 4, "", line);

	if (line != buf)	/* not obtained from malloc */
		free(line);
	
	return 0;
}

bool
td_set_select_recording(val)
bool val;
{
	bool oldval;

	oldval = Want_select_recording;
	Want_select_recording = val;
	return oldval;
}

void
td_record_select(obj, flags)
objid_t obj;
int flags;
{
	const char *op;

	if (Record_fp == NULL || !Want_select_recording)
		return;
	
	if ((flags & (SEL_CLEARING | SEL_CHANGING | SEL_DELETING))
							!= SEL_CHANGING)
		return;
	
	if ((flags & SEL_ON) != 0)
		op = (get_num_selected() > 1) ? "addselect" : "select";
	else
		op = "deselect";

	show_obj_path(op, obj, '\n', Record_fp);
}

static void
show_obj_path(op, obj, endc, fp)
const char *op;
objid_t obj;
int endc;
FILE *fp;
{
	alloc_id_t alloc_id;
	int i, ncomp;
	const char **vec;

	alloc_id = alloc_create_pool();
	obj_to_vec(alloc_id, obj, &vec, &ncomp);

	fputs(op, fp);
	for (i = 0; i < ncomp; ++i)
		fprintf(fp, "%c%s", (i == 0) ? ' ' : '|', vec[i]);
	
	fputc(endc, fp);
	fflush(fp);

	alloc_free_pool(alloc_id);
}

void
td_record_menu_command(mname, rv)
const char *mname;
int rv;
{
	const char *rv_name;
	mcmds_t *mc;

	if (Record_fp == NULL)
		return;
	
	if ((mc = mname_to_menu(mname)) == NULL)
		panic("unknown mname");

	rv_name = rv_to_name(mc, rv);

	fprintf(Record_fp, "menu %s %s\n", mname, rv_name);
	r_flush();
}

void
td_record_debug_command(cmd)
const char *cmd;
{
	if (Record_fp != NULL) {
		fprintf(Record_fp, "debug %s\n", cmd);
		r_flush();
	}
}

void
td_record_show_var(fil, lnum, name)
fil_t *fil;
int lnum;
const char *name;
{
	if (Record_fp != NULL) {
		if (fil != NULL) {
			fprintf(Record_fp, "showvar %s %d %s\n",
						fil->fi_name, lnum, name);
		}
		else {
			fprintf(Record_fp, "show %s\n", name);
		}

		r_flush();
	}
}

bool
td_set_displayed_source(fil, lnum, op)
fil_t *fil;
int lnum;
const char *op;
{
	/*  We don't want the initial source display of main to be the
	 *  first line in the file.  This is because it would then come
	 *  before the `version' line and any SCCS comment.  Thus this
	 *  flag, which is set by a call to us with fil==NULL after the
	 *  initial source display (see ups()).
	 */
	static bool no_source_stuff_yet = TRUE;

	if (fil == NULL) {
		no_source_stuff_yet = FALSE;
		return FALSE;
	}

	if (Record_fp != NULL || Replay_fp != NULL)
		Current_displayed_fil = fil;
	
	if (no_source_stuff_yet)
		return !Have_window;

	if (Record_fp != NULL) {
		fprintf(Record_fp, "%s %s %d\n", op, fil->fi_name,lnum);
		r_flush();
	}

	if (Replay_fp != NULL && Want_displayed_source_check) {
		char *line;

		line = strf("%s %d", fil->fi_name, lnum);
		if (check_message(op, line) != 0)
			Check_message_failed = TRUE;
		free(line);
	}
	
	return !Have_window;
}

int
td_get_displayed_fil(p_fil)
fil_t **p_fil;
{
	if (Have_window)
		return -1;
	
	*p_fil = Current_displayed_fil;
	return 0;
}

void
td_record_func_and_lnum_cmd(f, lnum, op)
func_t *f;
int lnum;
const char *op;
{
	if (Record_fp != NULL) {
		fprintf(Record_fp, "%s %s %s %d\n", op,
					f->fu_fil->fi_name, f->fu_name, lnum);
		r_flush();
	}
}

static void
record_start_edit(dr)
draw_t *dr;
{
	if (Record_fp != NULL) {
		show_obj_path("edit", dr->dr_code, ' ', Record_fp);
		Doing_record_edit = TRUE;
	}
}

static void
record_edit(res, what, value)
int res;
const char *what, *value;
{
	if (Doing_record_edit) {
		fprintf(Record_fp, "%s\n", what);
		Doing_record_edit = FALSE;
	}

	switch (res) {
	case EDR_CANCEL:
		fprintf(Record_fp, "%s: <cancel>\n", what);
		break;
	case EDR_CONFIRM_NO_CHANGE:
		fprintf(Record_fp, "%s: <confnochange>\n", what);
		break;
	case EDR_CONFIRM_CHANGE:
		fprintf(Record_fp, "%s: %s\n", what, value);
		break;
	default:
		panic("ef value botch");
	}
	r_flush();
}


void
td_record_adjust_index(what, curpos, up)
const char *what;
int curpos;
bool up;
{
	if (Record_fp != NULL) {
		if (Doing_record_edit) {
			fprintf(Record_fp, "%s\n", what);
			Doing_record_edit = FALSE;
		}

		fprintf(Record_fp, "<adjust-index %d %d>\n", up, curpos);
		r_flush();
	}
}

void
td_record_refresh()
{
	if (Record_fp != NULL) {
		fputs("refresh\n", Record_fp);
		r_flush();
	}
}

static int
cf_version(cmd, argc, args)
const char *cmd;
int argc;
char **args;
{
	char *endstr;
	int version;

	if (argc == 0) {
		printf("Version %d - %s\n", CURRENT_VERSION, _ups_sccsdata[0]);
		return 0;
	}

	version = strtol(args[0], &endstr, 10);

	if (endstr == args[0] || *endstr != '\0' || version < 0) {
		sysf("Version `%s' is not a positive integer", args[0]);
		return -1;
	}

	if (version > CURRENT_VERSION) {
		sysf("Warning: expected file version <= %d - found %d",
						CURRENT_VERSION, version);
	}

	return 0;
}

static int
cf_cond(cmd, argc, args)
const char *cmd;
int argc;
char **args;
{
	if (argc != 1) {
		sysf("Usage: %s cond", cmd);
		return -1;
	}

	return handle_cond(cmd, args[0]);
}

static int
cond_exists(cond, len)
const char *cond;
int len;
{
	static const char *conds[] = {
		"sun3", "sun386", "sun4", "vax", "i386", "bsdi386", "clipper",
		"mips", "sunos4", "43bsd", "ultrix",
		"unknown", "never", "verbose"
	};
	int i;

	for (i = 0; i < NEL(conds); ++i)
		if (strncmp(conds[i], cond, len) == 0 && conds[i][len] == '\0')
			return TRUE;

	return FALSE;
}

static int
check_cond(cond, p_cond_holds)
const char *cond;
bool *p_cond_holds;
{
	const char *os;
	int condlen;
	bool invert, holds;

	if (invert = (*cond == '!'))
		++cond;
	
	if ((os = strchr(cond, '_')) != 0) {
		condlen = os - cond;
		++os;
	}
	else {
		condlen = strlen(cond);
		os = NULL;
	}
	
	if (!cond_exists(cond, condlen) ||
			    (os != NULL && !cond_exists(os, strlen(os)))) {
		sysf("Unknown if condition `%s'", cond);
		return -1;
	}

	holds = (strncmp(cond, THIS_ARCH, condlen) == 0 &&
		 (os == NULL || strcmp(os, THIS_OS) == 0)) ||
		(Want_verbose_test && strcmp(cond, "verbose") == 0);
	
	*p_cond_holds = invert ? !holds : holds;
	return 0;
}

static const char *
not(cond)
const char *cond;
{
	static char *last = NULL;

	if (*cond == '!')
		return cond + 1;
	
	if (last != NULL)
		free(last);
	
	return last = strf("!%s", cond);
}

static int
handle_cond(cmd, cond)
const char *cmd, *cond;
{
	typedef struct condlist_s {
		char *cond;
		bool seen_else;
		int lnum;
		bool old_runcmds;
		struct condlist_s *outer;
	} condlist_t;
	static condlist_t *condlist = NULL;
	condlist_t *cl;
	const char *expected_cond;
	bool cond_holds, is_else;

	if (check_cond(cond, &cond_holds) != 0)
		return -1;

	if (strcmp(cmd, "if") == 0) {
		cl = (condlist_t *)e_malloc(sizeof(condlist_t));
		cl->cond = strsave(cond);
		cl->seen_else = FALSE;
		cl->lnum = Replay_lnum;
		cl->old_runcmds = Runcmds;
		cl->outer = condlist;
		condlist = cl;

		if (!cond_holds)
			Runcmds = FALSE;

		return 0;
	}

	if ((cl = condlist) == NULL) {
		sysf("`%s' command with no corresponding `if'", cmd);
		return -1;
	}

	is_else = strcmp(cmd, "endif") != 0;
	if (is_else && strcmp(cmd, "else") != 0)
		panic("bad cmd in hc");

	if (is_else && cl->seen_else) {
		sysf("Already seen `else %s' on line %d",
						not(cl->cond), cl->lnum);
		return -1;
	}
		
	expected_cond = (is_else || cl->seen_else) ? not(cl->cond) : cl->cond;

	if (strcmp(cond, expected_cond) != 0) {
		sysf("`%s %s' doesn't match line %d", cmd, cond, cl->lnum);
		return -1;
	}

	if (is_else) {
		if (cl->old_runcmds)
			Runcmds = !Runcmds;
		
		cl->seen_else = TRUE;
		cl->lnum = Replay_lnum;
	}
	else if (strcmp(cmd, "endif") == 0) {
		Runcmds = cl->old_runcmds;
		condlist = cl->outer;

		free(cl->cond);
		free((char *)cl);
	}

	return 0;
}

static int
cf_message(cmd, argc, args)
const char *cmd;
int argc;
char **args;
{
	if (argc != 1)
		sysf("Usage: %s line", cmd);
	else
		sysf("Unexpected %s `%s'", cmd, args[0]);
	
	return -1;
}

static int
cf_echo(cmd, unused_argc, args)
const char *cmd;
int unused_argc;
char **args;
{
	const char *mesg;

	if (strcmp(cmd, "warn") == 0)
		printf("%s,%d: Warning - ", Replay_file_name, Replay_lnum);

	mesg = args[0];

	if (strncmp(mesg, "-n ", 3) == 0) {
		fputs(mesg + 3, stdout);
		fputc(' ', stdout);
	}
	else {
		puts(mesg);
	}

	return 0;
}

static int
cf_help(cmd, argc, unused_args)
const char *cmd;
int argc;
char **unused_args;
{
	int i;

	if (argc != 0) {
		sysf("Usage: %s", cmd);
		return -1;
	}

	fputs("Commands:", stdout);

	for (i = 0; i < sizeof Cmdtab / sizeof *Cmdtab; ++i)
		printf(" %s", Cmdtab[i].name);

	fputc('\n', stdout);
	fflush(stdout);

	return 0;
}

static int
cf_checksrc(cmd, argc, args)
const char *cmd;
int argc;
char **args;
{
	if (argc != 1 || (strcmp(args[0], "off") != 0 &&
						strcmp(args[1], "on") != 0)) {
		sysf("Usage: %s on|off", cmd);
		return -1;
	}

	Want_displayed_source_check = strcmp(args[0], "on") == 0;

	return 0;
}

static int
cf_replaymode(cmd, argc, args)
const char *cmd;
int argc;
char **args;
{
	if (argc != 1) {
		sysf("Usage: %s wait|waitcheck|echo|noecho|verbose", cmd);
		return -1;
	}

	return td_set_replay_mode(args[0]);
}

int
td_set_replay_mode(cmd)
const char *cmd;
{
	const char *mode;

	if ((mode = strchr(cmd, ':')) != NULL) {
		char *endstr;
		int lnum;

		lnum = strtol(cmd, &endstr, 10);

		if (endstr != mode || lnum <= 0) {
			sysf("Line number `%.*s' is not a positive integer",
						 	     mode - cmd, cmd);
			return -1;
		}

		if (Set_replay_mode_mode != NULL) {
			sysf("Can't have multiple -replaymode lnum:mode flags");
			return -1;
		}

		Set_replay_mode_lnum = lnum;
		Set_replay_mode_mode = mode + 1;

		return 0;
	}

	if (strcmp(cmd, "verbose") == 0) {
		Want_verbose_test = TRUE;
	}
	else if (strcmp(cmd, "wait") == 0) {
		Want_wait_for_user = TRUE;
		Want_cmd_echo = TRUE;
		Want_wait_on_check = FALSE;
	}
	else if (strcmp(cmd, "waitcheck") == 0) {
		Want_wait_for_user = FALSE;
		Want_cmd_echo = TRUE;
		Want_wait_on_check = TRUE;
	}
	else if (strcmp(cmd, "echo") == 0) {
		Want_wait_for_user = FALSE;
		Want_cmd_echo = TRUE;
		Want_wait_on_check = FALSE;
	}
	else if (strcmp(cmd, "noecho") == 0) {
		Want_wait_for_user = FALSE;
		Want_cmd_echo = FALSE;
		Want_wait_on_check = FALSE;
	}
	else {
		sysf("Unknown replay type `%s'", cmd);
		return -1;
	}

	return 0;
}

static int
cf_refresh(cmd, argc, unused_args)
const char *cmd;
int argc;
char **unused_args;
{
	unsigned code;

	if (argc != 0) {
		sysf("Usage: %s", cmd);
		return -1;
	}

	code = (strcmp(cmd, "resize") == 0) ? EV_WINDOW_RESIZED
					    : EV_WINDOW_EXPOSED;

	td_record_refresh();

	if (Have_window)
		re_redraw_root(code, TRUE);

	return 0;
}

static int
cf_menu(cmd, argc, args)
const char *cmd;
int argc;
char **args;
{
	const char *mname, *action;
	mcmds_t *mc;
	int rv;

	if (argc != 2 && !(argc == 1 && strcmp(args[0], "?") == 0)) {
		sysf("Usage: %s menu_name action", cmd);
		return -1;
	}
	mname = args[0];
	action = args[1];

	if ((mc = mname_to_menu(mname)) == NULL) {
		if (strcmp(mname, "?") == 0) {
			list_mnames(stdout);
			return 0;
		}
		sysf("Unknown menu name `%s'%s", mname,
				Interactive ? " - say `menu ?' for list" : "");
		return -1;
	}

	if (name_to_rv(mc, action, &rv) != 0) {
		if (strcmp(action, "?") == 0) {
			list_rvs(stdout, mc);
			return 0;
		}

		if (Interactive) {
			sysf("Unknown menu action `%s' - say `menu %s ?' for list",
								action, mname);
		}
		else {
			sysf("Unknown menu action `%s'", action);
		}

		return -1;
	}

	td_record_menu_command(mname, rv);

	if (!mc->mc_apply_to_objs) {
		(*mc->mc_func)((objid_t)NULL, rv);
	}
	else {
		int cur_objtype;

		cur_objtype = get_cur_objtype();

		if (cur_objtype == OT_NO_TYPE || get_num_selected() == 0) {
			sysf("No object selected");
			return -1;
		}

		if (cur_objtype < 0 || cur_objtype > OT_MAXTYPE)
			panic("cur_objtype bad in cm");

		if (mc->mc_func != Objtab[cur_objtype].ot_mfunc) {
			sysf("Wrong menu type for currently selected objects");
			return -1;
		}

		dynamic_menu_func((char *)NULL, -1, rv);
	}

	return 0;
}

static int
cf_bptcode(cmd, argc, args)
const char *cmd;
int argc;
char **args;
{
	typedef struct line_s {
		const char *line;
		struct line_s *next;
	} line_t;
	const char **lines;
	char *line;
	line_t *linelist, *lp;
	int lnum, nlines;
	objid_t obj;
	alloc_id_t alloc_id;

	if (!Runcmds)	
		return skip_to("endcode");

	if (argc != 1) {
		sysf("Usage: %s objpath", cmd);
		return -1;
	}

	if ((obj = path_to_obj(args[0])) == NULL)
		return -1;

	alloc_id = alloc_create_pool();

	nlines = 0;
	linelist = NULL;
	while (getline(FALSE, &line) == 0 && strcmp(line, "endcode") != 0) {
		if (*line == '\t')
			++line;

		lp = (line_t *)alloc(alloc_id, sizeof(line_t));
		lp->line = alloc_strdup(alloc_id, line);
		lp->next = linelist;
		linelist = lp;

		++nlines;
	}

	if (line == NULL) {
		alloc_free_pool(alloc_id);
		return -1;
	}

	lines = (const char **)alloc(alloc_id, nlines * sizeof(const char *));

	lnum = nlines;
	lp = linelist;
	while (--lnum >= 0) {
		lines[lnum] = lp->line;
		lp = lp->next;
	}

	change_bpt_lines(obj, lines, nlines);

	alloc_free_pool(alloc_id);
	return 0;
}

static int
cf_edit(cmd, argc, args)
const char *cmd;
int argc;
char **args;
{
	objid_t obj;
	int objtype;
	fnamemap_t *fmtab, *fm;
	char *path, *fname;

	if (argc != 1) {
		sysf("Usage: %s objpath fname", cmd);
		return -1;
	}

	if (!Runcmds) {
		char *line;

		do {
			if (getline(TRUE, &line) != 0)
				return -1;
		} while (strchr(line, ':') == 0);

		return 0;
	}

	path = args[0];

	if ((fname = strrchr(path, ' ')) == NULL) {
		sysf("Usage: %s objpath fname", cmd);
		return -1;
	}
	*fname++ = '\0';

	if ((obj = path_to_obj(args[0])) == NULL)
		return -1;
	
	objtype = ups_get_object_type(obj);

	if (objtype < 0 || objtype > OT_MAXTYPE)
		panic("objtype bad in ce");
	fmtab = Objtab[objtype].ot_fnamemap;

	if (fmtab == NULL) {
		sysf("Object `%s' does not have any editable fields", path);
		return -1;
	}

	for (fm = fmtab; fm->fm_name != NULL; ++fm)
		if (strcmp(fm->fm_name, fname) == 0)
			break;
	
	if (fm->fm_name == NULL) {
		sysf("No editable field `%s' for object `%s'", fname, path);
		return -1;
	}

	obj_edit_field(obj, fm->fm_fnum, 0, 0);
	return 0;
}

static int
cf_select(cmd, argc, args)
const char *cmd;
int argc;
char **args;
{
	objid_t obj;
	bool on, clear;

	if (argc != 1) {
		sysf("Usage: %s objpath", cmd);
		return -1;
	}
	clear = strcmp(cmd, "select") == 0;
	on = clear || strcmp(cmd, "addselect") == 0;

	if (clear)
		clear_selection();

	if ((obj = path_to_obj(args[0])) == NULL)
		return -1;
	
	select_object(obj, on, OBJ_SELF);
	Obj_to_make_visible = obj;

	return 0;
}

static objid_t
path_to_obj(path)
const char *path;
{
	char **vec;
	int ncomp;
	objid_t obj;

	vec = ssplit(path, "|");
	for (ncomp = 0; vec[ncomp] != NULL; ++ncomp)
		;
	
	obj = vec_to_obj((const char **)vec, ncomp);

	free((char *)vec);

	if (obj == NULL)
		sysf("Unknown object path `%s'", path);

	return obj;
}

static int
cf_debug(cmd, argc, args)
const char *cmd;
int argc;
char **args;
{
	if (argc != 1) {
		sysf("Usage: %s cmdline", cmd);
		return -1;
	}

	do_debug_command(args[0]);
	return 0;
}

static int
cf_show(cmd, argc, args)
const char *cmd;
int argc;
char **args;
{
	srcwin_id_t srcwin_id;

	if (argc != 1) {
		sysf("Usage: %s var", cmd);
		return -1;
	}

	srcwin_id = Have_window ? get_current_srcwin() : NULL;
	show_var_from_typing_line(srcwin_id, args[0]);

	td_record_show_var((fil_t *)NULL, 0, args[0]);
	return 0;
}

static int
cf_show_var(cmd, argc, args)
const char *cmd;
int argc;
char **args;
{
	const char *filename, *varname;
	char *endstr;
	fil_t *fil;
	srcwin_id_t srcwin_id;
	int lnum;

	if (argc != 3) {
		sysf("Usage: %s filename lnum varname", cmd);
		return -1;
	}
	filename = args[0];
	lnum = strtol(args[1], &endstr, 10);
	varname = args[2];

	if (endstr == args[1] || *endstr != '\0') {
		sysf("Line number `%s' is not a number", args[1]);
		return -1;
	}

	if ((fil = name_to_fil(filename)) == NULL) {
		sysf("Unknown filename `%s'", filename);
		return -1;
	}

	srcwin_id = Have_window ? get_current_srcwin() : NULL;
	show_var(srcwin_id, fil, lnum, varname);

	td_record_show_var(fil, lnum, varname);
	
	return 0;
}

static int
cf_to_lnum(cmd, argc, args)
const char *cmd;
int argc;
char **args;
{
	const char *filename, *funcname;
	char *endstr;
	fil_t *fil;
	func_t *f;
	int lnum;

	if (argc != 3) {
		sysf("Usage: %s filename funcname lnum", cmd);
		return -1;
	}
	filename = args[0];
	funcname = args[1];
	lnum = strtol(args[2], &endstr, 10);

	if (endstr == args[2] || *endstr != '\0') {
		sysf("Line number `%s' is not a number", args[2]);
		return -1;
	}

	if ((fil = name_to_fil(filename)) == NULL) {
		sysf("Unknown filename `%s'", filename);
		return -1;
	}

	if ((f = name_and_fil_to_func(funcname, fil)) == NULL) {
		sysf("Can't find function `%s' in file %s", funcname, filename);
		return -1;
	}

	td_record_func_and_lnum_cmd(f, lnum, cmd);

	if (strcmp(cmd, "execto") == 0)
		exec_to_lnum(f, lnum);
	else
		Obj_to_make_visible = add_breakpoint_object(f, lnum);
	
	return 0;
}

static cmdfunc_t
cmd_to_func(cmd, p_splitargs, p_alwaysexec)
const char *cmd;
bool *p_splitargs, *p_alwaysexec;
{
	int i;

	for (i = 0; i < sizeof Cmdtab / sizeof *Cmdtab; ++i) {
		if (strcmp(cmd, Cmdtab[i].name) == 0) {
			*p_splitargs = Cmdtab[i].splitargs;
			*p_alwaysexec = Cmdtab[i].alwaysexec;
			return Cmdtab[i].func;
		}
	}
	
	return NULL;
}

static int
process_command(line)
char *line;
{
	cmdfunc_t func;
	int argc, res;
	char *s, *cmd, **args;
	char *onearg[2];
	bool splitargs, alwaysexec;

	for (s = line; !isspace(*s) && *s != '\0'; ++s)
		;
	
	cmd = line;

	if (*s != '\0')
		*s++ = '\0';
	line = s;

	if ((func = cmd_to_func(cmd, &splitargs, &alwaysexec)) == NULL) {
		sysf("Unknown command `%s'%s", cmd,
				Interactive ? " - say `help' for a list" : "");
		return -1;
	}

	if (!Runcmds && !alwaysexec)
		return 0;

	if (splitargs) {
		args = ssplit(line, " \t");
	}
	else {
		onearg[0] = line;
		onearg[1] = NULL;
		args = onearg;
	}

	for (argc = 0; args[argc] != NULL; ++argc)
		;

	Obj_to_make_visible = NULL;

	td_set_obj_updating(OBJ_UPDATING_OFF);
	res = (*func)(cmd, argc, args);
	td_set_obj_updating(OBJ_UPDATING_ON);

	if (Obj_to_make_visible != NULL && Have_window)
		ensure_visible(Obj_to_make_visible);

	if (splitargs)
		free((char *)args);
	
	return res;
}

int
td_replay_from(filename)
const char *filename;
{
	if (strcmp(filename, "-") == 0) {
		Replay_fp = stdin;
		Replay_file_name = "the standard input";
	}
	else {
		if ((Replay_fp = fopen(filename, "r")) == NULL) {
			sysf("Can't open driver file %s (%m)", filename);
			return -1;
		}
		Replay_file_name = filename;
	}

	Replay_lnum = 0;
	Check_message_failed = FALSE;
	Interactive = isatty(fileno(Replay_fp));

	return 0;
}

static void
wait_for_user(line)
const char *line;
{
	const char *cmd;

	for (cmd = line; isspace(*cmd) && *cmd != '\0'; ++cmd)
		;

	if (Runcmds && Want_cmd_echo && strcmp(cmd, "noecho") != 0) {
		if (!Interactive)
			printf("%4d ", Replay_lnum);
		fputs(line, stdout);
		fflush(stdout);

		if ((Want_wait_for_user && strcmp(line, "nowait") != 0) ||
		    (Want_wait_on_check && strncmp(line, "check ", 6) == 0)) {
			int ch, lastch;

			fputs(" [ncq] ", stdout);

			lastch = '\0';
			while ((ch = getchar()) != EOF && ch != '\n')
				lastch = ch;
			
			if (lastch == 'q')
				exit(0);

			if (lastch == 'c' || ch == EOF)
				Want_cmd_echo = FALSE;
		}
		else {
			fputc('\n', stdout);
			fflush(stdout);
		}
	}
}

static void
show_error_on_tty(mesg)
const char *mesg;
{
	if (Have_window)
		(*Error_func)(mesg);

	if (*mesg == '\b')
		++mesg;

	if (Record_fp != NULL) {
		fprintf(Record_fp, "message %s\n", mesg);
		r_flush();
	}

	if (Replay_fp != NULL && !Interactive) {
		if (check_message("message", mesg) != 0)
			Check_message_failed = TRUE;
	}
	else {
		fprintf(stderr, "%s\n", mesg);
	}
}

static int
check_message(what, mesg)
const char *what, *mesg;
{
	char *line;
	int len;

	if (getline(TRUE, &line) != 0)
		return -1;

	len = strlen(what);

	if (strncmp(line, what, len) != 0 || line[len] != ' ') {
		sysf("Expected `%s %s'", what, mesg);
		return -1;
	}
	line += strlen(what) + 1;

	return cmp_lines(what, line, mesg);
}

static int
cmp_lines(what, expected, got)
const char *what, *expected, *got;
{
	if (!lines_match(expected, got)) {
		sysf("%c%s mismatch", toupper(what[0]), &what[1]);
		fprintf(stderr, "Expected: `%s'\n", expected);
		fprintf(stderr, "     Got: `%s'\n", got);
		return -1;
	}

	return 0;
}

bool
td_set_obj_updating(val)
bool val;
{
	return Have_window ? updating(val) : FALSE;
}

bool
td_have_window()
{
	return Have_window;
}

static int
getline(lose_leading_whitespace, p_line)
bool lose_leading_whitespace;
char **p_line;
{
	char *line;

	if ((line = fpgetline(Replay_fp)) == NULL) {
		if (ferror(Replay_fp))
			sysf("Read error (%m)");
		return -1;
	}

	if (lose_leading_whitespace) {
		while (isspace(*line))
			++line;
	}
	
	++Replay_lnum;

	*p_line = line;
	return 0;
}

void
td_set_no_window_flag()
{
	Have_window = FALSE;
}

int
td_event_loop(p_eof)
bool *p_eof;
{
	char *line;
	int res;

	if (Have_window)
		set_bm_cursor(WN_STDWIN, CU_MENU);
	else
		set_dynamic_menu_updating_state(DMU_OFF);

	res = 0;

	if (Error_func == NULL)
		Error_func = errf_set_ofunc(show_error_on_tty);

	for (;;) {
		if (Interactive) {
			fputs("ups> ", stdout);
			fflush(stdout);
		}

		if (getline(FALSE, &line) != 0)
			break;

		if (Replay_lnum == Set_replay_mode_lnum) {
			if (td_set_replay_mode(Set_replay_mode_mode) != 0) {
				res = -1;
				break;
			}
		}

		line = config_trim_line(line);
		if (*line == '\0')
			continue;
		
		wait_for_user(line);

		res = process_command(line);

		/*  Quit scripts if there is an error.
		 */
		if (Check_message_failed)
			res = -1;
		if (res != 0  && !Interactive)
			break;

		if (re_get_exit_event_loop_flag())
			break;
	}

	if (ferror(Replay_fp)) {
		fclose(Replay_fp);
		return -1;
	}

	*p_eof = feof(Replay_fp);
	fclose(Replay_fp);
	Replay_fp = NULL;

	if (Have_window)
		errf_set_ofunc(Error_func);

	return res;
}

static int
replay_edit(ed, what)
edesc_t *ed;
const char *what;
{
	char *value;
	int len;

	for (;;) {
		int curpos;
		bool up;

		if (Interactive) {
			printf("Enter %s: ", what);
			fflush(stdout);
		}

		if (getline(TRUE, &value) != 0)
			exit(1);
		wait_for_user(value);

		if (sscanf(value, "<adjust-index %d %d>", &up, &curpos) == 2) {
			int oldval;

			ed->ed_curpos = curpos;
			adjust_index(ed, up);
			oldval = td_set_obj_updating(OBJ_UPDATING_ON);
			td_set_obj_updating(oldval);
		}
		else {
			break;
		}
	}

	if (!Interactive) {
		len = strlen(what);
		if (strncmp(value, what, len) != 0 || value[len] != ':' ||
						      value[len + 1] != ' ')
			panic(strf("Expected `%s:' but got `%s'", what, value));
		
		value += len + 2;
	}

	if (strcmp(value, "<cancel>") == 0)
		return EDR_CANCEL;
	else if (strcmp(value, "<confnochange>") == 0)
		return EDR_CONFIRM_NO_CHANGE;

	len = strlen(value);
	if (len > ed->ed_maxlen)
		len = ed->ed_maxlen;
	value[len] = '\0';
	
	strcpy(ed->ed_copy, value);

	return (*ed->ed_quitfunc)(ed, 0);
}

void
td_add_outwin()
{
	if (Have_window)
		add_outwin();
	else {
		outwin_id_t outwin_id;

		outwin_id = ow_make_outwin(-1, -1, -1, (font_t *)NULL);
		set_current_outwin(outwin_id);
	}
}

void
td_handle_output(so, old_nlines, new_nlines)
so_id_t so;
int old_nlines, new_nlines;
{
	int lnum;

	if (Record_fp != NULL) {
		for (lnum = old_nlines; lnum < new_nlines; ++lnum) {
			fprintf(Record_fp, "output %s\n",
							so_getline(so, lnum));
		}
	}

	if (Replay_fp != NULL && !Interactive) {
		for (lnum = old_nlines; lnum < new_nlines; ++lnum) {
			if (check_message("output",
						so_getline(so, lnum)) != 0) {
				Check_message_failed = TRUE;
				break;
			}
		}
	}
}

void
td_obj_edit_field(obj, fnum, x, y)
objid_t obj;
int fnum, x, y;
{
	if (Record_fp == NULL) {
		obj_edit_field(obj, fnum, x, y);
	}
	else {
		obj_pre_edit_func_t save_func;

		save_func = obj_set_pre_edit_func((obj_pre_edit_func_t)NULL);
		obj_edit_field(obj, fnum, x, y);
		obj_set_pre_edit_func(save_func);
	}
}

int
td_record_edit_field(ed, what)
edesc_t *ed;
const char *what;
{
	int res;

	if (Replay_fp != NULL)
		res = replay_edit(ed, what);	
	else
		res = edit_field(ed);

	if (Record_fp != NULL && what != NULL)
		record_edit(res, what, ed->ed_copy);

	return res;
}

void
td_record_bpt_code_edit(obj, lines, nlines)
objid_t obj;
const char **lines;
int nlines;
{
	int i;

	if (Record_fp == NULL)
		return;

	show_obj_path("bptcode", obj, '\n', Record_fp);

	for (i = 0; i < nlines; ++i)
		fprintf(Record_fp, "\t%s\n", lines[i]);
	
	fprintf(Record_fp, "endcode\n");
	r_flush();
}

int
td_record_to(filename)
const char *filename;
{
	if (strcmp(filename, "-") == 0) {
		Record_fp = stdout;
		Record_file_name = "the standard output";
	}
	else {
		struct stat stbuf;

		if (lstat(filename, &stbuf) == 0) {
			sysf("Record file %s already exists", filename);
			return -1;
		}
		if (errno != ENOENT) {
			sysf("Can't stat record file %s (%m)", filename);
			return -1;
		}
		if ((Record_fp = fopen(filename, "w")) == NULL) {
			sysf("Can't create record file %s (%m)", filename);
			return -1;
		}
		Record_file_name = filename;
	}


	if (Error_func == NULL)
		Error_func = errf_set_ofunc(show_error_on_tty);

	obj_set_pre_edit_func(record_start_edit);

	fprintf(Record_fp, "version %d - %s\n",
					CURRENT_VERSION, _ups_sccsdata[0]);
	r_flush();

	return 0;
}
