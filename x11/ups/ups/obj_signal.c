/* obj_signal.c - target signal handling routines */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_obj_signal_c_sccsid[] = "@(#)obj_signal.c	1.17 26/7/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <sys/types.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>

#include <local/wn.h>
#include <local/obj/obj.h>
#include <local/ukcprog.h>
#include "objtypes.h"

#include "ui.h"
#include "obj_signal.h"
#include "menudata.h"
#include "tdr.h"

static void add_signal_object PROTO((objid_t par, int sig));
static char *sig_attr_string PROTO((int sig));
static int sigcmp PROTO((objid_t obj1, objid_t obj2));

typedef struct sigst {
	const char *sig_name;
	int sig_attr;
} siginfo_t;

#define SIG_CONT		 01	/* continue automatically after sig */
#define SIG_REDRAW		 02	/* refresh display before continuing */
#define SIG_ACCEPT		 04	/* target accepts signal */
#define SIG_DFL_CORE		010	/* signal causes core dump by default */
#define SIG_DFL_TERMINATES	020	/* signal terminates process by default */
#define SIG_DFL_STOPS		040	/* signal stops target by default */

#define SIG_DFL_NO_EFFECT	  0	/* sig doesn't terminate proc by default */

static siginfo_t Sigtab[] = {
	"",		  0,		
	"SIGHUP    (1) ", SIG_DFL_TERMINATES | SIG_CONT | SIG_ACCEPT,
	"SIGINT    (2) ", SIG_DFL_TERMINATES,
	"SIGQUIT   (3) ", SIG_DFL_TERMINATES | SIG_DFL_CORE,
	"SIGILL    (4) ", SIG_DFL_TERMINATES | SIG_DFL_CORE,
	"SIGTRAP   (5) ", SIG_DFL_TERMINATES | SIG_DFL_CORE,
	"SIGIOT    (6) ", SIG_DFL_TERMINATES | SIG_DFL_CORE,
	"SIGEMT    (7) ", SIG_DFL_TERMINATES | SIG_DFL_CORE | SIG_CONT | SIG_ACCEPT,
	"SIGFPE    (8) ", SIG_DFL_TERMINATES | SIG_DFL_CORE | SIG_CONT | SIG_ACCEPT,
	"SIGKILL   (9) ", SIG_DFL_TERMINATES | SIG_CONT | SIG_ACCEPT,
	"SIGBUS    (10)", SIG_DFL_TERMINATES | SIG_DFL_CORE,
	"SIGSEGV   (11)", SIG_DFL_TERMINATES | SIG_DFL_CORE,
	"SIGSYS    (12)", SIG_DFL_TERMINATES | SIG_DFL_CORE,
	"SIGPIPE   (13)", SIG_DFL_TERMINATES,
	"SIGALRM   (14)", SIG_DFL_TERMINATES | SIG_CONT | SIG_ACCEPT,
	"SIGTERM   (15)", SIG_DFL_TERMINATES,
	"SIGURG    (16)", SIG_DFL_NO_EFFECT  | SIG_CONT | SIG_ACCEPT,
	"SIGSTOP   (17)", SIG_DFL_STOPS,	
	"SIGTSTP   (18)", SIG_DFL_STOPS      | SIG_CONT | SIG_ACCEPT,	
	"SIGCONT   (19)", SIG_DFL_NO_EFFECT  | SIG_CONT | SIG_ACCEPT,
	"SIGCHLD   (20)", SIG_DFL_NO_EFFECT  | SIG_CONT | SIG_ACCEPT,
	"SIGTTIN   (21)", SIG_DFL_STOPS,
	"SIGTTOU   (22)", SIG_DFL_STOPS,
	"SIGIO     (23)", SIG_DFL_NO_EFFECT  | SIG_CONT | SIG_ACCEPT,	
	"SIGXCPU   (24)", SIG_DFL_TERMINATES,	
	"SIGXFSZ   (25)", SIG_DFL_TERMINATES,	
	"SIGVTALARM(26)", SIG_DFL_TERMINATES | SIG_CONT | SIG_ACCEPT,	
	"SIGPROF   (27)", SIG_DFL_TERMINATES,	
	"SIGWINCH  (28)", SIG_DFL_NO_EFFECT  | SIG_CONT | SIG_ACCEPT,	
	"SIGLOST   (29)", SIG_DFL_TERMINATES | SIG_DFL_CORE,	
	"SIGUSR1   (30)", SIG_DFL_TERMINATES,	
	"SIGUSR2   (31)", SIG_DFL_TERMINATES,	
};

const char Sig_format[] = "signal %[-]15cs%[-]46cs\n";

#define FN_SIG_NAME		0
#define FN_SIG_ACTION		1
#define FN_SIG_LAST		2

#define SIG_TO_OBJ(sig)		((objid_t) (Sigtab + (sig)))
#define OBJ_TO_SIG(obj)		((int)(((siginfo_t *)(obj)) - Sigtab))

#define SIGTAB_SIZE	(sizeof Sigtab / sizeof Sigtab[0])

const char *
sig_getobjname(obj)
objid_t obj;
{
	static char name[30];
	char *s;

	strncpy(name, get_field_value(obj, FN_SIG_NAME), sizeof(name) - 1);
	name[sizeof(name) - 1] = '\0';

	for (s = name; isalnum(*s); ++s)
		if (isupper(*s))
			*s = tolower(*s);
	*s = '\0';

	return name;
}

bool
accept_signal(sig)
int sig;
{
	return (Sigtab[sig].sig_attr & SIG_ACCEPT) != 0;
}

bool
sig_causes_refresh(sig)
int sig;
{
	return (Sigtab[sig].sig_attr & SIG_REDRAW) != 0;
}

bool
sig_stops_target(sig)
int sig;
{
	return (Sigtab[sig].sig_attr & SIG_CONT) == 0;
}

bool
sig_kills_target_by_default(sig)
int sig;
{
	return (Sigtab[sig].sig_attr & SIG_DFL_TERMINATES) != 0;
}

bool
sig_is_fatal(sig)
int sig;
{
	return sig == SIGILL || sig == SIGSEGV || sig == SIGBUS;
}

const char *
signame(sig)
int sig;
{
	static char buf[50];

	if (sig > 0 && sig < SIGTAB_SIZE)
		return Sigtab[sig].sig_name;
	
	(void) sprintf(buf, "<signal #%d>", sig);
	return buf;
}

const char Sghead_format[] = "Signals\n";

#define SGHEAD_OBJCODE	((objid_t)Sghead_format)

/*  Add the signal list header to the form.
 */
void
add_signals_header(par)
objid_t par;
{
	new_object(SGHEAD_OBJCODE, OT_SGHEAD, par, OBJ_CHILD);
}

static int
sigcmp(obj1, obj2)
objid_t obj1, obj2;
{
	return (obj1 < obj2) ? -1 : 1;
}

/*  Process the return from the signal header menu. Expand or collapse
 *  the signal list
 */
void
do_sgh(obj, command)
objid_t obj;
int command;
{
	int sig;

	switch(command) {
	case MR_SHOW_ALL_SIGNALS:
		for (sig = 31; sig > 0; --sig) {
			if (!obj_child_exists(obj, SIG_TO_OBJ(sig)))
				add_signal_object(obj, sig);
		}
		sort_children(obj, sigcmp);
		break;
	case MR_HIDE_ALL_SIGNALS:
		remove_object(obj, OBJ_DESCENDENTS);
		break;
	default:
		panic("bad cmd in dsh");
	}
}

/*  Create an object describing an individual signal.
 */
static void
add_signal_object(par, sig)
objid_t par;
int sig;
{
	fval_t fields[FN_SIG_LAST + 1];

	fields[FN_SIG_NAME] = (fval_t) Sigtab[sig].sig_name;
	fields[FN_SIG_ACTION] = (fval_t) strsave(sig_attr_string(sig));
	fields[FN_SIG_LAST] = (fval_t) NULL;
	
	new_object(SIG_TO_OBJ(sig), OT_SIG, par, OBJ_CHILD);
	set_all_fields(SIG_TO_OBJ(sig), fields, (fval_t)NULL);
}

void
sig_getsize(obj, unused_par, sz)
objid_t obj, unused_par;
struct szst *sz;
{
	font_t *font;
	int namelen, actlen;

	font = wn_get_sysfont();

	namelen = strlen((char *)get_field_value(obj, FN_SIG_NAME));
	actlen = strlen((char *)get_field_value(obj, FN_SIG_ACTION));

	sz->sz_depth = font->ft_height;
	sz->sz_width = font->ft_width * (sizeof("signal") + namelen + 1 + actlen);
}

void
free_sig(obj)
objid_t obj;
{
	free((char *)get_field_value(obj, FN_SIG_ACTION));
}

/*  Return a string describing the signal's attributes.
 */
static char *
sig_attr_string(sig)
int sig;
{
	static char buf[100];
	int flags;

	flags = Sigtab[sig].sig_attr;
	if ((flags & SIG_CONT) == 0) {
		(void) strcpy(buf, "Stop -");
		if (flags & SIG_ACCEPT)
			strcat(buf, " accept signal on continue.");
		else
			strcat(buf, " ignore signal on continue.");
		return buf;
	}
	if (flags & SIG_REDRAW) {
		(void) strcpy(buf, "Refresh display and continue,");
		if (flags & SIG_ACCEPT)
			strcat(buf, " accepting signal.");
		else
			strcat(buf, " ignoring signal.");
		return buf;
	}
	if (flags & SIG_ACCEPT)
		(void) strcpy(buf, "Accept and continue.");
	else
		(void) strcpy(buf, "Ignore.");
	return buf;
}

int
sig_dumpobj(arg, code, level)
char *arg;
objid_t code;
int level;
{
	int sig;

	sig = OBJ_TO_SIG(code);

	return td_outf(level, "%s%s",
				Sigtab[sig].sig_name, sig_attr_string(sig));
}

/*  Process the return from a signal menu. Switching attributes on and off.
 */
void
do_sig(obj, command)
objid_t obj;
int command;
{
	int sig;

	sig = OBJ_TO_SIG(obj);

	switch(command) {
	case MR_SIG_TOGGLE_STOP_CONT:
		Sigtab[sig].sig_attr ^= SIG_CONT;
		break;
	case MR_SIG_TOGGLE_ACCEPT_IGNORE:
		Sigtab[sig].sig_attr ^= SIG_ACCEPT;
		break;
	case MR_SIG_TOGGLE_REDRAW:
		Sigtab[sig].sig_attr ^= SIG_REDRAW;
		break;
	case MR_HIDE_SIGNAL:
		remove_object(obj, OBJ_SELF);
		break;
	default:
		panic("bad cmd in ds");
	}

	if (command != MR_HIDE_SIGNAL)
		change_field(obj, FN_SIG_ACTION, sig_attr_string(sig));
}
