/* wn_framed.c - frame daemon code for Sunview */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char wn_wn_framed_c_sccsid[] = "@(#)wn_sunframe.c	1.15 25/4/92 (UKC)";

#ifdef SUNVIEW
#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#include <string.h>

#include <local/ukcprog.h>

#include "wn.h"
#include "wn_priv.h"
#include "framed.h"
#include "wn_sunframe.h"

extern char *_wn_Sysfont_file;

/*  Argument table
 */
typedef struct argst {
	char a_shortflag;
	char *a_longflag;
	short a_inherit;
	char *a_types;
	char *a_arg_descrip;
	char *a_notes;
} arg_t;

static char Crange_note[] = "0-255 (no color-full color)";

static arg_t Argtab[] = {
	'w', "width",		FALSE,	"d",	"columns",		"",
	'h', "height",		FALSE,	"d",	"lines",		"",
	's', "size",		FALSE,	"dd",	"x y",			"",
	'p', "position",	FALSE,	"dd",	"x y",			"",
	'P', "icon_position",	FALSE,	"dd",	"x y",			"",
	'l', "label",		FALSE,	"s",	"\"string\"",		"",
	'i', "iconic",		FALSE,	"",	"",			"",
	'n', "no_name_stripe",	TRUE,	"",	"",			"",
	't', "font",		TRUE,	"s",	"filename",		"",
	'f', "foreground_color",TRUE,	"ddd",	"red green blue",	Crange_note,
	'b', "background_color",TRUE,	"ddd",	"red green blue",	Crange_note,
	'I', "icon_image",	FALSE,	"s",	"filename",		"",
	'L', "icon_label",	FALSE,	"s",	"\"string\"",		"",
	'T', "icon_font",	FALSE,	"s",	"filename",		"",
	'H', "help",		FALSE,	"",	"",			"",
};

#define ARGTABSIZE	(sizeof(Argtab) / sizeof(Argtab[0]))

static void
show_sunview_help(fp)
FILE *fp;
{
	arg_t *arg;
	char longflag[25];

	fputs("Usage of shelltool generic window arguments:\n", fp);
	fprintf(fp, "FLAG\t%-24s%-16s%s\n", "(LONG FLAG)", "ARGS", "NOTES");

	for (arg = Argtab; arg < Argtab + ARGTABSIZE; arg++) {
		(void) sprintf(longflag, "(-%s)", arg->a_longflag);
		fprintf(fp, "-W%c\t%-24s%s\n", arg->a_shortflag,
					       longflag,
					       arg->a_arg_descrip,
					       arg->a_notes);
	}
}

static char **Frame_argv = NULL;
static int Frame_argc = 0;

static arg_t *
lookup_sun_flag(s)
char *s;
{
	arg_t *arg;

	if (*s++ != '-')
		return NULL;
	
	for (arg = Argtab; arg < Argtab + ARGTABSIZE; arg++) {
		if (strcmp(s, arg->a_longflag) == 0)
			return arg;
		if (*s == 'W' && s[1] == arg->a_shortflag && s[2] == '\0')
			return arg;
	}
	return NULL;
}

int
_wn_grab_sun_args(argc, argv)
int argc;
char **argv;
{
	arg_t *arg;
	char **winargv, **dst, **orig_argv;
	int i, winargc, nwinargs, nleft, nparams;

	nleft = argc;
	nwinargs = 8;
	winargv = (char **) wn__e_malloc(nwinargs * sizeof(char *));
	winargc = 0;
	dst = orig_argv = argv;
	while (*argv != NULL) {
		--nleft;
		if ((arg = lookup_sun_flag(*argv)) == NULL)
			*dst++ = *argv++;
		else if (arg->a_shortflag == 'H') {
			show_sunview_help(stderr);
			exit(0);
		}
		else {
			nparams = strlen(arg->a_types);
			if (nleft >= nparams) {
				if (arg->a_shortflag == 't')
					_wn_Sysfont_file = argv[1];
				if (arg->a_shortflag == 'f' ||
				    arg->a_shortflag == 'b') {
					_wn_set_fgbg(arg->a_shortflag == 'f',
						     argv[1], argv[2], argv[3]);
				}
				if (winargc + nparams >= nwinargs - 1) {
					nwinargs *= 2;
					winargv = (char **) wn__e_realloc(
							(char *)winargv,
							sizeof(char *) * nwinargs);
				}
				if (arg->a_inherit)
					_wn_add_to_unmunge_list(argv, nparams + 1);
				for (i = 0; i <= nparams; i++)
					winargv[winargc++] = *argv++;
			}
		}
	}
	*dst = NULL;
	Frame_argv = winargv;
	Frame_argc = winargc;
	return dst - orig_argv;
}

static int Got_sigemt;

static void
catch_sigemt()
{
	Got_sigemt = TRUE;
}

static void
kill_frame(ignored_status, arg)
int ignored_status;
caddr_t arg;
{
	(void) signal(SIGTERM, SIG_IGN);
	(void) kill((int)arg, SIGHUP);
}

int
_wn_create_sun_window(title)
char *title;
{
	static char framed_path_dir[] = "/usr/local/lib";
	static char framed_name[] = "ukcframed";
	static char framed_path[sizeof(framed_path_dir) + sizeof(framed_name)];
	static struct itimerval itval = { 0 , 0, 0, 0 };
	int i, parent_pid, child_pid, wfd, have_framed;
	void (*old_sigemt_handler)PROTO((int sig));
	static char wfdbuf[20], pidbuf[20], titlebuf[80];
	char wname_buf[WIN_NAMESIZE];
	char **argv;
	
	(void) sprintf(framed_path, "%s/%s", framed_path_dir, framed_name);

	wfd = win_getnewwindow();
	title = strncpy(titlebuf, title, sizeof(titlebuf)-1);

	/*  Build argv for frame daemon.
	 */
	(void) sprintf(pidbuf, "%d", getpid());
	(void) sprintf(wfdbuf, "%d", wfd);
	argv = (char **) wn__e_malloc((ARGN_ARGS_OFFSET + Frame_argc + 1) *
								sizeof(char *));
	argv[0] = framed_name;
	argv[ARGN_PID] = pidbuf;
	argv[ARGN_WFD] = wfdbuf;
	argv[ARGN_TITLE] = title;
	for (i = 0; i < Frame_argc; i++)
		argv[i + ARGN_ARGS_OFFSET] = Frame_argv[i];
	argv[i + ARGN_ARGS_OFFSET] = NULL;

	Got_sigemt = FALSE;
	old_sigemt_handler = signal(SIGEMT, catch_sigemt);

	have_framed = access(framed_path, X_OK) == 0;
	child_pid = have_framed ? vfork() : fork();

	if (child_pid == -1)
		return -1;

	if (child_pid == 0) {
		for (i = getdtablesize(); i > 2; --i)
			if (i != wfd)
				(void) close(i);
		signal(SIGHUP, SIG_DFL);
		signal(SIGTERM, SIG_DFL);
		signal(SIGEMT, SIG_DFL);
		(void) setitimer(ITIMER_REAL, &itval, (struct itimerval *)NULL);
		(void) setitimer(ITIMER_VIRTUAL, &itval, (struct itimerval *)NULL);
		(void) setitimer(ITIMER_PROF, &itval, (struct itimerval *)NULL);
		if (have_framed)
			execv(framed_path, argv);
		else
			_wn_framed(ARGN_ARGS_OFFSET + Frame_argc, argv);
		wn__panic("framed should not have returned");
	}

	/*  Wait for the frame daemon to say the window is ready.
	 */
	while (!Got_sigemt)
		sigpause(0);
	(void) signal(SIGEMT, old_sigemt_handler);

	win_fdtoname(wfd, wname_buf);
	we_setgfxwindow(wname_buf);
	if (on_exit(kill_frame, (caddr_t)child_pid) != 0)
		return -1;
	return wfd;
}
#endif /* SUNVIEW */
