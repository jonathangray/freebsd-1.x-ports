/* testarg.c - routines for testing the arg library */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char arg_testarg_sccsid[] = "@(#)testarg.c	1.9 26/4/92 (UKC)";

#include <sys/types.h>
#include <signal.h>
#include <sys/wait.h>
#include <sys/file.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#ifdef __STDC__
#include <unistd.h>
#endif

#include <local/ukcprog.h>

#include "arg.h"

static void print_dvec PROTO((dvec_t dv));
static void do_twiddle PROTO((const char *buf));
static void do_curlies PROTO((const char *buf));
static void do_lparse PROTO((const char *line));
static void do_cmd PROTO((dvec_t dv, long lrdl));
static void do_shell PROTO((const char *line));
static void do_glob PROTO((const char *pat));

int main PROTO((int argc, const char **argv));

static void
print_dvec(dv)
dvec_t dv;
{
	const char **sptr;

	for (sptr = get_dvec_vec(dv); *sptr != NULL; sptr++)
		(void) printf("%s ", *sptr);
	(void) putchar('\n');
}

static void
do_twiddle(buf)
const char *buf;
{
	char *s;

	if ((s = arg_expand_twiddle(buf, '~')) != NULL)
		errf("\"%s\" -> \"%s\"", buf, s);
}

static void
do_curlies(buf)
const char *buf;
{
	dvec_t dv;

	if ((dv = arg_expand_braces(buf, '{', '}', ',')) != 0) {
		print_dvec(dv);
		free_dvec_and_strings(dv);
	}
}
		
static void
do_lparse(line)
const char *line;
{
	dvec_t dv;
	long lrdl;

	if (arg_lparse(line, &dv, &lrdl) == 0) {
		print_dvec(dv);
		arg_tidy_redirs_in_parent(lrdl);
	}
	free_dvec_and_strings(dv);
}

static void
do_cmd(dv, lrdl)
dvec_t dv;
long lrdl;
{
	const char **args, *dir;
	int pid;

	args = get_dvec_vec(dv);
	if (*args == NULL)
		return;

	if (strcmp(*args, "cd") == 0) {
		if (args[1] != NULL && args[2] != NULL) {
			errf("cd: too many arguments");
			return;
		}
		dir = (args[1] != NULL) ? args[1] : getenv("HOME");
		if (chdir(dir) != 0)
			errf("%s: %m", dir);
		return;
	}

	if ((pid = vfork()) == -1) {
		errf("can't fork (%m)");
		return;
	}
	if (pid == 0) {
		arg_do_redirs_in_child(lrdl);
		execvp(args[0], (char **)args);
		errf("%s: %m\n", args[0]);
		_exit(1);
	}
	(void) signal(SIGINT, SIG_IGN);
	(void) signal(SIGQUIT, SIG_IGN);
	while (wait((int *)NULL) != pid)
		;
	(void) signal(SIGINT, SIG_DFL);
	(void) signal(SIGQUIT, SIG_DFL);
}

static void
do_shell(line)
const char *line;
{
	dvec_t dv;
	long lrdl;

	if (arg_lparse(line, &dv, &lrdl) != 0)
		return;
	do_cmd(dv, lrdl);
	arg_tidy_redirs_in_parent(lrdl);
	free_dvec_and_strings(dv);
}

static void
do_glob(pat)
const char *pat;
{
	dvec_t dv;

	dv = arg_glob(&pat);
	if (dv != 0)
		print_dvec(dv);
	free_dvec_and_strings(dv);
}

int
main(argc, argv)
int argc;
const char **argv;
{
	const char *prompt;
	char buf[200];
	int i;
	void (*func)PROTO((const char *pat));

	prompt = "testsh";
	if (argc < 2) {
		errf("usage: %s opt [args ...]", argv[0]);
		exit(1);
	}

	switch(argv[1][0]) {
	case 's':
		prompt = "shell";
		func = do_shell;
		break;
	case 't':
		prompt = "twiddle";
		func = do_twiddle;
		break;
	case 'l':
		prompt = "lparse";
		func = do_lparse;
		break;
	case 'b':
		prompt = "braces";
		func = do_curlies;
		break;
	case 'g':
		prompt = "glob";
		func = do_glob;
		break;
	default:
		errf("unknown test type '%c'", argv[1][0]);
		exit(1);
		func = NULL;	/* to satisfy gcc */
	}
	
	if (argc == 2) {
		for (;;) {
			(void) fprintf(stderr, "%s> ", prompt);
			if (gets(buf) == NULL) {
				(void) putchar('\n');
				break;
			}
			(*func)(buf);
		}
	}
	else {
		for (i = 2; i < argc; i++)
			(*func)(argv[i]);
	}

	exit(0);
	return 0;	/* to satisfy gcc */
}
