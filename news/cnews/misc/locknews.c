/*
 * locknews - lock the news system with newslock() and spawn a shell;
 *	unlock with newsunlock() when the shell exits.
 */
#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#include "news.h"
#include "libc.h"

#ifndef SIGFUNCTYPE
#define SIGFUNCTYPE void	/* was int */
#endif
typedef SIGFUNCTYPE (*sigtype)();

struct sigs {
	sigtype	hup;
	sigtype	intr;
	sigtype	quit;
	sigtype	term;
};

/* imports */
extern int optind;
extern char *optarg;
extern FILE *efopen();

/* exports */
int debug;
char *progname = "locknews";

/*
 * main - parse arguments and handle options
 */
main(argc, argv)
int argc;
char *argv[];
{
	register int pid, kid;
	register int c, errflg = 0;
	int verbose = 0;
	int status = -1;
	struct sigs sigs;

	if (argc > 0)
		progname = argv[0];
	while ((c = getopt(argc, argv, "v")) != EOF)
		switch (c) {
		case 'v':
			++verbose;
			break;
		default:
			errflg++;
			break;
		}
	if (errflg || optind < argc) {
		(void) fprintf(stderr, "usage: %s [-v]\n", progname);
		exit(2);
	}

	/*
	 * parent process needs to ignore at least keyboard signals
	 * to avoid leaving stray locks around by being killed.
	 */
	sigs.hup =  signal(SIGHUP, SIG_IGN);
	sigs.intr = signal(SIGINT, SIG_IGN);
	sigs.quit = signal(SIGQUIT, SIG_IGN);
	sigs.term = signal(SIGTERM, SIG_IGN);

	(void) newslock();
	if (verbose) {
		(void) fprintf(stderr,
			"%s: you now have the news system locked\n", progname);
		(void) fflush(stderr);
	}

	pid = fork();
	if (pid < 0)
		errunlock("fork failed; unlocking news system", "");
	if (pid == 0)
		execsh(&sigs);			/* child */
	while ((kid = wait(&status)) != pid && kid != -1)
		;

	(void) newsunlock();
	if (verbose)
		(void) fprintf(stderr,
			"%s: the news system is now unlocked\n", progname);
	exit(status | status>>8);
	/* NOTREACHED */
}

execsh(sigsp)
register struct sigs *sigsp;			/* initial signal states */
{
	char *oldps, *newps, *shell;
	static char defuxsh[] = "/bin/sh";	/* for Unix */
	static char defp9sh[] = "/bin/rc";	/* for Plan 9 */
	static char uxshpsname[] = "PS1";
	static char p9shpsname[] = "prompt";

	/* the child, however, can safely be killed */
	(void) signal(SIGHUP,  sigsp->hup);
	(void) signal(SIGINT,  sigsp->intr);
	(void) signal(SIGQUIT, sigsp->quit);
	(void) signal(SIGTERM, sigsp->term);

	oldps = getenv(uxshpsname);
	if (oldps == NULL)
		oldps = "$ ";
	newps = str3save(uxshpsname, "=: newslocked; ", oldps);
	if (putenv(newps))
		warning("can't add %s to environment", newps);

	oldps = getenv(p9shpsname);
	if (oldps == NULL)
		oldps = "; ";
	newps = str3save(p9shpsname, "=newslocked=:; ", oldps);
	if (putenv(newps))
		warning("can't add %s to environment", newps);

	shell = getenv("SHELL");
	if (shell != NULL) {
		(void) execl(shell, shell, (char *)NULL);
		warning("can't exec shell %s", shell);
	} else {
		(void) execl(defuxsh, defuxsh, (char *)NULL);
		(void) execl(defp9sh, defp9sh, (char *)NULL);
		warning("can't exec standard shells", "");
	}
	(void) fflush(stderr);
	(void) _exit(127);
	/* NOTREACHED */
}
