/*
 * updatemin - update the "min" fields in the active file
 */

#include <stdio.h>
#include <sys/types.h>
#include <string.h>
#include <signal.h>
#include <errno.h>
#include "fixerrno.h"

#ifndef BERKDIR
#include <dirent.h>
#else
#include <sys/dir.h>
#define	dirent	direct
#endif

#include "config.h"
#include "fgetmfs.h"
#include "alloc.h"

void fail();
void nfail();
void catch();
void slash();
long lowest();

char newname[] = "active.tmp";

char *progname;

extern long atol();

/*
 - main - parse arguments and handle options
 */
main(argc, argv)
int argc;
char *argv[];
{
	DIR *d;
	register FILE *a;
	register FILE *new;
	register char *line;
#	define	NF	4
	char *field[NF];
	register int nf;
	register long min;
	register char *name;
	char minstr[6];
	register int i;

	progname = argv[0];

	cd(ctlfile((char *)NULL));
	(void) umask(newsumask());

	catch(SIGINT);
	catch(SIGQUIT);
	catch(SIGHUP);
	catch(SIGTERM);
	newslock();

	a = fopen("active", "r");
	if (a == NULL)
		fail("cannot open `%s'", ctlfile("active"));
	if (fopen(newname, "r") != NULL)
		nfail("`%s' already exists", newname);
	new = fopen(newname, "w");
	if (new == NULL)
		fail("cannot create `%s'", ctlfile(newname));

	cd(fullartfile((char *)NULL));
	while ((line = fgetms(a)) != NULL) {
		i = strlen(line);
		if (i > 0 && line[i-1] == '\n')
			line[i-1] = '\0';
		nf = split(line, field, NF, "");
		if (nf != NF)
			nfail("bad number of fields in `%s ...'", field[0]);
		name = strsave(field[0]);
		slash(name);
		d = opendir(artfile(name));
		if (d != NULL) {
			min = lowest(d);
			if (min < 0)		/* no articles */
				min = atol(field[1])+1;
			closedir(d);
		} else				/* no directory */
			min = atol(field[2]);
		fprintf(new, "%s %s ", field[0], field[1]);
		if (min < 10000) {	/* insist on at least five digits */
			sprintf(minstr, "%ld", min);
			fprintf(new, "%s", "00000"+strlen(minstr));
		}
		fprintf(new, "%ld %s\n", min, field[3]);
		if (ferror(new))
			fail("unable to write `%s'", ctlfile(newname));
		free(name);
		free(line);
	}

	if (nfclose(new) == EOF)
		fail("close failed on `%s'", ctlfile(newname));
	cd(ctlfile((char *)NULL));
	(void) unlink("active.old");
	if (link("active", "active.old") < 0)
		fail("can't link `active' to `active.old'", "");
	if (unlink("active") < 0)
		fail("can't unlink `active'", "");
	if (link(newname, "active") < 0) {
		if (link("active.old", "active") < 0)
			fail("disaster -- cannot recover `active'!!", "");
		else
			fail("can't link in new `active' -- old one used", "");
	}
	if (unlink(newname) < 0)
		fail("can't unlink `%s'", newname);
	newsunlock();

	exit(0);
}

/*
 - lowest - find numerically-lowest name in directory
 */
long				/* lowest; -1 if none */
lowest(d)
register DIR *d;
{
	register struct dirent *dp;
	register long this;
	register long min = -1;
	register int any = 0;

	while ((dp = readdir(d)) != NULL) {
		if (strspn(dp->d_name, "0123456789") == strlen(dp->d_name)) {
			this = atol(dp->d_name);
			if (this < min || !any) {
				min = this;
				any = 1;
			}
		}
	}

	return(min);
}

/*
 - slash - convert dots to slashes in string
 */
void
slash(s)
char *s;
{
	register char *p = s;

	while ((p = strchr(p, '.')) != NULL)
		*p = '/';
}

/*
 - interrupt - signal handler
 */
void
interrupt()
{
	nfail("interrupted", "");
}

/*
 - catch - set up to catch a signal
 */
void
catch(sig)
int sig;
{
	if (signal(sig, SIG_IGN) != SIG_IGN)
		(void) signal(sig, interrupt);
}

/*
 - fail - some sort of error occurred
 */
void
fail(s1, s2)
char *s1;
char *s2;
{
	/* possibly should rm newname, but it may be important evidence */
	errunlock(s1, s2);
}

/*
 - nfail - fail but with errno meaningless
 */
void
nfail(s1, s2)
char *s1;
char *s2;
{
	extern int errno;

	errno = 0;
	fail(s1, s2);
}

/*
 - unprivileged - keep the configuration stuff happy
 */
void
unprivileged(reason)
char *reason;
{
}
