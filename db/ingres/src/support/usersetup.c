#include <stdio.h>

#include <pwd.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#include <ingres.h>
#include <aux.h>
#include "sccs.h"

#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)usersetup.c	8.2	1/18/85)

extern int	optind;
extern char	*optarg;

/*
**  Initialize Users File From Passwd File
**
**	Everyone in /etc/passwd is entered into the users file.  All
**	users can access all databases.
**
**	User codes are assigned sequentially.  This should therefore
**	probably be run once only, when INGRES is first installed.
**	Otherwise, usercodes can change mysteriously.
**
**	The optional parameter replaces the root of the INGRES subtree
**	as found in /etc/passwd.  The INGRES user must be installed
**	(with that name) when usersetup is run.  If this parameter
**	is a minus ("-"), output goes to the standard output.
**
**	The initialization file is initialized to "<home>/.ingres",
**	where <home> is the home directory in the passwd file.
*/

/*
**  NEXT -- return successor to code.
*/
void
next(char *code)
{
	char	*c;
	char	a, b;

	c = code;
	a = c[0];
	b = c[1];

	if (++b > 'z') {
		b = '0';
	} else if (b == '9' + 1) {
		b = 'a';
		if (a == 'Z') {
			write(2, "Too many users\n", 15);
			exit(-1);
		}
		if (++a > 'z') {
			a = 'A';
		}
	}

	c[0] = a;
	c[1] = b;
}

/*
** sysexit
**	A simple function that just exits, this is for the benefit
**	of syserr, so it does not core dump.
*/
void
sysexit(int value)
{
	exit(value);
}

void
main(int argc, char **argv)
{
	struct passwd	*pwp;
	register int	i;
	char		buf[MAX_LINE_SIZE + 1];
	char		*pathname;
        char            code[USERCODE_SIZE + 1];
	char		*status = "000001";

	setprocname("USERSETUP");

	pathname = NULL;
	while ((i = getopt(argc, argv, "d:p:")) != EOF) {
		switch(i) {
		case 'd' :
			pathname = optarg;
			break;
		case 'p' :
			setpwfile(optarg);
			break;
		}
	}

	if (optind < argc) {
		status = *++argv;
	}

	(void) strcpy(code, "aa");

	if ((pwp = getpwnam(USERINGRES)) == (struct passwd *) NULL) {
		syserr("USERINGRES not installed as UNIX user ");
	}

	if (pathname == (char *) NULL) {
		pathname = pwp->pw_dir;
	}

	/* open output file as needed */
	if (pathname[0] != '-') {
		concat(pathname, "/files/users", buf);
		if ((i = open(buf, O_RDONLY)) >= 0) {
			syserr("%s already exists", buf);
		}
		if ((i = open(buf, O_CREAT | O_TRUNC | O_WRONLY, 0644)) < 0) {
			syserr("Cannot create %s", buf);
		}
		(void) close(i);
		if (freopen(buf, "w", stdout) == NULL) {
			syserr("cannot open %s", buf);
		}

	}

	setpwent();
	while ((pwp = getpwent()) != (struct passwd *) NULL) {
		/* print username & code */
		printf("%s:%s:%d:%d:%s:::%s/.ingres::\n",
			pwp->pw_name,	/* user name */
			code,
			pwp->pw_uid,	/* user id */
			pwp->pw_gid,	/* user group */
			(strcmp(pwp->pw_name, USERINGRES) == 0) ? "177777" : status,
			pwp->pw_dir);	/* working directory */
		next(code);
	}
	(void) fflush(stdout);
}
