/*
 * defaults - generate default values for headers.
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <pwd.h>
#include <sys/types.h>
#include "news.h"
#include "config.h"
#include "fgetmfs.h"

/* imports */
extern int optind;
extern char *optarg;
extern FILE *efopen();
extern char *getenv(), *getlogin();
extern char *emalloc();
extern struct passwd *getmypwent();
extern time_t time();

/* exports */
char *progname = "";
int debug;

/* forwards */
char *fullname(), *readline(), *getdomain(), *getuserid(), *getfullname();
char *getorg();

/* privates */
int usggcos;			/* strictly speaking, "BTL RJE format" */
time_t now;
long pid;

/*
 * main - parse arguments and handle options
 */
main(argc, argv)
int argc;
char *argv[];
{
	int c, errflg = 0;

	if (argc > 0)
		progname = argv[0];
	while ((c = getopt(argc, argv, "dp:t:u")) != EOF)
		switch (c) {
		case 'd':
			++debug;
			break;
		case 'p':
			pid = atol(optarg);
			break;
		case 't':
			now = atol(optarg);
			break;
		case 'u':
			usggcos++;
			break;
		default:
			errflg++;
			break;
		}
	if (errflg || optind < argc) {
		(void) fprintf(stderr, "usage: %s [-du] [-p pid] [-t time]\n",
			progname);
		exit(2);
	}

	getdefaults();
	exit(0);
}

getdefaults()
{
	register char *name, *domain, *org;
	register struct passwd *pwp = getmypwent();

	printvar("defhostname", hostname());
	domain = getdomain();
	printvar("defmailname", domain);
	printvar("defuser", getuserid(pwp));
	name = getfullname(pwp);
	printvar("defname", name);
	free(name);
	org = getorg();
	printvar("deforg", org);
	free(org);

	/* compute message-id */
	(void) fputs("defmsgid", stdout);
	(void) putchar('=');
	(void) putchar('\'');

	(void) putchar('<');
	intcode(now == 0? time(&now): now);
	(void) putchar('.');
	intcode((time_t)(pid == 0? getpid(): pid));
	(void) putchar('@');
	(void) fputs(domain, stdout);
	(void) putchar('>');

	(void) putchar('\'');
	(void) putchar('\n');
}

char *							/* malloced */
getdomain()
{
	register char *line, *name;

	line = readline(ctlfile("mailname"));
	if (line != NULL) {
		register char *s;

		for (s = line; *s != '\0'; s++)
			if (*s == '\t')
				*s = ' ';
	} else {
		line = strsave(hostname());
		if (strchr(line, '.') == NULL) { /* not internet addr.? */
			name = str3save(line, ".uucp", "");
			free(line);
			line = name;
		}
	}
	return line;
}

char *						/* static */
getuserid(pwp)
register struct passwd *pwp;
{
	register char *name;

	name = getenv("LOGNAME");
	if (name == NULL)
		name = getenv("USER");
	if (name == NULL)
		name = (pwp == NULL? "nobody": pwp->pw_name);
	return name;
}

char *						/* malloced */
getfullname(pwp)
register struct passwd *pwp;
{
	register char *line, *name;

	name = getenv("NAME");
	if (name != NULL)
		return strsave(name);
	else {
		name = str3save(getenv("HOME"), "/", ".name");
		line = readline(name);
		free(name);
		name = (line != NULL? line: strsave(fullname(pwp, usggcos)));
	}
	return name;
}

char *						/* malloced */
getorg()
{
	register char *line;

	line = getenv("ORGANIZATION");
	if (line != NULL)
		return strsave(line);
	else {
		line = readline(ctlfile("organization"));	/* american */
		if (line == NULL)
			line = readline(ctlfile("organization")); /* english */
		if (line == NULL)
			line = strsave("");
	}
	return line;
}

char *						/* malloced */
readline(file)
char *file;
{
	register FILE *fp = fopen(file, "r");
	register char *line;

	if (fp == NULL)
		return NULL;
	line = fgetms(fp);
	(void) fclose(fp);
	if (line != NULL)
		trim(line);
	return line;
}

printvar(var, value)		/* print var='value' with suitable sh quoting */
register char *var, *value;
{
	(void) fputs(var, stdout);
	(void) putchar('=');
	if (*value != '\0') {
		(void) putchar('\'');
		for (; *value != '\0'; value++)
			if (*value == '\'')
				(void) fputs("'\\''", stdout);
			else
				(void) putchar(*value);
		(void) putchar('\'');
	}
	(void) putchar('\n');
}
