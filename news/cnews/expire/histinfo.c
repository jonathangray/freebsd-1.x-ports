/*
 * histinfo - print history file lines for articles named on stdin
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>		/* for modified time (date received) */
#include <string.h>
#include "config.h"
#include "fgetmfs.h"
#include "alloc.h"
#include "case.h"

#define STRLEN(s) (sizeof(s) - 1)	/* s must be a char array */

char *progname;
int debug;

FILE *efopen();

char *spdir;
int spdirlen;

/*
 * main - parse arguments and handle options
 */
main(argc, argv)
int argc;
char *argv[];
{
	int c;
	int errflg = 0;
	FILE *in;
	char *inname;
	register int i;
	extern int optind;
	extern char *optarg;

	progname = argv[0];
	while ((c = getopt(argc, argv, "d")) != EOF)
		switch (c) {
		case 'd':
			++debug;
			break;
		default:
			errflg++;
			break;
		}
	if (optind < argc || errflg) {
		(void) fprintf(stderr, "usage: %s [-d]\n", progname);
		exit(2);
	}

	spdir = artfile((char *)NULL);
	spdirlen = strlen(spdir);
	
	while ((inname = fgetms(stdin)) != NULL) {
		i = strlen(inname);
		if (i > 0)
			inname[i-1] = '\0';	/* kill newline */
		if (strchr(inname, '.') == NULL) {	/* skip dot names */
			in = efopen(inname, "r");
			process(in, inname);
			(void) fclose(in);
		}
		free(inname);
	}
	exit(0);
}

/*
 * process - process input file
 */
process(in, inname)
FILE *in;
char *inname;
{
	char *name;
	char *line;
	char *msgid;
	char *expiry;
	time_t datercv;
	struct stat statb;
	static char msgidnm[] =  "Message-ID:";
	static char expnm[] =    "Expires:";
	register char *p;
	register int i;

	expiry = strsave("-");
	msgid = NULL;

	/* read until EOF or blank line (end of headers) */
	while ((line = fgetms(in)) != NULL && strcmp(line, "\n") != 0) {
		i = strlen(line);
		if (i > 0)
			line[i-1] = '\0';		/* trim newline */
		if (CISTREQN(line, msgidnm, STRLEN(msgidnm))) {
			if (msgid != NULL)
				free(msgid);
			p = line + STRLEN(msgidnm);
			msgid = strsave(p + strspn(p, " \t"));
		} else if (CISTREQN(line, expnm, STRLEN(expnm))) {
			free(expiry);
			p = line + STRLEN(expnm);
			expiry = strsave(p + strspn(p, " \t"));
		}
		free(line);
	}
	if (line != NULL)
		free(line);

	/* generate the file name */
	name = inname;
	if (strncmp(name, spdir, spdirlen) == 0 &&
	    name[spdirlen] == '/')
		name += spdirlen + 1;	/* skip spool dir. & slash */

	/* generate the date received */
	(void) fstat(fileno(in), &statb);
	datercv = statb.st_mtime;

	/* deal with empty and trash articles */
	if (msgid == NULL || strchr(msgid, '\t') != NULL || msgid[0] != '<' ||
					msgid[strlen(msgid)-1] != '>' ||
					strchr(expiry, '\t') != NULL ||
					strchr(expiry, '~') != NULL) {
		if (msgid != NULL)
			free(msgid);
		msgid = str3save("<", name, "@trash>");
		datercv = 0;
		free(expiry);
		expiry = strsave("0");
	}

	/* whomp out the history line */
	(void) fputs(msgid, stdout);
	printf("\t%ld~", (long)datercv);
	(void) fputs(expiry, stdout);
	(void) putchar('\t');
	(void) fputs(name, stdout);
	(void) putchar('\n');
	(void) fflush(stdout);
	free(msgid);
	free(expiry);
}

/*
 * unprivileged - no-op to keep pathname stuff happy
 */
void
unprivileged(reason)
char *reason;
{
}
