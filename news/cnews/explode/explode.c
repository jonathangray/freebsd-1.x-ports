/*
 * explode - read relaynews master batch file & write all the real batch files.
 * for UUNET, the C version needs to fnlock all descriptors
 */

/*
BEGIN	{ path = "error"; size = "error"; msgid = "error" }
/^($|#)/	{ next }	# comment
NF == 3 && /^</	{			# start a new article
	msgid = $1
	path = $2
	size = $3
	next
}
NF == 2 && /^F/	{ print path >>$2; next }		# B format
NF == 2 && /^f/	{ print path, size >>$2; next }		# C format
NF == 2 && /^n/	{ print path, msgid >>$2; next }	# NNTP format
NF == 2 && /^I/	{ print msgid >>$2; next }		# ihave/sendme format

	{ print "bad input on line", NR, ": " $0 | "cat >&2" }
 */

#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>
#include "libc.h"
#include "news.h"
#include "fgetmfs.h"
#include "trbatch.h"

/* imports */
extern int optind;
extern char *optarg;
extern FILE *efopen();

/* exports */
char *progname = "";
int debug;

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
	while ((c = getopt(argc, argv, "d")) != EOF)
		switch (c) {
		case 'd':
			++debug;
			break;
		default:
			errflg++;
			break;
		}
	if (errflg) {
		(void) fprintf(stderr, "usage: %s [-d] [file]...\n", progname);
		exit(2);
	}

	morefds();
	if (optind >= argc)
		process(stdin, "stdin");
	else
		for (; optind < argc; optind++)
			if (STREQ(argv[optind], "-"))
				process(stdin, "-");
			else {
				FILE *in = efopen(argv[optind], "r");

				process(in, argv[optind]);
				(void) fclose(in);
			}
	(void) bfrealclose();		/* paranoia */
	exit(0);
}

/*
 * process - process input file
 */
process(in, inname)
FILE *in;
char *inname;
{
	register char *line, *name;
	register struct batchfile *bf;
	register long lsize = 0;
	char *fields[3];
	char *msgid = NULL, *path = NULL, *size = NULL;

#ifdef UUNET
	if (!fnlockfile(in))
		error("can't lock %s", inname);
#endif
	while ((line = fgetms(in)) != NULL) {
		switch (*line) {
		case '#':
		case '\n':
			break;			/* ignore comments */
		case '<':			/* new article */
			trim(line);
			if (split(line, fields, 3, "") != 3) {
				errno = 0;
				warning("malformed article line: %s", line);
			} else {
				msgid = strsave(fields[0]);
				path = strsave(fields[1]);
				size = strsave(fields[2]);
				lsize = atol(size);
			}
			if (debug)
				(void) fprintf(stderr,
					       "%s: new article %s %s %s\n",
					       progname, msgid, path, size);
			break;
		case 'F':
		case 'f':
		case 'n':
		case 'I':
			trim(line);
			if (msgid == NULL) {
				errno = 0;
				warning(
				"batch file line before article line: %s",
					line);
			} else {
				for (name = line+1; isascii(*name) &&
				    isspace(*name); name++)
					;
				bf = bfopen(name);
				if (!bfappend(bf, *line, name, path, msgid,
				    lsize))
					error("error writing %s", name);
			}
			break;
		default:
			errno = 0;
			warning("bad input line: %s", line);
			break;
		}
		free(line);
	}
}
