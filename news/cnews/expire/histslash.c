/*
 * Convert slashed filenames to dotted group/article names in a history
 * file, for use in mkhistory.  Input comes only from stdin.
 */
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include "fgetmfs.h"

char *progname = "histslash";

main()
{
	register char *scan;
	register char *last;
	register char *line;

	while ((line = fgetms(stdin)) != NULL) {
		scan = strchr(line, '\t');
		if (scan != NULL)
			scan = strchr(scan+1, '\t');
		if (scan == NULL) {
			complain("bad number of fields in `%s'", line);
			exit(1);
		}
		scan++;
		last = NULL;
		while (*scan != '\0') {
			if (*scan == '/') {
				*scan = '.';
				last = scan;
			}
			if (*scan == ' ' || *scan == '\n') {
				assert(last != NULL);
				*last = '/';
			}
			scan++;
		}
		if (fputs(line, stdout) == EOF)
			error("fputs failed", "");
		free(line);
	}
}
