/* config.c - routines for parsing configuration files */

char ukcprog_config_sccsid[] = "@(#)config.c	1.2 27/3/92 UKC";

#include <stdio.h>	/* for NULL */
#include <ctype.h>
#include <ukcstring.h>

#include "ukcprog.h"

/*  Trim anything following a `#' and leading and trailing whitespace
 *  from a line.  We do this in place and return a pointer to the
 *  trimmed line.
 */
char *
config_trim_line(line)
char *line;
{
	char *hash;
	int len;
	
	while (isspace(*line))
		++line;
	if ((hash = strchr(line, '#')) != NULL)
		*hash = '\0';

	len = strlen(line);
	while (--len >= 0) {
		if (!isspace(line[len]))
			break;
	}
	line[len + 1] = '\0';

	return line;
}
