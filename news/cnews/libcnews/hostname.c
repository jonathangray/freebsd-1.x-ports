/*
 * hostname - return the Usenet name of this machine
 *
 * One interesting possibility would be to assume that the first
 * name in the sys file is our Usenet name, unless it is "ME",
 * which would require our current strategy anyway.
 */

#include <stdio.h>
#include <sys/types.h>

#include "libc.h"
#include "news.h"
#include "config.h"

#ifndef NAMEFILE
#define NAMEFILE ctlfile("whoami")
#endif

char *
hostname()			/* return this Usenet machine's name */
{
	static char name[MAXHOST];

	if (name[0] == '\0') {	/* try to get the "news hostname" */
		register FILE *fp;

		fp = fopenclex(NAMEFILE, "r");
		if (fp != NULL) {
			(void) fgets(name, sizeof name, fp);
			(void) nfclose(fp);
			if (name[0] != '\0' && name[strlen(name) - 1] == '\n')
				name[strlen(name) - 1] = '\0';
		}
	}
	if (name[0] == '\0')	/* else use the ordinary hostname */
		(void) gethostname(name, sizeof name);
	return name;
}
