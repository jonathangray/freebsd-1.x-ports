/*
 * time utilities
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/timeb.h>
#include "libc.h"
#include "news.h"

/*
 * Write a timestamp of the form "Jun 12 12:34:56.789" on fp.
 * N.B.: no trailing newline is written.
 */
void
timestamp(fp, timep)
FILE *fp;
time_t *timep;	/* if non-null, return time() here for later use */
{
	struct timeb ftnow;
	char ms[STRLEN("123") + SIZENUL];

	ftime(&ftnow);
	if (timep != NULL)
		*timep = ftnow.time;
	/* .15 excludes yyyy\n\0; + 4 omits day-of-week */
	(void) fprintf(fp, "%.15s.", ctime(&ftnow.time) + 4);
	(void) ltoza(ms, (long)ftnow.millitm, 3);	/* 3 digits of output */
	(void) fputs(ms, fp);
}
