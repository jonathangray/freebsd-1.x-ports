/*
 * efopen - fopen file, exit with message if impossible
 */

#include <stdio.h>
#include <string.h>
#include <errno.h>
#ifndef __STDC__
extern int errno;
#endif

/* imports from libc */
extern void error();

static char message[] = "can't open file \"%s\" mode ";

FILE *
efopen(file, mode)
char *file;
char *mode;
{
	FILE *fp;
	char fullmsg[sizeof(message)+10];

	errno = 0;		/* Wipe out residue of earlier errors. */
	fp = fopen(file, mode);
	if (fp == NULL) {
		(void) strcpy(fullmsg, message);
		(void) strncat(fullmsg, mode, 10);
		error(fullmsg, file);
		/* NOTREACHED */
	}
	return(fp);
}
