/*
 * SystemV getcwd simulation, courtesy peter honeyman
 */

#include <stdio.h>
#include <string.h>

/* imports from libc */
extern FILE *popen();

char *
getcwd(path, size)
register char *path;
int size;
{
	register char *nlp;
	register FILE *fp;

	fp = popen("PATH=/bin:/usr/bin pwd", "r");
	if (fp == NULL)
		return 0;
	if (fgets(path, size, fp) == NULL) {
		(void) pclose(fp);
		return 0;
	}
	if ((nlp = strrchr(path, '\n')) != NULL)
		*nlp = '\0';
	(void) pclose(fp);
	return path;
}
