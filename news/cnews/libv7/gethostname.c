/*
 * v7 gethostname simulation
 *	taken from pathalias and cleaned up.  Thanks, peter.
 */

#include <stdio.h>
#include <string.h>

/* imports from libc */
extern FILE *fopen(), *popen();

/* forwards */
void readhostfile();

#define MAXHOST 256
#define min(a,b) ((a) < (b)? (a): (b))

static char defhost[] = "INSERT-YOUR-HOST-NAME-HERE";
static char *namefiles[] = {
	"/etc/whoami",
	"/etc/systemid",
	NULL
};

int
gethostname(hostname, size)
register char *hostname;
int size;
{
	register FILE *whoami;
	register char **namep;

	*hostname = '\0';

	/* try files in namefiles */
	for (namep = namefiles; *namep != NULL; namep++) {
		readhostfile(hostname, size, *namep);
		if (*hostname != '\0')
			return 0;
	}

	/* try /usr/include/whoami.h */
	if ((whoami = fopen("/usr/include/whoami.h", "r")) != NULL) {
		while (!feof(whoami)) {
			char sysname[MAXHOST];

			if (fgets(sysname, MAXHOST, whoami) == NULL)
				break;
			if (sscanf(sysname, "#define sysname \"%[^\"]\"",
			    hostname) > 0)
				break;
		}
		(void) fclose(whoami);
		if (*hostname != '\0')
			return 0;
	}

	/* ask uucp */
	if ((whoami = popen("PATH=/bin:/usr/bin:/usr/ucb uuname -l", "r")) != NULL) {
		register char *ptr;

		(void) fgets(hostname, size, whoami);
		(void) pclose(whoami);
		if ((ptr = strchr(hostname, '\n')) != NULL)
			*ptr = '\0';
	}
	if (*hostname != '\0')
		return 0;

	/* aw hell, i give up!  is this a real unix? */
	(void) strncpy(hostname, defhost, min(sizeof defhost, size));
	return 0;
}

static void
readhostfile(hostname, size, file)	/* read host name from file */
char *hostname, *file;
int size;
{
	register FILE *whoami;

	if ((whoami = fopen(file, "r")) != NULL) {
		register char *ptr;

		(void) fgets(hostname, size, whoami);
		(void) fclose(whoami);
		if ((ptr = strchr(hostname, '\n')) != NULL)
			*ptr = '\0';
	}
}
