/*
 * string operations
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <string.h>
#include "libc.h"
#include "news.h"

/* forwards */
FORWARD char *findhost();
FORWARD _initishostchar();

/*
 * Return strsave() of the first word in "tokens".
 * Words are delimited by spaces.
 */
char *
first(tokens)
char *tokens;
{
	return strsvto(tokens, ' ');
}

/*
 * Turn a newsgroup name into a file name, in place.
 */
void
mkfilenm(ng)
register char *ng;
{
	for (; *ng != '\0'; ng++)
		if (*ng == NGDELIM)
			*ng = FNDELIM;
}

char *
trim(s)					/* trim trailing newline */
char *s;
{
	register char *nl;

/*	STRRCHR(s, '\n', nl);	*/
	nl = strrchr(s, '\n');
	if (nl != NULL)
		*nl = '\0';
	return s;
}

char *
skipsp(s)				/* skip any whitespace at *s */
register char *s;
{
	while (iswhite(*s))
		s++;
	return s;
}

char *
strsvto(s, c)				/* save s up to (but excluding) c */
char *s;
int c;
{
	register char *endp, *copy;

	STRCHR(s, c, endp);		/* find interesting part's end of s */
	if (endp != NULL)
		*endp = '\0';		/* restored below */
	copy = strsave(s);		/* copy interesting substring of s */
    	if (endp != NULL)
	    	*endp = c;
	return copy;
}

int
charcount(s, c)			/* how many c's in s? */
register char *s;
register int c;
{
	register int count = 0;

#ifdef CLASSY
	for (; (s = strchr(s, c)) != NULL; s = (s == NULL? NULL: s+1))
		++count;
#else
	while (*s != '\0')
		if (*s++ == c)
			++count;
#endif				/* CLASSY */
	return count;
}

char *
nullify(s)				/* return s or "" if NULL */
register char *s;
{
	if (s == NULL)
		return "";
	else
		return s;
}


/* hostname and path routines follow */


#define CHARSETWIDTH 8			/* bits per character */
#define CHARSETSIZE  (1<<CHARSETWIDTH)	/* 2^CHARSETWIDTH */

#define initishostchar() (setishostchar? 0: _initishostchar())
/* These macros are both (currently) safe. */
/* If c is NUL, hostchar will be false, so don't test (optimisation: ==). */
#define nothostchar(c) (!hostchar(c) /* || (c) == '\0' */ )
/* True if c can be part of a hostname.  False may mean c is NUL. */
#define hostchar(c) ishostchar[(c) & (CHARSETSIZE-1)]

static char ishostchar[CHARSETSIZE];	/* char. sets > Latin-1 are out of luck */
static int setishostchar = NO;

/*
 * RFC 850 allows letters, digits, periods, and hyphens and specifically
 * disallows blanks in hostnames.
 */
STATIC
_initishostchar()
{
	if (!setishostchar) {
		register char *p;
		register int c;

		setishostchar = YES;
		for (c = 0, p = ishostchar; c < sizeof ishostchar; c++)
			*p++ = isascii(c) && isalnum(c);
		ishostchar['.'] = ishostchar['-'] = YES;
	}
}

/*
 * Return true iff any host in hosts appears in s, as per hostin().
 * hosts are separated by non-hostname characters.
 */
boolean
anyhostin(hosts, s)
char *hosts, *s;
{
	register char *host = hosts;

	while (*host != '\0') {
		register char *delimp;
		register int delim;
		register boolean hostisin;

		initishostchar();
		while (nothostchar(*host) && *host != '\0')
			++host;			/* skip leading delims */
		if (*host == '\0')		/* no more hosts */
			break;
		for (delimp = host; hostchar(*delimp); delimp++)
			;			/* skip to next delim */
		delim = *delimp;		/* may be NUL */
		*delimp = '\0';			/* terminate host */
		hostisin = hostin(host, s);
		*delimp = delim;		/* restore hosts delimiter */
		if (hostisin)
			return YES;
		host = delimp;			/* advance to next host */
	}
	return NO;
}

/*
 * Return true iff host appears in s, with no characters from the alphabet
 * of legal hostname characters immediately adjacent.
 */
boolean
hostin(host, s)
register char *host, *s;
{
	return findhost(host, s) != NULL;
}

/*
 * Return the number of machines appearing in path,
 * by counting transitions from delimiters.
 * See anyhostin() for the rules, and the macros.
 */
int
hopcount(path)
register char *path;
{
	register int count = 0;

	initishostchar();
	for (; *path != '\0'; path++)
		if (nothostchar(path[0]) &&
		    (hostchar(path[1]) || path[1] == '\0'))
			++count;	/* trailing edge of delimiters */
	return count;
}

char *
sendersite(path)
register char *path;
{
	register char *p;
	static char *sender = NULL;

	initishostchar();
	nnfree(&sender);		/* free the last answer */
	for (p = path; hostchar(*p); p++)
		;
	if (*p == '\0')			/* only a user name */
		return hostname();	/* a local posting */
	else {
		register int delim = *p;

		*p = '\0';
		sender = strsave(path);	/* copy the first machine name */
		*p = delim;
		return sender;
	}
}

/*
 * Canonicalise rawpath: NULL -> "", chop last site (actually user name) but not
 * its leading delimiter, and if Approved:, chop everything after the site,
 * and its trailing delimiter, from Approved: (or Sender:) (user@host).
 * Result is malloced memory.
 * This is also a profiling hot spot.
 */
char *
canonpath(rawpath, approved, sender)
char *rawpath, *approved, *sender;
{
	register char *newpath = strsave(nullify(rawpath));	/* costly */
	register char *p, *lastdelim = newpath, *site = NULL;

	initishostchar();
	for (p = newpath; *p != '\0'; ++p)
		if (nothostchar(*p))
			lastdelim = p + 1;	/* just past delim */
	*lastdelim = '\0';			/* omit user's name */

	if (approved != NULL) {			/* moderated article */
		STRCHR(approved, '@', site);
		if (site == NULL)
			STRCHR(nullify(sender), '@', site);
	}
	if (site != NULL) {
		p = findhost(site+1, newpath);
		if (p != NULL && *p++ != '\0')	/* delim after site? */
			*p = '\0';		/* terminate newpath after site */
	}
	return newpath;
}

/*
 * Return pointer to the first byte after host in path, if any,
 * with no characters from the alphabet of legal hostname characters
 * immediately adjacent.
 * This function is a profiling hot spot, so it has been optimised.
 */
STATIC char *
findhost(host, path)
register char *host, *path;
{
	register char *pathp, *nxpathp;
	register int hostlen = strlen(host);

	initishostchar();
	for (pathp = path; ; pathp = nxpathp + 1) {
		STRCHR(pathp, host[0], nxpathp);	/* find plausible start */
		if (nxpathp == NULL)
			return NULL;		/* path exhausted */
		pathp = nxpathp;
		if (STREQN(pathp, host, hostlen) &&
		    (pathp == path || nothostchar(pathp[-1])) &&
		    nothostchar(pathp[hostlen]))
			return &pathp[hostlen];
	}
}
