/*
 * fullname - extract user's full name from a password entry
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <pwd.h>

/* imports */
extern char *getenv(), *emalloc();

char *
fullname(pwp, usggcos)
register struct passwd *pwp;
int usggcos;			/* strictly speaking, "BTL RJE format" */
{
	register char *name, *gcos, *delim;

	name = getenv("NAME");
	if (name != NULL)
		return name;

	if (pwp == NULL)
		return "";		/* no such user */

	gcos = pwp->pw_gecos;
	delim = strchr(gcos, ',');
	if (delim != NULL)
		*delim = '\0';		/* stop at first comma */
	if (usggcos) {
		delim = strchr(gcos, '-');
		if (delim != NULL)
			gcos = delim + 1;	/* skip past first dash */
		delim = strchr(gcos, '(');
		if (delim != NULL)
			*delim = '\0';	/* stop at first paren */
	}

	/* the horrible UCB & hack */
	delim = strchr(gcos, '&');
	if (delim != NULL) {
		register char *cap = pwp->pw_name;
		register char *newgcos;

		if (isascii(*cap) && islower(*cap))
			*cap = toupper(*cap);
		newgcos = emalloc(strlen(gcos) + strlen(cap) + 1);
		(void) memcpy(newgcos, gcos, delim - gcos);
		(void) strcpy(newgcos + (delim - gcos), cap);
		(void) strcat(newgcos, delim + 1);
		gcos = newgcos;
	}
	return gcos;
}
