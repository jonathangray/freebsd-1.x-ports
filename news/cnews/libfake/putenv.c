/*
 * putenv - add a variable to the environment, as in SysV
 */

#include <stdio.h>
#include <string.h>		/* the ANSI one, for memcpy */
#include <sys/types.h>
#include "libc.h"

#define YES 1
#define NO 0

/* peculiar return values */
#define WORKED 0
#define FAILED 1

int
putenv(var)			/* put var in the environment */
char *var;
{
	register char **envp, **newenv;
	register int oldenvcnt;
	extern char **environ;

	/* count variables, look for var */
	for (envp = environ; *envp != 0; envp++) {
		register char *varp = var, *ep = *envp;
		register int namesame;

		namesame = NO;
		for (; *varp == *ep && *varp != '\0'; ++ep, ++varp)
			if (*varp == '=')
				namesame = YES;
		if (*varp == *ep && *ep == '\0')
			return WORKED;	/* old & new var's are the same */
		if (namesame) {
			*envp = var;	/* replace var with new value */
			return WORKED;
		}
	}
	oldenvcnt = envp - environ;

	/* allocate new environment with room for one more variable */
	newenv = (char **)malloc((unsigned)((oldenvcnt+1+1)*sizeof(*envp)));
	if (newenv == NULL)
		return FAILED;

	/* copy old environment pointers, add var, switch environments */
	(void) memcpy((char *)newenv, (char *)environ, oldenvcnt*sizeof(*envp));
	newenv[oldenvcnt] = var;
	newenv[oldenvcnt+1] = NULL;
	environ = newenv;
	return WORKED;
}
