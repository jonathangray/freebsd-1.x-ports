#include "common.h"

#ifndef HAVE_STRDUP
char *
#ifndef ANSI_PROTOTYPES
strdup(str)
    const char *str;
#else /* ANSI_PROTOTYPES */
strdup(const char *str)
#endif /* ANSI_PROTOTYPES */
{
    char *nstr;

    if (str == (char*)0)
        return str;

    nstr = (char*)malloc((u_int)(strlen(str) + 1));

    if (nstr == (char*)0)
    {
        (void)fprintf(stderr, "strdup(): not enough memory to duplicate `%s'\n",
		      str);
	exit(1);
    }

    (void)strcpy(nstr, str);

    return nstr;
}
#endif /* HAVE_STRDUP */
