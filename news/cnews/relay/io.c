/*
 * common i/o operations
 */

#include <stdio.h>
#include <sys/types.h>
#include "news.h"
#include "headers.h"
#include "article.h"
#include "msgs.h"

/*
 * If *fpp is non-null, fclose it and check for errors.
 * On error, call fulldisk(art, name).
 */
void
nnfclose(art, fpp, name)
struct article *art;
register FILE **fpp;
char *name;
{
	if (*fpp != NULL) {
		if (nfclose(*fpp) == EOF)
			fulldisk(art, name);
		*fpp = NULL;		/* mark the stream closed */
	}
}
