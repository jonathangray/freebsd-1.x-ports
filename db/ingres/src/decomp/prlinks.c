#include <stdio.h>

#include <ingres.h>
#include <aux.h>
#include <tree.h>
#include <symbol.h>
#include "globs.h"
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)prlinks.c	8.1	12/31/84)

void
prlinks(char *label, char *linkmap)
{

	register char	*lm;
	register int	i;

	printf("\n%s: ", label);
	lm = linkmap;
	for (i = 0; i < MAX_DOMAINS; i++)
		if (*lm++)
			printf("dom %d,", i);
	putchar('\n');
}
