#include "monitor.h"
#include <ingres.h>
#include <aux.h>
#include "sccs.h"

#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)prompt.c	8.1	12/31/84)



/*
**  OUTPUT PROMPT CHARACTER
**
**	The prompt is output to the standard output.  It will not be
**	output if -ss mode is set or if we are not at a newline.
**
**	The parameter is printed out if non-zero.
**
**	Uses trace flag 14
*/
void
prompt(char *msg)
{
	if (!Prompt || GiveEof)
		return;
	if (Nodayfile >= 0) {
		if (msg) {
			noise(1);
			printf("%s\n", msg);
		}
		printf("* ");
	}
	fflush(stdout);
}


/*
**  PROMPT WITH CONTINUE OR GO
*/
void
cgprompt(void)
{
	prompt(Notnull ? "continue" : "go");
}
