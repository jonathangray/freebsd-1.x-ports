#include "constants.h"
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)cmap.c	8.1	12/31/84)


/*
**  CMAP -- character map
**
**	Defines:
**		map of lexical class of characters
*/


static char	Cmap [] = {
	EOF_TOK, CNTRL, CNTRL, CNTRL, CNTRL, CNTRL, CNTRL, CNTRL,
	CNTRL, PUNCT, PUNCT, CNTRL, CNTRL, PUNCT, CNTRL, CNTRL,
	CNTRL, CNTRL, CNTRL, CNTRL, CNTRL, CNTRL, CNTRL, CNTRL,
	CNTRL, CNTRL, CNTRL, CNTRL, CNTRL, CNTRL, CNTRL, CNTRL,
	PUNCT, OPATR, OPATR, OPATR, OPATR, OPATR, OPATR, OPATR,
	OPATR, OPATR, OPATR, OPATR, OPATR, OPATR, OPATR, OPATR,
	NUMBR, NUMBR, NUMBR, NUMBR, NUMBR, NUMBR, NUMBR, NUMBR,
	NUMBR, NUMBR, OPATR, OPATR, OPATR, OPATR, OPATR, OPATR,
	OPATR, ALPHA, ALPHA, ALPHA, ALPHA, ALPHA, ALPHA, ALPHA,
	ALPHA, ALPHA, ALPHA, ALPHA, ALPHA, ALPHA, ALPHA, ALPHA,
	ALPHA, ALPHA, ALPHA, ALPHA, ALPHA, ALPHA, ALPHA, ALPHA,
	ALPHA, ALPHA, ALPHA, OPATR, OPATR, OPATR, OPATR, ALPHA,
	OPATR, ALPHA, ALPHA, ALPHA, ALPHA, ALPHA, ALPHA, ALPHA,
	ALPHA, ALPHA, ALPHA, ALPHA, ALPHA, ALPHA, ALPHA, ALPHA,
	ALPHA, ALPHA, ALPHA, ALPHA, ALPHA, ALPHA, ALPHA, ALPHA,
	ALPHA, ALPHA, ALPHA, OPATR, OPATR, OPATR, OPATR, CNTRL
};

int
equel_cmap(char ch)
{
	return(Cmap[(int) ch]);
}
