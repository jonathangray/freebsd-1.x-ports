#include <stdio.h>
#include <ingres.h>
#include <symbol.h>
#include "IIglobals.h"
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)IIretrieve.c	8.1	12/31/84)


/*
**	IIretrieve is called once for each element
**	in the target list of a retrieve.
**
**	The purpose is to set up the IIretsym structure
**	for IIgettup.
*/
void
IIretrieve(char *addr, int type)
{
	register struct retsym	*sym;
	register int		t, l;

	t = l = 0;
	sym = &IIretsym[IIndomains++];
	switch (type) {

	  case opSHORT:
		t = INT_CONST;
		l = 2;
		break;

	  case opLONG:
		t = INT_CONST;
		l = 4;
		break;

	  case opFLOAT:
		t = FLOAT_CONST;
		l = 4;
		break;

	  case opDOUBLE:
		t = FLOAT_CONST;
		l = 8;
		break;

	  case opSTRING:
		t = CHAR_CONST;
		l = 255;	/* with the current implementation the length is not known */
		break;

	  default:
		IIsyserr("retrieve:bad type %d", type);
	}
	sym->type = t;
	sym->len = l;
	sym->addr = addr;
#ifdef xETR1
	if (IIdebug)
		printf("domain %d type %d len %d\n", IIndomains, t, l);
#endif
}
