#include <stdio.h>

#include <ingres.h>
#include <aux.h>
#include <access.h>
#include "sccs.h"
#include <func.h>

#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)sysmodfunc.c	8.1	12/31/84)

/*
**  Configuration table for SYSMOD
*/

char	Qbuf[10000];
int	QbufSize = sizeof(Qbuf);

extern func_t	KsortFn;
extern func_t	ModifyFn;
extern func_t	SysFuncFn;


func_t	*FuncVect[] = {
	&SysFuncFn,
	&ModifyFn,	
	&KsortFn,
};

int	NumFunc = sizeof(FuncVect) / sizeof(FuncVect[0]);

void
rubproc(void)
{
	ctlmod_rubproc();
}

void
main(int argc, char **argv)
{
#ifdef DEBUG
	printf("%d: sysmodfunc: calling ctlmod_main\n", getpid());
#endif
	exit(ctlmod_main(argc, argv, 0));
}
