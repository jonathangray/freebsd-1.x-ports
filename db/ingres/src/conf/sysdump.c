#include <stdio.h>

#include <ingres.h>
#include <func.h>
#include "sccs.h"

#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)sysdump.c	8.1	12/31/84)



char	Qbuf[400];
int	QbufSize = sizeof(Qbuf);

extern func_t	SysDmpFn;

func_t	*FuncVect[] = {
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
	&SysDmpFn,
};

int	NumFunc = sizeof(FuncVect) / sizeof(FuncVect[0]);

desc_t	Btreesec;

void
rubproc(void)
{
	ctlmod_rubproc();
}

void
main(int argc, char **argv)
{
#ifdef DEBUG
	printf("%d: sysdump: calling ctlmod_main\n", getpid());
#endif
	exit(ctlmod_main(argc, argv, 1));
}
