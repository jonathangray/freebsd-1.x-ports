#include <stdio.h>

#include <ingres.h>
#include <func.h>
#include "sccs.h"

#define INGRES_CTLMOD
#include "protos.h"


SCCSID(@(#)sysmonitor.c	8.1	12/31/84)


desc_t	Btreesec;

char	Qbuf[400];
int	QbufSize = sizeof(Qbuf);

extern func_t	SysTtyMonFn;

func_t	*FuncVect[] = {
	&SysTtyMonFn,
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
	printf("%d: sysmonitor: calling ctlmod_main\n", getpid());
#endif
	exit(ctlmod_main(argc, argv, 1));
}
