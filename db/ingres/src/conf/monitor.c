#include <stdio.h>

#include <ingres.h>
#include <func.h>
#include "sccs.h"

#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)monitor.c	8.2	12/18/85)



char	Qbuf[1600];
int	QbufSize = sizeof(Qbuf);

extern func_t	TtyMonFn;

func_t	*FuncVect[] = {
	&TtyMonFn,
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
	printf("%d: monitor: calling ctlmod_main\n", getpid());
#endif
	exit(ctlmod_main(argc, argv, 1));
}
