#include <stdio.h>
#include <signal.h>
#include <setjmp.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <ingres.h>
#include <version.h>
#include <pv.h>
#include <func.h>
#include <pipes.h>
#include "sccs.h"
#include <symbol.h>
#include "tree.h"
#include "../ctlmod/pipes.h"

#define INGRES_GUTIL
#define INGRES_CTLMOD
#include "protos.h"

/*
**	SYSTTYMON - 
**		sets up the pipe block for the sysmod function,
**		and passed the -R flags in the parmvector.
**
**		note: this is kludged in the same way that the
**			ingres monitor is-- there is no systm_mon.
**			all work is done in systm_init.
**
**	SCCSID @(#)sysmon.c	8.1	12/31/84
*/

void systm_init(int argc, char **argv);
int systm_mon(void);

short		tTsysttymon[100];
paramv_t		pv[PV_MAXPC+ 1];
int		pc;
extern jmp_buf  CmReset;

func_t	SysTtyMonFn = {
	"SYSMONITOR",
	systm_mon,
	systm_init,
	NULL,
	NULL,
	0,
	tTsysttymon,
	100,
	'S',
	0
};

/*
**  SYSSETP -- set parameter
**
**	Sets a parameter, to be later sent by 'call' to whomever.
**	This looks suspiciously like ctlmod/setp.c, except
**	it sets a local pc and pv, and doesnt need to know anything
**	about the Ctx struct.
**
**	Parameters:
**		type -- parameter type.
**			PV_STRING -- a string, 'len' is ignored.
**			PV_INT -- an integer, 'len' is ignored.
**		val -- the value (real value if PV_INT, pointer
**			otherwise).
**		len -- the length of the tuple.
**
**	Returns:
**		none
**
**		Adjusts pc & pv.
**
*/
void
syssetp(register int type, char *val, register int len)
{
	paramv_t	*pp;
	char	*newp;
	char	*buf;

	/*
	**  Check the magic bounds.
	*/

	pp = &pv[pc++];

	/*
	**  Figure out the length from the type.
	*/

	switch (type) {
	  case PV_STR:
		len = strlen(val) + 1;
		newp = need(Qbuf, len);
		bmove(val, newp, len);
		buf = newp;
		pp->pv_val.pv_str = newp;
		break;
	
	  case PV_INT:
		len = sizeof(short);
		pp->pv_val.pv_int = (int) val;
		break;

	
	  default:
		syserr("syssetp: type %d", type);
	}

	/*
	**  Set up the parameter.
	*/

	pp->pv_type = type;
	pp->pv_len = len;

#ifdef xSTR1
	if tTf(0,0) {
		lprintf("syssetp: ");
		pr_parm(pp);
	}
#endif
}

void
systm_init(int argc, char **argv)
{
	pb_t		pb;
	char		**p;
	char		*pp;

	/*
	**	THIS CODE IS A CLUDGE!!!
	**
	**	all work is done in the init function 
	** 	so that the sysmonitor will have control
	**	before the sysmod function.
	*/

	setjmp(CmReset);

	/* arrange to call the sysfunc */
	for ( p = &argv[6]; (pp = *p) != NULL; p++) {
		pp = *p;
		if (pp[1] == 'S')
			continue;
		syssetp(PV_STR, pp, 0);
	}
	call_setup(&pb, mdSYSFUNC, NULL);
	pb_prime(&pb, PB_REG);
	pb.pb_proc = 1;		/**** SYSFUNC MUST BE IN PROC ONE ****/
	send_off(&pb, pc, pv);
	pb_tput(PV_EOF, "", 0, &pb);
	pb_flush(&pb);

	/* wait for the response */
	readinput(&pb);
	resetp();
	exit();
}

/*
**  SYSTM_MON -- "function to implement this module"
**
**	Since we have cludged up this module to work, and hence
**	the init routine should never return, this routine just
**	syserr's.
*/
int
systm_mon(void)
{
	syserr("systm_mon");
	return(0);
}

/*
**  CLOSECATALOG -- dummy catalog close routine.
**
**	To keep from loading access methods.
*/
void
closecatalog(void)
{
#ifdef DEBUG
	printf("%d: fake closecatalog (sysmon)\n", getpid());
#endif
}

/*
**  GET FLAG
**
*/
/* need some data structure to hold the flags */
int
getflag(char **argv, char **dest)   
{
	int		destctr;
	int		i;

	destctr = 0;
	for (i = 0; i <= 6; i++) {
		if (argv[i][0] != 0) {
			strcpy( dest[destctr++], argv[i]);
		} else {
			return(0);
		}
	}
	return(0);
}
