#include <sys/types.h>

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#include <stdio.h>

#ifdef TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifndef HAVE_SYS_NERR
extern int	sys_nerr;
#endif

#include "monitor.h"
#include <ingres.h>
#include <aux.h>
#include <signal.h>
#include "sccs.h"

#define INGRES_IUTIL
#define INGRES_GUTIL
#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)quit.c	8.2	1/17/85)

/*
**  QUIT INGRES
**
**	This routine starts the death of the other processes.  It
**	then prints out the logout message, and then waits for the
**	rest of the system to die.  Note, however, that no relations
**	are removed; this must be done using the PURGE command.
**
**	Trace Flags:
**		35
*/

/* list of fatal signals */
char	*Siglist[] = {
	"Signal 0",
	"hangup",
	"interrupt",
	"quit",
	"illegal instruction",
	"trace trap",
	"IOT",
	"EMT",
	"floating point exception",
	"killed",
	"bus error",
	"segmentation violation",
	"bad system call",
	"broken pipe",
	"alarm",
};

extern void	(*ExitFn)();

RETSIGTYPE
quit(int i)
{
	register int	ndx;
	register int	pidptr;
	register int	err;
	time_t		t;
	char		buf[100];
	int		status;
	int		pidlist[50];
	char		indexx[256];

#ifdef xMTR1
	if (tTf(35, -1))
		printf("entered quit\n");
#endif

	/* INTERCEPT ALL FURTHER INTERRUPTS */
	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	ExitFn = exit;

	cm_close();

#ifdef xMTR3
	if (tTf(35, 2))
		printf("unlinking %s\n", Qbname);
#endif

	/* REMOVE THE QUERY-BUFFER FILE */
	fclose(Qryiop);
	unlink(Qbname);
	if (Trapfile != NULL)
		fclose(Trapfile);
	pidptr = 0;
	err = 0;

	/* clear out the system error index table */
	for (ndx = 0; ndx < 0400; ndx++)
		indexx[ndx] = 0;

	/* wait for all process to terminate */
	while ((ndx = wait(&status)) != -1) {
#ifdef xMTR2
		if (tTf(35, 5))
			printf("quit: pid %u: %d/%d\n",
				ndx, status >> 8, status & 0177);
#endif
		pidlist[pidptr++] = ndx;
		if ((status & 0177) != 0) {
			printf("%d: ", ndx);
			ndx = status & 0177;
			if (ndx > sizeof(Siglist) / sizeof(Siglist[0])) {
				printf("Abnormal Termination %d", ndx);
			} else {
				printf("%s", Siglist[ndx]);
			}
			if ((status & 0200) != 0)
				printf(" -- Core Dumped");
			printf("\n");
			err++;
			indexx[I1MASK - ndx]++;
		} else {
			indexx[(status >> 8) & I1MASK]++;
		}
	}
	if (err) {
		printf("pid list:");
		for (ndx = 0; ndx < pidptr; ndx++)
			printf(" %u", pidlist[ndx]);
		printf("\n");
	}

	/* print index of system errors */
	err = 0;
	for (ndx = 1; ndx <= I1MASK; ndx++) {
		if (indexx[ndx] == 0) {
			continue;
		}
		if (ndx <= sys_nerr) {
			if (err == 0) {
				printf("\nUNIX error dictionary:\n");
			}
			printf("%3d: %s\n", ndx, strerror(ndx));
		}
		if (err == 0) {
			err = ndx;
		}
	}
	if (err > 0 && err <= sys_nerr) {
		printf("\n");
	}

	/* PRINT LOGOUT CUE ? */
	if (Nodayfile >= 0) {
		(void) time(&t);
		printf("%s logout\n%s", getsysident(), ctime(&t));
		if (getuser(Usercode, buf) == 0) {
			for (ndx = 0; buf[ndx]; ndx++) {
				if (buf[ndx] == ':') {
					break;
				}
			}
			buf[ndx] = 0;
			printf("goodbye %s ", buf);
		} else {
			printf("goodbye ");
		}
		printf("-- come again\n");
	}
#ifdef xMTR1
	if (tTf(35, 3)) {
		printf("quit: exit(%d)\n", err);
	}
#endif
	exit(err);
}
