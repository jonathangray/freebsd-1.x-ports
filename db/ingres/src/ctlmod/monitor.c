#include <sys/types.h>

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

#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#else
struct tms {
	clock_t	tms_utime;
	clock_t	tms_stime;
	clock_t	tms_cutime;
	clock_t	tms_cstime;
};
#endif

#include <stdio.h>

#include "ctlmod.h"
#include <useful.h>
#include <pmon.h>
#include "sccs.h"

#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)monitor.c	8.1	12/31/84)

/*
**  MARKPERF -- mark the performance info in a monitor struct
**
**	At any point, there is a monitor struct representing the
**	current object running.  This is stored in the static
**	variable "curmon".  This call essentially does a "context
**	switch" to the structure passed as the argument.
**
**	Parameters:
**		mbuf -- a pointer to a monitor struct.
**
**	Returns:
**		a pointer to the previous monitor struct.
**
**	Side Effects:
**		none
*/

#define	HZ	60	/* speed of clock in ticks/second */

monitor_t *
markperf(register monitor_t *mbuf)
{
	register long		ut;
	register long		st;
	static monitor_t	*curmon;
	register monitor_t	*oldmon;

#ifdef HAVE_SYS_RESOURCE_H
	static struct rusage	baset;
	struct rusage		ru;

	(void) getrusage(RUSAGE_SELF, &ru);
	ut = ((ru.ru_utime.tv_sec - baset.ru_utime.tv_sec) * 1000000) +
				ru.ru_utime.tv_usec - baset.ru_utime.tv_usec;
	st = ((ru.ru_stime.tv_sec - baset.ru_stime.tv_sec) * 1000000) +
				ru.ru_stime.tv_usec - baset.ru_stime.tv_usec;
#else
	struct tms		tbuf;
	static struct tms	baset;

	times(&tbuf);
	ut = tbuf.tms_utime + tbuf.tms_cutime -
				baset.tms_utime - baset.tms_cutime;
	st = tbuf.tms_stime + tbuf.tms_cstime -
				baset.tms_stime - baset.tms_cstime;
#endif
	oldmon = curmon;
	if (oldmon != NULL) {
		oldmon->mon_utime += ut;
		oldmon->mon_stime += st;
	}
	curmon = mbuf;
#ifdef HAVE_SYS_RESOURCE_H
	bmove(&ru, &baset, sizeof(baset));
#else
	bmove(&tbuf, &baset, sizeof(baset));
#endif
	return (oldmon);
}

/*
**  ADD_MON -- "add" two monitor structs
**
**	The logical sum of two monitor structs is created
**
**	Parameters:
**		a -- the first monitor struct
**		b -- the second monitor struct; gets the result.
**
**	Returns:
**		none (value is returned through b)
**
**	Side Effects:
**		none.
*/
void
add_mon(register monitor_t *a, register monitor_t *b)
{
	b->mon_utime += a->mon_utime;
	b->mon_stime += a->mon_stime;
}

/*
**  CVT_TIME -- convert time for output
**
**	Converts a time in ticks to a string (in seconds) for
**	printing.
**
**	Parameters:
**		t -- time in ticks
**
**	Returns:
**		pointer to string suitable for printing or framing.
**
**	Side Effects:
**		previous return value is clobbered.
*/

char *
cvt_time(long t)
{
	static char	buf[30];

	(void) sprintf(buf, "%.3f", ((float) t) / ((float) HZ));
	return(buf);
}
