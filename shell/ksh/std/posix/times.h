/*
 * sys/times.h: POSIX times()
 */
/* $Id: times.h,v 1.1 1994/04/16 21:38:54 sean Exp $ */

#if ! _TIMES_H
#define	_TIMES_H 1

#include <time.h>		/* defines CLK_TCK */

#if __STDC__
#define	ARGS(args)	args
#else
#define	ARGS(args)	()
#endif

struct tms {
	clock_t	tms_utime, tms_stime;
	clock_t	tms_cutime, tms_cstime;
};

#if _V7
#define times times_
#endif

clock_t	times ARGS((struct tms *tmsp));

#endif

