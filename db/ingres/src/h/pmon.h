#ifndef INGRES_PMON_H_
#define INGRES_PMON_H_

/*
**  MONITOR.H -- structures dealing with performance monitoring
**
**	Version:
**		@(#)pmon.h	8.1	12/31/84
*/

typedef struct monitor {
	long	mon_utime;	/* user time in milliseconds */
	long	mon_stime;	/* system time in milliseconds */
	long	mon_pread;	/* pages read */
	long	mon_pwrit;	/* pages written */
	long	mon_cread;	/* catalog pages read */
	long	mon_cwrit;	/* catalog pages written */
	long	mon_xread;	/* ISAM/BTREE index pages read */
} monitor_t;

#endif /* !INGRES_PMON_H_ */
