/*
 * Replacement for BSD <sys/time.h>
 * because Ultrix screws it up.
 */
/* $Id: time.h,v 1.1 1994/04/16 21:38:55 sean Exp $ */

struct timeval {
	long tv_sec;		/* time_t */
	long tv_usec;		/* microsex */
};

struct timezone {
	int tz_minuteswest;	/* of Greenwinch */
	int tz_dsttime;		/* type of dst correction to apply */
};
