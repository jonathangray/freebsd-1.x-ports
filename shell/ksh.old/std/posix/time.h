/*
 * Replacement for BSD <sys/time.h>
 * because Ultrix screws it up.
 */
/* time.h,v 1.1.1.1 1993/05/21 05:37:51 cgd Exp */

struct timeval {
	long tv_sec;		/* time_t */
	long tv_usec;		/* microsex */
};

struct timezone {
	int tz_minuteswest;	/* of Greenwinch */
	int tz_dsttime;		/* type of dst correction to apply */
};
