/*
 * Uglix ftime simulation
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/timeb.h>		/* see news/include */
#include <sys/times.h>

#ifndef HZ
#define HZ 60
#endif

/* imports from libc */
extern time_t time();
extern time_t times();			/* only true on Uglix */
extern char *getenv();

ftime(tp)
struct timeb *tp;
{
	register char *tz;
#ifdef SANE
	/*
	 * Since times() is not required to use the same time base for its
	 * return value as time() uses, we can't easily get sub-second resolution.
	 */
	struct tms timesbuf;
	register int hz = times(&timesbuf) % HZ;	/* hertz beyond time(0) */

	tp->millitm = (hz*1000L)/HZ;
#else
	tp->millitm = 0;
#endif
	tp->time = time(&tp->time);
	tz = getenv("TZ");
	if (tz == NULL)			/* just pick one */
		tz = "EST5EDT";
	while (*tz != '\0' && isascii(*tz) && !isdigit(*tz) && *tz != '-')
		tz++;			/* find hrs from greenwich */
	tp->timezone = atoi(tz) * 60;	/* in minutes */
	while (*tz != '\0' && isascii(*tz) && (isdigit(*tz) || *tz == '-'))
		tz++;			/* find DST, if any */
	tp->dstflag = (*tz != '\0');
}
