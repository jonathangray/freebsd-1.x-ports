/*
 * dateconv - convert a split-out date back into a time_t
 */

#include <time.h>
#include <sys/types.h>
#include <sys/timeb.h>

/* imports */
extern time_t qmktime();

/* turn a (struct tm) and a few variables into a time_t, with range checking */
time_t
dateconv(tm, zone)
register struct tm *tm;
int zone;
{
	tm->tm_wday = tm->tm_yday = 0;

	/* validate, before going out of range on some members */
	if (tm->tm_year < 0 || tm->tm_mon < 0 || tm->tm_mon > 11 ||
	    tm->tm_mday < 1 || tm->tm_hour < 0 || tm->tm_hour >= 24 ||
	    tm->tm_min < 0 || tm->tm_min > 59 ||
	    tm->tm_sec < 0 || tm->tm_sec > 61)	/* allow 2 leap seconds */
		return -1;

	/*
	 * zone should really be -zone, and tz should be set to tp->value, not
	 * -tp->value.  Or the table could be fixed.
	 */
	tm->tm_min += zone;		/* mktime lets it be out of range */

	/* convert to seconds */
	return qmktime(tm);
}
