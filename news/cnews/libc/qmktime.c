/*
 * qmktime - convert a split-out date (struct tm) back into a time_t [ANSI]
 */

#include <time.h>
#include <sys/types.h>
#include <sys/timeb.h>

#define EPOCH 1970
#define DAYS_PER_400YRS	(time_t)146097
#define DAYS_PER_4YRS	(time_t)1461
#define DIVBY4(n) ((n) >> 2)
#define YRNUM(c, y) (DIVBY4(DAYS_PER_400YRS*(c)) + DIVBY4(DAYS_PER_4YRS*(y)))
#define DAYNUM(c,y,mon,d)	(YRNUM((c), (y)) + mdays[mon] + (d))
#define EPOCH_DAYNUM	DAYNUM(19, 69, 10, 1)	/* really January 1, 1970 */

static char nmdays[] = {
	0, 31, 28, 31,  30, 31, 30,  31, 31, 30,  31, 30, 31
};
/* days since start of year. mdays[0] is March, mdays[11] is February */
static short mdays[] = {
	0, 31, 61, 92, 122, 153, 184, 214, 245, 275, 306, 337
};

/*
 * near-ANSI qmktime suitable for use by dateconv; not necessarily as paranoid
 * as ANSI requires, and it may not canonicalise the struct tm.  Ignores tm_wday
 * and tm_yday.
 */
time_t
qmktime(tp)
register struct tm *tp;
{
	register int mon = tp->tm_mon + 1;	/* convert to 1-origin */
	register int day = tp->tm_mday, year = tp->tm_year + 1900;
	register time_t daynum;
	register int century;
	time_t nrdaynum;

	if (year < EPOCH)
		return -1;		/* can't represent early date */

	/*
	 * validate day against days-per-month table, with leap-year
	 * correction
	 */
	if (day > nmdays[mon])
		if (mon != 2 || year % 4 == 0 &&
		    (year % 100 != 0 || year % 400 == 0) && day > 29)
			return -1;	/* day too large for month */

	/* split year into century and year-of-century */
	century = year / 100;
	year %= 100;
	/*
	 * We calculate the day number exactly, assuming the calendar has
	 * always had the current leap year rules.  (The leap year rules are
	 * to compensate for the fact that the Earth's revolution around the
	 * Sun takes 365.2425 days).  We first need to rotate months so March
	 * is 0, since we want the last month to have the reduced number of
	 * days.
	 */
	if (mon > 2)
		mon -= 3;
	else {
		mon += 9;
		if (year == 0) {
			century--;
			year = 99;
		} else
			--year;
	}
	daynum = -EPOCH_DAYNUM + DAYNUM(century, year, mon, day);

	/* convert to seconds */
	nrdaynum = daynum =
		tp->tm_sec + (tp->tm_min +(daynum*24 + tp->tm_hour)*60)*60;

	/* daylight correction */
	if (tp->tm_isdst < 0)		/* unknown; find out */
		tp->tm_isdst = localtime(&nrdaynum)->tm_isdst;
	if (tp->tm_isdst > 0)
		daynum -= 60*60;

	return daynum < 0? -1: daynum;
}
