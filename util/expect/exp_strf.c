/* exp_strp.c - functions for exp_timestamp */
/*
 * strftime.c
 *
 * Public-domain relatively quick-and-dirty implemenation of
 * ANSI library routine for System V Unix systems.
 *
 * It's written in old-style C for maximal portability.
 * However, since I'm used to prototypes, I've included them too.
 *
 * If you want stuff in the System V ascftime routine, add the SYSV_EXT define.
 * For stuff needed to implement the P1003.2 date command, add POSIX2_DATE.
 *
 * The code for %c, %x, and %X is my best guess as to what's "appropriate".
 * This version ignores LOCALE information.
 * It also doesn't worry about multi-byte characters.
 * So there.
 *
 * Arnold Robbins
 * January, February, March, 1991
 * Updated March 1992
 *
 * Fixes from ado@elsie.nci.nih.gov
 * February 1991
 *
 * Modified by Don Libes for Expect, October, 1993
 * Forced POSIX semantics.
 * Replaced inline/min/max stuff with a single range function.
 * Removed tzset stuff.
 * Commented out tzname stuff.
 *
 */

#include "tcl.h"

#include <stdio.h>
#include <ctype.h>
#include "string.h"
#include <time.h>
#include <sys/types.h>

#define SYSV_EXT	1	/* stuff in System V ascftime routine */
#define POSIX2_DATE	1	/* stuff in Posix 1003.2 date command */

#if defined(POSIX2_DATE) && ! defined(SYSV_EXT)
#define SYSV_EXT	1
#endif

#if defined(POSIX2_DATE)
#define adddecl(stuff)	stuff
#else
#define adddecl(stuff)
#endif

#ifndef __STDC__
#define const

extern char *getenv();
static int weeknumber();
adddecl(static int iso8601wknum();)
#else

extern char *strchr(const char *str, int ch);
extern char *getenv(const char *v);
static int weeknumber(const struct tm *timeptr, int firstweekday);
adddecl(static int iso8601wknum(const struct tm *timeptr);)
#endif

#if 0
extern char *tzname[2];
extern int daylight;
#endif

static int
range(low,item,hi)
int low, item, hi;
{
	if (item < low) return low;
	if (item > hi) return hi;
	return item;
}

/* strftime --- produce formatted time */

#ifndef __STDC__
size_t
exp_strftime(s, maxsize, format, timeptr)
char *s;
size_t maxsize;
const char *format;
const struct tm *timeptr;
#else
size_t
exp_strftime(char *s, size_t maxsize, const char *format, const struct tm *timeptr)
#endif
{
	char *endp = s + maxsize;
	char *start = s;
	char tbuf[100];
	int i;

	/* various tables, useful in North America */
	static char *days_a[] = {
		"Sun", "Mon", "Tue", "Wed",
		"Thu", "Fri", "Sat",
	};
	static char *days_l[] = {
		"Sunday", "Monday", "Tuesday", "Wednesday",
		"Thursday", "Friday", "Saturday",
	};
	static char *months_a[] = {
		"Jan", "Feb", "Mar", "Apr", "May", "Jun",
		"Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
	};
	static char *months_l[] = {
		"January", "February", "March", "April",
		"May", "June", "July", "August", "September",
		"October", "November", "December",
	};
	static char *ampm[] = { "AM", "PM", };

	for (; *format && s < endp - 1; format++) {
		tbuf[0] = '\0';
		if (*format != '%') {
			*s++ = *format;
			continue;
		}
	again:
		switch (*++format) {
		case '\0':
			*s++ = '%';
			goto out;

		case '%':
			*s++ = '%';
			continue;

		case 'a':	/* abbreviated weekday name */
			if (timeptr->tm_wday < 0 || timeptr->tm_wday > 6)
				strcpy(tbuf, "?");
			else
				strcpy(tbuf, days_a[timeptr->tm_wday]);
			break;

		case 'A':	/* full weekday name */
			if (timeptr->tm_wday < 0 || timeptr->tm_wday > 6)
				strcpy(tbuf, "?");
			else
				strcpy(tbuf, days_l[timeptr->tm_wday]);
			break;

#ifdef SYSV_EXT
		case 'h':	/* abbreviated month name */
#endif
		case 'b':	/* abbreviated month name */
			if (timeptr->tm_mon < 0 || timeptr->tm_mon > 11)
				strcpy(tbuf, "?");
			else
				strcpy(tbuf, months_a[timeptr->tm_mon]);
			break;

		case 'B':	/* full month name */
			if (timeptr->tm_mon < 0 || timeptr->tm_mon > 11)
				strcpy(tbuf, "?");
			else
				strcpy(tbuf, months_l[timeptr->tm_mon]);
			break;

		case 'c':	/* appropriate date and time representation */
			sprintf(tbuf, "%s %s %2d %02d:%02d:%02d %d",
				days_a[range(0, timeptr->tm_wday, 6)],
				months_a[range(0, timeptr->tm_mon, 11)],
				range(1, timeptr->tm_mday, 31),
				range(0, timeptr->tm_hour, 23),
				range(0, timeptr->tm_min, 59),
				range(0, timeptr->tm_sec, 61),
				timeptr->tm_year + 1900);
			break;

		case 'd':	/* day of the month, 01 - 31 */
			i = range(1, timeptr->tm_mday, 31);
			sprintf(tbuf, "%02d", i);
			break;

		case 'H':	/* hour, 24-hour clock, 00 - 23 */
			i = range(0, timeptr->tm_hour, 23);
			sprintf(tbuf, "%02d", i);
			break;

		case 'I':	/* hour, 12-hour clock, 01 - 12 */
			i = range(0, timeptr->tm_hour, 23);
			if (i == 0)
				i = 12;
			else if (i > 12)
				i -= 12;
			sprintf(tbuf, "%02d", i);
			break;

		case 'j':	/* day of the year, 001 - 366 */
			sprintf(tbuf, "%03d", timeptr->tm_yday + 1);
			break;

		case 'm':	/* month, 01 - 12 */
			i = range(0, timeptr->tm_mon, 11);
			sprintf(tbuf, "%02d", i + 1);
			break;

		case 'M':	/* minute, 00 - 59 */
			i = range(0, timeptr->tm_min, 59);
			sprintf(tbuf, "%02d", i);
			break;

		case 'p':	/* am or pm based on 12-hour clock */
			i = range(0, timeptr->tm_hour, 23);
			if (i < 12)
				strcpy(tbuf, ampm[0]);
			else
				strcpy(tbuf, ampm[1]);
			break;

		case 'S':	/* second, 00 - 61 */
			i = range(0, timeptr->tm_sec, 61);
			sprintf(tbuf, "%02d", i);
			break;

		case 'U':	/* week of year, Sunday is first day of week */
			sprintf(tbuf, "%02d", weeknumber(timeptr, 0));
			break;

		case 'w':	/* weekday, Sunday == 0, 0 - 6 */
			i = range(0, timeptr->tm_wday, 6);
			sprintf(tbuf, "%d", i);
			break;

		case 'W':	/* week of year, Monday is first day of week */
			sprintf(tbuf, "%02d", weeknumber(timeptr, 1));
			break;

		case 'x':	/* appropriate date representation */
			sprintf(tbuf, "%s %s %2d %d",
				days_a[range(0, timeptr->tm_wday, 6)],
				months_a[range(0, timeptr->tm_mon, 11)],
				range(1, timeptr->tm_mday, 31),
				timeptr->tm_year + 1900);
			break;

		case 'X':	/* appropriate time representation */
			sprintf(tbuf, "%02d:%02d:%02d",
				range(0, timeptr->tm_hour, 23),
				range(0, timeptr->tm_min, 59),
				range(0, timeptr->tm_sec, 61));
			break;

		case 'y':	/* year without a century, 00 - 99 */
			i = timeptr->tm_year % 100;
			sprintf(tbuf, "%02d", i);
			break;

		case 'Y':	/* year with century */
			sprintf(tbuf, "%d", 1900 + timeptr->tm_year);
			break;

#if 0
		case 'Z':	/* time zone name or abbrevation */
			i = 0;
			if (daylight && timeptr->tm_isdst)
				i = 1;
			strcpy(tbuf, tzname[i]);
			break;
#endif

#ifdef SYSV_EXT
		case 'n':	/* same as \n */
			tbuf[0] = '\n';
			tbuf[1] = '\0';
			break;

		case 't':	/* same as \t */
			tbuf[0] = '\t';
			tbuf[1] = '\0';
			break;

		case 'D':	/* date as %m/%d/%y */
			exp_strftime(tbuf, sizeof tbuf, "%m/%d/%y", timeptr);
			break;

		case 'e':	/* day of month, blank padded */
			sprintf(tbuf, "%2d", range(1, timeptr->tm_mday, 31));
			break;

		case 'r':	/* time as %I:%M:%S %p */
			exp_strftime(tbuf, sizeof tbuf, "%I:%M:%S %p", timeptr);
			break;

		case 'R':	/* time as %H:%M */
			exp_strftime(tbuf, sizeof tbuf, "%H:%M", timeptr);
			break;

		case 'T':	/* time as %H:%M:%S */
			exp_strftime(tbuf, sizeof tbuf, "%H:%M:%S", timeptr);
			break;
#endif

#ifdef POSIX2_DATE
		case 'C':
			sprintf(tbuf, "%02d", (timeptr->tm_year + 1900) / 100);
			break;


		case 'E':
		case 'O':
			/* POSIX locale extensions, ignored for now */
			goto again;
		case 'V':	/* week of year according ISO 8601 */
			sprintf(tbuf, "%02d", iso8601wknum(timeptr));
			break;

		case 'u':
		/* ISO 8601: Weekday as a decimal number [1 (Monday) - 7] */
			sprintf(tbuf, "%d", timeptr->tm_wday == 0 ? 7 :
					timeptr->tm_wday);
			break;
#endif	/* POSIX2_DATE */
		default:
			tbuf[0] = '%';
			tbuf[1] = *format;
			tbuf[2] = '\0';
			break;
		}
		i = strlen(tbuf);
		if (i)
			if (s + i < endp - 1) {
				strcpy(s, tbuf);
				s += i;
			} else
				return 0;
	}
out:
	if (s < endp && *format == '\0') {
		*s = '\0';
		return (s - start);
	} else
		return 0;
}

#ifdef POSIX2_DATE
/* iso8601wknum --- compute week number according to ISO 8601 */

#ifndef __STDC__
static int
iso8601wknum(timeptr)
const struct tm *timeptr;
#else
static int
iso8601wknum(const struct tm *timeptr)
#endif
{
	/*
	 * From 1003.2:
	 *	If the week (Monday to Sunday) containing January 1
	 *	has four or more days in the new year, then it is week 1;
	 *	otherwise it is week 53 of the previous year, and the
	 *	next week is week 1.
	 *
	 * ADR: This means if Jan 1 was Monday through Thursday,
	 *	it was week 1, otherwise week 53.
	 */

	int weeknum, jan1day;

	/* get week number, Monday as first day of the week */
	weeknum = weeknumber(timeptr, 1);

	/*
	 * With thanks and tip of the hatlo to tml@tik.vtt.fi
	 *
	 * What day of the week does January 1 fall on?
	 * We know that
	 *	(timeptr->tm_yday - jan1.tm_yday) MOD 7 ==
	 *		(timeptr->tm_wday - jan1.tm_wday) MOD 7
	 * and that
	 * 	jan1.tm_yday == 0
	 * and that
	 * 	timeptr->tm_wday MOD 7 == timeptr->tm_wday
	 * from which it follows that. . .
 	 */
	jan1day = timeptr->tm_wday - (timeptr->tm_yday % 7);
	if (jan1day < 0)
		jan1day += 7;

	/*
	 * If Jan 1 was a Monday through Thursday, it was in
	 * week 1.  Otherwise it was last year's week 53, which is
	 * this year's week 0.
	 *
	 * What does that mean?
	 * If Jan 1 was Monday, the week number is exactly right, it can
	 *	never be 0.
	 * If it was Tuesday through Thursday, the weeknumber is one
	 *	less than it should be, so we add one.
	 * Otherwise, Friday, Saturday or Sunday, the week number is
	 * OK, but if it is 0, it needs to be 53.
	 */
	switch (jan1day) {
	case 1:		/* Monday */
		break;
	case 2:		/* Tuesday */
	case 3:		/* Wednesday */
	case 4:		/* Thursday */
		weeknum++;
		break;
	case 5:		/* Friday */
	case 6:		/* Saturday */
	case 0:		/* Sunday */
		if (weeknum == 0)
			weeknum = 53;
		break;
	}
	return weeknum;
}
#endif

/* weeknumber --- figure how many weeks into the year */

/* With thanks and tip of the hatlo to ado@elsie.nci.nih.gov */

#ifndef __STDC__
static int
weeknumber(timeptr, firstweekday)
const struct tm *timeptr;
int firstweekday;
#else
static int
weeknumber(const struct tm *timeptr, int firstweekday)
#endif
{
	if (firstweekday == 0)
		return (timeptr->tm_yday + 7 - timeptr->tm_wday) / 7;
	else
		return (timeptr->tm_yday + 7 -
			(timeptr->tm_wday ? (timeptr->tm_wday - 1) : 6)) / 7;
}
