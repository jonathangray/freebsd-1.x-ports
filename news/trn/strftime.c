/*
 * strftime: print formatted information about a given time.
 * Adapted from the routine by Eric R. Smith, Michal Jaegermann,
 * Arnold Robins, and Paul Close.
 */
/* The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

/* Configuration choices for %x and %X */

#undef LOCAL_DDMMYY	/* choose DD/MM/YY instead of MM/DD/YY */
#undef LOCAL_DOTTIME	/* choose HH.MM.SS instead of HH:MM:SS */

#include "EXTERN.h"
#include "common.h"

static char *mth_name[] = {
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
};

static char *day_name[] = {
    "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
    "Saturday"
};

#ifdef HAS_FTIME
# ifndef TM_ZONE
char tznm[16] = "";
# endif
#else
extern char *tzname[];
#endif

size_t
strftime(str, maxsize, fmt, ts)
char *str;
size_t maxsize;
CONST char *fmt;
CONST struct tm *ts;
{
    size_t num = 0, len;
    char ch;
    char *putstr;
    char tmpbuf[80];

    if (maxsize-- <= 0)
	return 0;

    for (;;) {
	if (!(ch = *fmt++))
	    break;
	if (num == maxsize) {
	    num = 0;
	    break;
	}
	if (ch != '%') {
	    *str++ = ch;
	    num++;
	    continue;
	}
	/* assume the finished product will be sprintf'ed into tmpbuf */
	putstr = tmpbuf;

	switch (ch = *fmt++) {
	case 'A':
	case 'a':
	    if (ts->tm_wday < 0 || ts->tm_wday > 6)
		putstr = "?";
	    else
		if (ch == 'A')
		    putstr = day_name[ts->tm_wday];
		else
		    sprintf(tmpbuf, "%-.3s", day_name[ts->tm_wday]);
	    break;
	case 'B':
	case 'b':
	case 'h':
	    if (ts->tm_mon < 0 || ts->tm_mon > 11)
		putstr = "?";
	    else if (ch == 'B')
		putstr = mth_name[ts->tm_mon];
	    else
		sprintf(tmpbuf, "%-.3s", mth_name[ts->tm_mon]);
	    break;
	case 'C':
	    strftime(tmpbuf, sizeof tmpbuf, "%A, %B %e, %Y", ts);
	    break;
	case 'c':
	    strftime(tmpbuf, sizeof tmpbuf, "%x %X", ts);
	    break;
	case 'D':
#ifndef LOCAL_DDMMYY
	case 'x':
#endif
	    strftime(tmpbuf, sizeof tmpbuf, "%m/%d/%y", ts);
	    break;
	case 'd':
	    sprintf(tmpbuf, "%02d", ts->tm_mday);
	    break;
	case 'e':	/* day of month, blank padded */
	    sprintf(tmpbuf, "%2d", ts->tm_mday);
	    break;
	case 'H':
	    sprintf(tmpbuf, "%02d", ts->tm_hour);
	    break;
	case 'I':
	{
	    int n;

	    n = ts->tm_hour;
	    if (n == 0)
		n = 12;
	    else if (n > 12)
		n -= 12;
	    sprintf(tmpbuf, "%02d", n);
	    break;
	}
	case 'j':
	    sprintf(tmpbuf, "%03d", ts->tm_yday + 1);
	    break;
	case 'm':
	    sprintf(tmpbuf, "%02d", ts->tm_mon + 1);
	    break;
	case 'M':
	    sprintf(tmpbuf, "%02d", ts->tm_min);
	    break;
	case 'p':
	    putstr = (ts->tm_hour < 12) ? "AM" : "PM";
	    break;
	case 'r':
	    strftime(tmpbuf, sizeof tmpbuf, "%I:%M:%S %p", ts);
	    break;
	case 'R':
	    strftime(tmpbuf, sizeof tmpbuf, "%H:%M", ts);
	    break;
	case 'S':
	    sprintf(tmpbuf, "%02d", ts->tm_sec);
	    break;
	case 'T':
#ifndef LOCAL_DOTTIME
	case 'X':
#endif
	    strftime(tmpbuf, sizeof tmpbuf, "%H:%M:%S", ts);
	    break;
	case 'U':	/* week of year - starting Sunday */
	    sprintf(tmpbuf, "%02d", (ts->tm_yday - ts->tm_wday + 10) / 7);
	    break;
	case 'W':	/* week of year - starting Monday */
	    sprintf(tmpbuf, "%02d", (ts->tm_yday - ((ts->tm_wday + 6) % 7)
			+ 10) / 7);
	    break;
	case 'w':
	    sprintf(tmpbuf, "%d", ts->tm_wday);
	    break;
	case 'y':
	    sprintf(tmpbuf, "%02d", ts->tm_year % 100);
	    break;
#ifdef LOCAL_DOTTIME
	case 'X':
	    strftime(tmpbuf, sizeof tmpbuf, "%H.%M.%S", ts);
	    break;
#endif
#ifdef LOCAL_DDMMYY
	case 'x':
	    strftime(tmpbuf, sizeof tmpbuf, "%d/%m/%y", ts);
	    break;
#endif
	case 'Y':
	    sprintf(tmpbuf, "%d", ts->tm_year + 1900);
	    break;
	case 'Z':
#ifdef HAS_FTIME
# ifdef TM_ZONE
	    sprintf(tmpbuf, "%s", ts->tm_zone);
# else
	    if (*tznm == '\0') {
		char *timezone();
		struct timeval tv;
		struct timezone tz;

		(void) gettimeofday(&tv, &tz);
		strcpy(tznm, timezone(tz.tz_minuteswest, ts->tm_isdst));
	    }
	    sprintf(tmpbuf, "%s", tznm);
# endif
#else
	    sprintf(tmpbuf, "%s", tzname[ts->tm_isdst]);
#endif
	    break;
	case '%':
	case '\0':
	    putstr = "%";
	    break;
	case 'n':	/* same as \n */
	    putstr = "\n";
	    break;
	case 't':	/* same as \t */
	    putstr = "\t";
	    break;
	default:
	    sprintf(tmpbuf, "%%%c", ch);
	    break;
	}
	len = strlen(putstr);
	num += len;
	if (num > maxsize) {
	    len -= num - maxsize;
	    num = 0;
	    ch = '\0';
	}
	strncpy(str, putstr, len);
	str += len;
	if (!ch)
	    break;
    }
    *str = '\0';
    return num;
}
