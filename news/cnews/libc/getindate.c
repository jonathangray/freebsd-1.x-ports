/*
 * getindate - parse the common Internet date case (rfc 822 & 1123) *fast*
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <time.h>
#include <sys/types.h>
#include <sys/timeb.h>
#include "dateconv.h"
#include "datetok.h"

/* STREQ is an optimised strcmp(a,b)==0 */
#define STREQ(a, b) ((a)[0] == (b)[0] && strcmp(a, b) == 0)

#define	PACK_TWO_CHARS(c1, c2)	(((c1)<<8)|(c2))
#define ISSPACE(c) ((c) == ' ' || (c) == '\n' || (c) == '\t')
#define SKIPTOSPC(s) \
	while ((ch = *(s)++), (!ISSPACE(ch) && ch != '\0')) \
		; \
	(s)--			/* N.B.: no semi-colon */
#define SKIPSPC(s) \
	while ((ch = *(s)++), ISSPACE(ch)) \
		; \
	(s)--			/* N.B.: no semi-colon */
#define SKIPOVER(s) \
	SKIPTOSPC(s); \
	SKIPSPC(s)		/* N.B.: no semi-colon */

/* this is fast but dirty.  note the return's in the middle. */
#define GOBBLE_NUM(cp, c, x, ip) \
	(c) = *(cp)++; \
	if ((c) < '0' || (c) > '9') \
		return -1;		/* missing digit */ \
	(x) = (c) - '0'; \
	(c) = *(cp)++; \
	if ((c) >= '0' && (c) <= '9') { \
		(x) = 10*(x) + (c) - '0'; \
		(c) = *(cp)++; \
	} \
	if ((c) != ':' && (c) != '\0' && !ISSPACE(c)) \
		return -1;		/* missing colon */ \
	*(ip) = (x)			/* N.B.: no semi-colon here */

/*
 * If the date is in the form
 *	[Weekday,] dd Mmm [19]yy hh:mm[:ss] Timezone
 * as most dates in news articles are, then we can parse it much quicker than
 * getdate and quite a bit faster than getabsdate.
 *
 * parse and convert Internet date in timestr (the normal interface)
 */
/* ARGSUSED */
time_t
getindate(line, now)
register char *line;			/* can be modified */
struct timeb *now;			/* unused; for getdate compatibility */
{
	int tz = 0;
	struct tm date;

	return prsindate(line, &date, &tz) < 0? -1: dateconv(&date, tz);
}

/*
 * just parse the Internet date in timestr and get back a broken-out date.
 */
int
prsindate(line, tm, tzp)
register char *line;			/* can be modified */
register struct tm *tm;
int *tzp;
{
	register int c;
	register char ch;		/* used by SKIPTOSPC */
	register char *cp;
	register char c2;

	tm->tm_isdst = 0;
	SKIPSPC(line);
	if ((ch = *line) < '0' || ch > '9') {
		cp = line;
		while ((ch = *cp++), (!ISSPACE(ch) && ch != ',' && ch != '\0'))
			;
		cp--;
		if (ch == ',') {
			line = cp;
			SKIPOVER(line);		/* skip weekday */
		} else
			return -1;		/* missing comma after weekday */
	}

	GOBBLE_NUM(line, ch, c, &tm->tm_mday);

	/*
	 * we have to map to canonical case because RFC 822 requires
	 * case independence, so we pay a performance penalty for the sake
	 * of 0.1% of dates actually seen in Date: headers in news.
	 * Way to go, IETF.
	 */
	ch = *line++;
	if (ch == '\0')
		return -1;		/* no month */
	if (isascii(ch) && islower(ch))
		ch = toupper(ch);
	c2 = *line++;
	if (c2 == '\0')
		return -1;		/* month too short */
	if (isascii(c2) && isupper(c2))
		c2 = tolower(c2);
	switch (PACK_TWO_CHARS(ch, c2)) {
	case PACK_TWO_CHARS('J', 'a'):
		tm->tm_mon = 1;
		break;
	case PACK_TWO_CHARS('F', 'e'):
		tm->tm_mon = 2;
		break;
	case PACK_TWO_CHARS('M', 'a'):	/* March, May */
		tm->tm_mon = ((ch = *line) == 'r' || ch == 'R'? 3: 5);
		break;
	case PACK_TWO_CHARS('A', 'p'):
		tm->tm_mon = 4;
		break;
	case PACK_TWO_CHARS('J', 'u'):
		tm->tm_mon = 6;
		if ((ch = *line) == 'l' || ch == 'L')
			tm->tm_mon++;		/* July */
		break;
	case PACK_TWO_CHARS('A', 'u'):
		tm->tm_mon = 8;
		break;
	case PACK_TWO_CHARS('S', 'e'):
		tm->tm_mon = 9;
		break;
	case PACK_TWO_CHARS('O', 'c'):
		tm->tm_mon = 10;
		break;
	case PACK_TWO_CHARS('N', 'o'):
		tm->tm_mon = 11;
		break;
	case PACK_TWO_CHARS('D', 'e'):
		tm->tm_mon = 12;
		break;
	default:
		return -1;		/* bad month name */
	}
	tm->tm_mon--;			/* convert month to zero-origin */
	SKIPOVER(line);			/* skip month */

	tm->tm_year = atoi(line);
	if (tm->tm_year <= 0)
		return -1;		/* year is non-positive or missing */
	if (tm->tm_year >= 1900)	/* convert year to 1900 origin, */
		tm->tm_year -= 1900;	/* but 2-digit years need no work */
	SKIPOVER(line);			/* skip year */

	if (parsetime(line, tm) < 0)
		return -1;
	SKIPOVER(line);			/* skip time */

	cp = line;
	if (*cp++ == 'G' && *cp++ == 'M' && *cp++ == 'T' &&
	    (*cp == '\n' || *cp == '\0'))
		*tzp = 0;
	else {				/* weirdo time zone */
		register datetkn *tp;

		cp = line;		/* time zone start */
		SKIPTOSPC(line);
		c = *line;		/* save old delimiter */
		*line = '\0';		/* terminate time zone */

		tp = datetoktype(cp, (int *)NULL);
		switch (tp->type) {
		case DTZ:
#if 0
			tm->tm_isdst++;
#endif
			/* FALLTHROUGH */
		case TZ:
			*tzp = FROMVAL(tp);
			/* FALLTHROUGH */
		case IGNORE:
			break;
		default:
			return -1;	/* bad token type */
		}

		*line = c;		/* restore old delimiter */
		SKIPSPC(line);
		if (*line != '\0') {	/* garbage after the date? */
			if (*line != '(')	/* not even an 822 comment? */
				return -1;
			/*
			 * a full 822 parse of the comment would
			 * be ridiculously complicated, so nested
			 * comments and quotes are not honoured.
			 * just look for a closing paren; it's only
			 * a time zone name.
			 */
			while ((c = *++line) != ')' && c != '\0')
				;
			if (c == ')')
				++line;
			else
				return -1;	/* comment not terminated */
			SKIPSPC(line);
			if (*line != '\0')	/* trash left? */
				return -1;
		}
	}
	return 0;
}

/* return -1 on failure */
int
parsetime(time, tm)
register char *time;
register struct tm *tm;
{
	register char c;
	register int x;

	tm->tm_sec = 0;
	GOBBLE_NUM(time, c, x, &tm->tm_hour);
	if (c != ':')
		return -1;		/* only hour; too short */
	GOBBLE_NUM(time, c, x, &tm->tm_min);
	if (c != ':')
		return 0;		/* no seconds; okay */
	GOBBLE_NUM(time, c, x, &tm->tm_sec);
	/* this may be considered too strict.  garbage at end of time? */
	return (c == '\0' || ISSPACE(c)? 0: -1);
}
