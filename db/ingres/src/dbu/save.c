#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <pv.h>
#include <ingres.h>
#include <aux.h>
#include <func.h>
#include "sccs.h"
#include <errors.h>

#ifdef TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif
#endif

#define INGRES_IUTIL
#define INGRES_GUTIL
#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)save.c	8.4	1/22/88)

extern	short	tTdbu[];

func_t SaveFn = {
	"SAVE",
	save,
	null_fn,		/* initialization function */
	null_fn,
	NULL,
	0,
	tTdbu,
	100,
	'Z',
	0
};

/*
**  SAVE RELATION UNTIL DATE
**
**	This function arranges to save a named relation until a
**	specified date.
**
**	Parameters:		(pv_type is PV_STR for all of them)
**	0 -- relation name
**	1 -- month (1 -> 12 or "jan" -> "dec" or a variety of other codes)
**	2 -- day (1 -> 31)
**	3 -- year (1970 -> ?)
**
**	Uses trace flag 44
**	Uses error messages 56xx
*/

int	DmSize[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

/*
**  MONTHCHECK
*/

struct monthtab {
	char	*code;
	int	month;
};

struct monthtab	Monthtab[] = {
	{ "jan",	1 },
	{ "feb",	2 },
	{ "mar",	3 },
	{ "apr",	4 },
	{ "may",	5 },
	{ "jun",	6 },
	{ "jul",	7 },
	{ "aug",	8 },
	{ "sep",	9 },
	{ "oct",	10 },
	{ "nov",	11 },
	{ "dec",	12 },
	{ "january",	1 },
	{ "february",	2 },
	{ "march",	3 },
	{ "april",	4 },
	{ "may",	5 },
	{ "june",	6 },
	{ "july",	7 },
	{ "august",	8 },
	{ "september",	9 },
	{ "october",	10 },
	{ "november",	11 },
	{ "december",	12 },
	{ 0 }
};

/* returns non-zero if 'year' is a leap year */
static int
isleapyear(int year)
{
	return((year % 400 == 0) || ((year % 4 == 0) && (year % 100 != 0)));
}

int
monthcheck(char *input, int *output)
{
	register struct monthtab	*p;
	int				month;

	/* month can be an integer, or an alphanumeric code */
	month = atoi(input);
	if (month != 0) {
		*output = month;
		return (month < 1 || month > 12);
	}
	for (p = Monthtab; p->code; p++) {
		if (strcmp(input, p->code) == 0) {
			*output = p->month;
			return (0);
		}
	}
	return (1);
}

int
dysize(int year)
{
	return(isleapyear(year) ? 366 : 365);
}

/*
**  MONTHSIZE -- determine size of a particular month
*/
int
monthsize(int month, int year)
{
	register int	size;

	size = DmSize[month];
	if (month == 1 && dysize(year) == 366)
		/* This is February of a leap year */
		size++;

	return (size);

}

int
save(int parmc, paramv_t *parmv)
{
#ifdef HAVE_TIME_H
	time_t		now;
#endif
	long		date;
	register int	i;
	extern desc_t	Reldes;
	tid_t		tid;
	extern char	*Usercode;
	relation_t	relk, relt;
	int		day, month, year;

	/* convert date */
	/* "date" will be # of days from 1970 for a while */
	date = 0;

	if (parmc == 1)
		goto nodate;

	/*
	**  Validate parameters.
	**
	**	According to my pocket calculator, a 31 bit number will
	**	hold 70 years of accuracy -- hence the 2035 cutoff.  If
	**	this code is still around in 2035, I apologize in
	**	advance.
	*/

	year = atoi(parmv[3].pv_val.pv_str);
	if (year < 1970 || year > 2035) {
		/* bad year */
		return (error(BADYEAR, parmv[3].pv_val.pv_str, 0));
	}
	if (monthcheck(parmv[1].pv_val.pv_str, &month)) {
		/* bad month */
		return (error(BADMONTH, parmv[1].pv_val.pv_str, 0));
	}
	day = atoi(parmv[2].pv_val.pv_str);
	if (day < 1 || day > monthsize(--month, year)) {
		/* bad day */
		return (error(BADDAY, parmv[2].pv_val.pv_str, 0));
	}

	/* do year conversion */
	for (i = 1970; i < year; i++) {
		date += dysize(i);
	}

	/* do month conversion */
	for (i = 0; i < month; i++) {
		date += DmSize[i];
	}

	/* once again, allow for leapyears */
	if (month >= 2 && isleapyear(year)) {
		date += 1;
	}

	/* do day conversion */
	date += day - 1;

	/* we now convert date to be the # of hours since 1970 */
	date *= 24;

	/* do daylight savings computations */
	/*  <<<<< none now >>>>> */

	/* convert to seconds */
	date *= 60 * 60;

	/* adjust to local time */
#ifdef HAVE_TIME_H
	now = time((time_t *) NULL);
	date += (localtime(&now))->tm_gmtoff;
#endif

#ifdef xZTR1
	if (tTf(45, 1)) {
		printf("%s", ctime(&date));
	}
#endif

nodate:
	/* let's check and see if the relation exists */
	opencatalog("relation", OR_WRITE);
	clearkeys(&Reldes);
	ingres_setkey(&Reldes, &relk, parmv[0].pv_val.pv_str, RELID);
	ingres_setkey(&Reldes, &relk, Usercode, RELOWNER);
	if (getequal(&Reldes, &relk, &relt, &tid)) {
		return (error(RELNOTFOUND, parmv[0].pv_val.pv_str, 0));	/* relation not found */
	}

	/* check that it is not a system catalog */
	if (relt.r_status & S_CATALOG)
		return (error(NOSAVESYSREL, parmv[0].pv_val.pv_str, 0));	/* cannot save sys rel */
	/* got it; lets change the date */
	relt.r_savetime = date;

#ifdef xZTR2
	if (tTf(45, 2)) {
		printup(&Reldes, (char *) &relt);
	}
#endif

	if ((i = replace(&Reldes, &tid, &relt, 0)) < 0)
		syserr("SAVE: replace %d", i);

	/* that's all folks.... */
	return (0);
}
