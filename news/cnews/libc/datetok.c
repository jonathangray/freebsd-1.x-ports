/*
 * datetok - date tokenisation
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <sys/types.h>		/* for dateconv.h */
#include "dateconv.h"
#include "datetok.h"

/* imports */
int dtok_numparsed;

/*
 * to keep this table reasonably small, we compact the lexval for TZ and DTZ
 * entries and truncate the text field at MAXTOKLEN characters.
 * the text field is not guaranteed to be NUL-terminated.
 * ST = Standard Time; DT = Daylight Time.
 */
static datetkn datetktbl[] = {
/*	text		token	lexval */
	"acsst",	DTZ,	PACK(630),	/* Cent. Australia */
	"acst",		TZ,	PACK(570),	/* Cent. Australia */
	"adt",		DTZ,	PACK(-180),	/* Atlantic DT */
	"aesst",	DTZ,	PACK(660),	/* E. Australia */
	"aest",		TZ,	PACK(600),	/* Australia Eastern ST */
	"akdt",		DTZ,	PACK(-480),	/* Alaska DT */
	"akst",		TZ,	PACK(-540),	/* Alaska ST */
	"am",		AMPM,	AM,
	"apr",		MONTH,	4,
	"april",	MONTH,	4,
	"ast",		TZ,	PACK(-240),	/* Atlantic ST (Canada) */
	"at",		IGNORE,	0,		/* "at" (throwaway) */
	"aug",		MONTH,	8,
	"august",	MONTH,	8,
	"awst",		TZ,	PACK(480),	/* W. Australia */
	"bst",		DTZ,	PACK(60),	/* British Summer Time */
	"cadt",		DTZ,	PACK(630),	/* Central Australian DT */
	"cast",		TZ,	PACK(570),	/* Central Australian ST */
	"cat",		TZ,	PACK(-600),	/* Central Alaska Time */
	"cct",		TZ,	PACK(480),	/* China Coast */
	"cdt",		DTZ,	PACK(-300),	/* Central DT */
	"cest",		DTZ,	PACK(120),	/* Central Europe Summer Time */
	"cet",		TZ,	PACK(60),	/* Central European Time */
	"cetdst",	DTZ,	PACK(120),	/* Central European DT */
	"cst",		TZ,	PACK(-360),	/* Central ST */
	"dec",		MONTH,	12,
	"decemb",	MONTH,	12,
	"dnt",		TZ,	PACK(60),	/* Dansk Normal Tid */
/*XX*/	"dst",		IGNORE,	0,
	"eadt",		DTZ,	PACK(660),	/* East Australian DT */
	"east",		TZ,	PACK(600),	/* East Australian ST */
	"edt",		DTZ,	PACK(-240),	/* Eastern DT */
	"eest",		DTZ,	PACK(180),	/* Eastern Europe Summer */
	"eet",		TZ,	PACK(120),	/* Eastern Europe */
	"eetdst",	DTZ,	PACK(180),	/* Eastern Europe */
	"est",		TZ,	PACK(-300),	/* Eastern ST */
	"feb",		MONTH,	2,
	"februa",	MONTH,	2,
	"fri",		IGNORE,	5,
	"friday",	IGNORE,	5,
	"fst",		DTZ,	PACK(120),	/* French Summer Time */
	"fwt",		TZ,	PACK(60),	/* French Winter Time  */
	"gmt",		TZ,	PACK(0),	/* Greenwich Mean Time */
	"gst",		TZ,	PACK(600),	/* Guam ST */
	"hadt",		DTZ,	PACK(-540),	/* Hawaii-Aleutian DT */
	"hast",		TZ,	PACK(-600),	/* Hawaii-Aleutian ST */
	"hkt",		TZ,	PACK(480),	/* Hong Kong Time */
	"hst",		TZ,	PACK(-600),	/* Hawaii ST */
	"idle",		TZ,	PACK(720),	/* Intl. Date Line, East */
	"idlw",		TZ,	PACK(-720),	/* Intl. Date Line, West */
	"idt",		DTZ,	PACK(180),	/* Israel DT */
	"ist",		TZ,	PACK(120),	/* Israel */
	"jan",		MONTH,	1,
	"januar",	MONTH,	1,
	"jst",		TZ,	PACK(540),	/* Japan ST */
	"jul",		MONTH,	7,
	"july",		MONTH,	7,
	"jun",		MONTH,	6,
	"june",		MONTH,	6,
	"kdt",		DTZ,	PACK(600),	/* Korea DT */
	"kst",		TZ,	PACK(540),	/* Korea ST */
/*XX*/	"ligt",		TZ,	PACK(600),	/* From Melbourne, Australia */
	"mar",		MONTH,	3,
	"march",	MONTH,	3,
	"may",		MONTH,	5,
	"mdt",		DTZ,	PACK(-360),	/* Mountain DT */
	"mest",		DTZ,	PACK(120),	/* Middle Europe Summer Time */
	"mesz",		DTZ,	PACK(120),	/* Mittel-Europaeische Sommerzeit */
	"met",		TZ,	PACK(60),	/* Middle Europe Time */
	"metdst",	DTZ,	PACK(120),	/* Middle Europe DT */
	"mewt",		TZ,	PACK(60),	/* Middle Europe Winter Time */
	"mez",		TZ,	PACK(60),	/* Mittel-Europaeische Zeit */
	"mon",		IGNORE,	1,
	"monday",	IGNORE,	1,
	"mst",		TZ,	PACK(-420),	/* Mountain ST */
	"ndt",		DTZ,	PACK(-150),	/* Nfld. DT */
/*XXN*/	"nft",		TZ,	PACK(-210),	/* Newfoundland ST */
/*XX*/	"nor",		TZ,	PACK(60),	/* Norway ST */
	"nov",		MONTH,	11,
	"novemb",	MONTH,	11,
	"nst",		TZ,	PACK(-210),	/* Nfld. ST */
	"nzdt",		DTZ,	PACK(780),	/* New Zealand DT */
	"nzst",		TZ,	PACK(720),	/* New Zealand ST */
	"nzt",		TZ,	PACK(720),	/* New Zealand Time */
	"oct",		MONTH,	10,
	"octobe",	MONTH,	10,
	"on",		IGNORE,	0,		/* "on" (throwaway) */
	"pdt",		DTZ,	PACK(-420),	/* Pacific DT */
	"pm",		AMPM,	PM,
	"pst",		TZ,	PACK(-480),	/* Pacific ST */
	"sadt",		DTZ,	PACK(630),	/* S. Australian DT */
	"sast",		TZ,	PACK(570),	/* South Australian ST */
	"sat",		IGNORE,	6,
	"saturd",	IGNORE,	6,
	"sep",		MONTH,	9,
	"sept",		MONTH,	9,
	"septem",	MONTH,	9,
	"sst",		DTZ,	PACK(120),	/* Swedish Summer Time */
	"sun",		IGNORE,	0,
	"sunday",	IGNORE,	0,
	"swt",		TZ,	PACK(60),	/* Swedish Winter Time  */
	"thu",		IGNORE,	4,
	"thur",		IGNORE,	4,
	"thurs",	IGNORE,	4,
	"thursd",	IGNORE,	4,
	"tue",		IGNORE,	2,
	"tues",		IGNORE,	2,
	"tuesda",	IGNORE,	2,
	"ut",		TZ,	PACK(0),
	"utc",		TZ,	PACK(0),
	"wast",		TZ,	PACK(480),	/* West Australian ST */
	"wat",		TZ,	PACK(-60),	/* West Africa Time */
	"wed",		IGNORE,	3,
	"wednes",	IGNORE,	3,
	"weds",		IGNORE,	3,
	"west",		DTZ,	PACK(60),	/* Western Europe Summer */
	"wet",		TZ,	PACK(0),	/* Western Europe */
	"wetdst",	DTZ,	PACK(60),	/* Western Europe */
	"wst",		TZ,	PACK(480),	/* West Australian ST */
	"ydt",		DTZ,	PACK(-480),	/* Yukon DT */
	"yst",		TZ,	PACK(-540),	/* Yukon ST */
	"zp4",		TZ,	PACK(-240),	/* GMT +4  hours. */
	"zp5",		TZ,	PACK(-300),	/* GMT +5  hours. */
	"zp6",		TZ,	PACK(-360),	/* GMT +6  hours. */
};

#if	0
/*
 * these time zones are orphans, i.e. the name is also used by a more
 * likely-to-appear time zone
 */
	"adt",		DTZ,	PACK(0),	/* Azores DT */
	"adt",		DTZ,	PACK(-240),	/* Acre DT */
	"ast",		TZ,	PACK(-60),	/* Azores ST */
	"ast",		TZ,	PACK(-300),	/* Acre ST */
	"bst",		TZ,	PACK(-180),	/* Brazil ST */
	"cdt",		DTZ,	PACK(-180),	/* Chile DT */
	"cdt",		DTZ,	PACK(-240),	/* Cuba DT */
	"cdt",		DTZ,	PACK(540),	/* China DT */
	"cst",		TZ,	PACK(-240),	/* Chile ST */
	"cst",		TZ,	PACK(-300),	/* Cuba ST */
	"cst",		TZ,	PACK(480),	/* China ST */
	"edt",		DTZ,	PACK(-300),	/* Easter Island DT */
	"edt",		DTZ,	PACK(-120),	/* East Brazil DT */
	"edt",		DTZ,	PACK(660),	/* Australian Eastern DT */
	"est",		TZ,	PACK(-360),	/* Easter Island ST */
	"est",		TZ,	PACK(-180),	/* East Brazil ST */
	"est",		TZ,	PACK(600),	/* Australian Eastern ST */
	"fdt",		DTZ,	PACK(-60),	/* Fernando de Noronha DT */
	"fst",		TZ,	PACK(-120),	/* Fernando de Noronha ST */
	"ist",		TZ,	PACK(330),	/* Indian ST */
	"sst",		TZ,	PACK(-660),	/* Samoa ST */
	"sst",		TZ,	PACK(480),	/* Singapore ST */
	"wdt",		DTZ,	PACK(-180),	/* Western Brazil DT */
	"wet",		TZ,	PACK(60),	/* Western European Time */
	"wst",		TZ,	PACK(-240),	/* Western Brazil ST */
/* military timezones are deprecated by RFC 1123 section 5.2.14 */
	"a",		TZ,	PACK(60),	/* UTC+1h */
	"b",		TZ,	PACK(120),	/* UTC+2h */
	"c",		TZ,	PACK(180),	/* UTC+3h */
	"d",		TZ,	PACK(240),	/* UTC+4h */
	"e",		TZ,	PACK(300),	/* UTC+5h */
	"f",		TZ,	PACK(360),	/* UTC+6h */
	"g",		TZ,	PACK(420),	/* UTC+7h */
	"h",		TZ,	PACK(480),	/* UTC+8h */
	"i",		TZ,	PACK(540),	/* UTC+9h */
	"k",		TZ,	PACK(600),	/* UTC+10h */
	"l",		TZ,	PACK(660),	/* UTC+11h */
	"m",		TZ,	PACK(720),	/* UTC+12h */
	"n",		TZ,	PACK(-60),	/* UTC-1h */
	"o",		TZ,	PACK(-120),	/* UTC-2h */
	"p",		TZ,	PACK(-180),	/* UTC-3h */
	"q",		TZ,	PACK(-240),	/* UTC-4h */
	"r",		TZ,	PACK(-300),	/* UTC-5h */
	"s",		TZ,	PACK(-360),	/* UTC-6h */
	"t",		TZ,	PACK(-420),	/* UTC-7h */
	"u",		TZ,	PACK(-480),	/* UTC-8h */
	"v",		TZ,	PACK(-540),	/* UTC-9h */
	"w",		TZ,	PACK(-600),	/* UTC-10h */
	"x",		TZ,	PACK(-660),	/* UTC-11h */
	"y",		TZ,	PACK(-720),	/* UTC-12h */
	"z",		TZ,	PACK(0),	/* UTC */
#endif

static unsigned int szdatetktbl = sizeof datetktbl / sizeof datetktbl[0];

datetkn *
datetoktype(s, bigvalp)
char *s;
int *bigvalp;
{
	register char *cp = s;
	register char c = *cp;
	static datetkn t;
	register datetkn *tp = &t;

	if (isascii(c) && isdigit(c)) {
		register int len = strlen(cp);

		if (len > 3 && (cp[1] == ':' || cp[2] == ':'))
			tp->type = TIME;
		else {
			if (bigvalp != NULL)
				/* won't fit in tp->value */
				*bigvalp = atoi(cp);
			if (len == 4)
				tp->type = YEAR;
			else if (++dtok_numparsed == 1)
				tp->type = DAY;
			else
				tp->type = YEAR;
		}
	} else if (c == '-' || c == '+') {
		register int val = atoi(cp + 1);
		register int hr =  val / 100;
		register int min = val % 100;

		val = hr*60 + min;
		if (c == '-')
			val = -val;
		tp->type = TZ;
		TOVAL(tp, val);
	} else {
		char lowtoken[TOKMAXLEN+1];
		register char *ltp = lowtoken, *endltp = lowtoken+TOKMAXLEN;

		/* copy to lowtoken to avoid modifying s */
		while ((c = *cp++) != '\0' && ltp < endltp)
			*ltp++ = (isascii(c) && isupper(c)? tolower(c): c);
		*ltp = '\0';
		tp = datebsearch(lowtoken, datetktbl, szdatetktbl);
		if (tp == NULL) {
			tp = &t;
			tp->type = IGNORE;
		}
	}
	return tp;
}

/*
 * Binary search -- from Knuth (6.2.1) Algorithm B.  Special case like this
 * is WAY faster than the generic bsearch().
 */
datetkn *
datebsearch(key, base, nel)
register char *key;
register datetkn *base;
unsigned int nel;
{
	register datetkn *last = base + nel - 1, *position;
	register int result;

	while (last >= base) {
		position = base + ((last - base) >> 1);
		result = key[0] - position->token[0];
		if (result == 0) {
			result = strncmp(key, position->token, TOKMAXLEN);
			if (result == 0)
				return position;
		}
		if (result < 0)
			last = position - 1;
		else
			base = position + 1;
	}
	return 0;
}
