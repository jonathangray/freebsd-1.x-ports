#ifndef DATETOK_H__
#define DATETOK_H__

#define AM 0
#define PM 1

/* can't have more of these than there are bits in an unsigned long */
#define MONTH	1
#define YEAR	2
#define DAY	3
#define TIME	4
#define TZ	5
#define DTZ	6
#define IGNORE	7
#define AMPM	8
/* below here are unused so far */
#define SECONDS	9
#define MONTHS	10
#define YEARS	11
#define NUMBER	12
/* these are only for relative dates */
#define BEFORE	13
#define AFTER	14
#define AGO	15


#define SECS(n)		((time_t)(n))
#define MINS(n)		((time_t)(n) * SECS(60))
#define HOURS(n)	((time_t)(n) * MINS(60))	/* 3600 secs */
#define DAYS(n)		((time_t)(n) * HOURS(24))	/* 86400 secs */
/* months and years are not constant length, must be specially dealt with */

#define TOKMAXLEN 6	/* only this many chars are stored in datetktbl */

/*
 * definitions for squeezing values into low 7 bits of "value" to avoid
 * overflow on extremely picky machines with signed chars.
 * all timezones we care about are divisible by 30, and the largest value
 * (780) when divided is 26, which fits in 5 bits (037), so have a bit to
 * spare(!).
 */
#define SIGNBIT 0100
#define VALMASK  037
#define DIVISOR 30
#define NEG(n)		((n)|SIGNBIT)
#define PACK(v)		((v) < 0? NEG((-(v))/DIVISOR): (v)/DIVISOR)
#define TOVAL(tp, v)	((tp)->value = PACK(v))
#define SIGNEDCHAR(c)	((c)&SIGNBIT? -((c)&VALMASK): (c))
#define FROMVAL(tp)	(-SIGNEDCHAR((tp)->value) * DIVISOR)	/* uncompress */

/* keep this struct small; it gets used a lot */
typedef struct {
	char token[TOKMAXLEN];
	char type;
	char value;		/* this may be unsigned, alas */
} datetkn;

extern datetkn *datetoktype(/* char *s */);
extern datetkn *datebsearch(/* char *key, datetkn *base, unsigned int nel */);

#endif /* DATETOK_H__ */ /* Do not add anything after this line */
