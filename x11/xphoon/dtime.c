#ifndef lint
static char rcsid[] =
    "@(#) $Header: /a/cvs/386BSD/ports/x11/xphoon/dtime.c,v 1.1 1994/05/18 14:07:11 asami Exp $";
#endif

/* Copyright (C) 1988, 1991 by Jef Poskanzer.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
*/

/* dtime.c - extracted from the phoon/libtws package */

#include "tws.h"
#include <stdio.h>
#include <sys/types.h>
#include <time.h>
#ifdef  SYS5
extern int daylight;
extern long timezone;
#else /*SYS5*/
#include <sys/timeb.h>
#endif /*SYS5*/

extern long time();
struct tm* localtime();

struct tws*
dtwstime()
    {
    long clock;

    (void) time( &clock );
    return dlocaltime( &clock );
    }

struct tws*
dlocaltime( clock )
    long* clock;
    {
    register struct tm* tm;
#ifndef SYS5
    struct timeb tb;
#endif not SYS5
    static struct tws tw;

    if ( clock == (long*) 0 )
	return (struct tws*) 0;
    tw.tw_flags = TW_NULL;

    tm = localtime( clock );
    tw.tw_sec = tm->tm_sec;
    tw.tw_min = tm->tm_min;
    tw.tw_hour = tm->tm_hour;
    tw.tw_mday = tm->tm_mday;
    tw.tw_mon = tm->tm_mon;
    tw.tw_year = tm->tm_year;
    tw.tw_wday = tm->tm_wday;
    tw.tw_yday = tm->tm_yday;
    if ( tm->tm_isdst )
	tw.tw_flags |= TW_DST;
#ifndef  SYS5
    ftime( &tb );
    tw.tw_zone = -tb.timezone;
#else   SYS5
    tzset();
    tw.tw_zone = -( timezone / 60 );
#endif  SYS5
    tw.tw_flags &= ~TW_SDAY;
    tw.tw_flags |= TW_SEXP;
    tw.tw_clock = *clock;

    return &tw;
    }
