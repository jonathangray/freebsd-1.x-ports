/******************************************************************************/
/*                                                                            */
/*  Title       : getUnixTime.c                                               */
/*  Author      : Manfred Bester, DL5KR                                       */
/*  Date        : 04Dec91                                                     */
/*  Last change : 15Mar92                                                     */
/*                                                                            */
/*  Synopsis    : This function gets the Unix system time (UTC).              */
/*                                                                            */
/******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <sys/types.h>

void getUnixTime(day,month,year,yday,hour,min,sec)

int *day, *month, *year, *yday, *hour, *min, *sec;

{
    char   timeString[80];

    time_t timeofday;
    struct tm *tm;

    time(&timeofday);
    tm = gmtime(&timeofday);
    strncpy(timeString,asctime(tm),16);
    timeString[16] = '\0';

    *day   = tm->tm_mday;
    *month = tm->tm_mon + 1;
    *year  = tm->tm_year;
    *yday  = tm->tm_yday + 1;
    *hour  = tm->tm_hour;
    *min   = tm->tm_min;
    *sec   = tm->tm_sec;

    return;
}

/******************************************************************************/
/*                                                                            */
/* end of function getUnixTime.c                                              */
/*                                                                            */
/******************************************************************************/
