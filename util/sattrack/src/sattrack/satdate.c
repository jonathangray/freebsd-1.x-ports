/******************************************************************************/
/*                                                                            */
/*  Title       : satdate.c                                                   */
/*  Author      : Manfred Bester, DL5KR                                       */
/*  Date        : 03Mar92                                                     */
/*  Last change : 29Dec92                                                     */
/*                                                                            */
/*  Synopsis    : Auxiliary routines for the satellite tracking program       */
/*                'sattrack'.                                                 */
/*                                                                            */
/******************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "satglobalsx.h"
#include "sattrack.h"
 
/******************************************************************************/
/*                                                                            */
/* getDayNum: gets the day number for a given date. Jan. 1 of the reference   */
/*            year is day 0. Note that the day number may be negative, if the */
/*            sidereal reference is in the future.                            */
/*                                                                            */
/*            January 1, 1900 is day 0 valid from 1900 through 2099           */
/*                                                                            */
/******************************************************************************/

int  monthDays[] = { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 };

long getDayNum(int year,int month,int day)

{
    long result;
    
    if (year < 50)                 /* allows 4 or 2 digit year specifications */
        year += 2000;
    else
        if (year < 100)
            year += 1900;

    result = ((((long) year - 1901) * 1461) >> 2) 
             + monthDays[month-1] + day + 365;

    if (year%4 == 0 && month > 2)         /* correct day number for leap year */
        result++;

    return(result);
}

/******************************************************************************/
/*                                                                            */
/* getDate: gets the date from a given day number (see getDayNum)             */
/*                                                                            */
/******************************************************************************/

void getDate(dayNum,year,month,day,yearDay)

long dayNum;
int  *year, *month, *day, *yearDay;

{
    long Y;
    int  M, L;

    Y         = 4 * dayNum;
    Y        /= 1461;
    dayNum   -= 365 + (((Y - 1) * 1461) >> 2);
    *yearDay  = dayNum;
    
    L = 0;

    if (Y%4 == 0 && dayNum > monthDays[2])
        L = 1;

    M = 1;
     
    while (dayNum > monthDays[M] + L)
        M++;

    dayNum -= (monthDays[M-1]);

    if (M > 2)
        dayNum -= L;
   
    *year  = Y + 1900;
    *month = M;
    *day   = dayNum;
}    

/******************************************************************************/
/*                                                                            */
/* printDate: prints date into output file                                    */
/*                                                                            */
/******************************************************************************/

void printDate(outFile,time)

double time;
FILE   *outFile;

{
    long dayNum;
    int  day, month, year, yearDay;

    dayNum = (long) time;

    getDate(dayNum,&year,&month,&day,&yearDay);

    if (shortPredFlag && lastDayNum != dayNum)
        fprintf(outFile,"\n");

    if (shortPredFlag && !firstLine && lastDayNum == dayNum)
        fprintf(outFile,"            ");
    else
    {
        fprintf(outFile,"%s ",dayNames[dayNum%7]);
        fprintf(outFile,"%02d-%02d-%02d",day,month,year-1900);
    }

    lastDayNum = dayNum;
}    

/******************************************************************************/
/*                                                                            */
/* printTime: prints time string                                              */
/*                                                                            */
/******************************************************************************/

void printTime(outFile,time)

double time;
FILE   *outFile;

{
    double dayTime;
    long   dayNum;
    int    hours, mins, secs, dummyi;

    dayNum  = (long) time;
    dayTime = time - (double) dayNum;

    convertTime(dayTime,&dummyi,&hours,&mins,&secs);

    if (time < 1000.0)                                /* for duration of pass */
        hours += (int) dayNum * 24;

    fprintf(outFile,"%02d:%02d:%02d",hours,mins,secs);
}

/******************************************************************************/
/*                                                                            */
/* printMET: prints MET string into output file                               */
/*                                                                            */
/******************************************************************************/

void printMET(outFile,met)

double met;
FILE   *outFile;

{
    double metDayTime;
    long   metDayNum;
    int    preLaunchFlag, metDay, metHour, metMin, metSec;

    metDayNum  = (long) met;
    metDayTime = met - (double) metDayNum;

    preLaunchFlag = (met < 0.0) ? TRUE : FALSE;

    if (preLaunchFlag)
        met *= -1.0;

    convertTime(met,&metDay,&metHour,&metMin,&metSec);

    if (preLaunchFlag)
    {
        if (metDay)
            fprintf(outFile,"%3d/%02d:%02d:%02d",
                             -metDay,metHour,metMin,metSec);
        else
            fprintf(outFile," -%d/%02d:%02d:%02d",
                             -metDay,metHour,metMin,metSec);
    }

    else
        fprintf(outFile,"%3d/%02d:%02d:%02d",metDay,metHour,metMin,metSec);
}

/******************************************************************************/
/*                                                                            */
/* End of function block satdate.c                                            */
/*                                                                            */
/******************************************************************************/
