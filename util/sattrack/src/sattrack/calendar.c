/******************************************************************************/
/*                                                                            */
/*  Title       : calendar.c                                                  */
/*  Author      : Manfred Bester, DL5KR                                       */
/*  Date        : 19Mar89                                                     */
/*  Last change : 05May92                                                     */
/*                                                                            */
/*  Synopsis    : This functions transforms the format of the calendar date.  */
/*                                                                            */
/*  Parameters  : Input:  1.  year            (---) : iyear                   */
/*                        2.  day in the year (---) : yearday                 */
/*                                                                            */
/*                Output: 1.  month           (---) : month                   */
/*                        2.  day             (---) : day                     */
/*                                                                            */
/******************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

void calendar(iyear,yearday,pmonth,pday)

int iyear, yearday, *pmonth, *pday;

{
    int month, day, check;

    static int noleapyear[13] = {
        0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
    };

    static int leapyear[13] = {
        0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
    };

/******************************************************************************/
/*                                                                            */
/*  check if the year "iyear" is a leap year                                  */
/*                                                                            */
/*  leap years    : 1600, 1984, 1988, 1992, 1996, 2000 etc.    (check = 0)    */
/*  no leap years : 1700, 1800, 1900, 2100 etc.                (check = 1)    */
/*                                                                            */
/******************************************************************************/

    month = 0;

    if (iyear%100 == 0 && iyear%400 != 0)
          check = 1;
    else
          check = iyear%4;

    if (check == 0)
    {
        while (yearday > 0)
        {
             month++; 
             yearday -= leapyear[month];
        }

        day = yearday + leapyear[month];
    } 

    else
    {
        while (yearday > 0)
        {
             month++;
             yearday -= noleapyear[month];
        } 

        day = yearday + noleapyear[month];
    }

/******************************************************************************/
/*                                                                            */
/*  prepare data for output from the function                                 */
/*                                                                            */
/******************************************************************************/

    *pmonth = month;
    *pday   = day;

    return;
}

/******************************************************************************/
/*                                                                            */
/*  end of function calendar.c                                                */
/*                                                                            */
/******************************************************************************/
