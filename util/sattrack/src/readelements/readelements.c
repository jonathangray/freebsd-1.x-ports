/******************************************************************************/
/*                                                                            */
/*  Title       : readelements.c                                              */
/*  Author      : Manfred Bester, DL5KR                                       */
/*  Date        : 04Dec90                                                     */
/*  Last change : 26Dec92                                                     */
/*                                                                            */
/*  Synopsis    : This program reads the two-line prediction bulletins and    */
/*                displays the results in plain language.                     */
/*                                                                            */
/******************************************************************************/

#include <math.h>
#include <stdio.h>
#include <string.h>

#include "vt100.h"

#define  TRUE  -1

/******************************************************************************/
/*                                                                            */
/*  main program starts here                                                  */
/*                                                                            */
/******************************************************************************/

extern int  readnorad();
extern char *monthname(); 
extern void calendar(), lowerCase(), upperCase(), trunkBlanks();

main()

{
    double epochDay, inclination, RAAN, eccentricity, argPerigee, meanAnomaly;
    double meanMotion, decayRate;
    double dayTime, sec;

    long   satNum, elementSet, orbitNum;
    int    error, verbose, year, fullYear, yearDay, month, day, hour, min;

    char   satName[20], elementFile[80], utcDate[20];
    char   *strp, *getenv();

/******************************************************************************/
/*                                                                            */
/*  enter satellite name                                                      */
/*                                                                            */
/******************************************************************************/

    strp = getenv("HOME");

    while (TRUE)
    {
        printf("enter name of satellite  : ");
        gets(satName);

        printf("enter NORAD element set  : ");
        gets(elementFile);

/******************************************************************************/
/*                                                                            */
/*  call function readnorad()                                                 */
/*                                                                            */
/******************************************************************************/

        verbose = TRUE;

        error = readnorad(strp,elementFile,satName,verbose,
                     &satNum,&elementSet,&epochDay,&inclination,
                     &RAAN,&eccentricity,&argPerigee,&meanAnomaly,&meanMotion,
                     &decayRate,&orbitNum);

/******************************************************************************/
/*                                                                            */
/*  convert time format                                                       */
/*                                                                            */
/******************************************************************************/

        if (!error)
        {
            year     = (int) (epochDay / 1000.0);
            yearDay  = (int) (epochDay - year * 1000);
            dayTime  = epochDay - (double) (year * 1000 + yearDay);
            fullYear = (year < 50) ? 2000 : 1900 + year;
    
            hour     = (int) (dayTime * 24.0);
            min      = (int) ((dayTime * 24.0 - hour) * 60.0);
            sec      = ((dayTime * 24.0 - hour) * 60.0 - min) * 60.0;

            calendar(fullYear,yearDay,&month,&day);
            sprintf(utcDate,"%02d%3s%02d",day,monthname(month),year);

/******************************************************************************/
/*                                                                            */
/*  display orbital elements                                                  */
/*                                                                            */
/******************************************************************************/

            printf("\n");
            printf("Satellite name       : %14s\n",satName);
            printf("Satellite number     : %14ld\n",satNum);
            printf("Element set          : %14ld\n",elementSet);
            printf("Epoch time           : %14.8f ",epochDay);
            printf("      %s  %02d:%02d:%06.3f UTC\n",utcDate,hour,min,sec);
            printf("Inclination          : %14.8f deg\n",inclination);
            printf("RA of ascending node : %14.8f deg\n",RAAN);
            printf("Eccentricity         : %14.8f\n",eccentricity);
            printf("Argument of perigee  : %14.8f deg\n",argPerigee);
            printf("Mean anomaly         : %14.8f deg\n",meanAnomaly);
            printf("Mean motion          : %14.8f rev/d\n",meanMotion);
            printf("Decay rate           : %14.8f rev/d^2\n",decayRate);
            printf("Orbit                : %14ld\n",orbitNum);
        }

        nl();
    }
}

/******************************************************************************/
/*                                                                            */
/* upperCase: changes lower to upper case letters                             */
/*                                                                            */
/******************************************************************************/

void upperCase(string)

char *string;

{
    int i;

    for (i = 0; i < strlen(string); i++)
    {
        if (string[i] >= 'a' && string[i] <= 'z')
            string[i] -= 'a' - 'A';
    }
}

/******************************************************************************/
/*                                                                            */
/* lowerCase: changes upper to lower case letters                             */
/*                                                                            */
/******************************************************************************/

void lowerCase(string)

char *string;

{
    int i;

    for (i = 0; i < strlen(string); i++)
    {
        if (string[i] >= 'A' && string[i] <= 'Z')
            string[i] -= 'A' - 'a';
    }
}

/******************************************************************************/
/*                                                                            */
/* truncBlanks: truncates trailing blanks from character string               */
/*                                                                            */
/******************************************************************************/

void truncBlanks(string)

char *string;

{
    int i;

    i = strlen(string) - 1;

    do
    {
        i--;
    }
    while (string[i] == ' ');

    string[i+1] = '\0';                          /* add termination character */
}

/******************************************************************************/
/*                                                                            */
/*  end of program readelements.c                                             */
/*                                                                            */
/******************************************************************************/
