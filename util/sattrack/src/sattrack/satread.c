/******************************************************************************/
/*                                                                            */
/*  Title       : satread.c                                                   */
/*  Author      : Manfred Bester, DL5KR                                       */
/*  Date        : 03Mar92                                                     */
/*  Last change : 26Jan93                                                     */
/*                                                                            */
/*  Synopsis    : Routines for reading data from resource files and other     */
/*                I/O functions.                                              */
/*                                                                            */
/*  Resources   : sites.dat, defaults.dat, modes.dat, tle.dat                 */
/*                                                                            */
/*  sites.dat:    contains information on various gorund stations             */
/*                column  1-24:  ground station name                          */
/*                       26-35:  latitude  [deg] (north = "+")                */
/*                       37-47:  longitude [deg] (west  = "+")                */
/*                       49-55:  altitude  [m]                                */
/*                                                                            */
/*  defaults.dat: contains default parameters                                 */
/*                                                                            */
/*  modes.dat:    contains mode information for amateur satellites            */
/*                                                                            */
/*  tle.dat:      contains NORAD two-line orbital element sets                */
/*                (also sts-55.dat, tvsat.dat, etc.)                          */
/*                                                                            */
/******************************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include "satglobalsx.h"
#include "satglobals.h"
#include "sattrack.h"
#include "vt100.h"

extern modeRec modes[MAXMODES];

/******************************************************************************/
/*                                                                            */
/* clipString: clips N characters plus leading blanks off the beginning of a  */
/*             character string                                               */
/*                                                                            */
/******************************************************************************/

void clipString(charStr,N)

int  N;
char *charStr;

{
    int  i, j, firstChar;
    char tmpStr[100];

    firstChar = FALSE;
    j = 0;

    for (i = 0; i < strlen(charStr)-N; i++) 
    {

       if (charStr[i+N] != ' ' || (charStr[i+N] == ' ' && firstChar))
       {
           tmpStr[j] = charStr[i+N];
           j++;
           firstChar = TRUE;
       }
    }

    tmpStr[j] = '\0';                            /* add termination character */
    sprintf(charStr,"%s",tmpStr);
}

/******************************************************************************/
/*                                                                            */
/* getTimeZone: gets time zone [h] from the time zone string information      */
/*                                                                            */
/******************************************************************************/

void getTimeZone()

{
    int i;

    timeZone = 0.0;

    for (i = 0; i < timeZones; i++)
    {
        if (!strcmp(timeZoneList[i],timeZoneStr))
            timeZone = timeZoneHour[i] / 24.0;          /* gets time zone [d] */
    }

    if (timeZone == 0.0)        /* set to UTC if time zone is specified wrong */
        sprintf(timeZoneStr,"UTC");
}

/******************************************************************************/
/*                                                                            */
/* getDefaultData: reads default parameters from 'defaults.dat'               */
/*                                                                            */
/******************************************************************************/

void getDefaultData()

{
    char defaultLine[120], defaultDuration[60], defaultMinElev[60];
    FILE *defaultFile;

    sprintf(defaultsDat,"%s/SatTrack/Data/%s",strp,DEFAULTS);

    if ((defaultFile = fopen(defaultsDat,"r")) == 0)
    {
        printf("cannot open input file '%s'\n",defaultsDat);
        exit(-1);
    }

    sprintf(defaultSite,"Zero Lat Long");
    sprintf(defaultSat,"Mir");
    sprintf(defaultSet,"000");
    sprintf(defaultTimeZoneStr,"UTC");
    sprintf(defaultDuration,"1.0");
    sprintf(defaultMinElev,"0.0");

    while (TRUE)
    {
        fgets(defaultLine,100,defaultFile);
        truncBlanks(defaultLine);

        if (!strncmp(defaultLine,"Site:",5))
        {
           clipString(defaultLine,5);
           sprintf(defaultSite,"%s",defaultLine);
        }

        if (!strncmp(defaultLine,"Satellite:",10))
        {
           clipString(defaultLine,10);
           sprintf(defaultSat,"%s",defaultLine);
        }

        sscanf(defaultLine,"Element set: %s",defaultSet);
        sscanf(defaultLine,"Element set type: %s",defaultType);
        sscanf(defaultLine,"Time zone: %s",defaultTimeZoneStr);
        sscanf(defaultLine,"Duration: %s",defaultDuration);
        sscanf(defaultLine,"Min elevation: %s",defaultMinElev);

        if (!strlen(defaultLine))
            break;                                             /* EOF */

        if (strlen(defaultLine) <= 2)
            break;                                             /* blank line */
    }

    strcpy(timeZoneStr,defaultTimeZoneStr);
    getTimeZone();
    defDuration     = atof(defaultDuration);
    defMinElevation = atof(defaultMinElev);

    fclose(defaultFile);

    if (verbose)
    {
        nl();
        printf("Default parameters:\n\n");
        printf("Ground station    :%17s\n",defaultSite);
        printf("Satellite         :%17s\n",defaultSat);
        printf("Element set       :%17s\n",defaultSet);
        printf("Element set type  :%17s\n",defaultType);
        printf("Time zone         :%17s (%+.0f)\n",timeZoneStr,timeZone*24.0);
        printf("Duration          :%17.1f d\n",defDuration);
        printf("Min elevation     :%17.1f deg\n",defMinElevation);
        nl();
    }
}

/******************************************************************************/
/*                                                                            */
/* getSatFileName: gets reduced satellite name for creating output file, i.e. */
/*                 replace blanks in file name with "_", delete "(" and ")"   */
/*                                                                            */
/******************************************************************************/

void getSatFileName()

{
    int i, j;

    i = 0; j = 0;

    do
    {
        if (satName[i] != ' ' && satName[i] != '(' && 
            satName[i] != ')' && satName[i] != '"')
        {
            satFileName[j] = satName[i];
            i++; j++;
        }

        if (satName[i] == ' ')
        {
            satFileName[j] = '_';
            i++; j++;
        }

        if (satName[i] == '(' || satName[i] == ')' || satName[i] == '"')
        {
            i++;
        }
    }
    while (satName[i] != '\0');

    satFileName[i] = '\0';
}

/******************************************************************************/
/*                                                                            */
/* getSatParams: reads satellite parameters from data file with the two-line  */
/*               orbital element sets                                         */
/*                                                                            */
/******************************************************************************/

void getSatParams()

{
    double epochDayTime, epochH, epochM, epochS, epochSec;
    int    epochYear, epochMonth, epochMonthDay, epochD, epochHour, epochMin;
    int    error, error1;
    char   stsName[20];

    error = TRUE;

    while (error)
    {
        printf("Satellite name      <%15s> : ",defaultSat);
        gets(satName);

        if (!strlen(satName))
            sprintf(satName,"%s",defaultSat);

        strcpy(stsName,satName);
        upperCase(stsName);
        stsFlag = strncmp(stsName,"STS",3) ? FALSE : TRUE;
        sprintf(defaultNORAD,"%s",(stsFlag) ? satName : defaultSet);

        printf("Two-line elements   <%15s> : ",defaultNORAD);
        gets(elementFile);

        if (!strlen(elementFile))
            sprintf(elementFile,"%s",defaultNORAD);

        if (stsFlag)
        {
            error1 = TRUE;

            while (error1)
            {
                printf("Element set type              <%5s> : ",defaultType);
                gets(elementType);

                if (!strlen(elementType))
                    sprintf(elementType,"%s",defaultType);
                else
                    upperCase(elementType);

                error1 = (!strcmp(elementType,"NASA") || 
                          !strcmp(elementType,"NORAD")) ? FALSE : TRUE;
            }
        }

        error = readnorad(strp,elementFile,satName,verbose,
                   &satNum,&elementSet,&epochDay,&inclination,&epochRAAN,
                   &eccentricity,&epochArgPerigee,&epochMeanAnomaly,
                   &epochMeanMotion,&decayRate,&epochOrbitNum);

        getSatFileName();                               /* remove blanks etc. */

        if (error) nl();
    }

    elsetEpochDay     = epochDay;
    epochYear         = (int) (epochDay  / 1000.0);
    epochDay         -= epochYear * 1000.0;
    elsetEpoch        = epochDay + getDayNum(epochYear,1,0);
    epochD            = (int) epochDay;
    epochDayTime      = epochDay - (double) epochD;
    epochDay         += getDayNum(epochYear,1,0);

    epochH            = 24.0 * epochDayTime;
    epochM            = 60.0 * modf(epochH,&epochH);
    epochS            = 60.0 * modf(epochM,&epochM);

    if (fabs(epochS - 60.0) < 5.0e-4)
    {
        epochS  = 0.0;
        epochM += 1.0;

        if (fabs(epochM - 60.0) < 1.0e-4)
        {
            epochM  = 0.0;
            epochH += 1.0;

            if (fabs(epochH - 24.0) < 1.0e-4)
            {
                epochH  = 0.0;
                epochD += 1;
            }
        }
    }

    epochHour = (int) epochH;
    epochMin  = (int) epochM;
    epochSec  = epochS;

    calendar(epochYear,epochD,&epochMonth,&epochMonthDay);

    sprintf(epochString,"%02d-%02d-%02d  %02d:%02d:%06.3f UTC",
            epochMonthDay,epochMonth,epochYear,epochHour,epochMin,epochSec);

    inclination      *= CDR;
    epochRAAN        *= CDR;
    epochArgPerigee  *= CDR;
    epochMeanAnomaly *= CDR;

    if (realTime-elsetEpoch > 0.0)
        sprintf(updateString,"%.1f days ago",realTime-elsetEpoch);
    else
        sprintf(updateString,"%.1f days ahead",elsetEpoch-realTime);

    if (verbose)
    {
        printf("Satellite name    :%17s\n",satName);
        printf("Satellite number  :%17ld\n",satNum);
        printf("Element set       :%17ld\n",elementSet);
        printf("Epoch             :%17.9lf d        ",elsetEpochDay);
        printf("%s\n",epochString);
        printf("Mean anomaly      :%17.9lf deg\n",epochMeanAnomaly * CRD);
        printf("Arg of perigee    :%17.9lf deg\n",epochArgPerigee * CRD);
        printf("RAAN              :%17.9lf deg\n",epochRAAN * CRD);
        printf("Inclination       :%17.9lf deg\n",inclination * CRD);
        printf("Eccentricity      :%17.9lf\n",eccentricity);
        printf("Mean motion       :%17.9lf rev/d\n",epochMeanMotion);
        printf("Decay rate        :%17.9lf rev/d/d\n",decayRate);
        printf("Orbit             :%17ld\n",epochOrbitNum);
        printf("\n");
    }

    else
    {
        printf("Last update for %-21s : ",satName);
        printf("%s (#%d)\n\n",updateString,elementSet);
    }
}

/******************************************************************************/
/*                                                                            */
/* getSatModes: reads modes of the specified satellite                        */
/*                                                                            */
/******************************************************************************/

void getSatModes()

{
    int  foundMode;
    char modeLine[120];
    FILE *modeFile;

    beaconFreq   = BEACON;
    attLong      = 0.0;
    attLat       = 0.0;
    perigeePhase = 0.0;
    maxPhase     = MAXPHASE;
    numModes     = 0;
    launchFlag   = FALSE;

    sprintf(satAlias,"");
    sprintf(modesDat,"%s/%s/%s",strp,DATA,MODES);

    if ((modeFile = fopen(modesDat,"r")) != 0)
    {
        foundMode  = FALSE;

        while (!foundMode)
        {
            fgets(modeLine,100,modeFile);

            if (!strlen(modeLine))
                break;                                                 /* EOF */

            truncBlanks(modeLine);
            sprintf(string,"Satellite: %s",satName);

            if (!strncmp(modeLine,string,strlen(string)))
                foundMode = TRUE;
        }

        if (foundMode)
        {
            while (TRUE)
            {
                fgets(modeLine,100,modeFile);

                if (!strlen(modeLine))
                    break;                                             /* EOF */

                if (strlen(modeLine) <= 2)
                    break;                                      /* blank line */

                sscanf(modeLine,"Alias: %s",satAlias);
                sscanf(modeLine,"Beacon: %lf",&beaconFreq);

                launchFlag = (sscanf(modeLine,"Launch: %d-%d-%d %d:%d:%d",
                       &launchDay,&launchMonth,&launchYear,
                       &launchHour,&launchMin,&launchSec)) ? TRUE : FALSE;

                sscanf(modeLine,"Perigee phase: %lf",&perigeePhase);
                sscanf(modeLine,"Attitude: %lf %lf",&attLong,&attLat);
                sscanf(modeLine,"Max phase: %lf",&maxPhase);

                if (sscanf(modeLine,"Mode: %20s %d - %d",
                    modes[numModes].modeStr,&modes[numModes].minPhase,
                    &modes[numModes].maxPhase) == 3 && numModes < MAXMODES)
                    numModes++;
            }
        }

        fclose(modeFile);

        if (launchFlag)
            sprintf(launchString,"%02d-%02d-%02d  %02d:%02d:%02d.000 UTC",
                launchDay,launchMonth,launchYear,
                launchHour,launchMin,launchSec);
    }
}

/******************************************************************************/
/*                                                                            */
/* getSiteParams: reads site parameters from input file 'sites.dat'           */
/*                                                                            */
/******************************************************************************/
 
void getSiteParams()

{
    int  i, siteNameLen, foundSite;
    char siteLine[120];
    FILE *siteFile;

    foundSite = FALSE;

    while (!foundSite)
    { 
        nl();
        printf("Ground station      <%15s> : ",defaultSite);
        gets(siteName);
 
        if (!strlen(siteName))
            sprintf(siteName,"%s",defaultSite);

        sprintf(sitesDat,"%s/%s/%s",strp,DATA,SITES);

        if ((siteFile = fopen(sitesDat,"r")) == 0)
        {
            printf("cannot open input file '%s'\n",sitesDat);
            exit(-1);
        }
 
        siteNameLen = strlen(siteName);
        upperCase(siteName);

        while (fgets(siteLine,100,siteFile) && !foundSite)
        {
            strncpy(string,siteLine,siteNameLen);
            upperCase(string);

            if (strncmp(string,siteName,siteNameLen) == 0)
            {
                foundSite = TRUE;
                strncpy(siteName,siteLine,16);
                truncBlanks(siteName);

                siteLat  = getElement(siteLine,26,35) * CDR;
                siteLong = getElement(siteLine,37,47) * CDR;
                siteAlt  = getElement(siteLine,49,55) * CMKM;

                if (verbose)
                {
                    nl();
                    printf("Ground station    :%17s\n",siteName);
                    printf("Latitude          :%17.6f deg N\n",siteLat * CRD);
                    printf("Longitude         :%17.6f deg W\n",siteLong * CRD);
                    printf("Altitude          :%17.6f m\n",siteAlt / CMKM);
                    nl();
                }

                else
                {
                    if (strcmp(siteName,defaultSite))
                        printf("%s\n",siteName);
                }
            }
        }

        fclose(siteFile);

        if (!foundSite)
        {
            reverseblink();
            printf(" no matching ground station name in 'sites.dat' \n");
            normal(); alarm();
        }
    }
}
 
/******************************************************************************/
/*                                                                            */
/* getTimeParams: reads additional parameters for orbit calculations          */
/*                                                                            */
/******************************************************************************/

void getTimeParams()

{
    double hour;
    int    sysYear, sysMonth, sysDay, sysYearDay, sysHour, sysMin, sysSec;
    char   dummys[40], dispStr[10];
 
    getUnixTime(&sysDay,&sysMonth,&sysYear,&sysYearDay,
                &sysHour,&sysMin,&sysSec);

    printf("Start time %02d-%02d-%02d  %02d:%02d:%02d UTC <Y> ? ",
            sysDay,sysMonth,sysYear,sysHour,sysMin,sysSec);

    gets(dispStr);
    upperCase(dispStr);

    if (strlen(dispStr) && strncmp(dispStr,YES,1))
    {
        printf("Start date (UTC)           [dd mm yy] : ");
        scanf("%d %d %d",&sysDay,&sysMonth,&sysYear);
        printf("Start time (UTC)           [hh mm ss] : ");
        scanf("%d %d %d",&sysHour,&sysMin,&sysSec);

        gets(dummys);                                         /* empty buffer */
    }

    defStepTime = 1.0 / epochMeanMotion * SPD / 150.0;
    defStepTime = (double) (((int) (defStepTime / 10.0)) * 5.0);

    if (!shortPredFlag)
    {
        printf("Time step [s]                  <%4.0f> : ",defStepTime);
        gets(dispStr);
        stepTime = (!strlen(dispStr)) ? defStepTime : atof(dispStr);
    }

    printf("Duration [d]                   <%4.1f> : ",defDuration);
    gets(dispStr);
    duration   = (!strlen(dispStr)) ? defDuration : atof(dispStr);

    startTime  = (double) getDayNum(sysYear,sysMonth,sysDay);          /* [d] */
    hour       = (double) (sysHour + sysMin/60.0 + sysSec/3600.0);     /* [h] */
    startTime += hour / 24.0;
    stepTime  /= SPD;                                                  /* [d] */
    stopTime   = startTime + duration;                                 /* [d] */

    if (!shortPredFlag)
    {
        printf("Minimum elevation [deg]        <%4.1f> : ",defMinElevation);
        gets(dispStr);
        minElevation  = (!strlen(dispStr)) ? defMinElevation : atof(dispStr);
        minElevation *= CDR;
    }
}
 
/******************************************************************************/
/*                                                                            */
/* printMode: prints phase-dependent mode of satellite into output file       */
/*                                                                            */
/******************************************************************************/

void printMode( outFile, phase)

{
    int curMode;
 
    for (curMode = 0; curMode < numModes; curMode++)
    {
        if ((phase >= modes[curMode].minPhase
            && phase < modes[curMode].maxPhase)
            || ((modes[curMode].minPhase > modes[curMode].maxPhase)
            && (phase >= modes[curMode].minPhase
            || phase < modes[curMode].maxPhase)))
        {
            fprintf((FILE *) outFile," %2s",modes[curMode].modeStr);
        }
    }

    if (numModes == 0)
        fprintf((FILE *) outFile,"   ");
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
/* End of function block satread.c                                            */
/*                                                                            */
/******************************************************************************/
