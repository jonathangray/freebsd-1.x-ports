/******************************************************************************/
/*                                                                            */
/*  Title       : sattrack.c                                                  */
/*  Author      : Manfred Bester, DL5KR                                       */
/*  Date        : 24Feb92                                                     */
/*  Last change : 17Feb93                                                     */
/*                                                                            */
/*  Synopsis    : Satellite orbit prediction and live tracking display        */
/*                program.                                                    */
/*                                                                            */
/*  Resources   : sites.dat, defaults.dat, modes.dat, tle.dat                 */
/*                                                                            */
/*  (c) 1992, 1993 Manfred Bester --- All Rights Reserved                     */
/*                                                                            */
/******************************************************************************/

#include <stdio.h>
#include <math.h>
#include <sys/types.h>
#include <sys/termios.h>
#include <sys/ioctl.h>

#include "satglobalsx.h"
#include "satglobals.h"
#include "sattrack.h"
#include "vt100.h"

/******************************************************************************/
/*                                                                            */
/*  global fields                                                             */
/*                                                                            */
/******************************************************************************/

char    strtName[]      = "SatTrack";
char    strtHeader[]    = "Satellite Tracking Program";
char    strtVersion[]   = "V1.3";
char    predHeader[]    = "Prediction";
char    dispHeader[]    = "TRACKING MONITOR";

char    *visibCode[]    = { "D", "P", "N", "V", " " };
char    *dayNames[]     = { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };

modeRec modes[MAXMODES];

/******************************************************************************/
/*                                                                            */
/*  time zone information                                                     */
/*                                                                            */
/*  'timeZones' must equal the number of entries in 'timeZoneList' and        */
/*  'timeZoneHour'                                                            */
/*                                                                            */
/******************************************************************************/

char    *timeZoneList[] = { "UTC",  "MEZ",  "MSZ", 
                            "EST",  "EDT",  "CST",  "CDT",  "MST",  "MDT", 
                            "PST",  "PDT",  "HST" };

double  timeZoneHour[]  = {  0.0,    1.0,    2.0, 
                            -5.0,   -4.0,   -6.0,   -5.0,   -7.0,   -6.0, 
                            -8.0,   -7.0,  -10.0  };

int     timeZones       = 12;

/******************************************************************************/
/*                                                                            */
/*  global variables                                                          */
/*                                                                            */
/******************************************************************************/

double refOrbit;                       /* orbit number at epoch               */
double curOrbit, orbitFract;           /* double current orbit number         */
double stsOrbit, stsOrbitFract;        /* current orbit number for STS        */
double curMotion;                      /* current number of rev/day           */
double meanAnomaly;                    /* decimal fraction of orbit number    */
double trueAnomaly;                    /* mean anomaly corrected for ellipse  */
double semiMajorAxis;                  /* semi major axis of satellite orbit  */
double satRadius;                      /* distance measured from geocenter    */
double satX, satY, satZ;               /* in Right Ascension based system     */
double satVX, satVY, satVZ;            /* velocity [km/s]                     */
double siteX, siteY, siteZ;            /* in Right Ascension based system     */
double siteVX, siteVY;                 /* velocity [km/s]                     */
double siteMat[3][3];                  /* site transformation matrix          */
double RAANPrec, perigeePrec;          /* precession parameters               */
double argPerigee;                     /* precessed argument of Perigee       */
double satLat, satLong, satHeight;     /* sub-satellite point                 */
double azimuth, elevation, range;      /* [rad], [km] or [NM]                 */
double riseAzimuth, setAzimuth;        /* [rad]                               */
double maxAzimuth;                     /* [rad]                               */
double maxElevation, relMaxElev;       /* [rad]                               */
double maxRange;                       /* range at MEL [km] or [NM]           */
double pathLoss;                       /* path loss [dB]                      */
double squintAngle;                    /* squint angle [deg]                  */
double dPhase;                         /* double of orbital phase             */
double rangeRate;                      /* velocity w.r.t. ground station      */
double velocity;                       /* orbital velocity [km/s]             */
double dopplerShift;                   /* Doppler shift [Hz]                  */
double curTime, tmpTime, deltaTime;    /* time [d]                            */
double predTime, predStopTime;         /* time [d]                            */
double lastTime, hour;                 /* time [d]                            */
double fastStepTime, slowStepTime;     /* time [d]                            */
double medStepTime;
double nextRiseTime, nextSetTime;      /* rise and set times [d]              */
double nextMaxTime;
double cUnit, dummyd;

long   orbitNum, stsOrbitNum;          /* int part of orbit number            */
long   riseOrbitNum, printOrbitNum;

int    sysYear, sysMonth, sysDay, sysYearDay, sysHour, sysMin, sysSec;
int    i, j, phase, curMode, prevVisible, foundRiseFlag, foundSetFlag, noPass;
int    trackingFlag, predictionFlag, writeFlag, restartFlag, finishPass;
int    sleepCount;

char   displayString[10], fileName[100], syscomm[120], buf[80], sUnit[10];
char   fond[30];

/******************************************************************************/
/*                                                                            */
/* getTrueAnomaly: gets mean and true anomaly and the orbit number            */
/*                                                                            */
/******************************************************************************/

getTrueAnomaly(timeArg)

double timeArg;

{
    deltaTime     = timeArg - epochDay;
    curMotion     = epochMeanMotion + deltaTime * decayRate;
    semiMajorAxis = KEPLER * exp(2.0 * log(MPD / curMotion) / 3.0);
    curOrbit      = refOrbit + deltaTime * curMotion;
    orbitNum      = (long) curOrbit;
    meanAnomaly   = (curOrbit - (double) orbitNum) * TWOPI;
    trueAnomaly   = kepler(meanAnomaly,eccentricity);
    orbitFract    = modf(curOrbit,&dummyd) * 100.0;
    orbitNum     += 1;
}

/******************************************************************************/
/*                                                                            */
/* printPredHeader: prints prediction header                                  */
/*                                                                            */
/******************************************************************************/

printPredHeader()

{
    fprintf(outFile,"\n%s %s %s (%s)\n\n",
        STATIONCALL,strtName,predHeader,strtVersion);
    fprintf(outFile,"Satellite         : %s",satName);
    if (strlen(satAlias))
        fprintf(outFile," (%s)",satAlias);
    fprintf(outFile,"\n");
    fprintf(outFile,"Data File         : %s\n",elementFile);
    if (stsFlag)
        fprintf(outFile,"Element Set Type  : %s\n",elementType);
    fprintf(outFile,"Element Set Number: %d\n",elementSet);
    fprintf(outFile,"Element Set Epoch : %s   ",epochString);
    fprintf(outFile,"(%s)\n",updateString);
    if (launchFlag)
        fprintf(outFile,"Launch Date/Time  : %s\n",launchString);
    fprintf(outFile,"Ground Station    : %s\n",siteName);
    fprintf(outFile,"Time Zone         : %s (%+.0f h)\n",
            timeZoneStr,timeZone*24.0);

    if (!shortPredFlag)
    {
        fprintf(outFile,"Min. Elevation    : %.1f deg\n",minElevation*CRD);
        fprintf(outFile,"Doppler Reference : %.4f MHz\n",beaconFreq*CHZMHZ);
    }
}

/******************************************************************************/
/*                                                                            */
/* doLongPrediction: performs long form orbit prediction                      */
/*                                                                            */
/******************************************************************************/

doLongPrediction()

{
    slowStepTime = 1.0 / epochMeanMotion / 2000.0;
    medStepTime  = slowStepTime *  5.0;
    fastStepTime = medStepTime * 20.0;
    firstLine    = TRUE;
    firstPage    = TRUE;

    if (curTime < realTime)
        predTime = curTime;
    else
        predTime = (elsetEpoch > curTime) ? elsetEpoch : curTime;

    predStopTime = predTime + (stopTime - startTime);

    do
    {
        if (predTime - lastTime > UPDATETIME)
        {
            initOrbitRoutines(predTime);
            lastTime = predTime;
        }

        getTrueAnomaly(predTime);

        getSatPosition(epochDay,epochRAAN,epochArgPerigee,semiMajorAxis,
            inclination,eccentricity,RAANPrec,perigeePrec,predTime,
            trueAnomaly,&argPerigee,&satX,&satY,&satZ,&satRadius,
            &satVX,&satVY,&satVZ);

        getSitePosition(siteLat,siteLong,siteAlt,predTime,
            &siteX,&siteY,&siteZ,&siteVX,&siteVY,siteMat);

        getAziElev(satX,satY,satZ,siteX,siteY,siteZ,siteMat,
            &azimuth,&elevation);

        if (elevation >= minElevation && predTime >= startTime)
        {
            getRange(siteX,siteY,siteZ,siteVX,siteVY,
                satX,satY,satZ,satVX,satVY,satVZ,&range,&rangeRate);

            getSubSatPoint(satX,satY,satZ,predTime,
                &satLat,&satLong,&satHeight);

            dopplerShift = -beaconFreq * rangeRate / CVAC;    /* [Hz] */

            getPathLoss(beaconFreq,range,&pathLoss);
            getPhase();

            if (!prevVisible || firstLine)
            {
                if (stsFlag)
                {
                    cUnit = CKMNM;
                    sprintf(sUnit,"NM");
                    getShuttleOrbit();
                    printOrbitNum = stsOrbitNum;
                }

                else
                {
                    cUnit = 1.0;
                    sprintf(sUnit,"km");
                    printOrbitNum = orbitNum;
                }

                if (printOrbitNum > 0)
                {
                    if (!firstPage)
                        fprintf(outFile,"\f");

                    if (firstPage && !writeFlag || writeFlag)
                        printPredHeader();

                    firstPage = FALSE;

                    fprintf(outFile,"\n\n");
                    printDate(outFile,predTime+timeZone);
                    fprintf(outFile,"  ---  Orbit %ld",printOrbitNum);

                    if (launchFlag)
                    {
                        fprintf(outFile,"  ---  MET: ");
                        printMET(outFile,predTime-launchEpoch);
                    }

                    fprintf(outFile,"\n\n");
                    fprintf(outFile,"   %3s",timeZoneStr);
                    fprintf(outFile,"   Azimuth  Elev   Range  ");
                    fprintf(outFile,"  Lat    Long    Height ");
                    fprintf(outFile," Doppler  Loss  Phs Md V\n");
                    fprintf(outFile,"          [deg]  [deg]");
                    fprintf(outFile,"    [%2s] ",sUnit);
                    fprintf(outFile,"  [deg]   [deg]    [%2s]  ",sUnit);
                    fprintf(outFile,"  [kHz]   [dB]\n");

                    firstLine = FALSE;
                }
            }

            eclipseFlag = satEclipse(satX,satY,satZ,siteX,siteY,siteZ,
                              siteMat,predTime,elevation);

            if (printOrbitNum > 0)
            {
                printTime(outFile,predTime+timeZone);
                fprintf(outFile,"  %5.1lf",azimuth*CRD);
                fprintf(outFile,"  %5.1lf",elevation*CRD);
                fprintf(outFile,"  %7.1lf",range*cUnit);
                fprintf(outFile,"  %+5.1lf",satLat*CRD);
                fprintf(outFile,"  %+6.1lf",satLong*CRD);
                fprintf(outFile,"  %7.1lf",satHeight*cUnit);
                fprintf(outFile,"  %+6.2lf",dopplerShift*CHZKHZ);
                fprintf(outFile,"  %5.1lf",pathLoss);
                fprintf(outFile,"  %3d",phase);
                printMode(outFile,phase);
                fprintf(outFile," %s\n",visibCode[eclipseFlag]);

                prevVisible = TRUE;
            }
        }

        else
            prevVisible = FALSE;

        predTime   += (elevation < VLOWELEV) ? fastStepTime : stepTime;
        finishPass  = (elevation >= minElevation) ? TRUE : FALSE;

    } while (predTime <= predStopTime || finishPass);
}

/******************************************************************************/
/*                                                                            */
/* doShortPrediction: performs short form orbit prediction                    */
/*                                                                            */
/******************************************************************************/

doShortPrediction()

{
    printPredHeader();

    if (curTime < realTime)
        predTime = curTime;
    else
        predTime = (elsetEpoch > curTime) ? elsetEpoch : curTime;

    predStopTime = predTime + (stopTime - startTime);
    firstLine    = TRUE;

    fprintf(outFile,"\n\n");
    fprintf(outFile," Date (%3s)          Time (%3s) at        ",
            timeZoneStr,timeZoneStr);
    fprintf(outFile,"Duration   Azimuth at   Max  Vis Orbit");
    if (launchFlag && stsFlag && writeFlag)
        fprintf(outFile,"   MET at AOS");
    fprintf(outFile,"\n");
    fprintf(outFile,"                 AOS      MEL      LOS     ");
    fprintf(outFile,"of Pass  AOS MEL LOS  Elev\n");

    do
    {
        if (predTime - lastTime > UPDATETIME)
        {
            initOrbitRoutines(predTime);
            lastTime = predTime;
        }

        getNextPass();

        if (riseOrbitNum > 0)
        {
            printDate(outFile,nextRiseTime+timeZone);
            fprintf(outFile,"  ");
            printTime(outFile,nextRiseTime+timeZone);
            fprintf(outFile," ");
            printTime(outFile,nextMaxTime+timeZone);
            fprintf(outFile," ");
            printTime(outFile,nextSetTime+timeZone);
            fprintf(outFile,"  ");
            printTime(outFile,nextSetTime-nextRiseTime);
            fprintf(outFile,"  %3.0f %3.0f %3.0f",
                    riseAzimuth*CRD,maxAzimuth*CRD,setAzimuth*CRD);
            fprintf(outFile,"  %4.1f",maxElevation*CRD);
            if (maxElevation > MAXELEV)
                fprintf(outFile,"*");
            else
                fprintf(outFile," ");
            fprintf(outFile," %s%s%s",visibCode[eclipseRise],
                    visibCode[eclipseMax],visibCode[eclipseSet]);
            fprintf(outFile," %5ld",riseOrbitNum);

            if (launchFlag && stsFlag && writeFlag)
            {
                fprintf(outFile," ");
                printMET(outFile,nextRiseTime-launchEpoch);
            }

            fprintf(outFile,"\n");

            firstLine = FALSE;
        }

        predTime = tmpTime + fastStepTime;

    } while (predTime <= predStopTime);
}

/******************************************************************************/
/*                                                                            */
/* getNextPass: find circumstances for the next pass                          */
/*                                                                            */
/******************************************************************************/

getNextPass()

{
    tmpTime      = predTime;
    slowStepTime = 1.0 / epochMeanMotion / 2000.0;
    medStepTime  = 5.0 * slowStepTime;
    fastStepTime = 20.0 * medStepTime;
    riseOrbitNum = -1;

    do
    {
        maxElevation  = -HALFPI;
        relMaxElev    = -HALFPI;
        riseAzimuth   = 0.0;
        foundRiseFlag = FALSE;
        foundSetFlag  = FALSE;
        noPass        = FALSE;

        do
        {
            getTrueAnomaly(tmpTime);

            getSatPosition(epochDay,epochRAAN,epochArgPerigee,semiMajorAxis,
                inclination,eccentricity,RAANPrec,perigeePrec,tmpTime,
                trueAnomaly,&argPerigee,&satX,&satY,&satZ,&satRadius,
                &satVX,&satVY,&satVZ);

            getSitePosition(siteLat,siteLong,siteAlt,tmpTime,
                &siteX,&siteY,&siteZ,&siteVX,&siteVY,siteMat);

            getAziElev(satX,satY,satZ,siteX,siteY,siteZ,siteMat,
                &azimuth,&elevation);

            getRange(siteX,siteY,siteZ,siteVX,siteVY,
                satX,satY,satZ,satVX,satVY,satVZ,&range,&rangeRate);

            if (elevation > 0.0 && !foundRiseFlag)
            {
                if (stsFlag)
                    getShuttleOrbit();

                riseOrbitNum = (stsFlag) ? stsOrbitNum : orbitNum;

                nextRiseTime  = tmpTime;
                riseAzimuth   = azimuth;
                eclipseRise   = satEclipse(satX,satY,satZ,siteX,siteY,siteZ,
                                    siteMat,tmpTime,EPSILON);
                foundRiseFlag = TRUE;
            }

            if (foundRiseFlag && elevation > maxElevation)
            {
                maxAzimuth    = azimuth;
                maxElevation  = elevation;
                maxRange      = range;
                nextMaxTime   = tmpTime;
                eclipseMax    = satEclipse(satX,satY,satZ,siteX,siteY,siteZ,
                                    siteMat,tmpTime,elevation);
            }

            if (elevation < 0.0 && foundRiseFlag)
            {
                nextSetTime   = tmpTime;
                setAzimuth    = azimuth;
                eclipseSet    = satEclipse(satX,satY,satZ,siteX,siteY,siteZ,
                                    siteMat,tmpTime,EPSILON);
                foundSetFlag  = TRUE;
            }

            if (elevation < VLOWELEV || elevation > VLOWELEV && 
                elevation < relMaxElev && !foundRiseFlag)
                tmpTime += fastStepTime;

            if (elevation > VLOWELEV && elevation > relMaxElev && 
                elevation < LOWELEV)
                tmpTime += medStepTime;

            if (elevation > LOWELEV && elevation > relMaxElev && !foundRiseFlag)
                tmpTime += slowStepTime;

            if (foundRiseFlag && fabs(elevation - maxElevation) < EPSILON || 
                elevation > -LOWELEV)
                tmpTime += medStepTime;

            if (elevation > 0.0 && elevation < maxElevation && 
                elevation < -LOWELEV)
                tmpTime += slowStepTime;

            if (tmpTime > predTime + MAXDAYS)           /* stop after MAXDAYS */
            {
                if (foundRiseFlag && !foundSetFlag)
                {
                    nextSetTime  = tmpTime;
                    setAzimuth   = azimuth;
                    eclipseSet   = satEclipse(satX,satY,satZ,siteX,siteY,siteZ,
                                       siteMat,tmpTime,EPSILON);
                    foundSetFlag = TRUE;
                }

                if (!foundRiseFlag && !foundSetFlag)
                {
                    nextRiseTime  = tmpTime;
                    nextSetTime   = tmpTime;
                    riseAzimuth   = 0.0;
                    maxAzimuth    = 0.0;
                    setAzimuth    = 0.0;
                    maxElevation  = 0.0;
                    maxRange      = 0.0;
                    eclipseRise   = BLANK;
                    eclipseMax    = BLANK;
                    eclipseSet    = BLANK;
                    foundRiseFlag = TRUE;
                    foundSetFlag  = TRUE;
                    noPass        = TRUE;
                }
            }

            relMaxElev = elevation;
        }
        while (!foundSetFlag);
    }
    while (riseOrbitNum <= 0 && !noPass);
}

/******************************************************************************/
/*                                                                            */
/* getPhase: gets phase of the satellite (0 - 256)                            */
/*                                                                            */
/******************************************************************************/

getPhase()

{
    dPhase = (meanAnomaly / TWOPI * maxPhase + perigeePhase);
    phase  = (int) dPhase;

    while (phase <         0) phase += maxPhase;
    while (phase >= maxPhase) phase -= maxPhase;
}
 
/******************************************************************************/
/*                                                                            */
/* getMode: gets satellite mode                                               */
/*                                                                            */
/******************************************************************************/

getMode()

{
    for (curMode = 0; curMode < numModes; curMode++)
    {
        if ((phase >= modes[curMode].minPhase
            && phase < modes[curMode].maxPhase)
            || ((modes[curMode].minPhase > modes[curMode].maxPhase)
            && (phase >= modes[curMode].minPhase
            || phase < modes[curMode].maxPhase)))
        {
            sprintf(modeString,"%s",modes[curMode].modeStr);
        }
    }

    if (strlen(modeString) == 0)
        sprintf(modeString," ");
}

/******************************************************************************/
/*                                                                            */
/* getShuttleOrbit: gets orbit number for the space shuttle                   */
/*                                                                            */
/* Convention for element sets from NASA  : -1                                */
/*                             from NORAD : +1                                */
/*                                                                            */
/******************************************************************************/

getShuttleOrbit()

{
    stsOrbit      = curOrbit + argPerigee * CRREV;
    stsOrbit     += (!strcmp(elementType,"NASA")) ? -1.0 : 1.0;
    stsOrbitNum   = (int) stsOrbit;
    stsOrbitFract = modf(stsOrbit,&dummyd) * 100.0;
}

/******************************************************************************/
/*                                                                            */
/* getRealTime: gets time from the system clock                               */
/*                                                                            */
/******************************************************************************/

getRealTime()

{
    getUnixTime(&sysDay,&sysMonth,&sysYear,&sysYearDay,
                &sysHour,&sysMin,&sysSec);

    dayNumber = getDayNum (sysYear,sysMonth,sysDay);
    hour      = (double) (sysHour + sysMin/60.0 + sysSec/3600.0);
    curTime   = (double) dayNumber + hour/24.0;
    realTime  = curTime;
}

/******************************************************************************/
/*                                                                            */
/*  checkKeyboard: checks keyboard entry when live display is running;        */
/*                 it returns zero if no character was entered, and the       */
/*                 ASCII code otherwise                                       */
/*                                                                            */
/******************************************************************************/

#define STDIN_FILENO 0

int checkKeyboard()

{
    static int structValid = FALSE;
    static struct termios original, modified;
    int retValue;

    if (!structValid)
    {
       ioctl(STDIN_FILENO,TIOCGETA,&original);
       bcopy((char *)&original, (char *)&modified, sizeof(struct termios));
       modified.c_lflag &= (~ICANON);         /* character input */
       modified.c_cc[VMIN] = 0;               /* minimum number of characters */
       modified.c_cc[VTIME] = 0;              /* do not wait */
       structValid = TRUE;
    }

    ioctl(STDIN_FILENO,TIOCSETA,&modified);
    retValue = getc(stdin);

    if (feof(stdin))
    {
       retValue = 0;
       clearerr(stdin);
    }

    ioctl(STDIN_FILENO,TIOCSETA,&original);
    return(retValue);
}

/******************************************************************************/
/*                                                                            */
/* main program sattrack starts here                                          */
/*                                                                            */
/******************************************************************************/

main(argc,argv)

int  argc;
char *argv[];

{
    int  keyValue = -1;
    char inbuf[80];

    verbose     = FALSE;         /* check if arguments are passed to sattrack */
    askTimeZone = FALSE;

    if (argc >= 2)
    {
        for (i = 1; i < argc; i++)
        {
            if (!strncmp(argv[i],TIMEZONE,2))
                askTimeZone = TRUE;

            if (!strncmp(argv[i],VERBOSE,2))
                verbose = TRUE;
        }
    } 

    strp = getenv("HOME");

/******************************************************************************/
/*                                                                            */
/* get satellite and ground station data                                      */
/*                                                                            */
/******************************************************************************/

start:

    system("clear");
    printf("%s %s\n",strtName,strtVersion);

    trackingFlag   = FALSE;
    predictionFlag = FALSE;
    writeFlag      = FALSE;
    restartFlag    = FALSE;
    finishPass     = FALSE;
    elevationFlag  = FALSE;
    newRiseFlag    = FALSE;
    passDispFlag   = TRUE;
    countdownFlag  = FALSE;
    checkCountdown = TRUE;

    getRealTime();
    getDefaultData();

    if (askTimeZone)
    {
        nl();
        printf("Time zone                       <%3s> : ",defaultTimeZoneStr);
        gets(timeZoneStr);
        upperCase(timeZoneStr);

        if (!strlen(timeZoneStr))
            strcpy(timeZoneStr,defaultTimeZoneStr);

        getTimeZone();
        printf("Time zone set to %s (%+.0f h)\n",timeZoneStr,timeZone*24.0);
    }

    while (!trackingFlag || restartFlag)
    {
        getSiteParams();
        getSatParams();
        getSatModes();

        launchDate  = getDayNum(launchYear,launchMonth,launchDay);
        launchTime  = (double) (launchHour + launchMin/60.0 + launchSec/3600.0);
        launchEpoch = (double) launchDate + launchTime / 24.0;

        trackingFlag   = FALSE;
        predictionFlag = FALSE;
        writeFlag      = FALSE;
        restartFlag    = FALSE;

        while (!trackingFlag && !predictionFlag && !writeFlag && !restartFlag)
        {
            underline(); printf("D"); normal();
            printf("isplay ");
            underline(); printf("P"); normal();
            printf("rediction ");
            underline(); printf("R"); normal();
            printf("estart ");
            underline(); printf("Q"); normal();
            printf("uit   <D> ? ");

            gets(displayString);
            upperCase(displayString);
        
            trackingFlag   = (!strlen(displayString) || 
                              !strncmp(displayString,DF,1)) ? TRUE : FALSE;
            predictionFlag = (!strncmp(displayString,PF,1)) ? TRUE : FALSE;
            restartFlag    = (!strncmp(displayString,RF,1)) ? TRUE : FALSE;

            if (!strncmp(displayString,QF,1))
            {
                nl();
                exit(-1);
            }

            if (predictionFlag)
            {
                underline(); printf("S"); normal();
                printf("hort or ");
                underline(); printf("L"); normal();
                printf("ong prediction format   <S> ? ");
                gets(displayString);
                upperCase(displayString);

                shortPredFlag = (!strlen(displayString) || 
                                 !strncmp(displayString,SF,1)) ? TRUE : FALSE;

                printf("Output on ");
                underline(); printf("V"); normal();
                printf("ideo terminal or ");
                underline(); printf("F"); normal();
                printf("ile  <V> ? ");
                gets(displayString);
                upperCase(displayString);

                predictionFlag = (!strlen(displayString) || 
                                  !strncmp(displayString,VF,1)) ? TRUE : FALSE;
                writeFlag      = (!strncmp(displayString,FF,1)) ? TRUE : FALSE;
            }
        }

        if (predictionFlag)
            outFile = stdout;
 
        if (writeFlag)
        {
            sprintf(fileName,"%s/%s/%s",strp,PREDICTION,satFileName);

            if ((outFile = fopen(fileName,"w")) == NULL)
            {
                printf("\ncannot open output file %s\n\n",fileName);
                exit(-1);
            }

            printf("\ndata are written into '%s'\n\n",fileName);
        }

        if (predictionFlag || writeFlag)
            getTimeParams();
 
        semiMajorAxis = KEPLER * exp(2.0 * log(MPD / epochMeanMotion) / 3.0);

        getPrecession(semiMajorAxis,eccentricity,inclination,
                      &RAANPrec,&perigeePrec);

        refOrbit      = (double) epochOrbitNum + epochMeanAnomaly / TWOPI;
        beaconFreq   /= CHZMHZ;                              /* convert to Hz */
        attLat       *= CDR;
        attLong      *= CDR;
        lastTime      = 0.0;
 
        if (predictionFlag || writeFlag)
        {
            printf("\ncalculating passes of %s over %s ...\n",satName,siteName);

            curTime = startTime;

            if (shortPredFlag)
                doShortPrediction();
            else
                doLongPrediction();

            if (writeFlag)
            {
                fclose(outFile);
                printf("\nHardcopy                          <Y> ? ");
                gets(displayString);
                upperCase(displayString);

                if (!strlen(displayString) || !strncmp(displayString,YES,1))
                {
                    printf("\nprinting %s ...\n",fileName);

                    if (shortPredFlag && !launchFlag)
                        sprintf(fond,"-f Courier10");

                    if (shortPredFlag && launchFlag)
                        sprintf(fond,"-f Courier9");

                    if (!shortPredFlag)
                        sprintf(fond,"-f Courier7 -2r");

                    system(sprintf(syscomm,"enscript %s %s",fond,fileName));
                }
            }

            nl();
        }
    }

/******************************************************************************/
/*                                                                            */
/* live tracking display starts here                                          */
/*                                                                            */
/******************************************************************************/

    lastTime     = 0.0;
    nextRiseTime = 0.0;
    nextSetTime  = 0.0;
    riseAzimuth  = 0.0;
    maxAzimuth   = 0.0;
    setAzimuth   = 0.0;
    maxRange     = 0.0;

    geoStatFlag  = (fabs(epochMeanMotion - 1.0) < 0.05) ? TRUE : FALSE;

    initDisp();

    do
    {
        getRealTime();

        if (curTime - lastTime > UPDATETIME)
        {
            initOrbitRoutines(curTime);
            lastTime = curTime;
        }

        if (curTime > nextSetTime && !geoStatFlag)
        {
            if (curTime < realTime)
                predTime = curTime;
            else
                predTime = (elsetEpoch > curTime) ? elsetEpoch : curTime;

            predStopTime = predTime + (stopTime - startTime);
            getNextPass(); 
            newRiseFlag  = TRUE;                   /* for controlling display */
        }

        getTrueAnomaly(curTime);

        if (stsFlag)
            getShuttleOrbit();

        getSatPosition(epochDay,epochRAAN,epochArgPerigee,semiMajorAxis,
            inclination,eccentricity,RAANPrec,perigeePrec,curTime,trueAnomaly,
            &argPerigee,&satX,&satY,&satZ,&satRadius,&satVX,&satVY,&satVZ);

        getSitePosition(siteLat,siteLong,siteAlt,curTime,
            &siteX,&siteY,&siteZ,&siteVX,&siteVY,siteMat);

        getAziElev(satX,satY,satZ,siteX,siteY,siteZ,siteMat,
            &azimuth,&elevation);

        getRange(siteX,siteY,siteZ,siteVX,siteVY,
            satX,satY,satZ,satVX,satVY,satVZ,&range,&rangeRate);

        velocity     = sqrt(satVX*satVX + satVY*satVY + satVZ*satVZ);
        dopplerShift = -beaconFreq * rangeRate / CVAC;                /* [Hz] */
 
        getSubSatPoint(satX,satY,satZ,curTime,&satLat,&satLong,&satHeight);
        getSquintAngle(satX,satY,satZ,&squintAngle);
        getPathLoss(beaconFreq,range,&pathLoss);
        getPhase();

        if (numModes)
            getMode();

        eclipseFlag  = satEclipse(satX,satY,satZ,siteX,siteY,siteZ,
                           siteMat,curTime,elevation);

        if (elsetEpoch > curTime)
            updateDisp(curTime,launchEpoch,
                   sysYear,sysMonth,sysDay,sysYearDay,sysHour,sysMin,sysSec,
                   0.0,0,0.0,0,0.0,
                   0.0,0.0,0.0,0.0,
                   0.0,-1.0e-6,0.0,0.0,0.0,0.0,0.0,
                   0.0,siteX,siteY,siteZ,0.0,0.0,0.0,0.0,0.0,0.0,
                   nextRiseTime,nextSetTime,riseAzimuth,maxAzimuth,setAzimuth,
                   maxElevation,maxRange);

        else
            updateDisp(curTime,launchEpoch,
                   sysYear,sysMonth,sysDay,sysYearDay,sysHour,sysMin,sysSec,
                   curOrbit,orbitNum,orbitFract,stsOrbitNum,stsOrbitFract,
                   dopplerShift,pathLoss,squintAngle,dPhase,
                   azimuth,elevation,range,rangeRate,satLat,satLong,satHeight,
                   velocity,siteX,siteY,siteZ,satX,satY,satZ,satVX,satVY,satVZ,
                   nextRiseTime,nextSetTime,riseAzimuth,maxAzimuth,setAzimuth,
                   maxElevation,maxRange);

        sleepCount = 0;

        do
        {
            sleep(1);
            sleepCount++;
        } while (!(keyValue = checkKeyboard()) && sleepCount < SLEEPTIME);

    } while (!keyValue);

    if (keyValue == 10)                                       /* exit on <CR> */
    {
        system("clear");
        exit(-1);
    }

    goto start;
}

/******************************************************************************/
/*                                                                            */
/* end of program sattrack.c                                                  */
/*                                                                            */
/******************************************************************************/
