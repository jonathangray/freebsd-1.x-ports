/******************************************************************************/
/*                                                                            */
/*  Title       : satdisp.c                                                   */
/*  Author      : Manfred Bester, DL5KR                                       */
/*  Date        : 24Feb92                                                     */
/*  Last change : 02Jan93                                                     */
/*                                                                            */
/*  Synopsis    : This function performs the live display part of the         */
/*                satellite tracking program 'sattrack'.                      */
/*                                                                            */
/******************************************************************************/

#include <stdlib.h>
#include <stdio.h>                /* standard C I/O                           */
#include <math.h>                 /* library of mathematical functions        */

#include "satglobalsx.h"          /* external global variables                */
#include "sattrack.h"             /* definition of various constants          */
#include "vt100.h"                /* definitions of VT100 macros              */

/******************************************************************************/
/*                                                                            */
/*  convertTime: converts format of time from decimal to integer              */
/*                                                                            */
/******************************************************************************/

void convertTime(time,days,hours,mins,secs)

double time;
int    *days, *hours, *mins, *secs;

{
    double timed, timeh, timem, times, timems;

    timed  = time;
    timeh  = 24.0*modf(timed,&timed);
    timem  = 60.0*modf(timeh,&timeh);
    times  = 60.0*modf(timem,&timem);
    timems = 1000.0*modf(times,&times);

    if (timems >= 500)
        times += 1.0;

    if (fabs(times - 60.0) < 5.0e-4)
    {
        times  = 0.0;
        timem += 1.0;

        if (fabs(timem - 60.0) < 1.0e-4)
        {
            timem  = 0.0;
            timeh += 1.0;

            if (fabs(timeh - 24.0) < 1.0e-4)
            {
                timeh  = 0.0;
                timed += 1.0;
            }
        }
    }

    *days  = (int) timed;
    *hours = (int) timeh;
    *mins  = (int) timem;
    *secs  = (int) times;
}

/******************************************************************************/
/*                                                                            */
/*  updateDisp: updates the tracking display                                  */
/*                                                                            */
/******************************************************************************/

void updateDisp(curTime,launchEpoch,year,month,day,yearDay,hour,min,sec,
           curOrbit,orbitNum,orbitFract,stsOrbitNum,stsOrbitFract,
           dopplerShift,pathLoss,squintAngle,phase,
           azimuth,elevation,range,rangeRate,latitude,longitude,height,
           velocity,siteX,siteY,siteZ,satX,satY,satZ,satVX,satVY,satVZ,
           nextRiseTime,nextSetTime,riseAzimuth,maxAzimuth,setAzimuth,
           maxElevation,maxRange)

double curTime, launchEpoch;
double curOrbit, orbitFract, stsOrbitFract, dopplerShift, pathLoss;
double squintAngle, phase, azimuth, elevation, range, rangeRate;
double latitude, longitude, height, velocity, nextRiseTime, nextSetTime;
double riseAzimuth, maxAzimuth, setAzimuth, maxElevation, maxRange;
double siteX, siteY, siteZ, satX, satY, satZ, satVX, satVY, satVZ;

long   orbitNum, stsOrbitNum;

int    year, month, day, yearDay, hour, min, sec;

{
    double MET, cUnit, gndTrkDist;
    int    tDay, tHour, tMin, tSec, METday, METhour, METmin, METsec, dummyi;
    int    preLaunch;
    char   gndTrkDir[10], gndTrkCity[80];

    if (stsFlag)
        cUnit = CKMNM;
    else
        cUnit = 1.0;

    if (elevation >= 0.0 && !elevationFlag)
    {
        gotoXY(headerCol,1);
        reverse();
        printf("%s",header);
        normal();
        alarm();
        elevationFlag = TRUE;
    }

    if (elevation < 0.0 && elevationFlag)
    {
        gotoXY(headerCol,1);
        underline();
        printf("%s",header);
        normal();
        alarm();
        elevationFlag = FALSE;
    }

    gotoXY(40,3);
    printf("%02d-%02d-%02d",day,month,year);
    gotoXY(40,4);
    printf("%-3d  %3s",yearDay,dayNames[dayNumber%7]);
    gotoXY(40,5);
    printf("%02d:%02d:%02d",hour,min,sec);

    if (timeZone != 0.0)
    {
        gotoXY(40,6);
        printf("%02d:%02d:%02d",
               (hour + (int) (24.0 * (1.0 + timeZone + 1.0e-6))) % 24,min,sec);
    }

    if (stsFlag)
    {
        gotoXY(14,6);
        printf("%5ld",stsOrbitNum);
        gotoXY(20,6);
        printf("%5.1f",stsOrbitFract);
    }

    else
    {
        gotoXY(14,6);
        printf("%5ld",orbitNum);
        gotoXY(20,6);
        printf("%5.1f",orbitFract);
    }

    gotoXY(28,6);
    if (orbitNum > 0 || stsOrbitNum > 0)
    {
        if (eclipseFlag == VISIBLE) reverse();
        printf("%s",visibCode[eclipseFlag]);
        if (eclipseFlag == VISIBLE) normal();
    }
    else
        printf("%s",visibCode[BLANK]);

    gotoXY(14,7);
    printf("%5.1f %5.1f",sunAzimuth*CRD,sunElevation*CRD);

    gotoXY(69,4);
    if (elevation > 0.0) reverse();
    printf("%+8.3f",dopplerShift*CHZKHZ);
    gotoXY(69,5);
    printf("%7.2f0",pathLoss);
    if (elevation > 0.0) normal();
    gotoXY(70,6);
    printf("%6.2f0",phase);
    gotoXY(75,7);
    printf("%2s",modeString);

    gotoXY(21,9);
    if (elevation > 0.0) reverse();
    printf("%9.3f",azimuth*CRD);
    gotoXY(21,10);
    printf("%9.3f",elevation*CRD);
    gotoXY(21,11);
    printf("%9.3f",range*cUnit);
    if (elevation > 0.0) normal();
    gotoXY(23,12);
    printf("%7.3f",rangeRate*cUnit);

    gotoXY(64,9);
    printf("%+7.3f",latitude*CRD);
    gotoXY(63,10);
    printf("%+8.3f",longitude*CRD);
    gotoXY(62,11);
    printf("%9.3f",height*cUnit);
    gotoXY(62,12);
    printf("%9.3f",velocity*cUnit);

/*
    gotoXY(18,14);
    printf("%+10.3f",siteX*cUnit);
    gotoXY(42,14);
    printf("%+10.3f",siteY*cUnit);
    gotoXY(66,14);
    printf("%+10.3f",siteZ*cUnit);
*/

    gotoXY(18,15);
    printf("%+10.3f",satX*cUnit);
    gotoXY(42,15);
    printf("%+10.3f",satY*cUnit);
    gotoXY(66,15);
    printf("%+10.3f",satZ*cUnit);

    gotoXY(18,16);
    printf("%+10.3f",satVX*cUnit);
    gotoXY(42,16);
    printf("%+10.3f",satVY*cUnit);
    gotoXY(66,16);
    printf("%+10.3f",satVZ*cUnit);

    if (!geoStatFlag)
    {
        if (newRiseFlag)
        {
            gotoXY(6,18);
            reverse();
            printf("Next AOS  :");
            normal();
            convertTime(nextRiseTime+timeZone,&tDay,&tHour,&tMin,&tSec);
            getDate((int)(nextRiseTime+timeZone),&dummyi,&dummyi,&dummyi,&tDay);
            gotoXY(18,18);
            printf("%3d/%02d:%02d:%02d",tDay,tHour,tMin,tSec);
        }

        if (nextRiseTime > curTime)
            convertTime(nextRiseTime-curTime,&tDay,&tHour,&tMin,&tSec);
        if (nextRiseTime < curTime && nextSetTime > curTime)
            convertTime(nextSetTime-curTime,&tDay,&tHour,&tMin,&tSec);
        if (nextRiseTime == curTime || nextSetTime < curTime)
            convertTime(0.0,&tDay,&tHour,&tMin,&tSec);

        if (tDay == 0 && tHour == 0 && tMin < 1)
        {
            countdownFlag = TRUE;
            alarm();
        }

        else
            countdownFlag = FALSE;

        if (countdownFlag && checkCountdown)
        {
            gotoXY(6,21);
            reverseblink();
            printf("Countdown :");
            normal();
            checkCountdown = FALSE;
        }

        if (!countdownFlag && !checkCountdown)
        {
            gotoXY(6,21);
            reverse();
            printf("Countdown :");
            normal();
            checkCountdown = TRUE;
        }

        gotoXY(18,21);
        if (countdownFlag || elevation > 0.0) reverse();
        printf("%3d/%02d:%02d:%02d",tDay,tHour,tMin,tSec);
        if (countdownFlag || elevation > 0.0) normal();

        if (curTime > nextRiseTime && curTime < nextSetTime && passDispFlag)
        {
            gotoXY(6,18);
            printf("Last AOS  :");
            gotoXY(6,20);
            reverse();
            printf("Next LOS  :");
            normal();
            passDispFlag = FALSE;
        }

        if (newRiseFlag)
        {
            convertTime(nextSetTime-nextRiseTime,&tDay,&tHour,&tMin,&tSec);
            gotoXY(18,19);
            printf("%3d/%02d:%02d:%02d",tDay,tHour,tMin,tSec);

            gotoXY(6,20);
            printf("Next LOS  :");

            convertTime(nextSetTime+timeZone,&tDay,&tHour,&tMin,&tSec);
            getDate((int)(nextSetTime+timeZone),&dummyi,&dummyi,&dummyi,&tDay);
            gotoXY(18,20);
            printf("%3d/%02d:%02d:%02d",tDay,tHour,tMin,tSec);

            gotoXY(64,18);
            printf("%5.1f00",riseAzimuth*CRD);
            gotoXY(77,18);
            printf("%s",visibCode[eclipseRise]);

            gotoXY(64,19);
            printf("%5.1f00",maxAzimuth*CRD);
            gotoXY(77,19);
            printf("%s",visibCode[eclipseMax]);

            gotoXY(64,20);
            printf("%5.1f00",setAzimuth*CRD);
            gotoXY(77,20);
            printf("%s",visibCode[eclipseSet]);

            gotoXY(62,21);
            if (maxElevation > MAXELEV) reverse();
            printf("%7.1f00",maxElevation*CRD);
            gotoXY(62,22);
            printf("%7.1f00",maxRange*cUnit);
            if (maxElevation > MAXELEV) normal();

            newRiseFlag  = FALSE;
            passDispFlag = TRUE;
        }
    }

    if (launchFlag)
    {
        MET = curTime - launchEpoch;

        preLaunch = (MET < 0.0) ? TRUE : FALSE;

        if (preLaunch)
            MET *= -1.0;

        convertTime(MET,&METday,&METhour,&METmin,&METsec);
        gotoXY(17,22);

        if (preLaunch)
        {
            if (METday)
                printf("%4d/%02d:%02d:%02d",-METday,METhour,METmin,METsec);
            else
                printf("  -%d/%02d:%02d:%02d",-METday,METhour,METmin,METsec);
        }

        else
            printf("%4d/%02d:%02d:%02d",METday,METhour,METmin,METsec);
    }

    getGroundTrack(latitude,longitude,&gndTrkDist,gndTrkDir,gndTrkCity);

    gotoXY(24,24);
    printf("%7.1f",gndTrkDist*cUnit);
    gotoXY(35,24);
    printf("%3s",gndTrkDir);
    gotoXY(42,24);
    printf("%-36s",gndTrkCity);
    fflush(stdout);
}

/******************************************************************************/
/*                                                                            */
/*  initDisp: initializes live display (text strings)                         */
/*                                                                            */
/******************************************************************************/

void initDisp()

{
    char stsUnit[6], satTrackName[20], str[80];

    sprintf(stsUnit,(stsFlag) ? "NM" : "km");

    sprintf(satTrackName,"%s",satName);
    upperCase(satTrackName);
    sprintf(header," %s %s ",satTrackName,dispHeader);
    headerCol = 41 - strlen(header) / 2;

    system("clear");
    gotoXY(1,1);
    underline();
    sprintf(str,"%58s"," ");
    printf("%-10s %s %10s",strtName,str,STATIONCALL);
    gotoXY(headerCol,1);
    printf("%s",header);
    normal();
    nl();
    nl();

    printf("Ground Stn : %15s",siteName);
    advCurs(5);
    printf("Date: __-__-__");
    advCurs(5);
    printf("Radio Beacon : %9.3f MHz",beaconFreq*CHZMHZ);
    nl();

    if (!strlen(satAlias))
        printf("Satellite  : %15s",satName);
    else
        printf("Satellite  : %15s",satAlias);
    advCurs(5);
    printf("Day : ___  ___");
    advCurs(5);
    printf("Doppler Shift:   -__.___ kHz");
    nl();

    printf("Inclination:     %7.3f deg",inclination*CRD);
    advCurs(5);
    printf("UTC : __:__:__");
    advCurs(5);
    printf("Path Loss    :   ___.___ dB");
    nl();

    printf("Orbit      : _____ ___._ ");
    putc('%',stdout);
    printf(" _");

    if (timeZone != 0.0)
    {
        advCurs(5);
        printf("%3s : __:__:__",timeZoneStr);
        advCurs(5);
    }

    else
        advCurs(24);

    printf("Phase (0-%3d):   ___.___",MAXPHASE);
    nl();

    printf("Sun Azi/Ele: ___._ -__._ deg");
    advCurs(24);
    printf("Mode  (ABJLS):        __");
    nl();
    nl();

    advCurs(5);
    printf("Azimuth   :      ___.___ deg");
    advCurs(12);
    printf("Latitude  N  :    -__.___ deg");
    nl();

    advCurs(5);
    printf("Elevation :      -__.___ deg");
    advCurs(12);
    printf("Longitude W  :   -___.___ deg");
    nl();

    advCurs(5);
    printf("Range     :    _____.___ %2s",stsUnit);
    advCurs(13);
    printf("Height       :  _____.___ %2s",stsUnit);
    nl();

    advCurs(5);
    printf("Range Rate:      -__.___ %2s/s",stsUnit);
    advCurs(11);
    printf("Velocity     :  _____.___ %2s/s",stsUnit);
    nl();
    nl();

/*
    printf("Site Vector   X: -_____.___ %2s",stsUnit);
    advCurs(8);
    printf("Y: -_____.___ %2s",stsUnit);
    advCurs(8);
    printf("Z: -_____.___ %2s",stsUnit);
*/
    nl();

    printf("State Vector  X: -_____.___ %2s",stsUnit);
    advCurs(8);
    printf("Y: -_____.___ %2s",stsUnit);
    advCurs(8);
    printf("Z: -_____.___ %2s",stsUnit);
    nl();
   
    printf("             VX: -_____.___ %2s/s",stsUnit);
    advCurs(5);
    printf("VY: -_____.___ %2s/s",stsUnit);
    advCurs(5);
    printf("VZ: -_____.___ %2s/s",stsUnit);
    nl();
    nl();

    if (geoStatFlag)
    {
        nl();
        advCurs(11);
        printf("Geostationary Satellite --- No Calculation of AOS and LOS");
        nl();
    }

    else
    {
        advCurs(5);
        printf("Next AOS  : ___/__:__:__ %3s",timeZoneStr);
        advCurs(12);
        printf("AOS Azimuth  :    ___.___ deg  _");
        nl();

        advCurs(5);
        printf("Duration  : ___/__:__:__");
        advCurs(16);
        printf("MEL Azimuth  :    ___.___ deg  _");
        nl();

        advCurs(5);
        printf("Next LOS  : ___/__:__:__ %3s",timeZoneStr);
        advCurs(12);
        printf("LOS Azimuth  :    ___.___ deg  _");
        nl();

        advCurs(5);
        reverse();
        printf("Countdown :");
        normal();
        printf(" ___/__:__:__");
        advCurs(16);
        printf("Max Elevation:     __.___ deg");
    }

    if (launchFlag)
    {
        nl();
        advCurs(5);
        printf("MET       : ___/__:__:__");
        advCurs(16);
    }

    if (!launchFlag && !geoStatFlag)
    {
        nl();
        advCurs(45);
    }

    if (!geoStatFlag)
    {
        printf("MEL Range    :  _____.___ %s",stsUnit);
        nl();
    }
    gotoXY(1,24);
    sprintf(str,"____________________________________");
    printf("Ground Track Location: _____._ %2s ___ of %s",stsUnit,str);
    fflush(stdout);
}

/******************************************************************************/
/*                                                                            */
/*  end of function satdisp.c                                                 */
/*                                                                            */
/******************************************************************************/
