/******************************************************************************/
/*                                                                            */
/*  Title       : satglobalsx.h                                               */
/*  Author      : Manfred Bester, DL5KR                                       */
/*  Date        : 03Mar92                                                     */
/*  Last change : 29Dec92                                                     */
/*                                                                            */
/*  Synopsis    : External variables and functions for 'sattrack'.            */
/*                                                                            */
/******************************************************************************/

/******************************************************************************/
/*                                                                            */
/*  external functions                                                        */
/*                                                                            */
/******************************************************************************/

extern double getElement(), kepler(), reduce();

extern long   getDayNum();

extern int    readnorad(), satEclipse();

extern char   *monthname();

extern void   calendar(), clipString(), convertTime();
extern void   getAziElev(), getDefaultData(), getGroundTrack();
extern void   getPathLoss(), getPrecession();
extern void   getRange(), getSatModes(), getSatParams(), getSatPosition();
extern void   getSiteParams(), getSitePosition();
extern void   getSquintAngle(), getSubSatPoint();
extern void   getTimeParams(), getTopocentric(), getUnixTime();
extern void   initDisp(), initOrbitRoutines(), lowerCase();
extern void   printDate(), printMET(), printMode(), printTime();
extern void   truncBlanks(), updateDisp(), upperCase();

/******************************************************************************/
/*                                                                            */
/*  external variables                                                        */
/*                                                                            */
/******************************************************************************/

extern double timeZoneHour[];

extern double epochDay, elsetEpochDay, elsetEpoch;
extern double epochMeanAnomaly;
extern double epochRAAN;
extern double epochMeanMotion;
extern double decayRate;
extern double epochArgPerigee;
extern double eccentricity;
extern double inclination;
extern double sunAzimuth, sunElevation;

extern double beaconFreq;
extern double perigeePhase;
extern double attLong, attLat;
extern double maxPhase;

extern double siteLat, siteLong, siteAlt;
extern double defMinElevation, minElevation;

extern double defDuration, duration;
extern double defStepTime, stepTime;
extern double startTime, stopTime, realTime;
extern double launchTime, launchEpoch;
extern double timeZone;

extern double sidDay, sidRef;
extern double sunEpochTime, sunInclination;
extern double sunRAAN, sunEccentricity;
extern double sunArgPerigee;
extern double sunMeanAnomaly, sunMeanMotion;

extern long   satNum;
extern long   epochOrbitNum;
extern long   dayNumber, lastDayNum;
extern long   launchDate;

extern int    launchYear, launchMonth, launchDay;
extern int    launchHour, launchMin, launchSec, launchFlag;
extern int    elementSet, numModes, verbose, askTimeZone, timeZones;
extern int    eclipseFlag, eclipseRise, eclipseMax, eclipseSet, stsFlag;
extern int    geoStatFlag, elevationFlag, newRiseFlag, passDispFlag;
extern int    countdownFlag, checkCountdown, shortPredFlag, firstLine;
extern int    firstPage, headerCol;

extern char   modesDat[80], sitesDat[80], defaultsDat[80];
extern char   defaultTimeZoneStr[10], timeZoneStr[10], elementFile[80];
extern char   satName[80], satAlias[80], satFileName[80], siteName[80];
extern char   defaultSat[40], defaultSet[40], defaultType[40];
extern char   defaultSite[40], defaultNORAD[40], elementType[40];
extern char   string[100], stringT[10], modeString[10], timeString[80];
extern char   epochString[40], launchString[40], updateString[40], header[80];

extern char   strtName[], strtHeader[], strtVersion[], predHeader[];
extern char   dispHeader[];
extern char   *visibCode[], *dayNames[], *timeZoneList[];
extern char   ch, *strp, *getenv();

extern FILE   *outFile;
 
