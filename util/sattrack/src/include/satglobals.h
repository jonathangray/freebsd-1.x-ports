/******************************************************************************/
/*                                                                            */
/*  Title       : satglobals.h                                                */
/*  Author      : Manfred Bester, DL5KR                                       */
/*  Date        : 03Mar92                                                     */
/*  Last change : 28Dec92                                                     */
/*                                                                            */
/*  Synopsis    : Global variables for 'sattrack'.                            */
/*                                                                            */
/******************************************************************************/

typedef  struct { int  minPhase, maxPhase;
                  char modeStr[20];
                } modeRec;

double epochDay, elsetEpochDay;        /* time of epoch                       */
double elsetEpoch;
double epochMeanAnomaly;               /* Mean Anomaly at epoch               */
double epochRAAN;                      /* RAAN at epoch                       */
double epochMeanMotion;                /* rev/day                             */
double decayRate;                      /* rev/day/day                         */
double epochArgPerigee;                /* argument of perigee at epoch        */
double eccentricity;
double inclination;
double sunAzimuth, sunElevation;

double beaconFreq;                     /* beacon frequency [Hz]               */
double perigeePhase;
double attLong, attLat;                /* spacecraft attitude [deg]           */
double maxPhase;                       /* phase units in 1 orbit              */

double siteLat, siteLong, siteAlt;     /* site parameters                     */
double defMinElevation, minElevation;  /* minimum elevation for pass calc.    */

double defDuration, duration;          /* duration of orbit prediction [d]    */
double defStepTime, stepTime;          /* step time for orbit prediction [s]  */
double startTime, stopTime, realTime;  /* time parameters [d]                 */
double launchTime, launchEpoch;        /* launch time parameters [d]          */
double timeZone;                       /* time zone [h] (west = '+')          */

double sidDay, sidRef;                 /* date and sidereal time              */
double sunEpochTime, sunInclination;   /* Keplerian elements for the Sun      */
double sunRAAN, sunEccentricity;
double sunArgPerigee;
double sunMeanAnomaly, sunMeanMotion;

long   satNum;                         /* number of satellite                 */
long   epochOrbitNum;                  /* integer orbit number of epoch       */
long   dayNumber;                      /* day number since ____               */
long   lastDayNum;
long   launchDate;

int    launchYear, launchMonth, launchDay, launchHour, launchMin, launchSec;
int    launchFlag, elementSet, numModes, verbose, askTimeZone, timeZones;
int    eclipseFlag, eclipseRise, eclipseMax, eclipseSet, stsFlag, geoStatFlag;
int    elevationFlag, newRiseFlag, passDispFlag, countdownFlag, checkCountdown;
int    shortPredFlag, firstLine, firstPage, headerCol;

char   modesDat[80], sitesDat[80], defaultsDat[80];
char   defaultTimeZoneStr[10], timeZoneStr[10], elementFile[80];
char   satName[80], satAlias[80], satFileName[80], siteName[80];
char   defaultSat[40], defaultSet[40], defaultType[40], defaultSite[40];
char   defaultNORAD[40], elementType[40];
char   string[100], stringT[10], modeString[10], timeString[80];
char   epochString[40], launchString[40], updateString[40], header[80];
char   ch, *strp, *getenv();

FILE   *outFile;

