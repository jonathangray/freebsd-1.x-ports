/******************************************************************************/
/*                                                                            */
/*  Title       : sattrack.h                                                  */
/*  Author      : Manfred Bester, DL5KR                                       */
/*  Date        : 24Feb92                                                     */
/*  Last change : 07Jan93                                                     */
/*                                                                            */
/*  Synopsis    : Definitions of constants for 'sattrack'.                    */
/*                                                                            */
/******************************************************************************/



#define STATIONCALL           "N5VMF"          /* call sign of ground station */

#define INSTALLDIR            "/usr/local/sattrack"
#define DATA                  "SatTrack/Data"
#define PREDICTION            "SatTrack/Prediction"

#define MODES                 "modes.dat"      /* resource file names         */
#define SITES                 "sites.dat"
#define DEFAULTS              "defaults.dat"
#define CITIES                "cities.dat"
#define TLE                   "tle.dat"

#define DF                    "D"              /* menu selection parameters   */
#define FF                    "F"
#define PF                    "P"
#define QF                    "Q"
#define RF                    "R"
#define SF                    "S"
#define VF                    "V"
#define YES                   "Y"

#define TIMEZONE              "-t"             /* specify time zone           */
#define VERBOSE               "-v"             /* verbose flag                */

#define TRUE                 -1
#define FALSE                 0
 
#ifdef  PI
#undef  PI
#endif

#ifdef  TWOPI
#undef  TWOPI
#endif

#define ABS(x)                ((x) < 0 ? (-(x)) : (x))
#define SQR(x)                ((x)*(x))

#define PI                    3.14159265358979323846264338
#define TWOPI                 (2.0*PI)
#define FOURPI                (2.0*TWOPI)
#define HALFPI                (PI/2.0)
#define THREEHALFPI           (HALFPI*3.0)
#define CRD                   (360.0/TWOPI)        /* change rad into deg     */
#define CDR                   (TWOPI/360.0)        /* change deg into rad     */
#define CRREV                 (1.0/TWOPI)          /* change rad into rev     */
#define EPSILON               (TWOPI/1296000.0)    /* 1 arcsec in rad         */
#define MPD                   1440.0               /* minutes per day         */
#define SPD                   86400.0              /* seconds per day         */
#define CMKM                  (1.0/1000.0)         /* change m to km          */
#define CKMNM                 (1.0/1.852)          /* change km to naut. mil. */
#define CHZKHZ                (1.0/1000.0)         /* change Hz to kHz        */
#define CKHZMHZ               (1.0/1000.0)         /* change kHz to MHz       */
#define CHZMHZ                (1.0/1000000.0)      /* change Hz to MHz        */

#define EARTHRADIUS           6378.164             /* equatorial radius [km]  */
#define EARTHFLAT             (1.0/298.257)        /* Earth flattening coeff. */
#define EARTHECCEN            0.01675104           /* Earth's orbit eccentr.  */

#define SUNRADIUS             695980.0             /* equatorial radius [km]  */
#define SUNSEMIMAJORAXIS      149597892.0          /* 1 au [km]               */
#define TWILIGHT              (-6.0*CDR)           /* nautical twilight       */

#define CVAC                  2.99792458e5         /* speed of light [km/s]   */
#define TROPICALYEAR          365.24219879         /* mean solar days / year  */
#define SIDSOLAR              1.002737909350       /* sidereal rate           */
#define SIDRATE               (TWOPI/SPD*SIDSOLAR) /* [rad/s]                 */
#define GM                    398603.2             /* [km^3/s^2]              */
#define KEPLER                331.254038           /* see below               */

/* KEPLER = (GM / (4 PI^2) * 3600)^(1/3) / 1000  with a [km] and T [min]      */

#define BEACON                146.0            /* default beacon frequ. [MHz] */
#define MAXPHASE              256              /* max. value for phase number */
#define MAXMODES              10               /* max. number of sat. modes   */

#define DAY                   0                /* code for visibility display */
#define PARTIAL               1                /* of eclipse conditions       */
#define NIGHT                 2
#define VISIBLE               3
#define BLANK                 4

#define UPDATETIME            0.005            /* update time for init [d]    */
#define VLOWELEV              (-15.0*CDR)      /* low elevation level 1 [rad] */
#define LOWELEV               (-2.0*CDR)       /* low elevation level 2 [rad] */
#define MAXELEV               (40.0*CDR)       /* max elevation warn limit    */
#define MAXDAYS               2.0              /* limit for rise time search  */
#define SLEEPTIME             5                /* update time for display [s] */

