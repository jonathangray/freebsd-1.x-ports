/******************************************************************************/
/*                                                                            */
/*  Title       : satcalc.c                                                   */
/*  Author      : Manfred Bester, DL5KR                                       */
/*  Date        : 03Mar92                                                     */
/*  Last change : 22Dec92                                                     */
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
#include "vt100.h"
 
/******************************************************************************/
/*                                                                            */
/*  reduce: reduces angle into interval -PI, +PI                              */
/*                                                                            */
/******************************************************************************/

double reduce(double angle)

                           /* input and output are in rad */
{
    double x, y, z, *dptr;

    x      = angle/TWOPI;                      /* subtract multiples of TWOPI */
    dptr   = &z;
    y      = modf(x,dptr);
    angle -= z*TWOPI;

    if (angle >  PI)                           /* correct if angle > 180 deg  */
        angle -= TWOPI;

    if (angle < -PI)                           /* correct if angle < -180 deg */
        angle += TWOPI;

    return(angle);
} 

/******************************************************************************/
/*                                                                            */
/* kepler: solves Kepler's equation                                           */
/*                                                                            */
/* inputs:     meanAnomaly     angle since last perigee [rad]                 */
/*                             TWOPI = one complete orbit                     */
/*             eccentricity    eccentricity of orbit                          */
/*                                                                            */
/* output:     trueAnomaly     angle between perigee, geocenter, and current  */
/*                             position [rad]                                 */
/*                                                                            */
/******************************************************************************/

double kepler(meanAnomaly,eccentricity)

double meanAnomaly, eccentricity;
 
{
    double eccAnomaly;                          /* eccentric anomaly */
    double trueAnomaly;
    double error;

    eccAnomaly = meanAnomaly;                            /* Initial guess     */

    do
    {
        error = (eccAnomaly - eccentricity * sin(eccAnomaly) - meanAnomaly) /
                (1.0 - eccentricity * cos(eccAnomaly));
        eccAnomaly -= error;
    }
    while (ABS(error) >= EPSILON);

    if (ABS(eccAnomaly - PI) < EPSILON)
        trueAnomaly = PI;
    else
        trueAnomaly = 2.0 * atan(sqrt((1.0 + eccentricity) /
                            (1.0 - eccentricity)) * tan(eccAnomaly / 2.0));

    if (trueAnomaly < 0.0)
        trueAnomaly += TWOPI;
 
    return(trueAnomaly);
}
 
/******************************************************************************/
/*                                                                            */
/* getSubSatPoint: calculates sub-satellite point                             */
/*                                                                            */
/******************************************************************************/

void getSubSatPoint(satX,satY,satZ,time,latitude,longitude,height)

double satX, satY, satZ, time;
double *latitude, *longitude, *height;

{
    double x, y, z, r, lgt, arg;

    r   = sqrt(SQR(satX) + SQR(satY) + SQR(satZ));

    x   = satX / r;
    y   = satY / r;
    z   = satZ / r;

    arg = z;

    if (arg >  1.0) arg =  1.0;
    if (arg < -1.0) arg = -1.0;

    *latitude  = asin(arg);
    lgt        = TWOPI * ((time - sidDay) * SIDSOLAR + sidRef) - atan2(y,x);
    *longitude = reduce(lgt);
    *height    = r - EARTHRADIUS;               /* assuming a spherical model */
}
 
/******************************************************************************/
/*                                                                            */
/* getPrecession: calculates precession of the satellite's orbit              */
/*                                                                            */
/******************************************************************************/

void getPrecession(semiMajorAxis,eccentricity,inclination,RAANPrec,perigeePrec)

double semiMajorAxis, eccentricity, inclination;
double *RAANPrec, *perigeePrec;

{
    *RAANPrec    = 9.95 * pow(EARTHRADIUS / semiMajorAxis, 3.5)
                        * cos(inclination) / SQR(1.0 - SQR(eccentricity)) * CDR;
 
    *perigeePrec = 4.97 * pow(EARTHRADIUS / semiMajorAxis, 3.5)
                        * (5.0 * SQR(cos(inclination)) - 1.0)
                        / SQR(1.0 - SQR(eccentricity)) * CDR;
}
 
/******************************************************************************/
/*                                                                            */
/* getSatPosition: calculates satellite position and velocity in the mean     */
/*                 equatorial coordinate system (RA, Dec)                     */
/*                                                                            */
/******************************************************************************/

void getSatPosition(epochTime,epochRAAN,epochArgPerigee,semiMajorAxis,
        inclination,eccentricity,RAANPrec,perigeePrec,time,trueAnomaly,
        argPerigee,X,Y,Z,radius,vX,vY,vZ)
 
double epochTime, epochRAAN, epochArgPerigee;
double semiMajorAxis, inclination, eccentricity;
double RAANPrec, perigeePrec, time, trueAnomaly;
double *argPerigee, *X, *Y, *Z, *radius, *vX, *vY, *vZ;

{
    double RAAN;
    double Xw, Yw, vXw, vYw;                    /* in orbital plane          */
    double tmp;
    double Px, Py, Pz, Qx, Qy, Qz;              /* Escobal transformation #31 */
    double cosArgPerigee, sinArgPerigee;
    double cosRAAN, sinRAAN, cosInclination, sinInclination;

    *radius = semiMajorAxis * (1.0 - SQR(eccentricity))
              / (1.0 + eccentricity * cos(trueAnomaly));

    if (*radius < EARTHRADIUS)
    {
        clearLine(1,23);
        reverseblink();
        printf("Satellite has crashed already!\n");
        normal(); alarm();
        clearLine(1,24);
        exit(-1);
    }

    Xw  = *radius * cos(trueAnomaly);
    Yw  = *radius * sin(trueAnomaly);
    tmp = sqrt(GM / (semiMajorAxis * (1.0 - SQR(eccentricity))));
    vXw = -tmp * sin(trueAnomaly);
    vYw = tmp * (cos(trueAnomaly) + eccentricity);

    *argPerigee    = epochArgPerigee + (time - epochTime) * perigeePrec;
    RAAN           = epochRAAN - (time - epochTime) * RAANPrec;

    cosRAAN        = cos(RAAN);
    sinRAAN        = sin(RAAN);
    cosArgPerigee  = cos(*argPerigee);
    sinArgPerigee  = sin(*argPerigee);
    cosInclination = cos(inclination);
    sinInclination = sin(inclination);
    
    Px  =  cosArgPerigee * cosRAAN - sinArgPerigee * sinRAAN * cosInclination;
    Py  =  cosArgPerigee * sinRAAN + sinArgPerigee * cosRAAN * cosInclination;
    Pz  =  sinArgPerigee * sinInclination;

    Qx  = -sinArgPerigee * cosRAAN - cosArgPerigee * sinRAAN * cosInclination;
    Qy  = -sinArgPerigee * sinRAAN + cosArgPerigee * cosRAAN * cosInclination;
    Qz  =  cosArgPerigee * sinInclination;

    *X  =  Px*Xw + Qx*Yw;                       /* Escobal transformation #31 */
    *Y  =  Py*Xw + Qy*Yw;
    *Z  =  Pz*Xw + Qz*Yw;

    *vX =  Px*vXw + Qx*vYw;                     /* satellite velocity         */
    *vY =  Py*vXw + Qy*vYw;
    *vZ =  Pz*vXw + Qz*vYw;
}

/******************************************************************************/
/*                                                                            */
/* getSitePosition: computes the site postion and velocity in the RA based    */
/*                  coordinate system. The siteMat is a matrix which is       */
/*                  used by getTopocentric to convert geocentric coordinates  */
/*                  to topocentric (observer-centered) coordinates.           */
/*                                                                            */
/******************************************************************************/

void getSitePosition(siteLat,siteLong,siteAlt,siteTime,
                siteX,siteY,siteZ,siteVX,siteVY,siteMatP)

double siteLat, siteLong, siteAlt, siteTime;
double *siteX, *siteY, *siteZ, *siteVX, *siteVY;
double siteMatP[3][3];

{
    static double G1, G2;                   /* Used to correct for flattening */
                                            /* of the Earth                   */
    static double cosLat, sinLat;
    static double oldSiteLat = -99.9;       /* Used to avoid unneccesary      */
    static double oldSiteAlt = -99.9;       /* recomputation                  */

    double lat;
    double siteRA;                          /* Right Ascension of the site    */
    double cosRA, sinRA;

    if ((siteLat != oldSiteLat) || (siteAlt != oldSiteAlt))
    {
        oldSiteLat = siteLat;
        oldSiteAlt = siteAlt;

        lat    = atan(1.0 / (1.0 - SQR(EARTHFLAT)) * tan(siteLat));
        cosLat = cos(lat);
        sinLat = sin(lat);

        G1  = EARTHRADIUS 
              / (sqrt(1.0 - (2.0 * EARTHFLAT - SQR(EARTHFLAT)) * SQR(sinLat)));
        G2  = G1 * SQR(1.0 - EARTHFLAT);
        G1 += siteAlt;
        G2 += siteAlt;
    }

    siteRA  =  TWOPI * ((siteTime - sidDay) * SIDSOLAR + sidRef);
    siteRA -=  siteLong;
    cosRA   =  cos(siteRA);
    sinRA   =  sin(siteRA);
    
    *siteX  =  G1 * cosLat * cosRA;
    *siteY  =  G1 * cosLat * sinRA;
    *siteZ  =  G2 * sinLat;

    *siteVX = -SIDRATE * (*siteY);
    *siteVY =  SIDRATE * (*siteX);

    siteMatP[0][0] =  sinLat * cosRA;
    siteMatP[0][1] =  sinLat * sinRA;
    siteMatP[0][2] = -cosLat;
    siteMatP[1][0] = -sinRA;
    siteMatP[1][1] =  cosRA;
    siteMatP[1][2] =  0.0;
    siteMatP[2][0] =  cosRA * cosLat;
    siteMatP[2][1] =  sinRA * cosLat;
    siteMatP[2][2] =  sinLat;
}

/******************************************************************************/
/*                                                                            */
/* getRange: calculates satellite range                                       */
/*                                                                            */
/******************************************************************************/

void getRange(siteX,siteY,siteZ,siteVX,siteVY,
              satX,satY,satZ,satVX,satVY,satVZ,range,rangeRate)

double siteX, siteY, siteZ, siteVX, siteVY;
double satX, satY, satZ, satVX, satVY, satVZ;
double *range, *rangeRate;

{
    double dX, dY, dZ;

    dX = satX - siteX;
    dY = satY - siteY;
    dZ = satZ - siteZ;

    *range     = sqrt(SQR(dX) + SQR(dY) + SQR(dZ));    
    *rangeRate = ((satVX-siteVX)*dX + (satVY-siteVY)*dY + satVZ*dZ) / *range;
}

/******************************************************************************/
/*                                                                            */
/* getTopocentric: converts from geocentric RA based coordinates to           */
/*                 topocentric coordinates                                    */
/*                                                                            */
/******************************************************************************/

void getTopocentric(satX,satY,satZ,siteX,siteY,siteZ,siteMatT,X,Y,Z)

double satX, satY, satZ, siteX, siteY, siteZ;
double *X, *Y, *Z;
double siteMatT[3][3];

{
    satX -= siteX;
    satY -= siteY;
    satZ -= siteZ;

    *X    = siteMatT[0][0]*satX + siteMatT[0][1]*satY + siteMatT[0][2]*satZ;
    *Y    = siteMatT[1][0]*satX + siteMatT[1][1]*satY + siteMatT[1][2]*satZ;
    *Z    = siteMatT[2][0]*satX + siteMatT[2][1]*satY + siteMatT[2][2]*satZ;
}

/******************************************************************************/
/*                                                                            */
/* getAziElev: calculates azimuth and elevation                               */
/*                                                                            */
/******************************************************************************/

void getAziElev(satX,satY,satZ,siteX,siteY,siteZ,siteMatA,azimuth,elevation)

double satX, satY, satZ, siteX, siteY, siteZ;
double siteMatA[3][3];
double *azimuth, *elevation;

{
    double  x, y, z, r, arg;

    getTopocentric(satX,satY,satZ,siteX,siteY,siteZ,siteMatA,&x,&y,&z);

    r  = sqrt(SQR(x) + SQR(y) + SQR(z));

    x /= r;
    y /= r;
    z /= r;

    *azimuth = PI - atan2(y,x);

    if (*azimuth < 0.0)
        *azimuth += PI;

    arg = z;

    if (arg >  1.0) arg =  1.0;      /* protect against out-of-range problems */
    if (arg < -1.0) arg = -1.0;

    *elevation = asin(arg);
}

/******************************************************************************/
/*                                                                            */
/* satEclipse: calculates the eclipses of the satellite                       */
/*                                                                            */
/* this function assumes that the Sun is point-like rather than extended      */
/*                                                                            */
/******************************************************************************/

int satEclipse(satX,satY,satZ,siteX,siteY,siteZ,siteMat,eclTime,satElevation)

double siteMat[3][3];
double satX, satY, satZ, siteX, siteY, siteZ, eclTime, satElevation;

{
    double meanAnomaly, trueAnomaly;
    double sunX, sunY, sunZ, sunVX, sunVY, sunVZ, sunRadius, sunDist;
    double satPara, satPerp, satDist, alpha, beta, dummyd;

    meanAnomaly  = sunMeanAnomaly;
    meanAnomaly += (eclTime - sunEpochTime) * sunMeanMotion * TWOPI;
    trueAnomaly  = kepler(meanAnomaly,sunEccentricity);

    getSatPosition(sunEpochTime,sunRAAN,sunArgPerigee,SUNSEMIMAJORAXIS,
          sunInclination,sunEccentricity,0.0,0.0,eclTime,trueAnomaly,
          &dummyd,&sunX,&sunY,&sunZ,&sunRadius,&sunVX,&sunVY,&sunVZ);

    getAziElev(sunX,sunY,sunZ,siteX,siteY,siteZ,siteMat,
               &sunAzimuth,&sunElevation);

    sunDist = sqrt(SQR(sunX) + SQR(sunY) + SQR(sunZ));
    satDist = sqrt(SQR(satX) + SQR(satY) + SQR(satZ));
    satPara = (satX*sunX + satY*sunY + satZ*sunZ) / sunDist;
    satPerp = sqrt(SQR(satDist) - SQR(satPara));


    /* case 1: satellite is on side of the Earth facing the Sun */

    if (satPara > 0.0)
    {
        if (satElevation < 0.0)
            return(DAY);
        
        if (satElevation >= 0.0 && sunElevation <= TWILIGHT)
            return(VISIBLE);
        else
            return(DAY);
    }

    alpha = asin(EARTHRADIUS / sunDist);
    beta  = atan(satPerp / (satPara + sunDist));

    /* case 2: satellite is on side of the Earth facing away from the Sun */

    if (beta >= alpha)    /* satellite can see the Sun */
    {
        if (satElevation >= 0.0 && sunElevation <= TWILIGHT)
            return(VISIBLE);
        else
            return(DAY);
    }

    else                  /* satellite is in the Earth's shadow */
        return(NIGHT);
}

/******************************************************************************/
/*                                                                            */
/* initOrbitRoutines: initializes the Sun's Keplerian elements for a given    */
/*                    epoch. Formulas are from "Explanatory Supplement to the */
/*                    Ephemeris, HMSO, 1974".                                 */
/*                    It also initializes the sidereal reference.             */
/*                                                                            */
/******************************************************************************/

void initOrbitRoutines(epochDay)

double epochDay;

{
    double T, T2, T3, omega;
    double sunTrueAnomaly, sunDistance;
    int n;

    /* time argument in Julian centuries since 1900.0 */
    /* see Astronomical Almanac, 1983, page B6 */

    T               = (epochDay - 0.5) / 36525.0;
    T2              = T*T;
    T3              = T2*T;

    sidDay          = floor(epochDay);
    sidRef          = 6.646065556 + 2400.051262*T + 2.580556e-5*T2;
    sidRef         /= 24.0;                                          /* [rev] */
    sidRef         -= floor(sidRef);

    /* for nutation correction see Explanatory Supplement, 1974, page 44 */

    omega           = 259.183275 - 1934.142008*T + 2.078e-3*T2 + 2.22e-6*T3;
    omega          *= CDR;
    n               = (int) (omega / TWOPI);
    omega          -= n * TWOPI;

    sunEpochTime    = epochDay;
    sunRAAN         = 0.0;

    /* for ephemeris of the Sun see Explanatory Supplement, 1974, page 98 */

    sunInclination  = 23.452294 - 0.0130125*T - 1.64e-6*T2 + 5.03e-7*T3;
    sunInclination += 2.558333e-3*cos(omega);                      /* page 41 */
    sunInclination *= CDR;

    sunEccentricity = 0.01675104 - 4.180e-5*T - 1.260e-7*T2;

    sunArgPerigee   = 281.220844 + 1.719175*T + 4.528e-4*T2 + 3.3333e-6*T3;
    sunArgPerigee  *= CDR;

    sunMeanAnomaly  = 358.475833 + 35999.04975*T - 1.5e-4*T2 - 3.3333e-6*T3;
    sunMeanAnomaly *= CDR;
    n               = (int) (sunMeanAnomaly / TWOPI);
    sunMeanAnomaly -= n * TWOPI;

    sunMeanMotion   = 1.0 / (365.24219878 - 6.134e-6*T);
    sunTrueAnomaly  = kepler(sunMeanAnomaly,sunEccentricity);
    sunDistance     = SUNSEMIMAJORAXIS * (1.0 - SQR(sunEccentricity))
                      / (1.0 + sunEccentricity * cos(sunTrueAnomaly));
}

/******************************************************************************/
/*                                                                            */
/* getSquintAngle: calculates squint angle                                    */
/*                                                                            */
/******************************************************************************/

/* ATTENTION: this calculation is not correct yet !!! */

void getSquintAngle(satX,satY,satZ,squintAngle)

double satX, satY, satZ, *squintAngle;

{
    double attX, attY, attZ, satNorm;
    double cosAttLat, sinAttLat, cosAttLong, sinAttLong;

    cosAttLat    = cos(attLat);
    sinAttLat    = sin(attLat);
    cosAttLong   = cos(attLong);
    sinAttLong   = sin(attLong);

    attX         = cosAttLat * cosAttLong;
    attY         = cosAttLat * sinAttLong;
    attZ         = sinAttLat;

    satNorm      = sqrt(satX*satX + satY*satY + satZ*satZ);

    *squintAngle = acos((attX*satX + attY*satY + attZ*satZ)/satNorm) * CRD;
    *squintAngle = 0.0;
}

/******************************************************************************/
/*                                                                            */
/* getPathLoss: calculates path loss with                                     */
/*              frequency [Hz], range [km], velocity of light [km/s]          */
/*                                                                            */
/******************************************************************************/

void getPathLoss(Freq,Range,pathLoss)

double Freq, Range, *pathLoss;

{
    *pathLoss = 20.0 * log10(FOURPI * Range / CVAC * Freq);
}

/******************************************************************************/
/*                                                                            */
/* End of function block satcalc.c                                            */
/*                                                                            */
/******************************************************************************/
