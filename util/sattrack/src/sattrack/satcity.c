/******************************************************************************/
/*                                                                            */
/*  Title       : satcity.c                                                   */
/*  Author      : Manfred Bester, DL5KR                                       */
/*  Date        : 17Sep92                                                     */
/*  Last change : 07Dec92                                                     */
/*                                                                            */
/*  Synopsis    : Calculates distance and direction to nearest city.          */
/*                                                                            */
/******************************************************************************/

#include <stdio.h>
#include <math.h>

#include "satglobalsx.h"
#include "sattrack.h"

/******************************************************************************/
/*                                                                            */
/* ground track vector segments                                               */
/*                                                                            */
/******************************************************************************/

char *gndTrkVector[] = { "N", "NNE", "NE", "ENE",
                         "E", "ESE", "SE", "SSE",
                         "S", "SSW", "SW", "WSW",
                         "W", "WNW", "NW", "NNW" };

/******************************************************************************/
/*                                                                            */
/* getGroundTrack: calculates the distance and direction of the nearest city  */
/*                                                                            */
/******************************************************************************/

void getGroundTrack(latSat,lngSat,gndTrkDist,gndTrkDir,gndTrkCity)

double latSat, lngSat, *gndTrkDist;
char   *gndTrkDir, *gndTrkCity;

{
    double cityX, cityY, cityZ, sspX, sspY, sspZ, distX, distY, distZ;
    double northX, northY, northZ, eastX, eastY, eastZ;
    double vectX, vectY, latSite, lngSite, azi;
    double cosLatSite, sinLatSite, cosLngSite, sinLngSite;
    double cosLatSat, sinLatSat, cosLngSat, sinLngSat;

    int    dir;

    /* fixed location for now */

    latSite = 28.625*CDR;
    lngSite = 80.650*CDR;
    sprintf(gndTrkCity,"Kennedy Space Center FL");       /* max 36 characters */

    if (fabs(latSat) < 1.0e-6 && fabs(lngSat) < 1.0e-6)
    {
        latSat = latSite + 1.0e-6;
        lngSat = lngSite;
    }

    cosLatSite = cos(latSite);
    sinLatSite = sin(latSite);
    cosLngSite = cos(lngSite);
    sinLngSite = sin(lngSite);

    cosLngSat  = cos(lngSat);
    sinLngSat  = sin(lngSat);
    cosLatSat  = cos(latSat);
    sinLatSat  = sin(latSat);

    cityX  = cosLngSite*cosLatSite;
    cityY  = sinLngSite*cosLatSite;
    cityZ  = sinLatSite;

    sspX   = cosLngSat*cosLatSat;
    sspY   = sinLngSat*cosLatSat;
    sspZ   = sinLatSat;

    distX  = cityX - sspX;
    distY  = cityY - sspY;
    distZ  = cityZ - sspZ;

    northX = -cosLngSite*sinLatSite;
    northY = -sinLngSite*sinLatSite;
    northZ =  cosLatSite;

    eastX  = -cityY*northZ + cityZ*northY;
    eastY  = -cityZ*northX + cityX*northZ;
    eastZ  = -cityX*northY + cityY*northX;

    vectX  = northX*distX + northY*distY + northZ*distZ;
    vectY  = eastX*distX  + eastY*distY  + eastZ*distZ;

    if (vectX == 0.0)
        azi = (vectY > 0.0) ? HALFPI : THREEHALFPI;

    else
    {
        azi = atan(vectY/vectX);

        if (vectX < 0.0)
            azi += PI;

        if (vectX > 0.0 && vectY < 0.0)
            azi += TWOPI;
    }

    azi  = 180.0 - azi*CRD;

    if (azi < 0.0)
        azi += 360.0;
    
    dir = (int) ((azi + 11.25) / 22.5);

    if (dir >= 16)
        dir -= 16;
    
    sprintf(gndTrkDir,"%s",gndTrkVector[dir]);
    *gndTrkDist = EARTHRADIUS * sqrt(distX*distX + distY*distY + distZ*distZ);
    return;
}

/******************************************************************************/
/*                                                                            */
/* End of function block satcity.c                                            */
/*                                                                            */
/******************************************************************************/
