/* $Id: math.c,v 1.1 1994/02/23 14:40:05 jkh Exp $
 *
 * XPilot, a multiplayer gravity war game.  Copyright (C) 1991-93 by
 *
 *      Bjørn Stabell        (bjoerns@staff.cs.uit.no)
 *      Ken Ronny Schouten   (kenrsc@stud.cs.uit.no)
 *      Bert Gÿsbers         (bert@mc.bio.uva.nl)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/*
 * Actually used by SERVER and CLIENT but define SERVER
 * so to avoid X11 dependencies for the server
 */
#define SERVER
#include <stdlib.h>
#include <math.h>
#include "config.h"
#include "types.h"
#include "rules.h"
#include "object.h"
#include "map.h"
#include "draw.h"
#include "proto.h"


#define PTS_IN_SHIP 3

#ifndef	lint
static char sourceid[] =
    "@(#)$Id: math.c,v 1.1 1994/02/23 14:40:05 jkh Exp $";
#endif

float  	tbl_sin[TABLE_SIZE];
wireobj		ships[RES];


int mod(int x, int y)
{
    if (x >= y || x < 0)
	x = x - y*(x/y);

    if (x < 0)
	x += y;

    return x;
}


float findDir(float x, float y)
{
    float angle;

    if (x != 0.0 || y != 0.0)
	angle = atan2(y, x) / (2 * PI);
    else
	angle = 0.0;

    if (angle < 0)
	angle++;
    return angle * RES;
}


void Make_table(void)
{
    int i;

    for (i=0; i<TABLE_SIZE; i++)
	tbl_sin[i] = sin((2*PI) * ((float)i/TABLE_SIZE));
}


void Make_ships(void)
{
    int i, z;


    ships[0].pts=(position *)malloc(PTS_IN_SHIP*sizeof(position));
    ships[0].pts[0].x = 15; ships[0].pts[0].y =  0;
    ships[0].pts[1].x = -9; ships[0].pts[1].y =  8;
    ships[0].pts[2].x = -9; ships[0].pts[2].y = -8;
    ships[0].num_points=PTS_IN_SHIP;

    for (i=1; i<RES; i++) {
	ships[i].pts=(position *)malloc(PTS_IN_SHIP*sizeof(position));

	for (z=0; z<PTS_IN_SHIP; z++) {
	    ships[i].pts[z].x = tcos(i)*ships[0].pts[z].x
		- tsin(i)*ships[0].pts[z].y;
	    ships[i].pts[z].y = tsin(i)*ships[0].pts[z].x
		+ tcos(i)*ships[0].pts[z].y;
	}
    }
}


void Free_ships(void)
{
    int dir;

    for (dir=0; dir<RES; dir++)
	free(ships[dir].pts);
}
