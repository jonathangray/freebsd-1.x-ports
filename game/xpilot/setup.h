/* $Id: setup.h,v 1.1 1994/02/23 14:40:08 jkh Exp $
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

#ifndef SETUP_H
#define SETUP_H

#include "const.h"

/*
 * Definitions to tell the client how the server has been setup.
 */

/*
 * If the high bit of a map block is set then the next block holds
 * the number of contiguous map blocks that have the same block type.
 */
#define SETUP_COMPRESSED	0x80

/*
 * Tell the client how and if the map is compressed.
 */
#define SETUP_MAP_ORDER_XY	1
#define SETUP_MAP_ORDER_YX	2
#define SETUP_MAP_UNCOMPRESSED	3

/*
 * Definitions for the map layout which permit a compact definition
 * of map data.
 */
#define SETUP_SPACE		0
#define SETUP_FILLED		1
#define SETUP_FILLED_NO_DRAW	2
#define SETUP_FUEL		3
#define SETUP_REC_RU		4
#define SETUP_REC_RD		5
#define SETUP_REC_LU		6
#define SETUP_REC_LD		7
#define SETUP_ACWISE_GRAV	8
#define SETUP_CWISE_GRAV	9
#define SETUP_POS_GRAV		10
#define SETUP_NEG_GRAV		11
#define SETUP_WORM_NORMAL	12
#define SETUP_WORM_IN		13
#define SETUP_WORM_OUT		14
#define SETUP_CANNON_UP		15
#define SETUP_CANNON_RIGHT	16
#define SETUP_CANNON_DOWN	17
#define SETUP_CANNON_LEFT	18
#define SETUP_SPACE_DOT		19
#define SETUP_TREASURE		20	/* + team number */
#define SETUP_BASE_LOWEST	30	/* lowest base number */
#define SETUP_BASE_UP		30	/* + team number */
#define SETUP_BASE_RIGHT	40	/* + team number */
#define SETUP_BASE_DOWN		50	/* + team number */
#define SETUP_BASE_LEFT		60	/* + team number */
#define SETUP_BASE_HIGHEST	69	/* highest base number */
#define SETUP_TARGET		70	/* + team number */
#define SETUP_CHECK		80	/* + check point number */

#define BLUE_UP			0x01
#define BLUE_RIGHT		0x02
#define BLUE_DOWN		0x04
#define BLUE_LEFT		0x08
#define BLUE_OPEN		0x10	/* diagonal botleft -> rightup */
#define BLUE_CLOSED		0x20	/* diagonal topleft -> rightdown */
#define BLUE_FUEL		0x30	/* when filled block is fuelstation */
#define BLUE_BELOW		0x40	/* when triangle is below diagonal */
#define BLUE_BIT		0x80	/* set when drawn with blue lines */

/*
 * Structure defining the server configuration, including the map layout.
 */
typedef struct {
    long		setup_size;		/* size including map data */
    long		map_data_len;		/* num. compressed map bytes */
    long		mode;			/* playing mode */
    short		lives;			/* max. number of lives */
    short		x;			/* width in blocks */
    short		y;			/* height in blocks */
    short		width;			/* width in pixels */
    short		height;			/* height in pixels */
    short		frames_per_second;	/* FPS */
    short		map_order;		/* row major or col major */
    char		name[MAX_CHARS];	/* name of map */
    char		author[MAX_CHARS];	/* name of author of map */
    unsigned char	map_data[4];		/* compressed map data */
    /* plus more mapdata here (HACK) */
} setup_t;

#ifndef NETSERVER_C
# ifdef FPS
#  error "FPS needs a different definition in the client"
#  undef FPS
# endif
# define FPS		(Setup->frames_per_second)

extern setup_t *Setup;

#endif

#endif

