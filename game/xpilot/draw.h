/* $Id: draw.h,v 1.1 1994/02/23 14:40:05 jkh Exp $
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

#ifndef	DRAW_H
#define	DRAW_H

#include "types.h"

/*
 * The server supports only 4 colors, except for spark/debris, which
 * may have 8 different colors.
 */
#define NUM_COLORS	    4

#define BLACK		    0
#define WHITE		    1
#define BLUE		    2
#define RED		    3

/*
 * The minimum and maximum playing window sizes supported by the server.
 */
#define MIN_VIEW_SIZE	    512
#define MAX_VIEW_SIZE	    1024
#define DEF_VIEW_SIZE	    640

/*
 * Spark rand limits.
 */
#define MIN_SPARK_RAND	    0		/* Not display spark */
#define MAX_SPARK_RAND	    0x80	/* Always display spark */
#define DEF_SPARK_RAND	    0x55	/* 66% */

#define SMART_SHOT_LEN	    15
#define DSIZE		    4	    /* Size of diamond (on radar) */

#define MSG_DURATION	    1024
#define MSG_FLASH	    892

#define SCROLL_DELAY	    10
#define SCROLL_LEN	    100

#define TITLE_DELAY	    500
#define	UPDATE_SCORE_DELAY  (FPS)

#define CLOAK_FAILURE	    130

#ifndef NO_ROTATING_DASHES
#define NUM_DASHES	    2
#define DASHES_LENGTH	    12
#endif

#define HUD_SIZE	    90		    /* Size/2 of HUD lines */
#define HUD_OFFSET	    20		    /* Hud line offset */
#define FUEL_GAUGE_OFFSET   6
#define HUD_FUEL_GAUGE_SIZE (2*(HUD_SIZE-HUD_OFFSET-FUEL_GAUGE_OFFSET))

enum alignment_t { RIGHT, LEFT };

typedef struct {
    char txt[MSG_LEN];
    short len;
    short pixelLen;
    enum alignment_t alignment;
    long life;
} message_t;

typedef struct {			/* Defines wire-obj, i.e. ship */
    position	*pts;
    int		num_points;
} wireobj;

#define HavePlanes(d) (DisplayPlanes(d, DefaultScreen(d)) > 2)
#define HaveColor(d)							\
    (DefaultVisual(d, DefaultScreen(d))->class == PseudoColor		\
     || DefaultVisual(d, DefaultScreen(d))->class == GrayScale)

#define FRAC(py)	    ((int)((py) * 1024.0/768.0))

#endif
