/* $Id: score.h,v 1.1 1994/02/23 14:40:07 jkh Exp $
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

#ifndef SCORE_H
#define SCORE_H

#define ED_SHOT			(-0.2*FUEL_SCALE_FACT)
#define ED_SMART_SHOT		(-30*FUEL_SCALE_FACT)
#define ED_MINE			(-60*FUEL_SCALE_FACT)
#define ED_ECM			(-60*FUEL_SCALE_FACT)
#define ED_TRANSPORTER		(-60*FUEL_SCALE_FACT)
#define ED_SHIELD		(-0.20*FUEL_SCALE_FACT)
#define ED_CLOAKING_DEVICE	(-0.07*FUEL_SCALE_FACT)
#define ED_SHOT_HIT		(-25.0*FUEL_SCALE_FACT)
#define ED_SMART_SHOT_HIT	(-120.0*FUEL_SCALE_FACT)
#define ED_PL_CRASH		(-100.0*FUEL_SCALE_FACT)
#define ED_LASER		(-10.0*FUEL_SCALE_FACT)
/* was 90 -> 2 -> 40 -> 20 -> 10 */
#define ED_LASER_HIT		(-50.0*FUEL_SCALE_FACT)
/* was 120 -> 80 -> 40 -> 50 */

#define PTS_PR_PL_KILL	    	13	/* Points if you kill a player */
#define PTS_PR_CANNON_KILL  	1	/* Points if you smash a cannon */
#define PTS_PR_CRASH	    	-2	/* Points if you crash in a object */
#define PTS_PR_PL_CRASH	    	-4	/* Points is you crash in a player */
#define PTS_PR_SHOT	    	-2	/* Points if you get shot */
#define PTS_PR_PL_SHOT	    	-2    	/* Points if you get shot by a player */
#define PTS_GAME_WON	    	20	/* Points if you win the game */
#define PTS_SUICIDE	    	-25	/* Points if you are #$@ enough to */
					/* commit suicide. */

#define CANNON_SCORE	    	-500
#define WALL_SCORE	    	2000

#define RATE_SIZE	    	20
#define RATE_RANGE	    	1024
#endif
