/* $Id: item.h,v 1.1 1994/02/23 14:40:05 jkh Exp $
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

#ifndef ITEM_H
#define ITEM_H

#define ITEM_ENERGY_PACK	0
#define ITEM_WIDEANGLE_SHOT	1
#define ITEM_BACK_SHOT		2
#define ITEM_AFTERBURNER	3
#define ITEM_CLOAKING_DEVICE	4
#define ITEM_SENSOR_PACK	5
#define ITEM_TRANSPORTER	6
#define ITEM_TANK		7
#define ITEM_MINE_PACK		8
#define ITEM_ROCKET_PACK	9
#define ITEM_ECM		10
#define ITEM_LASER		11
#define NUM_ITEMS		12

/* Each item is ITEM_SIZE x ITEM_SIZE */
#define ITEM_SIZE		16

#define ITEM_TRIANGLE_SIZE	(5*ITEM_SIZE/7 + 1)

#endif /* ITEM_H */
