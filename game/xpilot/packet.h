/* $Id: packet.h,v 1.1 1994/02/23 14:40:06 jkh Exp $
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

#ifndef PACKET_H
#define PACKET_H

#include "const.h"

#define KEYBOARD_SIZE		5

/*
 * Definition of various client/server packet types.
 */
#define PKT_UNDEFINED		0
#define PKT_VERIFY		1
#define PKT_REPLY		2
#define PKT_PLAY		3
#define PKT_QUIT		4
#define PKT_MESSAGE		5
#define PKT_START		6
#define PKT_END			7
#define PKT_SELF		8
#define PKT_DAMAGED		9
#define PKT_CONNECTOR		10
#define PKT_REFUEL		11
#define PKT_SHIP		12
#define PKT_ECM			13
#define PKT_PAUSED		14
#define PKT_ITEM		15
#define PKT_MINE		16
#define PKT_BALL		17
#define PKT_SMART		18
#define PKT_SHUTDOWN		19
#define PKT_STRING		20
#define PKT_DESTRUCT		21
#define PKT_RADAR		22
#define PKT_TARGET		23
#define PKT_KEYBOARD		24
#define PKT_SEEK		25
#define PKT_NODELAY		26
#define PKT_SEND_BUFSIZE	27
#define PKT_PLAYER		28
#define PKT_SCORE		29
#define PKT_FUEL		30
#define PKT_BASE		31
#define PKT_CANNON		32
#define PKT_LEAVE		33
#define PKT_POWER		34
#define PKT_POWER_S		35
#define PKT_TURNSPEED		36
#define PKT_TURNSPEED_S		37
#define PKT_TURNRESISTANCE	38
#define PKT_TURNRESISTANCE_S	39
#define PKT_WAR			40
#define PKT_MAGIC		41
#define PKT_RELIABLE		42
#define PKT_ACK			43
#define PKT_HEADER		44
#define PKT_TRANS		45
#define PKT_ACK_CANNON		46
#define PKT_ACK_FUEL		47
#define PKT_ACK_TARGET		48
#define	PKT_SCORE_OBJECT	49
#define PKT_AUDIO		50
#define PKT_TALK		51
#define PKT_TALK_ACK		52
#define PKT_TIME_LEFT		53
#define PKT_LASER		54
#define PKT_DISPLAY		55
#define PKT_EYES		56
#define PKT_SHOT		60		/* + color number */
#define PKT_FAILURE		101
#define PKT_SUCCESS		102
#define PKT_DEBRIS		128		/* + color + x + y */

#endif

