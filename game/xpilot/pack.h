/* $Id: pack.h,v 1.1 1994/02/23 14:40:06 jkh Exp $
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

#ifndef	PACK_H
#define	PACK_H

#include "socklib.h"
#include "const.h"
#include "types.h"

#define CAP_LETTER(c)	(c = (c>='a' && c<='z') ? c-'a'+'A' : c)

#define SERVER_PORT	15345		/* Port which server listens to. */
#define META_PORT	5500
#define META_HOST	"xpilot.cs.uit.no"
#define META_IP		"129.242.16.101"

/*
 * Magic contact word.
 * The low 16 bits are the real magic word.
 * Bits 31-28 are the major version number.
 * Bits 27-24 are the minor version number.
 * Bits 23-20 are the patchlevel number.
 * Bits 19-16 are free to mean beta release or so.
 * These high bits only need to be changed when a new
 * client can't talk to an old server or vise versa.
 * Please don't change it more often than strictly necessary.
 *
 * Reasons why it changed in the past:
 * 3.0.1: rewrite of contact pack protocol, because of
 * different structure layout rules on different architectures.
 * 3.0.2: rewrite of setup transmit from server to client to
 * make it possible for 64-bit machines and 32-bit machines
 * to join in the same game.  This was the last hardcoded
 * structure that was shared between client and server.
 * 3.0.3: implemented a version awareness system, so that
 * newer clients can join older servers and so that
 * newer servers can support older clients.
 * The client maintains a `version' variable indicating
 * the version of the server it has joined and the server
 * maintains for each connection a `connection_t->version'
 * and a `player->version' variable.
 * 3.0.4: the so-called `pizza-mode' introduced a new packet type.
 * The score packet now also includes pl->mychar.
 * 3.0.4.1: new laser weapon introduces another packet change.
 * Because there is an unofficial (and forbidden) 3.0.4 version floating
 * around the sub patchlevel number is used to distinguish versions.
 * A new display packet to tell the server what the view sizes are
 * and how many different debris intensities the client wants.
 * 3.0.4.2: new player-self status byte in self packet.
 * 3.0.4.3: different and incompatible laser packet.
 * New eyes packet to tell the client through wich players eyes we're
 * looking in case the client is in game over move and it is locked
 * on someone else.
 */
#define	MAGIC		0x3043F4ED

#define MAGIC2VERSION(M)	(((M) >> 16) & 0xFFFF)
#define VERSION2MAGIC(V)	((((V) & 0xFFFF) << 16) | (MAGIC & 0xFFFF))
#define MY_VERSION		MAGIC2VERSION(MAGIC)

/*
 * Which client versions can join this server.
 */
#define MIN_CLIENT_VERSION	0x3020
#define MAX_CLIENT_VERSION	MY_VERSION

/*
 * Which server versions can this client join.
 */
#define MIN_SERVER_VERSION	0x3020
#define MAX_SERVER_VERSION	MY_VERSION

#define	MAX_STR_LEN	4096
#define	MAX_ARG_LEN	256
#define	MAX_DISP_LEN	80
#define	MAX_NAME_LEN	16

/*
 * Different contact pack types.
 */
#define	REPLY_pack		0x10
#define	ENTER_GAME_pack		0x00
#define	REPORT_STATUS_pack	0x21
#define	LOCK_GAME_pack		0x22
#define	MESSAGE_pack		0x23
#define	SHUTDOWN_pack		0x24
#define	KICK_PLAYER_pack	0x25
#define	MAX_ROBOT_pack		0x26
#define	CORE_pack		0x30
#define	CONTACT_pack		0x31

/*
 * Possible error codes returned.
 */
#define	SUCCESS		0x00		/* Operation successful */
#define	E_NOT_OWNER	0x01		/* Permission denied, not owner */
#define	E_GAME_FULL	0x02		/* Game is full, play denied */
#define	E_TEAM_FULL	0x03		/* Team is full, play denied */
#define	E_TEAM_NOT_SET	0x04		/* Need to specify a team */
#define	E_GAME_LOCKED	0x05		/* Game is locked, entry denied */
#define	E_NOT_FOUND	0x07		/* Player was not found */
#define	E_IN_USE	0x08		/* Name is already in use */
#define	E_SOCKET	0x09		/* Can't setup socket */
#define	E_INVAL		0x0A		/* Invalid input parameters */
#define	E_VERSION	0x0C		/* Incompatible version */

#endif
