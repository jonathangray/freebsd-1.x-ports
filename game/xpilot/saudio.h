/* $Id: saudio.h,v 1.1 1994/02/23 14:40:07 jkh Exp $
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
/* This piece of code was provided by Greg Renda (greg@ncd.com). */

#ifndef _saudio_h
#define _saudio_h

#ifndef SOUND

/*
 * Define like this to avoid having to put #ifdef SOUND all over the place.
 */
#define sound_player_init(player)
#define sound_play_player(player, index)
#define sound_play_all(index)
#define sound_play_sensors(x, y, index)
#define sound_play_queued(player)
#define sound_close(player)

#else						/* SOUND */

#include "audio.h"

int             sound_player_init(player *);
void            sound_play_player(player *, int);
void            sound_play_all(int);
void            sound_play_sensors(float, float, int);
void            sound_play_queued(player * pl);
void            sound_close(player * pl);

#endif						/* SOUND */

#endif						/* _saudio_h */
