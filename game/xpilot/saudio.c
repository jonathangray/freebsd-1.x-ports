/* $Id: saudio.c,v 1.1 1994/02/23 14:40:07 jkh Exp $
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

#define SERVER
#include <stdlib.h>
#include "global.h"
#include "netserver.h"

#define SOUND_RANGE_FACTOR	0.5		/* factor to increase sound
						 * range by */
#define SOUND_DEFAULT_RANGE	(BLOCK_SZ*15)
#define SOUND_MAX_VOLUME	100
#define SOUND_MIN_VOLUME	10

#define sound_range(pl) \
	(SOUND_DEFAULT_RANGE + pl->sensors\
	 * SOUND_DEFAULT_RANGE * SOUND_RANGE_FACTOR)

typedef struct _AudioQRec
{
    int             index,
                    volume;
    struct _AudioQRec *next;
} AudioQRec, *AudioQPtr;


static void queue_audio(player * pl, int index, int volume)
{
    AudioQPtr       a, p, prev;

    if (!(a = (AudioQPtr) malloc(sizeof(AudioQRec))))
	return;

    a->index = index;
    a->volume = volume;
    a->next = NULL;

    p = prev = (AudioQPtr)pl->audio;

    while (p) {
	prev = p;
	p = p->next;
    }

    if (prev)
	prev->next = a;
    else
	pl->audio = (void *) a;
}

int sound_player_init(player * pl)
{
    pl->audio = NULL;

    return 0;
}

/*
 * Play a sound for a player.
 */
void sound_play_player(player * pl, int index)
{
    if (pl->conn != NOT_CONNECTED)
	queue_audio(pl, index, 100);
}

/*
 * Play a sound for all players.
 */
void sound_play_all(int index)
{
    int i;

    for (i = 0; i < NumPlayers; i++)
	sound_play_player(Players[i], index);
}

/*
 * Play a sound if location is within player's sound range. A player's sound
 * range depends on the number of sensors they have. The default sound range
 * is what the player can see on the screen. A volume is assigned to the
 * sound depending on the location within the sound range.
 */
void sound_play_sensors(float x, float y, int index)
{
    int             i,
                    volume;
    float           dx,
                    dy,
                    range,
                    factor;
    player         *pl;

    for (i = 0; i < NumPlayers; i++) {
	pl = Players[i];

	if (pl->conn == NOT_CONNECTED)
	    continue;

	dx = ABS(pl->pos.x - x);
	dy = ABS(pl->pos.y - y);
	range = sound_range(pl);

	if (dx >= 0 && dx <= range && dy >= 0 && dy <= range) {
	    /*
	     * scale the volume
	     */
	    factor = MAX(dx, dy) / range;
	    volume = MAX(SOUND_MAX_VOLUME - SOUND_MAX_VOLUME * factor,
			 SOUND_MIN_VOLUME);
	    queue_audio(pl, index, volume);
	}
    }
}

void sound_play_queued(player * pl)
{
    AudioQPtr       p,
                    n;

    p = (AudioQPtr)pl->audio;
    pl->audio = NULL;

    while (p) {
	n = p->next;
	Send_audio(pl->conn, p->index, p->volume);
	free(p);
	p = n;
    }
}

void sound_close(player * pl)
{
    AudioQPtr       p,
                    n;

    p = (AudioQPtr)pl->audio;
    pl->audio = NULL;

    while (p) {
	n = p->next;
	free(p);
	p = n;
    }
}
