/* $Id: ncdaudio.c,v 1.1 1994/02/23 14:40:05 jkh Exp $
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
/*
 * NCDaudio driver
 */

#ifdef SOUND
#include <stdlib.h>
#include <audio/audiolib.h>
#include <audio/soundlib.h>
#include "audio.h"

AuServer       *aud;
AuDeviceID      device;

int audioDeviceInit(char *display)
{
    int             i;

    if (!(aud = AuOpenServer(display, 0, NULL, 0, NULL, NULL)))
	return -1;

    for (i = 0; i < AuServerNumDevices(aud); i++)
	if ((AuDeviceKind(AuServerDevice(aud, i))
	     == AuComponentKindPhysicalOutput)
	    && AuDeviceNumTracks(AuServerDevice(aud, i)) == 1) {

	    device = AuDeviceIdentifier(AuServerDevice(aud, i));
	    break;
	}

    if (!device) {
	AuCloseServer(aud);
	return -1;
    }

#if defined(SOUNDLIB_VERSION) && SOUNDLIB_VERSION >= 2
    AuSoundRestartHardwarePauses = AuFalse;
#endif				/* defined(SOUNDLIB_VERSION) &&
				 * SOUNDLIB_VERSION >= 2 */

    return 0;
}


typedef struct
{
    int             playing;
    AuBucketID      bucket;
} audioRec, *audioPtr;

static void doneCB(AuServer * aud,
		   AuEventHandlerRec * handler,
		   AuEvent * event,
		   audioPtr info)
{
    info->playing = 0;
}

void audioDevicePlay(char *filename, int type, int volume, void **private)
{
    audioPtr       *info = (audioPtr *) private;

    if (!*info) {
	if (!(*info = (audioPtr) malloc(sizeof(audioRec))))
	    return;

	(*info)->playing = 0;
	(*info)->bucket
	    = AuSoundCreateBucketFromFile(aud, filename, AuAccessAllMasks,
					  NULL, NULL);
    }

    if ((*info)->bucket && (!(*info)->playing || type != FIRE_SHOT_SOUND)) {
	(*info)->playing = 1;
	AuSoundPlayFromBucket(aud, (*info)->bucket, device,
			      AuFixedPointFromFraction(volume, 100),
			      (void (*)) doneCB, (AuPointer) *info, 1, NULL,
			      NULL, NULL, NULL);
	AuFlush(aud);
    }
}

void audioDeviceEvents()
{
    if (aud)
	AuHandleEvents(aud);
}

#endif /* SOUND */
