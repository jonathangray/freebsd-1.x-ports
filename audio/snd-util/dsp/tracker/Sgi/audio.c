/* sgi/audio.c 
	vi:se ts=3 sw=3:
 */

/* $Id: audio.c,v 1.1 1994/02/19 16:03:14 ache Exp $
 * $Log: audio.c,v $
 * Revision 1.1  1994/02/19 16:03:14  ache
 * Initial revision
 *
 * Revision 4.0  1994/01/11  18:01:04  espie
 * Changed name.
 *
 * Revision 1.1  1993/12/26  00:55:53  Espie
 * Initial revision
 *
 * Revision 3.8  1993/12/04  16:12:50  espie
 * BOOL -> boolean.
 *
 * Revision 3.7  1993/11/11  20:00:03  espie
 * Amiga support.
 *
 * Revision 3.6  1993/07/14  16:33:41  espie
 * Added stuff.
 *
 * Revision 3.5  1993/05/09  14:06:03  espie
 * Corrected mix problem.
 *
 * Revision 3.4  1992/11/27  10:29:00  espie
 * General cleanup
 *
 * Revision 3.3  1992/11/24  10:51:19  espie
 * Added pseudo discardbuffer.
 *
 * Revision 3.2  1992/11/22  17:20:01  espie
 * Checks for finetune ?
 *
 * Revision 3.1  1992/11/19  20:44:47  espie
 * Protracker commands.
 *
 * Revision 3.0  1992/11/18  16:08:05  espie
 * New release.
 *
 * Revision 2.11  1992/11/17  15:38:00  espie
 * Dummy discard_buffer()
 * Changed sync_audio value again.
 * Added synchro for dump.
 * Bug fix: must ask new frequency after we tried to set it to get it
 * rounded up.
 * Added stereo option (kind of).
 * Separated mix/stereo stuff.
 * Checked buffer size.
 * Added possibility to get back to MONO for the sgi.
 * Added stereo capabilities to the indigo version.
 * Ask the frequency to the audio device.
 * Corrected bug: when closing audio,
 * we now wait for the samples queue to be empty.
 */

#include <audio.h>
#include <malloc.h>
#include <stdio.h>
#include "defs.h"
#include "extern.h"

XT int sginap(long ticks);
     
ID("$Id: audio.c,v 1.1 1994/02/19 16:03:14 ache Exp $")

LOCAL signed short *buffer;
LOCAL int index;

LOCAL int number;
LOCAL boolean sync = FALSE;

LOCAL ALport audio;
LOCAL ALconfig config;

LOCAL boolean donotwait = FALSE;
LOCAL long chpars[] = {AL_OUTPUT_RATE, 0};

LOCAL int stereo;  /* are we playing stereo or not ? */
/* 256th of primary/secondary source for that side. */
LOCAL int primary, secondary;

void set_mix(percent)
int percent;
    {
    percent *= 256;
    percent /= 100;
    primary = percent;
    secondary = 512 - percent;
    }

int open_audio(f, s)
int f, s;
    {

	donotwait = FALSE;
    chpars[1] = f;
    if (f != 0)
        ALsetparams(AL_DEFAULT_DEVICE, chpars, 2);
    ALgetparams(AL_DEFAULT_DEVICE, chpars, 2);
    config = ALnewconfig();
    stereo = s;
    if (stereo)
        {
        ALsetchannels(config, AL_STEREO);
        number = 2;
        }
    else
        {
        ALsetchannels(config, AL_MONO);
        number = 1;
        }
    ALsetwidth(config, AL_SAMPLE_16);
    audio = ALopenport("soundtracker mono", "w", config);
    index = 0;
    buffer = malloc(sizeof(signed short) * number * chpars[1]);
    return chpars[1];
    }

void set_synchro(s)
boolean s;
	{
	sync = s;
	}

int update_frequency()
	{
	int oldfreq;

	oldfreq = chpars[1];
	ALgetparams(AL_DEFAULT_DEVICE, chpars, 2);
	if (chpars[1] != oldfreq)
		{
		buffer = realloc(buffer, sizeof(signed short) * number * chpars[1]);
		return chpars[1];
		}
	else
		return 0;
	}


void output_samples(int left, int right)
    {
    if (stereo)
        {
        buffer[index++] = (left * primary + right * secondary)/256;
        buffer[index++] = (right * primary + left * secondary)/256;
        }
    else
        buffer[index++] = left + right;
    }

void flush_buffer(void)
    {
    ALwritesamps(audio, buffer, index);
	if (sync)
		while(ALgetfilled(audio) > index * 10)
			/* busy wait */
			;
    index = 0;
    }

void discard_buffer(void)
	{
	donotwait = TRUE;
	/* mostly not implemented, only working when using close_audio
	 * right after
	 */
	}

void close_audio(void)
    {
	if (!donotwait)
		{
		while(ALgetfilled(audio) != 0)
			sginap(1);
		}
    ALcloseport(audio);
    ALfreeconfig(config);
    free(buffer);
    }
