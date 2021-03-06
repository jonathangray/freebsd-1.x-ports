/* soundblaster/audio.c 
	vi:se ts=3 sw=3:
 */
/* IMPORTANT NOTE: I can't check that this file works.
 */

/* $Id: audio.c,v 1.1 1994/02/19 16:03:14 ache Exp $
 * $Log: audio.c,v $
 * Revision 1.1  1994/02/19 16:03:14  ache
 * Initial revision
 *
 * Revision 4.0  1994/01/11  18:01:44  espie
 * *** empty log message ***
 *
 * Revision 1.2  1993/12/26  18:54:21  Espie
 * Handle errors better.
 *
 * Revision 1.1  1993/12/26  00:55:53  Espie
 * Initial revision
 *
 * Revision 3.4  1993/11/17  15:31:16  espie
 * *** empty log message ***
 *
 * Revision 3.3  1992/12/03  15:00:50  espie
 * restore stty.
 *
 * Revision 3.1  1992/11/19  20:44:47  espie
 * Protracker commands.
 *
 * Revision 3.0  1992/11/18  16:08:05  espie
 * New release.
 *
 * Revision 1.5  1992/11/17  15:38:00  espie
 * Dummy discard_buffer()
 * Added stereo option (kind of).
 */

#include <malloc.h>
#include <stdio.h>
#include "defs.h"
#include "extern.h"
#include <i386at/sblast.h>

ID("$Id: audio.c,v 1.1 1994/02/19 16:03:14 ache Exp $")

LOCAL unsigned char *buffer;/* buffer for ready-to-play samples */
LOCAL int buf_index;   /* can't call this index, conflicts with index(3) */
FILE *audio;            /* /dev/sb_dsp */

/* are we playing stereo or not ? */
LOCAL int stereo;
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
int f;
int s;
    {
    audio = fopen("/dev/sb_dsp", "w");
    if (!audio)
		end_all("Error opening audio device");

    stereo = s;
    if (ioctl(fileno(audio), DSP_IOCTL_STEREO, stereo) == -1)
    	end_all("Error setting stereo/mono");

    if (stereo)
        f *= 2;     /* XXX Stereo takes twice the speed */

    if (f == 0)
        f = -1;     /* read current frequency from driver */

    if (ioctl(fileno(audio), DSP_IOCTL_SPEED, &f) == -1)
    	end_all("Error setting frequency");

    buffer = malloc(sizeof(SAMPLE) * f);    /* Stereo makes x2 */
    buf_index = 0;

    if (stereo)         /* tacky, I know.. */
        return f/ 2;
    else
        return f;
    }

void output_samples(left, right)
int left, right;
    {
    if (stereo)
        {
        buffer[buf_index++] = (((left * primary + right * secondary) / 256)
             + (1 << 15)) >> 8;
        buffer[buf_index++] = (((right * primary + left * secondary) / 256)
             + (1 << 15)) >> 8;
        }
    else
        buffer[buf_index++] = (left + right + (1 << 15)) >> 8;
    }

void discard_buffer()
	{
	/* not implemented */
	}

void flush_buffer()
    {
    if (fwrite(buffer, sizeof(*buffer), buf_index, audio) != buf_index)
		notice("fwrite didn't write all the bytes ?");
    buf_index = 0;
    }

/*
 * Closing the BSD SBlast sound device waits for all pending samples to play.
 * I think SysV aborts, so you might have to flush manually with ioctl()
 */
void close_audio()
    {
    fclose(audio);
    free(buffer);
    }

int update_frequency()
	{
	/* not implemented */
	return 0;
	}

void set_synchro()
	{
	/* not implemented */
	}
