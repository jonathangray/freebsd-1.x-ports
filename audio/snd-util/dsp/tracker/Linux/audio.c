/* linux/audio.c 
	vi:se ts=3 sw=3:
 */
/* minor mods for pl14 by Mike Battersby */
/* Modified from soundblaster_audio.c by Hannu Savolainen */
/* hsavolai@cs.helsinki.fi */

#include "defs.h"
#ifdef MALLOC_NOT_IN_STDLIB
#include <malloc.h>
#else
#include <stdlib.h>
#endif
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include "extern.h"

#ifdef PL_14
/* For some reason my pl14 kernel had no sys/soundcard.h (???) */
#include "/usr/src/linux/drivers/sound/soundcard.h"
#else
#ifndef __386BSD__
/*	This should be sys/soundcard.h	*/
#include <sys/soundcard.h>
#else
#include <machine/soundcard.h>
#endif
#endif

ID("$Id: audio.c,v 1.1 1994/02/19 16:03:14 ache Exp $")

LOCAL unsigned char *buffer;	/* buffer for ready-to-play samples */
LOCAL short *buffer16;			/* Sure this isn't unsigned short ? */
LOCAL int buf_index;   			/* index conflicts with index(3) */
LOCAL int buf_max;
LOCAL int audio;           	/* /dev/dsp */


LOCAL int stereo;					/* are we playing stereo or not ? */
/* 256th of primary/secondary source for that side. */
LOCAL int primary=512, secondary=0;
LOCAL int dsp_samplesize = 16; /* must be 8 or 16 */

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
    audio = open("/dev/dsp", O_WRONLY, 0);
    if (audio == -1)
		end_all("Error opening audio device");

    if (ioctl(audio, SNDCTL_DSP_SAMPLESIZE, &dsp_samplesize) == -1)
    	end_all("Error setting sample size");

    stereo = s;

    if (ioctl(audio, SNDCTL_DSP_STEREO, &stereo) == -1)
    	end_all("Error setting stereo/mono");

    if (f==0) f = 44100;

    if (ioctl(audio, SNDCTL_DSP_SPEED, &f) == -1)
		end_all("Error setting frequency");

    if (ioctl (audio, SNDCTL_DSP_GETBLKSIZE, &buf_max) == -1)
	  end_all("Error getting buffsize");

    buffer = malloc(buf_max);
    buffer16 = (short *)buffer;
    buf_index = 0;

	return f;
    }

LOCAL void actually_flush_buffer()
    {
    int l,i;

    l = sizeof(*buffer) * buf_index;
    if (dsp_samplesize !=8) l *= 2;
    write(audio, buffer, l);

    buf_index = 0;
    }

void output_samples(left, right)
int left, right;
    {
    if (dsp_samplesize != 8)	/* Cool! 16 bits/sample */
    {
	    if (stereo)
	        {
	        if (buf_index * 2 >= buf_max - 1) 
	           actually_flush_buffer();

	        buffer16[buf_index++] = 
	           ((left*primary + right*secondary) / 256);
	        buffer16[buf_index++] = 
	           ((right*primary + left*secondary) / 256);
	        }
	    else
	        {
	        if (buf_index * 2 >= buf_max) 
	           actually_flush_buffer();
	        buffer16[buf_index++] = (left + right);
	        }
    }
    else
    {
	    if (stereo)
	        {
	        if (buf_index >= buf_max - 1) 
				actually_flush_buffer();
	        buffer[buf_index++] = ((left*primary + right*secondary) >> 16)
	             + 128;
	        buffer[buf_index++] = ((right*primary + left*secondary) >> 16)
	             + 128;
	        }
	    else
	        {
	        if (buf_index >= buf_max) 
				actually_flush_buffer();
	        buffer[buf_index++] = ((left + right) >> 8) + 128;
	        }
	    }
    }

void flush_buffer()
    {	/* Dummy version */
    }

/*
 * Closing the Linux sound device waits for all pending samples to play.
 */
void close_audio()
    {
    actually_flush_buffer();
    close(audio);
    free(buffer);
    }

/* dummy system calls, to patch ? */
void set_synchro(s)
	{
	}

int update_frequency()
	{
	return 0;
	}

void discard_buffer()
	{
	}

