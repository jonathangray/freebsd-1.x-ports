/* sparc/audio.c 
	vi:se ts=3 sw=3:
 */

/* $Id: audio.c,v 1.1 1994/02/19 16:03:14 ache Exp $
 * $Log: audio.c,v $
 * Revision 1.1  1994/02/19 16:03:14  ache
 * Initial revision
 *
 * Revision 4.2  1994/01/13  09:19:08  espie
 * Forgotten something.
 *
 * Revision 4.0  1994/01/11  18:16:36  espie
 * New release.
 *
 * Revision 1.2  1993/12/26  18:54:21  Espie
 * Handle errors better.
 *
 * Revision 1.1  1993/12/26  00:55:53  Espie
 * Initial revision
 *
 * Revision 3.14  1993/12/04  16:12:50  espie
 * BOOL -> boolean.
 *
 * Revision 3.13  1993/12/02  15:45:33  espie
 * Merged ss10/solaris.
 *
 * Revision 3.12  1993/11/27  17:07:33  espie
 * Merged support for solaris together.
 *
 * Revision 3.11  1993/11/17  15:31:16  espie
 * *** empty log message ***
 *
 *
 * Revision 3.9  1993/07/14  16:33:41  espie
 * Fixed /16 bug.
 *
 * Revision 3.8  1993/05/09  14:06:03  espie
 * Corrected mix problem.
 *
 * Revision 3.6  1992/12/03  15:00:50  espie
 * restore stty.
 *
 * Revision 3.4  1992/11/24  10:51:19  espie
 * Sync pseudo call.
 *
 * Revision 3.3  1992/11/22  17:20:01  espie
 * Added update_frequency call, mostly unchecked
 *
 * Revision 3.2  1992/11/20  14:53:32  espie
 * Added finetune.
 *
 * Revision 3.1  1992/11/19  20:44:47  espie
 * Protracker commands.
 *
 * Revision 3.0  1992/11/18  16:08:05  espie
 * New release.
 *
 * Revision 1.3  1992/11/17  15:38:00  espie
 * discard_buffer() call for snappier interface calls.
 * - Unified support for all sparcs.
 * - moved down to level 2 io.
 */

#include <stdio.h>
#include "defs.h"
#include "extern.h"
#ifdef SOLARIS
#include <sys/audio.io.h>
#else
#include <sun/audioio.h>
#endif
#include <sys/ioctl.h>
#include <fcntl.h>
#include <stropts.h>
#include <malloc.h>
     
/* things that aren't defined in all sun/audioio.h */

#ifndef AUDIO_ENCODING_LINEAR
#define AUDIO_ENCODING_LINEAR (3)
#endif
#ifndef AUDIO_GETDEV
#define AUDIO_GETDEV	_IOR(A, 4, int)
#endif
#ifndef AUDIO_DEV_UNKNOWN
#define AUDIO_DEV_UNKNOWN (0)
#endif
#ifndef AUDIO_DEV_AMD
#define AUDIO_DEV_AMD (1)
#endif

ID("$Id: audio.c,v 1.1 1994/02/19 16:03:14 ache Exp $")

LOCAL int audio;

LOCAL struct audio_info ainfo;
LOCAL char *buffer;
LOCAL short *sbuffer;
LOCAL int index;
LOCAL int dsize;

LOCAL int stereo;
LOCAL int primary, secondary;

void set_mix(percent)
int percent;
    {
	percent *= 256;
	percent /= 100;
	primary = percent;
	secondary = 512 - percent;
    }

#define abs(x) ((x) < 0 ? -(x) : (x))

LOCAL int available(f)
int f;
	{
	static int possible[] = { 8000, 9600, 11025, 16000, 18900, 22050, 32000,
		37800, 44100, 48000, 0};
	int best = 0;
	int i;

	for (i = 0; possible[i]; i++)
		if (abs(possible[i] - f) < abs(best - f))
			best = possible[i];
	return best;
	}

int open_audio(f, s)
int f;
int s;
    {
	int type;
#ifdef SOLARIS
	audio_device_t dev;

	audio = open("/dev/audio", O_WRONLY);
#else
    audio = open("/dev/audio", O_WRONLY|O_NDELAY);
#endif
    if (audio == -1)
		end_all("Error: could not open audio");
	if (f == 0)
		f = 22050;
		/* round frequency to acceptable value */
	f = available(f);

		/* check whether we know about AUDIO_ENCODING_LINEAR */
#ifdef SOLARIS
	ioctl(audio, AUDIO_GETDEV, &dev);
	if (strcmp(dev.name, "SUNW,dbri") != 0)
#else
	if (ioctl(audio, AUDIO_GETDEV, &type) ||
	type == AUDIO_DEV_UNKNOWN || type == AUDIO_DEV_AMD)
#endif
		{
			/* not a ss 10 -> revert to base quality audio */
		stereo = 0;
		dsize = 1;
		ainfo.play.sample_rate = 8000;
		ainfo.play.encoding = AUDIO_ENCODING_ULAW;
		ainfo.play.channels = 1;
		}
	else
		{
			/* tentative set up */
		stereo = s;
		AUDIO_INITINFO(&ainfo);
		ainfo.play.sample_rate = f;
		ainfo.play.precision = 16;
		dsize = 2;
		if (stereo)
			{
			ainfo.play.channels = 2;
			}
		else
			ainfo.play.channels = 1;
			/* try it */
		ainfo.play.encoding = AUDIO_ENCODING_LINEAR;
		if (ioctl(audio, AUDIO_SETINFO, &ainfo) != 0)
			/* didn't work: fatal problem */
			end_all("Error: AUDIO_SETINFO");
		}
	index = 0;
	buffer = (char *)malloc(dsize * ainfo.play.channels * ainfo.play.sample_rate);
	sbuffer = (short *) buffer;
	if (!buffer)
		end_all("Error: could not allocate buffer");
	return ainfo.play.sample_rate;
    }

void set_synchro(s)
boolean s;
	{
	}

int update_frequency()
	{
	int oldfreq;

	oldfreq = ainfo.play.sample_rate;
	if (ioctl(audio, AUDIO_GETINFO, &ainfo) == 0)
		{
		if (oldfreq != ainfo.play.sample_rate)
			{
			buffer = realloc(buffer, 
				dsize * ainfo.play.channels * ainfo.play.sample_rate);
			sbuffer = (short *)buffer;
			return ainfo.play.sample_rate;
			}
		}
	return 0;
	}


LOCAL int sign(x)
unsigned char x;
    {
    return x;
    }

/************************************************************************/
/*      For routine 'cvt' only                                          */
/************************************************************************/
/*      Copyright 1989 by Rich Gopstein and Harris Corporation          */
/************************************************************************/

LOCAL unsigned int cvt(ch)
int ch;
    {
    int mask;

    if (ch < 0)
        {
        ch = -ch;
        mask = 0x7f;
        }
    else
        mask = 0xff;

    if (ch < 32)
        {
        ch = 0xF0 | 15 - (ch / 2);
        }
    else if (ch < 96)
        {
        ch = 0xE0 | 15 - (ch - 32) / 4;
        }
    else if (ch < 224)
        {
        ch = 0xD0 | 15 - (ch - 96) / 8;
        }
    else if (ch < 480)
        {
        ch = 0xC0 | 15 - (ch - 224) / 16;
        }
    else if (ch < 992)
        {
        ch = 0xB0 | 15 - (ch - 480) / 32;
        }
    else if (ch < 2016)
        {
        ch = 0xA0 | 15 - (ch - 992) / 64;
        }
    else if (ch < 4064)
        {
        ch = 0x90 | 15 - (ch - 2016) / 128;
        }
    else if (ch < 8160)
        {
        ch = 0x80 | 15 - (ch - 4064) /  256;
        }
    else
        {
        ch = 0x80;
        }
    return (mask & ch);
    }


void output_samples(left, right)
int left, right;
    {
	if (stereo)
		{
		sbuffer[index++] = (left * primary + right * secondary)/256;
		sbuffer[index++] = (right * primary + left * secondary)/256;
		}
	else
		switch(ainfo.play.encoding)
			{
		case AUDIO_ENCODING_LINEAR:
			sbuffer[index++] = left + right;
			break;
		case AUDIO_ENCODING_ULAW:
			buffer[index++] = cvt((left + right) /4);
			break;
			}
    }

void flush_buffer()
    {
	int actual;

	actual = write(audio, buffer, dsize * index);
	if (actual == -1)
		notice("Write to audio failed");
	else if (actual != dsize * index)
		notice("Short write to audio");
	index = 0;
    }

void discard_buffer()
	{
	ioctl(audio, I_FLUSH, FLUSHW);
	}

void close_audio()
    {
	free(buffer);
    close(audio);
    }
