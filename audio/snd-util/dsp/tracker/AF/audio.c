/* AF/audio.c
	vi:se ts=3 sw=3:

This code written by:

Andrew "Alf" Leahy                         email: alf@st.nepean.uws.edu.au
University of Western Sydney - Nepean.
Sydney, Australia.                         phone: (047) 360622 (work)


Modified by Marc Espie to adjust to tracker 4.0 API.

 */

#include <stdio.h>
#include "defs.h"
#include "extern.h"
#include "/usr/local/include/AF/AFlib.h"
ID("$Id: audio.c,v 1.1 1994/02/19 16:03:09 ache Exp $")

/* 0 external handset, 1 for internal speaker */
#define SPEAKER 0
     
LOCAL int stereo, primary, secondary;

LOCAL char *buffer, *buffer_r;
LOCAL int nbytes=0, nbytes_r=0, ssize;

LOCAL ATime t, t_r, act, act_r;
LOCAL AC ac, ac_r;
LOCAL AFAudioConn *aud, *aud_r;

int sample_sizes[] = {
	1,	/* MU255 */
	1,	/* ALAW */
	2,	/* Linear PCM, 16 bits, -1.0 <= x < 1.0 */
	2,	/* Linear PCM, 32 bits, -1.0 <= x < 1.0 */
	1,	/* G.721, 64Kbps to/from 32Kbps. */
	1,	/* G.723, 64Kbps to/from 32Kbps. */
	0
};

int open_audio(int f, int s)
{
	AFSetACAttributes attributes;
	int srate, device;
	unsigned int channels;
	AEncodeType type;
	char *server;

	device = SPEAKER;
	attributes.preempt = Mix;
	attributes.start_timeout = 0;
	attributes.end_silence = 0;
	attributes.play_gain = 0;
	attributes.rec_gain =  0;

	if ((server = (char *) getenv("AUDIOFILE")) == NULL)
		end_all("Error: AUDIOFILE unset");
	else
	{
		server = (char *) getenv("AUDIOFILE");
		if ((aud = AFOpenAudioConn( server )) == NULL)
			end_all("Error: can't open connection");
		ac = AFCreateAC(aud, device, ACPlayGain, &attributes);
		srate = ac->device->playSampleFreq;
		type = ac->device->playBufType;
		channels = ac->device->playNchannels;
		ssize = sample_sizes[type] * channels;

		if ((buffer = (char *)malloc(ssize * srate)) == NULL)
			end_all("Couldn't allocate play buffer");

		t = AFGetTime(ac);
	}

	stereo=s;

	if (stereo)
	{
		server = (char *) getenv("AUDIORIGHT");
		if ((aud = AFOpenAudioConn(server)) == NULL)
			end_all("Error: can't open connection");
		ac_r = AFCreateAC(aud, device, ACPlayGain, &attributes);
		srate = ac->device->playSampleFreq;
		type = ac->device->playBufType;
		channels = ac->device->playNchannels;
		ssize = sample_sizes[type] * channels;

		if ((buffer_r = (char *)malloc(ssize * srate)) == NULL)
			end_all("Couldn't allocate play buffer");
		t_r = AFGetTime(ac_r);
	}

	return srate;
}

void set_mix(int percent)
{
	percent *= 256;
	percent /= 100;
	primary = percent;
	secondary = 512 - percent;
}

void set_synchro(boolean s)
{
}

int update_frequency()
{
	return 0;
}

LOCAL unsigned int cvt(int ch)
{
    int mask;

    if (ch < 0)
	{
        ch = -ch;
        mask = 0x7f;
	}
    else
        mask = 0xff;

    if (ch < 32)        ch = 0xF0 | 15 - (ch / 2);
    else if (ch < 96)   ch = 0xE0 | 15 - (ch - 32) / 4;
    else if (ch < 224)  ch = 0xD0 | 15 - (ch - 96) / 8;
    else if (ch < 480)  ch = 0xC0 | 15 - (ch - 224) / 16;
    else if (ch < 992)  ch = 0xB0 | 15 - (ch - 480) / 32;
    else if (ch < 2016) ch = 0xA0 | 15 - (ch - 992) / 64;
    else if (ch < 4064) ch = 0x90 | 15 - (ch - 2016) / 128;
    else if (ch < 8160) ch = 0x80 | 15 - (ch - 4064) /  256;
    else ch = 0x80;

    return (mask & ch);
}

void output_samples(int left, int right)
{
	if (stereo)
		if (primary) /* mixing needed */
		{
			buffer[nbytes++] = cvt((left * primary + right * secondary) >>10);
			buffer_r[nbytes_r++] = cvt((right * primary + left * secondary) >>10);
		}
		else /* no mixing */
		{
			buffer[nbytes++] = cvt(left >>2);
			buffer_r[nbytes_r++] = cvt(right >>2);
		}
	else /* mono */
		buffer[nbytes++] = cvt((left + right) >>2);
}

void flush_buffer()
{
	act = AFPlaySamples(ac, t, nbytes, buffer);
	t += nbytes/ssize;
	nbytes = 0;

	if (stereo) 
	{
		act_r = AFPlaySamples(ac_r, t_r, nbytes_r, buffer_r);
		t_r += nbytes_r/ssize;
		nbytes_r = 0;
	}
}

void discard_buffer()
{
}

void close_audio()
{
	free(buffer);

/* Alf: I'm not sure whether these functions are needed
        I think these are Seg Faulting... */

 	(void) AFCloseAudioConn(aud);

	if (stereo)
		(void) AFCloseAudioConn(aud_r);
}
