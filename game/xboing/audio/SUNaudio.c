#include "include/copyright.h"

/* SUN Audio format - original code from play.c by Sun */

/*
 *  Include file dependencies:
 */

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <ctype.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/fcntl.h>
#include <sys/signal.h>

#if NeedFunctionPrototypes
extern void (*signal(int sig,
	void (*handler(int sig, int code,
    struct sigcontext *scp, char *addr))))();
extern unsigned int alarm(unsigned int seconds);
extern int open(char *path, int flags, int mode);
extern int read(int fd, char *buf, int nbyte);
extern int write(int fd, char *buf, int nbyte);
extern int close(int fd);
#else /* !NeedFunctionPrototypes */
extern void (*signal())();
extern unsigned int alarm();
extern int open();
extern int read();
extern int write();
extern int close();
#endif /* NeedFunctionPrototypes */

#include <libaudio.h>
#include <audio_device.h>

#if NeedFunctionPrototypes
extern int audio__setplayhdr(int F, struct Audio_hdr *H, unsigned int);
extern double audio__setgain(int F, double *V, unsigned int);
extern int audio__flush(int F, unsigned int);
extern int audio_drain(int F, int B);
extern int audio_read_filehdr(int F, struct Audio_hdr *H, char *buf, int);
extern int audio_cmp_hdr(struct Audio_hdr *H1, struct Audio_hdr *H2);
extern int audio_enc_to_str(struct Audio_hdr *H, char *buf);
#else /* !NeedFunctionPrototypes */
extern int audio__setplayhdr();
extern double audio__setgain();
extern int audio__flush();
extern int audio_drain();
extern int audio_read_filehdr();
extern int audio_cmp_hdr();
extern int audio_enc_to_str();
#endif /* NeedFunctionPrototypes */

#include "include/error.h"
#include "include/audio.h"

/*
 *  Internal macro definitions:
 */

#define	MAX_GAIN				(100)		/* maximum gain */
#define	SAMPLE_RATE_THRESHOLD	(.01)
#define BUFFER_SIZE				(1024 * SBUF_SIZE)	

/*
 *  Internal type declarations:
 */

#if NeedFunctionPrototypes
static void flushAudioDevice(void);
static int reconfig(void);
static void sigalarm_handler(void);
#else
static void sigalarm_handler();
static void flushAudioDevice();
static int reconfig();
#endif

/*
 *  Internal variable declarations:
 */

Audio_hdr		Dev_hdr;			/* audio header for device */
Audio_hdr		File_hdr;			/* audio header for file */

unsigned char	buf[BUFFER_SIZE];	/* size should depend on sample_rate */
double			Savevol;			/* saved volume level */
double			maxVolume;			/* maximum volume level */
char			*Audio_dev = "/dev/audio";
int				Audio_fd = -1;		/* file descriptor for audio device */
int 			err;
struct stat     st;
double          vol;
int             cnt;
char			errorString[255];

#if NeedFunctionPrototypes
int SetUpAudioSystem(Display *display)
#else
int SetUpAudioSystem(display)
	Display *display;
#endif
{
	/* Validate and open the audio device */
	if (stat(Audio_dev, &st) < 0)
	{
		/* No audio device so barf */
		sprintf(errorString, "Cannot stat audio device %s.", Audio_dev);
		ErrorMessage(errorString);
		return False;
	}

	/* Check that the audio device is a character device */
	if (!S_ISCHR(st.st_mode)) 
	{
		/* Not a character device which is not right */
		sprintf(errorString, "%s is not an audio device.", Audio_dev);
		ErrorMessage(errorString);
		return False;
	}

	/*
	 * We open the device without the O_NDELAY flag so that someone else
	 * who has the device can release it to us.  If we use the O_NDELAY
	 * flag, the open will fail even if the process that has the device is
	 * willing to release it.  Unfortunately, an ill-behaved process may not
	 * be willing to give up the device.  In this case, the open will hang.
	 * To prevent this, we wrap an alarm around the open.  If the open hangs
	 * and the alarm goes off, the open will return with errno = EINTR.
	 * suggestion by: jra@hrcms.jazz.att.com (Jeffry R. Abramson)
	 */
	if (signal(SIGALRM, sigalarm_handler) == (void*)SIG_NOADDR) 
	{
		ErrorMessage("signal(SIGALRM) failed.");
		return False;
	}

	alarm(2);
	/* Try to open audio device */
	Audio_fd = open(Audio_dev, O_WRONLY);
	alarm(0);

	if ((Audio_fd < 0) && (errno == EBUSY || errno == EINTR)) 
	{
		/* The audio is in use so barf */
		sprintf(errorString, "%s audio device is busy.", Audio_dev);
		ErrorMessage(errorString);
		return False;
	}

	/* Ok so we cannot open it for some reason */
	if (Audio_fd < 0) 
	{
		sprintf(errorString, "Cannot open audio device %s.", Audio_dev);
		ErrorMessage(errorString);
		return False;
	}

	/* Get the device output encoding configuration */
	if (audio_get_play_config(Audio_fd, &Dev_hdr) != AUDIO_SUCCESS) 
	{
		sprintf(errorString, "%s is not an audio device.", Audio_dev);
		ErrorMessage(errorString);
		return False;
	}

	/* Get the previous volume so we can reset it later */
	(void) audio_get_play_gain(Audio_fd, &Savevol);

	/* Clear any active audio for the new sound */
	flushAudioDevice();

	/* Success in opening audio device */
	return True;
}

#if NeedFunctionPrototypes
void FreeAudioSystem(void)
#else
void FreeAudioSystem()
#endif
{
	/*
	 * Though drain is implicit on close(), it's performed here
	 * for the sake of completeness, and to ensure that the volume
	 * is reset after all output is complete.
	 */
	(void) audio_drain(Audio_fd, FALSE);

	/* Reset the volume to the saved volume */
	(void) audio_set_play_gain(Audio_fd, &Savevol);
	
	/* Close the audio device thanks */
	(void) close(Audio_fd);
}

#if NeedFunctionPrototypes
static void flushAudioDevice(void)
#else
static void flushAudioDevice()
#endif
{
	/* Flush any audio activity */
	(void) audio_flush_play(Audio_fd);
}

#if NeedFunctionPrototypes
void SetMaximumVolume(int Volume)
#else
void SetMaximumVolume(Volume)
	int Volume;
#endif
{
	/* Set the maximum volume for the audio system */
	maxVolume = (double) (Volume / 100.0);
}

#if NeedFunctionPrototypes
int GetMaximumVolume(void)
#else
int GetMaximumVolume()
#endif
{
	/* Return the maximum volume as a % of 100 */
    return (int) ((double) maxVolume * 100.0);
}

#if NeedFunctionPrototypes
static void setNewVolume(unsigned int Volume)
#else
static void setNewVolume(Volume)
	unsigned int Volume;
#endif
{
	double oldVol, modVol;

	/* Use the saved volume as maximum or use the maxvolume set by user */
	if (maxVolume == 0.0)
		(void) audio_get_play_gain(Audio_fd, &maxVolume);

	oldVol = (maxVolume * MAX_GAIN) / 100;
	modVol = Volume * oldVol;

	/* volume is double value between 0 and 1 */
	vol = modVol / (double) MAX_GAIN;
	err = audio_set_play_gain(Audio_fd, &vol);

	/* Check if the volume was set correctly */
	if (err != AUDIO_SUCCESS) 
	{
		/* Nup - issue an error message */
		sprintf(errorString, "Unable to set output volume for %s\n",
		    Audio_dev);
		ErrorMessage(errorString);
	}
}

#if NeedFunctionPrototypes
void playSoundFile(char *filename, int volume)
#else
void playSoundFile(filename, volume)
	char *filename;
	int volume;
#endif
{
	int ifd;
	char soundfile[1024];
	char *str;

	/* Clear any active audio for the new sound */
	flushAudioDevice();

	/* Set to the required volume */
 	setNewVolume((unsigned) volume);

	/* Construct the sounds file path and use env var if exists */
    if ((str = getenv("XBOING_SOUND_DIR")) != NULL)
    	sprintf(soundfile, "%s/%s.au", str, filename);
    else            
        sprintf(soundfile, "%s/%s.au", SOUNDS_DIR, filename);

	/* Open the sound file for reading */
	if ((ifd = open(soundfile, O_RDONLY, 0)) < 0) 
	{
		/* Issue an error about not opening sound file */
		sprintf(errorString, "Unable to open sound file %s.", soundfile);
		WarningMessage(errorString);
		return;
	}

	/* Check to make sure this is an audio file */
	err = audio_read_filehdr(ifd, &File_hdr, (char *)NULL, 0);
	if (err != AUDIO_SUCCESS) 
	{
		/* Cannot understand the sound file so close file and return */
		sprintf(errorString, "Unable to parse sound file %s.", soundfile);
		WarningMessage(errorString);
		goto closeinput;
	}

	/* Check the device configuration */
	if (audio_cmp_hdr(&Dev_hdr, &File_hdr) != 0) 
	{
		/*
		 * The device does not match the input file.
		 * Wait for any old output to drain, then attempt
		 * to reconfigure the audio device to match the
		 * input data.
		 */
		if (audio_drain(Audio_fd, FALSE) != AUDIO_SUCCESS) 
		{
			/* Cannot drain the audio device so barf */
			sprintf(errorString, "Unable to drain the audio device.");
			ErrorMessage(errorString);
			return;
		}

		if (!reconfig()) goto closeinput;
	}

	/* At this point, we're all ready to copy the data. */
	while ((cnt = read(ifd, (char *) buf, BUFFER_SIZE)) >= 0) 
	{
		/* If input EOF, write an eof marker */
		err = write(Audio_fd, (char *)buf, cnt);
		if (err != cnt) 
		{
			/* Did we succeed in writing all the sound out */
			sprintf(errorString, 
				"Problem while writing to audio device.");
			WarningMessage(errorString);
			break;
		}

		if (cnt == 0) break;
	}

	if (cnt < 0) 
	{
		/* Some error - while reading - notify user */
		sprintf(errorString, "Problem while reading soundfile %s", soundfile);
		WarningMessage(errorString);
	}

closeinput:
	/* Close the sound file */
	(void) close(ifd);
}

#if NeedFunctionPrototypes
void audioDeviceEvents(void)
#else
void audioDeviceEvents()
#endif
{
	/* None to do */
}

#if NeedFunctionPrototypes
static int reconfig(void)
#else
static int reconfig()
#endif
{
	/*
	 * Try to reconfigure the audio device to match the file encoding.
	 * If this fails, we should attempt to make the input data match the
	 * device encoding.  For now, we give up on this file.
	 *
	 * Returns TRUE if successful.  Returns FALSE if not.
	 */

	char	msg[AUDIO_MAX_ENCODE_INFO];

	Dev_hdr = File_hdr;
	err = audio_set_play_config(Audio_fd, &Dev_hdr);

	switch (err) 
	{
		case AUDIO_SUCCESS:
			return (TRUE);

		case AUDIO_ERR_NOEFFECT:
			/*
			 * Couldn't change the device.
			 * Check to see if we're nearly compatible.
			 * audio_cmp_hdr() returns >0 if only sample rate difference.
			 */
			if (audio_cmp_hdr(&Dev_hdr, &File_hdr) > 0) 
			{
				double	ratio;
	
				ratio = (double) abs((int)
				    (Dev_hdr.sample_rate - File_hdr.sample_rate)) /
				    (double) File_hdr.sample_rate;
	
				if (ratio <= SAMPLE_RATE_THRESHOLD) 
					return (TRUE);
	
				return (FALSE);
			}
	
			(void) audio_enc_to_str(&File_hdr, msg);
	
			return (FALSE);
	
		default:
			sprintf(errorString, "I/O error (set config) audio device.");
			ErrorMessage(errorString);
	}
	return(FALSE);
}

#if NeedFunctionPrototypes
static void sigalarm_handler(void)
#else
static void sigalarm_handler()
#endif
{
	/* Used to fix problem when opening the audio device */
	return;
}
