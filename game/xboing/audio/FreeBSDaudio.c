#include "include/copyright.h"

/* Soundblaster Audio - PC LINUX - original code by 
 * Peter C. Ludwig, email: urpc01@ux1.uni-dortmund.de 
 */

/*
 *  Include file dependencies:
 */

#include <stdio.h>
#include <fcntl.h>
#include <machine/soundcard.h>

#include "include/error.h"
#include "include/audio.h"

/*
 *  Internal macro definitions:
 */

#define BUFFER_SIZE				(1024 * SBUF_SIZE)

/*
 *  Internal type declarations:
 */

/*
 *  Internal variable declarations:
 */

static char				*Audio_dev = "/dev/audio";
static int 				Audio_fd;
/* size should depend on sample_rate */
static unsigned char   	buf[BUFFER_SIZE];       
static char 			errorString[255];
static int 				parentid = 0;

#if NeedFunctionPrototypes
int SetUpAudioSystem(Display *display)
#else
int SetUpAudioSystem(display)
	Display *display;
#endif
{
	/* Try to open the audio device */
 	if (Audio_fd = open(Audio_dev, O_WRONLY))
  	{	
		/* Success - audio device opened */
  		return True;
  	}
 	else
  	{
		ErrorMessage("Cannot open audio device.");

		/* Bummer - cannot open audio device */
  		return False;
  	}
}

#if NeedFunctionPrototypes
void FreeAudioSystem(void)
#else
void FreeAudioSystem()
#endif
{
	/* Make sure that the audio device is flushed and reset */
 	ioctl(Audio_fd, SNDCTL_DSP_RESET, 0);

	/* Close the audio device */
 	if (close(Audio_fd) < 0)
		ErrorMessage("Cannot open audio device.");
}

#if NeedFunctionPrototypes
static void flushAudioDevice(void)
#else
static void flushAudioDevice()
#endif
{
 	/* Flush any audio activity */
 	if (ioctl(Audio_fd, SNDCTL_DSP_SYNC, 0) < 0)
  	{
  		sprintf(errorString, "Unable to flush audio device.");
  		WarningMessage(errorString);
  		return;
  	}
}

#if NeedFunctionPrototypes
void setNewVolume(unsigned int Volume)
#else
void setNewVolume(Volume)
	unsigned int Volume;
#endif
{
	/* Do nothing here as we don't have audio support */
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
void playSoundFile(char *filename, int volume)
#else
void playSoundFile(filename, volume)
	char *filename;
	int volume;
#endif
{
	int err, cnt, ifd;
	char soundfile[1024];
	char *str;

	if (parentid) kill(parentid, 9);
	parentid = getpid();

	if (fork() != 0)
	{
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

			exit(0);
		}

		/* At this point, we're all ready to copy the data. */
		while ((cnt = read(ifd, (char *) buf, BUFFER_SIZE)) >= 0) 
		{
			/* If input EOF, write an eof marker */
			err = write(Audio_fd, (char *)buf, cnt);

			if (err != cnt) 
			{
				sprintf(errorString, "Problem while writing to audio device");
				WarningMessage(errorString);
				break;
			}    

			/* End of file? */
			if (cnt == 0) break;
		}

		if (cnt < 0) 
		{
			/* Some error - while reading - notify user */
			sprintf(errorString, "Problem while reading soundfile %s", 
				soundfile);
			WarningMessage(errorString);
		}
	 
		flushAudioDevice();

		/* Close the sound file */
		(void) close(ifd);

		exit(0);
	}
}

#if NeedFunctionPrototypes
void SetMaximumVolume(int Volume)
#else
void SetMaximumVolume(Volume)
    int Volume;
#endif
{
}

#if NeedFunctionPrototypes
int GetMaximumVolume(void)
#else
int GetMaximumVolume()
#endif
{
    return 0;
}
