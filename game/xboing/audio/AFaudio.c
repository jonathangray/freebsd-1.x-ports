#include "include/copyright.h"

/*
 * Audiofile format - original code from Andrew Leahy 
 *                    <A.Leahy@st.nepean.uws.edu.au> 
 */

/*
 *  Include file dependencies:
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "AFlib.h"

/* 
 * Internal macro definitions: 
 */

#define BUFFER_SIZE				(1024 * SBUF_SIZE)	
#define False 0
#define True 1
#define SPEAKER 0 /* 0 external handset, 1 for internal speaker */

/*
 *  Internal type declarations:
 */

#if NeedFunctionPrototypes
static void flushAudioDevice(void);
#else
static void flushAudioDevice();
#endif

/* 
 * Internal variable declarations: 
 */

int sample_sizes[] = 
{
    1,  /* MU255 */
    1,  /* ALAW */
    2,  /* Linear PCM, 16 bits, -1.0 <= x < 1.0 */
    2,  /* Linear PCM, 32 bits, -1.0 <= x < 1.0 */
    1,  /* G.721, 64Kbps to/from 32Kbps. */
    1,  /* G.723, 64Kbps to/from 32Kbps. */
    0
};

static char		*buf;
int 			err;
struct stat     st;
static int      nbytes;
char			errorString[255];
int				ssize;
static ATime			t, act, nact;
static AC				ac;
static AFAudioConn		*aud;

#if NeedFunctionPrototypes
int SetUpAudioSystem(Display *display)
#else
int SetUpAudioSystem(display)
	Display *display;
#endif
{
    AFSetACAttributes 	attributes;
    int 				srate, device;
    unsigned int 		channels;
    AEncodeType 		type;
    char 				*server;

    device 						= SPEAKER;
    attributes.preempt 			= Mix;
    attributes.start_timeout 	= 0;
    attributes.end_silence 		= 0;
    attributes.play_gain 		= 0;
    attributes.rec_gain 		= 0;

    if ((server = (char *) getenv("AUDIOFILE")) == NULL)
    {
        sprintf(errorString, 
			"The environment variable AUDIOFILE is not set.\n");
		ErrorMessage(errorString);
		return False;
    }
    else
    {
        if ((aud = AFOpenAudioConn(server)) == NULL) 
		{
            sprintf(errorString, "Cannot open a connection to audio server.\n");
			ErrorMessage(errorString);
            return False;
        }

        ac = AFCreateAC(aud, device, ACPlayGain, &attributes);
        srate 		= ac->device->playSampleFreq;
        type 		= ac->device->playBufType;
        channels 	= ac->device->playNchannels;
        ssize 		= sample_sizes[type] * channels;

        if ((buf = (char *) malloc(BUFFER_SIZE)) == NULL) 
		{
            sprintf(errorString, "Couldn't allocate a play buffer.\n");
			ErrorMessage(errorString);
            return False;
        }
    }
 
	/* Success in opening audio device */
	return True;
}

#if NeedFunctionPrototypes
static void flushAudioDevice(void)
#else
static void flushAudioDevice()
#endif
{
	/* How is this done in audiofile? */
}

#if NeedFunctionPrototypes
void FreeAudioSystem(void)
#else
void FreeAudioSystem()
#endif
{
	/* Close connection to the audiofile sound server */
	AFFreeAC(ac);
	AFCloseAudioConn(aud);

	/* Free memory buffer */
	if (buf != (char *) NULL) free(buf);
}

#if NeedFunctionPrototypes
void SetMaximumVolume(int Volume)
#else
void SetMaximumVolume(Volume)
	int Volume;
#endif
{
	/* Not needed */
}

#if NeedFunctionPrototypes
int GetMaximumVolume(void)
#else
int GetMaximumVolume()
#endif
{
	return 0;
}

#if NeedFunctionPrototypes
void playSoundFile(char *filename, int volume)
#else
void playSoundFile(filename, volume)
	char *filename;
	int volume;
#endif
{
	int fd;
	char soundfile[1024];
	char *str;

	/* Construct the sounds file path and use env var if exists */
    if ((str = (char *)getenv("XBOING_SOUND_DIR")) != NULL)
    	sprintf(soundfile, "%s/%s.au", str, filename);
    else            
        sprintf(soundfile, "%s/%s.au", SOUNDS_DIR, filename);

	/* Open the sound file for reading */
	if ((fd = open(soundfile, O_RDONLY, 0)) < 0) 
	{
		/* Issue an error about not opening sound file */
		sprintf(errorString, "Unable to open sound file %s.", soundfile);
		WarningMessage(errorString);
		return;
	}

	/* At this point, we're all ready to copy the data. */
	if ((nbytes = read(fd, (char *) buf, BUFFER_SIZE)) <= 0) 
		return;

	t = AFGetTime(ac); 
	
	do
	{
		nact = AFPlaySamples(ac, t, nbytes, buf);
		act = nact;
		t += nbytes;
	} while ((nbytes = read(fd, buf, BUFFER_SIZE)) != 0);

	/* Close the sound file */
	(void) close(fd);
}

#if NeedFunctionPrototypes
void audioDeviceEvents(void)
#else
void audioDeviceEvents()
#endif
{
    /* None to do */
}

