#include "include/copyright.h"

/* NCD Audio format - original code by Dave Lemke <lemke@verbosa.ncd.com> */

/*
 *  Include file dependencies:
 */

#include <stdio.h>
#include "include/error.h"
#include "include/audio.h"
#include "include/init.h"

#include <audio/audiolib.h>
#include <audio/soundlib.h>

/*
 *  Internal variable declarations:
 */

#define	MAX_SOUNDS	64

AuServer   *aud = NULL;
AuDeviceID  device;
static int	audio_on = False;
static int  num_sounds = 0;
static char	errorString[255];

static struct 
{
    char       *name;
    char       *filename;
    void       *private;
} sound_table[MAX_SOUNDS];

typedef struct 
{
    int        playing;
    AuBucketID  bucket;
} audioRec, *audioPtr;

#if NeedFunctionPrototypes
int SetUpAudioSystem(Display *display)
#else
int SetUpAudioSystem(display)
    Display	*display;
#endif
{
    int         i;
    char       *displayname = DisplayString(display);

    if (audio_on)
		return True;

	/* try and connect to the NCD audio server */
    if (!(aud = AuOpenServer(displayname, 0, NULL, 0, NULL, NULL)))
	{
		ErrorMessage("Cannot connect to NCD audio server.");
		return False;
	}

	/* Look for an audio device that we can use */
    for (i = 0; i < AuServerNumDevices(aud); i++)
	{
		if ((AuDeviceKind(AuServerDevice(aud, i)) == 
			AuComponentKindPhysicalOutput) && 
			AuDeviceNumTracks(AuServerDevice(aud, i)) == 1) 
		{
	    	device = AuDeviceIdentifier(AuServerDevice(aud, i));
	    	break;
		}
	}

	/* Well we didn't get a device - all busy? */
    if (!device) 
	{
		ErrorMessage("Cannot obtain NCD audio device.");
		AuCloseServer(aud);
		return False;
    }

#if defined(SOUNDLIB_VERSION) && SOUNDLIB_VERSION >= 2
    AuSoundRestartHardwarePauses = AuFalse;
#endif		

	/* Success - we have an audio device */
    audio_on = True;
    return True;
}

#if NeedFunctionPrototypes
void FreeAudioSystem(void)
#else
void FreeAudioSystem()
#endif
{
    /* Turn off the connection to the au server */
	AuCloseServer(aud);
    audio_on = False;
}

#if NeedFunctionPrototypes
void setNewVolume(unsigned int Volume)
#else
void setNewVolume(Volume)
    unsigned int Volume;
#endif
{
    /* Do nothing here as we don't need too */
}

#if NeedFunctionPrototypes
static void doneCB(AuServer *aud, AuEventHandlerRec *handler, AuEvent *event,		audioPtr info)
#else
static void doneCB(aud, handler, event, info)
    AuServer   			*aud;
    AuEventHandlerRec 	*handler;
    AuEvent    			*event;
    audioPtr    		info;
#endif
{
    info->playing = False;
}

#if NeedFunctionPrototypes
void audioDevicePlay(char *filename, int volume, void **private)
#else
void audioDevicePlay(filename, volume, private)
    char	*filename;
    int     volume;
    void    **private;
#endif
{
    audioPtr   *info = (audioPtr *) private;
	char str[1024];

    if (!*info) 
	{
		if (!(*info = (audioPtr) malloc(sizeof(audioRec))))
		{
			sprintf(errorString, "Cannot malloc memory for sound file %s.", 
				filename);
			WarningMessage(errorString);
	    	return;
		}

		(*info)->playing = 0;
		(*info)->bucket = AuSoundCreateBucketFromFile(aud, filename, 
			AuAccessAllMasks, NULL, NULL);
    }
	
	sprintf(str, "Playing soundfile: %s\n", filename);
	DEBUG(str)

    if ((*info)->bucket && (!(*info)->playing)) 
	{
		(*info)->playing = True;
		AuSoundPlayFromBucket(aud, (*info)->bucket, device,
	    	AuFixedPointFromFraction(volume, 100),
		    (void (*)) doneCB, (AuPointer) * info, 1, NULL, NULL, NULL, NULL);
	
		/* Flush sound */
		AuFlush(aud);
    }
}

#if NeedFunctionPrototypes
void audioDeviceEvents(void)
#else
void audioDeviceEvents()
#endif
{
    if (aud) AuHandleEvents(aud);
}

#if NeedFunctionPrototypes
void playSoundFile(char *filename, int volume)
#else
void playSoundFile(filename, volume)
    char       *filename;
    int         volume;
#endif
{
    int         i;
    char        fbuf[1024];
    char        *str;

	/* Loop through the sound table looking for sound */
    for (i = 0; i < num_sounds; i++) 
	{
		if (!strcmp(sound_table[i].name, filename)) 
		{
			/* Yeah - already in sound table */
	    	break;
		}
    }

	/* Ok - not found so add it to the sound table */
    if (i == num_sounds) 
	{	
		/* new one - so add it to the table */
		sound_table[num_sounds].name = strdup(filename);

		/* Use the environment variable if it exists */
        if ((str = getenv("XBOING_SOUND_DIR")) != NULL)
            sprintf(fbuf, "%s/%s.au", str, filename);
        else            
            sprintf(fbuf, "%s/%s.au", SOUNDS_DIR, filename);

		sound_table[num_sounds].filename = strdup(fbuf);
		num_sounds++;
    }

    audioDevicePlay(sound_table[i].filename, volume, &sound_table[i].private);
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

