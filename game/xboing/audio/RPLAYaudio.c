#include "include/copyright.h"

/* RPLAYaudio.c - Use rplay to play sounds over network. Original source by
 *                Joel J. Fleck II - <joel@cc.bellcore.com> 
 * 				- I haven't the time to test this code. Should work though.
 */

/*
 *  Include file dependencies:
 */

#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stddef.h>
#include <ctype.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/fcntl.h>

#include <rplay.h>

#include "include/error.h"
#include "include/main.h"
#include "include/audio.h"

/*
 *  Internal macro definitions:
 */

/*
 *  Internal type declarations:
 */

/*
 *  Internal variable declarations:
 */

static RPLAY   	*rp;
static int 		rplay_fd;
static char 	errorString[255];

#if NeedFunctionPrototypes
int SetUpAudioSystem(Display *display)
#else
int SetUpAudioSystem(display)
	Display *display;
#endif
{
	char hostname[256];
   	char display_machine[256];
    char *displayname = XDisplayString(display);

	/* Obtain the systems hostname */
    gethostname(hostname, 256);

    if (strncmp(hostname, displayname, strlen(hostname)) != 0) 
	{
		/* Obtain the server name etc for rplay */
    	strncpy(display_machine, displayname, strcspn(displayname,":"));

		/* Connect to the rplay sound server */
        rplay_fd = rplay_open(display_machine);
    } 
	else 
	{
		/* Connect to the rplay sound server */
        rplay_fd = rplay_open("localhost");
    }

    if ( rplay_fd == -1 ) 
	{
		/* Error while trying to connect to rplay sound server */
    	rplay_perror("SetUpAudioSystem()");
		ErrorMessage("Error: Cannot connect to rplay sound server.");
        return False;
    } 
	else 
        return True;
}

#if NeedFunctionPrototypes
void FreeAudioSystem(void)
#else
void FreeAudioSystem()
#endif
{
	/* Close the rplay sound server */
    rplay_close(rplay_fd);
    rplay_destroy(rp);
}

#if NeedFunctionPrototypes
static void flushAudioDevice(void)
#else
static void flushAudioDevice()
#endif
{
	/* This is done when the sound is sent by setting the priority to highest
	 * for every sound - I hope this works. JCK
	 */
}

#if NeedFunctionPrototypes
void setNewVolume(unsigned int Volume)
#else
void setNewVolume(Volume)
	unsigned int Volume;
#endif
{
	/* How do you do thism with rplay? */
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
    char soundfile[1024];
    char *str;

    /* Clear any active audio for the new sound */
    flushAudioDevice();

    /* Construct the sounds file path and use env var if exists */
    if ((str = getenv("XBOING_SOUND_DIR")) != NULL)
        sprintf(soundfile, "%s/%s.au", str, filename);
    else
        sprintf(soundfile, "%s/%s.au", SOUNDS_DIR, filename);

	/* Create a connection to rplay server */
	if ((rp = rplay_create(RPLAY_PLAY)) == NULL) 
	{
		/* Cannot play sound for some reason */
    	rplay_perror("playSoundFile()");
		ErrorMessage(errorString);
    } 
	else 
	{
		/* Volume for rplay is 1 to 255 instead of 1 to 100 */
        volume += 50;

		/* Setup the sound for rplay */
        rplay_set(rp, RPLAY_APPEND, RPLAY_SOUND, soundfile, RPLAY_VOLUME, 
			volume, RPLAY_PRIORITY, 255, NULL);

		/* Send the rplay packet */
        rplay(rplay_fd, rp);
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

