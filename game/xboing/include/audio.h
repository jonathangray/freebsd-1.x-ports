#ifndef _AUDIO_H_
#define _AUDIO_H_

#include "copyright.h"

/*
 *  Dependencies on other include files:
 */

#include <X11/Xlib.h>

/*
 *  Constants and macros:
 */

#define SBUF_SIZE	32

/*
 *  Type declarations:
 */

/*
 *  Function prototypes:
 */

#if NeedFunctionPrototypes
int	 SetUpAudioSystem(Display *display);
void FreeAudioSystem(void);
void playSoundFile(char *filename, int volume);
void audioDeviceEvents(void);
void SetMaximumVolume(int Volume);
int GetMaximumVolume(void);
#else
int GetMaximumVolume();
void SetMaximumVolume();
void audioDeviceEvents();
int  SetUpAudioSystem();
void FreeAudioSystem();
void playSoundFile();
#endif

#endif
