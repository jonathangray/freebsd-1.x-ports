#ifndef _SFX_H_
#define _SFX_H_

#include "copyright.h"

/*
 *  Dependencies on other include files:
 */

#include <X11/Xlib.h>

/*
 *  Constants and macros:
 */

#define SFX_NONE        0
#define SFX_SHAKE      	1
#define SFX_FADE  		2
#define SFX_BLIND  		3
#define SFX_SHATTER  	4

/*
 *  Type declarations:
 */

/*
 *  Function prototypes:
 */

#if NeedFunctionPrototypes
void SetSfxEndFrame(int endFrame);
void changeSfxMode(int newMode);
int oldSfxMode(void);
int currentSfxMode(void);
int WindowShakeEffect(Display *display, Window window);
int WindowShatterEffect(Display *display, Window window);
int WindowBlindEffect(Display *display, Window window);
int WindowFadeEffect(Display *display, Window window);
int getSpecialEffects(Display *display);
void useSpecialEffects(int state);
void ResetBorderGlow(Display *display, Window window);
void BorderGlow(Display *display, Window window);
#else
void ResetBorderGlow();
int WindowShatterEffect();
int WindowBlindEffect();
int WindowFadeEffect();
int WindowShakeEffect();
void BorderGlow();
void useSpecialEffects();
int getSpecialEffects();
void SetSfxEndFrame();
void changeSfxMode();
int oldSfxMode();
int currentSfxMode();
#endif


#endif
