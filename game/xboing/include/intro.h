#ifndef _INTRO_H_
#define _INTRO_H_

#include "copyright.h"

/*
 *  Dependencies on other include files:
 */

#include <X11/Xlib.h>

/*
 *  Constants and macros:
 */

/*
 *  Type declarations:
 */

enum IntroStates 
{ 
	INTRO_TITLE, 
	INTRO_BLOCKS, 
	INTRO_TEXT, 
	INTRO_EXPLODE, 
	INTRO_WAIT, 
	INTRO_FINISH 
};

/*
 *  Function prototypes:
 */

#if NeedFunctionPrototypes
void SetUpIntroduction(Display *display, Window window, Colormap colormap);
void Introduction(Display *display, Window window);
void RedrawIntroduction(Display *display, Window window);
void FreeIntroduction(Display *display);
void ResetIntroduction(void);
void DoIntroTitle(Display *display, Window window);
void DrawIntroTitle(Display *display, Window window, int x, int y);
#else
void DrawIntroTitle();
void SetUpIntroduction();
void Introduction();
void RedrawIntroduction();
void FreeIntroduction();
void ResetIntroduction();
void DoIntroTitle();
#endif

extern 	enum IntroStates IntroState;
extern 	Pixmap bigtitlePixmap, bigtitlePixmapM;
extern	Pixmap stars[12], starsM[12];


#endif
