#ifndef _DEMO_H_
#define _DEMO_H_

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

enum DemoStates 
{ 
	DEMO_TITLE, 
	DEMO_BLOCKS, 
	DEMO_TEXT, 
	DEMO_SPARKLE, 
	DEMO_WAIT, 
	DEMO_FINISH 
};

/*
 *  Function prototypes:
 */

#if NeedFunctionPrototypes
void SetUpDemonstration(Display *display, Window window, Colormap colormap);
void Demonstration(Display *display, Window window);
void RedrawDemonstration(Display *display, Window window);
void FreeDemonstration(Display *display);
void ResetDemonstration(void);
void DoDemoTitle(Display *display, Window window);
#else
void SetUpDemonstration();
void Demonstration();
void RedrawDemonstration();
void FreeDemonstration();
void ResetDemonstration();
void DoDemoTitle();
#endif

extern 	enum DemoStates DemoState;


#endif
