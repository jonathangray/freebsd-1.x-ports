#ifndef _PRESENT_H_
#define _PRESENT_H_

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

enum PresentStates 
{ 
	PRESENT_WAIT,
	PRESENT_FLAG,
	PRESENT_FINISH,
	PRESENT_LETTERS,
	PRESENT_SFX,
	PRESENT_SPECIAL_TEXT1,
	PRESENT_SPECIAL_TEXT2,
	PRESENT_CLEAR,
	PRESENT_SHINE,
	PRESENT_TEXT1,
	PRESENT_TEXT2,
	PRESENT_TEXT3,
	PRESENT_TEXT_CLEAR
};

/*
 *  Function prototypes:
 */

#if NeedFunctionPrototypes
void SetUpPresents(Display *display, Window window, Colormap colormap);
void Presents(Display *display, Window window);
void RedrawPresents(Display *display, Window window);
void FreePresents(Display *display);
void ResetPresents(void);
void QuickFinish(Display *display, Window window);
#else
void QuickFinish();
void SetUpPresents();
void Presents();
void RedrawPresents();
void FreePresents();
void ResetPresents();
#endif

extern 	enum PresentStates PresentState;

#endif
