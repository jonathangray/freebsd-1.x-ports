#ifndef _INST_H_
#define _INST_H_

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

enum InstructStates 
{ 
	INSTRUCT_TITLE, 
	INSTRUCT_TEXT, 
	INSTRUCT_SPARKLE, 
	INSTRUCT_WAIT, 
	INSTRUCT_FINISH 
};

/*
 *  Function prototypes:
 */

#if NeedFunctionPrototypes
void SetUpInstructions(Display *display, Window window, Colormap colormap);
void Instructions(Display *display, Window window);
void RedrawInstructions(Display *display, Window window);
void FreeInstructions(Display *display);
void ResetInstructions(void);
#else
void SetUpInstructions();
void Instructions();
void RedrawInstructions();
void FreeInstructions();
void ResetInstructions();
#endif

extern enum InstructStates InstructState;

#endif
