#ifndef _MESS_H_
#define _MESS_H_

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

/*
 *  Function prototypes:
 */

#if NeedFunctionPrototypes
void InitialiseMessageSystem(Display *display, Window window, 
	Colormap colormap);
void FreeMessageSystem(Display *display);
void SetCurrentMessage(Display *display, Window window, 
	char *newMessage, int clear);
void DisplayCurrentMessage(Display *display, Window window);
#else
void InitialiseMessageSystem();
void FreeMessageSystem();
void SetCurrentMessage();
void DisplayCurrentMessage();
#endif

#endif
