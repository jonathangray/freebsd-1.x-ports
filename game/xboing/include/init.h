#ifndef _INIT_H_
#define _INIT_H_

#include "copyright.h"

/*
 *  Dependencies on other include files:
 */

#include <X11/Xlib.h>

/*
 *  Constants and macros:
 */

#define DEBUG(x)    if (debug == True) NormalMessage(x);

#define CURSOR_WAIT		1
#define CURSOR_PLUS		2
#define CURSOR_NONE		3

/*
 *  Type declarations:
 */

/*
 *  Function prototypes:
 */

#if NeedFunctionPrototypes
Display *InitialiseGame(char **argv, int argc);
void ShutDown(Display *display, int exitCode, char *message);
void GrabPointer(Display *display, Window window);
void UnGrabPointer(Display *display);
void ChangePointer(Display *display, Window window, int cursorState);
#else
void ChangePointer();
void GrabPointer();
void UnGrabPointer();
Display *InitialiseGame();
void ShutDown();
#endif

extern GC gc, gcxor, gcand, gcor, gcsfx;
extern XFontStruct *titleFont, *copyFont, *textFont, *dataFont;
extern int red, tann, yellow, green, white, black, blue, purple, reds[14];
extern int greens[14], grey50;
extern Colormap colormap;
extern int noSound, debug;

#endif
