#ifndef _KEYS_H_
#define _KEYS_H_

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

enum KeysStates 
{ 
	KEYS_TITLE, 
	KEYS_TEXT, 
	KEYS_SPARKLE, 
	KEYS_WAIT, 
	KEYS_FINISH 
};

/*
 *  Function prototypes:
 */

#if NeedFunctionPrototypes
void SetUpKeys(Display *display, Window window, Colormap colormap);
void Keys(Display *display, Window window);
void RedrawKeys(Display *display, Window window);
void FreeKeyControl(Display *display);
void ResetKeys(void);
#else
void SetUpKeys();
void Keys();
void RedrawKeys();
void FreeKeyControl();
void ResetKeys();
#endif

extern enum KeysStates KeysState;
extern Pixmap mouse, leftarrow, rightarrow;
extern Pixmap mouseM, leftarrowM, rightarrowM;

#endif
