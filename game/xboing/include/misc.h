#ifndef _MISC_H_
#define _MISC_H_

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
void DrawText(Display *display, Window window, int x, int y, XFontStruct *font,
	int colour, char *text, int numChar);
void DrawLine(Display *display, Window window, int x, int y, int x2, int y2,
	int colour, int width);
void RenderShape(Display *display, Window window, Pixmap pixmap,
	Pixmap mask, int x, int y, int w, int h, int clear);
void DrawShadowCentredText(Display *display, Window window, XFontStruct *font,
	char *string, int y, int colour, int width);
void DrawShadowText(Display *display, Window window, XFontStruct *font,
	char *string, int x, int y, int colour);
int ColourNameToPixel(Display *display, Colormap colormap, char *colourName);
void FreeMisc(Display *display);
char *getUsersFullName(void);
char *GetHomeDir(void);
#else
char *GetHomeDir();
char *getUsersFullName();
void FreeMisc();
int ColourNameToPixel();
void DrawText();
void DrawLine();
void RenderShape();
void DrawShadowCentredText();
void DrawShadowText();
#endif

#endif
