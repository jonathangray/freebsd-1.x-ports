#include "include/copyright.h"

/*
 *  Include file dependencies:
 */

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <xpm.h>

#include "bitmaps/digit0.xpm"
#include "bitmaps/digit1.xpm"
#include "bitmaps/digit2.xpm"
#include "bitmaps/digit3.xpm"
#include "bitmaps/digit4.xpm"
#include "bitmaps/digit5.xpm"
#include "bitmaps/digit6.xpm"
#include "bitmaps/digit7.xpm"
#include "bitmaps/digit8.xpm"
#include "bitmaps/digit9.xpm"

#include "include/error.h"
#include "include/init.h"
#include "include/special.h"
#include "include/misc.h"
#include "include/main.h"

#include "include/score.h"

/*
 *  Internal macro definitions:
 */

#define NUM_DIGITS	10

/*
 *  Internal type declarations:
 */

/*
 *  Internal variable declarations:
 */

Pixmap	digitPixmaps[NUM_DIGITS];
Pixmap	digitPixmapsM[NUM_DIGITS];

u_long score = 0L;


#if NeedFunctionPrototypes
void InitialiseScoreDigits(Display *display, Window window, Colormap colormap)
#else
void InitialiseScoreDigits(display, window, colormap)
	Display *display;
	Window window;
	Colormap colormap;
#endif
{
	XpmAttributes   attributes;
	int 			XpmErrorStatus;

	attributes.valuemask = XpmColormap;
	attributes.colormap = colormap;

	/* Create all xpm pixmap digits from the files */
	XpmErrorStatus = XpmCreatePixmapFromData(display, window, digit0_xpm, 
		&digitPixmaps[0], &digitPixmapsM[0], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseScoreDigits()");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, digit1_xpm, 
		&digitPixmaps[1], &digitPixmapsM[1], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseScoreDigits()");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, digit2_xpm, 
		&digitPixmaps[2], &digitPixmapsM[2], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseScoreDigits()");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, digit3_xpm, 
		&digitPixmaps[3], &digitPixmapsM[3], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseScoreDigits()");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, digit4_xpm, 
		&digitPixmaps[4], &digitPixmapsM[4], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseScoreDigits()");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, digit5_xpm, 
		&digitPixmaps[5], &digitPixmapsM[5], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseScoreDigits()");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, digit6_xpm, 
		&digitPixmaps[6], &digitPixmapsM[6], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseScoreDigits()");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, digit7_xpm, 
		&digitPixmaps[7], &digitPixmapsM[7], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseScoreDigits()");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, digit8_xpm, 
		&digitPixmaps[8], &digitPixmapsM[8], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseScoreDigits()");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, digit9_xpm, 
		&digitPixmaps[9], &digitPixmapsM[9], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseScoreDigits()");

	/* Free the xpm pixmap attributes */
	XpmFreeAttributes(&attributes);
}

#if NeedFunctionPrototypes
static void DrawDigit(Display *display, Window window, int digit, int x, int y)
#else
static void DrawDigit(display, window, digit, x, y)
	Display *display;
	Window window;
	int digit;
	int x;
	int y;
#endif
{
	/* Draw the digit in the window */
	RenderShape(display, window, 
		digitPixmaps[digit], digitPixmapsM[digit], x, y, 30, 40, True);
}

#if NeedFunctionPrototypes
void DrawOutNumber(Display *display, Window window, u_long score, int x, int y)
#else
void DrawOutNumber(display, window, score, x, y)
	Display *display;
	Window window;
	u_long score;
	int x;
	int y;
#endif
{
	int digit;

	/* Get the digit that we want from the score */
	if (score / 10)
		DrawOutNumber(display, window, (score / 10), x - 32, y);

	/* Work out the digit needed to draw */
	digit = (int) (score % 10);

	DrawDigit(display, window, digit, x - 32, y);
}

#if NeedFunctionPrototypes
void AddToScore(u_long inc)
#else
void AddToScore(inc)
	u_long inc;
#endif
{
	/* Compute the score */
	score += ComputeScore(inc);
}

#if NeedFunctionPrototypes
u_long ComputeScore(u_long inc)
#else
u_long ComputeScore(inc)
    u_long inc;
#endif
{
    /* Take into account any score bonuses */
    if (x2Bonus == True)
        inc *= 2;
    else if (x4Bonus == True)
        inc *= 4;

    /* return the adjusted score for the speed of the game and inc */
    return (u_long) (((float) inc * ((float) GetWarpSpeed() / 9.0)));
}

#if NeedFunctionPrototypes
void DisplayScore(Display *display, Window window, u_long score)
#else
void DisplayScore(display, window, score)
	Display *display;
	Window window;
	u_long score;
#endif
{
	/* Erase the old score in the window */
	XClearWindow(display, window);

	/* Draw a zero if no score */
	if (score == 0L)
		RenderShape(display, window, 
			digitPixmaps[0], digitPixmapsM[0], 192, 0, 30, 40, True);
	else
		/* Draw the score digits rescursively */
		DrawOutNumber(display, window, score, 224, 0);
}

#if NeedFunctionPrototypes
void FreeScoreDigits(Display *display)
#else
void FreeScoreDigits(display)
	Display *display;
#endif
{
	int i;

	/* Free the memory associated with the digit pixmaps */
	for (i = 0; i < NUM_DIGITS; i++)
	{
		/* Free the digits pixmap and mask */
		if (digitPixmaps[i])	XFreePixmap(display, digitPixmaps[i]);
		if (digitPixmapsM[i])	XFreePixmap(display, digitPixmapsM[i]);
	}
}

