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

#include "include/error.h"
#include "include/misc.h"
#include "include/main.h"
#include "include/init.h"
#include "include/intro.h"
#include "include/highscore.h"
#include "include/audio.h"
#include "include/stage.h"
#include "include/mess.h"
#include "include/version.h"

#include "bitmaps/flag.xpm"
#include "bitmaps/earth.xpm"
#include "bitmaps/titleX.xpm"
#include "bitmaps/titleB.xpm"
#include "bitmaps/titleO.xpm"
#include "bitmaps/titleI.xpm"
#include "bitmaps/titleN.xpm"
#include "bitmaps/titleG.xpm"
#include "bitmaps/justin.xpm"
#include "bitmaps/kibell.xpm"
#include "bitmaps/presents.xpm"

#include "include/presents.h"

/*
 *  Internal macro definitions:
 */

#define GAP 10

/*
 *  Internal type declarations:
 */

#if NeedFunctionPrototypes
void SetPresentWait(int newMode, int waitFrame);
void DoPresentWait(void);
static void DoSparkle(Display *display, Window window);
#else
static void DoSparkle();
void SetPresentWait();
void DoPresentWait();
#endif

/*
 *  Internal variable declarations:
 */

static int nextFrame = 0;
static int endFrame = 0;
static int startFrame = 0;
enum PresentStates PresentState;
static Pixmap flagPixmap, flagPixmapM, earthPixmap, earthPixmapM;
static Pixmap titlePixmap[6], titlePixmapM[6];
static Pixmap justin, justinM, kibell, kibellM, presents, presentsM;
static int waitingFrame, waitMode;

#if NeedFunctionPrototypes
void SetUpPresents(Display *display, Window window, Colormap colormap)
#else
void SetUpPresents(display, window, colormap)
	Display *display;
	Window window;
	Colormap colormap;
#endif
{
	XpmAttributes   attributes;
	int             XpmErrorStatus;

    attributes.valuemask = XpmColormap;
	attributes.colormap = colormap;

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, flag_xpm,
		&flagPixmap, &flagPixmapM, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialisePresent(flag)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, titleX_xpm,
		&titlePixmap[0], &titlePixmapM[0], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialisePresent(titleX)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, titleB_xpm,
		&titlePixmap[1], &titlePixmapM[1], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialisePresent(titleB)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, titleO_xpm,
		&titlePixmap[2], &titlePixmapM[2], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialisePresent(titleO)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, titleI_xpm,
		&titlePixmap[3], &titlePixmapM[3], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialisePresent(titleI)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, titleN_xpm,
		&titlePixmap[4], &titlePixmapM[4], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialisePresent(titleN)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, titleG_xpm,
		&titlePixmap[5], &titlePixmapM[5], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialisePresent(titleG)");

	/* Justin kibell presents */

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, justin_xpm,
		&justin, &justinM, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialisePresent(justin)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, kibell_xpm,
		&kibell, &kibellM, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialisePresent(kibell)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, presents_xpm,
		&presents, &presentsM, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialisePresent(presents)");

	/* Create the earth pixmap */

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, earth_xpm,
		&earthPixmap, &earthPixmapM, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialisePresent(earth)");

    /* Free the xpm pixmap attributes */
	XpmFreeAttributes(&attributes);

	/* Setup intro to initial state */
	ResetPresents();
}

#if NeedFunctionPrototypes
static void DrawLetter(Display *display, Window window, int letter, 
	int x, int y)
#else
static void DrawLetter(display, window, letter, x, y)
	Display *display;
	Window window;
	int letter;
	int x, y;
#endif
{
	switch(letter)
	{
		case 0:
			RenderShape(display, window, titlePixmap[0], titlePixmapM[0],
				x, y, 71, 74, False);
			break;

		case 1:
			RenderShape(display, window, titlePixmap[1], titlePixmapM[1],
				x, y, 73, 74, False);
			break;

		case 2:
			RenderShape(display, window, titlePixmap[2], titlePixmapM[2],
				x, y, 83, 74, False);
			break;

		case 3:
			RenderShape(display, window, titlePixmap[3], titlePixmapM[3],
				x, y, 41, 74, False);
			break;

		case 4:
			RenderShape(display, window, titlePixmap[4], titlePixmapM[4],
				x, y, 85, 74, False);
			break;

		case 5:
			RenderShape(display, window, titlePixmap[5], titlePixmapM[5],
				x, y, 88, 74, False);
			break;

		default:
			ErrorMessage("Error: In default for DrawLetter()");
	}
}

#if NeedFunctionPrototypes
void DoPresentFlag(Display *display, Window window)
#else
void DoPresentFlag(display, window)
	Display *display;
	Window window;
#endif
{
	char string[80];
	int y, x;

	DEBUG("Drawing earth in presents mode.")

	x = ((MAIN_WIDTH + PLAY_WIDTH) / 2) - 35;

	/* Draw the flag bitmap */
	RenderShape(display, window, flagPixmap, flagPixmapM,
		x, 15, 71, 40, True);
	DrawShadowCentredText(display, window, textFont, 
		"Made in Australia", 
		65, white, PLAY_WIDTH + MAIN_WIDTH);

	x = ((MAIN_WIDTH + PLAY_WIDTH) / 2) - 200;
	RenderShape(display, window, earthPixmap, earthPixmapM,
		x, 93, 400, 400, False);

	y = MAIN_HEIGHT + PLAY_HEIGHT - 50;

	/* Construct a copyright message leaving space for the copyright circle */
	strcpy(string, "  Copyright 1993 Justin C. Kibell, All Rights Reserved");
	string[0] = 0xa9;  /* Copyright circle */
	DrawShadowCentredText(display, window, copyFont, string, y, white, 
		PLAY_WIDTH + MAIN_WIDTH);

	/* My email address for contact and fame :-) */
	DrawShadowCentredText(display, window, copyFont,
		"email: jck@citri.edu.au", y + 15, white, PLAY_WIDTH + MAIN_WIDTH);
	sprintf(string, "Version %d.%d", VERSION, REVNUM / 1000);
	DrawShadowCentredText(display, window, copyFont,
		string, y + 30, white, PLAY_WIDTH + MAIN_WIDTH);

	if (noSound == False) playSoundFile("intro", 50);
	SetPresentWait(PRESENT_TEXT1, frame + 800);
}

#if NeedFunctionPrototypes
static void DoText1(Display *display, Window window)
#else
static void DoText1(display, window)
	Display *display;
	Window window;
#endif
{
	int x, y;

	DEBUG("Drawing Justin in presents mode.")

	x = ((MAIN_WIDTH + PLAY_WIDTH) / 2) - 142;
	y = 530;

	/* Render the Justin bitmap */
	RenderShape(display, window, justin, justinM,
		x, y, 285, 44, True);

	SetPresentWait(PRESENT_TEXT2, frame + 500);
}

#if NeedFunctionPrototypes
static void DoText2(Display *display, Window window)
#else
static void DoText2(display, window)
	Display *display;
	Window window;
#endif
{
	int x, y;

	DEBUG("Drawing kibell in presents mode.")

	x = ((MAIN_WIDTH + PLAY_WIDTH) / 2) - 130;
	y = 530 + 44 + 10;

	/* Render the Justin bitmap */
	RenderShape(display, window, kibell, kibellM,
		x, y, 260, 40, True);

	SetPresentWait(PRESENT_TEXT3, frame + 1000);
}

#if NeedFunctionPrototypes
static void DoText3(Display *display, Window window)
#else
static void DoText3(display, window)
	Display *display;
	Window window;
#endif
{
	int x, y;

	DEBUG("Drawing presents in presents mode.")

	XClearArea(display, window, 0, 530, PLAY_WIDTH + MAIN_WIDTH, 99, False);

	x = ((MAIN_WIDTH + PLAY_WIDTH) / 2) - 205;
	y = 530 + 44 + 10 - 22;

	/* Render the Justin bitmap */
	RenderShape(display, window, presents, presentsM,
		x, y, 410, 44, True);

	SetPresentWait(PRESENT_TEXT_CLEAR, frame + 950);
}

#if NeedFunctionPrototypes
static void DoTextClear(Display *display, Window window)
#else
static void DoTextClear(display, window)
	Display *display;
	Window window;
#endif
{
	int x, y;

	DEBUG("Clearing word presents in presents mode.")

	x = ((MAIN_WIDTH + PLAY_WIDTH) / 2) - 205;
	y = 530 + 44 + 10 - 22;

	XClearArea(display, window, x, y, 410, 44, False);

	SetPresentWait(PRESENT_LETTERS, frame + 10);
}

/* The distances for the gap inbetwen blocks */
static int dists[] =
{
	71, 73, 83, 41, 85, 88
};

#if NeedFunctionPrototypes
static void DoLetters(Display *display, Window window)
#else
static void DoLetters(display, window)
	Display *display;
	Window window;
#endif
{
	static int i = 0;
	static int x = 40;

	if (i < 6)
	{
		if (noSound == False) playSoundFile("stamp", 100);

		/* Draw the letters of xboing one at a time across screen */
		DrawLetter(display, window, i, x, 260);
		x += 10 + dists[i];

		SetPresentWait(PRESENT_LETTERS, frame + 500);
	}
	else
		SetPresentWait(PRESENT_SHINE, frame + 50);

	i++;
}

#if NeedFunctionPrototypes
static void DoSparkle(Display *display, Window window)
#else
static void DoSparkle(display, window)
    Display *display;
    Window window;
#endif
{
    static Pixmap store;
    static int in = 0;
	static int startFrame;
	static int x, y;


    if (!store)
    {
        store = XCreatePixmap(display, window, 20, 20,
            DefaultDepth(display, XDefaultScreen(display)));

        startFrame = frame;
		x = MAIN_WIDTH + PLAY_WIDTH - 50;
		y = 252;
		if (noSound == False) playSoundFile("ping", 70);

		DEBUG("start the sparkle on xboing.")
    }

    if (in == 0)
        XCopyArea(display, window, store, gc, x, y, 20, 20, 0, 0);

    if (frame == startFrame)
    {
        RenderShape(display, window, stars[in], starsM[in],
            x, y, 20, 20, False);

        in++;
        startFrame = frame + 15;

        if (in == 11)
        {
            XCopyArea(display, store, window, gc, 0, 0, 20, 20, x, y);
			SetPresentWait(PRESENT_SPECIAL_TEXT1, frame + 500);
        }
    }
}


#if NeedFunctionPrototypes
static void DoSpecialText1(Display *display, Window window)
#else
static void DoSpecialText1(display, window)
    Display *display;
    Window window;
#endif
{
	static int first = True;
	static int i;
	static int len;
	static int x, y;
	static char wisdom[100];

	if (first)
	{
		/* Do this section on the first entry */

		if (GetNickName() != NULL)
			sprintf(wisdom, "Welcome %s, report to holodeck 5 immediately.", 
				GetNickName());
		else
			sprintf(wisdom, "Welcome %s, report to holodeck 5 immediately.", 
				getUsersFullName());

		i = 1;
		y = 550;
		len = strlen(wisdom);
		x = ((PLAY_WIDTH + MAIN_WIDTH) / 2) - 
			(XTextWidth(dataFont, wisdom, len) / 2);
		nextFrame = frame + 20;
		first = False;
	}

	if (frame >= nextFrame)
	{
		if (noSound == False) playSoundFile("key", 60);
		DrawText(display, window, x, y, dataFont, red, wisdom, i);

		nextFrame = frame + 30;

		i++;
		if (i > len)
			SetPresentWait(PRESENT_SPECIAL_TEXT2, frame + 700);
	}
}


#if NeedFunctionPrototypes
static void DoSpecialText2(Display *display, Window window)
#else
static void DoSpecialText2(display, window)
    Display *display;
    Window window;
#endif
{
	static int first = True;
	static int i;
	static int len;
	static int x, y;
	static char wisdom2[100];

	if (first)
	{
		/* Do this section on the first entry */

		strcpy(wisdom2, 
		  "The use of unauthorised weapons and extreme violence ... granted.");
		i = 1;
		y = 550 + dataFont->ascent + dataFont->descent + 5;
		len = strlen(wisdom2);
		x = ((PLAY_WIDTH + MAIN_WIDTH) / 2) - 
			(XTextWidth(dataFont, wisdom2, len) / 2);
		nextFrame = frame + 20;
		first = False;
	}

	if (frame >= nextFrame)
	{
		if (noSound == False) playSoundFile("key", 60);
		DrawText(display, window, x, y, dataFont, red, wisdom2, i);

		nextFrame = frame + 30;

		i++;
		if (i > len)
			SetPresentWait(PRESENT_CLEAR, frame + 800);
	}
}


#if NeedFunctionPrototypes
static void DoClear(Display *display, Window window)
#else
static void DoClear(display, window)
	Display *display;
	Window window;
#endif
{
	static int yt, yb;
	static int first = True;

	if (first == True)
	{
		/* Do this section on the first entry */
		yt = 0;
		yb = PLAY_HEIGHT + MAIN_HEIGHT - 10;
		first = False;
		nextFrame = frame;

    	if (noSound == False) playSoundFile("whoosh", 70);

		DEBUG("Clearing presents screen.")
	}

	if (frame >= nextFrame)
	{
		/* Clear and draw lines */
		XClearArea(display, window, 0, yt, PLAY_WIDTH + MAIN_WIDTH, 10, False);
		yt += 10;
		DrawLine(display, window, 2, yt, PLAY_WIDTH + MAIN_WIDTH-2, yt, red, 1);

		XClearArea(display, window, 0, yb, PLAY_WIDTH + MAIN_WIDTH, 10, False);
		DrawLine(display, window, 2, yb-1, PLAY_WIDTH + MAIN_WIDTH-2, yb-1, 
			red, 1);
		yb -= 10;

		if (yt > ((PLAY_HEIGHT + MAIN_HEIGHT) / 2))
			SetPresentWait(PRESENT_FINISH, frame + 20);

		nextFrame = frame + 20;
	}
}

#if NeedFunctionPrototypes
void QuickFinish(Display *display, Window window)
#else
void QuickFinish(display, window)
	Display *display;
	Window window;
#endif
{
	DEBUG("finishing in presents mode.")

	/* User has pressed space so finish early */
	SetPresentWait(PRESENT_FINISH, frame);
}

#if NeedFunctionPrototypes
static void DoFinish(Display *display, Window window)
#else
static void DoFinish(display, window)
	Display *display;
	Window window;
#endif
{
	/* Free all the pixmaps used in this intro */
	FreePresents(display);

	/* Erase all stuff in window */
	XClearWindow(display, mainWindow);

	DEBUG("mapping all game windows.")

	/* Map all the windows */
	MapAllWindows(display);

	/* Now jump into the intro mode */
	ResetIntroduction();
	mode = MODE_INTRO;
}

#if NeedFunctionPrototypes
void Presents(Display *display, Window window)
#else
void Presents(display, window)
	Display *display;
	Window window;
#endif
{
	switch (PresentState)
	{
		case PRESENT_FLAG:
			DoPresentFlag(display, window);
			break;

		case PRESENT_TEXT1:
			DoText1(display, window);
			break;

		case PRESENT_TEXT2:
			DoText2(display, window);
			break;

		case PRESENT_TEXT3:
			DoText3(display, window);
			break;

		case PRESENT_TEXT_CLEAR:
			DoTextClear(display, window);
			break;

		case PRESENT_LETTERS:
			DoLetters(display, window);
			break;

		case PRESENT_SPECIAL_TEXT1:
			DoSpecialText1(display, window);
			break;

		case PRESENT_SPECIAL_TEXT2:
			DoSpecialText2(display, window);
			break;

		case PRESENT_CLEAR:
			DoClear(display, window);
			break;

		case PRESENT_SHINE:
			DoSparkle(display, window);
			break;

		case PRESENT_FINISH:
			DoFinish(display, window);
			break;

		case PRESENT_WAIT:
			DoPresentWait();
			break;

		default:
			break;
	}
}

#if NeedFunctionPrototypes
void RedrawPresents(Display *display, Window window)
#else
void RedrawPresents(display, window)
	Display *display;
	Window window;
#endif
{
}

#if NeedFunctionPrototypes
void FreePresents(Display *display)
#else
void FreePresents(display)
	Display *display;
#endif
{
	int i;

	/* Free the flag pixmaps */
    if (flagPixmap)		XFreePixmap(display, flagPixmap);         
	if (flagPixmapM)	XFreePixmap(display, flagPixmapM);

	/* Free the large earth pixmap and it's mask */
    if (earthPixmap)	XFreePixmap(display, earthPixmap);         
	if (earthPixmapM)	XFreePixmap(display, earthPixmapM);

	/* Free the text pixmaps for justin, kibell and presents */
    if (justin)		XFreePixmap(display, justin);         
	if (justinM)	XFreePixmap(display, justinM);

    if (kibell)		XFreePixmap(display, kibell);         
	if (kibellM)	XFreePixmap(display, kibellM);

    if (presents)	XFreePixmap(display, presents);         
	if (presentsM)	XFreePixmap(display, presentsM);

	/* Free the pixmaps for the letters of the work xboing */
	for (i = 0; i < 6; i++)
	{
		/* Free each of the letters in XBOING */
    	if (titlePixmap[i])		XFreePixmap(display, titlePixmap[i]);         
		if (titlePixmapM[i])	XFreePixmap(display, titlePixmapM[i]);
	}
}

#if NeedFunctionPrototypes
void ResetPresents(void)
#else
void ResetPresents()
#endif
{
	DEBUG("Reset presents mode.")

	PresentState = PRESENT_FLAG;
	nextFrame = frame + 100;
	startFrame = frame + 10;
	endFrame = frame + 3000;
}

#if NeedFunctionPrototypes
void SetPresentWait(int newMode, int waitFrame)
#else
void SetPresentWait(newMode, waitFrame)
	int newMode;
	int waitFrame;
#endif
{
	waitingFrame = waitFrame;
	waitMode = newMode;
	PresentState = PRESENT_WAIT;
}

#if NeedFunctionPrototypes
void DoPresentWait(void)
#else
void DoPresentWait()
#endif
{
	if (frame == waitingFrame)
		PresentState = waitMode;
}
