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

#include "include/error.h"
#include "include/level.h"
#include "include/init.h"
#include "include/stage.h"
#include "include/intro.h"
#include "include/main.h"
#include "include/misc.h"

#include "include/mess.h"

/*
 *  Internal macro definitions:
 */

#define CLEAR_DELAY		2000

/*
 *  Internal type declarations:
 */

/*
 *  Internal variable declarations:
 */

char currentMessage[1024];
int		clearFrame;

#if NeedFunctionPrototypes
void InitialiseMessageSystem(Display *display, Window window, Colormap colormap)
#else
void InitialiseMessageSystem(display, window, colormap)
	Display *display;
	Window window;
	Colormap colormap;
#endif
{
	/* Frame to clear message area */
	clearFrame = 0;
}

#if NeedFunctionPrototypes
void FreeMessageSystem(Display *display)
#else
void FreeMessageSystem(display)
	Display *display;
#endif
{
	/* Not much to free yet - maybe one day .... */
}

#if NeedFunctionPrototypes
void DrawMessage(Display *display, Window window, char *message, int clear)
#else
void DrawMessage(display, window, message, clear)
	Display *display; 
	Window window;
	char *message;
	int clear;
#endif
{
	int len = strlen(message);
	int plen;

	if (clear)
		clearFrame = frame + CLEAR_DELAY;
	else
		clearFrame = frame - 1;

	/* Clear the message window */
	XClearWindow(display, window);

	/* Obtain the text width so it can be centered */
    plen = XTextWidth(textFont, message, len);

	/* Draw the string in the message window */
	DrawText(display, window, ((PLAY_WIDTH/2) - plen) / 2, 5,
		textFont, green, message, len);

	/* Just to be sure, flush the display */
	XFlush(display);
}

#if NeedFunctionPrototypes
void SetCurrentMessage(Display *display, Window window, char *newMessage, 
	int clear)
#else
void SetCurrentMessage(display, window, newMessage, clear)
	Display *display;
	Window window;
	char *newMessage;
	int clear;
#endif
{
	/* Draw out new message */
	DrawMessage(display, window, newMessage, clear);
}

#if NeedFunctionPrototypes
void AddMessage(char *newMessage)
#else
void AddMessage(newMessage)
	char *newMessage;
#endif
{
}

#if NeedFunctionPrototypes
void DisplayCurrentMessage(Display *display, Window window)
#else
void DisplayCurrentMessage(display, window)
	Display *display;
	Window window;
#endif
{
	char str[80];
	char str2[80];

	/* Clear the frame when it's time */
	if (frame == clearFrame)
	{
		/* Effectively erases message */
		strcpy(str2, GetLevelName());
		if (str2[0] != '\0')
		{
			/* Set the message to the name of the level */
			sprintf(str, "- %s -", str2);
			SetCurrentMessage(display, window, str, False);
		}
		else
			DrawMessage(display, window, "", False);

		/* To be sure to be sure */
		XFlush(display);
	}
}
