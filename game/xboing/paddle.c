#include "include/copyright.h"

/*
 *  Include file dependencies:
 */

#include <stdio.h>
#include <xpm.h>

#include "bitmaps/paddlesmall.xpm"
#include "bitmaps/paddlemedium.xpm"
#include "bitmaps/paddlehuge.xpm"

#include "include/error.h"
#include "include/init.h"
#include "include/stage.h"
#include "include/blocks.h"
#include "include/misc.h"
#include "include/special.h"

#include "include/paddle.h"

/*
 *  Internal macro definitions:
 */

/*
 *  Internal type declarations:
 */

/*
 *  Internal variable declarations:
 */

static Pixmap paddleSmallPixmap, paddleMediumPixmap, paddleHugePixmap;
static Pixmap paddleSmallMask, paddleMediumMask, paddleHugeMask;

int	paddlePos;
int	currentPaddleSize;
static int	oldX;
int reverseOn, stickyOn;

#if NeedFunctionPrototypes
void DrawPaddle(Display *display, Window window, int x, int y, int size)
#else
void DrawPaddle(display, window, x, y, size)
	Display *display;
	Window window;
	int x;
	int y;
	int size;
#endif
{
	/* Switch on the paddle size */
	switch (size)
	{
		case PADDLE_SMALL:
			RenderShape(display, window, paddleSmallPixmap, paddleSmallMask,
				x - 20, y, 40, 15, True);
			break;

		case PADDLE_MEDIUM:
			RenderShape(display, window, paddleMediumPixmap, paddleMediumMask,
				x - 25, y, 50, 15, True);
			break;

		case PADDLE_HUGE:
			RenderShape(display, window, paddleHugePixmap, paddleHugeMask,
				x - 35, y, 70, 15, True);
			break;
	}
}

#if NeedFunctionPrototypes
void InitialisePaddle(Display *display, Window window, Colormap colormap)
#else
void InitialisePaddle(display, window, colormap)
	Display *display;
	Window window;
	Colormap colormap;
#endif
{
    XpmAttributes   attributes;
	int		    XpmErrorStatus;

    attributes.valuemask = XpmColormap;
	attributes.colormap = colormap;

	/* Create the xpm pixmap paddles */
	XpmErrorStatus = XpmCreatePixmapFromData(display, window, paddlesmall_xpm,
		&paddleSmallPixmap, &paddleSmallMask, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialisePaddle()");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, paddlemedium_xpm,
		&paddleMediumPixmap, &paddleMediumMask, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialisePaddle()");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, paddlehuge_xpm,
		&paddleHugePixmap, &paddleHugeMask, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialisePaddle()");

	/* Free the xpm pixmap attributes */
	XpmFreeAttributes(&attributes);
}

#if NeedFunctionPrototypes
void SetReverseOff(void)
#else
void SetReverseOff()
#endif
{
	/* Set the reverse state off */
	reverseOn = False;
}

#if NeedFunctionPrototypes
void ToggleReverse(Display *display)
#else
void ToggleReverse(display)
	Display *display;
#endif
{
	/* Set the reverse state */
	if (reverseOn == True)
		reverseOn = False;
	else
		reverseOn = True;

	DrawSpecials(display);
}

#if NeedFunctionPrototypes
void FreePaddle(Display *display)
#else
void FreePaddle(display)
	Display *display;
#endif
{
	 if (paddleSmallPixmap)		XFreePixmap(display, paddleSmallPixmap);
	 if (paddleMediumPixmap)	XFreePixmap(display, paddleMediumPixmap);
	 if (paddleHugePixmap)		XFreePixmap(display, paddleHugePixmap);

	 if (paddleSmallMask)		XFreePixmap(display, paddleSmallMask);
	 if (paddleMediumMask)		XFreePixmap(display, paddleMediumMask);
	 if (paddleHugeMask)		XFreePixmap(display, paddleHugeMask);
}

#if NeedFunctionPrototypes
void MovePaddle(Display *display, Window window, int direction, int size, 
	int xpos)
#else
void MovePaddle(display, window, direction, size, xpos)
	Display *display;
	Window window;
	int direction;
	int size;
	int xpos;
#endif
{
	static int	y = (PLAY_HEIGHT - DIST_BASE);

	if (reverseOn == True)
	{
		/* Handle the special reverse block for keys */
		if (direction == PADDLE_LEFT)
			direction = PADDLE_RIGHT;
		else if (direction == PADDLE_RIGHT)
			direction = PADDLE_LEFT;

		/* Handle reverse for the mouse control */
		if (direction == PADDLE_NONE)
			xpos = PLAY_WIDTH - xpos;
	}

	switch (direction)
	{
		case PADDLE_LEFT:
			paddlePos -= PADDLE_VEL;
			break;

		case PADDLE_RIGHT:
			paddlePos += PADDLE_VEL;
			break;

		case PADDLE_NONE:
			break;
	}

	switch (size)
	{
		case PADDLE_SMALL:
			if (xpos > 0)
				paddlePos = xpos - (MAIN_WIDTH / 2) + 20;

			if (paddlePos < 20) paddlePos = 20;
			if (paddlePos > (PLAY_WIDTH - 20)) 
				paddlePos = (PLAY_WIDTH - 20);

			XClearArea(display, window, oldX, y, 40, 15, False);
			oldX = paddlePos - 20; 

			DrawPaddle(display, window, paddlePos, PLAY_HEIGHT - DIST_BASE, 
				PADDLE_SMALL);
			break;

		case PADDLE_MEDIUM:
			if (xpos > 0)
				paddlePos = xpos - (MAIN_WIDTH / 2) + 25;

			if (paddlePos < 25) paddlePos = 25;
			if (paddlePos > (PLAY_WIDTH - 25)) 
				paddlePos = (PLAY_WIDTH - 25);

			XClearArea(display, window, oldX, y, 50, 15, False);
			oldX = paddlePos - 25; 

			DrawPaddle(display, window, paddlePos, 
				PLAY_HEIGHT - DIST_BASE, PADDLE_MEDIUM);
			break;

		case PADDLE_HUGE:
			if (xpos > 0)
				paddlePos = xpos - (MAIN_WIDTH / 2) + 35;

			if (paddlePos < 35) paddlePos = 35;
			if (paddlePos > (PLAY_WIDTH - 35)) 
				paddlePos = (PLAY_WIDTH - 35);

			XClearArea(display, window, oldX, y, 70, 15, False);
			oldX = paddlePos - 35; 

			DrawPaddle(display, window, paddlePos, 
				PLAY_HEIGHT - DIST_BASE, PADDLE_HUGE);
			break;
	}
}

#if NeedFunctionPrototypes
void FlushPaddleBackingStore(Display *display, Window window)
#else
void FlushPaddleBackingStore(display, window)
	Display *display;
	Window window;
#endif
{
	static int y = (PLAY_HEIGHT - DIST_BASE);

	/* Clear the entire paddle area */
	XClearArea(display, window, 0, y, PLAY_WIDTH, 15, False);
}

#if NeedFunctionPrototypes
int GetPaddleSize(void)
#else
int GetPaddleSize()
#endif
{
	switch (currentPaddleSize)
	{
		case PADDLE_SMALL:
			return 40;

		case PADDLE_MEDIUM:
			return 50;

		case PADDLE_HUGE:
			return 70;
	}

	return 0;
}

#if NeedFunctionPrototypes
void ResetPaddleStart(Display *display, Window window)
#else
void ResetPaddleStart(display, window)
	Display *display;
	Window window;
#endif
{
	paddlePos = PLAY_WIDTH / 2;
	oldX = PLAY_WIDTH / 2;

	FlushPaddleBackingStore(display, window);
	MovePaddle(display, window, PADDLE_NONE, currentPaddleSize, 0);
}

#if NeedFunctionPrototypes
void RedrawPaddle(Display *display, Window window)
#else
void RedrawPaddle(display, window)
	Display *display;
	Window window;
#endif
{
	MovePaddle(display, window, PADDLE_NONE, currentPaddleSize, 0);
	/*
	DrawPaddle(display, window, paddlePos, 
		PLAY_HEIGHT - DIST_BASE, currentPaddleSize);
		*/
}

#if NeedFunctionPrototypes
void ChangePaddleSize(Display *display, Window window, int type)
#else
void ChangePaddleSize(display, window, type)
	Display *display;
	Window window;
	int type;
#endif
{
	FlushPaddleBackingStore(display, window);

	if (type == PAD_SHRINK_BLK)
	{

		if (currentPaddleSize == PADDLE_MEDIUM)
		{
			/* Shrink the paddle */
        	currentPaddleSize = PADDLE_SMALL;
		} else if (currentPaddleSize == PADDLE_HUGE)
		{
			/* Shrink the paddle */
        	currentPaddleSize = PADDLE_MEDIUM;
		}
	}
	else
	{
		if (currentPaddleSize == PADDLE_SMALL)
		{
			/* Grow the paddle */
        	currentPaddleSize = PADDLE_MEDIUM;
		} else if (currentPaddleSize == PADDLE_MEDIUM)
		{
			/* Grow the paddle */
        	currentPaddleSize = PADDLE_HUGE;
		}
	}

	/* Draw the new paddle in its new size */
	RedrawPaddle(display, window);
}
