#include "include/copyright.h"

/*
 *  Include file dependencies:
 */

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <math.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <xpm.h>

#include "include/error.h"
#include "include/highscore.h"
#include "include/misc.h"
#include "include/main.h"
#include "include/init.h"
#include "include/stage.h"
#include "include/bonus.h"
#include "include/special.h"
#include "include/blocks.h"
#include "include/ball.h"
#include "include/score.h"
#include "include/paddle.h"
#include "include/level.h"
#include "include/mess.h"
#include "include/sfx.h"
#include "include/version.h"
#include "include/intro.h"
#include "include/audio.h"
#include "include/keys.h"

#if NeedFunctionPrototypes
extern void ResetDemonstration(void);
#else /* !NeedFunctionPrototypes */
extern void ResetDemonstration();
#endif /* NeedFunctionPrototypes */

#include "include/inst.h"

/*
 *  Internal macro definitions:
 */

#define GAP 		12

/*
 *  Internal type declarations:
 */

#if NeedFunctionPrototypes
void SetInstructWait(enum InstructStates newMode, int waitFrame);
static void DoSparkle(Display *display, Window window);
void DoInstructWait(void);
#else
static void DoSparkle();
void SetInstructWait();
void DoInstructWait();
#endif

/*
 *  Internal variable declarations:
 */

static int endFrame = 0;
static int nextFrame = 0;
enum InstructStates InstructState;
static int waitingFrame;
enum InstructStates waitMode;

#if NeedFunctionPrototypes
void SetUpInstructions(Display *display, Window window, Colormap colormap)
#else
void SetUpInstructions(display, window, colormap)
	Display *display;
	Window window;
	Colormap colormap;
#endif
{
	/* Umm. Reset the instructions to default state */
	ResetInstructions();
}

#if NeedFunctionPrototypes
static void DoText(Display *display, Window window)
#else
static void DoText(display, window)
	Display *display;
	Window window;
#endif
{
	char string[80];
	int y;

	SetCurrentMessage(display, messWindow, "Save the rainforests", False);

	DrawShadowCentredText(display, window, titleFont, 
		"- Instructions -", 140, red, PLAY_WIDTH);

	y = 190;

	DrawLine(display, window, 32, y+2, PLAY_WIDTH - 28, y+2, black, 3);
	DrawLine(display, window, 30, y, PLAY_WIDTH - 30, y, white, 3);
	y += textFont->ascent + GAP/3;

	strcpy(string, 
		"XBoing is a blockout type game where you must use");
	DrawShadowCentredText(display, window, textFont, 
		string, y, yellow, PLAY_WIDTH);
	y += textFont->ascent + GAP;

	strcpy(string, 
		"the paddle to bounce the ball around the play field and");
	DrawShadowCentredText(display, window, textFont, 
		string, y, yellow, PLAY_WIDTH);
	y += textFont->ascent + GAP;

	strcpy(string, 
		"destroy the blocks. The Boing Master will rule the world.");
	DrawShadowCentredText(display, window, textFont, 
		string, y, yellow, PLAY_WIDTH);
	y += textFont->ascent + GAP;
	y += textFont->ascent + GAP / 2;

	strcpy(string, "You can collect bullets and use them to shoot out");
	DrawShadowCentredText(display, window, textFont, 
		string, y, green, PLAY_WIDTH);
	y += textFont->ascent + GAP;

	strcpy(string, "blocks or bonus coins. You have a limited supply of");
	DrawShadowCentredText(display, window, textFont, 
		string, y, green, PLAY_WIDTH);
	y += textFont->ascent + GAP;

	strcpy(string, 
		"bullets which can be replenished by hitting an");
	DrawShadowCentredText(display, window, textFont, 
		string, y, green, PLAY_WIDTH);
	y += textFont->ascent + GAP;

	strcpy(string, 
		"ammunition block. You can shoot your ball. Beware.");
	DrawShadowCentredText(display, window, textFont, 
		string, y, green, PLAY_WIDTH);
	y += textFont->ascent + GAP;
	y += textFont->ascent + GAP / 2;

	strcpy(string, "If you collect more than 10 bonus coins on a level,");
	DrawShadowCentredText(display, window, textFont, 
		string, y, tann, PLAY_WIDTH);
	y += textFont->ascent + GAP;

	strcpy(string, "you will be rewarded with a super bonus of 50,000.");
	DrawShadowCentredText(display, window, textFont, 
		string, y, tann, PLAY_WIDTH);
	y += textFont->ascent + GAP;

	DrawLine(display, window, 32, y+2, PLAY_WIDTH - 28, y+2, black, 3);
	DrawLine(display, window, 30, y, PLAY_WIDTH - 30, y, white, 3);

	strcpy(string, "Insert coin to start the game");
	DrawShadowCentredText(display, window, textFont, string, 
		PLAY_HEIGHT - 40, tann, PLAY_WIDTH);
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
    static int x = 100;
    static int y = 20;
    static int in = 0;

    if (frame >= endFrame)
        InstructState = INSTRUCT_FINISH;

    if (!store)
    {
        store = XCreatePixmap(display, window, 20, 20,
            DefaultDepth(display, XDefaultScreen(display)));
    }

    if (in == 0)
        XCopyArea(display, window, store, gc, x, y, 20, 20, 0, 0);

    if (frame == nextFrame)
    {
        XCopyArea(display, store, window, gc, 0, 0, 20, 20, x, y);
        RenderShape(display, window, stars[in], starsM[in],
            x, y, 20, 20, False);

        in++;
        nextFrame = frame + 15;

        if (in == 11)
        {
            XCopyArea(display, store, window, gc, 0, 0, 20, 20, x, y);
            in = 0;
            nextFrame = frame + 500;
            x = (rand() % 474) + 5;
            y = (rand() % 74) + 5;
        }
    }
}

#if NeedFunctionPrototypes
static void DoFinish(Display *display, Window window)
#else
static void DoFinish(display, window)
	Display *display;
	Window window;
#endif
{
	ResetDemonstration();
	mode = MODE_DEMO;

    if (noSound == False)
		playSoundFile("whizzo", 50);
}


#if NeedFunctionPrototypes
void Instructions(Display *display, Window window)
#else
void Instructions(display, window)
	Display *display;
	Window window;
#endif
{
	switch (InstructState)
	{
		case INSTRUCT_TITLE:
			if (getSpecialEffects(display) == True)
				DoIntroTitle(display, bufferWindow);
			else
				DoIntroTitle(display, window);
			InstructState = INSTRUCT_TEXT;
			break;

		case INSTRUCT_TEXT:
			if (getSpecialEffects(display) == True)
			{
				DoText(display, bufferWindow);
				while (WindowShatterEffect(display, window));
			}
			else
				DoText(display, window);
			InstructState = INSTRUCT_SPARKLE;
			break;

		case INSTRUCT_SPARKLE:
			DoSparkle(display, window);
			BorderGlow(display, window);
			if ((frame % FLASH) == 0)
				RandomDrawSpecials(display);
			break;

		case INSTRUCT_FINISH:
			DoFinish(display, window);
			break;

		case INSTRUCT_WAIT:
			DoInstructWait();
			break;

		default:
			break;
	}
}

#if NeedFunctionPrototypes
void RedrawInstructions(Display *display, Window window)
#else
void RedrawInstructions(display, window)
	Display *display; 
	Window window;
#endif
{
	DoIntroTitle(display, window);
	DoText(display, window);
}

#if NeedFunctionPrototypes
void FreeInstructions(Display *display)
#else
void FreeInstructions(display)
	Display *display;
#endif
{
}

#if NeedFunctionPrototypes
void ResetInstructions(void)
#else
void ResetInstructions()
#endif
{
	InstructState = INSTRUCT_TITLE;
	nextFrame 	= frame + 100;
	endFrame 	= frame + 4000;

	DEBUG("Reset Instruction mode.")
}

#if NeedFunctionPrototypes
void SetInstructWait(enum InstructStates newMode, int waitFrame)
#else
void SetInstructWait(newMode, waitFrame)
	enum InstructStates newMode;
	int waitFrame;
#endif
{
	waitingFrame 	= waitFrame;
	waitMode 		= newMode;
	InstructState 	= INSTRUCT_WAIT;
}

#if NeedFunctionPrototypes
void DoInstructWait(void)
#else
void DoInstructWait()
#endif
{
	if (frame == waitingFrame)
		InstructState = waitMode;
}
