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
#include "include/init.h"
#include "include/stage.h"
#include "include/score.h"
#include "include/blocks.h"
#include "include/ball.h"
#include "include/main.h"
#include "include/mess.h"
#include "include/misc.h"
#include "include/intro.h"

#include "include/sfx.h"

/*
 *  Internal macro definitions:
 */

#define SHAKE_DELAY			5
#define NUM_SCAT 			10 
#define RANDY(range) 		(rand() % (range))

/*
 *  Internal type declarations:
 */

/*
 *  Internal variable declarations:
 */

static int sfxEndFrame, useSfx;
int modeSfx;
static int xscat[NUM_SCAT] = { 1, 9, 3, 6, 2, 4, 0, 7, 5, 8 };
static int yscat[NUM_SCAT] = { 2, 1, 0, 8, 6, 4, 9, 3, 7, 5 };

#if NeedFunctionPrototypes
void useSpecialEffects(int state)
#else
void useSpecialEffects(state)
	int state;
#endif
{
	/* Set the state of the special effects - True = use */
	/* Of course - if the sfx are not possible then no effect */
	useSfx = state;
}

#if NeedFunctionPrototypes
int getSpecialEffects(Display *display)
#else
int getSpecialEffects(display)
	Display *display;
#endif
{
	/* Only shake around if the server has backing store on */
	if (DoesBackingStore(XDefaultScreenOfDisplay(display)) != Always)
		return -1;

	/* Return special effects state - on or off */
	return useSfx;
}

#if NeedFunctionPrototypes
void changeSfxMode(int newMode)
#else
void changeSfxMode(newMode)
	int newMode;
#endif
{
	modeSfx = newMode;
}

#if NeedFunctionPrototypes
int currentSfxMode(void)
#else
int currentSfxMode()
#endif
{
	/* Return the current special effects mode */
	return modeSfx;
}

#if NeedFunctionPrototypes
static void resetEffect(Display *display)
#else
static void resetEffect(display)
	Display *display;
#endif
{
	/* Just re-centre window and return */
	modeSfx = SFX_NONE;
	XMoveWindow(display, playWindow, 35, 60);
}

#if NeedFunctionPrototypes
int WindowBlindEffect(Display *display, Window window)
#else
int WindowBlindEffect(display, window)
	Display *display;
	Window window;
#endif
{
	int x, i;

	/* Does the user want special effects */
	if (useSfx == False)
	{
		/* No - Just return out */
		resetEffect(display);
		return False;
	}

	XSetBackground(display, gcsfx, black);

	/* Draw a blinds effect where little doors close over screen */
	for (i = 0; i <= (PLAY_WIDTH / 8); i++)
		for (x = 0; x <= PLAY_WIDTH; x += (PLAY_WIDTH / 8))
			XCopyArea(display, bufferWindow, window, gc, 
				x+i, 0, 1, PLAY_HEIGHT, x+i, 0);

	/* End of special effect - reset off */
	resetEffect(display);
	return False;
}


#if NeedFunctionPrototypes
int WindowShatterEffect(Display *display, Window window)
#else
int WindowShatterEffect(display, window)
	Display *display;
	Window window;
#endif
{
    int offx, offy, sizeWidth, sizeHeight;
    int srcx, srcy, destx, desty;

	/* Does the user want special effects */
	if (useSfx == False)
	{
		/* No - Just return out */
		resetEffect(display);
		return False;
	}


    offx = RANDY(NUM_SCAT);
    offy = RANDY(NUM_SCAT);
	sizeWidth = 200;
	sizeHeight = 200;

	/* Spend a bit of time scattering new pixmap into view */
	/* Original idea for this effect from xjewel */
    for (srcx = 0; srcx < NUM_SCAT; srcx++)
    {
    	for (srcy = 0; srcy < NUM_SCAT; srcy++)
        {
            for (destx = 0; destx <= 4; destx++)
			{
            	for (desty = 0; desty <= 5; desty++)
                {
					XCopyArea(display, bufferWindow, window, gc, 
                    	(destx * sizeWidth) + 
                        	xscat[(srcx + srcy + offx) % NUM_SCAT] 
							* (sizeWidth / NUM_SCAT),

                    	(desty * sizeHeight) + 
                        	yscat[(srcy + offy) % NUM_SCAT] 
							* (sizeHeight / NUM_SCAT),

                    	(sizeWidth  / NUM_SCAT), 
						(sizeHeight / NUM_SCAT),

                    	(destx * sizeWidth) + 
                        	xscat[(srcx + srcy + offx) % NUM_SCAT] 
							* (sizeWidth / NUM_SCAT),

                    	(desty * sizeHeight) + 
                        	yscat[(srcy + offy) % NUM_SCAT] 
							* (sizeHeight / NUM_SCAT));
               	}
			}
        }
    }

	/* End of special effect - reset off */
	resetEffect(display);
	return False;
}

#if NeedFunctionPrototypes
int WindowFadeEffect(Display *display, Window window)
#else
int WindowFadeEffect(display, window)
	Display *display;
	Window window;
#endif
{
	static int done = False;
	static int first = True;
	int y;
	int x;
	static int i = 0;

	/* Does the user want special effects */
	if (useSfx == False)
	{
		/* No - Just return out */
		resetEffect(display);
		return False;
	}

	if (first == True)
	{
		first = False;
		XSetForeground(display, gcsfx, black);
		XSetBackground(display, gcsfx, black);
		XSetWindowBorder(display, playWindow, red);
	}

	/* Draw vertical lines */
	for (x = i; x <= PLAY_WIDTH; x += 12)
		XDrawLine(display, window, gcsfx, x, 0, x, PLAY_HEIGHT);

	/* Draw horizontal lines */
	for (y = i; y <= PLAY_HEIGHT; y += 12)
		XDrawLine(display, window, gcsfx, 0, y, PLAY_WIDTH, y);

	/* Fill in grid slowly */
	i++;
	if (i > 12) done = True;

	/* Also fade off the border */
	XSetWindowBorder(display, playWindow, reds[i]);

	if (done == True)
	{
		/* End of special effect - reset off */
		done = False;
		first = True;
		i = 0;
		resetEffect(display);
		return False;
	}

	/* Keep efect going please */
	return True;
}

#if NeedFunctionPrototypes
int WindowShakeEffect(Display *display, Window window)
#else
int WindowShakeEffect(display, window)
	Display *display;
	Window window;
#endif
{
	static int x = 35;
	static int y = 60;
	int xi, yi;

	/* Does the user want special effects */
	if (useSfx == False)
	{
		/* No - Just return out */
		resetEffect(display);
		return False;
	}

	/* Only shake around if the server has backing store on */
	if (DoesBackingStore(XDefaultScreenOfDisplay(display)) != Always)
	{
		resetEffect(display);
		return False;
	}

	if (frame >= sfxEndFrame) 
	{
		/* End of special effect - reset off */
		resetEffect(display);
		return False;
	}
	
	if ((frame % SHAKE_DELAY) != 0) return True;

	XMoveWindow(display, window, x, y);
	XFlush(display);

	xi = (rand() % 6) - 3; 
	yi = (rand() % 6) - 3; 
	x = xi + 35; y = yi + 60;

	return True;
}

#if NeedFunctionPrototypes
void SetSfxEndFrame(int endFrame)
#else
void SetSfxEndFrame(endFrame)
	int endFrame;
#endif
{
	sfxEndFrame = endFrame;
}

#if NeedFunctionPrototypes
void BorderGlow(Display *display, Window window)
#else
void BorderGlow(display, window)
    Display *display;
    Window window;
#endif
{
    static int i = 0;
    static int d = 1;
    static int t = 1;

	/* Only update every n frames */
    if ((frame % 60) == 0)
    {
		/* Alternate between the red and the green ranges */
        if (t > 0)
            XSetWindowBorder(display, playWindow, reds[i]);
        else
            XSetWindowBorder(display, playWindow, greens[i]);

		/* Ok change range or fade down again */
        if (i == 12)
        {
            d = -1;
            t *= -1;
        }

        if (i == 0)
            d = 1;

		/* Next range index */
        i += d;
     }
}

#if NeedFunctionPrototypes
void ResetBorderGlow(Display *display, Window window)
#else
void ResetBorderGlow(display, window)
    Display *display;
    Window window;
#endif
{
    XSetWindowBorder(display, playWindow, red);
}
