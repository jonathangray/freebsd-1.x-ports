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
#include "include/audio.h"
#include "include/highscore.h"
#include "include/misc.h"
#include "include/main.h"
#include "include/gun.h"
#include "include/init.h"
#include "include/stage.h"
#include "include/blocks.h"
#include "include/sfx.h"
#include "include/ball.h"
#include "include/score.h"
#include "include/paddle.h"
#include "include/level.h"
#include "include/mess.h"
#include "include/intro.h"

#include "bitmaps/titleSmall.xpm"

#include "include/bonus.h"

/*
 *  Internal macro definitions:
 */

#define GAP					30
#define KEY_TYPE_VOL		50
#define LINE_DELAY			15

#define BONUS_COIN_SCORE	3000
#define SUPER_BONUS_SCORE	50000
#define BULLET_SCORE		500
#define LEVEL_SCORE			1000
#define TIME_BONUS			100

#define BORDER_LEFT			55
#define BORDER_RIGHT		((PLAY_WIDTH + MAIN_WIDTH) - 50)
#define BORDER_TOP			73
#define BORDER_BOTTOM		((PLAY_HEIGHT + MAIN_HEIGHT) - 85)

/*
 *  Internal type declarations:
 */

#if NeedFunctionPrototypes
static void DoBullets(Display *display, Window window);
static void DoTimeBonus(Display *display, Window window);
#else
static void DoTimeBonus();
static void DoBullets();
#endif

/*
 *  Internal variable declarations:
 */

static int 		numBonus;
enum BonusStates 	BonusState;
static Pixmap 	titlePixmap, titlePixmapM;
static int 		ypos;
static int 		waitingFrame;
enum BonusStates		waitMode;
static char 	string[80];
static Pixmap 	backingStoreTitle = (Pixmap) NULL;
static u_long 	bonusScore;
static int 		firstTime = True;


#if NeedFunctionPrototypes
void SetUpBonus(Display *display, Window window, Colormap colormap)
#else
void SetUpBonus(display, window, colormap)
	Display *display;
	Window window;
	Colormap colormap;
#endif
{
	XpmAttributes   attributes;
	int             XpmErrorStatus;

	attributes.valuemask = XpmColormap;
	attributes.colormap = colormap;

	/* Create the small title pixmap */
	XpmErrorStatus = XpmCreatePixmapFromData(display, window, titleSmall_xpm,
		&titlePixmap, &titlePixmapM, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBonus()");

    /* Free the xpm pixmap attributes */
	XpmFreeAttributes(&attributes);
}

#if NeedFunctionPrototypes
void DecNumberBonus(void)
#else
void DecNumberBonus()
#endif
{
	/* bump down the number of bonuses. sic */
	numBonus--;
}

#if NeedFunctionPrototypes
void IncNumberBonus(void)
#else
void IncNumberBonus()
#endif
{
	/* bump up the number of bonuses. sic */
	numBonus++;
}

#if NeedFunctionPrototypes
int GetNumberBonus(void)
#else
int GetNumberBonus()
#endif
{
	/* Umm - return the number of bonuses */
	return numBonus;
}

#if NeedFunctionPrototypes
void ResetNumberBonus(void)
#else
void ResetNumberBonus()
#endif
{
	/* No more bonuses thanks */
	numBonus = 0;
}

#if NeedFunctionPrototypes
void DrawBallBorder(Display *display, Window window)
#else
void DrawBallBorder(display, window)
	Display *display;
	Window window;
#endif
{
	int x, y;
	static int slide = 0;

	/* Draw a row of balls along the top and bottom of screen */
	for (x = BORDER_LEFT; x < BORDER_RIGHT; x += 22)
	{
		DrawTheBall(display, window, x, BORDER_TOP, slide);
		DrawTheBall(display, window, x, BORDER_BOTTOM, slide);

		/* Next frame of ball animation */
		slide++;

		/* Wrap around animation */
		if (slide == BALL_SLIDES) slide = 0;
	}

	/* Draw a row of balls along both sides of the screen */
	for (y = BORDER_TOP; y < BORDER_BOTTOM; y += 22)
	{
		DrawTheBall(display, window, BORDER_LEFT, y, slide);
		DrawTheBall(display, window, BORDER_RIGHT, y, slide);

		/* Next frame of ball animation */
		slide++;

		/* Wrap around animation */
		if (slide == BALL_SLIDES) slide = 0;
	}

}

#if NeedFunctionPrototypes
void DrawSmallIntroTitle(Display *display, Window window, int x, int y)
#else
void DrawSmallIntroTitle(display, window, x, y)
	Display *display;
	Window window;
	int x;
	int y;
#endif
{
	/* Draw the small title pixmap */
	RenderShape(display, window, titlePixmap, titlePixmapM,
		x - SMALL_TITLE_WC, y - SMALL_TITLE_HC, 
		SMALL_TITLE_WIDTH, SMALL_TITLE_HEIGHT, False);
}

#if NeedFunctionPrototypes
void MoveSmallIntroTitle(Display *display, Window window, int x, int y)
#else
void MoveSmallIntroTitle(display, window, x, y)
	Display *display;
	Window window;
	int x;
	int y;
#endif
{
	static int oldx, oldy;

    /* Make a copy of the new titles position */
    if (backingStoreTitle != (Pixmap) NULL)
        XCopyArea(display, backingStoreTitle, window, gc,
            0, 0, SMALL_TITLE_WIDTH, SMALL_TITLE_HEIGHT, oldx, oldy);
    else
    {
        /* Create a pixmap for the backing store title */
        backingStoreTitle = XCreatePixmap(display, window,
            SMALL_TITLE_WIDTH, SMALL_TITLE_HEIGHT,
            DefaultDepth(display, XDefaultScreen(display)));
    }

    /* Update the variables for the title position */
    oldx = x - SMALL_TITLE_WC; oldy = y - SMALL_TITLE_HC;

    /* Copy back the old position picture */
    if (backingStoreTitle)
        XCopyArea(display, window, backingStoreTitle, gc,
            x - SMALL_TITLE_WC, y - SMALL_TITLE_HC, 
			SMALL_TITLE_WIDTH, SMALL_TITLE_HEIGHT, 0, 0);

    DrawSmallIntroTitle(display, window, x, y);
}


#if NeedFunctionPrototypes
void SetupBonusScreen(Display *display, Window window)
#else
void SetupBonusScreen(display, window)
	Display *display;
	Window window;
#endif
{
	/* Clear the background again */
	ClearMainWindow(display, window);

	/* Draw the rectangular border of balls */
	DrawBallBorder(display, window);

	/* Draw the main title pixmap */
	DrawSmallIntroTitle(display, window, TOTAL_WIDTH / 2, 120);

	/* The new state will be the text */
	ResetBonus();

	/* Remove the window to show bonus stuff */
	while (WindowFadeEffect(display, playWindow));
	XUnmapWindow(display, playWindow);
}

#if NeedFunctionPrototypes
void DrawTitleText(Display *display, Window window)
#else
void DrawTitleText(display, window)
	Display *display;
	Window window;
#endif
{
	SetCurrentMessage(display, messWindow, "- Bonus Tally -", True);
	
	/* Indicate which level the bonus is for */
	sprintf(string, "- Level %d -", level);
	DrawShadowCentredText(display, window, titleFont, 
		string, ypos, red, TOTAL_WIDTH);
	ypos += (titleFont->ascent + GAP);

	strcpy(string, "Press space for next level");
	DrawShadowCentredText(display, window, textFont, 
		string, PLAY_HEIGHT - 12, tann, TOTAL_WIDTH);

	BonusState = BONUS_SCORE;

	/* Just in case it has changed */
	SetGameSpeed(FAST_SPEED);
}

#if NeedFunctionPrototypes
static void DoScore(Display *display, Window window)
#else
static void DoScore(display, window)
	Display *display;
	Window window;
#endif
{
	SetGameSpeed(MEDIUM_SPEED);

	/* Nice message rewarding you for your efforts */
	strcpy(string, "Congratulations on finishing this level.");
	DrawShadowCentredText(display, window, textFont, 
		string, ypos, white, TOTAL_WIDTH);
	XFlush(display);

	ypos += (35 + GAP);
	SetBonusWait(BONUS_BONUS, frame + LINE_DELAY);
	SetGameSpeed(SLOW_SPEED);
}

#if NeedFunctionPrototypes
static void DoBonuses(Display *display, Window window)
#else
static void DoBonuses(display, window)
	Display *display;
	Window window;
#endif
{
	int x, plen, secs;
	static int maxLen;

	/* Get the number of seconds left on the clock */
	secs = GetLevelTimeBonus();

	if (secs == 0)
	{
		strcpy(string, "Bonus coins invalid - Timer ran out.");
		DrawShadowCentredText(display, window, textFont, 
			string, ypos, blue, TOTAL_WIDTH);

		SetGameSpeed(MEDIUM_SPEED);

		/* Now skip to the next sequence */
		SetBonusWait(BONUS_LEVEL, frame + LINE_DELAY);
		firstTime = True;
		ypos += (textFont->ascent + GAP * 2);
		return;
	}

	if (firstTime)
	{
		/* Set up the bonus coin sequence */
		firstTime = False;

		if (numBonus == 0)
		{
			/* No bonus coins - so tell user */
			strcpy(string, "Sorry, no bonus coins collected.");
			DrawShadowCentredText(display, window, textFont, 
				string, ypos, blue, TOTAL_WIDTH);

			SetGameSpeed(MEDIUM_SPEED);

			/* Now skip to the next sequence */
			SetBonusWait(BONUS_LEVEL, frame + LINE_DELAY);
			firstTime = True;
			ypos += (textFont->ascent + GAP * 2);
			return;
		}

		if (numBonus > MAX_BONUS)
		{
			/* Play the sound for the super bonus */
			if (noSound == False) 
				playSoundFile("superbonus", 80);

			/* More than 10 coins collected - super bonus reward */
			sprintf(string, "Super Bonus - %d", SUPER_BONUS_SCORE);
				DrawShadowCentredText(display, window, titleFont, 
					string, ypos, blue, TOTAL_WIDTH);

			/* Update the score with more points */
			bonusScore += ComputeScore(SUPER_BONUS_SCORE);
			DisplayScore(display, scoreWindow, bonusScore);
	
			SetGameSpeed(MEDIUM_SPEED);

			/* Now skip to the next sequence */
			SetBonusWait(BONUS_LEVEL, frame + LINE_DELAY);
			firstTime = True;
			ypos += (textFont->ascent + GAP * 2);
			return;
		}

		/* Calculate where to draw these coins centred */
		maxLen = ((numBonus * 27) + (10 * numBonus) + 5);
	}

	/* Find out where the next bonus coin will go next */
	plen = ((numBonus * 27) + (10 * numBonus));
	x = (((PLAY_WIDTH + MAIN_WIDTH) / 2) + (maxLen / 2)) - plen;

	/* Draw the bonus coin shape */
	DrawTheBlock(display, window, x, ypos, BONUS_BLK, 0, 0, 0);

	/* Play the sound for the bonus */
	if (noSound == False) 
		playSoundFile("bonus", 50);

	/* Increment the score by the value of the bonus */
	bonusScore += ComputeScore(BONUS_COIN_SCORE);
	DisplayScore(display, scoreWindow, bonusScore);

	/* Reduce number of bonuses */
	DecNumberBonus();

	if (numBonus <= 0)
	{
		/* Set up bonus state for next sequence */
		SetBonusWait(BONUS_LEVEL, frame + LINE_DELAY);

		/* Make sure bonus is reset */
		ResetNumberBonus();

		ypos += (int) (textFont->ascent + GAP * 1.5);
		firstTime = True;

		SetGameSpeed(MEDIUM_SPEED);
	}
}

#if NeedFunctionPrototypes
static void DoLevel(Display *display, Window window)
#else
static void DoLevel(display, window)
	Display *display;
	Window window;
#endif
{
	int secs, theLevel;

	SetGameSpeed(MEDIUM_SPEED);

	/* Get the number of seconds left on the clock */
	secs = GetLevelTimeBonus();

	if (secs > 0)
	{
    	/* Adjust the level so that the starting level is taken into account */
    	theLevel = (int) level - GetStartingLevel() + 1;

		/* Draw level bonus text */
		sprintf(string, "Level bonus - level %d x %d = %d points", 
			theLevel, LEVEL_SCORE, theLevel * LEVEL_SCORE);
		DrawShadowCentredText(display, window, textFont, 
			string, ypos, yellow, TOTAL_WIDTH);

		/* Increment the score by the value of the level bonus */
		bonusScore += ComputeScore((LEVEL_SCORE * theLevel));
		DisplayScore(display, scoreWindow, bonusScore);
	}
	else
	{
		strcpy(string, "No level bonus - Timer ran out.");
		DrawShadowCentredText(display, window, textFont, 
			string, ypos, yellow, TOTAL_WIDTH);
	}

	/* Next section setup */
	ypos += (int) (textFont->ascent + GAP * 1.5);
	SetBonusWait(BONUS_BULLET, frame + LINE_DELAY);
}

#if NeedFunctionPrototypes
static void DoBullets(Display *display, Window window)
#else
static void DoBullets(display, window)
	Display *display;
	Window window;
#endif
{
	int x, plen;
	static int maxLen;

	if (firstTime)
	{
		/* For the first time setup the bullet bonus seq. */
		firstTime = False;
		SetUnlimitedBullets(False);

		if (GetNumberBullets() == 0)
		{
			/* No bullets - say so */
			strcpy(string, "You have used all your bullets. No bonus!");
			DrawShadowCentredText(display, window, textFont, 
				string, ypos, blue, TOTAL_WIDTH);
			
			SetGameSpeed(MEDIUM_SPEED);

			/* Get ready for the next sequence */
			SetBonusWait(BONUS_TIME, frame + LINE_DELAY);
			firstTime = True;
			ypos += (textFont->ascent + GAP/2);
			return;
		}

		/* Position where the first bullet will be drawn */
		maxLen = ((GetNumberBullets() * 7) + (3 * GetNumberBullets()));
	}

	/* Find out where the next bonus bullet will go next */
	plen = ((GetNumberBullets() * 7) + (3 * GetNumberBullets()));
	x = (((PLAY_WIDTH + MAIN_WIDTH) / 2) + (maxLen / 2)) - plen;

	DrawTheBullet(display, window, x, ypos);

	/* Play the sound for the bullets */
	if (noSound == False) 
		playSoundFile("key", 50);

	/* Increment the score by the value of the bullet bonus */
	bonusScore += ComputeScore(BULLET_SCORE);
	DisplayScore(display, scoreWindow, bonusScore);

	/* Ummm. Draw a bullet */
	DeleteABullet(display);	

	if (GetNumberBullets() == 0)
	{
		/* Reset bullets and get ready for next sequence */
		SetBonusWait(BONUS_TIME, frame + LINE_DELAY);
		ypos += (textFont->ascent + GAP/2);
		firstTime = True;
		SetGameSpeed(MEDIUM_SPEED);
	}
}

#if NeedFunctionPrototypes
static void DoTimeBonus(Display *display, Window window)
#else
static void DoTimeBonus(display, window)
	Display *display;
	Window window;
#endif
{
	int secs = 0;

	SetGameSpeed(MEDIUM_SPEED);

	/* Get the number of seconds left on the clock */
	secs = GetLevelTimeBonus();

	if (secs > 0)
	{
		/* Draw time bonus text */
		sprintf(string, "Time bonus - %d seconds x %d = %d points", 
			secs, TIME_BONUS, secs * TIME_BONUS);
		DrawShadowCentredText(display, window, textFont, 
			string, ypos, yellow, TOTAL_WIDTH);

		/* Increment the score by the value of the time bonus */
		bonusScore += ComputeScore(TIME_BONUS * secs);
		DisplayScore(display, scoreWindow, bonusScore);
	}
	else
	{
		/* Draw no time bonus text */
		strcpy(string, "No time bonus - not quick enough!");
		DrawShadowCentredText(display, window, textFont, 
			string, ypos, yellow, TOTAL_WIDTH);
	}

	/* Next section setup */
	ypos += (textFont->ascent + GAP/2);
	SetBonusWait(BONUS_HSCORE, frame + LINE_DELAY);
}

#if NeedFunctionPrototypes
static void DoHighScore(Display *display, Window window)
#else
static void DoHighScore(display, window)
	Display *display;
	Window window;
#endif
{
	int myrank = 0;
	char str[5];

	SetGameSpeed(MEDIUM_SPEED);

	/* Obtain current ranking for this score */
	myrank = GetHighScoreRanking(score);

	if (myrank > 0)
	{
		/* Special case for first place */
		if (myrank == 1)
			sprintf(string, "You are ranked 1st. Well done!");
		else
		{
			/* Add the correct grammer for the sentence */
			switch (myrank)
			{
				case 1:
					strcpy(str, "st");
					break;
				
				case 2:
					strcpy(str, "nd");
					break;
				
				case 3:
					strcpy(str, "rd");
					break;
				
				case 4:
				case 5:
				case 6:
				case 7:
				case 8:
				case 9:
				case 10:
					strcpy(str, "th");
					break;
				
				default:
					strcpy(str, "");
					break;
			}

			/* Construct beautiful sentence */
			sprintf(string, "You are currently ranked %d%s.", 
				myrank, str);
		}
	}
	else
		/* What a loser ;-) */
		strcpy(string, "You haven't even qualified for a highscore!");

	/* Draw the text for the game ranking */
	DrawShadowCentredText(display, window, textFont, 
		string, ypos, red, TOTAL_WIDTH);
	ypos += (textFont->ascent + GAP/2);

	SetBonusWait(BONUS_END_TEXT, frame + LINE_DELAY);
}

#if NeedFunctionPrototypes
static void DoEndText(Display *display, Window window)
#else
static void DoEndText(display, window)
	Display *display;
	Window window;
#endif
{
	SetGameSpeed(MEDIUM_SPEED);

	/* Finishing sentence - so you know what level to do */
	sprintf(string, "Prepare for level %d", level+1);
	DrawShadowCentredText(display, window, textFont, 
		string, ypos, yellow, TOTAL_WIDTH);
	XFlush(display);

	SetBonusWait(BONUS_FINISH, frame + 60);
}

#if NeedFunctionPrototypes
static void DoFinish(Display *display, Window window)
#else
static void DoFinish(display, window)
	Display *display;
	Window window;
#endif
{
	/* Setup game window for the next level */
	level++;
	SetupStage(display, playWindow);

	BonusState = BONUS_TEXT;
	SetGameSpeed(FAST_SPEED);

	mode = MODE_GAME;

	XSetWindowBorder(display, playWindow, red);
	XMapWindow(display, playWindow);

	/* Only redraw if the server hasn't backing store on */
	if (DoesBackingStore(XDefaultScreenOfDisplay(display)) != Always)
		SelectiveRedraw(display);
}

#if NeedFunctionPrototypes
void SetBonusWait(enum BonusStates newMode, int waitFrame)
#else
void SetBonusWait(newMode, waitFrame)
	enum BonusStates newMode;
	int waitFrame;
#endif
{
	waitingFrame = waitFrame;
	waitMode = newMode;
	BonusState = BONUS_WAIT;
}

#if NeedFunctionPrototypes
void DoBonusWait(void)
#else
void DoBonusWait()
#endif
{
	/* Wait for the frame we want to come along - then change modes */
	if (frame == waitingFrame)
		BonusState = waitMode;
}

#if NeedFunctionPrototypes
void DoBonus(Display *display, Window window)
#else
void DoBonus(display, window)
	Display *display;
	Window window;
#endif
{
	/* The states within the bonus mode */
	switch (BonusState)
	{
		case BONUS_TEXT:
			DrawTitleText(display, window);
			break;

		case BONUS_SCORE:
			DoScore(display, window);
			break;

		case BONUS_BONUS:
			DoBonuses(display, window);
			break;

		case BONUS_LEVEL:
			DoLevel(display, window);
			break;

		case BONUS_BULLET:
			DoBullets(display, window);
			break;

		case BONUS_TIME:
			DoTimeBonus(display, window);
			break;

		case BONUS_HSCORE:
			DoHighScore(display, window);
			break;

		case BONUS_END_TEXT:
			DoEndText(display, window);
			break;

		case BONUS_FINISH:
			DoFinish(display, window);
			break;

		case BONUS_WAIT:
			DoBonusWait();
			break;

		default:
			break;
	}
}

#if NeedFunctionPrototypes
void RedrawBonus(Display *display, Window window)
#else
void RedrawBonus(display, window)
	Display *display;
	Window window;
#endif
{
	/* This will redraw the entire screen */
	SetupBonusScreen(display, mainWindow);
}

#if NeedFunctionPrototypes
void FreeBonus(Display *display)
#else
void FreeBonus(display)
	Display *display;
#endif
{
	/* Free all the hungry memory leaks */
	if (titlePixmap)	XFreePixmap(display, titlePixmap);
	if (titlePixmapM)	XFreePixmap(display, titlePixmapM);
}

#if NeedFunctionPrototypes
void ComputeAndAddBonusScore(void)
#else
void ComputeAndAddBonusScore()
#endif
{
	/* This function will pre compute the bonus score and add it to the
	 * score. The bonus screen will only change the score by updating the
	 * score display and not the score value. This means the user can hit
	 * space and go to the next screen without delay. Cool.
	 */
	int secs = 0;
	int theLevel = 0;

	secs = GetLevelTimeBonus();
	if (secs > 0)
	{
		/* Compute bonus coin bonus */
		if (numBonus > MAX_BONUS)
		{
			/* More than MAX_BONUS bonus so give super bonus */
			AddToScore((u_long) SUPER_BONUS_SCORE);
		}
		else
		{
			/* Less than MAX_BONUS so work out bonus score */
			AddToScore((u_long) (numBonus * BONUS_COIN_SCORE));
		}

   		/* Adjust the level so that the starting level is taken into account */
   		theLevel = (int) level - GetStartingLevel() + 1;

		/* Increment the score by the value of the level bonus */
		AddToScore((u_long) (LEVEL_SCORE * theLevel));
	}

	/* Increment the score by the value of the bullet bonus */
	if (GetNumberBullets() != 0)
		AddToScore((u_long) (GetNumberBullets() * BULLET_SCORE));

    /* Get the number of seconds left on the clock */
    secs = GetLevelTimeBonus();
    if (secs > 0)
    {
        /* Increment the score by the value of the time bonus */
        AddToScore((u_long) (TIME_BONUS * secs));
	}
}

#if NeedFunctionPrototypes
void ResetBonus(void)
#else
void ResetBonus()
#endif
{
	/* Setup for bonus screen from start */
	BonusState = BONUS_TEXT;
	firstTime = True;
	bonusScore = score;
	ComputeAndAddBonusScore();
	ypos = 180;
	SetGameSpeed(FAST_SPEED);

	DEBUG("Reset bonus mode.")
}
