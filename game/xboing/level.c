#include "include/copyright.h"

/*
 *  Include file dependencies:
 */

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <assert.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <xpm.h>

#if NeedFunctionPrototypes
extern int sscanf(const char *s, const char *format, ...);
extern int fgetc(FILE *stream);
extern int fclose(FILE *stream);
extern time_t time(time_t *tloc);
#else /* !NeedFunctionPrototypes */
extern int sscanf();
extern int fgetc();
extern int fclose();
extern time_t time();
#endif /* NeedFunctionPrototypes */

#include "bitmaps/life.xpm"

#include "include/error.h"
#include "include/audio.h"
#include "include/special.h"
#include "include/intro.h"
#include "include/gun.h"
#include "include/init.h"
#include "include/stage.h"
#include "include/sfx.h"
#include "include/score.h"
#include "include/paddle.h"
#include "include/blocks.h"
#include "include/bonus.h"
#include "include/highscore.h"
#include "include/ball.h"
#include "include/main.h"
#include "include/mess.h"
#include "include/misc.h"

#include "include/level.h"

/*
 *  Internal macro definitions:
 */

#define MAX_LIVES			6
#define START_LIVES			3
#define NEW_LIVE_SCORE_INC	100000L

#define BUF_SIZE			1024

/*
 *  Internal type declarations:
 */

#if NeedFunctionPrototypes
static void DrawLevelTimeBonus(Display *display, Window window, int timebonus);
#else
static void DrawLevelTimeBonus();
#endif

/*
 *  Internal variable declarations:
 */

Pixmap		lifePixmap, lifeMask;	
int 		livesLeft = 3;
u_long		level;
u_long		startlevel;
time_t		gameTime;
int			bonus = 1;
char 		levelTitle[BUF_SIZE];
int 		bonusBlock = False;
static int 	bulletPos;
static int 	timeBonus;

#if NeedFunctionPrototypes
void InitialiseLevelInfo(Display *display, Window window, Colormap colormap)
#else
void InitialiseLevelInfo(display, window, colormap)
	Display *display;
	Window window;
	Colormap colormap;
#endif
{
	XpmAttributes   attributes;
	int 			XpmErrorStatus;

	attributes.valuemask = XpmColormap;
	attributes.colormap = colormap;

	/* Create xpm pixmap for the life */
	XpmErrorStatus = XpmCreatePixmapFromData(display, window, life_xpm, 
		&lifePixmap, &lifeMask, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseLevelInfo()");

	/* Free the xpm pixmap attributes */
	XpmFreeAttributes(&attributes);
}

#if NeedFunctionPrototypes
void DecLevelTimeBonus(Display *display, Window window)
#else
void DecLevelTimeBonus(display, window)
	Display *display;
	Window window;
#endif
{
	if (timeBonus > 0) 
	{
		/* Decrement the time bonus */
		timeBonus--;	

		/* Draw the time bonus */
		DrawLevelTimeBonus(display, window, timeBonus);

		/* Is the time run out */
		if (timeBonus == 0)
		{
			/* Times up buzzer sound */
			if (noSound == False) playSoundFile("buzzer", 70);
		}
	}
}

#if NeedFunctionPrototypes
int GetLevelTimeBonus(void)
#else
int GetLevelTimeBonus()
#endif
{
	/* return the time bonus */
	return timeBonus;	
}

#if NeedFunctionPrototypes
void AddToLevelTimeBonus(Display *display, Window window, int seconds)
#else
void AddToLevelTimeBonus(display, window, seconds)
	Display *display; 
	Window window;
	int seconds;
#endif
{
	/* add to the time bonus */
	SetLevelTimeBonus(display, window, timeBonus + seconds);
}

#if NeedFunctionPrototypes
void SetLevelTimeBonus(Display *display, Window window, int seconds)
#else
void SetLevelTimeBonus(display, window, seconds)
	Display *display;
	Window window;
	int seconds;
#endif
{
	assert(timeBonus >= 0);

	/* Reset the time bonus to desired time */
	timeBonus = seconds;	

	/* Draw the time bonus */
	DrawLevelTimeBonus(display, window, timeBonus);
}

#if NeedFunctionPrototypes
static void DrawLevelTimeBonus(Display *display, Window window, int timebonus)
#else
static void DrawLevelTimeBonus(display, window, timebonus)
	Display *display;
	Window window;
	int timebonus;
#endif
{
	int len, minutes, seconds;
	char str[10];

	/* Work out the minutes and seconds using time bonus */
	minutes = timebonus / 60;
	seconds = timebonus % 60;

	/* Construct a string with the time bonus and draw it */
	sprintf(str, "%02d:%02d", minutes, seconds);
	len = strlen(str);

	/* Draw the text now thanks  - using title font for big numbers */
	XClearWindow(display, window);
	DrawText(display, window, 2, 7, titleFont, black, str, len);

	/* The less time you have the more drastic the colour comes */
	if (timebonus <= 10)
		DrawText(display, window, 0, 5, titleFont, red, str, len);
	else if (timebonus <= 60)
		DrawText(display, window, 0, 5, titleFont, yellow, str, len);
	else 
		DrawText(display, window, 0, 5, titleFont, green, str, len);
}

#if NeedFunctionPrototypes
void DrawLife(Display *display, Window window, int x, int y)
#else
void DrawLife(display, window, x, y)
	Display *display;
	Window window;
	int x;
	int y;
#endif
{
	/* Draw the life pixmap */
	RenderShape(display, window, lifePixmap, lifeMask, 
		x-12, y-12, 25, 24, True);
}

#if NeedFunctionPrototypes
void DisplayLevelInfo(Display *display, Window window, u_long level)
#else
void DisplayLevelInfo(display, window, level)
	Display *display;
	Window window;
	u_long level;
#endif
{
	int i;

	/* Clear the window for level information */
	XClearWindow(display, levelWindow);

	/* Put the level number up */
	DrawOutNumber(display, levelWindow, level, 260, 0);

	/* Draw out the lives left pixmaps */
	for (i = 0; i < livesLeft; i++)
		DrawLife(display, window, 175 - (i * 30), 21);

	/* Draw the score in the score window */
	DisplayScore(display, scoreWindow, score);

	/* Draw all the bullets in the ammo pouch ;-) */
	ReDrawBulletsLeft(display);

	XFlush(display);
}

#if NeedFunctionPrototypes
void SetLevelNumber(int levelNum)
#else
void SetLevelNumber(levelNum)
	int levelNum;
#endif
{
	level = (u_long) levelNum;
}

#if NeedFunctionPrototypes
void SetStartingLevel(int levelNum)
#else
void SetStartingLevel(levelNum)
	int levelNum;
#endif
{
	startlevel = (u_long) levelNum;
}

#if NeedFunctionPrototypes
int GetStartingLevel(void)
#else
int GetStartingLevel()
#endif
{
	return ((int) startlevel);
}

#if NeedFunctionPrototypes
void RedrawLevelInfo(Display *display, Window window)
#else
void RedrawLevelInfo(display, window)
	Display *display;
	Window	window;
#endif
{
	DisplayLevelInfo(display, window, level);
}

#if NeedFunctionPrototypes
void FreeLevelInfo(Display *display)
#else
void FreeLevelInfo(display)
	Display *display;
#endif
{
	/* Free the life pixmap  */
	if (lifePixmap)		XFreePixmap(display, lifePixmap);
	if (lifeMask)		XFreePixmap(display, lifeMask);
}

#if NeedFunctionPrototypes
void DeleteABullet(Display *display)
#else
void DeleteABullet(display)
	Display *display;
#endif
{
	bulletPos = 192 - (GetNumberBullets() * 9);

	/* Take a bullet away from ammo belt */
	EraseTheBullet(display, levelWindow, bulletPos, 43);

	DecNumberBullets();
}

#if NeedFunctionPrototypes
void AddABullet(Display *display)
#else
void AddABullet(display)
	Display *display;
#endif
{
	IncNumberBullets();

	bulletPos = 192 - (GetNumberBullets() * 9);

	/* Add a bullet to the ammo belt */
	DrawTheBullet(display, levelWindow, bulletPos, 43);
}

#if NeedFunctionPrototypes
void ReDrawBulletsLeft(Display *display)
#else
void ReDrawBulletsLeft(display)
	Display *display;
#endif
{
	int x, i;

	/* Draw the bullets in the ammo belt */
	for (i = 0; i < GetNumberBullets(); i++)
	{
		x = 192 - ((i+1) * 9);
		DrawTheBullet(display, levelWindow, x, 43);
	} 
}

#if NeedFunctionPrototypes
int GetNumberLife(void)
#else
int GetNumberLife()
#endif
{
	return livesLeft;
}

#if NeedFunctionPrototypes
void DecExtraLife(Display *display)
#else
void DecExtraLife(display)
	Display *display;
#endif
{
	/* Take a life */
	livesLeft--;

	if (livesLeft < 0) 
		livesLeft = 0;

	/* redraw the level info */
	DisplayLevelInfo(display, levelWindow, level);
}

#if NeedFunctionPrototypes
void AddExtraLife(Display *display)
#else
void AddExtraLife(display)
	Display *display;
#endif
{
	/* Add a new life */
	livesLeft++;

	/* Dont issue too many extra balls! */
	if (livesLeft >= MAX_LIVES) 
		livesLeft = MAX_LIVES;
	else
		SetCurrentMessage(display, messWindow, "Extra ball", True);

	/* redraw the level info */
	DisplayLevelInfo(display, levelWindow, level);
}

#if NeedFunctionPrototypes
void CheckAndAddExtraLife(Display *display, long score)
#else
void CheckAndAddExtraLife(display, score)
	Display *display;
	long score;
#endif
{
	static int ballInc = 0;

	/* Add a new life? */
	if ((score) && ((score / NEW_LIVE_SCORE_INC) != ballInc)) 
	{
		/* Add a new life */
		AddExtraLife(display);
	}

	/* Next inc before adding a new life */
	ballInc = score / NEW_LIVE_SCORE_INC;
}

#if NeedFunctionPrototypes
void HandleGameTimer(Display *display, Window window)
#else
void HandleGameTimer(display, window)
	Display *display;
	Window window;
#endif
{
	static time_t oldTime = 0;

	/* Time to decrement the timer */
	if (time(NULL) > oldTime)
	{
		/* Decrement the timer bonus */
		DecLevelTimeBonus(display, timeWindow);
		oldTime = time(NULL);
	}
}

#if NeedFunctionPrototypes
void CheckGameRules(Display *display, Window window)
#else
void CheckGameRules(display, window)
	Display *display;
	Window window;
#endif
{
	CheckAndAddExtraLife(display, score);

	HandleGameTimer(display, window);

	if (StillActiveBlocks() == False)
	{
		/* Turn off the x2 x4 bonuses */
		Togglex2Bonus(display, False);
		Togglex4Bonus(display, False);
		DrawSpecials(display);

		/* Give the play a big head */
		if (noSound == False) playSoundFile("applause", 70);

		/* Finished level now so set up bonus screen */
        mode = MODE_BONUS;
		SetupBonusScreen(display, mainWindow);
	}
}

#if NeedFunctionPrototypes
void UpdateHighScores(void)
#else
void UpdateHighScores()
#endif
{
	time_t endTime;
	u_long theLevel;

	/* Obtain the game duration in seconds - taking account for pauses */
	endTime = time(NULL) - gameTime - pausedTime;

	/* Adjust the level so that the starting level is taken into account */
	theLevel = level - (u_long) GetStartingLevel() + 1L;

	/* Update the high score table */
	(void) CheckAndAddScoreToHighScore(score, theLevel, endTime, PERSONAL);
	if (CheckAndAddScoreToHighScore(score, theLevel, endTime, GLOBAL) == False)
		ResetHighScore(PERSONAL);
	else
		ResetHighScore(GLOBAL);
}


#if NeedFunctionPrototypes
void EndTheGame(Display *display, Window window)
#else
void EndTheGame(display, window)
	Display *display;
	Window window;
#endif
{
	/* Game over man! */
	SetCurrentMessage(display, messWindow, "- Game Over - ", True);

	if (noSound == False)
		playSoundFile("game_over", 100);

	TurnSpecialsOff(display);

	UpdateHighScores();

	/* redraw the level info */
	DisplayLevelInfo(display, levelWindow, level);

	/* Reset game and setup for high score table */
	gameActive = False;
	SetLevelNumber(GetStartingLevel());
	ResetIntroduction();
	mode = MODE_HIGHSCORE;

}

#if NeedFunctionPrototypes
void DeadBall(Display *display, Window window)
#else
void DeadBall(display, window)
	Display *display;
	Window window;
#endif
{
	if (noSound == False) playSoundFile("balllost", 100);

	/* Disable some of the specials and update display */
	TurnSpecialsOff(display);
	SetReverseOff();
	ToggleMultiBall(display, False);
	DrawSpecials(display);

	SetCurrentMessage(display, messWindow, "Ball Terminated!", True);

	if (livesLeft <= 0 && GetAnActiveBall() == -1)
		EndTheGame(display, window);
	else 
	{
		/* Start a new ball if some to spare */
		if (GetAnActiveBall() == -1 && livesLeft > 0)
		{
			/* Decrement the number of lives left and display so */
			DecExtraLife(display);

			ResetBallStart(display, window);
		}
	}
}

#if NeedFunctionPrototypes
char *GetLevelName(void)
#else
char *GetLevelName()
#endif
{
	/* Return the name of the current level */
	return (levelTitle);
}

#if NeedFunctionPrototypes
int ReadNextLevel(Display *display, Window window, char *levelName)
#else
int ReadNextLevel(display, window, levelName)
	Display *display;
	Window window;
	char *levelName;
#endif
{
	FILE *levelFile;
	int row, col, type;
	char str[BUF_SIZE];
	int	timeLimit = 180;
	char *temp;

	/* Clear all existing mess in structures */
	ClearBlockArray();

	/* Setup the new level data */
    blocksExploding = 0;
	colWidth 	= PLAY_WIDTH / MAX_COL;
	rowHeight 	= PLAY_HEIGHT / MAX_ROW;
	bonusBlock 	= False;
	ResetNumberBonus();

	/* Open the new level data file for reading */
	if ((levelFile = fopen(levelName, "r")) == NULL)
	{
		ErrorMessage("Cannot load level data - check level directory.");
		return False;
	}

	/* Obtain the title string */
	fgets(levelTitle, BUF_SIZE, levelFile);

	/* Remove the carriage return in the title */
	temp = strchr(levelTitle, '\n');
	*temp = '\0';

	if (debug == True) sprintf(str, "level #%d : <%s>", level, levelTitle);
	DEBUG(str)

	/* Now get the time bonus from the level file */
	fgets(str, BUF_SIZE, levelFile);
	if (sscanf(str, "%d", &timeLimit) != 1)
	{
		ErrorMessage("Cannot parse level data - time bonus error.");
		return False;
	}

	/* Set and draw the time limit for the level */
	SetLevelTimeBonus(display, timeWindow, timeLimit);

	for (row = 0; row < (MAX_ROW - 3); row++)
	{
		for (col = 0; col < MAX_COL; col++)
		{
			/* Get the next character from the level data file */
			type = fgetc(levelFile);

			switch (type)
			{
				case 'H' :	/* hyperspace block - walls are now gone */
					AddNewBlock(display, window, row, col, HYPERSPACE_BLK, 0);
					break;
					
				case 'B' :	/* bullet block - ammo */
					AddNewBlock(display, window, row, col, BULLET_BLK, 0);
					break;
					
				case 'c' :	/* maximum ammo bullet block  */
					AddNewBlock(display, window, row, col, MAXAMMO_BLK, 0);
					break;
					
				case 'r' :	/* A red block */
					AddNewBlock(display, window, row, col, RED_BLK, 0);
					break;
					
				case 'g' :	/* A green block */
					AddNewBlock(display, window, row, col, GREEN_BLK, 0);
					break;
					
				case 'b' :	/* A blue block */
					AddNewBlock(display, window, row, col, BLUE_BLK, 0);
					break;
					
				case 't' :	/* A tan block */
					AddNewBlock(display, window, row, col, TAN_BLK, 0);
					break;
					
				case 'p' :	/* A purple block */
					AddNewBlock(display, window, row, col, PURPLE_BLK, 0);
					break;
					
				case 'y' :	/* A yellow block */
					AddNewBlock(display, window, row, col, YELLOW_BLK, 0);
					break;
					
				case 'w' :	/* A solid wall block */
					AddNewBlock(display, window, row, col, BLACK_BLK, 0);
					break;
					
				case '0' :	/* A counter block - no number */
					AddNewBlock(display, window, row, col, COUNTER_BLK, 0);
					break;
					
				case '1' :	/* A counter block level 1 */
					AddNewBlock(display, window, row, col, COUNTER_BLK, 1);
					break;
					
				case '2' : /* A counter block level 2 */
					AddNewBlock(display, window, row, col, COUNTER_BLK, 2);
					break;
					
				case '3' : /* A counter block level 3 */
					AddNewBlock(display, window, row, col, COUNTER_BLK, 3);
					break;
					
				case '4' : /* A counter block level 4 */
					AddNewBlock(display, window, row, col, COUNTER_BLK, 4);
					break;
					
				case '5' : /* A counter block level 5  - highest */
					AddNewBlock(display, window, row, col, COUNTER_BLK, 5);
					break;
					
				case '+' : /* A roamer block */
					AddNewBlock(display, window, row, col, ROAMER_BLK, 0);
					break;

				case 'X' : /* A bomb block - arggh! */
					AddNewBlock(display, window, row, col, BOMB_BLK, 0);
					break;

				case 'D' : /* A death block */
					AddNewBlock(display, window, row, col, DEATH_BLK, 
						SHOTS_TO_KILL_SPECIAL);
					break;

				case 'L' : /* An extra ball block */
					AddNewBlock(display, window, row, col, EXTRABALL_BLK, 0);
					break;

				case 'M' : /* A machine gun block */
					AddNewBlock(display, window, row, col, MGUN_BLK, 
						SHOTS_TO_KILL_SPECIAL);
					break;

				case 'W' : /* A wall off block */
					AddNewBlock(display, window, row, col, WALLOFF_BLK, 
						SHOTS_TO_KILL_SPECIAL);
					break;

				case '?' : /* A random changing block */
					AddNewBlock(display, window, row, col, RANDOM_BLK, 0);
					break;

				case 'd' : /* A dropping block */
					AddNewBlock(display, window, row, col, DROP_BLK, 0);
					break;

				case 'T' : /* A extra time block */
					AddNewBlock(display, window, row, col, TIMER_BLK, 0);
					break;

				case 'm' : /* A multiple ball block */
					AddNewBlock(display, window, row, col, MULTIBALL_BLK, 
						SHOTS_TO_KILL_SPECIAL);
					break;

				case 's' : /* A sticky block */
					AddNewBlock(display, window, row, col, STICKY_BLK, 
						SHOTS_TO_KILL_SPECIAL);
					break;

				case 'R' :	/* reverse block - switch paddle control */
					AddNewBlock(display, window, row, col, REVERSE_BLK, 
						SHOTS_TO_KILL_SPECIAL);
					break;
					
				case '<' :	/* shrink paddle block - make paddle smaller */
					AddNewBlock(display, window, row, col, PAD_SHRINK_BLK, 
						SHOTS_TO_KILL_SPECIAL);
					break;
					
				case '>' :	/* expand paddle block - make paddle bigger */
					AddNewBlock(display, window, row, col, PAD_EXPAND_BLK, 
						SHOTS_TO_KILL_SPECIAL);
					break;
					
				default:
					break;
			}
		}

		/* Get the newline */
		type = fgetc(levelFile);
	}

	/* Close our level data file */
	if (fclose(levelFile) < 0)
		WarningMessage("Cannot close level data file.");

	return True;
}
