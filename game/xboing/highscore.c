#include "include/copyright.h"

/*
 *  Include file dependencies:
 */

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/param.h>
#include <sys/file.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <xpm.h>

#if NeedFunctionPrototypes
extern size_t strftime(char *buf, size_t bufsize, const char *fmt, const struct tm *tm);
extern int fprintf(FILE *stream, const char* format, ...);
extern int fflush(FILE *stream);
extern time_t time(time_t *tloc);
extern int fread(void *ptr, size_t size, size_t nitems, FILE *stream);
extern int fwrite(const void *ptr, size_t size, size_t nitems, FILE *stream);
extern int fclose(FILE *stream);
extern int lockf(int fd, int cmd, long size);
#else /* !NeedFunctionPrototypes */
extern int strftime();
extern int fprintf();
extern int fflush();
extern time_t time();
extern int fread();
extern int fwrite();
extern int fclose();
extern int lockf();
#endif /* NeedFunctionPrototypes */

#include "include/error.h"
#include "include/misc.h"
#include "include/main.h"
#include "include/audio.h"
#include "include/special.h"
#include "include/init.h"
#include "include/inst.h"
#include "include/stage.h"
#include "include/blocks.h"
#include "include/ball.h"
#include "include/sfx.h"
#include "include/score.h"
#include "include/paddle.h"
#include "include/level.h"
#include "include/mess.h"
#include "include/intro.h"

#include "bitmaps/highscores.xpm"

#include "include/highscore.h"

/*
 *  Internal macro definitions:
 */

#define GAP				13
#define NUM_HIGHSCORES	10

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

/* My locking defines only */
#define LOCK_FILE 		0
#define UNLOCK_FILE 	1

/* System locking defines */

#ifndef NO_LOCKING
#ifndef LOCK_EX
#define LOCK_EX F_LOCK
#endif
#ifndef LOCK_UN
#define LOCK_UN F_ULOCK
#endif
#endif /* NO_LOCKING */

/*
 *  Internal type declarations:
 */

#if NeedFunctionPrototypes
static void SetHighScoreWait(enum HighScoreStates newMode, int waitFrame);
static void InitialiseHighScores(void);
static void SortHighScores(void);
static void DeleteScore(int i);
static int LockUnlock(int cmd);
#else
static int LockUnlock();
static void DeleteScore();
static void SetHighScoreWait();
static void InitialiseHighScores();
static void SortHighScores();
#endif

/*
 *  Internal variable declarations:
 */

static int nextFrame = 0;
static int endFrame = 0;
enum HighScoreStates HighScoreState;
static Pixmap titlePixmap, titlePixmapM;
static int waitingFrame;
enum HighScoreStates waitMode;
static int sparkley = 0;
static int sindex = 0;
static int si = 0;
static int scoreType = GLOBAL;
static char nickName[22];

highScoreEntry highScores[NUM_HIGHSCORES];

#if NeedFunctionPrototypes
void SetNickName(char *nick)
#else
void SetNickName(nick)
	char *nick;
#endif
{
	/* Change the users nick name */
	strncpy(nickName, nick, 20);
}

#if NeedFunctionPrototypes
char *GetNickName(void)
#else
char *GetNickName()
#endif
{
	/* Return the nickname or NULL */
	if (nickName[0] == '\0')
		return NULL;
	else
		return nickName;
}

#if NeedFunctionPrototypes
void SetUpHighScore(Display *display, Window window, Colormap colormap)
#else
void SetUpHighScore(display, window, colormap)
	Display *display;
	Window window;
	Colormap colormap;
#endif
{
	XpmAttributes   attributes;
	int             XpmErrorStatus;

	attributes.valuemask = XpmColormap;
	attributes.colormap = colormap;

	/* Load the highscore title pixmap */
	XpmErrorStatus = XpmCreatePixmapFromData(display, window, highscores_xpm,
		&titlePixmap, &titlePixmapM, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseHighScore()");

    /* Free the xpm pixmap attributes */
	XpmFreeAttributes(&attributes);

	/* Setup the high score table */
	InitialiseHighScores();
	ResetHighScore(GLOBAL);
}

#if NeedFunctionPrototypes
static void DoTitle(Display *display, Window window)
#else
static void DoTitle(display, window)
	Display *display;
	Window window;
#endif
{
	char string[80];

   	DrawStageBackground(display, window, BACKGROUND_SPACE, True);

	/* Draw the highscore title */
	RenderShape(display, window, titlePixmap, titlePixmapM,
		59, 20, 377, 37, False);

	/* Let the dudes know how to start the game */
	strcpy(string, "Insert coin to start the game");
	DrawShadowCentredText(display, window, textFont,
		string, PLAY_HEIGHT - 40, tann, PLAY_WIDTH);

	/* Set the message window to have the other display toggle key */
	if (scoreType == GLOBAL)
		SetCurrentMessage(display, messWindow, 
			"<H> - Personal Best", False);
	else
		SetCurrentMessage(display, messWindow, 
			"<h> - Roll of Honour", False);

	SetHighScoreWait(HIGHSCORE_SHOW, frame + 10);
}

#if NeedFunctionPrototypes
static void DoHighScores(Display *display, Window window)
#else
static void DoHighScores(display, window)
	Display *display;
	Window window;
#endif
{
	int i, len, plen;
	int xr = 30;
	int ym = 75;
	int xs = xr + 30;
	int xl = xs + 80;
	int xt = xl + 35;
	int xg = xt + 55;
	int xn = xg + 90;
	int y = 180;
	char string[80];
	char string2[80];
	char *p;
	time_t	theTime;

	/* Read the high score table */
	if (ReadHighScoreTable(GLOBAL) == False)
		InitialiseHighScores();

	/* Draw the boing master at top of the list */
	strcpy(string, "Boing Master");
	DrawShadowCentredText(display, window, textFont, 
		string, ym, red, PLAY_WIDTH);
	ym += textFont->ascent + GAP;

	/* Render the boing master's name */
	strcpy(string, highScores[0].name);
	DrawShadowCentredText(display, window, titleFont, 
		string, ym, yellow, PLAY_WIDTH);
	ym += textFont->ascent + GAP * 2;

	/* Read the high score table */
	if (ReadHighScoreTable(scoreType) == False)
		InitialiseHighScores();

	/* Explain that this is the roll of honour */
	if (scoreType == GLOBAL)
		strcpy(string, "- The Roll of Honour -");
	else
		strcpy(string, "- Personal Best -");
	DrawShadowCentredText(display, window, textFont, 
		string, ym, green, PLAY_WIDTH);
	ym += textFont->ascent + GAP;

	/* Draw the titles for the highscore table */
	strcpy(string, "#");
	len = strlen(string);
	DrawText(display, window, xr+2, y+2, textFont, black, string, len);
	DrawText(display, window, xr, y, textFont, yellow, string, len);

	strcpy(string, "Score");
	len = strlen(string);
	DrawText(display, window, xs+2, y+2, textFont, black, string, len);
	DrawText(display, window, xs, y, textFont, yellow, string, len);

	strcpy(string, "L");
	len = strlen(string);
	DrawText(display, window, xl+2, y+2, textFont, black, string, len);
	DrawText(display, window, xl, y, textFont, yellow, string, len);

	strcpy(string, "Time");
	len = strlen(string);
	DrawText(display, window, xt+2, y+2, textFont, black, string, len);
	DrawText(display, window, xt, y, textFont, yellow, string, len);

	strcpy(string, "Date");
	len = strlen(string);
	DrawText(display, window, xg+2, y+2, textFont, black, string, len);
	DrawText(display, window, xg, y, textFont, yellow, string, len);

	strcpy(string, "Player");
	len = strlen(string);
	DrawText(display, window, xn+2, y+2, textFont, black, string, len);
	DrawText(display, window, xn, y, textFont, yellow, string, len);

	y += textFont->ascent + GAP / 2;

	/* Draw the line above the table */
	DrawLine(display, window, 22, y+2, PLAY_WIDTH - 18, y+2, black, 3);
	DrawLine(display, window, 20, y, PLAY_WIDTH - 20, y, white, 3);

	y += textFont->ascent;

	/* Draw the scores into the table */
	for (i = 0; i < NUM_HIGHSCORES; i++)
	{
		if (ntohl(highScores[i].score) > (u_long) 0)
		{
			/* Draw the rank */
			sprintf(string, "%d", i+1);
			if (ntohl(highScores[i].score) != score)
				DrawShadowText(display, window, textFont, string, xr, y, tann);
			else
				DrawShadowText(display, window, textFont, string, xr, y, green);

			/* Draw the score */
			sprintf(string, "%ld", ntohl(highScores[i].score));
			if (ntohl(highScores[i].score) != score)
				DrawShadowText(display, window, textFont, string, xs, y, red);
			else
				DrawShadowText(display, window, textFont, string, xs, y, green);

			/* Write out the level reached */
			sprintf(string, "%ld", ntohl(highScores[i].level));
			DrawShadowText(display, window, textFont, string, xl, y, green);

			/* Game duration in minutes and seconds */
			sprintf(string, "%d'%d\"",
				ntohl(highScores[i].gameTime) / 60,
				ntohl(highScores[i].gameTime) % 60);
			if (ntohl(highScores[i].score) != score)
				DrawShadowText(display, window, textFont, string, xt, y, tann);
			else
				DrawShadowText(display, window, textFont, string, xt, y, green);

			/* Construct the date for the highscore entry */
			theTime = (time_t) ntohl(highScores[i].time);
			strftime(string, 10, "%d %b %y", localtime(&theTime));
			string[9] = '\0';	/* Just to be sure */
			if (ntohl(highScores[i].score) != score)
				DrawShadowText(display, window, textFont, string, xg, y, white);
			else	
				DrawShadowText(display, window, textFont, string, xg, y, green);

			/* Name of the boing master */
			strcpy(string, highScores[i].name);
			plen = XTextWidth(textFont, string, len);

			/* Only use the first name if too big for screen */
			if ((plen + xn) > PLAY_WIDTH)
			{
				/* Find the first space and null terminate there */
				p = strchr(string, ' ');
				*p = '\0';
				strcpy(string2, string);

				/* Draw a much smaller version of your name */
				if (ntohl(highScores[i].score) != score)
					DrawShadowText(display, window, textFont, string2, xn, y, 
						yellow);
				else
					DrawShadowText(display, window, textFont, string2, xn, y, 
						green);
			}
			else
			{
				/* Write out users name */
				if (ntohl(highScores[i].score) != score)
					DrawShadowText(display, window, textFont, string, xn, y, 
						yellow);
				else
					DrawShadowText(display, window, textFont, string, xn, y, 
						green);
			}

		}
		else
		{
			/* This bit is for when the table entry is blank */
			sprintf(string, "%d", i+1);
			DrawShadowText(display, window, textFont, string, xr, y, tann);

			/* Draw dashes for blank entries */
			strcpy(string, "--");
			DrawShadowText(display, window, textFont, string, xs, y, red);
			DrawShadowText(display, window, textFont, string, xl, y, green);
			DrawShadowText(display, window, textFont, string, xt, y, tann);
			DrawShadowText(display, window, textFont, string, xg, y, white);
			DrawShadowText(display, window, textFont, string, xn, y, yellow);
		}

		y += textFont->ascent + GAP;
	}

	/* Draw the line above the table */
	DrawLine(display, window, 22, y+2, PLAY_WIDTH - 18, y+2, black, 3);
	DrawLine(display, window, 20, y, PLAY_WIDTH - 20, y, white, 3);

	SetHighScoreWait(HIGHSCORE_SPARKLE, frame + 2);
}

#if NeedFunctionPrototypes
static void DoTitleSparkle(Display *display, Window window)
#else
static void DoTitleSparkle(display, window)
	Display *display;
	Window window;
#endif
{
	static int sindex = 0;
	static int delay = 30;

	if ((frame % delay) == 0)
	{
		if (delay == 800) delay = 30;

		/* Draw stars either side of high scores title */
		RenderShape(display, window, stars[sindex], starsM[sindex],
			25, 27, 20, 20, True);
		RenderShape(display, window, stars[10-sindex], starsM[10-sindex],
			PLAY_WIDTH-45, 27, 20, 20, True);

		sindex++;
		if (sindex == 11) 
		{
			/* Clear away the stars */
			XClearArea(display, window, 25, 27, 20, 20, False);
			XClearArea(display, window, PLAY_WIDTH-45, 27, 20, 20, False);

			sindex = 0;
			if (delay == 30) delay = 800;
		}
	}
}

#if NeedFunctionPrototypes
static void DoSparkle(Display *display, Window window)
#else
static void DoSparkle(display, window)
	Display *display;
	Window window;
#endif
{
	static Pixmap store;	/* backing store for the sparkle */
	static int x = 6;		/* X position of the sparkle */

	if (!store)
	{
		/* Create some backing store for the sparkle star */
		store = XCreatePixmap(display, window, 20, 20,
			DefaultDepth(display, XDefaultScreen(display)));
	}

	if (frame == endFrame)
	{
		/* Bug out of the sparkle and goto next sequence */
		si = 0;
		sindex = 0;
		sparkley = 180 + textFont->ascent + 20;

		/* End the sparkle and now set up for finish */
		SetHighScoreWait(HIGHSCORE_FINISH, frame + 1);
		return;
	}

	if (sindex == 0)
		XCopyArea(display, window, store, gc, x, sparkley, 20, 20, 0, 0);

	if (frame == nextFrame)
	{
		/* Draw the sparkle frame */
		RenderShape(display, window, stars[sindex], starsM[sindex],
			x, sparkley, 20, 20, True);


		sindex++;
		nextFrame = frame + 30;

		/* Last frame of sparkle so reset */
		if (sindex == 11)
		{
			XCopyArea(display, store, window, gc, 0, 0, 20, 20, x, sparkley);

			sindex = 0;
			nextFrame = frame + 100;
			sparkley += textFont->ascent + GAP;

			si++;

			if ((ntohl(highScores[si].score) <= (u_long) 0) || (si == 10))
			{
				si = 0;
				sindex = 0;
				sparkley = 180 + textFont->ascent + 20;
			}
		}
	}
}



#if NeedFunctionPrototypes
static void SetHighScoreWait(enum HighScoreStates newMode, int waitFrame)
#else
static void SetHighScoreWait(newMode, waitFrame)
	enum HighScoreStates newMode;
	int waitFrame;
#endif
{
	waitingFrame = waitFrame;
	waitMode = newMode;
	HighScoreState = HIGHSCORE_WAIT;
}

#if NeedFunctionPrototypes
void DoHighScoreWait(void)
#else
void DoHighScoreWait()
#endif
{
	/* Wait for the end frame then change mode */
	if (frame == waitingFrame)
		HighScoreState = waitMode;
}

#if NeedFunctionPrototypes
static void DoFinish(Display *display, Window window)
#else
static void DoFinish(display, window)
	Display *display;
	Window window;
#endif
{
	mode = MODE_INTRO;
	HighScoreState = HIGHSCORE_TITLE;
	ResetIntroduction();

	if (noSound == False)
		playSoundFile("whizzo", 50);

	SetGameSpeed(FAST_SPEED);
}

#if NeedFunctionPrototypes
void HighScore(Display *display, Window window)
#else
void HighScore(display, window)
	Display *display;
	Window window;
#endif
{
	/* Switch on the highscore table state */
	switch (HighScoreState)
	{
		case HIGHSCORE_TITLE:
			if (getSpecialEffects(display) == True)
			{
				/* Clear bffer and draw title */
    			DrawStageBackground(display, bufferWindow, BACKGROUND_SPACE, 
					True);
    			DrawStageBackground(display, window, BACKGROUND_SPACE, 
					False);
				DoTitle(display, bufferWindow);
			}
			else
				DoTitle(display, window);
			break;

		case HIGHSCORE_SHOW:
			if (getSpecialEffects(display) == True)
			{
				DoHighScores(display, bufferWindow);
				while (WindowBlindEffect(display, window));
			}
			else
				DoHighScores(display, window);
			break;

		case HIGHSCORE_SPARKLE:
			DoTitleSparkle(display, window);
			DoSparkle(display, window);
			if ((frame % FLASH) == 0)
				RandomDrawSpecials(display);
			BorderGlow(display, window);
			break;

		case HIGHSCORE_FINISH:
			DoFinish(display, window);
			break;

		case HIGHSCORE_WAIT:
			DoHighScoreWait();
			break;

		default:
			break;
	}
}

#if NeedFunctionPrototypes
void CommandlineHighscorePrint(void)
#else
void CommandlineHighscorePrint()
#endif
{
	char string[11];
	int i;
	time_t theTime;

	InitialiseHighScores();

	/* Must have table initialised with scores */
	if (ReadHighScoreTable(GLOBAL) == False)
		InitialiseHighScores();

	/* Print out a pretty title message for scores */
	fprintf(stdout, "XBoing Roll of Honour\n\n");
	fprintf(stdout, "Rank\tScore\t  Level\tTime\tDate       Name\n");
	fprintf(stdout, 
		"----------------------------------------------------------------\n");

	/* Zoom through the highscore table from the top */
	for (i = 0; i < NUM_HIGHSCORES; i++)
	{
		theTime = (time_t) ntohl(highScores[i].time);
		strftime(string, 10, "%d %b %y", localtime(&theTime));

		if (ntohl(highScores[i].score) > 0)
		{
			/* Print out the actual record */
			fprintf(stdout, "%d\t%ld\t  %ld\t%d'%d\"\t%s  %s\n", 
				i + 1, ntohl(highScores[i].score), 
				ntohl(highScores[i].level),
				ntohl(highScores[i].gameTime) / 60, 
				ntohl(highScores[i].gameTime) % 60, 
				string, highScores[i].name);
		}
	}

	/* Print a trailing line to make the table look good */
	fprintf(stdout, 
		"----------------------------------------------------------------\n");
	fflush(stdout);

	/* Now display the personal highscore table */

	InitialiseHighScores();

	/* Must have table initialised with scores */
	if (ReadHighScoreTable(PERSONAL) == False)
		InitialiseHighScores();

	/* Print out a pretty title message for scores */
	fprintf(stdout, "\nPersonal Best\n\n");
	fprintf(stdout, "Rank\tScore\t  Level\tTime\tDate       Name\n");
	fprintf(stdout, 
		"----------------------------------------------------------------\n");

	/* Zoom through the highscore table from the top */
	for (i = 0; i < NUM_HIGHSCORES; i++)
	{
		theTime = (time_t) ntohl(highScores[i].time);
		strftime(string, 10, "%d %b %y", localtime(&theTime));

		if (ntohl(highScores[i].score) > 0)
		{
			/* Print out the actual record */
			fprintf(stdout, "%d\t%ld\t  %ld\t%d'%d\"\t%s  %s\n", 
				i + 1, ntohl(highScores[i].score), 
				ntohl(highScores[i].level),
				ntohl(highScores[i].gameTime) / 60, 
				ntohl(highScores[i].gameTime) % 60, 
				string, highScores[i].name);
		}
	}

	/* Print a trailing line to make the table look good */
	fprintf(stdout, 
		"----------------------------------------------------------------\n");
	fflush(stdout);
}

#if NeedFunctionPrototypes
int GetHighScoreRanking(u_long score)
#else
int GetHighScoreRanking(score)
	u_long score;
#endif
{
	int i;

	/* Must have table initialised with scores */
	if (ReadHighScoreTable(GLOBAL) == False)
		InitialiseHighScores();

	/* Zoom through the highscore table from the top */
	for (i = 0; i < NUM_HIGHSCORES; i++)
	{
		/* Is my score better than theirs */
		if (score >= ntohl(highScores[i].score))
			return (i + 1);
	}

	/* Not even in highscore table yet! */
	return -1;
}

#if NeedFunctionPrototypes
static void ShiftScoresDown(int j, u_long score, u_long level, 
	time_t gameTime, char *name)
#else
static void ShiftScoresDown(j, score, level, gameTime, name)
	int j;
	u_long score;
	u_long level;
	time_t gameTime;
	char *name;
#endif
{
	/* This function will shift all score below the index down
	 * towards the end and kill off the last dude. Sorry mate.
	 */
	int i;

	/* Move all the scores down one notch */
	for (i = NUM_HIGHSCORES-1; i > j; i--)
	{
		/* Shift the scores down one notch */
		highScores[i].score 	= highScores[i-1].score;
		highScores[i].level 	= highScores[i-1].level;
		highScores[i].time 		= highScores[i-1].time;
		highScores[i].gameTime 	= highScores[i-1].gameTime;
		highScores[i].userId 	= highScores[i-1].userId;
		strcpy(highScores[i].name, highScores[i-1].name);
	}

	/* Add our new high score to the high score table */
	highScores[j].score 	= htonl(score);
	highScores[j].level 	= htonl(level);
	highScores[j].gameTime 	= htonl(gameTime);
	highScores[j].userId 	= htonl(getuid());
	highScores[j].time 		= htonl(time(NULL));
	strcpy(highScores[j].name, name);
}

#if NeedFunctionPrototypes
static void DeleteScore(int j)
#else
static void DeleteScore(j)
	int j;
#endif
{
	/* Delete the given score and shift all others up to fill in the hole */
	int i;

	/* Move all the scores down one notch */
	for (i = j; i < NUM_HIGHSCORES-1; i++)
	{
		/* Shift the scores up one notch */
		highScores[i].score 	= highScores[i+1].score;
		highScores[i].level 	= highScores[i+1].level;
		highScores[i].time 		= highScores[i+1].time;
		highScores[i].gameTime 	= highScores[i+1].gameTime;
		highScores[i].userId 	= highScores[i+1].userId;
		strcpy(highScores[i].name, highScores[i+1].name);
	}

	highScores[i].score 	= htonl((u_long)0);
	highScores[i].level 	= htonl((u_long)1);
	highScores[i].gameTime 	= htonl(0);
	highScores[i].userId 	= htonl(getuid());
	highScores[i].time 		= htonl(time(NULL));
	strcpy(highScores[i].name, "To be announced!");
}

#if NeedFunctionPrototypes
int CheckAndAddScoreToHighScore(u_long score, u_long level, time_t gameTime,
	int type)
#else
int CheckAndAddScoreToHighScore(score, level, gameTime, type)
	u_long score;
	u_long level;
	time_t gameTime;
	int type;
#endif
{
	int i;
	int id = -1;
	char name[80];

	/* Lock the file for me only */
	if (type == GLOBAL)
		id = LockUnlock(LOCK_FILE);

	/* Read in the lastest scores */
	if (ReadHighScoreTable(type) == False)
		InitialiseHighScores();

	/* Speed up by obtaining users name */
	if (nickName[0] != '\0')
		strcpy(name, nickName);
	else
		strcpy(name, getUsersFullName());

	if (type == GLOBAL)
	{
		/* Go through the highscore table */
		for (i = 0; i < NUM_HIGHSCORES; i++)
		{
			if (ntohl(highScores[i].userId) == getuid())
			{
				/* Can the last score be added to the highscores */
				if (score > ntohl(highScores[i].score))
				{
					/* Delete and move up all old scores */
					DeleteScore(i);

					break;
				}
				else
				{
					/* Don't add as score is smaller */
					return False;
				}
			}
		}	/* for */

		/* Now add the new score into the table */
		for (i = 0; i < NUM_HIGHSCORES; i++)
		{
			/* Can the last game be added to the highscores */
			if (score > ntohl(highScores[i].score))
			{
				ShiftScoresDown(i, score, level, gameTime, name);

				/* Add to the highscore by writing it out */
				(void) WriteHighScoreTable(type);

				/* Unlock the file now thanks */
				if (id != -1) 
					id = LockUnlock(UNLOCK_FILE);

				/* Yes - it was placed in the highscore */
				return True;
			}
		}

		/* Unlock the file now thanks */
		if (id != -1) 
			id = LockUnlock(UNLOCK_FILE);

		/* Not even a highscore - loser! */
		return False;
	}
	else	/* Type == PERSONAL */
	{
		/* Go through the highscore table */
		for (i = 0; i < NUM_HIGHSCORES; i++)
		{
			/* Can the last game be added to the highscores */
			if (score > ntohl(highScores[i].score))
			{
				ShiftScoresDown(i, score, level, gameTime, name);

				/* Add to the highscore by writing it out */
				(void) WriteHighScoreTable(type);

				/* Yes - it was placed in the highscore */
				return True;
			}
		}

		/* Not even a highscore - loser! */
		return False;
	}
}

#if NeedFunctionPrototypes
static void SortHighScores(void)
#else
static void SortHighScores()
#endif
{
	int i, j;
	highScoreEntry tempHighScore;

	/* The old bubble sort strikes again :-) */
	for (i = 0; i < NUM_HIGHSCORES - 1; ++i)
	{
		for (j = NUM_HIGHSCORES - 1; j > i; --j)
		{
			/* Who has the higher score */
			if (ntohl(highScores[j-1].score)
				< ntohl(highScores[j].score))
			{
				/* Swap the entries around - use memcpy in future */
				tempHighScore.score 		= highScores[j-1].score;
				tempHighScore.level 		= highScores[j-1].level;
				tempHighScore.gameTime 		= highScores[j-1].gameTime;
				tempHighScore.userId 		= highScores[j-1].userId;
				strcpy(tempHighScore.name, highScores[j-1].name);

				highScores[j-1].score 		= highScores[j].score;
				highScores[j-1].level 		= highScores[j].level;
				highScores[j-1].gameTime 	= highScores[j].gameTime;
				highScores[j-1].userId 		= highScores[j].userId;
				strcpy(highScores[j-1].name, highScores[j].name);

				highScores[j].score 		= tempHighScore.score;
				highScores[j].level 		= tempHighScore.level;
				highScores[j].gameTime 		= tempHighScore.gameTime;
				highScores[j].userId 		= tempHighScore.userId;
				strcpy(highScores[j].name, tempHighScore.name);
			}
		}
	}
}

#if NeedFunctionPrototypes
static void InitialiseHighScores(void)
#else
static void InitialiseHighScores()
#endif
{
	int i;

	/* Loop down table clearing everything out */
	for (i = 0; i < NUM_HIGHSCORES; i++)
	{
		/* Null out all entries */
		highScores[i].score 	= htonl((u_long) 0);
		highScores[i].level 	= htonl((u_long) 1);
		highScores[i].gameTime 	= htonl(0);
		highScores[i].userId 	= htonl(0);
		highScores[i].time 		= htonl(time(NULL));
		strcpy(highScores[i].name, "To be announced!");
	}
}

#if NeedFunctionPrototypes
int ReadHighScoreTable(int type)
#else
int ReadHighScoreTable(type)
	int type;
#endif
{
	/* Read the high score table into memory structure */
	FILE *hsfp;
	int i;
	char filename[MAXPATHLEN];
	char *str;

	/* Are we going to use the global or personal highscore file */
	if (type == GLOBAL)
	{
		/* Use the environment variable if it exists */
		if ((str = getenv("XBOING_SCORE_FILE")) != NULL)
			strcpy(filename, str);
		else
			strcpy(filename, HIGH_SCORE_FILE);
	}
	else
		sprintf(filename, "%s/.xboing-scores", GetHomeDir());

	/* Open the high score file */
    if ((hsfp = fopen(filename, "r")) == NULL)
	{
		/* Cannot open the high score file */
		WarningMessage("Cannot open high score file for reading.");
		return False;
	}

	/* Read all high score entries */
	for (i = 0; i < NUM_HIGHSCORES; i++)
	{
		/* Read the highscore entry */
    	if (fread((char*)&highScores[i], sizeof(highScoreEntry), 1, hsfp) != 1)
		{
			if (fclose(hsfp) < 0)
				WarningMessage("Cannot close high score file.");
			return False;
		}
	}

	/* Close the high score file */
	if (fclose(hsfp) < 0)
		WarningMessage("Cannot close high score file.");

	return True;
}


#if NeedFunctionPrototypes
int WriteHighScoreTable(int type)
#else
int WriteHighScoreTable(type)
	int type;
#endif
{
	/* write the high score table to the high score file */
	FILE *hsfp;
	int i;
	char filename[MAXPATHLEN];
	char *str;

	/* Make sure the table is sorted */
	SortHighScores();

	/* Are we going to use the global or personal highscore file */
	if (type == GLOBAL)
	{
		/* Use the environment variable if it exists */
		if ((str = getenv("XBOING_SCORE_FILE")) != NULL)
			strcpy(filename, str);
		else
			strcpy(filename, HIGH_SCORE_FILE);
	}	
	else
		sprintf(filename, "%s/.xboing-scores", GetHomeDir());

	/* Open the high score file */
    if ((hsfp = fopen(filename, "w+")) == NULL)
	{
		/* Cannot open the high score file */
		WarningMessage("Cannot open high score file for writing.");
		return False;
	}

	/* Write out all high score entries */
	for (i = 0; i < NUM_HIGHSCORES; i++)
	{
		/* Write the highscore entry */
    	if (fwrite((char*)&highScores[i], sizeof(highScoreEntry), 1, hsfp) != 1)
		{
			if (fclose(hsfp) < 0)
				WarningMessage("Cannot close high score file.");
			return False;
		}
	}
	
	/* Close the high score file */
	if (fclose(hsfp) < 0)
		WarningMessage("Cannot close high score file.");

	return True;
}

#if NeedFunctionPrototypes
void RedrawHighScore(Display *display, Window window)
#else
void RedrawHighScore(display, window)
	Display *display;
	Window window;
#endif
{
	/* Draw the title screen for highscore table */
	DoTitle(display, window);
}

#if NeedFunctionPrototypes
void FreeHighScore(Display *display)
#else
void FreeHighScore(display)
	Display *display;
#endif
{
	/* Free up those memory leaks thanks */
	if (titlePixmap) 	XFreePixmap(display, titlePixmap);
	if (titlePixmapM) 	XFreePixmap(display, titlePixmapM);
}

#if NeedFunctionPrototypes
void ResetHighScore(int type)
#else
void ResetHighScore(type)
	int type;
#endif
{
	HighScoreState = HIGHSCORE_TITLE;
	nextFrame = frame + 100;
	endFrame = frame + 4000;

	/* Reset the sparkles for the names */
	sparkley = 180 + textFont->ascent + 20;
	sindex = 0;
	si = 0;
	scoreType = type;

	DEBUG("Reset highscore mode.")
}

#if NeedFunctionPrototypes
static int LockUnlock(int cmd)
#else
static int LockUnlock(cmd)
	int cmd;
#endif
{
	int 	inter = -1;
	char 	filename[1024];
	char 	*str;
	int		theCmd;


#ifndef NO_LOCKING

	/* Suss out what command to use */
	if (cmd == LOCK_FILE)
#ifndef USE_FLOCK
		theCmd = F_LOCK;
#else
		theCmd = LOCK_EX;
#endif
	else
#ifndef USE_FLOCK
		theCmd = F_ULOCK;
#else
		theCmd = LOCK_UN;
#endif

#endif /* NO_LOCKING */


	/* Use the environment variable if it exists */
	if ((str = getenv("XBOING_SCORE_FILE")) != NULL)
		strcpy(filename, str);
	else
		strcpy(filename, HIGH_SCORE_FILE);

	/* Open the highscore file for both read & write */
	if (cmd == LOCK_FILE)
		inter = open(filename, O_CREAT | O_RDWR);


#ifndef NO_LOCKING

	/* Ok - if successful then lock or unlock the file */
	if (inter != -1) 
#ifndef USE_FLOCK
		lockf(inter, theCmd, sizeof(highScores));
#else
		flock(inter, theCmd);
#endif

#endif /* NO_LOCKING */


	/* Are we unlocking the file */
	if (cmd == UNLOCK_FILE)
	{
		/* Close the file now thanks */
		close(inter);
		inter = -1;
	}

	/* Return success status */
	return inter;
}
