#ifndef _HIGHSCORE_H_
#define _HIGHSCORE_H_

#include "copyright.h"

/*
 *  Dependencies on other include files:
 */

#include <X11/Xlib.h>
#include <sys/time.h>
#include <sys/types.h>

/*
 *  Constants and macros:
 */

#define PERSONAL 	1
#define GLOBAL 		2

/*
 *  Type declarations:
 */

enum HighScoreStates 
{ 
	HIGHSCORE_TITLE, 
	HIGHSCORE_SHOW, 
	HIGHSCORE_WAIT, 
	HIGHSCORE_SPARKLE, 
	HIGHSCORE_FINISH
};

typedef struct 
{
	u_long 	score;		/* Score */
	u_long 	level;		/* delta Level reached */
	time_t 	gameTime;	/* Time taken to complete game */
	time_t 	time;		/* Date when played */
	char 	name[40];	/* Full user name */
	uid_t	userId;		/* Real user id of player */
} highScoreEntry;

/*
 *  Function prototypes:
 */

#if NeedFunctionPrototypes
void SetUpHighScore(Display *display, Window window, Colormap colormap);
void HighScore(Display *display, Window window);
void RedrawHighScore(Display *display, Window window);
void FreeHighScore(Display *display);
void ResetHighScore(int type);
int ReadHighScoreTable(int type);
int WriteHighScoreTable(int type);
int CheckAndAddScoreToHighScore(u_long score, u_long level, time_t gameTime, 
	int type);
int GetHighScoreRanking(u_long score);
void CommandlineHighscorePrint(void);
void SetNickName(char *nick);
char *GetNickName(void);
#else
char *GetNickName();
void SetNickName();
void CommandlineHighscorePrint();
void SetUpHighScore();
void HighScore();
void RedrawHighScore();
void FreeHighScore();
void ResetHighScore();
int ReadHighScoreTable();
int WriteHighScoreTable();
int CheckAndAddScoreToHighScore();
int GetHighScoreRanking();
#endif

extern enum HighScoreStates HighScoreState;

#endif
