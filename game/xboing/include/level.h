#ifndef _LEVEL_H_
#define _LEVEL_H_

#include "copyright.h"

/*
 *  Dependencies on other include files:
 */

#include <X11/Xlib.h>

/*
 *  Constants and macros:
 */

#define SHOTS_TO_KILL_SPECIAL   3

/*
 *  Type declarations:
 */

/*
 *  Function prototypes:
 */

#if NeedFunctionPrototypes
void InitialiseLevelInfo(Display *display, Window window, Colormap colormap);
void FreeLevelInfo(Display *display);
void DisplayLevelInfo(Display *display, Window window, u_long level);
void CheckGameRules(Display *display, Window window);
void DeadBall(Display *display, Window window);
void DeleteABullet(Display *display);
void AddABullet(Display *display);
void ReDrawBulletsLeft(Display *display);
void RedrawLevelInfo(Display *display, Window window);
void SetLevelNumber(int levelNum);
void SetStartingLevel(int levelNum);
int GetStartingLevel(void);
int ReadNextLevel(Display *display, Window window, char *levelName);
char *GetLevelName(void);
void DecLevelTimeBonus(Display *display, Window window);
void SetLevelTimeBonus(Display *display, Window window, int seconds);
int GetLevelTimeBonus(void);
void UpdateHighScores(void);
void AddExtraLife(Display *display);
void EndTheGame(Display *display, Window window);
void HandleGameTimer(Display *display, Window window);
void DecExtraLife(Display *display);
int GetNumberLife(void);
void AddToLevelTimeBonus(Display *display, Window window, int seconds);
#else
void AddToLevelTimeBonus();
int GetNumberLife();
void DecExtraLife();
void HandleGameTimer();
void EndTheGame();
void AddExtraLife();
void UpdateHighScores();
int GetLevelTimeBonus();
void SetLevelTimeBonus();
void DecLevelTimeBonus();
char *GetLevelName();
void InitialiseLevelInfo();
void FreeLevelInfo();
void DisplayLevelInfo();
void CheckGameRules();
void DeadBall();
void DeleteABullet();
void AddABullet();
void ReDrawBulletsLeft();
void RedrawLevelInfo();
void SetLevelNumber();
void SetStartingLevel();
int GetStartingLevel();
int ReadNextLevel();
#endif

extern int bonus, livesLeft, bonusBlock;
extern time_t gameTime;
extern u_long level;

#endif
