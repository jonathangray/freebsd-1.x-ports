#ifndef _BONUS_H_
#define _BONUS_H_

#include "copyright.h"

/*
 *  Dependencies on other include files:
 */

#include <X11/Xlib.h>

/*
 *  Constants and macros:
 */

#define SMALL_TITLE_WIDTH		237
#define SMALL_TITLE_HEIGHT		37

#define SMALL_TITLE_WC		(SMALL_TITLE_WIDTH / 2)
#define SMALL_TITLE_HC		(SMALL_TITLE_HEIGHT / 2)

#define MAX_BONUS           10

/*
 *  Type declarations:
 */

enum BonusStates 
{ 
	BONUS_TEXT, 
	BONUS_SCORE, 
	BONUS_BONUS, 
	BONUS_LEVEL, 
	BONUS_BULLET, 
	BONUS_TIME, 
	BONUS_HSCORE, 
	BONUS_END_TEXT, 
	BONUS_WAIT, 
	BONUS_FINISH
};

/*
 *  Function prototypes:
 */

#if NeedFunctionPrototypes
void SetUpBonus(Display *display, Window window, Colormap colormap);
void DoBonus(Display *display, Window window);
void RedrawBonus(Display *display, Window window);
void FreeBonus(Display *display);
void ResetBonus(void);
void SetupBonusScreen(Display *display, Window window);
void IncNumberBonus(void);
void ResetNumberBonus(void);
void DrawSmallIntroTitle(Display *display, Window window, int x, int y);
void MoveSmallIntroTitle(Display *display, Window window, int x, int y);
void ComputeAndAddBonusScore(void);
void SetBonusWait(enum BonusStates newMode, int waitFrame);
int GetNumberBonus(void);
#else
int GetNumberBonus();
void SetBonusWait();
void ComputeAndAddBonusScore();
void DrawSmallIntroTitle();
void MoveSmallIntroTitle();
void SetUpBonus();
void DoBonus();
void RedrawBonus();
void FreeBonus();
void ResetBonus();
void SetupBonusScreen();
void IncNumberBonus();
void ResetNumberBonus();
#endif


extern enum BonusStates BonusState;

#endif
