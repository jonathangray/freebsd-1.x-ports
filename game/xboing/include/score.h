#ifndef _SCORE_H_
#define _SCORE_H_

#include "copyright.h"

/*
 *  Dependencies on other include files:
 */

#include <X11/Xlib.h>

/*
 *  Constants and macros:
 */

/*
 *  Type declarations:
 */

/*
 *  Function prototypes:
 */

#if NeedFunctionPrototypes
void InitialiseScoreDigits(Display *display, Window window, Colormap colormap);
void FreeScoreDigits(Display *display);
void DisplayScore(Display *display, Window window, u_long score);
void DrawOutNumber(Display *display, Window window, u_long score, int x, int y);
void AddToScore(u_long inc);
u_long ComputeScore(u_long inc);
#else
u_long ComputeScore();
void AddToScore();
void InitialiseScoreDigits();
void FreeScoreDigits();
void DisplayScore();
void DrawOutNumber();
#endif

extern u_long score;

#endif
