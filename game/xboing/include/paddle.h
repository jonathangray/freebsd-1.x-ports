#ifndef _PADDLE_H_
#define _PADDLE_H_

#include "copyright.h"

/*
 *  Dependencies on other include files:
 */

#include <X11/Xlib.h>

/*
 *  Constants and macros:
 */

#define PADDLE_LEFT		1
#define PADDLE_SHOOT	2
#define PADDLE_RIGHT	3
#define PADDLE_NONE		0

#define PADDLE_SMALL	4
#define PADDLE_MEDIUM	5
#define PADDLE_HUGE		6

#define DIST_BASE   	30
#define PADDLE_VEL  	10

#define PADDLE_HC  		4
#define PADDLE_HEIGHT 	9

/*
 *  Type declarations:
 */

/*
 *  Function prototypes:
 */

#if NeedFunctionPrototypes
void InitialisePaddle(Display *display, Window window, Colormap colormap);
void FreePaddle(Display *display);
void DrawPaddle(Display *display, Window window, int x, int y, int size);
void MovePaddle(Display *display, Window window, int direction, 
	int size, int xpos);
void ResetPaddleStart(Display *display, Window window);
int GetPaddleSize(void);
void RedrawPaddle(Display *display, Window window);
void FlushPaddleBackingStore(Display *display, Window window);
void ToggleReverse(Display *display);
void SetReverseOff(void);
void ChangePaddleSize(Display *display, Window window, int type);
#else
void ChangePaddleSize();
void SetReverseOff();
void ToggleReverse();
void InitialisePaddle();
void FreePaddle();
void DrawPaddle();
void MovePaddle();
void ResetPaddleStart();
int GetPaddleSize();
void RedrawPaddle();
void FlushPaddleBackingStore();
#endif

extern int currentPaddleSize, paddlePos, reverseOn, stickyOn;

#endif
