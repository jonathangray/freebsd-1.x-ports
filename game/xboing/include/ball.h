#ifndef _BALL_H_
#define _BALL_H_

#include "copyright.h"

/*
 *  Dependencies on other include files:
 */

#include <X11/Xlib.h>

/*
 *  Constants and macros:
 */

#define BALL_WIDTH  		20
#define BALL_HEIGHT 		19
#define MAX_BALL_MASS 		3.0
#define MIN_BALL_MASS 		1.0

#define BALL_WC     		(BALL_WIDTH / 2)
#define BALL_HC     		(BALL_HEIGHT / 2)

#define BIRTH_SLIDES        8
#define BALL_SLIDES         5

#define MAX_BALLS 			5

#define MAX_X_VEL           14
#define MAX_Y_VEL           14

#define MIN_DY_BALL			2
#define MIN_DX_BALL			2

#define BALL_ANIM_RATE      50
#define BIRTH_FRAME_RATE    5
#define BALL_FRAME_RATE     5
#define BORDER_ANIM_DELAY   15

#define PADDLE_HIT_SCORE    10

#define BALL_AUTO_ACTIVE_DELAY  3000

#define DIST_BALL_OF_PADDLE 45

#define PADDLE_BALL_FRAME_TILT  10000


/*
 *  Type declarations:
 */

enum BallStates 
{ 
	BALL_POP, 
	BALL_ACTIVE, 
	BALL_STOP, 
	BALL_CREATE, 
	BALL_DIE, 
	BALL_WAIT, 
	BALL_READY, 
	BALL_NONE 
};

typedef struct ball
{
	enum BallStates	waitMode;		/* Ball waiting mode */
	int				waitingFrame;	/* Frame to wait until */
	int				newMode;		/* Ball's new mode */
	int				nextFrame;		/* next frame for something */
	int				active;			/* True - in use, False - dead */
    int             oldx;			/* Old x coord of ball centre */
    int             oldy;			/* Old y coord of ball centre */
    int             ballx;			/* Current x coord of ball centre */
    int             bally;			/* Current y coord of ball centre */
    int             dx;				/* Change in x axis increment */
    int             dy;				/* Change in y axis increment */
    int             slide;			/* Current pixmap visible */
	float          	radius;			/* The radius of the ball */
	float          	mass;			/* The mass of the ball */
	int 			lastPaddleHitFrame;	/* Last frame the ball hit paddle */
    enum BallStates	ballState;		/* The state of the ball */
} BALL;

/*
 *  Function prototypes:
 */

#if NeedFunctionPrototypes
void InitialiseBall(Display *display, Window window, Colormap colormap);
void FreeBall(Display *display);
void RedrawBall(Display *display, Window window);
void DrawTheBall(Display *display, Window window, int x, int y, int slide);
void DrawTheBallBirth(Display *display, Window window, int x, int y, int slide);
void KillBallNow(Display *display, Window window, int i);
void GetBallPosition(int *ballX, int *ballY, int i);
void ResetBallStart(Display *display, Window window);
int GetBallMode(int i);
void ChangeBallMode(enum BallStates newMode, int i);
int AddANewBall(Display *display, int x, int y, int dx, int dy);
void ClearAllBalls(void);
void HandleBallMode(Display *display, Window window);
int StartAnotherBall(Display *display, Window window);
int IsBallWaiting(void);
void ClearBall(int i);
void SplitBallInTwo(Display *display, Window window);
void ClearBallNow(Display *display, Window window, int i);
int GetAnActiveBall(void);
int ActivateWaitingBall(Display *display, Window window);
int GetNumberOfActiveBalls(void);
void DoBoardTilt(Display *display, int i);
#else
void DoBoardTilt();
int GetNumberOfActiveBalls();
int ActivateWaitingBall();
int GetAnActiveBall();
void ClearBallNow();
void SplitBallInTwo();
void ClearBall();
void InitialiseBall();
void FreeBall();
void RedrawBall();
void DrawTheBall();
void DrawTheBallBirth();
void KillBallNow();
void GetBallPosition();
void ResetBallStart();
int GetBallMode();
void ChangeBallMode();
int AddANewBall();
void ClearAllBalls();
void HandleBallMode();
int StartAnotherBall();
int IsBallWaiting();
#endif

extern BALL balls[MAX_BALLS];
extern int speedLevel;
extern int paddleDx;

#endif
