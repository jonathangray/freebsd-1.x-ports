#ifndef _MAIN_H_
#define _MAIN_H_

#include "copyright.h"

/*
 *  Dependencies on other include files:
 */

/*
 *  Constants and macros:
 */

#define ms_sleep(ms) 				\
{ 									\
	struct timeval tv; 				\
	tv.tv_sec=((ms)/1000); 			\
	tv.tv_usec=(((ms)%1000)*1000); 	\
	select(1,NULL,NULL,NULL,&tv);   \
}

#define CONTROL_KEYS		0	
#define CONTROL_MOUSE		1	

#define MODE_NONE			0	
#define MODE_HIGHSCORE		1
#define MODE_INTRO			2
#define MODE_GAME			3
#define MODE_PAUSE			4
#define MODE_BALL_WAIT		5
#define MODE_WAIT			6
#define MODE_BONUS			7
#define MODE_INSTRUCT		8
#define MODE_KEYS			9
#define MODE_PRESENTS		10
#define MODE_QUIT			11
#define MODE_EXIT_GAME		12
#define MODE_DEMO			13

#define FAST_SPEED              1
#define MEDIUM_SPEED            5
#define SLOW_SPEED              9

/*
 *  Type declarations:
 */

/*
 *  Function prototypes:
 */

#if NeedFunctionPrototypes
void SetGameSpeed(int delay);
int GetWarpSpeed(void);
int paddleIsMoving(void);
void SetUserSpeed(int delay);
void SetPaddleControlMode(int type);
int GetPaddleControlMode(void);
void SelectiveRedraw(Display *display);
void handlePaddleMoving(Display *display);
#else
int GetWarpSpeed();
void handlePaddleMoving();
void SelectiveRedraw();
int GetPaddleControlMode();
void SetControlMode();
void SetGameSpeed();
int paddleIsMoving();
void SetUserSpeed();
#endif

extern int frame, mode, holdMode, modeSfx, gameActive;
extern time_t pausedTime;

#endif
