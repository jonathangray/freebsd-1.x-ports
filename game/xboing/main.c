#include "include/copyright.h"

/*
 *  Include file dependencies:
 */

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <sys/time.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/keysym.h>

#if NeedFunctionPrototypes
extern time_t time(time_t *tloc);
extern int gettimeofday(struct timeval *tp, struct timezone *tzp);
extern int select(int width, fd_set *readfds, fd_set *writefds,
		  fd_set *exceptfds, struct timeval *timeout);
#else /* !NeedFunctionPrototypes */
extern time_t time();
extern int gettimeofday();
extern int select();
#endif /* NeedFunctionPrototypes */

#include "include/score.h"
#include "include/presents.h"
#include "include/special.h"
#include "include/audio.h"
#include "include/mess.h"
#include "include/ball.h"
#include "include/gun.h"
#include "include/demo.h"
#include "include/sfx.h"
#include "include/init.h"
#include "include/blocks.h"
#include "include/misc.h"
#include "include/level.h"
#include "include/bonus.h"
#include "include/stage.h"
#include "include/paddle.h"
#include "include/intro.h"
#include "include/inst.h"
#include "include/highscore.h"
#include "include/keys.h"
#include "include/error.h"

#if NeedFunctionPrototypes
extern void ResetDemonstration(void);
#else /* !NeedFunctionPrototypes */
extern void ResetDemonstration();
#endif /* NeedFunctionPrototypes */

#include "include/main.h"

/*
 *  Internal macro definitions:
 */

#define PADDLE_ANIMATE_DELAY	5
#define BONUS_SEED				2000	

/*
 *  Internal type declarations:
 */

#if NeedFunctionPrototypes
static KeySym 	GetKeySym(XEvent event);
static void 	handleGameMode(Display *display);
static void 	handleEventLoop(Display *display);
static void 	ToggleGamePaused(Display *display);
static void 	SetGamePaused(Display *display);
static void 	handleGameStates(Display *display);
static void 	sleepSync(Display *display, unsigned long ms);
static void 	handleMiscKeys(Display *display, KeySym keysym);
static void 	handleSpeedKeys(Display *display, KeySym keysym);
static void 	handleExitKeys(Display *display, KeySym keysym);
#else
static void 	handleExitKeys();
static void 	handleSpeedKeys();
static void 	handleMiscKeys();
static void 	sleepSync();
static void 	handleGameStates();
static KeySym 	GetKeySym();
static void 	handleGameMode();
static void 	handleEventLoop();
static void 	ToggleGamePaused();
static void 	SetGamePaused();
#endif

/*
 *  Internal variable declarations:
 */

int paddleMotion = 0;
int paddleDx = 0;
int speedLevel = 5;
int frame, gameActive;
int mode, oldmode;
static int iconified = False;
long speed;
static int userDelay = 1;
static int paddleControl;
static time_t pauseStartTime;
time_t pausedTime;

#if NeedFunctionPrototypes
int GetWarpSpeed(void)
#else
int GetWarpSpeed()
#endif
{
	/* Return warp speed in user terms */
	return (speedLevel);
}

#if NeedFunctionPrototypes
void SetUserSpeed(int delay)
#else
void SetUserSpeed(delay)
	int delay;
#endif
{
	long temp;

	/* Set an entire game speedup or slowdown speed */
	temp = (speed / (long) userDelay);
	userDelay = delay;
	speed = (long) (temp * userDelay);
	speedLevel = 10 - delay;
}

#if NeedFunctionPrototypes
int GetPaddleControlMode(void)
#else
int GetPaddleControlMode()
#endif
{
	/* Get the paddle control mode */
	return paddleControl;
}

#if NeedFunctionPrototypes
void SetPaddleControlMode(int type)
#else
void SetPaddleControlMode(type)
	int type;
#endif
{
	/* Set the paddle control mode to the new mode */
	paddleControl = type;
}

#if NeedFunctionPrototypes
void SetGameSpeed(int delay)
#else
void SetGameSpeed(delay)
	int delay;
#endif
{
	/* This is the speed used in the sleeping routine */
	if (delay >= 0)
		speed = (long) (delay * userDelay);
}

#if NeedFunctionPrototypes
static void sleepSync(Display *display, unsigned long ms)
#else
static void sleepSync(display, ms)
	Display *display;
	unsigned long ms;
#endif
{
    struct timeval st, et;
    long SyncTime;

    gettimeofday(&st, NULL);
    XSync(display, False);
    gettimeofday(&et, NULL);

    SyncTime = (((et.tv_sec - st.tv_sec) * 1000) +
               ((et.tv_usec - st.tv_usec) / 1000) );

    if ((ms) > ((1000 / 60) + SyncTime))
        ms_sleep(ms - SyncTime);
}

#if NeedFunctionPrototypes
static KeySym GetKeySym(XEvent event)
#else
static KeySym GetKeySym(event)
	XEvent event;
#endif
{
	int count;
	char key;
	KeySym keysym;
	XComposeStatus compose;

	/* Lookup a keysym using the event key */
	count = XLookupString(&event.xkey, &key, 1, &keysym, &compose);

	return keysym;
}

#if NeedFunctionPrototypes
int paddleIsMoving(void)
#else
int paddleIsMoving()
#endif
{
	/* Returns direction of paddle 1 right -1 left 0 stopped */
	return paddleMotion;
}

#if NeedFunctionPrototypes
void handlePaddleMoving(Display *display)
#else
void handlePaddleMoving(display)
	Display *display;
#endif
{
	static oldx = 0;
	int rx, ry, x, y;
	unsigned int mask;
	Window root, child;

	if (paddleControl == CONTROL_KEYS)
	{
		switch (paddleMotion)
		{
			case 1:		/* Move the paddle to the right 1 increment */
				MovePaddle(display, playWindow, 
					PADDLE_RIGHT, currentPaddleSize, 0);
				break;

			case -1:		/* Move the paddle to the left 1 increment */
				MovePaddle(display, playWindow, 
					PADDLE_LEFT, currentPaddleSize, 0);
				break;

			default:
				break;
		}
	} else if (paddleControl == CONTROL_MOUSE)
	{
		/* Obtain the position of the pointer in the play window */
		if (XQueryPointer(display, playWindow, &root, &child, 
			&rx, &ry, &x, &y, &mask) == True)
		{
			/* Has the pointer moved since our last poll */
			if (x != oldx)
			{
				paddleDx = x - oldx;

				/* Move the paddle to the position of the mouse pointer */
				MovePaddle(display, playWindow, 
					PADDLE_NONE, currentPaddleSize, x);
				oldx = x;

				/* Adjust the paddle motion variable so the ball moves when in
				 * the BALL_READY state and BALL_CREATE state.
				 */
				if (x > oldx)
					paddleMotion = 1;
				else
					paddleMotion = -1;
			}
			else
			{
				/* Reset to no motion */
				paddleMotion = 0;
				paddleDx = 0;
			}
		}
	}
}


#if NeedFunctionPrototypes
static void ToggleGamePaused(Display *display)
#else
static void ToggleGamePaused(display)
	Display *display;
#endif
{
	if (mode == MODE_PAUSE)
	{
		/* Finished pause resume game */
		mode = MODE_GAME;
		SetCurrentMessage(display, messWindow, "- Play ball -", False);
		
		/* How many seconds were we paused for? */
		pausedTime += (time(NULL) - pauseStartTime);

 		XSelectInput(display, mainWindow, 
			KeyPressMask | KeyReleaseMask | ButtonPressMask |
   			ButtonReleaseMask | ExposureMask | StructureNotifyMask);

 		GrabPointer(display, mainWindow);
	}
	else 
		SetGamePaused(display);
}

#if NeedFunctionPrototypes
static void SetGamePaused(Display *display)
#else
static void SetGamePaused(display)
	Display *display;
#endif
{
	if (mode == MODE_GAME)
	{
		/* Set game to paused mode */
		mode = MODE_PAUSE;
		SetCurrentMessage(display, messWindow, 
			"- Game paused -", False);
		
		/* we need to keep track of how long we were paused so that later
		 * in the highscore thing I can take that off the time.
		 */
		pauseStartTime = time(NULL);

		XSelectInput(display, mainWindow, 
			KeyPressMask | ExposureMask | StructureNotifyMask);

		UnGrabPointer(display);
	}
}

#if NeedFunctionPrototypes
void handleIconify(Display *display)
#else
void handleIconify(display)
	Display *display;
#endif
{
	ToggleGamePaused(display);
}

#if NeedFunctionPrototypes
void SelectiveRedraw(Display *display)
#else
void SelectiveRedraw(display)
	Display *display;
#endif
{
	switch (mode)
	{
		case MODE_GAME:
		case MODE_PAUSE:
			RedrawPlayWindow(display, playWindow);
			break;

		case MODE_INTRO:
			RedrawIntroduction(display, playWindow);
			break;

		case MODE_DEMO:
			RedrawDemonstration(display, playWindow);
			break;

		case MODE_INSTRUCT:
			RedrawInstructions(display, playWindow);
			break;

		case MODE_KEYS:
			RedrawKeys(display, playWindow);
			break;

		case MODE_BONUS:
			RedrawBonus(display, mainWindow);
			break;

		case MODE_HIGHSCORE:
			RedrawHighScore(display, playWindow);
			break;

		default:
			break;
	}

	/* Redisplay the message and the level/score info */
	RedrawLevelInfo(display, levelWindow);
	DisplayCurrentMessage(display, messWindow);

	/* To be sure - to be sure */
	XFlush(display);
}

#if NeedFunctionPrototypes
void handleExposure(Display *display, XEvent event)
#else
void handleExposure(display, event)
	Display *display;
	XEvent event;
#endif
{
	/* Only redraw window once so wait until all expose events have sent
	 * and then redraw all that we need to redraw based on current game
	 * mode.
	 */
	if (event.xexpose.count == 0)
		SelectiveRedraw(display);
}

#if NeedFunctionPrototypes
void handleMouseButtons(Display *display, XEvent event, int Down)
#else
void handleMouseButtons(display, event, Down)
	Display *display;
	XEvent event;
	int Down;
#endif
{
	if (Down == True)
	{
		/* Button pressed down */
		switch(event.xbutton.button)
		{
			/* Shoot a bullet */
			case Button1:
			case Button2: 
			case Button3:
				/* If we are playing the game and a ball needs to be started
				 * then start it otherwise shoot a bullet.
				 */
				if (mode == MODE_GAME)
					if (ActivateWaitingBall(display, playWindow) == False)
						shootBullet(display, playWindow);
				break;
		}
	}
}

#if NeedFunctionPrototypes
static void handleControlKeys(Display *display)
#else
static void handleControlKeys(display)
	Display *display;
#endif
{
	/* Toggle game mode */
	if (GetPaddleControlMode() == CONTROL_KEYS)
	{
		SetCurrentMessage(display, messWindow, 
			"Control: Mouse", True);
		SetPaddleControlMode(CONTROL_MOUSE);
	}
	else
	{
		SetCurrentMessage(display, messWindow, 
			"Control: Keys", True);
		SetPaddleControlMode(CONTROL_KEYS);
	}

	/* Play a bit of sound */
	if (noSound == False)
		playSoundFile("toggle", 50);
}

#if NeedFunctionPrototypes
static void handleSoundKey(Display *display)
#else
static void handleSoundKey(display)
	Display *display;
#endif
{
	if (noSound == False)
	{
		/* Try and turn audio off */
		FreeAudioSystem();

		noSound = True;
		SetCurrentMessage(display, messWindow, 
			"- Audio OFF -", True);
	}
	else
	{
		/* Try and turn audio on */
		if (SetUpAudioSystem(display) == False)
		{
			/* Unable to turn audio on */
			noSound = True;
			SetCurrentMessage(display, messWindow, 
				"- Audio unavailable -", True);
		}
		else
		{
			/* Audio is now active */
			noSound = False;
			SetCurrentMessage(display, messWindow, 
				"- Audio ON -", True);
		}
	}
}

#if NeedFunctionPrototypes
static void handleGameKeys(Display *display, KeySym keysym)
#else
static void handleGameKeys(display, keysym)
	Display *display;
	KeySym keysym;
#endif
{
	int temp;

	/* Switch on the keysym */
	switch (keysym)
	{
		case XK_t:	
		case XK_T:	
			/* Obtain an active ball and tilt it */
			if ((temp = GetAnActiveBall()) >= 0)
			{
				/* Bump the ball please */
				DoBoardTilt(display, temp);
			}
			break;

		case XK_d:	
		case XK_D:	
			/* Obtain an active ball - ie: not on paddle */
			if ((temp = GetAnActiveBall()) >= 0)
			{
				/* Erase and reset ball to new one */
				ClearBallNow(display, playWindow, temp);
			}
			break;

		case XK_Left:
		case XK_j:
		case XK_J:
			/* Set paddle to move left */
			paddleMotion = -1;
			break;

		case XK_k:
		case XK_K:
			/* Shoot a bullet if available */
			if (ActivateWaitingBall(display, playWindow) == False)
				shootBullet(display, playWindow);
			break;

		case XK_Right:
		case XK_l:
		case XK_L:
			/* Set paddle to move right */
			paddleMotion = 1;
			break;

		case XK_Escape:
			/* Issue message and change to exit mode */
			SetCurrentMessage(display, messWindow, 
				"Really finish this Game? [Y/N]", False);
			oldmode = mode;
			mode = MODE_QUIT;
			break;

      	case XK_minus:
			if (debug == True)
			{
				/* Special cheat key for debugging mode */
            	SkipToNextLevel(display, playWindow);
            	SetCurrentMessage(display, messWindow,
            		"Skipping to next level ...", True);
			}
            break;

		case XK_p:
		case XK_P:
			ToggleGamePaused(display);
			break;

		default: 	/* All other keys */
			handleMiscKeys(display, keysym);
	}
}

#if NeedFunctionPrototypes
static void handleIntroKeys(Display *display, KeySym keysym)
#else
static void handleIntroKeys(display, keysym)
	Display *display;
	KeySym keysym;
#endif
{
	/* Switch on the keysym */
	switch (keysym)
	{

		case XK_space:
			if (mode == MODE_INTRO || mode == MODE_HIGHSCORE 
			  || mode == MODE_INSTRUCT || mode == MODE_KEYS 
			  || mode == MODE_DEMO)
			{
				ResetBorderGlow(display, playWindow);
				SetGameSpeed(FAST_SPEED);
				gameActive = False;
				mode = MODE_GAME;
			}

			if (mode == MODE_BONUS)
				SetBonusWait(BONUS_FINISH, frame);
			break;

		case XK_c:
		case XK_C:
			/* Cycle through the introduction screens if note in a game */
			if (mode == MODE_INTRO)
			{
				/* Ok - Goto the instructions mode */
				SetGameSpeed(FAST_SPEED);
				ResetInstructions();
				mode = MODE_INSTRUCT;
			} else if (mode == MODE_INSTRUCT)
			{
				/* Ok - Goto the keys mode */
				SetGameSpeed(FAST_SPEED);
				ResetDemonstration();
				mode = MODE_DEMO;
			} else if (mode == MODE_DEMO)
			{
				/* Ok - Goto the demo mode */
				SetGameSpeed(FAST_SPEED);
				ResetKeys();
				mode = MODE_KEYS;
			} else if (mode == MODE_KEYS)
			{
				/* Ok - Goto the highscore mode */
				SetGameSpeed(FAST_SPEED);
				ResetHighScore(GLOBAL);
				mode = MODE_HIGHSCORE;
			} else if (mode == MODE_HIGHSCORE)
			{
				/* Ok - Goto back to the intro mode */
				SetGameSpeed(FAST_SPEED);
				ResetIntroduction();
				mode = MODE_INTRO;
			} 
			break;

		case XK_H:	/* Personal highscores */
			if (mode == MODE_INTRO || mode == MODE_INSTRUCT 
				|| mode == MODE_KEYS || mode == MODE_HIGHSCORE 
				|| mode == MODE_DEMO)
			{
				/* Display the high scores thanks */
				SetGameSpeed(FAST_SPEED);
				ResetHighScore(PERSONAL);
				mode = MODE_HIGHSCORE;

				/* Play a bit of sound */
				if (noSound == False)
					playSoundFile("toggle", 50);
				
			}
			break;

		case XK_h:	/* Global highscores */
			if (mode == MODE_INTRO || mode == MODE_INSTRUCT 
				|| mode == MODE_KEYS || mode == MODE_HIGHSCORE
				|| mode == MODE_DEMO)
			{
				SetGameSpeed(FAST_SPEED);
				ResetHighScore(GLOBAL);
				mode = MODE_HIGHSCORE;

				/* Play a bit of sound */
				if (noSound == False)
					playSoundFile("toggle", 50);
			}
			break;

		case XK_s:
		case XK_S:
			if (mode == MODE_INTRO || mode == MODE_INSTRUCT 
				|| mode == MODE_KEYS || mode == MODE_HIGHSCORE
				|| mode == MODE_DEMO)
			{
				/* toggle the special effects system */
				if (getSpecialEffects(display) == True)
				{
					/* Turn off special effects */
					useSpecialEffects(False);

					SetCurrentMessage(display, messWindow, 
						"- SFX OFF -", True);
				}
				else
				{
					/* Cannot use sfx on this display */
					if (getSpecialEffects(display) == -1)
					{
						SetCurrentMessage(display, messWindow, 
							"- SFX Unavailable -", True);
					}
					else
					{
						/* Try and turn on special effects */
						useSpecialEffects(True);

						SetCurrentMessage(display, messWindow, 
							"- SFX ON -", True);
					}
				}
			}
			break;

		default: 	/* All other keys */
			handleMiscKeys(display, keysym);
			handleSpeedKeys(display, keysym);
			break;
	}
}

#if NeedFunctionPrototypes
static void handleQuitKeys(Display *display, KeySym keysym)
#else
static void handleQuitKeys(display, keysym)
	Display *display;
	KeySym keysym;
#endif
{
	/* Switch on the keysym */
	switch (keysym)
	{
		case XK_y:
		case XK_Y:
			/* Save out the scores if you were playing */
			if (oldmode == MODE_GAME || oldmode == MODE_BONUS)
			{
				/* Save out scores when quitting */
				UpdateHighScores();
			}

			/* Abort game and return to intros */
			SetGameSpeed(FAST_SPEED);
			ResetIntroduction();
			mode = MODE_INTRO;

			break;

		case XK_n:
		case XK_N:
			/* Change back to the previous mode */
			SetCurrentMessage(display, messWindow, "Finish aborted!", True);
			mode = oldmode;
			break;

		default:
			/* Yes or No keys thanks */
			XBell(display, 0);
			break;
	}
}

#if NeedFunctionPrototypes
static void handleExitKeys(Display *display, KeySym keysym)
#else
static void handleExitKeys(display, keysym)
	Display *display;
	KeySym keysym;
#endif
{
	/* Switch on the keysym */
	switch (keysym)
	{
		case XK_y:
		case XK_Y:
			/* Save out the scores if you were playing */
			if (oldmode == MODE_GAME || oldmode == MODE_BONUS)
			{
				/* Save out scores when quitting */
				UpdateHighScores();
			}

			if (noSound == False) playSoundFile("game_over", 100);

			/* Shut down and exit game */
			ShutDown(display, 0, "Thank you for playing XBoing.");
			break;

		case XK_n:
		case XK_N:
			/* Change back to the previous mode */
			SetCurrentMessage(display, messWindow, "XBoing Exit aborted!", 
				True);
			mode = oldmode;
			break;

		default:
			/* Yes or No keys thanks */
			XBell(display, 0);
			break;
	}
}

#if NeedFunctionPrototypes
static void handlePresentsKeys(Display *display, KeySym keysym)
#else
static void handlePresentsKeys(display, keysym)
	Display *display;
	KeySym keysym;
#endif
{
	/* Switch on the keysym */
	switch (keysym)
	{
		case XK_space:
			QuickFinish(display, mainWindow);
			break;

		case XK_Q:
		case XK_q:
			/* Shut down and exit game */
			ShutDown(display, 0, "Thank you for playing XBoing.");
			break;

		default:
			break;
	}
}

#if NeedFunctionPrototypes
static void handleSpeedKeys(Display *display, KeySym keysym)
#else
static void handleSpeedKeys(display, keysym)
	Display *display;
	KeySym keysym;
#endif
{
	/* Switch on the keysym */
	switch (keysym)
	{
		case XK_1:	/* Set speed to speed 1 */
			SetUserSpeed(9);
			SetCurrentMessage(display, messWindow, "Warp 1", True);
			if (noSound == False) playSoundFile("tone", 10);
			break;

		case XK_2:	/* Set speed to speed 2 */
			SetUserSpeed(8);
			SetCurrentMessage(display, messWindow, "Warp 2", True);
			if (noSound == False) playSoundFile("tone", 20);
			break;

		case XK_3:	/* Set speed to speed 3 */
			SetUserSpeed(7);
			SetCurrentMessage(display, messWindow, "Warp 3", True);
			if (noSound == False) playSoundFile("tone", 30);
			break;

		case XK_4:	/* Set speed to speed 4 */
			SetUserSpeed(6);
			SetCurrentMessage(display, messWindow, "Warp 4", True);
			if (noSound == False) playSoundFile("tone", 40);
			break;

		case XK_5:	/* Set speed to speed 5 */
			SetUserSpeed(5);
			SetCurrentMessage(display, messWindow, "Warp 5", True);
			if (noSound == False) playSoundFile("tone", 50);
			break;

		case XK_6:	/* Set speed to speed 6 */
			SetUserSpeed(4);
			SetCurrentMessage(display, messWindow, "Warp 6", True);
			if (noSound == False) playSoundFile("tone", 60);
			break;

		case XK_7:	/* Set speed to speed 7 */
			SetUserSpeed(3);
			SetCurrentMessage(display, messWindow, "Warp 7", True);
			if (noSound == False) playSoundFile("tone", 70);
			break;

		case XK_8:	/* Set speed to speed 8 */
			SetUserSpeed(2);
			SetCurrentMessage(display, messWindow, "Warp 8", True);
			if (noSound == False) playSoundFile("tone", 80);
			break;

		case XK_9:	/* Set speed to speed 9 */
			SetUserSpeed(1);
			SetCurrentMessage(display, messWindow, "Warp 9", True);
			if (noSound == False) playSoundFile("tone", 90);
			break;

		default: 	/* All other keys */
			break;
	}
}

#if NeedFunctionPrototypes
static void handleMiscKeys(Display *display, KeySym keysym)
#else
static void handleMiscKeys(display, keysym)
	Display *display;
	KeySym keysym;
#endif
{
	int vol = 0;
	char str[30];

	/* Switch on the keysym */
	switch (keysym)
	{
		case XK_plus:		/* Turn the volume up! 1% */
		case XK_KP_Add:
			if (noSound == False)
			{
				vol = GetMaximumVolume();
				if (vol < 100)
					vol++;
				SetMaximumVolume(vol);
				sprintf(str, "Maximum volume: %d%%", vol);
				SetCurrentMessage(display, messWindow, str, True);
			}
			break;

		case XK_minus:		/* Turn the volume down 1% */
		case XK_KP_Subtract:
			if (noSound == False)
			{
				vol = GetMaximumVolume();
				if (vol > 0)
					vol--;
				SetMaximumVolume(vol);
				sprintf(str, "Maximum volume: %d%%", vol);
				SetCurrentMessage(display, messWindow, str, True);
			}
			break;

		case XK_a:
		case XK_A:
			handleSoundKey(display);
			break;

		case XK_i:
		case XK_I:
			/* Iconify the window quickly - main loop handles events */
			XIconifyWindow(display, mainWindow, 0);
			break;

		case XK_g:
		case XK_G:
			handleControlKeys(display);
			break;

		case XK_Q:
		case XK_q:
			/* Issue message and change to exit mode */
			SetCurrentMessage(display, messWindow, 
				"Really Exit XBoing? [Y/N]", False);
			oldmode = mode;
			mode = MODE_EXIT_GAME;
			break;

		default: 	/* All other keys */
			break;
	}
}

#if NeedFunctionPrototypes
void handleKeyPress(Display *display, KeySym keysym, int Pressed)
#else
void handleKeyPress(display, keysym, Pressed)
	Display *display;
	KeySym keysym;
	int Pressed;
#endif
{

	if (Pressed == False)
	{
		/* key was released */
		paddleMotion = 0;
	}
	else
	{
		/* Switch on the game mode */
		switch (mode)
		{
			case MODE_EXIT_GAME:
				handleExitKeys(display, keysym);
				break;

			case MODE_QUIT:
				handleQuitKeys(display, keysym);
				break;

			case MODE_WAIT:
			case MODE_BALL_WAIT:
			case MODE_PAUSE:
			case MODE_GAME:
				handleGameKeys(display, keysym);
				break;

			case MODE_HIGHSCORE:
			case MODE_BONUS:
			case MODE_INTRO:
			case MODE_INSTRUCT:
			case MODE_DEMO:
			case MODE_KEYS:
				handleIntroKeys(display, keysym);
				break;

			case MODE_PRESENTS:
				handlePresentsKeys(display, keysym);
				break;

			case MODE_NONE:
				break;
		}
	}
}



#if NeedFunctionPrototypes
static void handleGameMode(Display *display)
#else
static void handleGameMode(display)
	Display *display;
#endif
{
	static int bonusRow = 0;
	static int bonusCol = 0;
	static int nextBonusFrame = 0;

	/* If we are going to play then setup first level */
	if (gameActive == False)
	{
		/* Choose a random velocity for the ball */

		/* Always start at level 1 or level specified */
		SetLevelNumber(GetStartingLevel());

		/* Set some important variables */
		livesLeft 			= 3;
		score 				= 0L;
		nextBonusFrame 		= 0;
		currentPaddleSize 	= PADDLE_HUGE;
		pausedTime			= 0;
		bonusBlock 			= False;

		/* Setup the stage and load 1st level */
		SetupStage(display, playWindow);

		/* Start game play */
		gameActive = True;

		/* Keep track of the game duration - shown in highscores */
		gameTime = time(NULL);
	}

	/* If we need to move the paddle then do so */
	if ((frame % PADDLE_ANIMATE_DELAY) == 0)
		handlePaddleMoving(display);

	if (mode == MODE_GAME)
	{
		HandleBallMode(display, playWindow);

		/* Add bonus coin block at random intervals */
		if (nextBonusFrame == 0 && bonusBlock == False)
			nextBonusFrame = frame + (rand() % BONUS_SEED);

		/* Do we need to add a bonus coin or special? */
		if (nextBonusFrame <= frame && bonusBlock == False)
		{
			/* Add the bonus block now - different types */
			switch (rand() % 25)
			{
				case 0: case 1: 
				case 2: case 3: 
				case 4: case 5: 
				case 6: case 7:
					/* Add a normal bonus block */
					AddBonusBlock(display, playWindow, &bonusRow, &bonusCol, 
						BONUS_BLK);
					DEBUG("Attempting Adding a bonus block.")
					break;

				case 8: case 9:
				case 10: case 11:
					/* Add the x2 bonus block */
					AddBonusBlock(display, playWindow, &bonusRow, &bonusCol, 
						BONUSX2_BLK);
					DEBUG("Attempting Adding a bonus x2 block.")
					break;

				case 12: case 13:
					/* Add the x4 bonus block */
					AddBonusBlock(display, playWindow, &bonusRow, &bonusCol, 
						BONUSX4_BLK);
					DEBUG("Attempting Adding a bonus x4 block.")
					break;

				case 14: case 15: 
					/* Add the shrink paddle special block */
					AddSpecialBlock(display, playWindow, &bonusRow, &bonusCol, 
						PAD_SHRINK_BLK, SHOTS_TO_KILL_SPECIAL);
					DEBUG("Attempting Adding a special paddle shrink block.")
					break;

				case 16: case 17: 
					/* Add the expand paddle  special block */
					AddSpecialBlock(display, playWindow, &bonusRow, &bonusCol, 
						PAD_EXPAND_BLK, SHOTS_TO_KILL_SPECIAL);
					DEBUG("Attempting Adding a special paddle expand block.")
					break;

				case 18: 
					/* Add the multiball special block */
					AddSpecialBlock(display, playWindow, &bonusRow, &bonusCol, 
						MULTIBALL_BLK, SHOTS_TO_KILL_SPECIAL);
					DEBUG("Attempting Adding a special multiball block.")
					break;

				case 19: 
					/* Add the reverse special block */
					AddSpecialBlock(display, playWindow, &bonusRow, &bonusCol, 
						REVERSE_BLK, SHOTS_TO_KILL_SPECIAL);
					DEBUG("Attempting Adding a special reverse control block.")
					break;

				case 20: case 21: 
					/* Add the machine gun special block */
					AddSpecialBlock(display, playWindow, &bonusRow, &bonusCol, 
						MGUN_BLK, SHOTS_TO_KILL_SPECIAL);
					DEBUG("Attempting Adding a special machine gun block.")
					break;

				case 22:
					/* Add the walloff special block */
					AddSpecialBlock(display, playWindow, &bonusRow, &bonusCol, 
						WALLOFF_BLK, SHOTS_TO_KILL_SPECIAL);
					DEBUG("Attempting Adding a special walls off block.")
					break;

				case 23:
					/* Add the extraball special block */
					AddSpecialBlock(display, playWindow, &bonusRow, &bonusCol, 
						EXTRABALL_BLK, 0);
					DEBUG("Attempting Adding a special extra ball block.")
					break;

				case 24:
					/* Add the death special block */
					AddSpecialBlock(display, playWindow, &bonusRow, &bonusCol, 
						DEATH_BLK, SHOTS_TO_KILL_SPECIAL);
					DEBUG("Attempting Adding a special death block.")
					break;

				default:
					break;
			}

			nextBonusFrame = 0;
		}
	}

	HandleBulletMode(display, playWindow);

	/* If any blocks need exploding then do so */
	ExplodeBlocksPending(display, playWindow);

	/* So blocks need animation all the time so do it */
	HandlePendingAnimations(display, playWindow);

	/* See if the level is finished and update level info if needed */
	if (mode == MODE_GAME)
		CheckGameRules(display, playWindow);
}

#if NeedFunctionPrototypes
static void handleGameStates(Display *display)
#else
static void handleGameStates(display)
	Display *display;
#endif
{
	/* Update the message window if any new messages come along */
	DisplayCurrentMessage(display, messWindow);

	/* In game effects */
	switch (currentSfxMode())
	{
		case SFX_SHAKE:
			/* Something exploded or bumped the screen */
			WindowShakeEffect(display, playWindow);
			break;

		case SFX_FADE:
			/* fade when play arena is taken off screen */
			WindowFadeEffect(display, playWindow);
			break;
			
		case SFX_BLIND:
			/* bring the backing buffer pixmap into view via a blind effect */
			WindowBlindEffect(display, playWindow);
			break;
			
		case SFX_SHATTER:
			/* bring the backing buffer pixmap into view via a blind effect */
			WindowShatterEffect(display, playWindow);
			break;
			
		case SFX_NONE:
		default:
			break;
	}

	/* Switch on the current game mode */
	switch (mode)
	{
		case MODE_GAME:
			handleGameMode(display);
			break;

		case MODE_PRESENTS:
			Presents(display, mainWindow);
			break;

		case MODE_BONUS:
			DoBonus(display, mainWindow);
			break;

		case MODE_INTRO:
			Introduction(display, playWindow);
			break;

		case MODE_INSTRUCT:
			Instructions(display, playWindow);
			break;

		case MODE_KEYS:
			Keys(display, playWindow);
			break;

		case MODE_DEMO:
			Demonstration(display, playWindow);
			break;

		case MODE_HIGHSCORE:
			HighScore(display, playWindow);
			break;

		case MODE_PAUSE:
		case MODE_EXIT_GAME:
		case MODE_QUIT:
			break;
	}

	/* Flush the display */
	XFlush(display);
}

#if NeedFunctionPrototypes
static void handleEventLoop(Display *display)
#else
static void handleEventLoop(display)
	Display *display;
#endif
{
	XEvent event;
	int pending;
	KeySym keysym;

	pending = frame = 0;

	/* Initial mode for game is Introduction */
	mode = MODE_PRESENTS;

	/* No special effects yet */
	changeSfxMode(SFX_NONE);

	/* Flush all events until app is fully mapped */
    do
	{
		/* handle audio device events if they exist */
		audioDeviceEvents();

		/* Get the next event */
		XNextEvent(display, &event);
	}
	while (event.type != MapNotify);

	ChangePointer(display, mainWindow, CURSOR_NONE);

 	/* Grab the pointer to the main window */
	GrabPointer(display, mainWindow);

	/* Loop forever and ever */
	while (True)
	{
		/* handle and audio device events if supported */
		audioDeviceEvents();

		/* Sleep a bit if not iconified */
		if (iconified == False)
			sleepSync(display, speed);

		/* See if any events are waiting for me to handle */
		if (iconified == False && mode != MODE_PAUSE && mode != MODE_EXIT_GAME
			&& mode != MODE_QUIT)
		{
			/* Get an event but don't wait if none arrives */
			pending = XPending(display);
			frame++;
		}
		else
		{
			/* Wait here for an event and then get the number waiting */
			XPeekEvent(display, &event);
			pending = XPending(display);
		}
		
		/* Handle any events pending */
		while (pending > 0)
		{
			/* Get the next X event thanks */
			XNextEvent(display, &event);

			switch(event.type)
			{
				case UnmapNotify:
					/* Turn off just all events except the mapping ones */
    				XSelectInput(display, mainWindow, StructureNotifyMask);
					handleIconify(display);
					iconified = True;
					break;

				case MapNotify:
					/* Turn back on all the events that are needed */
    				XSelectInput(display, mainWindow, 
						KeyPressMask | KeyReleaseMask | ButtonPressMask |
       					ButtonReleaseMask | ExposureMask | StructureNotifyMask);

					SelectiveRedraw(display);
 					GrabPointer(display, mainWindow);
					iconified = False;

					break;

				case ButtonRelease:
					handleMouseButtons(display, event, False);
					break;

				case ButtonPress:
					handleMouseButtons(display, event, True);
					break;

				case KeyRelease:
					keysym = GetKeySym(event);
					handleKeyPress(display, keysym, False);
					break;

				case KeyPress:
					keysym = GetKeySym(event);
					handleKeyPress(display, keysym, True);
					break;

				case Expose:
					handleExposure(display, event);
					break;

				default:
					break;
			}

			/* Decrement the number of pending events */
			pending--;
		}

		/* handle all game states and animations */
		if (iconified == False) 
			handleGameStates(display);
	}

	/* NOT REACHED */
}

#if NeedFunctionPrototypes
void main(int argc, char **argv)
#else
void main(argc, argv)
	int argc;
	char **argv;
#endif
{
	static Display *display;

	/* Initialise everything and return display */
	display = InitialiseGame(argv, argc);

	SetGameSpeed(FAST_SPEED);
	gameActive = False;
	iconified = False;

	/* main event loop */
	handleEventLoop(display);

	/* NOTREACHED */
}
