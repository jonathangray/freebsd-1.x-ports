#include "include/copyright.h"

/*
 *  Include file dependencies:
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <values.h>
#include <xpm.h>

#include "bitmaps/ball1.xpm"
#include "bitmaps/ball2.xpm"
#include "bitmaps/ball3.xpm"
#include "bitmaps/ball4.xpm"
#include "bitmaps/killer.xpm"

#include "bitmaps/guide1.xpm"
#include "bitmaps/guide2.xpm"
#include "bitmaps/guide3.xpm"
#include "bitmaps/guide4.xpm"
#include "bitmaps/guide5.xpm"
#include "bitmaps/guide6.xpm"
#include "bitmaps/guide7.xpm"
#include "bitmaps/guide8.xpm"
#include "bitmaps/guide9.xpm"
#include "bitmaps/guide10.xpm"
#include "bitmaps/guide11.xpm"

#include "bitmaps/ballbirth1.xpm"
#include "bitmaps/ballbirth2.xpm"
#include "bitmaps/ballbirth3.xpm"
#include "bitmaps/ballbirth4.xpm"
#include "bitmaps/ballbirth5.xpm"
#include "bitmaps/ballbirth6.xpm"
#include "bitmaps/ballbirth7.xpm"
#include "bitmaps/ballbirth8.xpm"

#include "include/audio.h"
#include "include/error.h"
#include "include/score.h"
#include "include/sfx.h"
#include "include/init.h"
#include "include/main.h"
#include "include/stage.h"
#include "include/blocks.h"
#include "include/paddle.h"
#include "include/misc.h"
#include "include/level.h"
#include "include/mess.h"
#include "include/special.h"

#include "include/ball.h"

/*
 *  Internal macro definitions:
 */

#define X2COL(col, x) (col = x / colWidth)
#define Y2ROW(row, y) (row = y / rowHeight)

#define COL2X(x, col) (x = col * colWidth)
#define ROW2Y(y, row) (y = row * rowHeight)

/* MIN returns the smallest at a and b */
#ifndef MIN
#define MIN(a,b) ((a)<(b) ? (a):(b))
#endif

/* MAX returns the largest of a and b */
#ifndef MAX
#define MAX(a,b) ((a)>(b) ? (a):(b))
#endif

/* SQR returns the square of x */
#ifndef SQR
#define SQR(x) ((x)*(x))
#endif

/*
 *  Internal type declarations:
 */

#if NeedFunctionPrototypes
static void MoveBall(Display *display, Window window, int x, int y, int replace,
	int i);
static void MoveBallBirth(Display *display, Window window, int x, int y, 
	int slide, int replace, int i);
static void TeleportBall(Display *display, Window window, int i);
static int BallHitPaddle(Display *display, Window window, int *hit, int i,
	int *x, int *y);
static void UpdateABall(Display *display, Window window, int i);
static int CheckRegions(Display *display, Window window, int row, int col,
	int x, int y, int i);
static int CheckForCollision(Display *display, Window window, int x, int y, 
	int *r, int *c, int i);
static void updateBallVariables(int i);
static void SetBallWait(enum BallStates newMode, int waitFrame, int i);
static void DoBallWait(int i);
static void EraseTheBall(Display *display, Window window, int x, int y);
static void ChangeBallDirectionToGuide(int i);
static void Ball2BallCollision(BALL *ball1, BALL *ball2);
static int WhenBallsCollide(BALL *ball1, BALL *ball2, float *time);
#else
static void Ball2BallCollision();
static int WhenBallsCollide();
static void ChangeBallDirectionToGuide();
static void MoveBall();
static void MoveBallBirth();
static void TeleportBall();
static int BallHitPaddle();
static void UpdateABall();
static int CheckRegions();
static int CheckForCollision();
static void updateBallVariables();
static void SetBallWait();
static void DoBallWait();
static void EraseTheBall();
#endif

typedef struct
{
   float x, y;
} vector_t;

/*
 *  Internal variable declarations:
 */

static Pixmap ballsPixmap[BALL_SLIDES];
static Pixmap ballsMask[BALL_SLIDES];
static Pixmap ballBirthPixmap[BIRTH_SLIDES];
static Pixmap ballBirthMask[BIRTH_SLIDES];
static Pixmap guides[11];
static Pixmap guidesM[11];
BALL balls[MAX_BALLS];
static int guidePos = 6;	 /* Start in middle of guider */

/* global constant machine epsilon */
float MACHINE_EPS;

#if NeedFunctionPrototypes
void InitialiseBall(Display *display, Window window, Colormap colormap)
#else
void InitialiseBall(display, window, colormap)
	Display 	*display;
	Window 		window;
	Colormap 	colormap;
#endif
{
	/*
	 * Read and create all the animation frames for the balls and guides.
	 */

    XpmAttributes   attributes;
	int		    	XpmErrorStatus;

    attributes.valuemask = XpmColormap;
	attributes.colormap = colormap;

	/* Create the xpm pixmap ball frames */
	XpmErrorStatus = XpmCreatePixmapFromData(display, window, ball1_xpm,
		&ballsPixmap[0], &ballsMask[0], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBall(ball1)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, ball2_xpm,
		&ballsPixmap[1], &ballsMask[1], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBall(ball2)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, ball3_xpm,
		&ballsPixmap[2], &ballsMask[2], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBall(ball3)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, ball4_xpm,
		&ballsPixmap[3], &ballsMask[3], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBall(ball4)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, killer_xpm,
		&ballsPixmap[4], &ballsMask[4], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBall(killer)");

	/* Ball birth sequence */

	/* Create the xpm pixmap ball birth frames */
	XpmErrorStatus = XpmCreatePixmapFromData(display, window, ballbirth1_xpm,
		&ballBirthPixmap[0], &ballBirthMask[0], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBall(ballbirth1)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, ballbirth2_xpm,
		&ballBirthPixmap[1], &ballBirthMask[1], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBall(ballbirth2)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, ballbirth3_xpm,
		&ballBirthPixmap[2], &ballBirthMask[2], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBall(ballbirth3)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, ballbirth4_xpm,
		&ballBirthPixmap[3], &ballBirthMask[3], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBall(ballbirth4)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, ballbirth5_xpm,
		&ballBirthPixmap[4], &ballBirthMask[4], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBall(ballbirth5)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, ballbirth6_xpm,
		&ballBirthPixmap[5], &ballBirthMask[5], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBall(ballbirth6)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, ballbirth7_xpm,
		&ballBirthPixmap[6], &ballBirthMask[6], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBall(ballbirth7)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, ballbirth8_xpm,
		&ballBirthPixmap[7], &ballBirthMask[7], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBall(ballbirth8)");

	/* Now load in the guide pixmaps */

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, guide1_xpm,
		&guides[0], &guidesM[0], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBall(guide1)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, guide2_xpm,
		&guides[1], &guidesM[1], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBall(guide2)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, guide3_xpm,
		&guides[2], &guidesM[2], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBall(guide3)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, guide4_xpm,
		&guides[3], &guidesM[3], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBall(guide4)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, guide5_xpm,
		&guides[4], &guidesM[4], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBall(guide5)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, guide6_xpm,
		&guides[5], &guidesM[5], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBall(guide6)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, guide7_xpm,
		&guides[6], &guidesM[6], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBall(guide7)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, guide8_xpm,
		&guides[7], &guidesM[7], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBall(guide8)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, guide9_xpm,
		&guides[8], &guidesM[8], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBall(guide9)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, guide10_xpm,
		&guides[9], &guidesM[9], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBall(guide10)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, guide11_xpm,
		&guides[10], &guidesM[10], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBall(guide11)");

	/* Free the xpm pixmap attributes */
	XpmFreeAttributes(&attributes);

	MACHINE_EPS = sqrt(MINFLOAT);

	/* Make sure that all the balls are initialised */
	ClearAllBalls();
}

#if NeedFunctionPrototypes
void FreeBall(Display *display)
#else
void FreeBall(display)
	Display *display;
#endif
{
	/*
	 * Free all the animation frames for the balls and guides etc.
	 */

	int i;

	/* Free all animation frames for the ball */
	for (i = 0; i < BALL_SLIDES; i++)
	{
		if (ballsPixmap[i]) 	XFreePixmap(display, ballsPixmap[i]);
	 	if (ballsMask[i]) 		XFreePixmap(display, ballsMask[i]);
	}

	/* Free all animation frames for the guides */
	for (i = 0; i < 11; i++)
	{
		if (guides[i]) 	XFreePixmap(display, guides[i]);
	 	if (guidesM[i]) XFreePixmap(display, guidesM[i]);
	}

	/* Free all animation frames for the ball birth */
	for (i = 0; i < BIRTH_SLIDES; i++)
	{
		/* Free the ball birth animation pixmaps */
		if (ballBirthPixmap[i]) 	XFreePixmap(display, ballBirthPixmap[i]);
	 	if (ballBirthMask[i]) 		XFreePixmap(display, ballBirthMask[i]);
	}
}

#if NeedFunctionPrototypes
void RedrawBall(Display *display, Window window)
#else
void RedrawBall(display, window)
	Display *display; 
	Window window;
#endif
{
	/* not hard - STILL TO BE IMPLEMENTED */
}

#if NeedFunctionPrototypes
static void EraseTheBall(Display *display, Window window, int x, int y)
#else
static void EraseTheBall(display, window, x, y)
	Display *display;
	Window window;
	int x;
	int y;
#endif
{
	/* 
	 * Clear the ball area! The x, y coordinates are the centre of ball 
	 */

    XClearArea(display, window, x - BALL_WC, y - BALL_HC, 
		BALL_WIDTH, BALL_HEIGHT, False);
}

#if NeedFunctionPrototypes
void DrawTheBall(Display *display, Window window, int x, int y, int slide)
#else
void DrawTheBall(display, window, x, y, slide)
	Display *display;
	Window window;
	int x;
	int y; 
	int slide;
#endif
{
	/* 
	 * Draw the ball using the slide variable as the index into the frames
	 * of the ball animation. The x,y are the centre of the ball.
	 */

    RenderShape(display, window, ballsPixmap[slide], ballsMask[slide],
		x - BALL_WC, y - BALL_HC, BALL_WIDTH, BALL_HEIGHT, False);
}

#if NeedFunctionPrototypes
void DrawTheBallBirth(Display *display, Window window, int x, int y, int slide)
#else
void DrawTheBallBirth(display, window, x, y, slide)
	Display *display;
	Window window;
	int x;
	int y;
	int slide;
#endif
{
	/* 
	 * Draw the ball using the slide variable as the index into the frames
	 * of the ball animation. The x,y are the centre of the ball birth anim.
	 */

    RenderShape(display, window, ballBirthPixmap[slide], ballBirthMask[slide],
		x - BALL_WC, y - BALL_HC, BALL_WIDTH, BALL_HEIGHT, False);
}

#if NeedFunctionPrototypes
static void MoveBallBirth(Display *display, Window window, int x, int y, 
	int slide, int replace, int i)
#else
static void MoveBallBirth(display, window, x, y, slide, replace, i)
	Display *display;
	Window window;
	int x;
	int y; 
	int slide;
	int replace;
	int i;
#endif
{
	/* 
	 * Remove any debris under ball first by clearing it 
	 */

	if (replace)
	{
		XClearArea(display, window, 
			balls[i].oldx - BALL_WC, balls[i].oldy - BALL_HC, 
			BALL_WIDTH, BALL_HEIGHT, False);
	}	

	balls[i].oldx = x;
	balls[i].oldy = y;

    /* If slide is -1 then clear the ball area */
    if (slide == -1)
        XClearArea(display, window,
            x - BALL_WC, y - BALL_HC, BALL_WIDTH, BALL_HEIGHT, False);
    else
        DrawTheBallBirth(display, window, x, y, slide);
}

#if NeedFunctionPrototypes
static void MoveBall(Display *display, Window window, int x, int y, int replace,
	int i)
#else
static void MoveBall(display, window, x, y, replace, i)
	Display *display; 
	Window window;
	int x;
	int y;
	int replace;
	int i;
#endif
{
	/*
	 * Move the ball from one position to the next and also update the
	 * balls old positions. Will only move if the frame is correct for
	 * this move, ie: framrate. Ball animates as well - hard to see though.
	 */

	if (replace)
		EraseTheBall(display, window, balls[i].oldx, balls[i].oldy);

	/* Update the old position of this ball */
	balls[i].oldx = x;
	balls[i].oldy = y;

	if (Killer == True)
	{
		/* Render the killer ball now ie: red ball */
		DrawTheBall(display, window, x, y, BALL_SLIDES-1);
	}
	else
	{
		/* Render the ball now */
		DrawTheBall(display, window, x, y, balls[i].slide);
	}	

	/* Change slide for ball every n frames of animation */
	if ((frame % BALL_ANIM_RATE) == 0)
		balls[i].slide++;
	
	/* wrap around slides */
	if (balls[i].slide == BALL_SLIDES-1) balls[i].slide = 0;
}

#if NeedFunctionPrototypes
static void MoveGuides(Display *display, Window window, int i, int remove)
#else
static void MoveGuides(display, window, i, remove)
	Display *display; 
	Window window;
	int i;
	int remove;
#endif
{
	/*
	 * The guide hangs out above a READY ball when waiting to be sent into
	 * action. It has a rotating yellow dot that indicates which direction
	 * the ball will be heading off in. This code animates and also clears
	 * the guide.
	 */

	static int oldgx = 0;
	static int oldgy = 0;
	static int inc = 1;

	/* Clear the old slide */
    XClearArea(display, window, oldgx - 14, oldgy - 6, 29, 12, False);

	if (remove == False)
	{
		/* Update its old positions */
		oldgx = balls[i].oldx;
		oldgy = balls[i].oldy - 16;

		/* Check for any silly errors */
		if (guidePos < 0 || guidePos > 10)
			ErrorMessage("Guidepos out of range.");

		/* draw the guide pixmap */
    	RenderShape(display, window, guides[guidePos], guidesM[guidePos],
			oldgx - 14, oldgy - 6, 29, 12, False);

		/* Don't draw it ever frame */
		if ((frame % (BALL_FRAME_RATE*8)) == 0)
			guidePos += inc;

		/* wrap around slides */
		if (guidePos == 10) inc = -1;
		if (guidePos == 0) inc = 1;
	}
	else
		guidePos = 6;
}

#if NeedFunctionPrototypes
void RandomiseBallVelocity(int i)
#else
void RandomiseBallVelocity(i)
	int i;
#endif
{
	balls[i].dx = balls[i].dy = 0;

	/* Loop until values are random */
	while (balls[i].dx == 0 || balls[i].dy == 0)
	{
		/* Randomise the ball */
   	 	balls[i].dx = (2 - (rand() % 4)) * 2;
   	 	balls[i].dy = (2 - (rand() % 4)) * 3;
		balls[i].lastPaddleHitFrame = frame + PADDLE_BALL_FRAME_TILT;
	}
}

#if NeedFunctionPrototypes
void DoBoardTilt(Display *display, int i)
#else
void DoBoardTilt(display, i)
	Display *display;
	int i;
#endif
{
	/*
	 * In the event of a ball loop bounce then this function will fiddle
	 * with the velocity and it may just jump out of the loop.
	 */

	/* Only worry about active balls */
	if (balls[i].ballState == BALL_ACTIVE)
	{
		DEBUG("Auto Tilt activated.");

		/* Tilt the board to remove any endless loops */
		SetCurrentMessage(display, messWindow, 
			"Auto Tilt Activated", True);
	
		RandomiseBallVelocity(i);
	}
}

#if NeedFunctionPrototypes
static void TeleportBall(Display *display, Window window, int i)
#else
static void TeleportBall(display, window, i)
	Display *display;
 	Window window;
	int i;
#endif
{
	/* 
	 * This function will teleport the ball to some other space not occupied
	 * and start off there.
	 */

	int r1, c1, s1, r2, c2, s2, r3, c3, s3, r4, c4, s4;
    int r, c, x, y;
    struct aBlock *blockP, *bP;
	int done = False;

	/* Loop until we find a block to move to */
	while (done == False)
	{
		/* Give me a new random block position */
		r = (rand() % (MAX_ROW - 6)) + 1;
		c = (rand() % MAX_COL) + 1;

    	if (r < 0 || r >= MAX_ROW) continue;
    	if (c < 0 || c >= MAX_COL) continue;

		/* Pointer to the correct block we need - speed things up */
		blockP = &blocks[r][c];

		/* Check that the block is not occupied and not exploding */
		if ((blockP->occupied == False) && (blockP->exploding == False))
		{
        	/* Check that the block is not a closed position */

            r1 = r;     c1 = c - 1;  s1 = 0;
            if (r1 < 0 || r1 >= MAX_ROW) s1 = 1;
            if (c1 < 0 || c1 >= MAX_COL) s1 = 1;
            if (s1 == 0) 
			{
            	bP = &blocks[r1][c1];
                if (bP->blockType == BLACK_BLK)
                s1 = 1;
            }

            r2 = r - 1; c2 = c;      s2 = 0;
            if (r2 < 0 || r2 >= MAX_ROW) s2 = 1;
            if (c2 < 0 || c2 >= MAX_COL) s2 = 1;
            if (s2 == 0) 
			{
                bP = &blocks[r2][c2];
                if (bP->blockType == BLACK_BLK)
                    s2 = 1;
            }

            r3 = r;     c3 = c + 1;  s3 = 0;
            if (r3 < 0 || r3 >= MAX_ROW) s3 = 1;
            if (c3 < 0 || c3 >= MAX_COL) s3 = 1;
            if (s3 == 0) 
			{
                bP = &blocks[r3][c3];
                if (bP->blockType == BLACK_BLK)
                    s3 = 1;
            }

            r4 = r + 1; c4 = c;      s4 = 0;
            if (r4 < 0 || r4 >= MAX_ROW) s4 = 1;
            if (c4 < 0 || c4 >= MAX_COL) s4 = 1;
            if (s4 == 0) 
			{
                bP = &blocks[r4][c4];
                if (bP->blockType == BLACK_BLK)
                    s4 = 1;
            }

			/* If it isn't ok to go here then keep searching */
            if ((s1 == 1) && (s2 == 1) && (s3 == 1) && (s4 == 1))
                continue;

			/* Calculate the new ball coordinates */
			COL2X(x, c);
			ROW2Y(y, r);

			balls[i].ballx = x;
			balls[i].bally = y;

			/* Move the ball to the new position */
			MoveBall(display, window, x, y, True, i);

			balls[i].lastPaddleHitFrame = frame + PADDLE_BALL_FRAME_TILT;

			/* Ok jump out now thanks. */
			done = True;

			DEBUG("Ball was Teleported.");
		}
	}
}

#if NeedFunctionPrototypes
void SplitBallInTwo(Display *display, Window window)
#else
void SplitBallInTwo(display, window)
	Display *display;
 	Window window;
#endif
{
	/*
	 * If a multiball block was hit then start another ball from somewhere.
	 * Start it somewhere random and also randomise the velocity.
	 */

	int j;

	j = AddANewBall(display, 0, 0, 3, 3);
	if (j > 0)
	{
		/* Make this new ball move straight away */
		ChangeBallMode(BALL_ACTIVE, j);
		TeleportBall(display, window, j);
		RandomiseBallVelocity(j);

		SetCurrentMessage(display, messWindow, "Another ball!", True);
	}
	else
		SetCurrentMessage(display, messWindow,
			"Cannot add ball!", True);
}

#if NeedFunctionPrototypes
void ClearBallNow(Display *display, Window window, int i)
#else
void ClearBallNow(display, window, i)
	Display *display;
 	Window window;
	int i;
#endif
{
	/*
	 * Terminate and clear this ball.
	 */

	EraseTheBall(display, window, balls[i].ballx, balls[i].bally);
	ClearBall(i);
	DeadBall(display, window);

	DEBUG("Clear ball now called.");
}

#if NeedFunctionPrototypes
void KillBallNow(Display *display, Window window, int i)
#else
void KillBallNow(display, window, i)
	Display *display;
 	Window window;
	int i;
#endif
{
	/*
	 * Set the ball to die by popping.
	 */

	ChangeBallMode(BALL_POP, i);

	DEBUG("Ball mode now BALL_POP.");
}

#if NeedFunctionPrototypes
void GetBallPosition(int *ballX, int *ballY, int i)
#else
void GetBallPosition(ballX, ballY, i)
	int *ballX;
	int *ballY;
	int i;
#endif
{
	/*
	 * Get the position of ball i.
	 */

	*ballX = balls[i].ballx;
	*ballY = balls[i].bally;
}

#if NeedFunctionPrototypes
static int BallHitPaddle(Display *display, Window window, int *hit, int i,
	int *x, int *y)
#else
static int BallHitPaddle(display, window, hit, i, x, y)
	Display *display;
	Window window;
	int *hit;
	int i;
	int *x, *y;
#endif
{
	/*
	 * Handle the ball hitting the paddle if it is. The bounce it back
	 * at an angle that is described below.
	 */

	float x1, x2, y1, y2, alpha, beta, xP1, xP2, xH, yH;
    int paddleLine;

	/***********************************************************************

                        A1 (x1,y1)
                        *
                       .
                      .
         P1 =========.=========== P2   <----   paddle (x, y pos is known )
        (xP1,yP1)   . H (xH, yH)    (xP2,yP2)
                   .
                  .
                 .
                *
               A2 (x2,y2)

   		Given the line A1A2, is the intersecting point H (xH, yH) in the paddle 
		segment ? (i.e xH in [xP1,xP2])

   		A1A2 is :  y = alpha * x + beta

   		A1 and A2 are in A1A2 than beta = [(y1 + y2) - alpha*(x1+x2)] / 2

   		yH = yP1 = yP2

   		so xH = (yP1 - beta) / alpha

	**********************************************************************/

    paddleLine = (PLAY_HEIGHT - DIST_BASE - 2);

    if (balls[i].bally + BALL_HC > paddleLine)
   	{
   		xP1 = (float)(paddlePos - (GetPaddleSize() / 2) - BALL_WC);
   		xP2 = (float)(paddlePos + (GetPaddleSize() / 2) + BALL_WC);

   		if (balls[i].dx == 0)
      	{
			/* process the vertical moving balls */
      		if (((float)balls[i].ballx > xP1) && ((float)balls[i].ballx < xP2))
         	{
         		/* the ball hit the paddle */
         		*hit = balls[i].ballx - paddlePos;
         		*x 	 = balls[i].ballx;
         		*y 	 = paddleLine - BALL_HC;

         		return True;
         	}
      		else
        		return False;
      	}
   		else
      	{
 			/* compute the line coefficients of the ball */

			alpha 	= (float) balls[i].dy;
    		x1 		= (float) (balls[i].ballx - balls[i].dx);
    		y1 		= (float) (balls[i].bally - balls[i].dy);
    		x2 		= (float) (balls[i].ballx);
    		y2 		= (float) (balls[i].bally);
    		beta 	= ((y1 + y2) - alpha * (x1 + x2)) / 2.0;

    		yH = (float) paddleLine;
    		xH = (yH - beta) / alpha;

			if ((xH > xP1) && (xH < xP2))
    		{
    			/* the ball hit the paddle */
        		*hit 	= (int) (xH + 0.5) - paddlePos;
        		*x 		= (int) (xH + 0.5);
        		*y 		= paddleLine - BALL_HC;

        		return True;
    		}
    		else
    			return False;
		}
	}

   	/* We didn't hit the paddle */
   	return False;
}

#if NeedFunctionPrototypes
static int HandleTheBlocks(Display *display, Window window, int row, int col,
	int i)
#else
static int HandleTheBlocks(display, window, row, col, i)
	int row;
	int col;
	int i;
#endif
{
	/*
	 * When a ball hits a block it calls this routine and this routine
	 * will perform the function relevant to that block. eg: teleport
	 * will teleport the ball.
	 *
	 * Will return TRUE if the ball is not to bounce off the block.
	 */

	struct aBlock *blockP;

	/* Pointer to the block the ball is in */
	blockP = &blocks[row][col];

	/* There has been a collision so handle it */
	if (blockP->exploding == False)
	{
		if (blockP->blockType == COUNTER_BLK)
		{
			balls[i].lastPaddleHitFrame = frame + PADDLE_BALL_FRAME_TILT;

			/* If in killer mode then don't bounce off block */
			if (Killer == True)
			{
				DrawBlock(display, window, row, col, KILL_BLK);
				return True;
			}

			/* Special case for counter - reduce count on block */
			if (blockP->counterSlide == 0)
				DrawBlock(display, window, row, col, KILL_BLK);
			else
			{
				/* Draw the counter block minus one number */
				blockP->counterSlide--;
				DrawBlock(display, window, row, col, COUNTER_BLK);
			}
		}
		else
		{
			if (blockP->blockType == MGUN_BLK)
			{
				/* Turn the machine gun on */
				ToggleFastGun(display, True);
				DrawSpecials(display);
				SetCurrentMessage(display, messWindow,
					"Machine Gun", True);

				/* Not a wall so explode the block */
				DrawBlock(display, window, row, col, KILL_BLK);
				balls[i].lastPaddleHitFrame = frame + PADDLE_BALL_FRAME_TILT;

				/* If in killer mode then don't bounce off block */
				if (Killer == True) return True;

			} else if (blockP->blockType == DEATH_BLK)
			{
				/* Ha ha - hit death block so die */

				/* Kill the ball now */
				ClearBallNow(display, window, i);

				/* Not a wall so explode the block */
				DrawBlock(display, window, row, col, KILL_BLK);
				balls[i].lastPaddleHitFrame = frame + PADDLE_BALL_FRAME_TILT;

				/* If in killer mode then don't bounce off block */
				if (Killer == True) return True;

			} else if (blockP->blockType == HYPERSPACE_BLK)
			{
				/* Teleport to somewhere else */
				TeleportBall(display, window, i);

				/* Redraw it just in case */
				DrawBlock(display, window, row, col, HYPERSPACE_BLK);
				balls[i].lastPaddleHitFrame = frame + PADDLE_BALL_FRAME_TILT;

				PlaySoundForBlock(HYPERSPACE_BLK);

				return True;

			} else if (blockP->blockType == WALLOFF_BLK)
			{
				/* Walls are now turned off */
				ToggleWallsOn(display, True);
				DrawSpecials(display);
				SetCurrentMessage(display, messWindow,
					"Walls off", True);

				/* Not a wall so explode the block */
				DrawBlock(display, window, row, col, KILL_BLK);
				balls[i].lastPaddleHitFrame = frame + PADDLE_BALL_FRAME_TILT;

				/* If in killer mode then don't bounce off block */
				if (Killer == True) return True;

			} else if (blockP->blockType == REVERSE_BLK)
			{
				/* Paddle control now reverse */
				ToggleReverse(display);
				SetCurrentMessage(display, messWindow,
					"Reverse Control", True);
				DrawSpecials(display);

				/* Move the paddle to reflect reversed paddle */
				handlePaddleMoving(display);

				/* Not a wall so explode the block */
				DrawBlock(display, window, row, col, KILL_BLK);
				balls[i].lastPaddleHitFrame = frame + PADDLE_BALL_FRAME_TILT;

				/* If in killer mode then don't bounce off block */
				if (Killer == True) return True;

			} else if (blockP->blockType == PAD_SHRINK_BLK)
			{
				/* Paddle shrinking block */
				ChangePaddleSize(display, window, PAD_SHRINK_BLK);
				SetCurrentMessage(display, messWindow,
					"Shrink Paddle", True);

				/* Not a wall so explode the block */
				DrawBlock(display, window, row, col, KILL_BLK);
				balls[i].lastPaddleHitFrame = frame + PADDLE_BALL_FRAME_TILT;

				/* If in killer mode then don't bounce off block */
				if (Killer == True) return True;

			} else if (blockP->blockType == PAD_EXPAND_BLK)
			{
				/* Paddle expanding block */
				ChangePaddleSize(display, window, PAD_EXPAND_BLK);
				SetCurrentMessage(display, messWindow,
					"Expand Paddle", True);

				/* Not a wall so explode the block */
				DrawBlock(display, window, row, col, KILL_BLK);
				balls[i].lastPaddleHitFrame = frame + PADDLE_BALL_FRAME_TILT;

				/* If in killer mode then don't bounce off block */
				if (Killer == True) return True;

			} else if (blockP->blockType == EXTRABALL_BLK)
			{
				/* Extra ball */
				AddExtraLife(display);
				SetCurrentMessage(display, messWindow,
					"Extra ball", True);

				/* Not a wall so explode the block */
				DrawBlock(display, window, row, col, KILL_BLK);
				balls[i].lastPaddleHitFrame = frame + PADDLE_BALL_FRAME_TILT;

				/* If in killer mode then don't bounce off block */
				if (Killer == True) return True;

			} else if (blockP->blockType == STICKY_BLK)
			{
				ToggleStickyBat(display, True);
				DrawSpecials(display);
				SetCurrentMessage(display, messWindow,
					"Sticky Bat", True);

				/* Not a wall so explode the block */
				DrawBlock(display, window, row, col, KILL_BLK);
				balls[i].lastPaddleHitFrame = frame + PADDLE_BALL_FRAME_TILT;

				/* If in killer mode then don't bounce off block */
				if (Killer == True) return True;

			} else if (blockP->blockType == MULTIBALL_BLK)
			{
				ToggleMultiBall(display, True);
				DrawSpecials(display);
				SplitBallInTwo(display, window);

				/* Not a wall so explode the block */
				DrawBlock(display, window, row, col, KILL_BLK);
				balls[i].lastPaddleHitFrame = frame + PADDLE_BALL_FRAME_TILT;

				/* If in killer mode then don't bounce off block */
				if (Killer == True) return True;

			} else if (blockP->blockType != BLACK_BLK)
			{
				/* Not a wall so explode the block */
				DrawBlock(display, window, row, col, KILL_BLK);
				balls[i].lastPaddleHitFrame = frame + PADDLE_BALL_FRAME_TILT;

				/* If in killer mode then don't bounce off block */
				if (Killer == True) return True;
			}
			else if (blockP->blockType == BLACK_BLK)
			{
				/* Redraw the solid wall block to make sure */
				DrawBlock(display, window, row, col, BLACKHIT_BLK);
				blockP->nextFrame = frame + 100;
			}
		}
	}

	/* Don't return after returning */
	return False;
}

#if NeedFunctionPrototypes
static void UpdateABall(Display *display, Window window, int i)
#else
static void UpdateABall(display, window, i)
	Display *display;
	Window window;
	int i;
#endif
{
	/*
	 * Main routine that will update the ball given and handle all collisions
	 * and also bouce off all walls and blocks.
	 */

	int row, col, hitPos, ret, t;
	int cx, cy, step, j, r, ddx, ddy, Hx, Hy;
	float incx, incy, x, y;
	float Vs, Vx, Vy, alpha, beta, gamma, padSize;
	float dummy;

	/* Update ball position using dx and dy values */	
	balls[i].ballx = balls[i].oldx + balls[i].dx;
	balls[i].bally = balls[i].oldy + balls[i].dy;

	/* Mark the ball to die as it is past the paddle */
	if (balls[i].bally > (PLAY_HEIGHT - DIST_BASE + BALL_HEIGHT))
		ChangeBallMode(BALL_DIE, i);

	/* Check if ball has hit left wall and bounce off */		
	if (balls[i].ballx < BALL_WC && noWalls == False)
	{
		balls[i].dx = abs(balls[i].dx);
		if (noSound == False) playSoundFile("boing", 20);

	} else if (noWalls == True && balls[i].ballx < BALL_WC)
	{
		/* If the no walls mode is on then wrap around onto right wall */
		balls[i].ballx = (PLAY_WIDTH - BALL_WC);

		/* Move the ball to the new position */
		MoveBall(display, window, balls[i].ballx, balls[i].bally, True, i);

		return;
	}

	/* Check if ball has hit right wall and bounce off */		
	if (balls[i].ballx > (PLAY_WIDTH - BALL_WC) && noWalls == False)
	{
		balls[i].dx = -(abs(balls[i].dx));
		if (noSound == False) playSoundFile("boing", 20);

	} else if (noWalls == True && balls[i].ballx > (PLAY_WIDTH - BALL_WC))
	{
		/* If the no walls mode is on then wrap around onto left wall */
		balls[i].ballx = BALL_WC;

		/* Move the ball to the new position */
		MoveBall(display, window, balls[i].ballx, balls[i].bally, True, i);

		return;
	}

	/* Check if ball has hit top wall and bounce off */		
	if (balls[i].bally < BALL_HC) 
	{
		balls[i].dy = abs(balls[i].dy);
		if (noSound == False) playSoundFile("boing", 20);
	}

	if (balls[i].ballState != BALL_DIE)
	{
		/* Check if the ball has hit the paddle */
		if (BallHitPaddle(display, window, &hitPos, i, &Hx, &Hy) == True)
		{
			/* Keep track of how long it was since the last paddle hit */
			balls[i].lastPaddleHitFrame = frame + PADDLE_BALL_FRAME_TILT;
			if (noSound == False) playSoundFile("boing", 20);

			/* Add a paddle hit bonus score, I'm nice ;-) */
			AddToScore((u_long) PADDLE_HIT_SCORE);
			DisplayScore(display, scoreWindow, score);

			/* speed vector of the ball */
           	Vx = (float) balls[i].dx;    
           	Vy = (float) balls[i].dy;

			/* speed intensity of the ball */
           	Vs = sqrt(Vx * Vx + Vy * Vy );

           	alpha = atan(Vx / -Vy);

           	padSize = (float) (GetPaddleSize() + BALL_WC);
           	Vx = (float) hitPos;
           	Vy = (float) padSize / 1.0;

           	beta = atan(Vx / Vy);
           	gamma = 2.0 * beta - alpha;

           	Vx = Vs * sin(gamma);
           	Vy = -Vs * cos(gamma);

			/* take in account the horizontal speed of the paddle: 
			 * vectorial summ 
			 */
           	Vx += (float) (paddleDx / 10.0);

           	if (Vx > 0.0)
            	balls[i].dx = (int) (Vx + 0.5);
           	else
             	balls[i].dx = (int) (Vx - 0.5);

           	if (Vy < 0.0)
             	balls[i].dy = (int) (Vy - 0.5);
           	else
             	balls[i].dy = -MIN_DY_BALL;

           	if (balls[i].dy > -MIN_DY_BALL) 
				balls[i].dy = -MIN_DY_BALL;

           	balls[i].ballx = Hx;
           	balls[i].bally = Hy;

			/* handle the sticky paddle special by changing the ball mode
			 * to BALL_READY so it will need user to press space to start
			 * the ball moving again.
			 */
			if (stickyBat == True)	
			{
				ChangeBallMode(BALL_READY, i);

				/* Move the ball to the new position */
				MoveBall(display, window, 
					balls[i].ballx, balls[i].bally, True, i);

				/* So that it will auto shoot off if you wait too long */
				balls[i].nextFrame = frame + BALL_AUTO_ACTIVE_DELAY;

				return;
			}
		}
		else
		{
			/* Ball didn't hit the paddle so check if it time to autotilt the
			 * board as it is in an infinite loop most likely.
			 */
			if (balls[i].lastPaddleHitFrame == frame)
				DoBoardTilt(display, i);
		}

       	Vx = (float) balls[i].dx;
       	Vy = (float) balls[i].dy;
       	Vs = sqrt(Vx * Vx + Vy * Vy);

       	alpha = sqrt((float)MAX_X_VEL*(float)MAX_X_VEL + (float)MAX_Y_VEL*
			(float)MAX_Y_VEL );
       	alpha /= 9.0; /* number of speed level */
       	alpha *= (float) speedLevel;
       	beta = alpha / Vs;
       	Vx *= beta;
       	Vy *= beta;

       	if (Vx > 0.0)
         	balls[i].dx = (int) (Vx + 0.5);
       	else
         	balls[i].dx = (int) (Vx - 0.5);

       	if (Vy > 0.0)
         	balls[i].dy = (int) (Vy + 0.5);
       	else
         	balls[i].dy = (int) (Vy - 0.5);

       	if (balls[i].dy == 0) 
			balls[i].dy = MIN_DY_BALL;

       	if (balls[i].dx == 0) 
			balls[i].dx = MIN_DX_BALL;
	}

	/* Has the player lost the ball of the bottom of the screen */
	if (balls[i].bally > (PLAY_HEIGHT + BALL_HEIGHT*2))
	{
		DEBUG("Ball lost off bottom.");

		/* Make ball start to die */
		ClearBallNow(display, window, i);
		return;
	}

	/* Convert the new ball positions to rows and cols for collision */
	X2COL(col, balls[i].ballx);
	Y2ROW(row, balls[i].bally);

	x = balls[i].oldx;
	y = balls[i].oldy;

	cx = balls[i].dx > 0 ? 1 : -1;
	cy = balls[i].dy > 0 ? 1 : -1;

	if (abs(balls[i].dx) == abs(balls[i].dy))
	{
		incx = (float) cx;
		incy = (float) cy;
		step = abs(balls[i].dx);
	} else if (abs(balls[i].dx) > abs(balls[i].dy))
	{
		incx = (float) cx;
		incy = ((float) abs(balls[i].dy) / (float) abs(balls[i].dx)) * cy;
		step = abs(balls[i].dx);
	} 
	else 
	{
		incy = (float) cy;
		incx = ((float) abs(balls[i].dx) / (float) abs(balls[i].dy)) * cx;
		step = abs(balls[i].dy);
	}

	for (j = 0; j < step; j++)
	{
		/* Check if the ball has hit a brick or something */
		if ((ret = CheckForCollision(display, window, 
			(int) x, (int) y, &row, &col, i)) != REGION_NONE)
		{
			if (HandleTheBlocks(display, window, row, col, i) == True)
				return;

			ddx = ddy = 0;
			r = (rand() >> 16) % 4;

			/* Find out which side the ball hit the brick */
			switch (ret)
			{
				case REGION_LEFT:
					ddx = -r/4;
					balls[i].dx = -(abs(balls[i].dx));
					break;

				case REGION_RIGHT:
					ddx = r/4;
					balls[i].dx = abs(balls[i].dx);
					break;

				case REGION_TOP:
					ddy = -r/4;
					balls[i].dy = -(abs(balls[i].dy));
					break;

				case REGION_BOTTOM:
					ddy = r/4;
					balls[i].dy = abs(balls[i].dy);
					break;

				case (REGION_BOTTOM | REGION_RIGHT):
					ddy = r;
					ddx = r;
					balls[i].dy = abs(balls[i].dy);
					balls[i].dx = abs(balls[i].dx);
					break;

				case (REGION_TOP | REGION_RIGHT):
					ddy = -r;
					ddx = r;
					balls[i].dy = -(abs(balls[i].dy));
					balls[i].dx = abs(balls[i].dx);
					break;

				case (REGION_BOTTOM | REGION_LEFT):
					ddy = r;
					ddx = -r;
					balls[i].dx = -(abs(balls[i].dx));
					balls[i].dy = abs(balls[i].dy);
					break;

				case (REGION_TOP | REGION_LEFT):
					ddy = -r;
					ddx = -r;
					balls[i].dx = -(abs(balls[i].dx));
					balls[i].dy = -(abs(balls[i].dy));
					break;
			}


			/* Update ball position using dx and dy values */	
			balls[i].ballx = (int) x + balls[i].dx + ddx;
			balls[i].bally = (int) y + balls[i].dy + ddy;

			break;
		}

		x += incx;
		y += incy;

	}	/* for */

	/* Move the ball to the new position */
	MoveBall(display, window, balls[i].ballx, balls[i].bally, True, i);

	/* Loop all the balls checking for ball2ball collisions */
	for (t = 0; t < MAX_BALLS; t++)
	{
		/* Online check out active balls */
		if (balls[t].ballState == BALL_ACTIVE)
		{
			/* Will they collide */
			if (WhenBallsCollide(&balls[i], &balls[t], &dummy))
			{
				DEBUG("Ball hit ball - rebound.");

				/* Ok collided - so rebound please */
				Ball2BallCollision(&balls[i], &balls[t]);

				if (noSound == False) playSoundFile("ball2ball", 90);
			}
		}
	}
}

#if NeedFunctionPrototypes
static int CheckRegions(Display *display, Window window, int row, int col,
	int x, int y, int i)
#else
static int CheckRegions(display, window, row, col, x, y, i)
	Display *display;
	Window window;
	int row;
	int col;
	int x, y;
	int i;
#endif
{
	/*
	 * Check each region and see if the ball has hit it.
	 *
	 * Returns the region hit or REGION_NONE.
	 */

    struct aBlock *blockP;
	int region = REGION_NONE;

    if (row < 0 || row >= MAX_ROW) return REGION_NONE;
    if (col < 0 || col >= MAX_COL) return REGION_NONE;

    blockP = &blocks[row][col];

    /* If blocks is occupied then check for collision */
    if (blockP->occupied == 1 && blockP->exploding == False)
    {
        /* Suss out if ball is moving more vertically than horizontally */
        if (abs(balls[i].dx) > abs(balls[i].dy))
        {
            /* Check left and right first as ball is moving more horizontal */

            /* See if the ball intersects with the block's left region */
            if (XRectInRegion(blockP->regionLeft, x - BALL_WC, y - BALL_HC,
                BALL_WIDTH, BALL_HEIGHT) != RectangleOut)
                region |= REGION_LEFT;

            /* See if the ball intersects with the block's right region */
            if (XRectInRegion(blockP->regionRight, x - BALL_WC, y - BALL_HC,
                BALL_WIDTH, BALL_HEIGHT) != RectangleOut)
                region |= REGION_RIGHT;

            /* See if the ball intersects with the block's bottom region */
            if (XRectInRegion(blockP->regionBottom, x - BALL_WC, y - BALL_HC,
                BALL_WIDTH, BALL_HEIGHT) != RectangleOut)
                region |= REGION_BOTTOM;

            /* See if the ball intersects with the block's top region */
            if (XRectInRegion(blockP->regionTop, x - BALL_WC, y - BALL_HC,
                BALL_WIDTH, BALL_HEIGHT) != RectangleOut)
                region |= REGION_TOP;
        }
        else
        {
            /* Check top and bottom first as ball is moving more vertical */

            /* See if the ball intersects with the block's bottom region */
            if (XRectInRegion(blockP->regionBottom, x - BALL_WC, y - BALL_HC,
                BALL_WIDTH, BALL_HEIGHT) != RectangleOut)
                region |= REGION_BOTTOM;

            /* See if the ball intersects with the block's top region */
            if (XRectInRegion(blockP->regionTop, x - BALL_WC, y - BALL_HC,
                BALL_WIDTH, BALL_HEIGHT) != RectangleOut)
                region |= REGION_TOP;

            /* See if the ball intersects with the block's left region */
            if (XRectInRegion(blockP->regionLeft, x - BALL_WC, y - BALL_HC,
                BALL_WIDTH, BALL_HEIGHT) != RectangleOut)
                region |= REGION_LEFT;

            /* See if the ball intersects with the block's right region */
            if (XRectInRegion(blockP->regionRight, x - BALL_WC, y - BALL_HC,
                BALL_WIDTH, BALL_HEIGHT) != RectangleOut)
                region |= REGION_RIGHT;
        }
    }

	/* Return the region combination */
	return region;
}

#if NeedFunctionPrototypes
static int CheckForCollision(Display *display, Window window, int x, int y, 
	int *r, int *c, int i)
#else
static int CheckForCollision(display, window, x, y, r, c, i)
	Display *display;
	Window window;
	int x, y;
	int *r, *c;
	int i;
#endif
{
	/*
	 * Check each adjoining block and see if the ball has hit any region in
	 * it. Will return the hit region or REGION_NONE.
	 */
	int ret, row, col;

	row = *r;
	col = *c;

	/* Check all the regions around block */
	if ((ret = CheckRegions(display, window, row, col, x, y, i)) 
		!= REGION_NONE)	/*nothin*/;
	else if ((ret = CheckRegions(display, window, row+1, col, x, y, i)) 
		!= REGION_NONE)	row++;
	else if ((ret = CheckRegions(display, window, row-1, col, x, y, i)) 
		!= REGION_NONE)	row--;
	else if ((ret = CheckRegions(display, window, row, col+1, x, y, i)) 
		!= REGION_NONE)	col++;
	else if ((ret = CheckRegions(display, window, row, col-1, x, y, i)) 
		!= REGION_NONE)	col--;
	else if ((ret = CheckRegions(display, window, row+1, col+1, x, y, i)) 
		!= REGION_NONE)	{ row++; col++; }
	else if ((ret = CheckRegions(display, window, row-1, col-1, x, y, i)) 
		!= REGION_NONE)	{ row--; col--; }
	else if ((ret = CheckRegions(display, window, row+1, col-1, x, y, i)) 
		!= REGION_NONE) { row++; col--; }
	else if ((ret = CheckRegions(display, window, row-1, col+1, x, y, i)) 
		!= REGION_NONE)	
	{
		*r = row-1;
		*c = col+1;
		return REGION_NONE;
	}

	/* Return the row and column where the ball hit */
	*r = row;
	*c = col;

	return ret;
}

#if NeedFunctionPrototypes
static int WhenBallsCollide(BALL *ball1, BALL *ball2, float *time)
#else
static int WhenBallsCollide(ball1, ball2, time)
    BALL *ball1;
    BALL *ball2;
    float *time;
#endif
{
    /*
     * Calculate when 2 balls will collide.
     * If the balls collide, a True status is returned, and when the
     * incident takes place is returned in time.
     * If the balls don't collide, a False status is returned.
     */

   	vector_t 	p, v;          /* deltas between the 2 balls */
   	float      	tmp1, tmp2, t1, t2, tmin, v2, r2;

   	p.x = ball1->ballx   -  ball2->ballx;
   	p.y = ball1->bally   -  ball2->bally;
   	v.x = ball1->dx  	-  ball2->dx;
    v.y = ball1->dy  	-  ball2->dy;

   	v2 = SQR(v.x) + SQR(v.y);
   	r2 = SQR(ball1->radius + ball2->radius);

   	/*
     * tmp2 >  0   Balls will collide, or are off from a collision direction
     * tmp2 == 0   Balls will touch or have already touched
     * tmp2 <  0   Balls will not be/have not been close to eachother.
     */

   	tmp2 = (v2 * r2) - SQR((v.x * p.y) - (v.y * p.x));

    /*
     * Check the magnitude of v2 to safeguard against numerical trouble.
     * The velocities must be scaled so that this is not a problem,
     * and rather change the time scale so that the travelled distance
     * v * t is constant.
     */

   	if (tmp2 >= 0.0 && v2 > MACHINE_EPS)
   	{
      	tmp2 = sqrt(tmp2) / v2;
      	tmp1 = -((p.x * v.x) + (p.y * v.y)) / v2;

      	t1 = tmp1 - tmp2;
      	t2 = tmp1 + tmp2;

      	/*
       	 * Choose the smallest of t1 and t2.
       	 * Note that both solutions t1 and t2 will (should) have the same sign.
       	 * If t1 and t2 are opposite sign, this means that the two ball centers
       	 * are closer to eachother than their combined radius.
       	 */
      	tmin = MIN(t1, t2);

      	if (tmin >= 0.0 && tmin <= 1.0)
	  	{
         	*time = tmin;
			return True;
		}
   	}

   	*time = 0.0;

   	return False;
}

#if NeedFunctionPrototypes
static void Ball2BallCollision(BALL *ball1, BALL *ball2)
#else
static void Ball2BallCollision(ball1, ball2)
    BALL *ball1;
    BALL *ball2;
#endif
{
    /*
     * Calclulate the new velocity (direction) of the balls after a collision.
     * On entry, the balls positions and velocities are set to those values
     * when the collision takes place.  On exit, the balls velocities are set
     * to their new directions, whilst the position info remains unchanged.
     */

   vector_t   p, v;          /* deltas between the 2 balls */
   float      k, plen, massrate;

   p.x  = ball1->ballx  -  ball2->ballx;
   p.y  = ball1->ballx  -  ball2->bally;
   v.x  = ball1->dx  	-  ball2->dx;
   v.y  = ball1->dy  	-  ball2->dy;

   /*
    * p is the direction between the 2 balls centers, and will
    * have the langth of ball1->radius + ball2->radius
    */
   plen = sqrt(SQR(p.x) + SQR(p.y));
   p.x /= plen;
   p.y /= plen;

   massrate = ball1->mass / ball2->mass;

   k = -2.0 * ((v.x * p.x) + (v.y * p.y)) / (1.0 + massrate);
   ball1->dx += (int) (k * p.x);
   ball1->dy += (int) (k * p.y);

   /* New k for ball 2 */
   k *= -massrate;
   ball2->dx += (int) (k * p.x);
   ball2->dy += (int) (k * p.y);
}

#if NeedFunctionPrototypes
static void updateBallVariables(int i)
#else
static void updateBallVariables(i)
	int i;
#endif
{
	/*
	 * Update the balls x and old pos for a ball moving on the paddle 
	 * waiting to be shot off. Also ball birth.
	 */

	balls[i].ballx 	= paddlePos;
	balls[i].bally 	= PLAY_HEIGHT - DIST_BALL_OF_PADDLE;
	balls[i].oldx 	= balls[i].ballx;
	balls[i].oldy 	= balls[i].bally;
}

#if NeedFunctionPrototypes
int GetNumberOfActiveBalls(void)
#else
int GetNumberOfActiveBalls()
#endif
{
	/*
	 * Return the number of balls active in the arena.
	 */

	int i;
	int t;

	/* Zap through the list of balls */
	for (i = 0, t = 0; i < MAX_BALLS; i++)
	{
		/* Found an active abll - add to total */
		if (balls[i].ballState == BALL_ACTIVE)
			t++;
	}

	/* Return the number of active balls */
	return t;
}

#if NeedFunctionPrototypes
int GetAnActiveBall(void)
#else
int GetAnActiveBall()
#endif
{
	/*
	 * 
	 */
	int i;

	/* Zap through the list of balls */
	for (i = 0; i < MAX_BALLS; i++)
	{
		/* Return this ball */
		if (balls[i].ballState == BALL_ACTIVE)
			return i;
	}

	return -1;
}

#if NeedFunctionPrototypes
int IsBallWaiting(void)
#else
int IsBallWaiting()
#endif
{
	/*
	 * Check to see if any ball is ready to be actiavted.
	 */
	int i;

	/* Zap through the list of balls */
	for (i = 0; i < MAX_BALLS; i++)
	{
		/* Ok it must be on the paddle so shoot it off */
		if (balls[i].ballState == BALL_READY)
			return True;
	}

	return False;
}

#if NeedFunctionPrototypes
static void ChangeBallDirectionToGuide(int i)
#else
static void ChangeBallDirectionToGuide(i)
	int i;
#endif
{
	/* 
	 * Change the direction vector of the ball to that of the guide
	 * marker so that it will shoot off in the direction you want.
	 */
	int dx, dy;

	dx = dy = 0;

	switch (guidePos)
	{
		/* Left to middle to right */
		case 0: dx = -5; dy = -1; break;

		case 1: dx = -4; dy = -2; break;

		case 2: dx = -3; dy = -3; break;

		case 3: dx = -2; dy = -4; break;

		case 4: dx = -1; dy = -5; break;

		case 5:	dx = 0; dy = -5; break;

		case 6: dx = 1; dy = -5; break;

		case 7: dx = 2; dy = -4; break;

		case 8: dx = 3; dy = -3; break;

		case 9: dx = 4; dy = -2; break;

		case 10: dx = 5; dy = -1; break;
	}

	/* Make the ball go off in the direction of the guide */
	balls[i].dx	= dx; balls[i].dy	= dy;

	DEBUG("Changed ball start direction to guide.");
}

#if NeedFunctionPrototypes
int ActivateWaitingBall(Display *display, Window window)
#else
int ActivateWaitingBall(display, window)
	Display *display;
	Window window;
#endif
{
	/* 
	 * Loop through all balls and find the first one that is ready to
	 * be activated and activate it. Also erase the guide marker.
	 */
	int i;

	/* Zap through the list of balls */
	for (i = 0; i < MAX_BALLS; i++)
	{
		/* Ok it must be on the paddle so shoot it off */
		if (balls[i].ballState == BALL_READY)
		{
			/* Change the balls mode so that it shoots off */
			ChangeBallMode(BALL_ACTIVE, i);
			balls[i].lastPaddleHitFrame = frame + PADDLE_BALL_FRAME_TILT;

			ChangeBallDirectionToGuide(i);
			MoveGuides(display, window, i, True);

			return True;
		}
	}

	return False;
}

#if NeedFunctionPrototypes
void ResetBallStart(Display *display, Window window)
#else
void ResetBallStart(display, window)
	Display *display;
	Window window;
#endif
{
	/*
	 * Add a new ball and create it on the paddle. It will then wait to be
	 * activated and have guides above it until activated.
	 */

	int i;

	i = AddANewBall(display, 0, 0, 3, -3);
	if (i >= 0)
	{
		/* Make sure that all variables are updated */
		updateBallVariables(i);

		/* Add 2 bullets every ball death or creation as it happens */
		AddABullet(display);
		AddABullet(display);

		/* Set up animation for ball creation */
		SetBallWait(BALL_CREATE, frame + 1, i);

		DEBUG("Reset ball start and create.");
	}
}

#if NeedFunctionPrototypes
static void AnimateBallPop(Display *display, Window window, int i)
#else
static void AnimateBallPop(display, window, i)
	Display *display;
	Window window;
	int i;
#endif
{
	/*
	 * Animate a ball popping and then kill it off.
	 */

	static int slide = BIRTH_SLIDES + 1;

	if (frame == balls[i].nextFrame)
	{
		/* We are imploding so go backwards through slides */
		slide--;

		/* Wait for the next frame */
		balls[i].nextFrame += BIRTH_FRAME_RATE;

		/* First frame is to clear the ball away */
		if (slide == BIRTH_SLIDES)
		{
			/* Clear the ball area */
    		EraseTheBall(display, window, balls[i].oldx, balls[i].oldy);
			slide--;
		}

		if (slide < 0)
		{
			/* Erase the ball birth image */
			MoveBallBirth(display, window, 
				balls[i].oldx, balls[i].oldy, -1, True, i);

			slide = BIRTH_SLIDES + 1;

			/* Stop the ball by killing it! */
			ClearBall(i);
			ResetBallStart(display, window);
			DeadBall(display, window);
		}
		else
			/* Draw ball birth - handles ball moving as well */
			MoveBallBirth(display, window, 
				balls[i].oldx, balls[i].oldy, slide, True, i);
	}
}

#if NeedFunctionPrototypes
static void AnimateBallCreate(Display *display, Window window, int i)
#else
static void AnimateBallCreate(display, window, i)
	Display *display;
	Window window;
	int i;
#endif
{
	/*
	 * Animate a ball being created and then make it READY on the paddle.
	 */

	static int slide = 0;

	/* Draw the ball birth at the new position */
	MoveBallBirth(display, window, paddlePos, 
		PLAY_HEIGHT - DIST_BALL_OF_PADDLE, slide, True, i);

	if (frame == balls[i].nextFrame)
	{
		/* Next slide thanks */
		slide++;

		/* Frame that will trigger the new slide */
		balls[i].nextFrame += BIRTH_FRAME_RATE;

		if (slide == BIRTH_SLIDES)
		{
			/* Erase the ball birth image */
			MoveBallBirth(display, window, 
				paddlePos, PLAY_HEIGHT - DIST_BALL_OF_PADDLE, -1, 
				True, i);

			slide = 0;

			updateBallVariables(i);

			MoveBall(display, window, paddlePos, 
				PLAY_HEIGHT - DIST_BALL_OF_PADDLE, True, i);

			ChangeBallMode(BALL_READY, i);

			/* This frame will trigger the auto shoot off the ball if you
			 * don't press space within a specified time 
			 */
			balls[i].nextFrame = frame + BALL_AUTO_ACTIVE_DELAY;
		}
		else
			MoveBallBirth(display, window, 
				paddlePos, PLAY_HEIGHT - DIST_BALL_OF_PADDLE, 
				slide, True, i);

		if (paddleIsMoving())
			updateBallVariables(i);
	}
}

#if NeedFunctionPrototypes
void ChangeBallMode(enum BallStates newMode, int i)
#else
void ChangeBallMode(newMode, i)
	enum BallStates newMode;
	int i;
#endif
{
	/*
	 * Change the mode of a ball to a new one.
	 */

	/* Change the ball mode */
	balls[i].ballState = newMode;
}

#if NeedFunctionPrototypes
static void SetBallWait(enum BallStates newMode, int waitFrame, int i)
#else
static void SetBallWait(newMode, waitFrame, i)
	enum BallStates newMode;
	int waitFrame;
	int i;
#endif
{
	/*
	 * While the ball is waiting stay in ball wait mode.
	 */

	/* Set up the ball waiting loop */
	balls[i].waitingFrame	= waitFrame;
	balls[i].waitMode 		= newMode;
	balls[i].ballState 		= BALL_WAIT;
}

#if NeedFunctionPrototypes
static void DoBallWait(int i)
#else
static void DoBallWait(i)
	int i;
#endif
{
	/*
	 * Do the waiting for a ball that is in BALL_WAIT mode.
	 */

	/* Once the waiting frame is reached then activate new state */
	if (frame == balls[i].waitingFrame)
	{
		balls[i].nextFrame = frame + 10;
		balls[i].ballState = balls[i].waitMode;
	}
}

#if NeedFunctionPrototypes
int AddANewBall(Display *display, int x, int y, int dx, int dy)
#else
int AddANewBall(display, x, y, dx, dy)
	Display *display;
	int x;
	int y;
	int dx;
	int dy;
#endif
{
	/*
	 * Function that adds a new ball.
	 *
	 * If one exists to be added it sets up the structure and returns its
	 * index into the ball array. If it fails then -1 is returned.
	 */

	int i;

	/* Add a new ball to the balls array */
	for (i = 0; i < MAX_BALLS; i++)
	{
		/* Is the ball free for us to use? */
		if (balls[i].active == False && GetNumberLife() >= 0)
		{
			/* Make sure that it is clear */
			ClearBall(i);

			/* We have found a new ball spot so setup the ball */
			balls[i].active 	= True;
			balls[i].ballx 		= x;
			balls[i].bally 		= y;
			balls[i].oldx 		= balls[i].ballx;
			balls[i].oldy 		= balls[i].bally;
			balls[i].dx 		= dx;
			balls[i].dy 		= dy;
			balls[i].ballState 	= BALL_CREATE;
			balls[i].mass 		= (rand() % (int)MAX_BALL_MASS) + MIN_BALL_MASS;
			balls[i].slide 		= 0;
			balls[i].nextFrame 	= frame + BIRTH_FRAME_RATE;

			DEBUG("Added new ball to arena.");
			return i;
		}
	}

	/* No more free balls available */
	WarningMessage("Cannot create a new ball - all slots full.");

	DEBUG("Cannot create a new ball as all slots are full.");

	return -1;
}

#if NeedFunctionPrototypes
void ClearBall(int i)
#else
void ClearBall(i)
	int i;
#endif
{
	/*
	 * Initialise all the ball structure to default values.
	 */

	balls[i].waitMode 			= BALL_NONE;
	balls[i].waitingFrame 		= 0;
	balls[i].lastPaddleHitFrame = 0;
	balls[i].nextFrame 			= 0;
	balls[i].newMode 			= BALL_NONE;
	balls[i].active 			= False;
	balls[i].oldx 				= 0;
	balls[i].oldy 				= 0;
	balls[i].ballx 				= 0;
	balls[i].bally 				= 0;
	balls[i].dx 				= 0;
	balls[i].dy 				= 0;
	balls[i].slide 				= 0;
	balls[i].radius 			= BALL_WC;
	balls[i].mass 			 	= MIN_BALL_MASS;
	balls[i].ballState 			= BALL_CREATE;
}

#if NeedFunctionPrototypes
void ClearAllBalls(void)
#else
void ClearAllBalls()
#endif
{
	/*
	 * Clear all ball data so we can start afresh next time.
	 */

	int i;

	DEBUG("Clearing all balls from slots.");

	/* Clear all the balls in the balls array */
	for (i = 0; i < MAX_BALLS; i++)
	{
		/* "Clear the ball" - in an American accent */
		ClearBall(i);
	}
}

#if NeedFunctionPrototypes
void HandleBallMode(Display *display, Window window)
#else
void HandleBallMode(display, window)
	Display *display;
	Window window;
#endif
{
	/*
	 * Handle each active ball and handle each of their modes.
	 *
	 * This function is called very heavily. ;-(
	 */

	int i;

	/* Loop through all the balls */
	for (i = 0; i < MAX_BALLS; i++)
	{
		/* Only handle active balls - sounds disgusting! :-) */
		if (balls[i].active == True)
		{
			/* Switch on the state of the ball */
			switch (balls[i].ballState)
			{
				case BALL_POP:		/* Ball pop animation */
					AnimateBallPop(display, window, i);
					break;

				case BALL_ACTIVE:	/* Animate the ball normally */
					if ((frame % BALL_FRAME_RATE) == 0)
						UpdateABall(display, window, i);
					break;

				case BALL_READY:	/* ball created and waiting to move */
					if (paddleIsMoving())
					{
						balls[i].ballx = paddlePos;
						balls[i].bally = PLAY_HEIGHT - DIST_BALL_OF_PADDLE;

						MoveBall(display, window, balls[i].ballx, 
							balls[i].bally, True, i);
					}

					if ((frame % (BALL_FRAME_RATE)) == 0)
						MoveGuides(display, window, i, False);

					/* After a certain number of seconds fire off anyway */
					if (frame == balls[i].nextFrame)
					{
						ChangeBallMode(BALL_ACTIVE, i);
						ChangeBallDirectionToGuide(i);
						MoveGuides(display, window, i, True);
					}
					break;

				case BALL_STOP:		/* Ball dead and stopped */
					break;

				case BALL_CREATE:	/* Create ball animation */
					AnimateBallCreate(display, window, i);
					break;

				case BALL_WAIT:		/* In wait mode waiting to change state */
					DoBallWait(i);
					break;

				case BALL_DIE:		/* Ball is going to die */
					if ((frame % BALL_FRAME_RATE) == 0)
						UpdateABall(display, window, i);
					break;

				case BALL_NONE:		/* Really cool mode ;-) */
				default:
					break;

			}	/* Ball modes */
		}	/* If active */	
	}	/* For loop */
}
