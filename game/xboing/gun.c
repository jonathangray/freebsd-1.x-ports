#include "include/copyright.h"

/*
 *  Include file dependencies:
 */

#include <xpm.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>

#include "bitmaps/bullet.xpm"
#include "bitmaps/tink.xpm"

#include "include/error.h"
#include "include/audio.h"
#include "include/score.h"
#include "include/init.h"
#include "include/main.h"
#include "include/stage.h"
#include "include/blocks.h"
#include "include/paddle.h"
#include "include/misc.h"
#include "include/level.h"
#include "include/ball.h"
#include "include/special.h"

#include "include/gun.h"

/*
 *  Internal macro definitions:
 */

#define BULLET_DY			-7

#define BULLET_WIDTH		7
#define BULLET_HEIGHT		16

#define BULLET_START_Y		(PLAY_HEIGHT - 40)

#define BULLET_WC			(BULLET_WIDTH / 2)
#define BULLET_HC			(BULLET_HEIGHT / 2)

#define TINK_WIDTH			10
#define TINK_HEIGHT			5

#define TINK_WC				(TINK_WIDTH / 2)
#define TINK_HC				(TINK_HEIGHT / 2)

#define X2COL(col, x) 		(col = x / colWidth)
#define Y2ROW(row, y) 		(row = y / rowHeight)

#define BULLET_FRAME_RATE	3

/* Should be the same */
#define MAX_MOVING_BULLETS	10	
#define MAX_TINKS			10	

#define TINK_DELAY			100	

/*
 *  Internal type declarations:
 */

#if NeedFunctionPrototypes
static int 	ResetBulletStart(Display *display, Window window);
static void CheckTinks(Display *display, Window window);
static void AddTink(Display *display, Window window, int xpos);
static void ClearTinks(void);
static void UpdateBullet(Display *display, Window window);
static int	StartABullet(Display *display, Window window, int xpos);
static void ClearBullet(int i);
static void DrawTheTink(Display *display, Window window, int x, int y);
static void EraseTheTink(Display *display, Window window, int x, int y);
static void DrawBullet(Display *display, Window window, int i);
static int CheckForBulletCollision(Display *display, Window window, 
	int x, int y);
static int CheckBallBulletCollision(Display *display, Window window, 
	int bx, int by, int j);
#else
static int 	ResetBulletStart();
static void CheckTinks();
static void AddTink();
static void ClearTinks();
static void UpdateBullet();
static int 	StartABullet();
static void ClearBullet();
static void DrawTheTink();
static void EraseTheTink();
static void DrawBullet();
static int CheckForBulletCollision();
static int CheckBallBulletCollision();
#endif

static struct 
{
	int xpos;			/* x position of tink centre */
	int	clearFrame;		/* Last frame to clear it */
} tinks[MAX_TINKS];

static struct 
{
	int xpos;			/* x position of bullet */
	int ypos;			/* y position of bullet */
	int oldypos;		/* previous y position */
	int dy;				/* Change in y positoon */
} bullets[MAX_MOVING_BULLETS];

/*
 *  Internal variable declarations:
 */

static Pixmap bulletPixmap, bulletMask;
static Pixmap tinkPixmap, tinkMask;
static int numBullets, unlimitedBullets;

#if NeedFunctionPrototypes
void InitialiseBullet(Display *display, Window window, Colormap colormap)
#else
void InitialiseBullet(display, window, colormap)
	Display *display;
	Window window;
	Colormap colormap;
#endif
{
    XpmAttributes   attributes;
	int		    XpmErrorStatus;

    attributes.valuemask = XpmColormap;
	attributes.colormap = colormap;

	/* Create the xpm pixmap bullet */
	XpmErrorStatus = XpmCreatePixmapFromData(display, window, bullet_xpm,
		&bulletPixmap, &bulletMask, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBullet(bullet)");

	/* Create the xpm pixmap tink for bullet */
	XpmErrorStatus = XpmCreatePixmapFromData(display, window, tink_xpm,
		&tinkPixmap, &tinkMask, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBullet(tink)");

    /* Free the xpm pixmap attributes */
	XpmFreeAttributes(&attributes);

	SetNumberBullets(4);
	ClearTinks();
	ClearBullets();
}

#if NeedFunctionPrototypes
static void CheckTinks(Display *display, Window window)
#else
static void CheckTinks(display, window)
	Display *display;
	Window window;
#endif
{
	int i;

	/* Clear and tinks that need to be cleared */
	for (i = 0; i < MAX_TINKS; i++)
	{
		/* Is this tink active */
		if (tinks[i].xpos != -1)
		{
			/* Time to clear tink? */
			if (frame >= tinks[i].clearFrame)
			{
				/* Clear the tink! */
				EraseTheTink(display, window, tinks[i].xpos, 2);
					
				/* Free the tink up for another */
				tinks[i].xpos = -1;
				tinks[i].clearFrame = 0;
			}
		}
	}
}

#if NeedFunctionPrototypes
static void AddTink(Display *display, Window window, int xpos)
#else
static void AddTink(display, window, xpos)
	Display *display;
	Window window;
	int xpos;
#endif
{
	int i;

	/* Cycle through tinks and try to add one */
	for (i = 0; i < MAX_TINKS; i++)
	{
		/* Is this tink free? */
		if (tinks[i].xpos == -1)
		{
			/* Set the tink array position */
			tinks[i].xpos = xpos;
			tinks[i].clearFrame = frame + TINK_DELAY;

			/* Draw the new found tink! */
			DrawTheTink(display, window, xpos, 2);

			return;
		}
	}

	/* Full tink array - lots of shooting? */
	WarningMessage("Cannot draw tink - tink array full.");
}

#if NeedFunctionPrototypes
static void ClearTinks(void)
#else
static void ClearTinks()
#endif
{
	int i;

	/* Initialise tinks array to empty */
	for (i = 0; i < MAX_TINKS; i++)
	{
		tinks[i].xpos = -1;
		tinks[i].clearFrame = 0;
	}
}


#if NeedFunctionPrototypes
static void UpdateBullet(Display *display, Window window)
#else
static void UpdateBullet(display, window)
	Display *display;
	Window window;
#endif
{
	int i, j;
	int row, col;
	int ballX, ballY;
	struct aBlock *blockP;

	/* Obtain the position of the ball */
	GetBallPosition(&ballX, &ballY, 0);

	/* Draw all bullets that need updating */
	for (i = 0; i < MAX_MOVING_BULLETS; i++)
	{
		/* Is this bullet active */
		if (bullets[i].xpos != -1)
		{
			/* Update bullet position using dy value */	
			bullets[i].ypos = bullets[i].oldypos + bullets[i].dy;

			/* Has the bullet gone off the top edge */
			if (bullets[i].ypos < -BULLET_HC)
			{
				/* Clear the bullet from the screen */
				EraseTheBullet(display, window, 
					bullets[i].xpos, bullets[i].oldypos);

				/* Draw a tink on the top edge */
				AddTink(display, window, bullets[i].xpos);

				/* Free the bullet up for another */
				ClearBullet(i);

				continue;
			}

			if (mode == MODE_GAME)
			{
				for (j = 0; j < MAX_BALLS; j++)
				{
					if (balls[j].active == True)
					{
						/* Has the bullet killed the ball */
						if (CheckBallBulletCollision(display, window, 
							bullets[i].xpos, bullets[i].ypos, j))
						{
							/* Clear the bullet from the screen */
							EraseTheBullet(display, window, 
								bullets[i].xpos, bullets[i].oldypos);
							ClearBullet(i);

							/* Kill the ball off */
							ClearBallNow(display, window, j);

							/* Play the lovel ahhh pop sound for ball shot */
							if (noSound == False) playSoundFile("ballshot", 50);
							break;
						}
					}
				}
			}

			/* Convert the new bullet pos to rows and cols for collision */
			X2COL(col, bullets[i].xpos);
			Y2ROW(row, bullets[i].ypos);

			/* Pointer to the correct block we need - speed things up */
			blockP = &blocks[row][col];

			/* Check if the bullet has hit a brick or something */
			if (CheckForBulletCollision(display, window, 
				bullets[i].xpos, bullets[i].ypos) == True)
			{
				/* Clear the bullet from the screen */
				EraseTheBullet(display, window, 
					bullets[i].xpos, bullets[i].oldypos);

				/* Switch on the type of block hit */
				switch (blockP->blockType)
				{
					case COUNTER_BLK:
						if (blockP->counterSlide == 0)
						{
							/* Counter has counted down to 0 so kill off */
							DrawBlock(display, window, row, col, KILL_BLK);
						}
						else
						{
							/* Decrement counter block and draw new one */
							blockP->counterSlide--;
							DrawBlock(display, window, row, col, 
								COUNTER_BLK);
						}
						break;

					case HYPERSPACE_BLK:
						DrawBlock(display, window, row, col, HYPERSPACE_BLK);
						break;

					case BLACK_BLK:
						DrawBlock(display, window, row, col, BLACK_BLK);
						break;

					case REVERSE_BLK:		
					case MGUN_BLK:		
					case STICKY_BLK:		
					case WALLOFF_BLK:		
					case MULTIBALL_BLK:		
					case DEATH_BLK:		
					case PAD_EXPAND_BLK:		
					case PAD_SHRINK_BLK:		
						/* Shoot the block times to kill it */
						blockP->counterSlide--;

						if (blockP->counterSlide == 0)
						{
							/* Ok then a hit, explode that block */
							DrawBlock(display, window, row, col, KILL_BLK);
						}
						else if (noSound == False)
							playSoundFile("shootdeath", 70);
						break;

					default:
						/* Ok then a hit, explode that block */
						DrawBlock(display, window, row, col, KILL_BLK);
						break;
				}

				/* Free the bullet up for another */
				ClearBullet(i);
			}
			else
				DrawBullet(display, window, i);

			/* Keep track of old position */
			bullets[i].oldypos = bullets[i].ypos;

		}	/* Bullet active? */
	}	/* For loop */
}


#if NeedFunctionPrototypes
static int StartABullet(Display *display, Window window, int xpos)
#else
static int StartABullet(display, window, xpos)
	Display *display;
	Window window;
	int xpos;
#endif
{
	int i;

	/* Cycle through bullets and try to add one */
	for (i = 0; i < MAX_MOVING_BULLETS; i++)
	{
		/* Is this bullet free? */
		if (bullets[i].xpos == -1)
		{
			/* Set the bullet array position */
			bullets[i].xpos = xpos;

			/* Get out of here */
			return True;
		}
		
		/* Break out as the machine gun is not active */
		if (fastGun == False) return False;
	}

	/* Full moving bullet array - lots of shooting? */
	WarningMessage("Cannot draw bullet - bullet array full.");

	return False;
}

#if NeedFunctionPrototypes
static void ClearBullet(int i)
#else
static void ClearBullet(i)
	int i;
#endif
{
	/* Setup the bullet entry */
	bullets[i].xpos 			= -1;
	bullets[i].ypos 			= BULLET_START_Y;
	bullets[i].oldypos 			= BULLET_START_Y;
	bullets[i].dy 				= BULLET_DY;
}

#if NeedFunctionPrototypes
void ClearBullets(void)
#else
void ClearBullets()
#endif
{
	int i;

	/* Initialise bullets array to empty */
	for (i = 0; i < MAX_MOVING_BULLETS; i++)
		ClearBullet(i);
}

#if NeedFunctionPrototypes
void FreeBullet(Display *display)
#else
void FreeBullet(display)
	Display *display;
#endif
{
	if (bulletPixmap)	XFreePixmap(display, bulletPixmap);
 	if (bulletMask) 	XFreePixmap(display, bulletMask);

	if (tinkPixmap)	XFreePixmap(display, tinkPixmap);
 	if (tinkMask) 	XFreePixmap(display, tinkMask);
}

#if NeedFunctionPrototypes
void SetNumberBullets(int num)
#else
void SetNumberBullets(num)
	int num;
#endif
{
	/* Set the number of bullets available */
	numBullets = num;
}

#if NeedFunctionPrototypes
void IncNumberBullets(void)
#else
void IncNumberBullets()
#endif
{
	/* Increment the number of bullets */
	numBullets++;

	/* But don't give to many */
	if (numBullets > MAX_BULLETS) 
		numBullets = MAX_BULLETS;
}

#if NeedFunctionPrototypes
void SetUnlimitedBullets(int state)
#else
void SetUnlimitedBullets(state)
	int state;
#endif
{
	/* Set the unlimit bullets state */
	unlimitedBullets = state;
}

#if NeedFunctionPrototypes
void DecNumberBullets(void)
#else
void DecNumberBullets()
#endif
{
	/* Only decrement number of bullets if the unlimited ammo is off */
	if (unlimitedBullets == False)
	{
		/* Decrement the number of bullets */
		numBullets--;

		/* But not to far */
		if (numBullets < 0) 
			numBullets = 0;
	}
}

#if NeedFunctionPrototypes
int GetNumberBullets(void)
#else
int GetNumberBullets()
#endif
{
	assert(numBullets >= 0);

	/* How many bullets do I have */
	return numBullets;
}

#if NeedFunctionPrototypes
void shootBullet(Display *display, Window window)
#else
void shootBullet(display, window)
	Display *display;
	Window window;
#endif
{
	/* Only shoot if no bullet is active and bullets and ball active */
	if ((GetNumberBullets() > 0) && (IsBallWaiting() == False))
	{
		/* Reset the bullet to the starting possy to go forward */
		if (ResetBulletStart(display, window) == True)
		{
			/* Remove a bullet from the ammunition */
			DeleteABullet(display);

			/* Play a shooting sound */
			if (noSound == False) playSoundFile("shoot", 50);
		}
	}
	else if (GetNumberBullets() == 0)
	{
		/* Play an trigger clicking sound */
		if (noSound == False) playSoundFile("click", 100);
	}
}

#if NeedFunctionPrototypes
void EraseTheBullet(Display *display, Window window, int x, int y)
#else
void EraseTheBullet(display, window, x, y)
	Display *display;
	Window window;
	int x;
	int y;
#endif
{
	/* Erase the bullet pixmap from the window */
    XClearArea(display, window, x - BULLET_WC, y - BULLET_HC, 
		BULLET_WIDTH, BULLET_HEIGHT, False);
}

#if NeedFunctionPrototypes
static void DrawTheTink(Display *display, Window window, int x, int y)
#else
static void DrawTheTink(display, window, x, y)
	Display *display;
	Window window;
	int x;
	int y;
#endif
{
	/* Draw the tink pixmap into the window */
    RenderShape(display, window, tinkPixmap, tinkMask,
		x - TINK_WC, y - TINK_HC, TINK_WIDTH, TINK_HEIGHT, False);
}

#if NeedFunctionPrototypes
static void EraseTheTink(Display *display, Window window, int x, int y)
#else
static void EraseTheTink(display, window, x, y)
	Display *display;
	Window window;
	int x;
	int y;
#endif
{
	/* Erase the tink pixmap from the window */
    XClearArea(display, window, x - TINK_WC, y - TINK_HC, 
		TINK_WIDTH, TINK_HEIGHT, False);
}

#if NeedFunctionPrototypes
void DrawTheBullet(Display *display, Window window, int x, int y)
#else
void DrawTheBullet(display, window, x, y)
	Display *display;
	Window window;
	int x;
	int y;
#endif
{
	/* Draw the bullet pixmap into the window */
    RenderShape(display, window, bulletPixmap, bulletMask,
		x - BULLET_WC, y - BULLET_HC, BULLET_WIDTH, BULLET_HEIGHT, False);
}

#if NeedFunctionPrototypes
static void DrawBullet(Display *display, Window window, int i)
#else
static void DrawBullet(display, window, i)
	Display *display;
	Window window;
	int i;
#endif
{
	/* Clear the window of the bullet in the old position */
	XClearArea(display, window, bullets[i].xpos - BULLET_WC, 
		bullets[i].oldypos - BULLET_HC, BULLET_WIDTH, BULLET_HEIGHT, False);

	/* Now draw the new bullet in the new position */
	DrawTheBullet(display, window, bullets[i].xpos, bullets[i].ypos);
}

#if NeedFunctionPrototypes
static int CheckBallBulletCollision(Display *display, Window window, 
	int bx, int by, int j)
#else
static int CheckBallBulletCollision(display, window, bx, by, j)
	Display *display;
	Window window;
	int bx;
	int by;
	int j;
#endif
{
	int ballX, ballY;

	GetBallPosition(&ballX, &ballY, j);

    /* Check if any part of the bullets coords is inside the balls box */
    if (((bx + BULLET_WC) >= (ballX - BALL_WC)) &&
   	    ((bx - BULLET_WC) <= (ballX + BALL_WC)) &&
        ((by + BULLET_HC) >= (ballY - BALL_HC)) &&
        ((by - BULLET_HC) <= (ballY + BALL_HC)))
		return True;
	else
		return False;
}

#if NeedFunctionPrototypes
static int CheckForBulletCollision(Display *display, Window window, 
	int x, int y)
#else
static int CheckForBulletCollision(display, window, x, y)
	Display *display;
	Window window;
	int x;
	int y;
#endif
{
    /* Check for bullet to block collision */
    int row, col;
    struct aBlock *blockP;

    /* Get the row and col for block where bullet is */
    X2COL(col, x);
    Y2ROW(row, y);

    blockP = &blocks[row][col];

    /* If blocks is occupied then check for collision */
    if (blockP->occupied == 1 && blockP->exploding == False)
    {
        /* Check if x adjusted for bullet width is in block region */
        if (((x + BULLET_WC) > blockP->x) &&
          ((x - BULLET_WC) < (blockP->x + blockP->width)))
        {
            if (((y + BULLET_HC) > blockP->y) &&
              ((y - BULLET_HC) < (blockP->y + blockP->height)))
            {
                /* Collision */
                return True;
            }
        }
    }

    /* No collision if reached here */
    return False;
}

#if NeedFunctionPrototypes
static int ResetBulletStart(Display *display, Window window)
#else
static int ResetBulletStart(display, window)
	Display *display;
	Window window;
#endif
{
	/* Start a bullet on the way if possible */
	return StartABullet(display, window, paddlePos);
}

#if NeedFunctionPrototypes
void HandleBulletMode(Display *display, Window window)
#else
void HandleBulletMode(display, window)
	Display *display;
	Window window;
#endif
{
	/* Update all the bullets that may be moving */
	if ((frame % BULLET_FRAME_RATE) == 0)
		UpdateBullet(display, window);

	/* Clear any tinks that are due to be cleared */
	CheckTinks(display, window);
}

