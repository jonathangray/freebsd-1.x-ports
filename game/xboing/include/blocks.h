#ifndef _BLOCKS_H_
#define _BLOCKS_H_

#include "copyright.h"

/*
 *  Dependencies on other include files:
 */

#include <X11/Xlib.h>

/*
 *  Constants and macros:
 */

#define NONE_BLK		-2
#define KILL_BLK		-1
#define RED_BLK			0
#define BLUE_BLK		1
#define GREEN_BLK		2
#define TAN_BLK			3
#define YELLOW_BLK		4
#define PURPLE_BLK		5
#define BULLET_BLK		6
#define BLACK_BLK		7
#define COUNTER_BLK		8
#define BONUSX2_BLK		9
#define BONUSX4_BLK		10
#define BONUS_BLK		11
#define BOMB_BLK		12
#define DEATH_BLK		13
#define RANDOM_BLK		14
#define REVERSE_BLK		15
#define HYPERSPACE_BLK	16
#define EXTRABALL_BLK	17
#define MGUN_BLK		18
#define WALLOFF_BLK		19
#define MULTIBALL_BLK	20
#define STICKY_BLK		21
#define PAD_SHRINK_BLK	22
#define PAD_EXPAND_BLK	23
#define DROP_BLK		24
#define MAXAMMO_BLK		25
#define BLACKHIT_BLK	26
#define ROAMER_BLK		27
#define TIMER_BLK		28

#define MAX_ROW			18	
#define MAX_COL			9	

#define BLOCK_WIDTH		40
#define BLOCK_HEIGHT	20

#define SPACE			7

#define MAX_NUM_LEVELS  55

#define REGION_NONE		0
#define REGION_TOP		1
#define REGION_BOTTOM	2
#define REGION_LEFT		4
#define REGION_RIGHT	8

/*
 *  Type declarations:
 */

struct aBlock
{
	int     	occupied;
	int     	exploding;
	int     	specialPopup;
	int     	currentFrame;
	int     	nextFrame;
	int     	lastFrame;
	int			blockOffsetX;
	int			blockOffsetY;
	Region		regionTop;
	Region		regionBottom;
	Region		regionLeft;
	Region		regionRight;
	int			x;
	int			y;
	int         width;
	int         height;
	int         blockType;
	int 		hitPoints;
	int 		explodeStartFrame;
	int 		explodeNextFrame;
	int 		explodeSlide;
	int 		counterSlide;		/* For counter blocks only */
	int 		bonusSlide;			/* For bonus blocks only */
	int			random;
	int			drop;

	/* Used for splitting of the ball in multiball mode */
	int 		ballHitIndex;
	int			balldx;
	int			balldy;
};

typedef struct aBlock **BLOCKPTR;

/*
 *  Function prototypes:
 */

#if NeedFunctionPrototypes
void FreeBlockPixmaps(Display *display);
void InitialiseBlocks(Display *display, Window window, Colormap colormap);
void DrawBlock(Display *display, Window window, int row, int col, 
	int blockType);
void SetupStage(Display *display, Window window);
void ExplodeBlocksPending(Display *display, Window window);
void RedrawAllBlocks(Display *display, Window window);
void DrawTheBlock(Display *display, Window window, int x, int y, 
	int blockType, int slide, int r, int c);
void ExplodeBlockType(Display *display, Window window, int x, int y,
	int row, int col, int type, int slide);
void AddNewBlock(Display *display, Window window, int row, int col,
	int blockType, int counterSlide);
void HandlePendingAnimations(Display *display, Window window);
void AddBonusBlock(Display *display, Window window, int *row, int *col,
	int type);
void ClearBlockArray(void);
int StillActiveBlocks(void);
void SkipToNextLevel(Display *display, Window window);
void PlaySoundForBlock(int type);
void AddSpecialBlock(Display *display, Window window, int *row, int *col,
	int type, int kill_shots);
void HandlePendingSpecials(Display *display, Window window, int type,
	int r, int c);
int GetRandomType(int blankBlock);
#else
int GetRandomType();
void HandlePendingSpecials();
void AddSpecialBlock();
void PlaySoundForBlock();
void FreeBlockPixmaps();
void InitialiseBlocks();
void DrawBlock();
void SetupStage();
void ExplodeBlocksPending();
void RedrawAllBlocks();
void DrawTheBlock();
void ExplodeBlockType();
void AddNewBlock();
void HandlePendingAnimations();
void AddBonusBlock();
void ClearBlockArray();
int StillActiveBlocks();
void SkipToNextLevel();
#endif

extern struct aBlock blocks[MAX_ROW][MAX_COL];
extern int rowHeight;
extern int colWidth;
extern int blocksExploding;
extern Pixmap exyellowblock[3], exyellowblockM[3];


#endif
