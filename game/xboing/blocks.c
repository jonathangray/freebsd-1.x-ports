#include "include/copyright.h"

/*
 *  Include file dependencies:
 */

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <xpm.h>

#include "bitmaps/blueblock.xpm"
#include "bitmaps/exblueblock1.xpm"
#include "bitmaps/exblueblock2.xpm"
#include "bitmaps/exblueblock3.xpm"

#include "bitmaps/redblock.xpm"
#include "bitmaps/exredblock1.xpm"
#include "bitmaps/exredblock2.xpm"
#include "bitmaps/exredblock3.xpm"

#include "bitmaps/yellowblock.xpm"
#include "bitmaps/exyellowblock1.xpm"
#include "bitmaps/exyellowblock2.xpm"
#include "bitmaps/exyellowblock3.xpm"

#include "bitmaps/greenblock.xpm"
#include "bitmaps/exgreenblock1.xpm"
#include "bitmaps/exgreenblock2.xpm"
#include "bitmaps/exgreenblock3.xpm"

#include "bitmaps/tanblock.xpm"
#include "bitmaps/extanblock1.xpm"
#include "bitmaps/extanblock2.xpm"
#include "bitmaps/extanblock3.xpm"

#include "bitmaps/purpleblock.xpm"
#include "bitmaps/expurpleblock1.xpm"
#include "bitmaps/expurpleblock2.xpm"
#include "bitmaps/expurpleblock3.xpm"

#include "bitmaps/bombblock.xpm"
#include "bitmaps/exbombblock1.xpm"
#include "bitmaps/exbombblock2.xpm"
#include "bitmaps/exbombblock3.xpm"

#include "bitmaps/counterblock1.xpm"
#include "bitmaps/counterblock2.xpm"
#include "bitmaps/counterblock3.xpm"
#include "bitmaps/counterblock4.xpm"
#include "bitmaps/counterblock5.xpm"
#include "bitmaps/counterblock.xpm"
#include "bitmaps/excounterblock1.xpm"
#include "bitmaps/excounterblock2.xpm"
#include "bitmaps/excounterblock3.xpm"

#include "bitmaps/blackblock.xpm"
#include "bitmaps/blackhitblock.xpm"
#include "bitmaps/reverse.xpm"
#include "bitmaps/hyperspace.xpm"
#include "bitmaps/machinegun.xpm"
#include "bitmaps/walloff.xpm"
#include "bitmaps/multiballblock.xpm"
#include "bitmaps/stickyblock.xpm"
#include "bitmaps/paddleshrink.xpm"
#include "bitmaps/paddleexpand.xpm"
#include "bitmaps/unlimitammo.xpm"
#include "bitmaps/clock.xpm"

#include "bitmaps/roamer.xpm"
#include "bitmaps/roamerL.xpm"
#include "bitmaps/roamerR.xpm"
#include "bitmaps/roamerU.xpm"
#include "bitmaps/roamerD.xpm"

#include "bitmaps/xtraball.xpm"
#include "bitmaps/xtraball2.xpm"

#include "bitmaps/x2bonus1.xpm"
#include "bitmaps/x2bonus2.xpm"
#include "bitmaps/x2bonus3.xpm"
#include "bitmaps/x2bonus4.xpm"

#include "bitmaps/x4bonus1.xpm"
#include "bitmaps/x4bonus2.xpm"
#include "bitmaps/x4bonus3.xpm"
#include "bitmaps/x4bonus4.xpm"

#include "bitmaps/bonus1.xpm"
#include "bitmaps/bonus2.xpm"
#include "bitmaps/bonus3.xpm"
#include "bitmaps/bonus4.xpm"

#include "bitmaps/exx2bonus1.xpm"
#include "bitmaps/exx2bonus2.xpm"
#include "bitmaps/exx2bonus3.xpm"

#include "bitmaps/death1.xpm"
#include "bitmaps/death2.xpm"
#include "bitmaps/death3.xpm"
#include "bitmaps/death4.xpm"
#include "bitmaps/death5.xpm"

#include "bitmaps/exdeath1.xpm"
#include "bitmaps/exdeath2.xpm"
#include "bitmaps/exdeath3.xpm"
#include "bitmaps/exdeath4.xpm"

#include "include/audio.h"
#include "include/misc.h"
#include "include/mess.h"
#include "include/gun.h"
#include "include/error.h"
#include "include/bonus.h"
#include "include/special.h"
#include "include/main.h"
#include "include/init.h"
#include "include/stage.h"
#include "include/ball.h"
#include "include/score.h"
#include "include/paddle.h"
#include "include/level.h"
#include "include/sfx.h"

#include "include/blocks.h"

/*
 *  Internal macro definitions:
 */

#define EXPLODE_DELAY				10	
#define BONUS_DELAY					150	
#define BONUS_LENGTH				1500
#define NUMBER_OF_BULLETS_NEW_LEVEL 4
#define DEATH_DELAY1				100
#define DEATH_DELAY2				700
#define EXTRABALL_DELAY				300
#define RANDOM_DELAY				500
#define DROP_DELAY					1000
#define INFINITE_DELAY				9999999
#define ROAM_EYES_DELAY				300
#define ROAM_DELAY					1000
#define EXTRA_TIME					20

#define X2COL(col, x) (col = x / colWidth)
#define Y2ROW(row, y) (row = y / rowHeight)

/*
 *  Internal type declarations:
 */

#if NeedFunctionPrototypes
static void ClearBlock(int row, int col);
static void CalculateBlockGeometry(int row, int col);
static void SetBlockUpForExplosion(int row, int col, int frame);
#else
static void SetBlockUpForExplosion();
static void ClearBlock();
static void CalculateBlockGeometry();
#endif

/*
 *  Internal variable declarations:
 */

static Pixmap	exredblock[3],		exredblockM[3];
static Pixmap	extanblock[3],		extanblockM[3];
Pixmap	exyellowblock[3],	exyellowblockM[3];
static Pixmap	exgreenblock[3],	exgreenblockM[3];
static Pixmap	exblueblock[3],		exblueblockM[3];
static Pixmap	expurpleblock[3],	expurpleblockM[3];
static Pixmap	exbombblock[3],		exbombblockM[3];
static Pixmap	excounterblock[3],	excounterblockM[3];
static Pixmap	exx2bonus[3],		exx2bonusM[3];
static Pixmap	x2bonus[4],			x2bonusM[4];
static Pixmap	x4bonus[4],			x4bonusM[4];
static Pixmap	Bonus[4],			BonusM[4];
static Pixmap	death[5],			deathM[5];
static Pixmap	exdeath[5],			exdeathM[5];
static Pixmap	counterblock[6],	counterblockM[6];
static Pixmap	extraball[3],		extraballM[3];
static Pixmap	roamer[5],			roamerM[5];

static Pixmap	redblock, greenblock, blueblock, yellowblock, purpleblock;	
static Pixmap	tanblock, blackblock, bombblock, revblock, 	hyperblock;
static Pixmap	mgunblock, walloffblock, multiball, sticky, paddleshrink;
static Pixmap	paddleexpand, unlimitammo, blackhit, timeblock;

static Pixmap	redblockM, greenblockM, blueblockM, yellowblockM, purpleblockM;	
static Pixmap	tanblockM, blackblockM, bombblockM, revblockM, 	hyperblockM;
static Pixmap	mgunblockM, walloffblockM, multiballM, stickyM, paddleshrinkM;
static Pixmap	paddleexpandM, unlimitammoM, blackhitM, timeblockM;

struct aBlock 	blocks[MAX_ROW][MAX_COL];
int			blocksExploding = 0;
int			rowHeight;
int			colWidth;

#if NeedFunctionPrototypes
void InitialiseBlocks(Display *display, Window window, Colormap colormap)
#else
void InitialiseBlocks(display, window, colormap)
	Display *display;
	Window window;
	Colormap colormap;
#endif
{
	XpmAttributes   attributes;
	int 			XpmErrorStatus;

	attributes.valuemask = XpmColormap;
	attributes.colormap = colormap;

	/* Create all xpm pixmap blocks from the files */
	XpmErrorStatus = XpmCreatePixmapFromData(display, window, redblock_xpm, 
		&redblock, &redblockM, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(red)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, blueblock_xpm, 
		&blueblock, &blueblockM, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(blue)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, greenblock_xpm, 
		&greenblock, &greenblockM, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(green)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, yellowblock_xpm, 
		&yellowblock, &yellowblockM, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(yellow)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, tanblock_xpm, 
		&tanblock, &tanblockM, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(tan)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, purpleblock_xpm, 
		&purpleblock, &purpleblockM, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(purple)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, blackblock_xpm, 
		&blackblock, &blackblockM, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(wall)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		blackhitblock_xpm, &blackhit, &blackhitM, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(wallhit)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, bombblock_xpm, 
		&bombblock, &bombblockM, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(bomb)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, reverse_xpm, 
		&revblock, &revblockM, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(reverse)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, hyperspace_xpm, 
		&hyperblock, &hyperblockM, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(hyperspace)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, machinegun_xpm, 
		&mgunblock, &mgunblockM, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(machinegun)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, walloff_xpm, 
		&walloffblock, &walloffblockM, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(walloff)");

	/* Explosion for yellow block */

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		exyellowblock1_xpm, &exyellowblock[0], &exyellowblockM[0], 
		&attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(exyellow)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		exyellowblock2_xpm, &exyellowblock[1], &exyellowblockM[1], 
		&attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(exyellow)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		exyellowblock3_xpm, &exyellowblock[2], &exyellowblockM[2], 
		&attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(exyellow)");

	/* Explosion for red block */

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, exredblock1_xpm, 
		&exredblock[0], &exredblockM[0], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(exred)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, exredblock2_xpm, 
		&exredblock[1], &exredblockM[1], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(exred)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, exredblock3_xpm, 
		&exredblock[2], &exredblockM[2], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(exred)");

	/* Explosion for green block */

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		exgreenblock1_xpm, &exgreenblock[0], &exgreenblockM[0], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(exgreen)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		exgreenblock2_xpm, &exgreenblock[1], &exgreenblockM[1], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(exgreen)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		exgreenblock3_xpm, &exgreenblock[2], &exgreenblockM[2], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(exgreen)");

	/* Explosion for blue block */

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		exblueblock1_xpm, &exblueblock[0], &exblueblockM[0], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(exblue)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		exblueblock2_xpm, &exblueblock[1], &exblueblockM[1], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(exblue)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		exblueblock3_xpm, &exblueblock[2], &exblueblockM[2], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(exblue)");

	/* Explosion for tan block */

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		extanblock1_xpm, &extanblock[0], &extanblockM[0], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(extan)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		extanblock2_xpm, &extanblock[1], &extanblockM[1], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(extan)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		extanblock3_xpm, &extanblock[2], &extanblockM[2], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(extan)");

	/* Explosion for purple block */

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		expurpleblock1_xpm, &expurpleblock[0], &expurpleblockM[0], 
		&attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(expurple)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		expurpleblock2_xpm, &expurpleblock[1], &expurpleblockM[1], 
		&attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(expurple)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		expurpleblock3_xpm, &expurpleblock[2], &expurpleblockM[2], 
		&attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(expurple)");

	/* Explosion for bomb block */

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		exbombblock1_xpm, &exbombblock[0], &exbombblockM[0], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(exbomb)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		exbombblock2_xpm, &exbombblock[1], &exbombblockM[1], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(exbomb)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		exbombblock3_xpm, &exbombblock[2], &exbombblockM[2], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(exbomb)");

	/* Explosion for counter block */

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		excounterblock1_xpm, &excounterblock[0], &excounterblockM[0], 
		&attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(excounter)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		excounterblock2_xpm, &excounterblock[1], &excounterblockM[1], 
		&attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(excounter)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		excounterblock3_xpm, &excounterblock[2], &excounterblockM[2], 
		&attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(excounter)");

	/* countdown for counter block */

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		counterblock_xpm, &counterblock[0], &counterblockM[0], 
		&attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(counter0)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		counterblock1_xpm, &counterblock[1], &counterblockM[1], 
		&attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(counter1)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		counterblock2_xpm, &counterblock[2], &counterblockM[2], 
		&attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(counter2)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		counterblock3_xpm, &counterblock[3], &counterblockM[3], 
		&attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(counter3)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		counterblock4_xpm, &counterblock[4], &counterblockM[4], 
		&attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(counter4)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		counterblock5_xpm, &counterblock[5], &counterblockM[5], 
		&attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(counter5)");

	/* Bonus block stuff */

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		x2bonus1_xpm, &x2bonus[0], &x2bonusM[0], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(x2bonus1)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		x2bonus2_xpm, &x2bonus[1], &x2bonusM[1], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(x2bonus2)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		x2bonus3_xpm, &x2bonus[2], &x2bonusM[2], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(x2bonus3)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		x2bonus4_xpm, &x2bonus[3], &x2bonusM[3], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(x2bonus4)");


	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		x4bonus1_xpm, &x4bonus[0], &x4bonusM[0], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(x4bonus1)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		x4bonus2_xpm, &x4bonus[1], &x4bonusM[1], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(x4bonus2)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		x4bonus3_xpm, &x4bonus[2], &x4bonusM[2], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(x4bonus3)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		x4bonus4_xpm, &x4bonus[3], &x4bonusM[3], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(x4bonus4)");


	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		bonus1_xpm, &Bonus[0], &BonusM[0], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(bonus1)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		bonus2_xpm, &Bonus[1], &BonusM[1], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(bonus2)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		bonus3_xpm, &Bonus[2], &BonusM[2], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(bonus3)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		bonus4_xpm, &Bonus[3], &BonusM[3], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(bonus4)");


	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		exx2bonus1_xpm, &exx2bonus[0], &exx2bonusM[0], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(exbonus)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		exx2bonus2_xpm, &exx2bonus[1], &exx2bonusM[1], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(exbonus)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		exx2bonus3_xpm, &exx2bonus[2], &exx2bonusM[2], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(exbonus)");

	/* Death block initialisation */

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		death1_xpm, &death[0], &deathM[0], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(death)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		death2_xpm, &death[1], &deathM[1], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(death)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		death3_xpm, &death[2], &deathM[2], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(death)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		death4_xpm, &death[3], &deathM[3], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(death)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		death5_xpm, &death[4], &deathM[4], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(death)");

	/* Death block explosion init */

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		exdeath1_xpm, &exdeath[0], &exdeathM[0], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(exdeath)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		exdeath2_xpm, &exdeath[1], &exdeathM[1], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(exdeath)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		exdeath3_xpm, &exdeath[2], &exdeathM[2], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(exdeath)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		exdeath4_xpm, &exdeath[3], &exdeathM[3], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(exdeath)");

	/* Extra balll pixmaps */

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		xtraball_xpm, &extraball[0], &extraballM[0], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(extraball)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		xtraball2_xpm, &extraball[1], &extraballM[1], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(extraball)");

	/* Multiple ball pixmap */

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		multiballblock_xpm, &multiball, &multiballM, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(multiball)");

	/* Sticky block */

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		stickyblock_xpm, &sticky, &stickyM, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(sticky)");

	/* Unlimited ammo block symbol */

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		unlimitammo_xpm, &unlimitammo, &unlimitammoM, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(unlimitammo)");

	/* paddle shrink and expand block */

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		paddleshrink_xpm, &paddleshrink, &paddleshrinkM, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(paddleshrink)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		paddleexpand_xpm, &paddleexpand, &paddleexpandM, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(paddleexpand)");

	/* Frames for the roaming block */

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		roamer_xpm, &roamer[0], &roamerM[0], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(roamer)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		roamerL_xpm, &roamer[1], &roamerM[1], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(roamer1)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		roamerR_xpm, &roamer[2], &roamerM[2], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(roamer2)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		roamerU_xpm, &roamer[3], &roamerM[3], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(roamer3)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		roamerD_xpm, &roamer[4], &roamerM[4], &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(roamer4)");

	/* Clock pixmap */

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		clock_xpm, &timeblock, &timeblockM, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseBlocks(timeblock)");

	/* Free the xpm pixmap attributes */
	XpmFreeAttributes(&attributes);
}

#if NeedFunctionPrototypes
void PlaySoundForBlock(int type)
#else
void PlaySoundForBlock(type)
	int type;
#endif
{
	/* If no sound the no sound */
	if (noSound == True) return;

	/* Play the sound effect for the block being hit */
	switch (type)
	{
		case BOMB_BLK:		
			playSoundFile("bomb", 50);
			break;

		case BULLET_BLK:
			playSoundFile("ammo", 30);
			break;

		case MAXAMMO_BLK:
			playSoundFile("ammo", 70);
			break;

		case RED_BLK:
		case GREEN_BLK:		
		case BLUE_BLK:	
		case TAN_BLK:
		case PURPLE_BLK:	
		case YELLOW_BLK:
		case COUNTER_BLK:
		case RANDOM_BLK:
		case DROP_BLK:
			playSoundFile("touch", 100);
			break;

		case ROAMER_BLK:
			playSoundFile("ouch", 100);
			break;

		case EXTRABALL_BLK:	
			playSoundFile("ddloo", 100);
			break;

		case MGUN_BLK:	
			playSoundFile("mgun", 100);
			break;

		case WALLOFF_BLK:	
			playSoundFile("wallsoff", 100);
			break;

		case BONUSX2_BLK:	
		case BONUSX4_BLK:	
		case BONUS_BLK:	
		case REVERSE_BLK:	
		case MULTIBALL_BLK:
		case PAD_SHRINK_BLK:
		case PAD_EXPAND_BLK:
		case TIMER_BLK:
			playSoundFile("bonus", 50);
			break;

		case STICKY_BLK:
			playSoundFile("sticky", 90);
			break;

		case DEATH_BLK:	
			playSoundFile("evillaugh", 100);
			break;

		case BLACK_BLK:		
			playSoundFile("metal", 100);
			break;

		case HYPERSPACE_BLK:
			playSoundFile("hyperspace", 70);
			break;

		case KILL_BLK:
			ErrorMessage("Error: kill block type in PlaySoundForBlock()");
			break;

		default:
			ErrorMessage("Error: Unknown block type in PlaySoundForBlock()");
			break;
	}
}

#if NeedFunctionPrototypes
void ExplodeBlockType(Display *display, Window window, int x, int y, 
	int row, int col, int type, int slide)
#else
void ExplodeBlockType(display, window, x, y, row, col, type, slide)
	Display *display;
	Window window;
	int x;
	int y; 
	int row;
	int col;
	int type;
	int slide;
#endif
{
    struct aBlock *blockP;
	int y1, y2, h;

	/* Draw a frame from the blocks explode animation */
	switch (type)
	{
		case BOMB_BLK:		/* Draw the bomb block explosion slide */
			RenderShape(display, window, exbombblock[slide], 
				exbombblockM[slide], x, y, 30, 30, True);
			break;

		case MULTIBALL_BLK:
		case RED_BLK:		/* Draw a red block explosion slide */
			RenderShape(display, window, exredblock[slide], 
				exredblockM[slide], x, y, 40, 20, True);
			break;

		case GREEN_BLK:		/* Draw a green block explosion slide */
		case DROP_BLK:		/* Draw a drop block explosion slide */
			RenderShape(display, window, exgreenblock[slide], 
				exgreenblockM[slide], x, y, 40, 20, True);
			break;

		case BLUE_BLK:		/* Draw a blue block explosion slide */
			RenderShape(display, window, exblueblock[slide], 
				exblueblockM[slide], x, y, 40, 20, True);
			break;

		case TAN_BLK:		/* Draw a tan block explosion slide */
			RenderShape(display, window, extanblock[slide], 
				extanblockM[slide], x, y, 40, 20, True);
			break;

		case PURPLE_BLK:	/* Draw a purple block explosion slide */
			RenderShape(display, window, expurpleblock[slide], 
				expurpleblockM[slide], x, y, 40, 20, True);
			break;

		case BULLET_BLK:	/* Draw a bullet block explosion slide */
		case YELLOW_BLK:	/* Draw a yellow block explosion slide */
			RenderShape(display, window, exyellowblock[slide], 
				exyellowblockM[slide], x, y, 40, 20, True);
			break;

		case COUNTER_BLK:	/* Draw a counter block explosion slide */
			RenderShape(display, window, excounterblock[slide], 
				excounterblockM[slide], x, y, 40, 20, True);
			break;

		case BONUS_BLK:		/* Draw a bonus coin block explosion slide */
		case BONUSX4_BLK:	/* Draw a bonus x2 oin block explosion slide */
		case BONUSX2_BLK:	/* Draw a bonus x4 coin block explosion slide */
		case TIMER_BLK:		/* Draw a timer clock block explosion slide */
			RenderShape(display, window, exx2bonus[slide], 
				exx2bonusM[slide], x, y, 27, 27, True);
			break;

		case DEATH_BLK:		/* Draw a pirate death block explosion slide */
			RenderShape(display, window, exdeath[slide], 
				exdeathM[slide], x, y, 30, 30, True);
			break;

		case BLACK_BLK:		
			break;

		case REVERSE_BLK:	/* The reverse doesn't have any animation */
		case HYPERSPACE_BLK:/* The hyperspace doesn't have any animation */
		case EXTRABALL_BLK:	/* The extra ball doesn't have any animation */
		case MGUN_BLK:		/* The machine gun doesn't have any animation */
		case WALLOFF_BLK:	/* The wall off doesn't have any animation */
		case STICKY_BLK:	/* No animation for sticky block either */
		case PAD_SHRINK_BLK:
		case PAD_EXPAND_BLK:
		case MAXAMMO_BLK:
		case ROAMER_BLK:
    		blockP = &blocks[row][col];
			y1 = blockP->y;
			y2 = blockP->y + blockP->height;

			switch (slide)
			{
				case 0:	 /* First frame to clear */
					h = (blockP->height / 2) / 3;
        			XClearArea(display, window, 
						blockP->x, y1, blockP->width, h, False);
        			XClearArea(display, window, 
						blockP->x, y2-h, blockP->width, h, False);
					break;

				case 1:	 /* Second frame to clear */
					h = (blockP->height / 2) / 2;
        			XClearArea(display, window, 
						blockP->x, y1, blockP->width, h, False);
        			XClearArea(display, window, 
						blockP->x, y2-h, blockP->width, h, False);
					break;

				case 2:	 /* thrid frame to clear */
					h = (int) ((blockP->height / 2) / 1.5);
        			XClearArea(display, window, 
						blockP->x, y1, blockP->width, h, False);
        			XClearArea(display, window, 
						blockP->x, y2-h, blockP->width, h, False);
					break;
			}
			break;

		default:
			ErrorMessage("Error: Unknown block type in ExplodeBlockType()");
			break;
	}
}

#if NeedFunctionPrototypes
void AddSpecialBlock(Display *display, Window window, int *row, int *col,
    int type, int kill_shots)
#else
void AddSpecialBlock(display, window, row, col, type, kill_shots)
    Display *display;
    Window window;
    int *row;
    int *col;
    int type;
    int kill_shots;
#endif
{
    int r, c;
    struct aBlock *blockP;

    /* Give me a new random block position */
    r = rand() % (MAX_ROW - 6);
    c = rand() % MAX_COL;

    /* Pointer to the correct block we need - speed things up */
    blockP = &blocks[r][c];

    /* Add a special in this block only if it isn't occupied and
     * it isn't exploding.
     */
    if ((blockP->occupied == False) && (blockP->explodeStartFrame == 0))
    {
        AddNewBlock(display, window, r, c, type, kill_shots);
		bonusBlock = True;

        /* Setup the block structure for new block */
        blockP->nextFrame       = frame + 1;
        blockP->lastFrame       = frame + BONUS_LENGTH;
        blockP->bonusSlide      = 0;
        blockP->specialPopup    = True;

        /* Return the new specials row & coloumn position */
        *row = r; *col = c;
    }
}


#if NeedFunctionPrototypes
void AddBonusBlock(Display *display, Window window, int *row, int *col, 
	int type)
#else
void AddBonusBlock(display, window, row, col, type)
	Display *display;
	Window window;
	int *row;
	int *col;
	int type;
#endif
{
	int r, c;
	struct aBlock *blockP;

	/* Give me a new random block position */
	r = rand() % (MAX_ROW - 6);
	c = rand() % MAX_COL;

	/* Pointer to the correct block we need - speed things up */
	blockP = &blocks[r][c];

	/* Add a bonus coin in this block only if it isn't occupied and 
	 * it isn't exploding.
	 */
	if ((blockP->occupied == False) && (blockP->explodeStartFrame == 0))
	{
		AddNewBlock(display, window, r, c, type, 0);
		bonusBlock = True;

		/* Setup the block structure for new block */
		blockP->nextFrame 		= frame + BONUS_DELAY;
		blockP->lastFrame 		= frame + BONUS_LENGTH;
		blockP->bonusSlide 		= 3;
        blockP->specialPopup    = True;

		/* Return the new bonus row & coloumn position */
		*row = r; *col = c;
	}
}

#if NeedFunctionPrototypes
int GetRandomType(int blankBlock)
#else
int GetRandomType(blankBlock)
	int blankBlock;
#endif
{
	/* This function will return a random new block block type */
	switch (rand() % 8)
	{
		case 0:
			return RED_BLK;

		case 1:
			return BLUE_BLK;

		case 2:
			return GREEN_BLK;

		case 3:
			return TAN_BLK;

		case 4:
			return YELLOW_BLK;

		case 5:
			return PURPLE_BLK;

		case 6:
			return BULLET_BLK;

		case 7:
			if (blankBlock == True)
				return NONE_BLK;
			else
				return YELLOW_BLK;
	}

	/* Shouldn't get here but it stops warnings on compiler */
	return YELLOW_BLK;
}

#if NeedFunctionPrototypes
void HandlePendingSpecials(Display *display, Window window, int type,
    int r, int c)
#else
void HandlePendingSpecials(display, window, type, r, c)
    Display *display;
    Window window;
    int type;
    int r, c;
#endif
{
    struct aBlock *blockP;

    blockP = &blocks[r][c];

    if (frame >= blockP->lastFrame)
    {
    	/* Kill off special block */
		bonusBlock = False;
        XClearArea(display, window,
            blockP->x, blockP->y,
            blockP->width, blockP->height,
            False);
		ClearBlock(r, c);
		DEBUG("Clearing special random block.")
    }
}


#if NeedFunctionPrototypes
void HandlePendingBonuses(Display *display, Window window, int type, 
	int r, int c)
#else
void HandlePendingBonuses(display, window, type, r, c)
	Display *display;
	Window window;
	int type;
	int r, c;
#endif
{
	struct aBlock *blockP;

	blockP = &blocks[r][c];

	if (blockP->nextFrame == frame) 
	{
		if (frame <= blockP->lastFrame)
		{
			DEBUG("turning bonus coin block.")

			/* Advance to the next frame of animation */
			DrawTheBlock(display, window, 
				blockP->x, blockP->y, 
				type, blockP->bonusSlide, r, c);

				blockP->nextFrame = frame + BONUS_DELAY;
				blockP->bonusSlide--;

				if (blockP->bonusSlide < 0)
					blockP->bonusSlide = 3;
		}
		else
		{
			/* Kill off bonus block */
			bonusBlock = False;
			XClearArea(display, window, 
				blockP->x, blockP->y,
				blockP->width, blockP->height, 
				False);
			ClearBlock(r, c);
			DEBUG("Clearing bonus block.")
		}
	}
}

#if NeedFunctionPrototypes
static int CheckAdjacentBlocks(Display *display, Window window, int r, int c)
#else
static int CheckAdjacentBlocks(display, window, r, c)
	Display *display;
	Window window;
	int r;
	int c;
#endif
{
	/* True - yes go ahead moved down one block - false no don't */
	struct aBlock *blockP;
	int row, col, i;

	if (r < 0 || r >= MAX_ROW) return False;
	if (c < 0 || c >= MAX_COL) return False;

	blockP = &blocks[r][c];

	if ((blockP->occupied == True) || (blockP->explodeStartFrame != 0))
		return False;

	if ((r+1) >= (MAX_ROW - 2)) return False;

    /* Loop through all the balls */
	for (i = 0; i < MAX_BALLS; i++)
	{
		/* Only handle active balls - sounds disgusting! :-) */
		if (balls[i].active == True)
		{
			X2COL(col, balls[i].ballx);
			Y2ROW(row, balls[i].bally);

			/* Is the ball in the way of the new block? */
			if ((row == r) && (col == c)) return False;
		}
	}

	/* Ok to move down one block */
	return True;
}


#if NeedFunctionPrototypes
void HandlePendingAnimations(Display *display, Window window)
#else
void HandlePendingAnimations(display, window)
	Display *display;
	Window window;
#endif
{
	int r, c, d, r1 = 0, c1 = 0;
	struct aBlock *blockP;

	/* Cycle through all rows and columns and update any animations that
	 * need to be done every frame. ie: bonus etc
	 */
	for (r = 0; r < MAX_ROW; r++)
	{
		for (c = 0; c < MAX_COL; c++)
		{
			blockP = &blocks[r][c];

			/* Only bother if the block is occupied! */
			if (blockP->occupied == True) 
			{
				switch (blockP->blockType)
				{
					case PAD_SHRINK_BLK:
						HandlePendingSpecials(display, window, 
							PAD_SHRINK_BLK, r, c);
						break;

					case PAD_EXPAND_BLK:	
						HandlePendingSpecials(display, window, 
							PAD_EXPAND_BLK, r, c);
						break;

					case MULTIBALL_BLK:	
						HandlePendingSpecials(display, window, 
							MULTIBALL_BLK, r, c);
						break;

					case REVERSE_BLK:
						HandlePendingSpecials(display, window, 
							REVERSE_BLK, r, c);
						break;

					case MGUN_BLK:	
						HandlePendingSpecials(display, window, 
							MGUN_BLK, r, c);
						break;

					case WALLOFF_BLK:	
						HandlePendingSpecials(display, window, 
							WALLOFF_BLK, r, c);
						break;

					case BONUS_BLK:	/* bonus coin symbol */
						HandlePendingBonuses(display, window, BONUS_BLK, r, c);
						break;

					case BONUSX2_BLK:	/* Bonus x2 coin symbol */
						HandlePendingBonuses(display, window, BONUSX2_BLK, r,c);
						break;

					case BONUSX4_BLK:	/* Bonus x4 coin symbol */
						HandlePendingBonuses(display, window, BONUSX4_BLK, r,c);
						break;

					case DEATH_BLK:	/* Death block animation */
						if (blockP->nextFrame == frame) 
						{
							/* Advance to the next frame of animation */
							DrawTheBlock(display, window, 
								blockP->x, blockP->y, 
								DEATH_BLK, blockP->bonusSlide, r, c);

							blockP->nextFrame = frame + DEATH_DELAY1;
							blockP->bonusSlide++;

							/* Have the delay bit between winks */
							if (blockP->bonusSlide > 4)
							{
								blockP->bonusSlide = 0;
								blockP->nextFrame = frame + DEATH_DELAY2;
								DrawTheBlock(display, window, 
									blockP->x, blockP->y, 
									DEATH_BLK, blockP->bonusSlide, r, c);
							}
						}

						HandlePendingSpecials(display, window, DEATH_BLK, r, c);
						break;

					case EXTRABALL_BLK:	/* extra ball block animation */
						if (blockP->nextFrame == frame) 
						{
							/* Advance to the next frame of animation */
							DrawTheBlock(display, window, 
								blockP->x, blockP->y, 
								EXTRABALL_BLK, blockP->bonusSlide, r, c);

							blockP->nextFrame = frame + EXTRABALL_DELAY;
							blockP->bonusSlide++;

							/* Have the delay bit between flashes */
							if (blockP->bonusSlide > 1)
								blockP->bonusSlide = 0;
						}

						HandlePendingSpecials(display, window, 
							EXTRABALL_BLK, r, c);
						break;

					case BLACK_BLK:
						if (blockP->nextFrame == frame) 
						{
							/* Clear the red bit in wall block */
							DrawTheBlock(display, window, 
								blockP->x, blockP->y, 
								BLACK_BLK, 0, r, c);

							blockP->nextFrame = frame - 1;
						}
						break;

					case ROAMER_BLK:	/* Roamer block animation */
						if (blockP->nextFrame == frame) 
						{
							/* Advance to the next frame of animation */
							DrawTheBlock(display, window, 
								blockP->x, blockP->y, 
								ROAMER_BLK, blockP->bonusSlide, r, c);

							blockP->nextFrame = frame + 
								(rand() % ROAM_EYES_DELAY) + 50;
							blockP->bonusSlide = rand() % 5;

						} else if (blockP->lastFrame == frame) 
						{
							/* Work out which way to move block if we can */
							d = blockP->bonusSlide + 1; /* 1 - 4 */
							switch (d)
							{
								case 1:
									r1 = 0; c1 = -1;
									break;
								case 2:
									r1 = 0; c1 = 1;
									break;
								case 3:
									r1 = -1; c1 = 0;
									break;
								case 4:
									r1 = 1; c1 = 0;
									break;
							}

							/* check if we can move the roamer to next block */
							if (CheckAdjacentBlocks(display, window, 
								r+r1, c+c1))
							{
								/* Ok add a new block one space down */
								AddNewBlock(display, window, r+r1, c+c1, 
									ROAMER_BLK, 0);
								blockP = &blocks[r+r1][c+c1];
								blockP->nextFrame = frame + 
									(rand() % ROAM_EYES_DELAY) + 50;

								/* Erase the old block */
								blockP = &blocks[r][c];
								XClearArea(display, window, 
									blockP->x, blockP->y,
									blockP->width, blockP->height, False);
								ClearBlock(r, c);
							}
							else
							{
								/* Ok - cannot go so just wait for a while */
								blockP->lastFrame = frame + 
									(rand() % ROAM_DELAY) + 300;
							}
						}
						break;
				}

				/* If it is a random block then change? */
				if (blockP->random == True)
				{
					if (blockP->nextFrame == frame) 
					{
						/* Change the block to a new block block. We should
						 * be allright in just changing the blocktype etc.
						 * as the blocks are all the same .
						 */
						blockP->blockType = GetRandomType(False);
						blockP->bonusSlide = 0;

						DrawTheBlock(display, window, 
							blockP->x, blockP->y, 
							blockP->blockType, blockP->bonusSlide, r, c);

						blockP->nextFrame = 
							frame + (rand() % RANDOM_DELAY) + 300;
					}	
				}	/* random */

				/* Handle the dropping blocks */
				if (blockP->drop == True)
				{
					/* Time to drop down one notch? */
					if (blockP->nextFrame == frame) 
					{
						/* Can the drop block move down */
						if (CheckAdjacentBlocks(display, window, r+1, c))
						{
							/* Ok add a new block one space down */
							AddNewBlock(display, window, r+1, c, 
								DROP_BLK, 0);
    						blockP = &blocks[r+1][c];
							blockP->nextFrame = frame + 
								(rand() % DROP_DELAY) + 200;

							/* Erase the old block */
    						blockP = &blocks[r][c];
							XClearArea(display, window, 
								blockP->x, blockP->y,
								blockP->width, blockP->height, False);
							ClearBlock(r, c);
						}
						else
						{
							/* Ok - cannot go down so just wait for a while */
    						blockP = &blocks[r][c];
							blockP->nextFrame = frame + DROP_DELAY;
						}
					}	
				}	/* droppers */

			}
		}
	}
}

#if NeedFunctionPrototypes
void ExplodeBlocksPending(Display *display, Window window)
#else
void ExplodeBlocksPending(display, window)
	Display *display;
	Window window;
#endif
{
	int r, c, x, y, type, i;
	struct aBlock *blockP;
	char str[50];

	/* If none are exploding then bug out */
	if (blocksExploding == 0) return;

	/* Cycle through all blocks exploding pending animation blocks */
	for (r = 0; r < MAX_ROW; r++)
	{
		for (c = 0; c < MAX_COL; c++)
		{
			/* Get the pointer to the block we need */
			blockP = &blocks[r][c];

			/* Will be non-zero if animation is required */
			if (blockP->explodeStartFrame)
			{
				/* Is it time to explode this frame */
				if (blockP->explodeNextFrame == frame) 
				{
					x = blockP->x;
					y = blockP->y;
					type = blockP->blockType;

					/* Switch on slide of animation */
					switch (blockP->explodeSlide)
					{
						case 1:	/* First frame of animation */
							ExplodeBlockType(display, window, x, y, r, c, 
								type, 0);
							blockP->explodeNextFrame =
								blockP->explodeStartFrame;
							break;
	
						case 2:	/* Second frame of animation */
							ExplodeBlockType(display, window, x, y, r, c, 
								type, 1);
							break;
	
						case 3:	/* Third frame of animation */
							ExplodeBlockType(display, window, x, y, r, c, 
								type, 2);
							break;
	
						case 4:	/* Last frame of animation  - clear */
							XClearArea(display, window, x, y, 
								blockP->width, 
								blockP->height, False);
							break;
					}
	
					/* procede to next frame in animation */
					blockP->explodeSlide++;
					blockP->explodeNextFrame += EXPLODE_DELAY;
	
					/* last frame so clean up animation and block */
					if (blockP->explodeSlide > 4)
					{
						blocksExploding--;
						blockP->occupied = 0;
						blockP->exploding = False;

						AddToScore((u_long) blockP->hitPoints);
						DisplayScore(display, scoreWindow, score);

						switch (blockP->blockType)
						{
							case BLACK_BLK:
							case PAD_SHRINK_BLK:
							case PAD_EXPAND_BLK:
								break;

							case BOMB_BLK:
								/* Explode all the ones around it */
								SetBlockUpForExplosion(r+1, c, 
									frame + EXPLODE_DELAY);
								SetBlockUpForExplosion(r, c+1, 
									frame + EXPLODE_DELAY);
								SetBlockUpForExplosion(r-1, c, 
									frame + EXPLODE_DELAY);
								SetBlockUpForExplosion(r, c-1, 
									frame + EXPLODE_DELAY);
								SetBlockUpForExplosion(r-1, c-1, 
									frame + EXPLODE_DELAY);
								SetBlockUpForExplosion(r-1, c+1, 
									frame + EXPLODE_DELAY);
								SetBlockUpForExplosion(r+1, c-1, 
									frame + EXPLODE_DELAY);
								SetBlockUpForExplosion(r+1, c+1, 
									frame + EXPLODE_DELAY);

								/* Special effect where screen shakes 
								 * during explosion 
								 */
								SetSfxEndFrame(frame + 70);
								changeSfxMode(SFX_SHAKE);
								break;

							case TIMER_BLK:
								AddToLevelTimeBonus(display, timeWindow, 
									EXTRA_TIME);
								SetCurrentMessage(display, messWindow, 
									"- Extra Time = 20 seconds -", True);
								break;

							case BULLET_BLK:
								SetCurrentMessage(display, messWindow, 
									"More ammunition, cool!", True);
								for (i=0; i < NUMBER_OF_BULLETS_NEW_LEVEL; i++)
									AddABullet(display);
								break;

							case MAXAMMO_BLK:
								SetCurrentMessage(display, messWindow, 
									"Unlimited bullets!", True);
								SetUnlimitedBullets(True);
								SetNumberBullets(MAX_BULLETS+1);
								DisplayLevelInfo(display, levelWindow, level);
								break;

							case BONUS_BLK:
								IncNumberBonus();

								if (GetNumberBonus() <= MAX_BONUS)
									sprintf(str, 
										"- Bonus #%d -", GetNumberBonus());
								else
									sprintf(str, "<<< Super Bonus >>>");

								SetCurrentMessage(display, messWindow, str, 
									True);
								bonusBlock = False;

								/* Turn on killer mode after 10 bonuses */
								if (GetNumberBonus() == 10)
								{
									/* Turn on killer mode */
									ToggleKiller(display, True);
									DrawSpecials(display);

									SetCurrentMessage(display, messWindow, 
										"- Killer Mode -", True);
								}
								break;

							case BONUSX2_BLK:
								Togglex2Bonus(display, True);
								Togglex4Bonus(display, False);
								DrawSpecials(display);

								bonusBlock = False;
								SetCurrentMessage(display, messWindow, 
									"- x2 Bonus -", True);
								break;

							case BONUSX4_BLK:
								Togglex2Bonus(display, False);
								Togglex4Bonus(display, True);
								DrawSpecials(display);

								bonusBlock = False;
								SetCurrentMessage(display, messWindow, 
									"- x4 Bonus -", True);
								break;

							default :
								break;
						}

						/* Reset to a non exploding block */
						ClearBlock(r, c);
					}
				}
			}
		}
	}
}

#if NeedFunctionPrototypes
void DrawTheBlock(Display *display, Window window, int x, int y, 
	int blockType, int slide, int r, int c)
#else
void DrawTheBlock(display, window, x, y, blockType, slide, r, c)
	Display *display;
	Window window;
	int x;
	int y; 
	int blockType;
	int slide;
	int r, c;
#endif
{
	struct aBlock *blockP;
	char tmp[10];
	int w, h, x1, y1, len;

	/* Get the pointer to the block we need */
	blockP = &blocks[r][c];

	switch(blockType)
	{
		case TIMER_BLK:		/* extra time clock block */
			RenderShape(display, window, timeblock,
				timeblockM, x, y, 21, 21, False);
			break;

		case PAD_SHRINK_BLK:	/* Paddle shrink block */
			RenderShape(display, window, paddleshrink,
				paddleshrinkM, x, y, 40, 15, False);
			break;

		case PAD_EXPAND_BLK:	/* Paddle expand block */
			RenderShape(display, window, paddleexpand,
				paddleexpandM, x, y, 40, 15, False);
			break;

		case BULLET_BLK:	/* Draw a bullet shape */
			RenderShape(display, window, yellowblock, yellowblockM, 
				x, y, 40, 20, False);
			DrawTheBullet(display, window, x+6, y+10);
			DrawTheBullet(display, window, x+15, y+10);
			DrawTheBullet(display, window, x+24, y+10);
			DrawTheBullet(display, window, x+33, y+10);
			break;

		case MAXAMMO_BLK:
			RenderShape(display, window, unlimitammo, unlimitammoM, 
				x, y, 40, 20, False);
			break;

		case MULTIBALL_BLK:	/* Draw multiple ball block */
			RenderShape(display, window, multiball, multiballM, 
				x, y, 40, 20, False);
			break;

		case STICKY_BLK:	/* Draw sticky block */
			RenderShape(display, window, sticky, stickyM, 
				x, y, 32, 32, False);
			break;

		case RED_BLK:	/* Draw a red block shape */
			RenderShape(display, window, redblock, redblockM, 
				x, y, 40, 20, False);
			break;

		case BLACK_BLK:		/* Draw a solid wall block */	
			RenderShape(display, window, blackblock, blackblockM, 
				x, y, 50, 30, False);
			break;

		case BLACKHIT_BLK:		/* Draw a solid wall block hit */	
			RenderShape(display, window, blackhit, blackhitM, 
				x, y, 50, 30, False);
			break;

		case GREEN_BLK:		/* Draw a green block */
			RenderShape(display, window, greenblock, greenblockM, 
				x, y, 40, 20, False);
			break;

		case DROP_BLK:		/* Draw a drop block same as green block */
			RenderShape(display, window, greenblock, greenblockM, 
				x, y, 40, 20, False);
			sprintf(tmp, "%d", blockP->hitPoints);	
			len = strlen(tmp);
			w 	= XTextWidth(dataFont, tmp, len);
			h 	= dataFont->ascent + dataFont->descent;
			x1 	= x + (20 - w/2);
			y1  = y + (10 - h/2);
			DrawText(display, window, x1, y1, dataFont, black, tmp, len);
			break;

		case BLUE_BLK:		/* Draw a blue block */
			RenderShape(display, window, blueblock, blueblockM, 
				x, y, 40, 20, False);
			break;

		case YELLOW_BLK:	/* Draw a yellow block */
			RenderShape(display, window, yellowblock, yellowblockM, 
				x, y, 40, 20, False);
			break;

		case TAN_BLK:		/* Draw a tan block */
			RenderShape(display, window, tanblock, tanblockM, 
				x, y, 40, 20, False);
			break;

		case PURPLE_BLK:	/* Draw a purple block */
			RenderShape(display, window, purpleblock, purpleblockM, 
				x, y, 40, 20, False);
			break;

		case ROAMER_BLK:	/* Draw a frame of roaming dude block */
			RenderShape(display, window, roamer[slide], 
				roamerM[slide], x, y, 25, 27, False);
			break;

		case COUNTER_BLK:	/* Draw a frame of counter block */
			RenderShape(display, window, counterblock[slide], 
				counterblockM[slide], x, y, 40, 20, False);
			break;

		case BONUSX2_BLK:	/* Draw a bonus x2 coin block */
			RenderShape(display, window, x2bonus[slide], 
				x2bonusM[slide], x, y, 27, 27, False);
			break;

		case BONUSX4_BLK:	/* Draw a bonus x4 coin block */
			RenderShape(display, window, x4bonus[slide], 
				x4bonusM[slide], x, y, 27, 27, False);
			break;

		case BONUS_BLK:	/* Draw a bonus coin block */
			RenderShape(display, window, Bonus[slide], 
				BonusM[slide], x, y, 27, 27, False);
			break;

		case BOMB_BLK:		/* Draw a bomb block */
			RenderShape(display, window, bombblock, bombblockM, 
				x, y, 30, 30, False);
			break;

		case DEATH_BLK:		/* Draw the pirate death block */
			RenderShape(display, window, death[slide], deathM[slide], 
				x, y, 30, 30, False);
			break;

		case REVERSE_BLK:	/* Draw the reverse block */
			RenderShape(display, window, revblock, revblockM, 
				x, y, 33, 16, False);
			break;

		case EXTRABALL_BLK:	/* Draw the extra ball block */
			RenderShape(display, window, extraball[slide], extraballM[slide], 
				x, y, 30, 19, False);
			break;

		case HYPERSPACE_BLK:	/* Draw the hyperspace block */
			RenderShape(display, window, hyperblock, hyperblockM, 
				x, y, 31, 31, False);
			break;

		case MGUN_BLK:	/* Draw the machine gun block */
			RenderShape(display, window, mgunblock, mgunblockM, 
				x, y, 35, 15, False);
			break;

		case WALLOFF_BLK:	/* Draw the wall off block */
			RenderShape(display, window, walloffblock, walloffblockM, 
				x, y, 27, 23, False);
			break;
	}
}

#if NeedFunctionPrototypes
static void SetBlockUpForExplosion(int row, int col, int frame)
#else
static void SetBlockUpForExplosion(row, col, frame)
	int row;
	int col;
	int frame;
#endif
{
    struct aBlock *blockP;

	if (row < 0 || row >= MAX_ROW) return;
	if (col < 0 || col >= MAX_COL) return;

	/* Obtain a pointer to the affected block */
	blockP = &blocks[row][col];

	/* Do not have any effect on a specials block */
	if (blockP->blockType == HYPERSPACE_BLK) return;

	/* If it isn't occupied then why blow it up */
	if (blockP->occupied == 1 && blockP->exploding == False)
	{
		/* Keep track of how many blocks are exploding */
		blocksExploding++;

		/* Some special variables used for timing */
		blockP->explodeStartFrame 	= frame;
		blockP->explodeNextFrame 	= frame;
		blockP->explodeSlide 		= 1;
		blockP->exploding 			= True;

		/* If it was poped up then reset bonus or special flag */
		if (blockP->specialPopup == True) bonusBlock = False;

		/* If it is a dropper then make sure it wont keep dropping */
		if (blockP->drop == True) blockP->drop = False;
	}
}


#if NeedFunctionPrototypes
void DrawBlock(Display *display, Window window, int row, int col, int blockType)
#else
void DrawBlock(display, window, row, col, blockType)
	Display *display;
	Window window;
	int row;
	int col;
	int blockType;
#endif
{
	struct aBlock *blockP;

	if (row < 0 || row > MAX_ROW) 
	{
		ErrorMessage("Block out of bounds row.");
		return;
	}

	if (col < 0 || col > MAX_COL) 
	{
		ErrorMessage("Block out of bounds column.");
		return;
	}

	/* Pointer to the block in question */
	blockP = &blocks[row][col];

	switch(blockType)
	{
		case KILL_BLK:		/* Special block - blow it up */
			PlaySoundForBlock(blockP->blockType);

			if (blockP->blockType != BLACK_BLK)
				SetBlockUpForExplosion(row, col, frame);
			break;

		default:			/* Your average block - draw it */
			DrawTheBlock(display, window, blockP->x, blockP->y, 
				blockType, blockP->counterSlide, row, col);
	}
}

#if NeedFunctionPrototypes
void FreeBlockPixmaps(Display *display)
#else
void FreeBlockPixmaps(display)
	Display *display;
#endif
{
	int i;

	/* Free the memory associated with the block pixmaps */
	if (redblock)		XFreePixmap(display, redblock);			
	if (redblockM)		XFreePixmap(display, redblockM);
	if (blueblock)		XFreePixmap(display, blueblock);		
	if (blueblockM)		XFreePixmap(display, blueblockM);
	if (greenblock)		XFreePixmap(display, greenblock);		
	if (greenblockM)	XFreePixmap(display, greenblockM);
	if (tanblock)		XFreePixmap(display, tanblock);			
	if (tanblockM)		XFreePixmap(display, tanblockM);
	if (yellowblock)	XFreePixmap(display, yellowblock);		
	if (yellowblockM)	XFreePixmap(display, yellowblockM);
	if (purpleblock)	XFreePixmap(display, purpleblock);		
	if (purpleblockM)	XFreePixmap(display, purpleblockM);
	if (blackblock)		XFreePixmap(display, blackblock);		
	if (blackblockM)	XFreePixmap(display, blackblockM);
	if (blackhit)		XFreePixmap(display, blackhit);		
	if (blackhitM)		XFreePixmap(display, blackhitM);		
	if (bombblock)		XFreePixmap(display, bombblock);		
	if (bombblockM)		XFreePixmap(display, bombblockM);
	if (revblock)		XFreePixmap(display, revblock);		
	if (revblockM)		XFreePixmap(display, revblockM);
	if (hyperblock)		XFreePixmap(display, hyperblock);		
	if (hyperblockM)	XFreePixmap(display, hyperblockM);
	if (mgunblock)		XFreePixmap(display, mgunblock);		
	if (mgunblockM)		XFreePixmap(display, mgunblockM);
	if (walloffblock)	XFreePixmap(display, walloffblock);		
	if (walloffblockM)	XFreePixmap(display, walloffblockM);
	if (multiball)		XFreePixmap(display, multiball);		
	if (multiballM)		XFreePixmap(display, multiballM);
	if (sticky)			XFreePixmap(display, sticky);		
	if (stickyM)		XFreePixmap(display, stickyM);
	if (unlimitammo)	XFreePixmap(display, unlimitammo);		
	if (unlimitammoM)	XFreePixmap(display, unlimitammoM);		
	if (paddleexpand)	XFreePixmap(display, paddleexpand);		
	if (paddleexpandM)	XFreePixmap(display, paddleexpandM);
	if (paddleshrink)	XFreePixmap(display, paddleshrink);		
	if (paddleshrinkM)	XFreePixmap(display, paddleshrinkM);
	if (timeblock)		XFreePixmap(display, timeblock);		
	if (timeblockM)		XFreePixmap(display, timeblockM);

	for (i = 0; i < 5; i++)
	{
		/* Free the frames for the roamer block */
		if (roamer[i])	XFreePixmap(display, roamer[i]); 	
		if (roamerM[i])	XFreePixmap(display, roamerM[i]);
	}

	for (i = 0; i < 5; i++)
	{
		/* Free the frames for the death block */
		if (death[i])	XFreePixmap(display, death[i]); 	
		if (deathM[i])	XFreePixmap(display, deathM[i]);
	}

	for (i = 0; i < 6; i++)
	{
		if (counterblock[i])	XFreePixmap(display, counterblock[i]); 	
		if (counterblockM[i])	XFreePixmap(display, counterblockM[i]);
	}

	for (i = 0; i < 4; i++)
	{
		if (x2bonus[i])			XFreePixmap(display, x2bonus[i]); 	
		if (x2bonusM[i])		XFreePixmap(display, x2bonusM[i]);

		if (x4bonus[i])			XFreePixmap(display, x4bonus[i]); 	
		if (x4bonusM[i])		XFreePixmap(display, x4bonusM[i]);

		if (Bonus[i])			XFreePixmap(display, Bonus[i]); 	
		if (BonusM[i])			XFreePixmap(display, BonusM[i]);
	}

	for (i = 0; i < 2; i++)
	{
		if (extraball[i])		XFreePixmap(display, extraball[i]); 	
		if (extraballM[i])		XFreePixmap(display, extraballM[i]);
	}

	for (i = 0; i < 3; i++)
	{
		if (exgreenblock[i])   	XFreePixmap(display, exgreenblock[i]); 	
		if (exgreenblockM[i])   XFreePixmap(display, exgreenblockM[i]);

		if (exyellowblock[i])   XFreePixmap(display, exyellowblock[i]);
		if (exyellowblockM[i])  XFreePixmap(display, exyellowblockM[i]);

		if (exredblock[i])     	XFreePixmap(display, exredblock[i]);
		if (exredblockM[i])     XFreePixmap(display, exredblockM[i]);

		if (exblueblock[i])     XFreePixmap(display, exblueblock[i]);
		if (exblueblockM[i])    XFreePixmap(display, exblueblockM[i]);

		if (extanblock[i])     	XFreePixmap(display, extanblock[i]);
		if (extanblockM[i])     XFreePixmap(display, extanblockM[i]);

		if (excounterblock[i])  XFreePixmap(display, excounterblock[i]);
		if (excounterblockM[i]) XFreePixmap(display, excounterblockM[i]);

		if (exbombblock[i])     XFreePixmap(display, exbombblock[i]);
		if (exbombblockM[i])    XFreePixmap(display, exbombblockM[i]);
		
		if (expurpleblock[i])   XFreePixmap(display, expurpleblock[i]);
		if (expurpleblockM[i])  XFreePixmap(display, expurpleblockM[i]);
		
		if (exx2bonus[i])     	XFreePixmap(display, exx2bonus[i]);
		if (exx2bonusM[i])     	XFreePixmap(display, exx2bonusM[i]);

		if (exdeath[i])			XFreePixmap(display, exdeath[i]); 	
		if (exdeathM[i])		XFreePixmap(display, exdeathM[i]);
	}
}

#if NeedFunctionPrototypes
static void CalculateBlockGeometry(int row, int col)
#else
static void CalculateBlockGeometry(row, col)
	int row, col;
#endif
{
	struct aBlock *blockP;
	XPoint points[4];
	int halfWidth, halfHeight;

	/* Pointer to the correct block we need - speed things up */
	blockP = &blocks[row][col];

	switch (blockP->blockType)
	{
		case COUNTER_BLK:
			blockP->width 			= BLOCK_WIDTH;
			blockP->height			= BLOCK_HEIGHT;
			blockP->blockOffsetX	= (colWidth - BLOCK_WIDTH) / 2;
			blockP->blockOffsetY 	= (rowHeight - BLOCK_HEIGHT) / 2;
			break;

		case TIMER_BLK:
			blockP->width 			= 21;
			blockP->height			= 21;
			blockP->blockOffsetX	= (colWidth - 21) / 2;
			blockP->blockOffsetY 	= (rowHeight - 21) / 2;
			break;

		case ROAMER_BLK:
			blockP->width 			= 25;
			blockP->height			= 27;
			blockP->blockOffsetX	= (colWidth - 25) / 2;
			blockP->blockOffsetY 	= (rowHeight - 27) / 2;
			break;

		case MGUN_BLK:
			blockP->width 			= 35;
			blockP->height			= 15;
			blockP->blockOffsetX	= (colWidth - 35) / 2;
			blockP->blockOffsetY 	= (rowHeight - 15) / 2;
			break;

		case WALLOFF_BLK:
			blockP->width 			= 27;
			blockP->height			= 23;
			blockP->blockOffsetX	= (colWidth - 27) / 2;
			blockP->blockOffsetY 	= (rowHeight - 23) / 2;
			break;

		case REVERSE_BLK:
			blockP->width 			= 33;
			blockP->height			= 16;
			blockP->blockOffsetX	= (colWidth - 33) / 2;
			blockP->blockOffsetY 	= (rowHeight - 16) / 2;
			break;

		case EXTRABALL_BLK:
			blockP->width 			= 30;
			blockP->height			= 19;
			blockP->blockOffsetX	= (colWidth - 30) / 2;
			blockP->blockOffsetY 	= (rowHeight - 19) / 2;
			break;

		case HYPERSPACE_BLK:
			blockP->width 			= 31;
			blockP->height			= 31;
			blockP->blockOffsetX	= (colWidth - 31) / 2;
			blockP->blockOffsetY 	= (rowHeight - 31) / 2;
			break;

		case BOMB_BLK:
		case DEATH_BLK:
			blockP->width 			= 30;
			blockP->height			= 30;
			blockP->blockOffsetX	= (colWidth - 30) / 2;
			blockP->blockOffsetY 	= (rowHeight - 30) / 2;
			break;

		case STICKY_BLK:
			blockP->width 			= 32;
			blockP->height			= 32;
			blockP->blockOffsetX	= (colWidth - 32) / 2;
			blockP->blockOffsetY 	= (rowHeight - 32) / 2;
			break;

		case BLACK_BLK:
			blockP->width 			= 50;
			blockP->height			= 30;
			blockP->blockOffsetX	= (colWidth - 50) / 2;
			blockP->blockOffsetY 	= (rowHeight - 30) / 2;
			break;

        case PAD_SHRINK_BLK:
		case PAD_EXPAND_BLK:
			blockP->width 			= 40;
			blockP->height			= 15;
			blockP->blockOffsetX	= (colWidth - 40) / 2;
			blockP->blockOffsetY 	= (rowHeight - 15) / 2;
			break;

		case BONUS_BLK:
		case BONUSX4_BLK:
		case BONUSX2_BLK:
			blockP->width 			= 27;
			blockP->height			= 27;
			blockP->blockOffsetX	= (colWidth - 27) / 2;
			blockP->blockOffsetY 	= (rowHeight - 27) / 2;
			break;

		default:		/* All other blocks */
			blockP->width 			= BLOCK_WIDTH;
			blockP->height			= BLOCK_HEIGHT;
			blockP->blockOffsetX	= (colWidth - BLOCK_WIDTH) / 2;
			blockP->blockOffsetY 	= (rowHeight - BLOCK_HEIGHT) / 2;
			break;

	}	

	/* Calculate the offset within the block grid */
	blockP->x = (col * colWidth) + blockP->blockOffsetX;
	blockP->y = (row * rowHeight) + blockP->blockOffsetY;

	/* Used below */
	halfWidth = blockP->x + (blockP->width / 2);
	halfHeight = blockP->y + (blockP->height / 2);

	/* Create the XPoint array for the top region */
	points[0].x = blockP->x;
	points[0].y = blockP->y;
	points[1].x = halfWidth;
	points[1].y = halfHeight;
	points[2].x = blockP->x + blockP->width;
	points[2].y = blockP->y;
	points[3].x = points[0].x;
	points[3].y = points[0].y;

	/* Create the top region for the block */
    blockP->regionTop = XPolygonRegion(points, 4, EvenOddRule);

	/* Create the XPoint array for the bottom region */
	points[0].x = blockP->x;
	points[0].y = blockP->y + blockP->height;
	points[1].x = halfWidth;
	points[1].y = halfHeight;
	points[2].x = blockP->x + blockP->width;
	points[2].y = points[0].y;
	points[3].x = points[0].x;
	points[3].y = points[0].y;

	/* Create the bottom region for the block */
    blockP->regionBottom = XPolygonRegion(points, 4, EvenOddRule);

	/* Create the XPoint array for the left region */
	points[0].x = blockP->x;
	points[0].y = blockP->y;
	points[1].x = halfWidth;
	points[1].y = halfHeight;
	points[2].x = blockP->x;
	points[2].y = blockP->y + blockP->height;
	points[3].x = points[0].x;
	points[3].y = points[0].y;

	/* Create the left region for the block */
    blockP->regionLeft = XPolygonRegion(points, 4, EvenOddRule);

	/* Create the XPoint array for the right region */
	points[0].x = blockP->x + blockP->width;
	points[0].y = blockP->y;
	points[1].x = halfWidth;
	points[1].y = halfHeight;
	points[2].x = points[0].x;
	points[2].y = blockP->y + blockP->height;
	points[3].x = points[0].x;
	points[3].y = points[0].y;

	/* Create the right region for the block */
    blockP->regionRight = XPolygonRegion(points, 4, EvenOddRule);
}

#if NeedFunctionPrototypes
void AddNewBlock(Display *display, Window window, int row, int col,
	int blockType, int counterSlide)
#else
void AddNewBlock(display, window, row, col, blockType, counterSlide)
	Display *display;
	Window window;
	int row;
	int col;
	int blockType;
	int counterSlide;
#endif
{
	struct aBlock *blockP;

	if (row > MAX_ROW || row < 0) return;
	if (col > MAX_COL || col < 0) return;

	/* Pointer to the block we want */
	blockP = &blocks[row][col];

	/* Now set the block structure with new values */
	blockP->blockType 		= blockType;
	blockP->occupied 		= 1;
	blockP->counterSlide 	= counterSlide;
	blockP->lastFrame 		= frame + INFINITE_DELAY;

	/* Handle the special case for a random block */
	if (blockType == RANDOM_BLK)
	{
		/* Setup the random block so it has a next frame and new type */
		blockP->random 	  = True;
		blockP->blockType = RED_BLK;
		blockP->nextFrame = frame + 1;
	} else if (blockType == DROP_BLK)
	{
		/* Setup for a dropping block */
		blockP->drop 	  = True;
		blockP->nextFrame = frame + (rand() % DROP_DELAY) + 200;
	} else if (blockType == ROAMER_BLK)
	{
		/* Setup for a roaming block */
		blockP->nextFrame = frame + (rand() % ROAM_EYES_DELAY) + 50;
		blockP->lastFrame = frame + (rand() % ROAM_DELAY) + 300;
	}

	/* Work out all the block geometry stuff */
	CalculateBlockGeometry(row, col);
	
	/* Add the number of points that will be awarded for each block */
	switch(blockType)
	{
		case BULLET_BLK:
		case MAXAMMO_BLK:
			blockP->hitPoints = 50;
			break;

		case RED_BLK:
			blockP->hitPoints = 100;
			break;

		case GREEN_BLK:
			blockP->hitPoints = 120;
			break;

		case BLUE_BLK:
			blockP->hitPoints = 110;
			break;

		case TAN_BLK:
			blockP->hitPoints = 130;
			break;

		case YELLOW_BLK:
			blockP->hitPoints = 140;
			break;

		case PURPLE_BLK:
			blockP->hitPoints = 150;
			break;

		case BOMB_BLK:
			blockP->hitPoints = 50;
			break;

		case ROAMER_BLK:
			blockP->hitPoints = 400;
			break;

		case DROP_BLK:
			blockP->hitPoints = ((MAX_ROW - row) * 100);
			break;

		case COUNTER_BLK:
			blockP->hitPoints = 200;
			break;

		case EXTRABALL_BLK:
			blockP->nextFrame = frame + EXTRABALL_DELAY;
			blockP->hitPoints = 100;
			break;

		case TIMER_BLK:
		case HYPERSPACE_BLK:
		case MGUN_BLK:
		case WALLOFF_BLK:
		case REVERSE_BLK:
		case MULTIBALL_BLK:
		case STICKY_BLK:
        case PAD_SHRINK_BLK:
		case PAD_EXPAND_BLK:
			blockP->hitPoints = 100;
			break;

		case DEATH_BLK:
			blockP->hitPoints = 0;
			blockP->nextFrame = frame + DEATH_DELAY2;
			break;

		default:
			break;
	}

	/* Draw the blocks please */
	DrawBlock(display, window, row, col, blockType);
}

#if NeedFunctionPrototypes
void SkipToNextLevel(Display *display, Window window)
#else
void SkipToNextLevel(display, window)
	Display *display;
	Window window;
#endif
{
    struct aBlock *blockP;
	int r, c;

	/* This will kill all blocks that need to go before next level can
	 * be reached. Used in debug mode and maybe in special bonus mode.
	 */

	for (r = 0; r < MAX_ROW; r++)
		for (c = 0; c < MAX_COL; c++)
		{
			/* Pointer to the block we want */
			blockP = &blocks[r][c];

			if (blockP->occupied == True) 
			{
				switch (blockP->blockType)
				{
					case BONUSX2_BLK:
					case TIMER_BLK:
					case BONUSX4_BLK:
					case BONUS_BLK:
					case BLACK_BLK:
					case BULLET_BLK:
					case MAXAMMO_BLK:
					case BOMB_BLK:
					case DEATH_BLK:
					case REVERSE_BLK:
					case HYPERSPACE_BLK:
					case EXTRABALL_BLK:
					case MGUN_BLK:
					case WALLOFF_BLK:
					case MULTIBALL_BLK:
					case STICKY_BLK:
        			case PAD_SHRINK_BLK:
					case PAD_EXPAND_BLK:
						break;

					default:
						DrawBlock(display, window, r, c, KILL_BLK);
						break;
				}
			}
		}
}

#if NeedFunctionPrototypes
void RedrawAllBlocks(Display *display, Window window)
#else
void RedrawAllBlocks(display, window)
	Display *display;
	Window window;
#endif
{
    struct aBlock *blockP;
	int r, c;

	for (r = 0; r < MAX_ROW; r++)
	{
		for (c = 0; c < MAX_COL; c++)
		{
			/* Pointer to the block we want */
			blockP = &blocks[r][c];

			if (blockP->occupied == True)
				DrawBlock(display, window, r, c, blockP->blockType);
		}
	}	
}

#if NeedFunctionPrototypes
int StillActiveBlocks(void)
#else
int StillActiveBlocks()
#endif
{
    struct aBlock *blockP;
	int r, c;

	/* Check all blocks to see if they still are active */
	for (r = 0; r < MAX_ROW; r++)
	{
		for (c = 0; c < MAX_COL; c++)
		{
			/* Pointer to the block we want */
			blockP = &blocks[r][c];

			/* Don't bother me if the block is not occupied */
			if (blockP->occupied == True) 
			{
				/* Check the type of block as specials are not needed to be
				 * killed - only some blocks are required to finish a level.
				 */
				switch (blockP->blockType)
				{
					/* These blocks don't count */
					case BONUSX2_BLK:
					case BONUSX4_BLK:
					case BONUS_BLK:
					case BLACK_BLK:
					case BULLET_BLK:
					case MAXAMMO_BLK:
					case ROAMER_BLK:
					case BOMB_BLK:
					case DEATH_BLK:
					case TIMER_BLK:
					case REVERSE_BLK:
					case HYPERSPACE_BLK:
					case EXTRABALL_BLK:
					case MGUN_BLK:
					case WALLOFF_BLK:
					case MULTIBALL_BLK:
					case STICKY_BLK:
        			case PAD_SHRINK_BLK:
					case PAD_EXPAND_BLK:
						break;

					default:
						return True;
				}
			}
		} 	/* cols */
	}	/* rows */

	/* Only all done when explosions are finished */
	if (blocksExploding > 0)
		return True;

	return False;
}

#if NeedFunctionPrototypes
static void ClearBlock(int row, int col)
#else
static void ClearBlock(row, col)
	int row;
	int col;
#endif
{
    struct aBlock *blockP;

	/* Pointer to the block we want */
	blockP = &blocks[row][col];

	/* Initialise everything in block */
	blockP->occupied 			= False;
	blockP->exploding 			= False;
	blockP->x 					= 0;
	blockP->y 					= 0;
	blockP->width 				= 0;
	blockP->height 				= 0;
	blockP->hitPoints 			= 0;
	blockP->blockType 			= NONE_BLK;
	blockP->explodeStartFrame 	= 0;
	blockP->explodeNextFrame 	= 0;
	blockP->explodeSlide 		= 0;
	blockP->counterSlide 		= 0;
	blockP->bonusSlide 			= 0;
	blockP->blockOffsetY 		= 0;
	blockP->blockOffsetX 		= 0;
	blockP->lastFrame 			= INFINITE_DELAY;
	blockP->nextFrame 			= 0;
	blockP->currentFrame 		= 0;
	blockP->random 				= False;
	blockP->drop 				= False;
	blockP->ballHitIndex 		= 0;
	blockP->balldx 				= 0;
	blockP->balldy 				= 0;
	blockP->specialPopup 		= False;

	/* Destroy the top region of the block */
	if (blockP->regionTop != (Region) NULL)
	{
		XDestroyRegion(blockP->regionTop);
		blockP->regionTop = (Region) NULL;
	}

	/* Destroy the bottom region of the block */
	if (blockP->regionBottom != (Region) NULL)
	{
		XDestroyRegion(blockP->regionBottom);
		blockP->regionBottom = (Region) NULL;
	}

	/* Destroy the left region of the block */
	if (blockP->regionLeft != (Region) NULL)
	{
		XDestroyRegion(blockP->regionLeft);
		blockP->regionLeft = (Region) NULL;
	}

	/* Destroy the right region of the block */
	if (blockP->regionRight != (Region) NULL)
	{
		XDestroyRegion(blockP->regionRight);
		blockP->regionRight = (Region) NULL;
	}
}


#if NeedFunctionPrototypes
void ClearBlockArray(void)
#else
void ClearBlockArray()
#endif
{
	int r, c;

	/* Scan through all block entries and erase their contents */
	for (r = 0; r < MAX_ROW; r++)
	{
		for (c = 0; c < MAX_COL; c++)
		{
			/* Clear the block thanks */
			ClearBlock(r, c);
		}
	}
}

#if NeedFunctionPrototypes
void SetupStage(Display *display, Window window)
#else
void SetupStage(display, window)
	Display *display;
	Window window;
#endif
{
	char levelPath[1024];
	char *str;
	char str2[1024];
	static int bgrnd = 1;
	u_long newLevel;

	bgrnd++;
	if (bgrnd == 10) bgrnd = 2;
	DrawStageBackground(display, window, bgrnd, True);

	ClearAllBalls();
	currentPaddleSize   = PADDLE_HUGE;
	ResetPaddleStart(display, window);
	ResetBallStart(display, playWindow);

	SetUnlimitedBullets(False);
	ClearBullets();
	SetNumberBullets(NUMBER_OF_BULLETS_NEW_LEVEL);
	ResetNumberBonus();

	TurnSpecialsOff(display);		
	SetReverseOff();
	DisplayScore(display, scoreWindow, score);
	DisplayLevelInfo(display, levelWindow, level);
	DrawSpecials(display);

    /* map the level number for the filename so that after the maximum
	 * level is reached it will wrap around to 1 again but will not
  	 * affect the level number
   	 */
   	newLevel = level % (MAX_NUM_LEVELS + 1);
  	if (newLevel == 0) newLevel = 1;

	/* Construct the level filename */
	if ((str = getenv("XBOING_LEVELS_DIR")) != NULL)
		sprintf(levelPath, "%s/level%02ld.data", str, newLevel);
	else
		sprintf(levelPath, "%s/level%02ld.data", LEVEL_INSTALL_DIR, newLevel);

	if (ReadNextLevel(display, window, levelPath) == False)
		ShutDown(display, 1, "Sorry, invalid level specified.");
	
	/* Display level name for all to see */
	sprintf(str2, "- %s -", GetLevelName());
	SetCurrentMessage(display, messWindow, str2, True);

	XFlush(display);
}
