/* menu3.h - menu library public header file */

/*  Copyright 1991 John Bovey, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)menu3.h	1.11 4/5/92 (UKC) */

#ifdef __STDC__
#define MPROTO(x) x
#else
#define MPROTO(x) ()
#define const
#endif /* !__STDC__ */

/*  Structures and constants used in a menu tree structure
 */
typedef struct mnode {
	unsigned short me_flags;
	short me_pos;	/*  The position at which the field is divided */
	const char *me_cap;	/*  Caption to be displayed in the field */
	short me_rv; 		/* result to be returned */
	short me_xstart;	/* The window area of this node */
	short me_ystart;
	short me_xend;
	short me_yend;
	short me_xcurs;		/* cursor coordinates for cursor */
	short me_ycurs;		/* relative popup menus */
	short me_colour;
	long me_save;		/* For saving popup backgrounds */
	struct mnode * me_topleft; /* pointer to top or left sub-menu */
	struct mnode * me_botrite; /* pointer to b. or r. sub_menu */
	struct mnode * me_parent;  /* pointer to parent */
	struct omenst *me_omen;	   /* pointer to open menu structure */
} MENU;

#ifndef NULL
#define NULL	0
#endif

#define MAXMEN 40	/* the maximum number of open menus */

/*  menerr error values
 */
#define MENOK	0
#define MTABFL	1	/* already MAXMEN open menus */
#define MNOTMEN	2	/* menu descriptor is not an open menu */
#define MBADMD	3	/* menu descriptor out of range */
#define MBADFIL 4	/* can't read menu from menu file */
#define MNOFIL	5	/* can't open menu file */
#define MBADWD	6	/* bad window file descriptor */
#define MNOTDIS 7	/* the menu is not currently displayed */
#define MBADARG 8	/* bad function arguments */
#define MAERROR 9	/* message area error */

/*  structure returned by Mopenerr() to report diagnostics on errors in
 *  reading C menu files.
 */
#define MAXMERR	128
struct merrst {
	char mr_message[MAXMERR];
	int  mr_line;		/* line number */
};

/*  modes for Mselect.
 */
#define MS_PRESSED	0
#define MS_RELEASED	1
#define MS_CONTINUE	2

/*  style flags for Mmake.
 */
#define MM_DRAG		1	/* select on drag */
#define MM_NOLINE	2	/* don't draw the background lines */
#define MM_LEFT		4	/* Left justify rather than centre */

/*  Macros for extracting Mselect return values
 */
#define MS_md(x) (((x) >> 16) & 0177)
#define MS_rv(x) ((x) & 077777)

/*  Different ways of highlighting submenus
 */
#define MH_WHITE 0	/* no highlighting */
#define MH_GREY  1	/* shaded light grey */
#define MH_BLACK 2	/* inverted */
#define MH_BOLD  4	/* text emboldened, single box only */

/*  Macros used to construct return values from ASCII part and layer
 */
#define Mrv_ascii(rv)	((rv) & 0177)
#define Mrv_layer(rv)	(((rv) >> 8) & 3)
#define Mrvalue(l,c)	(((l) << 8) | (c))

/*  Offset styles used by Mlink.
 */
#define ME_MLBUTR	1
#define ME_MLBUTB	2
#define ME_MLPOPR	4
#define ME_MLPOPB	8
#define ME_MLCREL	16

/*  Function prototypes.
 */

int Msopen MPROTO((int wn, int x, int y, int width, int height));
int Msclose MPROTO((int mad));
int Mputs MPROTO((int mad, const char *str));
int Mgets MPROTO((int mad, char *str));
int Mputc MPROTO((int mad, int c));
int Merase MPROTO((int mad, int n));

int Mclose MPROTO((int md));
int Mmake MPROTO((const char **captions, const int *rvalues,
					const int *fonts, int style, int width));
void Mcheck MPROTO((int md));
int Mpushsu MPROTO((int wn));
int Mpopsu MPROTO((int wn));
int Mclear MPROTO((int md));
int Mrepair MPROTO((int wn));
int Mredraw MPROTO((int wn));
void Mmredraw MPROTO((int md));
#ifdef WN_STDWIN
int Mfonts MPROTO((int md, font_t *font0, font_t *font1,
			  font_t *font2, font_t *font3));
#endif
int Mnobox MPROTO((int md));
int Mnonsel MPROTO((int md, const char *rvs));
int Mreset MPROTO((int md));
int Mcansel MPROTO((int x, int y, int wout));
int Mselect MPROTO((int x, int y, int mode, int wout, int ignore));
int Mdisplay MPROTO((int md, int wout, int save));
int Mremove MPROTO((int md));
int Mdup MPROTO((int md));
int Mfmodes MPROTO((int md, int reveal, int toact, int acting));
int Mleft MPROTO((int md));
int Mright MPROTO((int md));
int Mtop MPROTO((int md));
int Mbottom MPROTO((int md));
int Mwidth MPROTO((int md));
int Mheight MPROTO((int md));
int Minsert MPROTO((MENU *menu));
int Mlink MPROTO((int mainmd, int submd, int rv, int xoff, int yoff, int mlstyle));
void Mdump MPROTO((int md));
int Mchange MPROTO((int md, int oldrv, int newrv, const char *str, int fontno));
int Mopen MPROTO((const char *name));
struct merrst *Mopenerr MPROTO((void));
int Mperror MPROTO((const char *str));
int Msize MPROTO((int md, int width, int height));
int Mplace MPROTO((int md, int x, int y));
struct fontst *Mstdfont MPROTO((void));
void Msetstdfont MPROTO((struct fontst *font));
const char *Mversion MPROTO((void));

/*  No parameter names for fpush and fpop as they give g++ heartburn.
 *  I think this is a bug in g++.
 */
void Msetpushpop MPROTO((int (*)(int wn), int (*)(int wn)));
