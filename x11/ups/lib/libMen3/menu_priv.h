/* menu_priv.h - private header file for the menu library */

/*  Copyright 1991 John Bovey, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)menu_priv.h	1.5 26/4/92 (UKC) */

/*  define these to make the source look more like jdb's version
 */
typedef int WOTYPE, WITYPE;
typedef struct fontst FONT;

/* flags for me_flags
 */
#define ME_FONT		0x3	/* mask for font */
#define ME_VER		0x4	/* node is divided vertically */
#define ME_HOR		0x8	/* node divided horizontally */
#define ME_DIV		0xc	/* node is divided */
#define ME_NSEL		0x10	/* This node can't be selected */
#define ME_POPUP	0x20	/* use in pop-up mode */
#define ME_FREC		0x40	/* me_cap can be free()'d */
#define ME_FREN		0x80	/* the node itself can be free()'d */
#define ME_OPEN		0x100	/* the node is open */
#define ME_ISUB		0x200	/* has an indivisible submenu */
#define ME_FREE		0x400	/* free standing node */
#define ME_BGLINE	0x800	/* draw lines in background colour */
#define ME_NOSCALE	0x1000	/* don't scale submenu */
#define ME_CREL		0x2000	/* cursor relative submenu */
#define ME_REDRAW	0x4000  /* flag to force menu redrawing */
#define ME_LEFT		0x8000	/* left justify caption rather than centre it */

#define ISCLEAR(m)	(((m)->me_cap == NULL) || \
			(((m)->me_flags & (ME_OPEN|ME_ISUB)) == ME_OPEN))

/*  Definitions of structures used to replace the Perq ClipCtl structure
 *  in the menu library.  Will someday get rid of these.
 */
typedef struct posst {
	short x;
	short y;
} Pos_t;

typedef struct sizest {
	short w;
	short h;
} Size_t;

typedef struct boxst {
	Pos_t pos;
	Size_t size;
} Box_t;

/*  structure defining an opened menu
 */
struct omenst {
	MENU *om_root;		/* root node of the menu tree */
	MENU *om_last;		/* last node displayed (NULL if not displayed) */
	WOTYPE om_wout;		/* file descriptor of display window */
	short om_md;		/* menu descriptor of the menu */
	Box_t om_area;		/* location of saved screen area */ 
	long om_rect;		/* pointer to saved screen area */
	short om_fback[3];	/* feedback colours used */
	FONT *om_font[4];	/* fonts for captions */
	int om_wantbox;		/* do we want a box round the menu? */
};

extern int menerr;
extern struct omenst _menu_[];

extern window_t _Last_wid;
extern int _Last_wn;

int wid_to_wn MPROTO((WOTYPE wid));
int wblank MPROTO((WOTYPE wout, Box_t *clip));
int mshift MPROTO((MENU * menu, int x, int y, int root));
void momen MPROTO((MENU *menu, struct omenst *omen));
int mshow MPROTO((MENU *menu, int drawall));
