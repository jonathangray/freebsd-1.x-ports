/* mhole.c - mouse hole implementation over wn */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char shared_mhole_c_sccsid[] = "@(#)mhole.c	1.12 26/4/92 (UKC)";


#ifdef perq
#include <wuser.h>
#include <sgwin.h>
#include <sys/types.h>
#include <sys/stat.h>
#endif /* perq */

#include <local/wn.h>
#include <string.h>

#include <local/ukcprog.h>

#include "mhole.h"

extern int errno;

static window_t Mh_wn;		/* file descriptor of the current window */

static short Mh_x, Mh_y;	/* mouse hole top-left corner */

static short Mh_width, Mh_height;	/* width and height of mousehole */

/*  Number of buttons - will be overwritten with 4 on a Perq with
 *  the high resolution 4 button tablet
 */
static int Num_buttons = 3;

/*  Structure describing the position of a mouse hole caption.
 *  The position is that of the top-centre of the caption.
 */
struct mhcst {
	short mh_capnum;
	short mh_x;
	short mh_y;
} Bpos[4] = { {0}, {1}, {2}, {3} };

/*  We do not record absolute positions for mousehole captions, as
 *  the size of the mousehole depends on the font size. Instead we
 *  record value with this structure.
 *  The actual value is calculated at runtime as
 *
 *	(ex_mul * Sysfont_{width,height}) / 4 + ex_add
 */
struct expst {
	short ex_mul;
	short ex_add;
};

static void draw_mousehole_boxes PROTO((void));
static void put_mhcap PROTO((const char **caps, int cnum, int x, int y));
static int calcpos PROTO((struct expst exp, int scale));

/*  Given a struct expst exp and a scale, return a size (see comment
 *  on struct expst above.
 */
static int
calcpos(exp, scale)
struct expst exp;
int scale;
{
	return (exp.ex_mul * scale) / 4 + exp.ex_add;
}

/*  Structure for recording a caption position - the x and y coords,
 *  stored as expst structures (above).
 */
struct pbposst {
	struct expst pb_x;
	struct expst pb_y;
};

/*  caption positions for physical buttons for the 3 button puck
 *  and the sun mouse
 */
static struct pbposst pb3pos[4] = {
	 1, -8,	2, -9,		/* Left button */
	 2, 0,	0, 4,		/* Middle button */
	 3, 8,	2, -9,		/* Right button */
	 0, -1, 	0, -1,		/* dummy */
};

#ifdef perq
/*  caption positions for physical buttons for the 4 button puck
 */
static struct pbposst pb4pos[4] = {
	 2, 0,	0, 2,		/* Yellow button */
	 1, -8,	2, -9,		/* White button */
	 2, 0,	2, 18,		/* Blue button */
	 3, 8,	2, -9,		/* Green button */
};

#define DEV_4BT		6	/* major device number for 4 button tablet */
#define DEV_3BT		12	/* major device number for 3 button tablet */

/*  Draw a diamond with centre at x,y and all four points radius pixels
 *  distant (horizontally or vertically) from the centre.
 *  Used for drawing mousehole buttons for the Perq 4 button tablet.
 */
static int
draw_diamond(wn, x, y, radius)
register window_t wn;
int x, y, radius;
{
	wn_draw_line(wn, x,        y-radius, x+radius, y,        WN_FG);
	wn_draw_line(wn, x+radius, y,        x,        y+radius, WN_FG);
	wn_draw_line(wn, x,        y+radius, x-radius, y,        WN_FG);
	wn_draw_line(wn, x-radius, y,        x,        y-radius, WN_FG);
}
#endif /* perq */

/*  Determine the mouse hole position and draw the box. Should be called after
 *  a window size change. Box is drawn at x,y.
 */
void
draw_mousehole(wn, x, y, width, height)
int wn;
int x, y, width, height;
{
	int pb;
	struct pbposst *pbpos;
#ifdef perq
	static int first_call;
	int lb;
	struct stat sbuf;
	union sgwinb op;
#endif /* perq */

#ifdef perq
	if (first_call) {
		if (stat("/dev/tablet", &sbuf) < 0)
			panic("Can't stat /dev/tablet");
		Num_buttons = (major(sbuf.st_rdev) == DEV_4BT) ? 4 : 3;
		first_call = FALSE;
	}
	pbpos = (Num_buttons == 4) ? pb4pos : pb3pos;
#else /* perq */
	pbpos = pb3pos;
#endif /* !perq */

	for (pb = 0; pb < Num_buttons; pb++) {
		Bpos[pb].mh_x = calcpos(pbpos[pb].pb_x, width);
		Bpos[pb].mh_y = calcpos(pbpos[pb].pb_y, height);
	}

#ifdef perq
	ioctl(wn_menu_id(wn), WIOCGCONF, &op);
	for (pb = 0; pb < 4; pb++) {
		for (lb = 0; lb < 4; lb++)
			if (op.sg_WConf.WCTabMap[pb] & (1 << lb)) {
				Bpos[pb].mh_capnum = lb;
			}
	}
#endif /* perq */
	Mh_width = width;
	Mh_height = height;
	Mh_wn = wn;
	Mh_x = x;
	Mh_y = y;
	put_mhcap((const char **)NULL, 0, 0, 0);
	wn_updating_off(Mh_wn);
	draw_mousehole_boxes();
	wn_updating_on(Mh_wn);
}

/*  Various #defines for the sizes of the button rectangles and
 *  other uninteresting stuff
 */
#define B_TOTAL_WIDTH 36
#define B_TOTAL_DEPTH 40

#define N_BOXES 3
#define B_WIDTH (B_TOTAL_WIDTH / N_BOXES)

#define B_DEPTH B_TOTAL_DEPTH

#define B_HBORDER 3
#define B_VBORDER 6

#define D_RADIUS (B_TOTAL_WIDTH / 4)

/*  Draw the mousehole with the given captions.
 */
static void
draw_mousehole_boxes()
{
	int l;
	int bx, by, ystart;
#ifdef perq
	int cx, cy;
#endif

	if (Num_buttons == 3) {
		bx = Mh_x + (Mh_width - B_TOTAL_WIDTH) / 2 + B_HBORDER;
		ystart = calcpos(pb3pos[1].pb_y, Mh_height) +
					wn_get_sysfont()->ft_height + 6;
		by = Mh_y + ystart + ((Mh_height - ystart) - B_DEPTH) / 2;
		for (l = 0; l < 3; l++) {
			wn_box_round(Mh_wn, bx, by,
				B_WIDTH - B_HBORDER*2,
				B_DEPTH - B_VBORDER*2, WN_FG);
			bx += B_WIDTH;
		}
	}
#ifdef perq
	else {
		cx = Mh_x + Mh_width / 2;
		cy = Mh_y + Mh_height / 2;
		draw_diamond(Mh_wn, cx-D_RADIUS, cy         , D_RADIUS-3);
		draw_diamond(Mh_wn, cx         , cy-D_RADIUS, D_RADIUS-3);
		draw_diamond(Mh_wn, cx+D_RADIUS, cy         , D_RADIUS-3);
		draw_diamond(Mh_wn, cx         , cy+D_RADIUS, D_RADIUS-3);
	}
#endif /* perq */
}

/*  Display a mousehole caption s at x, y, relative to the mousehole.
 *  Clip to the mousehole size, and centre about x
 */
static void
put_mhcap(caps, cnum, x, y)
const char **caps;
int cnum, x, y;
{
	struct boxst {
		short box_x;
		short box_y;
		short box_w;
		short box_h;
	};
	static struct boxst boxtab[4];
	register struct boxst *b;
	char buf[30];
	int w, h;
	
	if (caps == NULL) {
		for (b = boxtab; b < boxtab + 4; b++)
			b->box_w = 0;
		return;
	}
	b = boxtab + cnum;
	if (b->box_w != 0)
		wn_set_area(Mh_wn, b->box_x, b->box_y, b->box_w, b->box_h, WN_BG);

	if (caps[cnum] == NULL) {
		b->box_w = 0;
		return;
	}

	(void) strncpy(buf, caps[cnum], sizeof(buf));

	w = ((x >= Mh_width/2) ? Mh_width - x : x) * 2;
	h = wn_get_sysfont()->ft_height;

	buf[sizeof(buf) - 1] = '\0';
	buf[wn_strpos(buf, w, (font_t *)NULL, FALSE)] = '\0';
	w = wn_strwidth(buf, (font_t *)NULL);

	b->box_x = Mh_x + x - w / 2;
	b->box_y = Mh_y + y;
	b->box_w = w;
	b->box_h = h;
	wn_tputs(Mh_wn, buf, b->box_x, b->box_y);
}

void
mhdraw(caps)
const char **caps;
{
	int pb;

	wn_updating_off(Mh_wn);
	for (pb = 0; pb < Num_buttons; pb++)
		put_mhcap(caps, Bpos[pb].mh_capnum, Bpos[pb].mh_x, Bpos[pb].mh_y);
	wn_updating_on(Mh_wn);
}
