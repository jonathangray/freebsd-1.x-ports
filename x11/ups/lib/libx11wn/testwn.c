/* testwn - test program for the wn library */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char wn_testwn_c_sccsid[] = "@(#)testwn.c	1.21 25/4/92 (UKC)";

#include <sys/types.h>
#include <sys/time.h>
#include <stdio.h>
#include <signal.h>
#include <sys/file.h>
#include <stdlib.h>
#include <string.h>

#ifdef __STDC__
#include <unistd.h>
#endif

#include <local/ukcprog.h>

#include "wn.h"

#define UNSET 0

/*  Radio button.
 */
typedef struct rbst {
	const char *rb_cap;
	int rb_group;
	int rb_arg;
	int (*rb_selfunc)PROTO((int group, int arg, int was_set));
	int rb_flags;
	int rb_rel_group;
	int rb_rel_arg;
	int rb_x;
	int rb_y;
	int rb_cur_x;
	int rb_cur_y;
} rb_t; 

#define MAXWINS		500

void _wn_want_own_icon PROTO((void));
void _wn_enable_self_deiconise PROTO((void));

static int pixnum PROTO((int n));
static void init_colors PROTO((void));
static void test_shade PROTO((int wn));
static void dump_buttons PROTO((int buttons));
static void dump_event PROTO((event_t *ev));
static int set_sc PROTO((int group, int arg, int was_set));
static int toggle_coord PROTO((int group, int arg, int was_set));
static int set_mode PROTO((int group, int arg, int was_set));
static int set_mask_cursor PROTO((int group, int arg, int was_set));
static int set_cursor PROTO((int group, int arg, int was_set));
static void set_rb PROTO((int wn, rb_t *rb, int colors));
static void draw_rb PROTO((int wn, rb_t *rbtab, int n_rbs));
static int get_rb_pos PROTO((rb_t *rbtab, int n_rbs, int group, int arg, int *p_x, int *p_y));
static void select_rb PROTO((int wn, rb_t *rbtab, int n_rbs, int x, int y));
static void print_args PROTO((int argc, const char **argv));
static void square PROTO((int wn, int x, int y, int x_offs, int y_offs, int on));
static void draw_box PROTO((int wn, int pat, int x, int y));
static void draw_rop_boxes PROTO((int wn, int x, int y));
static void drag_box PROTO((int wn, int x, int y));
static void test_subwins PROTO((int wn));
static void catch_sigterm PROTO((int unused_sig));
static int deiconise PROTO((int wn));
static int run_multiple_windows PROTO((int *wntab, int nwins));
static void test_planes PROTO((int wn));
static const char *reason PROTO((void));
static void draw_string PROTO((int wn, font_t *font, const char *s,
					int x, int y, bool want_textbox, int which));
void main PROTO((int argc, const char **argv));

#define GREEN	0
#define MAGENTA	1
#define YELLOW	2
#define MEDBLUE	3
#define KHAKI	4
#define FGREEN	5
#define RED	6

static color_t Colors[] = {
	0, 0x0000, 0xffff, 0x0000,	/* green */
	0, 0xffff, 0x0000, 0xffff,	/* magenta */
	0, 0xffff, 0xffff, 0x0000,	/* yellow */
	0, 0x0000, 0x0000, 0x7f00,	/* medium blue */
	0, 0x9f00, 0x9f00, 0x5f00,	/* khaki */
	0, 0x2300, 0x8e00, 0x2300,	/* forest green */
	0, 0xa000, 0x0000, 0x0000,	/* red */
};

#define NCOLORS	(sizeof(Colors) / sizeof(Colors[0]))

#define Cross_width 16
#define Cross_height 16
#define Cross_x_hot 13
#define Cross_y_hot 4
static char Cross_bits[] = {
   0x00, 0x06, 0x00, 0x0c, 0x00, 0x18, 0x00, 0x18, 0xf8, 0x3f, 0xfc, 0x3f,
   0x0c, 0x18, 0x0c, 0x0c, 0x0c, 0x06, 0x0c, 0x03, 0x0c, 0x01, 0x0c, 0x00,
   0x0c, 0x00, 0x0c, 0x00, 0x00, 0x00, 0x00, 0x00};
bitmap_t Cross_bm = wn_static_bm(Cross_width, Cross_height, 1, 4, 4, BM_BIT0_LEFT, (unsigned short *)Cross_bits);

#define Cross_mask_width 16
#define Cross_mask_height 16
static char Cross_mask_bits[] = {
   0x00, 0x1f, 0x00, 0x1e, 0x00, 0x3e, 0xf8, 0x7f, 0xfc, 0x7f, 0xfe, 0x7f,
   0xfe, 0x3f, 0x1e, 0x1f, 0x9e, 0x0f, 0x9e, 0x07, 0x9e, 0x03, 0x9e, 0x01,
   0x1e, 0x00, 0x1e, 0x00, 0x00, 0x00, 0x00, 0x00};
bitmap_t Cross_mask_bm = wn_static_bm(Cross_mask_width, Cross_mask_height, 1, 4, 4, BM_BIT0_LEFT, (unsigned short *)Cross_mask_bits);

unsigned short Cu8l_data[] = {
	0x0101, 0x0203, 0x0405, 0x0809, 0x1011, 0x2021, 0x4041, 0x8081,
};
bitmap_t Cu8l = wn_static_bm(8, 8, 1, 4, 4, BM_BIT0_LEFT, Cu8l_data);

unsigned short Cu8r_data[] = {
	0x0101, 0x0203, 0x0405, 0x0809, 0x1011, 0x2021, 0x4041, 0x8081,
};
bitmap_t Cu8r = wn_static_bm(8, 8, 1, 4, 4, BM_BIT0_RIGHT, Cu8r_data);

unsigned short Cu16l_data[] = {
	0x0001, 0x0002, 0x0004, 0x0008, 0x0010, 0x0020, 0x0040, 0x0080,
	0x0100, 0x0200, 0x0400, 0x0800, 0x1000, 0x2000, 0x4000, 0x8000,
};
bitmap_t Cu16l = wn_static_bm(16, 16, 1, 4, 4, BM_BIT0_LEFT, Cu16l_data);

unsigned short Cu16r_data[] = {
	0x0001, 0x0002, 0x0004, 0x0008, 0x0010, 0x0020, 0x0040, 0x0080,
	0x0100, 0x0200, 0x0400, 0x0800, 0x1000, 0x2000, 0x4000, 0x8000,
};
bitmap_t Cu16r = wn_static_bm(16, 16, 1, 4, 4, BM_BIT0_RIGHT, Cu16r_data);

unsigned char Cu64l_data[] = {
0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
0x00,0x80,0x03,0x00,0x00,0x18,0x00,0x00,0xc0,0x7f,0x08,0x00,0x00,0x00,0x38,0x00,
0x00,0x04,0x08,0x02,0x0c,0x10,0x08,0x00,0x00,0x04,0x08,0x80,0x07,0x10,0x18,0x00,
0x00,0x04,0x38,0x80,0x00,0x10,0x70,0x00,0x00,0x04,0x38,0x82,0x07,0x10,0xc0,0x00,
0x00,0x04,0x68,0x02,0x0c,0x60,0xc0,0x00,0x00,0x04,0x48,0x06,0x0c,0x60,0x7c,0x00,
0x00,0x04,0x4c,0x8c,0x07,0x00,0x00,0x00,0x00,0x04,0x44,0x0c,0x00,0x00,0x00,0x00,
0x00,0x04,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x30,
0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x18,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x0c,
0x00,0x00,0x00,0x00,0x00,0xf0,0x00,0x04,0x00,0x00,0x00,0x00,0x00,0x88,0x30,0x08,
0x00,0x00,0x00,0x00,0x06,0x08,0x11,0x08,0x00,0x00,0x40,0x00,0x1e,0x88,0x09,0xc8,
0x00,0x00,0x40,0x00,0x32,0xc8,0x08,0x68,0x60,0x00,0x40,0x00,0x22,0x78,0x08,0x38,
0xf0,0x00,0x40,0x00,0x62,0x68,0x08,0x11,0x10,0x01,0x40,0x00,0x7e,0xc8,0x18,0x13,
0x10,0x01,0x40,0x00,0x86,0x88,0x31,0x11,0x10,0x01,0x40,0x00,0x82,0x09,0xe7,0x11,
0x30,0x03,0x40,0x00,0x02,0x09,0x00,0xd0,0xc0,0x07,0xc0,0x00,0x02,0x08,0x00,0x30,
0x00,0x18,0x80,0xe1,0x02,0x00,0x00,0x00,0x00,0x00,0x00,0x1e,0x00,0x00,0x00,0x00,
0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
0x00,0x00,0x00,0x70,0x10,0x00,0x00,0x00,0x00,0x00,0x00,0xd0,0x79,0x00,0x10,0x03,
0x00,0x0c,0x02,0x10,0x09,0xc0,0xe0,0x0e,0x00,0x1e,0x02,0x10,0xf0,0xa0,0x63,0x00,
0x00,0x03,0x42,0x10,0x80,0x21,0x46,0x00,0x00,0x01,0x42,0x10,0x00,0x21,0x44,0x00,
0x00,0x01,0x46,0x10,0x00,0x21,0x88,0x00,0x00,0x03,0x65,0x10,0x00,0x61,0x8c,0x00,
0x00,0x86,0xe5,0x10,0xf0,0xc1,0x07,0x01,0x00,0xf8,0xbc,0x14,0x00,0x00,0x00,0x00,
0x00,0x00,0x18,0x07,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
};
bitmap_t Cu64l = wn_static_bm(64, 64, 1, 4, 4, BM_BIT0_LEFT, (unsigned short *)Cu64l_data);

unsigned char Cu64r_data[] = {
0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
0x00,0x80,0x03,0x00,0x00,0x18,0x00,0x00,0xc0,0x7f,0x08,0x00,0x00,0x00,0x38,0x00,
0x00,0x04,0x08,0x02,0x0c,0x10,0x08,0x00,0x00,0x04,0x08,0x80,0x07,0x10,0x18,0x00,
0x00,0x04,0x38,0x80,0x00,0x10,0x70,0x00,0x00,0x04,0x38,0x82,0x07,0x10,0xc0,0x00,
0x00,0x04,0x68,0x02,0x0c,0x60,0xc0,0x00,0x00,0x04,0x48,0x06,0x0c,0x60,0x7c,0x00,
0x00,0x04,0x4c,0x8c,0x07,0x00,0x00,0x00,0x00,0x04,0x44,0x0c,0x00,0x00,0x00,0x00,
0x00,0x04,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x30,
0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x18,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x0c,
0x00,0x00,0x00,0x00,0x00,0xf0,0x00,0x04,0x00,0x00,0x00,0x00,0x00,0x88,0x30,0x08,
0x00,0x00,0x00,0x00,0x06,0x08,0x11,0x08,0x00,0x00,0x40,0x00,0x1e,0x88,0x09,0xc8,
0x00,0x00,0x40,0x00,0x32,0xc8,0x08,0x68,0x60,0x00,0x40,0x00,0x22,0x78,0x08,0x38,
0xf0,0x00,0x40,0x00,0x62,0x68,0x08,0x11,0x10,0x01,0x40,0x00,0x7e,0xc8,0x18,0x13,
0x10,0x01,0x40,0x00,0x86,0x88,0x31,0x11,0x10,0x01,0x40,0x00,0x82,0x09,0xe7,0x11,
0x30,0x03,0x40,0x00,0x02,0x09,0x00,0xd0,0xc0,0x07,0xc0,0x00,0x02,0x08,0x00,0x30,
0x00,0x18,0x80,0xe1,0x02,0x00,0x00,0x00,0x00,0x00,0x00,0x1e,0x00,0x00,0x00,0x00,
0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
0x00,0x00,0x00,0x70,0x10,0x00,0x00,0x00,0x00,0x00,0x00,0xd0,0x79,0x00,0x10,0x03,
0x00,0x0c,0x02,0x10,0x09,0xc0,0xe0,0x0e,0x00,0x1e,0x02,0x10,0xf0,0xa0,0x63,0x00,
0x00,0x03,0x42,0x10,0x80,0x21,0x46,0x00,0x00,0x01,0x42,0x10,0x00,0x21,0x44,0x00,
0x00,0x01,0x46,0x10,0x00,0x21,0x88,0x00,0x00,0x03,0x65,0x10,0x00,0x61,0x8c,0x00,
0x00,0x86,0xe5,0x10,0xf0,0xc1,0x07,0x01,0x00,0xf8,0xbc,0x14,0x00,0x00,0x00,0x00,
0x00,0x00,0x18,0x07,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
};
bitmap_t Cu64r = wn_static_bm(64, 64, 1, 4, 4, BM_BIT0_RIGHT, (unsigned short *)Cu64r_data);

static unsigned short cu_link_data[] = {
	0x8002,0x4004,0x2008,0x1010,0x0920,0x0540,0x0380,0x1FF0,
	0x0380,0x0540,0x0920,0x1010,0x2008,0x4004,0x8002,0x0000
};
bitmap_t Nbic_bm = wn_static_bm(16, 16, 1, 8, 8, BM_BIT0_RIGHT, cu_link_data);

bitmap_t *Cursors[] = {
#define CN_8L	0
	&Cu8l,

#define CN_8R	1
	&Cu8r,

#define CN_16L	2
	&Cu16l,

#define CN_16R	3
	&Cu16r,

#define CN_64L	4
	&Cu64r,

#define CN_64R	5
	&Cu64l,
};

#define BG_PIXEL	(-1)
#define FG_PIXEL	(-2)

static struct imagest {
	bitmap_t *i_bm;
	int i_fg;
	int i_bg;
} Images[] = {
	&Cu64r, MAGENTA, BG_PIXEL,
	&Cu64r, BG_PIXEL, MAGENTA,
	&Cu64r, MAGENTA, FG_PIXEL,
	&Cu64r, FG_PIXEL, MAGENTA,
	&Cu64r, FG_PIXEL, BG_PIXEL,
	&Cu64r, BG_PIXEL, FG_PIXEL,
};

#define N_IMAGES	(sizeof(Images) / sizeof(Images[0]))

static int
pixnum(n)
int n;
{
	switch(n) {
	case BG_PIXEL:
		return WN_BG;
	case FG_PIXEL:
		return WN_FG;
	default:
		return Colors[n].co_pixel;
	}
}

static void
init_colors()
{
	int i;

	if (getenv("TESTCOLOR") != NULL) {
		if (wn_get_pixels_by_color(Colors, NCOLORS) != 0) {
			fprintf(stderr, "get colors failed\n");
			abort();
		}
	}
	else {
		for (i = 0; i < NCOLORS; i++)
			Colors[i].co_pixel = WN_FG;
	}
}

static void
test_shade(wn)
int wn;
{
	static int shades[] = { WN_GREY0, WN_GREY2, WN_GREY4, WN_GREY6,
				WN_GREY8, WN_GREY10, WN_GREY12, WN_GREY14 };
	int i;
	event_t event;

	wn_tputs(wn, "this is a line of text to test shading", 0, 0);
	for (i = 0; i < sizeof(shades) / sizeof(shades[0]); i++) {
		wn_shade_area(wn, i * 50, 0, 40, 40, shades[i], R_AND);
		wn_shade_area(wn, i * 50, i * 50, 40, 40, shades[i], R_RPL);
	}
	wn_next_event(wn, EV_BUTTON_UP, &event);
}

static void
dump_buttons(buttons)
int buttons;
{
	if (buttons & B_SHIFT_KEY)
		putchar('S');
	if (buttons & B_CONTROL_KEY)
		putchar('C');
		
	if (buttons & B_LEFT)
		putchar('l');
	if (buttons & B_MIDDLE)
		putchar('m');
	if (buttons & B_RIGHT)
		putchar('r');
}

/*  Dump an event in ascii
 */
static void
dump_event(ev)
event_t *ev;
{
	fputs(" [", stdout);
	switch(ev->ev_type) {
	case EV_KEY:
		printf("KEY %d '%c'",ev->ev_char,ev->ev_char);
		break;
	case EV_BUTTON_DOWN:
		dump_buttons(ev->ev_flags);
		fputs(" DOWN -> ", stdout);
		dump_buttons(ev->ev_buttons);
		break;
	case EV_BUTTON_UP:
		dump_buttons(ev->ev_flags);
		fputs(" UP -> ", stdout);
		dump_buttons(ev->ev_buttons);
		break;
	case EV_MOUSE_MOVED:
		printf("MOVED %d,%d,",ev->ev_x,ev->ev_y);
		dump_buttons(ev->ev_buttons);
		break;
	case EV_WINDOW_RESIZED:
		fputs("RESIZED", stdout);
		break;
	case EV_WINDOW_EXPOSED:
		fputs("EXPOSED", stdout);
		break;
	case EV_WINDOW_SELECTED:
		fputs("SELECTED", stdout);
		break;
	case EV_WINDOW_DESELECTED:
		fputs("DESELECTED", stdout);
		break;
	case EV_INTERRUPT:
		fputs("INTERRUPT", stdout);
		break;
	case EV_OTHER_INPUT:
		printf("OTHER_INPUT 0%o",ev->ev_fdmask);
		break;
	case EV_OTHER:
		fputs("OTHER", stdout);
		break;
	default:
		printf("UNKNOWN 0%o",ev->ev_type);
	}
	putchar(']');
	fflush(stdout);
}

/*  Values for rb_flags.
 */
#define RBF_XREL	01	/*  x coord is relative to that of rb_relto */
#define RBF_YREL	02	/*  ditto for y coord */
#define RBF_ON		04	/*  button is lit up. */
#define RBF_SKIP	010	/*  don't draw this button */
#define RBF_LAST	020	/*  this button is relative to the last */

#define RBF_REL		(RBF_XREL | RBF_YREL)
#define RBF_REL_LAST	(RBF_REL | RBF_LAST)

#define RBD_HEIGHT	15
#define RBD_WIDTH	RBD_HEIGHT
#define RBD_CAP_LGAP	10

#define RBD_HBORDER	3
#define RBD_VBORDER	3

#define RBD_HGAP		75
#define RBD_VGAP		50

#define RBR_CLEAR	0
#define RBR_SET		1

static bool Toggles[] = {
#define WANT_X_COORD	0
	FALSE,

#define WANT_Y_COORD	1
	FALSE,

#define WANT_COUNT	2
	FALSE,

#define WANT_BUTTONS	3
	FALSE,

#define TEXT_TOGGLE	4
	TRUE,
};

static char X_caption[] = "x:", Y_caption[] = "y:", Count_caption[] = "count:";
static char Buttons_caption[] = "buttons:";
static char Text_caption[] = "text: ";

/*  Groups;
 */
#define GR_ROP		(-4)
#define GR_COORD	(-3)
#define GR_SETREL	(-2)
#define GR_NONE		(-1)
#define GR_CU_TYPE	0
#define GR_SET		1
#define GR_FAKE		2
#define GR_MODE		3
#define GR_SETPOS	4
#define GR_COORDS	5
#define GR_CURSOR  	6
#define GR_MASK_CURSOR 	7

#define SET_P1		0
#define SET_P2		2

rb_t Sc_rbtab[] = {
	{
		NULL,		GR_NONE,-1,			NULL,
		RBF_SKIP,		UNSET,UNSET,		100,100
/* ------------------------------------------------------------------------*/
	}, {
		"off",		GR_CU_TYPE,WN_SC_OFF,		set_sc,
		RBF_REL|RBF_ON,		GR_NONE,-1,		RBD_HGAP,0
	}, {
		"box",		GR_CU_TYPE,WN_SC_RECT,		set_sc,
		RBF_REL_LAST,		UNSET,UNSET,		RBD_HGAP,0
	}, {
		"line",		GR_CU_TYPE,WN_SC_LINE,		set_sc,
		RBF_REL_LAST,		UNSET,UNSET,		RBD_HGAP,0
	}, {
		"cross",	GR_CU_TYPE,WN_SC_CROSS,		set_sc,
		RBF_REL_LAST,		UNSET,UNSET,		RBD_HGAP,0
/* ------------------------------------------------------------------------*/
	}, {
		"x1r",		GR_SETREL,WN_SC_X1REL,		set_sc,
		RBF_REL,		GR_CU_TYPE,WN_SC_OFF,	0,RBD_VGAP
	}, {
		"y1r",		GR_SETREL,WN_SC_Y1REL,		set_sc,
		RBF_REL_LAST, 		UNSET,UNSET,		RBD_HGAP,0
	}, {
		"x2r",		GR_SETREL,WN_SC_X2REL,		set_sc,
		RBF_REL_LAST,		UNSET,UNSET,		RBD_HGAP,0
	}, {
		"y2r",		GR_SETREL,WN_SC_Y2REL,		set_sc,
		RBF_REL_LAST, 		UNSET,UNSET,		RBD_HGAP,0
/* ------------------------------------------------------------------------*/

	}, {
		"p1",		GR_SETPOS,SET_P1,		set_sc,
		RBF_REL|RBF_ON,		GR_SETREL,WN_SC_X1REL,	0,RBD_VGAP
	}, {
		"p2",		GR_SETPOS,SET_P2,		set_sc,
		RBF_REL_LAST, 		UNSET,UNSET,		RBD_HGAP,0
/* ------------------------------------------------------------------------*/

	}, {
		"cu8l",		GR_CURSOR,CN_8L,		set_cursor,
		RBF_REL,		GR_SETPOS,SET_P1,	0,RBD_VGAP
	}, {
		"cu8r",		GR_CURSOR,CN_8R,		set_cursor,
		RBF_REL_LAST, 		UNSET,UNSET,		RBD_HGAP,0
	}, {
		"cu16l",	GR_CURSOR,CN_16L,		set_cursor,
		RBF_REL_LAST, 		UNSET,UNSET,		RBD_HGAP,0
	}, {
		"cu16r",	GR_CURSOR,CN_16R,		set_cursor,
		RBF_REL_LAST, 		UNSET,UNSET,		RBD_HGAP,0
	}, {
		"cu64l",	GR_CURSOR,CN_64L,		set_cursor,
		RBF_REL_LAST, 		UNSET,UNSET,		RBD_HGAP,0
	}, {
		"cu64r",	GR_CURSOR,CN_64R,		set_cursor,
		RBF_REL_LAST, 		UNSET,UNSET,		RBD_HGAP,0
/* ------------------------------------------------------------------------*/

	}, {
		"nbic",		GR_MASK_CURSOR,0,	        set_mask_cursor,
		RBF_REL, 		GR_CURSOR,CN_8L,	0,RBD_VGAP
	}, {
		"cross",	GR_MASK_CURSOR,1,	        set_mask_cursor,
		RBF_REL_LAST, 		UNSET,UNSET,		RBD_HGAP,0
	}, {
		"crossmask",	GR_MASK_CURSOR,2,	        set_mask_cursor,
		RBF_REL_LAST, 		UNSET,UNSET,		RBD_HGAP,0

/* ------------------------------------------------------------------------*/

	}, {
		"request",	GR_MODE,WN_REQUEST,		set_mode,
		RBF_REL|RBF_ON,		GR_MASK_CURSOR,0,	0,RBD_VGAP
	}, {
		"sample",	GR_MODE,WN_SAMPLE,		set_mode,
		RBF_REL_LAST, 		UNSET,UNSET,		RBD_HGAP*2,0

/* ------------------------------------------------------------------------*/
	}, {
		X_caption,	GR_COORD,WANT_X_COORD,	toggle_coord,
		RBF_REL,		GR_MODE,WN_REQUEST,	0,RBD_VGAP
	}, {
		Y_caption,	GR_COORD,WANT_Y_COORD,	toggle_coord,
		RBF_REL_LAST, 		UNSET,UNSET,		RBD_HGAP*2,0
	}, {
		Count_caption,	GR_COORD,WANT_COUNT,	toggle_coord,
		RBF_REL_LAST, 		UNSET,UNSET,		RBD_HGAP*2,0
	}, {
/* ------------------------------------------------------------------------*/
		Buttons_caption,GR_COORD,WANT_BUTTONS,	toggle_coord,
		RBF_REL,		GR_COORD,WANT_X_COORD,	0,RBD_VGAP
	}, {
/* ------------------------------------------------------------------------*/
		Text_caption,   GR_COORD,TEXT_TOGGLE,	toggle_coord,
		RBF_REL_LAST|RBF_ON,	UNSET,UNSET,		0,RBD_VGAP,
	}, {
/* ------------------------------------------------------------------------*/
		NULL,		GR_ROP,0,			NULL,
		RBF_SKIP|RBF_REL_LAST,	UNSET,UNSET,		0,RBD_VGAP,
	},
};

#define N_SC_RBS	(sizeof(Sc_rbtab) / sizeof(Sc_rbtab[0]))

int Middle_x = 0, Middle_y = 0;

static int
set_sc(group,arg,was_set)
int group, arg, was_set;
{
	static unsigned type = WN_SC_OFF, flags = 0, to_set = SET_P1;
	static int p1x = 0, p1y = 0, p2x = 0, p2y = 0;
	int res;

	if (group == GR_SET) {
		to_set = arg;
		return RBR_SET;
	}
	else if (group == GR_FAKE) {
		if (to_set == SET_P1) {
			p1x = Middle_x;
			p1y = Middle_y;
		}
		else {
			p2x = Middle_x;
			p2y = Middle_y;
		}
		res = RBR_SET;
	}
	else if (group == GR_CU_TYPE) {
		type = arg;
		res = RBR_SET;
	}
	else {
		flags ^= arg;
		res = was_set ? RBR_CLEAR : RBR_SET;
	}
	wn_spcu(WN_STDWIN,type | flags,p1x,p1y,p2x,p2y);
	return res;
}

/*  ARGSUSED */
static int
toggle_coord(group,arg,was_set)
int group, arg, was_set;
{
	Toggles[arg] = !was_set;
	return was_set ? RBR_CLEAR : RBR_SET;
}

/*  ARGSUSED */
static int
set_mode(group,arg,was_set)
int group, arg, was_set;
{
	wn_inmode(WN_STDWIN,arg);
	return RBR_SET;
}

/*  ARGSUSED */
static int
set_mask_cursor(group,arg,was_set)
int group, arg, was_set;
{
	static cursor_t cidtab[3];

	if (cidtab[arg] == 0) {
		bitmap_t *bm, *mask_bm;

		if (arg == 0)
			bm = &Nbic_bm;
		else {
			Cross_bm.bm_byte_order = BM_LSB_FIRST;
			Cross_bm.bm_xhot = Cross_x_hot;
			Cross_bm.bm_yhot = Cross_y_hot;
			Cross_mask_bm.bm_byte_order = BM_LSB_FIRST;
			bm = &Cross_bm;
		}
		mask_bm = (arg == 2) ? &Cross_mask_bm : NULL;
		cidtab[arg] = wn_create_cursor(bm, mask_bm);
	}
	wn_define_cursor(WN_STDWIN, cidtab[arg]);
	return RBR_SET;
}

/*  ARGSUSED */
static int
set_cursor(group,arg,was_set)
int group, arg, was_set;
{
	wn_set_cursor(WN_STDWIN, Cursors[arg]);
	return RBR_SET;
}

static void
set_rb(wn,rb,colors)
int wn;
rb_t *rb;
int colors;
{
	wn_set_area(wn, rb->rb_cur_x + RBD_HBORDER, rb->rb_cur_y + RBD_VBORDER,
			RBD_WIDTH - 2 * RBD_HBORDER, RBD_HEIGHT - 2 * RBD_VBORDER,
			colors);
}

static void
draw_rb(wn,rbtab,n_rbs)
int wn;
rb_t *rbtab;
int n_rbs;
{
	int x, y, font_offset; 
	register rb_t *rb, *rrb;

	font_offset = (RBD_HEIGHT - wn_get_sysfont()->ft_height) / 2;
	for (rb = rbtab; rb < rbtab + n_rbs; rb++) {
		x = rb->rb_x;
		y = rb->rb_y;
		if (rb->rb_flags & RBF_REL) {
			rrb = rb;
			if (rb->rb_flags & RBF_LAST)
				--rrb;
			else {
				while (--rrb >= rbtab)
					if (rrb->rb_group == rb->rb_rel_group &&
					    rrb->rb_arg == rb->rb_rel_arg)
						break;
			}
			if (rrb < rbtab) {
				fprintf(stderr,"bad rel for rb %d",rb - rbtab);
				abort();
			}
			if (rb->rb_flags & RBF_XREL)
				x += rrb->rb_cur_x;
			if (rb->rb_flags & RBF_YREL)
				y += rrb->rb_cur_y;
		}
		rb->rb_cur_x = x;
		rb->rb_cur_y = y;
		if ((rb->rb_flags & RBF_SKIP) == 0) {
			wn_box_round(wn,x,y,RBD_WIDTH,RBD_HEIGHT,
							Colors[MAGENTA].co_pixel);
			wn_text(wn, (font_t *)NULL, rb->rb_cap,
						x + RBD_WIDTH + RBD_CAP_LGAP,
						y + font_offset,
						Colors[RED].co_pixel,
						WN_TRANSPARENT,
						WN_USE_TOP);
			if (rb->rb_flags & RBF_ON)
				set_rb(wn,rb,Colors[GREEN].co_pixel);
		}
	}
}

static int
get_rb_pos(rbtab,n_rbs,group,arg,p_x,p_y)
rb_t *rbtab;
int n_rbs, group, arg;
int *p_x, *p_y;
{
	int i;

	for (i = 0; i < n_rbs; i++)
		if (rbtab[i].rb_group == group && rbtab[i].rb_arg == arg) {
			*p_x = rbtab[i].rb_cur_x;
			*p_y = rbtab[i].rb_cur_y;
			return 0;
		}
	abort();
	return -1;
}

static void
select_rb(wn,rbtab,n_rbs,x,y)
int wn;
rb_t *rbtab;
int n_rbs, x, y;
{
	register rb_t *rb, *crb;
	int group, val;

	group = -1;
	for (rb = rbtab; rb < rbtab + n_rbs; rb++) {
		if (x >= rb->rb_cur_x && x < rb->rb_cur_x + RBD_WIDTH &&
		    y >= rb->rb_cur_y && y < rb->rb_cur_y + RBD_HEIGHT) {
			val = (*rb->rb_selfunc)(rb->rb_group,rb->rb_arg,
							rb->rb_flags & RBF_ON);
			if (val == RBR_SET) {
				set_rb(wn,rb,Colors[GREEN].co_pixel);
				rb->rb_flags |= RBF_ON;
				group = rb->rb_group;
			}
			else {
				set_rb(wn,rb,WN_BG);
				rb->rb_flags &= ~RBF_ON;
			}
			break;
		}
	}
	if (group >= 0)
		for (crb = rbtab; crb < rbtab + n_rbs; crb++)
			if (crb != rb && crb->rb_group == group) {
				set_rb(wn,crb,WN_BG);
				rb->rb_flags &= ~RBF_ON;
			}
}

static void
print_args(argc,argv)
int argc;
const char **argv;
{
	const char **sptr;

	if (getenv("PRINTARGS") != NULL) {
		fputs("[ ", stdout);
		for (sptr = argv; *sptr != NULL; sptr++)
			printf("\"%s\" ",*sptr);
		printf(" ] [%d, %d]\n", argc, sptr - argv);
	}
}

#define RT_SRC	014
#define RT_DST	 05

#define BOX_W	16
#define BOX_H	16

#define BOX_HGAP (BOX_W + 15)
#define BOX_VGAP (BOX_H + 5)

static void
square(wn, x, y, x_offs, y_offs, on)
int wn, x, y, x_offs, y_offs, on;
{
	wn_set_area(wn, x + x_offs * BOX_W/2, y + y_offs * BOX_H/2,
				BOX_W/2, BOX_H/2, on ? Colors[GREEN].co_pixel : WHITE);
}

static void
draw_box(wn, pat, x, y)
int wn, pat, x, y;
{
	wn_box_round(wn, x, y, BOX_W, BOX_H, Colors[YELLOW].co_pixel);
	square(wn, x, y, 0, 0, pat & 1);
	square(wn, x, y, 0, 1, pat & 2);
	square(wn, x, y, 1, 0, pat & 4);
	square(wn, x, y, 1, 1, pat & 8);
}

static void
draw_rop_boxes(wn, x, y)
int wn, x, y;
{
	static struct rtabst {
		int rt_func;
		int rt_expected;
		const char *rt_name;
	} rtab[] = {
		R_RPL,			  RT_SRC,	"=",
		R_NOT,			 ~RT_SRC,	"=~",
		R_AND,		RT_DST &  RT_SRC,	"&",
		R_ANDNOT,	RT_DST & ~RT_SRC,	"&~",
		R_OR,		RT_DST |  RT_SRC,	"|",
		R_ORNOT,	RT_DST | ~RT_SRC,	"|~",
		R_XOR,		RT_DST ^  RT_SRC,	"^",
		R_XNOR,		RT_DST ^ ~RT_SRC,	"^~",
	};
	int i, src;

	src = RT_SRC;
	for (i = 0; i < 8; i++, x += BOX_HGAP) {
		draw_box(wn, src, x, y);
		wn_mono_text(wn, (font_t *)NULL, rtab[i].rt_name, x, y + BOX_VGAP,
						rtab[i].rt_func, WN_USE_TOP);
		draw_box(wn, RT_DST, x, y + BOX_VGAP * 2);
		draw_box(wn, 0,	 x, y + BOX_VGAP * 3);
		wn_mono_rop(wn, x, y + BOX_VGAP * 2, BOX_W, BOX_H,
					x, y + BOX_VGAP * 3, R_RPL);
		wn_mono_rop(wn, x, y, BOX_W, BOX_H,
				x, y + BOX_VGAP * 3, rtab[i].rt_func);
		draw_box(wn, rtab[i].rt_expected, x, y + BOX_VGAP * 4);
		/* src = ~src; */
	}
}

static void
drag_box(wn,x,y)
int wn, x, y;
{
	int t, width, height, new_x, new_y, old_x, old_y, done_one_loop;
	bitmap_t *bm;
	static int rfunc = -1;

	if (rfunc == -1) {
		if (getenv("RFUNC") != NULL)
			rfunc = atoi(getenv("RFUNC"));
		else
			rfunc = R_XOR;
	}
	wn_sc_rbox(wn, x, y);
	while (wn_getpuck(wn, &new_x, &new_y) & B_RIGHT)
		;
	wn_sc_off(wn);
	if (new_x == x || new_y == y)
		return;
	if (new_x < x) {
		t = x;
		x = new_x;
		new_x = t;
	}
	if (new_y < y) {
		t = y;
		y = new_y;
		new_y = t;
	}
	width = new_x - x;
	height = new_y - y;
	bm = wn_make_bitmap(width, height, wn_get_nplanes(), 
				BM_NATURAL_BIT_ORDER, BM_NATURAL_PIXEL_FORMAT);
	if (bm == NULL) {
		fprintf(stderr,"wn_make_bitmap failed - aborting\n");
		abort();
	}
	wn_rop_to_mem(wn, x, y, width, height, bm, 0, 0, R_RPL);
	done_one_loop = FALSE;
	old_x = old_y = -1;
	while ((wn_getpuck(wn, &x, &y) & B_ANY) == 0) {
		if (x != old_x || y != old_y) {
			if (done_one_loop && rfunc == R_XOR)
				wn_rop_from_mem(bm, 0, 0, width, height,
							wn, old_x, old_y, rfunc);
			wn_rop_from_mem(bm, 0, 0, width, height, wn, x, y, rfunc);
			old_x = x;
			old_y = y;
			done_one_loop = TRUE;
		}
	}
	if (done_one_loop && rfunc == R_XOR)
		wn_rop_from_mem(bm, 0, 0, width, height, wn, old_x, old_y, rfunc);
	wn_free_bitmap(bm);
}

#define X_OFFS	10
#define Y_OFFS	20

#define N_SLOTS	30
#define SLOT_W	50

#define MARGIN	20
#define IMARG	2

static void
test_subwins(wn)
int wn;
{
	char buf[10];
	int x, y, mwn, last_wn, w, h, slot_h, i;
	static int types[] = { WN_OUTPUT_ONLY, WN_INPUT_OUTPUT };
	int wntab[N_SLOTS];
	long last_data, data;
	font_t *font;
	event_t event;

	font = wn_get_sysfont();
	slot_h = font->ft_height + IMARG * 2;

	w = SLOT_W + 2 * MARGIN;
	h = (N_SLOTS * slot_h) + 2 * MARGIN;
	mwn = wn_create_subwin(wn, X_OFFS, Y_OFFS, w, h, WN_INPUT_OUTPUT);

	x = IMARG;
	w = SLOT_W - 2 * IMARG;
	h = slot_h - 2 * IMARG;
	for (i = 0, y = IMARG; i < N_SLOTS; i++, y += slot_h)
		wntab[i] = wn_create_subwin(mwn, x, y, w, h, types[i % 2]);
	wn_updating_off(wn);
	x = IMARG;
	w = SLOT_W - 2 * IMARG;
	h = slot_h - 2 * IMARG;
	for (i = 0, y = IMARG; i < N_SLOTS; i++, y += slot_h) {
		wn_set_win_data(wntab[i], i + 1);
		wn_box_round(mwn, x, y, w, h, WN_FG);
		sprintf(buf, "%d", i);
		wn_tputs(wntab[i], buf, w - wn_strwidth(buf, font),
					(h - font->ft_height) / 2);
	}
	wn_updating_on(wn);
	last_data = -1;
	last_wn = -1;
	for (;;) {
		wn_next_event(WN_ANY, EV_BUTTON_UP|EV_BUTTON_DOWN|EV_MOUSE_MOVED|EV_WINDOW_SELECTED|EV_WINDOW_DESELECTED,
									&event);
		if (event.ev_type == EV_BUTTON_UP && !(event.ev_buttons & B_LEFT))
			break;
		data = wn_get_win_data(event.ev_wn);
		if (data != last_data && data != 0) {
			if (last_wn != -1) {
				wn_get_window_size(last_wn, &w, &h);
				wn_set_area(last_wn, 0, 0, w, h, WN_INVERT);
			}
			wn_get_window_size(event.ev_wn, &w, &h);
			wn_set_area(event.ev_wn, 0, 0, w, h, WN_INVERT);
			last_data = data;
			last_wn = event.ev_wn;
		}
	}
	wn_close_window(mwn);
}

static int Got_sigterm = 0;

static void
catch_sigterm(unused_sig)
int unused_sig;
{
	Got_sigterm = 1;
}

static int
deiconise(wn)
int wn;
{
	if (getenv("NODEICONISE") == NULL)
		return wn_deiconise(wn);
	return FALSE;
}

#define TWOWIN_MASK	 (EV_BUTTON_UP | EV_BUTTON_DOWN | EV_MOUSE_MOVED | \
				EV_WINDOW_EXPOSED | EV_WINDOW_RESIZED | \
				EV_WINDOW_SELECTED | EV_WINDOW_DESELECTED)

static int
run_multiple_windows(wntab, nwins)
int *wntab;
int nwins;
{
	int i, x[MAXWINS], y[MAXWINS], color[MAXWINS], w, h;
	int wincount, event_wn;
	event_t event;

	if (nwins > MAXWINS) {
		fprintf(stderr, "too many windows (got %d, max %d)\n",
								nwins, MAXWINS);
		return -1;
	}

	for (i = 0; i < nwins; ++i) {
		x[i] = -1;
		color[i] = WN_FG;
	}

	event_wn = WN_ANY;
	wincount = nwins;
	for (;;) {
		wn_next_event(event_wn, TWOWIN_MASK, &event);
		for (i = 0; i < nwins; ++i) {
			if (event.ev_wn == wntab[i])
				break;
		}
		if (i == nwins) {
			fprintf(stderr, "ev_wn botch\n");
			abort();
		}

		if (event.ev_type == EV_WINDOW_DESELECTED) {
			x[i] = -1;
			continue;
		}
		if (event.ev_type & (EV_WINDOW_EXPOSED | EV_WINDOW_RESIZED)) {
			wn_get_window_size(event.ev_wn, &w, &h);
			wn_set_area(event.ev_wn, 0, 0, w, h, color[i]);
			color[i] = (color[i] == WN_FG) ? WN_BG : WN_FG;
			continue;
		}
		if (event.ev_buttons & B_RIGHT) {
			wn_close_window(event.ev_wn);
			if (--wincount == 0)
				break;
		}
		if (event.ev_buttons & B_MIDDLE) {
			event_wn = (event_wn == WN_ANY) ? event.ev_wn : WN_ANY;
			printf("setting event_wn to %d\n", event_wn);
		}
		if (event.ev_buttons & B_LEFT) {
			if (x[i] != -1)
				wn_draw_line(event.ev_wn, x[i], y[i],
						event.ev_x, event.ev_y, color[i]);
			x[i] = event.ev_x;
			y[i] = event.ev_y;
		}
	}
	return 0;
}

static void
test_planes(wn)
int wn;
{
	static char white1[] = "white text - ";
	static char white2[] = "inverted";
	static char yellow1[] = "yellow text - ";
	static char yellow2[] = "inverted";
#define NPIXELS 2
	static color_t colors[] = {
		0, 0, 0, 0,			/* BG */
		0, 0xffff, 0xffff, 0xffff,	/* white */
		0, 0, 0, 0,			/* BG */
		0, 0xffff, 0xffff, 0x0000,	/* yellow */
	};
	int pixels[NPIXELS], planes;
	font_t *font;

	if (wn_get_pixels_and_planes(NPIXELS, 1, TRUE, pixels, &planes) != 0) {
		fprintf(stderr, "wn_get_pixels and planes failed");
		exit(1);
	}
	
	colors[0].co_pixel = WN_BG;
	wn_get_pixel_colors(&colors[0], 1);
	colors[2] = colors[0];

	colors[0].co_pixel = pixels[0];
	colors[1].co_pixel = pixels[0] | planes;
	colors[2].co_pixel = pixels[1];
	colors[3].co_pixel = pixels[1] | planes;
	wn_set_pixel_colors(colors, 4);

	font = wn_get_sysfont();

	wn_text(wn, font, white1, 0, 0,
				colors[1].co_pixel, colors[0].co_pixel, WN_USE_TOP);
	wn_text(wn, font, white2, wn_strwidth(white1, font), 0, 
				colors[1].co_pixel, colors[0].co_pixel, WN_USE_TOP);
	wn_invert_area(wn, wn_strwidth(white1, font), 0,
					wn_strwidth(white2, font), font->ft_height);

	wn_text(wn, font, yellow1, 0, font->ft_height,
				colors[3].co_pixel, colors[2].co_pixel, WN_USE_TOP);
	wn_text(wn, font, yellow2, wn_strwidth(yellow1, font), font->ft_height, 
				colors[3].co_pixel, colors[2].co_pixel, WN_USE_TOP);
	wn_invert_area(wn, wn_strwidth(yellow1, font), font->ft_height,
					wn_strwidth(yellow2, font), font->ft_height);
}
#undef NPIXELS
			
/*  return sys_errlist[errno] if in range
 */
static const char *
reason()
{
	extern char *sys_errlist[];
	extern int errno, sys_nerr;

	return (errno > 0 && errno < sys_nerr) ? sys_errlist[errno]
						: "unknown reason";
} 

static void
draw_string(wn, font, s, x, y, want_textbox, which)
int wn;
font_t *font;
const char *s;
int x, y;
bool want_textbox;
int which;
{
	int i, len, font_y, base_y;


	len = strlen(s);

	font_y = (which == WN_USE_TOP) ? y : y + font->ft_baseline;

	/*  Draw the string onto a solid foreground rectangle so we can see
	 *  exactly which pixels are set.
	 */
	wn_set_area(wn, x - 2, y - 2,
			wn_strwidth(s, font) + 4, font->ft_height + 4,
			want_textbox ? WN_FG : WN_BG);

	wn_xputs(wn, font, s, x, font_y, R_RPL, which);

	/*  Put a diagonal cross at the coords handed to wn_xputs.
	 */
	wn_draw_line(wn, x - 4, font_y - 4, x + 4, font_y + 4, WN_FG);
	wn_draw_line(wn, x + 4, font_y - 4, x - 4, font_y + 4, WN_FG);

	if (want_textbox) {
		for (i = 0; i <= len; ++i) {
			int xpos;

			xpos = x + wn_strnwidth(s, i, font);
			wn_draw_line(wn, xpos, y, xpos, y + font->ft_height, WN_FG);
		}
	}

	base_y = y + font->ft_baseline;

	wn_draw_line(wn, x - 8, base_y, x - 4, base_y, WN_FG);
	wn_draw_line(wn, x + wn_strwidth(s, font) + 4, base_y,
			 x + wn_strwidth(s, font) + 8, base_y, WN_FG);

	/*  Draw a baseline under every second character
	 */
	for (i = 0; i < len; i += 2) {
		int x1, x2;

		x1 = x + wn_strnwidth(s, i, font);
		x2 = x1 + font->ft_width_tab[s[i]];
		wn_draw_line(wn, x1, base_y, x2, base_y, WN_FG);
	}
}

void
main(argc,argv)
int argc;
const char **argv;
{
	int wn, buttons, x, y, count, w, h, font_offset;
	int x_x, x_y, y_x, y_y, count_x, count_y, bu_x, bu_y, tx_x, tx_y;
	bool want_textbox, updating;
	int rop_x, rop_y;
	const char *errmsg;
	char c;
	event_t evbuf;
	char nbuf[20], ch;

	if (getenv("CATCHTERM") != NULL)
		signal(SIGTERM, catch_sigterm);
	
	wn_set_classname("Test");

	print_args(argc,argv);
	argc = wn_munge_args(argc,argv);
	print_args(argc,argv);
	if (getenv("PRINTARGS") != NULL) {
		const char **args;
		int nargs;

		args = wn_unmunge_args(argv, 1);
		for (nargs = 0; args[nargs] != NULL; ++nargs)
			;
		print_args(nargs, args);
	}


	if ((errmsg = wn_open_display()) != NULL) {
		fprintf(stderr, "%s\n", errmsg);
		exit(1);
	}

	if (argc > 1 && strcmp(argv[1], "-I") == 0) {
		_wn_want_own_icon();
		_wn_enable_self_deiconise();
		--argc;
		++argv;
	}

	if (argc > 3 && strcmp(argv[1], "-wpos") == 0) {
		wn_suggest_window_position(atoi(argv[2]), atoi(argv[3]));
		argc -= 3;
		argv += 3;
	}
	if (argc > 3 && strcmp(argv[1], "-wsize") == 0) {
		wn_suggest_window_size(atoi(argv[2]), atoi(argv[3]));
		argc -= 3;
		argv += 3;
	}

	if ((wn = wn_open_stdwin()) == -1) {
		printf("testwn: not running in a window\n");
		exit(1);
	}

	if (argc > 2 && strcmp(argv[1], "-f") == 0) {
		font_t *font;
		const char *fontpath;

		if ((fontpath = getenv("FONTPATH")) != NULL)
			wn_add_font_path(fontpath);
		if ((font = wn_open_font(argv[2])) == NULL) {
			fprintf(stderr, "Can't open font file %s (%s)\n",
								argv[2], reason());
			exit(1);
		}
		wn_set_sysfont(font);
	}

	init_colors();

	if (getenv("TESTPICS") != NULL) {
		int i, ly;
		bitmap_t *bm;

		ly = 0;
		for (i = 0; i < N_IMAGES; i++) {
			ly += 10;
			bm = Images[i].i_bm;
			wn_put_image(bm,
				     0, 0,
				     bm->bm_width, bm->bm_height,
				     wn,
				     10, ly,
				     R_RPL,
				     pixnum(Images[i].i_fg),
				     pixnum(Images[i].i_bg));

			ly += bm->bm_height;
		}
		wn_next_event(wn, EV_BUTTON_UP, &evbuf);
		exit(0);
	}

	wn_set_deiconise_func(wn, deiconise);

	if (argc > 2 && strcmp(argv[1], "-nwins") == 0) {
		int nwins, i;
		int wntab[MAXWINS];

		nwins = atoi(argv[2]);
		if (nwins >= MAXWINS) {
			fprintf(stderr, "too many windows - max %d\n", MAXWINS);
			exit(1);
		}
		wntab[0] = wn;
		for (i = 1; i < nwins; ++i) {
			char tag[50], wname[50];

			(void) sprintf(wname, "win%02d", i);
			if ((wntab[i] = wn_create_window(wname)) == -1) {
				fprintf(stderr, "Can't create window #%d\n", i);
				exit(1);
			}
			(void) sprintf(tag, "Window #%d", i);
			wn_tputs(wntab[i], tag, 10, 10);
		}
		if (run_multiple_windows(wntab, nwins) != 0)
			exit(1);
		exit(0);
	}

	if (getenv("REPORTWN") != NULL)
		printf("fd mask:%d\n", wn_get_wn_fds());
redraw:
	wn_get_window_size(wn,&w,&h);
	wn_updating_off(wn);
	if (wn_get_nplanes() == 1 || getenv("TESTCOLOR") == NULL)
		wn_set_area(wn,0,0,w,h,WHITE);
	else {
		wn_set_area(wn, 0, 0, w, h/3, Colors[FGREEN].co_pixel);
		wn_set_area(wn, 0, h/3, w, h/3, Colors[KHAKI].co_pixel);
		wn_set_area(wn, 0, (h+h)/3, w, h/3, WN_BG);
		wn_set_area(wn, (w+w)/5, 0, w/5, h, Colors[RED].co_pixel);
	}
	draw_rb(wn,Sc_rbtab, N_SC_RBS);
	get_rb_pos(Sc_rbtab, N_SC_RBS, GR_COORD, WANT_X_COORD, &x_x, &x_y);
	get_rb_pos(Sc_rbtab, N_SC_RBS, GR_COORD, WANT_Y_COORD, &y_x, &y_y);
	get_rb_pos(Sc_rbtab, N_SC_RBS, GR_COORD, WANT_COUNT,&count_x,&count_y);
	get_rb_pos(Sc_rbtab, N_SC_RBS, GR_COORD, WANT_BUTTONS,&bu_x,&bu_y);
	get_rb_pos(Sc_rbtab, N_SC_RBS, GR_COORD, TEXT_TOGGLE,&tx_x,&tx_y);
	get_rb_pos(Sc_rbtab, N_SC_RBS, GR_ROP,   0,		 &rop_x,&rop_y);
	draw_rop_boxes(wn, rop_x, rop_y);
	wn_updating_on(wn);
	x_x += RBD_WIDTH + RBD_CAP_LGAP + wn_strwidth(X_caption,(font_t *)NULL);
	y_x += RBD_WIDTH + RBD_CAP_LGAP + wn_strwidth(Y_caption,(font_t *)NULL);
	count_x += RBD_WIDTH + RBD_CAP_LGAP + wn_strwidth(Count_caption,(font_t *)NULL);
	bu_x += RBD_WIDTH + RBD_CAP_LGAP + wn_strwidth(Buttons_caption,(font_t *)NULL);
	tx_x += RBD_WIDTH + RBD_CAP_LGAP + wn_strwidth(Text_caption,(font_t *)NULL);
	font_offset = (RBD_HEIGHT - wn_get_sysfont()->ft_height) / 2;
	x_y += font_offset;
	y_y += font_offset;
	count_y += font_offset;
	bu_y += font_offset;
	tx_y += font_offset;
	count = 0;
	if (getenv("DUMPEVENTS") != NULL) {
		wn_set_fd_mask(01);
		for (;;) {
			wn_next_event(wn, ~0, &evbuf);
			dump_event(&evbuf);
			if (evbuf.ev_type == EV_KEY && evbuf.ev_char == 'q')
				break;
			if (evbuf.ev_type == EV_OTHER_INPUT) {
				if (read(0, &c, 1) != 1)
					abort();
				fprintf(stderr,"'%c'",c);
			}
		}
		wn_close_window(wn);
		exit(0);
	}
	if (getenv("TESTSUBWINS") != NULL)
		test_subwins(wn);
	if (getenv("TESTSHADE") != NULL)
		test_shade(wn);
	if (getenv("TESTPLANES") != NULL)
		test_planes(wn);
	updating = TRUE;
	want_textbox = !Toggles[TEXT_TOGGLE];
	for(;;) {
		buttons = wn_getpuck(wn,&x,&y);
		if (wn_get_resize_event(wn) != 0) {
			if (!updating) {
				wn_updating_on(wn);
				updating = TRUE;
			}
			goto redraw;
		}
		if (Toggles[TEXT_TOGGLE] != want_textbox) {
			font_t *f1, *f2;
			int lx;
			static const char s[] = "{{00aabbggiijjkkllppyy##@@}}";

			want_textbox = Toggles[TEXT_TOGGLE];
			f1 = wn_get_sysfont();
			wn_set_sysfont((font_t *)NULL);
			f2 = wn_get_sysfont();
			wn_set_sysfont(f1);

			lx = tx_x;
			draw_string(wn, f1, s, lx, tx_y, want_textbox, WN_USE_TOP);
			lx += wn_strwidth(s, f1) + 8;
			draw_string(wn, f1, s, lx, tx_y, want_textbox,
								   WN_USE_BASELINE);
			lx += wn_strwidth(s, f1) + 8;
			draw_string(wn, f2, s, lx, tx_y, want_textbox,
								   WN_USE_BASELINE);
			lx += wn_strwidth(s, f2) + 8;
			draw_string(wn, f2, s, lx, tx_y, want_textbox, WN_USE_TOP);
		}
		if (Toggles[WANT_X_COORD]) {
			sprintf(nbuf,"%d     ",x);
			wn_text(wn,(font_t *)NULL,nbuf,x_x,x_y,
					Colors[RED].co_pixel,WN_BG,WN_USE_TOP);
		}
		if (Toggles[WANT_Y_COORD]) {
			sprintf(nbuf,"%d     ",y);
			wn_text(wn,(font_t *)NULL,nbuf,y_x,y_y,
					Colors[YELLOW].co_pixel,WN_BG,WN_USE_TOP);
		}
		if (Toggles[WANT_COUNT]) {
			sprintf(nbuf,"%d",count);
			wn_text(wn,(font_t *)NULL,nbuf,count_x,count_y,
					Colors[FGREEN].co_pixel,WN_BG,WN_USE_TOP);
		}
		if (Toggles[WANT_BUTTONS]) {
			sprintf(nbuf,"%4o",buttons);
			wn_text(wn,(font_t *)NULL,nbuf,bu_x,bu_y,
							WN_FG,WN_BG,WN_USE_TOP);
		}
		count++;
		if (buttons & B_MIDDLE) {
			Middle_x = x;
			Middle_y = y;
			(void) set_sc(GR_FAKE,UNSET,UNSET);
		}
		if ((buttons & B_LEFT) && (buttons & B_STATE_CHANGE))
			select_rb(wn,Sc_rbtab,N_SC_RBS,x,y);
		if (buttons & B_RIGHT)
			drag_box(wn,x,y);
		if (wn_getc(wn,&ch)) {
			if (ch == 'u') {
				updating = !updating;
				if (updating)
					wn_updating_on(wn);
				else
					wn_updating_off(wn);
				printf("updating %s\n", updating ? "on" : "off");
			}
			if (ch == 'q')
				break;
			if (ch == 'i')
				wn_iconise(wn);
		}
	}
	wn_close_window(wn);
}
