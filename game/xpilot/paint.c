/* $Id: paint.c,v 1.1 1994/02/23 14:40:06 jkh Exp $
 *
 * XPilot, a multiplayer gravity war game.  Copyright (C) 1991-93 by
 *
 *      Bjørn Stabell        (bjoerns@staff.cs.uit.no)
 *      Ken Ronny Schouten   (kenrsc@stud.cs.uit.no)
 *      Bert Gÿsbers         (bert@mc.bio.uva.nl)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <X11/Intrinsic.h>
#include <X11/Xproto.h>
#include <X11/Xlib.h>
#include <X11/Xos.h>

#ifdef VMS
#include <unixio.h>
#include <unixlib.h>
#else
#include <unistd.h>
#endif
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include <time.h>

#include "version.h"
#include "client.h"
#include "const.h"
#include "error.h"
#include "draw.h"
#include "item.h"
#include "paint.h"
#include "xinit.h"
#include "setup.h"
#include "rules.h"
#include "bit.h"
#include "net.h"
#include "netclient.h"

#ifndef ERASE
#define ERASE		0
#endif

#define ERASE_INITIALISED	(1 << 0)
#define ERASE_DAMAGED		(1 << 1)

#define MAX_LINE_WIDTH	4	/* widest line drawn */

#define X(co)  ((int) ((co) - world.x))
#define Y(co)  ((int) (view_height - (co) + world.y))

extern float  		tbl_sin[];
extern wireobj		ships[];
extern setup_t		*Setup;
extern int		RadarHeight;
extern score_object_t	score_objects[MAX_SCORE_OBJECTS];
extern int 		score_object;


/*
 * Globals.
 */
XFontStruct* gameFont;		/* The fonts used in the game */
XFontStruct* messageFont;
XFontStruct* scoreListFont;
XFontStruct* buttonFont;
XFontStruct* textFont;
XFontStruct* talkFont;

char	gameFontName[FONT_LEN];	/* The fonts used in the game */
char	messageFontName[FONT_LEN];
char	scoreListFontName[FONT_LEN];
char	buttonFontName[FONT_LEN];
char	textFontName[FONT_LEN];
char	talkFontName[FONT_LEN];

Display	*dpy;			/* Display of player (pointer) */
short	about_page;		/* Which page is the player on? */
u_short	team;			/* What team is the player on? */

GC	gc;			/* GC for the game area */
GC	messageGC;		/* GC for messages in the game area */
GC	radarGC;		/* GC for the radar */
GC	buttonGC;		/* GC for the buttons */
GC	scoreListGC;		/* GC for the player list */
GC	textGC;			/* GC for the info/keys text */
GC	talkGC;			/* GC for the message window */

Window	top;			/* Top-level window (topshell) */
Window	draw;			/* Main play window */
Pixmap	p_draw;			/* Saved pixmap for the drawing */
					/* area (monochromes use this) */
Window	radar;			/* Radar window */
Window	players;		/* Player list window */
Pixmap	p_radar, s_radar;	/* Pixmaps for the radar (implements */
				/* the planes hack on the radar for */
				/* monochromes) */
long	dpl_1[2], dpl_2[2];	/* Used by radar hack */
Window	keys_w;			/* Help window */
Window	about_w;		/* About window */
Window	about_close_b;		/* About window's close button */
Window	about_next_b;		/* About window's next button */
Window	about_prev_b;		/* About window's previous button */
Window	keys_close_b;		/* Help window's close button */
Window	talk_w;			/* Talk window */
XColor	colors[MAX_COLORS];	/* Colors */
Colormap	colormap;	/* Private colormap */
int	maxColors;		/* Max. number of colors to use */
bool	gotFocus;
bool	radar_exposed;
bool	players_exposed;
short	view_width, view_height;	/* Visible area according to server */
u_byte	debris_colors;		/* Number of debris intensities from server */
u_byte	spark_rand;		/* Sparkling effect */
float	charsPerTick = 0.0;	/* Output speed of messages */

bool	markingLights;

dbuff_state_t   *dbuf_state;	/* Holds current dbuff state */

int		maxKeyDefs;
keydefs_t	*keyDefs;

other_t	*self;			/* player info */

message_t		*Msg[MAX_MSGS];


static void Paint_clock(int redraw);


static int		eyesId;		/* Player we get frame updates for */


/*
 * Local types and data
 */
typedef struct {
    short		x0, y0, x1, y1;
} refuel_t;

typedef struct {
    short		x0, y0, x1, y1;
} connector_t;

typedef struct {
    unsigned char	color, dir;
    short		x, y, len;
} laser_t;

typedef struct {
    short		x, y, dir;
} smart_t;

typedef struct {
    short		x, y, id;
} ball_t;

typedef struct {
    short		x, y, id, dir;
    u_byte		shield, cloak;
} ship_t;

typedef struct {
    short		x, y;
} mine_t;

typedef struct {
    short		x, y, type;
} itemtype_t;

typedef struct {
    short		x, y, color;
} shot_t;

typedef struct {
    short		x, y, size;
} ecm_t;

typedef struct {
    short		x1, y1, x2, y2;
} trans_t;

typedef struct {
    short		x, y, count;
} paused_t;

typedef struct {
    short		x, y;
} radar_t;

typedef struct {
    short		x, y, type;
} vcannon_t;

typedef struct {
    short		x, y;
    long		fuel;
} vfuel_t;

typedef struct {
    short		x, y, xi, yi, type;
} vbase_t;

typedef struct {
    u_byte		x, y;
} debris_t;

#if ERASE
typedef struct {
    int			flags;
    XRectangle		*rect_ptr;
    int			num_rect,
			max_rect;
    XArc		*arc_ptr;
    int			num_arc,
			max_arc;
    XSegment		*seg_ptr[MAX_LINE_WIDTH + 1];
    int			num_seg[MAX_LINE_WIDTH + 1],
			max_seg[MAX_LINE_WIDTH + 1];
} erase_t;

static erase_t		erase[2],
			*erp;
#endif	/* ERASE */

static refuel_t		*refuel_ptr;
static int		 num_refuel, max_refuel;
static connector_t	*connector_ptr;
static int		 num_connector, max_connector;
static laser_t		*laser_ptr;
static int		 num_laser, max_laser;
static smart_t		*smart_ptr;
static int		 num_smart, max_smart;
static ball_t		*ball_ptr;
static int		 num_ball, max_ball;
static ship_t		*ship_ptr;
static int		 num_ship, max_ship;
static mine_t		*mine_ptr;
static int		 num_mine, max_mine;
static itemtype_t	*itemtype_ptr;
static int		 num_itemtype, max_itemtype;
static shot_t		*shot_ptr;
static int		 num_shot, max_shot;
static ecm_t		*ecm_ptr;
static int		 num_ecm, max_ecm;
static trans_t 		*trans_ptr;
static int		 num_trans, max_trans;
static paused_t		*paused_ptr;
static int		 num_paused, max_paused;
static radar_t		*radar_ptr;
static int		 num_radar, max_radar;
static vcannon_t	*vcannon_ptr;
static int		 num_vcannon, max_vcannon;
static vfuel_t		*vfuel_ptr;
static int		 num_vfuel, max_vfuel;
static vbase_t		*vbase_ptr;
static int		 num_vbase, max_vbase;
static debris_t		*debris_ptr[DEBRIS_TYPES];
static int		 num_debris[DEBRIS_TYPES],
			 max_debris[DEBRIS_TYPES];

#define HANDLE(P,N,M,T)							\
    if (N >= M && ((M <= 0)						\
	? (P = (void *) malloc((M = 1) * sizeof(*P)))			\
	: (P = (void *) realloc(P, (M += M) * sizeof(*P)))) == NULL) {	\
	error("No memory");						\
	N = M = 0;							\
	return -1;							\
    } else								\
    	(P[N++] = T)

#ifndef PAINT_FREE
# define PAINT_FREE	1
#endif
#if PAINT_FREE
# define RELEASE(P, N, M)	(free(P), (M) = 0, (N) = 0)
#else
# define RELEASE(P, N, M)	((N) = 0)
#endif

#if ERASE
#define EXPAND(P,N,M,T,E)						\
    if ((N) + (E) > (M)) {						\
	if ((M) <= 0) {							\
	    M = (E) + 2;						\
	    P = (T *) malloc((M) * sizeof(T));				\
	    N = 0;							\
	} else {							\
	    M = ((M) << 1) + (E);					\
	    P = (T *) realloc(P, (M) * sizeof(T));			\
	}								\
	if (P == NULL) {						\
	    error("No memory");						\
	    N = M = 0;							\
	    return;	/* ! */						\
	}								\
    }

#define UNEXPAND(P,N,M)							\
    if ((N) < ((M) >> 2)) {						\
	free(P);							\
	M = 0;								\
    }									\
    N = 0;
#endif	/* ERASE */

static long		loops = 0,
			start_loops,
			end_loops,
			time_left = -1;

static XPoint		points[5];
static XGCValues	gcv;

static XPoint		diamond[] = {
    { 0, -DSIZE },
    { DSIZE, -DSIZE },
    { -DSIZE, -DSIZE },
    { -DSIZE, DSIZE },
    { DSIZE, DSIZE }
};

static XRectangle	*rect_ptr[MAX_COLORS];
static int		num_rect[MAX_COLORS], max_rect[MAX_COLORS];
static XArc		*arc_ptr[MAX_COLORS];
static int		num_arc[MAX_COLORS], max_arc[MAX_COLORS];
static XSegment		*seg_ptr[MAX_COLORS];
static int		num_seg[MAX_COLORS], max_seg[MAX_COLORS];

static Pixel		current_foreground;

#define RESET_FG()	(current_foreground = -1)
#define SET_FG(PIXEL)				\
    if ((PIXEL) == current_foreground) ;	\
    else XSetForeground(dpy, gc, current_foreground = (PIXEL))

#define FIND_NAME_WIDTH(other)						\
    if ((other)->name_width == 0) {					\
	(other)->name_len = strlen((other)->name);			\
	(other)->name_width = XTextWidth(gameFont, (other)->name,	\
					 (other)->name_len);		\
    }

#if ERASE
static void Erase_start(void)
{
    int			i;

    if (erase[0].flags == 0) {
	printf("ERASE is On!\n");
	erp = &erase[0];
    }
    if (BIT(erp->flags, ERASE_INITIALISED) == 0) {
	SET_FG(colors[BLACK].pixel);
	XFillRectangle(dpy, p_draw, gc, 0, 0, draw_width, draw_height);
	SET_BIT(erp->flags, ERASE_INITIALISED);
    }
    erp->num_rect = 0;
    erp->num_arc = 0;
    for (i = 0; i <= MAX_LINE_WIDTH; i++) {
	erp->num_seg[i] = 0;
    }
}

static void Erase_end(void)
{
    int			i,
			linewidth = false;

    if (erp == &erase[0]) {
	erp = &erase[1];
    } else {
	erp = &erase[0];
    }

    if (damaged > 0) {
	return;
    }

    SET_FG(colors[BLACK].pixel);
    if (BIT(erp->flags, ERASE_DAMAGED)) {
	XFillRectangle(dpy, draw, gc, 0, 0, draw_width, draw_height);
	CLR_BIT(erp->flags, ERASE_DAMAGED);
	return;
    }

    if (erp->num_rect != 0) {
	XFillRectangles(dpy, p_draw, gc, erp->rect_ptr, erp->num_rect);
	UNEXPAND(erp->rect_ptr, erp->num_rect, erp->max_rect);
    }
    if (erp->num_arc != 0) {
	XDrawArcs(dpy, p_draw, gc, erp->arc_ptr, erp->num_arc);
	UNEXPAND(erp->arc_ptr, erp->num_arc, erp->max_arc);
    }
    for (i = 0; i <= MAX_LINE_WIDTH; i++) {
	if (erp->num_seg[i] != 0) {
	    if (i != 0) {
		XSetLineAttributes(dpy, gc, i,
				   LineSolid, CapButt, JoinMiter);
		linewidth = true;
	    }
	    XDrawSegments(dpy, p_draw, gc, erp->seg_ptr[i], erp->num_seg[i]);
	    UNEXPAND(erp->seg_ptr[i], erp->num_seg[i], erp->max_seg[i]);
	}
    }
    if (linewidth == true) {
	XSetLineAttributes(dpy, gc, 0,
			   LineSolid, CapButt, JoinMiter);
    }
}

static void Erase_rectangle(int x, int y, int width, int height)
{
    XRectangle		*p;

    EXPAND(erp->rect_ptr, erp->num_rect, erp->max_rect, XRectangle, 1);
    p = &erp->rect_ptr[erp->num_rect++];
    p->x = x;
    p->y = y;
    p->width = width;
    p->height = height;
}

static void Erase_rectangles(XRectangle *rectp, int n)
{
    EXPAND(erp->rect_ptr, erp->num_rect, erp->max_rect, XRectangle, n);
    memcpy(&erp->rect_ptr[erp->num_rect], rectp, n * sizeof(XRectangle));
    erp->num_rect += n;
}

static void Erase_arc(int x, int y, int width, int height,
		      int angle1, int angle2)
{
    XArc		*p;

    EXPAND(erp->arc_ptr, erp->num_arc, erp->max_arc, XArc, 1);
    p = &erp->arc_ptr[erp->num_arc++];
    p->x = x;
    p->y = y;
    p->width = width;
    p->height = height;
    p->angle1 = angle1;
    p->angle2 = angle2;
}

static void Erase_arcs(XArc *arcp, int n)
{
    EXPAND(erp->arc_ptr, erp->num_arc, erp->max_arc, XArc, n);
    memcpy(&erp->arc_ptr[erp->num_arc], arcp, n * sizeof(XArc));
    erp->num_arc += n;
}

static void Erase_segment(int width, int x1, int y1, int x2, int y2)
{
    XSegment		*p;

    EXPAND(erp->seg_ptr[width], erp->num_seg[width], erp->max_seg[width],
	   XSegment, 1);
    p = &erp->seg_ptr[width][erp->num_seg[width]++];
    p->x1 = x1;
    p->y1 = y1;
    p->x2 = x2;
    p->y2 = y2;
}

static void Erase_segments(XSegment *segp, int n)
{
    EXPAND(erp->seg_ptr[0], erp->num_seg[0], erp->max_seg[0],
	   XSegment, n);
    memcpy(&erp->seg_ptr[0][erp->num_seg[0]], segp, n * sizeof(XSegment));
    erp->num_seg[0] += n;
}

static void Erase_points(int width, XPoint *pointp, int n)
{
    XSegment		*p;
    int			i;

    EXPAND(erp->seg_ptr[width], erp->num_seg[width], erp->max_seg[width],
	   XSegment, n - 1);
    p = &erp->seg_ptr[width][erp->num_seg[width]];
    for (i = 1; i < n; i++) {
	p->x1 = pointp->x;
	p->y1 = pointp->y;
	pointp++;
	p->x2 = pointp->x;
	p->y2 = pointp->y;
	p++;
    }
    erp->num_seg[width] += n - 1;
}

static void Erase_4point(int x, int y, int width, int height)
{
    XSegment		*p;

    EXPAND(erp->seg_ptr[0], erp->num_seg[0], erp->max_seg[0],
	   XSegment, 4);
    p = &erp->seg_ptr[0][erp->num_seg[0]];
    p->x1 = x;
    p->y1 = y;
    p->x2 = x + width;
    p->y2 = y;
    p++;
    p->x1 = x + width;
    p->y1 = y;
    p->x2 = x + width;
    p->y2 = y + height;
    p++;
    p->x1 = x + width;
    p->y1 = y + height;
    p->x2 = x;
    p->y2 = y + height;
    p++;
    p->x1 = x;
    p->y1 = y + height;
    p->x2 = x;
    p->y2 = y;
    p++;
    erp->num_seg[0] += 4;
}
#else	/* ERASE */
#define Erase_start()
#define Erase_end()
#define Erase_rectangle(A,B,C,D)
#define Erase_rectangles(A,B)
#define Erase_arc(A,B,C,D,E,F)
#define Erase_arcs(A,B)
#define Erase_segment(A,B,C,D,E)
#define Erase_segments(A,B)
#define Erase_points(A,B,C)
#define Erase_4point(A,B,C,D)
#endif	/* ERASE */

static void Rectangle_start(void)
{
    int i;

    for (i = 0; i < maxColors; i++) {
	num_rect[i] = 0;
    }
}

static void Rectangle_end(void)
{
    int i;

    for (i = 0; i < maxColors; i++) {
	if (num_rect[i] > 0) {
	    SET_FG(colors[i].pixel);
	    XFillRectangles(dpy, p_draw, gc, rect_ptr[i], num_rect[i]);
	    Erase_rectangles(rect_ptr[i], num_rect[i]);
	    RELEASE(rect_ptr[i], num_rect[i], max_rect[i]);
	}
    }
}

static int Rectangle_add(int color, int x, int y, int width, int height)
{
    XRectangle		t;

    t.x = x;
    t.y = y;
    t.width = width;
    t.height = height;
    HANDLE(rect_ptr[color], num_rect[color], max_rect[color], t);
    return 0;
}

static void Arc_start(void)
{
    int i;

    for (i = 0; i < maxColors; i++) {
	num_arc[i] = 0;
    }
}

static void Arc_end(void)
{
    int i;

    for (i = 0; i < maxColors; i++) {
	if (num_arc[i] > 0) {
	    SET_FG(colors[i].pixel);
	    XDrawArcs(dpy, p_draw, gc,
		arc_ptr[i], num_arc[i]);
	    Erase_arcs(arc_ptr[i], num_arc[i]);
	    RELEASE(arc_ptr[i], num_arc[i], max_arc[i]);
	}
    }
}

static int Arc_add(int color,
		   int x, int y,
		   int width, int height,
		   int angle1, int angle2)
{
    XArc t;

    t.x = x;
    t.y = y;
    t.width = width;
    t.height = height;
    t.angle1 = angle1;
    t.angle2 = angle2;
    HANDLE(arc_ptr[color], num_arc[color], max_arc[color], t);
    return 0;
}

static void Segment_start(void)
{
    int i;

    for (i = 0; i < maxColors; i++) {
	num_seg[i] = 0;
    }
}

static void Segment_end(void)
{
    int i;

    for (i = 0; i < maxColors; i++) {
	if (num_seg[i] > 0) {
	    SET_FG(colors[i].pixel);
	    XDrawSegments(dpy, p_draw, gc,
		seg_ptr[i], num_seg[i]);
	    Erase_segments(seg_ptr[i], num_seg[i]);
	    RELEASE(seg_ptr[i], num_seg[i], max_seg[i]);
	}
    }
}

static int Segment_add(int color, int x1, int y1, int x2, int y2)
{
    XSegment t;

    t.x1 = x1;
    t.y1 = y1;
    t.x2 = x2;
    t.y2 = y2;
    HANDLE(seg_ptr[color], num_seg[color], max_seg[color], t);
    return 0;
}

#ifdef SCROLL
static char *scroll(char *string, int start, int length)
{
    static char str[MAX_SCROLL_LEN];
    int i;
    
    for (i=0; string[i+start] && i<length; i++)
	str[i] = string[i+start];
    str[length] = '\0';

    return (str);
}
#endif

#define METER_WIDTH		200
#define METER_HEIGHT		8

static void Paint_meter(int x, int y, char *title, int val, int max)
{
    const int	mw1_4 = METER_WIDTH/4,
    		mw2_4 = METER_WIDTH/2,
		mw3_4 = 3*METER_WIDTH/4,
		mw4_4 = METER_WIDTH,
    		BORDER = 5;

    Rectangle_add(RED,
		  x+2, y+2,
		  (int)(((METER_WIDTH-3)*val)/max), METER_HEIGHT-3);
    SET_FG(colors[WHITE].pixel);
    XDrawRectangle(dpy, p_draw, gc,
		   x, y, METER_WIDTH, METER_HEIGHT);
    Erase_4point(x, y, METER_WIDTH, METER_HEIGHT);

    /* Paint scale levels(?) */
    Segment_add(WHITE, x, y-4,		x,	y+METER_HEIGHT+4);
    Segment_add(WHITE, x+mw4_4, y-4,	x+mw4_4, y+METER_HEIGHT+4);
    Segment_add(WHITE, x+mw2_4, y-3,	x+mw2_4, y+METER_HEIGHT+3);
    Segment_add(WHITE, x+mw1_4, y-1,	x+mw1_4, y+METER_HEIGHT+1);
    Segment_add(WHITE, x+mw3_4, y-1,	x+mw3_4, y+METER_HEIGHT+1);
    XDrawString(dpy, p_draw, gc,
		x+METER_WIDTH+BORDER, y+(gameFont->ascent+METER_HEIGHT)/2,
		title, strlen(title));
    Erase_rectangle(x+METER_WIDTH+BORDER - 1,
		    y+(gameFont->ascent+METER_HEIGHT)/2 - gameFont->ascent,
		    XTextWidth(gameFont, title, strlen(title)) + 2,
		    gameFont->ascent + gameFont->descent);
}

static int wrap(int *x, int *y)
{
    int x1, y1;

    if (*x >= world.x
	&& *x <= world.x + view_width
	&& *y >= world.y
	&& *y <= world.y + view_height) {
	return 1;
    }
    if (BIT(Setup->mode, WRAP_PLAY) && wrappedWorld) {
	if ((wrappedWorld & 1) && *x > view_width) {
	    x1 = *x - Setup->width;
	} else {
	    x1 = *x;
	}
	if ((wrappedWorld & 2) && *y > view_height) {
	    y1 = *y - Setup->height;
	} else {
	    y1 = *y;
	}
	if (x1 >= realWorld.x
	    && x1 <= realWorld.x + view_width
	    && y1 >= realWorld.y
	    && y1 <= realWorld.y + view_height) {
	    
	    if ((wrappedWorld & 1) && *x == x1) {
		*x = x1 + Setup->width;
	    }
	    if ((wrappedWorld & 2) && *y == y1) {
		*y = y1 + Setup->height;
	    }
	    return 1;
	}
    }
#if 0
    errno = 0;
    error("Object (%d, %d) not in view (%d, %d)",
	  *x, *y, pos.x, pos.y);
#endif
    return 0;
}

void Game_over_action(u_byte stat)
{
    static u_byte old_stat = 0;
    
    if (BIT(old_stat, GAME_OVER) && !BIT(stat, GAME_OVER))
	XMapRaised(dpy, top);
    if (toggle_shield) {
	if (BIT(old_stat, PLAYING|PAUSE|GAME_OVER) != PLAYING) {
	    if (BIT(stat, PLAYING|PAUSE|GAME_OVER) == PLAYING) {
		Reset_shields();
	    }
	}
    }

    old_stat = stat;
}

static void Paint_item_symbol(u_byte type, Drawable d, GC gc, int x, int y)
{
    gcv.stipple = itemBitmaps[type];
    gcv.fill_style = FillStippled;
    gcv.ts_x_origin = x;
    gcv.ts_y_origin = y;
    XChangeGC(dpy, gc,
	      GCStipple|GCFillStyle|GCTileStipXOrigin|GCTileStipYOrigin,
	      &gcv);
    XFillRectangle(dpy, d, gc, x, y, ITEM_SIZE, ITEM_SIZE);
    gcv.fill_style = FillSolid;
    XChangeGC(dpy, gc, GCFillStyle, &gcv);
}


void Paint_item(u_byte type, Drawable d, GC gc, int x, int y)
{
    const int		SIZE = ITEM_TRIANGLE_SIZE;

    points[0].x = x - SIZE;
    points[0].y = y - SIZE;
    points[1].x = x;
    points[1].y = y + SIZE;
    points[2].x = x + SIZE;
    points[2].y = y - SIZE;
    points[3] = points[0];
    SET_FG(colors[BLUE].pixel);
    XDrawLines(dpy, d, gc, points, 4, CoordModeOrigin);

    SET_FG(colors[RED].pixel);
#if 0
    str[0] = itemtype_ptr[i].type + '0';
    str[1] = '\0';
    XDrawString(dpy, d, gc,
		x - XTextWidth(gameFont, str, 1)/2,
		y + SIZE - 1,
		str, 1);
#endif
    Paint_item_symbol(type, d, gc, x - ITEM_SIZE/2, y - SIZE + 2);
}


static void Paint_shots(void)
{
    int		color, i, j, id, x, y, xs, ys, x1, x2, y1, y2;
    int		x_areas, y_areas, areas, max, len, dir;

    if (num_itemtype > 0) {
	SET_FG(colors[RED].pixel);
	for (i = 0; i < num_itemtype; i++) {
	    x = itemtype_ptr[i].x;
	    y = itemtype_ptr[i].y;
	    if (wrap(&x, &y)) {
		Paint_item(itemtype_ptr[i].type, p_draw, gc, X(x), Y(y));
		Erase_rectangle(X(x) - ITEM_TRIANGLE_SIZE,
				Y(y) - ITEM_TRIANGLE_SIZE,
				2*ITEM_TRIANGLE_SIZE + 1,
				2*ITEM_TRIANGLE_SIZE + 1);
	    }
	}
	RELEASE(itemtype_ptr, num_itemtype, max_itemtype);
    }

    if (num_shot > 0) {
	for (i = 0; i < num_shot; i++) {
	    x = shot_ptr[i].x;
	    y = shot_ptr[i].y;
	    if (wrap(&x, &y)) {
		Rectangle_add(shot_ptr[i].color, X(x), Y(y), 3, 3);
	    }
	}
	RELEASE(shot_ptr, num_shot, max_shot);
    }

    if (num_ball > 0) {
	for (i = 0; i < num_ball; i++) {
	    x = ball_ptr[i].x;
	    y = ball_ptr[i].y;
	    id = ball_ptr[i].id;
	    if (wrap(&x, &y)) {
		x = X(x);
		y = Y(y);
		Arc_add(WHITE, x - 10, y - 10, 20, 20, 0, 64*360);
		if (ball_ptr[i].id == -1) {
		    continue;
		}
		for (j = 0; j < num_ship; j++) {
		    if (ship_ptr[j].id == id) {
			break;
		    }
		}
		if (j >= num_ship) {
		    continue;
		}
		xs = ship_ptr[j].x;
		ys = ship_ptr[j].y;
		if (wrap(&xs, &ys)) {
		    xs = X(xs);
		    ys = Y(ys);
		    Segment_add(WHITE, x, y, xs, ys);
		}
	    }
	}
	RELEASE(ball_ptr, num_ball, max_ball);
    }

    if (num_mine > 0) {
	static XPoint mine_points[21] = {
	    { 0, 0 },
	    { 1, 0 },
	    { 0, -1 },
	    { 4, 0 },
	    { 0, -1 },
	    { 6, 0 },
	    { 0, 1 },
	    { 4, 0 },
	    { 0, 1 },
	    { 1, 0 },
	    { 0, 2 },
	    { -1, 0 },
	    { 0, 1 },
	    { -4, 0 },
	    { 0, 1 },
	    { -6, 0 },
	    { 0, -1 },
	    { -4, 0 },
	    { 0, -1 },
	    { -1, 0 },
	    { 0, -2 }
	};

	for (i = 0; i < num_mine; i++) {
	    x = mine_ptr[i].x;
	    y = mine_ptr[i].y;
	    if (wrap(&x, &y)) {
		x = X(x);
		y = Y(y);
		mine_points[0].x = x - 8;
		mine_points[0].y = y - 1;
		SET_FG(colors[BLUE].pixel);
		XFillRectangle(dpy, p_draw, gc, x - 7, y - 2, 15, 5);
		SET_FG(colors[WHITE].pixel);
		XDrawLines(dpy, p_draw, gc,
			   mine_points, 21, CoordModePrevious);
		Erase_rectangle(x - 8, y - 3, 17, 7);
		/* Arc_add(BLUE, x - 4, y - 4, 8, 8, 0, 64*360); */
	    }
	}
	RELEASE(mine_ptr, num_mine, max_mine);
    }

    x_areas = (view_width + 255) >> 8;
    y_areas = (view_height + 255) >> 8;
    areas = x_areas * y_areas;
    max = areas * (debris_colors > 0 ? debris_colors : 4);
    for (i = 0; i < max; i++) {
	if (num_debris[i] > 0) {
	    x = ((i % x_areas) << 8);
	    y = (((i / x_areas) % y_areas) << 8);
	    color = i / areas;
	    if (debris_colors > 4) {
		color = 4 + (((color & 1) << 2) | (color >> 1));
	    }
	    else if (debris_colors >= 3) {
		color += 4;
	    }
	    y = view_height - 1 - y;
	    for (j = 0; j < num_debris[i]; j++) {
		Rectangle_add(color,
			      x + debris_ptr[i][j].x - spark_size/2,
			      y - debris_ptr[i][j].y - spark_size/2,
			      spark_size, spark_size);
	    }
	    RELEASE(debris_ptr[i], num_debris[i], max_debris[i]);
	}
    }

    if (num_smart > 0) {
	SET_FG(colors[WHITE].pixel);
	XSetLineAttributes(dpy, gc, 4,
			   LineSolid, CapButt, JoinMiter);
	for (i = 0; i < num_smart; i++) {
	    x = smart_ptr[i].x;
	    y = smart_ptr[i].y;
	    if (wrap(&x, &y)) {
		x1 = X(x);
		y1 = Y(y);
		x2 = (int)(x1 - tcos(smart_ptr[i].dir) * SMART_SHOT_LEN);
		y2 = (int)(y1 + tsin(smart_ptr[i].dir) * SMART_SHOT_LEN);
		XDrawLine(dpy, p_draw, gc, x1, y1, x2, y2);
		Erase_segment(4, x1, y1, x2, y2);
	    }
	}
	XSetLineAttributes(dpy, gc, 0,
			   LineSolid, CapButt, JoinMiter);
	RELEASE(smart_ptr, num_smart, max_smart);
    }

    if (num_laser > 0) {
	XSetLineAttributes(dpy, gc, 3,
			   LineSolid, CapButt, JoinMiter);
	for (i = 0; i < num_laser; i++) {
	    x1 = laser_ptr[i].x;
	    y1 = laser_ptr[i].y;
	    len = laser_ptr[i].len;
	    dir = laser_ptr[i].dir;
	    if (wrap(&x1, &y1)) {
		x2 = x1 + (int)(len * tcos(dir));
		y2 = y1 + (int)(len * tsin(dir));
		if ((unsigned)(color = laser_ptr[i].color) >= NUM_COLORS) {
		    color = WHITE;
		}
		SET_FG(colors[color].pixel);
		XDrawLine(dpy, p_draw, gc,
			  X(x1), Y(y1),
			  X(x2), Y(y2));
		Erase_segment(3, X(x1), Y(y1), X(x2), Y(y2));
	    }
	}
	XSetLineAttributes(dpy, gc, 0,
			   LineSolid, CapButt, JoinMiter);
	RELEASE(laser_ptr, num_laser, max_laser);
    }
}

static void Paint_ships(void)
{
    int			i, x, y, dir, x0, y0, x1, y1, size;
    unsigned long	mask;
    other_t		*other;
    static int		pauseCharWidth = -1;

    gcv.dash_offset = DASHES_LENGTH - (loops % DASHES_LENGTH);
    if (num_paused > 0) {
	const int half_pause_size = 3*BLOCK_SZ/7;

	if (pauseCharWidth < 0) {
	    pauseCharWidth = XTextWidth(gameFont, "P", 1);
	}

	for (i = 0; i < num_paused; i++) {
	    x = paused_ptr[i].x;
	    y = paused_ptr[i].y;
	    if (wrap(&x, &y)) {
		SET_FG(colors[BLUE].pixel);
		x0 = X(x - half_pause_size);
		y0 = Y(y + half_pause_size);
		XFillRectangle(dpy, p_draw, gc,
			       x0, y0,
			       2*half_pause_size+1, 2*half_pause_size+1);
		if (paused_ptr[i].count <= 0 || loops % 10 >= 5) {
		    SET_FG(colors[WHITE].pixel);
		    XDrawRectangle(dpy, p_draw, gc,
				   x0 - 1,
				   y0 - 1,
				   2*(half_pause_size+1),
				   2*(half_pause_size+1));
		    XDrawString(dpy, p_draw, gc,
				X(x - pauseCharWidth/2),
				Y(y - gameFont->ascent/2),
				"P", 1);
		}
		Erase_rectangle(x0 - 1, y0 - 1,
				2*half_pause_size+3, 2*half_pause_size+3);
	    }
	}
	RELEASE(paused_ptr, num_paused, max_paused);
    }

    if (num_ecm > 0) {
	for (i = 0; i < num_ecm; i++) {
	    if ((size = ecm_ptr[i].size) > 0) {
		x = ecm_ptr[i].x;
		y = ecm_ptr[i].y;
		if (wrap(&x, &y)) {
		    Arc_add(WHITE,
			    X(x - size / 2),
			    Y(y + size / 2),
			    size, size, 0, 64 * 360);
		}
	    }
	}
	RELEASE(ecm_ptr, num_ecm, max_ecm);
    }

    if (num_ship > 0) {
	SET_FG(colors[WHITE].pixel);
	for (i = 0; i < num_ship; i++) {
	    x = ship_ptr[i].x;
	    y = ship_ptr[i].y;
	    if (wrap(&x, &y)) {
		dir = ship_ptr[i].dir;
		points[0].x = X(x + ships[dir].pts[0].x);
		points[0].y = Y(y + ships[dir].pts[0].y);
		points[1].x = X(x + ships[dir].pts[1].x);
		points[1].y = Y(y + ships[dir].pts[1].y);
		points[2].x = X(x + ships[dir].pts[2].x);
		points[2].y = Y(y + ships[dir].pts[2].y);
		points[3] = points[0];

		/*
		 * Determine if the name of the player should be drawn below
		 * his/her ship.
		 */
		if (BIT(instruments, SHOW_SHIP_NAME)
		    && self != NULL
		    && self->id != ship_ptr[i].id
		    && (other = Other_by_id(ship_ptr[i].id)) != NULL) {
		    FIND_NAME_WIDTH(other);
		    XDrawString(dpy, p_draw, gc,
				X(x - other->name_width / 2),
				Y(y - gameFont->ascent - 15),
				other->name, other->name_len);
		    Erase_rectangle(X(x - other->name_width / 2) - 1,
				    Y(y - gameFont->ascent - 15)
					- gameFont->ascent,
				    other->name_width + 2,
				    gameFont->ascent + gameFont->descent);
		}

		if (ship_ptr[i].cloak == 0) {
		    if (gcv.line_style != LineSolid) {
			gcv.line_style = LineSolid;
			XChangeGC(dpy, gc, GCLineStyle, &gcv);
		    }
		    if (lock_id == ship_ptr[i].id
			&& ship_ptr[i].id != -1
			&& lock_dist != 0) {
			XFillPolygon(dpy, p_draw, gc, points, 4,
				     Convex, CoordModeOrigin);
			if (points[0].x < points[1].x) {
			    x0 = points[0].x;
			    x1 = points[1].x;
			} else {
			    x0 = points[1].x;
			    x1 = points[0].x;
			}
			if (points[2].x < x0) {
			    x0 = points[2].x;
			}
			else if (points[2].x > x1) {
			    x1 = points[2].x;
			}
			if (points[0].y < points[1].y) {
			    y0 = points[0].y;
			    y1 = points[1].y;
			} else {
			    y0 = points[1].y;
			    y1 = points[0].y;
			}
			if (points[2].y < y0) {
			    y0 = points[2].y;
			}
			else if (points[2].y > y1) {
			    y1 = points[2].y;
			}
			Erase_rectangle(x0, y0, x1 + 1 - x0, y1 + 1 - y0);
		    } else {
			XDrawLines(dpy, p_draw, gc, points, 4, 0);
			Erase_points(0, points, 4);
		    }
		    if (markingLights) {
			if (((loops + ship_ptr[i].id) & 0xF) == 0) {
			    Rectangle_add(RED,
					  points[1].x-2, points[1].y-2, 6, 6);
			    Segment_add(RED,
					points[1].x-8, points[1].y,
					points[1].x+8, points[1].y);
			    Segment_add(RED,
					points[1].x, points[1].y-8,
					points[1].x, points[1].y+8);
			} else if (((loops + ship_ptr[i].id) & 0xF) == 2) {
			    Rectangle_add(BLUE,
					  points[2].x-2, points[2].y-2, 6, 6);
			    Segment_add(BLUE,
					points[2].x-8, points[2].y,
					points[2].x+8, points[2].y);
			    Segment_add(BLUE,
					points[2].x, points[2].y-8,
					points[2].x, points[2].y+8);
			}
		    }
		}

		if (ship_ptr[i].shield || ship_ptr[i].cloak) {
		    if (gcv.line_style != LineOnOffDash) {
			gcv.line_style = LineOnOffDash;
			mask = GCLineStyle;
#ifndef NO_ROTATING_DASHES
			mask |= GCDashOffset;
#endif
			XChangeGC(dpy, gc, mask, &gcv);
		    }
		    if (ship_ptr[i].cloak) {
#if ERASE
			int j;
			for (j = 0; j < 3; j++) {
			    XDrawLine(dpy, p_draw, gc,
				      points[j].x, points[j].y,
				      points[j + 1].x, points[j + 1].y);
			}
			Erase_points(1, points, 4);
#else
			XDrawLines(dpy, p_draw, gc, points, 4, 0);
#endif
		    }
		    if (ship_ptr[i].shield) {
			XDrawArc(dpy, p_draw, gc,
				 X(x - 17),
				 Y(y + 17),
				 34, 34, 0, 64 * 360);
			Erase_arc(X(x - 17), Y(y + 17),
				  34, 34, 0, 64 * 360);
		    }
		}
	    }
	}
	RELEASE(ship_ptr, num_ship, max_ship);
    }

    if (num_refuel > 0 || num_connector > 0 || num_trans > 0) {
	SET_FG(colors[WHITE].pixel);
	if (gcv.line_style != LineOnOffDash) {
	    gcv.line_style = LineOnOffDash;
	    mask = GCLineStyle;
#ifndef NO_ROTATING_DASHES
	    mask |= GCDashOffset;
#endif
	    XChangeGC(dpy, gc, mask, &gcv);
	}
	if (num_refuel > 0) {
	    for (i = 0; i < num_refuel; i++) {
		x0 = refuel_ptr[i].x0;
		y0 = refuel_ptr[i].y0;
		x1 = refuel_ptr[i].x1;
		y1 = refuel_ptr[i].y1;
		if (wrap(&x0, &y0)
		    && wrap(&x1, &y1)) {
		    XDrawLine(dpy, p_draw, gc,
			      X(x0), Y(y0),
			      X(x1), Y(y1));
		    Erase_segment(1, X(x0), Y(y0), X(x1), Y(y1));
		}
	    }
	    RELEASE(refuel_ptr, num_refuel, max_refuel);
	}
	if (num_connector > 0) {
	    for (i = 0; i < num_connector; i++) {
		x0 = connector_ptr[i].x0;
		y0 = connector_ptr[i].y0;
		x1 = connector_ptr[i].x1;
		y1 = connector_ptr[i].y1;
		if (wrap(&x0, &y0)
		    && wrap(&x1, &y1)) {
		    XDrawLine(dpy, p_draw, gc,
			      X(x0), Y(y0),
			      X(x1), Y(y1));
		    Erase_segment(1, X(x0), Y(y0), X(x1), Y(y1));
		}
	    }
	    RELEASE(connector_ptr, num_connector, max_connector);
	}
	if (num_trans > 0) {
	    for (i = 0; i < num_trans; i++) {
		x0 = trans_ptr[i].x1;
		y0 = trans_ptr[i].y1;
		x1 = trans_ptr[i].x2;
		y1 = trans_ptr[i].y2;
		if (wrap(&x0, &y0) && wrap(&x1, &y1))
		    XDrawLine(dpy, p_draw, gc, 
			      X(x0), Y(y0), X(x1), Y(y1));
		    Erase_segment(1, X(x0), Y(y0), X(x1), Y(y1));
	    }
	    RELEASE(trans_ptr, num_trans, max_trans);
	}
    }

    if (gcv.line_style != LineSolid) {
	gcv.line_style = LineSolid;
	mask = GCLineStyle;
	XChangeGC(dpy, gc, mask, &gcv);
    }
    gcv.dash_offset = 0;
}


static void Paint_score_objects(void)
{
    int		i, x, y;

    for (i=0; i < MAX_SCORE_OBJECTS; i++) {
	score_object_t*	sobj = &score_objects[i];
	if (sobj->count > 0) {
	    if (sobj->count%3) {
		x = sobj->x * BLOCK_SZ + BLOCK_SZ/2 - sobj->msg_width/2; 
		y = sobj->y * BLOCK_SZ + BLOCK_SZ/2 - gameFont->ascent/2;
		if (wrap(&x, &y)) {
		    SET_FG(colors[RED].pixel);
		    XDrawString(dpy, p_draw, gc,
				X(x), 
				Y(y),
				sobj->msg, 
				sobj->msg_len);
		    Erase_rectangle(X(x) - 1, Y(y) - gameFont->ascent,
				    sobj->msg_width + 2,
				    gameFont->ascent + gameFont->descent);
		}
	    }
	    sobj->count++;
	    if (sobj->count > SCORE_OBJECT_COUNT) {
		sobj->count = 0;
		sobj->hud_msg_len = 0;
	    }
	}
    }
}


static void Paint_meters(void)
{
    if (BIT(instruments, SHOW_FUEL_METER))
	Paint_meter(10, 10, "Fuel", fuelSum, fuelMax);
    if (BIT(instruments, SHOW_POWER_METER) || control_count)
     	Paint_meter(10, 40, "Power", power, MAX_PLAYER_POWER);
    if (BIT(instruments, SHOW_TURNSPEED_METER) || control_count)
	Paint_meter(10, 60, "Turnspeed", turnspeed, MAX_PLAYER_TURNSPEED);
    if (control_count > 0)
	control_count--;
    if (BIT(instruments, SHOW_PACKET_SIZE_METER))
	Paint_meter(10, 80, "Packet",
		   (packet_size >= 4096) ? 4096 : packet_size, 4096);
    if (BIT(instruments, SHOW_PACKET_LOSS_METER))
	Paint_meter(10, 100, "Loss", packet_loss, FPS);
    if (BIT(instruments, SHOW_PACKET_DROP_METER))
	Paint_meter(10, 120, "Drop", packet_drop, FPS);

    if (destruct > 0)
	Paint_meter((view_width-300)/2 -32, 3*view_height/4,
		   "Self destructing", destruct, 150);
    if (shutdown_count >= 0)
	Paint_meter((view_width-300)/2 -32, 4*view_height/5,
		   "SHUTDOWN", shutdown_count, shutdown_delay);
}


static void Paint_lock(int hud_pos_x, int hud_pos_y)
{
    const int	BORDER = 2;
    int		x, y;
    other_t	*target;
    char	str[50];
    static int	warningCount;
    static int	mapdiag = 0;

    if (mapdiag == 0) {
	mapdiag = LENGTH(Setup->x * BLOCK_SZ, Setup->y * BLOCK_SZ);
    }

    /*
     * Display direction arrow and miscellaneous target information.
     */
    if ((target = Other_by_id(lock_id)) == NULL) {
	return;
    }
    FIND_NAME_WIDTH(target);
    XDrawString(dpy, p_draw, gc,
		hud_pos_x - target->name_width / 2,
		hud_pos_y - HUD_SIZE+HUD_OFFSET - gameFont->descent - BORDER,
		target->name, target->name_len);
    Erase_rectangle(hud_pos_x - target->name_width / 2 - 1,
		    hud_pos_y - HUD_SIZE+HUD_OFFSET - gameFont->descent
			- BORDER - gameFont->ascent,
		    target->name_width + 2,
		    gameFont->ascent + gameFont->descent);
    if (lock_dist != 0) {
	sprintf(str, "%03d", lock_dist / BLOCK_SZ);
	XDrawString(dpy, p_draw, gc,
		    hud_pos_x + HUD_SIZE - HUD_OFFSET + BORDER,
		    hud_pos_y - HUD_SIZE+HUD_OFFSET
		    - gameFont->descent - BORDER,
		    str, 3);
	Erase_rectangle(hud_pos_x + HUD_SIZE - HUD_OFFSET + BORDER - 1,
			hud_pos_y - HUD_SIZE+HUD_OFFSET
			    - gameFont->descent - BORDER - gameFont->ascent,
			XTextWidth(gameFont, str, 3) + 2,
			gameFont->ascent + gameFont->descent);
	if (lock_dist > WARNING_DISTANCE || warningCount++ % 2 == 0) {
	    int size = MIN(mapdiag / lock_dist, 10);

	    SET_FG(colors[RED].pixel);
	    if (size == 0) {
		size = 1;
	    }
	    if (self != NULL
		&& self->team == target->team
		&& BIT(Setup->mode, TEAM_PLAY)) {
		Arc_add(RED,
			(int)(hud_pos_x + HUD_SIZE * 0.6 * tcos(lock_dir)
			      - size * 0.5),
			(int)(hud_pos_y - HUD_SIZE * 0.6 * tsin(lock_dir)
			      - size * 0.5),
			size, size, 0, 64*360);
	    } else {
		x = (int)(hud_pos_x + HUD_SIZE * 0.6 * tcos(lock_dir)
			  - size * 0.5),
		y = (int)(hud_pos_y - HUD_SIZE * 0.6 * tsin(lock_dir)
			  - size * 0.5),
		XFillArc(dpy, p_draw, gc,
			 x, y,
			 size, size, 0, 64*360);
		Erase_rectangle(x, y, size, size);
	    }
	    SET_FG(colors[BLUE].pixel); 
	}
    }
}


static void Paint_HUD(void)
{
    const int BORDER = 3;
    int vert_pos, horiz_pos, size;
    char str[50];
    int hud_pos_x;
    int hud_pos_y;
    int	i, j, maxWidth = -1;
    int rect_x, rect_y, rect_width, rect_height;
    static int vertSpacing = -1;
    
    /* 
     * Show speed pointer
     */
    if (ptr_move_fact != 0.0
	&& selfVisible != 0
	&& (vel.x != 0 || vel.y != 0)) {
        Segment_add(RED,
		    view_width / 2,
		    view_height / 2,
		    view_width / 2 - ptr_move_fact*vel.x,
		    view_height / 2 + ptr_move_fact*vel.y);
    }

    if (!BIT(instruments, SHOW_HUD_INSTRUMENTS)) {
	return;
    }

    /*
     * Display the HUD
     */
    SET_FG(colors[BLUE].pixel);

    hud_pos_x = view_width / 2 - hud_move_fact*vel.x;
    hud_pos_y = view_height / 2 + hud_move_fact*vel.y;

    /* HUD frame */
    gcv.line_style = LineOnOffDash;
    XChangeGC(dpy, gc, GCLineStyle | GCDashOffset, &gcv);
    
    if (BIT(instruments, SHOW_HUD_HORIZONTAL)) {
	XDrawLine(dpy, p_draw, gc,
		  hud_pos_x-HUD_SIZE, hud_pos_y-HUD_SIZE+HUD_OFFSET,
		  hud_pos_x+HUD_SIZE, hud_pos_y-HUD_SIZE+HUD_OFFSET);
	Erase_segment(0,
		      hud_pos_x-HUD_SIZE, hud_pos_y-HUD_SIZE+HUD_OFFSET,
		      hud_pos_x+HUD_SIZE, hud_pos_y-HUD_SIZE+HUD_OFFSET);
	XDrawLine(dpy, p_draw, gc,
		  hud_pos_x-HUD_SIZE, hud_pos_y+HUD_SIZE-HUD_OFFSET,
		  hud_pos_x+HUD_SIZE, hud_pos_y+HUD_SIZE-HUD_OFFSET);
	Erase_segment(0,
		      hud_pos_x-HUD_SIZE, hud_pos_y+HUD_SIZE-HUD_OFFSET,
		      hud_pos_x+HUD_SIZE, hud_pos_y+HUD_SIZE-HUD_OFFSET);
    }
    if (BIT(instruments, SHOW_HUD_VERTICAL)) {
	XDrawLine(dpy, p_draw, gc,
		  hud_pos_x-HUD_SIZE+HUD_OFFSET, hud_pos_y-HUD_SIZE, 
		  hud_pos_x-HUD_SIZE+HUD_OFFSET, hud_pos_y+HUD_SIZE);
	Erase_segment(0,
		      hud_pos_x-HUD_SIZE+HUD_OFFSET, hud_pos_y-HUD_SIZE, 
		      hud_pos_x-HUD_SIZE+HUD_OFFSET, hud_pos_y+HUD_SIZE);
	XDrawLine(dpy, p_draw, gc,
		  hud_pos_x+HUD_SIZE-HUD_OFFSET, hud_pos_y-HUD_SIZE,
		  hud_pos_x+HUD_SIZE-HUD_OFFSET, hud_pos_y+HUD_SIZE);
	Erase_segment(0,
		      hud_pos_x+HUD_SIZE-HUD_OFFSET, hud_pos_y-HUD_SIZE,
		      hud_pos_x+HUD_SIZE-HUD_OFFSET, hud_pos_y+HUD_SIZE);
    }
    gcv.line_style = LineSolid;
    XChangeGC(dpy, gc, GCLineStyle, &gcv);
    
    
    /* Special itemtypes */
    if (vertSpacing < 0)
	vertSpacing
	    = MAX(ITEM_SIZE, gameFont->ascent + gameFont->descent) + 1;
    vert_pos = hud_pos_y - HUD_SIZE+HUD_OFFSET + BORDER;
    horiz_pos = hud_pos_x - HUD_SIZE+HUD_OFFSET - BORDER;
    rect_width = 0;
    rect_height = 0;
    rect_x = horiz_pos;
    rect_y = vert_pos;

    for (i=0; i<NUM_ITEMS; i++) {
	int num;
	switch (i) {
	case ITEM_ROCKET_PACK:
	    num = numRockets;
	    break;
	case ITEM_CLOAKING_DEVICE:
	    num = numCloaks;
	    break;
	case ITEM_WIDEANGLE_SHOT:
	    num = numFrontShots;
	    break;
	case ITEM_BACK_SHOT:
	    num = numBackShots;
	    break;
	case ITEM_MINE_PACK:
	    num = numMines;
	    break;
	case ITEM_SENSOR_PACK:
	    num = numSensors;
	    break;
	case ITEM_TANK:
	    num = numTanks;
	    break;
	case ITEM_ECM:
	    num = numEcms;
	    break;
	case ITEM_AFTERBURNER:
	    num = numAfterburners;
	    break;
	case ITEM_TRANSPORTER:
	    num = numTransporters;
	    break;
	case ITEM_LASER:
	    num = numLasers;
	    break;
	case ITEM_ENERGY_PACK:
	default:
	    num = 0;
	    break;
	}
	if (num > 0) {
	    int len, width;

	    /* Paint item symbol */
	    Paint_item_symbol(i, p_draw, gc, horiz_pos - ITEM_SIZE, vert_pos);

	    /* Paint item count */
	    sprintf(str, "%d", num);
	    len = strlen(str);
	    width = XTextWidth(gameFont, str, len);
	    XDrawString(dpy, p_draw, gc,
			horiz_pos - ITEM_SIZE - BORDER - width,
			vert_pos + ITEM_SIZE/2 + gameFont->ascent/2,
			str, len);

	    maxWidth = MAX(maxWidth, width + BORDER + ITEM_SIZE);
	    vert_pos += vertSpacing;

	    if (vert_pos+vertSpacing > hud_pos_y+HUD_SIZE-HUD_OFFSET-BORDER) {
		rect_width += maxWidth + 2*BORDER;
		rect_height = vert_pos - rect_y;
		horiz_pos -= maxWidth + 2*BORDER;
		vert_pos = hud_pos_y - HUD_SIZE+HUD_OFFSET + BORDER;
		maxWidth = -1;
	    }
	}
    }
    if (maxWidth != -1) {
	rect_width += maxWidth + BORDER;
    }
    if (rect_width > 0) {
	if (rect_height == 0) {
	    rect_height = vert_pos - rect_y;
	}
	rect_x -= rect_width;
	Erase_rectangle(rect_x, rect_y, rect_width, rect_height);
    }

    /* Fuel notify, HUD meter on */
    if (fuelCount || fuelSum < fuelLevel3) {
 	sprintf(str, "%04d", fuelSum);
 	XDrawString(dpy, p_draw, gc,
		    hud_pos_x + HUD_SIZE-HUD_OFFSET+BORDER,
 		    hud_pos_y + HUD_SIZE-HUD_OFFSET+BORDER + gameFont->ascent,
		    str, strlen(str));
	Erase_rectangle(hud_pos_x + HUD_SIZE-HUD_OFFSET+BORDER - 1,
			hud_pos_y + HUD_SIZE-HUD_OFFSET+BORDER,
			XTextWidth(gameFont, str, strlen(str)) + 2,
			gameFont->ascent + gameFont->descent);
	if (numTanks) {
	    if (fuelCurrent == 0)
		strcpy(str,"M ");
	    else
		sprintf(str, "T%d", fuelCurrent);
 	    XDrawString(dpy, p_draw, gc,
			hud_pos_x + HUD_SIZE-HUD_OFFSET + BORDER,
 		        hud_pos_y + HUD_SIZE-HUD_OFFSET + BORDER
			+ gameFont->descent + 2*gameFont->ascent,
			str, strlen(str));
	    Erase_rectangle(hud_pos_x + HUD_SIZE-HUD_OFFSET + BORDER - 1,
			    hud_pos_y + HUD_SIZE-HUD_OFFSET + BORDER
				+ gameFont->descent + gameFont->ascent,
			    XTextWidth(gameFont, str, strlen(str)) + 2,
			    gameFont->ascent + gameFont->descent);
	}
    }

    /* Update the lock display */
    Paint_lock(hud_pos_x, hud_pos_y);

    /* Draw last score on hud if it is an message attached to it */
    for (i=0, j=0; i < MAX_SCORE_OBJECTS; i++) {
	score_object_t*	sobj
	    = &score_objects[(i+score_object)%MAX_SCORE_OBJECTS];
	if (sobj->hud_msg_len > 0) {
	    XDrawString(dpy, p_draw, gc,
			hud_pos_x - sobj->hud_msg_width/2,
			hud_pos_y + HUD_SIZE-HUD_OFFSET + BORDER
			+ gameFont->ascent
			+ j * (gameFont->ascent + gameFont->descent),
			sobj->hud_msg, sobj->hud_msg_len);
	    Erase_rectangle(hud_pos_x - sobj->hud_msg_width/2 - 1,
			    hud_pos_y + HUD_SIZE-HUD_OFFSET + BORDER
				+ j * (gameFont->ascent + gameFont->descent),
			    sobj->hud_msg_width + 2,
			    gameFont->ascent + gameFont->descent);
	    j++;
	}
    }

    if (time_left >= 0) {
	sprintf(str, "%3d:%02d", time_left / 60, time_left % 60);
	size = XTextWidth(gameFont, str, strlen(str));
	XDrawString(dpy, p_draw, gc,
		    hud_pos_x - HUD_SIZE+HUD_OFFSET - BORDER - size,
		    hud_pos_y - HUD_SIZE+HUD_OFFSET - BORDER
			- gameFont->descent,
		    str, strlen(str));
	Erase_rectangle(hud_pos_x - HUD_SIZE+HUD_OFFSET - BORDER - size - 1,
			hud_pos_y - HUD_SIZE+HUD_OFFSET - BORDER,
			size + 2, gameFont->ascent + gameFont->descent);
    }

    /* Fuel gauge, must be last */
    if (BIT(instruments, SHOW_FUEL_GAUGE) == 0
	|| !((fuelCount)
	   || (fuelSum < fuelLevel3
	      && ((fuelSum < fuelLevel1 && (loops%4) < 2)
		  || (fuelSum < fuelLevel2
		      && fuelSum > fuelLevel1
		      && (loops%8) < 4)
		  || (fuelSum > fuelLevel2)))))
	return;

    if (fuelCount > 0) {
	fuelCount--;
    }

    SET_FG(colors[BLUE].pixel);
    XDrawRectangle(dpy, p_draw, gc,
		  hud_pos_x + HUD_SIZE - HUD_OFFSET + FUEL_GAUGE_OFFSET - 1,
		  hud_pos_y - HUD_SIZE + HUD_OFFSET + FUEL_GAUGE_OFFSET - 1,
		  HUD_OFFSET - (2*FUEL_GAUGE_OFFSET) + 3,
		  HUD_FUEL_GAUGE_SIZE + 3);
    Erase_4point(hud_pos_x + HUD_SIZE - HUD_OFFSET + FUEL_GAUGE_OFFSET - 1,
		 hud_pos_y - HUD_SIZE + HUD_OFFSET + FUEL_GAUGE_OFFSET - 1,
		 HUD_OFFSET - (2*FUEL_GAUGE_OFFSET) + 3,
		 HUD_FUEL_GAUGE_SIZE + 3);

    size = (HUD_FUEL_GAUGE_SIZE * fuelSum) / fuelMax;
    XFillRectangle(dpy, p_draw, gc,
		   hud_pos_x + HUD_SIZE - HUD_OFFSET + FUEL_GAUGE_OFFSET + 1,
		   hud_pos_y - HUD_SIZE + HUD_OFFSET + FUEL_GAUGE_OFFSET
		   + HUD_FUEL_GAUGE_SIZE - size + 1,
		   HUD_OFFSET - (2*FUEL_GAUGE_OFFSET), size);
    Erase_rectangle(hud_pos_x + HUD_SIZE - HUD_OFFSET + FUEL_GAUGE_OFFSET + 1,
		    hud_pos_y - HUD_SIZE + HUD_OFFSET + FUEL_GAUGE_OFFSET
			+ HUD_FUEL_GAUGE_SIZE - size + 1,
		    HUD_OFFSET - (2*FUEL_GAUGE_OFFSET), size);
}

static void Paint_messages(void)
{
    int i, x, y, width;
    const int	BORDER = 10,
		SPACING = messageFont->ascent+messageFont->descent+1;

    if (charsPerTick <= 0.0)
	charsPerTick = (float)charsPerSecond / FPS;

    for (y = view_height - messageFont->descent - BORDER, i = 0;
	 i < MAX_MSGS;
	 i++, y -= SPACING) {
	int len;

	if (Msg[i]->len == 0)
	    continue;
	else if (Msg[i]->life-- > MSG_FLASH)
	    XSetForeground(dpy, messageGC, colors[RED].pixel);
	else if (Msg[i]->life > 0)
	    XSetForeground(dpy, messageGC, colors[WHITE].pixel);
	else {
	    Msg[i]->txt[0] = '\0';
	    Msg[i]->len = 0;
	    continue;
	}
	len = charsPerTick * (MSG_DURATION - Msg[i]->life);
	len = MIN(Msg[i]->len, len);
	if (Msg[i]->alignment == RIGHT) {
	    x = view_width-BORDER - Msg[i]->pixelLen;
	} else {
	    x = BORDER;
	}
	XDrawString(dpy, p_draw, messageGC,
		    x, y,
		    Msg[i]->txt, len);
	if (len < Msg[i]->len) {
	    width = XTextWidth(messageFont, Msg[i]->txt, len);
	} else {
	    width = Msg[i]->pixelLen;
	}
	Erase_rectangle(x - 1, y - messageFont->ascent,
			width + 2,
			messageFont->ascent + messageFont->descent);
    }
}


void Add_message(char *message)
{
    int i;
    message_t *tmp;
    
    
    tmp = Msg[MAX_MSGS-1];
    for (i=MAX_MSGS-1; i>0; i--)
	Msg[i] = Msg[i-1];
    
    Msg[0] = tmp;
    
    Msg[0]->life = MSG_DURATION;
    strcpy(Msg[0]->txt, message);
    Msg[0]->len = strlen(message);
    Msg[0]->pixelLen = XTextWidth(messageFont, Msg[0]->txt, Msg[0]->len);

    if (Msg[0]->txt[Msg[0]->len-1] == ']'
	|| strncmp(Msg[0]->txt, " <", 2) == 0)
	Msg[0]->alignment = RIGHT;
    else
	Msg[0]->alignment = LEFT;
}


static void Paint_radar(void)
{
    int			i, x, y, x1, y1, xw, yw;
    const float		xf = 256.0f / (float)Setup->width,
    			yf = (float)RadarHeight / (float)Setup->height;

    if (radar_exposed == false) {
	return;
    }
    if (s_radar != p_radar) {
	/* Draw static radar onto radar */
	XCopyArea(dpy, s_radar, p_radar, gc,
		  0, 0, 256, RadarHeight, 0, 0);
    } else {
	/* Clear radar */
	XSetForeground(dpy, radarGC, colors[BLACK].pixel);
	XFillRectangle(dpy, p_radar,
		       radarGC, 0, 0, 256, RadarHeight);
    }
    
    XSetForeground(dpy, radarGC, colors[WHITE].pixel);

    /* Checkpoint */
    if (BIT(Setup->mode, TIMING)) {
	Check_pos_by_index(nextCheckPoint, &x, &y);
	x = (int)(x * BLOCK_SZ * xf + 0.5);
	y = RadarHeight - (int)(y * BLOCK_SZ * yf + 0.5) + DSIZE - 1;
	diamond[0].x = x;
	diamond[0].y = y;
	XDrawLines(dpy, p_radar, radarGC,
		   diamond, 5, CoordModePrevious);
    }
    if (selfVisible != 0 && loops % 16 < 13) {
	x = (int)(pos.x * xf + 0.5);
	y = RadarHeight - (int)(pos.y * yf + 0.5) - 1;
	x1 = (int)(x + 8 * tcos(heading));
	y1 = (int)(y - 8 * tsin(heading));
	XDrawLine(dpy, p_radar, radarGC,
		  x, y, x1, y1);
	if (BIT(Setup->mode, WRAP_PLAY)) {
	    xw = x1 - (x1 + 256) % 256;
	    yw = y1 - (y1 + RadarHeight) % RadarHeight;
	    if (xw != 0) {
		XDrawLine(dpy, p_radar, radarGC,
			  x - xw, y, x1 - xw, y1);
	    }
	    if (yw != 0) {
		XDrawLine(dpy, p_radar, radarGC,
			  x, y - yw, x1, y1 - yw);
		if (xw != 0) {
		    XDrawLine(dpy, p_radar, radarGC,
			      x - xw, y - yw, x1 - xw, y1 - yw);
		}
	    }
	}
    }
    for (i = 0; i<num_radar; i++) {
	x = (int)(radar_ptr[i].x * xf + 0.5);
	y = RadarHeight - (int)(radar_ptr[i].y * yf + 0.5) - 1;
	XFillRectangle(dpy, p_radar, radarGC,
		       x-1, y-1, 3, 3);
	if (BIT(Setup->mode, WRAP_PLAY)) {
	    xw = (x - 1 < 0) ? -256 : (x + 1 >= 256) ? 256 : 0;
	    yw = (y - 1 < 0) ? -RadarHeight
			     : (y + 1 >= RadarHeight) ? RadarHeight : 0;
	    if (xw != 0) {
		XFillRectangle(dpy, p_radar, radarGC,
			       x-1 - xw, y-1, 3, 3);
	    }
	    if (yw != 0) {
		XFillRectangle(dpy, p_radar, radarGC,
			       x-1, y-1 - yw, 3, 3);
		if (xw != 0) {
		    XFillRectangle(dpy, p_radar, radarGC,
				   x-1 - xw, y-1 - yw, 3, 3);
		}
	    }
	}
    }
}

static void Paint_vcannon(void)
{
    int			i, x, y, type;

    if (num_vcannon > 0) {
	SET_FG(colors[WHITE].pixel);
	for (i = 0; i < num_vcannon; i++) {
	    type = vcannon_ptr[i].type;
	    x = vcannon_ptr[i].x;
	    y = vcannon_ptr[i].y;
	    switch (type) {
	    case SETUP_CANNON_UP:
		points[0].x = X(x);
		points[0].y = Y(y);
		points[1].x = X(x+BLOCK_SZ);
		points[1].y = Y(y);
		points[2].x = X(x+BLOCK_SZ/2);
		points[2].y = Y(y+BLOCK_SZ/3);
		break;
	    case SETUP_CANNON_DOWN:
		points[0].x = X(x);
		points[0].y = Y(y+BLOCK_SZ);
		points[1].x = X(x+BLOCK_SZ);
		points[1].y = Y(y+BLOCK_SZ);
		points[2].x = X(x+BLOCK_SZ/2);
		points[2].y = Y(y+2*BLOCK_SZ/3);
		break;
	    case SETUP_CANNON_RIGHT:
		points[0].x = X(x);
		points[0].y = Y(y);
		points[1].x = X(x);
		points[1].y = Y(y+BLOCK_SZ);
		points[2].x = X(x+BLOCK_SZ/3);
		points[2].y = Y(y+BLOCK_SZ/2);
		break;
	    case SETUP_CANNON_LEFT:
		points[0].x = X(x+BLOCK_SZ);
		points[0].y = Y(y);
		points[1].x = X(x+BLOCK_SZ);
		points[1].y = Y(y+BLOCK_SZ);
		points[2].x = X(x+2*BLOCK_SZ/3);
		points[2].y = Y(y+BLOCK_SZ/2);
		break;
	    default:
		errno = 0;
		error("Unknown cannon type %d", type);
		continue;
	    }
	    points[3] = points[0];
	    XDrawLines(dpy, p_draw, gc, points, 4, 0);
	    Erase_points(0, points, 4);
	}
	RELEASE(vcannon_ptr, num_vcannon, max_vcannon);
    }
}

static void Paint_vfuel(void)
{
#define FUEL_BORDER 2

    int			i, x, y, size;
    long		fuel;
    char		s[2];

    if (num_vfuel > 0) {
	SET_FG(colors[RED].pixel);
	for (i = 0; i < num_vfuel; i++) {
	    x = vfuel_ptr[i].x;
	    y = vfuel_ptr[i].y;
	    fuel = vfuel_ptr[i].fuel;
	    size = (BLOCK_SZ - 2*FUEL_BORDER) * fuel / MAX_STATION_FUEL;
	    XFillRectangle(dpy, p_draw, gc,
			  X(x + FUEL_BORDER), Y(y + FUEL_BORDER + size),
			  BLOCK_SZ - 2*FUEL_BORDER + 1, size + 1);
	    Erase_rectangle(X(x + FUEL_BORDER),
			    Y(y - FUEL_BORDER + BLOCK_SZ),
			    BLOCK_SZ - 2*FUEL_BORDER + 1,
			    BLOCK_SZ - 2*FUEL_BORDER + 1);
	}
	/* Draw F in fuel cells */
	s[0] = 'F'; s[1] = '\0';
	XSetFunction(dpy, gc, GXxor);
	SET_FG(colors[BLACK].pixel ^ colors[RED].pixel);
	for (i = 0; i < num_vfuel; i++) {
	    x = vfuel_ptr[i].x;
	    y = vfuel_ptr[i].y;
	    XDrawString(dpy, p_draw, gc,
			X(x + BLOCK_SZ/2 - XTextWidth(gameFont, s, 1)/2),
			Y(y + BLOCK_SZ/2 - gameFont->ascent/2),
			s, 1);
	}
	XSetFunction(dpy, gc, GXcopy);
	RELEASE(vfuel_ptr, num_vfuel, max_vfuel);
    }
}

static void Paint_vbase(void)
{
    const int	BORDER = 4;					/* in pixels */
    int		i, id, x, y, xi, yi, team, type, size;
    other_t	*other;
    char	s[3];

    if (num_vbase > 0) {
	SET_FG(colors[WHITE].pixel);
	for (i = 0; i < num_vbase; i++) {
	    x = vbase_ptr[i].x;
	    y = vbase_ptr[i].y;
	    xi = vbase_ptr[i].xi;
	    yi = vbase_ptr[i].yi;
	    type = vbase_ptr[i].type;
	    switch (type) {
	    case SETUP_BASE_UP:
		Segment_add(WHITE,
			    X(x), Y(y-1),
			    X(x+BLOCK_SZ), Y(y-1));
		y -= BORDER + gameFont->ascent;
		break;
	    case SETUP_BASE_DOWN:
		Segment_add(WHITE,
			    X(x), Y(y+BLOCK_SZ+1),
			    X(x+BLOCK_SZ), Y(y+BLOCK_SZ+1));
		y += BORDER + BLOCK_SZ;
		break;
	    case SETUP_BASE_LEFT:
		Segment_add(WHITE,
			    X(x+BLOCK_SZ+1), Y(y+BLOCK_SZ),
			    X(x+BLOCK_SZ+1), Y(y));
		x += BLOCK_SZ + BORDER;
		y += BLOCK_SZ/2 - gameFont->ascent/2;
		break;
	    case SETUP_BASE_RIGHT:
		Segment_add(WHITE,
			    X(x-1), Y(y+BLOCK_SZ),
			    X(x-1), Y(y));
    		y += BLOCK_SZ/2 - gameFont->ascent/2;
		x -= BORDER;
		break;
	    default:
		errno = 0;
		error("Bad base dir.");
		continue;
	    }
	    if (Base_info_by_pos(xi, yi, &id, &team) == -1) {
		continue;
	    }
	    if (BIT(Setup->mode, TEAM_PLAY)) {
		s[0] = '0' + team;
		s[1] = ' ';
		s[2] = '\0';
		size = XTextWidth(gameFont, s, 2);
		if (type == SETUP_BASE_RIGHT) {
		    x -= size;
		}
		XDrawString(dpy, p_draw, gc,
			    X(x), Y(y),
			    s, 2);
		Erase_rectangle(X(x) - 1, Y(y) - gameFont->ascent,
				size + 2,
				gameFont->ascent + gameFont->descent);
		if (type != SETUP_BASE_RIGHT) {
		    x += size;
		}
	    }
	    if ((other = Other_by_id(id)) != NULL) {
		FIND_NAME_WIDTH(other);
		if (type == SETUP_BASE_RIGHT) {
		    x -= other->name_width;
		}
		XDrawString(dpy, p_draw, gc,
			    X(x), Y(y),
			    other->name, other->name_len);
		Erase_rectangle(X(x) - 1, Y(y) - gameFont->ascent,
				other->name_width + 2,
				gameFont->ascent + gameFont->descent);
	    }
	}
	RELEASE(vbase_ptr, num_vbase, max_vbase);
    }
}

static void Paint_world(void)
{
    int xi, yi, xb, yb, size, fuel, color;
    int rxb, ryb;
    int dot;
    int x, y;
    const int WS_PR_SC_W = 1+(float)view_width/BLOCK_SZ;
    const int WS_PR_SC_H = 1+(float)view_height/BLOCK_SZ;
    static const int INSIDE_WS=BLOCK_SZ-2;
    static int wormDrawCount;
    int type, mapoff;
    int sx = 0, sy = 0;
    char s[2];


    wormDrawCount = (wormDrawCount + 1) & 7;

    xb = (world.x/BLOCK_SZ);
    yb = (world.y/BLOCK_SZ);
    if (!BIT (Setup->mode, WRAP_PLAY)) {
	if (xb < 0)
	    sx = -xb;
	if (yb < 0)
	    sy = -yb;
	if (world.x < 0) {
	    Segment_add(BLUE,
			X(0), Y(0),
			X(0), Y(Setup->height));
	}
	if (world.x + view_width >= Setup->width) {
	    Segment_add(BLUE,
			X(Setup->width), Y(0),
			X(Setup->width), Y(Setup->height));
	}
	if (world.y < 0) {
	    Segment_add(BLUE,
			X(0), Y(0),
			X(Setup->width), Y(0));
	}
	if (world.y + view_height >= Setup->height) {
	    Segment_add(BLUE,
			X(0), Y(Setup->height),
			X(Setup->width), Y(Setup->height));
	}
    }

    for (rxb = sx, xi = xb + rxb, x = xi * BLOCK_SZ, mapoff = xi * Setup->y;
	    rxb <= WS_PR_SC_W;
	    rxb++, xi++, x += BLOCK_SZ, mapoff += Setup->y) {

	if (xi >= Setup->x) {
	    if (xi == Setup->x) {
		if (!BIT(Setup->mode, WRAP_PLAY))
		    break;
	    }
	    xi -= Setup->x;
	    mapoff = xi * Setup->y;
	}

	for (ryb = sy, yi = ryb + yb, y = yi * BLOCK_SZ;
		ryb <= WS_PR_SC_H;
		ryb++, yi++, y += BLOCK_SZ) {

	    if (yi >= Setup->y) {
		if (yi == Setup->y) {
		    if (!BIT(Setup->mode, WRAP_PLAY))
			break;
		}
		yi -= Setup->y;
	    }

	    type = Setup->map_data[mapoff + yi];

	    if (type & BLUE_BIT) {
		if (type & BLUE_LEFT) {
		    Segment_add(BLUE,
				X(x),
				Y(y),
				X(x),
				Y(y+BLOCK_SZ));
		}
		if (type & BLUE_DOWN) {
		    Segment_add(BLUE,
				X(x),
				Y(y),
				X(x+BLOCK_SZ),
				Y(y));
		}
		if (type & BLUE_RIGHT) {
		    Segment_add(BLUE,
				X(x+BLOCK_SZ),
				Y(y),
				X(x+BLOCK_SZ),
				Y(y+BLOCK_SZ));
		}
		if (type & BLUE_UP) {
		    Segment_add(BLUE,
				X(x),
				Y(y+BLOCK_SZ),
				X(x+BLOCK_SZ),
				Y(y+BLOCK_SZ));
		}
		if ((type & BLUE_FUEL) == BLUE_FUEL) {
		    fuel = Fuel_by_pos(xi, yi);
		    Handle_vfuel(x, y, fuel);
		}
		else if (type & BLUE_OPEN) {
		    Segment_add(BLUE,
				X(x),
				Y(y),
				X(x+BLOCK_SZ),
				Y(y+BLOCK_SZ));
		}
		else if (type & BLUE_CLOSED) {
		    Segment_add(BLUE,
				X(x),
				Y(y+BLOCK_SZ),
				X(x+BLOCK_SZ),
				Y(y));
		}
		continue;
	    }

	    switch (type) {
		
	    case SETUP_CHECK:
		SET_FG(colors[BLUE].pixel);
		points[0].x = X(x+(BLOCK_SZ/2));
		points[0].y = Y(y);
		points[1].x = X(x);
		points[1].y = Y(y+BLOCK_SZ/2);
		points[2].x = X(x+BLOCK_SZ/2);
		points[2].y = Y(y+BLOCK_SZ);
		points[3].x = X(x+BLOCK_SZ);
		points[3].y = Y(y+(BLOCK_SZ/2));
		points[4] = points[0];

		if (Check_index_by_pos(xi, yi) == nextCheckPoint) {
		    XFillPolygon(dpy, p_draw, gc,
				 points, 5, Convex, CoordModeOrigin);
		    Erase_rectangle(X(x), Y(y+BLOCK_SZ),
				    BLOCK_SZ, BLOCK_SZ);
		} else {
		    XDrawLines(dpy, p_draw, gc,
			       points, 5, 0); 
		    Erase_points(0, points, 5);
		}
		break;
		
	    case SETUP_ACWISE_GRAV:
		Arc_add(RED,
			X(x+5), Y(y+BLOCK_SZ-5),
			BLOCK_SZ-10, BLOCK_SZ-10, 64*150, 64*300);
		Segment_add(RED,
			    X(x+BLOCK_SZ/2),
			    Y(y+BLOCK_SZ-5),
			    X(x+BLOCK_SZ/2+4),
			    Y(y+BLOCK_SZ-1));
		Segment_add(RED,
			    X(x+BLOCK_SZ/2),
			    Y(y+BLOCK_SZ-5),
			    X(x+BLOCK_SZ/2+4),
			    Y(y+BLOCK_SZ-9));
		break;
		
	    case SETUP_CWISE_GRAV:
		Arc_add(RED,
			X(x+5), Y(y+BLOCK_SZ-5),
			BLOCK_SZ-10, BLOCK_SZ-10, 64*90, 64*300);
		Segment_add(RED,
			    X(x+BLOCK_SZ/2),
			    Y(y+BLOCK_SZ-5),
			    X(x+BLOCK_SZ/2-4),
			    Y(y+BLOCK_SZ-1));
		Segment_add(RED,
			    X(x+BLOCK_SZ/2),
			    Y(y+BLOCK_SZ-5),
			    X(x+BLOCK_SZ/2-4),
			    Y(y+BLOCK_SZ-9));
		break;
		
	    case SETUP_POS_GRAV:
		Arc_add(RED,
			X(x+1), Y(y+BLOCK_SZ-1),
			INSIDE_WS, INSIDE_WS, 0, 64*360);
		Segment_add(RED,
			  X(x+BLOCK_SZ/2),
			  Y(y+5),
			  X(x+BLOCK_SZ/2),
			  Y(y+BLOCK_SZ-5));
		Segment_add(RED,
			    X(x+5),
			    Y(y+BLOCK_SZ/2),
			    X(x+BLOCK_SZ-5),
			    Y(y+BLOCK_SZ/2));
		break;
		
	    case SETUP_NEG_GRAV:
		Arc_add(RED,
			X(x+1), Y(y+BLOCK_SZ-1),
			INSIDE_WS, INSIDE_WS, 0, 64*360);
		Segment_add(RED,
			    X(x+5),
			    Y(y+BLOCK_SZ/2),
			    X(x+BLOCK_SZ-5),
			    Y(y+BLOCK_SZ/2));
		break;
		
	    case SETUP_WORM_IN:
	    case SETUP_WORM_NORMAL:
		{
		    static int wormOffset[8][3] = {
			{ 10, 10, 10 },
			{ 5, 10, 10 },
			{ 0, 10, 10 },
			{ 0, 5, 10 },
			{ 0, 0, 10 },
			{ 5, 0, 10 },
			{ 10, 0, 10 },
			{ 10, 5, 10 }
		    };
#define _O 	wormOffset[wormDrawCount]
#define ARC(_x, _y, _w)						\
    Arc_add(RED,						\
	    X(x) + (_x),					\
	    Y(y + BLOCK_SZ) + (_y),				\
	    INSIDE_WS - (_w), INSIDE_WS - (_w), 0, 64 * 360)

		    SET_FG(colors[RED].pixel);
		    ARC(0, 0, 0);
		    ARC(_O[0], _O[1], _O[2]);
		    ARC(_O[0] * 2, _O[1] * 2, _O[2] * 2);
		    break;
		}

	    case SETUP_CANNON_UP:
	    case SETUP_CANNON_DOWN:
	    case SETUP_CANNON_RIGHT:
	    case SETUP_CANNON_LEFT:
		if (Cannon_dead_time_by_pos(xi, yi, &dot) <= 0) {
		    Handle_vcannon(x, y, type);
		    break;
		}
		if (dot == 0) {
		    break;
		}
		/*FALLTHROUGH*/

	    case SETUP_SPACE_DOT:
		Rectangle_add(BLUE,
			      X(x + BLOCK_SZ / 2),
			      Y(y + BLOCK_SZ / 2),
			      map_point_size, map_point_size);
		break;

	    case SETUP_BASE_UP:
	    case SETUP_BASE_RIGHT:
	    case SETUP_BASE_DOWN:
	    case SETUP_BASE_LEFT:
		Handle_vbase(x, y, xi, yi, type);
		break;

	    case SETUP_TARGET+0:
	    case SETUP_TARGET+1:
	    case SETUP_TARGET+2:
	    case SETUP_TARGET+3:
	    case SETUP_TARGET+4:
	    case SETUP_TARGET+5:
	    case SETUP_TARGET+6:
	    case SETUP_TARGET+7:
	    case SETUP_TARGET+8:
	    case SETUP_TARGET+9:
		{
		    int 	a1,a2,b1,b2;
		    int 	damage;
		    
		    if (Target_alive(xi, yi, &damage) != 0)
			break;

		    if (team == type - SETUP_TARGET) {
			color = BLUE;
		    } else {
			color = RED;
		    }
		    SET_FG(colors[color].pixel);

		    a1 = X(x);
		    b1 = Y(y+BLOCK_SZ);
		    a2 = a1 + BLOCK_SZ;
		    b2 = b1 + BLOCK_SZ;
		    Segment_add(color, a1, b1, a1, b2);
		    Segment_add(color, a2, b1, a2, b2);
		    Segment_add(color, a1, b1, a2, b1);
		    Segment_add(color, a1, b2, a2, b2);

		    XDrawRectangle(dpy, p_draw, gc,
				   X(x+(BLOCK_SZ+2)/4),
				   Y(y+3*BLOCK_SZ/4),
				   BLOCK_SZ/2, BLOCK_SZ/2);
		    Erase_4point(X(x+(BLOCK_SZ+2)/4),
				 Y(y+3*BLOCK_SZ/4),
				 BLOCK_SZ/2, BLOCK_SZ/2);

		    if (BIT(Setup->mode, TEAM_PLAY)) {
			s[0] = '0' + type - SETUP_TARGET; s[1] = '\0';
			size = XTextWidth(gameFont, s, 1);
			XDrawString(dpy, p_draw, gc,
				    X(x + BLOCK_SZ/2 - size/2),
				    Y(y + BLOCK_SZ/2 - gameFont->ascent/2),
				    s, 1);
			Erase_rectangle(X(x + BLOCK_SZ/2 - size/2) - 1,
					Y(y + BLOCK_SZ/2 - gameFont->ascent/2)
					    - gameFont->ascent,
					size + 2,
					gameFont->ascent + gameFont->descent);
		    }

		    if (damage != TARGET_DAMAGE) {
			size = (damage * BLOCK_SZ) / (TARGET_DAMAGE * 2);
			a1 = x + size;
			a2 = y + size;
			b1 = x + (BLOCK_SZ - size);
			b2 = y + (BLOCK_SZ - size);

			Segment_add(RED,
				    X(a1), Y(a2),
				    X(b1), Y(b2));

			Segment_add(RED,
				    X(a1), Y(b2),
				    X(b1), Y(a2));
		    }
		}
		break;

	    case SETUP_TREASURE+0:
	    case SETUP_TREASURE+1:
	    case SETUP_TREASURE+2:
	    case SETUP_TREASURE+3:
	    case SETUP_TREASURE+4:
	    case SETUP_TREASURE+5:
	    case SETUP_TREASURE+6:
	    case SETUP_TREASURE+7:
	    case SETUP_TREASURE+8:
	    case SETUP_TREASURE+9:
		if (team == type - SETUP_TREASURE) {
		    color = BLUE;
		} else {
		    color = RED;
		}
		SET_FG(colors[color].pixel);
		Segment_add(color,
			    X(x),Y(y),
			    X(x),Y(y + BLOCK_SZ/2));
		Segment_add(color,
			    X(x + BLOCK_SZ),Y(y),
			    X(x + BLOCK_SZ),
			    Y(y + BLOCK_SZ/2));
		Segment_add(color,
			    X(x),Y(y),
			    X(x + BLOCK_SZ),Y(y));
		Arc_add(color,
			X(x),
			Y(y + BLOCK_SZ),
			BLOCK_SZ, BLOCK_SZ, 0, 64*180);
		s[1] = '\0'; s[0] = '0' + type - SETUP_TREASURE;
		XDrawString(dpy, p_draw, gc,
			    X(x+BLOCK_SZ/2),
			    Y(y+BLOCK_SZ/2), s, 1);
		Erase_rectangle(X(x+BLOCK_SZ/2) - 1, Y(y+BLOCK_SZ/2),
				XTextWidth(gameFont, s, 1) + 2,
				gameFont->ascent + gameFont->descent);
		break;

	    default:
		break;
	    }
	}
    }
}


void Paint_sliding_radar(void)
{
    if (BIT(Setup->mode, WRAP_PLAY) == 0) {
	return;
    }
    if (p_radar != s_radar) {
	return;
    }
    if (BIT(instruments, SHOW_SLIDING_RADAR) != 0) {
	if (s_radar != radar) {
	    return;
	}
	s_radar = XCreatePixmap(dpy, radar,
				256, RadarHeight,
				DefaultDepth(dpy, DefaultScreen(dpy)));
	p_radar = s_radar;
	if (radar_exposed == true) {
	    Paint_world_radar();
	}
    } else {
	if (s_radar == radar) {
	    return;
	}
	XFreePixmap(dpy, s_radar);
	s_radar = radar;
	p_radar = s_radar;
	if (radar_exposed == true) {
	    Paint_world_radar();
	}
    }
}


void Paint_world_radar(void)
{
    int			i, xi, yi, xm, ym, xp, yp;
    float		xs, ys;
    int			npoint = 0, nsegment = 0, start, end;
    const int		max = 256;
    u_byte		visible[256];
    XSegment		segments[256];
    XPoint		points[256];

    if (s_radar == p_radar) 
	XSetPlaneMask(dpy, radarGC, 
		      AllPlanes&(~(dpl_1[0]|dpl_1[1])));
    if (s_radar != radar) {
	/* Clear radar */
	XSetForeground(dpy, radarGC, colors[BLACK].pixel);
	XFillRectangle(dpy, s_radar, radarGC, 0, 0, 256, RadarHeight);
    } else {
	XClearWindow(dpy, radar);
    }
    XSetForeground(dpy, radarGC, colors[BLUE].pixel);

    memset(visible, 0, sizeof visible);
    visible[SETUP_FILLED] = 1;
    visible[SETUP_FILLED_NO_DRAW] = 1;
    visible[SETUP_REC_LU] = 1;
    visible[SETUP_REC_RU] = 1;
    visible[SETUP_REC_LD] = 1;
    visible[SETUP_REC_RD] = 1;
    visible[SETUP_FUEL] = 1;
    for (i = 0; i < 10; i++) {
	visible[SETUP_TARGET+i] = 1;
    }
    for (i = BLUE_BIT; i < sizeof visible; i++) {
	visible[i] = 1;
    }

    if (Setup->x >= 256) {
	xs = (float)(256 - 1) / (Setup->x - 1);
	ys = (float)(RadarHeight - 1) / (Setup->y - 1);
	for (xi=0; xi<Setup->x; xi++) {
	    start = end = -1;
	    xp = (int)(xi * xs + 0.5);
	    for (yi=0; yi<Setup->y; yi++) {
		if (visible[Setup->map_data[xi * Setup->y + yi]] != 0) {
		    yp = (int)(yi * ys + 0.5);
		    if (start == -1) {
			start = end = yp;
		    } else {
			end = yp;
		    }
		}
		if (start != -1
		    && (visible[Setup->map_data[xi * Setup->y + yi]] == 0
		    || yi == Setup->y - 1)) {
		    if (end > start) {
			segments[nsegment].x1 = xp;
			segments[nsegment].y1 = RadarHeight - 1 - start;
			segments[nsegment].x2 = xp;
			segments[nsegment].y2 = RadarHeight - 1 - end;
			nsegment++;
			if (nsegment >= max || yi == Setup->y - 1) {
			    XDrawSegments(dpy, s_radar, radarGC,
					  segments, nsegment);
			    nsegment = 0;
			}
		    } else {
			points[npoint].x = xp;
			points[npoint].y = RadarHeight - 1 - start;
			npoint++;
			if (npoint >= max || yi == Setup->y - 1) {
			    XDrawPoints(dpy, s_radar, radarGC,
					points, npoint, CoordModeOrigin);
			    npoint = 0;
			}
		    }
		    start = end = -1;
		}
	    }
	}
    } else {
	xs = (float)(Setup->x - 1) / (256 - 1);
	ys = (float)(Setup->y - 1) / (RadarHeight - 1);
	for (xi=0; xi<256; xi++) {
	    xm = (int)(xi * xs + 0.5) * Setup->y;
	    start = end = -1;
	    xp = xi;
	    for (yi=0; yi<RadarHeight; yi++) {
		ym = (int)(yi * ys + 0.5);
		if (visible[Setup->map_data[xm + ym]] != 0) {
		    yp = yi;
		    if (start == -1) {
			start = end = yp;
		    } else {
			end = yp;
		    }
		}
		if (start != -1
		    && (visible[Setup->map_data[xm + ym]] == 0
		    || yi == RadarHeight - 1)) {
		    if (end > start) {
			segments[nsegment].x1 = xp;
			segments[nsegment].y1 = RadarHeight - 1 - start;
			segments[nsegment].x2 = xp;
			segments[nsegment].y2 = RadarHeight - 1 - end;
			nsegment++;
			if (nsegment >= max || yi == RadarHeight - 1) {
			    XDrawSegments(dpy, s_radar, radarGC,
					  segments, nsegment);
			    nsegment = 0;
			}
		    } else {
			points[npoint].x = xp;
			points[npoint].y = RadarHeight - 1 - start;
			npoint++;
			if (npoint >= max || yi == RadarHeight - 1) {
			    XDrawPoints(dpy, s_radar, radarGC,
					points, npoint, CoordModeOrigin);
			    npoint = 0;
			}
		    }
		    start = end = -1;
		}
	    }
	}
    }
    if (nsegment > 0) {
	XDrawSegments(dpy, s_radar, radarGC,
		      segments, nsegment);
    }
    if (npoint > 0) {
	XDrawPoints(dpy, s_radar, radarGC,
		    points, npoint, CoordModeOrigin);
    }

    if (s_radar == p_radar)
	XSetPlaneMask(dpy, radarGC, 
		      AllPlanes&(~(dpl_2[0]|dpl_2[1])));
}


void Paint_frame(void)
{
    static long scroll_i = 0;

    if (start_loops != end_loops) {
	errno = 0;
	error("Start neq. End (%ld,%ld,%ld)", start_loops, end_loops, loops);
    }
    loops = end_loops;

#ifdef SCROLL
    /*
     * Scroll a message in the window title.
     */
    if ((loops % SCROLL_DELAY) == 0) {
	if (++scroll_i >= LONG_MAX)
	    scroll_i = 0;
	XStoreName(dpy, top,
		   scroll(scroll,
			  scroll_i % scroll_len,
			  SCROLL_LEN));
    }
#else
    /*
     * Switch between two different window titles.
     */
    if ((loops % TITLE_DELAY) == 0) {
	scroll_i = !scroll_i;
	if (scroll_i)
	    XStoreName(dpy, top, COPYRIGHT);
	else
	    XStoreName(dpy, top, TITLE);
	
    }
#endif

    /*
     * Do we really need to draw all this if the player is damaged?
     */
    if (damaged <= 0) {
	Arc_start();

	Rectangle_start();
	Segment_start();
	Paint_world();
	Segment_end();
	Rectangle_end();

	Rectangle_start();
	Segment_start();

	Paint_vfuel();
	Paint_vcannon();
	Paint_vbase();
	Paint_shots();
	Paint_ships();

	Rectangle_end();
	Segment_end();

	Rectangle_start();
	Segment_start();
	Paint_meters();
	Paint_HUD();
	Rectangle_end();
	Segment_end();

	Arc_end();

	Paint_messages();
	Paint_radar();
	Paint_score_objects();
    }

    /*
     * Now switch planes and clear the screen.
     */
    if (p_radar != radar && radar_exposed == true) {
	if (BIT(instruments, SHOW_SLIDING_RADAR) == 0
	    || BIT(Setup->mode, WRAP_PLAY) == 0) {
	    XCopyArea(dpy, p_radar, radar, gc,
		      0, 0, 256, RadarHeight, 0, 0);
	} else {
	    int x, y, w, h;
	    float xp, yp, xo, yo;

	    xp = (float) (pos.x * 256) / Setup->width;
	    yp = (float) (pos.y * RadarHeight) / Setup->height;
	    xo = (float) 256 / 2;
	    yo = (float) RadarHeight / 2;
	    if (xo <= xp) {
		x = (int) (xp - xo + 0.5);
	    } else {
		x = (int) (256 + xp - xo + 0.5);
	    }
	    if (yo <= yp) {
		y = (int) (yp - yo + 0.5);
	    } else {
		y = (int) (RadarHeight + yp - yo + 0.5);
	    }
	    y = RadarHeight - y - 1;
	    w = 256 - x;
	    h = RadarHeight - y;
	    XCopyArea(dpy, p_radar, radar, gc,
		      0, 0, x, y, w, h);
	    XCopyArea(dpy, p_radar, radar, gc,
		      x, 0, w, y, 0, h);
	    XCopyArea(dpy, p_radar, radar, gc,
		      0, y, x, h, w, 0);
	    XCopyArea(dpy, p_radar, radar, gc,
		      x, y, w, h, 0, 0);
	}
    }
    if (p_draw != draw)
	XCopyArea(dpy, p_draw, draw, gc,
		  0, 0, view_width, view_height, 0, 0);

    dbuff_switch(dbuf_state);

    if (colorSwitch) {
	XSetPlaneMask(dpy, gc, dbuf_state->drawing_planes);
	XSetPlaneMask(dpy, messageGC, dbuf_state->drawing_planes);
    }

    if (damaged > 0) {
	XSetFunction(dpy, gc, GXxor);
	SET_FG(colors[BLACK].pixel ^ colors[BLUE].pixel);
	XFillRectangle(dpy, draw, gc, 0, 0, draw_width, draw_height);
	XSetFunction(dpy, gc, GXcopy);
#if ERASE
	SET_BIT(erp->flags, ERASE_DAMAGED);
	Erase_end();
#endif
    }
    else {
#if ERASE
	Erase_end();
#else
	SET_FG(colors[BLACK].pixel);
	XFillRectangle(dpy, p_draw, gc, 0, 0, draw_width, draw_height);
#endif
    }

    if (talk_mapped == true) {
	static bool toggle;
	static long last_toggled;

	if (loops >= last_toggled + FPS / 2 || loops < last_toggled) {
	    toggle = (toggle == false) ? true : false;
	    last_toggled = loops;
	}
	Talk_cursor(toggle);
    }
    Paint_clock(0);

    XFlush(dpy);
}


int Handle_start(long server_loops)
{
    int			i;

    start_loops = server_loops;

    num_refuel = 0;
    num_connector = 0;
    num_smart = 0;
    num_ball = 0;
    num_ship = 0;
    num_mine = 0;
    num_itemtype = 0;
    num_shot = 0;
    num_ecm = 0;
    num_trans = 0;
    num_paused = 0;
    num_radar = 0;
    num_vcannon = 0;
    num_vfuel = 0;
    num_vbase = 0;
    for (i = 0; i < DEBRIS_TYPES; i++) {
	num_debris[i] = 0;
    }
    Erase_start();

    damaged = 0;
    destruct = 0;
    shutdown_delay = 0;
    shutdown_count = -1;
    eyesId = (self != NULL) ? self->id : 0;

    return 0;
}

int Handle_end(long server_loops)
{
    end_loops = server_loops;
    Paint_frame();
    return 0;
}

int Handle_self(int x, int y, int vx, int vy, int newHeading,
		float newPower, float newTurnspeed, float newTurnresistance,
		int newLockId, int newLockDist, int newLockBearing,
		int newNextCheckPoint,
		int newNumCloaks, int newNumSensors, int newNumMines,
		int newNumRockets, int newNumEcms, int newNumTransporters,
		int newExtraShots, int newRearShots,
		int newAfterBurners, int newNumLasers,
		int newNumTanks, int newCurrentTank,
		int newFuelSum, int newFuelMax, int newPacketSize)
{
    pos.x = x;
    pos.y = y;
    vel.x = vx;
    vel.y = vy;
    heading = newHeading;
    power = newPower;
    turnspeed = newTurnspeed;
    turnresistance = newTurnresistance;
    lock_id = newLockId;
    lock_dist = newLockDist;
    lock_dir = newLockBearing;
    nextCheckPoint = newNextCheckPoint;
    numCloaks = newNumCloaks;
    numSensors = newNumSensors;
    numMines = newNumMines;
    numRockets = newNumRockets;
    numEcms = newNumEcms;
    numTransporters = newNumTransporters;
    numFrontShots = newExtraShots;
    numBackShots = newRearShots;
    numAfterburners = newAfterBurners;
    numLasers = newNumLasers;
    numTanks = newNumTanks;
    fuelCurrent = newCurrentTank;
    if (newFuelSum > fuelSum && selfVisible != 0) {
	fuelCount = FUEL_NOTIFY;
    }
    fuelSum = newFuelSum;
    fuelMax = newFuelMax;
    selfVisible = 0;
    if (newPacketSize + 16 < packet_size) {
	packet_size -= 16;
    } else {
	packet_size = newPacketSize;
    }

    world.x = pos.x - (view_width / 2);
    world.y = pos.y - (view_height / 2);
    realWorld = world;
    wrappedWorld = 0;
    if (BIT(Setup->mode, WRAP_PLAY)) {
	if (world.x < 0) {
	    wrappedWorld |= 1;
	    world.x += Setup->width;
	} else if (world.x + view_width >= Setup->width) {
	    realWorld.x -= Setup->width;
	    wrappedWorld |= 1;
	}
	if (world.y < 0) {
	    wrappedWorld |= 2;
	    world.y += Setup->height;
	} else if (world.y + view_height >= Setup->height) {
	    realWorld.y -= Setup->height;
	    wrappedWorld |= 2;
	}
    }
    return 0;
}


int Handle_eyes(int id)
{
    eyesId = id;
    return 0;
}


int Handle_damaged(int dam)
{
    damaged = dam;
    return 0;
}


int Handle_destruct(int count)
{
    destruct = count;
    return 0;
}


int Handle_shutdown(int count, int delay)
{
    shutdown_count = count;
    shutdown_delay = delay;
    return 0;
}


int Handle_refuel(int x0, int y0, int x1, int y1)
{
    refuel_t	t;

    t.x0 = x0;
    t.x1 = x1;
    t.y0 = y0;
    t.y1 = y1;
    HANDLE(refuel_ptr, num_refuel, max_refuel, t);
    return 0;
}

int Handle_connector(int x0, int y0, int x1, int y1)
{
    connector_t	t;

    t.x0 = x0;
    t.x1 = x1;
    t.y0 = y0;
    t.y1 = y1;
    HANDLE(connector_ptr, num_connector, max_connector, t);
    return 0;
}

int Handle_laser(int color, int x, int y, int len, int dir)
{
    laser_t	t;

    t.color = color;
    t.x = x;
    t.y = y;
    t.len = len;
    t.dir = dir;
    HANDLE(laser_ptr, num_laser, max_laser, t);
    return 0;
}

int Handle_smart(int x, int y, int dir)
{
    smart_t	t;

    t.x = x;
    t.y = y;
    t.dir = dir;
    HANDLE(smart_ptr, num_smart, max_smart, t);
    return 0;
}

int Handle_ball(int x, int y, int id)
{
    ball_t	t;

    t.x = x;
    t.y = y;
    t.id = id;
    HANDLE(ball_ptr, num_ball, max_ball, t);
    return 0;
}

int Handle_ship(int x, int y, int id, int dir, int shield, int cloak)
{
    ship_t	t;

    t.x = x;
    t.y = y;
    t.id = id;
    t.dir = dir;
    t.shield = shield;
    t.cloak = cloak;
    HANDLE(ship_ptr, num_ship, max_ship, t);

    if (id == eyesId) {
	selfVisible = 1;
	return Handle_radar(x, y);
    }

    return 0;
}

int Handle_mine(int x, int y)
{
    mine_t	t;

    t.x = x;
    t.y = y;
    HANDLE(mine_ptr, num_mine, max_mine, t);
    return 0;
}

int Handle_item(int x, int y, int type)
{
    itemtype_t	t;

    t.x = x;
    t.y = y;
    t.type = type;
    HANDLE(itemtype_ptr, num_itemtype, max_itemtype, t);
    return 0;
}

int Handle_shot(int x, int y, int color)
{
    shot_t	t;

    t.x = x;
    t.y = y;
    t.color = color;
    HANDLE(shot_ptr, num_shot, max_shot, t);
    return 0;
}

int Handle_debris(int type, u_byte *p, int n)
{
#define num		(num_debris[type])
#define max		(max_debris[type])
#define ptr		(debris_ptr[type])

    if (n > max) {
	if (max == 0) {
	    ptr = (debris_t *) malloc (n * sizeof(*ptr));
	} else {
	    ptr = (debris_t *) realloc (ptr, n * sizeof(*ptr));
	}
	if (ptr == NULL) {
	    error("No memory for debris");
	    num = max = 0;
	    return -1;
	}
	max = n;
    }
    else if (n <= 0) {
	printf("debris %d < 0\n", n);
	return 0;
    }
    num = n;
    memcpy(ptr, p, n * sizeof(*ptr));
    return 0;

#undef num
#undef max
#undef ptr
}

int Handle_ecm(int x, int y, int size)
{
    ecm_t	t;

    t.x = x;
    t.y = y;
    t.size = size;
    HANDLE(ecm_ptr, num_ecm, max_ecm, t);
    return 0;
}

int Handle_trans(int x1, int y1, int x2, int y2)
{
    trans_t	t;

    t.x1 = x1;
    t.y1 = y1;
    t.x2 = x2;
    t.y2 = y2;
    HANDLE(trans_ptr, num_trans, max_trans, t);
    return 0;
}

int Handle_paused(int x, int y, int count)
{
    paused_t	t;

    t.x = x;
    t.y = y;
    t.count = count;
    HANDLE(paused_ptr, num_paused, max_paused, t);
    return 0;
}

int Handle_radar(int x, int y)
{
    radar_t	t;

    t.x = x;
    t.y = y;
    HANDLE(radar_ptr, num_radar, max_radar, t);
    return 0;
}

int Handle_message(char *msg)
{
    Add_message(msg);
    return 0;
}

int Handle_time_left(long sec)
{
    if (sec >= 0 && sec < 10 && (time_left > sec || sec == 0)) {
	XBell(dpy, 0);
	XFlush(dpy);
    }
    time_left = (sec >= 0) ? sec : 0;
    return 0;
}

int Handle_vcannon(int x, int y, int type)
{
    vcannon_t	t;

    t.x = x;
    t.y = y;
    t.type = type;
    HANDLE(vcannon_ptr, num_vcannon, max_vcannon, t);
    return 0;
}

int Handle_vfuel(int x, int y, long fuel)
{
    vfuel_t	t;

    t.x = x;
    t.y = y;
    t.fuel = fuel;
    HANDLE(vfuel_ptr, num_vfuel, max_vfuel, t);
    return 0;
}

int Handle_vbase(int x, int y, int xi, int yi, int type)
{
    vbase_t	t;

    t.x = x;
    t.y = y;
    t.xi = xi;
    t.yi = yi;
    t.type = type;
    HANDLE(vbase_ptr, num_vbase, max_vbase, t);
    return 0;
}

#define BORDER			6
#define SCORE_LIST_WINDOW_WIDTH	256

void Paint_score_start(void)
{
    static bool	first = true;
    char	headingStr[MSG_LEN];
    static int thisLine;

    if (first) {
	thisLine = BORDER + scoreListFont->ascent;
	first = false;
    }

    if (showRealName) {
	strcpy(headingStr, "NICK=USER@HOST");
    } else {
	if (BIT(Setup->mode, TEAM_PLAY))
	    strcpy(headingStr, " TM ");
	else
	    strcpy(headingStr, "  ");
	strcat(headingStr, "SCORE ");
	if (BIT(Setup->mode, LIMITED_LIVES))
	    strcat(headingStr, "LIFE");
	strcat(headingStr, " NAME");
    }

    XClearWindow(dpy, players);
    ShadowDrawString(dpy, players, scoreListGC,
		     BORDER, thisLine,
		     headingStr,
		     colors[WHITE].pixel,
		     colors[BLACK].pixel);

    gcv.line_style = LineSolid;
    XChangeGC(dpy, scoreListGC, GCLineStyle, &gcv);
    XDrawLine(dpy, players, scoreListGC,
	      BORDER, thisLine,
	      SCORE_LIST_WINDOW_WIDTH - BORDER, thisLine);

    gcv.line_style = LineOnOffDash;
    XChangeGC(dpy, scoreListGC, GCLineStyle, &gcv);

    Paint_clock(1);
}


void Paint_score_entry(int entry_num,
		       other_t* other,
		       bool best)
{
    static char	teamStr[3], lifeStr[6], label[MSG_LEN];
    static int lineSpacing = -1, firstLine;
    int thisLine;

    /*
     * First time we're here, set up miscellaneous strings for
     * efficiency and calculate some other constants.
     */
    if (lineSpacing == -1) {
	teamStr[0] = teamStr[2] = '\0';
	teamStr[1] = ' ';
	lifeStr[0] = '\0';

	lineSpacing
	    = scoreListFont->ascent + scoreListFont->descent + 3;
	firstLine
	    = 2*BORDER + scoreListFont->ascent + lineSpacing;
    }
    thisLine = firstLine + lineSpacing * entry_num;

    /*
     * Setup the status line
     */
    if (showRealName) {
	sprintf(label, "%s=%s@%s", other->name, other->real, other->host);
    } else {
	other_t*	war = Other_by_id(other->war_id);

	if (BIT(Setup->mode, TEAM_PLAY))
	    teamStr[0] = other->team + '0';

	if (BIT(Setup->mode, LIMITED_LIVES))
	    sprintf(lifeStr, " %3d", other->life);

	if (war) {
	    sprintf(label, "%c %s%5d%s  %s (%s)",
		    other->mychar, teamStr, other->score, lifeStr,
		    other->name, war->name);
	} else {
	    sprintf(label, "%c %s%5d%s  %s",
		    other->mychar, teamStr, other->score, lifeStr,
		    other->name);
	}
    }

    /*
     * Draw the line
     */
    ShadowDrawString(dpy, players, scoreListGC,
		     BORDER, thisLine,
		     label,
		     colors[WHITE].pixel,
		     colors[BLACK].pixel);

    /*
     * Underline the best player
     */
    if (best) {
	XDrawLine(dpy, players, scoreListGC,
		  BORDER, thisLine,
  		  SCORE_LIST_WINDOW_WIDTH - BORDER, thisLine);
    }
}


static void Paint_clock(int redraw)
{
    int			minute,
			hour,
			height = scoreListFont->ascent + scoreListFont->descent
				+ 3,
			border = 3;
    time_t		t;
    struct tm		*m;
    char		buf[16];
    static long		prev_loops;
    static int		width;

    if (BIT(instruments, SHOW_CLOCK) == 0) {
	if (width != 0) {
	    XSetForeground(dpy, scoreListGC, colors[windowColor].pixel);
	    XFillRectangle(dpy, players, scoreListGC,
			   256 - (width + 2 * border), 0,
			   width + 2 * border, height);
	    width = 0;
	}
	return;
    }
    if (redraw == 0
	&& loops > prev_loops
	&& loops - prev_loops < (FPS << 5)) {
	return;
    }
    prev_loops = loops;
    time(&t);
    m = localtime(&t);
    minute = m->tm_min;
    hour = m->tm_hour;
    if (minute++ == 59) {
	minute = 0;
	if (hour++ == 23) {
	    hour = 0;
	}
    }
    sprintf(buf, "%02d:%02d", hour, minute);
    width = XTextWidth(scoreListFont, buf, strlen(buf));
    XSetForeground(dpy, scoreListGC, colors[windowColor].pixel);
    XFillRectangle(dpy, players, scoreListGC,
		   256 - (width + 2 * border), 0,
		   width + 2 * border, height);
    ShadowDrawString(dpy, players, scoreListGC,
		     256 - (width + border),
		     scoreListFont->ascent + 2,
		     buf,
		     colors[WHITE].pixel,
		     colors[BLACK].pixel);
}
