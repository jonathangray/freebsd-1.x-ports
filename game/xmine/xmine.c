/* xmine - Copyright (c) 1993 by Paul Falstad
 * freely redistributable
 */

/*
 * Sorry, the original code did not have any legal notice, so i add a
 * berkeley-style one here.
 */
/*
 * xmine - Xaw version,
 * Copyright (c) 1994 by Joerg Wunsch, <joerg_wunsch@uriah.sax.de>
 *
 * This program is free software.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by Paul Falstad and
 *      Joerg Wunsch
 * 4. The name of the developers may not be used to endorse or promote
 *    products derived from this software without specific prior written
 *    permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE DEVELOPERS ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE DEVELOPERS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */


#include <stdio.h>
#include <stdlib.h>
#include <X11/Xos.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#if USE_XAW_3D
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/SmeLine.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Text.h>
#include <X11/Xaw/AsciiSrc.h>
#else /* standard Xaw */
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/SmeLine.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Text.h>
#include <X11/Xaw/AsciiSrc.h>
#endif /* USE_XAW_3D */

#include "Canvas.h"

#include "patchlevel.h"

#define GSPACEX 16
#define GSPACEY 16

/* some systems might be better off with using "random()" instead of "rand()"*/
#if defined __FreeBSD__
# define USE_RANDOM 1
#else
# define USE_RANDOM 0
#endif

#define SCORE_FILE "~/.xmine_scores"
#define TOPMARGIN 60
#define BOTMARGIN 12
#define SIDEMARGIN 12
#define STATUS_Y_OFFSET 12
#define TOTAL_WIDTH (SIDEMARGIN*2+gsizex*GSPACEX)
#define TOTAL_HEIGHT (TOPMARGIN+BOTMARGIN+gsizey*GSPACEY)
#define STATUS_HEIGHT (TOPMARGIN-STATUS_Y_OFFSET*2)
#define DIGIT_X_OFFSET (SIDEMARGIN+4)
#define DIGIT_Y_OFFSET (STATUS_Y_OFFSET+4)
#define DIGIT_HEIGHT (TOPMARGIN-DIGIT_Y_OFFSET*2)
#define DIGIT_WIDTH 16
#define DIGIT_COUNT 3
#define DIGIT_MARGIN 2
#define DIGIT_HWEDGE_WIDTH (DIGIT_WIDTH-(DIGIT_MARGIN+1)*2)
#define DIGIT_VWEDGE_HEIGHT ((DIGIT_HEIGHT-7)/2)

#define COVERED 0
#define MARKED 1
#define UNCOVERED 2
#define QUESTIONED 3
#define MINE -1
#define MAX_GSIZEX 100
#define MAX_GSIZEY 100

int grid[MAX_GSIZEX][MAX_GSIZEY]; /* fix this */
int gridview[MAX_GSIZEX][MAX_GSIZEY]; /* fix this */

#define INITIAL_SIZE_X 8
#define INITIAL_SIZE_Y 8

int gsizex = INITIAL_SIZE_X, gsizey = INITIAL_SIZE_Y;
int layed_out = 0, game_on = 1, face_armed = 0;
int questions_allowed = 0, game_level = 0;
int dialog_up;

XFontStruct *font_info;
Widget drawarea;
Display *disp;
Window win;
int colordisp;

#define COL_BLUE 0
#define COL_LIMEGREEN 1
#define COL_RED 2
#define COL_NAVY 3
#define COL_BROWN 4
#define COL_AQUA 5
#define COL_BLACK 6
#define COL_GRAY 7
#define COL_WHITE 8
#define COL_LITEGRAY 9
#define COL_DARKGRAY 10
#define COL_YELLOW 11
#define COL_DARKRED 12
#define COL_COUNT 13

char *colnames[COL_COUNT] = {
	"Blue", "SpringGreen4", "Red", "Navy",
	"Brown", "MediumAquamarine", "Black", "Gray",
	"White", "Grey75", "Grey50", "Yellow", "darkred"
};

XColor cols[COL_COUNT];

#define BW_BLACK(X) (colordisp ? (X) : COL_BLACK)
#define BW_WHITE(X) (colordisp ? (X) : COL_WHITE)

int numcols[] = {
	COL_BLUE, COL_LIMEGREEN, COL_RED, COL_NAVY,
	COL_BROWN, COL_AQUA, COL_BLACK, COL_GRAY
};

#include "cool.xbm"
#include "fillface.xbm"
#include "dead.xbm"
#include "happy.xbm"
#include "ohno.xbm"
#include "press.xbm"
#include "mine.xbm"

#define FACE_HAPPY 0
#define FACE_OHNO 1
#define FACE_COOL 2
#define FACE_DEAD 3
#define FACE_PRESS 4
#define FACE_COUNT 5

Pixmap faces[FACE_COUNT];
static char *face_bits[FACE_COUNT] = {
	happy_bits, ohno_bits, cool_bits, dead_bits, press_bits
};

Pixmap mine;

int cur_face = FACE_HAPPY;
int mine_count, covered_count, marked_count;
int armed_x, armed_y, armed, timer;
XtIntervalId timer_id;

#define FACE_BUTTON_WIDTH (fillface_width+5)
#define FACE_BUTTON_HEIGHT (fillface_height+5)
#define FACE_X_POS ((TOTAL_WIDTH-FACE_BUTTON_WIDTH)/2+1)
#define FACE_Y_POS ((TOPMARGIN-FACE_BUTTON_HEIGHT)/2)

GC gc;

#if NeedFunctionPrototypes
#define P(X) X
#else
#define P(X) ()
#endif

/* action routines */
void	search_action P ((Widget, XEvent *, String *, Cardinal *));
void	clear_action P ((Widget, XEvent *, String *, Cardinal *));
void	mark_action P ((Widget, XEvent *, String *, Cardinal *));

/* callbacks */
void	redraw_cb P ((Widget, XExposeEvent*, Region, XtPointer));
void	restart P ((Widget, XtPointer, XtPointer));
void	set_level P ((Widget, XtPointer, XtPointer));
void	set_custom P ((Widget, XtPointer, XtPointer));
void	best_times P ((Widget, XtPointer, XtPointer));
void	about P ((Widget, XtPointer, XtPointer));
void	dialog_ok P ((Widget, XtPointer, XtPointer));
void	clear_scores P ((Widget, XtPointer, XtPointer));
void	exit_game P ((Widget, XtPointer, XtPointer));
void	new_best P ((XtPointer, XtIntervalId *));
void	timer_callback P ((XtPointer, XtIntervalId *));
void	resize_handler P ((Widget, XtPointer, XEvent *, Boolean *));

/* utility functions */
void	redrawsquare P ((int, int));
void	drawsquare P ((int, int));
int	hasmine P ((int, int));
void	uncover P ((int, int));
void	clear_around P ((int, int));
int	is_state P ((int, int, int));
void	shadow_rev_rect P ((int, int, int, int, int));
void	shadow_rect P ((int, int, int, int, int));
void	layout_board P ((int, int));
void	boom P ((void));
void	draw_face P ((void));
void	set_face P ((int));
void	winner P ((void));
void	draw_blank_square P ((int, int, int));
void	draw_button P ((int, int));
void	arm_clear P ((int, int, int));
void	track_face P ((int, int, int));
void	arm_face P ((int));
void	draw_digit P ((int, int, int));
void	draw_digits P ((int, int));
void	tog_question P ((void));
void	get_text_int P ((Widget, int *));
struct scores	*get_scores P ((void));
void	write_scores P ((struct scores *));
void	get_score_file_name P ((char *));
int	cant_write_score_file P ((void));
void	fix_size P ((void));
void	relax_size P ((void));

Pixmap fillface;
XtAppContext app;
Widget toplevel, menubar;
Widget level_buttons[4];

struct scores {
	char names[3][80];
	int times[3];
};

char *fallbacks[] = {
#include "fallback.h"
	NULL
};

#if NeedFunctionPrototypes
int
main(int argc, char **argv)
#else
int
main(argc, argv)
	unsigned int argc;
	char *argv[];
#endif
{
	Widget main_w, drawing_a, menu, btn;
	XGCValues gcv;
	void draw(), redraw_cb(), set_color();
	XColor unused;
	int i;

	XtActionsRec actions[3];
	String translations =
		"<Btn1Down>:   search(down)\n\
		 <Btn1Up>:     search(up)\n\
	         <Btn1Motion>: search(move)\n\
		 <Btn2Down>:   clear(down)\n\
		 <Btn2Motion>: clear(move)\n\
		 <Btn2Up>:     clear(up)\n\
		 <Btn3Down>:   mark(down)\n";

	toplevel = XtVaAppInitialize(&app, "Xmine", NULL, 0,
				     &argc, argv, fallbacks, NULL);

	main_w = XtVaCreateManagedWidget("main_w", boxWidgetClass, toplevel,
					 XtNorientation, XtorientVertical,
					 NULL);
	colordisp = PlanesOfScreen(XtScreen(main_w)) > 1;

	menubar = XtVaCreateManagedWidget("menubar", boxWidgetClass, main_w,
					  XtNtop, XawChainTop,
					  XtNbottom, XawChainTop,
					  XtNleft, XawChainLeft,
					  XtNright, XawChainLeft,
					  NULL);
	XtVaCreateManagedWidget("game", menuButtonWidgetClass,
				menubar,
				XtNmenuName, "game_menu",
				NULL);

	menu = XtVaCreatePopupShell("game_menu", simpleMenuWidgetClass,
				    menubar, NULL);

	btn = XtVaCreateManagedWidget("new",
				      smeBSBObjectClass, menu,
				      NULL);
	XtAddCallback(btn, XtNcallback, restart, NULL);

	XtVaCreateManagedWidget("_sep1", smeLineObjectClass, menu, NULL);

	level_buttons[0] = btn =
		XtVaCreateManagedWidget("beginner",
					smeBSBObjectClass, menu,
					NULL);
	XtAddCallback(btn, XtNcallback, set_level,
		      (XtPointer) 1);

	level_buttons[1] = btn =
		XtVaCreateManagedWidget("intermediate",
					smeBSBObjectClass, menu,
					NULL);
	XtAddCallback(btn, XtNcallback, set_level,
		      (XtPointer) 2);

	level_buttons[2] = btn =
		XtVaCreateManagedWidget("expert",
					smeBSBObjectClass, menu,
					NULL);
	XtAddCallback(btn, XtNcallback, set_level,
		      (XtPointer) 3);

	level_buttons[3] = btn =
		XtVaCreateManagedWidget("custom_popup",
					smeBSBObjectClass, menu,
					NULL);
	XtAddCallback(btn, XtNcallback, set_custom, NULL);

	XtVaCreateManagedWidget("_sep2", smeLineObjectClass, menu, NULL);

	btn = XtVaCreateManagedWidget("marks",
				      smeBSBObjectClass, menu,
				      NULL);
	XtAddCallback(btn, XtNcallback, (XtPointer) tog_question, NULL);

	XtVaCreateManagedWidget("_sep3", smeLineObjectClass, menu, NULL);

	btn = XtVaCreateManagedWidget("best_popup",
				      smeBSBObjectClass, menu,
				      NULL);
	XtAddCallback(btn, XtNcallback, best_times, NULL);
	btn = XtVaCreateManagedWidget("about_popup",
				      smeBSBObjectClass, menu,
				      NULL);
	XtAddCallback(btn, XtNcallback, about, NULL);
	btn = XtVaCreateManagedWidget("exit",
				      smeBSBObjectClass,   menu,
				      NULL);
	XtAddCallback(btn, XtNcallback, exit_game, NULL);
	XtManageChild(menubar);

	for (i = 0; i != COL_COUNT; i++)
		XAllocNamedColor(XtDisplay(main_w),
				 DefaultColormapOfScreen(XtScreen(main_w)),
				 colnames[i], &cols[i], &unused);

	cols[COL_DARKRED].red = 60*256;
	cols[COL_DARKRED].green = cols[COL_DARKRED].blue = 0;
	XAllocColor(XtDisplay(main_w),
		    DefaultColormapOfScreen(XtScreen(main_w)),
		    &cols[COL_DARKRED]);


	gcv.foreground = cols[COL_WHITE].pixel;
	gc = XCreateGC(XtDisplay(main_w),
		       RootWindowOfScreen(XtScreen(main_w)),
		       GCForeground, &gcv);

	actions[0].string = "clear";
	actions[0].proc = clear_action;
	actions[1].string = "search";
	actions[1].proc = search_action;
	actions[2].string = "mark";
	actions[2].proc = mark_action;
	XtAppAddActions(app, actions, 3);

	drawing_a = XtVaCreateManagedWidget
		("drawing_a",
		 canvasWidgetClass, main_w,
		 XtNtranslations, XtParseTranslationTable(translations),
		 XtNwidth,        GSPACEX*gsizex+SIDEMARGIN*2,
		 XtNheight,       GSPACEY*gsizey+TOPMARGIN+BOTMARGIN,
		 XtNresizable,    False,  /* remain this a fixed size */
		 XtNbackground,	  cols[BW_WHITE(COL_LITEGRAY)].pixel,
		 XtNexposeProc,   redraw_cb,
		 NULL);

	font_info = XLoadQueryFont(XtDisplay(main_w),
				   "-*-courier-bold-r-normal-*-*-120-*");
	XSetFont(XtDisplay(main_w), gc, font_info->fid);

	set_level((Widget) 0, (XtPointer) 1, NULL);
	XtAddEventHandler(toplevel,
			  StructureNotifyMask, False, resize_handler, 0);
	XtRealizeWidget(toplevel);
	fillface = XCreateBitmapFromData(XtDisplay(main_w),
					 XtWindow(main_w), fillface_bits,
					 fillface_width, fillface_height);
	for (i = 0; i != FACE_COUNT; i++)
		faces[i] =
			XCreateBitmapFromData(XtDisplay(main_w),
					      XtWindow(main_w), face_bits[i],
					      fillface_width, fillface_height);

	drawarea = drawing_a;
	win = XtWindow(drawing_a);
	disp = XtDisplay(drawing_a);

	mine = XCreatePixmapFromBitmapData
		(XtDisplay(main_w), XtWindow(main_w),
		 mine_bits, mine_width, mine_height,
		 cols[COL_BLACK].pixel, cols[BW_WHITE(COL_LITEGRAY)].pixel,
		 DefaultDepth(disp, DefaultScreen(disp)));
	
	XtAppMainLoop(app);
	return 0;
}

#if NeedFunctionPrototypes
void
search_action(Widget widget, XEvent *ev, String *args, Cardinal *num_args)
#else
void
search_action(widget, ev, args, num_args)
	Widget widget;
	XEvent *ev;
	String *args;
	Cardinal *num_args;
#endif
{
	int xg, yg;
	XButtonEvent *event = (XButtonEvent *)ev;

	track_face(event->x, event->y, *args[0]);
	if (!game_on) return;
	xg = (event->x-SIDEMARGIN)/GSPACEX;
	yg = (event->y-TOPMARGIN)/GSPACEY;
	if (!strcmp(args[0], "down")) {
		armed = False;
		if (!(xg >= 0 && xg < gsizex && yg >= 0 && yg < gsizey))
			return;
		if (gridview[xg][yg] != COVERED) return;
		armed_x = xg; armed_y = yg;
		armed = True;
		set_face(FACE_OHNO);
		draw_blank_square(xg, yg, True);
		return;
	}
	if (!strcmp(args[0], "move")) {
		if (xg == armed_x && yg == armed_y) {
			if (!armed) {
				set_face(FACE_OHNO);
				draw_blank_square(armed_x, armed_y, True);
				armed = True;
			}
		} else {
			if (armed) {
				set_face(FACE_HAPPY);
				draw_button(armed_x, armed_y);
				armed = False;
			}
		}
		return;
	}
	if (!armed) return;
	if (!layed_out) layout_board(armed_x, armed_y);
	uncover(armed_x, armed_y);
	if (game_on) set_face(FACE_HAPPY);
}

#if NeedFunctionPrototypes
void
mark_action(Widget widget, XEvent *ev, String *args, Cardinal *num_args)
#else
void
mark_action(widget, ev, args, num_args)
	Widget widget;
	XEvent *ev;
	String *args;
	Cardinal *num_args;
#endif
{
	XButtonEvent *event = (XButtonEvent *)ev;

	if (!game_on) return;
	if (!layed_out) return;
	if (!strcmp(args[0], "down")) {
		int xg, yg;

		xg = (event->x-SIDEMARGIN)/GSPACEX;
		yg = (event->y-TOPMARGIN)/GSPACEY;
		if (xg >= 0 && xg < gsizex &&
		    yg >= 0 && yg < gsizey) {
			if (gridview[xg][yg] == UNCOVERED) return;
			if (gridview[xg][yg] == MARKED) {
				gridview[xg][yg] =
					(questions_allowed) ?
					QUESTIONED : COVERED;
				marked_count--;
			} else if (gridview[xg][yg] == QUESTIONED)
				gridview[xg][yg] = COVERED;
			else {
				gridview[xg][yg] = MARKED;
				marked_count++;
			}
			redrawsquare(xg, yg);
			draw_digits(mine_count-marked_count, 0);
			draw_digits(timer, 1);
		}
	}
}

#if NeedFunctionPrototypes
void
clear_action(Widget widget, XEvent *ev, String *args, Cardinal *num_args)
#else
void
clear_action(widget, ev, args, num_args)
	Widget widget;
	XEvent *ev;
	String *args;
	Cardinal *num_args;
#endif
{
	int xg, yg;
	XButtonEvent *event = (XButtonEvent *)ev;

	if (!game_on) return;
	if (!layed_out) return;
	xg = (event->x-SIDEMARGIN)/GSPACEX;
	yg = (event->y-TOPMARGIN)/GSPACEY;
	if (!(xg >= 0 && xg < gsizex && yg >= 0 && yg < gsizey))
		return;
	if (!strcmp(args[0], "down")) {
		arm_clear(xg, yg, True);
		armed_x = xg; armed_y = yg;
		return;
	}
	if (!strcmp(args[0], "move")) {
		int arm_it = (xg == armed_x && yg == armed_y);
		if (arm_it != armed) arm_clear(armed_x, armed_y, arm_it);
		return;
	}
	if (!armed) return;
	arm_clear(armed_x, armed_y, False);
	clear_around(xg, yg);
	if (game_on)
		set_face(FACE_HAPPY);
}

#if NeedFunctionPrototypes
void
track_face(int x, int y, int op)
#else
void
track_face(x, y, op)
	int x, y, op;
#endif
{
	int onface = False;

	if (x >= FACE_X_POS && x <= FACE_X_POS+FACE_BUTTON_WIDTH &&
	    y >= FACE_Y_POS && y <= FACE_Y_POS+FACE_BUTTON_HEIGHT)
		onface = True;
	if (op == 'u') {
		if (!face_armed) return;
		arm_face(False);
		restart(NULL, NULL, NULL);
		return;
	}
	if (op == 'd') {
		if (!onface) return;
		arm_face(True);
		armed = False;
		return;
	}
	if (onface != face_armed)
		arm_face(onface);
}

#if NeedFunctionPrototypes
void
arm_face(int armit)
#else
void
arm_face(armit)
	int armit;
#endif
{
	face_armed = armit;
	XClearArea(disp, win,
		   FACE_X_POS, FACE_Y_POS,
		   FACE_BUTTON_WIDTH, FACE_BUTTON_HEIGHT,
		   False);
	if (face_armed)
		shadow_rev_rect(FACE_X_POS+2, FACE_Y_POS+2,
				FACE_BUTTON_WIDTH-4, FACE_BUTTON_HEIGHT-4, 2);
	else
		shadow_rect(FACE_X_POS, FACE_Y_POS,
			    FACE_BUTTON_WIDTH, FACE_BUTTON_HEIGHT, 2);
	draw_face();
}


#if NeedFunctionPrototypes
void
arm_clear(int x, int y, int armit)
#else
void
arm_clear(x, y, armit)
	int x, y, armit;
#endif
{
	int dx, dy;

	armed = armit;
	set_face(armit ? FACE_OHNO : FACE_HAPPY);
	for (dx = -1; dx <= 1; dx++)
		for (dy = -1; dy <= 1; dy++)
			if (is_state(x+dx, y+dy, COVERED))
				if (armit)
					draw_blank_square(x+dx, y+dy, True);
				else
					draw_button(x+dx, y+dy);
}

#if NeedFunctionPrototypes
void
draw_face(void)
#else
void
draw_face()
#endif

{
	int x, y;

	x = (TOTAL_WIDTH-fillface_width)/2+face_armed;
	y = (TOPMARGIN-fillface_height)/2+face_armed;
	XSetClipMask(disp, gc, fillface);
	XSetClipOrigin(disp, gc, x, y);
	XSetBackground(disp, gc, cols[BW_WHITE(COL_YELLOW)].pixel);
	XSetForeground(disp, gc, cols[COL_BLACK].pixel);
	XCopyPlane(disp,
		   (Drawable) faces[(face_armed) ? FACE_PRESS : cur_face],
		   win, gc, 0, 0,
		   fillface_width, fillface_height, x, y, 1L);
	XSetClipMask(disp, gc, None);
}

#if NeedFunctionPrototypes
void
set_face(int f)
#else
void
set_face(f)
	int f;
#endif
{
	if (cur_face == f) return;
	cur_face = f;
	draw_face();
}

/* ARGSUSED */
#if NeedFunctionPrototypes
void
redraw_cb(Widget drawing_a, XExposeEvent *event,
	  Region r, XtPointer client_data)
#else
void
redraw_cb(drawing_a, event, r, client_data)
	Widget    drawing_a;
	XExposeEvent *event;
	Region r;
	XtPointer client_data;
#endif
{
	int x, y, w, h, xf, yf, xt, yt;

	x = event->x-SIDEMARGIN;
	y = event->y-TOPMARGIN;
	w = event->width;
	h = event->height;
	xf = x/GSPACEX;
	yf = y/GSPACEY;
	xt = (x+w+GSPACEX-1)/GSPACEX;
	yt = (y+h+GSPACEX-1)/GSPACEX;
	for (x = xf; x <= xt; x++)
		for (y = yf; y <= yt; y++)
			drawsquare(x, y);
	if (xf < 0 || yf < 0 || xt >= gsizex || yt >= gsizey) {
		shadow_rect(0, 0, TOTAL_WIDTH, TOTAL_HEIGHT, 3);
		shadow_rev_rect(SIDEMARGIN, TOPMARGIN,
				TOTAL_WIDTH-SIDEMARGIN*2,
				TOTAL_HEIGHT-(TOPMARGIN+BOTMARGIN), 3);
		shadow_rev_rect(SIDEMARGIN, STATUS_Y_OFFSET,
				TOTAL_WIDTH-SIDEMARGIN*2,
				STATUS_HEIGHT, 3);
		shadow_rect(FACE_X_POS, FACE_Y_POS,
			    FACE_BUTTON_WIDTH, FACE_BUTTON_HEIGHT, 2);
		draw_face();
		draw_digits(mine_count-marked_count, 0);
		draw_digits(timer, 1);
	}
}

#if NeedFuncionPrototypes
void
redrawsquare(int x, int y)
#else
void
redrawsquare(x, y)
	int x, y;
#endif
{
	XClearArea(disp, win, x*GSPACEX+SIDEMARGIN,
		   y*GSPACEY+TOPMARGIN, GSPACEX, GSPACEY, True);
}

#if NeedFunctionPrototypes
void
redraw_all(void)
#else
void
redraw_all()
#endif
{
	XClearArea(disp, win, SIDEMARGIN,
		   TOPMARGIN, GSPACEX*gsizex, GSPACEY*gsizey, True);
}

#if NeedFunctionPrototypes
void
redraw_entire(void)
#else
void
redraw_entire()
#endif
{
	if (disp)
		XClearArea(disp, win, 0, 0, TOTAL_WIDTH, TOTAL_HEIGHT, True);
}

#if NeedFunctionPrototypes
void
shadow_rect(int xp, int yp, int w, int h, int thick)
#else
void
shadow_rect(xp, yp, w, h, thick)
	int xp, yp, w, h, thick;
#endif
{
	int off = 0;

	w--, h--;
	while (thick--) {
		XSetForeground(disp, gc, cols[COL_WHITE].pixel);
		XDrawLine(disp, win, gc,
			  xp, yp+off, xp+w-off, yp+off);
		XDrawLine(disp, win, gc,
			  xp+off, yp, xp+off, yp+h-off);
		XSetForeground(disp, gc, cols[BW_BLACK(COL_DARKGRAY)].pixel);
		XDrawLine(disp, win, gc,
			  xp+off, yp+h-off, xp+w, yp+h-off);
		XDrawLine(disp, win, gc,
			  xp+w-off, yp+off, xp+w-off, yp+h);
		off++;
	}
}

#if NeedFunctionPrototypes
void
shadow_rev_rect(int xp, int yp, int w, int h, int thick)
#else
void
shadow_rev_rect(xp, yp, w, h, thick)
	int xp, yp, w, h, thick;
#endif
{
	int off = 0;

	w--, h--;
	xp -= thick; yp -= thick;
	w += thick*2; h += thick*2;
	while (thick--) {
		XSetForeground(disp, gc, cols[BW_BLACK(COL_DARKGRAY)].pixel);
		XDrawLine(disp, win, gc,
			  xp, yp+off, xp+w-off, yp+off);
		XDrawLine(disp, win, gc,
			  xp+off, yp, xp+off, yp+h-off);
		XSetForeground(disp, gc, cols[COL_WHITE].pixel);
		XDrawLine(disp, win, gc,
			  xp+off, yp+h-off, xp+w, yp+h-off);
		XDrawLine(disp, win, gc,
			  xp+w-off, yp+off, xp+w-off, yp+h);
		off++;
	}
}

#if NeedFunctionPrototypes
void
draw_button(int xp, int yp)
#else
void
draw_button(xp, yp)
	int xp, yp;
#endif
{
	xp = xp*GSPACEX+SIDEMARGIN;
	yp = yp*GSPACEY+TOPMARGIN;
	shadow_rect(xp, yp, GSPACEX, GSPACEY, 2);
}

#if NeedFunctionPrototypes
void
draw_blank_square(int xp, int yp, int clr)
#else
void
draw_blank_square(xp, yp, clr)
	int xp, yp, clr;
#endif
{
	xp = xp*GSPACEX+SIDEMARGIN;
	yp = yp*GSPACEY+TOPMARGIN;
	if (clr)
		XClearArea(disp, win,
			   xp, yp, GSPACEX, GSPACEY, False);
	XSetForeground(disp, gc, cols[BW_BLACK(COL_DARKGRAY)].pixel);
	XDrawLine(disp, win, gc,
		  xp, yp, xp+GSPACEX-1, yp);
	XDrawLine(disp, win, gc,
		  xp, yp, xp, yp+GSPACEY-1);
}

#if NeedFunctionPrototypes
void
drawsquare(int x, int y)
#else
void
drawsquare(x, y)
	int x, y;
#endif
{
	int xp, yp;
	char buf[2];
	int wid;
	int color, yoffset = 0;

	if (x < 0 || y < 0 || x >= gsizex || y >= gsizey) return;
	buf[1] = 0;
	xp = x*GSPACEX+SIDEMARGIN;
	yp = y*GSPACEY+TOPMARGIN;
	if (gridview[x][y] == UNCOVERED) {
		draw_blank_square(x, y, False);
		if (!grid[x][y]) return;
		if (grid[x][y] == MINE) {
			buf[0] = 'M';
			color = COL_BLACK;
		} else {
			color = numcols[grid[x][y]-1];
			buf[0] = grid[x][y]+'0';
			yoffset = 2;
		}
	} else if (gridview[x][y] == MARKED) {
		draw_button(x, y);
		buf[0] = 'X';
		color = COL_RED;
	} else if (gridview[x][y] == QUESTIONED) {
		draw_button(x, y);
		buf[0] = '?';
		color = COL_RED;
	} else {
		draw_button(x, y);
		return;
	}

	wid = XTextWidth(font_info, buf, 1);
	XSetForeground(disp, gc, cols[BW_BLACK(color)].pixel);
	if(buf[0] == 'M')
		XCopyArea(disp, mine, win, gc, 0, 0, mine_width, mine_height,
			  xp+(GSPACEX-mine_width)/2,
			  yp+(GSPACEY-mine_height)/2+yoffset);
	else
		XDrawString(disp, win, gc,
			    xp+(GSPACEX-wid)/2,
			    yp+(GSPACEY+font_info->ascent-font_info->descent)
			    /2+yoffset,
			    buf, 1);
}

#if NeedFunctionPrototypes
void
layout_board(int fx, int fy)
#else
void
layout_board(fx, fy)
	int fx, fy;
#endif
{
	int i, x, y, xd, yd, tries;

#if USE_RANDOM
	srandom((unsigned) time(0));
#else
	srand((unsigned int) time(0));
#endif
	for (i = 0; i != mine_count; i++) {
		tries = 1000;
		do {
#if USE_RANDOM
			x = (random()>>1) % gsizex;
			y = (random()>>1) % gsizey;
#else
			x = (rand()>>1) % gsizex;
			y = (rand()>>1) % gsizey;
#endif
			tries--;
		} while (tries && (grid[x][y] ||
				   !(x < fx-1 || x > fx+1 || y < fy-1
				     || y > fy+1)));
		grid[x][y] = MINE;
		if (!tries) {
			mine_count = i;
			break;
		}
	}
	for (x = 0; x != gsizex; x++)
		for (y = 0; y != gsizey; y++) {
			if (grid[x][y] == MINE) continue;
			i = 0;
			for (xd = -1; xd <= 1; xd++)
				for (yd = -1; yd <= 1; yd++)
					if (hasmine(x+xd, y+yd)) i++;
			grid[x][y] = i;
		}
	layed_out = 1;
	if (timer_id)
		XtRemoveTimeOut(timer_id);
	timer_id = XtAppAddTimeOut(app, 1000L, timer_callback,
				   NULL);
	covered_count = gsizex*gsizey;
	fix_size();
}

/* ARGSUSED */
#if NeedFunctionPrototypes
void
timer_callback(XtPointer closure, XtIntervalId *id)
#else
void
timer_callback(closure, id)
	XtPointer closure;
	XtIntervalId *id;
#endif
{
	if (!(layed_out && game_on)) {
		timer_id = 0;
		return;
	}
	if (timer >= 999) return;
	draw_digits(++timer, 1);
	timer_id =
		XtAppAddTimeOut(app, 1000L, timer_callback, NULL);
}

#if NeedFunctionPrototypes
int
hasmine(int x, int y)
#else
int
hasmine(x, y)
	int x, y;
#endif
{
	if (x < 0 || y < 0 || x >= gsizex || y >= gsizey) return False;
	return grid[x][y] == MINE;
}

#if NeedFunctionPrototypes
int
is_state(int x, int y, int state)
#else
int
is_state(x, y, state)
	int x, y, state;
#endif
{
	if (x < 0 || y < 0 || x >= gsizex || y >= gsizey) return False;
	return gridview[x][y] == state;
}

#if NeedFunctionPrototypes
int
iscovered(int x, int y)
#else
int
iscovered(x, y)
	int x, y;
#endif
{
	if (x < 0 || y < 0 || x >= gsizex || y >= gsizey) return False;
	return gridview[x][y] == COVERED;
}

#if NeedFunctionPrototypes
void
uncover(int x, int y)
#else
void
uncover(x, y)
	int x, y;
#endif
{
	int dx, dy;

	if (x < 0 || y < 0 || x >= gsizex || y >= gsizey) return;
	if (gridview[x][y] != COVERED) return;
	gridview[x][y] = UNCOVERED;
	covered_count--;
	if (grid[x][y] == MINE)
	boom();
	else if (covered_count == mine_count)
		winner();
	redrawsquare(x, y);
	if (grid[x][y] == 0)
		for (dx = -1; dx <= 1; dx++)
			for (dy = -1; dy <= 1; dy++)
				uncover(x+dx, y+dy);
}

#if NeedFunctionPrototypes
void
boom(void)
#else
void
boom()
#endif
{
	int x, y;

	for (x = 0; x != gsizex; x++)
		for (y = 0; y != gsizey; y++) {
			if (grid[x][y] != MINE || gridview[x][y] == MARKED)
				continue;
			gridview[x][y] = UNCOVERED;
			redrawsquare(x, y);
		}
	game_on = 0;
	relax_size();
	set_face(FACE_DEAD);
}

#if NeedFunctionPrototypes
void
winner(void)
#else
void
winner()
#endif
{
	int x, y;

	game_on = 0;
	relax_size();
	for (x = 0; x != gsizex; x++)
		for (y = 0; y != gsizey; y++)
			if (gridview[x][y] == COVERED) {
				gridview[x][y] = MARKED;
				redrawsquare(x, y);
			}
	draw_digits(0, 0);
	set_face(FACE_COOL);
	XtAppAddTimeOut(app, 500L, new_best,
			(XtPointer) game_level);
}

/* ARGSUSED */
#if NeedFunctionPrototypes
void
restart(Widget w, XtPointer closure, XtPointer call_data)
#else
void
restart(w, closure, call_data)
	Widget w;
	XtPointer closure, call_data;
#endif
{
	int x, y;

	for (x = 0; x != gsizex; x++)
		for (y = 0; y != gsizey; y++) {
			gridview[x][y] = COVERED;
			grid[x][y] = 0;
		}
	game_on = 1;
	layed_out = 0;
	relax_size();
	if (drawarea) {
		redraw_all();
		set_face(FACE_HAPPY);
	}
	timer = marked_count = 0;
}

#if NeedFunctionPrototypes
void
clear_around(int x, int y)
#else
void
clear_around(x, y)
	int x, y;
#endif
{
	int dx, dy, ct = 0;

	if (gridview[x][y] != UNCOVERED) return;
	for (dx = -1; dx <= 1; dx++)
		for (dy = -1; dy <= 1; dy++)
			if (is_state(x+dx, y+dy, MARKED)) ct++;
	if (grid[x][y] != ct) return;
	for (dx = -1; dx <= 1; dx++)
		for (dy = -1; dy <= 1; dy++)
			if (is_state(x+dx, y+dy, COVERED)) uncover(x+dx, y+dy);
}

#if NeedFunctionPrototypes
void
draw_hwedge(int x, int y, int dir)
#else
void
draw_hwedge(x, y, dir)
	int x, y, dir;
#endif
{
	int i;

	for (i = 0; i <= 2; i++, y += dir)
		XDrawLine(disp, win, gc, x+i, y,
			  x+DIGIT_HWEDGE_WIDTH-(i+1), y);
}

#if NeedFunctionPrototypes
void
draw_midwedge(int x, int y)
#else
void
draw_midwedge(x, y)
	int x, y;
#endif
{

	XDrawLine(disp, win, gc, x, y, x+DIGIT_HWEDGE_WIDTH-1, y);
	XDrawLine(disp, win, gc, x+1, y+1, x+DIGIT_HWEDGE_WIDTH-2, y+1);
	XDrawLine(disp, win, gc, x+1, y-1, x+DIGIT_HWEDGE_WIDTH-2, y-1);
}

#if NeedFunctionPrototypes
void
draw_vwedge(int x, int y, int dir)
#else
void
draw_vwedge(x, y, dir)
	int x, y, dir;
#endif
{
	int i;

	for (i = 0; i <= 2; i++, x += dir)
		XDrawLine(disp, win, gc, x, y+i,
			  x, y+DIGIT_VWEDGE_HEIGHT-(i+1));
}

#if NeedFunctionPrototypes
void
draw_digit(int val, int num, int which)
#else
void
draw_digit(val, num, which)
	int val, num, which;
#endif
{
	int x, y;
	static int segs[11] = {
		127-2, 32+64, 127-(8+64),
		127-(8+16), 8+32+2+64, 127-(32+16),
		127-32, 1+32+64, 127,
		127-16, 2
	};
	int seg = segs[val];
	int segcol1 = BW_WHITE(COL_RED);
	int segcol0 = BW_BLACK(COL_DARKRED);

	x = !which ? DIGIT_X_OFFSET :
		TOTAL_WIDTH-(DIGIT_X_OFFSET+DIGIT_COUNT*DIGIT_WIDTH);
	x += num*DIGIT_WIDTH+DIGIT_MARGIN;
	y = DIGIT_Y_OFFSET+DIGIT_MARGIN+1;
	XSetForeground(disp, gc, cols[(seg & 1) ? segcol1 : segcol0].pixel);
	draw_hwedge(x+1, y, 1);
	XSetForeground(disp, gc, cols[(seg & 2) ? segcol1 : segcol0].pixel);
	draw_midwedge(x+1, y+1+DIGIT_VWEDGE_HEIGHT);
	XSetForeground(disp, gc, cols[(seg & 4) ? segcol1 : segcol0].pixel);
	draw_hwedge(x+1, y+(1+DIGIT_VWEDGE_HEIGHT)*2, -1);
	XSetForeground(disp, gc, cols[(seg & 8) ? segcol1 : segcol0].pixel);
	draw_vwedge(x, y+1, 1);
	XSetForeground(disp, gc, cols[(seg & 16) ? segcol1 : segcol0].pixel);
	draw_vwedge(x, y+2+DIGIT_VWEDGE_HEIGHT, 1);
	XSetForeground(disp, gc, cols[(seg & 32) ? segcol1 : segcol0].pixel);
	draw_vwedge(x+1+DIGIT_HWEDGE_WIDTH, y+1, -1);
	XSetForeground(disp, gc, cols[(seg & 64) ? segcol1 : segcol0].pixel);
	draw_vwedge(x+1+DIGIT_HWEDGE_WIDTH, y+2+DIGIT_VWEDGE_HEIGHT, -1);
}

#if NeedFunctionPrototypes
void
draw_digits(int val, int which)
#else
void
draw_digits(val, which)
	int val, which;
#endif
{
	XSetForeground(disp, gc, cols[COL_BLACK].pixel);
	XFillRectangle(disp, win, gc,
		       !which ? DIGIT_X_OFFSET :
		       TOTAL_WIDTH-(DIGIT_X_OFFSET+DIGIT_COUNT*DIGIT_WIDTH),
		       DIGIT_Y_OFFSET,
		       DIGIT_WIDTH * DIGIT_COUNT, DIGIT_HEIGHT);
	if (val < 0) {
		draw_digit(10, 0, which);
		val = -val;
	} else
		draw_digit((val/100)%10, 0, which);
	draw_digit((val/10)%10, 1, which);
	draw_digit(val%10, 2, which);
}

/* ARGSUSED */
#if NeedFunctionPrototypes
void
set_level(Widget w, XtPointer closure, XtPointer call_data)
#else
void
set_level(w, level)
	Widget w;
	XtPointer closure, call_data;
#endif
{
	int level = (int)closure;

	if (level == 1) {
		mine_count = 10;
		gsizex = gsizey = 8;
	} else if (level == 2) {
		mine_count = 40;
		gsizex = gsizey = 16;
	} else if (level == 3) {
		mine_count = 99;
		gsizex = 30;
		gsizey = 16;
	}
	game_level = level-1;
	if (!drawarea) return;
/*
	for (i = 0; i != XtNumber(level_buttons); i++)
		if (i != level-1)
			XmToggleButtonSetState(level_buttons[i], False, False);
 */
	XtVaSetValues(toplevel, XtNallowShellResize, True, NULL);
	XtVaSetValues(drawarea,
		      XtNwidth,        GSPACEX*gsizex+SIDEMARGIN*2,
		      XtNheight,       GSPACEY*gsizey+TOPMARGIN+BOTMARGIN,
		      NULL);
	XtVaSetValues(toplevel, XtNallowShellResize, False, NULL);
	redraw_entire();
	restart(NULL, NULL, NULL);
}

#if NeedFunctionPrototypes
void
tog_question(void)
#else
void
tog_question()
#endif
{
	questions_allowed ^= 1;
}

/* ARGSUSED */
#if NeedFunctionPrototypes
void
set_custom(Widget dummy, XtPointer closure, XtPointer call_data)
#else
void
set_custom(dummy, closure, call_data)
	Widget dummy;
	XtPointer closure, call_data;
#endif
{
	Widget custom, pane, w, rc2, mine_w, height_w, width_w;
	char buf[20];

	custom = XtVaCreatePopupShell("custom",
				      topLevelShellWidgetClass, toplevel,
				      NULL);
	pane = XtVaCreateWidget("custom_pane", panedWidgetClass, custom,
				NULL);
	rc2 = XtVaCreateManagedWidget("custom_input", boxWidgetClass, pane,
				      NULL);

	sprintf(buf, "%d", gsizey);
	height_w = XtVaCreateManagedWidget("height_text", dialogWidgetClass,
					   rc2,
					   XtNorientation, XtorientHorizontal,
					   XtNvalue, buf,
					   NULL);

	sprintf(buf, "%d", gsizex);
	width_w = XtVaCreateManagedWidget("width_text",
					  dialogWidgetClass, rc2,
					  XtNorientation, XtorientHorizontal,
					  XtNvalue, buf,
					  NULL);

	sprintf(buf, "%d", mine_count);
	mine_w = XtVaCreateManagedWidget("mines_text",
					 dialogWidgetClass, rc2,
					 XtNorientation, XtorientHorizontal,
					 XtNvalue, buf,
					 NULL);

	rc2 = XtVaCreateManagedWidget("custom_commands",
				      boxWidgetClass, pane,
				      NULL);

	w = XtVaCreateManagedWidget("ok",
				    commandWidgetClass, rc2,
				    XtNtop, XawChainTop,
				    XtNbottom, XawChainTop,
				    XtNleft, XawChainLeft,
				    XtNright, XawChainLeft,
				    NULL);
	XtAddCallback(w, XtNcallback, dialog_ok, NULL);
	XtManageChild(pane);
	XtPopup(custom, XtGrabExclusive);
	dialog_up = True;
	while (dialog_up) {
		XtAppProcessEvent(app, XtIMAll);
		XSync(disp, 0);
	}
	XtPopdown(custom);
	get_text_int(width_w, &gsizex);
	if (gsizex > 100) gsizex = 100;
	if (gsizex < 8) gsizex = 8;
	get_text_int(height_w, &gsizey);
	if (gsizey > 100) gsizey = 100;
	if (gsizey < 8) gsizey = 8;
	get_text_int(mine_w, &mine_count);
	if (mine_count < 1) mine_count = 1;
	set_level((Widget) 0, (XtPointer) 4, NULL);
}

#if NeedFunctionPrototypes
void
dialog_ok(Widget w, XtPointer closure, XtPointer call_data)
#else
void
dialog_ok(w, closure, call_data)
	Widget w;
	XtPointer closure, call_data;
#endif
{
	dialog_up = False;
}

#if NeedFunctionPrototypes
void
get_text_int(Widget w, int *val)
#else
void
get_text_int(w, val)
	Widget w;
	int *val;
#endif
{
	char *str;

	str = XawDialogGetValueString(w);
	if (!str) return;
	if (atoi(str)) *val = atoi(str);
	XawAsciiSourceFreeString(w);
}

/* ARGSUSED */
#if NeedFunctionPrototypes
void
best_times(Widget dummy, XtPointer closure, XtPointer call_data)
#else
void
best_times(dummy, closure, call_data)
	Widget dummy;
	XtPointer closure, call_data;
#endif
{
	Widget best, pane, w, rc, rc2, ok_w;
	Widget matrix[9];
	char buf[20];
	struct scores *sc;

	sc = get_scores();
	best = XtVaCreatePopupShell("best",
				    topLevelShellWidgetClass, toplevel,
				    NULL);
	pane = XtVaCreateWidget("scores", panedWidgetClass, best,
				NULL);
	XtVaCreateManagedWidget("label", labelWidgetClass, pane, NULL);
	rc2 = XtVaCreateManagedWidget("scoretable", boxWidgetClass, pane,
				      NULL);

	rc = XtVaCreateManagedWidget("beginner_score", boxWidgetClass, rc2,
				     NULL);
	
	matrix[0] = XtVaCreateManagedWidget("begin_label",
					    labelWidgetClass, rc,
					    XtNtop, XawChainTop,
					    XtNbottom, XawChainTop,
					    XtNleft, XawChainLeft,
					    XtNright, XawChainLeft,
					    NULL);
	sprintf(buf, "%d", sc->times[0]);
	matrix[1] = XtVaCreateManagedWidget("begin_time",
					    labelWidgetClass, rc,
					    XtNlabel, buf,
					    NULL);

	matrix[2] = XtVaCreateManagedWidget("begin_name",
					    labelWidgetClass, rc,
					    XtNtop, XawChainTop,
					    XtNbottom, XawChainTop,
					    XtNleft, XawChainLeft,
					    XtNright, XawChainLeft,
					    XtNlabel, sc->names[0],
					    NULL);

	rc = XtVaCreateManagedWidget("intermediate_score", boxWidgetClass, rc2,
				     NULL);
	
	matrix[3] = XtVaCreateManagedWidget("inter_label",
					    labelWidgetClass, rc,
					    XtNtop, XawChainTop,
					    XtNbottom, XawChainTop,
					    XtNleft, XawChainLeft,
					    XtNright, XawChainLeft,
					    NULL);
	sprintf(buf, "%d", sc->times[1]);
	matrix[4] = XtVaCreateManagedWidget("inter_time",
					    labelWidgetClass, rc,
					    XtNlabel, buf,
					    NULL);

	matrix[5] = XtVaCreateManagedWidget("inter_name",
					    labelWidgetClass, rc,
					    XtNtop, XawChainTop,
					    XtNbottom, XawChainTop,
					    XtNleft, XawChainLeft,
					    XtNright, XawChainLeft,
					    XtNlabel, sc->names[1],
					    NULL);

	rc = XtVaCreateManagedWidget("expert_score", boxWidgetClass, rc2,
				     NULL);
	
	matrix[6] = XtVaCreateManagedWidget("expert_label",
					    labelWidgetClass, rc,
					    XtNtop, XawChainTop,
					    XtNbottom, XawChainTop,
					    XtNleft, XawChainLeft,
					    XtNright, XawChainLeft,
					    NULL);
	sprintf(buf, "%d", sc->times[2]);
	matrix[7] = XtVaCreateManagedWidget("expert_time",
					    labelWidgetClass, rc,
					    XtNlabel, buf,
					    NULL);

	matrix[8] = XtVaCreateManagedWidget("expert_name",
					    labelWidgetClass, rc,
					    XtNtop, XawChainTop,
					    XtNbottom, XawChainTop,
					    XtNleft, XawChainLeft,
					    XtNright, XawChainLeft,
					    XtNlabel, sc->names[2],
					    NULL);

	rc2 = XtVaCreateManagedWidget("score_buttons",
				      boxWidgetClass, pane,
				      NULL);
	ok_w = XtVaCreateManagedWidget("ok",
				       commandWidgetClass, rc2,
				       NULL);
	XtAddCallback(ok_w, XtNcallback, dialog_ok, NULL);
	w = XtVaCreateManagedWidget("clear",
				    commandWidgetClass, rc2,
				    NULL);
	XtAddCallback(w, XtNcallback, clear_scores, NULL);

	if (cant_write_score_file()) XtSetSensitive(w, False);
	XtManageChild(pane);
	XtPopup(best, XtGrabExclusive);
	dialog_up = True;
	while (dialog_up) {
		XtAppProcessEvent(app, XtIMAll);
		XSync(disp, 0);
	}
	XtPopdown(best);

}

/* ARGSUSED */
#if NeedFunctionPrototypes
void
about(Widget dummy, XtPointer closure, XtPointer call_data)
#else
void
about(dummy, closure, call_data)
	Widget dummy;
	XtPointer closure, call_data;
#endif
{
	Widget about, pane, w, rc2;
	char buf[100];

	about = XtVaCreatePopupShell("about",
				     topLevelShellWidgetClass, toplevel,
				     NULL);
	pane = XtVaCreateWidget("about_pane", panedWidgetClass, about,
				NULL);
	sprintf(buf,
		"xmine 1.0.%d by Paul Falstad\n\
Xaw version by J%crg Wunsch", PATCHLEVEL, 0xf6 /* o-umlaut :-) */);
	XtVaCreateManagedWidget("label",
				labelWidgetClass, pane,
				XtNlabel, buf,
				NULL);
	rc2 = XtVaCreateManagedWidget("about_commands",
				      boxWidgetClass, pane,
				      NULL);
	w = XtVaCreateManagedWidget("ok",
				    commandWidgetClass, rc2,
				    NULL);
	XtAddCallback(w, XtNcallback, dialog_ok, NULL);
	XtManageChild(pane);
	XtPopup(about, XtGrabExclusive);
	dialog_up = True;
	while (dialog_up) {
		XtAppProcessEvent(app, XtIMAll);
		XSync(disp, 0);
	}
	XtPopdown(about);
}

#if NeedFunctionPrototypes
void
get_score_file_name(char *buf)
#else
void
get_score_file_name(buf)
	char *buf;
#endif
{
	if (*SCORE_FILE == '~' && getenv("HOME"))
		sprintf(buf, "%s%s", getenv("HOME"), SCORE_FILE+1);
	else
		strcpy(buf, SCORE_FILE);
}

#if NeedFunctionPrototypes
struct scores *
get_scores(void)
#else
struct scores *
get_scores()
#endif
{
	static struct scores sc;
	char buf[1000];
	int i, score;
	FILE *in;

	for (i = 0; i != 3; i++) {
		strcpy(sc.names[i], "Anonymous");
		sc.times[i] = 999;
	}
	get_score_file_name(buf);
	if ((in = fopen(buf, "r"))) {
		while (fscanf(in, "%d %d \"%[^\"]\"", &i, &score, buf) == 3) {
			if (i < 0 || i > 2) break;
			strcpy(sc.names[i], buf);
			sc.times[i] = score;
		}
		fclose(in);
	}
	return &sc;
}

#if NeedFunctionPrototypes
void
write_scores(struct scores *sc)
#else
void
write_scores(sc)
	struct scores *sc;
#endif
{
	int i;
	char buf[1000];
	FILE *out;

	get_score_file_name(buf);
	if (!(out = fopen(buf, "w")))
		return;
	for (i = 0; i != 3; i++)
		if (sc->times[i] != 999)
			fprintf(out, "%d %d \"%s\"\n", i,
				sc->times[i], sc->names[i]);
	fclose(out);
}

#if NeedFunctionPrototypes
void
new_best(XtPointer closure, XtIntervalId *id)
#else
void
new_best(closure, id)
	XtPointer closure;
	XtIntervalId *id;
#endif
{
	Widget custom, pane;
	char *str;
	struct scores *sc;
	int level = (int) closure;

	if (level < 0 || level >= 3) return;
	sc = get_scores();
	if (timer >= sc->times[level]) return;
	custom = XtVaCreatePopupShell("high_scorer",
				      topLevelShellWidgetClass, toplevel,
				      NULL);

	pane = XtVaCreateWidget("high_score_dialog", dialogWidgetClass, custom,
				XtNvalue, "", /* empty value */
				NULL);

	XawDialogAddButton(pane, "ok", dialog_ok, NULL);
	
	XtManageChild(pane);
	XtPopup(custom, XtGrabExclusive);

	dialog_up = True;
	while (dialog_up) {
		XtAppProcessEvent(app, XtIMAll);
		XSync(disp, 0);
	}
	XtPopdown(custom);
	str = XawDialogGetValueString(pane);
	if (str && *str) {
		strcpy(sc->names[level], str);
		sc->times[level] = timer;
		write_scores(sc);
	}
	if (str) XawAsciiSourceFreeString(pane);
	best_times(NULL, NULL, NULL);
}

#if NeedFunctionPrototypes
void
clear_scores(Widget dummy, XtPointer closure, XtPointer call_data)
#else
void
clear_scores(dummy, closure, call_data)
	Widget dummy;
	XtPointer closure, call_data;
#endif
{
	char buf[1000];
	FILE *out;

	dialog_up = False;
	get_score_file_name(buf);
	if ((out = fopen(buf, "w"))) fclose(out);
}

#if NeedFunctionPrototypes
int
cant_write_score_file(void)
#else
int
cant_write_score_file()
#endif
{
	return geteuid() != getuid();
}

#if NeedFunctionPrototypes
void
fix_size(void)
#else
void
fix_size()
#endif
{
	XSizeHints hints;
	Dimension wid, hgt;

	XtVaGetValues(toplevel,
		      XtNwidth, &wid,
		      XtNheight, &hgt,
		      NULL);
	hints.flags = PMaxSize|PMinSize;
	hints.max_width = hints.min_width = wid;
	hints.max_height = hints.min_height = hgt;
	XSetWMNormalHints(disp, XtWindow(toplevel), &hints);
}

#if NeedFunctionPrototypes
void
relax_size(void)
#else
void
relax_size()
#endif
{
	XSizeHints hints;
	Dimension wid, hgt, menu_hgt;

	XtVaGetValues(toplevel,
		      XtNwidth, &wid,
		      XtNheight, &hgt,
		      NULL);
	hints.flags = PMaxSize|PMinSize;
#ifdef NOT_NOW
	hints.flags = PMaxSize|PMinSize|PResizeInc;
#endif /* NOT_NOW */
	XtVaGetValues(menubar,
		      XtNheight, &menu_hgt,
		      NULL);
	hints.min_width = SIDEMARGIN*2+GSPACEX*INITIAL_SIZE_X;
	hints.min_height = TOPMARGIN+BOTMARGIN+menu_hgt+GSPACEY*INITIAL_SIZE_Y;
	hints.max_width = hints.max_height = 10000;
#ifdef NOT_NOW
	/*
	 * this doesn't seem to work under mwm; it forces the window
	 * width/height to be an exact multiple of GSPACEX/GSPACEY,
	 * which is not what we want.
	 */
	hints.width_inc = GSPACEX;
	hints.height_inc = GSPACEY;
#endif /* NOT_NOW */
	XSetWMNormalHints(disp, XtWindow(toplevel), &hints);
}

#if NeedFunctionPrototypes
void
resize_handler(Widget w, XtPointer data, XEvent *event, Boolean *contflag)
#else
void
resize_handler(w, data, event, contflag)
	Widget w;
	XtPointer data;
	XEvent *event;
	Boolean *contflag;
#endif
{
	int new_gsizex, new_gsizey;
	static int started = 0;
	Dimension menu_hgt;

	if (!started) {
		relax_size();
		started = 1;
	}
	if (event->type != ConfigureNotify) return;
	if (layed_out && game_on) return;
	XtVaGetValues(menubar,
		      XtNheight, &menu_hgt,
		      NULL);
	new_gsizex = (event->xconfigure.width-SIDEMARGIN*2)/GSPACEX;
	new_gsizey = (event->xconfigure.height-
		      (menu_hgt+TOPMARGIN+BOTMARGIN))/GSPACEY;
	if (new_gsizex > MAX_GSIZEX)
		new_gsizex = MAX_GSIZEX;
	if (new_gsizey > MAX_GSIZEY)
		new_gsizey = MAX_GSIZEY;
	if (new_gsizex != gsizex || new_gsizey != gsizey) {
		gsizex = new_gsizex;
		gsizey = new_gsizey;
		set_level(w, (XtPointer) 4, NULL);
	}
}

#if NeedFunctionPrototypes
void
exit_game(Widget dummy, XtPointer closure, XtPointer call_data)
#else
void
exit_game(dummy, closure, call_data)
	Widget dummy;
	XtPointer closure, call_data;
#endif
{
	exit(0);
}
