/* ui_layout.c - division of the window into regions, and window (re)drawing */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_ui_layout_c_sccsid[] = "@(#)ui_layout.c	1.25 29/6/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <sys/types.h>
#include <stdlib.h>

#include <local/wn.h>
#include <local/menu3.h>

#include <local/ukcprog.h>
#include <local/obj/obj.h>
#include <local/obj/mhole.h>
#include <local/obj/fed.h>
#include <local/obj/newtb.h>

#include "cursors.h"
#include "reg.h"
#include "ui.h"
#include "tdr.h"
#include "textwin.h"
#include "ups.h"
#include "symtab.h"
#include "src.h"
#include "output.h"
#include "ui_priv.h"
#include "state.h"
#include "menudata.h"
#include "debug.h"
#include "sccsdata.h"

typedef struct {
	int ma_md;
	const char *ma_menu_name;
	bool ma_md_changed;
	void (*ma_menu_func)PROTO((char *data, int md, int rv));
	char *ma_data;
} menu_arg_t;

#define FIXED	0.0
#define REST	1.0

static void dragbox_input PROTO((region_id_t region_id, event_t *ev));
static void dragbox_draw PROTO((region_id_t region_id, int wn,
				    int width, int height,
				    re_draw_reason_t draw_reason));

static void line_input PROTO((region_id_t region_id, event_t *ev));
static void line_draw PROTO((region_id_t region_id, int wn,
				    int width, int height,
				    re_draw_reason_t draw_reason));

static void tbar_input PROTO((region_id_t region_id, event_t *ev));
static void tbar_draw PROTO((region_id_t region_id, int wn,
			     int width, int height,
			     re_draw_reason_t draw_reason));

static void outwin_draw PROTO((region_id_t region_id, int wn,
			       int width, int height,
			       re_draw_reason_t draw_reason));

static void srcwin_input PROTO((region_id_t region_id, event_t *ev));
static void srcwin_draw PROTO((region_id_t region_id, int wn,
			       int width, int height,
			       re_draw_reason_t draw_reason));

static void typing_line_input PROTO((region_id_t region_id, event_t *ev));
static void typing_line_draw PROTO((region_id_t region_id, int wn,
				    int width, int height,
				    re_draw_reason_t draw_reason));

static void display_area_input PROTO((region_id_t region_id, event_t *ev));
static void display_area_draw PROTO((region_id_t region_id, int wn,
			       int width, int height,
			       re_draw_reason_t draw_reason));

static void menu_input PROTO((region_id_t region_id, event_t *ev));
static void menu_draw PROTO((region_id_t region_id, int wn,
			     int width, int height,
			     re_draw_reason_t draw_reason));


static void message_area_draw PROTO((region_id_t region_id, int wn,
				     int width, int height,
				     re_draw_reason_t draw_reason));

static void mousehole_draw PROTO((region_id_t region_id, int wn,
				  int width, int height,
				  re_draw_reason_t draw_reason));

void initialise_menu PROTO((region_id_t region_id, int md,
			    const char *menu_name,
			    void (*menu_func)(char *data, int mfmd, int rv),
			    char *menu_func_data));
static void add_srcwin PROTO((region_id_t region, double srcwin_fraction));
static tbar_id_t make_tbar PROTO((region_id_t region_id));

static int tbar_srcwin_callback PROTO((char *data, tb_action_t action, int val));
static int tbar_outwin_callback PROTO((char *data, tb_action_t action, int val));
static int tbar_obj_callback PROTO((char *data, tb_action_t action, int val));

static void srcwin_tbar_callback PROTO((char *data, tw_change_t change,
						int lnum_val, int pixel_val));
static void display_area_tbar_callback PROTO((char *data,
					      obj_change_t change, int val));
static void initialise_typing_line PROTO((region_id_t region_id));
static int get_default PROTO((const char *name, int default_value, int min_value));
static void output_window_menu_func PROTO((char *data, int md, int rv));
static void init_line_region PROTO((region_id_t line));

static int
get_default(name, default_value, min_value)
const char *name;
int default_value, min_value;
{
	const char *valstr;
	int value;

	/*  Some calls to us calculate default_value as 100 - percentage, so
	 *  set minumum constrain the default to ve above the minimum.
	 */
	if ((valstr = wn_get_default(name)) == NULL)
		return (default_value > min_value) ? default_value : min_value;

	if ((value = atoi(valstr)) >= min_value)
		return value;

	errf("Value %d for %s too small - using min value %d",
							value, name, min_value);
	return min_value;
}

/*  Do the initial division of the window into regions.
 */
region_id_t
divide_window_into_regions(wn)
int wn;
{
	enum {
		TOP, DISP_AREA_AND_TB, TOPLEFT,
		TYPING_LINE_AND_PMENU, TARGET_MENU_AND_DRAGBOX,
		TYPING_LINE, DMENU, MESSAGE_AREA, DRAGBOX,
		PMENU, MOUSEHOLE, DISPLAY_AREA_TB, DISPLAY_AREA,
		TARGET_MENU,
		START_TARGET_MENU, NEXT_TARGET_MENU, STEP_TARGET_MENU,
		CONT_TARGET_MENU, STOP_TARGET_MENU, EXIT_TARGET_MENU,
		OUTPUT, MAX_REGNUM
	};
	region_id_t root, line, dragbox_line, regs[(int)MAX_REGNUM];
	target_menu_info_t tmbuf;
	font_t *sysfont, *menufont;

	/*  Dimensions and percentages */
	int mousehole_width, menu_height;
	int pmenu_width, tbar_width, srcwin_name_width;
	int display_area_percent, srcwin_percent;
	float display_area_fraction, srcwin_fraction;

	int i;
	tbar_id_t obj_tbar_id;
	extern MENU cmd_men;
	extern MENU tgt_start_men, tgt_next_men, tgt_step_men;
	extern MENU tgt_cont_men, tgt_stop_men, tgt_exit_men;
	static MENU *tgtmenus[(int)TM_NTAGS] = {
		&tgt_start_men, &tgt_next_men, &tgt_step_men,
		&tgt_cont_men, &tgt_stop_men, &tgt_exit_men
	};

	/*  Set up fixed dimensions.  Some of these vary with font heights.
	 */
	sysfont = wn_get_sysfont();
	menufont = Mstdfont();
	menu_height = menufont->ft_height + 8;

	/*  BUG: get rid of these explicit pixel sizes.
	 */
	mousehole_width = get_default("MouseholeWidth", 160, 5);
	pmenu_width = get_default("PermanentMenuWidth", 90, 5);
	tbar_width = get_default("ScrollbarWidth", 20, 5);
	srcwin_name_width = get_default("SrcwinNameWidth", 150, 20);

	if (wn_get_default("SrcwinPercent") != NULL) {
		srcwin_percent = get_default("SrcwinPercent", 50, 10);
		display_area_percent = get_default("DisplayAreaPercent",
							  100 - srcwin_percent, 1);
	}
	else {
		display_area_percent = get_default("DisplayAreaPercent", 50, 1);
		srcwin_percent = get_default("SrcwinPercent", 
						100 - display_area_percent, 1);
	}

	display_area_fraction = (float)display_area_percent / 100;
	srcwin_fraction = (float)srcwin_percent / 100;

	for (i = 0; i < (int)MAX_REGNUM; ++i)
		regs[i] = NULL;
	line = NULL;

	root = re_make_region(wn);

	re_divide(root, RE_HORIZONTAL,
		  &regs[(int)TOP],	        0,		0.0,
		  &line,			1,		FIXED,
		  RE_END);
	init_line_region(line);
	re_divide(root, RE_HORIZONTAL,
		  &regs[(int)DISP_AREA_AND_TB],	0,	       display_area_fraction,
		  &line,			1,		FIXED,
		  &regs[(int)TARGET_MENU_AND_DRAGBOX],menu_height,FIXED,
		  RE_END);
	dragbox_line = line;
	add_srcwin(root, srcwin_fraction);

	re_divide(regs[(int)TOP], RE_VERTICAL,
		  &regs[(int)TOPLEFT],		0,			1.0,
		  &line,			1,			FIXED,
		  &regs[(int)MOUSEHOLE],	mousehole_width,	FIXED,
		  RE_END);
	
	re_divide(regs[(int)TOPLEFT], RE_HORIZONTAL,
		  &regs[(int)TYPING_LINE_AND_PMENU],	menu_height,	FIXED,
		  &line,				1,		FIXED,
		  &regs[(int)DMENU],			menu_height,	FIXED,
		  &line,				1,		FIXED,
		  &regs[(int)MESSAGE_AREA],		menu_height,	FIXED,
		  RE_END);
	
	re_divide(regs[(int)TYPING_LINE_AND_PMENU], RE_VERTICAL,
		  &regs[(int)TYPING_LINE],		0,		REST,
		  &line,				1,		FIXED,
		  &regs[(int)PMENU],			pmenu_width,	FIXED,
		  RE_END);
	
	re_divide(regs[(int)TARGET_MENU_AND_DRAGBOX], RE_VERTICAL,
		  &regs[(int)TARGET_MENU],		0,		REST,
		  &line,				1,		FIXED,
		  &regs[(int)DRAGBOX],			menu_height,	FIXED,
		  RE_END);
	
	re_divide(regs[(int)TARGET_MENU], RE_VERTICAL,
		  &regs[(int)START_TARGET_MENU],	0,		1.0,
		  &line,				1,		FIXED,
		  &regs[(int)NEXT_TARGET_MENU],		0,		1.0,
		  &line,				1,		FIXED,
		  &regs[(int)STEP_TARGET_MENU],		0,		1.0,
		  &line,				1,		FIXED,
		  &regs[(int)CONT_TARGET_MENU],		0,		1.0,
		  &line,				1,		FIXED,
		  &regs[(int)STOP_TARGET_MENU],		0,		1.0,
		  &line,				1,		FIXED,
		  &regs[(int)EXIT_TARGET_MENU],		0,		1.0,
		  RE_END);
	
	re_divide(regs[(int)DISP_AREA_AND_TB], RE_VERTICAL,
		  &regs[(int)DISPLAY_AREA_TB],		tbar_width,	FIXED,
		  &line,				1,		FIXED,
		  &regs[(int)DISPLAY_AREA],		0,		1.0,
		  RE_END);
	
	initialise_menu(regs[(int)PMENU], Minsert(&cmd_men), "pmenu",
					permanent_menu_func, (char *)NULL);

	initialise_menu(regs[(int)DMENU], -1, NULL,
					dynamic_menu_func, (char *)NULL);
	set_dynamic_menu_region(regs[(int)DMENU]);

	re_set_callbacks(regs[(int)MESSAGE_AREA], message_area_draw,
				(re_input_proc_t)NULL, (re_destroy_proc_t)NULL);
	set_message_wn(re_get_wn(regs[(int)MESSAGE_AREA]));

	re_set_callbacks(regs[(int)MOUSEHOLE], mousehole_draw,
				(re_input_proc_t)NULL, (re_destroy_proc_t)NULL);

	re_set_callbacks(regs[(int)DRAGBOX], dragbox_draw,
					dragbox_input, (re_destroy_proc_t)NULL);
	re_set_margins(regs[(int)DRAGBOX], 1, 1, 1, 1);
	re_set_data(regs[(int)DRAGBOX], (char *)dragbox_line);
	re_set_mhcaps(regs[(int)DRAGBOX], "drag", "up", "down");
	re_set_cursor(regs[(int)DRAGBOX], CU_DRAGBOX);

	obj_tbar_id = make_tbar(regs[(int)DISPLAY_AREA_TB]);
	tb_set_callback_and_data(obj_tbar_id, tbar_obj_callback, (char *)NULL);
	obj_set_callback_and_data(display_area_tbar_callback, (char *)obj_tbar_id);

	re_set_callbacks(regs[(int)DISPLAY_AREA], display_area_draw,
				display_area_input, (re_destroy_proc_t)NULL);
	set_display_area_wn(re_get_wn(regs[(int)DISPLAY_AREA]));
	re_set_cursor(regs[(int)DISPLAY_AREA], CU_DISPLAY);
	re_set_mhcaps(regs[(int)DISPLAY_AREA], "select", "edit", "toggle");
	
	for (i = 0; i < (int)TM_NTAGS; ++i) {
		tmbuf.tm_mdtab[i].md = Minsert(tgtmenus[i]);
		tmbuf.tm_mdtab[i].wn = re_get_wn(regs[(int)START_TARGET_MENU + i]);
		initialise_menu(regs[(int)START_TARGET_MENU + i],
				tmbuf.tm_mdtab[i].md, "tmenu", target_menu_func,
				(char *)NULL);
	}
	tmbuf.tm_current_md = -1;
	set_target_menu_info(&tmbuf);

	re_set_minsize(regs[(int)DISPLAY_AREA], 1, 2 * sysfont->ft_height + 3);

	initialise_typing_line(regs[(int)TYPING_LINE]);

	re_set_rootwin(root);
	re_redraw_root(EV_WINDOW_RESIZED, TRUE);

	return root;
}

static void
line_input(region_id, ev)
region_id_t region_id;
event_t *ev;
{
	int buttons, width, height;
	bool changed_position, is_vertical_shift;

	buttons = ev->ev_buttons;

	wn_get_window_size(re_get_wn(region_id), &width, &height);
	is_vertical_shift = width > height;
	changed_position = FALSE;

	if (buttons & B_LEFT) {
		int wn, x, y, last_coord;

		/*  As we are shifting windows around, get a stable window
		 *  to read events from, and translate the current event
		 *  coordinates to that window.
		 */
		wn = re_get_wn(re_get_parent(region_id));
		wn_trans_coords(ev->ev_wn, ev->ev_x, ev->ev_y, wn, &x, &y);

		last_coord = is_vertical_shift ? y : x;
		do {
			int coord;

			wn_next_event(wn, EV_MOUSE_MOVED | EV_BUTTON_UP, ev);
			coord = is_vertical_shift ? ev->ev_y : ev->ev_x;
			if (coord != last_coord) {
				re_change_position(region_id, coord - last_coord);
				last_coord = coord;
				changed_position = TRUE;
			}
		} while (ev->ev_type != EV_BUTTON_UP);
	}
	else if (buttons & (B_MIDDLE | B_RIGHT)) {
		int delta;

		delta = (buttons & B_MIDDLE) ? -1 : 1;
		re_change_position(region_id, delta);
		wn_wait_for_release_of(ev->ev_wn, B_MIDDLE | B_RIGHT);

		/*  The shift we've just done has moved the window, and we
		 *  want the cursor in the same window relative position so
		 *  that it remains over the line, so we warp it.
		 */
		wn_warp_mouse(ev->ev_wn, ev->ev_x, ev->ev_y);

		changed_position = TRUE;
	}

	if (changed_position)
		re_expose(re_get_parent(region_id), TRUE);
}

static void
dragbox_input(region_id, ev)
region_id_t region_id;
event_t *ev;
{
	line_input((region_id_t)re_get_data(region_id), ev);
}

static void
dragbox_draw(region_id, wn, win_width, win_height, draw_reason)
region_id_t region_id;
int wn;
int win_width, win_height;
re_draw_reason_t draw_reason;
{
	/*  The arrow_* #defines and data were generated by the X10 bitmap program.
	 *  They define the double headed arrow used in the source window drag box.
	 */
#define arrow_bitmap_width 23
#define arrow_bitmap_height 23
	static short arrow_bitmap_bits[] = {
	   0x0000, 0x0000, 0x0000, 0x0000,
	   0x0000, 0x0000, 0x0800, 0x0000,
	   0x1c00, 0x0000, 0x3e00, 0x0000,
	   0x7f00, 0x0000, 0xff80, 0x0000,
	   0xffc0, 0x0001, 0xffe0, 0x0003,
	   0x0000, 0x0000, 0x0000, 0x0000,
	   0x0000, 0x0000, 0xffe0, 0x0003,
	   0xffc0, 0x0001, 0xff80, 0x0000,
	   0x7f00, 0x0000, 0x3e00, 0x0000,
	   0x1c00, 0x0000, 0x0800, 0x0000,
	   0x0000, 0x0000, 0x0000, 0x0000,
	   0x0000, 0x0000
	};

	static bitmap_t arrow_bm = wn_static_bm(arrow_bitmap_width,
						arrow_bitmap_height, 1,
						0, 0, BM_BIT0_LEFT,
						(unsigned short *)arrow_bitmap_bits);
	int bm_x, bm_y, win_x, win_y;
	int width, height;

	if (draw_reason != RE_EXPOSED && draw_reason != RE_RESIZED)
		return;

	if (arrow_bm.bm_width < win_width) {
		bm_x = 0;
		width = arrow_bm.bm_width;
		win_x = (win_width - arrow_bm.bm_width) / 2;
	}
	else {
		bm_x = (arrow_bm.bm_width - win_width) / 2;
		width = win_width;
		win_x = 0;
	}

	if (arrow_bm.bm_height < win_height) {
		bm_y = 0;
		height = arrow_bm.bm_height;
		win_y = (win_height - arrow_bm.bm_height) / 2;
	}
	else {
		bm_y = (arrow_bm.bm_height - win_height) / 2;
		height = win_height;
		win_y = 0;
	}

	wn_rop_from_mem(&arrow_bm, bm_x, bm_y, width, height,
			wn, win_x, win_y, R_RPL);
}

static void
line_draw(region_id, wn, width, height, draw_reason)
region_id_t region_id;
int wn;
int width, height;
re_draw_reason_t draw_reason;
{
	switch (draw_reason) {
	case RE_EXPOSED:
	case RE_RESIZED:
		wn_set_area(wn, 0, 0, width, height, WN_FG);
		break;
	case RE_RESIZING:
	case RE_UNDRAW:
		wn_invert_area(wn, 0, 0, width, height);
		break;
	default:
		panic("bad dr in ld");
	}
}

static void
initialise_typing_line(region_id)
region_id_t region_id;
{
	int font_height;
	edesc_t *edesc;

	font_height = wn_get_sysfont()->ft_height;

	re_set_callbacks(region_id, typing_line_draw,
					typing_line_input, (re_destroy_proc_t)NULL);
	re_set_margins(region_id, 5, 5, 4, 4);
	re_set_cursor(region_id, CU_DISPLAY);
	re_set_mhcaps(region_id, "", "edit", "");
	re_set_keyboard_focus(region_id);

	edesc = (edesc_t *)e_malloc(sizeof(edesc_t));
	re_set_data(region_id, (char *)edesc);
	set_typing_line_edesc(edesc);
}

static void
message_area_draw(region_id, wn, width, height, draw_reason)
region_id_t region_id;
int wn;
int width, height;
re_draw_reason_t draw_reason;
{
	if (draw_reason == RE_EXPOSED || draw_reason == RE_RESIZED)
		wn_set_area(wn, 0, 0, width, height, WN_BG);
}

/*  Set up the typing area
 */
static void
typing_line_draw(region_id, wn, width, height, draw_reason)
region_id_t region_id;
int wn;
int width, height;
re_draw_reason_t draw_reason;
{
	if (draw_reason == RE_EXPOSED || draw_reason == RE_RESIZED)
		ta_init((edesc_t *)re_get_data(region_id), wn, wn_get_sysfont());
}

static void
typing_line_input(region_id, ev)
region_id_t region_id;
event_t *ev;
{
	edesc_t *edesc;

	edesc = (edesc_t *)re_get_data(region_id);

	if (ev->ev_type == EV_KEY) {
		if (ev->ev_char == CONTROL('V'))
			errf("%s", _ups_sccsdata[0]);
		else {
			edesc->ed_act = EDA_KEY;
			edesc->ed_char = ev->ev_char;
			edit_field(edesc);
		}
	}
	else if (ev->ev_type == EV_BUTTON_DOWN) {
		edesc->ed_act = EDA_PUCK;
		edesc->ed_puck_x = ev->ev_x;
		edesc->ed_puck_y = ev->ev_y;
		edesc->ed_buttons = ev->ev_buttons;
		edit_field(edesc);
	}
}

static void
init_line_region(line)
region_id_t line;
{
	if ((Debug_flags & DBFLAG_MVLINES) == 0) {
		re_set_callbacks(line, line_draw, (re_input_proc_t)NULL,
							(re_destroy_proc_t)NULL);
	}
	else {
		re_set_callbacks(line, line_draw, line_input,
							(re_destroy_proc_t)NULL);
		re_set_mhcaps(line, "drag", "up", "down");
		re_set_cursor(line, CU_LINE);
	}
}

void
add_outwin()
{
	extern MENU out_men;
	static int md = -1;
	region_id_t srcwin_region, root, line, dragbox_line;
	region_id_t text_and_tb_region;
	region_id_t outwin_region, tbar_region;
	region_id_t menu_and_dragbox_region, dragbox_region, menu_region;
	double outwin_fraction;
	int header_height, outwin_percent, tbar_width;
	outwin_id_t outwin_id;
	tbar_id_t tbar_id;

	srcwin_region = get_current_srcwin_region();

	root = srcwin_region;
	for(;;) {
		region_id_t parent;

		parent = re_get_parent(root);
		if (parent == NULL)
			break;
		root = parent;
	}

	outwin_percent = get_default("OutwinPercent", 50, 1);
	header_height = wn_get_sysfont()->ft_height + 6;
	tbar_width = get_default("ScrollbarWidth", 20, 5);

	outwin_fraction = (double)outwin_percent / 100;

	line = NULL;
	re_divide(root, RE_HORIZONTAL,
		  &line,			1,		FIXED,
		  RE_END);
	dragbox_line = line;
	init_line_region(line);

	menu_and_dragbox_region = text_and_tb_region = NULL;
	re_divide(root, RE_HORIZONTAL,
		  &menu_and_dragbox_region,	header_height,	FIXED,
		  &line,			1,		FIXED,
		  &text_and_tb_region,		0,		outwin_fraction,
		  RE_END);

	tbar_region = outwin_region = NULL;
	re_divide(text_and_tb_region, RE_VERTICAL,
		  &tbar_region,			tbar_width,	FIXED,
		  &line,			1,		FIXED,
		  &outwin_region,		0,		REST,
		  RE_END);
	
	menu_region = dragbox_region = NULL;
	re_divide(menu_and_dragbox_region, RE_VERTICAL,
		  &menu_region,			0,		REST,
		  &line,			1,		FIXED,
		  &dragbox_region,		header_height,	FIXED,
		  RE_END);

	outwin_id = ow_make_outwin(re_get_wn(outwin_region), WN_FG, WN_BG,
								wn_get_sysfont());
	re_set_data(outwin_region, (char *)outwin_id);
	re_set_callbacks(outwin_region, outwin_draw,
				(re_input_proc_t)NULL, (re_destroy_proc_t)NULL);

	re_set_callbacks(dragbox_region, dragbox_draw,
					dragbox_input, (re_destroy_proc_t)NULL);
	re_set_margins(dragbox_region, 1, 1, 1, 1);
	re_set_data(dragbox_region, (char *)dragbox_line);
	re_set_mhcaps(dragbox_region, "drag", "up", "down");
	re_set_cursor(dragbox_region, CU_DRAGBOX);

	tbar_id = make_tbar(tbar_region);
	tb_set_callback_and_data(tbar_id, tbar_outwin_callback, (char *)outwin_id);
	ow_set_textwin_callback_and_data(outwin_id, srcwin_tbar_callback,
								(char *)tbar_id);

	if (md == -1)
		md = Minsert(&out_men);
	else
		md = Mdup(md);

	initialise_menu(menu_region, md, "outwin",
			output_window_menu_func, (char *)outwin_id);

	re_redraw_root(EV_WINDOW_RESIZED, TRUE);

	set_current_outwin(outwin_id);
}
	
static void
add_srcwin(region, srcwin_fraction)
region_id_t region;
double srcwin_fraction;
{
	static int md = -1;
	extern MENU src_men;
	enum {
		LINE, HEADER, TEXT_AND_TB,
		TEXT, TB, NAMEWIN, SRC_MENU,
		NREGS
	};
	tbar_id_t tbar_id;
	srcwin_id_t srcwin_id;
	region_id_t regs[(int)NREGS];
	int i, tbar_width;

	tbar_width = get_default("ScrollbarWidth", 20, 5);

	for (i = 0; i < (int)NREGS; ++i)
		regs[i] = NULL;

	re_divide(region, RE_HORIZONTAL,
		  &regs[(int)LINE],		1,		FIXED,
		  RE_END);

	init_line_region(regs[(int)LINE]);

	re_divide(region, RE_HORIZONTAL,
		  &regs[(int)HEADER],		1,		0.0,
		  &regs[(int)LINE],		1,		FIXED,
		  &regs[(int)TEXT_AND_TB],	0,		srcwin_fraction,
		  RE_END);

	re_divide(regs[(int)HEADER], RE_VERTICAL,
		  &regs[(int)NAMEWIN],		1,		FIXED,
		  &regs[(int)LINE],		1,		FIXED,
		  &regs[(int)SRC_MENU],		0,		REST,
		  RE_END);

	re_divide(regs[(int)TEXT_AND_TB], RE_VERTICAL,
		  &regs[(int)TB],		tbar_width,	FIXED,
		  &regs[(int)LINE],		1,		FIXED,
		  &regs[(int)TEXT],		0,		REST,
		  RE_END);

	re_set_minsize(regs[(int)NAMEWIN], wn_get_sysfont()->ft_width * 30,
					   wn_get_sysfont()->ft_height + 6);
	re_set_minsize(regs[(int)SRC_MENU], 0, Mstdfont()->ft_height + 6);

	tbar_id = make_tbar(regs[(int)TB]);
	srcwin_id = src_make_srcwin(re_get_wn(regs[(int)TEXT]));

	/*  Tie all the bits together with callbacks.
	 */
	re_set_data(regs[(int)TEXT], (char *)srcwin_id);

	tb_set_callback_and_data(tbar_id, tbar_srcwin_callback, (char *)srcwin_id);
	src_set_textwin_callback_and_data(srcwin_id, srcwin_tbar_callback,
								(char *)tbar_id);
	src_set_data(srcwin_id, (char *)re_get_wn(regs[(int)NAMEWIN]));

	re_set_callbacks(regs[(int)TEXT], srcwin_draw, srcwin_input,
							(re_destroy_proc_t)NULL);
	re_set_cursor(regs[(int)TEXT], CU_DISPLAY);
	re_set_mhcaps(regs[(int)TEXT], "select", "edit", "menu");

	if (md == -1)
		md = Minsert(&src_men);
	else
		md = Mdup(md);

	initialise_menu(regs[(int)SRC_MENU], md, "srcwin",
			source_window_menu_func, (char *)srcwin_id);
	
	set_current_srcwin_region(regs[(int)TEXT]);
}

static int
tbar_obj_callback(unused_data, action, val)
char *unused_data;
tb_action_t action;
int val;
{
	int retval;

	switch (action) {
	case TB_GOTO:
		display_from(0, val);
		retval = val;
		break;
	case TB_SCROLL:
		retval = v_scroll(val);
		break;
	case TB_SHOW_POS:
		retval = 0;
		break;
	default:
		panic("bad action in tsc");
		retval = 0;	/* to satisfy gcc */
	}
	return retval;
}

static int
tbar_outwin_callback(data, action, val)
char *data;
tb_action_t action;
int val;
{
	outwin_id_t outwin_id;
	int retval;

	outwin_id = (outwin_id_t)data;

	switch (action) {
	case TB_GOTO:
		ow_goto_ypos(outwin_id, val);
		retval = val;
		break;
	case TB_SCROLL:
		retval = ow_scroll(outwin_id, val);
		break;
	case TB_SHOW_POS:
		retval = 0;
		break;
	default:
		panic("bad action in toc");
		retval = 0;	/* to satisfy gcc */
	}
	return retval;
}

static int
tbar_srcwin_callback(data, action, val)
char *data;
tb_action_t action;
int val;
{
	srcwin_id_t srcwin_id;
	int retval;
	const char *name;
	srcinfo_t sibuf;

	srcwin_id = (srcwin_id_t)data;

	switch (action) {
	case TB_GOTO:
		src_goto_ypos(srcwin_id, val);
		retval = val;
		break;
	case TB_SCROLL:
		retval = src_scroll(srcwin_id, val);
		break;
	case TB_SHOW_POS:
		src_get_info(srcwin_id, -1, val, &sibuf);
		name = (sibuf.si_fil != NULL) ? sibuf.si_fil->fi_name : NULL;
		echo_src_name_and_lnum(srcwin_id, name, sibuf.si_fil_lnum);
		retval = 0;
		break;
	default:
		panic("bad action in tsc");
		retval = 0;	/* to satisfy gcc */
	}
	return retval;
}

static void
display_area_tbar_callback(data, change, val)
char *data;
obj_change_t change;
int val;
{
	tbar_id_t tbar_id;

	tbar_id = (tbar_id_t)data;
	switch (change) {
	case OBJ_NEW_WINDOW_HEIGHT:
		tb_set_visible_units(tbar_id, val);
		break;
	case OBJ_NEW_HEIGHT:
		tb_set_total_units(tbar_id, val);
		break;
	case OBJ_NEW_YPOS:
		tb_goto(tbar_id, val, FALSE);
		break;
	default:
		panic("bad change in datc");
	}
}

static void
srcwin_tbar_callback(data, change, unused_lnum_val, pixel_val)
char *data;
tw_change_t change;
int unused_lnum_val, pixel_val;
{
	tbar_id_t tbar_id;

	tbar_id = (tbar_id_t)data;
	switch (change) {
	case TW_NEW_VISIBLE_UNITS:
		tb_set_visible_units(tbar_id, pixel_val);
		break;
	case TW_NEW_TOTAL_UNITS:
		tb_set_total_units(tbar_id, pixel_val);
		break;
	case TW_NEW_POS:
		tb_goto(tbar_id, pixel_val, FALSE);
		break;
	default:
		panic("bad change in stc");
	}
}

static tbar_id_t
make_tbar(region_id)
region_id_t region_id;
{
	int wn;
	tbar_id_t tbar_id;

	wn = re_get_wn(region_id);
	tbar_id = tb_create(wn);
	re_set_callbacks(region_id, tbar_draw, tbar_input, (re_destroy_proc_t)NULL);
	re_set_data(region_id, (char *)tbar_id);
	re_set_cursor(region_id, CU_SBAR);
	re_set_mhcaps(region_id, "scroll", "goto", "");
	return tbar_id;
}

/*  Input function for the display area.  Just push back the event
 *  and hand control over to the object library.
 *
 *  BUG: for the moment, we ignore the region_id, and assume that there
 *       is only one display area.
 */
static void
display_area_input(region_id, ev)
region_id_t region_id;
event_t *ev;
{
	int buttons;

	buttons = ev->ev_buttons & B_ANY;

	if (buttons != 0) {
		if (buttons != B_MIDDLE)
			set_dynamic_menu_updating_state(DMU_OFF);
		wn_pushback_event(ev);
	 	select_list();
		if (buttons != B_MIDDLE)
			set_dynamic_menu_updating_state(DMU_ON);
	}
}

static void
display_area_draw(region_id, wn, width, height, draw_reason)
region_id_t region_id;
int wn;
int width, height;
re_draw_reason_t draw_reason;
{
	if (draw_reason == RE_EXPOSED || draw_reason == RE_RESIZED)
		set_obj_wn(wn);
}

static void
output_window_menu_func(arg, md, rv)
char *arg;
int md, rv;
{
	outwin_id_t outwin_id;

	outwin_id = (outwin_id_t)arg;

	switch (rv) {
	case MR_OUTWIN_PAGE_UP:
		ow_page(outwin_id, OW_PAGE_UP);
		break;
	case MR_OUTWIN_PAGE_DOWN:
		ow_page(outwin_id, OW_PAGE_DOWN);
		break;
	case MR_OUTWIN_CLEAR:
		ow_clear(outwin_id);
		break;
	case MR_OUTWIN_SEARCH_FORWARDS:
	case MR_OUTWIN_SEARCH_BACKWARDS:
		ow_search(outwin_id, get_typing_line_string(),
						rv == MR_OUTWIN_SEARCH_FORWARDS);
		break;
	default:
		panic("bad rv in owmf");
	}
}

static void
outwin_draw(region_id, wn, width, height, draw_reason)
region_id_t region_id;
int wn;
int width, height;
re_draw_reason_t draw_reason;
{
	if (draw_reason == RE_EXPOSED || draw_reason == RE_RESIZED)
		ow_redraw((outwin_id_t)re_get_data(region_id));
}

static void
srcwin_draw(region_id, wn, width, height, draw_reason)
region_id_t region_id;
int wn;
int width, height;
re_draw_reason_t draw_reason;
{
	if (draw_reason == RE_EXPOSED || draw_reason == RE_RESIZED)
		src_redraw((srcwin_id_t)re_get_data(region_id));
}

static void
srcwin_input(region_id, ev)
region_id_t region_id;
event_t *ev;
{
	source_window_event_handler((srcwin_id_t)re_get_data(region_id), ev);
}

static void
tbar_draw(region_id, wn, width, height, draw_reason)
region_id_t region_id;
int wn;
int width, height;
re_draw_reason_t draw_reason;
{
	tbar_id_t tbar_id;

	if (draw_reason != RE_EXPOSED && draw_reason != RE_RESIZED)
		return;

	tbar_id = (tbar_id_t)re_get_data(region_id);
	if (draw_reason == RE_RESIZED)
		tb_set_box_pos_and_size(tbar_id, TRUE, 0, width / 3);
	tb_expose(tbar_id);
}

static void
tbar_input(region_id, ev)
region_id_t region_id;
event_t *ev;
{
	tbar_event_handler((tbar_id_t)re_get_data(region_id), ev);
}

void
initialise_menu(region_id, md, menu_name, menu_func, menu_func_data)
region_id_t region_id;
int md;
const char *menu_name;
void (*menu_func)PROTO((char *mf_data, int mf_md, int mf_rv));
char *menu_func_data;
{
	menu_arg_t *ma;

	ma = (menu_arg_t *)e_malloc(sizeof(menu_arg_t));

	if (md != -1) {
		ma->ma_md = md;
		Mnobox(ma->ma_md);
	}
	else
		ma->ma_md = -1;

	ma->ma_menu_name = menu_name;
	ma->ma_menu_func = menu_func;
	ma->ma_data = menu_func_data;
	ma->ma_md_changed = FALSE;

	re_set_data(region_id, (char *)ma);
	re_set_callbacks(region_id, menu_draw, menu_input, (re_destroy_proc_t)NULL);
	re_set_cursor(region_id, CU_MENU);
	re_set_mhcaps(region_id, "select", "", "");
}

static void
menu_input(region_id, ev)
region_id_t region_id;
event_t *ev;
{
	menu_arg_t *ma;
	int wn, mask, rv;

	ma = (menu_arg_t *)re_get_data(region_id);

	if (ma->ma_md == -1 || (ev->ev_buttons & B_LEFT) == 0)
		return;

	wn = ev->ev_wn;
	mask = ~(1 << ma->ma_md);
	if (!Mselect(ev->ev_x, ev->ev_y, wn, MS_PRESSED, mask))
		return;

	for (;;) {
		wn_next_event(wn, EVENT_MASK, ev);
		if ((ev->ev_buttons & B_LEFT) == 0)
			break;
		Mselect(ev->ev_x, ev->ev_y, wn, MS_CONTINUE, mask);
	}
	rv = MS_rv(Mselect(ev->ev_x, ev->ev_y, wn, MS_RELEASED, mask));

	if (rv != 0) {
		td_record_menu_command(ma->ma_menu_name, rv);
		(*ma->ma_menu_func)(ma->ma_data, ma->ma_md, rv);
	}

	Mclear(ma->ma_md);
}

/*  Show the target menu.
 */
static void
menu_draw(region_id, wn, width, height, draw_reason)
region_id_t region_id;
int wn;
int width, height;
re_draw_reason_t draw_reason;
{
	menu_arg_t *ma;

	if (draw_reason != RE_EXPOSED && draw_reason != RE_RESIZED)
		return;

	ma = (menu_arg_t *)re_get_data(region_id);

	if (ma->ma_md != -1) {
		if (draw_reason == RE_RESIZED || ma->ma_md_changed) {
			Mplace(ma->ma_md, 0, 0);
			Msize(ma->ma_md, width, height);
			ma->ma_md_changed = FALSE;
		}
		Mdisplay(ma->ma_md, wn, FALSE);
	}
	else
		wn_shade_area(wn, 0, 0, width, height, WN_GREY4, R_RPL);
}

/*  Set the dynamic menu to md.
 */
void
set_dynamic_menu(md, menu_name)
int md;
const char *menu_name;
{
	menu_arg_t *ma;
	region_id_t dmenu_region;

	dmenu_region = get_dynamic_menu_region();

	ma = (menu_arg_t *)re_get_data(dmenu_region);

	if (md != ma->ma_md && ma->ma_md != -1)
		Mremove(ma->ma_md);

	ma->ma_menu_name = menu_name;
	ma->ma_md = md;
	ma->ma_md_changed = TRUE;

	re_expose(dmenu_region, FALSE);
}

static void
mousehole_draw(region_id, wn, width, height, draw_reason)
region_id_t region_id;
int wn;
int width, height;
re_draw_reason_t draw_reason;
{
	if (draw_reason == RE_EXPOSED || draw_reason == RE_RESIZED) {
		wn_set_area(wn, 0, 0, width, height, WN_BG);
		draw_mousehole(wn, 0, 0, width, height);
	}
}
