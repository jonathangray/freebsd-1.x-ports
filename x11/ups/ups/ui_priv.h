/* ui_priv.h - private header file for the ui_*.c files */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)ui_priv.h	1.11 13/8/92 (UKC) */

/*  Event mask for wn_next_event()
 */
#define EVENT_MASK	(EV_BUTTON_UP | EV_BUTTON_DOWN | EV_MOUSE_MOVED | \
					EV_KEY | EV_INTERRUPT | \
					EV_WINDOW_EXPOSED | EV_WINDOW_RESIZED)

#define NO_SELECTION (-1)

/*  Definition of a popup menu for use with select_from_popup().
 */
typedef struct popupst {
	int po_mdesc;		/* Menu descriptor */
	int po_save_last;	/* If set, remember caption the user last selected */
	int po_last;		/* Last caption selected by user */
	const char **po_caps;	/* NULL terminated array of captions */
} popup_t;

/*  Source display window functions.
 */
void get_source_title_area PROTO((int wn, int *p_x, int *p_y,
				  int *p_width, int *p_height,
				  int *p_min_y, int *p_max_y));
int get_source_window_height PROTO((void));

#ifdef SRC_H_INCLUDED
void echo_src_name_and_lnum PROTO((srcwin_id_t srcwin_id,
						const char *name, int lnum));
#endif

/*  Miscellaneous functions.
 */
int mhc_setcaps PROTO((int val));
void show_menu PROTO((int md, int wn));

#ifdef FED_H_INCLUDED
void ta_init PROTO((edesc_t *edesc, int wn, font_t *font));
#endif

void scroll PROTO((event_t *ev, char *arg));
void display_area_scroll PROTO((int dist));
void display_area_goto PROTO((int y));
int select_from_popup PROTO((int wn, int button, popup_t *po, int x, int y));
void updating_callback_func PROTO((int oldstate, int newstate));
void set_dynamic_menu PROTO((int md, const char *menu_name));

/*  Input handling functions.
 */
void source_window_menu_func PROTO((char *data, int md, int rv));
#ifdef TB_H_INCLUDED
void tbar_event_handler PROTO((tbar_id_t tbar_id, event_t *ev));
#endif
#ifdef SRC_H_INCLUDED
void source_window_event_handler PROTO((srcwin_id_t srcwin_id, event_t *ev));
#endif

/*  Mousehole functions.
 */
int mfn_dmenu PROTO((int caps, char *arg));
int mfn_source PROTO((int caps, char *arg));
