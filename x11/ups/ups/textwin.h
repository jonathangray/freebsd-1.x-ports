/* textwin.h - public header file for textwin.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)textwin.h	1.6 5/3/91 (UKC) */

#define TEXTWIN_H_INCLUDED

typedef struct { int dummy_member; } *textwin_id_t;

typedef enum {
	TW_HL_NONE,
	TW_HL_BOX,
	TW_HL_INVERT,
	TW_HL_INVERT_ALL
} tw_hltype_t;

typedef struct {
	tw_hltype_t hl_type;
	int hl_cnum;
	int hl_nchars;
} tw_hlinfo_t;

typedef void (*tw_getline_proc_t)PROTO((char *arg, int lnum,
					const char **p_line,
					int *p_fg, int *p_bg,
					font_t **p_font, tw_hlinfo_t *hl));

typedef enum { TW_NEW_TOTAL_UNITS, TW_NEW_VISIBLE_UNITS, TW_NEW_POS } tw_change_t;

typedef void (*tw_callback_t)PROTO((char *data, tw_change_t change,
						int lnum_val, int pixel_val));

textwin_id_t tw_make_textwin PROTO((int wn, int bg, int line_height));
void tw_set_text PROTO((textwin_id_t textwin_id, int bg,
			tw_getline_proc_t getline, char *arg, int nlines,
			int lnum, bool centre));
void tw_handle_nlines_delta PROTO((textwin_id_t textwin_id,
				   int lnum, int old_nlines, int new_nlines,
				   int vis_lnum, bool centre));

void tw_redraw_text PROTO((textwin_id_t textwin_id, bool redraw_all_lines));
void tw_goto PROTO((textwin_id_t textwin_id, int lnum, bool centre));
int tw_scroll PROTO((textwin_id_t textwin_id, int npixels));

int tw_get_line_info PROTO((textwin_id_t textwin_id, int x, int y,
			       const char **p_line, font_t **p_font, int *p_lnum));
bool tw_is_visible PROTO((textwin_id_t textwin_id, int lnum));

int tw_ypos_to_lnum PROTO((textwin_id_t textwin_id, int ypos));
int tw_lnum_to_ypos PROTO((textwin_id_t textwin_id, int lnum));

int tw_get_line_height PROTO((textwin_id_t textwin_id));
int tw_get_window_height PROTO((textwin_id_t textwin_id));
int tw_get_window_width PROTO((textwin_id_t textwin_id));

long tw_draw_arrow PROTO((textwin_id_t textwin_id, int lnum));
void tw_draw_box PROTO((textwin_id_t textwin_id, int lnum, int col, int nchars));

void tw_set_callback_and_data PROTO((textwin_id_t textwin_id,
				     tw_callback_t callback, char *arg));

typedef enum {
	TW_SR_FOUND,
	TW_SR_NOT_FOUND,
	TW_SR_SEARCH_ERROR,
	TW_SR_INTERRUPTED
} tw_search_res_t;

tw_search_res_t tw_search PROTO((textwin_id_t textwin_id, const char *pattern,
				 int lnum, int cnum, bool forwards,
				 int *p_lnum, int *p_cnum, int *p_nchars));
