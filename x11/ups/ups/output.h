/* output.h - public header file for output.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)output.h	1.3 3/5/91 (UKC) */

#define OUTPUT_H_INCLUDED

/*  Opaque handle on a source window.
 */
typedef struct { int dummy_member; } *outwin_id_t;

#ifdef WN_H_INCLUDED
outwin_id_t ow_make_outwin PROTO((int wn, int fg, int bg, font_t *font));
#endif

#ifdef TEXTWIN_H_INCLUDED
void ow_set_textwin_callback_and_data PROTO((outwin_id_t outwin_id,
					     tw_callback_t textwin_callback,
					     char *arg));
#endif

typedef enum { OW_PAGE_DOWN, OW_PAGE_UP } ow_page_direction_t;

void ow_redraw PROTO((outwin_id_t outwin_id));
int ow_scroll PROTO((outwin_id_t outwin_id, int npixels));
void ow_page PROTO((outwin_id_t outwin_id, ow_page_direction_t direction));
void ow_goto_ypos PROTO((outwin_id_t outwin_id, int ypos));

void ow_putc PROTO((outwin_id_t outwin_id, int c));
void ow_write PROTO((outwin_id_t outwin_id, const char *buf, int nbytes));
void ow_refresh PROTO((outwin_id_t outwin_id));
void ow_clear PROTO((outwin_id_t outwin_id));

void ow_search PROTO((outwin_id_t outwin_id, const char *pattern,
							bool forwards));
