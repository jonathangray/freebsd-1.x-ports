/* reg.h - public header file for reg.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)reg.h	1.9 29/6/92 (UKC) */

#define REG_H_INCLUDED

/*  Opaque handle on a region.
 */
typedef struct { int dummy_member; } *region_id_t;

typedef enum { RE_VERTICAL, RE_HORIZONTAL } re_orientation_t;

typedef unsigned long re_nomsize_t;

#define RE_END		((region_id_t)NULL)

typedef enum {
	RE_EXPOSED,	/* region exposed, but no size change since last draw */
	RE_RESIZED,	/* region resized */
	RE_RESIZING,	/* in middle of resizing - only draw if cheap to do so */
	RE_UNDRAW	/* undo effects of last RE_RESIZING draw, if any */
} re_draw_reason_t;

#define RE_MAX_GROUPS	32

typedef void (*re_draw_proc_t)PROTO((region_id_t region_id, int wn,
				     int width, int height,
				     re_draw_reason_t draw_reason));
typedef void (*re_input_proc_t)PROTO((region_id_t region_id, event_t *ev));
typedef void (*re_destroy_proc_t)PROTO((region_id_t region_id));

region_id_t re_make_region PROTO((int wn));
void re_divide PROTO((region_id_t region_id, re_orientation_t orientation, ...));
int re_change_position PROTO((region_id_t region_id, int delta));

void re_set_rootwin PROTO((region_id_t region_id));
void re_set_keyboard_focus PROTO((region_id_t region_id));
void re_set_minsize PROTO((region_id_t region_id, int width, int height));
void re_set_data PROTO((region_id_t region_id, char *data));
void re_set_cursor PROTO((region_id_t region_id, int cursor));
void re_set_callbacks PROTO((region_id_t region_id,
			     re_draw_proc_t draw_proc,
			     re_input_proc_t input_proc,
			     re_destroy_proc_t destroy_proc));
void re_set_margins PROTO((region_id_t region_id, int left, int right,
						  int top, int bottom));
void re_set_mhcaps PROTO((region_id_t region_id,
			  const char *left_caption,
			  const char *middle_caption,
			  const char *right_caption));



char *re_get_data PROTO((region_id_t region_id));
int re_get_wn PROTO((region_id_t region_id));
region_id_t re_get_parent PROTO((region_id_t region_id));

void re_expose PROTO((region_id_t region_id, bool clear_first));
void re_redraw_root PROTO((unsigned long event_type, bool clear_first));

void re_event_loop PROTO((region_id_t region_id));

void re_set_exit_event_loop_flag PROTO((void));
bool re_get_exit_event_loop_flag PROTO((void));

/*  clear_message() must be provided by the application.
 *  It should remove any currently displayed error message.
 */
void clear_message PROTO((void));
