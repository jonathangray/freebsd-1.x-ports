/* newtb.h - type declarations for the thumb bar routines in newtb.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)newtb.h	1.4 7/13/91 (UKC) */

#define TB_H_INCLUDED

typedef struct { int dummy_member; } *tbar_id_t;

typedef enum { TB_GOTO, TB_SCROLL, TB_SHOW_POS } tb_action_t;

typedef int (*tb_callback_t)PROTO((char *arg, tb_action_t action, int val));

void tb_set_callback_and_data PROTO((tbar_id_t tbar_id,
					tb_callback_t callback, char *data));
void tb_destroy PROTO((tbar_id_t tbar_id));
tbar_id_t tb_create PROTO((int wn));
void tb_set_box_pos_and_size PROTO((tbar_id_t tbar_id, bool box_is_vertical,
				    int box_fstart, int box_fsize));
void tb_set_marks_pos_and_size PROTO((tbar_id_t tbar_id,
						int box_fstart, int box_fsize));
void tb_expose PROTO((tbar_id_t tbar_id));
void tb_set_visible_units PROTO((tbar_id_t tbar_id, int visible_units));
int tb_get_visible_units PROTO((tbar_id_t tbar_id));
void tb_set_total_units PROTO((tbar_id_t tbar_id, int total_units));
void tb_clear_marks PROTO((tbar_id_t tbar_id));
void tb_refresh PROTO((tbar_id_t tbar_id));
void tb_mark PROTO((tbar_id_t tbar_id, int pos, int size, int color, int update));
void tb_goto PROTO((tbar_id_t tbar_id, int pos, bool call_callback));
void tb_scroll PROTO((tbar_id_t tbar_id, int n_units, bool call_callback));
void tb_show_pos PROTO((tbar_id_t tbar_id, int unit));
int tb_tbpos_to_unit PROTO((tbar_id_t tbar_id, int wn, int coord, bool centre));
