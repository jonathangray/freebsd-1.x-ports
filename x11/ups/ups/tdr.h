/* tdr.h - public header file for tdr.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)tdr.h	1.3 13/9/92 (UKC) */

void td_record_menu_command PROTO((const char *menu_name, int rv));
int td_record_to PROTO((const char *filename));
bool td_have_window PROTO((void));

int td_replay_from PROTO((const char *filename));
void td_set_no_window_flag PROTO((void));
int td_set_replay_mode PROTO((const char *cmd));
int td_event_loop PROTO((bool *p_eof));

void td_record_adjust_index PROTO((const char *what, int curpos, bool up));

bool td_set_select_recording PROTO((bool val));
bool td_set_obj_updating PROTO((bool val));

void td_check PROTO((const char *objpath));

#ifdef OBJ_H_INCLUDED
void td_record_bpt_code_edit PROTO((objid_t obj,
					const char **lines, int nlines));
void td_record_select PROTO((objid_t obj, bool on));
void td_obj_edit_field PROTO((objid_t obj, int fnum, int x, int y));
#endif

void td_add_outwin PROTO((void));
#ifdef SO_H_INCLUDED
void td_handle_output PROTO((so_id_t so, int old_nlines, int new_nlines));
#endif

void td_set_default_obj_to_selection PROTO((void));

#ifdef SYMTAB_H_INCLUDED
void td_record_func_and_lnum_cmd PROTO((func_t *f, int lnum, const char *op));
void td_record_show_var PROTO((fil_t *fil, int lnum, const char *name));
bool td_set_displayed_source PROTO((fil_t *fil, int lnum, const char *op));
int td_get_displayed_fil PROTO((fil_t **p_fil));
#endif

void td_record_refresh PROTO((void));
void td_record_debug_command PROTO((const char *cmd));

int td_outf PROTO((int level, const char *fmt, ...)) FORMF_ARGS(2, 3);

#ifdef FED_H_INCLUDED
int td_record_edit_field PROTO((edesc_t *edesc, const char *what));
#endif
