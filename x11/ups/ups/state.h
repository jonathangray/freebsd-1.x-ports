/* state.h - public header file for state.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)state.h	1.7 13/8/92 (UKC) */

typedef enum {
	TM_START, TM_NEXT, TM_STEP, TM_CONT, TM_STOP, TM_EXIT, TM_NTAGS
} target_menu_index_t;

typedef struct {
	struct {
		int md;
		int wn;
	} tm_mdtab[TM_NTAGS];
	int tm_current_md;
} target_menu_info_t;

void set_tabwidth PROTO((int tabwidth));
int get_tabwidth PROTO((void));

void set_target_menu_info PROTO((target_menu_info_t *tm));
target_menu_info_t *get_target_menu_info PROTO((void));

void set_message_wn PROTO((int wn));
int get_message_wn PROTO((void));

void set_display_area_wn PROTO((int wn));
int get_display_area_wn PROTO((void));

#ifdef FED_H_INCLUDED
void set_typing_line_edesc PROTO((edesc_t *edesc));
edesc_t *get_typing_line_edesc PROTO((void));
#endif
const char *get_typing_line_string PROTO((void));

#ifdef OBJ_TARGET_H_INCLUDED
void set_target_state PROTO((tstate_t target_state));
tstate_t get_target_state PROTO((void));
#endif

#ifdef OUTPUT_H_INCLUDED
void set_current_outwin PROTO((outwin_id_t outwin_id));
outwin_id_t get_current_outwin PROTO((void));
#endif

#ifdef SRC_H_INCLUDED
srcwin_id_t get_current_srcwin PROTO((void));
#endif

#ifdef REG_H_INCLUDED
void set_current_srcwin_region PROTO((region_id_t region));
region_id_t get_current_srcwin_region PROTO((void));

void set_dynamic_menu_region PROTO((region_id_t region));
region_id_t get_dynamic_menu_region PROTO((void));
#endif
