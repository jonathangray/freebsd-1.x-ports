/* ui.h - declarations of public user interface routines */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)ui.h	1.18 13/8/92 (UKC) */

#ifdef REG_H_INCLUDED
region_id_t divide_window_into_regions PROTO((int wn));
#endif

void display_message PROTO((const char *mesg));
void update_target_menu_state PROTO((void));
void add_outwin PROTO((void));

typedef void (*abort_func_t)PROTO((void));
abort_func_t set_user_abort_func PROTO((abort_func_t func));
bool user_wants_stop PROTO((void));

#ifdef SYMTAB_H_INCLUDED
void show_source PROTO((fil_t *fil, int lnum, bool centre));
int highlight_source PROTO((fil_t *fil, int lnum));
fil_t *get_displayed_fil PROTO((void));
#endif

#ifdef OBJ_H_INCLUDED
void dynamic_menu_func PROTO((char *data, int md, int rv));
int ups_get_object_type PROTO((objid_t obj));
void add_to_new_selection PROTO((objid_t obj));
void ensure_visible PROTO((objid_t obj));
bool change_field PROTO((objid_t obj, int fnum, const char *new));
void n_draw PROTO((struct drawst *dets));
bool can_select PROTO((objid_t obj));
void gen_select PROTO((int wn, objid_t obj, int x, int y,
					int width, int height, int flags));
#endif /* OBJ_H_INCLUDED */

#ifdef FED_H_INCLUDED
int suppress_ta_cursor_then_edit_field PROTO((edesc_t *edesc,
							const char *what));
int message_on_null_name PROTO((edesc_t *edesc, const char *field_type));
int force_quit PROTO((int n_tries));
void num_filter PROTO((edesc_t *edesc));
#endif

#ifdef CI_H_INCLUDED
typedef struct {
	parse_id_t cr_parse_id;
	code_id_t cr_code_id;
	bool cr_code_has_func_calls;
} compile_res_t;

compile_res_t *compile_code PROTO((const char **lines, int nlines,
				   block_t *block, char *grp_arg,
				   lexinfo_t *error_lx,
				   const char *firstline, const char *lastline,
				   const char *repl_text, const char *repl_val));

code_id_t recompile_code PROTO((parse_id_t parse_id, code_id_t code_id,
								char *grp_arg));
void free_parse_and_code_ids PROTO((parse_id_t parse_id, code_id_t code_id));
#endif

/*  The following stuff would be in ui_priv.h were it not for the fact
 *  that tdr.c needs access to it.
 */
void target_menu_func PROTO((char *data, int md, int rv));
void permanent_menu_func PROTO((char *data, int md, int rv));
void do_debug_command PROTO((const char *s));

/*  State for set_dynamic_menu_updating_state().
 */
typedef enum { DMU_OFF, DMU_ON } dmu_state_t;

void set_dynamic_menu_updating_state PROTO((dmu_state_t new_state));

