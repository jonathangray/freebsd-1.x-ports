/* o_sel.h - header file for o_sel.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)o_sel.h	1.4 29/6/92 (UKC) */

void sel_obj_tree PROTO((struct objst *par, int sel_self, int sel_children, int sel_descendents, int new_state));
void no_edit PROTO((struct drawst fdets));
fval_t iget_field_value PROTO((struct objst *obj, int fnum));
void obj_pos PROTO((struct objst *stop_obj, int *p_x, int *p_y));
void do_pending_selection PROTO((void));
objid_t get_object_at PROTO((int puck_x, int puck_y));
struct fvalst *get_p_fval PROTO((struct objst *obj, int fnum));
void deselect PROTO((struct objst *obj));
