/* obj_buildf.h - header file for obj_buildf.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)obj_buildf.h	1.8 13/8/92 (UKC) */

void do_formats PROTO((bool have_window));
void close_target_display PROTO((void));
objid_t rebuild_display PROTO((proc_t proc));
void update_variable_values PROTO((void));
void initialise_display_area PROTO((const char *textname, const char *corename,
				    const char *args, int have_common_blocks));
