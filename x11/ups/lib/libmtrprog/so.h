/* so.h - header file for the so file reading module */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)so.h	1.10 5/5/91 (UKC) */

#ifndef SO_H_INCLUDED
#define SO_H_INCLUDED

typedef struct so_idst { int soi_dummy; } *so_id_t;

/*  We would like to use time_t and off_t here, but that would
 *  require anyone who #included so.h to also #include sys/types.h.
 */
typedef struct {
	long si_mtime;
	long si_size;
} so_info_t;

/*  A type for the line callback function passed to so_open().
 */
typedef int (*so_line_callback_t)PROTO((so_id_t so_id, long nlines, long pos));

typedef int (*so_input_func_t)PROTO((char *arg,
				     long offset, char *buf, int nbytes));
typedef void (*so_close_func_t)PROTO((char *arg));
typedef int (*so_info_func_t)PROTO((char *arg, so_info_t *si));

void so_add_to_source_path PROTO((const char *path));

so_id_t so_open_file PROTO((const char *name, so_line_callback_t line_callback));
so_id_t so_open_via_func PROTO((const char *name,
				so_input_func_t input_func,
				so_close_func_t close_func,
				so_info_func_t get_info_func,
				char *arg,
				so_line_callback_t line_callback));
			   
void so_close PROTO((so_id_t so_id));
long so_get_size PROTO((so_id_t so_id));
const char *so_get_name PROTO((so_id_t so_id));
int so_get_max_linelen PROTO((so_id_t so_id));
int so_get_nlines PROTO((so_id_t so_id));
void so_read_more PROTO((so_id_t so_id, bool reread));
int so_has_changed PROTO((so_id_t so_id, int *p_reread));
char *so_getline PROTO((so_id_t so_id, int lnum));
char *so_peekline PROTO((so_id_t so_id, int lnum));
long so_mod_time PROTO((so_id_t so_id));
int so_set_default_tabwidth PROTO((int tabwidth));
int so_set_tabwidth PROTO((so_id_t so_id, int tabwidth));
#endif /* !SO_H_INCLUDED */
