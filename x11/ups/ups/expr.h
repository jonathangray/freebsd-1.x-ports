/* expr.h - header file for expr.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)expr.h	1.4 26/7/92 (UKC) */

int get_varname PROTO((fil_t *fil, const char *text, const char *pos, int bufsize,
					const char **p_start, const char **p_end));
func_t *lnum_to_func PROTO((fil_t *fil, int lnum));

#ifdef SRC_H_INCLUDED
void show_var_from_typing_line PROTO((srcwin_id_t srcwin_id, const char *name));
void show_var PROTO((srcwin_id_t srcwin_id, fil_t *fil, int lnum,
							const char *wholename));
#endif
