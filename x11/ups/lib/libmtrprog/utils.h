/* utils.h - header file for utils.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)utils.h	1.13 26/4/92 (UKC) */

typedef void (*message_output_func_t)PROTO((const char *mesg));

message_output_func_t set_message_output_func PROTO((message_output_func_t func));
void message PROTO((const char *fmt, ...));
void error PROTO((const char *fmt, ...));

int query_user PROTO((const char *prompt, const char *yes, const char *no,
								bool *p_isyes));

const char *mtrprog_version PROTO((void));
int is_number PROTO((const char *s));
int string_to_bufsize PROTO((const char *s, long *p_res));
int string_to_offset PROTO((const char *s, long *p_res));
void swap_longs PROTO((long *base, int count));

const char *filemode_to_string PROTO((int mode));
const char *filetype_to_string PROTO((int mode));
int e_close PROTO((int fd, const char *filename));
int remove_directory_tree PROTO((const char *name));
int apply_to_files_in PROTO((const char *name, int (*func)(const char *dirname,
				     const char *filename, char *farg), char *arg));

#ifdef EOF
FILE *fopen_new_file PROTO((const char *name));
int fclose_new_file PROTO((FILE *fp, const char *name));
#endif

/*  This macro will is intended for use on constant expressions.
 *  It will work for variables, but it evaluates its argument four
 *  times, and is inefficient.
 */
#define SWAP_LONG(n)	(((n) << 24) | (((n) << 8) & 0xff0000) | \
					((n) >> 8 & 0xff00) | (((n) >> 24) & 0xff)) 

/*  Return TRUE if mode (as returned by stat(2)) represents a directory.
 */
#define isdir(mode)	(((mode) & S_IFMT) == S_IFDIR)
