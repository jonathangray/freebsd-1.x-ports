/* ci_lex.h - header file for ci_lex.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)ci_lex.h	1.3 26/4/92 (UKC) */

/*  Environment for yylex.
 */
typedef struct lex_envst {
	const char *le_lptr;
	const char *le_filename;
	int le_lnum;
	bool le_had_error;
	const char *le_line;
	const char *(*le_getline)PROTO((char *arg));
	ci_report_error_func_t le_report_func;
	char *le_getline_arg;
	bool le_abort_parse;
} lex_env_t;

typedef int token_t;

extern lex_env_t *Lex_env;

int yylex PROTO((void));
void yyerror PROTO((const char *s));
void ci_lex_error PROTO((char *arg, errtype_t errtype,
					lexinfo_t *lx, const char *mesg));

/*  is_typedef() is actually defined in ci_parse.y, but it is only used
 *  from ci_lex.c, so we put it here for minumum visibility.
 */
token_t name_type PROTO((const char *name));
