/* va.h - public header file for the va_* package */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)va.h	1.7 26/7/92 (UKC) */

#ifdef OBJ_H_INCLUDED
const char *var_getobjname PROTO((objid_t obj));
int var_dumpobj PROTO((char *arg, objid_t code, int level));
void do_vars PROTO((objid_t obj, int command));
const char *expr_getobjname PROTO((objid_t obj));
int expr_dumpobj PROTO((char *arg, objid_t code, int level));
void do_expr PROTO((objid_t obj, int command));
void free_displayed_var PROTO((objid_t obj));
void free_displayed_expr PROTO((objid_t obj));
void var_getcolor PROTO((objid_t obj, short *p_fg, short *p_bg));
void expr_getcolor PROTO((objid_t obj, short *p_fg, short *p_bg));
void update_vars_of PROTO((objid_t par, int change_caused_by_edit));

#ifdef SYMTAB_H_INCLUDED
objid_t find_var PROTO((objid_t par, var_t *v));
objid_t add_var_object PROTO((objid_t par, var_t *v, int poscode));
void add_expr_object PROTO((objid_t par, block_t *bl, language_t language));
int get_member_of_aggr PROTO((objid_t par, const char *name,
						var_t **p_v, taddr_t *p_addr));
#endif

extern char Var_format[], Expr_format[];
extern fdef_t Var_fdefs[], Expr_fdefs[];
extern fnamemap_t Var_fnamemap[], Expr_fnamemap[];
#endif

#ifdef FED_H_INCLUDED
/*  This is used only by tdr.c.
 */
void adjust_index PROTO((edesc_t *edesc, bool up));
#endif
