/* obj_bpt.h - public header file for obj_bpt.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)obj_bpt.h	1.8 26/7/92 (UKC) */

void recalculate_bpt_addrs PROTO((void));
void reinitialise_bpt_code_data PROTO((void));

#ifdef SYMTAB_H_INCLUDED
void remove_symtab_breakpoints PROTO((symtab_id_t symtab_id));
void set_main_func PROTO((func_t *f));
#endif

#ifdef BREAKPOINT_H_INCLUDED
bool execute_bp_code PROTO((breakpoint_t bp, taddr_t fp, taddr_t ap));
#endif

#ifdef OBJ_H_INCLUDED

#ifdef SYMTAB_H_INCLUDED
objid_t add_breakpoint_object PROTO((func_t *f, int lnum));
#endif

const char *bpt_getobjname PROTO((objid_t code));
int bpt_dumpobj PROTO((char *arg, objid_t code, int level));
void do_bpt PROTO((objid_t obj, int command));
void add_breakpoint_header PROTO((objid_t par));
void do_bps PROTO((objid_t obj, int command));
void remove_breakpoint_object PROTO((objid_t obj));
void bpt_select PROTO((int wn, objid_t obj, int x, int y,
					int width, int height, int flags));

int change_bpt_lines PROTO((objid_t obj, const char **lines, int nlines));

extern const char Bpt_format[];
extern const char Bphead_format[];

#endif /* OBJ_H_INCLUDED */

#ifdef OBJTYPES_H_INCLUDED
extern fdef_t Bpt_fdefs[];
extern fnamemap_t Bpt_fnamemap[];
#endif
