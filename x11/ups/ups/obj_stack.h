/* obj_stack.h - header file for stack.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)obj_stack.h	1.6 13/8/92 (UKC) */

/*  Type of the pos arguments to interate_over_stack_funcs().
 *
 *  If there is exactly one stack frame selected, STK_SELECTED refers to it.
 *  Otherwise it means the same as STK_CURRENT.
 */
typedef enum stackposen {
	STK_INNER,	/* innermost frame */
	STK_OUTER,	/* outermost frame */
	STK_CURRENT,	/* `current' frame (for which source is displayed) */
	STK_SELECTED	/* see above */
} stackpos_t;

taddr_t get_outer_fp PROTO((void));

#ifdef OBJ_H_INCLUDED
const char *block_getobjname PROTO((objid_t obj));
taddr_t get_reg_addr PROTO((objid_t par, int reg));
int get_command_line_from_stack PROTO((char *command_line, int command_line_size));
objid_t add_functions PROTO((proc_t proc, objid_t par));
void update_local_variable_values PROTO((void));
void delete_functions PROTO((objid_t par));

const char *func_getobjname PROTO((objid_t code));
int func_dumpobj PROTO((char *arg, objid_t code, int level));
void do_func PROTO((objid_t par, int command));
int block_dumpobj PROTO((char *arg, objid_t code, int level));
void do_block PROTO((objid_t obj, int command));
int badstack_dumpobj PROTO((char *arg, objid_t code, int level));
int fsig_dumpobj PROTO((char *arg, objid_t code, int level));
void free_func PROTO((objid_t obj));
void free_badfunc PROTO((objid_t obj));
void free_fsig PROTO((objid_t obj));
void free_block PROTO((objid_t obj));

#ifdef SYMTAB_H_INCLUDED
func_t *get_stack_func PROTO((objid_t obj, taddr_t *p_fp, taddr_t *p_ap));
objid_t find_block PROTO((objid_t par, block_t *block));
objid_t add_block_object PROTO((objid_t par, block_t *block));
bool iterate_over_stack_funcs PROTO((
			    bool (*func)(objid_t stack_obj, func_t *f, char *args),
			    char *args, stackpos_t startpos, stackpos_t endpos));
#endif /* SYMTAB_H_INCLUDED */
#endif /* OBJ_H_INCLUDED */

#ifdef SYMTAB_H_INCLUDED
void get_current_func PROTO((func_t **p_func, taddr_t *p_fp,
					taddr_t *p_pc, taddr_t *p_adjusted_pc));
#endif

const char *stack_addr_label PROTO((taddr_t addr, bool *p_is_labelled));

extern const char Func_format[];
extern const char Fsig_format[];
extern const char Badfunc_format[];
extern const char Block_format[];

#ifdef OBJTYPES_H_INCLUDED
extern fdef_t Fsig_fdefs[];
extern fdef_t Func_fdefs[];
#endif
