/* obj_target.h - public header file for obj_target.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)obj_target.h	1.7 26/7/92 (UKC) */

#define OBJ_TARGET_H_INCLUDED

/*  Various states of of the target process.
 */
typedef enum tstateen {
	TS_STOPPED_AND_CAN_CONTINUE,
	TS_STOPPED_AND_CANT_CONTINUE,
	TS_CORE,			/* debugging from a core file */
	TS_NOTR,			/* not run yet and there is no core file */
	TS_RUNNING			/* running (transient, in ui_menu.c only */
} tstate_t;

int setup_args PROTO((const char **p_path, const char ***p_argv,
				     const char ***p_envp, long *p_rdlist));
int target_process_exists PROTO((void));
int can_get_target_vars PROTO((void));
void set_tstate PROTO((tstate_t state));

#ifdef OBJ_H_INCLUDED
void add_target_object PROTO((objid_t par, const char *efile,
			      const char *minus_a_cmdline));
int target_dumpobj PROTO((char *arg, objid_t code, int level));
void do_target PROTO((objid_t par, int command));
void free_com PROTO((objid_t obj));
#endif

extern const char Com_format[];

#ifdef OBJTYPES_H_INCLUDED
extern fdef_t Com_fdefs[];
extern fnamemap_t Com_fnamemap[];
#endif
