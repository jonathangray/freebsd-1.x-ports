/* obj_env.h - header file for obj_env.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)obj_env.h	1.4 26/7/92 (UKC) */

const char *env_getobjname PROTO((objid_t obj));
void add_env_header PROTO((objid_t par));
int env_dumpobj PROTO((char *arg, objid_t code, int level));
void do_envhead PROTO((objid_t obj, int command));
void do_env PROTO((objid_t obj, int command));
void free_env PROTO((objid_t obj));
void env_getsize PROTO((objid_t obj, objid_t unused_par, sz_t *sz));

const char **get_environment PROTO((void));

extern const char Envhead_format[];
extern const char Env_format[];

extern fdef_t Env_fdefs[];
extern fnamemap_t Env_fnamemap[];
