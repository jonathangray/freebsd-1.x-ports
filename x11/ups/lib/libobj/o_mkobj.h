/* o_mkobj.h - header file for o_mkobj.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)o_mkobj.h	1.4 4/7/91 (UKC) */

void enter_obj PROTO((struct objst *obj, objid_t code));
void set_likely PROTO((struct objst *obj));
struct objst *code_to_obj PROTO((objid_t code));
void link_object PROTO((struct objst *obj, struct objst *par, struct objst *next, struct objst *prev));
void unlink_object PROTO((struct objst *obj));
void make_obj PROTO((struct objst *par, struct objst *next, struct objst *prev, objid_t code, int type));
void rm_obj_tree PROTO((struct objst *obj, int kill_children, int kill_parent, int kill_descendents));
void hash_stats PROTO((void));
struct fvalst *new_fval PROTO((void));
struct fvalst *new_fval_list PROTO((int nfields));
void free_fval_list PROTO((struct fvalst *fval));
