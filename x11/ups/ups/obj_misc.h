/* obj_misc.h - public header file for src.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)obj_misc.h	1.7 26/7/92 (UKC) */

void add_globals_header PROTO((objid_t par));
int addvars PROTO((objid_t par));
void hide_source_vars PROTO((void));

#ifdef SYMTAB_H_INCLUDED
void add_common_block_object_if_necessary PROTO((common_block_id_t cblock));
void add_source_file_object_if_necessary PROTO((fil_t *fil));
#endif

int src_cmp PROTO((objid_t obj1, objid_t obj2));
objid_t find_or_add_object PROTO((objid_t par, objid_t wanted,
				  void (*add_object)PROTO((objid_t wobj))));

void do_globals PROTO((objid_t obj, int command));
void do_srchead PROTO((objid_t obj, int command));
void do_cbhead PROTO((objid_t obj, int command));

int header_dumpobj PROTO((char *arg, objid_t code, int level));

int file_dumpobj PROTO((char *arg, objid_t code, int level));
void do_file PROTO((objid_t obj, int command));

const char *cblock_getobjname PROTO((objid_t obj));
void do_cblock PROTO((objid_t obj, int command));
int cblock_dumpobj PROTO((char *arg, objid_t code, int level));

const char *srcfile_getobjname PROTO((objid_t obj));
void srcfile_getsize PROTO((objid_t obj, objid_t pat, struct szst *sz));

void free_common_block_object PROTO((objid_t obj));

/*  Objcodes for header objects which must be visible in more than one file.
 *  To make a unique objcode, we cast the address of an arbitrary data object.
 *  Here, we use the address of the format string.
 */
extern const char Globals_format[];
#define GLOBALS_OBJCODE	((objid_t)Globals_format)

extern const char Srchead_format[];
#define SRCHEAD_OBJCODE	((objid_t)Srchead_format)

extern const char Cbhead_format[];
#define CBHEAD_OBJCODE	((objid_t)Cbhead_format)

extern const char Fhead_format[];
#define FHEAD_OBJCODE   ((objid_t)Fhead_format)

extern const char Cblock_format[];
extern const char Sfile_ex_format[];
