/* objtypes.h - ot_t structure and #defines for the various object types */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)objtypes.h	1.9 26/7/92 (UKC) */

#define OBJTYPES_H_INCLUDED

/*  The object types.  These are used to index an array, so don't change
 *  them.
 */
#define OT_COM		0
#define OT_GLOBALS	1
#define OT_SRCHEAD	2
#define OT_SFILE	3
#define OT_FHEAD	4
#define OT_FUNC		5
#define OT_BLOCK	6
#define OT_FSIG		7
#define OT_BADFUNC	8
#define OT_BADFRAME	9
#define OT_VAR		10
#define OT_EXPR		11
#define OT_BPHEAD	12
#define OT_BPT		13
#define OT_SGHEAD	14
#define OT_SIG		15
#define OT_CBHEAD	16
#define OT_CBLOCK	17
#define OT_ENVHEAD	18
#define OT_ENV		19

#define OT_MAXTYPE	19

/*  A special type meaning that there are no objects selected.
 */
#define OT_NO_TYPE	(-1)

/*  Ugly special case - expanded source file has a different format
 *  to a collapsed one.
 *
 *  BUG: must fix Objtab[] table.
 */
#define OT_SFILE_EX	20

/*  Field definition table structure.
 *  Table is terminated by a NUL fd_char.
 */
typedef struct fdefst {
	char fd_char;
	obj_draw_func_t fd_draw;
	obj_edit_func_t fd_edit;
	obj_getwidth_func_t fd_getwidth;
} fdef_t;

/*  Structure mapping field names to field numbers.
 *  This is used by the test code in tdr.c.
 */
typedef struct fnamemap_s {
	const char *fm_name;
	int fm_fnum;
} fnamemap_t;

/*  Element in the table giving per object type information.
 */
typedef struct {
	struct mnode *ot_men; /* should be MENU *, but don't want menu3.h */
	const char *ot_menuname;
	const char *ot_menupath;
	int ot_md;

	void (*ot_mfunc) PROTO((objid_t obj, int command));
	const char *ot_format;
	fdef_t *ot_fdefs;
	fnamemap_t *ot_fnamemap;

	obj_select_func_t ot_select;
	obj_can_select_func_t ot_can_select;
	obj_free_obj_func_t ot_free;
	obj_get_size_func_t ot_get_size;
	obj_get_color_func_t ot_get_color;

	obj_get_name_func_t ot_get_name;
	obj_dump_func_t ot_dump;
} ot_t;

/*  The table of object types.
 */
extern ot_t Objtab[];

int get_cur_objtype PROTO((void));
