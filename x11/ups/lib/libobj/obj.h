/* obj.h - public header file for the obj routines */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)obj.h	1.12 26/7/92 (UKC) */

#ifndef OBJ_H_INCLUDED
#define OBJ_H_INCLUDED

#ifdef __STDC__
#define obj__PROTO(x) x
#else
#define obj__PROTO(x) ()
#define const
#endif /* !__STDC__ */

/*  Type of objcodes. This will usually be cast to a structure
 *  pointer by the routines that use objects (a NOP on most machines)
 */
typedef int *objid_t;

/*  Type of field values stored and retrieved by set_field_value()
 *  and get_field_value()
 */
typedef const char *fval_t;

/*  The structure passed to the drawing and edit routines to display
 *  or edit a field.
 */
typedef struct drawst {
	window_t dr_wn;		/* viewport that coords are relative to */
	short dr_fg;		/* foreground color for this object */
	short dr_bg;		/* background color for this object */
	short dr_x;		/* origin ... */ 
	short dr_y;
	short dr_width;		/* ... and size of the field */
	short dr_depth;
	short dr_fnum;		/* field number within the parent object */
	objid_t dr_code;	/* code of the parent object */
	fval_t dr_fval;		/* field value */
	char *dr_user_info;	/* userinfo from format string - see o_defs.c */
} draw_t;

/*  Element of the linked list of structures returned by get_selection().
 *  This is a list of all the objects selected at the time get_selection()
 *  was called. The list is not affected by subsequent changes to the
 *  selection.
 *
 *  The list is guaranteed to be ordered by level. I.e., objects are
 *  guaranteed to appear before their parents.
 *
 *  The se_user field is set by get_selection to the level of the
 *  object - that is, how many times you could call do
 *  code = get_code(code,OBJ_PARENT) before code became NULL.
 *  It is not used by the obj routines otherwise, and is free for the
 *  user to set.
 */
typedef struct selst {
	objid_t se_code;	/* code of the selected object */
	int se_user;		/* see above */
	struct selst *se_next;	/* next element of the list */
} sel_t;

/* values for get_code and new_object
 */
#define OBJ_PREV	 1
#define OBJ_NEXT	 2
#define OBJ_PARENT	 3
#define OBJ_CHILD	 4
#define OBJ_AFTER	 5
#define OBJ_BEFORE	 6
#define OBJ_CHILDREN	 7
#define OBJ_SELF	 8
#define OBJ_DESCENDENTS	 9
#define OBJ_LAST_CHILD	10

/*  Alternative name for OBJ_CHILD.
 */
#define OBJ_FIRST_CHILD	OBJ_CHILD

/*  Arguments to updating
 */
#define OBJ_UPDATING_OFF	0	/* turn updating on */
#define OBJ_UPDATING_ON		1	/* turn updating off */
#define OBJ_UPDATING_QUERY	2	/* report current state */

/*   This is set whenever the display changes (apart from 
 *   selection/deselection of objects). It is up to the user program
 *   to clear it when it thinks fit.
 */
extern int Display_has_changed;

/*  Flags for the select routines. The values of SEL_VISIBLE and
 *  SEL_ON are not arbitrary (i.e. don't change them).
 *
 *  SEL_ON means the object is currently being selected.
 *  SEL_OFF is zero, not a bit value. It is used for routines which
 *  take an argument which is either on (SEL_ON) or off (SEL_OFF) - e.g.
 *  select_objects().
 *  SEL_FLIP is also not a bit value. It means flip the selected state of
 *  the object.
 *
 *  SEL_CHANGING is set when the selection state is being changed,
 *  as opposed to when we are just redisplaying the object.
 *
 *  SEL_CLEARING, SEL_SELECTING, SEL_UPDATING, SEL_PUCKSELECT
 *  SEL_DISPLAYING and SEL_DELETING  are a set of mutually exclusive bit
 *  values telling the selection routine who is calling it.
 *
 *  They can be used, for example, to tell whether this is a bulk
 *  selection, or one that the user has made.
 *
 *  The basic rules for a select routine are:
 *
 *	If you need to remember the selected/deselected state of
 *	an object, flip this when the SEL_CHANGING bit is set.
 *
 *	If objects are highlighted in some way to indicate that they
 *	are selected, turn this highlighting on or off if and only if
 *	the SEL_VISIBLE bit is set.
 */
#define SEL_FLIP		(-1)	/* imperative: flip selection state */
#define SEL_OFF			   0	/* object deselected */
#define SEL_ON			  01	/* object selected */
#define SEL_WAS_ON		  02	/* object was selected before */
#define SEL_VISIBLE		  04	/* object is currently visible */
#define SEL_CHANGING	 	 010	/* selection state changing */
#define SEL_CLEARING		 020	/* called from clear_selection() */
#define SEL_SELECTING		 040	/* called from select_object() */
#define SEL_UPDATING		0100	/* called 'cos updating turned on */
#define SEL_PUCKSELECT		0200	/* user selected with the puck */
#define SEL_DISPLAYING		0400	/* called 'cos redisplaying */
#define SEL_DELETING	       01000	/* object is about to be deleted */

#define BITPOS_SEL_WAS_ON	1
#define BITPOS_SEL_VISIBLE	2	

/*  structure passed to size routines. See def.c
 */
typedef struct szst {
	short sz_width;
	short sz_depth;
	int sz_same;
} sz_t;

/*  Typedefs for the callback functions.
 */
typedef int (*obj_can_select_func_t)obj__PROTO((objid_t code));

typedef void (*obj_select_func_t)obj__PROTO((int wn, objid_t code,
					int x, int y, int width, int height,
					int flags));

typedef void (*obj_free_obj_func_t)obj__PROTO((objid_t code));

typedef void (*obj_get_size_func_t)obj__PROTO((objid_t code, objid_t par,
							struct szst *size));

typedef void (*obj_get_color_func_t)obj__PROTO((objid_t code,
						short *p_fg, short *p_bg));

typedef void (*obj_updating_callback_func_t)obj__PROTO((int oldstate,
							int newstate));

typedef const char *(*obj_get_name_func_t)obj__PROTO((objid_t code));
typedef int (*obj_dump_func_t)obj__PROTO((char *arg, objid_t code, int level));

typedef void (*obj_draw_func_t)obj__PROTO((struct drawst *p_dets));
typedef void (*obj_edit_func_t)obj__PROTO((struct drawst dets));
typedef int (*obj_getwidth_func_t)obj__PROTO((objid_t obj, int fnum,
								fval_t fval));

typedef int (*obj_visit_objects_func_t)obj__PROTO((objid_t code, fval_t arg));
typedef int (*obj_sort_func_t)obj__PROTO((objid_t code1, objid_t code2));

typedef void (*obj_pre_edit_func_t)obj__PROTO((struct drawst *p_dets));

typedef enum {
	OBJ_NEW_YPOS,
	OBJ_NEW_HEIGHT,
	OBJ_NEW_WINDOW_HEIGHT
} obj_change_t;

typedef void (*obj_change_callback_t)obj__PROTO((char *data, obj_change_t change, int val));

#define define_format(fch, f_edit, f_draw) \
		define_vsformat(fch, f_edit, f_draw,(obj_getwidth_func_t)NULL)

obj_pre_edit_func_t obj_set_pre_edit_func obj__PROTO((obj_pre_edit_func_t func));
void obj_set_callback_and_data obj__PROTO((obj_change_callback_t callback, char *data));
void new_object obj__PROTO((objid_t code, int type, objid_t poscode, int where));
void move_object obj__PROTO((objid_t code, objid_t poscode, int where));
int remove_object obj__PROTO((objid_t code, int which));
void set_no_indent obj__PROTO((objid_t code, int set));
void select_object obj__PROTO((objid_t code, int val, int which));
int dump_object obj__PROTO((objid_t code, char *arg, int which));
void set_all_fields obj__PROTO((objid_t objcode, fval_t *flist, fval_t terminator));
void set_field_value obj__PROTO((objid_t objcode, int fnum, fval_t fval));
void update_field_value obj__PROTO((objid_t objcode, int fnum, fval_t fval));
fval_t get_field_value obj__PROTO((objid_t objcode, int fnum));
void get_all_fields obj__PROTO((objid_t code, fval_t *fields));
void change_type obj__PROTO((objid_t objcode, int newtype));
void init_objects obj__PROTO((void));
void obj_set_default_colors obj__PROTO((int fg, int bg));
objid_t get_code obj__PROTO((objid_t code, int which_rel));
objid_t get_ancestor_of_selection obj__PROTO((void));
int get_num_children obj__PROTO((objid_t parcode));
void sort_children obj__PROTO((objid_t parcode, obj_sort_func_t cmp_func));
int obj_child_exists obj__PROTO((objid_t parcode, objid_t code));
int visit_objects obj__PROTO((objid_t code, int rel,
			 obj_visit_objects_func_t func,
			 fval_t arg, objid_t *p_code));

int get_num_selected obj__PROTO((void));
struct selst *get_selection obj__PROTO((void));
struct selst *get_reverse_selection obj__PROTO((void));
struct selst *get_unordered_selection obj__PROTO((void));
void clear_selection obj__PROTO((void));
objid_t get_object_at obj__PROTO((int puck_x, int puck_y));
void select_list obj__PROTO((void));
void obj_edit_field obj__PROTO((objid_t objcode, int fnum, int x, int y));

int def_can_select obj__PROTO((objid_t code));
int cannot_select obj__PROTO((objid_t code));

int v_scroll obj__PROTO((int npixels));
int visible obj__PROTO((objid_t code));
void get_position obj__PROTO((objid_t code, int *p_x, int *p_y, int *p_width, int *p_depth));
int get_cur_posn obj__PROTO((void));
void set_obj_wn obj__PROTO((window_t wn));
void display_from obj__PROTO((int obj_x, int obj_y));

void obj_has_changed obj__PROTO((objid_t code));
int get_object_type obj__PROTO((objid_t code));
int get_info_depth obj__PROTO((void));
int updating obj__PROTO((int val));
void obj_register_updating_callback_func obj__PROTO((obj_updating_callback_func_t func));

#ifdef UKCPROG_H_DEFINED
void obj_to_vec obj__PROTO((alloc_id_t alloc_id, objid_t code,
					const char ***p_vec, int *p_ncomp));
#endif

objid_t vec_to_obj obj__PROTO((const char **p_vec, int ncomp));

void set_field_scale_factors obj__PROTO((int x, int y));

void define_objtype obj__PROTO((int type, const char *format, int child_indent,
			   obj_select_func_t select_func,
			   obj_can_select_func_t can_select_func,
			   obj_free_obj_func_t free_obj_func,
			   obj_get_size_func_t getsize_func,
			   obj_get_color_func_t get_color_func));
void set_objtype_get_name_func obj__PROTO((int type, obj_get_name_func_t func));
void set_objtype_dump_func obj__PROTO((int type, obj_dump_func_t func));

void define_vsformat obj__PROTO((int fch,
			    obj_edit_func_t f_edit,
			    obj_draw_func_t f_draw,
			    obj_getwidth_func_t f_getwidth));

void s_draw obj__PROTO((struct drawst *dets));
void l_draw obj__PROTO((struct drawst *dets));
void no_edit obj__PROTO((struct drawst fdets));

const char *obj_version obj__PROTO((void));

#endif /* !OBJ_H_INCLUDED */
