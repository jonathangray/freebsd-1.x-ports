/* o_chds.c - user callable routines to add and remove objects */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char obj_chds_sccsid[] = "@(#)o_chds.c	1.14 26/7/92 (UKC)";

#include <local/wn.h>

#include <local/ukcprog.h>
#include <mtrprog/genmergesort.h>

#include "obj.h"
#include "o_priv.h"

#include "o_disp.h"
#include "o_sel.h"
#include "o_lptrs.h"
#include "o_mkobj.h"
#include "sccsdata.h"

static struct objst *last_child PROTO((struct objst *par));
static void update_display PROTO((struct objst *obj));
static int objcmp PROTO((struct objst *obj1, struct objst *obj2));
static int dump_obj_tree PROTO((struct objst *par, bool sel_self,
				 bool sel_children, bool sel_descendents,
				 int level, char *arg));

static obj_sort_func_t Sort_cmp_func;

/*  See genmergesort.h
 */
GENERIC_MERGE_SORT(static,objsort,struct objst,ob_next)

/*  Return the version string for the obj library
 */
const char *
obj_version()
{
	return _obj_sccsdata[0];
}

/*  Return the last child of object par, or NULL if par has no children.
 */
static struct objst *
last_child(par)
struct objst *par;
{
	register struct objst *obj, *prev;

	prev = NULL;
	for (obj = par->ob_child; obj != NULL; obj = obj->ob_next)
		prev = obj;
	return prev;
}

/*  Make a new object with code code, of type type. Poscode
 *  specifies already existing object, and where gives the relation
 *  of the new object to it.
 *  A poscode of NULL refers to the root object. Thus the tree
 *  starts with new_object(code,type,NULL,OBJ_CHILD).
 *  It is illegal for where to have any other value than OBJ_CHILD
 *  when poscode is NULL, though this is not checked for.
 */
void
new_object(code, type, poscode, where)
objid_t code;
int type;
objid_t poscode;
int where;
{
	struct objst *obj;
	
	obj = code_to_obj(poscode);
	switch(where) {
	case OBJ_LAST_CHILD:
		make_obj(obj, (struct objst *)NULL, last_child(obj), code, type);
		break;
	case OBJ_CHILD:
		make_obj(obj, obj->ob_child, (struct objst *)NULL, code, type);
		break;
	case OBJ_AFTER:
		make_obj(obj->ob_parent, obj->ob_next, obj, code, type);
		break;
	case OBJ_BEFORE:
		make_obj(obj->ob_parent, obj, obj->ob_prev, code, type);
		break;
	default:
		panic("unknown where in new_object");
	}
}

/*  Move object code to where in relation to poscode. Check
 *  that we are not making an object a child of itself.
 */
void
move_object(code, poscode, where)
objid_t code, poscode;
int where;
{
	struct objst *obj, *orig, *dest;
	int old_indent;
	
	orig = code_to_obj(code);
	dest = code_to_obj(poscode);
	for (obj = dest; obj != NULL; obj = obj->ob_parent)
		if (obj == orig)
			panic("illegal destination in move_object");
	old_indent = get_indent(orig);
	unlink_object(orig);
	switch(where) {
	case OBJ_CHILD:
		link_object(orig, dest, dest->ob_child, (struct objst *)NULL);
		break;
	case OBJ_AFTER:
		link_object(orig, dest->ob_parent, dest->ob_next, dest);
		break;
	case OBJ_BEFORE:
		link_object(orig, dest->ob_parent, dest, dest->ob_prev);
		break;
	default:
		panic("unknown where in move_object");
	}
	if (get_indent(orig) != old_indent)
		set_update_lptrs(orig, TRUE);
}

/*  Remove objects. Action depends on the value of which (see code).
 *  See rm_obj_tree in mkobj.c for the actual removal of objects.
 */
int
remove_object(code, which)
objid_t code;
int which;
{
	struct objst *obj;
	int retval;
	
	retval = 0;
	obj = code_to_obj(code);
	switch(which) {
	case OBJ_SELF: /* remove object code, if it has no children
			*/
		if (obj->ob_child != NULL)
			retval = -1;
		else
			rm_obj_tree(obj, FALSE, TRUE, FALSE);
		break;
	case OBJ_CHILDREN: /* remove children of code, which themselves
			    * have no children
			    */
		rm_obj_tree(obj, TRUE, FALSE, FALSE);
		break;
	case OBJ_DESCENDENTS: /* recursivly remove all descendents of code */
		rm_obj_tree(obj, TRUE, FALSE, TRUE);
		break;
	default:
		panic("unknown which in remove_object");
	}
	return retval;
}

void
set_no_indent(code, set)
objid_t code;
int set;
{
	struct objst *obj;

	obj = code_to_obj(code);
	if (set)
		obj->ob_flags |= OB_NO_INDENT;
	else
		obj->ob_flags &= ~OB_NO_INDENT;
	set_update_lptrs(obj, TRUE);
}

/*  Call the (*od_dump_obj)() function for the object and relations
 *  specified by the which argument, which can be OBJ_SELF, OBJ_CHILDREN
 *  or OBJ_DESCENDENTS.
 */
int
dump_object(code, arg, which)
objid_t code;
char *arg;
int which;
{
	struct objst *obj;
	int level, res;
	
	if (code == NULL) {
		level = -1;
		obj = &Rootobj;
	}
	else {
		level = 0;
		obj = code_to_obj(code);
	}

	switch(which) {
	case OBJ_SELF:
		res = dump_obj_tree(obj, TRUE, FALSE, FALSE, level, arg);
		break;
	case OBJ_CHILDREN:
		res = dump_obj_tree(obj, FALSE, TRUE, FALSE, level, arg);
		break;
	case OBJ_DESCENDENTS:
		res = dump_obj_tree(obj, FALSE, TRUE, TRUE, level, arg);
		break;
	default:
		panic("unknown which in dump_object");
		res = -1;	/* to satisfy gcc */
	}

	return res;
}

/*  Recursively call the dump function for an object and its children.
 *	Select the object itself if sel_self is TRUE.
 *  	Select the parent's immediate children if sel_children is TRUE
 *	Select all the objects below the parent if sel_descendents is TRUE.
 */
static int
dump_obj_tree(par, sel_self, sel_children, sel_descendents, level, arg)
struct objst *par;
bool sel_self, sel_children, sel_descendents;
int level;
char *arg;
{
	struct objst *obj;
	int res;
	
	if (sel_self) {
		res = (*Odesc[par->ob_type].od_dump_obj)(arg, par->ob_code,
									level);
		if (res != 0)
			return res;
	}

	if (sel_children) {
		for (obj = par->ob_child; obj != NULL; obj = obj->ob_next) {
			res = dump_obj_tree(obj, TRUE, sel_descendents, TRUE,
								level + 1, arg);
			if (res != 0)
				return res;
		}
	}
	
	return 0;
}


/*  select specified relations of object code. If val is non-zero,
 *  add them to the selection, otherwise remove them from the selection
 *  The which argument has the same set of meanings as in remove_object.
 */
void
select_object(code, val, which)
objid_t code;
int val, which;
{
	struct objst *obj;
	
	obj = code_to_obj(code);
	switch(which) {
	case OBJ_SELF:
		sel_obj_tree(obj, TRUE, FALSE, FALSE, val);
		break;
	case OBJ_CHILDREN:
		sel_obj_tree(obj, FALSE, TRUE, FALSE, val);
		break;
	case OBJ_DESCENDENTS:
		sel_obj_tree(obj, FALSE, TRUE, TRUE, val);
		break;
	default:
		panic("unknown which in select_object");
	}
}

void
get_all_fields(code, fields)
objid_t code;
fval_t *fields;
{
	struct objst *obj;
	struct fvalst *fv;

	obj = code_to_obj(code);
	if (obj->ob_flags & OB_ONE_FVAL)
		*fields = (fval_t) obj->ob_fval;
	else
		for (fv = obj->ob_fval; fv != NULL; fv = fv->fv_next)
			*fields++ = fv->fv_val;
}
	
/*  Update the display after a change to an object which is guaranteed
 *  not to affect other objects.
 *  If updating is off, mark the object as modified, otherwise just
 *  redraw the object if it is visible.
 */
static void
update_display(obj)
struct objst *obj;
{
	int x, y;
	extern int Updating_state, Offs_x, Offs_y;

	if (Updating_state == OBJ_UPDATING_OFF)
		set_update_lptrs(obj->ob_parent, FALSE);
	else {
		obj_pos(obj, &x, &y);	
		x -= Offs_x;
		y -= Offs_y;
		if (y >= 0 && y + obj->ob_depth < wn_get_height(Obj_wn)) {
			wn_set_area(Obj_wn, 0, y, wn_get_width(Obj_wn),
					obj->ob_depth, _ob_Default_bg_pixel);
			dr_obj(obj, x, y);
		}
	}
}

/*  Set all the fields of object objcode from the array flist, which
 *  is terminated by an entry with the value termionator.
 *  If there are too many or too few field values, abort
 */
void
set_all_fields(objcode, flist, terminator)
objid_t objcode;
fval_t *flist, terminator;
{
	struct objst *obj;
	struct fvalst *p_fval;
	
	obj = code_to_obj(objcode);
	if (obj->ob_flags & OB_ONE_FVAL) {
		if (*flist == terminator)
			panic("flist too short in set_all_fields");
		obj->ob_fval = (struct fvalst *)*flist++;
	}
	else {
		for(p_fval=obj->ob_fval; p_fval!=NULL; p_fval=p_fval->fv_next)
			if ((p_fval->fv_val = *flist++) == terminator)
				panic("flist too short in set_all_fields");
	}
	if (*flist != terminator)
		panic("flist too long in set_all_fields");
	update_display(obj);
}	
		
/*  Set field fnum of object objcode to the value fval.
 */
void
set_field_value(objcode, fnum, fval)
objid_t objcode;
int fnum;
fval_t fval;
{
	struct objst *obj;
	
	obj = code_to_obj(objcode);
	if (obj->ob_flags & OB_ONE_FVAL) {
		if (fnum != 0)
			panic("fnum bad in set_field_value");
		obj->ob_fval = (struct fvalst *)fval;
	}
	else
		get_p_fval(obj, fnum)->fv_val = fval;
	update_display(obj);
}

/*  Like set_field_value, but does not cause an update.
 *  This is provided for the situation where a field's value has
 *  changed, but the display need not be redrawn.
 */
void
update_field_value(objcode, fnum, fval)
objid_t objcode;
int fnum;
fval_t fval;
{
	struct objst *obj;
	
	obj = code_to_obj(objcode);
	if (obj->ob_flags & OB_ONE_FVAL) {
		if (fnum != 0)
			panic("fnum bad in set_field_value");
		obj->ob_fval = (struct fvalst *)fval;
	}
	else
		get_p_fval(obj, fnum)->fv_val = fval;
}

/*  Called by the user when the object needs redisplaying for some
 *  reason unknown to us - e.g. if the field value is a char pointer
 *  and the pointed to string has changed
 */
void
obj_has_changed(code)
objid_t code;
{
	set_update_lptrs(code_to_obj(code)->ob_parent, FALSE);
}

/*  Return the type of object code.
 */
int
get_object_type(code)
objid_t code;
{
	return code_to_obj(code)->ob_type;
}

/*  Return the value of field fnum of object objcode.
 */
fval_t
get_field_value(objcode, fnum)
objid_t objcode;
int fnum;
{
	struct objst *obj;

	obj = code_to_obj(objcode);
	if (obj->ob_flags & OB_ONE_FVAL) {
		if (fnum != 0)
			panic("fnum bad in get_field_value");
		return (fval_t) obj->ob_fval;
	}
	else
		return get_p_fval(obj, fnum)->fv_val;
}

#define ODFLAGS (OB_NL_BEFORE | OB_NL_AFTER | OB_FIXED_SIZE | OB_ONE_FVAL)

/*  Change the type of objcode to newtype. If the new number of fields
 *  is the same as the old, leave the values unchanged, otherwise
 *  get a new list of fields, initilaised to 0.
 */
void
change_type(objcode, newtype)
objid_t objcode;
int newtype;
{
	int new_nfields;
	struct objst *obj;
	
	obj = code_to_obj(objcode);
	new_nfields = Odesc[newtype].od_nfields;
	if (Odesc[obj->ob_type].od_nfields != new_nfields) {
		if ((obj->ob_flags & OB_ONE_FVAL) == 0)
			free_fval_list(obj->ob_fval);
		if (new_nfields > 1)
			obj->ob_fval = new_fval_list(new_nfields);
	}
	obj->ob_type = newtype;
	obj->ob_flags = (obj->ob_flags & ~ODFLAGS) | Odesc[newtype].od_flags;
	set_update_lptrs(obj->ob_parent, FALSE);
}
	
/*  Initialise the objects, by setting up a null tree consisting of
 *  just the root object.
 */
void
init_objects()
{
	Rootobj.ob_type = -1;
	Rootobj.ob_flags = Odesc[-1].od_flags;
	Rootobj.ob_lptr = new_lptr();
	Rootobj.ob_lptr->lp_obj = &Rootobj;
	Rootobj.ob_lptr->lp_next = Rootobj.ob_lptr->lp_prev = NULL;
	Rootobj.ob_lptr->lp_ldepth = 0;
	enter_obj(&Rootobj, (objid_t)0);
	obj_set_default_colors(WN_FG, WN_BG);
}

/*  Return the code of the specified relation of object code.
 */
objid_t
get_code(code, which_rel)
objid_t code;
int which_rel;
{
	struct objst *obj, *res;
	
	obj = code_to_obj(code);
	switch(which_rel) {
	case OBJ_PARENT:
		res = obj->ob_parent;
		break;
	case OBJ_CHILD:
		res = obj->ob_child;
		break;
	case OBJ_LAST_CHILD:
		res = last_child(obj);
		break;
	case OBJ_NEXT:
		res = obj->ob_next;
		break;
	case OBJ_PREV:
		res = obj->ob_prev;
		break;
	default:
		panic("unknown rel");
		res = NULL; /* to satisy gcc */
	}
	/*  user is probably going to refer to the result soon
	 */
	if (res != NULL)
		set_likely(res);
	return (res == NULL) ? NULL : res->ob_code;
}

#ifdef NOTUSED
/*  Return the number number of objects between this object and the
 *  root object.  The level of the first object ever inserted in 0.
 */
int
get_level(code)
objid_t code;
{
	struct objst *obj;
	int level;
	
	level = 0;
	for (obj = code_to_obj(parcode); obj != &Rootobj; obj = obj->ob_parent)
		level++;
	return level;
}
#endif /* NOTUSED */

/*  Return the number of children of object parcode.
 */
int
get_num_children(parcode)
objid_t parcode;
{
	struct objst *obj;
	int n_children;
	
	obj = code_to_obj(parcode);
	n_children = 0;
	for (obj = obj->ob_child; obj != NULL; obj = obj->ob_next)
		n_children++;
	return n_children;
}

static int
objcmp(obj1, obj2)
struct objst *obj1, *obj2;
{
	return (*Sort_cmp_func)(obj1->ob_code, obj2->ob_code);
}

/*  Sort the children of parcode, using the comparison function cmp.
 *
 *  	cmp is called in the style
 *  	(*cmp)(code1,code2)
 *  	objid_t code1, code2;
 *
 *  and should return -1, 0, 1 according as code1 should be considered
 *  less than, equal to, or greater than code2.
 */
void
sort_children(parcode, cmp)
objid_t parcode;
obj_sort_func_t cmp;
{
	int len;
	struct objst *par, *obj, *prev;
	obj_sort_func_t savecmp;
	
	par = code_to_obj(parcode);
	len = get_num_children(parcode);

	savecmp = Sort_cmp_func;
	Sort_cmp_func = cmp;
	par->ob_child = objsort(par->ob_child, len, objcmp);
	Sort_cmp_func = savecmp;

	prev = NULL;
	for (obj = par->ob_child; obj != NULL; obj = obj->ob_next) {
		obj->ob_prev = prev;
		prev = obj;
	}
	set_update_lptrs(par, FALSE);
}

/*  Visit each member of the list of objects starting at code - i.e.,
 *  call (*func)() for each object in the list. Stop if (*func)()
 *  returns non-zero or the end of the list is reached, whichever is
 *  sooner. Return whatever (*func)() returned, or 0 if the end of 
 *  the list is reached, and set *p_code (if non-NULL) to the code
 *  we reached (possibly NULL). The 'next' member of the list is determined
 *  by the rel argument - see the code.
 *
 *  There is one restriction on (*func)() - it must not delete the
 *  object 'next' in the list after the one it is given as an argument.
 *  Apart from this, it can use any of the obj calls (including deleting
 *  the object it is passed).
 *
 *  func is called in the style:
 *
 *  	(*func)(code,arg)
 *  	objid_t code;
 *  	fval_t arg;
 *
 */
int
visit_objects(code, rel, func, arg, p_code)
objid_t code;
int rel;
obj_visit_objects_func_t func;
fval_t arg;
objid_t *p_code;
{
	int res;
	struct objst *obj, *next;
	
	obj = (code == NULL) ? NULL : code_to_obj(code);
	if (rel == OBJ_CHILDREN) {
		obj = (obj != NULL) ? obj->ob_child : NULL;
		rel = OBJ_NEXT;
	}
	res = 0;
	for (; obj != NULL && obj->ob_code != NULL; obj = next) {
		switch(rel) {
		case OBJ_NEXT:
			next = obj->ob_next;
			break;
		case OBJ_PREV:
			next = obj->ob_prev;
			break;
		case OBJ_PARENT:
			next = obj->ob_parent;
			break;
		default:
			panic("bad rel in visit_objects");
			next = 0; /* to satisfy gcc */
		}
		if ((res = (*func)(obj->ob_code, arg)) != 0)
			break;
	}
	if (p_code != NULL)
		*p_code = (obj != NULL) ? obj->ob_code : NULL;
	return (obj != NULL) ? res : 0;
}

bool
obj_child_exists(parcode, code)
objid_t parcode, code;
{
	struct objst *obj;

	for (obj = code_to_obj(parcode)->ob_child; obj != NULL; obj = obj->ob_next)
		if (obj->ob_code == code)
			return TRUE;
	return FALSE;
}
