/* o_lptrs.c - code to keep the list of lptrs up to date */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char obj_lptrs_sccsid[] = "@(#)o_lptrs.c	1.11 26/4/92 (UKC)";

#include <local/wn.h>

#include <local/ukcprog.h>
#include <mtrprog/alloc.h>

#include "obj.h"
#include "o_priv.h"

#include "o_mkobj.h"
#include "o_lptrs.h"
#include "o_sel.h"

static int child_indent PROTO((struct objst *obj));
static struct lptrst *redo_lptrs PROTO((struct objst *obj, struct lptrst *lptr,
							int indent, int no_skip));
static struct lptrst *reclaim_lptrs PROTO((struct objst *obj,
							struct lptrst *prevlptr));
static void set_desc_update_lptrs PROTO((struct objst *par));

/*  For translating between y coordinates and pointers into the object
 *  tree, we keep a list of lptrs. This conceptually runs down
 *  beside the left hand side of the tree.
 *
 *  Objects are arranged in lines of variable depth. Normally objects
 *  are packed as many as possible per line, but the format for an
 *  object may specify that it should start or end a line (or both).
 *  The depth of a line is the maximum depth of the line.
 *
 *  An lptr has pointers to the next and previous list member (lp_next,lp_prev),
 *  a pointer to the first object on this line (lp_obj), the absolute
 *  y position of this line (lp_ypos) and the depth of this line (lp_depth).
 *
 *  If updating is turned off, then lptrs are not kept up to date as
 *  changes are made to objects. When updating is turned on again, all
 *  the updating is done.
 *
 */

ALLOC_NEW_FREELIST(extern,struct lptrst,lptr,lp_next)

/*  These are set by display_from() in disp.c when it is called with
 *  updating off, so that when updating is turned on again, we can
 *  do the requested display.
 */
int New_offs_x = 0, New_offs_y = 0;

/*  Current display position - needed so we can call display_from()
 *  at the current position, to go to New_offs_x, New_offs_y if
 *  necessary.
 */
extern int Offs_x, Offs_y;

/*  Return the indentation in pixels of obj from the left of the
 *  display area.
 */
int
get_indent(obj)
struct objst *obj;
{
	struct objst *par;
	int indent;
	
	indent = 0;
	for(;;) {
		par = obj->ob_parent;
		if (par == NULL)
			break;
		if (!(obj->ob_flags & OB_NO_INDENT))
			indent += Odesc[par->ob_type].od_child_indent;
		obj = par;
	};
	return indent;
}

static int
child_indent(obj)
struct objst *obj;
{
	if (obj->ob_flags & OB_NO_INDENT)
		return 0;
	else
		return Odesc[obj->ob_parent->ob_type].od_child_indent;
}

/*  Redo the lptrs for obj, which must be the first child of a parent.
 *  On entry, lptr points at an empty block at the end of the list so far
 *  built (only the prev field is set). On exit, a pointer to the last
 *  block in the new list is returned.
 */
static struct lptrst *
redo_lptrs(obj, lptr, indent, no_skip)
struct objst *obj;
struct lptrst *lptr;
int indent, no_skip;
{
	int ldepth, first_on_line, xpos, type, i;
	struct objst *prevobj;
	struct lptrst *next_lptr;
	struct szst *sizetab[MAX_OBJTYPES], *size;
	
	for (i = 0; i < MAX_OBJTYPES; i++)
		sizetab[i] = NULL;
	while (obj != NULL) {
		ldepth = 0;
		xpos = indent;
		first_on_line = TRUE;
		prevobj = NULL;
		next_lptr = new_lptr();
		if (lptr != NULL) {
			lptr->lp_next = next_lptr;
			next_lptr->lp_ypos = lptr->lp_ypos + lptr->lp_ldepth;
		}
		else
			next_lptr->lp_ypos = 0;
		next_lptr->lp_prev = lptr;
		lptr = next_lptr;
		lptr->lp_obj = obj;
		for (;;) {
			type = obj->ob_type;
			if (obj->ob_flags & OB_FIXED_SIZE) {
				obj->ob_width = Odesc[type].od_width;
				obj->ob_depth = Odesc[type].od_depth;
			}
			else {
				if (sizetab[type] == NULL) {
					size = (struct szst *)e_malloc(
							sizeof(struct szst));
					size->sz_same = FALSE;
					sizetab[type] = size;
				}
				else
					size = sizetab[type];
				if (!size->sz_same) {
					(*Odesc[type].od_getsize)(obj->ob_code,
						obj->ob_parent->ob_code, size);
				}
				obj->ob_width = size->sz_width;
				obj->ob_depth = size->sz_depth;
			}
			if (obj->ob_flags & OB_NL_BEFORE && !first_on_line)
				break;
			xpos += obj->ob_width;
			obj->ob_flags |= OB_LAST_ON_LINE;
			if (xpos > wn_get_width(Obj_wn) && !first_on_line)
				break;
			if (prevobj != NULL)
				prevobj->ob_flags &= ~OB_LAST_ON_LINE;
			if (obj->ob_depth > ldepth)
				ldepth = obj->ob_depth;
			obj->ob_lptr = lptr;
			prevobj = obj;
			obj = obj->ob_next;
			if (prevobj->ob_flags & OB_NL_AFTER ||
				     obj == NULL || prevobj->ob_child != NULL)
				break;
			first_on_line = FALSE;
		}
		lptr->lp_ldepth = ldepth;
		if (prevobj != NULL) {
			if (prevobj->ob_child != NULL) {
				if ((prevobj->ob_flags & OB_UPDATE_LPTRS) ||
								      no_skip) {
					struct objst *child;

					child = prevobj->ob_child;
					lptr = redo_lptrs(child,
							  lptr,
							  indent +
							  	child_indent(child),
					  		  no_skip);
				}
				else
					lptr= reclaim_lptrs(prevobj->ob_child,
									lptr);
			}
			prevobj->ob_flags &= ~OB_UPDATE_LPTRS;
		}
	}
	return lptr;
}

/*  Called from redo_lptrs() when we encounter an object whose lptrs
 *  are still up to date. This means that all the object's descendents
 *  lptrs are up to date, so we grab lptrs off the free list (see do_lptrs()
 *  below) up to the lptr of the next object after this one.
 *  All the ypos fields of these lptrs are updated by the amount that
 *  this section of the object list has shifted vertically.
 */
static struct lptrst *
reclaim_lptrs(obj, prevlptr)
struct objst *obj;
struct lptrst *prevlptr;
{
	struct lptrst *first, *lptr;
	struct objst *prevobj;
	int offset;
	
	first = obj->ob_lptr;
	offset = prevlptr->lp_ypos + prevlptr->lp_ldepth - first->lp_ypos;

	prevobj = NULL; /* to keep gcc happy */

	/*  Find the last object that is a descendent of this object
	 */
	do {
		for (; obj != NULL; obj = obj->ob_next)
			prevobj = obj;
		obj = prevobj->ob_child;
	} while (obj != NULL);

	/*  update the lp_ypos fields of objects from the free list
	 */
	lptr = first;
	for(;;) {
		lptr->lp_ypos += offset;
		if (prevobj->ob_lptr == lptr)
			break;
		lptr = lptr->lp_next;
	}

	/*  chop the reclaimed lptrs out of the free list and splice them
	 *  into the new list
	 */
	prevlptr->lp_next = first;
	first->lp_prev->lp_next = lptr->lp_next;
	if (lptr->lp_next != NULL)
		lptr->lp_next->lp_prev = first->lp_prev;
	first->lp_prev = prevlptr;
	return lptr;
}

/*  Set by the obj routines if the display has changed. Purely for
 *  the use of users of the obj routines, who are responsible for
 *  clearing it when they think fit.
 */
int Display_has_changed = FALSE;

/*  Internal flag, set when the object tree has changed.
 */
int Obj_tree_changed = FALSE;

/*  Current total depth of the object tree in pixels - i.e. the
 *  sum of all the lp_depth fields.
 */
static int Info_depth;

/*  Flag to say whether updating is on of off. Note that updating
 *  is initially off.
 */
int Updating_state = OBJ_UPDATING_OFF;

/*  Callback function for updating().
 */
obj_updating_callback_func_t Updating_callback_func = NULL;

/*  TRUE if the lptrs are out of date. May be FALSE even when Obj_tree_changed
 *  is TRUE as get_info_depth() below clears it. I.e. if Do_lptrs_pending is
 *  FALSE and Obj_tree_changed is TRUE, the lptrs are up to date, but
 *  we must still to a redisplay.
 */
int Do_lptrs_pending = TRUE;

/*  Return the depth of the object tree in pixels - the value of
 *  Info_depth. We must redo the lptrs if these are out of date
 *  to get the correct depth.
 */
int
get_info_depth()
{
	if (Do_lptrs_pending)
		do_lptrs(FALSE);
	return Info_depth;
}

/*  Update the list of lptrs, using redo_lptrs(). 
 *  dummy is pointed at the list of lptrs to simplify the list
 *  splicing in reclaim_lptrs(). redo_lptrs() takes lptrs that
 *  haven't changed out of the free list, so that only the junk
 *  ones are freed by the free_lptr_list() call.
 *  We update Info_depth and clear Do_lptrs_pending.
 *
 *  The no_skip parameter is set when the window size has changed - 
 *  it means that all the lptrs are out of date.
 */
void
do_lptrs(no_skip)
int no_skip;
{
	register struct lptrst *lptr;
	struct lptrst dummy;
	
	dummy.lp_prev = NULL;
	dummy.lp_next = Rootobj.ob_lptr;
	Rootobj.ob_lptr->lp_prev = &dummy;
	lptr = redo_lptrs(&Rootobj, (struct lptrst *)NULL, 0, no_skip);
	free_lptr_list(dummy.lp_next);
	lptr->lp_next = NULL;
	Info_depth = lptr->lp_ypos + lptr->lp_ldepth;
	Do_lptrs_pending = FALSE;
	if (obj__Change_callback != NULL)
		(*obj__Change_callback)(obj__Change_callback_data,
						OBJ_NEW_HEIGHT, Info_depth);
}

void
obj_set_callback_and_data(callback, data)
obj_change_callback_t callback;
char *data;
{
	obj__Change_callback = callback;
	obj__Change_callback_data = data;
}

/*  Set the lptrs out of date flag for this object. We also
 *  set the bits for all this object's ancestors, as changes
 *  can propagate upwards.
 *  If recurse is set, we set the bit for all of the object's
 *  descendents - this is used e.g. when moving an object (and hence
 *  its descendents to a different position in the tree at a different
 *  indentation.
 *  If updating is turned on we do an immediate update.
 */
void
set_update_lptrs(obj, recurse)
struct objst *obj;
int recurse;
{
	struct objst *par;
	
	for (par = obj; par != NULL && !(par->ob_flags & OB_UPDATE_LPTRS);
							par = par->ob_parent)
		par->ob_flags |= OB_UPDATE_LPTRS;
	if (recurse)
		set_desc_update_lptrs(obj);
	Display_has_changed = Obj_tree_changed = Do_lptrs_pending = TRUE;
	if (Updating_state == OBJ_UPDATING_ON && Obj_wn != 0) {
		do_lptrs(FALSE);
		display_from(Offs_x, Offs_y);
		Obj_tree_changed = Do_lptrs_pending = FALSE;
	}
}

/*  Recursively set the lptrs out of date flag for this object
 *  and all its descendents.
 */
static void
set_desc_update_lptrs(par)
register struct objst *par;
{
	register struct objst *obj;
	
	for (obj = par->ob_child; obj != NULL; obj = obj->ob_next) {
		if (obj->ob_child != NULL)
			set_desc_update_lptrs(obj);
		obj->ob_flags |= OB_UPDATE_LPTRS;
	}
}

/*  Register a function to be called whenever updating is turned on or off.
 */
void
obj_register_updating_callback_func(func)
obj_updating_callback_func_t func;
{
	Updating_callback_func = func;
}

/*  Turn updating on (new_state == OBJ_UPDAING_ON) or off (OBJ_UPDAING_ON).
 *  If new_state is OBJ_UPDATING_QUERY, just return the current state.
 *  For all requests, return the previous updating state.
 *
 *  For OBJ_UPDATING_ON, redo the lptrs if necessary, redisplay
 *  if necessary, and do any pending selection (i.e. do the updates
 *  for objects that were selected while updating was off).
 *
 *  For OBJ_UPDATING_OFF, we just record the current position of the
 *  display in New_offs_{x,y} so we can do a display_from() from the
 *  right place when updating is turned on again. If the user calls
 *  display_from() while updating is off, New_offs_{x,y} will be updated.
 *
 *  If there is a callback registered, we call it if new_state is
 *  OBJ_UPDATING_{ON,OFF}.
 */
int
updating(new_state)
int new_state;
{
	static int public_updating_state = OBJ_UPDATING_OFF;
	int old_state;

	if (new_state == OBJ_UPDATING_QUERY || new_state == Updating_state)
		return public_updating_state;

	old_state = Updating_state;
	Updating_state = new_state;
	
	if (new_state == OBJ_UPDATING_ON) {
		wn_updating_off(Obj_wn);
		if (Obj_tree_changed) {
			if (Do_lptrs_pending)
				do_lptrs(FALSE);
			display_from(New_offs_x, New_offs_y);
			Obj_tree_changed = FALSE;
		}
		do_pending_selection();
		wn_updating_on(Obj_wn);
	}
	else {
		New_offs_x = Offs_x;
		New_offs_y = Offs_y;
	}

	public_updating_state = Updating_state;

	if (Updating_callback_func != NULL)
		(*Updating_callback_func)(old_state, new_state);

	return old_state;
}
