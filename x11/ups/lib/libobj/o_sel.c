/* o_sel.c - routines handling the selection of objects */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char obj_sel_sccsid[] = "@(#)o_sel.c	1.17 26/7/92 (UKC)";

#include <local/wn.h>

#include <local/ukcprog.h>
#include <mtrprog/alloc.h>

#include "obj.h"
#include "o_priv.h"

#include "o_mkobj.h"
#include "o_lptrs.h"
#include "o_sel.h"

struct iselst {
	struct objst *is_obj;
	struct iselst *is_next;
	int is_level;
};

static struct iselst *new_isel PROTO((void));
static void free_isel_list PROTO((struct iselst *isel));
static struct selst *new_sel PROTO((void));
static void free_sel_list PROTO((struct selst *sel));
static void select_field PROTO((int puck_x, int puck_y, struct objst *obj, int obj_x, int obj_y));
static void getobj PROTO((int puck_x, int puck_y, struct objst **p_obj, int *p_x, int *p_y));
static void uselect PROTO((struct objst *obj, int new_state, int flags));
static void sel_obj PROTO((struct objst *obj, int new_state, int flags));
static void get_field_pos PROTO((struct objst *obj, int fnum, int *p_x, int *p_y));
static struct fdescst *fnum_to_fdesc PROTO((struct objst *obj, int fnum));

/*  Stupid Ultrix C compiler can't handle comparison of two pointers
 *  to functions returning void.
 */
#ifdef ultrix
#define PFV_CAST(f)	((int (*)())f)
#else
#define PFV_CAST(f)	f
#endif

extern int Offs_x, Offs_y;

extern int Updating_state;

static struct iselst *Head_isel = NULL;

static int Obj_selection_changed = TRUE;

static int Select_pending = FALSE;

static obj_pre_edit_func_t Pre_edit_func = NULL;

ALLOC_NEW_FREELIST(static,struct iselst,isel,is_next)

ALLOC_NEW_FREELIST(static,struct selst,sel,se_next)

#ifdef DEBUG
void
dump_isel()
{
	struct iselst *isel;
	int flags;
	char buf[256+1];
	
	for (isel = Head_isel; isel != NULL; isel = isel->is_next) {
		getfullname(buf, 256, isel->is_obj->ob_code);
		printf("[%s %x", buf, isel->is_obj);
		flags = isel->is_obj->ob_flags;
		if (!(flags & OB_IN_SEL_LIST))
			panic("IN_SEL_LIST not set");
		if (flags & OB_NL_AFTER)	puts(" NL_A");
		if (flags & OB_NL_BEFORE)	puts(" NL_B");
		if (flags & OB_LAST_ON_LINE)	puts(" LAST_ONL");
		if (flags & OB_HIGHLIGHTED)	puts(" HIGHLIGHTED");
		if (flags & OB_USERSEL_PENDING)	puts(" USEL_PENDING");
		if (flags & OB_UPDATE_LPTRS)	puts(" UPD_LPTRS");
		if (flags & OB_SELECTED) 	puts(" SELECTED");
		if (flags & OB_ONE_FVAL)	puts(" ONE_FVAL");
		if (flags & OB_FIXED_SIZE)	puts(" FIXED_SIZE");
		puts("]\n");
	}
	puts("\n\n");
}
#endif /* DEBUG */

/*  Recursively select an object and its children.
 *	Select the object itself if sel_self is TRUE.
 *  	Select the parent's immediate children if sel_children is TRUE
 *	Select all the objects below the parent if sel_descendents is TRUE.
 */
void
sel_obj_tree(par, sel_self, sel_children, sel_descendents, new_state)
struct objst *par;
int sel_self, sel_children, sel_descendents, new_state;
{
	struct objst *obj;
	
	if (sel_children)
		for (obj = par->ob_child; obj != NULL; obj = obj->ob_next)
			sel_obj_tree(obj, TRUE, sel_descendents, TRUE, new_state);
	if (sel_self)
		sel_obj(par, new_state, SEL_SELECTING);
}

/*  edit function for non-editable fields.
 *  swallow the input record and return
 */
/* ARGSUSED */
void
no_edit(fdets)
struct drawst fdets;
{
	wn_wait_for_release_of(fdets.dr_wn, B_ANY);
}	

/*  Return a pointer to field fnum of object obj.
 *  Does not work on objects with the OB_ONE_FVAL bit set.
 *  Internal use only, but used by iget_field_value() in sel.c so
 *  not static.
 */
struct fvalst *
get_p_fval(obj, fnum)
struct objst *obj;
int fnum;
{
	struct fvalst *p_fval;
	
	for (p_fval = obj->ob_fval; fnum > 0; --fnum) {
		p_fval = p_fval->fv_next;
		if (p_fval == NULL)
			panic("fnum too high in get_p_fval");
	}
	return p_fval;
}

/*  Return the value of field fnum of object obj.
 *  Used only in select_field below.
 */
fval_t
iget_field_value(obj, fnum)
struct objst *obj;
int fnum;
{
	if (obj->ob_flags & OB_ONE_FVAL) {
		if (fnum != 0)
			panic("fnum bad in iget_field_value");
		return (fval_t) obj->ob_fval;
	}
	else
		return get_p_fval(obj, fnum)->fv_val;
}

obj_pre_edit_func_t
obj_set_pre_edit_func(func)
obj_pre_edit_func_t func;
{
	obj_pre_edit_func_t last;

	last = Pre_edit_func;
	Pre_edit_func = func;
	return last;
}

/*  Called when B_MIDDLE has been pressed over an object. Works out
 *  which field (if any) of the object has been selected, and calls
 *  the select routine for that object.
 *  If B_MIDDLE has been pressed to the right of all the fields, or
 *  over a non editable field, and there is an editable field to the
 *  left of where it has been pressed, select that field.
 *
 *  puck_x, puck_y is the Obj_wn relative location of the button press.
 *  obj is the object that has been selected.
 *  obj_x, obj_y is the Obj_wn relative location of the top left corner
 *  of the object.
 */
static void
select_field(puck_x, puck_y, obj, obj_x, obj_y)
int puck_x, puck_y;
struct objst *obj;
int obj_x, obj_y;
{
	struct fldlnst *fldln;
	register struct fdescst *fdesc, *prev_fdesc;
	struct fvalst *fval, fval_buf;
	struct drawst fdets;
	int xpos, ypos, fnum, old_xpos, fwidth;
	
	/* BUG: I am not sure if this is necessary.  Gcc complains about
	 *      "fwidth used initialised" if the assignment is left out.
	 */
	fwidth = 0;

	fldln = Odesc[obj->ob_type].od_fldln;
	ypos = obj_y;
	
	/*  search down the lines of fields of this object
	 */
	for (;;) {
		ypos += fldln->fl_ldepth;
		if (ypos > puck_y)
			break;
		fldln = fldln->fl_next;
	}
	fdesc = fldln->fl_fdesc;
	xpos = obj_x;
	
	/*  found the correct line - now search along this line for the
	 *  right field.
	 */
	prev_fdesc = NULL;
	for (;;) {
		old_xpos = xpos;
		if (fdesc == NULL)
			break;
		if (fdesc->fd_getwidth != NULL)
			fwidth = (*fdesc->fd_getwidth)(obj->ob_code,
					fdesc->fd_fnum,
					iget_field_value(obj, fdesc->fd_fnum));
		else
			fwidth = fdesc->fd_width;
		xpos += fwidth;
		if (xpos > puck_x)
			break;
		prev_fdesc = fdesc;
		fdesc = fdesc->fd_next;
	}
	
	/*  If the user selected a non editable field to the right of
	 *  an editable field, use the editable field
	 */
	if (fdesc == NULL || 
		    (PFV_CAST(fdesc->fd_edit) == PFV_CAST(no_edit) &&
		     prev_fdesc != NULL &&
		     PFV_CAST(prev_fdesc->fd_edit) != PFV_CAST(no_edit))) {
		fdesc = prev_fdesc;
		xpos = old_xpos;
		if (fdesc->fd_getwidth != NULL)
			fwidth = (*fdesc->fd_getwidth)(obj->ob_code,
					fdesc->fd_fnum,
					iget_field_value(obj, fdesc->fd_fnum));
		else
			fwidth = fdesc->fd_width;
		if (fdesc == NULL) {
			wn_wait_for_release_of(Obj_wn, B_ANY);
			return;
		}
	}
	
	/*  Now we have the correct fdescst structure for the object type,
	 *  use this to look up the fval for this particular object.
	 */
	if (fdesc->fd_fnum == -1) {
		fval = NULL;
		fnum = -1;
	}
	else {
		if (obj->ob_flags & OB_ONE_FVAL) {
			if (fdesc->fd_fnum != 0)
				panic("bad fd_fnum in select_field");
			fval_buf.fv_val = (fval_t) obj->ob_fval;
			fval = &fval_buf;
			fnum = 0;
		}
		else {
			fval = obj->ob_fval;
			for (fnum = 0; fnum < fdesc->fd_fnum; ++fnum)
				fval = fval->fv_next;
		}
	}
	
	/*  Build an fdets structure to pass to the (*fd_edit)() routine.
	 *  Includes a window whose origin in the window and whose size
	 *  is the same as that of the field to be edited.
	 */
	fdets.dr_wn = wn_create_subwin(Obj_wn, xpos - fwidth,
					       ypos - fldln->fl_ldepth,
					       fwidth, fldln->fl_ldepth,
					       WN_INPUT_OUTPUT);
	fdets.dr_fnum = fnum;
	fdets.dr_code = obj->ob_code;
	if (Odesc[obj->ob_type].od_get_color != NULL) {
		(*Odesc[obj->ob_type].od_get_color)(fdets.dr_code,
						    &fdets.dr_fg, &fdets.dr_bg);
	}
	else {
		fdets.dr_fg = _ob_Default_fg_pixel;
		fdets.dr_bg = _ob_Default_bg_pixel;
	}
	fdets.dr_fval = (fval != NULL) ? fval->fv_val : NULL;
	fdets.dr_user_info = fdesc->fd_user_info;

	if (Pre_edit_func != NULL)
		(*Pre_edit_func)(&fdets);

	(*fdesc->fd_edit)(fdets);

	wn_close_window(fdets.dr_wn);

	/*  The edit may have changed the size of a variable size
	 *  object, so get the size of the object again.
	 *
	 *  BUG: what if the height has changed or there are other
	 *       objects on the same line as this.
	 */
	if (Odesc[obj->ob_type].od_getsize != NULL) {
		sz_t size;

		(*Odesc[obj->ob_type].od_getsize)(obj->ob_code,
						obj->ob_parent->ob_code, &size);
		obj->ob_width = size.sz_width;
		obj->ob_depth = size.sz_depth;
	}
}

/*  Set *p_x and *p_y to the offset of the top left pixel of field number
 *  fnum from the top left pixel of object obj.
 */
static void
get_field_pos(obj, fnum, p_xpos, p_ypos)
struct objst *obj;
int fnum;
int *p_xpos, *p_ypos;
{
	struct fldlnst *fl, *prev;
	struct fdescst *fd;
	int xpos, ypos;

	fl = Odesc[obj->ob_type].od_fldln;
	
	/*  search down the lines of fields of this object
	 */
	ypos = 0;
	prev = NULL; /* to satisfy gcc */
	for (;;) {
		if (fl == NULL || fl->fl_fdesc->fd_fnum > fnum)
			break;
		ypos += fl->fl_ldepth;
		prev = fl;
		fl = fl->fl_next;
	}
	ypos -= prev->fl_ldepth;

	xpos = 0;
	for (fd = prev->fl_fdesc; fd->fd_fnum != fnum; fd = fd->fd_next) {
		if (fd == NULL)
			panic("fnum too high in gfp");
		if (fd->fd_getwidth != NULL)
			xpos += (*fd->fd_getwidth)(obj->ob_code,
					fd->fd_fnum,
					iget_field_value(obj, fd->fd_fnum));
		else
			xpos += fd->fd_width;
	}

	*p_xpos = xpos;
	*p_ypos = ypos;
}

static struct fdescst *
fnum_to_fdesc(obj, fnum)
struct objst *obj;
int fnum;
{
	struct fldlnst *fl;
	struct fdescst *fd;

	for (fl = Odesc[obj->ob_type].od_fldln; fl != NULL; fl = fl->fl_next) {
		for (fd = fl->fl_fdesc; fd != NULL; fd = fd->fd_next)
			if (fd->fd_fnum == fnum)
				return fd;
	}
	
	return NULL;
}

void
obj_edit_field(objcode, fnum, x_offset, y_offset)
objid_t objcode;
int fnum, x_offset, y_offset;
{
	struct objst *obj;
	
	obj = code_to_obj(objcode);

	if (Obj_wn == -1) {
		struct drawst fdets;
		struct fdescst *fdesc;
		
		if ((fdesc = fnum_to_fdesc(obj, fnum)) == NULL)
			panic("bad fnum in obj_edit_field");

		fdets.dr_wn = -1;
		fdets.dr_fnum = fnum;
		fdets.dr_code = obj->ob_code;
		fdets.dr_fg = -1;
		fdets.dr_bg = -1;
		fdets.dr_fval = get_field_value(objcode, fnum);
		fdets.dr_user_info = fdesc->fd_user_info;

		if (Pre_edit_func != NULL)
			(*Pre_edit_func)(&fdets);

		(*fdesc->fd_edit)(fdets);
	}
	else {
		int obj_x, field_x, x;
		int obj_y, field_y, y;

		obj_pos(obj, &obj_x, &obj_y);
		obj_x -= Offs_x;
		obj_y -= Offs_y;

		get_field_pos(obj, fnum, &field_x, &field_y);

		x = obj_x + field_x + x_offset;
		y = obj_y + field_y + y_offset;

		select_field(x, y, obj, obj_x, obj_y);
	}
}

/*  Set *p_x and *p_y to the Obj_wn relative coordinates of stop_obj.
 *  Called by get_position() and by uselect().
 */
void
obj_pos(stop_obj, p_x, p_y)
struct objst *stop_obj;
int *p_x, *p_y;
{
	struct lptrst *lptr;
	register struct objst *obj;
	int xpos;
	
	lptr = stop_obj->ob_lptr;
	*p_y = lptr->lp_ypos;
	obj = lptr->lp_obj;
	xpos = get_indent(obj);
	for (; obj != stop_obj; obj = obj->ob_next)
		xpos += obj->ob_width;
	*p_x = xpos;
}
		
/*  Set *p_obj the object under the pixel at puck_x,puck_y relative to
 *  Obj_wn. Set p_x,p_y to the origin of this object.
 *
 *  Scans down the lptr list to find the right line, then along the objects
 *  on that line to find the right object.
 *
 *  Return NULL if the object is not completely on screen in the y direction.
 */
static void
getobj(puck_x, puck_y, p_obj, p_x, p_y)
int puck_x, puck_y;
struct objst **p_obj;
int *p_x, *p_y;
{
	int xpos, ypos;
	struct lptrst *lptr;
	struct objst *obj;
	
	ypos = -Offs_y;
	for (lptr=Rootobj.ob_lptr->lp_next; lptr!=NULL; lptr = lptr->lp_next){
		ypos += lptr->lp_ldepth;
		if (ypos > puck_y)
			break;
	}
	if (lptr == NULL) {
		*p_obj = NULL;
		return;
	}
	obj = lptr->lp_obj;
	xpos = get_indent(obj) - Offs_x;
	if (xpos > puck_x) {
		*p_obj = NULL;
		return;
	}
	for (;;) {
		xpos += obj->ob_width;
		if (xpos > puck_x)
			break;
		if (obj->ob_flags & OB_LAST_ON_LINE) {
			*p_obj = NULL;
			return;
		}
		obj = obj->ob_next;
	}
	if (lptr->lp_ypos < Offs_y ||
	    lptr->lp_ypos + obj->ob_depth >= Offs_y + wn_get_height(Obj_wn))
		*p_obj = NULL;
	else {
		*p_obj = obj;
		*p_x = xpos - obj->ob_width;
		*p_y = ypos - lptr->lp_ldepth;
	}
}

static void
sel_obj(obj, new_state, flags)
register struct objst *obj;
int new_state, flags;
{
	int old_state;
	struct iselst *isel;
	
	/* If the object can't be selected, no action necessary
	 */
	if (!(*Odesc[obj->ob_type].od_can_select)(obj->ob_code))
		return;
	
	old_state = (obj->ob_flags & OB_SELECTED) ? SEL_ON : SEL_OFF;
	if (new_state == SEL_FLIP)
		new_state = (old_state == SEL_ON) ? SEL_OFF : SEL_ON;
		
	/*  If the object's selection state is already what was wanted,
	 *  no action is necessary.
	 */
	if (old_state == new_state)
		return;
	
	/*  We are going to have to select or deselect the object.
	 *  If it is not already in the selected list, put it there.
	 */
	if ((obj->ob_flags & OB_IN_SEL_LIST) == 0) {
		isel = new_isel();
		isel->is_obj = obj;
		isel->is_next = Head_isel;
		Head_isel = isel;
		obj->ob_flags |= OB_IN_SEL_LIST;
	}
	
	/*  We now flip the OB_SELECTED bit, as the real state of
	 *  selection of the object has changed.
	 */
	obj->ob_flags ^= OB_SELECTED;

	/*  The od_select() function we are about to call is allowed to
	 *  call get_num_selected(), so make sure that get_num_selected()
	 *  realises that it need to recount the number of selected objects.
	 */
	Obj_selection_changed = TRUE;

	/*  We guarantee that the user will get SEL_CHANGING type selections
	 *  in the order that they happen, so we call the select routine at
	 *  this point if we're not just about to call it anyway.
	 *
	 *  As updating may be off, don't set SEL_VISIBLE, and don't bother
	 *  with meaningful values for the x, y, width, height values.
	 */
	if ((obj->ob_flags & OB_USERSEL_PENDING) ||
						Updating_state != OBJ_UPDATING_ON) {
		(*Odesc[obj->ob_type].od_select)(Obj_wn, obj->ob_code, 0,0, 0,0,
					 new_state | flags | SEL_CHANGING |
					 (old_state << BITPOS_SEL_WAS_ON));
	}
	
	/*  If the USERSEL_PENDING bit is set, all we have to do is
	 *  turn the bit off. We don't need to call the user's select
	 *  routine as the final state of the object from his point of
	 *  view would be unchanged.
	 */
	if (obj->ob_flags & OB_USERSEL_PENDING)
		obj->ob_flags &= ~OB_USERSEL_PENDING;
	else {
		/*  If updating is on, we have to select the object.
		 *  Otherwise, we just set the select pending bit.
		 */
		if (Updating_state == OBJ_UPDATING_ON)
			uselect(obj, new_state, flags | SEL_CHANGING);
		else {
			obj->ob_flags |= OB_USERSEL_PENDING;
			Select_pending = TRUE;
		}
	}
}

#define MAX_LEVEL 30000 /* must be > greatest possible */

static int Num_selected;

/*  Select the object obj:
 *	Get the Obj_wn relative coordinates of the object
 *	Set the SEL_VISIBLE bit in flags accordingly
 *	Set the SEL_ON bit in flags if we are turning the object on.
 *	Call the select routine for this object type.
 */
static void
uselect(obj, new_state, flags)
struct objst *obj;
int new_state, flags;
{
	int scr_x, scr_y, on_screen, old_state;
	struct odescst *p_odesc;
	
	p_odesc = Odesc + obj->ob_type;

	old_state = (obj->ob_flags & OB_HIGHLIGHTED) ? SEL_ON : SEL_OFF;
	if (new_state == SEL_FLIP)
		new_state = (old_state == SEL_ON) ? SEL_OFF : SEL_ON;

	if (old_state != new_state) {
		/* change of user highlighted state - flip the highlighted bit
		 */
		obj->ob_flags ^= OB_HIGHLIGHTED;
	}

	obj_pos(obj, &scr_x, &scr_y);
	scr_x -= Offs_x;
	scr_y -= Offs_y;
	on_screen = scr_x + obj->ob_width >= 0 && 
				    scr_x < wn_get_width(Obj_wn) &&
				    scr_y >= 0 &&
				    scr_y+obj->ob_depth< wn_get_height(Obj_wn);
	(*p_odesc->od_select)(Obj_wn, obj->ob_code, scr_x, scr_y,
					obj->ob_width, obj->ob_depth,
					flags | new_state |
					(on_screen << BITPOS_SEL_VISIBLE) |
					(old_state << BITPOS_SEL_WAS_ON)
			     );
}

/*  Do pending selections. These are selections done when updating
 *  was turned off.
 */
void
do_pending_selection()
{
	register struct iselst *isel;
	
	if (Updating_state == OBJ_UPDATING_OFF)
		panic("do_pending_selection called with updating off");
	if (Select_pending) {
		for (isel = Head_isel; isel != NULL; isel = isel->is_next) {
			if (isel->is_obj->ob_flags & OB_USERSEL_PENDING) {
				uselect(isel->is_obj, SEL_FLIP, SEL_UPDATING);
				isel->is_obj->ob_flags &= ~OB_USERSEL_PENDING;
			}
		}
		Select_pending = FALSE;
	}
}

/*  Return the code of the the nearest common ancestor of the
 *  objects in the selected list
 */
objid_t
get_ancestor_of_selection()
{
	register struct objst *cur_obj, *anc_obj, *obj;
	int cur_level, anc_level, ancestor_not_yet_set;
	register struct iselst *isel;

	ancestor_not_yet_set = TRUE;
	anc_obj = NULL;
	anc_level = 0; /* to keep gcc happy */
	for (isel = Head_isel; isel != NULL; isel = isel->is_next) {
		cur_obj = isel->is_obj;
		if ((cur_obj->ob_flags & OB_SELECTED) == 0)
			continue;
		cur_level = 0;
		for (obj = cur_obj; obj != NULL; obj = obj->ob_parent)
			cur_level++;
		if (ancestor_not_yet_set) {
			anc_obj = cur_obj->ob_parent;
			anc_level = cur_level - 1;
			ancestor_not_yet_set = FALSE;
		}
		for ( ; cur_level > anc_level; --cur_level)
			cur_obj = cur_obj->ob_parent;
		for ( ; anc_level > cur_level; --anc_level)
			anc_obj = anc_obj->ob_parent;
		while (anc_obj != cur_obj) {
			anc_obj = anc_obj->ob_parent;
			cur_obj = cur_obj->ob_parent;
			--anc_level;
		}
	}
	return (anc_obj != NULL) ? anc_obj->ob_code : NULL;
}

/*  Return the number of selected objects.  
 *
 *  It is legal to call this from an od_can_select() or od_select() callback
 *  function.  If called from an od_select() callback, the returned count
 *  is the number of objects that will be selected after the callback
 *  has returned.
 */
int
get_num_selected()
{
	register struct iselst *isel;
	int count;

	if (Obj_selection_changed) {
		count = 0;
		for (isel = Head_isel; isel != NULL; isel = isel->is_next)
			if (isel->is_obj->ob_flags & OB_SELECTED)
				++count;
		Num_selected = count;
	}
	return Num_selected;
}

/*  Return the selection, but in reverse of normal order. This is a
 *  temporary hack - what we should really do is have the user specify
 *  a comparison function for sorting the selection.
 */
struct selst *
get_reverse_selection()
{
	struct selst *sel, *next, *newlist;
	
	newlist = NULL;
	for (sel = get_selection(); sel != NULL; sel = next) {
		next = sel->se_next;
		sel->se_next = newlist;
		newlist = sel;
	}
	Obj_selection_changed = TRUE;
	return newlist;
}

struct selst *
get_unordered_selection()
{
	static struct selst *head_sel = NULL;
	register struct iselst *isel, *next;
	struct iselst *newlist, *freelist;
	register struct selst *sel;
	register int selected;

	if (head_sel != NULL) {
		free_sel_list(head_sel);
		head_sel = NULL;
	}

	newlist = freelist = NULL;
	Num_selected = 0;
	for (isel = Head_isel; isel != NULL; isel = next) {

		selected = isel->is_obj->ob_flags & OB_SELECTED;
		if (selected) {
			sel = new_sel();
			sel->se_code = isel->is_obj->ob_code;
			sel->se_next = head_sel;
			sel->se_user = -1;
			head_sel = sel;
			++Num_selected;
		}

		next = isel->is_next;

		if (selected || (isel->is_obj->ob_flags & OB_USERSEL_PENDING)) {
			isel->is_next = newlist;
			newlist = isel;
		}
		else {
			isel->is_obj->ob_flags &= ~OB_IN_SEL_LIST;
			isel->is_next = freelist;
			freelist = isel;
		}
	}
	Head_isel = newlist;
	free_isel_list(freelist);
	return head_sel;
}

struct selst *
get_selection()
{
	static struct selst *head_sel;
	int min_level, level, next_level;
	struct selst *sel;
	struct iselst *isel;
	struct objst *obj;
	
	if (!Obj_selection_changed)
		return head_sel;
	Num_selected = 0;
	if (head_sel != NULL) {
		free_sel_list(head_sel);
		head_sel = NULL;
	}
	min_level = MAX_LEVEL;
	for (isel = Head_isel; isel != NULL; isel = isel->is_next) {
		obj = isel->is_obj;
		if (obj->ob_flags & OB_SELECTED) {
			for (level = 0; obj != NULL; obj = obj->ob_parent)
				level++;
			isel->is_level = level;
			if (level < min_level)
				min_level = level;
		}
		else
			isel->is_level = MAX_LEVEL;
	}
	for (level = min_level; level < MAX_LEVEL; level = next_level) {
		next_level = MAX_LEVEL;
		for (isel = Head_isel; isel != NULL; isel = isel->is_next) {
			if (isel->is_level == level) {
				sel = new_sel();
				sel->se_code = isel->is_obj->ob_code;
				sel->se_next = head_sel;
				sel->se_user = isel->is_level;
				head_sel = sel;
				isel->is_level = MAX_LEVEL;
				Num_selected++;
			}
			else if (isel->is_level < next_level)
				next_level = isel->is_level;
		}
	}
	Obj_selection_changed = FALSE;
	return head_sel;
}

void
deselect(obj)
struct objst *obj;
{
	struct iselst *isel, *prev_isel;
	
	Obj_selection_changed = TRUE;
	prev_isel = NULL;
	for (isel = Head_isel; ; isel = isel->is_next) {
		if (isel->is_obj == obj) {
			if (obj->ob_flags & OB_SELECTED)
				sel_obj(obj, SEL_OFF, SEL_DELETING);
			if (prev_isel != NULL)
				prev_isel->is_next = isel->is_next;
			else
				Head_isel = isel->is_next;
			isel->is_obj->ob_flags &= ~(OB_HIGHLIGHTED |
					OB_USERSEL_PENDING | OB_IN_SEL_LIST);
			return;
		}
		prev_isel = isel;
	}
}

void
clear_selection()
{
	register struct iselst *isel, *freehead, *newhead, *next;
	
	if (Head_isel == NULL)
		return;
	freehead = newhead = NULL;
	for (isel = Head_isel; isel != NULL; isel = next) {
		next = isel->is_next;
		if (isel->is_obj->ob_flags & OB_SELECTED)
			sel_obj(isel->is_obj, SEL_OFF, SEL_CLEARING);
		if (isel->is_obj->ob_flags & OB_USERSEL_PENDING) {
			isel->is_next = newhead;
			newhead = isel;
		}
		else {
			isel->is_next = freehead;
			freehead = isel;
			isel->is_obj->ob_flags &= ~OB_IN_SEL_LIST;
		}
	}
	Head_isel = newhead;
	if (freehead != NULL)
		free_isel_list(freehead);
}

objid_t
get_object_at(puck_x, puck_y)
int puck_x, puck_y;
{
	struct objst *obj;
	int junk;
	
	getobj(puck_x, puck_y, &obj, &junk, &junk);
	return (obj != NULL) ? obj->ob_code : NULL;
}

void
select_list()
{
	int puck_x, puck_y, obj_x, obj_y, xpos, forwards;
	int orig_x, orig_y, orig_ldepth, below, button, old_inmode;
	int objwn_height;
	struct objst *obj, *newobj;
	struct lptrst *lptr, *newlptr;
	
	obj = NULL;
	button = wn_getpuck(Obj_wn, &puck_x, &puck_y) & B_ANY;
	if (!(button & B_ANY))
		return;
	if (button & B_LEFT) {
		wn_updating_off(Obj_wn);
		clear_selection();
		wn_updating_on(Obj_wn);
	}
	getobj(puck_x, puck_y, &obj, &obj_x, &obj_y);
	if (obj == NULL)
		return;

	/*  Middle button means select for editing.
	 */
	if (button & B_MIDDLE) {
		wn_ungetpuck(Obj_wn, puck_x, puck_y, B_MIDDLE);
		select_field(puck_x, puck_y, obj, obj_x, obj_y);
		return;
	}

	lptr = obj->ob_lptr;
	sel_obj(obj, SEL_FLIP, SEL_PUCKSELECT);
	orig_x = obj_x;
	orig_y = obj_y;
	orig_ldepth = lptr->lp_ldepth;
	objwn_height = wn_get_height(Obj_wn);
	old_inmode = wn_inmode(Obj_wn, WN_SAMPLE);
	while (wn_getpuck(Obj_wn, &puck_x, &puck_y) & button) {
		forwards = FALSE;
		newlptr = lptr;
		if (puck_y < obj_y) {
			if (lptr->lp_prev != Rootobj.ob_lptr)
				newlptr = lptr->lp_prev;
			else
				puck_x = -Offs_x;
		}
		else if (puck_y > obj_y + lptr->lp_ldepth) {
			forwards = TRUE;
			if (lptr->lp_next != NULL)
				newlptr = lptr->lp_next;
			else
				puck_x = wn_get_width(Obj_wn) - Offs_x;
		}
		newobj = newlptr->lp_obj;
		xpos = get_indent(newobj) - Offs_x;
		while( !(newobj->ob_flags & OB_LAST_ON_LINE) ) {
			xpos += newobj->ob_width;
			if (xpos > puck_x)
				break;
			if (newobj == obj)
				forwards = TRUE;
			newobj = newobj->ob_next;
		}
		if (obj == newobj || newlptr->lp_ypos < Offs_y ||
		    newlptr->lp_ypos + newobj->ob_depth >= Offs_y + objwn_height)
			continue;
		/* at this point, newobj points at the destination */
		wn_updating_off(Obj_wn);
		do {
			if (obj_y < orig_y)
				below = FALSE;
			else if (obj_y >= orig_y + orig_ldepth)
				below = TRUE;
			else
				below = obj_x >= orig_x;
			if(below!=forwards&&(obj_x!=orig_x || obj_y!=orig_y))
				sel_obj(obj, SEL_FLIP, SEL_PUCKSELECT);
			if (forwards) {
				if (obj->ob_flags & OB_LAST_ON_LINE) {
					obj_y += lptr->lp_ldepth;
					lptr = lptr->lp_next;
					obj = lptr->lp_obj;
					obj_x = get_indent(obj) - Offs_x;
				} 
				else {
					obj_x += obj->ob_width;
					obj = obj->ob_next;
				}
			}
			else {
				if (lptr->lp_obj == obj) {
					lptr = lptr->lp_prev;
					obj_y -= lptr->lp_ldepth;
					obj = lptr->lp_obj;
					obj_x = get_indent(obj) - Offs_x;
					while (!(obj->ob_flags&OB_LAST_ON_LINE)){
						obj_x += obj->ob_width;
						obj = obj->ob_next;
					
					}
						
				} 
				else {
					obj = obj->ob_prev;
					obj_x -= obj->ob_width;
				}
			}
			if (obj_y < orig_y)
				below = FALSE;
			else if (obj_y >= orig_y + orig_ldepth)
				below = TRUE;
			else
				below = obj_x >= orig_x;
			/* flip whether object is set or not */
			if(below==forwards&&(obj_x!=orig_x || obj_y!=orig_y))
				sel_obj(obj, SEL_FLIP, SEL_PUCKSELECT);
		} while (obj != newobj);
		wn_updating_on(Obj_wn);
	}
	(void) wn_inmode(Obj_wn, old_inmode);
}

/* ARGSUSED */
int
def_can_select(code)
objid_t code;
{
	return TRUE;
}
		
/* ARGSUSED */
int
cannot_select(code)
objid_t code;
{
	return FALSE;
}
