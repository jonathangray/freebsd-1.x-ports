/* o_disp.c - routines for displaying the list of objects */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char obj_disp_sccsid[] = "@(#)o_disp.c	1.13 26/4/92 (UKC)";

#include <local/wn.h>

#include <local/ukcprog.h>

#include "obj.h"
#include "o_priv.h"

#include "o_sel.h"
#include "o_disp.h"
#include "o_lptrs.h"
#include "o_mkobj.h"

static void dr_objline PROTO((struct lptrst *lptr, int scr_x, int scr_y));

static struct lptrst *Top_lptr = NULL;

/*  Offset in pixels of the start of the displayed information
 *  from the start of whole object tree.
 *  Not static because they are also used in sel.c, o_chds.c and lptrs.c
 */
int Offs_x, Offs_y;	/* also used in sel.c, o_chds.c and lptrs.c */

/*  See display_from() below
 */
extern int New_offs_x, New_offs_y;

/*  Flag to say whther we are updating or not - see obj_updating
 */
extern int Updating_state;

/*  Set if some change is made to the display while updating is off
 */
extern int Do_lptrs_pending;

/*  Scroll vertically npixels. npixels < 0 means scroll down.
 *  Return the number of pixels scrolled - will be less than asked for
 *  if we hit the end or start of the file.
 *
 *  Complicated by the need to avoid partially displayed lines.
 *  Must avoid these because (a) they are difficult to do without
 *  clipping which wn.c doesn't provide, (b) because it would make
 *  single pixel scrolling a lot slower - each line would have to be
 *  displayed many times, and (c) because redisplaying lines would
 *  break things like highlighting by inverting.
 *  At the moment, if a whole object will not fit, it is not displayed
 *  at all. This is ugly for multiline objects - some time we must
 *  improve this to  decide whether to display at the granularity of
 *  fields rather than objects.
 *
 *  Mechanism is the same for scrolling up and down. We find three
 *  lptrs, start, xover, and lim. We start displaying from start,
 *  flip state at xover, and stop at the lptr before lim.
 *  The initial state is displaying for downwards scrolling, not
 *  displaying for upwards scrolling.
 *
 *  We call wn_scroll to scroll the part of the display that hasn't
 *  changed and draw any newly appeared lines with dr_objline.
 */
int
v_scroll(npixels)
int npixels;
{
	register struct lptrst *lptr, *start, *xover, *lim, *prev;
	int offset, new_offs_y, forwards, wndepth;
	int rsrc, rdst, rdepth, drawing, xover_ypos, ypos, ldepth;
	
	forwards = npixels > 0;
	wndepth = wn_get_height(Obj_wn);
	lptr = Top_lptr;
	new_offs_y = Offs_y + npixels;
	
	/* check that it is a scroll rather than a goto
	 */
	if ((forwards ? npixels : -npixels) > wndepth) {
		ypos = Offs_y;
		display_from(Offs_x, new_offs_y);
		return Offs_y - ypos;
	}
	
	start = xover = lim = NULL;
	if (forwards)
		offset = wndepth;
	else {
		/* back up to find the new top
		 */
		offset = 0;
		while (lptr != NULL && lptr->lp_ypos > new_offs_y)
			lptr = lptr->lp_prev;
		if (lptr == NULL) {
			/*  fell off start of file - reduce scroll amount
			 */
			lptr = Rootobj.ob_lptr;
			npixels = -Offs_y;
			new_offs_y = 0;
		}
		xover = Top_lptr;
	}
	ypos = lptr->lp_ypos;
	
	/*  find start, xover and lim based on Offs_y, ypos,
	 *  wndepth and new_offs_y. Maintain prev in case we fall off
	 *  the end of the list.
	 */
	prev = NULL;
	for (; lptr != NULL && lim == NULL; lptr = lptr->lp_next) {
		prev = lptr;
		ldepth = lptr->lp_ldepth;
		if (start == NULL && ypos >= new_offs_y)
			start = lptr;
		if (xover == NULL && ypos + ldepth >= Offs_y + offset)
			xover = lptr;
		if (lim == NULL && ypos + ldepth >= new_offs_y + wndepth)
			lim = lptr;
		ypos += ldepth;
	}
	xover_ypos = (xover != NULL) ? xover->lp_ypos : ypos;
	if (start == NULL) {
		/*  fell off the start of the file - so reduce scroll amount
		 *  so we don't scroll past this point
		 */
		start = prev;
		npixels = start->lp_ypos - Offs_y;
		new_offs_y = start->lp_ypos;
	}
	if (npixels == 0) /* don't bother */
		return 0;
	
	/*  calculate the rasterop source y, dest y, and depth.
	 */
	rsrc = (forwards ? start->lp_ypos : xover_ypos) - Offs_y;
	rdst = rsrc - npixels;
	if (forwards)
		rdepth = xover_ypos - start->lp_ypos;
	else
		rdepth = ((lim != NULL) ? lim->lp_ypos : ypos) - xover_ypos;
	wn_rop(Obj_wn, 0, rsrc, wn_get_width(Obj_wn), rdepth, 0, rdst);

	/*  If the rasterop was damaged, give up at this point and repaint.
	 *  This happens under X when the source of the rasterop is
	 *  partially or completely obscured by another window.
	 */
	if (wn_last_rop_was_damaged(Obj_wn)) {
		display_from(Offs_x, new_offs_y);
		return npixels;
	}
	
	/*  blank out the areas above and below the bit scrolled
	 */
	wn_set_area(Obj_wn, 0, 0, wn_get_width(Obj_wn), rdst, _ob_Default_bg_pixel);
	wn_set_area(Obj_wn, 0, rdst+rdepth, wn_get_width(Obj_wn),
						wndepth-(rdst+rdepth),
						_ob_Default_bg_pixel);
	
	/* set initial state depending on direction of scroll
	 */
	drawing = !forwards;
	
	/*  Record the new offset
	 */
	Offs_y = new_offs_y;
	
	/*  Loop to draw any newly appeared lines.
	 */
	for (lptr = start; lptr != lim; lptr = lptr->lp_next) {
		if (lptr == xover)
			drawing = !drawing;
		if (drawing)
			dr_objline(lptr, -Offs_x, lptr->lp_ypos-new_offs_y);
	}
#ifdef DISPLAY_PART
	/* code to try and display fields of partially displayed
	 * objects - doesn't work
	 */
	if ((lptr = start->lp_prev) != NULL &&
				lptr->lp_ypos + lptr->lp_ldepth > Offs_y)
		dr_objline(lptr, -Offs_x, lptr->lp_ypos - new_offs_y);
	if (lim != NULL && lim->lp_ypos < Offs_y + wndepth)
		dr_objline(lim, -Offs_x, lim->lp_ypos-new_offs_y);
#endif /* DISPLAY_PART */
	
	/*  Record the new top line of the screen
	 */
	Top_lptr = start;

	if (obj__Change_callback != NULL)
		(*obj__Change_callback)(obj__Change_callback_data,
							OBJ_NEW_YPOS, Offs_y);
	
	/*  say how far we scrolled
	 */
	return npixels;
}
	 
/*  Return true of object code is visible in the display area
 *  We may get the sequence - <updating off>
 *  <changes to the tree making the lptrs out of date> <visible(code)>.
 *  In this case we must update the lptrs to find out if the code will
 *  be visible when the display is updated.
 */
int
visible(code)
objid_t code;
{
	int ypos;
	struct objst *obj;
	
	if (Do_lptrs_pending)
		do_lptrs(FALSE);
	obj = code_to_obj(code);
	ypos = obj->ob_lptr->lp_ypos;
	return ypos > Offs_y &&
			ypos + obj->ob_depth < Offs_y + wn_get_height(Obj_wn);
}

/*  Set *p_x, *p_y to the offset from the start of the window Obj_wn of
 *  object code. Set *p_width and *p_depth to the width and depth of the
 *  object.
 */
void
get_position(code, p_x, p_y, p_width, p_depth)
objid_t code;
int *p_x, *p_y, *p_width, *p_depth;
{
	struct objst *obj;
	
	if (Do_lptrs_pending)
		do_lptrs(FALSE);
	obj = code_to_obj(code);
	obj_pos(obj, p_x, p_y);
	*p_width = obj->ob_width;
	*p_depth = obj->ob_depth;
}

/*  Return the offset in pixels of the start of the display from the
 *  start of the objects.
 */
int
get_cur_posn()
{
	return Offs_y;
}

/*  Set the window of the object to wn. If this is not the first
 *  call, it means that the wn has changed size, so we must redo 
 *  the lptrs.
 */
void
set_obj_wn(wn)
window_t wn;
{
	static int last_width = -1;
	int window_width, window_height;
	
	Obj_wn = wn;
	wn_get_window_size(wn, &window_width, &window_height);

	if (obj__Change_callback != NULL)
		(*obj__Change_callback)(obj__Change_callback_data,
					OBJ_NEW_WINDOW_HEIGHT, window_height);

	if (last_width != -1 && Updating_state == OBJ_UPDATING_ON) {
		if (window_width != last_width)
			do_lptrs(TRUE);
		display_from(Offs_x, Offs_y);
	}

	last_width = window_width;
}

/*  Display the tree starting at obj_x, obj_y pixels offset
 */
void
display_from(obj_x, obj_y)
register int obj_x, obj_y;
{
	register struct lptrst *lptr, *next;
	int ypos, win_width, win_height;
	extern int Obj_tree_changed;
	
	/*  If updating is off, just record the new position so that
	 *  when updating is turned on again we will go to the new
	 *  position
	 */
	if (Updating_state == OBJ_UPDATING_OFF) {
		New_offs_x = obj_x;
		New_offs_y = obj_y;
		Obj_tree_changed = TRUE;
		return;
	}
	
	wn_get_window_size(Obj_wn, &win_width, &win_height);
	
	/*  Find out which lptr we should start from. If we fall of the end
	 *  of the list, display from the last line
	 */
	lptr = Rootobj.ob_lptr->lp_next;
	for(;;) {
		next = lptr->lp_next;
		if (next == NULL || lptr->lp_ypos >= obj_y)
			break;
		lptr = next;
	}
	
	/*  Record the new top of the display
	 */
	Top_lptr = lptr;
	ypos = lptr->lp_ypos;
	if (next == NULL)
		obj_y = ypos;
#ifdef DISPLAY_PART
	if (lptr->lp_prev != NULL)
		dr_objline(lptr->lp_prev, -obj_x, lptr->lp_prev->lp_ypos-obj_y);
#endif /* DISPLAY_PART */
	if (ypos > obj_y)
		wn_set_area(Obj_wn, 0, 0, win_width, ypos - obj_y,
							_ob_Default_bg_pixel);

	/*  fill the display starting from lptr, using dr_objline to paint
	 *  each line of objects.
	 */
	while (lptr != NULL && ypos + lptr->lp_ldepth - obj_y < win_height) {
		wn_set_area(Obj_wn, 0, ypos - obj_y,
			    win_width, lptr->lp_ldepth, _ob_Default_bg_pixel);

		dr_objline(lptr, -obj_x, ypos - obj_y);
		ypos += lptr->lp_ldepth;
		lptr = lptr->lp_next;
	}

	if (ypos - obj_y < win_height)
		wn_set_area(Obj_wn, 0, ypos - obj_y,
			    win_width, win_height - (ypos - obj_y),
			    _ob_Default_bg_pixel);
	/*  Record the new offsets
	 */
	Offs_x = obj_x;
	Offs_y = obj_y;

	if (obj__Change_callback != NULL)
		(*obj__Change_callback)(obj__Change_callback_data,
							OBJ_NEW_YPOS, Offs_y);
}

/*  Display the line of objects pointed to by lptr.
 *  Start displaying at scr_x, scr_y in the display area.
 *  If scr_x is negative, dr_obj() will not display objects
 *  until it goes positive.
 *  Stop when we hit an object with OB_LAST_ON_LINE set.
 */
static void
dr_objline(lptr, scr_x, scr_y)
struct lptrst *lptr;
int scr_x, scr_y;
{
	register struct objst *obj;
	int xpos;
	
	obj = lptr->lp_obj;
	xpos = get_indent(obj);
	for(;;) {
		if (obj == NULL)
			panic("obj NULL in dr_objline");
		dr_obj(obj, scr_x + xpos, scr_y);
		xpos += obj->ob_width;
		if (obj->ob_flags & OB_LAST_ON_LINE)
			break;
		obj = obj->ob_next;
	}
}

/*  Default colors.
 */
int _ob_Default_fg_pixel, _ob_Default_bg_pixel;

void
obj_set_default_colors(fg, bg)
int fg, bg;
{
	_ob_Default_fg_pixel = fg;
	_ob_Default_bg_pixel = bg;
}

/*  Display object obj at scr_x, scr_y in the display area.
 *  There should be code here to check for scr_x negative, but there is not.
 *  Two nested loops, one displaying each line of fields, one displaying
 *  each field of the line.
 *
 *  Note that it is legal for the (*fd_draw)() function to alter the
 *  fd_width and fd_depth fields of fdets, thus affecting the layout
 *  of the fields.
 *
 *  Not static because it is called from update_display() in o_chds.c
 *  as well as from dr_objline() in this file.
 */
void
dr_obj(obj, scr_x, scr_y)
register struct objst *obj;
int scr_x, scr_y;
{
	struct drawst fdets;
	struct fldlnst *flnptr;
	struct fdescst *fdescp;
	struct fvalst *fvalptr, fval_buf;
	int objwn_depth;
	
	if (obj->ob_flags & OB_ONE_FVAL) {
		fval_buf.fv_val = (fval_t) obj->ob_fval;
		fval_buf.fv_next = NULL;
		fvalptr = &fval_buf;
	}
	else
		fvalptr = obj->ob_fval;
	objwn_depth = wn_get_height(Obj_wn); /* to save recalculating it */
	
	/*  set the fields of fdets which stay constant in both loops
	 */
	fdets.dr_wn = Obj_wn;
	fdets.dr_y = scr_y;

	/*  get the color for drawing
	 */
	if (Odesc[obj->ob_type].od_get_color != NULL)
		(*Odesc[obj->ob_type].od_get_color)(obj->ob_code, &fdets.dr_fg,
								  &fdets.dr_bg);
	else {
		fdets.dr_fg = _ob_Default_fg_pixel;
		fdets.dr_bg = _ob_Default_bg_pixel;
	}
	
	/*  Outer loop - looping over lines of fields - ie fldlnst
	 *  structures.
	 */
	for (flnptr = Odesc[obj->ob_type].od_fldln; flnptr != NULL;
						flnptr = flnptr->fl_next) {
		/*  set the fields of fdets which stay constant in the
		 *  inner loop
		 */
		fdets.dr_depth = flnptr->fl_ldepth;
		fdets.dr_code = obj->ob_code;
		fdets.dr_x = scr_x;
		
		/*  loop over fields in a line - fdescp structures.
		 *  fvalptr advances in step with fdescp and fldnlptr
		 */
		for (fdescp = flnptr->fl_fdesc; fdescp != NULL;
						fdescp = fdescp->fd_next) {
			fdets.dr_width = fdescp->fd_width;
			if (fdets.dr_y>=0 && fdets.dr_y+fdets.dr_depth
					      < objwn_depth) {
				if ((fdets.dr_fnum = fdescp->fd_fnum) != -1)
					fdets.dr_fval = fvalptr->fv_val;
				fdets.dr_user_info = fdescp->fd_user_info;
				
				/* call the user function to display the
				 * field
				 */
				(*fdescp->fd_draw)(&fdets);
			}
			fdets.dr_x += fdets.dr_width;
			if (fdescp->fd_fnum != -1)
				fvalptr = fvalptr->fv_next;
		}
		fdets.dr_y += fdets.dr_depth;
	}

	/*  If the object is selected, call the user select function
	 *  for this type to highlight it.
	 *
	 *  Note that there is a possibility of inefficiency here, when
	 *  we are displaying as a result of updating being turned on,
	 *  as the object may be selected and then deselected again
	 *  by do_pending_selection(). However, the bit twiddling to
	 *  avoid this is tortuous and error-prone.
	 */
	if (obj->ob_flags & OB_HIGHLIGHTED) {
		(*Odesc[obj->ob_type].od_select)(Obj_wn,
					obj->ob_code, scr_x, scr_y,
					obj->ob_width,
					obj->ob_depth,
					SEL_ON|SEL_VISIBLE|SEL_DISPLAYING);
	}
}
