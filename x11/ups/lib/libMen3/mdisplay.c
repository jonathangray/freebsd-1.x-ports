/* mdisplay.c - code for displaying menus */

/*  Copyright 1991 John Bovey, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char Men3_mdisplay_c_sccsid[] = "@(#)mdisplay.c	1.12 26/4/92 (UKC)";

#include <stdio.h>
#include <stdlib.h>

#include <local/wn.h>
#include "menu3.h"
#include "menu_priv.h"
#include "sccsdata.h"
#include "wdbyte.h"

static void mclear MPROTO((MENU *menu, int popup));
static void imredraw MPROTO((WOTYPE wn, int redo_save));
static void mredraw MPROTO((MENU * menu));
static void setarea MPROTO((int md, int save, WOTYPE wn));
static int mnonsel MPROTO((MENU *menu, const char *rvs));
static int dragin MPROTO((MENU *menu));
static int mbpress MPROTO((int x, int y, MENU *menu));
static void msadd MPROTO((MENU *menu));
static MENU *mfind MPROTO((MENU *menu, int x, int y));
static MENU *msfind MPROTO((int x, int y, WOTYPE wn, int ignore));
static void msdelete MPROTO((MENU *menu));
static void rmshow MPROTO((MENU *menu, WOTYPE wn));
static void mshade MPROTO((MENU *menu, int colour));
static void dcmess MPROTO((WOTYPE wn, MENU *mp));
static int wbold MPROTO((WOTYPE wn, Box_t *clip));
static void wgrey MPROTO((WOTYPE wn, Box_t *clip, int colour));
static int b_in_m MPROTO((MENU *menu, MENU *button));

#define inmenu(m,x,y)	(((x) > (m)->me_xstart) && ((x) <= (m)->me_xend) &&\
			((y) > (m)->me_ystart) && ((y) <= (m)->me_yend))

#define MSMAX        100     /* the maximum number of stacked submenus */

struct omenst _menu_[MAXMEN];
int menerr;

static MENU *lastpress;	/* menu button of the last press */
static MENU *mstack[MSMAX];	/* stack of displayed menus */
static int msnext = 0;		/* the number of menus in the stack */

static int (*fpopsu)MPROTO((int wn)) = NULL, (*fpushsu)MPROTO((int wn)) = NULL;

/*  Set the functions to be used for turning screen updating on and
 *  off. The functions are called in the same way as Mpushsu and Mpopsu,
 *  and should have the same semantics - in particular, they should stack.
 *
 *  If this function is called, it should be before any other menu
 *  package function.
 */
void
Msetpushpop(fpush,fpop)
int (*fpush)MPROTO((int wn)), (*fpop)MPROTO((int wn));
{
	fpushsu = fpush;
	fpopsu = fpop;
}

/*  Return the version string for the menu library.
 */
const char *
Mversion()
{
	return(_Men3wn_sccsdata[0]);
}

/*  Push screen updating onto the update stack.
 */
int
Mpushsu(wn)
window_t wn;
{	
	if (fpushsu != NULL)
		return((*fpushsu)(wn));
	wn_updating_off(wn);
	return(0);
}

/*  Pop a screen update from the stack and do the update if level 0
 *  is reached.
 */
int
Mpopsu(wn)
window_t wn;
{
	if (fpopsu != NULL)
		return((*fpopsu)(wn));
	wn_updating_on(wn);
	return(0);
}

/*  Clear highlighting and popup submenus from the display
 */
int
Mclear(md)
int md;
{
	if ((md < 0) || (md >= MAXMEN)) {
		menerr = MBADMD;
		return(-1);
	}
	if (_menu_[md].om_root == NULL) {
		menerr = MNOTMEN;
		return(-1);
	}
	if (_menu_[md].om_last == NULL) {
		menerr = MNOTDIS;
		return(-1);
	}
	mclear(_menu_[md].om_root,0);
	mshow(_menu_[md].om_last,0);
	return(0);
}

int
Mrepair(wn)
WOTYPE wn;
{
	imredraw(wn,0);
	return(0);
}

int
Mredraw(wn)
WOTYPE wn;
{
	imredraw(wn,1);
	return(0);
}

/*  Redraw all the menus in the window.
 *  If redo_save is non zero, refresh the saved background.
 */
static void
imredraw(wn,redo_save)
WOTYPE wn;
int redo_save;
{
	int i;
	struct omenst *om;

	for (i = 0; i < MAXMEN; i++) {
		if (_menu_[i].om_root == NULL)
			continue;
		if (_menu_[i].om_wout != wn)
			continue;
		if (redo_save) {
			if (_menu_[i].om_rect != NULL) {
				setarea(i,wn,1);
			}
			else
				setarea(i,wn,0);
		}
	}
	for (i = 0; i < msnext; i++) {
		om = mstack[i]->me_omen;
		if (wn != om->om_wout)
			continue;
		mshow(mstack[i],1);
	}
}

/*  Redraw the menu with menu descriptor md
 */
void
Mmredraw(md)
int md;
{
	int i;
	struct omenst *om;
	MENU *ms;
	Pos_t src;
	Size_t size;

	if (_menu_[md].om_root == NULL)
		return;
	if (_menu_[md].om_rect != NULL) {
		/* pr_destroy(_menu_[md].om_pr); */
		setarea(md,_menu_[md].om_wout,1);
	}
	else
		setarea(md,_menu_[md].om_wout,0);

	for (i = 0; i < msnext; i++) {
		om = (ms = mstack[i])->me_omen;
		if (md != om->om_md)
			continue;
		if (ms->me_save != 0) {
			wn_free_area(ms->me_save);
			src.x = ms->me_xstart;
			size.w = ms->me_xend - ms->me_xstart + 1;
			src.y = ms->me_ystart;
			size.h = ms->me_yend - ms->me_ystart + 1;
			ms->me_save = wn_save_area(om->om_wout,src.x,src.y,
								size.w,size.h);
		}
		mshow(mstack[i],1);
	}
}

static void
mredraw(menu)
MENU * menu;
{
	if (menu == NULL)
		return;
	mredraw(menu->me_parent);
	mshow(menu,1);
}


/*  set the menu fonts
 */
int
Mfonts(md,font0,font1,font2,font3)
int md;
font_t *font0, *font1, *font2, *font3;
{
	if ((md < 0) || (md >= MAXMEN)) {
		menerr = MBADMD;
		return(-1);
	}
	if (_menu_[md].om_root == NULL) {
		menerr = MNOTMEN;
		return(-1);
	}
	_menu_[md].om_font[0] = font0 == NULL ? Mstdfont() : font0;
	_menu_[md].om_font[1] = font1 == NULL ? Mstdfont() : font1;
	_menu_[md].om_font[2] = font2 == NULL ? Mstdfont() : font2;
	_menu_[md].om_font[3] = font3 == NULL ? Mstdfont() : font3;
	return(0);
}

/*  Set the menu buttons with the given return values to be non-selectable.
 *
 *  rvs is assumed to be a zero terminated arrayof return values to be
 *  disabled.
 */
int
Mnonsel(md,rvs)
int md;
const char *rvs;
{
	if ((md < 0) || (md >= MAXMEN)) {
		menerr = MBADMD;
		return(-1);
	}
	if (_menu_[md].om_root == NULL) {
		menerr = MNOTMEN;
		return(-1);
	}
	if (rvs == NULL) {
		menerr = MBADARG;
		return(-1);
	}
	mnonsel(_menu_[md].om_root, rvs);
	if (_menu_[md].om_last != NULL) {
		Mpushsu(_menu_[md].om_wout);
		mredraw(_menu_[md].om_last);
		Mpopsu(_menu_[md].om_wout);
	}
	return(0);
}

static int
mnonsel(menu,rvs)
MENU *menu;
const char *rvs;
{
	register const char *rvp;
	int sel = 0;

	if (menu == NULL)
		return(1);

	sel += mnonsel(menu->me_topleft, rvs);
	if (!(menu->me_flags & ME_ISUB))
		sel += mnonsel(menu->me_botrite, rvs);

	if (menu->me_cap != NULL) {
		for (rvp = rvs; *rvp != 0; rvp++)
			if (*rvp == menu->me_rv) {
				sel = 0;
				break;
			}
	}
	if (sel)
		menu->me_flags &= ~ME_NSEL;
	else
		menu->me_flags |= ME_NSEL;
	return(sel);
}

int
Mreset(md)
int md;
{
	if ((md < 0) || (md >= MAXMEN)) {
		menerr = MBADMD;
		return(-1);
	}
	if (_menu_[md].om_root == NULL) {
		menerr = MNOTMEN;
		return(-1);
	}
	if (_menu_[md].om_last == NULL) {
		menerr = MNOTDIS;
		return(-1);
	}
	_menu_[md].om_last = _menu_[md].om_root;
	mclear(_menu_[md].om_root,1);
	mshow(_menu_[md].om_root,0);
	return(0);
}

/*  Return true if the coordinates would select a menu button.
 */
int
Mcansel(x,y,wn)
int x, y;
WOTYPE wn;
{
	return(mfind(msfind(x,y,wn,0),x,y) != NULL);
}


/*  Use the coordinates x, y and window output descriptor wn to select a
 *  menu field. For action on button release, mode should be one of:
 *
 *	MS_PRESSED  when the button is first pressed.
 *	MS_CONTINUE while the button is pressed if continuous feedback
 *		    is wanted.
 *	MS_RELEASED when the button is released.
 *
 *  For action on button press the mode should be MS_PRESSED
 *  The argument ignore should be a bitmap of menu descriptors that will be
 *  ignored. If a command button has been selected the return value will
 *  have the menu descriptor in the upper 16 bits and the command number
 *  in the lower 16.
 */
int
Mselect(x,y,wn,mode,ignore)
int x, y;
WOTYPE wn;
int mode, ignore;
{
	int rv, mrv;
	MENU *m, *menu;
	struct omenst *om;
	static MENU *lastb;	/* The last button we were in */

	menu = msfind(x,y,wn,ignore);
	m = mfind(menu,x,y);
	if (mode == MS_PRESSED) {
		if (m == NULL) {
			lastpress = dragin(menu) ? menu : NULL;
			lastb = m;
			return(0);
		}
		rv = mbpress(x,y,m);
		lastb = lastpress;
		/*  Try and cope with any popup menu which may now
		 *  be under the coords.
		 */
		mrv = Mselect(x,y,wn,MS_CONTINUE,ignore);
		return((m == lastb) ? rv : mrv);
	}
	if (mode == MS_CONTINUE) {
		if (lastpress == NULL)
			return(0);
		if (menu == NULL)
			m = NULL;
		if (m == lastb || (!dragin(m) && lastb != NULL &&
						(lastb->me_flags & ME_ISUB)))
			return(0);
		lastb = m;
		if (dragin(m))
			return(mbpress(x,y,m));

		if (lastpress->me_flags & ME_ISUB)
			/*  Don't try giving feedback on a submenu button
			 */
			return(0);
		om = lastpress->me_omen;
		if (m == lastpress) {
			lastpress->me_colour = om->om_fback[1];
			mshow(lastpress,1);
			return(0);
		} else {
			lastpress->me_colour = 0;
			mshow(lastpress,1);
			return(m != NULL && m->me_omen == om);
		}
	}
	if (mode == MS_RELEASED) {
		if (lastpress == NULL)
			return(0);
		om = lastpress->me_omen;
		if ((m != lastpress) || (m->me_flags & ME_ISUB)) {
			/*  Nothing selected, tidy up
			 */
			Mpushsu(wn);
			mclear(om->om_root,0);
			mshow(om->om_last,0);
			lastpress  = NULL;
			Mpopsu(wn);
			return(0);
		}
		lastpress->me_colour = om->om_fback[2];
		mshow(lastpress,1);
		lastpress = NULL;
		return(om->om_md << 16 | m->me_rv);
	}
	abort();		/* to make me happy */
	return(0);		/* to make gcc happy */
}

/*  Clear away any highlighting and dragin submenus. popup should be 0 when
 *  called non recursively
 */
static void
mclear(menu,popup)
MENU *menu;
int popup;
{
	struct omenst *om;
	MENU *ms;

	if (menu == NULL)
		return;
	if (menu->me_flags & ME_POPUP)
		popup = 1;
	om = menu->me_omen;
	Mpushsu(om->om_wout);
	if (ISCLEAR(menu)) {
		mclear(menu->me_topleft,popup);
		mclear(menu->me_botrite,popup);
	}
	if ((menu->me_flags & (ME_ISUB | ME_OPEN)) == (ME_ISUB | ME_OPEN))
		mclear(menu->me_topleft,popup);

	if (menu->me_colour != 0) {
		menu->me_colour = 0;
		menu->me_flags |= ME_REDRAW;
	}
	if (popup && (menu->me_flags & ME_OPEN)) {
		if (menu->me_flags & ME_ISUB) {
			/* Remove a popup menu
			 */
			ms = menu->me_topleft;
			wn_restore_area(ms->me_save);
			wn_free_area(ms->me_save);
			ms->me_save = NULL;
			msdelete(ms);
		}
		menu->me_flags &= ~ME_OPEN;
		om->om_last = menu;
	}
	Mpopsu(om->om_wout);
}

/*  Return true if the menu button should select on drag in
 */
static int
dragin(menu)
MENU *menu;
{
	register MENU *m;

	if (menu == NULL)
		return(0);
	for (m = menu->me_parent; m != NULL; m = m->me_parent)
		if(m->me_flags & ME_POPUP)
			return(1);
	return(0);
}

/*  Do the selection for with a button press, either real or assumed.
 */
static int
mbpress(x,y,menu)
int x,y;
MENU *menu;
{
	MENU *m, *ms;
	struct omenst *om;
	Pos_t src;
	Size_t size;
	
	if (menu == NULL)
		return(0);
	om = menu->me_omen;
	Mpushsu(om->om_wout);
	for (m = om->om_last; ; m = m->me_parent) {
		if ((m->me_flags & (ME_OPEN|ME_ISUB)) == (ME_OPEN|ME_ISUB)) {
			/* Remove a popup menu
			 */
			ms = m->me_topleft;
			wn_restore_area(ms->me_save);
			wn_free_area(ms->me_save);
			ms->me_save = NULL;
			msdelete(ms);
			if (m->me_flags & ME_OPEN) {
				m->me_flags &= ~ME_OPEN;
				m->me_flags |= ME_REDRAW;
			}
		}
		if (m->me_colour != 0) {
			m->me_flags |= ME_REDRAW;
			m->me_colour = 0;
		}
		if (inmenu(m,x,y))
			break;
		if (m->me_flags & ME_OPEN) {
			m->me_flags &= ~ME_OPEN;
			m->me_flags |= ME_REDRAW;
		}
	}
	mshow(m,0);
	m = mfind(m,x,y);
	if (m->me_flags & ME_DIV) {
		m->me_flags |= ME_OPEN;
		m->me_flags |= ME_REDRAW;
		om->om_last = m;
		lastpress = m;
		mshow(m,0);
		Mpopsu(om->om_wout);
		return(0);
	}
	if (m->me_flags & ME_ISUB) {
		/*  Display a popup menu.
		 */
		ms = m->me_topleft;
		if (m->me_flags & ME_CREL) {
			/* The submenu is cursor-relative so we need
			 * to move it.
			 */
			mshift(ms,x + ms->me_xcurs,y + ms->me_ycurs,1);
		}
		src.x = ms->me_xstart;
		size.w = ms->me_xend - ms->me_xstart + 1;
		src.y = ms->me_ystart;
		size.h = ms->me_yend - ms->me_ystart + 1;

		ms->me_save = wn_save_area(om->om_wout,src.x,src.y,
							size.w,size.h);

		om->om_last = ms;
		msadd(ms);
		m->me_flags |= ME_OPEN;
		m->me_flags |= ME_REDRAW;
		m->me_colour = om->om_fback[2];
		mshow(m,1);
		mshow(ms,1);
	}
	else {
		om->om_last = m;
		m->me_colour = om->om_fback[1];
		mshow(m,1);
	}
	lastpress = m;
	Mpopsu(om->om_wout);
	return(om->om_md << 16 | m->me_rv);
}

/*  Find the smallest visible subfield containing the coordinates. Return NULL
 *  if there isn't one.
 */
static MENU *
mfind(menu,x,y)
MENU *menu;
int x,y;
{
	MENU *res;

	if (menu == NULL)
		return(NULL);
	if (!inmenu(menu,x,y))
		return(NULL);
	if (menu->me_flags & ME_NSEL)
		return(NULL);
	if (!ISCLEAR(menu))
		return(menu);
	if (menu->me_flags & ME_HOR) {
		if (x <= menu->me_pos)
			res = mfind(menu->me_topleft,x,y);
		else
			res = mfind(menu->me_botrite,x,y);
	}
	else {
		if (y <= menu->me_pos)
			res = mfind(menu->me_topleft,x,y);
		else
			res = mfind(menu->me_botrite,x,y);
	}
	return(res);
}

/*  Routines used to manipulate the stack of displayed submenus. These are
 *  root menus or free submenus.
 */

static void
msadd(menu)
MENU *menu;
{
	if (msnext == MSMAX)
		abort();	/* surely not legitimately */
	mstack[msnext++] = menu;
}

static MENU *
msfind(x,y,wn,ignore)
int x, y;
WOTYPE wn;
int ignore;
{
	register i;

	for (i = msnext - 1; i >= 0; i--) {
		if (!(inmenu(mstack[i],x,y)))
			continue;
		if (wn != mstack[i]->me_omen->om_wout)
			continue;
		if (ignore && (ignore & (1 << mstack[i]->me_omen->om_md)))
			continue;
		return(mstack[i]);
	}
	return(NULL);
}

static void
msdelete(menu)
MENU *menu;
{
	register i;

	for (i = 0; i < msnext; i++)
		if (menu == mstack[i])
			break;
	if (i < msnext)
		msnext--;
	for (; i < msnext; i++)
		mstack[i] = mstack[i + 1];
}

/*  The menu identified by descriptor md is displayed in the window opened
 *  with descriptor wn. If the menu is already displayed the old display
 *  will be removed. If save is nonzero the area of screen under the menu will
 *  be saved to be restored when the menu is removed.
 *  0 is returned if successful, otherwise -1.
 */
int
Mdisplay(md,wn,save)
int md, save;
WOTYPE wn;
{
	register MENU * mp;
	
	if ((md < 0) || (md >= MAXMEN)) {
		menerr = MBADMD;
		return(-1);
	}
	if ((mp = _menu_[md].om_root) == NULL) {
		menerr = MNOTMEN;
		return(-1);
	}

	Mpushsu(wn);
	if (_menu_[md].om_last != NULL)
		if (Mremove(md) < 0) {
			Mpopsu(wn);
			return(-1);
		}
	
	mp->me_omen->om_wout = wn;
	setarea(md,wn,save);
	if (mshow(mp,1) == 0) {
		_menu_[md].om_wout = wn;
		_menu_[md].om_last = _menu_[md].om_root;
		msadd(mp);
		Mpopsu(wn);
		return(0);
	}
	else {
		Mpopsu(wn);
		return(-1);
	}
}

/*  set up the om_area and om_rect elements of the menu structure
 */
static void
setarea(md,wn,save)
int md, save;
WOTYPE wn;
{
	MENU * mp;
	Pos_t src;
	Size_t size;
	

	mp = _menu_[md].om_root;
	_menu_[md].om_area.pos.x = mp->me_xstart;
	_menu_[md].om_area.pos.y = mp->me_ystart;
	_menu_[md].om_area.size.h = mp->me_yend - _menu_[md].om_area.pos.y + 1;
	_menu_[md].om_area.size.w = mp->me_xend - _menu_[md].om_area.pos.x + 1;

	if (!_menu_[md].om_wantbox) {
		++_menu_[md].om_area.pos.x;
		++_menu_[md].om_area.pos.y;
		_menu_[md].om_area.size.w -= 2;
		_menu_[md].om_area.size.h -= 2;
	}
	if (_menu_[md].om_rect != NULL) {
		wn_free_area(_menu_[md].om_rect);
		_menu_[md].om_rect = NULL;
	}
	if (save) {
		/*  save the area that will be covered by the menu
		 */
		src = _menu_[md].om_area.pos;
		size = _menu_[md].om_area.size;
		_menu_[md].om_rect = wn_save_area(wn,src.x,src.y,size.w,size.h);
	}
}

/*  Return 1 if the button is in the menu, 0 otherwise.
 */
static int
b_in_m(menu,button)
MENU *menu, *button;
{
	if (menu == NULL)
		return(0);
	if (menu == button)
		return(1);
	return(b_in_m(menu->me_topleft,button) || b_in_m(menu->me_botrite,button));
}

/*  Remove the menu from the display and restore what was there before
 *  it was overlayed by the menu.
 */
int
Mremove(md)
int md;
{
	if ((md < 0) || (md >= MAXMEN)) {
		menerr = MBADMD;
		return(-1);
	}
	if (_menu_[md].om_root == NULL) {
		menerr = MNOTMEN;
		return(-1);
	}
	if (_menu_[md].om_last == NULL) {
		menerr = MNOTDIS;
		return(-1);
	}
	mclear(_menu_[md].om_root,1);
	msdelete(_menu_[md].om_root);
	_menu_[md].om_last = NULL;
	if (b_in_m(_menu_[md].om_root,lastpress))
		lastpress = NULL;

	if (_menu_[md].om_rect == NULL)
		wblank(_menu_[md].om_wout,&_menu_[md].om_area);
	else {
		wn_restore_area(_menu_[md].om_rect);
		wn_free_area(_menu_[md].om_rect);
		_menu_[md].om_rect = NULL;
	}
	return(0);
}

/*  Display the menu in the window open with file descriptor wn.
 */
int
mshow(menu,drawall)
MENU *menu;
int drawall;
{
	Box_t cl;
	int wn;

	if (menu == NULL)
		return(0);
	wn = menu->me_omen->om_wout;
	if (menu->me_flags & ME_REDRAW)
		drawall = 1;
	if (drawall) {
		Mpushsu(wn);
		cl.pos.x = menu->me_xstart + 1;
		cl.pos.y = menu->me_ystart + 1;
		cl.size.w = menu->me_xend - cl.pos.x;
		cl.size.h = menu->me_yend - cl.pos.y;
		if (wblank(wn,&cl) < 0) {
			menerr = MBADWD;
			Mpopsu(wn);
			return(-1);
		}
		rmshow(menu,wn);
		Mpopsu(wn);
		return(0);
	}
	if (ISCLEAR(menu)) {
		mshow(menu->me_topleft,0);
		mshow(menu->me_botrite,0);
	}
	return(0);
}

/*  Highlight the menu.
 */
static void
mshade(menu,colour)
MENU * menu;
int colour;
{
	Box_t cl;
	int wn;

	if ((menu == NULL) || (colour == 0))
		return;
	wn = menu->me_omen->om_wout;
	cl.pos.x = menu->me_xstart + 1;
	cl.pos.y = menu->me_ystart + 1;
	cl.size.w = menu->me_xend - cl.pos.x;
	cl.size.h = menu->me_yend - cl.pos.y;
	if (colour & MH_BOLD)
		wbold(wn,&cl);
	if (colour & MH_GREY)
		wgrey(wn,&cl,1);
	if (colour & MH_BLACK)
		wn_invert_area(wn,cl.pos.x,cl.pos.y,cl.size.w,cl.size.h);
}

int
Mnobox(md)
int md;
{
	if ((md < 0) || (md >= MAXMEN)) {
		menerr = MBADMD;
		return(-1);
	}
	_menu_[md].om_wantbox = 0;
	return(0);
}

/*  Recusive menu displaying function. This function is only called by
 *  mshow.
 */
static void
rmshow(menu,wn)
MENU * menu;
WOTYPE wn;
{
	Pos_t p1, p2;
	
	if (menu == NULL)
		return;
	menu->me_flags &= ~ME_REDRAW;
	if (((menu->me_flags & ME_FREE) || menu->me_parent == NULL) &&
	    (menu->me_parent != NULL || menu->me_omen->om_wantbox)){
		/*  draw a box round the whole menu */
		p1.x = menu->me_xstart;
		p2.x = menu->me_xend;
		p1.y = p2.y = menu->me_yend;
		wn_draw_line(wn,p1.x,p1.y,p2.x,p2.y,BLACK);
		p1.y = p2.y = menu->me_ystart;
		wn_draw_line(wn,p1.x,p1.y,p2.x,p2.y,BLACK);
		p1.y = menu->me_ystart;
		p2.y = menu->me_yend;
		p1.x = p2.x = menu->me_xend;
		wn_draw_line(wn,p1.x,p1.y,p2.x,p2.y,BLACK);
		p1.x = p2.x = menu->me_xstart;
		wn_draw_line(wn,p1.x,p1.y,p2.x,p2.y,BLACK);
	}
	if (ISCLEAR(menu)) {
		if (menu->me_flags & ME_HOR) {
			p1.y = menu->me_ystart + 1;
			p1.x = menu->me_pos;
			p2.y = menu->me_yend;
			p2.x = p1.x;
		}
		else {
			p1.x = menu->me_xstart + 1;
			p1.y = menu->me_pos;
			p2.x = menu->me_xend;
			p2.y = p1.y;
		}
		if (!(menu->me_flags & ME_BGLINE))
			wn_draw_line(wn,p1.x,p1.y,p2.x,p2.y,BLACK);
		rmshow(menu->me_topleft,wn);
		rmshow(menu->me_botrite,wn);
	}
	else 
		dcmess(wn,menu);
	mshade(menu,menu->me_colour);
}

/*  Display string  in the centre of menu field. Only called by rmshow.
 */
static void
dcmess(wn,mp)
WOTYPE wn;
MENU *mp;
{
	register int l, cx, cy, x, y;
	int charheight, charbase;
	struct DBCtl db;
	Box_t cl;
	short *fe;
	FONT *fnt;
	struct omenst *om;
	const char *str, *sp;

	cl.pos.x = mp->me_xstart + 1;
	cl.pos.y = mp->me_ystart + 1;
	cl.size.w = mp->me_xend - cl.pos.x;
	cl.size.h = mp->me_yend - cl.pos.y;

	om = mp->me_omen;
	fnt = om->om_font[mp->me_flags & ME_FONT];
	fe = fnt->ft_width_tab;
	charheight = fnt->ft_depth + 2;
	charbase = fnt->ft_baseline + 1;

	/* find the centre of the display area
	 */
	cx = cl.pos.x + cl.size.w / 2;
	cy = cl.pos.y + cl.size.h / 2;
	str = mp->me_cap;
	
	/* find the number of lines in the string
	 */
	for (l = 1,sp = str; *sp; sp++)
		if (*sp == '\n')
			l++;
	y = cy - (l * charheight) / 2 + charbase;
	wblank(wn,&cl);
	while (*str) {
		/* calculate the length of a caption line
		 */
		l = 0;
		for (sp = str; *sp; sp++) {
			if (*sp == '\n')
				break;
			l += fe[*sp & 0177];
			if (l > cl.size.w - 5) {
				l -= fe[*sp & 0177];
				break;
			}
		}
		if (mp->me_flags & ME_LEFT)
			x = cl.pos.x + 3;
		else
			x = cx - l / 2;
		db.DBX = x;
		db.DBY = y;
		db.DBByteOffset = 0;
		db.DBFunc = R_RPL;
		db.DBMaxX = cl.pos.x + cl.size.w - 2;
		db.DBMaxByte = sp - str;
		db.DBSrcString = str;
		db.DBScreen = 0;
		db.DBFont = fnt;
		wn_wdbyte(wn,&db,&cl);
		str = sp;
		while ((*str != '\n') && (*str != 0))
			str++;
		if (*str != '\0')
			str++;
		y += charheight;
	}
	if (mp->me_flags & ME_NSEL)
	wgrey(wn,&cl,0);
}

/*  Blank the area described in the clip area.
 */
int
wblank(wn,clip)
WOTYPE wn;
Box_t *clip;
{
	wn_set_area(wn, clip->pos.x, clip->pos.y, clip->size.w, clip->size.h, WN_BG);
	return(0);
}

/*  Enbolden the area described in the clip area.
 */
static int
wbold(wn,clip)
WOTYPE wn;
Box_t *clip;
{
	wn_mono_rop(wn,clip->pos.x,clip->pos.y,clip->size.w - 1,clip->size.h,
			clip->pos.x + 1,clip->pos.y,R_OR);
	return (0);
}

/*  Draw a grey pattern in the clip area. If the colour is 1 the pattern
 *  is in the foreground colour. If 0 it is in the background;
 */
static void
wgrey(wn,clip,colour)
WOTYPE wn;
Box_t *clip;
int colour;
{
	int shade, func;

	if (colour == 1) {
		shade = WN_GREY4;
		func = R_OR;
	}
	else {
		shade = WN_GREY12;
		func = R_AND;
	}
	wn_shade_area(wn, clip->pos.x, clip->pos.y,
					clip->size.w, clip->size.h, shade, func);
}
