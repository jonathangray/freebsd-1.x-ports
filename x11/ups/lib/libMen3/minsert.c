/* minsert.c - menu creation from static data */

/*  Copyright 1991 John Bovey, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char Men3_minsert_c_sccsid[] = "@(#)minsert.c	1.10 26/4/92 (UKC)";

#include <stdio.h>

#include <local/wn.h>
#include "menu3.h"
#include "menu_priv.h"

static void rplink MPROTO((MENU *mp, MENU *parent));
static MENU *mrvfind MPROTO((MENU *main, int rv));
static void mdump MPROTO((MENU *m, int indent));

/*  Insert the menu in the _menu_ array.  A menu identifier is returned which
 *  is used to identify the menu in all subsequent function calls.
 *  -1 is returned if unsuccessful.
 */
int
Minsert(menu)
MENU * menu;
{
	register int i;

	for (i = 0; i < MAXMEN; i++)
		if (_menu_[i].om_root == NULL)
			break;
	if (i == MAXMEN) {
		menerr = MTABFL;
		return(-1);
	}

	rplink(menu,(MENU *)NULL);

	_menu_[i].om_root = menu;
	_menu_[i].om_last = NULL;
	_menu_[i].om_md = i;
	_menu_[i].om_font[0] =
	_menu_[i].om_font[1] =
	_menu_[i].om_font[2] = 
	_menu_[i].om_font[3] = Mstdfont();
	_menu_[i].om_fback[0] = MH_GREY;
	_menu_[i].om_fback[1] = MH_BLACK;
	_menu_[i].om_fback[2] = MH_GREY;
	_menu_[i].om_wantbox = 1;
	momen(_menu_[i].om_root,&_menu_[i]);
	menerr = MENOK;
	return(i);
}

static void
rplink(mp,parent)
MENU *mp, *parent;
{
	if (mp == NULL)
		return;
	rplink(mp->me_topleft,mp);
	rplink(mp->me_botrite,mp);
	mp->me_parent = parent;
	mp->me_flags &= ~(ME_FREC | ME_FREN);
}

/*  Link submd in as a popup submenu of mainmd under the button with
 *  return value rv. If the covering button has a popup menu 
 *  already then its position and attributes will be transfered to the
 *  new submenu. If the covering button does not have a submenu then
 *  the position of the submenu is determined by the triple xoff, yoff
 *  and mlstyle. xoff and yoff give the offset of the popup menu relative
 *  to the pressed button and mlstyle describes what is offset relative to
 *  what. If mlstyle is an |'rd combination of the following
 *
 *	ME_MLBUTR	use the right side of the button
 *	ME_MLBUTB	use the bottom of the button
 *	ME_MLPOPR	use the right side of the pop-up
 *	ME_MLPOPB	use the bottom of the pop-up
 *	ME_MLCREL	make the pop-up cursor relative
 */
int
Mlink(mainmd,submd,rv,xoff,yoff,mlstyle)
int mainmd, submd, rv,xoff,yoff,mlstyle;
{
	int x, y;
	MENU *mb, *ms;

	if ((mainmd < 0) || (mainmd >= MAXMEN)) {
		menerr = MBADMD;
		return(-1);
	}
	if (_menu_[mainmd].om_root == NULL) {
		menerr = MNOTMEN;
		return(-1);
	}
	if ((submd < 0) || (submd >= MAXMEN)) {
		menerr = MBADMD;
		return(-1);
	}
	if ((ms = _menu_[submd].om_root) == NULL) {
		menerr = MNOTMEN;
		return(-1);
	}
	if ((mb = mrvfind(_menu_[mainmd].om_root,rv)) == NULL) {
		menerr = MBADARG;
		return(-1);
	}
	if ((mb->me_flags & ME_ISUB) && (mb->me_topleft != NULL)) {
		/*  The button already has a submenu so use its
		 *  position for the new submenu
		 */
		ms->me_xcurs = mb->me_topleft->me_xcurs;
		ms->me_ycurs = mb->me_topleft->me_ycurs;
		mshift(ms,mb->me_topleft->me_xstart,mb->me_topleft->me_ystart,1);
	} else if (mlstyle & ME_MLCREL) {
		ms->me_xcurs = xoff;
		ms->me_ycurs = yoff;
		mb->me_flags |= ME_CREL;
	} else {
    		x = xoff + (mlstyle & ME_MLBUTR ? mb->me_xend : mb->me_xstart);
		if (mlstyle & ME_MLPOPR)
			x -= ms->me_xend - ms->me_xstart;
    		y = yoff + (mlstyle & ME_MLBUTB ? mb->me_yend : mb->me_ystart);
		if (mlstyle & ME_MLPOPB)
			y -= ms->me_yend - ms->me_ystart;
		mshift(ms,x,y,1);
	}
	mb->me_flags |= ME_ISUB|ME_POPUP;
	ms->me_flags |= ME_FREE;
	mb->me_topleft = ms;
	ms->me_parent = mb;
	momen(_menu_[mainmd].om_root,&_menu_[mainmd]);
	return(0);
}

/*  Find the menu button with the given return value.
 */
static MENU *
mrvfind(main,rv)
MENU *main;
int rv;
{
	MENU *ms;

	if (main == NULL)
		return(NULL);
	if (main->me_rv == rv && main->me_cap != NULL)
		return(main);
	if ((ms = mrvfind(main->me_topleft,rv)) != NULL)
		return(ms);
	if ((ms = mrvfind(main->me_botrite,rv)) != NULL)
		return(ms);
	return(NULL);
}

/*  Dump the menu on the standard output. Used for debugging only.
 */
void
Mdump(md)
int md;
{
	mdump(_menu_[md].om_root,0);
}

static void
mdump(m,indent)
MENU *m;
int indent;
{
	int i;

	for (i = 0; i < indent; i++)
		putchar(' ');
	if (m == NULL) {
		printf("NULL\n");
		return;
	}
	printf("%-20s %x %x(%c) (%d,%d) (%d,%d) (%d,%d)\n",
		m->me_cap,
		m->me_flags,
		m->me_rv,m->me_rv & 0177,
		m->me_xstart,m->me_ystart,
		m->me_xend,m->me_yend,
		m->me_xcurs,m->me_ycurs);
	mdump(m->me_topleft,indent + 5);
	mdump(m->me_botrite,indent + 5);
}

