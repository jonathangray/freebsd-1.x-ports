/* msize.c - code for changing menu geometry */

/*  Copyright 1991 John Bovey, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char Men3_msize_c_sccsid[] = "@(#)msize.c	1.8 26/4/92 (UKC)";

#include <stdio.h>

#include <local/wn.h>
#include "menu3.h"
#include "menu_priv.h"

static int mscale MPROTO((MENU * menu, int width, int height));

/*  The menu identified by descriptor md is scaled so that its width and
 *  and height are as given by the arguments. If either 'width' or 'height'
 *  are zero then that dimension is left unchanged.
 *  -1 is returned if unsuccessful, 0 if ok.
 */
int
Msize(md,width,height)
int md, width, height;
{
	if ((md < 0) || (md >= MAXMEN)) {
		menerr = MBADMD;
		return(-1);
	}
	if (_menu_[md].om_root == NULL) {
		menerr = MNOTMEN;
		return(-1);
	}
	if ((width < 0) || (height < 0)) {
		menerr = MBADARG;
		return(-1);
	}

	return(mscale(_menu_[md].om_root,width,height));
}

/*  Scale the menu so that it has the new height and width. If either the
 *  width or height arguments are zero then the corresponding parameter
 *  is left unchanged in the menu.
 */
static int
mscale(menu,width,height)
MENU * menu;
int width, height;
{
	static float xratio, yratio;
	static short xstart, ystart;
	int x, y;

	if (menu == NULL)
		return(0);
	if (menu->me_parent == NULL) {
		xratio = 1.0;
		yratio = 1.0;
		if (width)
		  xratio = (float)(width+1)/(menu->me_xend-menu->me_xstart);
		if (height)
		  yratio = (float)(height+1)/(menu->me_yend-menu->me_ystart);
		xstart = menu->me_xstart;
		ystart = menu->me_ystart;
	} else if ((menu->me_flags & ME_FREE) && 
				(menu->me_parent->me_flags & ME_NOSCALE)) {
		/*  Don't scale free submenu.
		 */
		x = xstart + (int)(xratio*(menu->me_xstart-xstart)+0.5);
		y = ystart + (int)(yratio*(menu->me_ystart-ystart)+0.5);
		mshift(menu,x + 1,y + 1,1);
		return(0);
	}
 
	menu->me_xstart = xstart + (int)(xratio*(menu->me_xstart-xstart)+0.5);
	menu->me_ystart = ystart + (int)(yratio*(menu->me_ystart-ystart)+0.5);
	menu->me_xend=xstart+(int)(xratio*(menu->me_xend-xstart)+0.5);
	menu->me_yend=ystart+(int)(yratio*(menu->me_yend-ystart)+0.5);
	menu->me_xcurs = menu->me_xcurs * xratio + 0.5;
	menu->me_ycurs = menu->me_ycurs * yratio + 0.5;
	if (menu->me_flags & ME_VER)
		menu->me_pos = ystart+(int)(yratio*(menu->me_pos-ystart)+0.5);
	if (menu->me_flags & ME_HOR)
		menu->me_pos = xstart+(int)(xratio*(menu->me_pos-xstart)+0.5);
	mscale(menu->me_topleft,0,0);
	mscale(menu->me_botrite,0,0);
	return(0);
}

/*  Move the menu identified by menu descriptor md so that its top left hand
 *  corner is at position x,y.
 */
int
Mplace(md,x,y)
int md, x, y;
{

	if ((md < 0) || (md >= MAXMEN)) {
		menerr = MBADMD;
		return(-1);
	}
	if (_menu_[md].om_root == NULL) {
		menerr = MNOTMEN;
		return(-1);
	}
	return(mshift(_menu_[md].om_root,x,y,1));
}

/*  Shift the menu to location x,y.
 */
int
mshift(menu,x,y,root)
MENU * menu;
int x,y,root;
{
	static short dx,dy;

	if (menu == NULL)
		return(0);
	if (root) {
		dx = x - menu->me_xstart - 1;
		dy = y - menu->me_ystart - 1;
	}
	menu->me_xstart += dx;
	menu->me_xend += dx;
	menu->me_ystart += dy;
	menu->me_yend += dy;
	if (menu->me_flags & ME_VER)
		menu->me_pos += dy;
	if (menu->me_flags & ME_HOR)
		menu->me_pos += dx;
	mshift(menu->me_topleft,x,y,0);
	mshift(menu->me_botrite,x,y,0);
	return(0);
}
