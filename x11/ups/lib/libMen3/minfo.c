/* minfo.c - functions giving information about menu geometry */

/*  Copyright 1991 John Bovey, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char Men3_minfo_c_sccsid[] = "@(#)minfo.c	1.6 4/7/91 (UKC)";

#include <stdio.h>

#include <local/wn.h>
#include "menu3.h"
#include "menu_priv.h"

/*  Return the left edge of the menu.
 */
int
Mleft(md)
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
	return(_menu_[md].om_root->me_xstart + 1);
}

/*  Return the right edge of the menu.
 */
int
Mright(md)
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
	return(_menu_[md].om_root->me_xend);
}
/*  Return the top edge of the menu.
 */
int
Mtop(md)
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
	return(_menu_[md].om_root->me_ystart + 1);
}

/*  Return the bottom edge of the menu.
 */
int
Mbottom(md)
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
	return(_menu_[md].om_root->me_yend);
}

/*  Return the width of the menu.
 */
int
Mwidth(md)
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
	return(_menu_[md].om_root->me_xend - _menu_[md].om_root->me_xstart - 1);
}

/*  Return the height of the menu.
 */
int
Mheight(md)
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
	return(_menu_[md].om_root->me_yend - _menu_[md].om_root->me_ystart - 1);
}
