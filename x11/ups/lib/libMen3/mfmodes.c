/* mfmodes.c - Mfmodes code */

/*  Copyright 1991 John Bovey, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char Men3_mfmodes_c_sccsid[] = "@(#)mfmodes.c	1.6 4/7/91 (UKC)";

#include <stdio.h>

#include <local/wn.h>
#include "menu3.h"
#include "menu_priv.h"

/*  Set the feedback modes used. They should be some combination of the
 *  MH_ modes defined in menu.h.
 *  reveal - mode used when revealing a hidden submenu
 *  toact  - mode used on selecting a button that will cause some action.
 *  acting - mode used to show what command is being executed.
 */
int
Mfmodes(md,reveal,toact,acting)
int md, reveal, toact, acting;
{
	if ((md < 0) || (md >= MAXMEN)) {
		menerr = MBADMD;
		return(-1);
	}
	if (_menu_[md].om_root == NULL) {
		menerr = MNOTMEN;
		return(-1);
	}

	if (reveal >= 0)
		_menu_[md].om_fback[0] = reveal;
	if (toact >= 0)
		_menu_[md].om_fback[1] = toact;
	if (acting >= 0)
		_menu_[md].om_fback[2] = acting;
	return(0);
}
