/* mclose.c - Mclose code */

/*  Copyright 1991 John Bovey, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char Men3_mclose_c_sccsid[] = "@(#)mclose.c	1.7 26/4/92 (UKC)";

#include <stdio.h>
#include <stdlib.h>

#include <local/wn.h>
#include "menu3.h"
#include "menu_priv.h"

static void freemen MPROTO((MENU *menu));

/*  Remove the menu from the list of open menus and free the menu descriptor
 *  for re-use. If the menu is currently displayed it will be removed.
 */
int
Mclose(md)
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
	if (_menu_[md].om_last != NULL)
		if (Mremove(md) < 0)
			return(-1);

	freemen(_menu_[md].om_root);
	_menu_[md].om_root = NULL;
	return(0);
}

/*  Free the memory occupied by the menu.
 */
static void
freemen(menu)
MENU * menu;
{
	if (menu == NULL)
		return;
	freemen(menu->me_topleft);
	freemen(menu->me_botrite);
	if ((menu->me_cap != NULL) && (menu->me_flags & ME_FREC)) {
		/*  BUG: we cast "const char *" to "char *" here.
		 *  me_cap is of type const char *, because menus are
		 *  statically initialised from strings.  
		 */
		free((char *)menu->me_cap);
	}
	if (menu->me_flags & ME_FREN)
		free((char *)menu);
}
