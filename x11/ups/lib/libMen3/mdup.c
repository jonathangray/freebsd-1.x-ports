/* mdup.c - Mdup code */

/*  Copyright 1991 John Bovey, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char Men3_mdup_c_sccsid[] = "@(#)mdup.c	1.9 26/4/92 (UKC)";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <local/wn.h>
#include "menu3.h"
#include "menu_priv.h"

static MENU *mdup MPROTO((const MENU *menu));

/*  Duplicate the menu described by menu descriptor md and return the
 *  descriptor for the new menu.
 */
int
Mdup(md)
int md;
{
	register dupmd, i;

	if ((md < 0) || (md >= MAXMEN)) {
		menerr = MBADMD;
		return(-1);
	}
	if (_menu_[md].om_root == NULL) {
		menerr = MNOTMEN;
		return(-1);
	}
	for (dupmd = 0; dupmd < MAXMEN; dupmd++)
		if (_menu_[dupmd].om_root == NULL)
			break;
	if (dupmd == MAXMEN) {
		menerr = MTABFL;
		return(-1);
	}
	_menu_[dupmd].om_root = mdup(_menu_[md].om_root);
	_menu_[dupmd].om_root->me_parent = NULL;
	_menu_[dupmd].om_last = NULL;
	_menu_[dupmd].om_rect = NULL;
	_menu_[dupmd].om_md = dupmd;
	for (i = 0; i < 3; i++)
		_menu_[dupmd].om_fback[i] = _menu_[md].om_fback[i];
	for (i = 0; i < 4; i++)
		_menu_[dupmd].om_font[i] = _menu_[md].om_font[i];
	_menu_[dupmd].om_wantbox = _menu_[md].om_wantbox;
	momen(_menu_[dupmd].om_root,&_menu_[dupmd]);
	menerr = MENOK;
	return(dupmd);
}

/*  recursive menu duplicating function
 */
static MENU *
mdup(menu)
const MENU *menu;
{
	MENU *dupmenu;
	char *newcap;

	dupmenu = (MENU *)malloc(sizeof(MENU));
	*dupmenu = *menu;
	if (menu->me_cap != NULL) {
		newcap = malloc(strlen(menu->me_cap) + 1);
		strcpy(newcap, menu->me_cap);
		dupmenu->me_cap = newcap;
	}
	dupmenu->me_flags |= (ME_FREC | ME_FREN);
	if (menu->me_topleft != NULL) {
		dupmenu->me_topleft = mdup(menu->me_topleft);
		dupmenu->me_topleft->me_parent = dupmenu;
	}
	if (menu->me_botrite != NULL) {
		dupmenu->me_botrite = mdup(menu->me_botrite);
		dupmenu->me_botrite->me_parent = dupmenu;
	}
	return(dupmenu);
}
