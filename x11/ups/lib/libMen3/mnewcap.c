/* mnewcap.c - code to change menu captions on the fly */

/*  Copyright 1991 John Bovey, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char Men3_mnewcap_c_sccsid[] = "@(#)mnewcap.c	1.8 26/4/92 (UKC)";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <local/wn.h>
#include "menu3.h"
#include "menu_priv.h"

static int mchange MPROTO((MENU * menu, int oldrv, int newrv,
						const char *str, int fontno));

/*  Change the menu caption return value and font number of the menu
 *  buttons with return value oldrv. If str is NULL the caption is left
 *  unchanged, if newrv is zero the return value is unchanged and if
 *  fontno is zero the font number is left unchanged.
 *  The number of matched return values is returned.
 */
int
Mchange(md,oldrv,newrv,str,fontno)
int md, oldrv, newrv;
const char *str;
int fontno;
{
	int found;

	if ((md < 0) || (md >= MAXMEN)) {
		menerr = MBADMD;
		return(-1);
	}
	if (_menu_[md].om_root == NULL) {
		menerr = MNOTMEN;
		return(-1);
	}
	found = mchange(_menu_[md].om_root,oldrv,newrv,str,fontno);
	if (_menu_[md].om_last != NULL && found > 0)
		mshow(_menu_[md].om_root,0);
	return(found);
}

static int
mchange(menu,oldrv,newrv,str,fontno)
MENU * menu;
int oldrv, newrv;
const char *str;
int fontno;
{
	int found = 0;
	char *newcap;

	if (menu == NULL)
		return(0);
	if ((oldrv == menu->me_rv) && (menu->me_cap != NULL)) {
		if (str != NULL) {
			if (menu->me_flags & ME_FREC) {
				/* BUG: me_cap is "const char *", so
				 * we have to cast it.
				 */
				free((char *)menu->me_cap);
			}
			newcap = malloc(strlen(str) + 1);
			strcpy(newcap, str);
			menu->me_cap = newcap;
			menu->me_flags |= ME_FREC;
			menu->me_flags |= ME_REDRAW;
		}
		if (newrv != 0)
			menu->me_rv = newrv;
		if (fontno != 0) {
			menu->me_flags &= ~ME_FONT;
			menu->me_flags |= (fontno - 1) & ME_FONT;
			menu->me_flags |= ME_REDRAW;
		}
		found++;
	}
	found += mchange(menu->me_topleft,oldrv,newrv,str,fontno);
	found += mchange(menu->me_botrite,oldrv,newrv,str,fontno);
	return(found);
}
