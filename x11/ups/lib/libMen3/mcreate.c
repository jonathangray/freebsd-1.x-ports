/* mcreate.c - menu creation from static data */

/*  Copyright 1991 John Bovey, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char Men3_mcreate_c_sccsid[] = "@(#)mcreate.c	1.10 13/5/92 (UKC)";

#include <stdio.h>
#include <stdlib.h>

#include <local/wn.h>
#include "menu3.h"
#include "menu_priv.h"

static int result;

/* to do - make the line depth dependent on the fonts of this menu */
#define LDEPTH (16 + 4) /* the depth of one menu slot */

static void mcheck MPROTO((MENU * m));
static MENU *mcreate MPROTO((const char **caps, const int *rvs, const int *fonts,
				int ncaps, int ystart, int xend, int style,
				MENU *parent));

int
Mmake(captions,rvalues,fonts,style,width)
const char **captions;
const int *rvalues, *fonts;
int style, width;
{
	register i, j;
	MENU *m;

	for (i = 0; i < MAXMEN; i++)
		if (_menu_[i].om_root == NULL)
			break;
	if (i == MAXMEN) {
		menerr = MTABFL;
		return(-1);
	}
	for (j = 0; captions[j] != NULL; j++)
		;
	if (j == 0) {
		menerr = MBADARG;
		return(-1);
	}

	result = 1;
	m = mcreate(captions,rvalues,fonts,j,0,width,style,(MENU *)NULL);
	m->me_flags |= ME_FREE;
	if (style & MM_DRAG)
		m->me_flags |= ME_POPUP;

	_menu_[i].om_root = m;
	_menu_[i].om_md = i;
	_menu_[i].om_last = NULL;
	_menu_[i].om_fback[0] = MH_GREY;
	_menu_[i].om_fback[1] = MH_BLACK;
	_menu_[i].om_fback[2] = MH_GREY;
	_menu_[i].om_font[0] =
	_menu_[i].om_font[1] =
	_menu_[i].om_font[2] =
	_menu_[i].om_font[3] = Mstdfont();
	_menu_[i].om_wantbox = 1;
	momen(_menu_[i].om_root,&_menu_[i]);
	menerr = MENOK;
	return(i);
}

/*  insert the omen pointers
 */
void
momen(menu,omen)
MENU *menu;
struct omenst *omen;
{
	if (menu == NULL)
		return;
	menu->me_omen = omen;
	momen(menu->me_topleft,omen);
	momen(menu->me_botrite,omen);
}

static MENU *
mcreate(caps,rvs,fonts,ncaps,ystart,xend,style,parent)
const char **caps;
const int *rvs, *fonts;
int ncaps,ystart,xend,style;
MENU *parent;
{
	MENU * mp;

	mp = (MENU *)malloc(sizeof(MENU));
	mp->me_xstart = 0;
	mp->me_xend = xend;
	mp->me_ystart = ystart;
	mp->me_yend = ystart + ncaps * LDEPTH;
	mp->me_xcurs = 0;
	mp->me_ycurs = 0;
	mp->me_flags = ME_FREN;
	if (style & MM_LEFT)
		mp->me_flags |= ME_LEFT;
	mp->me_colour = 0;
	mp->me_pos = ystart + (ncaps / 2) * LDEPTH;
	mp->me_parent = parent;
	if (ncaps > 1) {
		mp->me_cap = NULL;
		mp->me_flags |= ME_VER;
		if (style & MM_NOLINE)
			mp->me_flags |= ME_BGLINE;
		mp->me_topleft = mcreate(caps,rvs,fonts,ncaps/2,ystart,
					xend,style,mp);
		if (rvs != NULL)
			rvs += ncaps/2;
		if (fonts != NULL)
			fonts += ncaps/2;
		mp->me_botrite = mcreate(caps + ncaps/2,rvs,fonts,
				ncaps - ncaps/2,mp->me_pos,xend,style,mp);
	} else {
		mp->me_topleft = NULL;
		mp->me_botrite = NULL;
		mp->me_cap = caps[0];
		mp->me_rv = (rvs != NULL) ? rvs[0] : result++;
		if (fonts != NULL)
			mp->me_flags |= (ME_FONT & (fonts[0] - 1));
	}

	return(mp);
}

void
Mcheck(md)
int md;
{
	mcheck(_menu_[md].om_root);
}

static void
mcheck(m)
MENU * m;
{
	if (m == NULL)
		return;
	printf("xstart=%d, xend=%d, ystart=%d, yend=%d, pos=%d, flags=%x\n",
	m->me_xstart,m->me_xend,m->me_ystart,m->me_yend,m->me_pos,m->me_flags);
	mcheck(m->me_topleft);
	mcheck(m->me_botrite);
}

