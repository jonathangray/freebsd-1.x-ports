/* mstdfont.c - Mstdfont code and data */

/*  Copyright 1991 John Bovey, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char Men3_mstdfont_c_sccsid[] = "@(#)mstdfont.c	1.6 5/5/91 (UKC)";

#include <local/wn.h>
#include "menu3.h"

#ifndef NULL
#define NULL 0
#endif

#define MXSTDFONT	"-*-courier-bold-r-*-*-*-140-*-*-*-*-*-*"
#define MSUNSTDFONT	"/usr/lib/fonts/fixedwidthfonts/screen.b.12"

static font_t *Stdfont = NULL;

font_t *
Mstdfont()
{
	if (Stdfont == NULL) {
		switch (wn_get_wm_type()) {
		case WN_X11:
			Stdfont = wn_open_font(MXSTDFONT);
			break;
		case WN_SUNVIEW:
			Stdfont = wn_open_font(MSUNSTDFONT);
			break;
		default:
			/* Just leave stdfont as NULL */
			break;
		}
	}

	if (Stdfont == NULL)
		Stdfont = wn_get_sysfont();

	return(Stdfont);
}

void
Msetstdfont(font)
font_t *font;
{
	Stdfont = font;
}
