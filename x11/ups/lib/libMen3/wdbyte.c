/* wdbyte.c - Perq wdbyte emulation (should zap this) */

/*  Copyright 1991 John Bovey, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char Men3_wdbyte_c_sccsid[] = "@(#)wdbyte.c	1.9 20/5/92 (UKC)";

#include <sys/types.h>
#include <stdlib.h>
#include <string.h>

#include <local/wn.h>
#include "menu3.h"
#include "menu_priv.h"
#include "wdbyte.h"

#define NULL 0

/*  Emulation of the perq wdbyte(2) system call over wn.
 *  Deals correctly with variable width fonts, and the various
 *  return codes.
 *  It may differ from wdbyte() when two stop conditions arise
 *  simultaneously.
 */
/* ARGSUSED */
int
wn_wdbyte(wn,dbargp,clipp)
int wn;
struct DBCtl *dbargp;
Box_t *clipp;
{
	static char *buf;
	static unsigned buflen = 0;
	register font_t *font;
	const char *cptr, *start, *lim;
	int rv, xpos, max_x, chwidth, len;
	
	if (buflen == 0) {
		buflen = 128;
		buf = malloc((size_t)buflen + 1);
		if (buf == NULL)
			abort();
	}
	font = (dbargp->DBFont != NULL) ? dbargp->DBFont : wn_get_sysfont();
	start = dbargp->DBSrcString + dbargp->DBByteOffset;
	lim = dbargp->DBSrcString + dbargp->DBMaxByte;
	if (start >= lim)
		return(0);
	xpos = dbargp->DBX;
	max_x = dbargp->DBMaxX;
	for (cptr = start; ; cptr++) {
		if (*cptr >= 0 && *cptr < 32) {
			rv = HIT_CTL_CHAR;
			break;
		}
		chwidth = font->ft_width_tab[*cptr & 127];
		if (xpos + chwidth >= max_x) {
			rv = HIT_MAX_X;
			break;
		}
		if (cptr >= lim) {
			rv = HIT_MAXBYTE;
			break;
		}
		xpos += chwidth;
	}

	len = cptr - start;
	while (len > buflen) {
		buflen *= 2;
		free(buf);
		buf = malloc((size_t)buflen + 1);
	}
	(void) strncpy(buf, start, len);
	buf[len] = '\0';

	if (dbargp->DBScreen == 0) {
		wn_xputs(wn,font,buf,
				dbargp->DBX,dbargp->DBY,
				dbargp->DBFunc,WN_USE_BASELINE);
	}
	dbargp->DBX = xpos;
	dbargp->DBByteOffset += len;
	return(rv);
}
