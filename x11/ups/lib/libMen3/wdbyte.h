/* wdbyte.h - header file for the Perq wdbyte emulation */

/*  Copyright 1991 John Bovey, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)wdbyte.h	1.4 26/4/92 (UKC) */

#ifndef __STDC__
#define const
#endif

struct DBCtl                   /*  WDBYTE control block */
{
        short   DBX            ;  /* destination X co-ord	     	*/
        short   DBByteOffset   ;  /* start char offset from String   	*/
        short   DBFunc         ;  /* rasterop function             	*/
        short   DBY            ;  /* destination Y co-ord of baseline 	*/
        short   DBMaxX         ;  /* max X co-ord  for whole chars 	*/
        short   DBMaxByte      ;  /* max char offset from String	*/
        const char   *DBSrcString    ;  /* source string                 	*/
        int    *DBScreen       ;  /* Base of dest area (0 = window)	*/
        /* struct font */font_t *DBFont ;  /* address of font              */
	short	DBDstInc       ;  /* scan line increment		*/
};
	/* NB both MaxX and MaxByte are defined as 1 greater than the 	*/
	/*    last allowed value, ie as lengths if starting from zero.	*/

#define HIT_MAXBYTE 0
#define HIT_MAX_X 1
#define HIT_CTL_CHAR 2

int wn_wdbyte MPROTO((int fd, struct DBCtl *dbargp, Box_t *clipp));
