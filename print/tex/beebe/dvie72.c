/* -*-C-*- dvie72.c */
/*-->dvie72*/
/**********************************************************************/
/******************************* dvie72 *******************************/
/**********************************************************************/

/***********************************************************************
DVIE72 was  developed from  one  of the  dot-matrix printer  drivers  by
Marcus  Moehrmann  (EMAIL:   marcus%fkihh@unido.uucp).   The   following
remarks are his.

This is an implementation for  Epson and compatible printers.   Printers
with 9 needles are supported; those with 24 are not. I hope that I  have
only used the ESC/P-Standard.  The only documentation I  have had was  a
description for the NEC Pinwriter P6/P7,  but our printer is a  Logitec,
which I used for testing.

There is a difference in paper feeding, because the P6/P7-handbook  says
that

	<ESC> J (n)

performs n/180 inch paper feeding,  but our printer performs n/216  inch
paper feeding, which I think is the standard, so I have used this in the
driver.

For horizontal resolution I have used the Esc-sequences

	<ESC> * (m) (n1) (n2)		[m = 0, 1, 2, 3]

instead of

	<ESC> (x) (n1) (n2)		[x = 'K', 'L', 'Y', 'Z']

If this  will  not work  on  your  printer, change  the  definitions  of
'EPSON_BIT_IMAGE' and 'EPSON_MICRO_STEP'  and the variable  'resolution'
in procedure 'outline'.

Be careful with the reset command

	<ESC> @

I have done this before any printing, and then after about 1.5 pages the
printer stopped. The same works quite well on a second printer.

Specialities: There is a switch '-t' for twice-a-line printing, that is,
print one  line in  1/120  inch horizontal  spacing, return  print  head
without paper feeding,  perform a  1/240 inch  step and  then print  the
second line with 1/120 inch spacing.

***********************************************************************/

#include "dvihead.h"

/**********************************************************************/
/************************  Device Definitions  ************************/
/**********************************************************************/

/* All output-device-specific definitions go here.  This section must
be changed when modifying a dvi driver for use on a new device */

#undef EPSON
#define EPSON	1			/* conditional compilation flag */

#define VERSION_NO	"2.10 [experimental]"	/* DVI driver version number */

#undef HIRES
#define  HIRES		  0		/* 0 for 72-dpi version */

#if    HIRES
#define  DEVICE_ID	"Epson 9-pin family 240/216-dpi matrix printer"
					/* this string is printed at runtime */
#define OUTFILE_EXT	"eps"

#define  XDPI		240		/* horizontal dots/inch */
#define  YDPI		216		/* vertical dots/inch */

#else /* NOT HIRES */
#define  DEVICE_ID	"Epson 9-pin family 60/72-dpi matrix printer"
					/* this string is printed at runtime */

#define OUTFILE_EXT	"e72"

#define  XDPI		60		/* horizontal dots/inch */
#define  YDPI		72		/* vertical dots/inch */
#endif /* HIRES */

#define  BYTE_SIZE	  8		/* output file byte size */

#undef STDRES
#if    HIRES
#define STDRES  1			/* 0 for low-resolution devices */
#else /* NOT HIRES */
#define STDRES  0			/* 0 for low-resolution devices */
#endif /* HIRES */

#define  XPSIZE		8		/* horizontal paper size in inches */
#define  XSIZE		(((XDPI*XPSIZE+2*HOST_WORD_SIZE-1)/\
				(2*HOST_WORD_SIZE))*(2*HOST_WORD_SIZE))
					/* number of horizontal dots; */
					/* MUST BE multiple of */
					/* 2*HOST_WORD_SIZE */
#define  XWORDS		((XSIZE + HOST_WORD_SIZE - 1)/HOST_WORD_SIZE)
					/* number of words in rows  */
					/* of bitmap array */

#define  YPSIZE		11		/* vertical paper size in inches */
#define  YSIZE		(YDPI*YPSIZE)	/* number of vertical dots */

#if    HIRES
INT16 paperfeeding;			/* outstanding 1/216 inch */
					/* form-feedings */

BOOLEAN twice_a_line = FALSE;		/* this is runtime option '-t' */
					/* print in 1/120-inch-mode, */
					/* 2nd line shifted 1/240 inch */

					/* need single-arg outline() */
BOOLEAN micro_step;			/* used in outline() */
#define OUTLINE(v,flag) {micro_step = flag; outline(v);}
#endif /* HIRES */

/* The printer bit map. */

#define XBIT XWORDS
#define YBIT YSIZE

#if    (IBM_PC_LATTICE | IBM_PC_MICROSOFT | IBM_PC_WIZARD)
#undef SEGMEM
#define SEGMEM 1		/* ( ((long)XBIT * (long)YBIT) > 65536L ) */
#endif /* (IBM_PC_LATTICE | IBM_PC_MICROSOFT | IBM_PC_WIZARD) */

#define EPSON_ADV_PAPER		"\033J%c"	/* advance paper n/216 inch */
#define EPSON_BIT_IMAGE		"\033*%c%c%c"
#define EPSON_MICRO_STEP	OUTF2("\033*\003\001%c%c",'\000','\000')

#include "bitmap.h"

#include "main.h"
#include "abortrun.h"
#include "actfact.h"
#include "alldone.h"
#include "chargf.h"
#include "charpk.h"
#include "charpxl.h"
#include "clrbmap.h"
#include "clrrow.h"
#include "dbgopen.h"

/*-->devinit*/
/**********************************************************************/
/****************************** devinit *******************************/
/**********************************************************************/

void
devinit(argc, argv)		/* initialize device */
int argc;
char *argv[];
{
    (void)getbmap();
    if (runlengthcode && !quiet)
    {
	(void)fprintf(stderr, "[Run-length encoding of output file]");
	NEWLINE(stderr);
    }

#if    HIRES
    if (twice_a_line && !quiet)
    {
	(void)fprintf(stderr, "[Twice-a-line-printing for high resolution]");
	NEWLINE(stderr);
    }
#endif /* HIRES */

}

/*-->devterm*/
/**********************************************************************/
/****************************** devterm *******************************/
/**********************************************************************/

void
devterm()			/* terminate device */
{
}

#include "dispchar.h"
#include "dvifile.h"
#include "dviinit.h"
#include "dviterm.h"
#include "f20open.h"
#include "fatal.h"
#include "fillrect.h"
#include "findpost.h"
#include "fixpos.h"
#include "fontfile.h"
#include "fontsub.h"
#include "getbmap.h"
#include "getbytes.h"
#include "getfntdf.h"
#include "getpgtab.h"
#include "inch.h"
#include "initglob.h"
#include "loadchar.h"

/*-->makechar*/
/**********************************************************************/
/****************************** makechar ******************************/
/**********************************************************************/

char
makechar(p, mask)
register UNSIGN32 *p[];
register UNSIGN32 mask;
{
    register char c;

#if    SEGMEM
#define Q(n) p[n]		/* use precomputed pointers */
#else /* NOT SEGMEM */
#define Q(n) (p[0] - (n)*XBIT)	/* compute pointers on the fly */
#endif /* SEGMEM */

    c = '\000';				/* MSB controls top needle */

#if    HIRES
    if (*Q(0) & mask)
	c |= '\200';
    if (*Q(3) & mask)
	c |= '\100';
    if (*Q(6) & mask)
	c |= '\040';
    if (*Q(9) & mask)
	c |= '\020';
    if (*Q(12) & mask)
	c |= '\010';
    if (*Q(15) & mask)
	c |= '\004';
    if (*Q(18) & mask)
	c |= '\002';
    if (*Q(21) & mask)
	c |= '\001';
#else /* NOT HIRES */
    if (*Q(0) & mask)
	c |= '\200';
    if (*Q(1) & mask)
	c |= '\100';
    if (*Q(2) & mask)
	c |= '\040';
    if (*Q(3) & mask)
	c |= '\020';
    if (*Q(4) & mask)
	c |= '\010';
    if (*Q(5) & mask)
	c |= '\004';
    if (*Q(6) & mask)
	c |= '\002';
    if (*Q(7) & mask)
	c |= '\001';
#endif /* HIRES */

    return (c);
}

#include "movedown.h"
#include "moveover.h"
#include "moveto.h"
#include "nosignex.h"
#include "openfont.h"
#include "option.h"

#if    HIRES
/*-->outpaperfeed*/
/**********************************************************************/
/*************************** outpaperfeed *****************************/
/**********************************************************************/

void
outpaperfeed(count)		/* accumulate 1/216 inch paperfeedings */
INT16 count;			/* and write them out if count == 0 */
{

    register INT16 k;

    if (count)
	paperfeeding += count;
    else
    {
	for (k = paperfeeding / 255; k; --k)
	    OUTF(EPSON_ADV_PAPER, 255);
	if ((k = paperfeeding % 255) > 0)
	    OUTF(EPSON_ADV_PAPER, k);
	paperfeeding = 0;
    }
}
#endif /* HIRES */

/*-->outline*/
/**********************************************************************/
/****************************** outline *******************************/
/**********************************************************************/

void
outline(pline)
char *pline;
{
    register char* a;
    register char* b;
    register char* c;
    register INT16 left;
    INT16 len;
    INT16 linelength;
    char resolution;
    BYTE spacing;
    BYTE space_width;

#if    HIRES
    if (twice_a_line)			/* step 1/240 inch forward */
    {
	resolution = '\001';		/* 1/120 */
	space_width = 12;		/* 12/120 */
	linelength = XSIZE / 2;
    }
    else
    {
	resolution = '\003';		/* 1/240 */
	space_width = 24;		/* 24/240 */
	linelength = XSIZE;
    }
#else /* NOT HIRES */
    resolution = '\000';		/* 1/60 */
    space_width = 6;			/* 6/60 */
    linelength = XSIZE;
#endif /* HIRES */

    for ((left = linelength, c = pline + linelength - 1);
	 (*c == '\000') && (left > 0);
	 (--left, --c))			/* trim white space */
	;

    if (left == 0)
	return;		/* NEWLINE() is not needed for any paperfeeding */

#if    HIRES
    (void)outpaperfeed(0);		/* print accumulated paperfeedings */
#endif /* HIRES */

    /*******************************************************************
    We search for runs as follows.  "b" marks the beginning of unwritten
    data, and "a" anchors the beginning of a run which continues to just
    before "c".   The  run length  is  "c"-"a".  Only  runs  longer than
    "space_width" are taken, because we print spaces in text mode.  This
    implies that the run only consists of '\000' characters.

    If a long  enough run  is found,  then the  string "b"  .. "a"-1  is
    output since its length is now  known, followed by the run  encoding
    for the character at "a".  Then "a" and "b" are advanced to "c", and
    the scan continues.

    If the  run  beginning at  "a"  is too  short,  then "a"  is  simply
    advanced.

    Since the  character strings  take on  all values  on 0  .. 255,  we
    cannot store a termination marker, but must instead keep a  counter,
    "left", which is  decremented to  0 when the  end of  the string  is
    reached.

    ?????????????0000000000000000????????????????????
    ^            ^               ^
    |            |               |
    b            a               c

                 <-----run------>


    We cannot use fprintf %s because 0 bytes terminate string, so we are
    using putc for writing the line.
    *******************************************************************/

    if (runlengthcode && (left >= space_width))
    {
	for (a = b = pline; (left > 0); --left)
	{
	    for (c = a; (*a == *++c) && (*a == '\000') && left; --left)
		;			/* advance over run */
	    len = (INT16) (c - a);	/* "c" points past run */

	    if (len >= space_width)	/* output long run */
	    {
		if (a > b)		/* output previous string */
		{
#if    HIRES
		    if (micro_step)
		    {
			EPSON_MICRO_STEP;
			micro_step = FALSE;
		    }
#endif /* HIRES */
		    OUTF3(EPSON_BIT_IMAGE, resolution, ((int) (a - b)) % 256,
			  ((int) (a - b)) / 256);
		    for (; b < a; b++)
			OUTC(*b);
		}

		c -= len % space_width;	/* shorten run */
		left += len % space_width;

		for (spacing = (BYTE)(len / space_width); spacing; --spacing)
		    OUTC(' ');		/* output run */

		a = b = c;
	    }
	    else			/* ignore short run */
	    {
		++a;
		left += len - 1;
	    }
	}				/* end for() loop */

	if (a > b)			/* output remaining string */
	{
#if    HIRES
	    if (micro_step)
	    {
		EPSON_MICRO_STEP;
		micro_step = FALSE;
	    }
#endif /* HIRES */
	    OUTF3(EPSON_BIT_IMAGE,resolution,((int) (a - b)) % 256,
		  ((int) (a - b)) / 256);
	    for (; b < a; b++)
		OUTC(*b);
	}
    }
    else				/* no runlength coding */
    if (left > 0)
    {
#if    HIRES
	if (micro_step)
	{
	    EPSON_MICRO_STEP;
	    micro_step = FALSE;
	}
#endif /* HIRES */
	OUTF3(EPSON_BIT_IMAGE,resolution,left % 256,left / 256);
	for (c = pline; left; --left, c++)
	    OUTC(*c);
    }
    NEWLINE(plotfp);
}

#include "outrow.h"

/*-->prtbmap*/
/**********************************************************************/
/****************************** prtbmap *******************************/
/**********************************************************************/

void
prtbmap()
{
    register BYTE *c;			/* pointer into v8[][] and v8t[][][] */
    UNSIGN32 *p;			/* pointer into bitmap[][] */

#if    SEGMEM
#else /* NOT SEGMEM */
    UNSIGN32 *ptmp;			/* pointer into bitmap[][] */
#endif /* SEGMEM */

    register UNSIGN32 mask;		/* mask for single bit selection */
    register INT16 k;
    INT16 i;
    INT16 j;
    INT16 ybottom;
    INT16 ytop;

#if    HIRES
    BYTE v8[3][XSIZE];			/* vertical 8-bit raster encodings */
    BYTE v8t[3][2][XSIZE / 2];		/* vertical 8-bit raster encodings */
					/* (twice-a-line) */
    INT16 second = 0;			/* address second byte if not zero */

#undef Q
#if    SEGMEM
    UNSIGN32 *q[24];			/* pointers to 24 raster lines */
#define Q(n) q[n]			/* use precomputed pointers */
#else /* NOT SEGMEM */
#define Q(n) (p - (n)*XBIT)		/* compute pointers on the fly */
#endif /* SEGMEM */

    (void)clearerr(plotfp);

    ytop = YBIT - 1;

    k = -1;				/* find bottom non-zero raster */
    for (j = 0; (j < ytop) && (k < 0); ++j)	/* loop over raster lines */
    {

#if    IBM_PC_MICROSOFT
	for (k = XBIT - 1; ((k >= 0) && (*BITMAP(j, k) == 0)); --k)
	    ;				/* trim white space */

#else /* NOT IBM_PC_MICROSOFT */
	p = BITMAP(j, XBIT - 1);
	for (k = XBIT - 1; ((k >= 0) && (*p == 0)); --k)
	    --p;			/* trim white space */
#endif /* IBM_PC_MICROSOFT */

    }
    ybottom = (j / 24) * 24 + 23;

    paperfeeding = 0;			/* for outpaperfeed() */
    OUTF("\0333%c", 0);			/* no paperfeeding */

    for (j = ytop; (j >= ybottom); j -= 24)	/* loop over raster lines */
    {					/* in groups of 24 */

#if    SEGMEM
#else /* NOT SEGMEM */
	p = BITMAP(j, 0);		/* the j-th raster line */
#endif /* SEGMEM */

	if (twice_a_line)
	    c = &v8t[0][0][0];		/* vertical 8-bit encodings */
	else
	    c = &v8[0][0];		/* vertical 8-bit encodings */

	for (i = 0; i < XBIT; (++p, ++i))	/* loop over raster words */
	{
	    /* PCC-20 compiled (1 << (HOST_WORD_SIZE-1)) to 0, so do it the
	       hard way, arghh... */
	    mask = 1;
	    mask <<= (HOST_WORD_SIZE - 1);	/* to select leftmost bit */

#if    SEGMEM
	    for (k = 0; k < 24; ++k)	/* compute pointers to 24 rasters */
		q[k] = BITMAP(j - k, i);
#endif /* SEGMEM */

	    for (k = 0; k < HOST_WORD_SIZE; (++k)) /* loop over word bits */
	    {
		/* examine bits in 24 adjacent rows and build two 8-bit */
		/* character values, the first with rows n,n+3,...,n+21, */
		/* the second with rows n+1,n+4,...,n+22, the third with */
		/* rows n+2,n+5,...,n+23. The 2nd and 3rd characters are */
		/* then printed with 1 dot vertical spacing. */

		/* if twice_a_line is TRUE, the bytes will be written */
		/* alternating in v8t[x][0][y] and v8t[x][1][y] */

		if (twice_a_line)
		    second = (k % 2) * XSIZE / 2;

#if    SEGMEM
		*(c + second) = (BYTE) makechar(&q[0], mask);
		*(c + XSIZE + second) = (BYTE) makechar(&q[1], mask);
		*(c + 2 * XSIZE + second) = (BYTE) makechar(&q[2], mask);
#else /* NOT SEGMEM */
		*(c + second) = (BYTE) makechar(&p, mask);
		ptmp = p - XBIT;
		*(c + XSIZE + second) = (BYTE) makechar(&ptmp, mask);
		ptmp = p - 2 * XBIT;
		*(c + 2 * XSIZE + second) = (BYTE) makechar(&ptmp, mask);
#endif /* SEGMEM */

		if (twice_a_line)
		{
		    if (second)
			c++;		/* increment after second byte */
		}
		else
		    c++;

		mask >>= 1;		/* move masking bit right 1 position */
	    }				/* end loop over k */
	}				/* end loop over i */

	if (twice_a_line)
	{
	    OUTLINE((char *) &v8t[0][0][0], FALSE);
	    OUTLINE((char *) &v8t[0][1][0], TRUE);
	    (void)outpaperfeed(1);
	    OUTLINE((char *) &v8t[1][0][0], FALSE);
	    OUTLINE((char *) &v8t[1][1][0], TRUE);
	    (void)outpaperfeed(1);
	    OUTLINE((char *) &v8t[2][0][0], FALSE);
	    OUTLINE((char *) &v8t[2][1][0], TRUE);
	    (void)outpaperfeed(22);
	}
	else
	{
	    OUTLINE((char *) &v8[0][0], FALSE);
	    (void)outpaperfeed(1);
	    OUTLINE((char *) &v8[1][0], FALSE);
	    (void)outpaperfeed(1);
	    OUTLINE((char *) &v8[2][0], FALSE);
	    (void)outpaperfeed(22);
	}

    }					/* end loop over j */

    OUTS("\0332");			/* ESC 2 = normal paperfeeding */
    OUTC('\f');				/* eject page with FF */

    (void)fflush(plotfp);
    if (DISKFULL(plotfp))
	(void)fatal("prtbmap(): Output error -- disk storage probably full");
}

#else /* NOT HIRES */
    BYTE v8[XSIZE + 2];			/* vertical 8-bit raster encodings */

#undef Q
#if    SEGMEM
    UNSIGN32 *q[8];			/* pointers to 8 raster lines */
#define Q(n) q[n]			/* use precomputed pointers */
#else /* NOT SEGMEM */
#define Q(n) (p - (n)*XBIT)		/* compute pointers on the fly */
#endif /* SEGMEM */

    ytop = YBIT - 1;

    k = -1;				/* find bottom non-zero raster */
    for (j = 0; (j < ytop) && (k < 0); ++j)	/* loop over raster lines */
    {

#if    IBM_PC_MICROSOFT
	for (k = XBIT - 1; ((k >= 0) && (*BITMAP(j, k) == 0)); --k)
	    ;				/* trim white space */
#else /* NOT IBM_PC_MICROSOFT */
	p = BITMAP(j, XBIT - 1);
	for (k = XBIT - 1; ((k >= 0) && (*p == 0)); --k)
	    --p;			/* trim white space */
#endif /* IBM_PC_MICROSOFT */

    }					/* end loop over j */
    ybottom = (j / 8) * 8 + 7;

    OUTF("\0333%c", 24);		/* 24/216 inch paperfeeding */

    for (j = ytop; (j >= ybottom); j -= 8)	/* loop over raster lines */
    {					/* in groups of 8 */

#if    SEGMEM
#else /* NOT SEGMEM */
	p = BITMAP(j, 0);		/* the j-th raster line */
#endif /* SEGMEM */

	c = &v8[0];			/* vertical 8-bit encodings */
	for (i = 0; i < XBIT; (++p, ++i))	/* loop over raster words */
	{
	    /* PCC-20 compiled (1 << (HOST_WORD_SIZE-1)) to 0, so do it the
	    hard way, arghh... */
	    mask = 1;
	    mask <<= (HOST_WORD_SIZE - 1);	/* to select leftmost bit */

#if    SEGMEM
	    for (k = 0; k < 8; ++k)	/* compute pointers to 8 rasters */
		q[k] = BITMAP(j - k, i);
#endif /* SEGMEM */

	    for (k = 0; k < HOST_WORD_SIZE; (++c, ++k))/* loop over word bits */
	    {
		/* examine bits in 8 adjacent rows and build 8-bit */
		/* character value */

#if    SEGMEM
		*c = (BYTE) makechar(q, mask);
#else /* NOT SEGMEM */
		*c = (BYTE) makechar(&p, mask);
#endif /* SEGMEM */

		mask >>= 1;		/* move masking bit right 1 position */
	    }				/* end loop over k */
	}				/* end loop over i */
	--c;				/* last character encoded */

	(void)outline((char *) &v8[0]);
    }					/* end loop over j */

    OUTS("\0332");			/* ESC 2 = normal paperfeeding */
    OUTC('\f');				/* eject page with FF */

    (void)fflush(plotfp);
    if (DISKFULL(plotfp))
	(void)fatal("prtbmap(): Output error -- disk storage probably full");
}
#endif /* HIRES */

#include "prtpage.h"
#include "readfont.h"
#include "readgf.h"
#include "readpk.h"
#include "readpost.h"
#include "readpxl.h"
#include "reldfont.h"
#include "rulepxl.h"
#include "setchar.h"
#include "setfntnm.h"
#include "setrule.h"
#include "signex.h"
#include "skgfspec.h"
#include "skipfont.h"
#include "skpkspec.h"
#include "special.h"
#include "strchr.h"
#include "strcm2.h"
#include "strid2.h"
#include "strrchr.h"
#include "tctos.h"
#include "usage.h"
#include "warning.h"
