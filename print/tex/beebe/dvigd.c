/* -*-C-*- dvigd.c */
/*-->dvigd*/
/**********************************************************************/
/******************************* dvigd ********************************/
/**********************************************************************/

#include "dvihead.h"

/**********************************************************************/
/************************  Device Definitions  ************************/
/**********************************************************************/

/* All output-device-specific definitions go here.  This section must
be changed when modifying a dvi driver for use on a new device */

#undef GOLDENDAWNGL100
#define  GOLDENDAWNGL100   1		/* conditional compilation flag */

#define VERSION_NO	"2.10"		/* DVI driver version number */

#define  DEVICE_ID	"Golden Dawn Golden Laser 100 laser printer"
				/* this string is printed at runtime */

#define OUTFILE_EXT	"gd"

#define  BYTE_SIZE        7		/* output file byte size */

#undef STDRES
#define STDRES  1			/* to get standard font resolution */

#define  XDPI		200		/* horizontal dots/inch */
#define  XPSIZE		 7		/* horizontal paper size in inches */
#define  XSIZE		(((XDPI*XPSIZE+2*HOST_WORD_SIZE-1)/\
				(2*HOST_WORD_SIZE))*(2*HOST_WORD_SIZE))
					/* number of horizontal dots; */
					/* MUST BE multiple of */
					/* 2*HOST_WORD_SIZE */
#define  XWORDS		((XSIZE + HOST_WORD_SIZE - 1)/HOST_WORD_SIZE)
					/* number of words in rows  */
					/* of bitmap array */

#define  YDPI		XDPI		/* HP Laser Jet vertical dots/inch */
#define  YPSIZE		 9		/* vertical paper size in inches */
#define  YSIZE		(YDPI*YPSIZE)	/* number of vertical dots */

#define  V24SIZE	(4*XWORDS*HOST_WORD_SIZE)
					/* 4 chars encode 24 bits in each  */
					/* bit position of each word */

/* The printer bit map. */

#define XBIT XWORDS
#define YBIT YSIZE

#if    (IBM_PC_LATTICE | IBM_PC_MICROSOFT | IBM_PC_WIZARD)
#undef SEGMEM
#define SEGMEM 1 /* ( ((long)XBIT * (long)YBIT) > 65536L ) */
#endif

#undef MAXOPEN				/* default too big with large bitmap */
#define  MAXOPEN	 3		/* reduce number of open PXL files */

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
devinit(argc,argv)		/* initialize device */
int argc;
char *argv[];
{
    (void)getbmap();
    if (runlengthcode)
    {
	(void)fprintf(stderr,"[Run-length encoding of output file]");
	NEWLINE(stderr);
    }
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
#include "initglob.h"
#include "inch.h"
#include "loadchar.h"
#include "movedown.h"
#include "moveover.h"
#include "moveto.h"
#include "nosignex.h"
#include "openfont.h"
#include "option.h"

/*-->outline*/
/**********************************************************************/
/****************************** outline *******************************/
/**********************************************************************/

void
outline(pline)
char *pline;
{
    register INT16 left;
    register char *a,*b,*c;
    INT16 len;


#if    IBM_PC_MICROSOFT
    for ((left = (XSIZE + 5) / 6,
	c = (char *)normaddr(pline,(XSIZE + 5) / 6 - 1));
        (*c == '\100') && (left > 1);
        (--left,--c))                   /* trim white space but leave */
        ;                               /* at least 1 character in line */

#else
    for ((left = (XSIZE + 5) / 6, c = pline + (XSIZE + 5) / 6 - 1);
        (*c == '\100') && (left > 1);
        (--left,--c))                   /* trim white space but leave */
        ;                               /* at least 1 character in line */
#endif

    /*******************************************************************
    We search for runs as follows.  "b" marks the beginning of unwritten
    data, and "a" anchors the beginning of a run which continues to just
    before "c".   The  run length  is  "c"-"a".  Since  the  run  length
    encoding carries a penalty of additional output equal to the  length
    of the run length prefix (<ESC>nnnn), short runs are not so encoded.

    If a long  enough run  is found,  then the  string "b"  .. "a"-1  is
    output since its length is now  known, followed by the run  encoding
    for the character at "a".  Then "a" and "b" are advanced to "c", and
    the scan continues.

    If the  run  beginning at  "a"  is too  short,  then "a"  is  simply
    advanced.

    We keep a counter, "left", which is decremented to 0 when the end of
    the string is reached.

    ?????????????xxxxxxxxxxxxxxxx????????????????????
        ^        ^               ^
        |        |               |
        b        a               c

                 <-----run------>

    A run is encoded as <ESC>nnnnc, implying that the character c is  to
    be repeated  nnnn  times,  when  nnnn is  four  ASCII  digits.   All
    characters, except this  <ESC>nnnn prefix and  the terminal  newline
    character, are biased by  64 to move them  into the printable  range
    64..127 ('@'..DEL).

    *******************************************************************/
    if (runlengthcode)
    {
        for (a = b = pline; (left > 0); --left)
        {
            for (c = a; (*a == *++c) && left; --left)
                ;			/* advance over run */
            len = (INT16)(c-a);		/* "c" points past run */
            if (len > 6)		/* output long run */
	    {
                for ( ; b < a; b++)	/* output previous string */
                    OUTC(*b);
                OUTF2("\033%04d%c",len,*a);
		a = b = c;
	    }
            else			/* ignore short run */
	    {
		++a;
		left += len - 1;
	    }
        }
        for ( ; b < a; b++)		/* output remaining string */
            OUTC(*b);
    }
    else				/* no run-length encoding */
        OUTS(pline);

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

    /***********************************************************************
    Each raster line is broken into 6-bit groups from left to right  and
    encoded as  ASCII characters  in the  range 64..127  ('@'..DEL)  (by
    OR'ing with 64)  and these are  then sent to  procedure outline  for
    run-length encoding and output.  The  latter is necessary because  a
    full page needs 8.5 * 11 * 300 * 300 / 6 = 1,402,500 characters.
    ***********************************************************************/

    register char *c;                   /* pointer into raster[] */
    register UNSIGN32 *p;		/* pointer into bitmap[][] */
    register INT16 i,k;			/* inner loop indices */
    COORDINATE j,ybottom,ytop;
    char raster[XBIT*6+1];		/* 6-bit raster encodings */

#if    (HOST_WORD_SIZE == 32)
    UNSIGN32 word;
#endif

    (void)clearerr(plotfp);

    ytop = YBIT-1;

    k = -1;         /* find bottom non-zero raster */
    for (j = 0; (j < ytop) && (k < 0); ++j) /* loop over raster lines */
    {

#if    IBM_PC_MICROSOFT
        for (k = XBIT - 1; ((k >= 0) && (*BITMAP(j,k) == 0)); --k)
            ;			/* trim white space */
#else
        p = BITMAP(j,XBIT-1);
        for (k = XBIT - 1; ((k >= 0) && (*p == 0)); --k)
            --p;                /* trim white space */
#endif
    }
    ybottom = j;

    for (j = ytop; (j >= ybottom) ; j--)    /* loop over raster lines */
    {
        p = BITMAP(j,0);	/* the j-th raster line */
        c = &raster[0];         /* 6-bit encodings */
#if    (HOST_WORD_SIZE == 36)
        for (i = 0; i < XBIT; (++p,++i))      /* loop over raster words */
        {
            *c++ = 0100 | (((*p) >> 30) & 077);
            *c++ = 0100 | (((*p) >> 24) & 077);
            *c++ = 0100 | (((*p) >> 18) & 077);
            *c++ = 0100 | (((*p) >> 12) & 077);
            *c++ = 0100 | (((*p) >>  6) & 077);
            *c++ = 0100 | (((*p)      ) & 077);
        }
#else /* HOST_WORD_SIZE == 32 */
	for (i = 0; i <= k; i += 3)/* loop over trimmed raster */
	{ /* use machine-specific coding here for efficiency */
            *c++ = 0100 | (((*p) >> 26) & 077);
            *c++ = 0100 | (((*p) >> 20) & 077);
            *c++ = 0100 | (((*p) >> 14) & 077);
            *c++ = 0100 | (((*p) >>  8) & 077);
            *c++ = 0100 | (((*p) >>  2) & 077);
	    word = (*p) << 4;
	    ++p;
	    word |= (*p) >> 28;
	    *c++ = (char)(word & 077);
            *c++ = 0100 | (((*p) >> 22) & 077);
            *c++ = 0100 | (((*p) >> 16) & 077);
            *c++ = 0100 | (((*p) >> 10) & 077);
            *c++ = 0100 | (((*p) >>  4) & 077);
	    word = (*p) << 2;
	    ++p;
	    word |= (*p) >> 30;
	    *c++ = (char)(word & 077);
            *c++ = 0100 | (((*p) >> 24) & 077);
            *c++ = 0100 | (((*p) >> 18) & 077);
            *c++ = 0100 | (((*p) >> 12) & 077);
            *c++ = 0100 | (((*p) >>  6) & 077);
            *c++ = 0100 | (((*p)      ) & 077);
	    ++p;
	}
#endif

        for (i = (XSIZE + 5) / 6; i <= XBIT*6; i++)
             raster[i] = '\0';                  /* string terminator */
        outline(&raster[0]);
    }

    OUTC('\f');				/* eject page with FF */

    (void)fflush(plotfp);
    if (DISKFULL(plotfp))
	(void)fatal("Output error -- disk storage probably full");
}

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
