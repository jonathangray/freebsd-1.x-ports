/*
 * The routines in this file provide support for the IBM-PC and other
 * compatible terminals. The routines access graphics RAM directly to do
 * screen output. They utilise the zortech 'disp' package and make no attempt
 * to be portable between compilers.
 * Supported monitor cards include CGA, MONO, EGA and VGA.
 *
 *	Differs from standard "ibmpc.c" in that the Zortech display library
 *  is called whenever possible.
 *  Modifications by Pete Ruczynski (pjr).
 * $Log: z_ibmpc.c,v $
 * Revision 1.1  1994/02/01 03:29:45  jkh
 * Initial revision
 *
 * Revision 1.6  1993/08/05  14:29:12  pgf
 * tom's 3.57 changes
 *
 * Revision 1.5  1993/07/27  18:06:20  pgf
 * see tom's 3.56 CHANGES entry
 *
 * Revision 1.4  1993/05/04  17:05:14  pgf
 * see tom's CHANGES, 3.45
 *
 * Revision 1.3  1993/04/01  12:53:33  pgf
 * removed redundant includes and declarations
 *
 * Revision 1.2  1992/08/20  23:40:48  foxharp
 * typo fixes -- thanks, eric
 *
 * Revision 1.1  1992/07/01  16:57:45  foxharp
 * Initial revision
 *
 *
 */

#define	termdef	1			/* don't define "term" external */

#include	"estruct.h"
#include	"edef.h"

#if ZIBMPC

#include	<disp.h>

#define NROW	50			/* Max Screen size.		*/
#define NCOL    80                      /* Edit if you want to.         */
#define	MARGIN	8			/* size of minimim margin and	*/
#define	SCRSIZ	64			/* scroll size for extended lines */
#define	NPAUSE	200			/* # times thru update to pause */
#define	SPACE	32			/* space character		*/

#define	SCADC	0xb8000000L		/* CGA address of screen RAM	*/
#define	SCADM	0xb0000000L		/* MONO address of screen RAM	*/
#define SCADE	0xb8000000L		/* EGA address of screen RAM	*/

#define MONOCRSR 0x0B0D			/* monochrome cursor	    */
#define CGACRSR 0x0607			/* CGA cursor		    */
#define EGACRSR 0x0709			/* EGA cursor		    */

#define	CDCGA	0			/* color graphics card		*/
#define	CDMONO	1			/* monochrome text card		*/
#define	CDEGA	2			/* EGA color adapter		*/
#define	CDVGA	3			/* VGA color adapter		*/
#define	CDSENSE	9			/* detect the card type		*/

#define NDRIVE	4			/* number of screen drivers	upped to 4 (vga) */

extern int zibmtype;		/* pjr - what to do about screen resolution */


int dtype = -1;			/* current display type		*/
long scadd;			/* address of screen ram	*/
int *scptr[NROW];		/* pointer to screen lines	*/
UINT sline[NCOL];		/* screen line image		*/
extern union REGS rg;		/* cpu register for use of DOS calls */

extern  void	ttopen();       /* Forward references.          */
extern  int		ttgetc();
extern  void	ttputc();
extern  void	ttflush();
extern  void	ttclose();

extern  int	zibmmove();
extern  int	zibmeeol();
extern  int	zibmeeop();
extern  int	zibmbeep();
extern  int	zibmopen();
extern	int	zibmrev();
extern	int	zibmcres();
extern	int	zibmclose();
extern	int	zibmputc();
extern	int	zibmkopen();
extern	int	zibmkclose();

#if	COLOR
extern	int	zibmfcol();
extern	int	zibmbcol();

int	cfcolor = -1;		/* current forground color */
int	cbcolor = -1;		/* current background color */
int	ctrans[] =		/* ansi to zibm color translation table */
	{0, 4, 2, 6, 1, 5, 3, 7};
#endif

/*
 * Standard terminal interface dispatch table. Most of the fields point into
 * "termio" code.
 */
TERM    term    = {
	NROW-1,
	NROW-1,
	NCOL,
	NCOL,
	MARGIN,
	SCRSIZ,
	NPAUSE,
	zibmopen,
	zibmclose,
	zibmkopen,
	zibmkclose,
	ttgetc,
	zibmputc,
	ttflush,
	zibmmove,
	zibmeeol,
	zibmeeop,
	zibmbeep,
	zibmrev,
	zibmcres
#if	COLOR
	, zibmfcol,
	zibmbcol
#endif
};

#if	COLOR
zibmfcol(color)		/* set the current output color */

int color;	/* color to set */

{
	cfcolor = ctrans[color];
}

zibmbcol(color)		/* set the current background color */

int color;	/* color to set */

{
	cbcolor = ctrans[color];
}
#endif


/*
 * zibmmove
 *
 * set cursor position
 */
/*****************************************************************************/
int
zibmmove(row, col)
/*****************************************************************************/
{
	disp_move(row, col);
	disp_flush();
} /* end of zibmmove */


/*
 * zibmeeol
 *
 * erase to the end of the line
 */
/*****************************************************************************/
int
zibmeeol(void)
/*****************************************************************************/
{
	disp_eeol();
} /* end of zibmeeol */


/*
 * zibmputc
 *
 * put a character at the current cursor position in the current colors
 */
/*****************************************************************************/
int
zibmputc(ch)
/*****************************************************************************/
int ch;
{
	disp_putc(ch);
	disp_flush();
} /* end of zibmputc */


/*
 * zibmeeop
 *
 * erase from the current position to the end of the screen
 */
/*****************************************************************************/
int
zibmeeop(void)
/*****************************************************************************/
{
	disp_eeop();
} /* end of zibmeeop */


/*
 * zibmrev
 *
 * change reverse video state
 */
/*****************************************************************************/
int
zibmrev()
/*****************************************************************************/
{
	/* This never gets used under the IBM-PC driver */
} /* end of zibmrev */


int set43 = TRUE;		/* pjr - try and force 43/50 line mode first */

/*
 * zibmcres
 *
 * change screen resolution
 */
/*****************************************************************************/
int
zibmcres(res)
char	*res;
/*****************************************************************************/
{
	int	type = atoi(res);
	union {
		long laddr;	/* long form of address */
		short *paddr;	/* pointer form of address */
	} addr;
	int i;

	disp_close();

	if (set43)
	{
		disp_set43();
		disp_setcursortype(DISP_CURSORUL);
	}

	disp_open();

	/*
	 * this bit is all about getting the base address for the scwrite
	 * routine, needed because its faster than disp_puts()!
	 */
	scadd = (long)disp_base;
	scadd = scadd << 16;

	/* initialize the screen pointer array */
	addr.laddr = scadd;
	for (i = 0; i < NROW; i++) {
		scptr[i] = addr.paddr + NCOL * i;
	}

	/* set screen size */
	newscreensize(disp_numrows, term.t_ncol);
	disp_move(disp_numrows-1, 0);
	disp_flush();

	dtype = type;	/* history */

} /* end of zibmcres */


/*
 * spal
 *
 * reset the palette registers
 */
/*****************************************************************************/
int
spal()
/*****************************************************************************/
{
	/* nothing here now..... */
} /* end of spal */


/*
 * zibmbeep
 *
 * make a beep noise, disabled because its annoying!
 */
/*****************************************************************************/
int
zibmbeep()
/*****************************************************************************/
{
#if	MWC86
	putcnb(BEL);
#else
	bdos(6, BEL, 0);	/* annoying!! */
#endif
} /* end of zibmbeep */


int zibmtype;		/* pjr - what to do about screen resolution */
/*
 * zibmopen
 *
 * open the screen
 */
/*****************************************************************************/
int
zibmopen()
/*****************************************************************************/
{
	revexist = TRUE;
	ttopen();
	disp_open();
} /* end of zibmopen */


/*
 * zibmclose
 *
 * close the display
 */
/*****************************************************************************/
int
zibmclose()
/*****************************************************************************/
{
	disp_close();
	if (set43)
	{
		disp_reset43();
	}
} /* end of zibmclose */


/*
 * scwrite
 *
 * write a line out
 * Note that this is the original code as its faster than the disp package
 * equivalents!!
 */
/*****************************************************************************/
void
scwrite(row, col, nchar, outstr, forg, bacg)
/*****************************************************************************/
int row, col, nchar;		/* row,col of screen to place nchars of outstr on */
char *outstr;		/* string to write out (must be term.t_ncol long) */
int forg;		/* forground color of string to write */
int bacg;		/* background color */
{
	UINT attr;	/* attribute byte mask to place in RAM */
	UINT *lnptr;	/* pointer to the destination line */
	int i;

	/* build the attribute byte and setup the screen pointer */
#if	COLOR
	if (dtype != CDMONO)
		attr = ((ctrans[bacg] & 15) << 4) | (ctrans[forg] & 15);
	else
#endif
	 attr = ((bacg & 15) << 4) | (forg & 15);
	attr <<= 8;
	lnptr = &sline[0];
	for (i=0; i<nchar; i++)
		*lnptr++ = (outstr[i] & 255) | attr;

	if (flickcode && (dtype == CDCGA)) {
		/* wait for vertical retrace to be off */
		while ((inp(0x3da) & 8))
			;
	
		/* and to be back on */
		while ((inp(0x3da) & 8) == 0)
			;
	}

	/* and send the string out */
	movmem(&sline[0], scptr[row]+col, nchar*2);
} /* end of scwrite */


/*
 * zibmsetcur
 *
 * change cursor
 * can be either underline param == TRUE
 * or half block param == FALSE
 * used to indicate mode we are in
 */
/*****************************************************************************/
void
zibmsetcur(param)
/*****************************************************************************/
int param;
{
	if (param)
		disp_setcursortype(DISP_CURSORUL);
	else
		disp_setcursortype(DISP_CURSORHALF);

} /* end of zibmsetcur */


#if	FLABEL
fnclabel(f, n)		/* label a function key */

int f,n;	/* default flag, numeric argument [unused] */

{
	/* on machines with no function keys...don't bother */
	return(TRUE);
}
#endif

zibmkopen()	/* open the keyboard */

{
}

zibmkclose()	/* close the keyboard */

{
}

#else
zibmhello()
{
}
#endif
