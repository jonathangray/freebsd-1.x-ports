/*
 * DVI previewer for X.
 *
 * Eric Cooper, CMU, September 1985.
 *
 * Code derived from dvi-imagen.c.
 *
 * Modification history:
 * 1/1986	Modified for X.10	--Bob Scheifler, MIT LCS.
 * 7/1988	Modified for X.11	--Mark Eichin, MIT
 * 12/1988	Added 'R' option, toolkit, magnifying glass
 *					--Paul Vojta, UC Berkeley.
 * 2/1989	Added tpic support	--Jeffrey Lee, U of Toronto
 * 4/1989	Modified for System V	--Donald Richardson, Clarkson Univ.
 * 3/1990	Added VMS support	--Scott Allendorf, U of Iowa
 * 7/1990	Added reflection mode	--Michael Pak, Hebrew U of Jerusalem
 * 1/1992	Added greyscale code	--Till Brychcy, Techn. Univ. Muenchen
 *					  and Lee Hetherington, MIT
 *
 *	Compilation options:
 *	SYSV	compile for System V
 *	VMS	compile for VMS
 *	X10	compile for X10
 *	NOTOOL	compile without toolkit (X11 only)
 *	BUTTONS	compile with buttons on the side of the window (needs toolkit)
 *	MSBITFIRST	store bitmaps internally with most significant bit first
 *	BMSHORT	store bitmaps in shorts instead of bytes
 *	BMLONG	store bitmaps in longs instead of bytes
 *	ALTFONT	default for -altfont option
 *	A4	use European size paper
 *	TEXXET	support reflection dvi codes (right-to-left typesetting)
 *	GREY	use grey levels to shrink fonts
 */

#include "config.h"
#include <kpathsea/c-ctype.h>
#include <kpathsea/c-vararg.h>
#include "dvi.h"

static	struct frame	frame0;		/* dummy head of list */
#ifdef	TEXXET
static	struct frame	*scan_frame;	/* head frame for scanning */
#endif

#ifndef	DVI_BUFFER_LEN
#define	DVI_BUFFER_LEN	512
#endif

static	ubyte	dvi_buffer[DVI_BUFFER_LEN];
static	struct frame	*current_frame;

#ifndef	TEXXET
#define	DIR	1
#else
#define	DIR	currinf.dir
#endif

/*
 *	Explanation of the following constant:
 *	offset_[xy]   << 16:	margin (defaults to one inch)
 *	shrink_factor << 16:	one pixel page border
 *	shrink_factor << 15:	rounding for pixel_conv
 */
#define OFFSET_X	(offset_x << 16) + (shrink_factor * 3 << 15)
#define OFFSET_Y	(offset_y << 16) + (shrink_factor * 3 << 15)

#ifndef	BMLONG
#ifndef	BMSHORT
unsigned char	bit_masks[9] = {
	0x0,	0x1,	0x3,	0x7,
	0xf,	0x1f,	0x3f,	0x7f,
	0xff
};
#else	/* BMSHORT */
unsigned short	bit_masks[17] = {
	0x0,	0x1,	0x3,	0x7,
	0xf,	0x1f,	0x3f,	0x7f,
	0xff,	0x1ff,	0x3ff,	0x7ff,
	0xfff,	0x1fff,	0x3fff,	0x7fff,
	0xffff
};
#endif	/* BMSHORT */
#else	/* BMLONG */
unsigned long	bit_masks[33] = {
	0x0,		0x1,		0x3,		0x7,
	0xf,		0x1f,		0x3f,		0x7f,
	0xff,		0x1ff,		0x3ff,		0x7ff,
	0xfff,		0x1fff,		0x3fff,		0x7fff,
	0xffff,		0x1ffff,	0x3ffff,	0x7ffff,
	0xfffff,	0x1fffff,	0x3fffff,	0x7fffff,
	0xffffff,	0x1ffffff,	0x3ffffff,	0x7ffffff,
	0xfffffff,	0x1fffffff,	0x3fffffff,	0x7fffffff,
	0xffffffff
};
#endif	/* BMLONG */

#ifdef	VMS
#define	off_t	int
#endif
extern	off_t	lseek();

#ifndef	SEEK_SET	/* if <unistd.h> is not provided (or for <X11R5) */
#define	SEEK_SET	0
#define	SEEK_CUR	1
#define	SEEK_END	2
#endif

static	void	draw_part();

/*
 *	Byte reading routines for dvi file.
 */

#define	xtell(pos)	(lseek(fileno(dvi_file), 0L, SEEK_CUR) - \
			    (currinf.end - (pos)))

static	ubyte
xxone()
{
	if (currinf.virtual) {
	    ++currinf.pos;
	    return EOP;
	}
	currinf.end = dvi_buffer +
	    read(fileno(dvi_file), (char *) (currinf.pos = dvi_buffer),
		DVI_BUFFER_LEN);
	return currinf.end > dvi_buffer ? *(currinf.pos)++ : EOF;
}

#define	xone()  (currinf.pos < currinf.end ? *(currinf.pos)++ : xxone())

static	unsigned long
xnum(size)
	register ubyte size;
{
	register long x = 0;

	while (size--) x = (x << 8) | xone();
	return x;
}

static	long
xsnum(size)
	register ubyte size;
{
	register long x;

#ifdef	__STDC__
	x = (signed char) xone();
#else
	x = xone();
	if (x & 0x80) x -= 0x100;
#endif
	while (--size) x = (x << 8) | xone();
	return x;
}

#define	xsfour()	xsnum(4)

static	void
xskip(offset)
	long	offset;
{
	currinf.pos += offset;
	if (!currinf.virtual && currinf.pos > currinf.end)
	    (void) lseek(fileno(dvi_file), (long) (currinf.pos - currinf.end),
		SEEK_CUR);
}

#if	NeedVarargsPrototypes
static	NORETURN void
tell_oops(_Xconst char *message, ...)
#else
/* VARARGS */
static	NORETURN void
tell_oops(va_alist)
	va_dcl
#endif
{
#if	!NeedVarargsPrototypes
	_Xconst char *message;
#endif
	va_list	args;

	Fprintf(stderr, "%s: ", prog);
#if	NeedVarargsPrototypes
	va_start(args, message);
#else
	va_start(args);
	message = va_arg(args, _Xconst char *);
#endif
	(void) vfprintf(stderr, message, args);
	va_end(args);
	if (currinf.virtual)
	    Fprintf(stderr, " in virtual font %s\n", currinf.virtual->fontname);
	else
	    Fprintf(stderr, ", offset %ld\n", xtell(currinf.pos - 1));
	exit(1);
}


/*
 *	Code for debugging options.
 */

static	void
print_bitmap(bitmap)
	register struct bitmap *bitmap;
{
	register BMUNIT *ptr = (BMUNIT *) bitmap->bits;
	register int x, y, i;

	if (ptr == NULL) oops("print_bitmap called with null pointer.");
	Printf("w = %d, h = %d, bytes wide = %d\n",
	    bitmap->w, bitmap->h, bitmap->bytes_wide);
	for (y = 0; y < bitmap->h; ++y) {
	    for (x = bitmap->bytes_wide; x > 0; x -= BYTES_PER_BMUNIT) {
#ifndef	MSBITFIRST
		for (i = 0; i < BITS_PER_BMUNIT; ++i)
#else
		for (i = BITS_PER_BMUNIT - 1; i >= 0; --i)
#endif
		    Putchar((*ptr & (1 << i)) ? '@' : ' ');
		++ptr;
	    }
	    Putchar('\n');
	}
}

static	void
print_char(ch, g)
	ubyte ch;
	struct glyph *g;
{
	Printf("char %d", ch);
	if (ISPRINT(ch))
	    Printf(" (%c)", ch);
	Putchar('\n');
	Printf("x = %d, y = %d, dvi = %ld\n", g->x, g->y, g->dvi_adv);
	print_bitmap(&g->bitmap);
}

static	_Xconst	char	*dvi_table1[] = {
	"SET1", NULL, NULL, NULL, "SETRULE", "PUT1", NULL, NULL,
	NULL, "PUTRULE", "NOP", "BOP", "EOP", "PUSH", "POP", "RIGHT1",
	"RIGHT2", "RIGHT3", "RIGHT4", "W0", "W1", "W2", "W3", "W4",
	"X0", "X1", "X2", "X3", "X4", "DOWN1", "DOWN2", "DOWN3",
	"DOWN4", "Y0", "Y1", "Y2", "Y3", "Y4", "Z0", "Z1",
	"Z2", "Z3", "Z4"};

static	_Xconst	char	*dvi_table2[] = {
	"FNT1", "FNT2", "FNT3", "FNT4", "XXX1", "XXX2", "XXX3", "XXX4",
	"FNTDEF1", "FNTDEF2", "FNTDEF3", "FNTDEF4", "PRE", "POST", "POSTPOST",
	"SREFL", "EREFL", NULL, NULL, NULL, NULL};

static	void
print_dvi(ch)
	ubyte ch;
{
	_Xconst	char	*s;

	Printf("%4d %4d ", PXL_H, PXL_V);
	if (ch <= SETCHAR0 + 127) {
	    Printf("SETCHAR%-3d", ch - SETCHAR0);
	    if (ISPRINT(ch))
		Printf(" (%c)", ch);
	    Putchar('\n');
	    return;
	}
	else if (ch < FNTNUM0) s = dvi_table1[ch - 128];
	else if (ch <= FNTNUM0 + 63) {
	    Printf("FNTNUM%d\n", ch - FNTNUM0);
	    return;
	}
	else s = dvi_table2[ch - (FNTNUM0 + 64)];
	if (s) Puts(s);
	else
	    tell_oops("unknown op-code %d", ch);
}


/*
 *	Count the number of set bits in a given region of the bitmap
 */

char	sample_count[]	= {0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4};

static	int
sample(bits, bytes_wide, bit_skip, w, h)
	BMUNIT	*bits;
	int	bytes_wide, bit_skip, w, h;
{
	BMUNIT	*ptr, *endp;
	register BMUNIT *cp;
	int	bits_left;
	register int n, bit_shift, wid;

	ptr = bits + bit_skip / BITS_PER_BMUNIT;
	endp = ADD(bits, h * bytes_wide);
	bits_left = w;
#ifndef	MSBITFIRST
	bit_shift = bit_skip % BITS_PER_BMUNIT;
#else
	bit_shift = BITS_PER_BMUNIT - bit_skip % BITS_PER_BMUNIT;
#endif
	n = 0;
	while (bits_left) {
#ifndef	MSBITFIRST
	    wid = BITS_PER_BMUNIT - bit_shift;
#else
	    wid = bit_shift;
#endif
	    if (wid > bits_left) wid = bits_left;
	    if (wid > 4) wid = 4;
#ifdef	MSBITFIRST
	    bit_shift -= wid;
#endif
	    for (cp = ptr; cp < endp; cp = ADD(cp, bytes_wide))
		n += sample_count[(*cp >> bit_shift) & bit_masks[wid]];
#ifndef	MSBITFIRST
	    bit_shift += wid;
	    if (bit_shift == BITS_PER_BMUNIT) {
		bit_shift = 0;
		++ptr;
	    }
#else
	    if (bit_shift == 0) {
		bit_shift = BITS_PER_BMUNIT;
		++ptr;
	    }
#endif
	    bits_left -= wid;
	}
	return n;
}

#ifdef	GREY
static	void
shrink_glyph_grey(g)
	register struct glyph *g;
{
	int	rows_left, rows, init_cols, cols_left;
	register int	cols;
	int	x, y;
	long	thesample;
	BMUNIT	*old_ptr;

	/* These machinations ensure that the character is shrunk according to
	   its hot point, rather than its upper left-hand corner. */
	g->x2 = g->x / shrink_factor;
	init_cols = g->x - g->x2 * shrink_factor;
	if (init_cols <= 0) init_cols += shrink_factor;
	else ++g->x2;
	g->bitmap2.w = g->x2 + ROUNDUP(g->bitmap.w - g->x, shrink_factor);
	/* include row zero with the positively numbered rows */
	cols = g->y + 1; /* spare register variable */
	g->y2 = cols / shrink_factor;
	rows = cols - g->y2 * shrink_factor;
	if (rows <= 0) {
	    rows += shrink_factor;
	    --g->y2;
	}
	g->bitmap2.h = g->y2 + ROUNDUP(g->bitmap.h - cols, shrink_factor) + 1;

	g->image2 = XCreateImage(DISP, DefaultVisualOfScreen(SCRN),
				 DefaultDepthOfScreen(SCRN),
				 ZPixmap, 0, (char *) NULL,
				 g->bitmap2.w, g->bitmap2.h,
				 BITS_PER_BMUNIT, 0);
	g->pixmap2 = g->image2->data = xmalloc((unsigned)
			g->image2->bytes_per_line * g->bitmap2.h,
			"character pixmap");

	old_ptr = (BMUNIT *) g->bitmap.bits;
	rows_left = g->bitmap.h;
	y = 0;
	while (rows_left) {
	    x = 0;
	    if (rows > rows_left) rows = rows_left;
	    cols_left = g->bitmap.w;
	    cols = init_cols;
	    while (cols_left) {
		if (cols > cols_left) cols = cols_left;

		thesample = sample(old_ptr, g->bitmap.bytes_wide,
			g->bitmap.w - cols_left, cols, rows);
		XPutPixel(g->image2, x, y, pixeltbl[thesample]);

		cols_left -= cols;
		cols = shrink_factor;
		x++;
	    }
	    *((char **) &old_ptr) += rows * g->bitmap.bytes_wide;
	    rows_left -= rows;
	    rows = shrink_factor;
	    y++;
	}

	while (y < g->bitmap2.h) {
	    for (x = 0; x < g->bitmap2.w; x++)
		XPutPixel(g->image2, x, y, *pixeltbl);
	    y++;
	}

	g->y2 = g->y / shrink_factor;
}
#endif	/* GREY */

static	void
shrink_glyph(g)
	register struct glyph *g;
{
	int shrunk_bytes_wide, shrunk_height;
	int rows_left, rows, init_cols, cols_left;
	register int cols;
	BMUNIT *old_ptr, *new_ptr;
	register BMUNIT m, *cp;
	int min_sample = shrink_factor * shrink_factor * density / 100;

	/* These machinations ensure that the character is shrunk according to
	   its hot point, rather than its upper left-hand corner. */
	g->x2 = g->x / shrink_factor;
	init_cols = g->x - g->x2 * shrink_factor;
	if (init_cols <= 0) init_cols += shrink_factor;
	else ++g->x2;
	g->bitmap2.w = g->x2 + ROUNDUP(g->bitmap.w - g->x, shrink_factor);
	/* include row zero with the positively numbered rows */
	cols = g->y + 1; /* spare register variable */
	g->y2 = cols / shrink_factor;
	rows = cols - g->y2 * shrink_factor;
	if (rows <= 0) {
	    rows += shrink_factor;
	    --g->y2;
	}
	g->bitmap2.h = shrunk_height = g->y2 +
	    ROUNDUP(g->bitmap.h - cols, shrink_factor) + 1;
	alloc_bitmap(&g->bitmap2);
	old_ptr = (BMUNIT *) g->bitmap.bits;
	new_ptr = (BMUNIT *) g->bitmap2.bits;
	shrunk_bytes_wide = g->bitmap2.bytes_wide;
	rows_left = g->bitmap.h;
	bzero((char *) new_ptr, shrunk_bytes_wide * shrunk_height);
	while (rows_left) {
	    if (rows > rows_left) rows = rows_left;
	    cols_left = g->bitmap.w;
#ifndef	MSBITFIRST
	    m = (1 << 0);
#else
	    m = (1 << (BITS_PER_BMUNIT-1));
#endif
	    cp = new_ptr;
	    cols = init_cols;
	    while (cols_left) {
		if (cols > cols_left) cols = cols_left;
		if (sample(old_ptr, g->bitmap.bytes_wide,
			g->bitmap.w - cols_left, cols, rows) >= min_sample)
		    *cp |= m;
#ifndef	MSBITFIRST
		if (m == (BMUNIT)(1 << (BITS_PER_BMUNIT-1))) {
		    m = (1 << 0);
		    ++cp;
		}
		else m <<= 1;
#else
		if (m == (1 << 0)) {
		    m = (1 << (BITS_PER_BMUNIT-1));
		    ++cp;
		}
		else m >>= 1;
#endif
		cols_left -= cols;
		cols = shrink_factor;
	    }
	    *((char **) &new_ptr) += shrunk_bytes_wide;
	    *((char **) &old_ptr) += rows * g->bitmap.bytes_wide;
	    rows_left -= rows;
	    rows = shrink_factor;
	}
	g->y2 = g->y / shrink_factor;
	if (debug & DBG_BITMAP)
	    print_bitmap(&g->bitmap2);
}

/*
 *	Find font #n.
 */

static	void
change_font(n)
	unsigned long n;
{
	register struct tn *tnp;

	currinf.fontp = NULL;
	for (tnp = currinf.tn_head; tnp != NULL; tnp = tnp->next)
	    if (tnp->TeXnumber == n) {
		currinf.fontp = tnp->fontp;
		break;
	    }
	if (currinf.fontp == NULL) tell_oops("non-existent font #%d", n);
	maxchar = currinf.fontp->maxchar;
	currinf.set_char_p = currinf.fontp->set_char_p;
}


/*
 *	Open a font file.
 */

static	void
open_font_file(fontp)
	struct font *fontp;
{
	if (fontp->file == NULL) {
	    fontp->file = xfopen(fontp->filename);
	    if (fontp->file == NULL)
		oops("Font file disappeared:  %s", fontp->filename);
	}
}


/*
 *	Routines to print characters.
 */

#ifndef	TEXXET
#define	ERRVAL	0L
#else
#define	ERRVAL
#endif

#ifndef	TEXXET
long
set_char(ch)
#else
void
set_char(cmd, ch)
	WIDEARG(ubyte, int)	cmd;
#endif
	WIDEARG(ubyte, int)	ch;
{
	register struct glyph *g;
#ifdef	TEXXET
	long	dvi_h_sav;
#endif

	if (ch > maxchar) realloc_font(currinf.fontp, WIDEARG(,(int)) ch);
	if ((g = &currinf.fontp->glyph[ch])->bitmap.bits == NULL) {
	    if (g->addr == 0) {
		if (!hush_chars)
		    Fprintf(stderr, "Character %d not defined in font %s\n", ch,
			currinf.fontp->fontname);
		g->addr = -1;
		return ERRVAL;
	    }
	    if (g->addr == -1)
		return ERRVAL;	/* previously flagged missing char */
	    open_font_file(currinf.fontp);
	    Fseek(currinf.fontp->file, g->addr, 0);
	    (*currinf.fontp->read_char)(currinf.fontp, ch);
	    if (debug & DBG_BITMAP) print_char((ubyte) ch, g);
	    currinf.fontp->timestamp = ++current_timestamp;
	}

#ifdef	TEXXET
	dvi_h_sav = DVI_H;
	if (currinf.dir < 0) DVI_H -= g->dvi_adv;
	if (scan_frame == NULL) {
#endif
	    if (shrink_factor == 1)
		put_bitmap(&g->bitmap, PXL_H - g->x, PXL_V - g->y);
	    else {
#ifdef	GREY
		if (use_grey) {
		    if (g->pixmap2 == NULL) {
			shrink_glyph_grey(g);
		    }
		    put_image(g->image2, PXL_H - g->x2, PXL_V - g->y2);
		} else {
		    if (g->bitmap2.bits == NULL) {
			shrink_glyph(g);
		    }
		    put_bitmap(&g->bitmap2, PXL_H - g->x2, PXL_V - g->y2);
		}
#else
		if (g->bitmap2.bits == NULL) {
		    shrink_glyph(g);
		}
		put_bitmap(&g->bitmap2, PXL_H - g->x2, PXL_V - g->y2);
#endif
	    }
#ifndef	TEXXET
	return g->dvi_adv;
#else
	}
	if (cmd == PUT1)
	    DVI_H = dvi_h_sav;
	else
	    if (currinf.dir > 0) DVI_H += g->dvi_adv;
#endif
}


/* ARGSUSED */
#ifndef	TEXXET
long
set_empty_char(ch)
#else
void
set_empty_char(cmd, ch)
	WIDEARG(ubyte, int)	cmd;
#endif
	WIDEARG(ubyte, int)	ch;
{
#ifndef	TEXXET
	return 0;
#else
	return;
#endif
}


#ifndef	TEXXET
long
load_n_set_char(ch)
#else
void
load_n_set_char(cmd, ch)
	WIDEARG(ubyte, int)	cmd;
#endif
	WIDEARG(ubyte, int)	ch;
{
	if (load_font(currinf.fontp)) {	/* if not found */
	    Fputs("Character(s) will be left blank.\n", stderr);
	    currinf.set_char_p = currinf.fontp->set_char_p = set_empty_char;
#ifndef	TEXXET
	    return 0;
#else
	    return;
#endif
	}
	maxchar = currinf.fontp->maxchar;
	currinf.set_char_p = currinf.fontp->set_char_p;
#ifndef	TEXXET
	return (*currinf.set_char_p)(ch);
#else
	(*currinf.set_char_p)(cmd, ch);
	return;
#endif
}


#ifndef	TEXXET
long
set_vf_char(ch)
#else
void
set_vf_char(cmd, ch)
	WIDEARG(ubyte, int)	cmd;
#endif
	WIDEARG(ubyte, int)	ch;
{
	register struct macro *m;
	struct drawinf	oldinfo;
	ubyte	oldmaxchar;
	static	ubyte	c;
#ifdef	TEXXET
	long	dvi_h_sav;
#endif

	if (ch > maxchar) realloc_virtual_font(currinf.fontp, ch);
	if ((m = &currinf.fontp->macro[ch])->pos == NULL) {
	    if (!hush_chars)
		Fprintf(stderr, "Character %d not defined in font %s\n", ch,
		    currinf.fontp->fontname);
	    m->pos = m->end = &c;
	    return ERRVAL;
	}
#ifdef	TEXXET
	dvi_h_sav = DVI_H;
	if (currinf.dir < 0) DVI_H -= m->dvi_adv;
	if (scan_frame == NULL) {
#endif
	    oldinfo = currinf;
	    oldmaxchar = maxchar;
	    WW = XX = YY = ZZ = 0;
	    currinf.tn_head = currinf.fontp->vf_chain;
	    currinf.pos = m->pos;
	    currinf.end = m->end;
	    currinf.virtual = currinf.fontp;
	    draw_part(current_frame, currinf.fontp->dimconv);
	    if (currinf.pos != currinf.end + 1)
		tell_oops("virtual character macro does not end correctly");
	    currinf = oldinfo;
	    maxchar = oldmaxchar;
#ifndef	TEXXET
	return m->dvi_adv;
#else
	}
	if (cmd == PUT1)
	    DVI_H = dvi_h_sav;
	else
	    if (currinf.dir > 0) DVI_H += m->dvi_adv;
#endif
}


#ifndef	TEXXET
static	long
set_no_char(ch)
#else
static	void
set_no_char(cmd, ch)
	ubyte	cmd;
#endif
	ubyte	ch;
{
	if (currinf.virtual) {
	    currinf.fontp = currinf.virtual->first_font;
	    if (currinf.fontp != NULL) {
		maxchar = currinf.fontp->maxchar;
		currinf.set_char_p = currinf.fontp->set_char_p;
#ifndef	TEXXET
		return (*currinf.set_char_p)(ch);
#else
		(*currinf.set_char_p)(cmd, ch);
		return;
#endif
	    }
	}
	tell_oops("attempt to set character of unknown font");
	/* NOTREACHED */
}


/*
 *	Set rule.  Arguments are coordinates of lower left corner.
 */

static	void
set_rule(h, w)
	int h, w;
{
#ifndef	TEXXET
	put_rectangle(PXL_H, PXL_V - h + 1, w, h, False);
#else
	put_rectangle(PXL_H - (currinf.dir < 0 ? w - 1 : 0), PXL_V - h + 1,
	    w, h, False);
#endif
}

static	void
put_border(w, h)
	int w, h;
{
	put_rectangle(0, 0, w, 1, True);	/* top */
	put_rectangle(w, 0, 1, h, True);	/* right */
	put_rectangle(1, h, w, 1, True);	/* bottom */
	put_rectangle(0, 1, 1, h, True);	/* left */
}

static	void
special(nbytes)
	long	nbytes;
{
	static	char	*cmd	= NULL;
	static	long	cmdlen	= -1;
	char	*p;

	if (cmdlen < nbytes) {
	    if (cmd) free(cmd);
	    cmd = xmalloc((unsigned) nbytes + 1, "special");
	    cmdlen = nbytes;
	}
	p = cmd;
	for (;;) {
	    int i = currinf.end - currinf.pos;

	    if (i > nbytes) i = nbytes;
	    bcopy((_Xconst char *) currinf.pos, p, i);
	    currinf.pos += i;
	    p += i;
	    nbytes -= i;
	    if (nbytes == 0) break;
	    (void) xxone();
	    --(currinf.pos);
	}
	*p = '\0';
	applicationDoSpecial(cmd);
}

#define	xspell_conv(n)	spell_conv0(n, current_dimconv)

static	void
draw_part(minframe, current_dimconv)
	struct frame	*minframe;
	double		current_dimconv;
{
	ubyte ch;
#ifdef	TEXXET
	struct drawinf	oldinfo;
	ubyte	oldmaxchar;
	off_t	file_pos;
	int	refl_count;
#endif

	currinf.fontp = NULL;
	currinf.set_char_p = set_no_char;
#ifdef	TEXXET
	currinf.dir = 1;
	scan_frame = NULL;	/* indicates we're not scanning */
#endif
	for (;;) {
	    ch = xone();
	    if (debug & DBG_DVI)
		print_dvi(ch);
	    if (ch <= SETCHAR0 + 127)
#ifndef	TEXXET
		DVI_H += (*currinf.set_char_p)(ch);
#else
		(*currinf.set_char_p)(ch, ch);
#endif
	    else if (FNTNUM0 <= ch && ch <= FNTNUM0 + 63)
		change_font((unsigned long) (ch - FNTNUM0));
	    else {
		long a, b;

		switch (ch) {
		    case SET1:
		    case PUT1:
#ifndef	TEXXET
			a = (*currinf.set_char_p)(xone());
			if (ch != PUT1) DVI_H += a;
#else
			(*currinf.set_char_p)(ch, xone());
#endif
			break;

		    case SETRULE:
			/* Be careful, dvicopy outputs rules with
			   height = 0x80000000.  We don't want any
			   SIGFPE here. */
			a = xsfour();
			b = xspell_conv(xsfour());
#ifndef	TEXXET
			if (a > 0 && b > 0)
#else
			if (a > 0 && b > 0 && scan_frame == NULL)
#endif
			    set_rule(pixel_round(xspell_conv(a)),
				pixel_round(b));
			DVI_H += DIR * b;
			break;

		    case PUTRULE:
			a = xspell_conv(xsfour());
			b = xspell_conv(xsfour());
#ifndef	TEXXET
			if (a > 0 && b > 0)
#else
			if (a > 0 && b > 0 && scan_frame == NULL)
#endif
			    set_rule(pixel_round(a), pixel_round(b));
			break;

		    case NOP:
			break;

		    case BOP:
			xskip((long) 11 * 4);
			DVI_H = OFFSET_X;
			DVI_V = OFFSET_Y;
			PXL_V = pixel_conv(DVI_V);
			WW = XX = YY = ZZ = 0;
			break;

		    case EOP:
			if (current_frame != minframe)
			    tell_oops("stack not empty at EOP");
			return;

		    case PUSH:
			if (current_frame->next == NULL) {
			    struct frame *newp = (struct frame *)
				xmalloc(sizeof(struct frame), "stack frame");
			    current_frame->next = newp;
			    newp->prev = current_frame;
			    newp->next = NULL;
			}
			current_frame = current_frame->next;
			current_frame->data = currinf.data;
			break;

		    case POP:
			if (current_frame == minframe)
			    tell_oops("more POPs than PUSHes");
			currinf.data = current_frame->data;
			current_frame = current_frame->prev;
			break;

#ifdef	TEXXET
		    case SREFL:
			if (scan_frame == NULL) {
			    /* we're not scanning:  save some info. */
			    oldinfo = currinf;
			    oldmaxchar = maxchar;
			    if (!currinf.virtual)
				file_pos = xtell(currinf.pos);
			    scan_frame = current_frame; /* now we're scanning */
			    refl_count = 0;
			    break;
			}
			/* we are scanning */
			if (current_frame == scan_frame) ++refl_count;
			break;

		    case EREFL:
			if (scan_frame != NULL) {	/* if we're scanning */
			    if (current_frame == scan_frame && --refl_count < 0)
			    {
				/* we've hit the end of our scan */
				scan_frame = NULL;
				/* first:  push */
				if (current_frame->next == NULL) {
				    struct frame *newp = (struct frame *)
					xmalloc(sizeof(struct frame),
					    "stack frame");
				    current_frame->next = newp;
				    newp->prev = current_frame;
				    newp->next = NULL;
				}
				current_frame = current_frame->next;
				current_frame->data = currinf.data;
				/* next:  restore old file position, XX, etc. */
				if (!currinf.virtual) {
				    off_t bgn_pos = xtell(dvi_buffer);

				    if (file_pos >= bgn_pos) {
					oldinfo.pos = dvi_buffer
					    + (file_pos - bgn_pos);
					oldinfo.end = currinf.end;
				    }
				    else {
					(void) lseek(fileno(dvi_file), file_pos,
					    SEEK_SET);
					oldinfo.pos = oldinfo.end;
				    }
				}
				currinf = oldinfo;
				maxchar = oldmaxchar;
				/* and then:  recover position info. */
				DVI_H = current_frame->data.dvi_h;
				DVI_V = current_frame->data.dvi_v;
				PXL_V = current_frame->data.pxl_v;
				/* and finally, reverse direction */
				currinf.dir = -currinf.dir;
			    }
			    break;
			}
			/* we're not scanning, */
			/* so just reverse direction and then pop */
			currinf.dir = -currinf.dir;
			currinf.data = current_frame->data;
			current_frame = current_frame->prev;
			break;
#endif	/* TEXXET */

		    case RIGHT1:
		    case RIGHT2:
		    case RIGHT3:
		    case RIGHT4:
			DVI_H += DIR * xspell_conv(xsnum(ch - RIGHT1 + 1));
			break;

		    case W1:
		    case W2:
		    case W3:
		    case W4:
			WW = xspell_conv(xsnum(ch - W0));
		    case W0:
			DVI_H += DIR * WW;
			break;

		    case X1:
		    case X2:
		    case X3:
		    case X4:
			XX = xspell_conv(xsnum(ch - X0));
		    case X0:
			DVI_H += DIR * XX;
			break;

		    case DOWN1:
		    case DOWN2:
		    case DOWN3:
		    case DOWN4:
			DVI_V += xspell_conv(xsnum(ch - DOWN1 + 1));
			PXL_V = pixel_conv(DVI_V);
			break;

		    case Y1:
		    case Y2:
		    case Y3:
		    case Y4:
			YY = xspell_conv(xsnum(ch - Y0));
		    case Y0:
			DVI_V += YY;
			PXL_V = pixel_conv(DVI_V);
			break;

		    case Z1:
		    case Z2:
		    case Z3:
		    case Z4:
			ZZ = xspell_conv(xsnum(ch - Z0));
		    case Z0:
			DVI_V += ZZ;
			PXL_V = pixel_conv(DVI_V);
			break;

		    case FNT1:
		    case FNT2:
		    case FNT3:
		    case FNT4:
			change_font(xnum(ch - FNT1 + 1));
			break;

		    case XXX1:
		    case XXX2:
		    case XXX3:
		    case XXX4:
			a = xnum(ch - XXX1 + 1);
			if (a > 0)
			    special(a);
			break;

		    case FNTDEF1:
		    case FNTDEF2:
		    case FNTDEF3:
		    case FNTDEF4:
			xskip((long) (12 + ch - FNTDEF1 + 1));
			xskip((long) xone() + (long) xone());
			break;

#ifndef	TEXXET
		    case SREFL:
		    case EREFL:
#endif
		    case PRE:
		    case POST:
		    case POSTPOST:
			tell_oops("shouldn't happen: %s encountered",
				dvi_table2[ch - (FNTNUM0 + 64)]);
			break;

		    default:
			tell_oops("unknown op-code %d", ch);
		} /* end switch*/
	    } /* end else (ch not a SETCHAR or FNTNUM) */
	} /* end for */
}

#undef	xspell_conv

void
draw_page()
{
	/* Check for changes in dvi file. */
	if (!check_dvi_file()) return;

	put_border(ROUNDUP(unshrunk_paper_w, shrink_factor) + 1,
	    ROUNDUP(unshrunk_paper_h, shrink_factor) + 1);

	(void) lseek(fileno(dvi_file), page_offset[current_page], SEEK_SET);

	bzero((char *) &currinf.data, sizeof(currinf.data));
	currinf.tn_head = tn_head;
	currinf.pos = currinf.end = dvi_buffer;
	currinf.virtual = NULL;
	draw_part(current_frame = &frame0, dimconv);
}
