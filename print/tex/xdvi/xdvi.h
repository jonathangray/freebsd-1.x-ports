/*
 *	Written by Eric C. Cooper, CMU
 */
/* $Header: /a/cvs/386BSD/ports/print/tex/xdvi/xdvi.h,v 1.1 1993/08/09 11:01:05 alm Exp $ */

#ifndef X10
#include <X11/Xos.h>	/* same as below */
#else	/* X10 */
#include <sys/types.h>	/* for sites without X11 */
#ifdef	SYSV
#include <string.h>
#define	index	strchr
#define	rindex	strrchr
#include <fcntl.h>
#else /* SYSV */
#include <strings.h>
#endif /* SYSV */
#include <sys/file.h>
#endif	/* X10 */

#ifdef	VMS
#include <string.h>
#define	index	strchr
#define	rindex	strrchr
#define	bzero(a, b)	(void) memset ((void *) (a), 0, (size_t) (b))
#endif

#include <setjmp.h>

#ifndef	OPEN_MODE
#ifndef	VMS
#define	OPEN_MODE	"r"
#else	/* VMS */
#define	OPEN_MODE	"r", "ctx=stm"
#endif	/* VMS */
#endif	/* OPEN_MODE */

#define	Printf	(void) printf
#define	Fprintf	(void) fprintf
#define	Sprintf	(void) sprintf
#define	Fseek	(void) fseek
#define	Fread	(void) fread
#define	Fputs	(void) fputs
#define	Putc	(void) putc
#define	Putchar	(void) putchar
#define	Fclose	(void) fclose
#define	Strcpy	(void) strcpy

unsigned long num();
long snum();

#define one(fp)		((unsigned long) getc(fp) & 0xff)
#define sone(fp)	((long) getc(fp))
#define two(fp)		num (fp, 2)
#define stwo(fp)	snum(fp, 2)
#define four(fp)	num (fp, 4)
#define sfour(fp)	snum(fp, 4)

typedef	unsigned char ubyte;
#define	Boolean	char
#define	True	1
#define	False	0

#define	MAXDIM		32767

/*
 *	pixel_conv is currently used only for converting absolute positions
 *	to pixel values; although normally it should be
 *		((int) ((x) / shrink_factor + (1 << 15) >> 16)),
 *	the rounding is achieved instead by moving the constant 1 << 15 to
 *	PAGE_OFFSET in dvi_draw.c.
 */
#define	pixel_conv(x)	((int) ((x) / shrink_factor >> 16))
#define	pixel_round(x)	((int) ROUNDUP(x, shrink_factor << 16))
#define	spellfour(f)	((long) (sfour(f) * fraction))
#define	spellnum(f,n)	((long) (snum(f,n) * fraction))

#ifdef X10
#undef	MSBITFIRST
#undef	BMLONG
#define	BMSHORT
#endif

#ifndef	SYSV
#ifndef	VMS
#define	HAS_SIGIO		/* has SIGIO on _sockets_ */
#endif
#endif

#ifdef	BMLONG
#define	BMUNIT			unsigned long
#define	BITS_PER_BMUNIT		32
#define	BYTES_PER_BMUNIT	4
#else	/* BMLONG */
#ifdef	BMSHORT
#define	BMUNIT			unsigned short
#define	BITS_PER_BMUNIT		16
#define	BYTES_PER_BMUNIT	2
#else	/* BMSHORT */
#define	BMUNIT			unsigned char
#define	BITS_PER_BMUNIT		8
#define	BYTES_PER_BMUNIT	1
#endif	/* BMSHORT */
#endif	/* BMLONG */

#define	ADD(a, b)	((BMUNIT *) (((char *) a) + b))
#define	SUB(a, b)	((BMUNIT *) (((char *) a) - b))

extern	BMUNIT	bit_masks[BITS_PER_BMUNIT + 1];

struct frame {
	long dvi_h, dvi_v, w, x, y, z;
	int pxl_v;
};

extern	struct frame 	*stack;
extern	struct frame 	*stackp;

/* entries below with the characters 'dvi' in them are actually stored in
   scaled pixel units */

#define DVI_H   stackp->dvi_h
#define PXL_H   pixel_conv(stackp->dvi_h)
#define DVI_V   stackp->dvi_v
#define PXL_V   stackp->pxl_v
#define WW      stackp->w
#define XX      stackp->x
#define YY      stackp->y
#define ZZ      stackp->z
#define ROUNDUP(x,y) (((x)+(y)-1)/(y))

extern	int	current_page;
extern	int	total_pages;
extern	double	fraction;
extern	int	maxstack;
extern	int	n_fonts_left;		/* for LRU management of fonts */
extern	time_t	dvi_time;		/* last mod. time for dvi file */
extern	int	page_w, page_h;

/*
 * Table of page offsets in DVI file, indexed by page number - 1.
 * Initialized in prepare_pages().
 */
extern	long	*page_offset;

/*
 * Mechanism for reducing repeated warning about specials, lost characters, etc.
 */
extern	Boolean	hush_spec, hush_spec_now;
extern	Boolean	hush_chars;


/*
 * Bitmap structure for raster ops.
 */
struct bitmap{
	short w, h;		/* width and height in pixels */
	short bytes_wide;	/* scan-line width in bytes */
	char *bits;		/* pointer to the bits */
};

/*
 * Per-character information.
 * There is one of these for each character in a font.
 * All fields are filled in at font definition time,
 * except for the bitmap, which is "faulted in"
 * when the character is first referenced.
 */
struct glyph {
	long addr;		/* address of bitmap in PXL file */
	long dvi_adv;		/* DVI units to move reference point */
	short x, y;		/* x and y offset in pixels */
	struct bitmap bitmap;	/* bitmap for character */
	short x2, y2;		/* x and y offset in pixels (shrunken bitmap) */
	struct bitmap bitmap2;	/* shrunken bitmap for character */
};

/*
 * The layout of a font information block.
 * There is one of these for every loaded font or
 * magnification thereof.
 *
 * Also note the strange units.  The design size is in 1/2^20 point
 * units (also called micro-points), and the individual character widths
 * are in the TFM file in 1/2^20 ems units, i.e. relative to the design size.
 *
 * We then change the sizes to SPELL units (unshrunk pixel / 2^16).
 */

void	alloc_bitmap();

typedef	void (*read_char_proc)();
	/* struct font *fp; */
	/* ubyte ch; */

	/* the corresponding read_font_index procedures occur in dvi_init.c */
typedef	void (*read_font_index_proc)();
	/* struct font *fontp; */
extern	read_font_index_proc read_GF_index, read_PK_index, read_PXL_index;

struct font {
	struct font *next;		/* link to next font info block */
	int TeXnumber;			/* font number (in DVI file) */
	int scale;			/* scaled size in SPELL units */
	char *fontname;			/* PXL file name */
	short size;			/* dots per 5 inches */
	FILE *file;			/* open PXL file or NULL */
	char *filename;			/* name of PXL file */
	ubyte maxchar;			/* largest character code */
	read_char_proc read_char;	/* function to read bitmap */
	struct glyph glyph[256];
};

extern	struct font	*current_font;
extern	ubyte	maxchar;

/*
 * Command line flags.
 */

extern	int	debug;

#define DBG_BITMAP	0x1
#define DBG_DVI		0x2
#define DBG_PK          0x4
#define DBG_BATCH       0x8
#define	DBG_EVENT	0x10
#define	DBG_OPEN	0x20
#define DBG_ALL		(DBG_BITMAP|DBG_DVI|DBG_PK|DBG_EVENT|DBG_OPEN)

extern	Boolean	list_fonts;

extern	int	pixels_per_inch;
extern  double  aspect_ratio;
extern	int	offset_x, offset_y;
extern	int	unshrunk_paper_w, unshrunk_paper_h;
extern	int	unshrunk_page_w, unshrunk_page_h;
extern	int	density;

extern	char	*dvi_name;
extern	FILE	*dvi_file;				/* user's file */
extern	char	*alt_font;
extern	char	*prog;

extern	struct	WindowRec {
	caddr_t	win;		/* type Window is not defined yet */
	int	shrinkfactor;
	int	base_x, base_y;
	int	width, height;
	int	min_x, max_x, min_y, max_y;	/* for pending expose events */
} mane, alt, curr;

#define	WINDOW(wr)	((Window) (wr).win)
#define	shrink_factor	curr.shrinkfactor

jmp_buf	dvi_env;		/* mechanism to communicate dvi file errors */
