/*
 *	Written by Eric C. Cooper, CMU
 */

/********************************
 *	The C environment	*
 *******************************/

/* Some xdvi options we want by default.  */
#define xmalloc xdvi_xmalloc
#define xfopen xdvi_xfopen
#define USE_PK
#define USE_GF

#ifndef NOGREY
#define GREY
#endif

#ifndef NOTEXXET
#define TEXXET
#endif

/* For wchar_t et al., that the X files might want. */
#include <kpathsea/systypes.h>

#ifndef X10
/* See the kpathsea/INSTALL file for the purpose of these #define's.  */
#ifndef NO_FOIL_X_WCHAR_T
#define wchar_t foil_x_defining_wchar_t
#define X_WCHAR
#endif
#include <X11/Xlib.h>	/* include Xfuncs.h, if available */
#include <X11/Xutil.h>	/* needed for XDestroyImage */
#include <X11/Xos.h>	/* same as below */
#undef wchar_t
#else	/* X10 */
#include <X/Xlib.h>	/* get type of Window */
#endif	/* X10 */

#ifndef	XlibSpecificationRelease
#define	XlibSpecificationRelease 0
#endif

#ifndef	OPEN_MODE
#define OPEN_MODE FOPEN_R_MODE
#endif	/* OPEN_MODE */

#ifndef	NeedFunctionPrototypes
#ifdef	__STDC__
#define	NeedFunctionPrototypes	1
#else	/* STDC */
#define	NeedFunctionPrototypes	0
#endif	/* STDC */
#endif	/* NeedFunctionPrototypes */

#ifndef	NeedWidePrototypes
#define	NeedWidePrototypes	NeedFunctionPrototypes
#endif

#if	NeedWidePrototypes
#define	WIDEARG(a, b)	b
#else
#define	WIDEARG(a, b)	a
#endif

#ifndef	NeedVarargsPrototypes
#define	NeedVarargsPrototypes	NeedFunctionPrototypes
#endif

#ifndef	_XFUNCPROTOBEGIN
#define	_XFUNCPROTOBEGIN
#define	_XFUNCPROTOEND
#endif

#ifndef	_Xconst
#ifdef	__STDC__
#define	_Xconst	const
#else	/* STDC */
#define	_Xconst
#endif	/* STDC */
#endif	/* _Xconst */

#ifndef	VOLATILE
#if	defined(__STDC__) || (defined(__stdc__) && defined(__convex__))
#define	VOLATILE	volatile
#else
#define	VOLATILE	/* nothing */
#endif
#endif

#ifndef	NORETURN
#ifdef	__GNUC__
#define	NORETURN	VOLATILE
#else
#define	NORETURN	/* nothing */
#endif
#endif

#define	Printf	(void) printf
#define	Puts	(void) puts
#define	Fprintf	(void) fprintf
#define	Sprintf	(void) sprintf
#define	Fseek	(void) fseek
#define	Fread	(void) fread
#define	Fputs	(void) fputs
#define	Putc	(void) putc
#define	Putchar	(void) putchar
#define	Fclose	(void) fclose
#define	Strcpy	(void) strcpy

/********************************
 *	 Types and data		*
 *******************************/

#ifndef	EXTERN
#define	EXTERN	extern
#define	INIT(x)
#define	NTINIT(x)
#endif

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
#define	pixel_conv(x)		((int) ((x) / shrink_factor >> 16))
#define	pixel_round(x)		((int) ROUNDUP(x, shrink_factor << 16))
#define	spell_conv0(n, f)	((long) (n * f))
#define	spell_conv(n)		spell_conv0(n, dimconv)

#ifdef X10
#undef	MSBITFIRST
#undef	BMLONG
#define	BMSHORT
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
	struct framedata {
		long dvi_h, dvi_v, w, x, y, z;
		int pxl_v;
	} data;
	struct frame *next, *prev;
};

#if	NeedFunctionPrototypes
#ifndef	TEXXET
typedef	long	(*set_char_proc)(WIDEARG(ubyte, int));
#else	/* TEXXET */
typedef	void	(*set_char_proc)(WIDEARG(ubyte, int), WIDEARG(ubyte, int));
#endif	/* TEXXET */
#else	/* NeedFunctionPrototypes */
#ifndef	TEXXET
typedef	long	(*set_char_proc)();
#else	/* TEXXET */
typedef	void	(*set_char_proc)();
#endif	/* TEXXET */
#endif	/* NeedFunctionPrototypes */

struct drawinf {	/* this information is saved when using virtual fonts */
	struct framedata data;
	struct font	*fontp;
	set_char_proc	set_char_p;
	struct tn	*tn_head;
	ubyte		*pos, *end;
	struct font	*virtual;
#ifdef	TEXXET
	int		dir;
#endif
};

EXTERN	struct drawinf	currinf;

/* entries below with the characters 'dvi' in them are actually stored in
   scaled pixel units */

#define DVI_H   currinf.data.dvi_h
#define PXL_H   pixel_conv(currinf.data.dvi_h)
#define DVI_V   currinf.data.dvi_v
#define PXL_V   currinf.data.pxl_v
#define WW      currinf.data.w
#define XX      currinf.data.x
#define YY      currinf.data.y
#define ZZ      currinf.data.z
#define ROUNDUP(x,y) (((x)+(y)-1)/(y))

EXTERN	int	current_page;
EXTERN	int	total_pages;
EXTERN	double	dimconv;
EXTERN	int	n_files_left;	/* for LRU closing of fonts */
EXTERN	time_t	dvi_time;		/* last mod. time for dvi file */
EXTERN	int	page_w, page_h;

/*
 * Table of page offsets in DVI file, indexed by page number - 1.
 * Initialized in prepare_pages().
 */
EXTERN	long	*page_offset;

/*
 * Mechanism for reducing repeated warning about specials, lost characters, etc.
 */
EXTERN	Boolean	hush_spec	NTINIT(False);
EXTERN	Boolean	hush_spec_now;
EXTERN	Boolean	hush_chars	NTINIT(False);
/* 
 * Options for turning off and speeding up rendering of extended PostScript
 * (epsf) \special commands.
 */
EXTERN	Boolean no_epsf           NTINIT(False);
EXTERN	Boolean epsf_grey         NTINIT(False);
/* Enable/disable MakeTEXPK at runtime; ifdef is only for default. */
#ifdef MAKETEXPK
EXTERN	Boolean use_maketexpk     NTINIT(True);
#else
EXTERN	Boolean use_maketexpk     NTINIT(False);
#endif
EXTERN  char *mf_mode             NTINIT("cx");
/*
 * Bitmap structure for raster ops.
 */
struct bitmap {
	short w, h;		/* width and height in pixels */
	short bytes_wide;	/* scan-line width in bytes */
	char *bits;		/* pointer to the bits */
};

/*
 * Per-character information.
 * There is one of these for each character in a font (raster fonts only).
 * All fields are filled in at font definition time,
 * except for the bitmap, which is "faulted in"
 * when the character is first referenced.
 */
struct glyph {
	long addr;		/* address of bitmap in font file */
	long dvi_adv;		/* DVI units to move reference point */
	short x, y;		/* x and y offset in pixels */
	struct bitmap bitmap;	/* bitmap for character */
	short x2, y2;		/* x and y offset in pixels (shrunken bitmap) */
#ifdef	GREY
	XImage *image2;
	char *pixmap2;
#endif
	struct bitmap bitmap2;	/* shrunken bitmap for character */
};

/*
 * Per character information for virtual fonts
 */
struct macro {
	ubyte	*pos;		/* address of first byte of macro */
	ubyte	*end;		/* address of last+1 byte */
	long	dvi_adv;	/* DVI units to move reference point */
	Boolean	free_me;	/* if free(pos) should be called when */
				/* freeing space */
};

/*
 * The layout of a font information block.
 * There is one of these for every loaded font or magnification thereof.
 * Duplicates are eliminated:  this is necessary because of possible recursion
 * in virtual fonts.
 *
 * Also note the strange units.  The design size is in 1/2^20 point
 * units (also called micro-points), and the individual character widths
 * are in the TFM file in 1/2^20 ems units, i.e., relative to the design size.
 *
 * We then change the sizes to SPELL units (unshrunk pixel / 2^16).
 */

#define	NOMAGSTP (-29999)

#if	NeedFunctionPrototypes
typedef	void (*read_char_proc)(struct font *, WIDEARG(ubyte, int));
#else
typedef	void (*read_char_proc)();
#endif

struct font {
	struct font *next;		/* link to next font info block */
	char *fontname;			/* name of font */
	float fsize;			/* size information (dots per inch) */
	int magstepval;			/* magstep number * two, or NOMAGSTP */
	FILE *file;			/* open font file or NULL */
	char *filename;			/* name of font file */
	unsigned short timestamp;	/* for LRU management of fonts */
	ubyte flags;			/* flags byte (see values below) */
	ubyte maxchar;			/* largest character code */
	double dimconv;			/* size conversion factor */
	set_char_proc set_char_p;	/* proc used to set char */
		/* these fields are used by (loaded) raster fonts */
	read_char_proc read_char;	/* function to read bitmap */
	struct glyph *glyph;
		/* these fields are used by (loaded) virtual fonts */
	struct tn *vf_chain;		/* list of fonts used by this vf */
	struct font *first_font;	/* first font defined */
	struct macro *macro;
		/* I suppose the above could be put into a union, but we */
		/* wouldn't save all that much space. */
};

#define	FONT_IN_USE	1	/* used for housekeeping */
#define	FONT_LOADED	2	/* if font file has been read */
#define	FONT_VIRTUAL	4	/* if font is virtual */

struct tn {
	struct tn *next;		/* link to next TeXnumber info block */
	int TeXnumber;			/* font number (in DVI file) */
	struct font *fontp;		/* pointer to the rest of the info */
};

EXTERN	struct font	*font_head	INIT(NULL);
EXTERN	struct tn	*tn_head	INIT(NULL);
EXTERN	ubyte		maxchar;
EXTERN	unsigned short	current_timestamp INIT(0);

/*
 * Command line flags.
 */

EXTERN	int	debug	INIT(0);

#define	DBG_BITMAP	0x01
#define	DBG_DVI		0x02
#define	DBG_PK		0x04
#define	DBG_BATCH	0x08
#define	DBG_EVENT	0x10
#define	DBG_OPEN	0x20
#define	DBG_EPS		0x40
#define	DBG_ALL		(0xff & ~DBG_BATCH)

EXTERN	Boolean	list_fonts	NTINIT(False);

#ifndef	BDPI
#define	BDPI	300
#endif

EXTERN	int	pixels_per_inch	NTINIT(BDPI);
EXTERN	int	offset_x, offset_y;
EXTERN	int	unshrunk_paper_w, unshrunk_paper_h;
EXTERN	int	unshrunk_page_w, unshrunk_page_h;
EXTERN	int	density		NTINIT(40);
EXTERN	double	specialConv;

EXTERN	char	*dvi_name	INIT(NULL);
EXTERN	char	*title_name	INIT(NULL);
EXTERN	char	*icon_name	INIT(NULL);
EXTERN	FILE	*dvi_file;				/* user's file */
EXTERN	char	*prog;

struct	WindowRec {
	Window	win;
	int	shrinkfactor;
	int	base_x, base_y;
	int	width, height;
	int	min_x, max_x, min_y, max_y;	/* for pending expose events */
};

extern	struct WindowRec mane, alt, currwin;

#define	WINDOW(wr)	((Window) (wr).win)
#define	shrink_factor	currwin.shrinkfactor

EXTERN	jmp_buf	dvi_env;	/* mechanism to communicate dvi file errors */

#ifdef	GREY
#ifndef	X10
EXTERN	Display	*DISP;
EXTERN	Screen	*SCRN;
EXTERN	unsigned long	palette[17];
EXTERN	unsigned long	*pixeltbl;
EXTERN	Boolean	use_grey	NTINIT(True);
#else	/* Sorry - GREY works only with X11 */
#undef	GREY
#endif	/* X10 */
#endif	/* GREY */

/********************************
 *	   Procedures		*
 *******************************/

_XFUNCPROTOBEGIN
#if	NeedFunctionPrototypes

extern	void	line_btw(int, int, int, int);
extern	void	dot_at(int, int);
extern	void	do_attribute_path(int, int, int, int);
extern	void	put_bitmap(struct bitmap *, int, int);
#ifdef	GREY
extern	void	put_image(XImage *, int, int);
#endif
extern	void	put_rectangle(int, int, int, int, WIDEARG(Boolean, int));
extern	void	reconfig(void);
extern	void	redraw_page(void);
#if	NeedVarargsPrototypes
extern	NORETURN void	oops(_Xconst char *, ...);
#else
extern	NORETURN void	oops();
#endif
extern	char	*xmalloc(unsigned, _Xconst char *);
extern	void	alloc_bitmap(struct bitmap *);
extern	FILE	*xfopen(_Xconst char *);
extern	unsigned long	num(FILE *, int);
extern	long	snum(FILE *, int);
extern	void	reset_fonts(void);
extern	void	realloc_font(struct font *, WIDEARG(ubyte, int));
extern	void	realloc_virtual_font(struct font *, WIDEARG(ubyte, int));
extern	Boolean	load_font(struct font *);
extern	void	define_font(FILE *, WIDEARG(ubyte, unsigned int), struct font *,
			struct tn **);
extern	void	init_page(void);
extern	void	open_dvi_file(void);
extern	Boolean	check_dvi_file(void);
#ifndef	TEXXET
extern	long	set_char(WIDEARG(ubyte, int));
extern	long	load_n_set_char(WIDEARG(ubyte, int));
extern	long	set_vf_char(WIDEARG(ubyte, int));
#else
extern	void	set_char(WIDEARG(ubyte, int), WIDEARG(ubyte, int));
extern	void	load_n_set_char(WIDEARG(ubyte, int), WIDEARG(ubyte, int));
extern	void	set_vf_char(WIDEARG(ubyte, int), WIDEARG(ubyte, int));
#endif
extern	void	draw_page(void);
extern	void	init_font_open(void);
extern	FILE	*font_open(_Xconst char *, char **,
			WIDEARG(float, double), int *, char **);
extern	void	applicationDoSpecial(char *);
extern	void	read_PK_index(struct font *);
extern	void	read_GF_index(struct font *);
extern	void	read_PXL_index(struct font *);
extern	void	read_VF_index(struct font *);

#else	/* ! NeedFunctionPrototypes */

extern	void	line_btw();
extern	void	dot_at();
extern	void	do_attribute_path();
extern	void	put_bitmap();
#ifdef	GREY
extern	void	put_image();
#endif
extern	void	put_rectangle();
extern	void	reconfig();
extern	void	redraw_page();
extern	NORETURN void	oops();
extern	char	*xmalloc();
extern	void	alloc_bitmap();
extern	FILE	*xfopen();
extern	unsigned long	num();
extern	long	snum();
extern	void	reset_fonts();
extern	void	realloc_font();
extern	void	realloc_virtual_font();
extern	Boolean	load_font();
extern	void	define_font();
extern	void	init_page();
extern	void	open_dvi_file();
extern	Boolean	check_dvi_file();
#ifndef	TEXXET
extern	long	set_char();
extern	long	load_n_set_char();
extern	long	set_vf_char();
#else
extern	void	set_char();
extern	void	load_n_set_char();
extern	void	set_vf_char();
#endif
extern	void	draw_page();
extern	void	init_font_open();
extern	FILE	*font_open();
extern	void	applicationDoSpecial();
extern	void	read_PK_index();
extern	void	read_GF_index();
extern	void	read_PXL_index();
extern	void	read_VF_index();

#endif	/* NeedFunctionPrototypes */

#define one(fp)		((unsigned char) getc(fp))
#define sone(fp)	((long) one(fp))
#define two(fp)		num (fp, 2)
#define stwo(fp)	snum(fp, 2)
#define four(fp)	num (fp, 4)
#define sfour(fp)	snum(fp, 4)

_XFUNCPROTOEND
