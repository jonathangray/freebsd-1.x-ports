/*
 * DVI previewer for X.
 *
 * Eric Cooper, CMU, September 1985.
 *
 * Code derived from dvi-imagen.c.
 *
 * Modification history:
 * 1/1986	Modified for X.10 by Bob Scheifler, MIT LCS.
 * 7/1988	Modified for X.11 by Mark Eichin, MIT
 * 12/1988	Added 'R' option, toolkit, magnifying glass
 *			--Paul Vojta, UC Berkeley.
 * 2/1989	Added tpic support	--Jeffrey Lee, U of Toronto
 * 4/1989	Modified for System V by Donald Richardson, Clarkson Univ.
 * 3/1990	Added VMS support	--Scott Allendorf, U of Iowa
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
 */

#define	GF_PRE		247
#define	GF_ID_BYTE	131
#define	GF_MAGIC	(GF_PRE << 8) + GF_ID_BYTE
#define	PK_PRE		247
#define	PK_ID		89
#define	PK_MAGIC	(PK_PRE << 8) + PK_ID
#define	PXL_MAGIC1	0
#define	PXL_MAGIC2	1001

#include <stdio.h>
#include <ctype.h>
#include "xdvi.h"
#include "dvi.h"
#include <sys/stat.h>

#define	dvi_oops(str)	longjmp(dvi_env, (int) str);
#define XtOffset(type, field)    ((unsigned int)&(((type)NULL)->field))

read_font_index_proc read_GF_index, read_PK_index, read_PXL_index;

static	struct stat fstatbuf;		/* mechanism to see if file was */
time_t	dvi_time;			/* modified since last usage */

struct font *current_font = NULL;	/* ptr into linked list of fonts */
ubyte	maxchar;

static	Boolean	font_not_found;
static	struct font **old_fonts;	/* used by read_postamble */

int	n_fonts_left	= 32767;	/* for LRU management of fonts */

char	*realloc();
void	exit();

/*
 * DVI preamble and postamble information.
 */
int	current_page;
Boolean	hush_spec_now;
int	total_pages;
double	fraction;
int	maxstack;
static	char	job_id[300];
static	long	numerator, denominator, magnification;

/*
 * Table of page offsets in DVI file, indexed by page number - 1.
 * Initialized in prepare_pages().
 */
long	*page_offset;

/*
 * Offset in DVI file of last page, set in read_postamble().
 */
static	long	last_page_offset;

#ifdef	sun
char	*sprintf();
#endif

char	*malloc();
FILE	*pxl_open();

/*
 *	General (program-wide) utility routines.
 */

char *
xmalloc(size, why)
	unsigned size;
	char	*why;
{
	char *mem = malloc(size);

	if (mem == NULL)
	    oops("! Cannot allocate %u bytes for %s.\n", size, why);
	return mem;
}


/*
 *      define_font reads the rest of the fntdef command and then reads in
 *      the specified pixel file, adding it to the global linked-list holding
 *      all of the fonts used in the job.
 */
static	void
define_font(cmnd)
	ubyte cmnd;
{
        register struct font *fontp;
	struct font **fontpp = old_fonts;
	struct font *fontp1;
	float	fsize;
	int len;
	int design;
	int size;

	fontp = (struct font *) xmalloc((unsigned) sizeof(struct font),
	    "font structure");
	fontp->TeXnumber = num(dvi_file, (ubyte) cmnd - FNTDEF1 + 1);
	(void) four(dvi_file);	/* checksum */
	fontp->scale = four(dvi_file);
	design = four(dvi_file);
	len = one(dvi_file) + one(dvi_file);
	fontp->fontname = xmalloc((unsigned) len + 1, "font name");
	Fread(fontp->fontname, sizeof(char), len, dvi_file);
	fontp->fontname[len] = '\0';
	if(debug & DBG_PK)
	  Printf("Define font \"%s\" scale=%d design=%d\n",
	    fontp->fontname, fontp->scale, design);
	fsize = (float) fontp->scale / design * magnification
	    * pixels_per_inch * 0.005;
	fontp->size = size = fsize + 0.5;
	fontp->scale = fontp->scale * fraction;
	/*
	 * reuse font if possible
	 */
	for (;;) {
	    fontp1 = *fontpp;
	    if (fontp1 == NULL) {		/* if font not already loaded */
		read_font_index_proc read_font_index;
		char	*font_found;
		int	size_found;
		int	dpi = (size + 2) / 5;
		int	magic;

		if (n_fonts_left == 0)
		    close_a_file();
		fontp->file = pxl_open(fontp->fontname, &font_found,
		    fsize, &size_found, &fontp->filename);
		if (fontp->file == NULL) {
		    Fprintf(stderr, "Can't find font %s.\n", fontp->fontname);
		    font_not_found = True;
		    return;
		}
		--n_fonts_left;
		if (font_found != NULL) {
		    Fprintf(stderr,
			"Can't find font %s; using %s instead at %d dpi\n",
			fontp->fontname, font_found, dpi);
		    free(fontp->fontname);
		    fontp->fontname = font_found;
		}
		else if (size_found > (int) (1.002 * fsize + 0.5) ||
			size_found < (int) (0.998 * fsize + 0.5))
		    Fprintf(stderr,
			"Can't find font %s at %d dpi; using %d dpi instead.\n",
			fontp->fontname, dpi, (size_found + 2) / 5);
		maxchar = 255;
		magic = two(fontp->file);
		if (magic == GF_MAGIC) read_font_index = read_GF_index;
		else if (magic == PK_MAGIC) read_font_index = read_PK_index;
		else if (magic == PXL_MAGIC1 && two(fontp->file) == PXL_MAGIC2)
		    read_font_index = read_PXL_index;
		else oops("Cannot recognize format for font file %s",
		    fontp->filename);
		if (read_font_index == NULL)
		    oops("%s: unsupported font format.", fontp->filename);
		(*read_font_index)(fontp);
		while (maxchar > 0 && fontp->glyph[maxchar].addr == 0)
		    --maxchar;
		if (maxchar < 255)
		    fontp = (struct font *) realloc((char *) fontp,
			XtOffset(struct font *, glyph[(int) maxchar + 1]));
		fontp->maxchar = maxchar;
		break;
	    }
	    if (strcmp(fontp->fontname, fontp1->fontname) == 0
		    && size == fontp1->size) {
		*fontpp = fontp1->next;
		fontp1->TeXnumber = fontp->TeXnumber;
		free(fontp->fontname);
		free((char *) fontp);
		fontp = fontp1;
		if (list_fonts) Fputs("(reusing) ", stdout);
		break;
	    }
	    fontpp = &fontp1->next;
	}

	if (old_fonts == &current_font) old_fonts = &fontp->next;
	fontp->next = current_font;
	current_font = fontp;
	if (list_fonts)
	    puts(fontp->fontname);
}

/*
 *      process_preamble reads the information in the preamble and stores
 *      it into global variables for later use.
 */
static
process_preamble()
{
        ubyte   k;

        if (one(dvi_file) != PRE)
		dvi_oops("DVI file doesn't start with preamble");
	if (one(dvi_file) != 2)
		dvi_oops("Wrong version of DVI output for this program");
	numerator     = four(dvi_file);
	denominator   = four(dvi_file);
	magnification = four(dvi_file);
	fraction = (((double) numerator * magnification)
	                                 / ((double) denominator * 1000.));
	fraction = fraction * (((long) pixels_per_inch)<<16) / 254000;
	k = one(dvi_file);
	Fread(job_id, sizeof(char), (int) k, dvi_file);
	job_id[k] = '\0';
}

/*
 *      find_postamble locates the beginning of the postamble
 *	and leaves the file ready to start reading at that location.
 */
#define	TMPSIZ	516	/* 4 trailer bytes + 512 junk bytes allowed */
static
find_postamble()
{
	long	pos;
	ubyte	temp[TMPSIZ];
	ubyte	*p;
	ubyte	*p1;
	ubyte	byte;

	Fseek(dvi_file, (long) 0, 2);
	pos = ftell(dvi_file) - TMPSIZ;
	if (pos < 0) pos = 0;
	Fseek(dvi_file, pos, 0);
	p = temp + fread((char *) temp, sizeof(char), TMPSIZ, dvi_file);
	for (;;) {
	    p1 = p;
	    while (p1 > temp && *(--p1) != TRAILER) ;
	    p = p1;
	    while (p > temp && *(--p) == TRAILER) ;
	    if (p <= p1 - 4) break;	/* found 4 TRAILER bytes */
	    if (p <= temp) dvi_oops("DVI file corrupted");
	}
	pos += p - temp;
	byte = *p;
	while (byte == TRAILER) {
	    Fseek(dvi_file, --pos, 0);
	    byte = one(dvi_file);
	}
	if (byte != 2)
	    dvi_oops("Wrong version of DVI output for this program");
	Fseek(dvi_file, pos - 4, 0);
	Fseek(dvi_file, sfour(dvi_file), 0);
}

/*
 *      read_postamble reads the information in the postamble,
 *	storing it into global variables.
 *      It also takes care of reading in all of the pixel files for the fonts
 *      used in the job.
 */
static
read_postamble()
{
        ubyte   cmnd;

        if (one(dvi_file) != POST)
	    dvi_oops("Postamble doesn't begin with POST");
	last_page_offset = four(dvi_file);
	if (numerator != four(dvi_file)
	          ||  denominator != four(dvi_file)
		  ||  magnification != four(dvi_file))
	    dvi_oops("Postamble doesn't match preamble");
		/* read largest box height and width */
	unshrunk_page_h = (spellfour(dvi_file) >> 16) + offset_y;
	if (unshrunk_page_h < unshrunk_paper_h)
	    unshrunk_page_h = unshrunk_paper_h;
	unshrunk_page_w = (spellfour(dvi_file) >> 16) + offset_x;
	if (unshrunk_page_w < unshrunk_paper_w)
	    unshrunk_page_w = unshrunk_paper_w;
	maxstack = two(dvi_file);
	total_pages = two(dvi_file);
	old_fonts = &current_font;
	font_not_found = False;
	do {
	    switch(cmnd = one(dvi_file)) {
	        case FNTDEF1:
	        case FNTDEF2:
	        case FNTDEF3:
	        case FNTDEF4:
		    define_font(cmnd);
		    break;
		case POSTPOST:
		    break;
		default:
		    dvi_oops("Non-fntdef command found in postamble");
	    }
	} while (cmnd != POSTPOST);
	if (font_not_found)
	    dvi_oops("Not all pixel files were found");
	/*
	 * free up fonts no longer in use
	 */
	{
	    struct font *fontp = *old_fonts;
	    struct font *fontp1;
	    register struct glyph *g;
	    *old_fonts = NULL;
	    while (fontp != NULL) {
		if (fontp->file != NULL) {
		    Fclose(fontp->file);
		    ++n_fonts_left;
		}
		free(fontp->fontname);
		free(fontp->filename);
		for (g = fontp->glyph; g <= fontp->glyph + fontp->maxchar; ++g)
		{
		    if (g->bitmap.bits) free(g->bitmap.bits);
		    if (g->bitmap2.bits) free(g->bitmap2.bits);
		}
		fontp1 = fontp->next;
		free((char *) fontp);
		fontp = fontp1;
	    }
	}
}

static
prepare_pages()
{
	int i;

        stack = (struct frame *)
	    xmalloc((unsigned) sizeof(struct frame) * (maxstack+1),
	    "stack frame");
	page_offset = (long *) xmalloc((unsigned) total_pages * sizeof(long),
	    "page directory");
	i = total_pages;
	page_offset[--i] = last_page_offset;
	Fseek(dvi_file, last_page_offset, 0);
	/*
	 * Follow back pointers through pages in the DVI file,
	 * storing the offsets in the page_offset table.
	 */
	while (i > 0) {
		Fseek(dvi_file, (long) (1+4+(9*4)), 1);
		Fseek(dvi_file, page_offset[--i] = four(dvi_file), 0);
	}
}

init_page()
{
	page_w = ROUNDUP(unshrunk_page_w, mane.shrinkfactor) + 2;
	page_h = ROUNDUP(unshrunk_page_h, mane.shrinkfactor) + 2;
}

/*
 *	init_dvi_file is the main subroutine for reading the startup information
 *	from the dvi file.
 */
static
init_dvi_file()
{
	(void) fstat(fileno(dvi_file), &fstatbuf);
	dvi_time = fstatbuf.st_mtime;
	process_preamble();
	find_postamble();
	read_postamble();
	prepare_pages();
	init_page();
	if (current_page >= total_pages) current_page = total_pages - 1;
	hush_spec_now = hush_spec;
}

/**
 **	open_dvi_file opens the dvi file and calls init_dvi_file() to
 **	initialize it.
 **/

open_dvi_file()
{
	char *errmsg;

	if ((dvi_file = fopen(dvi_name, OPEN_MODE)) == NULL) {
		int n = strlen(dvi_name);
		char *file = dvi_name;

		if (strcmp(dvi_name + n - sizeof(".dvi") + 1, ".dvi") == 0) {
			perror(dvi_name);
			exit(1);
		}
		dvi_name = xmalloc((unsigned) n + sizeof(".dvi"),
		    "dvi file name");
		Sprintf(dvi_name, "%s.dvi", file);
		if ((dvi_file = fopen(dvi_name, OPEN_MODE)) == NULL) {
			perror(dvi_name);
			exit(1);
		}
	}

	if (errmsg = (char *) setjmp(dvi_env)) oops(errmsg);
	init_dvi_file();
}

/**
 **	Release all shrunken bitmaps for all fonts.
 **/

reset_fonts()
{
        register struct font *f;
	register struct glyph *g;

	for (f = current_font; f != NULL; f = f->next)
	    for (g = f->glyph; g <= f->glyph + f->maxchar; ++g)
		if (g->bitmap2.bits) {
		    free(g->bitmap2.bits);
		    g->bitmap2.bits = NULL;
		}
}

/**
 **	Check for changes in dvi file.
 **/

Boolean
check_dvi_file()
{
	if (dvi_file == NULL || fstat(fileno(dvi_file), &fstatbuf) != 0
	    || fstatbuf.st_mtime != dvi_time) {
		if (dvi_file) Fclose(dvi_file);
		free((char *) stack);
		free((char *) page_offset);
		dvi_file = fopen(dvi_name, OPEN_MODE);
		if (dvi_file == NULL)
		    dvi_oops("Cannot reopen dvi file.");
		if (list_fonts) Putchar('\n');
		init_dvi_file();
		redraw_page();
		return False;
	}
	return True;
}
