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

#include "xdvi.h"
#include "dvi.h"
#include <sys/stat.h>

#ifdef	sun
extern	char	*sprintf();
#endif

#ifndef	X_NOT_STDC_ENV
#include <stdlib.h>
#else
char	*realloc();
#endif
#if	defined(macII) && !defined(__STDC__) /* stdlib.h doesn't define these */
char	*realloc();
#endif /* macII */

#define	PK_PRE		247
#define	PK_ID		89
#define	PK_MAGIC	(PK_PRE << 8) + PK_ID
#define	GF_PRE		247
#define	GF_ID		131
#define	GF_MAGIC	(GF_PRE << 8) + GF_ID
#define	VF_PRE		247
#define	VF_ID_BYTE	202
#define	VF_MAGIC	(VF_PRE << 8) + VF_ID_BYTE
#define	PXL_MAGIC1	0
#define	PXL_MAGIC2	1001

#define	dvi_oops(str)	longjmp(dvi_env, (int) str);

static	struct stat fstatbuf;

static	Boolean	font_not_found;

/*
 * DVI preamble and postamble information.
 */
static	char	job_id[300];
static	long	numerator, denominator, magnification;

/*
 * Offset in DVI file of last page, set in read_postamble().
 */
static	long	last_page_offset;


/*
 *	free_vf_chain frees the vf_chain structure.
 */

static	void
free_vf_chain(tnp)
	struct tn *tnp;
{
	while (tnp != NULL) {
	    register struct tn *tnp1 = tnp->next;
	    free((char *) tnp);
	    tnp = tnp1;
	}
}


/*
 *	Release all shrunken bitmaps for all fonts.
 */

void
reset_fonts()
{
	register struct font *f;
	register struct glyph *g;

	for (f = font_head; f != NULL; f = f->next)
	    if ((f->flags & FONT_LOADED) && !(f->flags & FONT_VIRTUAL))
		for (g = f->glyph; g <= f->glyph + f->maxchar; ++g) {
		    if (g->bitmap2.bits) {
			free(g->bitmap2.bits);
			g->bitmap2.bits = NULL;
		    }
#ifdef	GREY
		    if (g->pixmap2) {
			XDestroyImage(g->image2);
			g->pixmap2 = NULL;
		    }
#endif
		}
}

/*
 *	realloc_font allocates the font structure to contain (newsize + 1)
 *	characters.
 */

void
realloc_font(fontp, newsize)
	struct font		*fontp;
	WIDEARG(ubyte, int)	newsize;
{
	struct glyph *glyph;

	glyph = fontp->glyph = (struct glyph *) realloc((char *) fontp->glyph,
	    ((unsigned int) newsize + 1) * sizeof(struct glyph));
	if (glyph == NULL) oops("! Cannot reallocate space for glyph array.");
	if (newsize > fontp->maxchar)
	    bzero((char *) (glyph + fontp->maxchar + 1),
		(int) (newsize - fontp->maxchar) * sizeof(struct glyph));
	maxchar = fontp->maxchar = newsize;
}


/*
 *	realloc_virtual_font does the same thing for virtual fonts.
 */

void
realloc_virtual_font(fontp, newsize)
	struct font		*fontp;
	WIDEARG(ubyte, int)	newsize;
{
	struct macro *macro;

	macro = fontp->macro = (struct macro *) realloc((char *) fontp->macro,
	    ((unsigned int) newsize + 1) * sizeof(struct macro));
	if (macro == NULL) oops("! Cannot reallocate space for macro array.");
	if (newsize > fontp->maxchar)
	    bzero((char *) (macro + fontp->maxchar + 1),
		(int) (newsize - fontp->maxchar) * sizeof(struct macro));
	maxchar = fontp->maxchar = newsize;
}


/*
 *	load_font locates the raster file and reads the index of characters,
 *	plus whatever other preprocessing is done (depending on the format).
 */

Boolean
load_font(fontp)
	struct font *fontp;
{
	float	fsize	= fontp->fsize;
	int	dpi	= fsize + 0.5;
	char	*font_found;
	int	size_found;
	int	magic;

	fontp->flags |= FONT_LOADED;
	fontp->file = font_open(fontp->fontname, &font_found,
	    (WIDEARG(float, double)) fsize, &size_found, fontp->magstepval,
	    &fontp->filename);
	if (fontp->file == NULL) {
	    Fprintf(stderr, "Can't find font %s.\n", fontp->fontname);
	    return True;
	}
	--n_files_left;
	if (font_found != NULL) {
	    Fprintf(stderr,
		    "Can't find font %s; using %s instead at %d dpi\n",
		    fontp->fontname, font_found, dpi);
	    free(fontp->fontname);
	    fontp->fontname = font_found;
	}
	else if (size_found > (int) (5 * 1.002 * fsize + 0.5) ||
		size_found < (int) (5 * 0.998 * fsize + 0.5))
	    Fprintf(stderr,
		"Can't find font %s at %d dpi; using %d dpi instead.\n",
		fontp->fontname, dpi, (size_found + 2) / 5);
	fontp->fsize = (float) size_found / 5;
	fontp->timestamp = ++current_timestamp;
	fontp->maxchar = maxchar = 255;
	fontp->set_char_p = set_char;
	magic = two(fontp->file);
#ifdef	USE_PK
	if (magic == PK_MAGIC) read_PK_index(fontp);
	else
#endif
#ifdef	USE_GF
	    if (magic == GF_MAGIC) read_GF_index(fontp);
	else
#endif
	    if (magic == VF_MAGIC) read_VF_index(fontp);
	else
#ifdef	USE_PXL
	    if (magic == PXL_MAGIC1 && two(fontp->file) == PXL_MAGIC2)
		read_PXL_index(fontp);
	else
#endif
	    oops("Cannot recognize format for font file %s", fontp->filename);

	if (fontp->flags & FONT_VIRTUAL) {
	    while (maxchar > 0 && fontp->macro[maxchar].pos == NULL) --maxchar;
	    if (maxchar < 255)
		realloc_virtual_font(fontp, WIDEARG(,(int)) maxchar);
	}
	else {
	    while (maxchar > 0 && fontp->glyph[maxchar].addr == 0) --maxchar;
	    if (maxchar < 255)
		realloc_font(fontp, WIDEARG(,(int)) maxchar);
	}
	return False;
}


/*
 *	MAGSTEPVALUE - If the given magnification is close to a \magstep
 *	or a \magstephalf, then return twice the number of \magsteps.
 *	Otherwise return NOMAGSTP.
 */

#define	NOMAGSTP (-29999)
#define	NOBUILD	29999

static	int
magstepvalue(mag)
	float	*mag;
{
	int	m	= 0;
	double	fmag	= *mag;
	double	xmag	= pixels_per_inch;
	float	margin	= fmag * 0.002;

	if (fmag < pixels_per_inch)
	    for (;;) {
		if (xmag - fmag < margin && -(xmag - fmag) < margin) {
		    *mag = xmag;
		    return m;
		}
		if (xmag < fmag) break;
		xmag *= 0.9128709292;
		--m;
	    }
	else
	    for (;;) {
		if (xmag - fmag < margin && -(xmag - fmag) < margin) {
		    *mag = xmag;
		    return m;
		}
		if (xmag > fmag) break;
		xmag *= 1.095445115;
		++m;
	    }
	return NOMAGSTP;
}


/*
 *	reuse_font recursively sets the flags for font structures being reused.
 */

static	void
reuse_font(fontp)
	struct font *fontp;
{
	struct tn *tnp;

	if (fontp->flags & FONT_IN_USE) return;

	fontp->flags |= FONT_IN_USE;
	if (list_fonts)
	    Printf("(reusing) %s at %d dpi\n", fontp->fontname,
		(int) (fontp->fsize + 0.5));
	if (fontp->flags & FONT_VIRTUAL)
	    for (tnp = fontp->vf_chain; tnp != NULL; tnp = tnp->next)
		reuse_font(tnp->fontp);
}


/*
 *      define_font reads the rest of the fntdef command and then reads in
 *      the specified pixel file, adding it to the global linked-list holding
 *      all of the fonts used in the job.
 */
void
define_font(file, cmnd, vfparent, tn_headpp)
	FILE		*file;
	WIDEARG(ubyte, unsigned int) cmnd;
	struct font	*vfparent;	/* vf parent of this font, or NULL */
	struct tn	**tn_headpp;	/* addr of head of list of TeXnumbers */
{
	register struct tn *tnp;
	struct font *fontp;
	float	fsize;
	double	scale_dimconv;
	int scale;
	int design;
	int magstepval;
	int len;
	char *fontname;
	int size;

	tnp = (struct tn *) xmalloc((unsigned) sizeof(struct tn),
	    "TeXnumber structure");
	tnp->next = *tn_headpp;
	*tn_headpp = tnp;
	tnp->TeXnumber = num(file, (int) cmnd - FNTDEF1 + 1);
	(void) four(file);	/* checksum */
	scale = four(file);
	design = four(file);
	len = one(file); len += one(file); /* sequence point in the middle */
	fontname = xmalloc((unsigned) len + 1, "font name");
	Fread(fontname, sizeof(char), len, file);
	fontname[len] = '\0';
	if(debug & DBG_PK)
	    Printf("Define font \"%s\" scale=%d design=%d\n",
		fontname, scale, design);
	if (vfparent == NULL) {
	    fsize = 0.001 * scale / design * magnification * pixels_per_inch;
	    scale_dimconv = dimconv;
	}
	else {
	    /*
	     *	The scaled size is given in units of vfparent->scale * 2 ** -20
	     *	SPELL units, so we convert it into SPELL units by multiplying by
	     *		vfparent->dimconv.
	     *	The design size is given in units of 2 ** -20 pt, so we convert
	     *	into SPELL units by multiplying by
	     *		(pixels_per_inch * 2**16) / (72.27 * 2**20).
	     */
	    fsize = (72.27 * (1<<4)) * vfparent->dimconv * scale / design;
	    scale_dimconv = vfparent->dimconv;
	}
	magstepval = magstepvalue(&fsize);
	size = 5 * fsize + 0.5;
	/*
	 * reuse font if possible
	 */
	for (fontp = font_head;; fontp = fontp->next) {
	    if (fontp == NULL) {		/* if font doesn't exist yet */
		if (list_fonts)
		    Printf("%s at %d dpi\n", fontname, (int) (fsize + 0.5));
		fontp = (struct font *) xmalloc((unsigned) sizeof(struct font),
		    "font structure");
		fontp->fontname = fontname;
		fontp->fsize = fsize;
		fontp->magstepval = magstepval;
		fontp->flags = FONT_IN_USE;
		fontp->dimconv = scale * scale_dimconv / (1<<20);
		fontp->set_char_p = load_n_set_char;
		if (vfparent == NULL) font_not_found |= load_font(fontp);
		fontp->next = font_head;
		font_head = fontp;
		break;
	    }
	    if (strcmp(fontname, fontp->fontname) == 0
		    && size == (int) (5 * fontp->fsize + 0.5)) {
			/* if font already in use */
		reuse_font(fontp);
		free(fontname);
		break;
	    }
	}
	tnp->fontp = fontp;
}


/*
 *      process_preamble reads the information in the preamble and stores
 *      it into global variables for later use.
 */
static	void
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
	dimconv = (((double) numerator * magnification)
		/ ((double) denominator * 1000.));
	dimconv = dimconv * (((long) pixels_per_inch)<<16) / 254000;
	specialConv = pixels_per_inch * magnification / 1000000.0;
	k = one(dvi_file);
	Fread(job_id, sizeof(char), (int) k, dvi_file);
	job_id[k] = '\0';
}

/*
 *      find_postamble locates the beginning of the postamble
 *	and leaves the file ready to start reading at that location.
 */
#define	TMPSIZ	516	/* 4 trailer bytes + 512 junk bytes allowed */
static	void
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
static	void
read_postamble()
{
	ubyte   cmnd;
	struct font	*fontp;
	struct font	**fontpp;

	if (one(dvi_file) != POST)
	    dvi_oops("Postamble doesn't begin with POST");
	last_page_offset = four(dvi_file);
	if (numerator != four(dvi_file)
		|| denominator != four(dvi_file)
		|| magnification != four(dvi_file))
	    dvi_oops("Postamble doesn't match preamble");
		/* read largest box height and width */
	unshrunk_page_h = (spell_conv(sfour(dvi_file)) >> 16) + offset_y;
	if (unshrunk_page_h < unshrunk_paper_h)
	    unshrunk_page_h = unshrunk_paper_h;
	unshrunk_page_w = (spell_conv(sfour(dvi_file)) >> 16) + offset_x;
	if (unshrunk_page_w < unshrunk_paper_w)
	    unshrunk_page_w = unshrunk_paper_w;
	(void) two(dvi_file);	/* max stack size */
	total_pages = two(dvi_file);
	font_not_found = False;
	while ((cmnd = one(dvi_file)) >= FNTDEF1 && cmnd <= FNTDEF4)
	    define_font(dvi_file, cmnd, (struct font *) NULL, &tn_head);
	if (cmnd != POSTPOST)
	    dvi_oops("Non-fntdef command found in postamble");
	if (font_not_found)
	    dvi_oops("Not all pixel files were found");
	/*
	 * free up fonts no longer in use
	 */
	fontpp = &font_head;
	while ((fontp = *fontpp) != NULL)
	    if (fontp->flags & FONT_IN_USE)
		fontpp = &fontp->next;
	    else {
		if (debug & DBG_PK)
		    Printf("Discarding font \"%s\" at %d dpi\n",
			fontp->fontname, (int) (fontp->fsize + 0.5));
		*fontpp = fontp->next;		/* remove from list */
		free(fontp->fontname);
		if (fontp->flags & FONT_LOADED) {
		    if (fontp->file != NULL) {
			Fclose(fontp->file);
			++n_files_left;
		    }
		    free(fontp->filename);
		    if (fontp->flags & FONT_VIRTUAL) {
			register struct macro *m;

			for (m = fontp->macro;
				m <= fontp->macro + fontp->maxchar; ++m)
			    if (m->free_me) free((char *) m->pos);
			free((char *) fontp->macro);
			free_vf_chain(fontp->vf_chain);
		    }
		    else {
			register struct glyph *g;

			for (g = fontp->glyph;
				g <= fontp->glyph + fontp->maxchar; ++g) {
			    if (g->bitmap.bits != NULL) free(g->bitmap.bits);
			    if (g->bitmap2.bits != NULL) free(g->bitmap2.bits);
#ifdef	GREY
			    if (g->pixmap2 != NULL) XDestroyImage(g->image2);
#endif
			}
			free((char *) fontp->glyph);
		    }
		    free((char *) fontp);
		}
	    }
}

static	void
prepare_pages()
{
	int i;

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

void
init_page()
{
	page_w = ROUNDUP(unshrunk_page_w, mane.shrinkfactor) + 2;
	page_h = ROUNDUP(unshrunk_page_h, mane.shrinkfactor) + 2;
}

/*
 *	init_dvi_file is the main subroutine for reading the startup
 *	information from the dvi file.  Returns True on success.
 */

static	Boolean
init_dvi_file()
{
	(void) fstat(fileno(dvi_file), &fstatbuf);
	if (S_ISDIR(fstatbuf.st_mode))
	    return False;
	dvi_time = fstatbuf.st_mtime;
	process_preamble();
	find_postamble();
	read_postamble();
	prepare_pages();
	init_page();
	if (current_page >= total_pages) current_page = total_pages - 1;
	hush_spec_now = hush_spec;
	return True;
}

/**
 **	open_dvi_file opens the dvi file and calls init_dvi_file() to
 **	initialize it.
 **/

void
open_dvi_file()
{
	char	*errmsg;
	int	n;
	char	*file;

	if ((errmsg = (char *) setjmp(dvi_env)) != NULL) oops(errmsg);

	if ((dvi_file = fopen(dvi_name, OPEN_MODE)) != NULL && init_dvi_file())
	    return;

	n = strlen(dvi_name);
	file = dvi_name;

	if (strcmp(dvi_name + n - sizeof(".dvi") + 1, ".dvi") == 0) {
	    perror(dvi_name);
	    exit(1);
	}
	dvi_name = xmalloc((unsigned) n + sizeof(".dvi"), "dvi file name");
	Sprintf(dvi_name, "%s.dvi", file);
	if ((dvi_file = fopen(dvi_name, OPEN_MODE)) == NULL
		|| !init_dvi_file()) {
	    perror(dvi_name);
	    exit(1);
	}
}

/**
 **	Check for changes in dvi file.
 **/

Boolean
check_dvi_file()
{
	struct font *fontp;

	if (dvi_file == NULL || fstat(fileno(dvi_file), &fstatbuf) != 0
	    || fstatbuf.st_mtime != dvi_time) {
		if (dvi_file) {
		    Fclose(dvi_file);
		    if (list_fonts) Putchar('\n');
		}
		free((char *) page_offset);
		free_vf_chain(tn_head);
		tn_head = NULL;
		for (fontp = font_head; fontp != NULL; fontp = fontp->next)
		    fontp->flags &= ~FONT_IN_USE;
		if ((dvi_file = fopen(dvi_name, OPEN_MODE)) == NULL
			|| !init_dvi_file())
		    dvi_oops("Cannot reopen dvi file.");
		reconfig();
		redraw_page();
		return False;
	}
	return True;
}
