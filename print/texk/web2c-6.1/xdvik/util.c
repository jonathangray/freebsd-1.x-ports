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
 */

#include "config.h"
#include <kpathsea/c-fopen.h>
#include <kpathsea/c-vararg.h>

#ifdef VMS
#include <rmsdef.h>
#endif /* VMS */

/*
 *	General utility routines.
 */

/*
 *	Print error message and quit.
 */

#if	NeedVarargsPrototypes
NORETURN void
oops(_Xconst char *message, ...)
#else
/* VARARGS */
NORETURN void
oops(va_alist)
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
	Putc('\n', stderr);
	exit(1);
}

/*
 *	Either allocate storage or fail with explanation.
 */

char *
xmalloc(size, why)
	unsigned	size;
	_Xconst char	*why;
{
        /* Don't try to allocate zero bytes -- this fails on the Alpha.
           Obviously it would be better to avoid such allocations in the
           first place, but it's easier to do this, and I'm hoping Paul
           will fix the real bug. */
	char *mem = malloc(size ? size : 1);

	if (mem == NULL)
	    oops("! Cannot allocate %u bytes for %s.\n", size, why);
	return mem;
}

/*
 *	Allocate bitmap for given font and character
 */

void
alloc_bitmap(bitmap)
	register struct bitmap *bitmap;
{
	register unsigned int	size;

	/* width must be multiple of 16 bits for raster_op */
	bitmap->bytes_wide = ROUNDUP(bitmap->w, BITS_PER_BMUNIT) *
	    BYTES_PER_BMUNIT;
	size = bitmap->bytes_wide * bitmap->h;
	bitmap->bits = xmalloc(size != 0 ? size : 1, "character bitmap");
}


/*
 *	Close the pixel file for the least recently used font.
 */

static	void
close_a_file()
{
	register struct font *fontp;
	unsigned short oldest = ~0;
	struct font *f = NULL;
	for (fontp = font_head; fontp != NULL; fontp = fontp->next)
	    if (fontp->file != NULL && fontp->timestamp <= oldest) {
		f = fontp;
		oldest = fontp->timestamp;
	    }
	if (f == NULL)
	    oops("Can't find an open pixel file to close");
	Fclose(f->file);
	f->file = NULL;
	++n_files_left;
}

/*
 *	Open a file in the given mode.
 */

FILE *
xfopen(filename)
	_Xconst char	*filename;
{
	FILE	*f;

        /* Try not to let the file table fill up completely.  */
	if (n_files_left <= 10) close_a_file();
        errno = 0;
	f = fopen(filename, OPEN_MODE);
	/* If the open failed, try closing a file unconditionally.
  	   Interactive Unix 2.2.1, at least, doesn't set errno to EMFILE
  	   or ENFILE even when it should.  */
	if (f == NULL)
	{
	    n_files_left = 0;
	    close_a_file();
	    f = fopen(filename, OPEN_MODE);
	}
	return f;
}


/*
 *
 *      Read size bytes from the FILE fp, constructing them into a
 *      signed/unsigned integer.
 *
 */

unsigned long
num(fp, size)
	register FILE *fp;
	register int size;
{
	register long x = 0;

	while (size--) x = (x << 8) | one(fp);
	return x;
}

long
snum(fp, size)
	register FILE *fp;
	register int size;
{
	register long x;

#ifdef	__STDC__
	x = (signed char) getc(fp);
#else
	x = (unsigned char) getc(fp);
	if (x & 0x80) x -= 0x100;
#endif
	while (--size) x = (x << 8) | one(fp);
	return x;
}
