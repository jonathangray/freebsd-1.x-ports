/* wn_color.c - colormap routines */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char wn_wn_color_c_sccsid[] = "@(#)wn_color.c	1.15 25/4/92 (UKC)";

#include <sys/types.h>
#include <stdlib.h>

#include <local/ukcprog.h>

#include "wn.h"
#include "wn_priv.h"
#include "wn_color.h"
#include "wn_misc.h"
#include "wn_cu.h"

static void set_mono_colors PROTO((color_t *color, int ncolors));

int wn__Use_mono = FALSE;

#ifdef SUNVIEW
static int get_pixels_and_planes PROTO((int npixels, int nplanes, int unused_contig, int cms_size, int *pixels, int *p_planes));
static void set_cms_size PROTO((int size));
static int get_pixnum PROTO((char *s));

#include <sunwindow/cms.h>

/*  The old names for WN_FG and WN_BG in wn were BLACK and WHITE.
 *  These conflict with the #defines in cms_mono.h so...
 */
#undef BLACK
#undef WHITE

#include <sunwindow/cms_mono.h>
#endif

int
wn_get_nplanes()
{
#ifdef X11
	return DisplayPlanes(wn__Dpy, DefaultScreen(wn__Dpy));
#endif
#ifdef SUNVIEW
	return WN_TO_W(WN_STDWIN)->w_pw->pw_pixrect->pr_depth;
#endif /* SUNVIEW */
}

static void
set_mono_colors(colors, ncolors)
color_t *colors;
int ncolors;
{
	while (ncolors > 0)
		colors[--ncolors].co_pixel = WN_FG;
}

#ifdef SUNVIEW
#define CMS_SIZE	256

/*  If Cms_used[i] is non zero, pixel i is in use.  We rely on this
 *  array being staticly initialised to all zeroes.
 */
static char Cms_used[CMS_SIZE];

/*  Current color map entries.
 */
static unsigned char Red[CMS_SIZE], Green[CMS_SIZE], Blue[CMS_SIZE];

/*  Current size of the color map segment.
 */
static int Cms_size = 0;

/*  Initial size set by _wn_init_sunview_colors().  Can be changed by
 *  the application via wn_npixels_hint().
 */
static int Initial_cms_size = 2;
#endif /* SUNVIEW */

void
wn_free_pixels(colors, ncolors)
color_t *colors;
int ncolors;
{
#ifdef SUNVIEW
	int i;

	for (i = 0; i < ncolors; i++)
		Cms_used[colors[i].co_pixel] = FALSE;
#endif /* SUNVIEW */
#ifdef X11
	int i;
	xpixel_t *pixels;

	if (DisplayPlanes(wn__Dpy, DefaultScreen(wn__Dpy)) == 1) {
		return;
	}
	pixels = (xpixel_t *) wn__e_malloc(ncolors * sizeof(int));
	for (i = 0; i < ncolors; i++)
		pixels[i] = colors[i].co_pixel;
	XFreeColors(wn__Dpy, _wn_Cmap, pixels, ncolors, (xplanemask_t)0);
	free((char *)pixels);
#endif /* X11 */
}

void
wn_npixels_hint(npixels)
int npixels;
{
#ifdef SUNVIEW
	int cms_size;

	/*  For WN_FG and WN_BG.
	 */
	npixels += 2;

	for (cms_size = 2; cms_size < npixels && cms_size < CMS_SIZE; cms_size *= 2)
		;
		
	Initial_cms_size = cms_size;
#endif /* SUNVIEW */
}

#ifdef SUNVIEW
static int
get_pixnum(s)
char *s;
{
	int pixel;

	pixel = atoi(s);
	if (pixel < 0)
		pixel = 0;
	if (pixel > 255)
		pixel = 255;
	return pixel;
}

void
_wn_set_fgbg(is_fg, sred, sgreen, sblue)
int is_fg;
char *sred, *sgreen, *sblue;
{
	int pixel;

	pixel = is_fg ? 1 : 0;
	Red[pixel] = get_pixnum(sred);
	Green[pixel] = get_pixnum(sgreen);
	Blue[pixel] = get_pixnum(sblue);
	Cms_used[pixel] = TRUE;
}

void
_wn_init_sunview_colors(w, is_mainwin)
swin_t *w;
int is_mainwin;
{
	unsigned char def_red[2], def_green[2], def_blue[2];
	struct pixwin *pw;
	char name[CMS_NAMESIZE];
	int i, planes;

	/*  All windows under wn share the same colormap, so if this is
	 *  not the main window, just set the colormap name from the
	 *  main window.
	 */
	if (!is_mainwin) {
		pw_getcmsname(WN_TO_W(WN_STDWIN)->w_pw, name);
		pw_setcmsname(pw, name);
		return;
	}

	pw = w->w_pw;

	if (!Cms_used[0] || !Cms_used[1]) {
		pw_setcmsname(pw, CMS_MONOCHROME);
		pw_getcolormap(pw, 0, 2, def_red, def_green, def_blue);
		for (i = 0; i < 2; i++) {
			if (!Cms_used[i]) {
				Red[i] = def_red[i];
				Green[i] = def_green[i];
				Blue[i] = def_blue[i];
				Cms_used[i] = TRUE;
			}
		}
	}

	Cms_size = Initial_cms_size;
	if (Cms_size != 2) {
		Red[Cms_size - 1] = Red[1];
		Green[Cms_size - 1] = Green[1];
		Blue[Cms_size - 1] = Blue[1];
		Cms_used[Cms_size - 1] = TRUE;
	}
	(void) sprintf(name, "wn.%d.initial", getpid());
	pw_setcmsname(pw, name);
	pw_putcolormap(pw, 0, Cms_size, Red, Green, Blue);
	pw_getattributes(pw, &planes);
	planes &= 0xff;
	w->w_bw->bw_planes = planes;
}

static void
set_cms_size(size)
int size;
{
	static int cmap_num = 0;
	char name[CMS_NAMESIZE];
	struct rect r;
	int cwd, wfd;
	int planes;
	struct pixwin *pw;

	if (size > 256)
		wn__panic("size too large in set_cms_size");

	Red[size - 1] = Red[1];
	Green[size - 1] = Red[1];
	Blue[size - 1] = Red[1];
	Cms_used[size - 1] = TRUE;

	(void) sprintf(name, "wn.%d.%d", getpid(), cmap_num++);
	pw = WN_TO_W(WN_STDWIN)->w_pw;
	pw_setcmsname(pw, name);
	pw_putcolormap(pw, 0, size, Red, Green, Blue);
	pw_getattributes(pw, &planes);
	planes &= 0xff;
	WN_TO_W(WN_STDWIN)->w_bw->bw_planes = planes;
	pr_putattributes(pw->pw_prretained, &planes);

	/*  Force a repaint of the window by covering it with another
	 *  window then uncovering it.
	 */
	wfd = WN_TO_W(WN_STDWIN)->w_pw->pw_windowfd;
	cwd = win_getnewwindow();
	if (cwd == -1)
		wn__panic("win_getnewwindow failed");
	win_setlink(cwd, WL_PARENT, win_fdtonumber(wfd));
	win_getsize(wfd, &r); 
	win_setrect(cwd, &r);
	win_insert(cwd);
	close(cwd);
		
	Cms_size = size;
}

/*  Allocate pixels and planes, as with the X XAllocColorCells() request.
 *
 *  Algorithm here is based on the one in the X server.
 *  We shift the plane mask left across the entire set of planes.
 *  For each position of plane mask, we step through all the
 *  pixel numbers, testing for each pixel number whether that
 *  pixel is free in all the planes asked for.
 *
 *  We only do the simple case where contig is TRUE.
 */
/* ARGSUSED */
static int
get_pixels_and_planes(npixels, nplanes, unused_contig, cms_size, pixels, p_planes)
int npixels, nplanes, unused_contig, cms_size;
int *pixels, *p_planes;
{
	int mask, base, npixels_found, fpm, i, pixels_found;
	register int pm, pixel, pm_lim;

	mask = (1 << nplanes) - 1;
	for (base = 1; (mask & cms_size) == 0; mask <<= 1, base <<= 1) {
		pixels_found = 0;
		pm_lim = base << nplanes;
		for (pixel = 0; pixel < cms_size; pixel++) {
			for (pm = 0; pm < pm_lim; pm += base) {
				if (Cms_used[pixel | pm])
					break;
				Cms_used[pixel | pm] = TRUE;
			}
			if (pm == pm_lim) {
				pixels[pixels_found++] = pixel;
				if (pixels_found == npixels) {
					*p_planes = mask;
					return 0;
				}
			}
			else {
				for (fpm = 0; fpm < pm; fpm += base)
					Cms_used[pixel | fpm] = FALSE;
			}
		}
		
		/*  Not enough pixels for this plane mask - free the ones
		 *  we allocated.
		 */
		for (i = 0; i < pixels_found; i++)
			for (pm = 0; pm < pm_lim; pm += base)
				Cms_used[pixels[i] | pm] = FALSE;
	}
	return -1;
}
#endif /* SUNVIEW */

int
wn_get_pixels_and_planes(npixels, nplanes, contig, pixels, p_planes)
int npixels, nplanes, contig;
int *pixels, *p_planes;
{
#ifdef SUNVIEW
	int pixel, nfree, nwanted, new_cms_size;

	/*  This really doesn't make any sense on a monochrome display.
	 */
	if (Cms_size == 0)
		return -1;

	nfree = 0;
	for (pixel = 0; pixel < Cms_size; ++pixel)
		if (!Cms_used[pixel])
			++nfree;

	/*  If there aren't (npixels * 2 ^ nplanes) free pixels, we know
	 *  the allocation will fail, so make the map at least that big.
	 */
	nwanted = npixels * (1 << nplanes);
	new_cms_size = Cms_size;

	while (nfree < nwanted) {
		if (new_cms_size > 256)
			wn__panic("cms_size botch");
		if (new_cms_size == 256)
			return -1;
		nfree += new_cms_size;
		new_cms_size *= 2;
	}

	while (get_pixels_and_planes(npixels, nplanes, contig, new_cms_size,
							pixels, p_planes) != 0) {
		if (new_cms_size > 256)
			wn__panic("cms_size botch");
		if (new_cms_size == 256)
			return -1;
		new_cms_size *= 2;
	}

	if (new_cms_size != Cms_size)
		set_cms_size(new_cms_size);
	
	return 0;
#endif /* SUNVIEW */
#ifdef X11
	unsigned long planes;

	if (wn__Use_mono)
		return -1;

	/*  BUG: we cast pixels to "unsigned long *" from "unsigned int *".
	 *  Revolting int == long assumption.
	 */
	if (XAllocColorCells(wn__Dpy, _wn_Cmap, contig,
			     &planes, (unsigned)nplanes,
			     (unsigned long *)pixels, (unsigned)npixels) == 0)
		return -1;
	*p_planes = planes;
	return 0;
#endif /* X11 */
}

int
wn_get_pixels(colors, ncolors)
color_t *colors;
int ncolors;
{
#ifdef SUNVIEW
	int pixel, cno, nwanted, minsize, new_cms_size;

	if (Cms_size == 0) {
		set_mono_colors(colors, ncolors);
		return 0;
	}

	cno = 0;
	for (pixel = 0; pixel < Cms_size; pixel++) {
		if (!Cms_used[pixel]) {
			Cms_used[cno] = TRUE;
			colors[cno++].co_pixel = pixel;
			if (cno >= ncolors)
				break;
		}
	}
	if (cno >= ncolors)
		return 0;
	nwanted = ncolors - cno;
	minsize = nwanted + Cms_size + 2;
	if (nwanted + Cms_size > CMS_SIZE)
		return -1;

	for (new_cms_size = Cms_size; new_cms_size < minsize; new_cms_size *= 2)
		;

	while (cno < ncolors) {
		if (Cms_used[pixel])
			wn__panic("reusing used cmap cell in wn_get_pixel_colors");
		colors[cno++].co_pixel = pixel;
		Cms_used[pixel++] = TRUE;
	}

	set_cms_size(new_cms_size);
	return 0;
#endif /* SUNVIEW */
#ifdef X11
	int i;
	xplanemask_t junk_planemask;
	xpixel_t *pixels;

	if (wn__Use_mono)
		return -1;

	if (DisplayPlanes(wn__Dpy, DefaultScreen(wn__Dpy)) == 1) {
		set_mono_colors(colors, ncolors);
		return 0;
	}

	pixels = (xpixel_t *) wn__e_malloc(ncolors * sizeof(int));
	if (XAllocColorCells(wn__Dpy, _wn_Cmap, FALSE, &junk_planemask,
						0, pixels, (unsigned)ncolors) == 0) {
		free((char *)pixels);
		return -1;
	}
	for (i = 0; i < ncolors; i++)
		colors[i].co_pixel = pixels[i];
	free((char *)pixels);
	return 0;
#endif /* X11 */
}

void
wn_get_pixel_colors(colors, ncolors)
color_t *colors;
int ncolors;
{
#ifdef SUNVIEW
	int i, pixel;

	if (Cms_size != 0) {
		for (i = 0; i < ncolors; i++) {
			pixel = colors[i].co_pixel;
			colors[i].co_red = Red[pixel] << 8;
			colors[i].co_green = Green[pixel] << 8;
			colors[i].co_blue = Blue[pixel] << 8;
		}
	}
#endif /* SUNVIEW */
#ifdef X11
	XColor cdef;
	color_t *co;

	if (DisplayPlanes(wn__Dpy, DefaultScreen(wn__Dpy)) == 1) {
		for (co = colors; co < colors + ncolors; co++)
			co->co_red = co->co_green = co->co_blue = 0xffff;
	}
	else {
		for (co = colors; co < colors + ncolors; co++) {
			cdef.pixel = co->co_pixel;
			XQueryColor(wn__Dpy, _wn_Cmap, &cdef);
			co->co_red = cdef.red;
			co->co_green = cdef.green;
			co->co_blue = cdef.blue;
		}
	}
#endif /* X11 */
}

int
wn_parse_color(name, color)
const char *name;
color_t *color;
{
#ifdef X11
	XColor xcolor;

	if (XParseColor(wn__Dpy, _wn_Cmap, name, &xcolor) == 0)
		return -1;
	color->co_red = xcolor.red;
	color->co_blue = xcolor.blue;
	color->co_green = xcolor.green;
	return 0;
#endif /* X11 */
#ifdef SUNVIEW
	return -1;
#endif /* SUNVIEW */
}

void
wn_set_pixel_colors(colors, ncolors)
color_t *colors;
int ncolors;
{
#ifdef SUNVIEW
	int i, pixel;

	if (Cms_size == 0)
		return;

	for (i = 0; i < ncolors; i++) {
		pixel = colors[i].co_pixel;
		Red[pixel] = colors[i].co_red >> 8;
		Green[pixel] = colors[i].co_green >> 8;
		Blue[pixel] = colors[i].co_blue >> 8;

		/*  The background pixel is duplicated at the top of the
		 *  colormap (by decree of Sunview).
		 */
		if (pixel == 1) {
			Red[Cms_size - 1] = Red[pixel];
			Green[Cms_size - 1] = Green[pixel];
			Blue[Cms_size - 1] = Blue[pixel];
		}
	}
	pw_putcolormap(WN_TO_W(WN_STDWIN)->w_pw, 0, Cms_size, Red, Green, Blue);
#endif /* SUNVIEW */
#ifdef X11
	XColor *cdefs;
	register XColor *cdef;
	register color_t *co;

	if (DisplayPlanes(wn__Dpy, DefaultScreen(wn__Dpy)) > 1) {
		cdefs = (XColor *) wn__e_malloc(ncolors * sizeof(XColor));
		cdef = cdefs;
		for (co = colors; co < colors + ncolors; co++, cdef++) {
			cdef->pixel = co->co_pixel;
			cdef->red = co->co_red;
			cdef->green = co->co_green;
			cdef->blue = co->co_blue;
#ifdef X11
			cdef->flags = DoRed | DoGreen | DoBlue;
			if (cdef->pixel == WN_FG)
				wn__set_x11_cursor_colors(cdef, (XColor *)NULL);
			if (cdef->pixel == WN_BG)
				wn__set_x11_cursor_colors((XColor *)NULL, cdef);
#endif /* X11 */
		}
		XStoreColors(wn__Dpy, _wn_Cmap, cdefs, ncolors);
		free((char *)cdefs);
	}
#endif /* X11 */
}

int
wn_get_pixels_by_color(colors, ncolors)
color_t *colors;
int ncolors;
{
	if (wn_get_pixels(colors, ncolors) != 0)
		return -1;
	wn_set_pixel_colors(colors, ncolors);
	return 0;
}
