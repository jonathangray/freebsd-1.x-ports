/*
 * Support drawing routines for TeXsun and TeX
 *
 *      Copyright, (C) 1987, 1988 Tim Morgan, UC Irvine
 *
 * At the time these routines are called, the values of hh and vv should
 * have been updated to the upper left corner of the graph (the position
 * the \special appears at in the dvi file).  Then the coordinates in the
 * graphics commands are in terms of a virtual page with axes oriented the
 * same as the Imagen and the SUN normally have:
 *
 *                      0,0
 *                       +-----------> +x
 *                       |
 *                       |
 *                       |
 *                      \ /
 *                       +y
 *
 * Angles are measured in the conventional way, from +x towards +y.
 * Unfortunately, that reverses the meaning of "counterclockwise"
 * from what it's normally thought of.
 *
 * A lot of floating point arithmetic has been converted to integer
 * arithmetic for speed.  In some places, this is kind-of kludgy, but
 * it's worth it.
 */

#include "config.h"
#include <kpathsea/c-ctype.h>
#include "epsf.h"
#define	MAXPOINTS	300	/* Max points in a path */
#define	TWOPI		(3.14159265359*2.0)
#define	MAX_PEN_SIZE	7	/* Max pixels of pen width */


static	int	xx[MAXPOINTS], yy[MAXPOINTS];	/* Path in milli-inches */
static	int	path_len = 0;	/* # points in current path */
static	int	pen_size = 1;	/* Pixel width of lines drawn */

static	Boolean	whiten = False;
static	Boolean	shade = False;
static	Boolean	blacken = False;

/* Unfortunately, these values also appear in dvisun.c */
#define	xRESOLUTION	(pixels_per_inch/shrink_factor)
#define	yRESOLUTION	(pixels_per_inch/shrink_factor)


/*
 * Issue warning messages
 */
static	void
Warning(fmt, msg)
	char	*fmt, *msg;
{
	Fprintf(stderr, fmt, msg);
	(void) fputc('\n', stderr);
}


/*
 * Set the size of the virtual pen used to draw in milli-inches
 */
/* ARGSUSED */
static	void
set_pen_size(cp)
	char *cp;
{
	int	ps;

	if (sscanf(cp, " %d ", &ps) != 1) {
	    Warning("illegal .ps command format: %s", cp);
	    return;
	}
	pen_size = (ps * (xRESOLUTION + yRESOLUTION) + 1000) / 2000;
	if (pen_size < 1) pen_size = 1;
	else if (pen_size > MAX_PEN_SIZE) pen_size = MAX_PEN_SIZE;
}


/*
 * Print the line defined by previous path commands
 */
static	void
flush_path()
{
	register int i;
	int	last_min_x, last_max_x, last_min_y, last_max_y;

	last_min_x = 30000;
	last_min_y = 30000;
	last_max_x = -30000;
	last_max_y = -30000;
	for (i = 1; i < path_len; i++) {
	    if (xx[i] > last_max_x) last_max_x = xx[i];
	    if (xx[i] < last_min_x) last_min_x = xx[i];
	    if (yy[i] > last_max_y) last_max_y = yy[i];
	    if (yy[i] < last_min_y) last_min_y = yy[i];
	    line_btw(xx[i], yy[i], xx[i+1], yy[i+1]);
	}
	if (xx[path_len] > last_max_x) last_max_x = xx[path_len];
	if (xx[path_len] < last_min_x) last_min_x = xx[path_len];
	if (yy[path_len] > last_max_y) last_max_y = yy[path_len];
	if (yy[path_len] < last_min_y) last_min_y = yy[path_len];
	path_len = 0;
	do_attribute_path(last_min_x, last_max_x, last_min_y, last_max_y);
}


/*
 * Print a dashed line along the previously defined path, with
 * the dashes/inch defined.
 */

static	void
flush_dashed(cp, dotted)
	char	*cp;
	int	dotted;
{
	int	i;
	int	numdots;
	int	lx0, ly0, lx1, ly1;
	int	cx0, cy0, cx1, cy1;
	float	inchesperdash;
	double	d, spacesize, a, b, dx, dy, milliperdash;

	if (sscanf(cp, " %f ", &inchesperdash) != 1) {
	    Warning("illegal format for dotted/dashed line: %s", cp);
	    return;
	}
	if (path_len <= 1 || inchesperdash <= 0.0) {
	    Warning("illegal conditions for dotted/dashed line", "");
	    return;
	}
	milliperdash = inchesperdash * 1000.0;
	lx0 = xx[1];	ly0 = yy[1];
	lx1 = xx[2];	ly1 = yy[2];
	dx = lx1 - lx0;
	dy = ly1 - ly0;
	if (dotted) {
	    numdots = sqrt(dx*dx + dy*dy) / milliperdash + 0.5;
	    if (numdots == 0) numdots = 1;
	    for (i = 0; i <= numdots; i++) {
		a = (float) i / (float) numdots;
		cx0 = lx0 + a * dx + 0.5;
		cy0 = ly0 + a * dy + 0.5;
		dot_at(cx0, cy0);
	    }
	}
	else {
	    d = sqrt(dx*dx + dy*dy);
	    numdots = d / (2.0 * milliperdash) + 1.0;
	    if (numdots <= 1)
		line_btw(lx0, ly0, lx1, ly1);
	    else {
		spacesize = (d - numdots * milliperdash) / (numdots - 1);
		for (i = 0; i < numdots - 1; i++) {
		    a = i * (milliperdash + spacesize) / d;
		    b = a + milliperdash / d;
		    cx0 = lx0 + a * dx + 0.5;
		    cy0 = ly0 + a * dy + 0.5;
		    cx1 = lx0 + b * dx + 0.5;
		    cy1 = ly0 + b * dy + 0.5;
		    line_btw(cx0, cy0, cx1, cy1);
		    b += spacesize / d;
		}
		cx0 = lx0 + b * dx + 0.5;
		cy0 = ly0 + b * dy + 0.5;
		line_btw(cx0, cy0, lx1, ly1);
	    }
	}
	path_len = 0;
}


/*
 * Add a point to the current path
 */
static	void
add_path(cp)
	char	*cp;
{
	int	pathx, pathy;

	if (++path_len >= MAXPOINTS) oops("Too many points");
	if (sscanf(cp, " %d %d ", &pathx, &pathy) != 2)
	    oops("Malformed path command");
	xx[path_len] = pathx;
	yy[path_len] = pathy;
}


/*
 * Draw to a floating point position
 */
static void
im_fdraw(x, y)
	double	x,y;
{
	if (++path_len >= MAXPOINTS) oops("Too many arc points");
	xx[path_len] = x + 0.5;
	yy[path_len] = y + 0.5;
}


/*
 * Draw an ellipse with the indicated center and radices.
 */
static	void
draw_ellipse(xc, yc, xr, yr)
	int	xc, yc, xr, yr;
{
	double	angle, theta;
	int	n;
	int	px0, py0, px1, py1;

	angle = (xr + yr) / 2.0;
	theta = sqrt(1.0 / angle);
	n = TWOPI / theta + 0.5;
	if (n < 12) n = 12;
	else if (n > 80) n = 80;
	n /= 2;
	theta = TWOPI / n;

	angle = 0.0;
	px0 = xc + xr;		/* cos(0) = 1 */
	py0 = yc;		/* sin(0) = 0 */
	while ((angle += theta) <= TWOPI) {
	    px1 = xc + xr*cos(angle) + 0.5;
	    py1 = yc + yr*sin(angle) + 0.5;
	    line_btw(px0, py0, px1, py1);
	    px0 = px1;
	    py0 = py1;
	}
	line_btw(px0, py0, xc + xr, yc);
}

/*
 * Draw an arc
 */
static	void
arc(cp, invis)
	char	*cp;
	Boolean	invis;
{
	int	xc, yc, xrad, yrad, n;
	float	start_angle, end_angle, angle, theta, r;
	double	xradius, yradius, xcenter, ycenter;

	if (sscanf(cp, " %d %d %d %d %f %f ", &xc, &yc, &xrad, &yrad,
		&start_angle, &end_angle) != 6) {
	    Warning("illegal arc specification: %s", cp);
	    return;
	}

	if (invis) return;

	/* We have a specialized fast way to draw closed circles/ellipses */
	if (start_angle <= 0.0 && end_angle >= 6.282) {
	    draw_ellipse(xc, yc, xrad, yrad);
	    return;
	}
	xcenter = xc;
	ycenter = yc;
	xradius = xrad;
	yradius = yrad;
	r = (xradius + yradius) / 2.0;
	theta = sqrt(1.0 / r);
	n = 0.3 * TWOPI / theta + 0.5;
	if (n < 12) n = 12;
	else if (n > 80) n = 80;
	n /= 2;
	theta = TWOPI / n;
	flush_path();
	im_fdraw(xcenter + xradius * cos(start_angle),
	    ycenter + yradius * sin(start_angle));
	angle = start_angle + theta;
	if (end_angle < angle) end_angle += TWOPI;
	while (angle < end_angle) {
	    im_fdraw(xcenter + xradius * cos(angle),
		ycenter + yradius * sin(angle));
	    angle += theta;
	}
	im_fdraw(xcenter + xradius * cos(end_angle),
	    ycenter + yradius * sin(end_angle));
	flush_path();
}


/*
 * APPROXIMATE integer distance between two points
 */
#define	dist(x0, y0, x1, y1)	(abs(x0-x1)+abs(y0-y1))


/*
 * Draw a spline along the previously defined path
 */
static	void
flush_spline()
{
	int	xp, yp;
	int	N;
	int	lastx, lasty;
	Boolean	lastvalid = False;
	int	t1, t2, t3;
	int	steps;
	int	j;
	register int i, w;

#ifdef	lint
	lastx = lasty = -1;
#endif
	N = path_len + 1;
	xx[0] = xx[1];
	yy[0] = yy[1];
	xx[N] = xx[N-1];
	yy[N] = yy[N-1];
	for (i = 0; i < N - 1; i++) {	/* interval */
	    steps = (dist(xx[i], yy[i], xx[i+1], yy[i+1]) +
		dist(xx[i+1], yy[i+1], xx[i+2], yy[i+2])) / 80;
	    for (j = 0; j < steps; j++) {	/* points within */
		w = (j * 1000 + 500) / steps;
		t1 = w * w / 20;
		w -= 500;
		t2 = (750000 - w * w) / 10;
		w -= 500;
		t3 = w * w / 20;
		xp = (t1*xx[i+2] + t2*xx[i+1] + t3*xx[i] + 50000) / 100000;
		yp = (t1*yy[i+2] + t2*yy[i+1] + t3*yy[i] + 50000) / 100000;
		if (lastvalid) line_btw(lastx, lasty, xp, yp);
		lastx = xp;
		lasty = yp;
		lastvalid = True;
	    }
	}
	path_len = 0;
}


/*
 * Shade the last box, circle, or ellipse
 */
static	void
shade_last()
{
	blacken = whiten = False;
	shade = True;
}


/*
 * Make the last box, circle, or ellipse, white inside (shade with white)
 */
static	void
whiten_last()
{
	whiten = True;
	blacken = shade = False;
}


/*
 * Make last box, etc, black inside
 */
static	void
blacken_last()
{
	blacken = True;
	whiten = shade = False;
}


/*
 *	The following copyright message applies to the rest of this file.  --PV
 */

/*
 *	This program is Copyright (C) 1987 by the Board of Trustees of the
 *	University of Illinois, and by the author Dirk Grunwald.
 *
 *	This program may be freely copied, as long as this copyright
 *	message remaines affixed. It may not be sold, although it may
 *	be distributed with other software which is sold. If the
 *	software is distributed, the source code must be made available.
 *
 *	No warranty, expressed or implied, is given with this software.
 *	It is presented in the hope that it will prove useful.
 *
 *	Hacked in ignorance and desperation by jonah@db.toronto.edu
 */

/*
 *      The code to handle the \specials generated by tpic was modified
 *      by Dirk Grunwald using the code Tim Morgan at Univ. of Calif, Irvine
 *      wrote for TeXsun.
 */

#define	COMLEN	4

void
applicationDoSpecial(cp)
	char	*cp;
{
	char	command[COMLEN], *orig_cp;
	register int len;

	orig_cp = cp;
	while (ISSPACE(*cp)) ++cp;
	len = 0;
	while (!ISSPACE(*cp) && *cp && len < COMLEN-1) command[len++] = *cp++;
	command[len] = '\0';
	if (strcmp(command, "pn") == 0) set_pen_size(cp);
	else if (strcmp(command, "fp") == 0) flush_path();
	else if (strcmp(command, "da") == 0) flush_dashed(cp, 0);
	else if (strcmp(command, "dt") == 0) flush_dashed(cp, 1);
	else if (strcmp(command, "pa") == 0) add_path(cp);
	else if (strcmp(command, "ar") == 0) arc(cp, False);
	else if (strcmp(command, "ia") == 0) arc(cp, True);
	else if (strcmp(command, "sp") == 0) flush_spline();
	else if (strcmp(command, "sh") == 0) shade_last();
	else if (strcmp(command, "wh") == 0) whiten_last();
	else if (strcmp(command, "bk") == 0) blacken_last();
	/* throw away the path -- jansteen */
	else if (strcmp(command, "ip") == 0) path_len = 0;
	else if (strncmp(orig_cp, "PSfile=", 7) == 0
	         || strncmp(orig_cp, "psfile=", 7) == 0) epsfile(orig_cp);
	else if (strncmp(orig_cp, "ps::[begin]", 11)==0) psfig_setup(orig_cp);
	else if (strncmp(orig_cp, "ps::[end]", 9) == 0) /* nop */ ;
	else if (strncmp(orig_cp, "ps: plotfile ", 13) == 0) psfig(orig_cp);
	else if (strncmp(orig_cp, "ps::", 4)==0) psfig_setup_angle(orig_cp); 
	        /* check as last ps:: option! */
	else if (!hush_spec_now)
	    Fprintf(stderr, "%s:  special \"%s\" not implemented\n", prog,
		orig_cp);
}
