/* 
 *	genps.c: PostScript driver for fig2dev
 *
*/

#if defined(hpux) || defined(SYSV)
#include <sys/types.h>
#endif
#include <sys/file.h>
#include <stdio.h>
#include <math.h>
#include <pwd.h>
#include "pi.h"
#include "object.h"
#include "fig2dev.h"
#include "psfonts.h"

#define		PAGE_WIDTH		612	/* points; 8.5" */
#define		PAGE_HEIGHT		792	/* points; 11" */
#define		TRUE			1
#define		FALSE			0
#define		POINT_PER_INCH		72
#define		ULIMIT_FONT_SIZE	300

int		coord_system;
int		show_page = 0;
int		cur_thickness;
int		center = 0;
int		landscape = 0;

extern int 	v2_flag, v21_flag;

#define GRAYVAL(F)	((F) <= 21 ? (21-(F))/20.0 : 0.0)

#define		BEGIN_PROLOG	"\
/$F2psDict 32 dict def \n\
$F2psDict begin\n\
	$F2psDict /mtrx matrix put\n\
"
#define		ELLIPSE_PS	" \
/DrawEllipse {\n\
	/endangle exch def\n\
	/startangle exch def\n\
	/yrad exch def\n\
	/xrad exch def\n\
	/y exch def\n\
	/x exch def\n\
	/savematrix mtrx currentmatrix def\n\
	x y translate xrad yrad scale 0 0 1 startangle endangle arc\n\
	savematrix setmatrix\n\
	} def\n\
"
/* The original PostScript definition for adding a spline section to the
 * current path uses recursive bisection.  The following definition using the
 * curveto operator is more efficient since it executes at compiled rather
 * than interpreted code speed.  The Bezier control points are 2/3 of the way
 * from z1 (and z3) to z2.
 *
 * ---Rene Llames, 21 July 1988.
 */
#define		SPLINE_PS	" \
/DrawSplineSection {\n\
	/y3 exch def\n\
	/x3 exch def\n\
	/y2 exch def\n\
	/x2 exch def\n\
	/y1 exch def\n\
	/x1 exch def\n\
	/xa x1 x2 x1 sub 0.666667 mul add def\n\
	/ya y1 y2 y1 sub 0.666667 mul add def\n\
	/xb x3 x2 x3 sub 0.666667 mul add def\n\
	/yb y3 y2 y3 sub 0.666667 mul add def\n\
	x1 y1 lineto\n\
	xa ya xb yb x3 y3 curveto\n\
	} def\n\
"
#define		END_PROLOG	"\
	end\n\
	/$F2psBegin {$F2psDict begin /$F2psEnteredState save def} def\n\
	/$F2psEnd {$F2psEnteredState restore end} def\n\
	%%EndProlog\n\
"

void genps_option(opt, optarg)
char opt;
char *optarg;
{
	int i;

	switch (opt) {

	case 'f':
		for ( i = 1; i <= MAX_PSFONT + 1; i++ )
			if ( !strcmp(optarg, PSfontnames[i]) ) break;

		if ( i > MAX_PSFONT + 1 )
			fprintf(stderr,
			    "warning: non-standard font name %s\n", optarg);

	    	psfontnames[0] = psfontnames[1] = optarg;
	    	PSfontnames[0] = PSfontnames[1] = optarg;
	    	break;

	case 'c':
	    	center = 1;
		break;

	case 's':
		if (font_size <= 0 || font_size > ULIMIT_FONT_SIZE) {
			fprintf(stderr,
				"warning: font size %d out of bounds\n", font_size);
		}
		break;

	case 'P':
		show_page = 1;
		break;

      	case 'm':
      	case 'L':
		break;

      	case 'l':
		landscape = 1;
		break;

	default:
		put_msg(Err_badarg, opt, "ps");
		exit(1);
		break;
	}
}

void genps_start(objects)
F_compound	*objects;
{
	char		host[256];
	struct passwd	*who;
	long		when;
	extern char	*ctime(), *strcpy();
	extern long	time();
	double		tx, scalex, scaley;
	double		dx, dy, origx, origy;
	int		itmp;

	coord_system = objects->nwcorner.y;
	scalex = scaley = mag * POINT_PER_INCH / (double)objects->nwcorner.x;
	/* convert to point unit */
	llx = (int)ceil(llx * scalex); lly = (int)ceil(lly * scaley);
	urx = (int)ceil(urx * scalex); ury = (int)ceil(ury * scaley);
	if (landscape)          /* swap x,y */
		{
		itmp = llx; llx = lly; lly = itmp;
		itmp = urx; urx = ury; ury = itmp;
		}
	if (center) {
	    dx = urx - llx;
	    dy = ury - lly;
	    tx = (PAGE_WIDTH - dx) / 2.0;
	    if (landscape)
		origx=0;
	    else
		origx = tx - llx;
	    urx = (llx=tx) + dx;
	    ury = (PAGE_HEIGHT + dy) / 2.0;
	    if (coord_system == 2)
		origy = ury + lly;
	    else
		origy = ury - dy - lly;
	    lly = ury - dy;
	    }
	else {
	    origx = 0.0;
	    origy = PAGE_HEIGHT;
	    }

	if (coord_system == 2) scaley = -scaley;

	fprintf(tfp, "%%!\n");	/* PostScript magic strings */
	who = getpwuid(getuid());
	if (-1 == gethostname(host, sizeof(host)))
	    (void)strcpy(host, "unknown-host!?!?");
	(void) time(&when);
	fprintf(tfp, "%%%%Title: %s\n", ((from) ? from : "stdin"));
	fprintf(tfp, "%%%%Creator: %s\n", prog);
	fprintf(tfp, "%%%%CreationDate: %s", ctime(&when));
	fprintf(tfp, "%%%%For: %s@%s (%s)\n",
			who->pw_name, host, who->pw_gecos);
	fprintf(tfp, "%%%%Pages: %d\n", show_page);

	if (show_page)
	  fprintf(tfp, "%%%%BoundingBox: %d %d %d %d\n", llx, lly, urx, ury);
	else
	  fprintf(tfp, "%%%%BoundingBox: 0 0 %d %d\n", urx-llx, ury-lly);

	fprintf(tfp, "%%%%EndComments\n");
	fprintf(tfp, "%s\n", BEGIN_PROLOG);
	if (ellipse_exist(objects)) fprintf(tfp, "%s\n", ELLIPSE_PS);
	if (normal_spline_exist(objects)) fprintf(tfp, "%s\n", SPLINE_PS);
	fprintf(tfp, "%s\n", END_PROLOG);
	fprintf(tfp, "$F2psBegin\n");
	fprintf(tfp, "1 setlinecap 1 setlinejoin\n");

	if (!show_page) fprintf(tfp, "%d %d translate\n", -llx, lly);
	fprintf(tfp, "%f %f translate %.3f %.3f scale\n",
		origx, (show_page ? origy : ury-lly), scalex, scaley);

	/* **** Land scape mode ****/
	if (landscape)
	    fprintf(tfp, "%d 0 translate 90 rotate\n", PAGE_WIDTH);

	/*    if ((t = PAGE_HEIGHT / SCALE / WIDTH) < 1.0)
	*	fprintf(tfp, "%f %f scale\n", t, t);
	*   }
	*else if ((t = PAGE_HEIGHT / SCALE / HEIGHT) < 1.0)
	*    fprintf(tfp, "%f %f scale\n", t, t);
	*/

	}

void genps_end()
{
	if (show_page) fprintf(tfp, "showpage\n");
	fprintf(tfp, "$F2psEnd\n");
	}

static set_style(s, v)
int	s;
double	v;
{
	if (s == DASH_LINE) {
	    if (v > 0.0) fprintf(tfp, "\t[%f] 0 setdash\n", v);
	    }
	else if (s == DOTTED_LINE) {
	    if (v > 0.0) fprintf(tfp, "\t[1 %f] 0 setdash\n", v);
	    }
	}

static reset_style(s, v)
int	s;
double	v;
{
	if (s == DASH_LINE) {
	    if (v > 0.0) fprintf(tfp, "\t[] 0 setdash\n");
	    }
	else if (s == DOTTED_LINE) {
	    if (v > 0.0) fprintf(tfp, "\t[] 0 setdash\n");
	    }
	}

static set_linewidth(w)
int	w;
{
	extern int	cur_thickness;

	if (w != cur_thickness) {
	    cur_thickness = w;
	    fprintf(tfp, "%.3f setlinewidth\n", 1.0 * cur_thickness);
	    }
	}

void genps_line(l)
F_line	*l;
{
	F_point		*p, *q;
	/* JNT */
	int		radius;
	
	set_linewidth(l->thickness);
	radius = l->radius;                /* radius of rounded-corner boxes */
	p = l->points;
	q = p->next;
	if (q == NULL) { /* A single point line */
	    fprintf(tfp, "newpath %d %d moveto %d %d lineto stroke\n",
			p->x, p->y, p->x, p->y);
	    return;
	    }
	if (l->back_arrow)
	    draw_arrow_head((double)q->x, (double)q->y, (double)p->x,
			(double)p->y, l->back_arrow->ht, l->back_arrow->wid);
	set_style(l->style, l->style_val);
	fprintf(tfp, "%% Polyline\n");
	/* JNT */
	        if (l->type == T_ARC_BOX)
                {
                register int xmin,xmax,ymin,ymax;

                xmin = xmax = p->x;
                ymin = ymax = p->y;
                while (p->next != NULL) /* find lower left and upper right corne
rs */
                        {
                        p=p->next;
                        if (xmin > p->x)
                                xmin = p->x;
                        else if (xmax < p->x)
                                xmax = p->x;
                        if (ymin > p->y)
                                ymin = p->y;
                        else if (ymax < p->y)
                                ymax = p->y;
			}
                fprintf(tfp, "newpath %d %d moveto",xmin+radius, ymin);
                fprintf(tfp, " %d %d %d %d %d arcto 4 {pop} repeat",
                                xmin, ymin, xmin, ymax-radius, radius);
                fprintf(tfp, " %d %d %d %d %d arcto 4 {pop} repeat", /* arc thro
ugh bl to br */
                                xmin, ymax, xmax-radius, ymax, radius);
                fprintf(tfp, " %d %d %d %d %d arcto 4 {pop} repeat", /* arc thro
ugh br to tr */
                                xmax, ymax, xmax, ymin+radius, radius);
                fprintf(tfp, " %d %d %d %d %d arcto 4 {pop} repeat", /* arc thro
ugh tr to tl */
                                xmax, ymin, xmin+radius, ymin, radius);
		}
        else
                {
	fprintf(tfp, "newpath %d %d moveto", p->x, p->y);
	while (q->next != NULL) {
	    p = q;
	    q = q->next;
	    fprintf(tfp, " %d %d lineto", p->x, p->y);
	    }
		}
	if (l->type == T_POLYLINE)
	    fprintf(tfp, " %d %d lineto stroke\n", q->x, q->y);
	else
	    fprintf(tfp, " closepath ");
	if (l->area_fill && (int)l->area_fill != DEFAULT)
		fprintf(tfp, "gsave %6.3f setgray fill grestore ",
			GRAYVAL(l->area_fill));
	fprintf(tfp, "stroke\n");
	reset_style(l->style, l->style_val);
	if (l->for_arrow)
	    draw_arrow_head((double)p->x, (double)p->y, (double)q->x,
			(double)q->y, l->for_arrow->ht, l->for_arrow->wid);
	}

void genps_spline(s)
F_spline	*s;
{
	if (int_spline(s))
	    genps_itp_spline(s);
	else
	    genps_ctl_spline(s);
	}

genps_itp_spline(s)
F_spline	*s;
{
	F_point		*p, *q;
	F_control	*a, *b;

	set_linewidth(s->thickness);
	a = s->controls;
	p = s->points;
	if (s->back_arrow)
	    draw_arrow_head(a->rx, a->ry, (double)p->x,
			(double)p->y, s->back_arrow->ht, s->back_arrow->wid);

	set_style(s->style, s->style_val);
	fprintf(tfp, "%% Interpolated spline\n");
	fprintf(tfp, "newpath %d %d moveto\n", p->x, p->y);
	for (q = p->next; q != NULL; p = q, q = q->next) {
	    b = a->next;
	    fprintf(tfp, "\t%.3f %.3f %.3f %.3f %d %d curveto\n",
			a->rx, a->ry, b->lx, b->ly, q->x, q->y);
	    a = b;
	    b = b->next;
	    }
	if (closed_spline(s)) fprintf(tfp, " closepath ");
	if (s->area_fill && (int)s->area_fill != DEFAULT)
		fprintf(tfp, "gsave %6.3f setgray fill grestore ",
			GRAYVAL(s->area_fill));
	fprintf(tfp, " stroke\n");
	reset_style(s->style, s->style_val);

	if (s->for_arrow)
	    draw_arrow_head(a->lx, a->ly, (double)p->x,
			(double)p->y, s->for_arrow->ht, s->for_arrow->wid);
	}

genps_ctl_spline(s)
F_spline	*s;
{
	double		a, b, c, d, x1, y1, x2, y2, x3, y3;
	F_point		*p, *q;

	/*
	if (first) {
	    first = FALSE;
	    fprintf(tfp, "%s\n", SPLINE_PS);
	    }
	*/

	p = s->points;
	x1 = p->x; y1 = p->y;
	p = p->next;
	c = p->x; d = p->y;
	set_linewidth(s->thickness);
	x3 = a = (x1 + c) / 2;
	y3 = b = (y1 + d) / 2;
	if (s->back_arrow) {
	    draw_arrow_head(c, d, x1, y1, s->back_arrow->ht, s->back_arrow->wid);
	    }
	set_style(s->style, s->style_val);
	if (! closed_spline(s)) {
	    fprintf(tfp, "%% Open spline\n");
	    fprintf(tfp, "newpath %.3f %.3f moveto %.3f %.3f lineto\n",
			x1, y1, x3, y3);
	    }
	else {
	    fprintf(tfp, "%% Closed spline\n");
	    fprintf(tfp, "newpath %.3f %.3f moveto\n", a, b);
	    }
	for (q = p->next; q != NULL; p = q, q = q->next) {
	    x1 = x3; y1 = y3;
	    x2 = c;  y2 = d;
	    c = q->x; d = q->y;
	    x3 = (x2 + c) / 2;
	    y3 = (y2 + d) / 2;
	    fprintf(tfp, "\t%.3f %.3f %.3f %.3f %.3f %.3f DrawSplineSection\n",
			x1, y1, x2, y2, x3, y3);
	    }
	/*
	* At this point, (x2,y2) and (c,d) are the position of the 
	* next-to-last and last point respectively, in the point list
	*/
	if (closed_spline(s)) {
	    fprintf(tfp, "\t%.3f %.3f %.3f %.3f %.3f %.3f DrawSplineSection closepath ",
			x3, y3, c, d, a, b);
	    }
	else {
	    fprintf(tfp, "\t%.3f %.3f lineto ", c, d);
	    }
	if (s->area_fill && (int)s->area_fill != DEFAULT)
		fprintf(tfp, "gsave %6.3f setgray fill grestore ",
			GRAYVAL(s->area_fill));
	fprintf(tfp, "stroke\n");
	reset_style(s->style, s->style_val);
	if (s->for_arrow) {
	    draw_arrow_head(x2, y2, c, d, s->for_arrow->ht,
				s->for_arrow->wid);
	    }
	}

void genps_ellipse(e)
F_ellipse	*e;
{
	set_linewidth(e->thickness);
	set_style(e->style, e->style_val);
	if (e->angle == 0)
	{
	    fprintf(tfp, "%% Ellipse\n");
	    fprintf(tfp, "newpath %d %d %d %d 0 360 DrawEllipse ",
	          e->center.x, e->center.y, e->radiuses.x, e->radiuses.y);
	}
	else
	{
	    fprintf(tfp, "%% Rotated Ellipse\n");
	    fprintf(tfp, "gsave\n");
	    fprintf(tfp, "%d %d translate\n",e->center.x, e->center.y);
	    fprintf(tfp, "%6.3f rotate\n",e->angle*180/M_PI);
	    fprintf(tfp, "newpath 0 0 %d %d 0 360 DrawEllipse ",
		 e->radiuses.x, e->radiuses.y);
	}
	if (e->area_fill && (int)e->area_fill != DEFAULT)
		fprintf(tfp, "gsave %6.3f setgray fill grestore ",
			GRAYVAL(e->area_fill));
	fprintf(tfp, "stroke\n");
	if (e->angle != 0)
	    fprintf(tfp, "grestore\n");
	reset_style(e->style, e->style_val);
	}

#define	TEXT_PS		"\
/%s findfont %.3f scalefont setfont\n\
"
void genps_text(t)
F_text	*t;
{
	char		*cp;

	fprintf(tfp, TEXT_PS, PSFONT(t), PSFONTMAG(t));

	fprintf(tfp, "%d %d moveto \n", t->base_x,  t->base_y);
	if (coord_system == 2) fprintf(tfp, "1 -1 scale\n");

	/* this loop escapes characters '(', ')', and '\' */
	fputc('(', tfp);
	for(cp = t->cstring; *cp; cp++) {
	      if (strchr("()\\", *cp)) fputc('\\', tfp);
	      fputc(*cp, tfp);
	      }
	fputc(')', tfp);

	if ((t->type == T_CENTER_JUSTIFIED) || (t->type == T_RIGHT_JUSTIFIED)){

	  	fprintf(tfp, "dup stringwidth pop ");
		if (t->type == T_CENTER_JUSTIFIED) fprintf(tfp, "2 div ");
		fprintf(tfp, "neg 0 rmoveto ");
	      	}

	else if ((t->type != T_LEFT_JUSTIFIED) && (t->type != DEFAULT))
		fprintf(stderr, "Text incorrectly positioned\n");

	fprintf(tfp, " gsave %6.3f rotate show grestore ", t->angle*180/M_PI);
	if (coord_system == 2) fprintf(tfp, "1 -1 scale\n");

	}

void genps_arc(a)
F_arc	*a;
{
	double		angle1, angle2, dx, dy, radius, x, y;
	double		cx, cy, sx, sy, ex, ey;
	int		direction;

	cx = a->center.x; cy = a->center.y;
	sx = a->point[0].x; sy = a->point[0].y;
	ex = a->point[2].x; ey = a->point[2].y;

	if (coord_system == 2)
	    direction = !a->direction;
	else
	    direction = a->direction;
	set_linewidth(a->thickness);
	if (a->for_arrow) {
	    arc_tangent(cx, cy, ex, ey, direction, &x, &y);
	    draw_arrow_head(x, y, ex, ey, a->for_arrow->ht, a->for_arrow->wid);
	    }
	if (a->back_arrow) {
	    arc_tangent(cx, cy, sx, sy, !direction, &x, &y);
	    draw_arrow_head(x, y, sx, sy, a->back_arrow->ht, a->back_arrow->wid);
	    }
	dx = cx - sx;
	dy = cy - sy;
	radius = hypot(dx, dy);
	angle1 = atan2(sy-cy, sx-cx) * 180 / M_PI;
	angle2 = atan2(ey-cy, ex-cx) * 180 / M_PI;
	/* direction = 1 -> Counterclockwise */
	set_style(a->style, a->style_val);
	fprintf(tfp, "newpath %.3f %.3f %.3f %.3f %.3f %s\n",
		cx, cy, radius, angle1, angle2,
		((direction == 1) ? "arc" : "arcn"));
	if (a->area_fill && (int)a->area_fill != DEFAULT)
		fprintf(tfp, "gsave %6.3f setgray fill grestore ",
			GRAYVAL(a->area_fill));
	fprintf(tfp, "stroke\n");
	reset_style(a->style, a->style_val);
	}

static arc_tangent(x1, y1, x2, y2, direction, x, y)
double	x1, y1, x2, y2, *x, *y;
int	direction;
{
	if (direction) { /* counter clockwise  */
	    *x = x2 + (y2 - y1);
	    *y = y2 - (x2 - x1);
	    }
	else {
	    *x = x2 - (y2 - y1);
	    *y = y2 + (x2 - x1);
	    }
	}

/*	draw arrow heading from (x1, y1) to (x2, y2)	*/

static draw_arrow_head(x1, y1, x2, y2, arrowht, arrowwid)
double	x1, y1, x2, y2, arrowht, arrowwid;
{
	double	x, y, xb, yb, dx, dy, l, sina, cosa;
	double	xc, yc, xd, yd;

	dx = x2 - x1;  dy = y1 - y2;
	l = hypot(dx, dy);
	sina = dy / l;  cosa = dx / l;
	xb = x2*cosa - y2*sina;
	yb = x2*sina + y2*cosa;
	x = xb - arrowht;
	y = yb - arrowwid / 2;
	xc = x*cosa + y*sina;
	yc = -x*sina + y*cosa;
	y = yb + arrowwid / 2;
	xd = x*cosa + y*sina;
	yd = -x*sina + y*cosa;
	fprintf(tfp, "newpath %.3f %.3f moveto %.3f %.3f lineto %.3f %.3f lineto stroke\n",
		xc, yc, x2, y2, xd, yd);
	}

static ellipse_exist(ob)
F_compound	*ob;
{
	F_compound	*c;

	if (NULL != ob->ellipses) return(1);

	for (c = ob->compounds; c != NULL; c = c->next) {
	    if (ellipse_exist(c)) return(1);
	    }

	return(0);
	}

static normal_spline_exist(ob)
F_compound	*ob;
{
	F_spline	*s;
	F_compound	*c;

	for (s = ob->splines; s != NULL; s = s->next) {
	    if (normal_spline(s)) return(1);
	    }

	for (c = ob->compounds; c != NULL; c = c->next) {
	    if (normal_spline_exist(c)) return(1);
	    }

	return(0);
	}

struct driver dev_ps = {
     	genps_option,
	genps_start,
	genps_arc,
	genps_ellipse,
	genps_line,
	genps_spline,
	genps_text,
	genps_end,
	INCLUDE_TEXT
};
