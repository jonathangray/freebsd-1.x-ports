/*
 *
 *    G N U P L O T  --  term.c
 *
 *  Copyright (C) 1986, 1987  Colin Kelley, Thomas Williams
 *
 *  You may use this code as you wish if credit is given and this message
 *  is retained.
 *
 *  Please e-mail any useful additions to vu-vlsi!plot so they may be
 *  included in later releases.
 *
 *  This file should be edited with 4-column tabs!  (:set ts=4 sw=4 in vi)
 */

#include "gnuplot.h"
#include <stdio.h>
#include "xlisp.h"
extern LVAL s_stdout, s_unbound;
static LVAL s_plotout = NIL;

LOCAL LVAL get_plot_stream()
{
  LVAL stream;

  if (s_plotout == NIL) {
    s_plotout = xlenter("*PLOT-OUTPUT*");
    if (getvalue(s_plotout) == s_unbound) 
      setvalue(s_plotout, getvalue(s_stdout));
  }

  stream = getvalue(s_plotout);
  if (! streamp(stream) && ! ustreamp(stream)) xlerror("not a stream", stream);

  return(stream);
}

extern struct termentry term_tbl[];
int term = 1;  /* gives tek */

#define NICE_LINE		0
#define POINT_TYPES		6

do_point(x,y,number)
     int x,y;
     int number;
{
  register int htic,vtic;
  register struct termentry *t;

  number %= POINT_TYPES;
  t = &term_tbl[term];
  htic = (t->h_tic/2);	/* should be in term_tbl[] in later version */
  vtic = (t->v_tic/2);	

  if ( x < t->h_tic || y < t->v_tic || x >= t->xmax-t->h_tic ||
      y >= t->ymax-t->v_tic ) 
    return;		/* add clipping in later version maybe */

  switch(number) {
  case 0: /* do diamond */ 
    (*t->move)(x-htic,y);
    (*t->vector)(x,y-vtic);
    (*t->vector)(x+htic,y);
    (*t->vector)(x,y+vtic);
    (*t->vector)(x-htic,y);
    (*t->move)(x,y);
    (*t->vector)(x,y);
    break;
  case 1: /* do plus */ 
    (*t->move)(x-htic,y);
    (*t->vector)(x-htic,y);
    (*t->vector)(x+htic,y);
    (*t->move)(x,y-vtic);
    (*t->vector)(x,y-vtic);
    (*t->vector)(x,y+vtic);
    break;
  case 2: /* do box */ 
    (*t->move)(x-htic,y-vtic);
    (*t->vector)(x+htic,y-vtic);
    (*t->vector)(x+htic,y+vtic);
    (*t->vector)(x-htic,y+vtic);
    (*t->vector)(x-htic,y-vtic);
    (*t->move)(x,y);
    (*t->vector)(x,y);
    break;
  case 3: /* do X */ 
    (*t->move)(x-htic,y-vtic);
    (*t->vector)(x-htic,y-vtic);
    (*t->vector)(x+htic,y+vtic);
    (*t->move)(x-htic,y+vtic);
    (*t->vector)(x-htic,y+vtic);
    (*t->vector)(x+htic,y-vtic);
    break;
  case 4: /* do triangle */ 
    (*t->move)(x,y+(4*vtic/3));
    (*t->vector)(x-(4*htic/3),y-(2*vtic/3));
    (*t->vector)(x+(4*htic/3),y-(2*vtic/3));
    (*t->vector)(x,y+(4*vtic/3));
    (*t->move)(x,y);
    (*t->vector)(x,y);
    break;
  case 5: /* do star */ 
    (*t->move)(x-htic,y);
    (*t->vector)(x-htic,y);
    (*t->vector)(x+htic,y);
    (*t->move)(x,y-vtic);
    (*t->vector)(x,y-vtic);
    (*t->vector)(x,y+vtic);
    (*t->move)(x-htic,y-vtic);
    (*t->vector)(x-htic,y-vtic);
    (*t->vector)(x+htic,y+vtic);
    (*t->move)(x-htic,y+vtic);
    (*t->vector)(x-htic,y+vtic);
    (*t->vector)(x+htic,y-vtic);
    break;
  }
}

/*
 * general point routine
 */
line_and_point(x,y,number)
     int x,y,number;
{
  /* temporary(?) kludge to allow terminals with bad linetypes 
     to make nice marks */
  
  (*term_tbl[term].linetype)(NICE_LINE);
  do_point(x,y,number);
}


#define TEK40XMAX 1024
#define TEK40YMAX 780

#define TEK40XLAST (TEK40XMAX - 1)
#define TEK40YLAST (TEK40YMAX - 1)

#define TEK40VCHAR		25
#define TEK40HCHAR		14
#define TEK40VTIC		11
#define TEK40HTIC		11	

#define HX 0x20		/* bit pattern to OR over 5-bit data */
#define HY 0x20
#define LX 0x40
#define LY 0x60

#define LOWER5 31
#define UPPER5 (31<<5)


  TEK40init()
{
}


TEK40graphics()
{
  xlputstr(get_plot_stream(), "\033\014");
  /*                   1
		       1. clear screen
		       */
  return(0);
}


TEK40text()
{
  TEK40move(0,12);
  xlputstr(get_plot_stream(), "\037");
  /*                   1
		       1. into alphanumerics
		       */
}


TEK40linetype(linetype)
     int linetype;
{
}


TEK40move(x,y)
     unsigned int x,y;
{
  xlputc(get_plot_stream(), '\035');	/* into graphics */
  TEK40vector(x,y);
}


TEK40vector(x,y)
     unsigned int x,y;
{
  xlputc(get_plot_stream(), (HY | (y & UPPER5)>>5));
  xlputc(get_plot_stream(), (LY | (y & LOWER5)));
  xlputc(get_plot_stream(), (HX | (x & UPPER5)>>5));
  xlputc(get_plot_stream(), (LX | (x & LOWER5)));
}


TEK40lrput_text(row,str)
     unsigned int row;
     char str[];
{
  TEK40move(TEK40XMAX - TEK40HTIC - TEK40HCHAR*(strlen(str)+1),
	    TEK40VTIC + TEK40VCHAR*(row+1));
  xlputc(get_plot_stream(), '\037');
  xlputstr(get_plot_stream(), str);
  xlputc(get_plot_stream(), '\n');
}


TEK40ulput_text(row,str)
     unsigned int row;
     char str[];
{
  TEK40move(TEK40HTIC, TEK40YMAX - TEK40VTIC - TEK40VCHAR*(row+1));
  xlputc(get_plot_stream(), '\037');
  xlputstr(get_plot_stream(), str);
  xlputc(get_plot_stream(), '\n');
}


TEK40reset()
{
}



UNKNOWN_null()
{
}


/*
 * term_tbl[] contains an entry for each terminal.  "unknown" must be the
 *   first, since term is initialized to 0.
 */
struct termentry term_tbl[] = {
  {"unknown", 100, 100, 1, 1, 1, 1, UNKNOWN_null, UNKNOWN_null, UNKNOWN_null,
     UNKNOWN_null, UNKNOWN_null, UNKNOWN_null, UNKNOWN_null, UNKNOWN_null,
     UNKNOWN_null, UNKNOWN_null},
  {"tek40xx",TEK40XMAX,TEK40YMAX,TEK40VCHAR, TEK40HCHAR, TEK40VTIC, 
      TEK40HTIC, TEK40init, TEK40reset, TEK40text, TEK40graphics, 
      TEK40move, TEK40vector, TEK40linetype, TEK40lrput_text,
      TEK40ulput_text, line_and_point}
  
};

#define TERMCOUNT 2


