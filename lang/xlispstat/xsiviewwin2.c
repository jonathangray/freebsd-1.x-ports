/* xsiviewwin2 - XLISP interface to IVIEW dynamic graphics package.    */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"
#include "iview.h"

/* external functions */
extern LVAL integer_list_2(), integer_list_4(), peekarg(), xsgetmatrix(),
  arraydata();
extern double makedouble();

/* external variables */
extern LVAL s_true, s_normal, s_xor, s_solid, s_dashed, s_dotword;
extern LVAL sk_allocate, sk_dispose, sk_resize, sk_redraw, sk_do_idle,
  sk_install, sk_remove, sk_update, sk_select, s_title,
  s_window_width, s_window_height, s_menu, s_hardware_address,
  s_has_h_scroll, s_has_v_scroll, s_cross, s_arrow,
  s_color_index, s_cursor_index, s_symbol_index;
  
/**************************************************************************/
/**                                                                      **/
/**                      Window Management Functions                     **/
/**                                                                      **/
/**************************************************************************/

/* :REMOVE message for IVIEW-WINDOW-CLASS */
LVAL iview_window_remove()
{
  char *gwinfo;
  LVAL object;
  
  object = xlgaobject();
  gwinfo = StGWObWinInfo(object);
  xllastarg();
  
  if (gwinfo != nil) {
    StGWRemove(gwinfo);
    standard_hardware_clobber(object);
  }
  return(NIL);
}

static LVAL button_fcn;

static button_down_action(w, x, y)
     IVIEW_WINDOW w;
     int x, y;
{
  LVAL Lx, Ly;

  xlsave1(Lx);
  xlsave1(Ly);
  Lx = cvfixnum((FIXTYPE) x);
  Ly = cvfixnum((FIXTYPE) y);
  xsfuncall2(button_fcn, Lx, Ly);
  xlpopn(2);
}

LVAL iview_window_while_button_down()
{
  char *gwinfo;
  int motionOnly;
  
  gwinfo = StGWObWinInfo(xlgaobject());
  button_fcn = xlgetarg();
  motionOnly = (! moreargs() || xlgetarg() != NIL) ? TRUE : FALSE;
  xllastarg();
  
  StGWWhileButtonDown(gwinfo, button_down_action, motionOnly);
  
  return(NIL);
}

/**************************************************************************/
/**                                                                      **/
/**             Window State Access and Mutation Functions               **/
/**                                                                      **/
/**************************************************************************/

decode_lisp_color(arg)
     LVAL arg;
{
  LVAL val;
  
  val = xlgetprop(arg, s_color_index);
  if (! fixp(val)) xlerror("unknown color", arg);
  else return(getfixnum(val));
}

LVAL encode_lisp_color(color)
     int color;
{
  LVAL sym;
  
  sym = (LVAL) StGWGetColRefCon(color);
  if (! symbolp(sym)) xlfail("unknown color");
  return(sym);
}

static LVAL window_state(var)
     int var;
{
  LVAL object, arg, result;
  int value, set = FALSE;
  char *gwinfo;
  
  object = xlgaobject();
  gwinfo = StGWObWinInfo(object);
  if (gwinfo == nil) return(NIL);
  
  if (moreargs()) {
    set = TRUE;
    arg = (var != 'C') ? xlgasymbol() : xlgetarg();
  }
  xllastarg();

  if (set) {
    /* decode lisp argument */
    switch (var) {
    case 'T':
      if (arg == s_solid) value = 0;
      else if (arg == s_dashed) value = 1;
      else xlerror("unknown line type", arg);
      break;
    case 'M':
      if (arg == s_normal) value = 0;
      else if (arg == s_xor) value = 1;
      else xlerror("unknown drawing mode", arg);
      break;
    case 'D':
    case 'B': value = decode_lisp_color(arg); break;
    case 'C': value = (arg != NIL) ? TRUE : FALSE; break;
    default: xltoomany();
    }
  
    /* set the state variable */
    switch (var) {
    case 'T':  StGWSetLineType(gwinfo, value); break;
    case 'M':  StGWSetDrawMode(gwinfo, value); break;
    case 'D':  StGWSetDrawColor(gwinfo, (ColorCode) value); break;
    case 'B':  StGWSetBackColor(gwinfo, (ColorCode) value); break;
    case 'C':  StGWSetUseColor(gwinfo, value); break;
    }
  }

  /* read the state variable */
  switch (var) {
  case 'W':  value = StGWCanvasWidth(gwinfo); break;
  case 'H':  value = StGWCanvasHeight(gwinfo); break;
  case 'T':  value = StGWLineType(gwinfo); break;
  case 'M':  value = StGWDrawMode(gwinfo); break;
  case 'D':  value = (int) StGWDrawColor(gwinfo); break;
  case 'B':  value = (int) StGWBackColor(gwinfo); break;
  case 'C':  value = StGWUseColor(gwinfo); break;
  case 'R':  StGWReverseColors(gwinfo); 
             value = StGWBackColor(gwinfo);
             break;
  }
  
  /* encode result as lisp value */
  switch (var) {
  case 'W':
  case 'H': result = cvfixnum((FIXTYPE) value); break;
  case 'T': result = (value == 0) ? s_solid : s_dashed; break;
  case 'M': result = (value == 0) ? s_normal : s_xor; break;
  case 'D':
  case 'B':
  case 'R': result = encode_lisp_color(value); break;
  case 'C': result = (value) ? s_true : NIL; break;
  }
  
  return(result);
}

LVAL iview_window_canvas_width()   { return(window_state('W')); }
LVAL iview_window_canvas_height()  { return(window_state('H')); }
LVAL iview_window_line_type()      { return(window_state('T')); }
LVAL iview_window_draw_mode()      { return(window_state('M')); }
LVAL iview_window_draw_color()     { return(window_state('D')); }
LVAL iview_window_back_color()     { return(window_state('B')); }
LVAL iview_window_use_color()      { return(window_state('C')); } 
LVAL iview_window_reverse_colors() { return(window_state('R')); } 

LVAL iview_window_view_rect()
{
  LVAL object;
  int left, top, width, height;
  char *gwinfo;
  
  object = xlgaobject();
  xllastarg();  
  
  gwinfo = StGWObWinInfo(object);
  if (gwinfo == nil) return(NIL);
  else {
    StGWGetViewRect(gwinfo, &left, &top, &width, &height);
    return(integer_list_4(left, top, width, height));
  }
}

LVAL iview_window_line_width()
{
  char *gwinfo;
  int width, set = FALSE;
  
  gwinfo = StGWObWinInfo(xlgaobject());
  if (gwinfo == nil) return(NIL);
  
  if (moreargs()) {
    set = TRUE;
    width = getfixnum(xlgafixnum());
  }
  xllastarg();
  
  if (set) StGWSetLineWidth(gwinfo, width);
  StGWGetLineWidth(gwinfo, &width);
  return(cvfixnum((FIXTYPE) width));
}

/**************************************************************************/
/**                                                                      **/
/**                       Window Scrolling Functions                     **/
/**                                                                      **/
/**************************************************************************/

static LVAL has_scroll(which)
     int which;
{
  char *gwinfo;
  int has, size, width, height, set = FALSE;
  LVAL arg;
  
  gwinfo = StGWObWinInfo(xlgaobject());
  if (gwinfo == nil) return(NIL);
  
  if (moreargs()) {
    set = TRUE;
    arg = xlgetarg();
    has = (arg != NIL) ? TRUE : FALSE;
    if (has && arg == s_true) {
      StGetScreenSize(&width, &height);
      size = (width > height) ? width : height;
    }
    else if (has) {
      if (! fixp(arg)) xlerror("bad canvas size", arg);
      size = getfixnum(arg);
    }
    else size = 0;
  }
  xllastarg();
  
  if (set) 
    switch (which) {
    case 'H': StGWSetHasHscroll(gwinfo, has, size); break;
    case 'V': StGWSetHasVscroll(gwinfo, has, size); break;
    }
  switch (which) {
  case 'H': has = StGWHasHscroll(gwinfo); break;
  case 'V': has = StGWHasVscroll(gwinfo); break;
  }
  return((has) ? s_true : NIL);
}

LVAL iview_window_has_h_scroll() { return(has_scroll('H')); }
LVAL iview_window_has_v_scroll() { return(has_scroll('V')); }

LVAL iview_window_scroll()
{
  LVAL object;
  int h, v, set = FALSE;
  char *gwinfo;
  
  object = xlgaobject();
  gwinfo = StGWObWinInfo(object);
  if (gwinfo == nil) return(NIL);
  
  if (moreargs()) {
    set = TRUE;
    h = getfixnum(xlgafixnum());
    v = getfixnum(xlgafixnum());
  }    
  xllastarg();
  
  if (set) StGWSetScroll(gwinfo, h, v, TRUE);
  StGWGetScroll(gwinfo, &h, &v);
  
  return(integer_list_2(h, v));
}

static LVAL scroll_increments(which)
        int which;
{
  char *gwinfo;
  int inc, pageinc;
  
  gwinfo = StGWObWinInfo(xlgaobject());
  if (gwinfo == nil) return(NIL);
  
  if (moreargs()) {
    inc = getfixnum(xlgafixnum());
    pageinc = getfixnum(xlgafixnum());
    switch(which) {
    case 'H': StGWSetHscrollIncs(gwinfo, inc, pageinc); break;
    case 'V': StGWSetVscrollIncs(gwinfo, inc, pageinc); break;
    }
  }
  switch (which) {
  case 'H': StGWGetHscrollIncs(gwinfo, &inc, &pageinc); break;
  case 'V': StGWGetVscrollIncs(gwinfo, &inc, &pageinc); break;
  }
  
  return(integer_list_2(inc, pageinc));
}

LVAL iview_window_h_scroll_incs() { return(scroll_increments('H')); }
LVAL iview_window_v_scroll_incs() { return(scroll_increments('V')); }

/**************************************************************************/
/**                                                                      **/
/**                  Line and Rectangle Drawing Functions                **/
/**                                                                      **/
/**************************************************************************/

static LVAL draw(what, how)
        int what, how;
{
  LVAL object;
  int a, b, c, d;
  char *gwinfo;
  double angle1, angle2;
  
  object = xlgaobject();
  gwinfo = StGWObWinInfo(object);
  if (gwinfo == nil) return(NIL);
  
  a = getfixnum(xlgafixnum());
  b = getfixnum(xlgafixnum());
  if (what != 'P') {
    c = getfixnum(xlgafixnum());
    d = getfixnum(xlgafixnum());
  }
  if (what == 'A') {
    angle1 = makedouble(xlgetarg());
    angle2 = makedouble(xlgetarg());
  }
  xllastarg();
  
  switch(what) {
  case 'L': StGWDrawLine(gwinfo, a, b, c, d); break;
  case 'P': StGWDrawPoint(gwinfo, a, b); break;
  case 'R':
    switch (how) {
    case 'E': StGWEraseRect(gwinfo, a, b, c, d); break;
    case 'F': StGWFrameRect(gwinfo, a, b, c, d); break;
    case 'P': StGWPaintRect(gwinfo, a, b, c, d); break;
    }
    break;
  case 'O':
    switch (how) {
    case 'E':  StGWEraseOval(gwinfo, a, b, c, d); break;
    case 'F':  StGWFrameOval(gwinfo, a, b, c, d); break;
    case 'P':  StGWPaintOval(gwinfo, a, b, c, d); break;
    }
    break;
  case 'A':
    switch (how) {
    case 'E':  StGWEraseArc(gwinfo, a, b, c, d, angle1, angle2); break;
    case 'F':  StGWFrameArc(gwinfo, a, b, c, d, angle1, angle2); break;
    case 'P':  StGWPaintArc(gwinfo, a, b, c, d, angle1, angle2); break;
    }
    break;
  }
  return(NIL);
}

LVAL iview_window_draw_line()   { return(draw('L', 'F')); }
LVAL iview_window_draw_point()  { return(draw('P', 'F')); }
LVAL iview_window_erase_rect()  { return(draw('R', 'E')); }
LVAL iview_window_frame_rect()  { return(draw('R', 'F')); }
LVAL iview_window_paint_rect()  { return(draw('R', 'P')); } 
LVAL iview_window_erase_oval()  { return(draw('O', 'E')); }
LVAL iview_window_frame_oval()  { return(draw('O', 'F')); }
LVAL iview_window_paint_oval()  { return(draw('O', 'P')); }
LVAL iview_window_erase_arc()   { return(draw('A', 'E')); }
LVAL iview_window_frame_arc()   { return(draw('A', 'F')); }
LVAL iview_window_paint_arc()   { return(draw('A', 'P')); }

static short *make_poly(poly, size)
     LVAL poly;
     int *size;
{
  LVAL temp, pt;
  short *p;
  int n, i;
  
  for (temp = poly, n = 0; consp(temp); temp = cdr(temp)) {
    if (! consp(car(temp)) || ! fixp(car(car(temp))) 
        || !  fixp(car(cdr(car(temp)))))
      xlfail("bad polygon data");
    n++;
  }
  if (n > 0) {
    p = (short *) StCalloc(2 * n, sizeof(short));
    for (i = 0; i < n; i++, poly = cdr(poly)) {
      pt = car(poly);
      p[2 * i] = getfixnum(car(pt));
      p[2 * i + 1] = getfixnum(car(cdr(pt)));
    }
  }
  else p = nil;
  *size = n;
  return(p);
}

static free_poly(p)
     short *p;
{
  StFree(p);
}

static LVAL draw_poly(how)
     int how;
{
  LVAL object, poly;
  char *gwinfo;
  short *p;
  int n, from_origin;
  
  object = xlgaobject();
  poly = xlgalist();
  if (moreargs())
    from_origin = (xlgetarg() != NIL) ? TRUE : FALSE;
  else from_origin = TRUE;
  xllastarg();

  gwinfo = StGWObWinInfo(object);
  if (gwinfo == nil) return(NIL);
  p = make_poly(poly, &n);
  
  if (p != nil) {
    switch (how) {
    case 'E': StGWErasePoly(gwinfo, n, p, from_origin); break;
    case 'F': StGWFramePoly(gwinfo, n, p, from_origin); break;
    case 'P': StGWPaintPoly(gwinfo, n, p, from_origin); break;
    }
    free_poly(p);
  }
  return(NIL);
}

LVAL iview_window_erase_poly()   { return(draw_poly('E')); }
LVAL iview_window_frame_poly()   { return(draw_poly('F')); }
LVAL iview_window_paint_poly()   { return(draw_poly('P')); }

/**************************************************************************/
/**                                                                      **/
/**                            Text Functions                            **/
/**                                                                      **/
/**************************************************************************/

static LVAL text(what, up)
     int what, up;
{
  char *gwinfo;
  char *s;
  int value, x, y, h, v;

  gwinfo = StGWObWinInfo(xlgaobject());
  if (gwinfo == nil) return(NIL);
  
  if (what != 'A' && what != 'd') s = (char *) getstring(xlgastring());
  if (what != 'A' && what != 'W' && what != 'd') {
    x = getfixnum(xlgafixnum());
    y = getfixnum(xlgafixnum());
  }
  if (what == 'T') {
    h = getfixnum(xlgafixnum());
    v = getfixnum(xlgafixnum());
  }
  xllastarg();
  
  switch (what) {
  case 'A':  value = StGWTextAscent(gwinfo); break;
  case 'd':  value = StGWTextDescent(gwinfo); break;
  case 'W':  value = StGWTextWidth(gwinfo, s); break;
  case 'D':  if (up) StGWDrawStringUp(gwinfo, s, x, y);
             else StGWDrawString(gwinfo, s, x, y);
             break;
  case 'T':  if (up) StGWDrawTextUp(gwinfo, s, x, y, h, v);
             else StGWDrawText(gwinfo, s, x, y, h, v);
             break;
  }

  return((what == 'A' || what == 'W' || what == 'd') ? cvfixnum((FIXTYPE) value) : NIL);
}

LVAL iview_window_text_ascent()    { return(text('A', FALSE)); }
LVAL iview_window_text_descent()   { return(text('d', FALSE)); }
LVAL iview_window_text_width()     { return(text('W', FALSE)); }
LVAL iview_window_draw_string()    { return(text('D', FALSE)); }
LVAL iview_window_draw_string_up() { return(text('D', TRUE));  }
LVAL iview_window_draw_text()      { return(text('T', FALSE)); }
LVAL iview_window_draw_text_up()   { return(text('T', TRUE));  }

/**************************************************************************/
/**                                                                      **/
/**                           Symbol Functions                           **/
/**                                                                      **/
/**************************************************************************/

LVAL iview_window_draw_symbol()
{
  char *gwinfo;
  LVAL symbol;
  int sym, hsym, hilited, x, y;
  
  gwinfo = StGWObWinInfo(xlgaobject());
  if (gwinfo == nil) return(NIL);
  
  symbol = xlgasymbol();
  hilited = xlgetarg() != NIL;
  x = getfixnum(xlgafixnum());
  y = getfixnum(xlgafixnum());
  xllastarg();

  decode_point_symbol(symbol, &sym, &hsym);
  StGWDrawSymbol(gwinfo, (hilited) ? hsym : sym, x, y);
  return(NIL);
}

LVAL iview_window_replace_symbol()
{
  char *gwinfo;
  LVAL oldsymbol, newsymbol;
  int oldsym, oldhsym, newsym, newhsym, oldhilited, newhilited, x, y;
  
  gwinfo = StGWObWinInfo(xlgaobject());
  if (gwinfo == nil) return(NIL);
  
  oldsymbol = xlgasymbol();
  oldhilited = xlgetarg() != NIL;
  newsymbol = xlgasymbol();
  newhilited = xlgetarg() != NIL;
  x = getfixnum(xlgafixnum());
  y = getfixnum(xlgafixnum());
  xllastarg();

  decode_point_symbol(oldsymbol, &oldsym, &oldhsym);
  decode_point_symbol(newsymbol, &newsym, &newhsym);
  StGWReplaceSymbol(gwinfo, (oldhilited) ? oldhsym : oldsym, 
                            (newhilited) ? newhsym : newsym, x, y);
  return(NIL);
}

/**************************************************************************/
/**                                                                      **/
/**                         Buffering Functions                          **/
/**                                                                      **/
/**************************************************************************/

static LVAL buffer(what)
     int what;
{
  LVAL object;
  int left, top, width, height;
  char *gwinfo;
  
  object = xlgaobject();
  gwinfo = StGWObWinInfo(object);
  if (gwinfo == nil) return(NIL);
  
  if (what == 'B') {
    if (moreargs()) {
      left = getfixnum(xlgafixnum());
      top = getfixnum(xlgafixnum());
      width = getfixnum(xlgafixnum());
      height = getfixnum(xlgafixnum());
    }
    else StGWGetViewRect(gwinfo, &left, &top, &width, &height);
  }
  xllastarg();

  switch (what) {
  case 'S':  StGWStartBuffering(gwinfo); break;
  case 'B':  StGWBufferToScreen(gwinfo, left, top, width, height); break;
  }
  
  return(NIL);
}

LVAL iview_window_start_buffering()  { return(buffer('S')); }
LVAL iview_window_buffer_to_screen() { return(buffer('B')); }

/**************************************************************************/
/**                                                                      **/
/**                         Clipping Functions                           **/
/**                                                                      **/
/**************************************************************************/

LVAL iview_window_clip_rect()
{
  char *gwinfo;
  int clipping, left, top, width, height;
  
  gwinfo = StGWObWinInfo(xlgaobject());
  if (gwinfo == nil) return(NIL);
  
  if (moreargs()) {
    clipping = (peekarg(0) != NIL);
    if (clipping) {
      left = getfixnum(xlgafixnum());
      top = getfixnum(xlgafixnum());
      width = getfixnum(xlgafixnum());
      height = getfixnum(xlgafixnum());
    }
    StGWSetClipRect(gwinfo, clipping, left, top, width, height);
  }
  clipping = StGWGetClipRect(gwinfo, &left, &top, &width, &height);
  return((clipping) ? integer_list_4(left, top, width, height) : NIL);
}

/**************************************************************************/
/**                                                                      **/
/**                       Miscellaneous Functions                        **/
/**                                                                      **/
/**************************************************************************/

decode_cursor(arg)
     LVAL arg;
{
  LVAL val;
  
  val = xlgetprop(arg, s_cursor_index);
  if (fixp(val)) return(getfixnum(val));
  else return(ARROW_CURSOR);
}

LVAL encode_cursor(cursor)
     int cursor;
{
  LVAL sym;
  
  sym = (LVAL) StGWGetCursRefCon(cursor);
  if (sym == NIL) sym = s_arrow;
  if (! symbolp(sym)) xlfail("unknown cursor");
  return(sym);
}

LVAL iview_window_cursor()
{
  char *gwinfo;
  LVAL cursor;
  
  gwinfo = StGWObWinInfo(xlgaobject());
  if (gwinfo == nil) return(NIL);
  
  if (moreargs()) {
    cursor = xlgetarg();
    StGWSetCursor(gwinfo, decode_cursor(cursor));
  }
  return(encode_cursor(StGWCursor(gwinfo)));
}

LVAL iview_window_reset_buffer() { StGWResetBuffer(); return(NIL); }

LVAL iview_window_dump_image()
{
  char *gwinfo;
  LVAL fptr;
  double scale;
  
  gwinfo = StGWObWinInfo(xlgaobject());
#ifndef AMIGA  /* requires file name to open for low level write JKL */
  fptr = xlgetfile();
#else
  fptr = xlgetarg();
#endif
  scale = (moreargs()) ? makedouble(xlgetarg()) : 1.0;
#ifndef AMIGA
  /* make sure the file exists */
  if (getfile(fptr) == NULL) xlfail("file not open");
  
  if (gwinfo != nil) StGWDumpImage(gwinfo, getfile(fptr), scale);
#else
  if (gwinfo != nil) StGWDumpImage(gwinfo, getstring(fptr), scale);
#endif AMIGA
  return(NIL);
}  

LVAL gw_make_color()
{
  LVAL sym;
  double red, green, blue;
  int index;

  sym = xlgasymbol();
  if (! syminterned(sym)) xlerror("symbol not interned", sym);
  if (xlgetprop(sym, s_color_index) != NIL) {
    StGWFreeColor(decode_lisp_color(sym));
    xlputprop(sym, NIL, s_color_index);
  }
  red = makedouble(xlgetarg());
  green = makedouble(xlgetarg());
  blue = makedouble(xlgetarg());
  xllastarg();
  
  index = StGWMakeColor(red, green, blue, sym);
  if (index < 0) xlfail("can't allocate color");
  xlputprop(sym, cvfixnum((FIXTYPE) index), s_color_index);
  return(NIL);
}

LVAL gw_free_color()
{
  LVAL sym;
  
  sym = xlgasymbol();
  xllastarg();
  
  if (xlgetprop(sym, s_color_index) != NIL) {
    StGWFreeColor(decode_lisp_color(sym));
    xlputprop(sym, NIL, s_color_index);
  }
  return(NIL);
}

static char *make_image(Limage)
     LVAL Limage;
{
  int i, n;
  char *image;
  
  Limage = arraydata(Limage);
  n = getsize(Limage);
  
  for (i = 0; i < n; i++) if (! fixp(getelement(Limage, i))) return(nil);
  image = StCalloc(n, 1);
  for (i = 0; i < n; i++) 
    image[i] = (getfixnum(getelement(Limage, i)) != 0) ? 1 : 0;
  return(image);
}

static free_image(image)
     char *image;
{
  if (image != nil) StFree(image);
}

LVAL gw_make_cursor()
{
  LVAL sym, Limage, Lmask = NIL, curs;
  int index = -1, n, h = 0, v = 0, num;
  char *image, *mask = nil, *name;
  
  sym = xlgasymbol();
  if (! syminterned(sym)) xlerror("symbol not interned", sym);
  if (xlgetprop(sym, s_cursor_index) != NIL) {
    StGWFreeCursor(decode_cursor(sym));
    xlputprop(sym, NIL, s_cursor_index);
  }
  if (stringp(peekarg(0)) || fixp(peekarg(0))) {
    curs = xlgetarg();
    name = (stringp(curs)) ? (char *) getstring(curs) : nil;
    num = (stringp(curs)) ? -1 : getfixnum(curs);
    index = StGWMakeResCursor(name, num, sym);
  }
  else {
    Limage = xsgetmatrix();
    if (moreargs()) Lmask = xsgetmatrix();
    if (moreargs()) h = getfixnum(xlgafixnum());
    if (moreargs()) v = getfixnum(xlgafixnum());
  
    n = numrows(Limage);
    if (n != numcols(Limage)) xlerror("not a square matrix", Limage);
  
    image = make_image(Limage);
    if (Lmask != NIL && n == numrows(Lmask) && n == numcols(Lmask))
      mask = make_image(Lmask);
    if (image != nil) 
      index = StGWMakeCursor(n, image, mask, h, v, sym);
    if (image != nil) free_image(image);
    if (mask != nil) free_image(mask);
  }
  if (index < 0) xlfail("can't allocate cursor");
  xlputprop(sym, cvfixnum((FIXTYPE) index), s_cursor_index);
  return(NIL);
}

LVAL gw_free_cursor()
{
  LVAL sym;
  
  sym = xlgasymbol();
  xllastarg();
  
  if (xlgetprop(sym, s_cursor_index) != NIL) {
    StGWFreeCursor(decode_cursor(sym));
    xlputprop(sym, NIL, s_cursor_index);
  }
  return(NIL);
}

decode_point_symbol(lsym, psym, phsym)
     LVAL lsym;
     int *psym, *phsym;
{
  LVAL val;
  int sym, hsym;
  
  val = xlgetprop(lsym, s_symbol_index);
  if (! consp(val) || !fixp(car(val)) || ! consp(cdr(val)) || ! fixp(car(cdr(val)))) {
    sym = 4;
    hsym = 5;
  }
  else {
    sym = getfixnum(car(val));
    hsym = getfixnum(car(cdr(val)));
  }
  if (psym != nil) *psym = sym;
  if (phsym != nil) *phsym = hsym;
}

LVAL encode_point_symbol(sym, hsym)
     int sym, hsym;
{
  LVAL lsym;
  
  if (sym == 0 && hsym == 3) lsym = s_dotword;
  else lsym = (LVAL) StGWGetSymRefCon(sym);
  if (lsym != NIL && symbolp(lsym)) return(lsym);
  else return(integer_list_2(sym, hsym));
}

LVAL gw_draw_bitmap()
{
  char *gwinfo, *image;
  LVAL Limage;
  int left, top, width, height;
  
  gwinfo = StGWObWinInfo(xlgaobject());
  Limage = xsgetmatrix();
  left = getfixnum(xlgafixnum());
  top = getfixnum(xlgafixnum());
  /*  xllastarg();*/ /* allow for optional mask bitmap */
  
  width = numcols(Limage);
  height = numrows(Limage);
  
  if (width <= 0 || height <= 0) xlerror("bad bitmap data", Limage);
  
  image = make_image(Limage);
  if (image != nil) {
    StGWDrawBitmap(gwinfo, left, top, width, height, image);
    free_image(image);
  }
  return(NIL);
}
