/* 
 * xvbutt.c - regular, 'radio', 'checkbox', and 'menu' pushbuttons
 *
 * callable functions:
 *
 *   BTCreate()             -  create a button
 *   BTSetActive()          -  change 'active' status of button
 *   BTRedraw()             -  redraw button
 *   BTTrack()              -  clicked in button.  track until mouse up
 *
 *   RBCreate()             -  create an RBUTT and append to supplied list
 *   RBRedraw()             -  redraw one or all RBUTTs in a list
 *   RBSelect()             -  change selected item in list of RBUTTs
 *   RBWhich()              -  returns index of selected RBUTT in list
 *   RBCount()              -  returns # of RBUTTs in list
 *   RBSetActive()          -  sets active status of an RBUTT
 *   RBClick()              -  finds clicked-on rb in a list
 *   RBTrack()              -  tracks rb after click, until release
 * 
 *   CBCreate()             -  create a CBUTT (checkbox button)
 *   CBRedraw()             -  redraw a CBUTT
 *   CBSetActive()          -  change active status of a CBUTT
 *   CBClick()              -  returns true if given CB was clicked on
 *   CBTrack()              -  tracks CBUTT after click, until release
 *
 *   MBCreate()             -  create a MBUTT (menu button)
 *   MBRedraw()             -  redraw a MBUTT
 *   MBSetActive()          -  change active status of a MBUTT
 *   MBClick()              -  returns true if given MB was clicked on
 *   MBTrack()              -  tracks MBUTT after click, until release
 */


/* Copyright Notice
 * ================
 * Copyright 1989, 1990, 1991, 1992, 1993 by John Bradley
 * 
 * Permission to use, copy, and distribute XV in its entirety, for 
 * non-commercial purposes, is hereby granted without fee, provided that
 * this license information and copyright notice appear in all copies.
 * 
 * Note that distributing XV 'bundled' in with ANY product is considered
 * to be a 'commercial purpose'.
 *
 * Also note that any copies of XV that are distributed MUST be built
 * and/or configured to be in their 'unregistered copy' mode, so that it
 * is made obvious to the user that XV is shareware, and that they should
 * consider donating, or at least reading this License Info.
 * 
 * The software may be modified for your own purposes, but modified
 * versions may NOT be distributed without prior consent of the author.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.  In no event will the author be held liable for any damages
 * arising from the use of this software.
 * 
 * If you would like to do something with XV that this copyright
 * prohibits (such as distributing it with a commercial product, 
 * using portions of the source in some other program, etc.), please
 * contact the author (preferably via email).  Arrangements can
 * probably be worked out.
 *
 * XV is shareware for PERSONAL USE only.  You may use XV for your own
 * amusement, and if you find it nifty, useful, generally cool, or of
 * some value to you, your non-deductable donation would be greatly
 * appreciated.  $25 is the suggested donation, though, of course,
 * larger donations are quite welcome.  Folks who donate $25 or more
 * can receive a Real Nice bound copy of the XV manual for no extra
 * charge.
 * 
 * Commercial, government, and institutional users MUST register their
 * copies of XV, for the exceedingly REASONABLE price of just $25 per
 * workstation/X terminal.  Site licenses are available for those who
 * wish to run XV on a large number of machines.  Contact the author
 * for more details.
 *
 * The author may be contacted via:
 *    US Mail:  John Bradley
 *              1053 Floyd Terrace
 *              Bryn Mawr, PA  19010
 *
 *    Phone:    (215) 898-8813
 *    EMail:    bradley@cis.upenn.edu
 */


#include "xv.h"
#include "bitmaps.h"

static Pixmap cboard50 = (Pixmap) NULL;   /* 50% gray checkerboard */


static int    rbpixmade = 0;
static Pixmap rb_on, rb_on1, rb_off, rb_off1;

static int    cbpixmade = 0;
static Pixmap cbcheck;

static int    mbpixmade = 0;
static Pixmap mbchk;

#ifdef __STDC__
static void drawRB(RBUTT *, int);
static void drawCB(CBUTT *, int);
#else
static void drawRB(), drawCB();
#endif



/******************* BUTT ROUTINES ************************/



/**********************************************/
void BTCreate(bp,win,x,y,w,h,str,fg,bg,hi,lo)
BUTT         *bp;
Window        win;
int           x,y,w,h;
char         *str;
unsigned long fg,bg,hi,lo;
{
  bp->win = win;
  bp->x = x;  bp->y = y;  bp->w = w;  bp->h = h;
  bp->str = str;
  bp->fg = fg;  bp->bg = bg;  bp->hi = hi;  bp->lo = lo;
  bp->lit = 0;
  bp->active = 1;
  bp->toggle = 0;
  bp->pix = None;
  bp->style = 0;
  bp->fwidth = 3;

  if (!cboard50) {
    cboard50 = XCreatePixmapFromBitmapData(theDisp, rootW, cboard50_bits,
		       cboard50_width, cboard50_height, 1, 0, 1);
    if (!cboard50) FatalError("Unable to create cboard50 bitmap\n");
  }
}



/**********************************************/
void BTSetActive(bp,act)
BUTT         *bp;
int           act;
{
  if (bp->active != act) {
    bp->active = act;
    BTRedraw(bp);
  }
}



/**********************************************/
void BTRedraw(bp)
BUTT *bp;
{
  int i,x,y,w,h,r,x1,y1;
  XPoint tpts[10], bpts[10], ipts[5];

  x = bp->x;  y=bp->y;  w=bp->w;  h=bp->h;  r=bp->fwidth;

  if (!bp->active) bp->lit = 0;
  if (bp->lit) {
    r -= 1;
    if (r<0) r = 0;
  }

  if (!ctrlColor) {
    /* set up 'ipts' */
    ipts[0].x = x+r;        ipts[0].y = y+r;         /* topleft */
    ipts[1].x = x+r;        ipts[1].y = y+h-r;       /* botleft */
    ipts[2].x = x+w-r;      ipts[2].y = y+h-r;       /* botright */
    ipts[3].x = x+w-r;      ipts[3].y = y+r;         /* topright */
    ipts[4].x = ipts[0].x;  ipts[4].y = ipts[0].y;   /* close path */

    /* top left polygon */
    tpts[0].x = x;            tpts[0].y = y;
    tpts[1].x = x;            tpts[1].y = y+h;
    tpts[2].x = ipts[1].x;    tpts[2].y = ipts[1].y;
    tpts[3].x = ipts[0].x;    tpts[3].y = ipts[0].y;
    tpts[4].x = ipts[3].x;    tpts[4].y = ipts[3].y;
    tpts[5].x = x+w;          tpts[5].y = y;
    tpts[6].x = x;            tpts[6].y = y;

    /* bot left polygon */
    bpts[0].x = x;            bpts[0].y = y+h;
    bpts[1].x = ipts[1].x;    bpts[1].y = ipts[1].y;
    bpts[2].x = ipts[2].x;    bpts[2].y = ipts[2].y;
    bpts[3].x = ipts[3].x;    bpts[3].y = ipts[3].y;
    bpts[4].x = x+w;          bpts[4].y = y;
    bpts[5].x = x+w;          bpts[5].y = y+h;
    bpts[6].x = x;            bpts[6].y = y+h;


    /* clear button and draw frame */
    XSetForeground(theDisp, theGC, bp->bg);
    XFillRectangle(theDisp, bp->win, theGC, x, y, w, h);
    XSetForeground(theDisp, theGC, bp->fg);
    XDrawRectangle(theDisp, bp->win, theGC, x, y, w, h);

    XSetForeground(theDisp, theGC, bp->fg);
    XSetFillStyle(theDisp, theGC, FillStippled);
    XSetStipple(theDisp, theGC, cboard50);
    XFillPolygon(theDisp, bp->win, theGC, bpts, 7, Nonconvex, CoordModeOrigin);
    XSetFillStyle(theDisp,theGC,FillSolid);

    XSetForeground(theDisp, theGC, bp->fg);
    XDrawLines(theDisp, bp->win, theGC, ipts, 5, CoordModeOrigin);  /* inset */

    XDrawLine(theDisp, bp->win, theGC, x+1,   y+1,  ipts[0].x,ipts[0].y);
    XDrawLine(theDisp, bp->win, theGC, x+1,   y+h-1,ipts[1].x,ipts[1].y);
    XDrawLine(theDisp, bp->win, theGC, x+w-1, y+h-1,ipts[2].x,ipts[2].y);
    XDrawLine(theDisp, bp->win, theGC, x+w-1, y+1,  ipts[3].x,ipts[3].y);

    if (bp->lit) {
      XDrawRectangle(theDisp, bp->win, theGC, x+2, y+2, w-4, h-4);
      XDrawRectangle(theDisp, bp->win, theGC, x+1, y+1, w-2, h-2);
    }
  }
    
  else {   /* ctrlColor */
    XSetForeground(theDisp, theGC, bp->bg);
    XFillRectangle(theDisp, bp->win, theGC, x+1, y+1, w-1, h-1);

    Draw3dRect(bp->win, x+1,y+1,w-2,h-2, R3D_OUT, bp->fwidth, 
	       bp->hi, bp->lo, bp->bg);

    XSetForeground(theDisp, theGC, bp->fg);
    XDrawRectangle(theDisp, bp->win, theGC, x, y, w, h);

    if (bp->lit)
      XDrawRectangle(theDisp, bp->win, theGC, x+1, y+1, w-2, h-2);
  }
    



  XSetForeground(theDisp, theGC, bp->fg);

  if (bp->pix != None) {                    /* draw pixmap centered in butt */
    x1 = x+(1+w-bp->pw)/2;
    y1 = y+(1+h-bp->ph)/2;

    XSetBackground(theDisp, theGC, bp->bg);
    XCopyPlane(theDisp, bp->pix, bp->win, theGC, 0,0,bp->pw,bp->ph, x1,y1, 1);
    if (!bp->active) DimRect(bp->win, x1,y1, bp->pw, bp->ph, bp->bg);
  }

  else {                                    /* draw string centered in butt */
    x1 = CENTERX(mfinfo, x + w/2, bp->str);
    y1 = CENTERY(mfinfo, y + h/2);

    if (bp->active) {
      XDrawString(theDisp, bp->win, theGC, x1,y1, bp->str, strlen(bp->str));
    }
    else {  /* stipple if not active */
      XSetFillStyle(theDisp, theGC, FillStippled);
      XSetStipple(theDisp, theGC, dimStip);
      XDrawString(theDisp, bp->win, theGC, x1,y1, bp->str, strlen(bp->str));
      XSetFillStyle(theDisp,theGC,FillSolid);
    }
  }

}



/**********************************************/
int BTTrack(bp)
BUTT *bp;
{
  /* called when we've gotten a click inside 'bp'.  returns 1 if button
     was still selected lit when mouse was released. */

  Window       rW, cW;
  int          x, y, rx, ry, rval, inval;
  unsigned int mask;

  if (!bp->active) return 0;   /* inactive button */

  inval = bp->lit;
  bp->lit = !bp->lit;

  BTRedraw(bp);  XFlush(theDisp);
  Timer(120);  /* long enough for turn on to be visible */

  while (XQueryPointer(theDisp,bp->win,&rW,&cW,&rx,&ry,&x,&y,&mask)) {
    if (!(mask & Button1Mask)) break;    /* button released */

    if (bp->lit==inval && PTINRECT(x, y, bp->x, bp->y, bp->w, bp->h)) {
      bp->lit = !inval;  BTRedraw(bp);  XFlush(theDisp);
    }
    
    if (bp->lit!=inval && !PTINRECT(x, y, bp->x, bp->y, bp->w, bp->h)) {
      bp->lit = inval;  BTRedraw(bp);  XFlush(theDisp);
    }
  }

  rval = (bp->lit != inval);
  
  if (bp->lit && !bp->toggle) 
    { bp->lit = 0;  BTRedraw(bp);  XFlush(theDisp); }

  return(rval);
}




/******************* RBUTT ROUTINES ************************/


#define RBSIZE rb_frame_width


/***********************************************/
RBUTT *RBCreate(rblist, win, x,y,str, fg, bg, hi, lo)
      RBUTT        *rblist;
      Window        win;
      int           x,y;
      char         *str;
      unsigned long fg,bg,hi,lo;
{
  /* mallocs an RBUTT, fills in the fields, and appends it to rblist
     if rblist is NULL, this is the first rb in the list.  It will
     be made the 'selected' one 

     Note: no need to check return status.  It'll fatal error if it 
     can't malloc */

  RBUTT *rb, *rbptr;
  Pixmap rb_frame, rb_frame1, rb_top, rb_bot, rb_dtop, rb_dbot, rb_body, 
         rb_dot;

  rb = (RBUTT *) malloc(sizeof(RBUTT));
  if (!rb) FatalError("couldn't malloc RBUTT");

  /* fill in the fields of the structure */
  rb->win      = win;
  rb->x        = x;
  rb->y        = y;
  rb->str      = str;
  rb->selected = 0;
  rb->active   = 1;
  rb->next     = (RBUTT *) NULL;
  rb->fg       = fg;
  rb->bg       = bg;
  rb->hi       = hi;
  rb->lo       = lo;

  if (rblist) {            /* append to end of list */
    rbptr = rblist;
    while (rbptr->next) rbptr = rbptr->next;
    rbptr->next = rb;
  }
  else {                   /* this is the first one in the list.  select it */
    rb->selected = 1;
  }


  /* and, on an unrelated note, if the RB pixmaps haven't been created yet,
     do so.  We'll be needing them, y'see... */

  if (!rbpixmade) {
    rb_frame  = XCreatePixmapFromBitmapData(theDisp, rootW, 
                  rb_frame_bits, RBSIZE, RBSIZE, 1,0,1);
    rb_frame1 = XCreatePixmapFromBitmapData(theDisp, rootW, 
                  rb_frame1_bits, RBSIZE, RBSIZE, 1,0,1);
    rb_top    = XCreatePixmapFromBitmapData(theDisp, rootW, 
                  rb_top_bits, RBSIZE, RBSIZE, 1,0,1);
    rb_bot    = XCreatePixmapFromBitmapData(theDisp, rootW, 
                  rb_bot_bits, RBSIZE, RBSIZE, 1,0,1);
    rb_dtop   = XCreatePixmapFromBitmapData(theDisp, rootW, 
                  rb_dtop_bits, RBSIZE, RBSIZE, 1,0,1);
    rb_dbot   = XCreatePixmapFromBitmapData(theDisp, rootW, 
                  rb_dbot_bits, RBSIZE, RBSIZE, 1,0,1);
    rb_body   = XCreatePixmapFromBitmapData(theDisp, rootW, 
                  rb_body_bits, RBSIZE, RBSIZE, 1,0,1);
    rb_dot    = XCreatePixmapFromBitmapData(theDisp, rootW, 
                  rb_dot_bits, RBSIZE, RBSIZE, 1,0,1);

    rb_on     = XCreatePixmap(theDisp, rootW, RBSIZE, RBSIZE, dispDEEP);
    rb_on1    = XCreatePixmap(theDisp, rootW, RBSIZE, RBSIZE, dispDEEP);
    rb_off    = XCreatePixmap(theDisp, rootW, RBSIZE, RBSIZE, dispDEEP);
    rb_off1   = XCreatePixmap(theDisp, rootW, RBSIZE, RBSIZE, dispDEEP);

    if (!rb_frame || !rb_frame1 || !rb_top || !rb_bot || !rb_dtop || 
	!rb_dbot  || !rb_body   || !rb_dot || !rb_on  || !rb_on1  ||
	!rb_off   || !rb_off1)
      FatalError("unable to create radio-button pixmaps");


    /* generate rb_on,on1,off,off1 pixmaps from mask pixmaps */
    XSetForeground(theDisp, theGC, bg);
    XFillRectangle(theDisp, rb_on,   theGC, 0,0,RBSIZE,RBSIZE);
    XFillRectangle(theDisp, rb_on1,  theGC, 0,0,RBSIZE,RBSIZE);
    XFillRectangle(theDisp, rb_off,  theGC, 0,0,RBSIZE,RBSIZE);
    XFillRectangle(theDisp, rb_off1, theGC, 0,0,RBSIZE,RBSIZE);

    XSetFillStyle(theDisp, theGC, FillStippled);

    if (ctrlColor) {
      XSetStipple(theDisp, theGC, rb_top);
      XSetForeground(theDisp, theGC, fg);
      XFillRectangle(theDisp, rb_on,   theGC, 0,0,RBSIZE,RBSIZE);
      XFillRectangle(theDisp, rb_on1,  theGC, 0,0,RBSIZE,RBSIZE);
      XSetForeground(theDisp, theGC, hi);
      XFillRectangle(theDisp, rb_off,  theGC, 0,0,RBSIZE,RBSIZE);
      XFillRectangle(theDisp, rb_off1, theGC, 0,0,RBSIZE,RBSIZE);

      XSetStipple(theDisp, theGC, rb_body);
      XSetForeground(theDisp, theGC, lo);
      XFillRectangle(theDisp, rb_on,   theGC, 0,0,RBSIZE,RBSIZE);
      XFillRectangle(theDisp, rb_on1,  theGC, 0,0,RBSIZE,RBSIZE);
      XSetForeground(theDisp, theGC, bg);
      XFillRectangle(theDisp, rb_off,  theGC, 0,0,RBSIZE,RBSIZE);
      XFillRectangle(theDisp, rb_off1, theGC, 0,0,RBSIZE,RBSIZE);

      XSetStipple(theDisp, theGC, rb_bot);
      XSetForeground(theDisp, theGC, bg);
      XFillRectangle(theDisp, rb_on,   theGC, 0,0,RBSIZE,RBSIZE);
      XFillRectangle(theDisp, rb_on1,  theGC, 0,0,RBSIZE,RBSIZE);
      XSetForeground(theDisp, theGC, lo);
      XFillRectangle(theDisp, rb_off,  theGC, 0,0,RBSIZE,RBSIZE);
      XFillRectangle(theDisp, rb_off1, theGC, 0,0,RBSIZE,RBSIZE);

      XSetStipple(theDisp, theGC, rb_dtop);
      XSetForeground(theDisp, theGC, bg);
      XFillRectangle(theDisp, rb_on,   theGC, 0,0,RBSIZE,RBSIZE);
      XFillRectangle(theDisp, rb_on1,  theGC, 0,0,RBSIZE,RBSIZE);
      XSetForeground(theDisp, theGC, hi);
      XFillRectangle(theDisp, rb_off,  theGC, 0,0,RBSIZE,RBSIZE);
      XFillRectangle(theDisp, rb_off1, theGC, 0,0,RBSIZE,RBSIZE);

      XSetStipple(theDisp, theGC, rb_dbot);
      XSetForeground(theDisp, theGC, fg);
      XFillRectangle(theDisp, rb_on,   theGC, 0,0,RBSIZE,RBSIZE);
      XFillRectangle(theDisp, rb_on1,  theGC, 0,0,RBSIZE,RBSIZE);
      XSetForeground(theDisp, theGC, lo);
      XFillRectangle(theDisp, rb_off,  theGC, 0,0,RBSIZE,RBSIZE);
      XFillRectangle(theDisp, rb_off1, theGC, 0,0,RBSIZE,RBSIZE);
    }
    else {
      XSetStipple(theDisp, theGC, rb_dot);
      XSetForeground(theDisp, theGC, fg);
      XFillRectangle(theDisp, rb_on,   theGC, 0,0,RBSIZE,RBSIZE);
      XFillRectangle(theDisp, rb_on1,  theGC, 0,0,RBSIZE,RBSIZE);
    }
      
    XSetStipple(theDisp, theGC, rb_frame);
    XSetForeground(theDisp, theGC, fg);
    XFillRectangle(theDisp, rb_on,   theGC, 0,0,RBSIZE,RBSIZE);
    XFillRectangle(theDisp, rb_off,  theGC, 0,0,RBSIZE,RBSIZE);

    XSetStipple(theDisp, theGC, rb_frame1);
    XSetForeground(theDisp, theGC, fg);
    XFillRectangle(theDisp, rb_on1,  theGC, 0,0,RBSIZE,RBSIZE);
    XFillRectangle(theDisp, rb_off1, theGC, 0,0,RBSIZE,RBSIZE);

    XSetFillStyle(theDisp, theGC, FillSolid);

    /* destroy mask pixmaps */
    XFreePixmap(theDisp, rb_frame);
    XFreePixmap(theDisp, rb_frame1);
    XFreePixmap(theDisp, rb_top);
    XFreePixmap(theDisp, rb_bot);
    XFreePixmap(theDisp, rb_dtop);
    XFreePixmap(theDisp, rb_dbot);
    XFreePixmap(theDisp, rb_body);

    rbpixmade = 1;
  }

  return(rb);
}
  



/***********************************************/
void RBRedraw(rblist, num)
RBUTT *rblist;
int    num;
{
  /* redraws the 'num-th' RB in the list.  if num < 0, redraws entire list */

  RBUTT *rb;
  int    i;

  /* point 'rb' at the appropriate RBUTT, *if* we're not drawing entire list */
  if (num>=0) {
    i=0;  rb=rblist;
    while (i!=num && rb) { rb = rb->next;  i++; }
    if (!rb) return;                     /* num is out of range.  do nothing */
    drawRB(rb,0);
  }

  else {                                 /* draw entire list */
    rb = rblist;
    while (rb) {
      drawRB(rb,0);
      rb = rb->next;
    }
  }
}


/***********************************************/
static void drawRB(rb, lit)
RBUTT *rb;
int   lit;
{
  /* draws the rb being pointed at */

  Pixmap pix;

  if (!rb) return;  /* rb = NULL */

  XSetForeground(theDisp, theGC, rb->fg);

  if (rb->selected) { pix = (lit) ? rb_on1 : rb_on; }
               else { pix = (lit) ? rb_off1 : rb_off; }

  XCopyArea(theDisp, pix, rb->win, theGC, 0,0,RBSIZE,RBSIZE, rb->x, rb->y);
  XDrawString(theDisp, rb->win, theGC, rb->x + RBSIZE + 4, 
	      rb->y + RBSIZE/2 - CHIGH/2 + ASCENT,rb->str,strlen(rb->str));

  if (!rb->active) {  /* if non-active, dim button and string */
    DimRect(rb->win, rb->x, rb->y, RBSIZE, RBSIZE, rb->bg);
    DimRect(rb->win, rb->x + RBSIZE + 4, rb->y + RBSIZE/2 - CHIGH/2, 
	    StringWidth(rb->str),CHIGH, rb->bg);
  }
}


/***********************************************/
void RBSelect(rblist, n)
RBUTT *rblist;
int    n;
{
  RBUTT *rbold, *rb;
  int    i;

  /* makes rb #n the selected rb in the list.  Does all redrawing.  Does
     nothing if rb already selected */

  /* get pointers to the currently selected rb and the desired rb */
  rbold = rblist;
  while (rbold && !rbold->selected) rbold = rbold->next;
  if (!rbold) return;    /* no currently selected item.  shouldn't happen */

  rb = rblist;  i=0;
  while (rb && i!=n) {rb = rb->next;  i++; }
  if (!rb) return;    /* 'n' is out of range */


  if (rb == rbold) return;   /* 'n' is already selected.  do nothing */

  rbold->selected = 0;
  rb->selected    = 1;
  drawRB(rbold, 0);
  drawRB(rb, 0);
}


	      
/***********************************************/
int RBWhich(rblist)
RBUTT *rblist;
{
  int i;

  /* returns index of currently selected rb.  if none, returns -1 */

  i = 0;
  while (rblist && !rblist->selected) { rblist = rblist->next;  i++; }

  if (!rblist) return -1;             /* didn't find one */
  return i;
}


/***********************************************/
int RBCount(rblist)
RBUTT *rblist;
{
  int i;

  /* returns # of rb's in the list */

  i = 0;
  while (rblist) { rblist = rblist->next; i++; }
  return i;
}


/***********************************************/
void RBSetActive(rblist, n, act)
RBUTT *rblist;
int n,act;
{
  RBUTT *rb;
  int    i;

  /* sets 'active' status of rb #n.  does redrawing */

  rb=rblist;  i=0;
  while (rb && i!=n) { rb = rb->next; i++; }
  if (!rb) return;                         /* n out of range.  do nothing */

  if (rb->active != act) {
    rb->active = act;
    drawRB(rb, 0);
  }
}


/***********************************************/
int RBClick(rblist, mx, my)
RBUTT *rblist;
int    mx,my;
{
  int i;

  /* searches through rblist to see if mouse click at mx,my is in the
     clickable region of any of the rb's.  If it finds one, it returns 
     it's index in the list.  If not, returns -1 */

  i = 0;
  while (rblist) {
    if (PTINRECT(mx, my, rblist->x, rblist->y, RBSIZE, RBSIZE)) 
      break;
    rblist = rblist->next;
    i++;
  }

  if (!rblist) return -1;
  return(i);
}


/***********************************************/
int RBTrack(rblist, n)
RBUTT *rblist;
int    n;
{
  RBUTT       *rb;
  Window       rW, cW;
  int          i, x, y, rx, ry, lit, rv;
  unsigned int mask;

  /* returns '1' if selection changed */

  rb=rblist;  i=0;
  while (rb && i!=n) { rb = rb->next; i++; }
  if (!rb) return 0;                    /* n out of range */

  /* called once we've figured out that the mouse clicked in 'rb' */

  if (!rb->active) return 0;

  lit = 1;
  drawRB(rb, lit);
  XFlush(theDisp);
  Timer(75);          /* give chance for 'turn on' to become visible */

  while (XQueryPointer(theDisp,rb->win,&rW,&cW,&rx,&ry,&x,&y,&mask)) {
    if (!(mask & Button1Mask)) break;    /* button released */

    if (!lit && PTINRECT(x, y, rb->x, rb->y, RBSIZE, RBSIZE)) {
      lit=1;
      drawRB(rb, lit);
      XFlush(theDisp);
    }
    
    if (lit && !PTINRECT(x, y, rb->x, rb->y, RBSIZE, RBSIZE)) {
      lit=0;
      drawRB(rb, lit);
      XFlush(theDisp);
    }
  }

  rv = 0;

  if (lit) {
    drawRB(rb, 0);
    if (RBWhich(rblist) != n) rv = 1;
    RBSelect(rblist, n);
  }

  XFlush(theDisp);
  return rv;
}




/******************* CBUTT ROUTINES ************************/



#define CBSIZE 16

/***********************************************/
void CBCreate(cb, win, x,y, str, fg, bg, hi, lo)
      CBUTT        *cb;
      Window        win;
      int           x,y;
      char         *str;
      unsigned long fg,bg,hi,lo;
{
  /* fill in the fields of the structure */
  cb->win      = win;
  cb->x        = x;
  cb->y        = y;
  cb->str      = str;
  cb->val      = 0;
  cb->active   = 1;
  cb->fg       = fg;
  cb->bg       = bg;
  cb->hi       = hi;
  cb->lo       = lo;

  /* and, on an unrelated note, if the CB pixmaps haven't been created yet,
     do so.  We'll be needing them, y'see... */

  if (!cbpixmade) {
    cbcheck = XCreatePixmapFromBitmapData(theDisp, rootW, cb_check_bits,
	     cb_check_width, cb_check_height, fg, bg, dispDEEP);

    cbpixmade = 1;
  }
}
  



/***********************************************/
void CBRedraw(cb)
CBUTT *cb;
{
  /* draws the cb being pointed at */

  XSetForeground(theDisp, theGC, cb->bg);
  XFillRectangle(theDisp, cb->win, theGC, cb->x+2, cb->y+2, CBSIZE-3,CBSIZE-3);

  XSetForeground(theDisp, theGC, cb->fg);
  XDrawRectangle(theDisp, cb->win, theGC, cb->x, cb->y, CBSIZE, CBSIZE);
  Draw3dRect(cb->win, cb->x+1, cb->y+1, CBSIZE-2, CBSIZE-2, R3D_OUT, 2,
	     cb->hi, cb->lo, cb->bg); 

  if (cb->val) XCopyArea(theDisp, cbcheck, cb->win, theGC, 
			 0, 0, cb_check_width, cb_check_height, 
			 cb->x+3, cb->y+3);
 
  XSetForeground(theDisp, theGC, cb->fg);
  XDrawString(theDisp, cb->win, theGC, cb->x + CBSIZE+4, 
	      cb->y+CBSIZE/2 - CHIGH/2 + ASCENT,cb->str,strlen(cb->str));

  if (!cb->active) {  /* if non-active, dim button and string */
    DimRect(cb->win, cb->x, cb->y, CBSIZE, CBSIZE, cb->bg);
    DimRect(cb->win, cb->x + CBSIZE+4, cb->y+CBSIZE/2 - CHIGH/2, 
	    StringWidth(cb->str),CHIGH, cb->bg);
  }
}


/**********************************************/
void CBSetActive(cb,act)
CBUTT        *cb;
int           act;
{
  if (cb->active != act) {
    cb->active = act;
    CBRedraw(cb);
  }
}


/***********************************************/
int CBClick(cb, mx, my)
CBUTT *cb;
int    mx,my;
{
  if (PTINRECT(mx, my, cb->x, cb->y, CBSIZE,CBSIZE)) return 1;
  return 0;
}


/***********************************************/
int CBTrack(cb)
CBUTT *cb;
{
  Window       rW, cW;
  int          x, y, rx, ry, lit;
  unsigned int mask;
  Pixmap litpix, darkpix;

  /* called once we've figured out that the mouse clicked in 'cb' */

  if (!cb->active) return 0;

  XSetForeground(theDisp, theGC, cb->fg);

  lit = 1;
  drawCB(cb, lit);
  XFlush(theDisp);
  Timer(75);          /* give chance for 'turn on' to become visible */

  while (XQueryPointer(theDisp,cb->win,&rW,&cW,&rx,&ry,&x,&y,&mask)) {
    if (!(mask & Button1Mask)) break;    /* button released */

    if (!lit && PTINRECT(x, y, cb->x, cb->y, CBSIZE, CBSIZE)) {
      lit=1;
      drawCB(cb,lit);
      XFlush(theDisp);
    }
    
    if (lit && !PTINRECT(x, y, cb->x, cb->y, CBSIZE, CBSIZE)) {
      lit=0;
      drawCB(cb,lit);
      XFlush(theDisp);
    }
  }

  if (lit) {
    cb->val = !cb->val;
    drawCB(cb,0);
    CBRedraw(cb);
  }

  XFlush(theDisp);

  return(lit);
}


/***********************************************/
static void drawCB(cb, lit)
CBUTT *cb;
int lit;
{
  /* draws highlighting */
  if (lit) {
    if (ctrlColor) 
      Draw3dRect(cb->win, cb->x+1, cb->y+1, CBSIZE-2, CBSIZE-2, R3D_IN, 2,
		 cb->hi, cb->lo, cb->bg);
    else {
      XSetForeground(theDisp, theGC, cb->fg);
      XDrawRectangle(theDisp, cb->win, theGC, cb->x+1, cb->y+1, 
		     CBSIZE-2, CBSIZE-2);
    }
  }

  else {
    if (ctrlColor) 
      Draw3dRect(cb->win, cb->x+1, cb->y+1, CBSIZE-2, CBSIZE-2, R3D_OUT, 2,
		 cb->hi, cb->lo, cb->bg);
    else {
      XSetForeground(theDisp, theGC, cb->bg);
      XDrawRectangle(theDisp, cb->win, theGC, cb->x+1, cb->y+1, 
		     CBSIZE-2, CBSIZE-2);
    }
  }
}
    


/******************* MBUTT ROUTINES ************************/




/***********************************************/
void MBCreate(mb, win, x,y,w,h, str, list, nlist, fg, bg, hi, lo)
      MBUTT        *mb;
      Window        win;
      int           x,y,w,h;
      char         *str;
      char        **list;
      int           nlist;
      unsigned long fg,bg,hi,lo;
{
  XSetWindowAttributes xswa;
  unsigned long        xswamask;
  int i;

  if (!mbpixmade) {
    mbchk = XCreatePixmapFromBitmapData(theDisp, rootW, mb_chk_bits,
	     mb_chk_width, mb_chk_height, fg, bg, dispDEEP);
    mbpixmade = 1;
  }


  /* fill in the fields of the structure */
  mb->win      = win;
  mb->x        = x;
  mb->y        = y;
  mb->w        = w;
  mb->h        = h;
  mb->title    = str;
  mb->active   = 1;
  mb->list     = list;
  mb->nlist    = nlist;
  mb->fg       = fg;
  mb->bg       = bg;
  mb->hi       = hi;
  mb->lo       = lo;

  mb->pix      = (Pixmap) NULL;
  mb->pw = mb->ph = 0;

  for (i=0; i<MAXMBLEN; i++) {
    mb->flags[i] = 0;
    mb->dim[i] = 0;
  }

  /* create popup window (it gets mapped, pos'd and sized later) */
  xswa.background_pixel = bg;
  xswa.border_pixel     = fg;
  xswa.save_under       = True;
  xswamask = CWBackPixel | CWBorderPixel | CWSaveUnder;

  mb->mwin = XCreateWindow(theDisp, mb->win, x, y, w, h, 
			bwidth, dispDEEP, InputOutput,
			theVisual, xswamask, &xswa);

  if (!mb->mwin) FatalError("can't create popup menu window!");

  XSelectInput(theDisp, mb->mwin, ExposureMask | VisibilityChangeMask);
  XSetTransientForHint(theDisp, mb->mwin, mb->win);
}
  



/***********************************************/
void MBRedraw(mb)
MBUTT *mb;
{
  /* draws a menu button in it's normal state.  (When it's actively being
     used (to select an item), all drawing is handled in MBTrack) */

  int x,y,w,h,i,r,x1,y1;

  r = 2;  /* amt of shadow */
  x = mb->x;  y = mb->y;  w = mb->w;  h = mb->h;

  XSetForeground(theDisp, theGC, mb->bg);
  XFillRectangle(theDisp, mb->win, theGC, x+1, y+1, w-1, h-1);

  XSetForeground(theDisp, theGC, mb->fg);
  XDrawRectangle(theDisp, mb->win, theGC, x, y, w, h);
  Draw3dRect(mb->win, x+1, y+1, w-2, h-2, R3D_OUT, 2, mb->hi, 
	     mb->lo, mb->bg);

  XSetForeground(theDisp, theGC, mb->fg);

  /* draw shadow */
  for (i=1; i<=r; i++) {
    XDrawLine(theDisp, mb->win, theGC, x+r, y+h+i, x+w+i, y+h+i);
    XDrawLine(theDisp, mb->win, theGC, x+w+i, y+h+i, x+w+i, y+r);
  }

  if (mb->pix != None) {                    /* draw pixmap centered in butt */
    x1 = x + (1+w - mb->pw)/2;
    y1 = y + (1+h - mb->ph)/2;

    XSetForeground(theDisp, theGC, mb->fg);
    XSetBackground(theDisp, theGC, mb->bg);
    XCopyPlane(theDisp, mb->pix, mb->win, theGC, 0,0,mb->pw,mb->ph, x1,y1, 1);
    if (!mb->active) DimRect(mb->win, x1,y1, mb->pw, mb->ph, mb->bg);
  }

  else {                                    /* draw string centered in butt */
    char *str;

    if (mb->title) str = mb->title;
    else {  /* find first checked item, and show that as the title */
      int i;
      for (i=0; i<mb->nlist && !mb->flags[i]; i++);
      if (i==mb->nlist) i = 0;   /* shouldn't happen */

      str = mb->list[i];
    }

    x1 = CENTERX(mfinfo, x + w/2, str);
    y1 = CENTERY(mfinfo, y + h/2);

    if (mb->active) {
      XDrawString(theDisp, mb->win, theGC, x1,y1, str, strlen(str));
    }
    else {  /* stipple if not active */
      XSetFillStyle(theDisp, theGC, FillStippled);
      XSetStipple(theDisp, theGC, dimStip);
      XDrawString(theDisp, mb->win, theGC, x1,y1, str, strlen(str));
      XSetFillStyle(theDisp,theGC,FillSolid);
    }
  }
}


/**********************************************/
void MBSetActive(mb,act)
MBUTT        *mb;
int           act;
{
  if (mb->active != act) {
    mb->active = act;
    MBRedraw(mb);
  }
}


/***********************************************/
int MBClick(mb, mx, my)
MBUTT *mb;
int    mx,my;
{
  if (PTINRECT(mx, my, mb->x, mb->y, mb->w, mb->h)) return 1;
  return 0;
}


/***********************************************/
int MBTrack(mb)
MBUTT *mb;
{
  Window       rW, cW, win;
  int          i, x, y, rx, ry, extratop, hascheck;
  unsigned int mask;
  int          mwide, mhigh, mx, my, j, lit, lastlit;
  XSizeHints   hints;
  XEvent       event;


  /* returns selected menu choice index, or '-1' if none */

  if (!mb->active || !mb->nlist) return 0;

  /* invert the button, for visual feedback */
  XSetFunction(theDisp, theGC, GXinvert);
  XSetPlaneMask(theDisp, theGC, mb->fg ^ mb->bg);
  XFillRectangle(theDisp, mb->win, theGC, mb->x+1, mb->y+1, mb->w-1, mb->h-1);
  XSetFunction(theDisp, theGC, GXcopy);
  XSetPlaneMask(theDisp, theGC, ~0L);

  extratop = (mb->title) ? LINEHIGH+3 : 1-SPACING; /*add extra line for title*/


  mwide = 1;                              /* compute maximum width */
  for (i=0; i<mb->nlist; i++) {
    j = StringWidth(mb->list[i]);
    if (j > mwide) mwide = j;
  }
  mwide += 8;                             /* extra room at edges */

  /* make wider if any checked menu items */
  for (i=0; i<mb->nlist && !mb->flags[i]; i++);
  hascheck = (i<mb->nlist);

  if (hascheck && mb->title) mwide += 8;

  if (mwide < (mb->w+1)) mwide = mb->w+1; /* at least as wide as button */
    
  mhigh = mb->nlist * LINEHIGH + 2 + extratop;

  mx = mb->x-1;  my = mb->y - 1;
  if (mb->title && mwide > mb->w) mx -= ((mwide - mb->w)/2);


  /* create/map window, and warp mouse if we had to move the window */
  win = mb->mwin;
  XMoveResizeWindow(theDisp, win, mx, my, mwide, mhigh);

  hints.width  = hints.min_width  = hints.max_width  = mwide;
  hints.height = hints.min_height = hints.max_height = mhigh;
  hints.x = mx;  hints.y = my;
  hints.flags  = (USSize | PMinSize | PMaxSize | PPosition);
  XSetNormalHints(theDisp, win, &hints);

  XMapRaised(theDisp, win);

  /* wait for window to become mapped */
  XWindowEvent(theDisp, win, VisibilityChangeMask, &event);


  /* draw the menu */
  XSetForeground(theDisp, theGC, mb->fg);
  x = (hascheck && mb->title) ? 12 : 4;
  if (mb->title) {                /* draw a title on this menu */
    CenterString(win, mb->title, mwide/2-1, (extratop-2)/2);

    if (ctrlColor) {
      XSetForeground(theDisp, theGC, mb->fg);
      XDrawLine(theDisp, win, theGC, 0, extratop-2, mwide, extratop-2);
      XSetForeground(theDisp, theGC, mb->lo);
      XDrawLine(theDisp, win, theGC, 0, extratop-1, mwide, extratop-1);
      XSetForeground(theDisp, theGC, mb->hi);
      XDrawLine(theDisp, win, theGC, 0, extratop,   mwide, extratop);
      XSetForeground(theDisp, theGC, mb->fg);
    }
    else {  /* b/w system */
      XDrawLine(theDisp, win, theGC, 0, extratop-2, mwide, extratop-2);
      XDrawLine(theDisp, win, theGC, 0, extratop,   mwide, extratop);
    }
  }

  y = ASCENT + SPACING + extratop;
  for (i=0; i<mb->nlist; i++) {
    if (mb->flags[i]) {
      XCopyArea(theDisp, mbchk, win, theGC, 0, 0, mb_chk_width, mb_chk_height, 
		x - 10, y - 8);
    }
    
    if (!strcmp(mb->list[i], MBSEP)) {
      mb->dim[i] = 1;    /* don't select this one */
      if (ctrlColor) {
	XSetForeground(theDisp, theGC, mb->fg);
	XDrawLine(theDisp,win,theGC,4,y-(ASCENT/2)-1, mwide-5, y-(ASCENT/2)-1);

	XSetForeground(theDisp, theGC, mb->lo);
	XDrawLine(theDisp,win,theGC,4,y-(ASCENT/2),   mwide-5, y-(ASCENT/2));

	XSetForeground(theDisp, theGC, mb->hi);
	XDrawLine(theDisp,win,theGC,4,y-(ASCENT/2)+1, mwide-5, y-(ASCENT/2)+1);
	XSetForeground(theDisp, theGC, mb->fg);
      }
      else 
	XDrawLine(theDisp, win, theGC, 4, y-(ASCENT/2), mwide-5, y-(ASCENT/2));
    }
    else {
      XDrawString(theDisp, win, theGC, x, y, mb->list[i], strlen(mb->list[i]));
      if (mb->dim[i]) DimRect(win, x, y-ASCENT, mwide, CHIGH, mb->bg);
      XSetForeground(theDisp, theGC, mb->fg);
    }

    y += LINEHIGH;
  }

  XFlush(theDisp);


  /* track the mouse */
  XSetFunction(theDisp, theGC, GXinvert);         /* go in to 'invert' mode */
  XSetPlaneMask(theDisp, theGC, mb->fg ^ mb->bg);

  lit = lastlit = -1;
  while (XQueryPointer(theDisp,win,&rW,&cW,&rx,&ry,&x,&y,&mask)) {
    if (!(mask & Button1Mask)) break;    /* button released */

    /* determine which choice the mouse is in.  -1 if none */
    j = extratop+2;
    if (x < 0 || x > mwide) lit = -1;
    else {
      for (i=0; i<mb->nlist; i++, j+=LINEHIGH) {
	if (y>=j && y < j+LINEHIGH) { lit = i; break; }
      }
      if (i == mb->nlist) lit = -1;
    }

    /* handle dimmed selections */
    if (lit >= 0 && mb->dim[lit]) lit = -1;

    if (lit != lastlit) {
      if (lit >= 0) {
	y = extratop + 2 + lit*LINEHIGH;
	XFillRectangle(theDisp, win, theGC, 0, y, mwide, LINEHIGH);
      }
      if (lastlit >= 0) {
	y = extratop + 2 + lastlit*LINEHIGH;
	XFillRectangle(theDisp, win, theGC, 0, y, mwide, LINEHIGH);
      }
      lastlit = lit;
    }
  }

  /* flash the selected choice, if any */
  if (lit >= 0) {
    y = extratop + 2 + lit*LINEHIGH;
    for (i=0; i<5; i++) {
      XFillRectangle(theDisp, win, theGC, 0, y, mwide, LINEHIGH);
      XFlush(theDisp);
      Timer(50);
    }
  }

  XSetFunction(theDisp, theGC, GXcopy);   /* back to 'normal' mode */
  XSetPlaneMask(theDisp, theGC, ~0L);

  /* could try eating all remaining events for 'win' before unmapping */

  XSync(theDisp, False);                  /* make sure 'map' has taken place */
  XUnmapWindow(theDisp, win);

  MBRedraw(mb);

  return lit;
}




