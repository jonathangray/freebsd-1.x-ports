/* 
 * xvscrl.c - Scroll Bar handling functions
 *
 * callable functions:
 *
 *   SCCreate()   -  creates the Scroll Bar window.
 *   SCChange()   -  resize/repositions the Scroll Bar window.
 *   SCSetRange() -  sets min/max/current values of control
 *   SCSetVal()   -  sets value of control 
 *   SCRedraw()   -  redraws scroll bar
 *   SCTrack()    -  called when clicked.  Operates control 'til mouseup
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


static Pixmap upPix,  downPix;   /* up/down arrows */
static Pixmap up1Pix, down1Pix;  /* up/down arrows (lit up) */
static Pixmap uphPix, downhPix;  /* up/down arrows, for horizontal scrl */
static Pixmap uph1Pix,downh1Pix; /* up/down arrows, horizontal (lit up) */
static Pixmap sgray;             /* gray stipple for lit scrollbar */
static int    pixmaps_built=0;   /* true if pixmaps created already */


/* scroll regions */
#define UPLINE 0
#define UPPAGE 1
#define DNLINE 2
#define DNPAGE 3
#define THUMB  4

#define SCRLWAIT 150   /* milliseconds to wait between scrolls */

/* local functions */
#ifdef __STDC__
static int  whereInScrl(SCRL *, int, int);
static void drawArrow(SCRL *, int);
static void drawThumb(SCRL *);
#else
static int  whereInScrl();
static void drawArrow();
static void drawThumb();
#endif


/***************************************************/
void SCCreate(sp, parent, x, y, vert, len, minv, maxv, curv, page, 
	          fg, bg, hi, lo, func)
SCRL         *sp;
Window        parent;
int           x,y,vert,len,minv,maxv,curv,page;
unsigned long fg,bg,hi,lo;

#ifdef __STDC__
  void          (*func)(int, SCRL *);
#else
  void          (*func)();
#endif
{


  if (!pixmaps_built) {
    upPix    = XCreatePixmapFromBitmapData(theDisp, parent, 
		up_bits, up_width, up_height, 1, 0, 1);
    downPix  = XCreatePixmapFromBitmapData(theDisp, parent, 
	        down_bits, down_width, down_height, 1, 0, 1);
    up1Pix   = XCreatePixmapFromBitmapData(theDisp, parent, 
                up1_bits, up1_width, up1_height, 1, 0, 1);
    down1Pix = XCreatePixmapFromBitmapData(theDisp, parent, 
	        down1_bits, down1_width, down1_height,1,0,1);

    uphPix   = XCreatePixmapFromBitmapData(theDisp, parent, 
		uph_bits, uph_width, uph_height, 1, 0, 1);
    downhPix = XCreatePixmapFromBitmapData(theDisp, parent, 
	        downh_bits, downh_width, downh_height, 1, 0, 1);
    uph1Pix  = XCreatePixmapFromBitmapData(theDisp, parent, 
                uph1_bits, uph1_width, uph1_height, 1, 0, 1);
    downh1Pix= XCreatePixmapFromBitmapData(theDisp, parent, 
	        downh1_bits, downh1_width, downh1_height,1,0,1);

    sgray    = XCreatePixmapFromBitmapData(theDisp, parent,
	        scrlgray_bits, scrlgray_width, scrlgray_height,fg,bg,dispDEEP);
  }

  sp->vert = vert;
  sp->len  = len;
  sp->fg   = fg;
  sp->bg   = bg;
  sp->hi   = hi;
  sp->lo   = lo;
  sp->uplit = sp->dnlit = 0;
  sp->tsize  =  19;

  if (vert) 
    sp->win = XCreateSimpleWindow(theDisp,parent,x,y,sp->tsize,len,1,fg,bg);
  else
    sp->win = XCreateSimpleWindow(theDisp,parent,x,y,len,sp->tsize,1,fg,bg);
  
  if (!sp->win) FatalError("can't create scrollbar window");

  sp->tmin   =  sp->tsize+1;
  sp->tmax   =  len - (sp->tsize+1) - sp->tsize;

  sp->drawobj = func;

  SCSetRange(sp, minv, maxv, curv, page);
  XSelectInput(theDisp, sp->win, ExposureMask | ButtonPressMask);
}


/***************************************************/
void SCChange(sp, x, y, vert, len, minv, maxv, curv, page)
SCRL         *sp;
int           x,y,vert,len,minv,maxv,curv,page;
{
  sp->vert = vert;
  sp->len  = len;
  sp->uplit = sp->dnlit = 0;

  if (vert) XMoveResizeWindow(theDisp, sp->win, x,y,sp->tsize,len);
       else XMoveResizeWindow(theDisp, sp->win, x,y,len,sp->tsize);

  sp->tmin   =  sp->tsize+1;
  sp->tmax   =  len - (sp->tsize+1) - sp->tsize;

  SCSetRange(sp, minv, maxv, curv, page);
}


/***************************************************/
void SCSetRange(sp, minv, maxv, curv, page)
SCRL *sp;
int   minv, maxv, curv, page;
{
  if (maxv<minv) maxv=minv;
  sp->min = minv;    sp->max = maxv;    sp->page = page;
  sp->active =  (minv < maxv);

  /* adjust scroll bar background */
  if (sp->active) {
    if (ctrlColor) XSetWindowBackground(theDisp, sp->win, sp->lo);
              else XSetWindowBackgroundPixmap(theDisp, sp->win, sgray);
  }
  else XSetWindowBackground(theDisp, sp->win, sp->bg);

  sp->val = -99999;  /* force a redraw */
  SCSetVal(sp, curv);
}


/***************************************************/
int SCSetVal(sp, curv)
SCRL *sp;
int   curv;
{
  /* returns '0' if no redraw was done */
  int oldval;

  RANGE(curv, sp->min, sp->max);   /* make sure curv is in-range */

  if (sp->val == curv) return 0;

  oldval = sp->val;
  if (oldval == -99999) oldval = curv;   /* ignore kludge */

  sp->val = curv;

  if (sp->active) 
    sp->tpos = sp->tmin + ((sp->tmax - sp->tmin)*(curv - sp->min))
               / (sp->max - sp->min);

  drawThumb(sp);
  (sp->drawobj)(curv-oldval, sp);   /* redraw what the scrlbar controls */

  XFlush(theDisp);
  return 1;
}


/***************************************************/
void SCRedraw(sp)
SCRL *sp;
{
  XSetForeground(theDisp, theGC, sp->fg);
  XSetBackground(theDisp, theGC, sp->bg);

  XClearWindow(theDisp, sp->win);
  
  drawArrow(sp,UPLINE);      /* draw up/down arrows */
  drawArrow(sp,DNLINE);

  XSetForeground(theDisp, theGC, sp->fg);

  if (sp->vert) {    
    XDrawLine(theDisp, sp->win, theGC, 0, sp->tsize, sp->tsize, sp->tsize);
    XDrawLine(theDisp, sp->win, theGC, 0, sp->len-sp->tsize-1, 
	      sp->tsize, sp->len-sp->tsize-1);
  }
  else {                       /* horizontal version */
    XDrawLine(theDisp, sp->win, theGC, sp->tsize, 0, sp->tsize, sp->tsize);
    XDrawLine(theDisp, sp->win, theGC, sp->len - sp->tsize-1, 0, 
	      sp->len - sp->tsize-1, sp->tsize);
  }

  drawThumb(sp);
}



/***************************************************/
static void drawArrow(sp,arr)
SCRL *sp;
int arr;
{
  Pixmap butpix;

  if (arr == UPLINE) {
    XSetForeground(theDisp, theGC, sp->bg);
    XFillRectangle(theDisp, sp->win, theGC, 0, 0, sp->tsize,sp->tsize);

    if (sp->vert) butpix = (sp->uplit) ? up1Pix  : upPix;
             else butpix = (sp->uplit) ? uph1Pix : uphPix;

    XSetForeground(theDisp, theGC, sp->fg);
    XSetBackground(theDisp, theGC, sp->bg);
    XCopyPlane(theDisp, butpix, sp->win, theGC, 0, 0, up_width,up_height,
	       3,3, 1L);
    Draw3dRect(sp->win, 0,0, sp->tsize-1, sp->tsize-1, 
	       (sp->uplit) ? R3D_IN : R3D_OUT, 2, sp->hi, sp->lo, sp->bg);
  }

  else if (arr == DNLINE) {
    if (sp->vert) {
      XSetForeground(theDisp, theGC, sp->bg);
      XFillRectangle(theDisp, sp->win, theGC, 0, sp->len - sp->tsize, 
		     sp->tsize,sp->tsize);
      butpix = (sp->dnlit) ? down1Pix : downPix;

      XSetForeground(theDisp, theGC, sp->fg);
      XSetBackground(theDisp, theGC, sp->bg);
      XCopyPlane(theDisp, butpix, sp->win, theGC, 0, 0, up_width,up_height,
		 3, sp->len - 3 - up_height, 1L);

      Draw3dRect(sp->win, 0, sp->len - sp->tsize, sp->tsize-1, sp->tsize-1, 
		 (sp->dnlit) ? R3D_IN : R3D_OUT, 2, sp->hi, sp->lo, sp->bg);
    }

    else {     /* horizontal scroll bar */
      XSetForeground(theDisp, theGC, sp->bg);
      XFillRectangle(theDisp, sp->win, theGC, sp->len - sp->tsize, 0,
		     sp->tsize, sp->tsize);
      butpix = (sp->dnlit) ? downh1Pix : downhPix;

      XSetForeground(theDisp, theGC, sp->fg);
      XSetBackground(theDisp, theGC, sp->bg);
      XCopyPlane(theDisp, butpix, sp->win, theGC, 0, 0, up_width,up_height,
		 sp->len - 3 - up_width, 3, 1L);

      Draw3dRect(sp->win, sp->len - sp->tsize, 0, sp->tsize-1, sp->tsize-1, 
		 (sp->dnlit) ? R3D_IN : R3D_OUT, 2, sp->hi, sp->lo, sp->bg);
    }
  }

  XFlush(theDisp);
}


/***************************************************/
static void drawThumb(sp)
SCRL *sp;
{
  if (sp->vert) {
    /* clear out thumb area with background color */
    XClearArea(theDisp, sp->win, 0, sp->tsize+1, sp->tsize, 
	       (sp->len-sp->tsize-1)-(sp->tsize+1), False);

    if (sp->active) {  /* a thumb is necessary */

      XSetForeground(theDisp, theGC, sp->bg);
      XFillRectangle(theDisp, sp->win, theGC,
		     1, sp->tpos+1, sp->tsize-2, sp->tsize-2);

      XSetForeground(theDisp, theGC, sp->fg);
      XDrawRectangle(theDisp, sp->win, theGC,
		     0, sp->tpos, sp->tsize-1, sp->tsize-1);

      XDrawLine(theDisp, sp->win, theGC, 9-3, sp->tpos+6, 9+3, sp->tpos+6);
      XDrawLine(theDisp, sp->win, theGC, 9-3, sp->tpos+8, 9+3, sp->tpos+8);
      XDrawLine(theDisp, sp->win, theGC, 9-3, sp->tpos+10,9+3, sp->tpos+10);
      XDrawLine(theDisp, sp->win, theGC, 9-3, sp->tpos+12,9+3, sp->tpos+12);

      Draw3dRect(sp->win, 1, sp->tpos+1, sp->tsize-3, sp->tsize-3, R3D_OUT,2,
		 sp->hi, sp->lo, sp->bg);
    }
  }

  else {   /* horizontal */
    /* clear out thumb area with background color */
    XClearArea(theDisp, sp->win, sp->tsize+1, 0, 
	       (sp->len-sp->tsize-1)-(sp->tsize+1), sp->tsize, False);

    if (sp->active) {  /* a thumb is necessary */
      XSetForeground(theDisp, theGC, sp->bg);
      XFillRectangle(theDisp, sp->win, theGC,
		     sp->tpos+1, 1, sp->tsize-2, sp->tsize-2);

      XSetForeground(theDisp, theGC, sp->fg);
      XDrawRectangle(theDisp, sp->win, theGC,
		     sp->tpos, 0, sp->tsize-1, sp->tsize-1);

      XDrawLine(theDisp, sp->win, theGC, sp->tpos+6, 9-3, sp->tpos+6, 9+3);
      XDrawLine(theDisp, sp->win, theGC, sp->tpos+8, 9-3, sp->tpos+8, 9+3);
      XDrawLine(theDisp, sp->win, theGC, sp->tpos+10,9-3, sp->tpos+10,9+3);
      XDrawLine(theDisp, sp->win, theGC, sp->tpos+12,9-3, sp->tpos+12,9+3);

      Draw3dRect(sp->win, sp->tpos+1, 1, sp->tsize-3, sp->tsize-3, R3D_OUT,2,
		 sp->hi, sp->lo, sp->bg);
    }
  }
}



/***************************************************/
static int whereInScrl(sp,x,y)
SCRL *sp;
int x,y;
{
  int v;

  /* returns region # that x,y is in.  Returns '-1' if none */

  v=0;
  if (sp->vert) {
    if (x<0 || x>sp->tsize || y<0 || y>sp->len) return -1;
    v = y;
  }
  else {
    if (y<0 || y>sp->tsize || x<0 || x>sp->len) return -1;
    v = x;
  }

  /* once we know it's in scroll bar, only have to check 'v' versus len */
  if (v < sp->tmin)               return UPLINE;
  if (sp->active) {
    if (v <  sp->tpos)             return UPPAGE;
    if (v <  sp->tpos + sp->tsize) return THUMB;
    if (v <= sp->tmax + sp->tsize) return DNPAGE;
  }
  if (v > sp->tmax+sp->tsize)    return DNLINE;

  return -1;
}


/***************************************************/
void SCTrack(sp,mx,my)
SCRL *sp;
int mx,my;
{
  Window       rW,cW;
  int          rx,ry, x,y, ipos, pos, lit, tx, ty, tyoff, txoff, ty1, tx1;
  unsigned int mask;

  /* determine in which of the five regions of the scroll bar the mouse
     was clicked (upline, downline, uppage, downpage, thumb) */

  tx = ty = txoff = tyoff = 0;

  if (!sp->active) return;

  XSetForeground(theDisp, theGC, sp->fg);
  XSetBackground(theDisp, theGC, sp->bg);

  /* light up appropriate bit of scroll bar */
  ipos = whereInScrl(sp,mx,my);
  lit = 1;

  switch (ipos) {
  case UPLINE:  sp->uplit = 1;
                drawArrow(sp, UPLINE);
                if (sp->val > sp->min) SCSetVal(sp,sp->val-1);
                Timer(SCRLWAIT);
                break;

  case DNLINE:  sp->dnlit = 1;
                drawArrow(sp, DNLINE);
                if (sp->val < sp->max) SCSetVal(sp,sp->val+1);
                Timer(SCRLWAIT);
                break;

  case UPPAGE:  SCSetVal(sp,sp->val - sp->page);  break;
  case DNPAGE:  SCSetVal(sp,sp->val + sp->page);  break;
  case THUMB:   tyoff = sp->tpos - my;
                txoff = sp->tpos - mx;
                ty = sp->tpos;
                tx = sp->tpos;
                break;
  }

  while (XQueryPointer(theDisp,sp->win,&rW,&cW,&rx,&ry,&x,&y,&mask)) {
    if (!(mask & Button1Mask)) break;    /* button released */

    switch (ipos) {

    case THUMB:
      /* do thumb tracking */

      if (sp->vert) {
	ty1 = y+tyoff;
	RANGE(ty1, sp->tmin, sp->tmax);
	if (ty != ty1) {    /* but only if mouse has moved */
	  int dt, dv;
	  dt = sp->tmax - sp->tmin;
	  dv = sp->max  - sp->min;
	  ty = ty1;
	  SCSetVal(sp, sp->min + (dv*(ty - sp->tmin)+dt/2) / dt);
	}
      }
      else {
	tx1 = x+txoff;
	RANGE(tx1, sp->tmin, sp->tmax);
	if (tx != tx1) {    /* but only if mouse has moved */
	  int dt, dv;
	  dt = sp->tmax - sp->tmin;
	  dv = sp->max  - sp->min;
	  tx = tx1;
	  SCSetVal(sp, sp->min + (dv*(tx - sp->tmin)+dt/2) / dt);
	}
      }
      break;


    case UPLINE:
    case DNLINE:                     /* arrows */
      pos = whereInScrl(sp,x,y);
      if (pos == ipos) {
	if (!lit) { 
	  lit = 1; 
	  if (ipos == UPLINE) { sp->uplit = 1;  drawArrow(sp,UPLINE); }
	                 else { sp->dnlit = 1;  drawArrow(sp,DNLINE); }
	}

	else {
	  if (sp->val > sp->min && pos==UPLINE) {
	    SCSetVal(sp, sp->val-1);
	    Timer(SCRLWAIT);
	  }
	  else if (sp->val < sp->max && pos==DNLINE) {
	    SCSetVal(sp, sp->val+1);
	    Timer(SCRLWAIT);
	  }
	}
      }
      
      else {
	if (lit) { 
	  lit = 0; 
	  if (ipos == UPLINE) { sp->uplit = 0;  drawArrow(sp,UPLINE); }
	                 else { sp->dnlit = 0;  drawArrow(sp,DNLINE); }
	}
      }
      break;
      
    }
  }


  if (lit && ipos == UPLINE) { sp->uplit = 0; drawArrow(sp, UPLINE); }
  if (lit && ipos == DNLINE) { sp->dnlit = 0; drawArrow(sp, DNLINE); }
}


















