/* 
 * xvdial.c - DIAL handling functions
 *
 * callable functions:
 *
 *   DCreate()   -  creates a dial
 *   DSetRange() -  sets min/max/current values of control
 *   DSetVal()   -  sets value of control 
 *   DSetActive() - turns dial '.active' on and off
 *   DRedraw()   -  redraws the dial
 *   DTrack()    -  called when clicked.  Operates control 'til mouseup
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

static Pixmap cw1Pix, ccw1Pix;  /* up/down arrows */
static Pixmap cw2Pix, ccw2Pix;  /* up/down page arrows */
static int    pixmaps_built=0;   /* true if pixmaps created already */

#define PW dial_cw1_width        /* size of arrows */
#define PH dial_cw1_height

/* dial regions */
#define INCW1  0
#define INCCW1 1
#define INCW2  2
#define INCCW2 3
#define INDIAL 4

#define INC1WAIT 150   /* milliseconds to wait after initial hit */
#define INC2WAIT 150   /* milliseconds to wait between increments */
#define DEG2RAD (3.14159265 / 180.0)
#define RAD2DEG (180.0 / 3.14159265)


/* local functions */
#ifdef __STDC__
static int  whereInDial(DIAL *, int, int);
static void drawArrow(DIAL *);
static void drawValStr(DIAL *);
static void drawButt(DIAL *, int, int);
static int  computeDialVal(DIAL *, int, int);
static void dimDial(DIAL *);
#else
static int  whereInDial(), computeDialVal();
static void drawArrow(), drawValStr(), drawButt(), dimDial();
#endif


/***************************************************/
void DCreate(dp, parent, x, y, w, h, minv, maxv, curv, page, 
	          fg, bg, hi, lo, title, units)
DIAL         *dp;
Window        parent;
int           x,y,w,h,minv,maxv,curv,page;
unsigned long fg,bg,hi,lo;
char         *title, *units;
{

  if (!pixmaps_built) {
    cw1Pix   = XCreatePixmapFromBitmapData(theDisp, parent, 
		dial_cw1_bits, PW, PH, fg, bg, dispDEEP);
    ccw1Pix  = XCreatePixmapFromBitmapData(theDisp, parent, 
	        dial_ccw1_bits, PW, PH, fg, bg, dispDEEP);
    cw2Pix   = XCreatePixmapFromBitmapData(theDisp, parent, 
                dial_cw2_bits, PW, PH, fg, bg, dispDEEP);
    ccw2Pix  = XCreatePixmapFromBitmapData(theDisp, parent, 
	        dial_ccw2_bits, PW, PH, fg, bg, dispDEEP);
  }

  dp->w     = w;
  dp->h     = h;
  dp->fg    = fg;
  dp->bg    = bg;
  dp->hi    = hi;
  dp->lo    = lo;
  dp->title = title;
  dp->units = units;
  dp->active = 1;
  dp->drawobj = NULL;

  if (w < h-24-16) dp->rad = (w - 8) / 2;
           else dp->rad = (h - 24 - 16 - 8) / 2;
  dp->cx = w / 2;
  dp->cy = dp->rad + 4 + 16;

  dp->bx[INCCW1] = 4;       dp->by[INCCW1] = h - 4 - 20;
  dp->bx[INCCW2] = 4;       dp->by[INCCW2] = h - 4 - 10;
  dp->bx[INCW1]  = w-14-4;  dp->by[INCW1]  = h - 4 - 20;
  dp->bx[INCW2]  = w-14-4;  dp->by[INCW2]  = h - 4 - 10;

  dp->win = XCreateSimpleWindow(theDisp, parent,x,y,w,h,1,fg,bg);
  if (!dp->win) FatalError("can't create dial window");

  DSetRange(dp, minv, maxv, curv, page);
  XSelectInput(theDisp, dp->win, ExposureMask | ButtonPressMask);
}


/***************************************************/
void DSetRange(dp, minv, maxv, curv, page)
DIAL *dp;
int   minv, maxv, curv, page;
{
  if (maxv<minv) maxv=minv;
  dp->min = minv;    dp->max = maxv;    dp->page = page;
  dp->active =  (minv < maxv);

  DSetVal(dp, curv);
}


/***************************************************/
void DSetVal(dp, curv)
DIAL *dp;
int   curv;
{
  RANGE(curv, dp->min, dp->max);   /* make sure curv is in-range */

  if (curv == dp->val) return;

  /* erase old arrow */
  XSetForeground(theDisp, theGC, dp->bg); 
  drawArrow(dp);

  dp->val = curv;

  /* draw new arrow and string */
  XSetForeground(theDisp, theGC, dp->fg);
  XSetBackground(theDisp, theGC, dp->bg); 
  drawArrow(dp);
  drawValStr(dp);
  if (!dp->active) dimDial(dp);

  XFlush(theDisp);
}


/***************************************************/
void DSetActive(dp, i)
DIAL *dp;
int   i;
{
  if (i == dp->active) return;

  dp->active = i;
  XClearWindow(theDisp, dp->win);
  DRedraw(dp);
  XFlush(theDisp);
}


/***************************************************/
void DRedraw(dp)
DIAL *dp;
{
  double tsize;
  int    i, rad, cx, cy, x1, y1, x2, y2;

  rad = dp->rad;  cx = dp->cx;  cy = dp->cy;

  Draw3dRect(dp->win, 0,0, dp->w-1, dp->h-1, R3D_OUT, 2,
	     dp->hi, dp->lo, dp->bg);

  XSetForeground(theDisp, theGC, dp->fg);
  XSetBackground(theDisp, theGC, dp->bg);

  /* draw title */
  CenterString(dp->win, dp->title, dp->w/2, 8);

  /* draw tick marks around circle */
  for (i = -60; i<=240; i += 10) {
    if (i%60 == 0) tsize = 0.85;  else tsize = 0.95;
    x1 = cx + (int) ((double) rad * cos(i * DEG2RAD));
    y1 = cy - (int) ((double) rad * sin(i * DEG2RAD));
    x2 = cx + (int) ((double) rad * tsize  * cos(i*DEG2RAD));
    y2 = cy - (int) ((double) rad * tsize  * sin(i*DEG2RAD));

    XDrawLine(theDisp, dp->win, theGC, x1, y1, x2, y2);
  }

  drawArrow(dp);

  /* draw the cw/ccw controls */
  for (i=0; i<4; i++) drawButt(dp, i, 0);

  drawValStr(dp);

  if (!dp->active) dimDial(dp);
}


/***************************************************/
int DTrack(dp, mx, my)
DIAL *dp;
int mx,my;
{
  Window       rW,cW;
  int          rx,ry, x,y, ipos, pos, lit, i, origval;
  unsigned int mask;

  lit = 0;

  if (!dp->active) return 0;

  XSetForeground(theDisp, theGC, dp->fg);
  XSetBackground(theDisp, theGC, dp->bg);

  /* determine in which of the five regions of the dial the mouse
     was clicked (cw1, ccw1, cw2, ccw2, dial-proper) */

  ipos = whereInDial(dp, mx, my);
  if (ipos<0) return 0;          /* didn't hit any of the actual controls */

  origval = dp->val;

  /* light up appropriate button, if it's in one of them */
  if (ipos != INDIAL) {
    drawButt(dp, ipos, 1);
    switch (ipos) {
    case INCW1:  if (dp->val < dp->max) DSetVal(dp, dp->val+1); break;
    case INCW2:  if (dp->val < dp->max) DSetVal(dp, dp->val+dp->page); break;
    case INCCW1: if (dp->val > dp->min) DSetVal(dp, dp->val-1); break;
    case INCCW2: if (dp->val > dp->min) DSetVal(dp, dp->val-dp->page); break;
    }
    if (dp->drawobj != NULL) (dp->drawobj)();  
    Timer(INC1WAIT);
    lit = 1;
  }

  else { 
    i = computeDialVal(dp, mx, my);
    DSetVal(dp, i);
    if (dp->drawobj != NULL) (dp->drawobj)();  
  }

  
  /* loop until mouse is released */
  while (XQueryPointer(theDisp,dp->win,&rW,&cW,&rx,&ry,&x,&y,&mask)) {
    if (!(mask & Button1Mask)) break;    /* button released */

    if (ipos == INDIAL) {
      int j;
      i = computeDialVal(dp, x, y);
      j = dp->val;
      DSetVal(dp, i);
      if (j != dp->val) {
	/* track whatever dial controls */
	if (dp->drawobj != NULL) (dp->drawobj)();  
      }
    }

    else {                            /* a button */
      pos = whereInDial(dp, x, y);
      if ( (pos==ipos && !lit) || (pos!=ipos && lit)) {
	/* need to toggle lit state */
	lit = !lit;
	drawButt(dp, ipos, lit);
      }

      if (lit) {
	switch (ipos) {
	case INCW1:  if (dp->val < dp->max) DSetVal(dp, dp->val+1); 
	             break;
	case INCW2:  if (dp->val < dp->max) DSetVal(dp, dp->val+dp->page);
                     break;
	case INCCW1: if (dp->val > dp->min) DSetVal(dp, dp->val-1);
                     break;
	case INCCW2: if (dp->val > dp->min) DSetVal(dp, dp->val-dp->page);
                     break;
	}

	/* track whatever dial controls */
	if (dp->drawobj != NULL) (dp->drawobj)();  

	Timer(INC2WAIT);
      }
    }
    XFlush(theDisp);
  }


  /* turn off button, if lit */
  if (ipos != INDIAL && lit) drawButt(dp, ipos, 0);

  return (dp->val != origval);
}





/***************************************************/
static int whereInDial(dp, x, y)
DIAL *dp;
int x, y;
{
  int i;

  /* returns region * that x,y is in.  returns -1 if none */

  for (i=0; i<4; i++) 
    if (PTINRECT(x,y, dp->bx[i], dp->by[i], 14, 10)) return i;

  if (PTINRECT(x,y, dp->cx - dp->rad, dp->cy - dp->rad, 
	       2*dp->rad, 2*dp->rad))
    return INDIAL;

  return -1;
}

	  
/***************************************************/
static void drawArrow(dp)
DIAL *dp;
{
  int i, rad, cx, cy;
  XPoint arrow[4];

  rad = dp->rad;  cx = dp->cx;  cy = dp->cy;

  /* map pos (range minv..maxv) into degrees (range 240..-60) */
  i = 240 + (-300 * (dp->val - dp->min)) / (dp->max - dp->min);
  arrow[0].x = cx + (int) ((double) rad * .80 * cos(i * DEG2RAD));
  arrow[0].y = cy - (int) ((double) rad * .80 * sin(i * DEG2RAD));
  arrow[1].x = cx + (int) ((double) rad * .33 * cos((i+160) * DEG2RAD));
  arrow[1].y = cy - (int) ((double) rad * .33 * sin((i+160) * DEG2RAD));
  arrow[2].x = cx + (int) ((double) rad * .33 * cos((i-160) * DEG2RAD));
  arrow[2].y = cy - (int) ((double) rad * .33 * sin((i-160) * DEG2RAD));
  arrow[3].x = arrow[0].x;
  arrow[3].y = arrow[0].y;
  XDrawLines(theDisp, dp->win, theGC, arrow, 4, CoordModeOrigin);
}


/***************************************************/
static void drawValStr(dp)
DIAL *dp;
{
  int  i, x1, x2;
  char foo[60], foo1[60];

  /* compute longest string necessary so we can right-align this thing */
  sprintf(foo,"%d",dp->min);    x1 = strlen(foo);
  sprintf(foo,"%d",dp->max);    x2 = strlen(foo);
  if (dp->min < 0 && dp->max > 0) x2++;   /* put '+' at beginning */
  i = x1;  if (x2>x1) i = x2;
  if (dp->units) i += strlen(dp->units);

  if (dp->min < 0 && dp->max > 0) sprintf(foo,"%+d", dp->val);
  else sprintf(foo,"%d", dp->val);

  if (dp->units) strcat(foo,dp->units);
  foo1[0] = '\0';
  if (strlen(foo)<i) {
    for (i = i - strlen(foo); i>0; i--) strcat(foo1," ");
  }
  strcat(foo1, foo);

  XSetForeground(theDisp, theGC, dp->fg);
  XSetBackground(theDisp, theGC, dp->bg);
  XSetFont(theDisp, theGC, monofont);
  XDrawImageString(theDisp, dp->win, theGC, 
		   dp->w/2 - XTextWidth(monofinfo, foo1, strlen(foo1))/2,
		   dp->h-14 - (monofinfo->ascent + monofinfo->descent)/2
		      + monofinfo->ascent, foo1, strlen(foo1));
  XSetFont(theDisp, theGC, mfont);
}


/***************************************************/
static void drawButt(dp, i, lit)
     DIAL *dp;
     int i, lit;
{
  Pixmap pix = (Pixmap) NULL;

  XSetForeground(theDisp, theGC, dp->fg);
  XDrawRectangle(theDisp, dp->win, theGC, dp->bx[i], dp->by[i], 14, 10);

  XSetForeground(theDisp, theGC, dp->bg);
  XFillRectangle(theDisp, dp->win, theGC, dp->bx[i]+1, dp->by[i]+1, 13, 9);

  if (!lit) Draw3dRect(dp->win, dp->bx[i]+1,dp->by[i]+1, 12,8, R3D_OUT, 1,
		       dp->hi, dp->lo, dp->bg);

  switch (i) {
  case INCCW1: pix = ccw1Pix;  break;
  case INCCW2: pix = ccw2Pix;  break;
  case INCW1:  pix = cw1Pix;   break;
  case INCW2:  pix = cw2Pix;   break;
  }

  XCopyArea(theDisp, pix, dp->win, theGC, 0, 0, PW, PH,
	    dp->bx[i]+(15-PW)/2, dp->by[i]+(11-PH)/2);

  if (lit) {
    XSetState(theDisp, theGC, dp->fg, dp->bg, GXinvert, dp->fg ^ dp->bg);
    XFillRectangle(theDisp, dp->win, theGC, dp->bx[i]+1, dp->by[i]+1, 13, 9);
    XSetState(theDisp, theGC, dp->fg, dp->bg, GXcopy, AllPlanes);
    XFlush(theDisp);
  }
}


/***************************************************/
static int computeDialVal(dp, x, y)
DIAL *dp;
int x, y;
{
  int dx, dy, val;
  double angle;

  /* compute dx, dy (distance from cx, cy).  Note: +dy is *up* */
  dx = x - dp->cx;  dy = dp->cy - y;

  /* if too close to center, return current value to avoid 'spazzing' */
  if (abs(dx) < 3 && abs(dy) < 3) return dp->val;

  /* figure out angle of vector dx,dy */
  if (dx==0) {     /* special case */
    if (dy>0) angle =  90.0;
         else angle = -90.0;
  }
  else if (dx>0) angle = atan((double)  dy / (double)  dx) * RAD2DEG;
  else           angle = atan((double) -dy / (double) -dx) * RAD2DEG + 180.0;
    
  /* map angle into range: -90..270, then into to value */
  if (angle > 270.0) angle -= 360.0;
  if (angle < -90.0) angle += 360.0;

  val = (int) ((dp->max - dp->min) * (240.0 - angle) / 300.0) + dp->min;

  return val;
}


/***************************************************/
static void dimDial(dp)
DIAL *dp;
{
  DimRect(dp->win, 0, 0, dp->w, dp->h, dp->bg);
}

