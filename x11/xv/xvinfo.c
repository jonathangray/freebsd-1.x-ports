/* 
 * xvinfo.c - 'Info' box handling functions
 *
 * callable functions:
 *
 *   CreateInfo(geom)       -  creates the infoW window.  Doesn't map it.
 *   InfoBox(vis)           -  random processing based on value of 'vis'
 *                             maps/unmaps window, etc.
 *   RedrawInfo(x,y,w,h)    -  called by 'expose' events
 *   SetInfoMode(mode)      -  changes amount of info Info window shows
 *   SetISTR(st, fmt, args) - sprintf's into ISTR #st.  Redraws it in window 
 *   char *GetISTR(st)      - returns pointer to ISTR #st, or NULL if st bogus
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


#define  NEEDSARGS

#include "xv.h"
#include "bitmaps.h"

/* max length of an Info String */
#define ISTRLEN 80

/* baseline of top line of text */
#define TOPBASE (36 + penn_height/2 + 4 + 8 + ASCENT)
#define STLEFT  100   /* left edge of strings */

static Pixmap graspPix, pennPix;
static char istrs[NISTR][ISTRLEN];

#ifdef __STDC__
static void DrawStrings(void);
static void DrawFieldName(int);
static void RedrawString(int);
#else
static void DrawStrings(), DrawFieldName(), RedrawString();
#endif



/***************************************************/
void CreateInfo(geom)
char *geom;
{
  infoW = CreateWindow("xv info", "XVinfo", geom, 
		       INFOWIDE, INFOHIGH, infofg, infobg, 0);
  if (!infoW) FatalError("can't create info window!");

  pennPix = XCreatePixmapFromBitmapData(theDisp, infoW, penn_bits, penn_width, 
			      penn_height, infofg, infobg, dispDEEP);
  graspPix = XCreatePixmapFromBitmapData(theDisp,infoW,grasp_bits,grasp_width, 
			      grasp_height, infofg, infobg, dispDEEP);
}
  

/***************************************************/
void InfoBox(vis)
int vis;
{
  if (vis) XMapRaised(theDisp, infoW);
  else     XUnmapWindow(theDisp, infoW);

  infoUp = vis;
}


/***************************************************/
void RedrawInfo(x,y,w,h)
int x,y,w,h;
{
  int  i;
  XRectangle xr;

#ifdef CLIPRECT
  xr.x = x;  xr.y = y;  xr.width = w;  xr.height = h;
  XSetClipRectangles(theDisp, theGC, 0,0, &xr, 1, Unsorted);
#endif

  XSetForeground(theDisp, theGC, infofg);
  XSetBackground(theDisp, theGC, infobg);

  /* draw the two icons */
  XCopyArea(theDisp, pennPix, infoW, theGC, 0, 0, penn_width, penn_height,
	    36 - penn_width/2, 36 - penn_height/2);
  XCopyArea(theDisp, graspPix, infoW, theGC, 0, 0, grasp_width, grasp_height,
	    INFOWIDE - 36 - grasp_width/2, 36 - grasp_height/2);

  /* draw the credits */
  sprintf(str,"XV   -   %s",REVDATE);
  CenterString(infoW, str, INFOWIDE/2, 36-LINEHIGH);
  CenterString(infoW, "by John Bradley  (bradley@cis.upenn.edu)"
	       , INFOWIDE/2, 36);
  CenterString(infoW, 
	       "Copyright 1993, John Bradley  -  All Rights Reserved",
	       INFOWIDE/2, 36+LINEHIGH);

  /* draw the dividing lines */
  i = 36 + penn_height/2 + 4;

  XDrawLine(theDisp, infoW, theGC, 0, i, INFOWIDE, i);
  XDrawLine(theDisp, infoW, theGC, 0, INFOHIGH-22, INFOWIDE, INFOHIGH-22);
  XDrawLine(theDisp, infoW, theGC, 0, INFOHIGH-42, INFOWIDE, INFOHIGH-42);

  if (ctrlColor) {
    XSetForeground(theDisp, theGC, locol);
    XDrawLine(theDisp, infoW, theGC, 0, i+1, INFOWIDE, i+1);
    XDrawLine(theDisp, infoW, theGC, 0, INFOHIGH-21, INFOWIDE, INFOHIGH-21);
    XDrawLine(theDisp, infoW, theGC, 0, INFOHIGH-41, INFOWIDE, INFOHIGH-41);
  }

  if (ctrlColor) XSetForeground(theDisp, theGC, hicol);
  XDrawLine(theDisp, infoW, theGC, 0, i+2, INFOWIDE, i+2);
  XDrawLine(theDisp, infoW, theGC, 0, INFOHIGH-20, INFOWIDE, INFOHIGH-20);
  XDrawLine(theDisp, infoW, theGC, 0, INFOHIGH-40, INFOWIDE, INFOHIGH-40);

  DrawStrings();

#ifdef CLIPRECT
  XSetClipMask(theDisp, theGC, None);
#endif
}


/***************************************************/
static void DrawStrings()
{
  int i;
  for (i=0; i<6; i++) DrawFieldName(i);     /* draw the field titles */
  for (i=0; i<NISTR; i++) RedrawString(i);  /* draw the field values */
  XFlush(theDisp);
}


/***************************************************/
static void DrawFieldName(fnum)
int fnum;
{
  static char *fname[6] = {  "Filename:", "Format:", "Resolution:", 
			     "Cropping:", "Expansion:", "Colors:" };

  XSetForeground(theDisp, theGC, infofg);
  XSetBackground(theDisp, theGC, infobg);

  if (infoMode == INF_NONE || infoMode == INF_STR) return;
  if (infoMode == INF_PART && fnum>=3) return;

  XDrawString(theDisp, infoW, theGC, 10, TOPBASE + fnum*LINEHIGH, 
		fname[fnum], strlen(fname[fnum]));
}


/***************************************************/
static void RedrawString(st)
int st;
{
  /* erase area of string, and draw it with new contents */
  
  if (infoMode == INF_NONE) return;
  if (infoMode == INF_STR && st > ISTR_WARNING) return;
  if (infoMode == INF_PART && st > ISTR_RES) return;


  if (st == ISTR_INFO) {
    XSetForeground(theDisp, theGC, infobg);
    XFillRectangle(theDisp, infoW, theGC, 0, INFOHIGH-39, INFOWIDE, 17);
    XSetForeground(theDisp, theGC, infofg);
    CenterString(infoW, istrs[st], INFOWIDE/2, INFOHIGH-31);
  }
  else if (st == ISTR_WARNING) {
    XSetForeground(theDisp, theGC, infobg);
    XFillRectangle(theDisp, infoW, theGC, 0, INFOHIGH-19, INFOWIDE, 17);
    XSetForeground(theDisp, theGC, infofg);
    CenterString(infoW, istrs[st], INFOWIDE/2, INFOHIGH-10);
  }
  else {
    XSetForeground(theDisp, theGC, infobg);
    XFillRectangle(theDisp, infoW, theGC, 
		   STLEFT, TOPBASE - ASCENT + (st-ISTR_FILENAME)*LINEHIGH, 
		   INFOWIDE-STLEFT, LINEHIGH);
    XSetForeground(theDisp, theGC, infofg);
    XDrawString(theDisp, infoW, theGC, STLEFT,TOPBASE
		+ (st-ISTR_FILENAME)*LINEHIGH,	istrs[st], strlen(istrs[st]));
  }
}



/***************************************************/
void SetInfoMode(mode)
int mode;
{
  int y1, y2;

  infoMode = mode;
  if (infoUp) {   /* only do this if window is mapped */
    y1 = TOPBASE - ASCENT;
    y2 = INFOHIGH-43;

    XSetForeground(theDisp, theGC, infobg);

    XFillRectangle(theDisp, infoW, theGC, 0, y1, INFOWIDE, y2-y1);
    XFillRectangle(theDisp, infoW, theGC, 0, INFOHIGH-39, INFOWIDE, 17);
    XFillRectangle(theDisp, infoW, theGC, 0, INFOHIGH-19, INFOWIDE, 17);

    DrawStrings();
  }
}


/***************************************************/
/* SetISTR( ISTR, format, arg1, arg2, ...)	   */

#if defined(__STDC__) && !defined(NOSTDHDRS)
void SetISTR(int stnum, ...)
{
  va_list args;
  char     *fmt;

  va_start(args, stnum);
#else
/*VARARGS0*/
void SetISTR(va_alist)
va_dcl
{
  va_list args;
  char    *fmt;
  int     stnum;

  va_start(args);

  stnum = va_arg(args, int);
#endif
  if (stnum>=0 && stnum < NISTR) {
    fmt = va_arg(args, char *);
    if (fmt) vsprintf(istrs[stnum], fmt, args);
    else istrs[stnum][0] = '\0';
  }
  va_end(args);
  
  if (stnum == ISTR_COLOR) {
    sprintf(istrs[ISTR_INFO], "%s  %s", formatStr, istrs[ISTR_COLOR]);
  }

  if (infoUp) {
    RedrawString(stnum);
    if (stnum == ISTR_COLOR) RedrawString(ISTR_INFO);
    XFlush(theDisp);
  }

  if (ctrlUp && (stnum == ISTR_INFO || stnum == ISTR_WARNING || 
		 stnum == ISTR_COLOR)) {
    DrawCtrlStr();
    XFlush(theDisp);
  }

  if (anyBrowUp && (stnum == ISTR_WARNING || stnum == ISTR_INFO) 
      && strlen(istrs[stnum])) {
    SetBrowStr(istrs[stnum]);
    XFlush(theDisp);
  }

  if (stnum == ISTR_WARNING && !ctrlUp && !infoUp && !anyBrowUp && 
      strlen(istrs[stnum])) {
    OpenAlert(istrs[stnum]);
    sleep(3);
    CloseAlert();
  }
}


/***************************************************/
char *GetISTR(stnum)
int stnum;
{
  /* returns pointer to ISTR string */
  if (stnum < 0 || stnum>=NISTR) return(NULL);
  return (istrs[stnum]);
}


