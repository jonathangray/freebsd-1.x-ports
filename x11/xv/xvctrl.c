/* 
 * xvctrl.c - Control box handling functions
 *
 * callable functions:
 *
 *   CreateCtrl(geom)       -  creates the ctrlW window.  Doesn't map it.
 *   CtrlBox(vis)           -  random processing based on value of 'vis'
 *                             maps/unmaps window, etc.
 *   RedrawCtrl(x,y,w,h)    -  called by 'expose' events
 *   ClickCtrl(x,y)
 *   DrawCtrlStr()          -  called to redraw 'ISTR_INFO' string in ctrlW
 *   ScrollToCurrent()      -  called when list selection is changed 
 *
 *   LSCreate()             -  creates a listbox
 *   LSRedraw()             -  redraws 'namelist' box
 *   LSClick()              -  operates list box
 *   LSChangeData()         -  like LSNewData(), but tries not to repos list
 *   LSNewData()            -  called when strings or number of them change
 *   LSKey()                -  called to handle page up/down, arrows
 *
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

#define DBLCLKTIME 500             /* double-click speed in milliseconds */

#define INACTIVE(lptr, item) ((lptr)->filetypes && (lptr)->dirsonly && \
			      (item) >= 0 && (item) < (lptr)->nstr && \
			      (lptr)->str[(item)][0] != C_DIR && \
			      (lptr)->str[(item)][0] != C_LNK)

#define NLINES 13                  /* # of lines in list control (keep odd) */

#define BUTTW   71   /* must be odd for 'half' buttons to work out */
#define BUTTH   24

static int    ptop;                /* y-coord of top of button area in ctrlW */

static Pixmap fifoPix, chrPix, dirPix, blkPix, lnkPix, sockPix, exePix, regPix;
static Pixmap rotlPix, rotrPix, fliphPix, flipvPix, p10Pix, m10Pix;

static XRectangle butrect;

/* NOTE: make sure this matches up with DMB_* definitions in xv.h */
static char *dispMList[] = { "Window", 
			     "Root: tiled",
			     "Root: integer tiled",
			     "Root: mirrored",
			     "Root: integer mirrored",
			     "Root: center tiled",
			     "Root: centered",
			     "Root: centered, warp",
			     "Root: centered, brick",
    		             "Root: symmetrical tiled",
			     "Root: symmetrical mirrored",
			     MBSEP,
			     "Read/Write Colors",
			     MBSEP,
			     "Normal Colors",
			     "Perfect Colors",
			     "Use Own Colormap",
			     "Use Std. Colormap" };

static char *conv24MList[] = { "8-bit mode",
			       "24-bit mode",
			       MBSEP,
			       "Lock current mode",
			       MBSEP,
                               "Quick 24->8",
			       "Slow 24->8",
			       "Best 24->8" };

static char *algMList[]    = { "Undo All",
			       MBSEP,
 			       "Blur (3x3)",
			       "Blur (5x5)",
			       "Blur (7x7)",
			       "Edge Detection",
			       "Emboss",
			       "Oil Painting"};

#ifdef __STDC__
static void drawSel(LIST *, int);
static void RedrawNList(int, SCRL *);
static void ls3d(LIST *);
#else
static void drawSel(), RedrawNList(), ls3d();
#endif


/***************************************************/
void CreateCtrl(geom)
char *geom;
{
  int i, listh, topskip;
  double skip;
  XSetWindowAttributes xswa;

  ctrlW = CreateWindow("xv controls", "XVcontrols", geom, 
		       CTRLWIDE, CTRLHIGH, infofg, infobg, 0);
  if (!ctrlW) FatalError("can't create controls window!");

#ifdef BACKING_STORE
  xswa.backing_store = WhenMapped;
  XChangeWindowAttributes(theDisp, ctrlW, CWBackingStore, &xswa);
#endif

  grayTile = XCreatePixmapFromBitmapData(theDisp, rootW, gray25_bits,
	     gray25_width, gray25_height, infofg, infobg, dispDEEP);

  dimStip = XCreatePixmapFromBitmapData(theDisp, ctrlW, gray50_bits,
	     gray50_width, gray50_height, 1, 0, 1);
  
  fifoPix  = XCreatePixmapFromBitmapData(theDisp, ctrlW, i_fifo_bits,
	     i_fifo_width, i_fifo_height, 1, 0, 1);
  
  chrPix  = XCreatePixmapFromBitmapData(theDisp, ctrlW, i_chr_bits,
	     i_chr_width, i_chr_height, 1,0,1);
  
  dirPix  = XCreatePixmapFromBitmapData(theDisp, ctrlW, i_dir_bits,
	     i_dir_width, i_dir_height, 1,0,1);
  
  blkPix  = XCreatePixmapFromBitmapData(theDisp, ctrlW, i_blk_bits,
	     i_blk_width, i_blk_height, 1,0,1);
  
  lnkPix  = XCreatePixmapFromBitmapData(theDisp, ctrlW, i_lnk_bits,
	     i_lnk_width, i_lnk_height, 1,0,1);
  
  sockPix  = XCreatePixmapFromBitmapData(theDisp, ctrlW, i_sock_bits,
	     i_sock_width, i_sock_height, 1,0,1);
  
  exePix   = XCreatePixmapFromBitmapData(theDisp, ctrlW, i_exe_bits,
	     i_exe_width, i_exe_height, 1,0,1);
  
  regPix  = XCreatePixmapFromBitmapData(theDisp, ctrlW, i_reg_bits,
	     i_reg_width, i_reg_height, 1,0,1);

  rotlPix = XCreatePixmapFromBitmapData(theDisp, ctrlW, h_rotl_bits, 
					h_rotl_width, h_rotl_height, 1, 0, 1);

  rotrPix = XCreatePixmapFromBitmapData(theDisp, ctrlW, h_rotr_bits, 
					h_rotr_width, h_rotr_height, 1, 0, 1);

  fliphPix = XCreatePixmapFromBitmapData(theDisp, ctrlW, fliph_bits, 
					fliph_width, fliph_height, 1, 0, 1);

  flipvPix = XCreatePixmapFromBitmapData(theDisp, ctrlW, flipv_bits, 
					flipv_width, flipv_height, 1, 0, 1);

  p10Pix   = XCreatePixmapFromBitmapData(theDisp, ctrlW, p10_bits, 
					p10_width, p10_height, 1, 0, 1);

  m10Pix   = XCreatePixmapFromBitmapData(theDisp, ctrlW, m10_bits, 
					m10_width, m10_height, 1, 0, 1);

  if (!grayTile || !dimStip || !fifoPix || !chrPix || !dirPix ||
      !blkPix || !lnkPix || !regPix || !rotlPix || !fliphPix || !flipvPix ||
      !p10Pix || !m10Pix) 
    FatalError("unable to create all pixmaps in CreateCtrl()\n");
  

  if (ctrlColor) XSetWindowBackground(theDisp, ctrlW, locol);
            else XSetWindowBackgroundPixmap(theDisp, ctrlW, grayTile);

  /* create doo-wahs */
  listh = LINEHIGH * NLINES;

  LSCreate(&nList, ctrlW, 10, 14+CHIGH+1, 230+16, LINEHIGH*NLINES, 
	   NLINES, dispnames, numnames, infofg, infobg, hicol, locol,
	   RedrawNList, 0, 0);
  nList.selected = 0;  /* default to first name selected */

  skip =  ((double) (nList.h - BUTTH)) / 6.0;
  topskip = nList.y;

/* expressions for positioning right-side buttons */
#define R_BW1 77
#define R_BW2 161
#define R_BX0 268
#define R_BX1 352
#define R_BY0 (topskip)
#define R_BY1 (topskip + (int)(1*skip))
#define R_BY2 (topskip + (int)(2*skip))
#define R_BY3 (topskip + (int)(3*skip))
#define R_BY4 (topskip + (int)(4*skip))
#define R_BY5 (topskip + (int)(5*skip))
#define R_BY6 (topskip + (int)(6*skip))

  BTCreate(&but[BVBROWSE], ctrlW, R_BX0, R_BY0, R_BW2, BUTTH, 
	   "Visual Schnauzer", infofg, infobg, hicol, locol);

  BTCreate(&but[BNEXT],    ctrlW, R_BX0, R_BY1, R_BW1, BUTTH,
	   "Next",  infofg, infobg, hicol, locol);
  BTCreate(&but[BPREV],    ctrlW, R_BX1, R_BY1, R_BW1, BUTTH, 
	   "Prev",  infofg, infobg, hicol, locol);

  BTCreate(&but[BLOAD],    ctrlW, R_BX0, R_BY2, R_BW1, BUTTH,
	   "Load",  infofg, infobg, hicol, locol);
  BTCreate(&but[BSAVE],    ctrlW, R_BX1, R_BY2, R_BW1, BUTTH,
	   "Save",  infofg, infobg, hicol, locol);

  BTCreate(&but[BGAMMA],   ctrlW, R_BX0, R_BY3, R_BW1, BUTTH,
	   "ColEdit", infofg, infobg, hicol, locol);
  BTCreate(&but[BINFO],    ctrlW, R_BX1, R_BY3, R_BW1, BUTTH,
	   "Info", infofg, infobg, hicol, locol);

  BTCreate(&but[BLICENSE], ctrlW, R_BX0, R_BY4, R_BW1, BUTTH,
	   "License", infofg, infobg, hicol, locol);
  BTCreate(&but[BTXVIEW],  ctrlW, R_BX1, R_BY4, R_BW1, BUTTH,
	   "TextView",infofg, infobg, hicol, locol);

  BTCreate(&but[BCMTVIEW], ctrlW, R_BX0, R_BY5, R_BW1, BUTTH,
	   "Comments", infofg, infobg, hicol, locol);
  BTCreate(&but[BGRAB],    ctrlW, R_BX1, R_BY5, R_BW1, BUTTH,
	   "Grab",     infofg, infobg, hicol, locol);

  BTCreate(&but[BDELETE],  ctrlW, R_BX0, R_BY6, R_BW1, BUTTH,
	   "Delete",   infofg, infobg, hicol, locol);
  BTCreate(&but[BQUIT],    ctrlW, R_BX1, R_BY6, R_BW1, BUTTH,
	   "Quit",     infofg, infobg, hicol, locol);



#define BXSPACE (BUTTW+1)
#define BYSPACE (BUTTH+1)

  ptop = CTRLHIGH - (3*BYSPACE + 5 + 4);

#define BX0 ((CTRLWIDE - (BXSPACE*6))/2)
#define BX1 (BX0 + BXSPACE)
#define BX2 (BX0 + BXSPACE*2)
#define BX3 (BX0 + BXSPACE*3)
#define BX4 (BX0 + BXSPACE*4)
#define BX5 (BX0 + BXSPACE*5)
#define BY0 (ptop+5)
#define BY1 (BY0 + BYSPACE)
#define BY2 (BY0 + BYSPACE*2)

  butrect.x = BX0-1;  butrect.y = BY0-1;
  butrect.width = 6*BXSPACE + 1;
  butrect.height = 3*BYSPACE + 1;

  BTCreate(&but[BCROP],   ctrlW,BX0,BY0,BUTTW,BUTTH,"Crop",     
	   infofg,infobg,hicol,locol);
  BTCreate(&but[BUNCROP], ctrlW,BX0,BY1,BUTTW,BUTTH,"UnCrop",   
	   infofg,infobg,hicol,locol);
  BTCreate(&but[BACROP],  ctrlW,BX0,BY2,BUTTW,BUTTH,"AutoCrop", 
	   infofg,infobg,hicol,locol);

  BTCreate(&but[BNORM],   ctrlW,BX1,BY0,BUTTW,BUTTH,"Normal",   
	   infofg,infobg,hicol,locol);
  BTCreate(&but[BMAX],    ctrlW,BX1,BY1,BUTTW,BUTTH,"Max Size", 
	   infofg,infobg,hicol,locol);
  BTCreate(&but[BMAXPECT],ctrlW,BX1,BY2,BUTTW,BUTTH,"Maxpect",  
	   infofg,infobg,hicol,locol);

  BTCreate(&but[BUP2],    ctrlW,BX2,BY0,BUTTW,BUTTH,"Dbl Size", 
	   infofg,infobg,hicol,locol);
  BTCreate(&but[BDN2],    ctrlW,BX2,BY1,BUTTW,BUTTH,"Half Size",
	   infofg,infobg,hicol,locol);
  BTCreate(&but[BSETSIZE],   ctrlW,BX2,BY2,BUTTW,BUTTH,"SetSize",
	   infofg,infobg,hicol,locol);


  BTCreate(&but[BASPECT], ctrlW,BX3,BY0,BUTTW,BUTTH,"Aspect",   
	   infofg,infobg,hicol,locol);
  BTCreate(&but[B4BY3],   ctrlW,BX3,BY1,BUTTW,BUTTH,"4x3",      
	   infofg,infobg,hicol,locol);
  BTCreate(&but[BINTSIZE],ctrlW,BX3,BY2,BUTTW,BUTTH,"IntExpnd", 
	   infofg,infobg,hicol,locol);

  BTCreate(&but[BRAW],    ctrlW,BX4,BY0,BUTTW,BUTTH,"Raw",	
	   infofg,infobg,hicol,locol);
  BTCreate(&but[BDITH],   ctrlW,BX4,BY1,BUTTW,BUTTH,"Dith",   
	   infofg,infobg,hicol,locol);
  BTCreate(&but[BSMOOTH], ctrlW,BX4,BY2,BUTTW,BUTTH,"Smooth",   
	   infofg,infobg,hicol,locol);

  BTCreate(&but[BDN10],   ctrlW,BX5,BY0,BUTTW/2,BUTTH,"",       
	   infofg,infobg,hicol,locol);
  BTCreate(&but[BUP10],   ctrlW,BX5+BUTTW/2 + 1,BY0,BUTTW/2,BUTTH,
	   "",     infofg,infobg,hicol,locol);

  BTCreate(&but[BROTL],    ctrlW,BX5,BY1,BUTTW/2,BUTTH,
	   "", infofg,infobg,hicol,locol);
  BTCreate(&but[BROTR],    ctrlW,BX5+BUTTW/2 + 1,BY1,BUTTW/2,BUTTH,
	   "", infofg,infobg,hicol,locol);

  BTCreate(&but[BFLIPH],    ctrlW,BX5,BY2,BUTTW/2,BUTTH,
	   "", infofg,infobg,hicol,locol);
  BTCreate(&but[BFLIPV],    ctrlW,BX5+BUTTW/2 + 1,BY2,BUTTW/2,BUTTH,
	   "", infofg,infobg,hicol,locol);


  but[BUP10].pix = p10Pix;
  but[BUP10].pw  = p10_width;
  but[BUP10].ph  = p10_height;

  but[BDN10].pix = m10Pix;
  but[BDN10].pw  = m10_width;
  but[BDN10].ph  = m10_height;

  but[BROTL].pix = rotlPix;
  but[BROTL].pw  = h_rotl_width;
  but[BROTL].ph  = h_rotl_height;

  but[BROTR].pix = rotrPix;
  but[BROTR].pw  = h_rotr_width;
  but[BROTR].ph  = h_rotr_height;

  but[BFLIPH].pix = fliphPix;
  but[BFLIPH].pw  = fliph_width;
  but[BFLIPH].ph  = fliph_height;

  but[BFLIPV].pix = flipvPix;
  but[BFLIPV].pw  = flipv_width;
  but[BFLIPV].ph  = flipv_height;

  if (numnames<1) BTSetActive(&but[BDELETE],0);
  if (numnames<1) BTSetActive(&but[BTXVIEW],0);

  XMapSubwindows(theDisp, ctrlW);

  /* have to create dispMB after XMapSubWindows, as we *don't* want the popup
     mapped */

  MBCreate(&dispMB, ctrlW, 125, 3, 100, 19,
	   "Display", dispMList, DMB_MAX, infofg, infobg,hicol,locol);
  if (!useroot) dispMode = DMB_WINDOW;
           else dispMode = rootMode + (DMB_ROOT - RM_NORMAL);
  dispMB.flags[dispMode] = 1;

  if (allocMode == AM_READWRITE) dispMB.flags[DMB_COLRW] = 1;

  dispMB.flags[colorMapMode + DMB_COLNORM - CM_NORMAL] = 1;


  MBCreate(&conv24MB, ctrlW, 125 + 100 + 1, 3, 100, 19,
	   "24/8 Bit", conv24MList, CONV24_MAX, 
	   infofg, infobg, hicol,locol);
  conv24MB.flags[conv24] = 1;  /* check 'conv24' */


  MBCreate(&algMB, ctrlW, 125 + 2*(100 + 1), 3, 100, 19, 
	   "Algorithms", algMList, ALG_MAX, infofg, infobg, hicol,locol);
}
  

/***************************************************/
void CtrlBox(vis)
int vis;
{
  if (vis) XMapRaised(theDisp, ctrlW);  
  else     XUnmapWindow(theDisp, ctrlW);

  ctrlUp = vis;
}


/***************************************************/
void RedrawCtrl(x,y,w,h)
int x,y,w,h;
{
  int i;
  XRectangle xr;

#ifdef CLIPRECT
  xr.x = x;  xr.y = y;  xr.width = w;  xr.height = h;
  XSetClipRectangles(theDisp, theGC, 0,0, &xr, 1, Unsorted);
#endif

  DrawCtrlNumFiles();

  XSetForeground(theDisp,theGC,infofg);
  XDrawRectangles(theDisp, ctrlW, theGC, &butrect, 1);

  for (i=0; i<NBUTTS; i++)
    BTRedraw(&but[i]);

  MBRedraw(&dispMB);
  MBRedraw(&conv24MB);
  MBRedraw(&algMB);

  DrawCtrlStr();

#ifdef CLIPRECT
  XSetClipMask(theDisp, theGC, None);
#endif
}


/***************************************************/
void DrawCtrlNumFiles()
{
  int y1;
  char foo[40];

  y1 = 3;

  XSetForeground(theDisp, theGC, infofg);
  XSetBackground(theDisp, theGC, infobg);

  sprintf(foo, "%d file%s", numnames, (numnames==1) ? "" : "s");
    
  XSetForeground(theDisp, theGC, infobg);
  XFillRectangle(theDisp,ctrlW, theGC, 10+1,y1+1,StringWidth(foo)+6,CHIGH+5);

  XSetForeground(theDisp,theGC,infofg);
  XDrawRectangle(theDisp,ctrlW, theGC, 10,y1,StringWidth(foo)+7,CHIGH+6);

  Draw3dRect(ctrlW, 10+1,y1+1,StringWidth(foo)+5,CHIGH+4, R3D_IN, 2,
	     hicol, locol, infobg);

  XSetForeground(theDisp,theGC,infofg);
  XDrawString(theDisp, ctrlW, theGC, 10+3, y1+ASCENT+4,
	      foo, strlen(foo));
}


/***************************************************/
void DrawCtrlStr()
{
  int   y;
  char *st,*st1;

  y = ptop - (CHIGH + 4)*2 - 2;
  st  = GetISTR(ISTR_INFO);
  st1 = GetISTR(ISTR_WARNING);

  XSetForeground(theDisp, theGC, infobg);
  XFillRectangle(theDisp, ctrlW, theGC, 0, y+1, CTRLWIDE, (CHIGH+4)*2+1);

  XSetForeground(theDisp, theGC, infofg);
  XDrawLine(theDisp, ctrlW, theGC, 0, y,   CTRLWIDE, y);
  XDrawLine(theDisp, ctrlW, theGC, 0, y+CHIGH+4, CTRLWIDE, y+CHIGH+4);
  XDrawLine(theDisp, ctrlW, theGC, 0, y+(CHIGH+4)*2, CTRLWIDE, y+(CHIGH+4)*2);

  if (ctrlColor) {
    XSetForeground(theDisp, theGC, locol);
    XDrawLine(theDisp, ctrlW, theGC, 0, y+1,   CTRLWIDE, y+1);
    XDrawLine(theDisp, ctrlW, theGC, 0, y+CHIGH+5, CTRLWIDE, y+CHIGH+5);
    XDrawLine(theDisp, ctrlW, theGC, 0, y+(CHIGH+4)*2+1, 
	      CTRLWIDE, y+(CHIGH+4)*2+1);
  }

  if (ctrlColor) XSetForeground(theDisp, theGC, hicol);
  XDrawLine(theDisp, ctrlW, theGC, 0, y+2, CTRLWIDE, y+2);
  XDrawLine(theDisp, ctrlW, theGC, 0, y+CHIGH+6, CTRLWIDE, y+CHIGH+6);
  if (ctrlColor) XSetForeground(theDisp, theGC, infobg);
  XDrawLine(theDisp, ctrlW, theGC, 0, ptop, CTRLWIDE, ptop);

  XSetForeground(theDisp, theGC, infofg);
  XDrawString(theDisp, ctrlW, theGC, 10, y+ASCENT+3,       st, strlen(st));
  XDrawString(theDisp, ctrlW, theGC, 10, y+ASCENT+CHIGH+7, st1, strlen(st1));
}


/***************************************************/
int ClickCtrl(x,y)
int x,y;
{
  BUTT *bp;
  int   i;

  for (i=0; i<NBUTTS; i++) {
    bp = &but[i];
    if (PTINRECT(x, y, bp->x, bp->y, bp->w, bp->h)) break;
  }

  if (i<NBUTTS) {                   /* found one */
    if (BTTrack(bp)) return (i);    /* and it was clicked */
  }

  return -1;
}



/***************************************************/
void ScrollToCurrent(lst)
LIST *lst;
{
  /* called when selected item on list is changed.  Makes the selected 
     item visible.  If it already is, nothing happens.  Otherwise, it
     attempts to scroll so that the selection appears in the middle of 
     the list window */

  int halfway;

  if (lst->selected < 0) return;  /* no selection, do nothing */

  if (lst->selected > lst->scrl.val && 
      lst->selected <  lst->scrl.val + lst->nlines-1) LSRedraw(lst, 0);
  else {
    halfway = (lst->nlines)/2;   /* offset to the halfway pt. of the list */
    if (!SCSetVal(&lst->scrl, lst->selected - halfway)) LSRedraw(lst, 0);
  }
}


/***************************************************/
static void RedrawNList(delta, sptr)
     int delta;
     SCRL *sptr;
{
  LSRedraw(&nList, delta);
}




/***************** LIST STUFF *********************/

/***************************************************/
void LSCreate(lp, win, x, y, w, h, nlines, strlist, nstr, fg, bg, hi, lo,
	      fptr, typ, donly)
LIST         *lp;
Window        win;
int           x,y,w,h,nlines,nstr,typ,donly;
unsigned long fg, bg, hi, lo;
char        **strlist;    /* a pointer to a list of strings */

#ifdef __STDC__
  void        (*fptr)(int,SCRL *);
#else
  void        (*fptr)();
#endif
{
  if (ctrlColor) h += 4;

  lp->win = XCreateSimpleWindow(theDisp,win,x,y,w,h,1,fg,bg);
  if (!lp->win) FatalError("can't create list window!");

  lp->x = x;    lp->y = y;   
  lp->w = w;    lp->h = h;
  lp->fg = fg;  lp->bg = bg;
  lp->hi = hi;  lp->lo = lo;
  lp->str      = strlist;
  lp->nstr     = nstr;
  lp->selected = -1;   /* no initial selection */
  lp->nlines   = nlines;
  lp->filetypes= typ;
  lp->dirsonly = donly;

  XSelectInput(theDisp, lp->win, ExposureMask | ButtonPressMask);

  SCCreate(&lp->scrl, lp->win, w-20, -1, 1, h, 0, 
	   nstr-nlines, 0, nlines-1, fg, bg, hi, lo, fptr);

  XMapSubwindows(theDisp, lp->win);
}



/***************************************************/
void LSChangeData(lp, strlist, nstr)
LIST         *lp;
char        **strlist;
int           nstr;
{
  /* tries to keep list selection and scrollbar in same place, if possible */

  lp->str = strlist;
  lp->nstr = nstr;
  if (lp->selected >= nstr) lp->selected = -1;

  RANGE(lp->scrl.val, 0, nstr - lp->nlines);
  SCSetRange(&lp->scrl, 0, nstr - lp->nlines, lp->scrl.val, lp->nlines-1);
}


/***************************************************/
void LSNewData(lp, strlist, nstr)
LIST         *lp;
char        **strlist;
int           nstr;
{
  lp->str = strlist;
  lp->nstr = nstr;
  lp->selected = -1;   /* no initial selection */
  SCSetRange(&lp->scrl, 0, nstr - lp->nlines, 0, lp->nlines-1);
}


/***************************************************/
static void ls3d(lp)
LIST *lp;
{
  /* redraws lists 3d-effect, which can be trounced by drawSel() */
  Draw3dRect(lp->win, 0, 0, lp->w-1, lp->h-1, R3D_IN, 2, 
	     lp->hi, lp->lo, lp->bg);
}


/***************************************************/
static void drawSel(lp,j)
LIST *lp;
int j;
{
  int i, inactive, x0,y0,wide, selected;
  unsigned long fg, bg;

  x0 = 0;  y0 = 0;  wide = lp->w;
  if (ctrlColor) { x0 = y0 = 2;  wide -= 6; }

  inactive = INACTIVE(lp,j);

  i = j - lp->scrl.val;
  if (i<0 || i>=lp->nlines) return;  /* off screen */

  selected = (j == lp->selected && !inactive && j<lp->nstr);
  if (selected) {  /* inverse colors */
    if (ctrlColor) { fg = lp->fg;  bg = lp->lo; }
              else { fg = lp->bg;  bg = lp->fg; }
  }
  else { fg = lp->fg;  bg = lp->bg; }

  XSetForeground(theDisp, theGC, bg);
  XFillRectangle(theDisp, lp->win, theGC, x0, y0+i*LINEHIGH, wide+1, LINEHIGH);

  if (j>=0 && j<lp->nstr) {   /* only draw string if valid */
    XSetForeground(theDisp, theGC, fg);
    XSetBackground(theDisp, theGC, bg);

    if (!lp->filetypes) 
      XDrawString(theDisp, lp->win, theGC, x0+3, y0+i*LINEHIGH + ASCENT + 1, 
		  lp->str[j], strlen(lp->str[j]));
    else {
      int ypos = y0 + i*LINEHIGH + (LINEHIGH - i_fifo_height)/2;

      if (lp->str[j][0] == C_FIFO) 
	XCopyPlane(theDisp, fifoPix, lp->win, theGC, 0, 0,
		   i_fifo_width, i_fifo_height, x0+3, ypos, 1L);

      else if (lp->str[j][0] == C_CHR) 
	XCopyPlane(theDisp, chrPix, lp->win, theGC, 0, 0,
		   i_chr_width, i_chr_height, x0+3, ypos, 1L);

      else if (lp->str[j][0] == C_DIR) 
	XCopyPlane(theDisp, dirPix, lp->win, theGC, 0, 0,
		   i_dir_width, i_dir_height, x0+3, ypos, 1L);

      else if (lp->str[j][0] == C_BLK) 
	XCopyPlane(theDisp, blkPix, lp->win, theGC, 0, 0,
		   i_blk_width, i_blk_height, x0+3, ypos, 1L);

      else if (lp->str[j][0] == C_LNK) 
	XCopyPlane(theDisp, lnkPix, lp->win, theGC, 0, 0,
		   i_lnk_width, i_lnk_height, x0+3, ypos, 1L);

      else if (lp->str[j][0] == C_SOCK) 
	XCopyPlane(theDisp, sockPix, lp->win, theGC, 0, 0,
		   i_sock_width, i_sock_height, x0+3, ypos, 1L);

      else if (lp->str[j][0] == C_EXE) 
	XCopyPlane(theDisp, exePix, lp->win, theGC, 0, 0,
		   i_exe_width, i_exe_height, x0+3, ypos, 1L);

      else  /* lp->str[j][0] == C_REG */
	XCopyPlane(theDisp, regPix, lp->win, theGC, 0, 0,
		   i_reg_width, i_reg_height, x0+3, ypos, 1L);


      XDrawString(theDisp, lp->win, theGC, x0+3 + i_fifo_width + 3, 
		  y0+i*LINEHIGH + ASCENT + 1, 
		  lp->str[j]+1, strlen(lp->str[j]+1));
    }
  }
}


/***************************************************/
void LSRedraw(lp, delta)
LIST *lp;
int   delta;
{
  int  i;

  for (i = lp->scrl.val; i < lp->scrl.val + lp->nlines; i++) 
    drawSel(lp,i);
  ls3d(lp);
}


/***************************************************/
int LSClick(lp,ev)
LIST *lp;
XButtonEvent *ev;
{
  /* returns '-1' normally.  returns 0 -> numnames-1 for a goto */

  Window       rW, cW;
  int          rx, ry, x, y, sel, oldsel, y0, high;
  unsigned int mask;
  static Time  lasttime=0;
  static int   lastsel = -1;

  y0   = (ctrlColor) ? 2 : 0;
  high = (ctrlColor) ? lp->h - 4 : lp->h;

  x = ev->x;  y = ev->y;
  sel = lp->scrl.val + (y-y0)/LINEHIGH;
  if (sel >= lp->nstr) sel = lp->selected;

  /* see if it's a double click */
  if (ev->time - lasttime < DBLCLKTIME && sel==lastsel 
      && (lp->scrl.val + (y-y0)/LINEHIGH) < lp->nstr
      && !INACTIVE(lp,sel)) {
    return (sel);
  }

  lasttime = ev->time;  lastsel = sel;

  /* if not clicked on selected, turn off selected and select new one */
  if (sel != lp->selected) {
    oldsel = lp->selected;
    lp->selected = sel;
    drawSel(lp,sel);  drawSel(lp,oldsel);
    ls3d(lp);
    XFlush(theDisp);
  }

  while (XQueryPointer(theDisp,lp->win,&rW,&cW,&rx,&ry,&x,&y,&mask)) {
    if (!(mask & Button1Mask)) break;    /* button released */

    if (y<y0) { /* scroll up in list */ 
      if (lp->scrl.val > lp->scrl.min) {
	lp->selected = lp->scrl.val - 1;
	SCSetVal(&lp->scrl, lp->scrl.val - 1);
	Timer(100);
      }
    }

    else if (y>high) { /* scroll down in list */
      if (lp->scrl.val < lp->scrl.max) {
	lp->selected = lp->scrl.val + lp->nlines;
	if (lp->selected >= lp->nstr) lp->selected = lp->nstr - 1;
	SCSetVal(&lp->scrl, lp->scrl.val + 1);
	Timer(100);
      }
    }

    else {
      sel = lp->scrl.val + (y-y0)/LINEHIGH;
      if (sel >= lp->nstr) sel = lp->nstr - 1;

      if (sel != lp->selected && sel >= lp->scrl.val &&
	  sel < lp->scrl.val + lp->nlines) {  
	/* dragged to another on current page */
	oldsel = lp->selected;
	lp->selected = sel;
	drawSel(lp, sel);  drawSel(lp, oldsel);
	ls3d(lp);
	XFlush(theDisp);
      }
    }
  }

  return(-1);
}



/***************************************************/
void LSKey(lp, key)
LIST         *lp;
int           key;
{
  if      (key==LS_PAGEUP)   SCSetVal(&lp->scrl,lp->scrl.val - (lp->nlines-1));
  else if (key==LS_PAGEDOWN) SCSetVal(&lp->scrl,lp->scrl.val + (lp->nlines-1));
  else if (key==LS_HOME)     SCSetVal(&lp->scrl,lp->scrl.min);
  else if (key==LS_END)      SCSetVal(&lp->scrl,lp->scrl.max);

  else if (key==LS_LINEUP)   {
    /* if the selected item visible, but not the top line */
    if (lp->selected > lp->scrl.val && 
	lp->selected <= lp->scrl.val + lp->nlines - 1) {
      /* then just move it */
      lp->selected--;
      drawSel(lp, lp->selected);  drawSel(lp, lp->selected+1);
      ls3d(lp);
    }

    /* if it's the top line... */
    else if (lp->selected == lp->scrl.val) {
      if (lp->selected > 0) {
	lp->selected--;
	SCSetVal(&lp->scrl, lp->selected);
      }
    }

    /* if it's not visible, put it on the bottom line */
    else {
      lp->selected = lp->scrl.val + lp->nlines - 1;
      if (lp->selected >= lp->nstr) lp->selected = lp->nstr - 1;
      drawSel(lp, lp->selected);
      ls3d(lp);
    }
  }
    
  else if (key==LS_LINEDOWN)   {
    /* if the selected item visible, but not the bottom line */
    if (lp->selected >= lp->scrl.val && 
	lp->selected < lp->scrl.val + lp->nlines - 1) {
      if (lp->selected < lp->nstr-1) {
	/* then just move it */
	lp->selected++;
	drawSel(lp, lp->selected);  drawSel(lp, lp->selected-1);
	ls3d(lp);
      }
    }

    /* if it's the bottom line... */
    else if (lp->selected == lp->scrl.val + lp->nlines - 1) {
      if (lp->selected < lp->nstr-1) {
	lp->selected++;
	SCSetVal(&lp->scrl, lp->scrl.val+1);
      }
    }

    /* if it's not visible, put it on the top line */
    else {
      lp->selected = lp->scrl.val;
      drawSel(lp, lp->selected);
      ls3d(lp);
    }
  }
}


