/*
 * xvroot.c - '-root' related code for XV
 *
 *  Author:    John Bradley, University of Pennsylvania
 *                (bradley@cis.upenn.edu)
 *
 *  Contains:
 *            MakeRootPic()
 *            ClearRoot()
 *            SaveRootInfo()
 *            KillOldRootInfo()
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


/* local function pre-definitions */
#ifdef __STDC__
static void killRootPix(void);
#else
static void killRootPix();
#endif


/***********************************/
void MakeRootPic()
{
  /* called after 'epic' has been generated (if we're using root).  
     creates the XImage and the pixmap, sets the root to the new
     pixmap, and refreshes the display */

  Pixmap tmpPix;
  int    i, j, k, rpixw, rpixh, rmode;

  killRootPix();

  rmode = rootMode;
  /* if eWIDE,eHIGH == dispWIDE,dispHIGH just use 'normal' mode to save mem */
  if (rmode>=RM_CENTER && eWIDE==dispWIDE && eHIGH==dispHIGH) rmode=RM_NORMAL;

  /* determine how big tmpPix should be based on rootMode */
  switch (rmode) {
  case RM_NORMAL:
  case RM_CENTILE:
  case RM_TILE:    rpixw = eWIDE;    rpixh = eHIGH;    break;
  case RM_MIRROR:
  case RM_IMIRROR: rpixw = 2*eWIDE;  rpixh = 2*eHIGH;  break;
  case RM_CSOLID:
  case RM_CWARP:
  case RM_CBRICK:  rpixw = dispWIDE; rpixh = dispHIGH; break;

  case 8:
  case 9:          rpixw = dispWIDE; rpixh = dispHIGH;  break;

  default:         rpixw = eWIDE;    rpixh = eHIGH;    break;
  }
  if (nolimits) { RANGE(rpixw, 1, maxWIDE);  RANGE(rpixh, 1, maxHIGH); }
           else { RANGE(rpixw, 1, dispWIDE);  RANGE(rpixh, 1, dispHIGH); }

  /* create tmpPix */
  xerrcode = 0;
  tmpPix = XCreatePixmap(theDisp, mainW, rpixw, rpixh, dispDEEP);
  XSync(theDisp, False);
  if (xerrcode || !tmpPix) {
    ErrPopUp("Insufficient memory in X server to build root pixmap.",
	     "\nDarn!");
    return;
  }


  if (rmode == RM_NORMAL || rmode == RM_TILE) {
    XPutImage(theDisp, tmpPix, theGC, theImage, 0,0, 0,0, eWIDE, eHIGH);
  }

  else if (rmode == RM_MIRROR || rmode == RM_IMIRROR) {
    /* quadrant 2 */
    XPutImage(theDisp, tmpPix, theGC, theImage, 0,0, 0,0, eWIDE, eHIGH);
    if (epic == NULL) FatalError("epic == NULL in RM_MIRROR code...\n");

    /* quadrant 1 */
    FlipPic(epic, eWIDE, eHIGH, 0);   /* flip horizontally */
    CreateXImage();
    XPutImage(theDisp, tmpPix, theGC, theImage, 0,0, eWIDE,0, eWIDE, eHIGH);

    /* quadrant 4 */
    FlipPic(epic, eWIDE, eHIGH, 1);   /* flip vertically */
    CreateXImage();
    XPutImage(theDisp, tmpPix, theGC, theImage, 0,0, eWIDE,eHIGH, eWIDE,eHIGH);

    /* quadrant 3 */
    FlipPic(epic, eWIDE, eHIGH, 0);   /* flip horizontally */
    CreateXImage();
    XPutImage(theDisp, tmpPix, theGC, theImage, 0,0, 0,eHIGH, eWIDE,eHIGH);

    FlipPic(epic, eWIDE, eHIGH, 1);   /* flip vertically  (back to orig) */
    CreateXImage();                   /* put back to original state */
  }


  else if (rmode == RM_CENTER || rmode == RM_CENTILE || rmode == RM_CSOLID ||
	   rmode == RM_CWARP || rmode == RM_CBRICK) {
    /* do some stuff to set up the border around the picture */

    if (rmode != RM_CENTILE) {
      XSetForeground(theDisp, theGC, rootbg);
      XFillRectangle(theDisp, tmpPix, theGC, 0,0, dispWIDE, dispHIGH);
    }

    if (rmode == RM_CENTILE) {    /* hagan-style tiling */
      int x, y, w, h, ax, ay, w1, h1, offx, offy;

      w = eWIDE;  h = eHIGH;

      /* compute anchor pt (top-left coords of top-left-most pic) */
      ax = (dispWIDE-w)/2;  ay = (dispHIGH-h)/2;
      while (ax>0) ax = ax - w;
      while (ay>0) ay = ay - h;

      for (i=ay; i < (int) eHIGH; i+=h) {
	for (j=ax; j < (int) eWIDE; j+=w) {
	  /* if image goes off tmpPix, only draw subimage */
	  
	  x = j;  y = i;  w1 = w;  h1 = h;  offx = offy = 0;
	  if (x<0)           { offx = -x;  w1 -= offx;  x = 0; }
	  if (x+w1>eWIDE) { w1 = (eWIDE-x); }

	  if (y<0)           { offy = -y;  h1 -= offy;  y = 0; }
	  if (y+h1>eHIGH)    { h1 = (eHIGH-y); }
	  
	  XPutImage(theDisp, tmpPix, theGC, theImage, offx, offy, 
		    x, y, w1, h1);
	}
      }
    }

    else if (rmode == RM_CSOLID) { }

    else if (rmode == RM_CWARP) {          /* warp effect */
      XSetForeground(theDisp, theGC, rootfg);
      for (i=0; i<=dispWIDE; i+=8) 
	XDrawLine(theDisp, tmpPix, theGC, i, 0, dispWIDE-i, dispHIGH);
      for (i=0; i<=dispHIGH; i+=8)
	XDrawLine(theDisp, tmpPix, theGC, 0, i, dispWIDE, dispHIGH-i);
    }

    else if (rmode == RM_CBRICK) {         /* brick effect */
      XSetForeground(theDisp, theGC, rootfg);
      for (i=k=0; i<dispHIGH; i+=20,k++) {
	XDrawLine(theDisp, tmpPix, theGC, 0, i, dispWIDE, i);
	for (j=(k&1) * 20 + 10; j<dispWIDE; j+=40) 
	  XDrawLine(theDisp, tmpPix, theGC, j,i,j,i+20);
      }
    }


    /* draw the image centered on top of the background */
    if (rmode != RM_CENTILE) 
      XPutImage(theDisp, tmpPix, theGC, theImage, 0,0, 
		(dispWIDE-eWIDE)/2, (dispHIGH-eHIGH)/2, eWIDE, eHIGH);
  }


  else if (rmode == RM_ECENTER || rmode == RM_ECMIRR) {
    int fliph, flipv;

    fliph = 0;  flipv = 0;

    if (dispWIDE == eWIDE) {
      /* horizontal center line */
      int y, ay;
      
      y = eHIGH - ((dispHIGH/2)%eHIGH); /* Starting point in picture to copy */
      ay = 0;    /* Vertical anchor point */
      while (ay < dispHIGH) {
	XPutImage(theDisp, tmpPix, theGC, theImage, 0,y,
		  0,ay, eWIDE, eHIGH);
	ay += eHIGH - y;
	y = 0;
	if (rmode == RM_ECMIRR) {
	  FlipPic(epic, eWIDE, eHIGH, 1);   flipv = !flipv;
	  CreateXImage();
	}
      }
    }
    else if (dispHIGH == eHIGH) {
      /* vertical centerline */
      int x, ax;
      
      x = eWIDE - ((dispWIDE/2)%eWIDE); /* Starting point in picture to copy */
      ax = 0;    /* Horizontal anchor point */
      while (ax < dispWIDE) {
	XPutImage(theDisp, tmpPix, theGC, theImage, x,0,
		  ax,0, eWIDE, eHIGH);
	ax += eWIDE - x;
	x = 0;
	if (rmode == RM_ECMIRR) {
	  FlipPic(epic, eWIDE, eHIGH, 0);   fliph = !fliph;
	  CreateXImage();
	}
      }
    }
    else {
      /* vertical and horizontal centerlines */
      int x,y, ax,ay;
      
      y = eHIGH - ((dispHIGH/2)%eHIGH); /* Starting point in picture to copy */
      ay = 0;    /* Vertical anchor point */
      
      while (ay < dispHIGH) {
	x = eWIDE - ((dispWIDE/2)%eWIDE);/* Starting point in picture to cpy */
	ax = 0;    /* Horizontal anchor point */
	while (ax < dispWIDE) {
	  XPutImage(theDisp, tmpPix, theGC, theImage, x,y,
		    ax,ay, eWIDE, eHIGH);
	  if (rmode == RM_ECMIRR) {
	    FlipPic(epic, eWIDE, eHIGH, 0);  fliph = !fliph;
	    CreateXImage();
	  }
	  ax += eWIDE - x;
	  x = 0;
	}
	if (rmode == RM_ECMIRR) {
	  FlipPic(epic, eWIDE, eHIGH, 1);   flipv = !flipv;
	  if (fliph) {   /* leftmost image is always non-hflipped */
	    FlipPic(epic, eWIDE, eHIGH, 0);   fliph = !fliph;
	  }
	  CreateXImage();
	}
	ay += eHIGH - y;
	y = 0;
      }
    }

    /* put epic back to normal */
    if (fliph) FlipPic(epic, eWIDE, eHIGH, 0);
    if (flipv) FlipPic(epic, eWIDE, eHIGH, 1);
  }


  XSetWindowBackgroundPixmap(theDisp, mainW, tmpPix);
  XFreePixmap(theDisp, tmpPix);

  XClearWindow(theDisp, mainW);
}



/************************************************************************/
void ClearRoot()
{
  killRootPix();
  XClearWindow(theDisp, vrootW);
  XFlush(theDisp);
}


/************************************************************************/
static void killRootPix()
{
  Pixmap pix, bitmap;
  GC gc;
  XGCValues gc_init;

  if (vrootW == rootW) {
    XSetWindowBackgroundPixmap(theDisp, rootW, None);
  }

  else {
    bitmap = XCreateBitmapFromData(theDisp, vrootW, root_weave_bits,
				   root_weave_width, root_weave_height);

    gc_init.foreground = BlackPixel(theDisp, theScreen);
    gc_init.background = WhitePixel(theDisp, theScreen);
    gc = XCreateGC(theDisp, vrootW, GCForeground|GCBackground, &gc_init);
    pix = XCreatePixmap(theDisp, vrootW, root_weave_width, 
			root_weave_height, 
			(unsigned int) DefaultDepth(theDisp, theScreen));

    XCopyPlane(theDisp, bitmap, pix, gc, 0,0, root_weave_width,
	       root_weave_height, 0,0, (unsigned long) 1);
    XSetWindowBackgroundPixmap(theDisp, vrootW, pix);

    XFreeGC(theDisp, gc);
    XFreePixmap(theDisp, bitmap);
    XFreePixmap(theDisp, pix);
  }
}



/***********************************/
void SaveRootInfo()
{
  /* called when using root window.  stores the pixmap ID used to draw the
     root window in a property.  This will be used later to free all resources
     allocated by this instantiation of xv (ie, the alloc'd colors).  These
     resources are kept alloc'ed after client exits so that rainbow effect
     is avoided */

  Atom          prop;
  static Pixmap riPix = (Pixmap) NULL;

  if ( !(theVisual->class & 1)) return;  /* no colormap to worry about */
  if (riPix) return;                     /* it's already been saved once */

  riPix = XCreatePixmap(theDisp, vrootW, 1, 1, 1);
  if (!riPix) return;   /* unable to save.  thankfully, unlikely to happen */

  prop = XInternAtom(theDisp, "_XSETROOT_ID", False);
  if (!prop) FatalError("couldn't create _XSETROOT_ID atom");

  XChangeProperty(theDisp, vrootW, prop, XA_PIXMAP, 32, PropModeReplace,
		  (unsigned char *) &riPix, 1);

  XSetCloseDownMode(theDisp, RetainPermanent);
}


/***********************************/
void KillOldRootInfo()
{
  /* get the pixmap ID from the _XSETROOT_ID property, and kill it */

  Atom           prop, type;
  int            format;
  unsigned long  length, after;
  unsigned char *data;

  prop = XInternAtom(theDisp, "_XSETROOT_ID", True);
  if (prop == None) return;    /* no old pixmap to kill */

  if (XGetWindowProperty(theDisp, vrootW, prop, 0L, 1L, True, 
			 AnyPropertyType, &type, &format, &length, 
			 &after, &data) == Success) {

    if (type==XA_PIXMAP && format==32 && length==1 && after==0 && data) {
      XKillClient(theDisp, *((Pixmap *)data));
      XDeleteProperty(theDisp, vrootW, prop);
    }

    if (data) XFree((char *) data);
  }
}



    
