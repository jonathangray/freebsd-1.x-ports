/*
 * xvmisc.c - random 'handy' routines used in XV
 *
 *  Author:    John Bradley, University of Pennsylvania
 *                (bradley@cis.upenn.edu)
 *
 *  Contains:
 *     Window CreateWindow(name, clname, geom, w, h, fg, bg, usesize)
 *     void   CenterString(win, str, x, y)
 *     void   ULineString(win, str, x, y)
 *     int    StringWidth(str)
 *     void   FakeButtonPress(bptr)
 *     void   FakeKeyPress(win, keysym);
 *     void   GenExpose(win, x, y, w, h);
 *     void   xvDestroyImage(XImage *);
 *     void   DimRect(win, x, y, w, h, bg);
 *     void   Draw3dRect(win, x,y,w,h, inout, bwidth, hicol, locol);
 *     void   SetCropString(active)
 *     void   Warning()
 *     void   FatalError(str)
 *     void   Quit(int)
 *     void   LoadFishCursors()
 *     void   WaitCursor()
 *     void   SetCursors(int)
 *     char  *BaseName(char *)
 *     void   DrawTempGauge(win, x,y,w,h, percent, fg,bg,hi,lo)
 *     void   xvbcopy(src, dst, length)
 *     int    xvbcmp (s1,  s2,  length)
 *     void   xvbzero(s, length)
 *     void   Timer(milliseconds)
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


#define NEEDSTIME
#include "xv.h"
#include "bitmaps.h"

#ifdef __STDC__
static void set_cursors(Cursor, Cursor);
#else
static void set_cursors();
#endif

static Atom atom_DELWIN = 0;
static Atom atom_PROTOCOLS = 0;

/***************************************************/
void StoreDeleteWindowProp (win)
     Window win;
{
  if (! atom_DELWIN)
    atom_DELWIN = XInternAtom (theDisp, "WM_DELETE_WINDOW", FALSE);

  /* the following fakes 'XSetWMProtocols(theDisp, win, &atom_DELWIN, 1);' */

  if (! atom_PROTOCOLS) 
    atom_PROTOCOLS = XInternAtom (theDisp, "WM_PROTOCOLS", False);

  if (atom_PROTOCOLS == None) return;

  XChangeProperty(theDisp, win, atom_PROTOCOLS, XA_ATOM, 32, 
		  PropModeReplace, (unsigned char *) &atom_DELWIN, 1);
}


/***************************************************/
Window CreateWindow(name,clname,geom,defw,defh,fg,bg,usesize)
char         *name, *clname, *geom;
int           defw,defh,usesize;
unsigned long fg, bg;
{
  Window               win;
  XSetWindowAttributes xswa;
  unsigned long        xswamask;
  XWMHints             xwmh;
  XSizeHints           hints;
  int                  i,x,y,w,h;
  XClassHint           classh;

  /* note: if usesize, w,h are extracted from geom spec, otherwise
     defw,defh are used */

  x = y = 1;
  i = XParseGeometry(geom,&x,&y, (unsigned int *) &w, (unsigned int *) &h);

  if ((i&XValue || i&YValue)) hints.flags = USPosition;  
                         else hints.flags = PPosition;
  
  if (!usesize || !(i&WidthValue))  w = defw;
  if (!usesize || !(i&HeightValue)) h = defh;

  hints.flags |= USSize;

  if (i&XValue && i&XNegative) x = dispWIDE - w - abs(x);
  if (i&YValue && i&YNegative) y = dispHIGH - h - abs(y);


#define VROOT_TRANS
#ifdef VROOT_TRANS
  if (vrootW != rootW) { /* virtual window manager running */
    int x1,y1;
    Window child;
    XTranslateCoordinates(theDisp, rootW, vrootW, x, y, &x1, &y1, &child);
    if (DEBUG) fprintf(stderr,"translate:  %d,%d -> %d,%d\n",x,y,x1,y1);
    x = x1;  y = y1;
  }
#endif

  hints.x = x;                  hints.y = y;
  hints.width = w;              hints.height = h;

  if (!usesize) {
    hints.min_width  = w;         hints.min_height = h;
    hints.max_width  = w;         hints.max_height = h;
    hints.flags |= PMaxSize | PMinSize;
  }

  xswa.background_pixel = bg;
  xswa.border_pixel     = fg;
  xswa.colormap         = theCmap;
  xswa.bit_gravity      = StaticGravity;
  xswamask = CWBackPixel | CWBorderPixel | CWColormap;
  if (!usesize) xswamask |= CWBitGravity;

  win = XCreateWindow(theDisp, rootW, x, y, w, h, 
			bwidth, dispDEEP, InputOutput,
			theVisual, xswamask, &xswa);
  if (!win) return(win);   /* leave immediately if couldn't create */


  XSetStandardProperties(theDisp, win, name, name, None, NULL, 0, &hints);

  xwmh.input = True;
  xwmh.flags = InputHint;
  if (iconPix) { xwmh.icon_pixmap = iconPix;  xwmh.flags |= IconPixmapHint; }
  XSetWMHints(theDisp, win, &xwmh);

  if (clname && strlen(clname)) {
    classh.res_name = "xv";
    classh.res_class = clname;
    XSetClassHint(theDisp, win, &classh);
    StoreDeleteWindowProp(win);
  }

  return(win);
}
  


/**************************************************/
void CenterString(win,str,x,y)
Window win;
char *str;
int x,y;
{
  XDrawString(theDisp, win, theGC, CENTERX(mfinfo, x, str),
	    CENTERY(mfinfo, y), str, strlen(str));
}

  
/**************************************************/
void ULineString(win,str,x,y)
Window win;
char *str;
int x,y;
{
  XDrawString(theDisp, win, theGC, x, y, str, strlen(str));
  XDrawLine(theDisp, win, theGC, x, y+DESCENT-1, 
	    x+StringWidth(str), y+DESCENT-1);
}

  
/**************************************************/
int StringWidth(str)
char *str;
{
  return(XTextWidth(mfinfo, str, strlen(str)));
}

  
/***********************************/
void FakeButtonPress(bp)
BUTT *bp;
{
  /* called when a button keyboard equivalent has been pressed.
     'fakes' a ButtonPress event in the button, which A) makes the button
     blink, and B) falls through to ButtonPress command dispatch code */

  Window       rW, cW;
  int          x, y, rx, ry;
  unsigned int mask;
  XButtonEvent ev;

  ev.type = ButtonPress;
  ev.send_event = True;
  ev.display = theDisp;
  ev.window = bp->win;
  ev.root = rootW;
  ev.subwindow = (Window) NULL;
  ev.x = bp->x;
  ev.y = bp->y;
  ev.state = 0;
  ev.button = Button1;
  XSendEvent(theDisp, bp->win, False, NoEventMask, (XEvent *) &ev);

  /* if button1 is pressed, loop until RELEASED to avoid probs in BTTrack */
  while (XQueryPointer(theDisp, rootW, &rW,&cW,&rx,&ry,&x,&y,&mask)) {
    if (!(mask & Button1Mask)) break;    /* button released */
  }
}


/************************************************************************/
void FakeKeyPress(win, ksym)
     Window win;
     KeySym ksym;
{
  XKeyEvent ev;

  ev.type = KeyPress;
  ev.send_event = True;
  ev.display = theDisp;
  ev.window = win;
  ev.root = rootW;
  ev.subwindow = (Window) NULL;
  ev.time = CurrentTime;
  ev.x = ev.y = ev.x_root = ev.y_root = 0;
  ev.state = 0;
  ev.keycode = XKeysymToKeycode(theDisp, ksym);
  ev.same_screen = True;
  XSendEvent(theDisp, win, False, NoEventMask, (XEvent *) &ev);
  XFlush(theDisp);
}


/***********************************/
void GenExpose(win, x, y, w, h)
     Window win;
     int x,y,w,h;
{
  /* generates an expose event on 'win' of the specified rectangle.  Unlike
     XClearArea(), it doesn't clear the rectangular region */

  XExposeEvent ev;

  ev.type = Expose;
  ev.send_event = True;
  ev.display = theDisp;
  ev.window = win;
  ev.x = x;  ev.y = y;  ev.width = w;  ev.height = h;
  ev.count = 0;
  
  XSendEvent(theDisp, win, False, NoEventMask, (XEvent *) &ev);
}    


/***********************************/
void xvDestroyImage(image)
     XImage *image;
{
  /* called in place of XDestroyImage().  Explicitly destroys *BOTH* the
     data and the structure.  XDestroyImage() doesn't seem to do this on all
     systems.  Also, can be called with a NULL image pointer */

  if (image) {
    /* free data by hand, since XDestroyImage is vague about it */
    if (image->data) free(image->data);
    image->data = NULL;
    XDestroyImage(image);
  }
}


/***********************************/
void DimRect(win, x, y, w, h, bg)
Window win;
int x,y,w,h;
u_long bg;
{
  /* stipple a rectangular region by drawing 'bg' where there's 1's 
     in the stipple pattern */

  XSetFillStyle(theDisp, theGC, FillStippled);
  XSetStipple(theDisp, theGC, dimStip);
  XSetForeground(theDisp, theGC, bg);
  XFillRectangle(theDisp,win,theGC,x,y,w,h);
  XSetFillStyle(theDisp, theGC, FillSolid);
}



/**************************************************/
void Draw3dRect(win, x,y,w,h, inout, bwidth, hi, lo, bg)
     Window   win;
     int      x,y,w,h,inout,bwidth;
     unsigned long hi, lo, bg;
{
  int i,r;

  if (ctrlColor) {
    /* draw top-left */
    if (inout == R3D_OUT) XSetForeground(theDisp, theGC, hi);
    else XSetForeground(theDisp, theGC, lo);

    for (i=0; i<bwidth; i++) {
      XDrawLine(theDisp, win, theGC, x+i, y+h-i, x+i, y+i);
      XDrawLine(theDisp, win, theGC, x+i, y+i, x+w-i, y+i);
    }
    
    /* draw bot-right */
    if (inout == R3D_OUT) XSetForeground(theDisp, theGC, lo);
    else XSetForeground(theDisp, theGC, hi);

    for (i=0; i<bwidth; i++) {
      XDrawLine(theDisp, win, theGC, x+i+1, y+h-i, x+w-i, y+h-i);
      XDrawLine(theDisp, win, theGC, x+w-i, y+h-i, x+w-i, y+i+1);
    }

    /* draw diagonals */
    XSetForeground(theDisp, theGC, bg);

    for (i=0; i<bwidth; i++) {
      XDrawPoint(theDisp, win, theGC, x+i, y+h-i);
      XDrawPoint(theDisp, win, theGC, x+w-i, y+i);
    }
  }
}
    


/**************************************************/
void SetCropString(active)
int active;
{
  /* sets the crop string in the info box to be correct.  should
     be called whenever 'but[BCROP].active', cXOFF,cYOFF,cWIDE,cHIGH 
     are changed */

  /* if 'active' there's a cropping rectangle drawn on the picture.
     the string should reflect that.  */

  if (active) {
    /* figure out current cropping rectangle in 'pic' coordinates */
    int x,y,x1,y1,w,h;
    int cx,cy,dx,dy;
    
    if (crx1<crx2) cx=crx1; else cx=crx2;
    if (cry1<cry2) cy=cry1; else cy=cry2;
    dx = abs(crx1-crx2);  dy = abs(cry1-cry2);

    x = cXOFF + (cx * cWIDE) / eWIDE;
    y = cYOFF + (cy * cHIGH) / eHIGH;
    x1 = cXOFF + ((cx+dx) * cWIDE) / eWIDE;
    y1 = cYOFF + ((cy+dy) * cHIGH) / eHIGH;
    w = (x1 - x) + 1;
    h = (y1 - y) + 1;

    if (w<1) w = 1;
    if (x+w > pWIDE) w = pWIDE - x;
    if (h<1) h = 1;
    if (y+h > pHIGH) h = pHIGH - y;

    SetISTR(ISTR_CROP, "%dx%d rectangle starting at %d,%d", w, h, x, y);
  }

  else {   /* cropping rectangle is turned off */
    if (cpic != pic)
      SetISTR(ISTR_CROP, "%dx%d rectangle starting at %d,%d", 
	      cWIDE, cHIGH, cXOFF, cYOFF);
    else
      SetISTR(ISTR_CROP, "<none>");
  }
}


/***********************************/
void Warning()
{
  char *st;

  /* give 'em time to read message */
  if (infoUp || ctrlUp || anyBrowUp) sleep(3); 
  else {
    st = GetISTR(ISTR_INFO);
    OpenAlert(st);
    sleep(3);
    CloseAlert();
  }
}
    

/***********************************/
void FatalError (identifier)
      char *identifier;
{
  fprintf(stderr, "%s: %s\n",cmd, identifier);
  Quit(-1);
}


/***********************************/
void Quit(i)
int i;
{ 
  /* called when the program exits.  frees everything explictly created
     EXCEPT allocated colors.  This is used when 'useroot' is in operation,
     as we have to keep the alloc'd colors around, but we don't want anything
     else to stay */

  if (!theDisp) exit(i);   /* called before connection opened */

  if (useroot && i==0) {   /* save the root info */
    SaveRootInfo();

    /* kill the various windows, since we're in RetainPermanent mode now */
    if (dirW)  XDestroyWindow(theDisp, dirW);
    if (infoW) XDestroyWindow(theDisp, infoW);
    if (ctrlW) XDestroyWindow(theDisp, ctrlW);
    if (gamW)  XDestroyWindow(theDisp, gamW);

    KillBrowseWindows();

    if (psW)   XDestroyWindow(theDisp, psW);

#ifdef HAVE_JPEG
    if (jpegW) XDestroyWindow(theDisp, jpegW);
#endif

#ifdef HAVE_TIFF
    if (tiffW) XDestroyWindow(theDisp, tiffW);
#endif

    /* if NOT using stdcmap for images, free stdcmap */
    if (colorMapMode != CM_STDCMAP) { 
      int j;
      for (j=0; j<stdnfcols; j++) 
	xvFreeColors(theDisp, theCmap, &stdfreecols[j], 1, 0L);
    }

    /* free browCmap, if any */
    if (browPerfect && browCmap) XFreeColormap(theDisp, browCmap);

    XFlush(theDisp);
  }

  KillPageFiles(pageBaseName, numPages);

  exit(i);
}


static Cursor flcurs, fl1curs, fmcurs, fr1curs, frcurs;

/***********************************/
void LoadFishCursors()
{
#define fc_w 16
#define fc_h 16 

  Pixmap flpix,flmpix,fmpix,fmmpix,frpix,frmpix;
  Pixmap fl1pix, fl1mpix, fr1pix, fr1mpix;
  XColor fg, bg;

  flcurs = fl1curs = fmcurs = fr1curs = frcurs = (Pixmap) NULL;

  flpix = XCreatePixmapFromBitmapData(theDisp, ctrlW, fc_left_bits,
	     fc_w, fc_h, 1, 0, 1);
  flmpix= XCreatePixmapFromBitmapData(theDisp, ctrlW, fc_leftm_bits,
	     fc_w, fc_h, 1, 0, 1);

  fl1pix = XCreatePixmapFromBitmapData(theDisp, ctrlW, fc_left1_bits,
	     fc_w, fc_h, 1, 0, 1);
  fl1mpix= XCreatePixmapFromBitmapData(theDisp, ctrlW, fc_left1m_bits,
	     fc_w, fc_h, 1, 0, 1);

  fmpix = XCreatePixmapFromBitmapData(theDisp, ctrlW, fc_mid_bits,
	     fc_w, fc_h, 1, 0, 1);
  fmmpix= XCreatePixmapFromBitmapData(theDisp, ctrlW, fc_midm_bits,
	     fc_w, fc_h, 1, 0, 1);

  fr1pix = XCreatePixmapFromBitmapData(theDisp, ctrlW, fc_right1_bits,
	     fc_w, fc_h, 1, 0, 1);
  fr1mpix = XCreatePixmapFromBitmapData(theDisp, ctrlW, fc_right1m_bits,
	     fc_w, fc_h, 1, 0, 1);

  frpix = XCreatePixmapFromBitmapData(theDisp, ctrlW, fc_right_bits,
	     fc_w, fc_h, 1, 0, 1);
  frmpix = XCreatePixmapFromBitmapData(theDisp, ctrlW, fc_rightm_bits,
	     fc_w, fc_h, 1, 0, 1);

  if (!flpix || !flmpix || !fmpix || !fmmpix || !frpix || !frmpix
      || !fl1pix || !fl1mpix || !fr1pix || !fr1mpix) return;

  fg.red = fg.green = fg.blue = 0;
  bg.red = bg.green = bg.blue = 0xffff;

  flcurs = XCreatePixmapCursor(theDisp, flpix, flmpix, &fg, &bg, 8,8);
  fl1curs= XCreatePixmapCursor(theDisp, fl1pix,fl1mpix,&fg, &bg, 8,8);
  fmcurs = XCreatePixmapCursor(theDisp, fmpix, fmmpix, &fg, &bg, 8,8);
  fr1curs= XCreatePixmapCursor(theDisp, fr1pix,fr1mpix,&fg, &bg, 8,8);
  frcurs = XCreatePixmapCursor(theDisp, frpix, frmpix, &fg, &bg, 8,8);

  if (!flcurs || !fmcurs || !frcurs || !fl1curs || !fr1curs) 
    { flcurs = fmcurs = frcurs = (Cursor) NULL; }
}

static int fishno=0;


/***********************************/
void WaitCursor()
{
  SetCursors(fishno);
  fishno = (fishno+1) % 8;
}


/***********************************/
void SetCursors(n)
int n;
{
  Cursor c;

  c = cross;
  /* if n < 0   sets normal cursor in all windows
     n = 0..6   cycles through fish cursors */

  if (n<0) {
    if (showzoomcursor) set_cursors(zoom, arrow);
                   else set_cursors(cross, arrow);
    fishno = 0;
  }

  else if (flcurs) {    /* was able to load the cursors */
    switch (n%8) {
    case 0: c = flcurs;   break;
    case 1: c = fl1curs;  break;
    case 2: c = fmcurs;   break;
    case 3: c = fr1curs;  break;
    case 4: c = frcurs;   break;
    case 5: c = fr1curs;  break;
    case 6: c = fmcurs;   break;
    case 7: c = fl1curs;  break;
    }

    set_cursors(c,c);
  }

  XFlush(theDisp);
}
  

static void set_cursors(mainc, otherc)
     Cursor mainc, otherc;
{    
  if (!useroot && mainW) XDefineCursor(theDisp, mainW, mainc);
  if (infoW) XDefineCursor(theDisp, infoW, otherc);
  if (ctrlW) XDefineCursor(theDisp, ctrlW, otherc);
  if (dirW)  XDefineCursor(theDisp, dirW, otherc);
  if (gamW)  XDefineCursor(theDisp, gamW, otherc);
  if (psW)   XDefineCursor(theDisp, psW, otherc);

  SetBrowseCursor(otherc);
  
#ifdef HAVE_JPEG
  if (jpegW) XDefineCursor(theDisp, jpegW, otherc);
#endif
  
#ifdef HAVE_TIFF
  if (tiffW) XDefineCursor(theDisp, tiffW, otherc);
#endif
}


/***************************************************/
char *BaseName(fname)
     char *fname;
{
  char *basname;

  /* given a complete path name ('/foo/bar/weenie.gif'), returns just the 
     'simple' name ('weenie.gif').  Note that it does not make a copy of
     the name, so don't be modifying it... */

  basname = rindex(fname, '/');
  if (!basname) basname = fname;
  else basname++;

  return basname;
}

  
/***************************************************/
void DrawTempGauge(win, x,y,w,h, ratio, fg,bg,hi,lo)
     Window win;
     int    x,y,w,h;
     double ratio;
     u_long fg,bg,hi,lo;
{
  /* draws a 'temprature'-style horizontal progess meter in the specified
     window, at the specified location */

  int barwide, maxwide;

  XSetForeground(theDisp, theGC, fg);
  XDrawRectangle(theDisp, win, theGC, x,y,w,h);

  if (ctrlColor) {
    Draw3dRect(win, x+1,y+1,w-2,h-2, R3D_IN, 2, hi, lo, bg);
    maxwide = w-5;
    barwide = (int) (maxwide * ratio + 0.5);

    XSetForeground(theDisp, theGC, fg);
    XFillRectangle(theDisp, win, theGC, x+3, y+3, barwide, h-5);

    XDrawLine(theDisp, win, theGC, x+3+barwide, y+h/2 + 0, x+w-3, y+h/2 + 0);

    XSetForeground(theDisp, theGC, lo);
    XDrawLine(theDisp, win, theGC, x+3+barwide, y+h/2 + 1, x+w-3, y+h/2 + 1);

    XSetForeground(theDisp, theGC, hi);
    XDrawLine(theDisp, win, theGC, x+3+barwide, y+h/2 + 2, x+w-3, y+h/2 + 2);

    XSetForeground(theDisp, theGC, bg);
    XFillRectangle(theDisp, win, theGC, x+3+barwide, y+3, 
		   (maxwide-barwide), (h/2 - 3));

    XFillRectangle(theDisp, win, theGC, x+3+barwide, y+h/2 + 3, 
		   (maxwide-barwide), ((h-3) - (h/2+3)) + 1);
  }
  else {
    maxwide = w-1;
    barwide = (int) (maxwide * ratio + 0.5);

    XSetForeground(theDisp, theGC, fg);
    XFillRectangle(theDisp, win, theGC, x+1, y+1, barwide, h-1);

    XDrawLine(theDisp, win, theGC, x+1+barwide, y+h/2, x+w-1, y+h/2);

    XSetForeground(theDisp, theGC, bg);
    XFillRectangle(theDisp, win, theGC, x+1+barwide, y+1, 
		   maxwide-barwide, h/2 - 1);

    XFillRectangle(theDisp, win, theGC, x+1+barwide, y+h/2 + 1, 
		   maxwide-barwide, ((h-1) - (h/2+1)) + 1);
  }

  XFlush(theDisp);
}
    


/***************************************************/
void XVDeletedFile(fullname)
     char *fullname;
{
  /* called whenever a file has been deleted.  Updates browser & dir windows,
     if necessary */

  BRDeletedFile(fullname);
  DIRDeletedFile(fullname);
}


/***************************************************/
void XVCreatedFile(fullname)
     char *fullname;
{
  /* called whenever a file has been deleted.  Updates browser & dir windows,
     if necessary */

  BRCreatedFile(fullname);
  DIRCreatedFile(fullname);
}


/***************************************************/
void xvbcopy(src, dst, len)
     char *src, *dst;
     int   len;
{
  /* Modern OS's (Solaris, etc.) frown upon the use of bcopy(),
   * and only want you to use memcpy().  However, memcpy() is broken,
   * in the sense that it doesn't properly handle overlapped regions
   * of memory.  This routine does, and also has its arguments in
   * the same order as bcopy() did, without using bcopy().
   */

  /* determine if the regions overlap
   *
   * 3 cases:  src=dst, src<dst, src>dst
   *
   * if src=dst, they overlap completely, but nothing needs to be moved
   * if src<dst and src+len>dst then they overlap
   * if src>dst and src<dst+len then they overlap
   */

  if (src==dst || len<=0) return;    /* nothin' to do */
  
  if (src<dst && src+len>dst) {  /* do a backward copy */
    src = src + len - 1;
    dst = dst + len - 1;
    for ( ; len>0; len--, src--, dst--) *dst = *src;
  }

  else {  /* they either overlap (src>dst) or they don't overlap */
    /* do a forward copy */
    for ( ; len>0; len--, src++, dst++) *dst = *src;
  }
}
    

/***************************************************/
int xvbcmp (s1, s2, len)
     char *s1, *s2;
     int   len;
{
  for ( ; len>0; len--, s1++, s2++) {
    if      (*s1 < *s2) return -1;
    else if (*s1 > *s2) return 1;
  }
  return 0;
}

/***************************************************/
void xvbzero(s, len)
     char *s;
     int   len;
{
  for ( ; len>0; len--) *s++ = 0;
}

/***************************************************/

static int timerdone;

/*******/
static void onalarm()
/*******/
{
  timerdone=1;
}


/*******/
void Timer(msec)   /* waits for 'n' milliseconds */
 int  msec;
/*******/
{
  long usec;

  if (msec <= 0) return;
  usec = (long) msec * 1000;


#ifdef VMS
  {
    float ftime;
    ftime = msec / 1000.0;
    lib$wait(&ftime);
    return;
  }
#endif


#ifdef sgi
  {
    float ticks_per_msec;
    long ticks;
    ticks_per_msec = (float) CLK_TCK / 1000.0;
    ticks = (long) ((float) msec * ticks_per_msec);
    sginap(ticks);
    return;
  }
#endif


#ifdef sco
  {
    int dummy;
    poll(&dummy, 0, msec);
    return;
  }
#endif


#ifdef USLEEP
  usleep(usec);  return;
#endif


#ifdef NOTIMER
  return;
#endif


#ifndef VMS          /* VMS hates multi-line #ifdef's */
#  if !defined(sgi) && !defined(sco) && !defined(USLEEP) && !defined(NOTIMER)
  {
    /* we aren't doing it any of the neat/clean/simple ways.  Do it the
       old clunky way, using setitimer()... */
    
    struct itimerval it;

    xvbzero((char *) &it, sizeof(it));
    if (usec>=1000000L) {  /* more than 1 second */
      it.it_value.tv_sec = usec / 1000000L;
      usec %= 1000000L;
    }

    it.it_value.tv_usec = usec;
    timerdone=0;
    signal(SIGALRM,onalarm);
    setitimer(ITIMER_REAL, &it, (struct itimerval *)0);
    while (1) {
      HOLD_SIG;                    /* note:  have to block, so that ALRM */
      if (timerdone) break;        /* doesn't occur between 'if (timerdone)' */
      else PAUSE_SIG;              /* and calling PAUSE_SIG */
    }

    RELEASE_SIG;                   /* turn ALRM blocking off */
    signal(SIGALRM,SIG_DFL);
  }
#  endif  /* !(sgi || sco || USLEEP || NOTIMER) */
#endif  /* !VMS */

}


