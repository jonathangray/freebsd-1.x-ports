/*
 * xvevent.c - EventLoop and support routines for XV
 *
 *  Author:    John Bradley, University of Pennsylvania
 *                (bradley@cis.upenn.edu)
 *
 *  Contains:
 *            int  EventLoop()
 *            void DrawWindow(x,y,w,h)
 *            void WResize(w,h)
 *            void WRotate()
 *            void WCrop(w,h)
 *            void WUnCrop()
 *            void GetWindowPos(&xwa)
 *            void SetWindowPos(&xwa)
 *            void InvCropRect()
 *            void SetEpicMode()
 *            int  xvErrorHandler(disp, err)
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


#define NEEDSTIME    /* for -wait handling in eventloop */

#include "xv.h"

static int rotatesLeft = 0;
static int origcropx, origcropy, origcropvalid=0;
static int canstartwait;
static int frominterrupt = 0;


/* local function pre-definitions */
#ifdef __STDC__
static void textViewCmd(void);
static void setSizeCmd(void);
static void WMaximize(void);
static void TrackCrop(int, int);
static void CropKey(int, int, int, int);
static int  Rect(int, int, int, int);
static void TrackPicValues(int, int);
static int  CheckForConfig(void);

#  ifdef _AIX
static void onInterrupt(int);
#  else
static void onInterrupt(void);
#  endif

#else
   static void textViewCmd(), WMaximize(), TrackCrop(), CropKey();
   static void setSizeCmd();
   static int  Rect(), CheckForConfig();
   static void TrackPicValues(), onInterrupt();
#endif




/****************/
int EventLoop()
/****************/
{
  XEvent event;
  int    retval,done,waiting;
  time_t orgtime, curtime;


#ifndef NOSIGNAL
  signal(SIGQUIT, onInterrupt);
#endif

  /* note: there's no special event handling if we're using the root window.
     if we're using the root window, we will recieve NO events for mainW */

  /* note: 'canstartwait' is magically turned 'true' in HandleEvent when I
     think I've finally gotten 'mainW' drawn.  It does not necessarily
     mean that any waiting is/will be done.  Also note that if we're
     using a root mode, canstartwait is instantly turned on, as we aren't
     going to be getting Expose/Configure events on the root window */

  done = retval = waiting = canstartwait = 0;

  if (useroot) canstartwait = 1;
  else if (mainW) {           /* if mainW iconified, start wait now */
    XWindowAttributes xwa;
    XSync(theDisp, False);
    if (XGetWindowAttributes(theDisp, mainW, &xwa)) {
      if (xwa.map_state != IsViewable) canstartwait = 1;
    }
  }

  while (!done) {

    if (waitsec > -1 && canstartwait && !waiting && XPending(theDisp)==0) {
      /* we wanna wait, we can wait, we haven't started waiting yet, and 
	 all pending events (ie, drawing the image the first time) 
	 have been dealt with:  START WAITING */
      time((time_t *) &orgtime);
      waiting = 1;
    }


    if ((waitsec == -1 && !polling) || XPending(theDisp)>0) {
      XNextEvent(theDisp, &event);
      retval = HandleEvent(&event,&done);
    }

    else {                      /* no events.  check wait status */
      if (polling) {
	if (CheckPoll(3)) return POLLED;
	else if (!XPending(theDisp)) sleep(1);
      }

      if (waitsec>-1 && waiting) {
	time((time_t *) &curtime);
	if (curtime - orgtime < waitsec) sleep(1);
	else {
	  if (waitloop) return NEXTLOOP;
	  else return NEXTQUIT;
	}
      }
    }
  }  /* while (!done) */

  if (!useroot && origcropvalid) WUnCrop();
  origcropvalid = 0;

  return(retval);
}



/****************/
int HandleEvent(event, donep)
XEvent *event;
int *donep;
{
  static int wasInfoUp=0, wasCtrlUp=0, wasDirUp=0, wasGamUp=0, wasPsUp=0;
  static int wasJpegUp=0, wasTiffUp=0;

  static int mainWKludge=0;  /* force first mainW expose after a mainW config
				to redraw all of mainW */

  int done=0, retval=0;

  switch (event->type) {

  case ClientMessage: {
    Atom proto, delwin;
    XClientMessageEvent *client_event = (XClientMessageEvent *) event;

    if (PUCheckEvent (event)) break;   /* event has been processed */

    proto = XInternAtom(theDisp, "WM_PROTOCOLS", FALSE);
    delwin = XInternAtom(theDisp, "WM_DELETE_WINDOW", FALSE);

    if (client_event->message_type == proto &&
	client_event->data.l[0]    == delwin) {
      /* it's a WM_DELETE_WINDOW event */

      if (BrowseDelWin(client_event->window)) break;
      if (TextDelWin(client_event->window)) break;

      if      (client_event->window == infoW) InfoBox(0);
      else if (client_event->window == gamW)  GamBox(0);
      else if (client_event->window == ctrlW) CtrlBox(0);
      else if (client_event->window == dirW)  DirBox(0);
      else if (client_event->window == psW)   PSDialog(0);

#ifdef HAVE_JPEG
      else if (client_event->window == jpegW) JPEGDialog(0);
#endif

#ifdef HAVE_TIFF
      else if (client_event->window == tiffW) TIFFDialog(0);
#endif

      else if (client_event->window == mainW) exit(0);
    }
  }
    break;


  case Expose: {
    XExposeEvent *exp_event = (XExposeEvent *) event;
    int x,y,w,h;
    Window win;

#ifdef VMS
    static int borders_sized = 0;
  
    if (!borders_sized  && !useroot && exp_event->window == mainW) {
      /*
       * Initial expose of main window, find the size of the ancestor
       * window just prior to the root window and adjust saved size
       * of display so that maximize functions will allow for window
       * decorations.
       */
      int status, count, mwid, mhgt, x, y, w, h, b, d, mbrd;
      Window root, parent, *children, crw = exp_event->window;
      borders_sized = 1;
      status = XGetGeometry(theDisp, crw, 
			    &root, &x, &y, &mwid, &mhgt, &mbrd, &d);
      
      for ( parent = crw, w=mwid, h=mhgt;
	   status && (parent != root) && (parent != vrootW); ) {
	crw = parent;
	status = XQueryTree ( theDisp, crw, &root, &parent, 
			     &children, &count );
	if ( children != NULL ) XFree ( children );
      }
      status = XGetGeometry(theDisp, crw, &root, &x, &y, &w, &h, &b, &d);
      if ( status ) {
	dispWIDE = dispWIDE + mwid - w + (2*b);
	dispHIGH = dispHIGH + mhgt - h + b;
	/*printf("New display dims: %d %d\n", dispWIDE, dispHIGH ); */
      }
    }
#endif


    win = exp_event->window;
    x = exp_event->x;      y = exp_event->y;
    w = exp_event->width;  h = exp_event->height;
    
    if (PUCheckEvent  (event)) break;   /* event has been processed */
    if (PSCheckEvent  (event)) break;   /* event has been processed */

#ifdef HAVE_JPEG
    if (JPEGCheckEvent(event)) break;   /* event has been processed */
#endif

#ifdef HAVE_TIFF
    if (TIFFCheckEvent(event)) break;   /* event has been processed */
#endif

    if (GamCheckEvent (event)) break;   /* event has been processed */
    if (BrowseCheckEvent (event, &retval, &done)) break;   /* event eaten */
    if (TextCheckEvent   (event, &retval, &done)) break;   /* event eaten */

    /* if the window doesn't do intelligent redraw, drop but last expose */
    if (exp_event->count>0 && 
	win != mainW && win != ctrlW &&	win != dirW && win != infoW) break;


    if (win==mainW || win==ctrlW || win==dirW || win==infoW) {
      /* must be a 'smart' window.  group exposes into an expose 'region' */
      int           count;
      Region        reg;
      XRectangle    rect;
      XEvent        evt;

      xvbcopy((char *) exp_event, (char *) &evt, sizeof(XEvent));
      reg = XCreateRegion();
      count = 0;

      do {
	rect.x      = evt.xexpose.x;
	rect.y      = evt.xexpose.y;
	rect.width  = evt.xexpose.width;
	rect.height = evt.xexpose.height;
	XUnionRectWithRegion(&rect, reg, reg);
	count++;
      } while (XCheckWindowEvent(theDisp,evt.xexpose.window,
				 ExposureMask, &evt));

      XClipBox(reg, &rect);  /* bounding box of region */
      XSetRegion(theDisp, theGC, reg);

      x = rect.x;      y = rect.y;
      w = rect.width;  h = rect.height;

      if (DEBUG) fprintf(stderr,"window: 0x%08x  collapsed %d expose events\n",
			 exp_event->window, count);
      if (DEBUG) fprintf(stderr,"        region bounding box: %d,%d %dx%d\n",
			 x, y, w, h);

      if (win == mainW) {
	if (DEBUG) fprintf(stderr,"EXPOSE:  ");
	if (!CheckForConfig()) {

	  if (mainWKludge) {
	    if (DEBUG) fprintf(stderr, "Using mainWKludge\n");
	    x = 0; y = 0;  w = eWIDE;  h = eHIGH;
	    XSetClipMask(theDisp, theGC, None);
	    mainWKludge = 0;
	  }

	  if (DEBUG) fprintf(stderr,"No configs pending.\n");
	  /* if (DEBUG) XClearArea(theDisp, mainW, x,y,w,h, False); */
	  DrawWindow(x,y,w,h);
	    
	  if (but[BCROP].active) InvCropRect();

	  canstartwait = 1;  /* finished drawing */
	  XSync(theDisp, False);
	}
	else
	  if (DEBUG) fprintf(stderr,"Ignored.  Config pending\n");
      }

      else if (win == infoW)          RedrawInfo(x,y,w,h);
      else if (win == ctrlW)          RedrawCtrl(x,y,w,h);
      else if (win == dirW)           RedrawDirW(x,y,w,h);
      
      XSetClipMask(theDisp, theGC, None);
      XDestroyRegion(reg);
    }

    else if (win == nList.win)      LSRedraw(&nList,0);
    else if (win == nList.scrl.win) SCRedraw(&nList.scrl);
    else if (win == dList.win)      LSRedraw(&dList,0);
    else if (win == dList.scrl.win) SCRedraw(&dList.scrl);
    else if (win == dnamW)          RedrawDNamW();
  }      
    break;
	

  case ButtonPress: {
    XButtonEvent *but_event = (XButtonEvent *) event;
    int i,x,y;
    Window win;

    win = but_event->window;
    x = but_event->x;  y = but_event->y;

    /* *always* check for pop-up events, as errors can happen... */
    if (PUCheckEvent  (event)) break;

    if (autoquit && win == mainW) Quit(0);

    if (viewonly) break;     /* ignore all other button presses */

    if (win == mainW && !useroot && showzoomcursor) {
      DoZoom(x, y, but_event->button);
      break;
    }

    if (PSCheckEvent  (event)) break;

#ifdef HAVE_JPEG
    if (JPEGCheckEvent(event)) break;
#endif

#ifdef HAVE_TIFF
    if (TIFFCheckEvent(event)) break;
#endif

    if (GamCheckEvent (event)) break;
    if (BrowseCheckEvent (event, &retval, &done)) break;
    if (TextCheckEvent   (event, &retval, &done)) break;

    switch (but_event->button) {

    case Button1:  
      if      (win == mainW) TrackPicValues(x,y);

      else if (win == ctrlW) {
	int   w,h;

	if (MBClick(&dispMB, x,y)) {
	  i = MBTrack(&dispMB);
	  if (i>=DMB_WINDOW && i<=DMB_ECMIRR) {
	    if (dispMB.flags[i]) break;    /* do nothing */

	    dispMode = i;

	    /* move checkmark */
	    for (i=DMB_WINDOW; i<=DMB_ECMIRR; i++) dispMB.flags[i] = 0;
	    dispMB.flags[dispMode] = 1;

	    HandleDispMode();
	  }

	  else if (i==DMB_COLRW) {   /* toggle rw on/off */
	    dispMB.flags[i] = !dispMB.flags[i];
	    allocMode = (dispMB.flags[i]) ? AM_READWRITE : AM_READONLY;
	    ChangeCmapMode(colorMapMode, 1, 0);
	  }

	  else if (i>=DMB_COLNORM && i<=DMB_COLSTDC) {
	    int j;

	    if (dispMB.flags[i]) break;    /* do nothing */

	    switch (i) {
	    case DMB_COLNORM:  ChangeCmapMode(CM_NORMAL, 1, 0);   
	                       defaultCmapMode = CM_NORMAL;    break;
	    case DMB_COLPERF:  ChangeCmapMode(CM_PERFECT,1, 0);
	                       defaultCmapMode = CM_PERFECT;   break;
	    case DMB_COLOWNC:  ChangeCmapMode(CM_OWNCMAP,1, 0);
	                       defaultCmapMode = CM_OWNCMAP;   break;
	    case DMB_COLSTDC:  ChangeCmapMode(CM_STDCMAP,1, 0);
	                       defaultCmapMode = CM_STDCMAP;   break;
	    }
	  }

	  break;
	}

	if (MBClick(&conv24MB, x,y)) {
	  i = MBTrack(&conv24MB);

	  if      (i==CONV24_8BIT || i==CONV24_24BIT) {
	    if (i != picType) {
	      Change824Mode(i);
	      if (i==CONV24_8BIT && state824==0) state824 = 1;  /* did 24->8 */
	      else if (i==CONV24_24BIT && state824==1) {
		/* went 24->8->24 */
		char buf[512];
		
		sprintf(buf,"Warning:  You appear to have taken a 24-bit ");
		strcat(buf, "image, turned it to an 8-bit image, and turned ");
		strcat(buf, "it back into a 24-bit image.  Understand that ");
		strcat(buf, "image data has probably been lost in this ");
		strcat(buf, "transformation.  You *may* want to reload the ");
		strcat(buf, "original image to avoid this problem.");

		ErrPopUp(buf, "\nI Know!");

		state824 = 2;   /* shut up until next image is loaded */
	      }
	    }
	  }

	  else if (i==CONV24_LOCK) {
	    conv24MB.flags[i] = !conv24MB.flags[i];
	  }

	  else if (i>=CONV24_FAST && i<=CONV24_BEST) {
	    conv24 = i;
	    for (i=CONV24_FAST; i<=CONV24_BEST; i++) {
	      conv24MB.flags[i] = (i==conv24);
	    }
	  }

	  break;
	}

	if (MBClick(&algMB, x,y)) {
	  i = MBTrack(&algMB);
	  if (i>=0) DoAlg(i);
	  break;
	}

	i=ClickCtrl(x,y);

	switch (i) {
	case BNEXT:   retval= NEXTPIC;  done=1;  break;
	case BPREV:   retval= PREVPIC;  done=1;  break;
	case BLOAD:   DirBox(BLOAD);    break;
	case BSAVE:   DirBox(BSAVE);    break;
	case BQUIT:   retval= QUIT;     done=1;  break;
	    
	case BCROP:   Crop();  break;
	case BUNCROP: UnCrop();  break;

	case BNORM: 
	  {
	    int w,h;

	    if (cWIDE>maxWIDE || cHIGH>maxHIGH) {
	      double r,wr,hr;
	      wr = ((double) cWIDE) / maxWIDE;
	      hr = ((double) cHIGH) / maxHIGH;

	      r = (wr>hr) ? wr : hr;   /* r is the max(wr,hr) */
	      w = (int) ((cWIDE / r) + 0.5);
	      h = (int) ((cHIGH / r) + 0.5);
	    }
	    else { w = cWIDE;  h = cHIGH; }

	    WResize(w, h);
	  }
	  break;

	case BMAX:    WMaximize();  break;
	case BUP10:   w = (eWIDE*11)/10;  h = (eHIGH*11)/10;
	              if (w==eWIDE) w++;
	              if (h==eHIGH) h++;
	              WResize(w,h);
	              break;

	case BDN10:   WResize((eWIDE*9)/10, (eHIGH*9)/10);  break;
	case BUP2:    WResize(eWIDE*2, eHIGH*2);  break;
	case BDN2:    WResize(eWIDE/2, eHIGH/2);  break;
	case B4BY3:   w = eWIDE;  h = (w * 3) / 4;
                      if (h>maxHIGH) { h = eHIGH;  w = (h*4)/3; }
                      WResize(w,h);
                      break;

	case BASPECT: FixAspect(1,&w,&h);  WResize(w,h);  break;

	case BSETSIZE: setSizeCmd();  break;

	case BINTSIZE: {
	                 /* round  (eWIDE/cWIDE),(eHIGH/cHIGH) to nearest
			    integer expansion/compression values */

	                 double w,h;

			 if (eWIDE >= cWIDE) {
			   w = ((double) eWIDE) / cWIDE;
			   w = floor(w + 0.5);
			   if (w<1.0) w = 1.0;
			 }
			 else {
			   int i;
			   double t,d,min,pick;
			   
			   /* compute nearest int divisor */
			   w = (double) eWIDE / (double) cWIDE;
			   min = 1.0;  pick=1.0;
			   for (i=1; i<cWIDE; i++) {
			     t = 1.0 / (double) i;
			     d = fabs(w - t);
			     if (d < min) { pick = t;  min = d; }
			     if (t < w) break;
			   }

			   w = pick;
			 }

			 if (eHIGH >= cHIGH) {
			   h = ((double) eHIGH) / cHIGH;
			   h = floor(h + 0.5);
			   if (h<1.0) h = 1.0;
			 }
			 else {
			   int i;
			   double t,d,min,pick;
			   
			   /* compute nearest int divisor */
			   h = (double) eHIGH / (double) cHIGH;
			   min = 1.0;  pick=1.0;
			   for (i=1; i<cHIGH; i++) {
			     t = 1.0 / (double) i;
			     d = fabs(h - t);
			     if (d < min) { pick = t;  min = d; }
			     if (t < h) break;
			   }

			   h = pick;
			 }

			 WResize((int) (w*cWIDE), (int) (h*cHIGH));
		       }
	               break;

	case BMAXPECT: { int w1,h1;
			 w1 = eWIDE;  h1 = eHIGH;
			 eWIDE = dispWIDE;  eHIGH = dispHIGH;
			 FixAspect(0,&w,&h);
			 eWIDE = w1;  eHIGH = h1;  /* play it safe */
			 WResize(w,h);
		       }   break;

	case BROTL:   Rotate(1);  break;
	case BROTR:   Rotate(0);  break;

	case BACROP:  AutoCrop();  break;

	case BFLIPH:  Flip(0);   break;
	case BFLIPV:  Flip(1);   break;


	case BRAW:
	case BDITH:
	case BSMOOTH: if      (i==BRAW)  epicMode = EM_RAW;
	              else if (i==BDITH) epicMode = EM_DITH;
	              else               epicMode = EM_SMOOTH;

                      SetEpicMode();	              
	              GenerateEpic(eWIDE, eHIGH);  /* gets drawn below... */
	              break;



	case BINFO:    InfoBox(!infoUp); break;

	case BVBROWSE: if (strlen(searchdir)) chdir(searchdir);
	                                 else chdir(initdir);
                       OpenBrowse();      break;

	case BGAMMA:   GamBox(!gamUp);   break;

	case BDELETE: if (DeleteCmd()) { done = 1;  retval = DELETE; }
	              break;

	case BGRAB:   if (Grab()) { done = 1;  retval = GRABBED; }
	              break;

	case BCMTVIEW: if (!commentUp) OpenCommentText();
	                          else CloseCommentText();
	               break;

	case BTXVIEW: textViewCmd();  break;

	case BLICENSE: ShowLicense(); break;

	default:      break;
	}

	if (i==BFLIPH || i==BFLIPV || i==BRAW || i==BDITH || i==BSMOOTH) {
	  DrawEpic();
	  SetCursors(-1);
	}
      }

      else if (win == nList.win) {
	i=LSClick(&nList,but_event);
	if (curname<0) ActivePrevNext();
	if (i>=0) { done = 1;  retval = i; }
      }

      else if (win == nList.scrl.win) SCTrack(&nList.scrl, x, y);

      else if (win == dirW) {
	i=ClickDirW(x,y);
	    
	switch (i) {
	case S_BOK:   if (dirUp == BLOAD) {
	                if (!DirCheckCD()) {
			  retval = LOADPIC;
			  done=1;
			}
		      }
	              else if (dirUp == BSAVE) {
		        DoSave();
		      }
	              break;

	case S_BCANC: DirBox(0);  break;

	case S_BRESCAN:
	              WaitCursor();  LoadCurrentDirectory();  SetCursors(-1);
	              break;
	}
      }

      else if (win == dList.win) {
	i=LSClick(&dList,but_event);
	SelectDir(i);
      }

      else if (win == dList.scrl.win) SCTrack(&dList.scrl, x,y);
      else if (win == infoW)          InfoBox(0);  /* close info */

      break;


    case Button2:  if (win == mainW) TrackCrop(x,y);
                   break;

    case Button3:  /* if using root, MUST NOT get rid of ctrlbox. */
	           if (!useroot) CtrlBox(!ctrlUp); 
                   break;

    default:       break;
    }
  }
    break;

	
  case KeyRelease: {
    XKeyEvent *key_event = (XKeyEvent *) event;
    char buf[128];  KeySym ks;
    int stlen, dealt;
	
    if (viewonly) break;     /* ignore all user input */

    stlen = XLookupString(key_event,buf,128,&ks,(XComposeStatus *) NULL);
    dealt = 0;

    if (key_event->window == mainW &&
	(ks == XK_Control_L || ks == XK_Control_R)) {
      if (showzoomcursor) {
	showzoomcursor = 0;
	SetCursors(-1);
      }
    }
  }
    break;

  case KeyPress: {
    XKeyEvent *key_event = (XKeyEvent *) event;
    char buf[128];  KeySym ks;
    int stlen, dealt, shift, ctrl;

    stlen = XLookupString(key_event,buf,128,&ks,(XComposeStatus *) NULL);
    shift = key_event->state & ShiftMask;
    ctrl  = key_event->state & ControlMask;
    dealt = 0;

    if (PUCheckEvent  (event)) break;          /* always check popups */

    if (autoquit && key_event->window == mainW) Quit(0);

    if (viewonly && !frominterrupt) break;     /* ignore all user input */

    if (PSCheckEvent  (event)) break;

    if (key_event->window == mainW &&
	(ks == XK_Control_L || ks == XK_Control_R)) {
      if (!showzoomcursor) {
	showzoomcursor = 1;
	SetCursors(-1);
      }
    }

#ifdef HAVE_JPEG
    if (JPEGCheckEvent(event)) break;
#endif

#ifdef HAVE_TIFF
    if (TIFFCheckEvent(event)) break;
#endif

    if (GamCheckEvent (event)) break;
    if (BrowseCheckEvent (event, &retval, &done)) break;
    if (TextCheckEvent   (event, &retval, &done)) break;


    /* check for pageup/pagedown, ^P in main window 
       (you can use shift-up or shift-down if no crop rectangle drawn)
       (for viewing multipage docs) */

    if (key_event->window == mainW) {
      dealt = 1;

      if (ks==XK_Prior || (ks==XK_Up && shift && !but[BCROP].active)) {
	if (strlen(pageBaseName) && numPages>1) {
	  done = 1;  retval = OP_PAGEUP;
	}
	else XBell(theDisp,0);
      }

      else if (ks==XK_Next || (ks==XK_Down && shift && !but[BCROP].active)) {
	if (strlen(pageBaseName) && numPages>1) {
	  done = 1;  retval = OP_PAGEDN;
	}
	else XBell(theDisp,0);
      }

      else if (buf[0] == '\020' && stlen>0) {
	if (strlen(pageBaseName) && numPages>1) {
	  int  i,j, okay;
	  char buf[64], txt[512];
	  static char *labels[] = { "\nOk", "\033Cancel" };

	  /* ask what page to go to */
	  sprintf(txt, "Go to page number...   (1-%d)", numPages);
	  sprintf(buf, "%d", curPage + 1);

	  okay = 0;
	  do {
	    i = GetStrPopUp(txt, labels, 2, buf, 64, "0123456789", 1);
	    if (!i && strlen(buf)>0) {   /* hit 'Ok', had a string entered */
	      /* check for page in range */
	      j = atoi(buf);
	      if (j>=1 && j<=numPages) {
		curPage = j;   /* one page past desired page */
		done = 1;
		retval = OP_PAGEUP;
		okay=1;
	      }
	      else XBell(theDisp, 0);
	    }
	    else okay = 1;
	  } while (!okay);
	}
	else XBell(theDisp, 0);
      }

      else dealt = 0;

      if (dealt) break;
    }
	


    /* check for crop rect keys */
    if (key_event->window == mainW) {
      dealt = 1;
      if      (ks==XK_Left  || ks==XK_KP_4 || ks==XK_F30) 
	CropKey(-1, 0,shift,ctrl);
      else if (ks==XK_Right || ks==XK_KP_6 || ks==XK_F32) 
	CropKey( 1, 0,shift,ctrl);
      else if (ks==XK_Up    || ks==XK_KP_8 || ks==XK_F28) 
	CropKey( 0,-1,shift,ctrl);
      else if (ks==XK_Down  || ks==XK_KP_2 || ks==XK_F34) 
	CropKey( 0, 1,shift,ctrl);
      else dealt = 0;
      if (dealt) break;
    }


    /* check for List keys */
    if (key_event->window == ctrlW || key_event->window == dirW) {
      LIST *theList;

      if (key_event->window == dirW) theList = &dList;
      else theList = &nList;

      dealt = 1;
      if      (ks==XK_Prior || (ks==XK_Up && shift)) 
	LSKey(theList,LS_PAGEUP);
      else if (ks==XK_Next || (ks==XK_Down && shift)) 
	LSKey(theList,LS_PAGEDOWN);

      else if (ks==XK_Up)    LSKey(theList,LS_LINEUP);
      else if (ks==XK_Down)  LSKey(theList,LS_LINEDOWN);

      else if (ks==XK_Home || (ks==XK_Prior && shift))
	LSKey(theList,LS_HOME);
      else if (ks==XK_End  || (ks==XK_End   && shift))
	LSKey(theList,LS_END);

      else dealt = 0;

      if (theList == &nList && dealt && curname<0) ActivePrevNext();

      if (theList == &dList && dealt) {  /* changed dir selection */
	SelectDir(-1);  /* nothing was double-clicked */
      }
      
      if (dealt) break;
    }


    /* check dir filename arrows */
    if (key_event->window == dirW && ks==XK_Left)  { DirKey('\002'); break; }
    if (key_event->window == dirW && ks==XK_Right) { DirKey('\006'); break; }


    /* check for preset keys (meta-1, meta-2, meta-3, meta-4) */
    if (key_event->state & Mod1Mask) {  /* meta is down */
      dealt = 1;
      if      (ks==XK_1) FakeButtonPress(&gbut[G_B1]);
      else if (ks==XK_2) FakeButtonPress(&gbut[G_B2]);
      else if (ks==XK_3) FakeButtonPress(&gbut[G_B3]);
      else if (ks==XK_4) FakeButtonPress(&gbut[G_B4]);
      else if (ks==XK_R || ks==XK_0) FakeButtonPress(&gbut[G_BRESET]);
      else dealt = 0;

      if (dealt) break;
    }

    if (!stlen) break;

    if (key_event->window == dirW) {
      if (DirKey(buf[0])) XBell(theDisp,0);
    }
    else {
      /* commands valid in any window */
	  
      switch (buf[0]) {
      case '\t':
      case ' ':    FakeButtonPress(&but[BNEXT]);    break;

      case '\r':
      case '\n':   if (nList.selected >= 0 && nList.selected < nList.nstr) {
	             done = 1;  retval = nList.selected; 
		     if (frominterrupt) retval = RELOAD;
		   }
	           break;

      case '\010':
      case '\177': FakeButtonPress(&but[BPREV]);    break;

      case '\004': FakeButtonPress(&but[BDELETE]);  break;  /* ^D */
      case '\014': FakeButtonPress(&but[BLOAD]);    break;  /* ^L */
      case '\023': FakeButtonPress(&but[BSAVE]);    break;  /* ^S */
      case '\007': FakeButtonPress(&but[BGRAB]);    break;  /* ^G */


      case '\003': FakeButtonPress(&but[BCMTVIEW]); break;  /* ^C */
      case '\024': FakeButtonPress(&but[BTXVIEW]);  break;  /* ^T */


      case '\026':
      case 'V':    FakeButtonPress(&but[BVBROWSE]); break;  /* ^V or V */

      case 'S':    FakeButtonPress(&but[BSETSIZE]); break;

      case '\021': 
      case 'q':    FakeButtonPress(&but[BQUIT]);    break;  /* ^Q or q */
	
      case '?':    if (!useroot) CtrlBox(!ctrlUp);  break;

      case 's':    FakeButtonPress(&but[BSMOOTH]);  break;
      case 'd':    FakeButtonPress(&but[BDITH]);    break;
      case 'r':    FakeButtonPress(&but[BRAW]);     break;
	    
      case 'a':    FakeButtonPress(&but[BASPECT]);  break;
      case 'A':    FakeButtonPress(&but[BACROP]);   break;

      case 'T':    FakeButtonPress(&but[BROTL]);    break;
      case 't':    FakeButtonPress(&but[BROTR]);    break;
      case 'h':    FakeButtonPress(&but[BFLIPH]);   break;
      case 'v':    FakeButtonPress(&but[BFLIPV]);   break;
      case '4':    FakeButtonPress(&but[B4BY3]);    break;
      case 'c':    FakeButtonPress(&but[BCROP]);    break;
      case 'u':    FakeButtonPress(&but[BUNCROP]);  break;
      case 'n':    FakeButtonPress(&but[BNORM]);    break;
      case 'm':    FakeButtonPress(&but[BMAX]);     break;
      case 'M':    FakeButtonPress(&but[BMAXPECT]); break;
      case ',':    FakeButtonPress(&but[BDN10]);    break;
      case '.':    FakeButtonPress(&but[BUP10]);    break;
      case '<':    FakeButtonPress(&but[BDN2]);     break;
      case '>':    FakeButtonPress(&but[BUP2]);     break;

      case 'i':    FakeButtonPress(&but[BINFO]);    break;
      case 'I':    FakeButtonPress(&but[BINTSIZE]); break;

      case 'e':    FakeButtonPress(&but[BGAMMA]);   break;

      case 'R':    FakeButtonPress(&gbut[G_BRESET]);   break;
      case 'p':    FakeButtonPress(&gbut[G_BAPPLY]);   break;
      case 'H':    FakeButtonPress(&gbut[G_BHISTEQ]);  break;
      case 'N':    FakeButtonPress(&gbut[G_BMAXCONT]); break;

      default:     break;
      }
    }
  }
    break;
	


  case ConfigureNotify: {
    XConfigureEvent *conf_event = (XConfigureEvent *) event;

    if (BrowseCheckEvent(event, &retval, &done)) break;
    if (TextCheckEvent  (event, &retval, &done)) break;

    if (conf_event->window == ctrlW ||
	conf_event->window == gamW  ||
	conf_event->window == infoW ||
	conf_event->window == mainW ||
	conf_event->window == dirW) {
      XSizeHints hints;
      if (DEBUG) fprintf(stderr,"got configure event.  %d,%d %dx%d\n",
	      conf_event->x, conf_event->y, conf_event->width,
	      conf_event->height);

      /* if there's a virtual window manager running (e.g. tvtwm), 
	 we're going to get 'conf_event' values in terms of the 
	 'real' root window (the one that is the size of the screen).
	 We'll want to translate them into values that are in terms of
	 the 'virtual' root window (the 'big' one) */

      if (vrootW != rootW) { /* virtual window manager running */
	int x1,y1;
	Window child;
	XTranslateCoordinates(theDisp, rootW, vrootW, 
			      conf_event->x, conf_event->y, 
			      &x1, &y1, &child);
	if (DEBUG) fprintf(stderr,"  conf translate:  -> %d,%d\n", x1,y1);
	conf_event->x = x1;  conf_event->y = y1;
      }

#ifndef VMS
      /* read hints for this window and adjust any position hints */
      if (XGetNormalHints(theDisp, conf_event->window, &hints)) {
	if (DEBUG) fprintf(stderr,"  got hints (0x%x  %d,%d)\n",
		hints.flags, hints.x, hints.y);
	hints.x = conf_event->x;
	hints.y = conf_event->y;
	XSetNormalHints(theDisp, conf_event->window, &hints);
	if (DEBUG) fprintf(stderr,"  set hints (0x%x  %d,%d)\n",
		hints.flags, hints.x, hints.y);
      }
#endif
    }

    if (conf_event->window == mainW) {
      if (!rotatesLeft) {
	if (DEBUG) fprintf(stderr,"CONFIG: (%d,%d %dx%d) ", 
			   conf_event->x, conf_event->y,
			   conf_event->width, conf_event->height);

	if (CheckForConfig()) {
	  if (DEBUG) fprintf(stderr,"more configs pending.  ignored\n");
	}
	else {
	  XEvent xev;
	  if (DEBUG) fprintf(stderr,"No configs pend.");
	  
	  if (conf_event->width == eWIDE && conf_event->height == eHIGH) {
	    if (DEBUG) fprintf(stderr,"No redraw\n");
	  }
	  else {
	    if (DEBUG) fprintf(stderr,"Do full redraw\n");
	    Resize(conf_event->width, conf_event->height);
	    
	    /* eat any pending expose events and do a full redraw */
	    while (XCheckTypedWindowEvent(theDisp, mainW, Expose, &xev)) {
	      XExposeEvent *exp = (XExposeEvent *) &xev;
	      if (DEBUG) 
		fprintf(stderr,"  ate expose (%s) (count=%d) %d,%d %dx%d\n",
			exp->send_event ? "program" : "user", exp->count,
			exp->x, exp->y, exp->width, exp->height);
	    }

	    DrawWindow(0,0,conf_event->width, conf_event->height);
            canstartwait=1;
	    XSync(theDisp, False);
	    SetCursors(-1);
	  }
	}
      }

      if (rotatesLeft>0) {
	rotatesLeft--;
	if (!rotatesLeft) mainWKludge = 1;
      }
      if (!rotatesLeft) SetCursors(-1);
    }

  }
    break;
	

	
  case CirculateNotify:
  case DestroyNotify:
  case GravityNotify:       break;

  case MapNotify: {
    XMapEvent *map_event = (XMapEvent *) event;

    if (map_event->window == mainW ||
	(map_event->window == ctrlW && dispMode != DMB_WINDOW)) {
      if (DEBUG) fprintf(stderr,"map event received on mainW/ctrlW\n");

      if (autoclose) {
	if (wasInfoUp) { InfoBox(wasInfoUp);     wasInfoUp=0; }
	if (wasCtrlUp) { CtrlBox(wasCtrlUp);     wasCtrlUp=0; }
	if (wasDirUp)  { DirBox(wasDirUp);       wasDirUp=0; }
	UnHideBrowseWindows();
	UnHideTextWindows();
	if (wasGamUp)  { GamBox(wasGamUp);       wasGamUp=0; }
	if (wasPsUp)   { PSDialog(wasPsUp);      wasPsUp=0; }
#ifdef HAVE_JPEG
	if (wasJpegUp) { JPEGDialog(wasJpegUp);  wasJpegUp=0; }
#endif

#ifdef HAVE_TIFF
	if (wasTiffUp) { TIFFDialog(wasTiffUp);  wasTiffUp=0; }
#endif
      }
    }
  }
    break;


  case UnmapNotify: {
    XUnmapEvent *unmap_event = (XUnmapEvent *) event;

    if (unmap_event->window == mainW ||
	(unmap_event->window == ctrlW && dispMode != DMB_WINDOW)) {
      if (DEBUG) fprintf(stderr,"unmap event received on mainW/ctrlW\n");
      if (DEBUG) fprintf(stderr,"dispMode = %d\n", dispMode);

      /* don't do it if we've just switched to a root mode */
      if ((unmap_event->window == mainW && dispMode == 0) ||
	  (unmap_event->window == ctrlW && dispMode != 0)) {  

	if (autoclose) {
	  if (unmap_event->window == mainW) {
	    if (ctrlUp) { wasCtrlUp = ctrlUp;  CtrlBox(0); }
	  }

	  if (infoUp) { wasInfoUp = infoUp;  InfoBox(0); }
	  if (dirUp)  { wasDirUp  = dirUp;   DirBox(0); }
	  HideBrowseWindows();
	  HideTextWindows();
	  if (gamUp)  { wasGamUp  = gamUp;   GamBox(0); }
	  if (psUp)   { wasPsUp   = psUp;    PSDialog(0); }
#ifdef HAVE_JPEG
	  if (jpegUp) { wasJpegUp = jpegUp;  JPEGDialog(0); }
#endif

#ifdef HAVE_TIFF
	  if (tiffUp) { wasTiffUp = tiffUp;  TIFFDialog(0); }
#endif
	}
      }
    }
  }
    break;

  case ReparentNotify: {
    XReparentEvent *reparent_event = (XReparentEvent *) event;

    if (DEBUG) {
      fprintf(stderr,"Reparent: mainW=%x ->win=%x ->ev=%x  ->parent=%x  ", 
	      mainW, reparent_event->window, reparent_event->event, 
	      reparent_event->parent);
      fprintf(stderr,"%d,%d\n", reparent_event->x, reparent_event->y);
    }

    if (reparent_event->window == mainW) {
      ch_offx = reparent_event->x;  /* offset required for ChangeAttr call */
      ch_offy = reparent_event->y;

      p_offx = p_offy = 0;          /* topleft correction for WMs titlebar */

      if (ch_offx == 0 && ch_offy == 0) {  
	/* looks like the user is running MWM or OLWM */

	XWindowAttributes xwa;

	/* first query the attributes of mainW.  x,y should be the offset
	   from the parent's topleft corner to the windows topleft.
	   OLWM puts the info here */

	XSync(theDisp, False);
	XGetWindowAttributes(theDisp, mainW, &xwa);
	
	if (DEBUG) 
	  fprintf(stderr,"XGetAttr: mainW %d,%d %dx%d\n", xwa.x, xwa.y,
		  xwa.width, xwa.height);

	if (xwa.x == 0 && xwa.y == 0) {
	  /* MWM, at least mine, puts 0's in those fields.  To get the
	     info, we'll have to query the parent window */

	  XSync(theDisp, False);
	  XGetWindowAttributes(theDisp, reparent_event->parent, &xwa);
	
	  if (DEBUG) 
	    fprintf(stderr,"XGetAttr: parent %d,%d %dx%d\n", xwa.x, xwa.y,
		    xwa.width, xwa.height);
	}
	else {
	  /* KLUDGE:  if we're running olwm, xwa.{x,y} won't be 0,0.
	     in olwm, the window drifts down and right each time
	     SetWindowPos() is called.  God knows why.  Anyway, I'm
	     inserting a kludge here to increase 'ch_offx' and 'ch_offy'
	     by bwidth so that this drifting won't happen.  No doubt this'll
	     screw up behavior on some *other* window manager, but it should
	     work with TWM, OLWM, and MWM (the big three) */
	  ch_offx += bwidth;
	  ch_offy += bwidth;
	}

	p_offx = xwa.x;
	p_offy = xwa.y;
      }
    }
  }
    break;
    

  case EnterNotify:
  case LeaveNotify: {
    XCrossingEvent *cross_event = (XCrossingEvent *) event;
    if (cross_event->window == mainW || 0
	/* (cross_event->window == gamW && cmapInGam) */ ) {

      if (cross_event->type == EnterNotify && cross_event->window == mainW) {
	if (cross_event->state & ControlMask) {  /* ctrl pressed */
	  if (!showzoomcursor) {
	    showzoomcursor = 1;
	    SetCursors(-1);
	  }
	}
	else {
	  if (showzoomcursor) {
	    showzoomcursor = 0;
	    SetCursors(-1);
	  }
	}
      }


      if (cross_event->type == EnterNotify && LocalCmap && !ninstall) 
	XInstallColormap(theDisp,LocalCmap);

      if (cross_event->type == LeaveNotify && LocalCmap && !ninstall) 
	XUninstallColormap(theDisp,LocalCmap);
    }
  }
    break;
	
	
  default: break;		/* ignore unexpected events */
  }  /* switch */

  frominterrupt = 0;
  *donep = done;
  return(retval);
}



/***********************************/
static void textViewCmd()
{
  int   i;
  char *name;

  i = nList.selected;
  if (i<0 || i>=numnames) return;     /* shouldn't happen */

  if (namelist[i][0] != '/') {  /* prepend 'initdir' */
    name = (char *) malloc(strlen(namelist[i]) + strlen(initdir) + 2);
    if (!name) FatalError("malloc in textViewCmd failed");
    sprintf(name,"%s/%s", initdir, namelist[i]);
  }
  else name = namelist[i];

  TextView(name);
  
  if (name != namelist[i]) free(name);
}


/***********************************/
static void setSizeCmd()
{
  /* open 'set size' prompt window, get a string, parse it, and try to
     set the window size accordingly */

  int   i, arg1, arg2, numargs, pct1, pct2, state, neww, newh;
  char  txt[512], buf[64], *sp, ch;
  static char *labels[] = { "\nOk", "\033Cancel" };
  
  sprintf(txt, "Enter new image display size (ex. '400 x 300'),\n");
  strcat (txt, "expansion ratio (ex. '75%'),\n");
  strcat (txt, "or expansion ratios (ex. '200% x 125%'):");

  sprintf(buf, "%d x %d", eWIDE, eHIGH);    /* current vals */

  i = GetStrPopUp(txt, labels, 2, buf, 64, "0123456789x% ", 1);

  if (i) return;     /* cancelled */
  if (strlen(buf) == 0) return;     /* no command */


  /* attempt to parse the string accordingly...
   * parses strings of the type: <num> [%] [ x <num> [%] ] 
   * (-ish.  <num> all by itself isn't legal)
   * there may be any # of spaces between items, including zero
   */

  arg1 = arg2 = numargs = pct1 = pct2 = state = 0;
  sp = buf;

  do  {
    ch = *sp++;

    switch (state) {
    case 0:             /* haven't seen arg1 yet */
      if      (ch == ' ') {}
      else if (ch == '%' || ch == 'x' || ch == '\0') state = 99;  /* no arg1 */
      else { arg1  = (ch - '0');  state = 1; }
      break;

    case 1:             /* parsing arg1 */
      numargs = 1;
      if      (ch == ' ')  state = 2;
      else if (ch == '%')  state = 3;
      else if (ch == 'x')  state = 4;
      else if (ch == '\0') state = 99;
      else arg1 = (arg1 * 10) + (ch - '0');
      break;

    case 2:             /* got arg1 and whitespace */
      if      (ch == ' ') {}
      else if (ch == '%') state = 3;
      else if (ch == 'x') state = 4;
      else state = 99;
      break;

    case 3:             /* got arg1 % */
      pct1 = 1;
      if      (ch == ' ')  {}
      else if (ch == '%')  state = 99;
      else if (ch == 'x')  state = 4;
      else if (ch == '\0') state = 100;
      else state = 99;
      break;

    case 4:             /* got arg1 [%] x */
      if      (ch == ' ') {}
      else if (ch == '%' || ch == 'x' || ch == '\0') state = 99;
      else { arg2 = (ch - '0');  state = 5; }
      break;

    case 5:             /* parsing arg2 */
      numargs = 2;
      if      (ch == ' ')  state = 6;
      else if (ch == '%')  state = 7;
      else if (ch == 'x')  state = 99;
      else if (ch == '\0') state = 100;
      else arg2 = (arg2 * 10) + (ch - '0');
      break;

    case 6:             /* got arg2 and whitespace */
      if      (ch == ' ')  {}
      else if (ch == '%')  state = 7;
      else if (ch == 'x')  state = 99;
      else if (ch == '\0') state = 100;
      else state = 99;
      break;

    case 7:             /* got arg1 [%] x arg2 % */
      pct2  = 1;
      state = 100;
      break;

    case 99:            /* error in parsing */
      break;

    case 100:           /* successful parse */
      break;
    }
  } while (state!=99 && state!=100);

  /* done parsing... */
  if (state == 99) {
    ErrPopUp("Error:  The entered SetSize string is not valid.", "\nRight.");
    return;
  }

  if (DEBUG)
    fprintf(stderr,"setSize:  arg1=%d, arg2=%d, numargs=%d, pct=%d,%d\n",
	    arg1, arg2, numargs, pct1, pct2);

  /* otherwise... */
  if (numargs == 1) {
    if (pct1) {
      neww = (cWIDE * arg1) / 100;
      newh = (cHIGH * arg1) / 100;
    }
    else return;    /* shouldn't happen */
  }
  else {   /* numargs = 2; */
    neww = (pct1) ? (cWIDE * arg1) / 100 : arg1;
    newh = (pct2) ? (cHIGH * arg2) / 100 : arg2;
  }

  if (neww < 1 || newh < 1 || neww > 64000 || newh > 64000) {
    sprintf(txt, "The new desired image display size of '%d x %d' is %s",
	    neww, newh, "ludicrous.  Ignored.");
    ErrPopUp(txt, "\nSez you!");
    return;
  }

  WResize(neww, newh);
}


/***********************************/
void DrawWindow(x,y,w,h)
int x,y,w,h;
{
  if (x+w < eWIDE) w++;  /* add one for broken servers (?) */
  if (y+h < eHIGH) h++;

  if (theImage)
    XPutImage(theDisp,mainW,theGC,theImage,x,y,x,y,w,h);
  else 
    if (DEBUG) fprintf(stderr,"Tried to DrawWindow when theImage was NULL\n");
}


/***********************************/
void WResize(w,h)
int w,h;
{
  XWindowAttributes xwa;

  RANGE(w,1,maxWIDE);  RANGE(h,1,maxHIGH);

  if (useroot) {
    Resize(w,h);
    MakeRootPic();
    SetCursors(-1);
    return;
  }

  /* determine if new size goes off edge of screen.  if so move window so it
     doesn't go off screen */

  GetWindowPos(&xwa);
  if (xwa.x + w > vrWIDE) xwa.x = vrWIDE - w;
  if (xwa.y + h > vrHIGH) xwa.y = vrHIGH - h;

  if (DEBUG) fprintf(stderr,"%s: resizing window to %d,%d at %d,%d\n",
		     cmd,w,h,xwa.x,xwa.y);

  /* resize the window */
  xwa.width = w;  xwa.height = h;

  SetWindowPos(&xwa);
}




/***********************************/
static void WMaximize()
{
  if (useroot) WResize(dispWIDE, dispHIGH);
  else {
    XWindowAttributes xwa;
    xvbzero((char *) &xwa, sizeof(XWindowAttributes));
    xwa.x = xwa.y = 0;
    xwa.width  = dispWIDE;  
    xwa.height = dispHIGH;
    SetWindowPos(&xwa);
  }
}




/***********************************/
void WRotate()
{
  /* rotate the window and redraw the contents  */

  if (but[BCROP].active) BTSetActive(&but[BCROP],0);

  if (useroot || eWIDE == eHIGH) {
    /* won't see any configure events.  Manually redraw image */
    DrawEpic();
    SetCursors(-1);
    return;
  }
  else {
    rotatesLeft++;
    XClearWindow(theDisp, mainW);  /* get rid of old bits */
    GenExpose(mainW, 0, 0, eWIDE, eHIGH);
    WResize(eWIDE, eHIGH);
  }
}


/***********************************/
void WCrop(w,h)
int w,h;
{
  XWindowAttributes xwa;

  if (useroot) {
    MakeRootPic();
    SetCursors(-1);
  }

  else {
    /* we want to move window to old x,y + crx1,cry1 */
    GetWindowPos(&xwa);
  
    if (!origcropvalid) {  /* first crop.  remember win pos */
      origcropvalid = 1;
      origcropx = xwa.x;
      origcropy = xwa.y;
    }

    xwa.x += crx1;  xwa.y += cry1;
    xwa.width = w;  xwa.height = h;
    GenExpose(mainW, 0, 0, eWIDE, eHIGH);
    SetWindowPos(&xwa);
  }
}


/***********************************/
void WUnCrop()
{
  int w,h;
  XWindowAttributes xwa;

  /* a proper epic has been generated.  eWIDE,eHIGH are the new window size */


  if (useroot) {
    MakeRootPic();
    SetCursors(-1);
  }

  else {   /* !useroot */
    GetWindowPos(&xwa);

    w = eWIDE;  h = eHIGH;

    /* restore to position when originally cropped */
    if (origcropvalid) {  /* *should* always be true... */
      origcropvalid = 0;
      xwa.x = origcropx;
      xwa.y = origcropy;
    }

    if (xwa.x + w > vrWIDE) xwa.x = vrWIDE - w;   /* keep on screen */
    if (xwa.y + h > vrHIGH) xwa.y = vrHIGH - h;

    if (xwa.x<0) xwa.x = 0;
    if (xwa.y<0) xwa.y = 0;
    xwa.width = w;  xwa.height = h;
    
    if (!useroot) {
      SetWindowPos(&xwa);
      GenExpose(mainW, 0, 0, eWIDE, eHIGH);
    }
  }
}



/***********************************/
void GetWindowPos(xwa)
XWindowAttributes *xwa;
{
  Window child;
  
  /* returns the x,y,w,h coords of mainW.  x,y are relative to rootW 
     the border is not included (x,y map to top-left pixel in window) */

  /* Get the window width/height */
  XGetWindowAttributes(theDisp,mainW,xwa);

  /* Get the window origin */
  XTranslateCoordinates(theDisp,mainW,rootW,0,0,&xwa->x,&xwa->y,&child);
}


/***********************************/
void SetWindowPos(xwa)
XWindowAttributes *xwa;
{
  /* sets window x,y,w,h values */
  XWindowChanges    xwc;

  /* Adjust from window origin, to border origin */
  xwc.x = xwa->x - xwa->border_width - ch_offx;
  xwc.y = xwa->y - xwa->border_width - ch_offy;

  if (!xwa->border_width) xwa->border_width = bwidth;
  xwc.border_width = xwa->border_width;

  /* if we're less than max size in one axis, allow window manager doohickeys
     on the screen */
  
  if (xwa->width  < dispWIDE && xwc.x < p_offx) xwc.x = p_offx;
  if (xwa->height < dispHIGH && xwc.y < p_offy) xwc.y = p_offy;

  xwc.width  = xwa->width;
  xwc.height = xwa->height;


  /* if there is a virtual window manager running, then we should translate
     the coordinates that are in terms of 'real' screen into coordinates
     that are in terms of the 'virtual' root window 
     from: Daren W. Latham <dwl@mentat.udev.cdc.com> */
  
  if (vrootW != rootW) { /* virtual window manager running */
    int x1,y1;
    Window child;
    XTranslateCoordinates(theDisp, rootW, vrootW,xwc.x,xwc.y,&x1,&y1,&child);
    if (DEBUG) fprintf(stderr,"SWP: translate: %d,%d -> %d,%d\n",
		       xwc.x,xwc.y,x1,y1);
    xwc.x = x1;  xwc.y = y1;
  }
  


  if (DEBUG) {
    fprintf(stderr,
	    "SWP: xwa=%d,%d %dx%d xwc=%d,%d %dx%d off=%d,%d bw=%d klg=%d,%d\n",
	    xwa->x, xwa->y, xwa->width, xwa->height,
	    xwc.x, xwc.y, xwc.width, xwc.height, p_offx, p_offy, 
	    xwa->border_width, kludge_offx, kludge_offy);
  }

  xwc.x += kludge_offx;
  xwc.y += kludge_offy;

#if defined(DXWM) || defined(VMS)
  /* dxwm seems to *only* pay attention to the hints */
  {
    XSizeHints hints;
    if (DEBUG) fprintf(stderr,"SWP: doing the DXWM thing\n");
    /* read hints for this window and adjust any position hints */
    if (XGetNormalHints(theDisp, mainW, &hints)) {
      hints.flags |= USPosition | USSize;
      hints.x = xwc.x;  hints.y = xwc.y;
      hints.width = xwc.width; hints.height = xwc.height;
      XSetNormalHints(theDisp, mainW, &hints);
    }

#ifndef MWM     /* don't do this if you're running MWM */
    xwc.x -= 5;   xwc.y -= 25;    /* EVIL KLUDGE */
#endif /* MWM */
  }
#endif

  /* all non-DXWM window managers (?) */
  /* Move/Resize the window. */
  XConfigureWindow(theDisp, mainW, 
		   CWX | CWY | CWWidth | CWHeight /*| CWBorderWidth*/, &xwc);
}


/***********************************/
static void TrackCrop(mx,my)
int mx,my;
{
  Window       rW,cW;
  int          rx,ry,ox,oy,x,y,active;
  unsigned int mask;

  if (but[BCROP].active) {             /* turn off old cropping rectangle */
    XSetFunction(theDisp,theGC,GXinvert);
    Rect(crx1,cry1,crx2,cry2);
    XSetFunction(theDisp,theGC,GXcopy);
  }
  active = 0;
  SetCropString(active);

  crx1 = ox = mx;  cry1 = oy = my;         /* nail down one corner */

  while (1) {
    if (XQueryPointer(theDisp,mainW,&rW,&cW,&rx,&ry,&x,&y,&mask)) {
      if (!(mask & Button2Mask)) break;    /* button released */

      if (x!=ox || y!=oy) {                /* moved.  erase and redraw */
	crx2 = x;  cry2 = y;
	XSetFunction(theDisp,theGC,GXinvert);
	Rect(crx1,cry1,ox,oy);
	active = Rect(crx1,cry1,crx2,cry2);
	XSetFunction(theDisp,theGC,GXcopy);
	XFlush(theDisp);
	ox=crx2;  oy=cry2;
	if (infoUp) SetCropString(active);
      }
    }
  }

  RANGE(crx1,0,eWIDE);  RANGE(cry1,0,eHIGH);
  RANGE(crx2,0,eWIDE);  RANGE(cry2,0,eHIGH);
  BTSetActive(&but[BCROP],active);
  SetCropString(active);
}


/***********************************/
static void CropKey(dx,dy,grow,crop)
     int dx,dy,grow,crop;
{
  int x1,x2,y1,y2,active;

  if (crop) { /* chop off a pixel from the appropriate edge */
    int dealt=1;
    if      (dx<0 && cWIDE>1) DoCrop(cXOFF,   cYOFF,   cWIDE-1, cHIGH);
    else if (dx>0 && cWIDE>1) DoCrop(cXOFF+1, cYOFF,   cWIDE-1, cHIGH);
    else if (dy<0 && cHIGH>1) DoCrop(cXOFF,   cYOFF,   cWIDE,   cHIGH-1);
    else if (dy>0 && cHIGH>1) DoCrop(cXOFF,   cYOFF+1, cWIDE,   cHIGH-1);
    else { dealt = 0;  XBell(theDisp, 0); }

    if (dealt) {
      if (useroot) DrawEpic();
      else {
	CreateXImage();
	WCrop(eWIDE, eHIGH);
      }
    }

    return;
  }
      

  if (!but[BCROP].active) return;

  /* x1,y1 = top-left,  x2,y2 = bot-right */
  if (crx1<crx2) { x1=crx1;  x2=crx2; }   else { x1=crx2;  x2=crx1; }
  if (cry1<cry2) { y1=cry1;  y2=cry2; }   else { y1=cry2;  y2=cry1; }

  if (!grow) {    /* move the rectangle */
    x1 += dx;  x2 += dx;  y1 += dy;  y2 += dy;
    if (x1<0 || x2>=eWIDE) { x1 -= dx;  x2 -= dx; }
    if (y1<0 || y2>=eHIGH) { y1 -= dy;  y2 -= dy; }
  }
  else {          /* grow the rectangle, pref. keeping top-left anchored */
    x2 += dx;  y2 += dy;
    if (x2>=eWIDE) { 
      x1 -= dx;  x2 -= dx;
      if (x1<0) x1=0;
    }

    if (y2>=eHIGH) { 
      y1 -= dy;  y2 -= dy;
      if (y1<0) y1=0;
    }
  }
    
  XSetFunction(theDisp,theGC,GXinvert);
  Rect(crx1,cry1,crx2,cry2);
  crx1 = x1;  cry1 = y1;  crx2 = x2;  cry2 = y2;
  active = Rect(crx1,cry1,crx2,cry2);
  XSetFunction(theDisp,theGC,GXcopy);

  BTSetActive(&but[BCROP], active);
  SetCropString(active);
}

  
/***********************************/
static int Rect(x,y,x1,y1)
int x,y,x1,y1;
{
  int w,h;

  /* returns 0 if it didn't draw anything (rect is too small), 1 if it did */
  w = abs(x-x1);  h = abs(y-y1);
  if (x>x1) x = x1;
  if (y>y1) y = y1;

  /* keep rectangle inside window */  
  if (x<0) { w+=x; x=0; }
  if (y<0) { h+=y; y=0; }
  if (x+w>=eWIDE) w=eWIDE-x-1;  
  if (y+h>=eHIGH) h=eHIGH-y-1;

  if (w<4 || h<4) return 0;   /* too small */

  XSetPlaneMask(theDisp, theGC, AllPlanes);
  XDrawRectangle(theDisp, mainW, theGC, x, y, w, h);
  XSetPlaneMask(theDisp, theGC, 1L);
  XDrawRectangle(theDisp, mainW, theGC, x+1, y+1, w-2, h-2);
  XSetPlaneMask(theDisp, theGC, AllPlanes);
  return 1;
}


/***********************************/
void InvCropRect()
{
  XSetFunction(theDisp,theGC,GXinvert);
  Rect(crx1,cry1,crx2,cry2);
  XSetFunction(theDisp,theGC,GXcopy);
}


/***********************************/
static void TrackPicValues(mx,my)
int mx,my;
{
  Window       rW,cW;
  int          rx,ry,ox,oy,x,y;
  unsigned int mask;
  int          ty, w, ecol;
  char         foo[64];
  unsigned long wh, bl;
  char         *str  = "8888,8888 = 123,123,123  (123,123,123 HSV)";

  ecol = 0;  wh = infobg;  bl = infofg;

  /* do a colormap search for black and white if LocalCmap 
     and use those colors instead of infobg and infofg */
  if (LocalCmap) {
    XColor ctab[256];
    int  i;
    long cval;

    for (i=0; i<nfcols; i++) ctab[i].pixel = freecols[i];
    XQueryColors(theDisp,LocalCmap,ctab,nfcols);
    
    /* find 'blackest' pixel */
    cval = 0x10000 * 3;
    for (i=0; i<nfcols; i++)
      if (ctab[i].red + ctab[i].green + ctab[i].blue < cval) {
	cval = ctab[i].red + ctab[i].green + ctab[i].blue;
	bl = ctab[i].pixel;
      }

    /* find 'whitest' pixel */
    cval = -1;
    for (i=0; i<nfcols; i++)
      if ((long)ctab[i].red + (long)ctab[i].green + (long)ctab[i].blue >cval) {
	cval = ctab[i].red + ctab[i].green + ctab[i].blue;
	wh = ctab[i].pixel;
      }
  }

  XSetFont(theDisp, theGC, monofont);
  w = XTextWidth(monofinfo, str, strlen(str));

  if (my > eHIGH/2) ty = 0;
               else ty = eHIGH-(monofinfo->ascent + mfinfo->descent)-4;

  ox = oy = -1;  /* kludge to force redraw first time through */

  XSetForeground(theDisp, theGC, bl);
  XFillRectangle(theDisp, mainW, theGC, 0, ty, w + 8, 
		 (monofinfo->ascent+monofinfo->descent) + 4);
  XSetForeground(theDisp, theGC, wh);
  XSetBackground(theDisp, theGC, bl);
  foo[0] = '\0';

  x = mx;  y = my;
  RANGE(x,0,eWIDE-1);   RANGE(y,0,eHIGH-1);
  rx = cXOFF + (x * cWIDE) / eWIDE;
  ry = cYOFF + (y * cHIGH) / eHIGH;

  if (picType == PIC8) ecol = pic[ry * pWIDE + rx];

  while (1) {
    int px, py, pix;

    if (XQueryPointer(theDisp,mainW,&rW,&cW,&rx,&ry,&x,&y,&mask)) {
      if (!(mask & Button1Mask)) break;    /* button released */

      RANGE(x,0,eWIDE-1);  
      RANGE(y,0,eHIGH-1);

      px = cXOFF + (x * cWIDE) / eWIDE;
      py = cYOFF + (y * cHIGH) / eHIGH;

      if (px!=ox || py!=oy) {                /* moved.  erase and redraw */
	double h1, s1, v1;
	int    rval, gval, bval;

	if (picType == PIC8) {
	  ecol = pix = pic[py * pWIDE + px];
	  rval = rcmap[pix];  gval = gcmap[pix];  bval = bcmap[pix];
	}
	else {  /* PIC24 */
	  rval = pic[py * pWIDE * 3 + px * 3];
	  gval = pic[py * pWIDE * 3 + px * 3 + 1];
	  bval = pic[py * pWIDE * 3 + px * 3 + 2];
	}

	rgb2hsv(rval, gval, bval, &h1, &s1, &v1);
	if (h1<0.0) h1 = 0.0;   /* map 'NOHUE' to 0.0 */

	sprintf(foo,"%4d,%4d = %3d,%3d,%3d  (%3d %3d %3d HSV)",
		px, py, rval, gval, bval, 
		(int) h1, (int) (s1 * 100), (int) (v1 * 100));
	
	XDrawImageString(theDisp,mainW,theGC, 4, ty + 2 + monofinfo->ascent, 
			 foo, strlen(foo));
	ox = px;  oy = py;
      }
    }
  }

  if (foo[0]) {
    strcat(foo, "\n");
    XStoreBytes(theDisp, foo, strlen(foo));
  }

  XSetFont(theDisp, theGC, mfont);
  DrawWindow(0,ty,eWIDE,(monofinfo->ascent+monofinfo->descent)+4);

  if (picType == PIC8 && ecol != editColor) ChangeEC(ecol);
}


/***********************************/
static Bool IsConfig(dpy, ev, arg)
Display *dpy;
XEvent  *ev;
char    *arg;
{
  XConfigureEvent *cev;

  if (ev->type == ConfigureNotify) {
    cev = (XConfigureEvent *) ev;
    if (cev->window == mainW && (cev->width != eWIDE || cev->height != eHIGH))
      *arg = 1;
  }
  return False;
}

/***********************************/
static int CheckForConfig()
{
  XEvent ev;
  char   foo;

  /* returns true if there's a config event in which mainW changes size
     in the event queue */
  
  XSync(theDisp, False);
  foo = 0;
  XCheckIfEvent(theDisp, &ev, IsConfig, &foo);
  return foo;
}


/************************************************************************/
void SetEpicMode()
{
  if (epicMode == EM_RAW) {
    BTSetActive(&but[BRAW],   0);
                               /* only enable dith if ncols>0 and 8-bit pic */
    BTSetActive(&but[BDITH],  (ncols>0 && picType == PIC8) );
    BTSetActive(&but[BSMOOTH],1);
  }

  else if (epicMode == EM_DITH) {
    BTSetActive(&but[BRAW],   1);
    BTSetActive(&but[BDITH],  0);
    BTSetActive(&but[BSMOOTH],1);
  }

  else if (epicMode == EM_SMOOTH) {
    BTSetActive(&but[BRAW],   1);
    BTSetActive(&but[BDITH],  0);
    BTSetActive(&but[BSMOOTH],0);
  }
}


/************************************************************************/
int xvErrorHandler(disp, err)
Display *disp;
XErrorEvent *err;
{
  char buf[128];

  XUngrabPointer(theDisp, CurrentTime);   /* in case error occurred in Grab */

  xerrcode = err->error_code;

  /* BadAlloc errors (on a XCreatePixmap() call)
     and BadAccess errors on XFreeColors are 'ignoreable' errors */

  if (xerrcode == BadAlloc || 
      (xerrcode == BadAccess && err->request_code==88)) return 0;

  else {
    /* all other errors are 'fatal' */
    XGetErrorText(disp, xerrcode, buf, 128);
    fprintf(stderr,"X Error: %s\n",buf);
    fprintf(stderr,"  Major Opcode:  %d\n",err->request_code);

    if (DEBUG) {   /* crash 'n' burn for debugging purposes */
      char *str;
      str  = NULL;
      *str = '0';
    }

    exit(-1);
  }

  return 0;
}


/************************************************************************/
#ifdef _AIX
  static void onInterrupt(int i)
#else
  static void onInterrupt()
#endif
{
  /* but first, if any input-grabbing popups are active, we have to 'cancel'
     them. */
  
  if (psUp) PSDialog(0);      /* close PS window */

#ifdef HAVE_JPEG
  if (jpegUp) JPEGDialog(0);  /* close jpeg window */
#endif

#ifdef HAVE_TIFF
  if (tiffUp) TIFFDialog(0);  /* close tiff window */
#endif

  ClosePopUp();

  /* make the interrupt signal look like a '\n' keypress in ctrlW */
  FakeKeyPress(ctrlW, XK_Return);

  frominterrupt = 1;
}


