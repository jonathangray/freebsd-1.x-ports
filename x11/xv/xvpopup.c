/* 
 * xvpopup.c - popup "Are you sure?  Yes/No/Maybe" sort of dialog box
 *
 * callable functions:
 *
 *   CenterMapWindow(win,x,y)  -  maps and centers a window around the mouse
 *   PopUp(str,...)        -  maps, sets up popW
 *   ErrPopUp(str,str)     -  maps, sets up popW
 *   GetStrPopUp(...)      -  opens a 1-line, editable text popup window
 *   ClosePopUp()          -  closes pop-up or alert window, if open
 *   OpenAlert(str)        -  maps a button-less window
 *   CloseAlert()          -  closes a button-less window
 *   PUCheckEvent(event)   -  called by event handler
 *   TextRect()            -  draws semi-complex strings in a rectangle
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

#define PUWIDE 400
#define PUHIGH 170
#define BUTTH   24

#ifdef __STDC__
static int  doPopUp(char *, char **, int, int, char *);
static void attachPUD(void);
static void createPUD(void);
static void drawPUD(int, int, int, int);
static void clickPUD(int, int);
static void doGetStrKey(int);
static int  doGSKey(int);
static void changedGSBuf(void);
static void drawGSBuf(void);
#else
static int  doPopUp(), doGSKey();
static void attachPUD(), createPUD(), drawPUD(), clickPUD(), doGetStrKey();
static void changedGSBuf(), drawGSBuf();
#endif


/* values 'popUp' can take */
#define ISPOPUP  1
#define ISALERT  2
#define ISGETSTR 3


/* local variables */
Window popW;
int    nbts, selected, popUp=0, firsttime=1;
BUTT  *bts;
char  *text;
char   accel[8];

char *gsBuf, *gsFilter;       /* stuff needed for GetStrPopUp() handling */
int   gsBufLen, gsAllow, gsCurPos, gsStPos, gsEnPos;
int   gsx, gsy, gsw, gsh;




/***************************************************/
void CenterMapWindow(win, dx, dy, w, h)
     Window win;
     int    dx, dy, w, h;
{
  XSizeHints hints;
  Window       rW,cW;
  int          rx,ry,x,y,wx,wy;
  unsigned int mask;


  if (!XQueryPointer(theDisp,rootW,&rW,&cW,&rx,&ry,&x,&y,&mask)) {
    /* couldn't query mouse.  just center on screen */
    wx = (dispWIDE-w)/2;   wy = (dispHIGH-h)/2;
  }
  else {
    wx = x - dx;
    wy = y - dy;
    if (wx<0) wx = 0;
    if (wy<0) wy = 0;
    if (wx + w > dispWIDE) wx = dispWIDE - w;
    if (wy + h > dispHIGH) wy = dispHIGH - h;
  }

  wx -= (p_offx + ch_offx);
  wy -= (p_offy + ch_offy);

  if (!XGetNormalHints(theDisp, win, &hints)) hints.flags = 0;
  hints.width  = hints.min_width  = hints.max_width  = w;
  hints.height = hints.min_height = hints.max_height = h;
  hints.x = wx;  hints.y = wy;
  hints.flags  |= (USSize | PMinSize | PMaxSize | USPosition);
  XSetNormalHints(theDisp, win, &hints);

  XMoveWindow(theDisp, win, wx, wy);
  XMapRaised(theDisp, win);
}


/***************************************************/
int PopUp(txt, labels, n)
     char *txt, *labels[];
     int   n;
{
  return doPopUp(txt, labels, n, ISPOPUP, "xv confirm");
}

/***************************************************/
static int doPopUp(txt, labels, n, poptyp, wname)
     char *txt, *labels[], *wname;
     int   n, poptyp;
{
  int    i;
  XEvent event;

  if (firsttime) createPUD();

  XStoreName(theDisp, popW, wname);
  XSetIconName(theDisp, popW, wname);
  attachPUD();

  bts = (BUTT *) malloc(n * sizeof(BUTT));
  if (!bts) FatalError("unable to malloc buttons in popup\n");
  nbts = n;
  selected = 0;
  text = txt;

  for (i=0; i<n; i++) {
    BTCreate(&bts[i], popW, PUWIDE - (n-i) * (80 + 10), PUHIGH - 10 - BUTTH,
	     80, BUTTH, labels[i]+1, infofg, infobg, hicol, locol);
    accel[i] = labels[i][0];
  }

  /* center first button in window around mouse position, with constraint that 
     window be fully on the screen */

  CenterMapWindow(popW, 40 + bts[0].x, BUTTH/2 + bts[0].y,
		  PUWIDE, PUHIGH);
  popUp = poptyp;

  /* MUST wait for VisibilityNotify event to come in, else we run the risk
     of UnMapping the window *before* the Map request completed.  This 
     appears to be bad, (It leaves an empty window frame up.) though it
     generally only happens on slow servers.  Better safe than screwed... */

  XWindowEvent(theDisp, popW, VisibilityChangeMask, &event);

  /* block until this window gets closed */
  while (popUp) {
    XNextEvent(theDisp, &event);
    HandleEvent(&event, &i);
  }

  /* free stuff */
  XUnmapWindow(theDisp, popW);
  free(bts);

  return(selected);
}


/***************************************************/
void ErrPopUp(txt, label)
     char *txt, *label;
{
  /* simplified interface to PopUp.  Takes a string and the label for the
     (one) button */

  PopUp(txt, &label, 1);
}


/***************************************************/
int GetStrPopUp(txt, labels, n, buf, buflen, filstr, allow)
     char *txt, *labels[], *buf, *filstr;
     int   n, buflen, allow;
{
  /* pops up a window with a prompt string, a 1-line editable
     text thingy, and a row of buttons.  'txt' is the prompt
     string, 'labels' are the labels for the buttons, 'n' is the
     number of buttons, 'buf' is the buffer displayed and edited
     in the window, buflen is its length, filstr is a filter string, of
     characters to block from entry (or, if 'allow' is '1', a list
     of the *only* characters allowed for entry)

     It returns the index of the button clicked on.  Note that the
     button labels have 1-character accellerators at the front, same
     as in PopUp().  Note that it would be suboptimal to make any
     of the 1-character accellerators be the same character as one of
     the edit-text command keys 

     Also note that the filter string should only contain normal printable
     characters (' ' through '\177'), as ctrl chars are pre-filtered 
     (ie, interpreted as emacs-like commands) */

  gsBuf = buf;        gsBufLen = buflen;
  gsFilter = filstr;  gsAllow = allow;

  gsCurPos = strlen(gsBuf);
  gsStPos = gsEnPos = 0;

  gsh = LINEHIGH+5;
  gsx = 10 + icon_width + 20;
  gsy = 10+(PUHIGH-30-BUTTH-gsh)/2;

  if (strlen(txt) > 60)
    gsy = PUHIGH - 10 - BUTTH - 10 - gsh - 20;

  gsw = PUWIDE - gsx - 10;
  
  changedGSBuf();      /* careful!  popW doesn't exist yet! */

  return doPopUp(txt, labels, n, ISGETSTR, "xv prompt");
}


/***************************************************/
void ClosePopUp()
{
  /* closes popW:  if it's a pop-up, returns 'cancel'.  If it's an alert,
     simply closes it */

  if      (popUp == ISALERT) CloseAlert();
  else if (popUp == ISPOPUP) {
    popUp = 0;
    selected = nbts-1;
  }
}


/***************************************************/
void OpenAlert(txt)
     char *txt;
{
  /* pops up a window with txt displayed in it (*no buttons*).  
     returns immediately.  window is closed by 'CloseAlert()'.
     No 'PopUp()' calls are allowed while an Alert is displayed. */

  XEvent event;

  if (firsttime) createPUD();

  XStoreName(theDisp, popW, "xv notice");
  XSetIconName(theDisp, popW, "xv notice");
  attachPUD();

  nbts = 0;
  selected = 0;
  text = txt;

  /* center last button in window around mouse position, with constraint that 
     window be fully on the screen */

  CenterMapWindow(popW, PUWIDE/2, PUHIGH/2, PUWIDE, PUHIGH);
  popUp = ISALERT;

  /* MUST wait for VisibilityNotify event to come in, else we run the risk
     of UnMapping the window *before* the Map request completed.  This 
     appears to be bad, (It leaves an empty window frame up.) though it
     generally only happens on slow servers.  Better safe than screwed... */

  XWindowEvent(theDisp, popW, VisibilityChangeMask, &event);
  drawPUD(0, 0, PUWIDE, PUHIGH);
  XFlush(theDisp);
}


/***************************************************/
void CloseAlert()
{
  popUp = 0;
  XUnmapWindow(theDisp, popW);
}


/***************************************************/
int PUCheckEvent(xev)
XEvent *xev;
{
  /* check event to see if it's for us.  If so, return 1, otherwise 0 */

  int rv = 0;

  if (!popUp) return(0);

  if (xev->type == Expose) {
    XExposeEvent *e = (XExposeEvent *) xev;
    if (e->window == popW) {
      drawPUD(e->x, e->y, e->width, e->height);
      rv = 1;
    }
  }

  else if (xev->type == ButtonPress) {
    XButtonEvent *e = (XButtonEvent *) xev;

    if (e->button == Button1 && e->window == popW) {
      clickPUD(e->x,e->y);
      rv = 1;
    }
  }


  else if (xev->type == KeyPress) {
    XKeyEvent *e = (XKeyEvent *) xev;
    char buf[128];  KeySym ks;
    int stlen, i;
	
    stlen = XLookupString(e,buf,128,&ks,(XComposeStatus *) NULL);
    buf[stlen] = '\0';

    /* note: we accept keyboard accellerators in *any* window */
    if (stlen) {
      if (buf[0] == '\r') buf[0] = '\n';

      /* search for character in accel table */
      for (i=0; i<nbts; i++) {
	if (buf[0] == accel[i] && buf[0] != ' ') {
	  FakeButtonPress(&bts[i]);
	  rv = 1;
	}
      }

      if (!rv && buf[0]=='\033' && nbts==1) { /* ESC accepted in 1-but pu's */
	FakeButtonPress(&bts[0]);
	rv = 1;
      }

      if (!rv && popUp == ISGETSTR) {     /* not an accel. key.  eat it */
	if (e->window == popW) {
	  doGetStrKey(buf[0]);
	  rv = 1;
	}
      }
    }
    else {     /* stlen==0:  shift, ctrl, arrow keys, etc. */
      if (popUp == ISGETSTR) {
	if (ks==XK_Left)  doGetStrKey('\002');
	if (ks==XK_Right) doGetStrKey('\006');
      }

      rv = 1;  /* eat all non-string-producing keys */
    }
  }

  else if (xev->type == ClientMessage) {
    Atom proto, delwin;
    XClientMessageEvent *client_event = (XClientMessageEvent *) xev;

    proto  = XInternAtom(theDisp, "WM_PROTOCOLS", FALSE);
    delwin = XInternAtom(theDisp, "WM_DELETE_WINDOW", FALSE);

    if (client_event->message_type == proto &&
	client_event->data.l[0]    == delwin) {
      /* it's a WM_DELETE_WINDOW event */

      if (client_event->window == popW) {
	FakeButtonPress(&bts[(nbts>1) ? nbts-1 : 0]);
	rv = 1;
      }
    }
  }

  if (rv==0 && (xev->type == KeyPress || xev->type == ButtonPress)) {
    XBell(theDisp, 0);
    rv = 1;            /* eat it */
  }

  return rv;
}



#define TR_MAXLN 10

/***************************************************/
void TextRect(win, txt, x, y, w, h, fg)
     Window  win;
     char   *txt;
     int     x,y,w,h;
     u_long  fg;
{
  char *sp, *ep, *oldep, *start[TR_MAXLN];
  int   i, inbreak, lineno, top, hardcr, maxln, len[TR_MAXLN];

  XSetForeground(theDisp, theGC, fg);
  
  sp = txt;  lineno = hardcr = 0;

  maxln = h / LINEHIGH;  
  RANGE(maxln,0,TR_MAXLN);
  while (*sp && lineno<maxln) {

    /* drop off any leading spaces (except on first line or after \n) */
    if (sp!=txt && !hardcr) {
      while(*sp==' ') sp++;
    }

    hardcr = 0;   ep = sp;

    /* increment ep until we   A) get too wide, B) hit eos or
       C) hit a '\n' character */

    /* NOTE: ep points to the character AFTER the end of the line */

    while (XTextWidth(mfinfo, sp, ep-sp) <= w && *ep && *ep!='\n') ep++;
    if (*ep=='\n') { ep++;  hardcr=1; }   /* eat newline */

    /* if we got too wide, back off until we find a break position 
       (last char before a space or a '/') */

    if (XTextWidth(mfinfo, sp, ep-sp) > w) {
      oldep = ep;  inbreak = 0;
      while (ep!=sp) {
	ep--;
	if ( inbreak && *ep!=' ') { ep++;  break; }
	if (!inbreak && *ep==' ') inbreak = 1;
	if (*ep=='/') { ep++; break; }
      }
      if (ep==sp) ep = oldep-1;  /* can't break this line.  oh well */
    }

    start[lineno] = sp;  len[lineno] = ep-sp;
    
    /* make sure we don't print a trailing '\n' character! */
    if (len[lineno] > 0) {
      while (sp[len[lineno]-1] == '\n') len[lineno] = len[lineno] - 1;
    }

    sp = ep;
    lineno++;
  }

  top = y + h/2 + (ASCENT-DESCENT)/2 - ((lineno-1)*LINEHIGH)/2;
  if (top<y+ASCENT) top = y+ASCENT;

  for (i=0, y=top; i<lineno; i++, y+=LINEHIGH) {
    if (start[i][0] != '\n')
      XDrawString(theDisp, win, theGC, x, y, start[i], len[i]);
  }
}


/***************************************************/
static void createPUD()
{
  popW = CreateWindow("xv confirm", "XVconfirm", "+0+0", 
		      PUWIDE, PUHIGH, infofg, infobg, 0);
  if (!popW) FatalError("can't create popup window!");

  XSelectInput(theDisp, popW, ExposureMask | ButtonPressMask | KeyPressMask
	       | VisibilityChangeMask);
  /* XSetTransientForHint(theDisp, popW, mainW); */

  bts = (BUTT *) NULL;
  nbts = selected = firsttime = 0;
}
  

/***************************************************/
static void attachPUD()
{
  /* used to make PUD a transient window of something.  Doesn't
     do anything anymore, as I got tired of having window layering
     shifted around everytime a popup window happened.  Screw the
     business about having the popup iconify when you iconify the
     appropriate XV window.  There generally ISN'T an appropriate
     XV window... */
}


/***************************************************/
static void drawPUD(x,y,w,h)
int x,y,w,h;
{
  int  i,xt,yt;
  XRectangle xr;

  xr.x = x;  xr.y = y;  xr.width = w;  xr.height = h;
  XSetClipRectangles(theDisp, theGC, 0,0, &xr, 1, Unsorted);

  XSetForeground(theDisp, theGC, infofg);
  XSetBackground(theDisp, theGC, infobg);

  XCopyPlane(theDisp, iconPix, popW, theGC, 0,0, icon_width, icon_height,
	     10,10+(PUHIGH-30-BUTTH-icon_height)/2,1);

  xt = 10+icon_width+20;  yt = 10;

  if (popUp == ISGETSTR) {
    TextRect(popW, text, xt, yt, PUWIDE-10-xt, gsy-20, infofg);
    drawGSBuf();
  }
  else 
    TextRect(popW, text, xt, yt, PUWIDE-10-xt, PUHIGH-10-BUTTH-20, infofg);

  for (i=0; i<nbts; i++) BTRedraw(&bts[i]);

  XSetClipMask(theDisp, theGC, None);
}


/***************************************************/
static void clickPUD(x,y)
int x,y;
{
  int i;
  BUTT *bp;

  for (i=0; i<nbts; i++) {
    bp = &bts[i];
    if (PTINRECT(x, y, bp->x, bp->y, bp->w, bp->h)) break;
  }

  if (i<nbts && BTTrack(bp)) {
    popUp = 0;  selected = i;
  }
}



/***************************************************/
static void doGetStrKey(c)
     int c;
{
  if (doGSKey(c)) XBell(theDisp, 0);
}


/***************************************************/
static int doGSKey(c)
     int c;
{
  /* handle characters typed at GetStrPopUp window.  Button accel. keys
     have already been checked for elsewhere.  Practical upshot is that
     we don't have to do anything with ESC or Return (as these will normally
     be Cancel and Ok buttons) 
 
     Normally returns '0'.  Returns '1' if character wasn't accepted, for
     whatever reason. */

  int i, len, flen;

  len = strlen(gsBuf);
  if (gsFilter) flen = strlen(gsFilter);
           else flen = 0;
  

  if (c>=' ' && c<'\177') {              /* 'NORMAL' CHARACTERS */
    if (flen) {                          /* check filter string */
      for (i=0; i<flen && c!=gsFilter[i]; i++);
      if (!gsAllow && i< flen) return 1;    /* found in 'disallow' filter */
      if ( gsAllow && i==flen) return 1;    /* not found in 'allow' filter */
    }
    
    if (len >= gsBufLen-1) return 1;     /* at max length */

    xvbcopy(&gsBuf[gsCurPos], &gsBuf[gsCurPos+1], len-gsCurPos+1);
    gsBuf[gsCurPos]=c;  gsCurPos++;
  }


  else if (c=='\010' || c=='\177') {    /* BS or DEL */
    if (gsCurPos==0) return 1;                     /* at beginning of str */
    xvbcopy(&gsBuf[gsCurPos], &gsBuf[gsCurPos-1], len-gsCurPos+1);
    gsCurPos--;
  }

  else if (c=='\025') {                 /* ^U: clear entire line */
    gsBuf[0] = '\0';
    gsCurPos = 0;
  }

  else if (c=='\013') {                 /* ^K: clear to end of line */
    gsBuf[gsCurPos] = '\0';
  }

  else if (c=='\001') {                 /* ^A: move to beginning */
    gsCurPos = 0;
  }

  else if (c=='\005') {                 /* ^E: move to end */
    gsCurPos = len;
  }

  else if (c=='\004') {                 /* ^D: delete character at gsCurPos */
    if (gsCurPos==len) return 1;
    xvbcopy(&gsBuf[gsCurPos+1], &gsBuf[gsCurPos], len-gsCurPos);
  }

  else if (c=='\002') {                 /* ^B: move backwards char */
    if (gsCurPos==0) return 1;
    gsCurPos--;
  }

  else if (c=='\006') {                 /* ^F: move forwards char */
    if (gsCurPos==len) return 1;
    gsCurPos++;
  }

  else return 1;                        /* unhandled character */

  changedGSBuf();      /* compute gsEnPos, gsStPos */

  if (ctrlColor) XClearArea(theDisp, popW, gsx+3,gsy+3,gsw-5,gsh-5,False);
            else XClearArea(theDisp, popW, gsx+1,gsy+1,gsw-1,gsh-1,False);

  drawGSBuf();

  /* if we have a string of any sort, turn on the default '\n' button
     (if there is one) */
  for (i=0; i<nbts && accel[i]!='\n'; i++);
  if (i<nbts) BTSetActive(&bts[i], strlen(gsBuf));

  return(0);
}



/***************************************************/
static void changedGSBuf()
{
  /* cursor position (or whatever) may have changed.  adjust displayed 
     portion of gsBuf */

  int len;

  len = strlen(gsBuf);

  if (gsCurPos < gsStPos) gsStPos = gsCurPos;
  if (gsCurPos > gsEnPos) gsEnPos = gsCurPos;

  if (gsStPos>len) gsStPos = (len>0) ? len-1 : 0;
  if (gsEnPos>len) gsEnPos = (len>0) ? len-1 : 0;

  /* while substring is shorter than window, inc enPos */

  while (XTextWidth(mfinfo, &gsBuf[gsStPos], gsEnPos-gsStPos) < (gsw-6)
	 && gsEnPos<len) { gsEnPos++; }

  /* while substring is longer than window, dec enpos, unless enpos==curpos,
     in which case, inc stpos */

  while (XTextWidth(mfinfo, &gsBuf[gsStPos], gsEnPos-gsStPos) > (gsw-6)) {
    if (gsEnPos != gsCurPos) gsEnPos--;
    else gsStPos++;
  }
}


/***************************************************/
static void drawGSBuf()
{
  /* draw edittext thingy in GetStrPopUp window */

  int cpos;

  XSetForeground(theDisp, theGC, infofg);
  XDrawRectangle(theDisp, popW, theGC, gsx, gsy, gsw, gsh);
  Draw3dRect(popW, gsx+1, gsy+1, gsw-2, gsh-2, R3D_IN, 2, hicol,locol,infobg);

  XSetForeground(theDisp, theGC, infofg);

  if (gsStPos>0) {  /* draw a "there's more over here" doowah */
    XDrawLine(theDisp, popW, theGC, gsx+1, gsy+1, gsx+1, gsy + gsh-1);
    XDrawLine(theDisp, popW, theGC, gsx+2, gsy+1, gsx+2, gsy + gsh-1);
    XDrawLine(theDisp, popW, theGC, gsx+3, gsy+1, gsx+3, gsy + gsh-1);
  }

  if (gsEnPos<strlen(gsBuf)) {  /* draw a "there's more over here" doowah */
    XDrawLine(theDisp, popW, theGC, gsx+gsw-3, gsy+1, gsx+gsw-3, gsy+gsh-1);
    XDrawLine(theDisp, popW, theGC, gsx+gsw-2, gsy+1, gsx+gsw-2, gsy+gsh-1);
    XDrawLine(theDisp, popW, theGC, gsx+gsw-1, gsy+1, gsx+gsw-1, gsy+gsh-1);
  }

  XDrawString(theDisp, popW, theGC, gsx+4, gsy+ASCENT+4,
	      gsBuf+gsStPos, gsEnPos-gsStPos);

  cpos = gsx+XTextWidth(mfinfo, &gsBuf[gsStPos], gsCurPos-gsStPos);
  XDrawLine(theDisp,popW,theGC, 4+cpos, gsy+3,         4+cpos, gsy+2+CHIGH+1);
  XDrawLine(theDisp,popW,theGC, 4+cpos, gsy+2+CHIGH+1, 6+cpos, gsy+2+CHIGH+3);
  XDrawLine(theDisp,popW,theGC, 4+cpos, gsy+2+CHIGH+1, 2+cpos, gsy+2+CHIGH+3);
}
