/*
 * xvimage.c - image manipulation functions (crop,resize,rotate...) for XV
 *
 *  Author:    John Bradley, University of Pennsylvania
 *                (bradley@cis.upenn.edu)
 *
 *  Contains:
 *            void Resize(int, int)
 *            void GenerateEpic(w,h)
 *            void Crop()
 *            void UnCrop()
 *            void AutoCrop()
 *            void DoCrop(x,y,w,h)
 *            void Rotate(int)
 *            void RotatePic();
 *            void InstallNewPic(void);
 *            void DrawEpic(void);
 *            byte *FSDither()
 *            void CreateXImage()
 *            void Set824Menus( pictype );
 *            void Change824Mode( pictype );
 *            void ConvertPics824(oldtype, newtype);
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


#ifdef __STDC__
static void do_zoom(int, int);
static void compute_zoom_crop(int, int);
static void do_unzoom(void);
static void crop1(int, int, int, int, int);
static int  doAutoCrop24(void);
static void FloydDitherize1(XImage *, byte *, int, int, int, 
			    byte *, byte *,byte *);
static int  highbit(unsigned long);
#else
static void do_zoom(), compute_zoom_crop(), do_unzoom();
static void crop1();
static int  doAutoCrop24();
static void FloydDitherize1();
static int  highbit();
#endif


#define DO_CROP 0
#define DO_ZOOM 1


/***********************************/
void Resize(w,h)
int w,h;
{
  static char *rstr = "Resizing Image.  Please wait...";

  RANGE(w,1,maxWIDE);  RANGE(h,1,maxHIGH);

  if (psUp) PSResize();   /* if PSDialog is open, mention size change  */

  /* if same size, and Ximage created, do nothing */
  if (w==eWIDE && h==eHIGH && theImage!=NULL) return;

  if (DEBUG) fprintf(stderr,"Resize(%d,%d)  eSIZE=%d,%d  cSIZE=%d,%d\n",
		     w,h,eWIDE,eHIGH,cWIDE,cHIGH);

  BTSetActive(&but[BCROP],0);
  SetCropString(but[BCROP].active);

  if (epicMode == EM_SMOOTH) {  /* turn off smoothing */
    epicMode = EM_RAW;  SetEpicMode();
  }

  GenerateEpic(w,h);
  CreateXImage();
}
                


/***********************************/
void GenerateEpic(w,h)
int w,h;
{
  int          cy,ex,ey,*cxarr, *cxarrp;
  byte        *clptr,*elptr,*epptr;

  WaitCursor();
  clptr = NULL;  cxarrp = NULL;  cy = 0;  /* shut up compiler */

  SetISTR(ISTR_EXPAND, "%.5g%% x %.5g%%  (%d x %d)",
	  100.0 * ((float) w) / cWIDE, 
	  100.0 * ((float) h) / cHIGH, w, h);

  if (DEBUG) 
    fprintf(stderr,"GenerateEpic(%d,%d) eSIZE=%d,%d cSIZE=%d,%d epicode=%d\n",
		     w,h,eWIDE,eHIGH,cWIDE,cHIGH, epicMode);


  FreeEpic();                   /* get rid of the old one */
  eWIDE = w;  eHIGH = h;


  if (epicMode == EM_SMOOTH) {  
    if (picType == PIC8) {
      epic = SmoothResize(cpic, cWIDE, cHIGH, eWIDE, eHIGH,
			  rMap,gMap,bMap, rdisp,gdisp,bdisp, numcols);
    }
    else {  /* PIC24 */
      epic = Smooth24(cpic, 1, cWIDE, cHIGH, eWIDE, eHIGH, NULL, NULL, NULL);
    }

    if (epic) return;   /* success */
    else {
      /* failed.  Try to generate a *raw* image, at least... */
      epicMode = EM_RAW;  SetEpicMode();
      /* fall through to rest of code */
    }
  }


  /* generate a 'raw' epic, as we'll need it for ColorDither if EM_DITH */
    
  if (eWIDE==cWIDE && eHIGH==cHIGH) {  /* 1:1 expansion.  point epic at cpic */
    epic = cpic;
  }
  else {
    /* run the rescaling algorithm */
    int bperpix;

    bperpix = (picType == PIC8) ? 1 : 3;

    WaitCursor();

    /* create a new epic of the appropriate size */

    epic = (byte *) malloc(eWIDE * eHIGH * bperpix);
    if (!epic) FatalError("GenerateEpic():  unable to malloc 'epic'");

    /* the scaling routine.  not really all that scary after all... */

    /* OPTIMIZATON:  Malloc an eWIDE array of ints which will hold the
       values of the equation px = (pWIDE * ex) / eWIDE.  Faster than doing 
       a mul and a div for every point in picture */

    cxarr = (int *) malloc(eWIDE * sizeof(int));
    if (!cxarr) FatalError("unable to allocate cxarr");

    for (ex=0; ex<eWIDE; ex++) 
      cxarr[ex] = bperpix * ((cWIDE * ex) / eWIDE);

    elptr = epptr = epic;

    for (ey=0;  ey<eHIGH;  ey++, elptr+=(eWIDE*bperpix)) {
      if ((ey&127) == 0) WaitCursor();
      cy = (cHIGH * ey) / eHIGH;
      epptr = elptr;
      clptr = cpic + (cy * cWIDE * bperpix);

      if (bperpix == 1) {
	for (ex=0, cxarrp = cxarr;  ex<eWIDE;  ex++, epptr++) 
	  *epptr = clptr[*cxarrp++];
      }
      else {
	int j;  byte *cp;

	for (ex=0, cxarrp = cxarr; ex<eWIDE; ex++,cxarrp++) {
	  cp = clptr + *cxarrp;
	  for (j=0; j<bperpix; j++) 
	    *epptr++ = *cp++;
	}
      }
    }
    free(cxarr);
  }


  /* at this point, we have a raw epic.  Potentially dither it */
  if (picType == PIC8 && epicMode == EM_DITH) {
    byte *tmp;

    tmp = DoColorDither(NULL, epic, eWIDE, eHIGH, rMap,gMap,bMap, 
			rdisp,gdisp,bdisp, numcols);
    if (tmp) {  /* success */
      FreeEpic();
      epic = tmp;
    }
    else {  /* well... just use the raw image. */
      epicMode = EM_RAW;  SetEpicMode();
    }
  }
}
                


/***********************************/
void DoZoom(x,y,button)
     int x,y,button;
{
  if      (button == Button1) do_zoom(x,y);
  else if (button == Button3) do_unzoom();
  else XBell(theDisp,0);
}


/***********************************/
static void do_zoom(mx,my)
     int mx,my;
{
  int i,w,h,x,y,x2,y2,ox,oy;

  /* if there's already a cropping rectangle drawn, turn it off */
  if (but[BCROP].active) InvCropRect();

  ox = mx;  oy = my;
  compute_zoom_crop(mx,my);
  InvCropRect();

  /* track until Button1 is released */
  while (1) {
    Window rW, cW;  unsigned int mask;  int rx, ry;
    if (XQueryPointer(theDisp, mainW, &rW, &cW, &rx, &ry, &mx, &my, &mask)) {
      if (!(mask & Button1Mask)) break;  /* button released */
      
      if (mx!=ox || my!=oy) {
	InvCropRect();
	compute_zoom_crop(mx,my);
	InvCropRect();
	ox = mx;  oy = my;
      }
    }
  }
      
  for (i=0; i<6; i++) {
    InvCropRect();
    XFlush(theDisp);
    Timer(150);
  }


  /* figure out what the crop rectangles coordinates are in pic coordinates */
  x = cXOFF + (crx1 * cWIDE) / eWIDE;
  y = cYOFF + (cry1 * cHIGH) / eHIGH;
  x2 = cXOFF + (crx2 * cWIDE) / eWIDE;
  y2 = cYOFF + (cry2 * cHIGH) / eHIGH;
  w = (x2 - x);
  h = (y2 - y);

  if (w<1) w = 1;
  if (x+w > pWIDE) w = pWIDE - x;
  if (h<1) h = 1;
  if (y+h > pHIGH) h = pHIGH - y;


  crop1(x,y,w,h, DO_ZOOM);
}


/***********************************/
static void compute_zoom_crop(x,y)
     int x,y;
{
  int w,h;

  w = eWIDE/2;  h = eHIGH/2;
  crx1 = x - w/2;  cry1 = y - h/2;  
  if (crx1 < 0) crx1 = 0;
  if (cry1 < 0) cry1 = 0;
  if (crx1 > eWIDE-w) crx1 = eWIDE-w;
  if (cry1 > eHIGH-h) cry1 = eHIGH-h;
  
  crx2 = crx1 + w;
  cry2 = cry1 + h;
}


/***********************************/
static void do_unzoom()
{
  int x,y,w,h;

  /* compute a cropping rectangle (in screen coordinates) that's twice 
     the size of eWIDE,eHIGH, centered around eWIDE/2, eHIGH/2, but no
     larger than pWIDE,PHIGH */

  if (!but[BUNCROP].active) {    /* not cropped, can't zoom out */
    XBell(theDisp, 0);
    return;
  }

  crx1 = -eWIDE/2;   cry1 = -eHIGH/2;

  /* figure out what the crop rectangles coordinates are in pic coordinates */
  x = cXOFF + (crx1 * cWIDE - (eWIDE/2)) / eWIDE;
  y = cYOFF + (cry1 * cHIGH - (eHIGH/2)) / eHIGH;
  w = cWIDE*2;
  h = cHIGH*2;
  RANGE(w, 1, pWIDE);
  RANGE(h, 1, pHIGH);

  if (x<0) x = 0;
  if (y<0) y = 0;
  if (x+w > pWIDE) x = pWIDE - w;
  if (y+h > pHIGH) y = pHIGH - h;

  crop1(x,y,w,h, DO_ZOOM);
}


/***********************************/
void Crop()
{
  int i, x, y, x2, y2, w, h;

  if (!but[BCROP].active) return;

  /* turn off the cropping rectangle */
  InvCropRect();  BTSetActive(&but[BCROP],0);

  /* sort crx1,crx2,cry1,cry2 so that crx1,cry1 are top left corner */
  if (crx1>crx2) { i = crx1; crx1 = crx2; crx2 = i; }
  if (cry1>cry2) { i = cry1; cry1 = cry2; cry2 = i; }

  /* see if cropping to same size, in which case do nothing */
  if (crx2-crx1 == eWIDE && cry2-cry1 == eHIGH) return;

  /* figure out what the crop rectangles coordinates are in pic coordinates */
  x = cXOFF + (crx1 * cWIDE) / eWIDE;
  y = cYOFF + (cry1 * cHIGH) / eHIGH;
  x2 = cXOFF + (crx2 * cWIDE) / eWIDE;
  y2 = cYOFF + (cry2 * cHIGH) / eHIGH;
  w = (x2 - x) + 1;
  h = (y2 - y) + 1;

  if (w<1) w = 1;
  if (x+w > pWIDE) w = pWIDE - x;
  if (h<1) h = 1;
  if (y+h > pHIGH) h = pHIGH - y;

  crop1(x,y,w,h,DO_CROP);
}


/**********************************/
static void crop1(x,y,w,h,zm)
int x,y,w,h,zm;
{
  int   i,j,oldew,oldeh;
  byte *cp, *pp;

  oldew = eWIDE;  oldeh = eHIGH;

  DoCrop(x,y,w,h);
  if (zm == DO_ZOOM) { eWIDE = oldew;  eHIGH = oldeh; }

  GenerateEpic(eWIDE, eHIGH);

  if (useroot) DrawEpic();
  else {
    if (zm == DO_CROP) {
      WCrop(eWIDE, eHIGH);  /* shrink window */
      CreateXImage();
    }
    else DrawEpic();
  }
  
  SetCursors(-1);
}


/***********************************/
void UnCrop()
{
  int w,h;

  if (cpic == pic) return;     /* not cropped */

  BTSetActive(&but[BUNCROP],0);
  
  if (epicMode == EM_SMOOTH) {   /* turn off smoothing */
    epicMode = EM_RAW;  SetEpicMode();
  }

  /* dispose of old cpic and epic */
  FreeEpic();
  if (cpic && cpic !=  pic) free(cpic);
  cpic = NULL;
  

  w = (pWIDE * eWIDE) / cWIDE;   h = (pHIGH * eHIGH) / cHIGH;
  if (w>maxWIDE || h>maxHIGH) {
    /* return to 'normal' size */
    if (pWIDE>maxWIDE || pHIGH>maxHIGH) {
      double r,wr,hr;
      wr = ((double) pWIDE) / maxWIDE;
      hr = ((double) pHIGH) / maxHIGH;

      r = (wr>hr) ? wr : hr;   /* r is the max(wr,hr) */
      w = (int) ((pWIDE / r) + 0.5);
      h = (int) ((pHIGH / r) + 0.5);
    }
    else { w = pWIDE;  h = pHIGH; }
  }

  cpic = pic;  cXOFF = cYOFF = 0;  cWIDE = pWIDE;  cHIGH = pHIGH;


  /* generate an appropriate 'epic' */
  GenerateEpic(w,h);
  CreateXImage();


  WUnCrop();
  SetCropString(but[BCROP].active);
}
  

/***********************************/
void AutoCrop()
{
  /* called when AutoCrop button is pressed */

  if (DoAutoCrop()) {
    if (useroot) DrawEpic();
    else {
      CreateXImage();
      WCrop(eWIDE, eHIGH);
    }
  }
  
  SetCursors(-1);
}


/***********************************/
int DoAutoCrop()
{
  /* returns '1' if any cropping was actually done. */

  byte *cp, *cp1;
  int  i, ctop, cbot, cleft, cright, bperpix;
  byte bgcol;

  ctop = cbot = cleft = cright = 0;

  bperpix = (picType == PIC8) ? 1 : 3;

  if (picType == PIC24) return( doAutoCrop24() );


  /* crop the top */
  cp = cpic;
  bgcol = cp[0];

  while (ctop+1 < cHIGH) {
    /* see if we can delete this line */
    for (i=0, cp1=cp; i<cWIDE && *cp1==bgcol; i++, cp1++);
    if (i==cWIDE) { cp += cWIDE;  ctop++; }
    else break;
  }


  /* crop the bottom */
  cp = cpic + (cHIGH-1) * cWIDE;
  bgcol = cp[0];

  while (ctop + cbot + 1 < cHIGH) {
    /* see if we can delete this line */
    for (i=0, cp1=cp; i<cWIDE && *cp1==bgcol; i++,cp1++);
    if (i==cWIDE) { cp -= cWIDE;  cbot++; }
    else break;
  }


  /* crop the left side */
  cp = cpic;
  bgcol = cp[0];

  while (cleft + 1 < cWIDE) {
    /* see if we can delete this line */
    for (i=0, cp1=cp; i<cHIGH && *cp1==bgcol; i++, cp1 += cWIDE);
    if (i==cHIGH) { cp++; cleft++; }
    else break;
  }


  /* crop the right side */
  cp = cpic + cWIDE-1;
  bgcol = cp[0];

  while (cleft + cright + 1 < cWIDE) {
    /* see if we can delete this line */
    for (i=0, cp1=cp; i<cHIGH && *cp1==bgcol; i++, cp1 += cWIDE);
    if (i==cHIGH) { cp--; cright++; }
    else break;
  }

  /* do the actual cropping */
  if (cleft || ctop || cbot || cright) {
    DoCrop(cXOFF+cleft, cYOFF+ctop, 
	    cWIDE-(cleft+cright), cHIGH-(ctop+cbot));
    return 1;
  }

  return 0;
}


/***********************************/
static int doAutoCrop24()
{
  /* returns '1' if any cropping was actually done */

  byte *cp, *cp1;
  int  i, ctop, cbot, cleft, cright;
  byte bgR, bgG, bgB;

  ctop = cbot = cleft = cright = 0;

  if (picType != PIC24) FatalError("doAutoCrop24 called when pic!=PIC24");

  /* crop the top */
  cp = cpic;
  bgR = cp[0];  bgG = cp[1];  bgB = cp[2];

  while (ctop+1 < cHIGH) {  /* see if we can delete this line */
    for (i=0, cp1=cp; i<cWIDE && cp1[0]==bgR && cp1[1]==bgG && cp1[2]==bgB; 
	 i++, cp1+=3);

    if (i==cWIDE) { cp += cWIDE*3;  ctop++; }
    else break;
  }


  /* crop the bottom */
  cp = cpic + (cHIGH-1) * cWIDE*3;
  bgR = cp[0];  bgG = cp[1];  bgB = cp[2];

  while (ctop + cbot + 1 < cHIGH) {  /* see if we can delete this line */
    for (i=0, cp1=cp; i<cWIDE && cp1[0]==bgR && cp1[1]==bgG && cp1[2]==bgB; 
	 i++, cp1+=3);

    if (i==cWIDE) { cp -= cWIDE*3;  cbot++; }
    else break;
  }


  /* crop the left side */
  cp = cpic;
  bgR = cp[0];  bgG = cp[1];  bgB = cp[2];

  while (cleft + 1 < cWIDE) {  /* see if we can delete this line */
    for (i=0, cp1=cp; i<cHIGH && cp1[0]==bgR && cp1[1]==bgG && cp1[2]==bgB; 
	 i++, cp1 += (cWIDE * 3));

    if (i==cHIGH) { cp+=3; cleft++; }
    else break;
  }


  /* crop the right side */
  cp = cpic + (cWIDE-1) * 3;
  bgR = cp[0];  bgG = cp[1];  bgB = cp[2];

  while (cleft + cright + 1 < cWIDE) {  /* see if we can delete this line */
    for (i=0, cp1=cp; i<cHIGH && cp1[0]==bgR && cp1[1]==bgG && cp1[2]==bgB; 
	 i++, cp1 += (cWIDE*3));

    if (i==cHIGH) { cp-=3; cright++; }
    else break;
  }


  /* do the actual cropping */
  if (cleft || ctop || cbot || cright) {
    DoCrop(cXOFF+cleft, cYOFF+ctop, 
	    cWIDE-(cleft+cright), cHIGH-(ctop+cbot));
    return 1;
  }

  return 0;
}


/*******************************/
void DoCrop(x,y,w,h)
     int x,y,w,h;
{
  /* given a cropping rectangle in image coordinates, it regens cpic
     and sticks likely values into eWIDE,eHIGH, assuming you wanted to
     crop.  epic is not regnerated (but is freed) */

  int   i,j,k,bperpix;
  byte *cp, *pp;
  double expw, exph;


  bperpix = (picType == PIC8) ? 1 : 3;

  BTSetActive(&but[BCROP],0);

  /* get the cropping rectangle inside pic, if it isn't... */
  RANGE(x, 0, pWIDE-1);
  RANGE(y, 0, pHIGH-1);
  if (w<1) w=1;
  if (h<1) h=1;
  if (x+w > pWIDE) w = pWIDE-x;
  if (y+h > pHIGH) h = pHIGH-y;



  FreeEpic();
  if (cpic && cpic !=  pic) free(cpic);
  cpic = NULL;


  expw = (double) eWIDE / (double) cWIDE;
  exph = (double) eHIGH / (double) cHIGH;

  crx1 = (int) ((x - cXOFF) * expw);
  cry1 = (int) ((y - cYOFF) * exph);

  cXOFF = x;  cYOFF = y;  cWIDE = w;  cHIGH = h;
  if (DEBUG) fprintf(stderr,"DoCrop(): cropping to %dx%d rectangle at %d,%d\n",
		     cWIDE, cHIGH, cXOFF, cYOFF);


  if (cWIDE == pWIDE && cHIGH == pHIGH) {   /* not really cropping */
    cpic = pic;
    cXOFF = cYOFF = 0;
  }
  else {
    /* at this point, we want to generate cpic, which will contain a
       cWIDE*cHIGH subsection of 'pic', top-left at cXOFF,cYOFF */

    cpic = (byte *) malloc(cWIDE * cHIGH * bperpix);
    if (cpic == NULL) {
      fprintf(stderr,"%s: unable to allocate memory for cropped image\n", cmd);
      WUnCrop();
      cpic = pic;  cXOFF = cYOFF = 0;  cWIDE = pWIDE;  cHIGH = pHIGH;
      SetCropString(but[BCROP].active);
      return;
    }

    /* copy relevant pixels from pic to cpic */
    cp = cpic;
    for (i=0; i<cHIGH; i++) {
      pp = pic + (i+cYOFF) * (pWIDE*bperpix) + (cXOFF * bperpix);
      for (j=0; j<cWIDE*bperpix; j++) 
	*cp++ = *pp++;
    }
  }


  SetCropString(but[BCROP].active);
  BTSetActive(&but[BUNCROP], (cpic!=pic));

  eWIDE = (int) (cWIDE * expw);  
  eHIGH = (int) (cHIGH * exph);

  SetCursors(-1);
}



/***********************************/
void Rotate(dir)
int dir;
{
  /* called when rotate CW and rotate CCW controls are clicked */
  /* dir=0: clockwise, else counter-clockwise */

  DoRotate(dir);
  CreateXImage();
  WRotate();
}


/***********************************/
void DoRotate(dir)
int dir;
{
  int i;

  /* dir=0: 90 degrees clockwise, else 90 degrees counter-clockwise */
  WaitCursor();

  RotatePic(pic, picType, &pWIDE, &pHIGH, dir);

  /* rotate clipped version and modify 'clip' coords */
  if (cpic != pic && cpic != NULL) {
    if (!dir) {
      i = pWIDE - (cYOFF + cHIGH);      /* have to rotate offsets */
      cYOFF = cXOFF;
      cXOFF = i;
    }
    else {
      i = pHIGH - (cXOFF + cWIDE);
      cXOFF = cYOFF;
      cYOFF = i;
    }
    WaitCursor();
    RotatePic(cpic, picType, &cWIDE, &cHIGH,dir);
  }
  else { cWIDE = pWIDE;  cHIGH = pHIGH; }

  /* rotate expanded version */
  if (epic != cpic && epic != NULL) {
    WaitCursor();
    RotatePic(epic, picType, &eWIDE, &eHIGH,dir);
  }
  else { eWIDE = cWIDE;  eHIGH = cHIGH; }
}


/************************/
void RotatePic(pic, ptype, wp, hp, dir)
byte *pic;
int  *wp, *hp;
int   ptype, dir;
{
  /* rotates a w*h array of bytes 90 deg clockwise (dir=0) 
     or counter-clockwise (dir != 0).  swaps w and h */

  byte *pic1, *pix1, *pix;
  int          i,j,bperpix;
  unsigned int w,h;

  bperpix = (ptype == PIC8) ? 1 : 3;

  w = *wp;  h = *hp;  
  pix1 = pic1 = (byte *) malloc(w*h*bperpix);
  if (!pic1) FatalError("Not enough memory to rotate!");

  /* do the rotation */
  if (dir==0) {
    for (i=0; i<w; i++) {       /* CW */
      if (bperpix == 1) {
	for (j=h-1, pix=pic+(h-1)*w + i;  j>=0;  j--, pix1++, pix-=w) 
	  *pix1 = *pix;
      }
      else {
	int bperlin = w*bperpix;
	int k;

	for (j=h-1, pix=pic+(h-1)*w*bperpix + i*bperpix;  
	     j>=0;  j--, pix -= bperlin) 
	  for (k=0; k<bperpix; k++) *pix1++ = pix[k];
      }
    }
  }
  else {
    for (i=w-1; i>=0; i--) {    /* CCW */
      if (bperpix == 1) {
	for (j=0, pix=pic+i; j<h; j++, pix1++, pix+=w) 
	  *pix1 = *pix;
      }
      else {
	int k;
	int bperlin = w*bperpix;

	for (j=0, pix=pic+i*bperpix; j<h; j++, pix+=bperlin) 
	  for (k=0; k<bperpix; k++) *pix1++ = pix[k];
      }
    }
  }


  /* copy the rotated buffer into the original buffer */
  xvbcopy(pic1, pic, w*h*bperpix);

  free(pic1);

  /* swap w and h */
  *wp = h;  *hp = w;
}

  

/***********************************/
void Flip(dir)
int dir;
{
  /* dir=0: flip horizontally, else vertically
   *
   * Note:  flips pic, cpic, and epic.  Doesn't touch Ximage, nor does it draw
   */

  WaitCursor();
  FlipPic(pic, pWIDE, pHIGH, dir);

  /* flip clipped version */
  if (cpic && cpic != pic) {
    WaitCursor();
    FlipPic(cpic, cWIDE, cHIGH, dir);
  }

  /* flip expanded version */
  if (epic && epic != cpic) {
    WaitCursor();
    FlipPic(epic, eWIDE, eHIGH, dir);
  }
}


/************************/
void FlipPic(pic, w, h, dir)
byte *pic;
int w, h;
int dir;
{
  /* flips a w*h array of bytes horizontally (dir=0) or vertically (dir!=0) */

  byte *plin;
  int   i,j,k,l,bperpix,bperlin;

  bperpix = (picType == PIC8) ? 1 : 3;
  bperlin = w * bperpix;

  if (dir==0) {                /* horizontal flip */
    byte *leftp, *rightp;

    for (i=0; i<h; i++) {
      plin   = pic + i*bperlin;
      leftp  = plin;
      rightp = plin + (w-1)*bperpix;

      for (j=0; j<w/2; j++, rightp -= (2*bperpix)) {
	for (l=0; l<bperpix; l++, leftp++, rightp++) {
	  k = *leftp;  *leftp = *rightp;  *rightp = k;
	}
      }
    }
  }

  else {                      /* vertical flip */
    byte *topp, *botp;

    for (i=0; i<w; i++) {
      topp = pic + i*bperpix;
      botp = pic + (h-1)*bperlin + i*bperpix;

      for (j=0; j<h/2; j++, topp+=(w-1)*bperpix, botp-=(w+1)*bperpix) {
	for (l=0; l<bperpix; l++, topp++, botp++) {
	  k = *topp;  *topp = *botp;  *botp = k;
	}
      }
    }
  }
}

  


/************************/
void InstallNewPic()
{
  /* given a new pic and colormap, (or new 24-bit pic) installs everything,
     regens cpic and epic, and redraws image */

  /* toss old cpic and epic, if any */
  FreeEpic();
  if (cpic && cpic != pic) free(cpic);
  cpic = NULL;

  /* toss old colors, and allocate new ones */
  NewPicGetColors(0,0);

  /* generate cpic,epic,theImage from new 'pic' */
  crop1(cXOFF, cYOFF, cWIDE, cHIGH, DO_ZOOM);
  HandleDispMode();
}



/***********************************/
void DrawEpic()
{
  /* given an 'epic', builds a new Ximage, and draws it.  Basically
     called whenever epic is changed, or whenever color allocation 
     changes (ie, the created X image will look different for the 
     same epic) */

  CreateXImage();
  if (useroot) MakeRootPic();
          else DrawWindow(0,0,eWIDE,eHIGH);

  if (but[BCROP].active) InvCropRect();
}


/************************************/
void KillOldPics()
{
  /* throw away all previous images */

  FreeEpic();
  if (cpic && cpic != pic) free(cpic);
  if (pic) free(pic);
  xvDestroyImage(theImage);   theImage = NULL;
  pic = egampic = epic = cpic = NULL;

  if (picComments) free(picComments);
  picComments = (char *) NULL;
  ChangeCommentText();
}

  

/************************/
static void FloydDitherize1(ximage,pic824,ptype, wide, high, rmap, gmap, bmap)
     XImage *ximage;
     byte   *pic824, *rmap, *gmap, *bmap;
     int     ptype, wide, high;
{
  /* does floyd-steinberg ditherizing algorithm.  
   *
   * takes a wide*high input image, of type 'ptype' (PIC8, PIC24)
   *     (if PIC8, colormap is specified by rmap,gmap,bmap)
   *
   * output is a 1-bit per pixel XYBitmap, packed 8 pixels per byte
   *
   * Note: this algorithm is *only* used when running on a 1-bit display
   */

  register byte   pix8, bit;
  int            *thisline, *nextline;
  int            *thisptr, *nextptr, *tmpptr;
  int             i, j, err, bperpix, bperln, order;
  byte           *pp, *image, w1, b1, w8, b8, rgb[256];


  if (ptype == PIC8) {   /* monoify colormap */
    for (i=0; i<256; i++)
      rgb[i] = MONO(rmap[i], gmap[i], bmap[i]);
  }


  image   = (byte *) ximage->data;
  bperln  = ximage->bytes_per_line;
  order   = ximage->bitmap_bit_order;
  bperpix = (ptype == PIC8) ? 1 : 3;


  thisline = (int *) malloc(wide * sizeof(int));
  nextline = (int *) malloc(wide * sizeof(int));
  if (!thisline || !nextline) 
    FatalError("ran out of memory in FloydDitherize1()\n");


  /* load up first line of picture */
  pp = pic824;
  if (ptype == PIC24) {
    for (j=0, tmpptr = nextline; j<wide; j++, pp+=3)
      *tmpptr++ = fsgamcr[MONO(pp[0], pp[1], pp[2])];
  }
  else {
    for (j=0, tmpptr = nextline; j<wide; j++, pp++)
      *tmpptr++ = fsgamcr[rgb[*pp]];
  }

      
  w1 = white&0x1;  b1=black&0x1;
  w8 = w1<<7;  b8 = b1<<7;        /* b/w bit in high bit */
  

  for (i=0; i<high; i++) {
    if ((i&63) == 0) WaitCursor();

    /* get next line of image */
    tmpptr = thisline;  thisline = nextline;  nextline = tmpptr;  /* swap */
    if (i!=high-1) {
      pp = pic824 + (i+1) * wide * bperpix;
      if (ptype == PIC24) {
	for (j=0, tmpptr = nextline; j<wide; j++, pp+=3)
	  *tmpptr++ = fsgamcr[MONO(pp[0], pp[1], pp[2])];
      }
      else {
	for (j=0, tmpptr = nextline; j<wide; j++, pp++)
	  *tmpptr++ = fsgamcr[rgb[*pp]];
      }
    }

    thisptr = thisline;  nextptr = nextline;

    pp  = image + i*bperln;


    if (order==LSBFirst) {
      bit = pix8 = 0;
      for (j=0; j<wide; j++, thisptr++, nextptr++) {
	if (*thisptr<128) { err = *thisptr;     pix8 |= b8; }
	             else { err = *thisptr-255; pix8 |= w8; }

	if (bit==7) { *pp++ = pix8;  bit=pix8=0; }
	       else { pix8 >>= 1;  bit++; }

	if (j<wide-1) thisptr[1] += ((err*7)/16);

	if (i<high-1) {
	  nextptr[0] += ((err*5)/16);
	  if (j>0)      nextptr[-1] += ((err*3)/16);
	  if (j<wide-1) nextptr[ 1] += (err/16);
	}
      }
      if (bit) *pp++ = pix8>>(7-bit);  /* write partial byte at end of line */
    }

    else {   /* order==MSBFirst */
      bit = pix8 = 0;
      for (j=0; j<wide; j++, thisptr++, nextptr++) {
	if (*thisptr<128) { err = *thisptr;     pix8 |= b1; }
	             else { err = *thisptr-255; pix8 |= w1; }

	if (bit==7) { *pp++ = pix8;  bit=pix8=0; }
 	       else { pix8 <<= 1; bit++; }

	if (j<wide-1) thisptr[1] += ((err*7)/16);

	if (i<high-1) {
	  nextptr[0] += ((err*5)/16);
	  if (j>0)       nextptr[-1] += ((err*3)/16);
	  if (j<wide-1)  nextptr[ 1] += (err/16);
	}
      }
      if (bit) *pp++ = pix8<<(7-bit);  /* write partial byte at end of line */
    }
  }


  free(thisline);  free(nextline);
}





/************************/
byte *FSDither(inpic, intype, w, h, rmap, gmap, bmap, 
	      bval, wval)
     byte *inpic, *rmap, *gmap, *bmap;
     int   w,h, intype, bval, wval;
{
  /* takes an input pic of size w*h, and type 'intype' (PIC8 or PIC24),
   *                (if PIC8, colormap specified by rmap,gmap,bmap)
   * and does the floyd-steinberg dithering algorithm on it.
   * generates (mallocs) a w*h 1-byte-per-pixel 'outpic', using 'bval'
   * and 'wval' as the 'black' and 'white' pixel values, respectively
   */

  int    i, j, err, w1, h1;
  byte  *pp, *outpic, rgb[256];
  int   *thisline, *nextline, *thisptr, *nextptr, *tmpptr;


  outpic = (byte *) malloc(w * h);
  if (!outpic) return outpic;
    

  if (intype == PIC8) {       /* monoify colormap */
    for (i=0; i<256; i++)
      rgb[i] = MONO(rmap[i], gmap[i], bmap[i]);
  }


  thisline = (int *) malloc(w * sizeof(int));
  nextline = (int *) malloc(w * sizeof(int));
  if (!thisline || !nextline) 
    FatalError("ran out of memory in FSDither()\n");


  w1 = w-1;  h1 = h-1;

  /* load up first line of picture */
  pp = inpic;
  if (intype == PIC24) {
    for (j=0, tmpptr=nextline; j<w; j++, pp+=3)
      *tmpptr++ = fsgamcr[MONO(pp[0], pp[1], pp[2])];
  }
  else {
    for (j=0, tmpptr=nextline; j<w; j++, pp++)
      *tmpptr++ = fsgamcr[rgb[*pp]];
  }


  for (i=0; i<h; i++) {
    if ((i&31) == 0) WaitCursor();

    /* get next line of picture */
    tmpptr = thisline;  thisline = nextline;  nextline = tmpptr;  /* swap */
    if (i!=h1) {
      if (intype == PIC24) {
	pp = inpic + (i+1) * w * 3;
	for (j=0, tmpptr=nextline; j<w; j++, pp+=3)
	  *tmpptr++ = fsgamcr[MONO(pp[0], pp[1], pp[2])];
      }
      else {
	pp = inpic + (i+1) * w;
	for (j=0, tmpptr = nextline; j<w; j++, pp++)
	  *tmpptr++ = fsgamcr[rgb[*pp]];
      }
    }

    pp  = outpic + i * w;
    thisptr = thisline;  nextptr = nextline;

    for (j=0; j<w; j++, pp++, thisptr++, nextptr++) {
      if (*thisptr<128) { err = *thisptr;     *pp = (byte) bval; }
                   else { err = *thisptr-255; *pp = (byte) wval; }

      if (j<w1) thisptr[1] += ((err*7)/16);

      if (i<h1) {
        nextptr[0] += ((err*5)/16);
        if (j>0)  nextptr[-1] += ((err*3)/16);
        if (j<w1) nextptr[ 1] += (err/16);
      }
    }
  }

  free(thisline);  free(nextline);
  return outpic;
}






/***********************************/
void CreateXImage()
{
  xvDestroyImage(theImage);   theImage = NULL;

  if (!epic) GenerateEpic(eWIDE, eHIGH);  /* shouldn't happen... */

  if (picType == PIC24) {  /* generate egampic */
    if (egampic && egampic != epic) free(egampic);
    egampic = GammifyPic24(epic, eWIDE, eHIGH);
    if (!egampic) egampic = epic;
  }


  if (picType == PIC8) 
    theImage = Pic8ToXImage(epic, eWIDE, eHIGH, cols, rMap, gMap, bMap);
  else if (picType == PIC24)
    theImage = Pic24ToXImage(egampic, eWIDE, eHIGH);
}




/***********************************/
XImage *Pic8ToXImage(pic8, wide, high, xcolors, rmap, gmap, bmap)
     byte          *pic8, *rmap, *gmap, *bmap;
     int            wide, high;
     unsigned long *xcolors;
{
  /*
   * this has to do the tricky bit of converting the data in 'pic8'
   * into something usable for X.
   *
   */


  int     i;
  unsigned long xcol;
  XImage *xim;
  byte   *dithpic;

  xim = (XImage *) NULL;
  dithpic = (byte *) NULL;

  if (!pic8) return xim;  /* shouldn't happen */

  if (DEBUG > 1) 
    fprintf(stderr,"Pic8ToXImage(): creating a %dx%d Ximage, %d bits deep\n",
	    wide, high, dispDEEP);


  /* special case: 1-bit display */
  if (dispDEEP == 1) {
    byte  *imagedata;

    xim = XCreateImage(theDisp, theVisual, dispDEEP, XYPixmap, 0, NULL, 
			    wide, high, 32, 0);
    if (!xim) FatalError("couldn't create xim!");

    imagedata = (byte *) malloc(xim->bytes_per_line * high);
    if (!imagedata) FatalError("couldn't malloc imagedata");

    xim->data = (char *) imagedata;
    FloydDitherize1(xim, pic8, PIC8, wide, high, rmap, gmap, bmap);
    return xim;
  }


  /* if ncols==0, do a 'black' and 'white' dither */
  if (ncols == 0) {
    /* note that if dispDEEP > 8, dithpic will just have '0' and '1' instead 
       of 'black' and 'white' */

    dithpic = FSDither(pic8, PIC8, wide, high, rmap, gmap, bmap,
		       (dispDEEP <= 8) ? black : 0, 
		       (dispDEEP <= 8) ? white : 1);
  }



  switch (dispDEEP) {

  case 8: {
    byte  *imagedata, *ip, *pp;
    int   j, imWIDE, nullCount;
  
    nullCount = (4 - (wide % 4)) & 0x03;  /* # of padding bytes per line */
    imWIDE = wide + nullCount;
 
    /* Now create the image data - pad each scanline as necessary */
    imagedata = (byte *) malloc(imWIDE * high);
    if (!imagedata) FatalError("couldn't malloc imagedata");
    
    pp = (dithpic) ? dithpic : pic8;

    for (i=0, ip=imagedata; i<high; i++) {
      if ((i&0x7f) == 0) WaitCursor();

      if (dithpic) {
	for (j=0; j<wide; j++, ip++, pp++) *ip = *pp;  /* pp already is Xval */
      }
      else {
	for (j=0; j<wide; j++, ip++, pp++) *ip = (byte) xcolors[*pp];
      }

      for (j=0; j<nullCount; j++, ip++) *ip = 0;
    }
      
    xim = XCreateImage(theDisp,theVisual,dispDEEP,ZPixmap,0,
		       (char *) imagedata, wide, high, 32, imWIDE);
    if (!xim) FatalError("couldn't create xim!");
  }
    break;



    /*********************************/
      
  case 4: {
    byte  *imagedata, *ip, *pp;
    byte *lip;
    int  bperline, half, j;

    xim = XCreateImage(theDisp, theVisual, dispDEEP, ZPixmap, 0, NULL, 
			      wide, high, 8, 0);
    if (!xim) FatalError("couldn't create xim!");

    bperline = xim->bytes_per_line;
    imagedata = (byte *) malloc(bperline * high);
    if (!imagedata) FatalError("couldn't malloc imagedata");
    xim->data = (char *) imagedata;

    
    pp = (dithpic) ? dithpic : pic8;

    if (xim->bits_per_pixel == 4) {
      for (i=0, lip=imagedata; i<high; i++, lip+=bperline) {
	if ((i&127) == 0) WaitCursor();

	for (j=0, ip=lip, half=0; j<wide; j++,pp++,half++) {
	  xcol = ((dithpic) ? *pp : xcolors[*pp]) & 0x0f;

	  if (ImageByteOrder(theDisp) == LSBFirst) {
	    if (half&1) { *ip = *ip + (xcol<<4);  ip++; }
	    else *ip = xcol;
	  }
	  else {
	    if (half&1) { *ip = *ip + xcol;  ip++; }
	    else *ip = xcol << 4;
	  }
	}
      }
    }

    else if (xim->bits_per_pixel == 8) {
      for (i=wide*high, ip=imagedata; i>0; i--,pp++,ip++) {
	if ((i&0x1ffff) == 0) WaitCursor();
	*ip = (dithpic) ? *pp : (byte) xcolors[*pp];
      }
    }

    else FatalError("This display's too bizarre.  Can't create XImage.");
  }
    break;
      

    /*********************************/
      
  case 2: {  /* by M.Kossa@frec.bull.fr (Marc Kossa) */
             /* MSBFirst mods added by dale@ntg.com (Dale Luck) */
             /* additional fixes by  evol@infko.uni-koblenz.de 
		(Randolf Werner) for NeXT 2bit grayscale with MouseX */

    byte  *imagedata, *ip, *pp;
    byte *lip;
    int  bperline, half, j;

    xim = XCreateImage(theDisp, theVisual, dispDEEP, ZPixmap, 0, NULL, 
			    wide, high, 8, 0);
    if (!xim) FatalError("couldn't create xim!");

    bperline = xim->bytes_per_line;
    imagedata = (byte *) malloc(bperline * high);
    if (!imagedata) FatalError("couldn't malloc imagedata");
    xim->data = (char *) imagedata;

    pp = (dithpic) ? dithpic : pic8;

    if (xim->bits_per_pixel == 2) {
      for (i=0, lip=imagedata; i<high; i++, lip+=bperline) {
	if ((i&127) == 0) WaitCursor();
	for (j=0, ip=lip, half=0; j<wide; j++,pp++,half++) {
	  xcol = ((dithpic) ? *pp : xcolors[*pp]) & 0x03;

	  if (xim->bitmap_bit_order == LSBFirst) {
	    if      (half%4==0) *ip  = xcol;
	    else if (half%4==1) *ip |= (xcol<<2);
	    else if (half%4==2) *ip |= (xcol<<4);
	    else              { *ip |= (xcol<<6); ip++; }
	  }

	  else {  /* MSBFirst.  NeXT, among others */
	    if      (half%4==0) *ip  = (xcol<<6);
	    else if (half%4==1) *ip |= (xcol<<4);
	    else if (half%4==2) *ip |= (xcol<<2);
	    else              { *ip |=  xcol;     ip++; }
	  }
	}
      }
    }

    else if (xim->bits_per_pixel == 4) {
      for (i=0, lip=imagedata; i<high; i++, lip+=bperline) {
	if ((i&127) == 0) WaitCursor();

	for (j=0, ip=lip, half=0; j<wide; j++,pp++,half++) {
	  xcol = ((dithpic) ? *pp : xcolors[*pp]) & 0x0f;

	  if (xim->bitmap_bit_order == LSBFirst) {
	    if (half&1) { *ip |= (xcol<<4);  ip++; }
	    else *ip = xcol;
	  }

	  else { /* MSBFirst */
	    if (half&1) { *ip |= xcol;  ip++; }
	    else *ip = xcol << 4;
	  }
	}
      }
    }

    else if (xim->bits_per_pixel == 8) {
      for (i=wide*high, ip=imagedata; i>0; i--,pp++,ip++) {
	if ((i&0x1ffff) == 0) WaitCursor();
	*ip = (dithpic) ? *pp : (byte) xcolors[*pp];
      }
    }
      
    else FatalError("This display's too bizarre.  Can't create XImage.");
  }
    break;
      

  /*********************************/

  case 5:
  case 6: {
    byte  *imagedata, *ip, *pp;
    int  bperline;
    
    xim = XCreateImage(theDisp, theVisual, dispDEEP, ZPixmap, 0, NULL, 
			    wide, high, 8, 0);
    if (!xim) FatalError("couldn't create xim!");

    if (xim->bits_per_pixel != 8)
      FatalError("This display's too bizarre.  Can't create XImage.");

    bperline = xim->bytes_per_line;
    imagedata = (byte *) malloc(bperline * high);
    if (!imagedata) FatalError("couldn't malloc imagedata");
    xim->data = (char *) imagedata;

    pp = (dithpic) ? dithpic : pic8;

    for (i=wide*high, ip=imagedata; i>0; i--,pp++,ip++) {
      if ((i&0x1ffff) == 0) WaitCursor();
      *ip = (dithpic) ? *pp : (byte) xcolors[*pp];
    }
  }
    break;
      

  /*********************************/

  case 12:
  case 16: {
    short  *imagedata, *ip;
    byte  *pp;

    imagedata = (short *) malloc(2*wide*high);
    if (!imagedata) FatalError("couldn't malloc imagedata");

    xim = XCreateImage(theDisp,theVisual,dispDEEP,ZPixmap,0,
			    (char *) imagedata, wide, high, 16, 0);
    if (!xim) FatalError("couldn't create xim!");

    if (dispDEEP == 12 && xim->bits_per_pixel != 16) {
      char buf[128];
      sprintf(buf,"No code for this type of display (depth=%d, bperpix=%d)",
	      dispDEEP, xim->bits_per_pixel);
      FatalError(buf);
    }

    pp = (dithpic) ? dithpic : pic8;

    if (xim->byte_order == MSBFirst) {
      for (i=wide*high, ip=imagedata; i>0; i--,pp++) {
	if ((i&0x1ffff) == 0) WaitCursor();
	if (dithpic) {
	  *ip++ = ((*pp) ? white : black) & 0xffff;
	}
	else *ip++ = xcolors[*pp] & 0xffff;
      }
    }
    else {   /* LSBFirst */
      for (i=wide*high, ip=imagedata; i>0; i--,pp++) {
	if ((i&0x1ffff) == 0) WaitCursor();

	if (dithpic) xcol = ((*pp) ? white : black) & 0xffff;
	        else xcol = xcolors[*pp];

	*ip++ = ((xcol>>8) & 0xff) | ((xcol&0xff) << 8);
      }
    }
  }
    break;

      
    /*********************************/

  case 24:
  case 32: {
    byte  *imagedata, *ip, *pp;
    imagedata = (byte *) malloc(4*wide*high);
    if (!imagedata) FatalError("couldn't malloc imagedata");
      
    xim = XCreateImage(theDisp,theVisual,dispDEEP,ZPixmap,0,
			    (char *) imagedata, wide, high, 32, 0);
    if (!xim) FatalError("couldn't create xim!");

    pp = (dithpic) ? dithpic : pic8;
      
    if (xim->byte_order == MSBFirst) {
      for (i=wide*high, ip=imagedata; i>0; i--,pp++) {
	if ((i&0x1ffff) == 0) WaitCursor();
	xcol = (dithpic) ? ((*pp) ? white : black) : xcolors[*pp];

	*ip++ = 0;
	*ip++ = (xcol>>16) & 0xff;
	*ip++ = (xcol>>8) & 0xff;
	*ip++ =  xcol & 0xff;
      }
    }

    else {  /* LSBFirst */
      for (i=wide*high, ip=imagedata; i>0; i--,pp++) {
	xcol = (dithpic) ? ((*pp) ? white : black) : xcolors[*pp];

	if ((i&0x1ffff) == 0) WaitCursor();
	*ip++ =  xcol & 0xff;
	*ip++ = (xcol>>8) & 0xff;
	*ip++ = (xcol>>16) & 0xff;
	*ip++ = 0;
      }
    }
  }
    break;


    /*********************************/
    
  default: 
    sprintf(str,"no code to handle this display type (%d bits deep)",
	    dispDEEP);
    FatalError(str);
    break;
  }

  return(xim);
}

static int foo = 0;

/***********************************/
XImage *Pic24ToXImage(pic24, wide, high)
     byte          *pic24;
     int            wide, high;
{
  /*
   * this has to do the none-to-simple bit of converting the data in 'pic24'
   * into something usable by X.
   *
   * There are two major approaches:  if we're displaying on a TrueColor
   * or DirectColor display, we've got all the colors we're going to need,
   * and 'all we have to do' is convert 24-bit RGB pixels into whatever
   * variation of RGB the X device in question wants.  No color allocation
   * is involved.
   *
   * Alternately, if we're on a PseudoColor, GrayScale, StaticColor or 
   * StaticGray display, we're going to continue to operate in an 8-bit 
   * mode.  (In that by this point, a 3/3/2 standard colormap has been
   * created for our use (though all 256 colors may not be unique...), and
   * we're just going to display the 24-bit picture by dithering with those
   * colors
   *
   */

  int     i,j;
  XImage *xim;

  xim     = (XImage *) NULL;

  if (!pic24) return xim;  /* shouldn't happen */


  /* special case: 1-bit display.  Doesn't much matter *what* the visual is */
  if (dispDEEP == 1) {
    byte  *imagedata;

    xim = XCreateImage(theDisp, theVisual, dispDEEP, XYPixmap, 0, NULL, 
		       wide, high, 32, 0);
    if (!xim) FatalError("couldn't create xim!");

    imagedata = (byte *) malloc(xim->bytes_per_line * high);
    if (!imagedata) FatalError("couldn't malloc imagedata");

    xim->data = (char *) imagedata;
    FloydDitherize1(xim, pic24, PIC24, wide, high, NULL, NULL, NULL);

    return xim;
  }




  if (theVisual->class == TrueColor || theVisual->class == DirectColor) {

    /************************************************************************/
    /* Non-ColorMapped Visuals:  TrueColor, DirectColor                     */
    /************************************************************************/

    unsigned long r, g, b, rmask, gmask, bmask, xcol;
    int           rshift, gshift, bshift, bperpix, bperline, border;
    byte         *imagedata, *lip, *ip, *pp;


    /* compute various shifting constants that we'll need... */

    rmask = theVisual->red_mask;
    gmask = theVisual->green_mask;
    bmask = theVisual->blue_mask;

    rshift = 7 - highbit(rmask);
    gshift = 7 - highbit(gmask);
    bshift = 7 - highbit(bmask);


    xim = XCreateImage(theDisp, theVisual, dispDEEP, ZPixmap, 0, NULL,
		       wide, high, 32, 0);
    if (!xim) FatalError("couldn't create X image!");

    bperline = xim->bytes_per_line;
    bperpix  = xim->bits_per_pixel;
    border   = xim->byte_order;

    imagedata = (byte *) malloc(high * bperline);
    if (!imagedata) FatalError("couldn't malloc imagedata");

    xim->data = (char *) imagedata;

    if (bperpix != 8 && bperpix != 16 && bperpix != 24 && bperpix != 32) {
      char buf[128];
      sprintf(buf,"Sorry, no code written to handle %d-bit %s",
	      bperpix, "TrueColor/DirectColor displays!");
      FatalError(buf);
    }


    lip = imagedata;  pp = pic24;
    for (i=0; i<high; i++, lip+=bperline) {
      for (j=0, ip=lip; j<wide; j++) {
	r = *pp++;  g = *pp++;  b = *pp++;

	/* shift r,g,b so that high bit of 8-bit color specification is 
	 * aligned with high bit of r,g,b-mask in visual, 
	 * AND each component with its mask,
	 * and OR the three components together
	 */

	/* shift the bits around */
	if (rshift<0) r = r << (-rshift);
	         else r = r >> rshift;
	
	if (gshift<0) g = g << (-gshift);
	         else g = g >> gshift;

	if (bshift<0) b = b << (-bshift);
	         else b = b >> bshift;

	r = r & rmask;
	g = g & gmask;
	b = b & bmask;

	xcol = r | g | b;

	if (bperpix == 32) {
	  if (border == MSBFirst) {
	    *ip++ = (xcol>>24) & 0xff;
	    *ip++ = (xcol>>16) & 0xff;
	    *ip++ = (xcol>>8)  & 0xff;
	    *ip++ =  xcol      & 0xff;
	  }
	  else {  /* LSBFirst */
	    *ip++ =  xcol      & 0xff;
	    *ip++ = (xcol>>8)  & 0xff;
	    *ip++ = (xcol>>16) & 0xff;
	    *ip++ = (xcol>>24) & 0xff;
	  }
	}

	else if (bperpix == 24) {
	  if (border == MSBFirst) {
	    *ip++ = (xcol>>16) & 0xff;
	    *ip++ = (xcol>>8)  & 0xff;
	    *ip++ =  xcol      & 0xff;
	  }
	  else {  /* LSBFirst */
	    *ip++ =  xcol      & 0xff;
	    *ip++ = (xcol>>8)  & 0xff;
	    *ip++ = (xcol>>16) & 0xff;
	  }
	}

	else if (bperpix == 16) {
	  if (border == MSBFirst) {
	    *ip++ = (xcol>>8)  & 0xff;
	    *ip++ =  xcol      & 0xff;
	  }
	  else {  /* LSBFirst */
	    *ip++ =  xcol      & 0xff;
	    *ip++ = (xcol>>8)  & 0xff;
	  }
	}

	else if (bperpix == 8) {
	  *ip++ =  xcol      & 0xff;
	}
      }
    }
  }

  else {

    /************************************************************************/
    /* CMapped Visuals:  PseudoColor, GrayScale, StaticGray, StaticColor... */
    /************************************************************************/

    byte *pic8;
    int   bwdith;

    /* in all cases, make an 8-bit version of the image, either using
       'black' and 'white', or the stdcmap */

    bwdith = 0;

    if (ncols == 0 && dispDEEP != 1) {   /* do 'black' and 'white' dither */
      /* note that if dispDEEP > 8, pic8 will just have '0' and '1' instead 
	 of 'black' and 'white' */

      pic8 = FSDither(pic24, PIC24, wide, high, NULL, NULL, NULL, 
		      (dispDEEP <= 8) ? black : 0, 
		      (dispDEEP <= 8) ? white : 1);
      bwdith = 1;
    }

    else {                               /* do color dither using stdcmap */
      pic8 = Do332ColorDither(pic24, NULL, wide, high, NULL, NULL, NULL,
			      stdrdisp, stdgdisp, stdbdisp, 256);
    }

    if (!pic8) FatalError("out of memory in Pic24ToXImage()\n");


    /* DISPLAY-DEPENDENT code follows... */


    switch (dispDEEP) {


    case 8: {
      byte  *imagedata, *ip, *pp;
      int   j, imWIDE, nullCount;
  
      nullCount = (4 - (wide % 4)) & 0x03;  /* # of padding bytes per line */
      imWIDE = wide + nullCount;
 
      /* Now create the image data - pad each scanline as necessary */
      imagedata = (byte *) malloc(imWIDE * high);
      if (!imagedata) FatalError("couldn't malloc imagedata");
      
      for (i=0, pp=pic8, ip=imagedata; i<high; i++) {
	if ((i&0x7f) == 0) WaitCursor();

	if (bwdith)
	  for (j=0; j<wide; j++, ip++, pp++) *ip = *pp;
	else
	  for (j=0; j<wide; j++, ip++, pp++) *ip = stdcols[*pp];

	for (j=0; j<nullCount; j++, ip++)  *ip = 0;
      }

      xim = XCreateImage(theDisp, theVisual, dispDEEP, ZPixmap, 0,
			 (char *) imagedata, wide, high, 32, imWIDE);
      if (!xim) FatalError("couldn't create xim!");
    }
      break;


      /*********************************/
      
    case 4: {
      byte         *imagedata, *ip, *pp;
      byte         *lip;
      int           bperline, half, j;
      unsigned long xcol;
      
      xim = XCreateImage(theDisp, theVisual, dispDEEP, ZPixmap, 0, NULL, 
			 wide, high, 32, 0);
      if (!xim) FatalError("couldn't create xim!");

      bperline = xim->bytes_per_line;
      imagedata = (byte *) malloc(bperline * high);
      if (!imagedata) FatalError("couldn't malloc imagedata");
      xim->data = (char *) imagedata;

      pp = pic8;

      if (xim->bits_per_pixel == 4) {
	for (i=0, lip=imagedata; i<high; i++, lip+=bperline) {
	  if ((i&127) == 0) WaitCursor();

	  for (j=0, ip=lip, half=0; j<wide; j++,pp++,half++) {
	    xcol = ((bwdith) ? *pp : stdcols[*pp]) & 0x0f;

	    if (xim->byte_order == LSBFirst) {
	      if (half&1) { *ip = *ip + (xcol<<4);  ip++; }
	      else *ip = xcol;
	    }
	    else {
	      if (half&1) { *ip = *ip + xcol;  ip++; }
	      else *ip = xcol << 4;
	    }
	  }
	}
      }

      else if (xim->bits_per_pixel == 8) {
	for (i=0,lip=imagedata; i<high; i++,lip+=bperline) {
	  if ((i&127)==0) WaitCursor();
	  for (j=0,ip=lip; j<wide; j++,pp++,ip++) {
	    *ip = (bwdith) ? *pp : (byte) stdcols[*pp];
	  }
	}
      }

      else FatalError("This display's too bizarre.  Can't create XImage.");
    }
      break;
      


      /*********************************/
      
    case 2: {  /* by M.Kossa@frec.bull.fr (Marc Kossa) */
               /* MSBFirst mods added by dale@ntg.com (Dale Luck) */
               /* additional fixes by  evol@infko.uni-koblenz.de 
		  (Randolf Werner) for NeXT 2bit grayscale with MouseX */

      byte  *imagedata, *ip, *pp;
      byte *lip;
      int  bperline, half, j;
      unsigned long xcol;

      xim = XCreateImage(theDisp, theVisual, dispDEEP, ZPixmap, 0, NULL, 
			 wide, high, 32, 0);
      if (!xim) FatalError("couldn't create xim!");

      bperline = xim->bytes_per_line;
      imagedata = (byte *) malloc(bperline * high);
      if (!imagedata) FatalError("couldn't malloc imagedata");
      xim->data = (char *) imagedata;

      pp = pic8;

      if (xim->bits_per_pixel == 2) {
	for (i=0, lip=imagedata; i<high; i++, lip+=bperline) {
	  if ((i&127) == 0) WaitCursor();
	  for (j=0, ip=lip, half=0; j<wide; j++,pp++,half++) {
	    xcol = ((bwdith) ? *pp : stdcols[*pp]) & 0x03;

	    if (xim->bitmap_bit_order == LSBFirst) {
	      if      (half%4==0) *ip  = xcol;
	      else if (half%4==1) *ip |= (xcol<<2);
	      else if (half%4==2) *ip |= (xcol<<4);
	      else              { *ip |= (xcol<<6); ip++; }
	    }

	    else {  /* MSBFirst.  NeXT, among others */
	      if      (half%4==0) *ip  = (xcol<<6);
	      else if (half%4==1) *ip |= (xcol<<4);
	      else if (half%4==2) *ip |= (xcol<<2);
	      else              { *ip |=  xcol;     ip++; }
	    }
	  }
	}
      }

      else if (xim->bits_per_pixel == 4) {
	for (i=0, lip=imagedata; i<high; i++, lip+=bperline) {
	  if ((i&127) == 0) WaitCursor();

	  for (j=0, ip=lip, half=0; j<wide; j++,pp++,half++) {
	    xcol = ((bwdith) ? *pp : stdcols[*pp]) & 0x0f;

	    if (xim->bitmap_bit_order == LSBFirst) {
	      if (half&1) { *ip |= (xcol<<4);  ip++; }
	      else *ip = xcol;
	    }

	    else { /* MSBFirst */
	      if (half&1) { *ip |= xcol;  ip++; }
	      else *ip = xcol << 4;
	    }
	  }
	}
      }

      else if (xim->bits_per_pixel == 8) {
	for (i=0, lip=imagedata; i<high; i++, lip+=bperline) {
	  if ((i&127) == 0) WaitCursor();

	  for (j=0, ip=lip; j<wide; j++,pp++,ip++) {
	    *ip = ((bwdith) ? *pp : stdcols[*pp]) & 0xff;
	  }
	}
      }
      
      else FatalError("This display's too bizarre.  Can't create XImage.");
    }
      break;
      

      /*********************************/
    
    case 6: {
      byte  *imagedata, *lip, *ip, *pp;
      int  bperline;
    
      xim = XCreateImage(theDisp, theVisual, dispDEEP, ZPixmap, 0, NULL, 
			 wide, high, 32, 0);
      if (!xim) FatalError("couldn't create xim!");
      
      if (xim->bits_per_pixel != 8)
	FatalError("This display's too bizarre.  Can't create XImage.");

      bperline = xim->bytes_per_line;
      imagedata = (byte *) malloc(bperline * high);
      if (!imagedata) FatalError("couldn't malloc imagedata");
      xim->data = (char *) imagedata;

      pp = pic8;

      for (i=0, lip=imagedata; i<high; i++, lip+=bperline) {
	if ((i&127) == 0) WaitCursor();

	for (j=0, ip=lip; j<wide; j++,pp++,ip++) {
	  *ip = ((bwdith) ? *pp : stdcols[*pp]) & 0x3f;

	}
      }
    }
      break;

      
      /*********************************/

    case 16: {
      short  *imagedata, *ip, *lip;
      byte   *pp;
      int     bperline;
      unsigned long xcol;

      imagedata = (short *) malloc(2*wide*high);
      if (!imagedata) FatalError("couldn't malloc imagedata");

      xim = XCreateImage(theDisp,theVisual,dispDEEP,ZPixmap,0,
			 (char *) imagedata, wide, high, 32, 0);
      if (!xim) FatalError("couldn't create xim!");
      bperline = xim->bytes_per_line;

      pp = pic8;

      if (xim->byte_order == MSBFirst) {
	for (i=0, lip=imagedata; i<high; i++, lip+=bperline) {
	  if ((i&127) == 0) WaitCursor();

	  for (j=0, ip=lip; j<wide; j++,pp++) {
	    *ip++ = ((bwdith) ? *pp : stdcols[*pp]) & 0xffff;
	  }
	}
      }

      else {   /* LSBFirst */
	for (i=0, lip=imagedata; i<high; i++, lip+=bperline) {
	  if ((i&127) == 0) WaitCursor();

	  for (j=0, ip=lip; j<wide; j++,pp++) {
	    xcol = ((bwdith) ? *pp : stdcols[*pp]) & 0xffff;
	    *ip++ = ((xcol>>8) & 0xff) | ((xcol&0xff) << 8);
	  }
	}
      }
    }
      break;

      
      /*********************************/

      /* this wouldn't seem likely to happen, but what the heck... */

    case 24:
    case 32: {
      byte  *imagedata, *ip, *pp;
      unsigned long xcol;
      int bperpix;

      imagedata = (byte *) malloc(4*wide*high);
      if (!imagedata) FatalError("couldn't malloc imagedata");
      
      xim = XCreateImage(theDisp,theVisual,dispDEEP,ZPixmap,0,
			 (char *) imagedata, wide, high, 32, 0);
      if (!xim) FatalError("couldn't create xim!");

      bperpix = xim->bits_per_pixel;

      pp = pic8;
      
      if (xim->byte_order == MSBFirst) {
	for (i=wide*high, ip=imagedata; i>0; i--,pp++) {
	  if ((i&0x1ffff) == 0) WaitCursor();
	  xcol = (bwdith) ? *pp : stdcols[*pp];

	  if (bperpix == 32) *ip++ = 0;
	  *ip++ = (xcol>>16) & 0xff;
	  *ip++ = (xcol>>8)  & 0xff;
	  *ip++ =  xcol      & 0xff;
	}
      }

      else {  /* LSBFirst */
	for (i=wide*high, ip=imagedata; i>0; i--,pp++) {
	  xcol = (bwdith) ? *pp : stdcols[*pp];

	  if ((i&0x1ffff) == 0) WaitCursor();
	  *ip++ =  xcol      & 0xff;
	  *ip++ = (xcol>>8)  & 0xff;
	  *ip++ = (xcol>>16) & 0xff;
	  if (bperpix == 32) *ip++ = 0;
	}
      }
    }     
      break;

    }   /* end of the switch */

    free(pic8);  /* since we ALWAYS make a copy of it into imagedata */
  }


  return xim;
}



/***********************************************************/
void Set824Menus(mode)
     int mode;
{
  /* move checkmark */
  conv24MB.flags[CONV24_8BIT]  = (mode==PIC8);  
  conv24MB.flags[CONV24_24BIT] = (mode==PIC24);

  if (mode == PIC24) {
    dispMB.dim[DMB_COLNORM] = 1;
    dispMB.dim[DMB_COLPERF] = 1;
    dispMB.dim[DMB_COLOWNC] = 1;

    /* turn off RAW/DITH/SMOOTH buttons (caused by picType) */
    epicMode = EM_RAW;
    SetEpicMode();

    /* turn off autoapply mode */
    /* GamSetAutoApply(0); */       /* or not! */
  }

  else if (mode == PIC8) {
    dispMB.dim[DMB_COLNORM] = 0;
    dispMB.dim[DMB_COLPERF] = (dispMode == DMB_WINDOW) ? 0 : 1;
    dispMB.dim[DMB_COLOWNC] = (dispMode == DMB_WINDOW) ? 0 : 1;

    /* turn on RAW/DITH/SMOOTH buttons */
    epicMode = EM_RAW;
    SetEpicMode();

    /* possibly turn autoapply back on */
    /* GamSetAutoApply(-1); */  /* -1 means 'back to default setting' */
  }

  SetDirRButt(F_COLORS, -1);    /* enable/disable REDUCED COLOR */
}


/***********************************************************/
void Change824Mode(mode)
     int mode;
{
  static int oldcmapmode = -1;

  if (mode == picType) return;   /* same mode, do nothing */

  Set824Menus(mode);

  if (!pic) {  /* done all we wanna do when there's no pic */
    picType = mode;
    return;  
  }

  /* should probably actually *do* something involving colors, regenrating
     pic's, drawing an Ximage, etc. */

  if (mode == PIC24) {
    byte *pic24;

    WaitCursor();
    pic24 = Conv8to24(pic, pWIDE, pHIGH, rorg,gorg,borg);
    if (!pic24) FatalError("Ran out of memory in Change824Mode()\n");

    KillOldPics();
    pic = pic24;  picType = PIC24;

    Set824Menus(picType);            /* RAW/DITH/SMOOTH buttons change */
    InstallNewPic();
  }


  else if (mode == PIC8) {
    byte *pic8;

    WaitCursor();
    pic8 = Conv24to8(pic, pWIDE, pHIGH, ncols, rMap,gMap,bMap);
    if (!pic8) FatalError("Ran out of memory in Change824Mode()\n");

    KillOldPics();
    pic = pic8;  picType = PIC8;

    Set824Menus(picType);            /* RAW/DITH/SMOOTH buttons change */
    InstallNewPic();
  }

  /* may have to explicitly redraw image window if not using root */
}


/***********************************************************/
void FreeEpic()
{
  if (egampic && egampic != epic) free(egampic);
  if (epic && epic != cpic) free(epic);
  epic = egampic = NULL;
}


/***********************************************************/
void InvertPic24(pic24, w, h)
     byte *pic24;
     int   w,h;
{
  int i;

  for (i=w*h*3; i; i--, pic24++) *pic24 = 255 - *pic24;
}




/***********************/
static int highbit(ul)
unsigned long ul;
{
  /* returns position of highest set bit in 'ul' as an integer (0-31),
   or -1 if none */

  int i;
  for (i=31; ((ul&0x80000000) == 0) && i>=0;  i--, ul<<=1);
  return i;
}
