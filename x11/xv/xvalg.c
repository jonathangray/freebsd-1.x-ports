/*
 * xvalg.c - image manipulation algorithms (Blur, etc.)
 *
 *  Author:    John Bradley, University of Pennsylvania
 *                (bradley@cis.upenn.edu)
 *
 *  Contains:
 *         void AlgInit();
 *         void DoAlg(int algnum);
 *  static void NoAlg();
 *  static void Blur(int size);
 *  static void EdgeDetect(int tinfoil);
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
static void NoAlg(void);
static void Blur(int);
static void EdgeDetect(int);
static void OilPaint(void);
static void saveOrigPic(void);
static void doConvolv(byte *, int,int, int *,int,int,int, byte *, int,int);
static void doBlurConvolv(byte *, int, int, byte *, int);
static void doEdgeConvolv(byte *, int, int, byte *, int);
static void doOilPaint(byte *, int, int, byte *, int);
static int  start24bitAlg(byte **, byte **);
static void end24bitAlg(byte *, byte *);
#else
static void NoAlg(), Blur(), EdgeDetect(), OilPaint(), saveOrigPic();
static void doConvolv(), doBlurConvolv(), doEdgeConvolv(), doOilPaint();
static int  start24bitAlg();
static void end24bitAlg();
#endif


static byte *origPic = (byte *) NULL;
static int  origPicType;
static byte origrmap[256], origgmap[256], origbmap[256];


#undef TIMING_TEST

#ifdef TIMING_TEST
#include <sys/resource.h>
#endif


/***************************/
static void printUTime(str)
     char *str;
{
#ifdef TIMING_TEST
  int i;  struct rusage ru;

  i = getrusage(RUSAGE_SELF, &ru);
  fprintf(stderr,"%s: utime = %ld.%ld seconds\n",
	    str, ru.ru_utime.tv_sec, ru.ru_utime.tv_usec);
#endif
}






/************************************************************/
void AlgInit()
{
  /* called whenver an image file is loaded.  disposes of origPic 
     if neccessary, and points it to null */

  if (origPic) free(origPic);
  origPic = (byte *) NULL;

  algMB.dim[ALG_NONE] = 1;    /* can't undo when init'ed already */
}


/************************/
void DoAlg(anum)
     int anum;
{
  /* called with a value from the algMB button.  Executes the specified
     algorithm */

  switch (anum) {
  case ALG_NONE:  NoAlg();  break;
  case ALG_BLUR3: Blur(3);  break;
  case ALG_BLUR5: Blur(5);  break;
  case ALG_BLUR7: Blur(7);  break;
  case ALG_EDGE:  EdgeDetect(0);  break;
  case ALG_TINF:  EdgeDetect(1);  break;
  case ALG_OIL:   OilPaint();     break;
  }

  algMB.dim[ALG_NONE] = (anum == ALG_NONE);
}



/************************/
static void NoAlg()
{
  int i;

  /* restore original picture */
  if (!origPic) return;  /* none to restore */

  WaitCursor();

  KillOldPics();   /* toss the old pic/cpic/epic/theImage away */

  picType = origPicType;
  Set824Menus(picType);

  pic = origPic;  origPic = NULL;

  if (picType == PIC8) {
    for (i=0; i<256; i++) {
      rMap[i] = origrmap[i];
      gMap[i] = origgmap[i];
      bMap[i] = origbmap[i];
    }
  }

  InstallNewPic();
}


/************************/
static void Blur(n)
     int n;
{
  /* runs a n*n convolution mask (all 1's) over 'pic',
     producing a 24-bit version.  Then calls 24to8 to generate a new 8-bit
     image, and installs it. 

     Note that 'n' must be odd for things to work properly */

  byte *pic24, *tmpPic;
  int  *blurMatrix, i;

  WaitCursor();

  SetISTR(ISTR_INFO, "Blurring with %dx%d convolution mask...",n,n);

  if (start24bitAlg(&pic24, &tmpPic)) return;
    
  doBlurConvolv(pic24, pWIDE,pHIGH, tmpPic, n);

  end24bitAlg(pic24, tmpPic);
}



/************************/
static void EdgeDetect(tinfoil)
     int tinfoil;
{
  byte *pic24, *p24, *tmpPic;
  char *str;
  int  i, v, maxv;

  WaitCursor();

  if (tinfoil) str = "Doing cheesy embossing effect...";
          else str = "Doing edge detection...";
  SetISTR(ISTR_INFO, str);


  if (start24bitAlg(&pic24, &tmpPic)) return;

  if (tinfoil) {  /* fill tmpPic with gray128 */
    int i;  byte *tp;
    for (i=0, tp=tmpPic; i<pWIDE*pHIGH*3; i++) *tp++ = 128;
  }

  doEdgeConvolv(pic24, pWIDE, pHIGH, tmpPic, (tinfoil) ? 0 : 1);

  if (tinfoil) {  /* mono-ify tmpPic */
    for (i=0, p24=tmpPic; i<pWIDE*pHIGH; i++,p24+=3) {
      v = MONO(p24[0], p24[1], p24[2]);
      RANGE(v,0,255);
      p24[0] = p24[1] = p24[2] = (byte) v;
    }
  }


  SetISTR(ISTR_INFO, "%snormalizing...", str);

  /* compute maximum value */
  for (i=0, maxv=0, p24=tmpPic; i<pWIDE*pHIGH; i++,p24+=3) {
    v = MONO(p24[0], p24[1], p24[2]);
    if (v>maxv) maxv = v;
  }

  for (i=0, p24=tmpPic; i<pWIDE*pHIGH*3; i++) {
    v = (((int) *p24) * 255) / maxv;
    RANGE(v,0,255);
    *p24++ = (byte) v;
  }


  end24bitAlg(pic24, tmpPic);
}


/************************/
static void OilPaint()
{
  byte *pic24, *tmpPic;

  WaitCursor();

  SetISTR(ISTR_INFO, "Doing oilpaint effect...");

  if (start24bitAlg(&pic24, &tmpPic)) return;

  doOilPaint(pic24, pWIDE, pHIGH, tmpPic, 7);

  end24bitAlg(pic24, tmpPic);
}



/************************/
static void doConvolv(pic24, w, h, fil, filw, filh, maxv, results, inset, 
		      absolute)
     byte *pic24, *results;
     int   w, h, *fil, filw, filh, maxv, inset, absolute;
{
  /* general purpose convolution routine

     does convolution, adding results into 'results' array (clips at 0,255)
     note:  if 'inset' is true, it only does convolution on pixels where the
     entire mask is 'on-screen'

     If absolute, adds absolute value of rsum,gsum,bsum to results pic */

  byte *p24, *rp;
  int   i,x,y,x1,y1,x0,y0,rsum,gsum,bsum,count,w0,h0,w2,h2;

  printUTime("start of doConvolv");

  x0 = y0 = 0;  w0 = w;  h0 = h;
  w2 = filw/2;  h2 = filh/2;

  if (inset) { 
    x0 = filw/2;  y0 = filh/2;
    w0 -= (filw/2);  h0 -= (filh/2);
  } 


  for (y=y0; y<h0; y++) {
    if ((y & 15) == 0) WaitCursor();

    for (x=x0, rp=results + (y*w + x)*3; x<w0; x++) {

      /* compute weighted average for pic24[x,y] */
      rsum = gsum = bsum = 0;  count = 0;

      for (i=0, y1=y-h2; y1<=y+h2; y1++) {
	if (y1>=0 && y1<h) {
	  p24 = pic24 + y1*w*3 + (x-filw/2)*3; 
	  for (x1=x-w2; x1<=x+w2; x1++, i++) {
	    if (fil[i] && x1>=0 && x1<w) {
	      rsum += (fil[i] * *p24++);
	      gsum += (fil[i] * *p24++);
	      bsum += (fil[i] * *p24++);
	      count++;
	    }
	    else p24 += 3;
	  }
	}
      }

      rsum = rsum / (count * maxv);
      gsum = gsum / (count * maxv);
      bsum = bsum / (count * maxv);

      if (absolute) {
	if (rsum<0) rsum = -rsum;
	if (gsum<0) gsum = -gsum;
	if (bsum<0) bsum = -bsum;
      }

      rsum += rp[0];  RANGE(rsum,0,255);
      gsum += rp[1];  RANGE(gsum,0,255);
      bsum += rp[2];  RANGE(bsum,0,255);

      *rp++ = (byte) rsum;
      *rp++ = (byte) gsum;
      *rp++ = (byte) bsum;
    }
  }

  printUTime("end of doConvolv");
}



/************************/
static void doBlurConvolv(pic24, w, h, results, n)
     byte *pic24, *results;
     int   w, h, n;
{

  /* convolves with an n*n array, consisting of only 1's.
     n must be odd */

  register byte *p24;
  register int   rsum,gsum,bsum;
  byte          *rp;
  int            i,j,k,x,y,x1,y1,count,n2;


  printUTime("start of blurConvolv");

  n2 = n/2;

  for (y=0; y<h; y++) {
    if ((y & 15) == 0) WaitCursor();

    p24 = pic24   + y*w*3;
    rp  = results + y*w*3;

    for (x=0; x<w; x++) {

      rsum = gsum = bsum = 0;  count = 0;

      for (y1=y-n2; y1<=y+n2; y1++) {

	if (y1>=0 && y1<h) {
	  p24 = pic24 + y1*w*3 +(x-n2)*3; 

	  for (x1=x-n2; x1<=x+n2; x1++) {
	    if (x1>=0 && x1<w) {
	      rsum += *p24++;
	      gsum += *p24++;
	      bsum += *p24++;
	      count++;
	    }
	    else p24 += 3;
	  }
	}
      }

      rsum = rsum / count;
      gsum = gsum / count;
      bsum = bsum / count;

      RANGE(rsum,0,255);
      RANGE(gsum,0,255);
      RANGE(bsum,0,255);

      *rp++ = (byte) rsum;
      *rp++ = (byte) gsum;
      *rp++ = (byte) bsum;
    }
  }


  printUTime("end of blurConvolv");
}



/************************/
static void doEdgeConvolv(pic24, w, h, results, absolute)
     byte *pic24, *results;
     int   w, h, absolute;
{

  /* convolves with two edge detection masks (vertical and horizontal)
     simultaneously, adding results together.
     
     The two masks are (hard coded):

          -1 0 1             -1 -1 -1        -2 -1 0
          -1 0 1     and      0  0  0    =   -1  0 1
          -1 0 1              1  1  1         0  1 2
          
     Also, only does pixels in which the masks fit fully onto the picture
     (no pesky boundary conditionals)

     If absolute, adds absolute value of rsum,gsum,bsum to results pic */

  register byte *p24;
  register int   bperlin,rsum,gsum,bsum;
  byte          *rp;
  int            i, x,y;


  printUTime("start of edgeConvolv");

  bperlin = w * 3;

  for (y=1; y<h-1; y++) {
    if ((y & 63) == 0) WaitCursor();

    rp  = results + (y*w + 1)*3;
    p24 = pic24   + (y*w + 1)*3;

    for (x=1; x<w-1; x++, p24+=3) {

      /* compute weighted average for *p24 (pic24[x,y]) */
      rsum = gsum = bsum = 0;

      rsum -= (p24[-bperlin-3] * 2);   /* top left */
      gsum -= (p24[-bperlin-2] * 2);
      bsum -= (p24[-bperlin-1] * 2);

      rsum -= p24[-bperlin];           /* top mid */
      gsum -= p24[-bperlin+1];
      bsum -= p24[-bperlin+2];

      rsum -= p24[-3];                 /* mid left */
      gsum -= p24[-2];
      bsum -= p24[-1];

      rsum += p24[3];                  /* mid right */
      gsum += p24[4];
      bsum += p24[5];

      rsum += p24[bperlin];            /* bottom mid */
      gsum += p24[bperlin+1];
      bsum += p24[bperlin+2];

      rsum += (p24[bperlin+3] * 2);    /* bottom right */
      gsum += (p24[bperlin+4] * 2);
      bsum += (p24[bperlin+5] * 2);

      rsum = rsum / 8;
      gsum = gsum / 8;
      bsum = bsum / 8;

      if (absolute) {
	if (rsum<0) rsum = -rsum;
	if (gsum<0) gsum = -gsum;
	if (bsum<0) bsum = -bsum;
      }

      rsum += rp[0];  RANGE(rsum,0,255);
      gsum += rp[1];  RANGE(gsum,0,255);
      bsum += rp[2];  RANGE(bsum,0,255);

      *rp++ = (byte) rsum;
      *rp++ = (byte) gsum;
      *rp++ = (byte) bsum;
    }
  }

  printUTime("end of edgeConvolv");
}




/************************/
static void doOilPaint(pic24, w, h, results, n)
     byte *pic24, *results;
     int   w, h, n;
{

  /* does an 'oil transfer', as described in the book "Beyond Photography",
     by Holzmann, chapter 4, photo 7.  It is a sort of localized smearing.

     The following algorithm (but no actual code) was borrowed from
     'pgmoil.c', written by Wilson Bent (whb@hoh-2.att.com),
     and distributed as part of the PBMPLUS package.

     for each pixel in the image (assume, for a second, a grayscale image),
     compute a histogram of the n*n rectangle centered on the pixel.
     replace the pixel with the color that had the greatest # of hits in
     the histogram.  Note that 'n' should be odd. 

     I've modified the algorithm to do this same thing for all three planes
     of a 24-bit image     (jhb, 8/14/92)  */


  register byte *pp;
  register int   bperlin,rsum,gsum,bsum;
  byte          *rp, *p24, *plin;
  int            i,j,k,x,y,n2,rval,rcnt,gval,gcnt,bval,bcnt;
  int            rhist[256], ghist[256], bhist[256];


  printUTime("start of doOilPaint");

  bperlin = w * 3;
  n2 = n/2;

  p24 = pic24 - n2*w*3 - n2*3;   /* follows top-left corner of mask */
  rp  = results;

  for (y=0; y<h; y++) {
    if ((y & 15) == 0) WaitCursor();

    for (x=0; x<w; x++) {

      /* compute histogram of (on-screen hunk of) n*n region centered 
	 around x,y, for each plane */

      pp = plin = p24;
      rval = rcnt = gval = gcnt = bval = bcnt = 0;

      for (i=0; i<64; i++) rhist[i] = ghist[i] = bhist[i] = 0;

      for (i=y-n2; i<y+n2; i++) {
	if (i>=0 && i<h) {
	  for (j=x-n2; j<x+n2; j++) {
	    if (j>=0 && j<w) { 
	      k = ++rhist[*pp>>2];  
	      if (k > rcnt) { rval = *pp;  rcnt = k; }
	      pp++;

	      k = ++ghist[*pp>>2];
	      if (k > gcnt) { gval = *pp;  gcnt = k; }
	      pp++;

	      k = ++bhist[*pp>>2];
	      if (k > bcnt) { bval = *pp;  bcnt = k; }
	      pp++;
	    }
	    else pp += 3;
	  }
	}
	plin += bperlin;  pp = plin;
      }

      *rp++ = (byte) rval;
      *rp++ = (byte) gval;
      *rp++ = (byte) bval;

      p24 += 3;
    }
  }

  printUTime("end of doOilPaint");
}




/***********************************************/
static int start24bitAlg(pic24, tmpPic)
     byte **pic24, **tmpPic;
{
  /* generates a 24-bit version of 'pic', if neccessary, and also mallocs
   * a pWIDE*pHIGH*3 24-bit output pic.  
   *
   * Returns '1' if there's some sort of screwup, '0' if cool
   */


  if (picType == PIC8) {
    *pic24 = Conv8to24(pic, pWIDE, pHIGH, rMap, gMap, bMap);
    if (!*pic24) { SetCursors(-1);  return 1; }
  }
  else *pic24 = pic;


  /* need to create another w*h*3 pic to hold results */
  *tmpPic = (byte *) calloc(pWIDE * pHIGH * 3, 1);
  if (!(*tmpPic)) {
    SetCursors(-1);
    ErrPopUp("Unable to malloc() tmp 24-bit image in start24bitAlg()", 
	     "\nTough!");
    if (picType == PIC8) free(*pic24);
    return 1;
  }

  return 0;
}



/***********************************************/
static void end24bitAlg(pic24, outPic)
     byte *pic24, *outPic;
{
  /* given pic24, and outPic, which has the new 24-bit image, installs it */


  saveOrigPic();  /* also kills pic/cpic/epic/egampic/theImage, NOT pic24 */

  xvbcopy(outPic, pic24, pWIDE*pHIGH*3);  /* copy results into pic24 */
  free(outPic);

  if (picType == PIC8) {
    pic = Conv24to8(pic24, pWIDE, pHIGH, ncols, rMap,gMap,bMap);
    free(pic24);
    if (!pic) { 
      SetCursors(-1);
      ErrPopUp("Some sort of failure occured in 24to8 conversion\n", "Damn!");
      NoAlg(); 
      return;
    }
  }
  else pic = pic24;

  InstallNewPic();
}


/************************/
static void saveOrigPic()
{
  /* saves original picture into origPic, if it hasn't already been done.
     This allows us to undo algorithms...  

     Also, frees all pics, (except 'pic', if we're in PIC24 mode) */


  int i;

  FreeEpic();
  if (cpic && cpic != pic) free(cpic);
  xvDestroyImage(theImage);
  theImage = NULL;
  cpic = NULL;

  if (!origPic) {
    /* make a backup copy of 'pic' */
    origPic = (byte *) malloc(pWIDE * pHIGH * ((picType==PIC8) ? 1 : 3));
    if (!origPic) FatalError("out of memory in 'saveOrigPic()'");
    xvbcopy(pic, origPic, pWIDE * pHIGH * ((picType==PIC8) ? 1 : 3));

    origPicType = picType;

    if (picType == PIC8) {
      for (i=0; i<256; i++) {   /* save old colormap */
	origrmap[i] = rorg[i];
	origgmap[i] = gorg[i];
	origbmap[i] = borg[i];
      }
    }
  }


  if (picType != PIC24) {  /* kill pic, as well */
    if (pic) free(pic);
    pic = NULL;
  }
}



