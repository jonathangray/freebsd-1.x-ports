#ifdef HAVE_JPEG

/*
 * xvjpeg.c - i/o routines for 'jpeg' format pictures
 *
 * LoadJFIF(fname, numcols)  -  loads a JPEG pic, does 24to8 code if nec.
 * WriteJFIF(fp, pic, w, h, rmap, gmap, bmap, numcols, colorstyle)
 */

/*
 * LoadJFIF Author: Markus Baur, University of Karlsruhe 
 *                  (s_baur@iravcl.ira.uka.de)
 * This software is provided "as is" without any express or implied warranty.
 */

/* WriteJFIF() and JPEG dialog routines written by John Bradley */

/* Copyright Notice
 * ================
 * Portions Copyright 1989, 1990, 1991, 1992, 1993 by John Bradley
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
#include "jinclude.h"
#include <setjmp.h>

#define CREATOR_STR "CREATOR: "


/**** Stuff for JPEGDialog box ****/

#define JWIDE 400
#define JHIGH 200
#define J_NBUTTS 2
#define J_BOK    0
#define J_BCANC  1
#define BUTTH    24

#ifdef __STDC__
  static void drawJD(int, int, int, int);
  static void clickJD(int, int);
  static void doCmd(int);
  static void writeJPEG(void);
  static void jselwxv(decompress_info_ptr);
  static int  writeJFIF(FILE *);
#else
  static void drawJD(), doCmd(), clickJD(), writeJPEG();
  static void jselwxv();
  static int writeJFIF();
#endif



/* local variables */
static char *filename;
static int   colorType;
static DIAL  qDial, smDial;
static BUTT  jbut[J_NBUTTS];
static byte *image8, *image24;
static char *loadComments;

static int   Width, Height;
static byte        *CurImagePtr;
static byte        *pic24, *pic8, *rmap, *gmap, *bmap;
static long  	    filesize;
static jmp_buf      jmpState;
static struct External_methods_struct   e_methods;


/***************************************************/
void CreateJPEGW()
{
  XClassHint classh;

  jpegW = CreateWindow("xv jpeg", "XVjpeg", NULL, 
		       JWIDE, JHIGH, infofg, infobg, 0);
  if (!jpegW) FatalError("can't create jpeg window!");

  XSelectInput(theDisp, jpegW, ExposureMask | ButtonPressMask | KeyPressMask);

  DCreate(&qDial, jpegW, 10, 10, 80, 100, 1, 100, 75, 5, 
	  infofg, infobg, hicol, locol, "Quality", "%");

  DCreate(&smDial, jpegW, 120, 10, 80, 100, 0, 100, 0, 5, 
	  infofg, infobg, hicol, locol, "Smoothing", "%");

  BTCreate(&jbut[J_BOK], jpegW, JWIDE-180-1, JHIGH-10-BUTTH-1, 80, BUTTH, 
	   "Ok", infofg, infobg, hicol, locol);

  BTCreate(&jbut[J_BCANC], jpegW, JWIDE-90-1, JHIGH-10-BUTTH-1, 80, BUTTH, 
	   "Cancel", infofg, infobg, hicol, locol);

  XMapSubwindows(theDisp, jpegW);
}
  

/***************************************************/
void JPEGDialog(vis)
int vis;
{
  if (vis) {
    CenterMapWindow(jpegW, jbut[J_BOK].x + jbut[J_BOK].w/2,
		    jbut[J_BOK].y + jbut[J_BOK].h/2, JWIDE, JHIGH);
  }
  else     XUnmapWindow(theDisp, jpegW);
  jpegUp = vis;
}


/***************************************************/
int JPEGCheckEvent(xev)
XEvent *xev;
{
  /* check event to see if it's for one of our subwindows.  If it is,
     deal accordingly, and return '1'.  Otherwise, return '0' */

  int rv;
  rv = 1;

  if (!jpegUp) return 0;

  if (xev->type == Expose) {
    int x,y,w,h;
    XExposeEvent *e = (XExposeEvent *) xev;
    x = e->x;  y = e->y;  w = e->width;  h = e->height;

    /* throw away excess expose events for 'dumb' windows */
    if (e->count > 0 && (e->window == qDial.win || 
			 e->window == smDial.win)) {}

    else if (e->window == jpegW)       drawJD(x, y, w, h);
    else if (e->window == qDial.win)   DRedraw(&qDial);
    else if (e->window == smDial.win)  DRedraw(&smDial);
    else rv = 0;
  }

  else if (xev->type == ButtonPress) {
    XButtonEvent *e = (XButtonEvent *) xev;
    int x,y;
    x = e->x;  y = e->y;

    if (e->button == Button1) {
      if      (e->window == jpegW)      clickJD(x,y);
      else if (e->window == qDial.win)  DTrack(&qDial,  x,y);
      else if (e->window == smDial.win) DTrack(&smDial, x,y);
      else rv = 0;
    }  /* button1 */
    else rv = 0;
  }  /* button press */


  else if (xev->type == KeyPress) {
    XKeyEvent *e = (XKeyEvent *) xev;
    char buf[128];  KeySym ks;
    int stlen;
	
    stlen = XLookupString(e,buf,128,&ks,(XComposeStatus *) NULL);
    buf[stlen] = '\0';

    if (e->window == jpegW) {
      if (stlen) {
	if (buf[0] == '\r' || buf[0] == '\n') { /* enter */
	  FakeButtonPress(&jbut[J_BOK]);
	}
	else if (buf[0] == '\033') {            /* ESC */
	  FakeButtonPress(&jbut[J_BCANC]);
	}
      }
    }
    else rv = 0;
  }
  else rv = 0;

  if (rv==0 && (xev->type == ButtonPress || xev->type == KeyPress)) {
    XBell(theDisp, 50);
    rv = 1;   /* eat it */
  }

  return rv;
}


/***************************************************/
void JPEGSaveParams(fname, col)
char *fname;
int col;
{
  filename = fname;
  colorType = col;
}


/***************************************************/
static void drawJD(x,y,w,h)
int x,y,w,h;
{
  char *title  = "Save JPEG file...";
  char *title1 = "Quality value determines";
  char *title2 = "compression rate: higher";
  char *title3 = "quality = bigger file.";
  char *title4 = "Use smoothing if saving";
  char *title5 = "an 8-bit image (eg, a GIF).";
  
  char *qtitle1 = "Default = 75.";
  char *qtitle2 = "Useful range";
  char *qtitle3 = "is 5-95.";
  char *smtitle1 = "Default = 0 (none).";
  char *smtitle2 = "10-30 is enough";
  char *smtitle3 = "for typical GIFs.";
  
  int  i;
  XRectangle xr;

  xr.x = x;  xr.y = y;  xr.width = w;  xr.height = h;
  XSetClipRectangles(theDisp, theGC, 0,0, &xr, 1, Unsorted);

  XSetForeground(theDisp, theGC, infofg);
  XSetBackground(theDisp, theGC, infobg);

  for (i=0; i<J_NBUTTS; i++) BTRedraw(&jbut[i]);


  XDrawString(theDisp, jpegW, theGC, 220, 10+ASCENT, 
	      title, strlen(title));
  XDrawString(theDisp, jpegW, theGC, 230, 10+ASCENT+LINEHIGH*1,
	      title1,strlen(title1));
  XDrawString(theDisp, jpegW, theGC, 230, 10+ASCENT+LINEHIGH*2,
	      title2,strlen(title2));
  XDrawString(theDisp, jpegW, theGC, 230, 10+ASCENT+LINEHIGH*3,
	      title3,strlen(title3));
  XDrawString(theDisp, jpegW, theGC, 230, 10+ASCENT+LINEHIGH*4,
	      title4,strlen(title4));
  XDrawString(theDisp, jpegW, theGC, 230, 10+ASCENT+LINEHIGH*5,
	      title5,strlen(title5));

  XDrawString(theDisp, jpegW, theGC, 15, 10+100+10+ASCENT,
	      qtitle1,strlen(qtitle1));
  XDrawString(theDisp, jpegW, theGC, 15, 10+100+10+ASCENT+LINEHIGH,
	      qtitle2,strlen(qtitle2));
  XDrawString(theDisp, jpegW, theGC, 15, 10+100+10+ASCENT+LINEHIGH*2,
	      qtitle3,strlen(qtitle3));
  
  XDrawString(theDisp, jpegW, theGC, 115, 10+100+10+ASCENT+LINEHIGH*0,
	      smtitle1,strlen(smtitle1));
  XDrawString(theDisp, jpegW, theGC, 115, 10+100+10+ASCENT+LINEHIGH*1,
	      smtitle2,strlen(smtitle2));
  XDrawString(theDisp, jpegW, theGC, 115, 10+100+10+ASCENT+LINEHIGH*2,
	      smtitle3,strlen(smtitle3));
  
  XSetClipMask(theDisp, theGC, None);
}


/***************************************************/
static void clickJD(x,y)
int x,y;
{
  int i;
  BUTT *bp;

  /* check BUTTs */

  for (i=0; i<J_NBUTTS; i++) {
    bp = &jbut[i];
    if (PTINRECT(x, y, bp->x, bp->y, bp->w, bp->h)) break;
  }

  if (i<J_NBUTTS) {  /* found one */
    if (BTTrack(bp)) doCmd(i);
  }
}



/***************************************************/
static void doCmd(cmd)
int cmd;
{
  switch (cmd) {
  case J_BOK:    writeJPEG();    JPEGDialog(0);  break;
  case J_BCANC:  JPEGDialog(0);  break;
  default:        break;
  }
}





/*******************************************/
static void writeJPEG()
{
  FILE *fp;
  int   i, nc, rv, w, h, ptype;
  register byte *ip, *ep;
  byte *inpix, *bwpic, *rmap, *gmap, *bmap, *gampic24;

  /* get the XV image into a format that the JPEG software can grok on.
     Also, open the output file, so we don't waste time doing this format
     conversion if we won't be able to write it out */

  if (savenormCB.val) { inpix = cpic;  w = cWIDE;  h = cHIGH; }
                 else { inpix = epic;  w = eWIDE;  h = eHIGH; }

  bwpic = NULL;  rmap = rMap;  gmap = gMap;  bmap = bMap;
  nc = numcols;
  ptype = picType;

  /* see if we can open the output file before proceeding */
  fp = OpenOutFile(filename);
  if (!fp) return;

  WaitCursor();
  
  bwpic = HandleBWandReduced(colorType, &nc, &rmap, &gmap, &bmap);
  if (bwpic) { inpix = bwpic;  ptype = PIC8; }


  /* deal with possiblity of Gamma-fication in 24-bit mode... */
  gampic24 = NULL;
  if (ptype == PIC24) {
    gampic24 = GammifyPic24(inpix, w, h);
    if (gampic24) inpix = gampic24;
  }


  /* monocity check.  if the image is mono, save it that way to save space */
  if (colorType != F_GREYSCALE) {
    if (ptype == PIC8) {
      for (i=0; i<nc && rmap[i]==gmap[i] && rmap[i]==bmap[i]; i++);
      if (i==nc) colorType = F_GREYSCALE;    /* made it all the way through */
    }
    else {  /* PIC24 */
      for (i=0,ip=inpix; i<w*h && ip[0]==ip[1] && ip[1]==ip[2]; i++,ip+=3);
      if (i==w*h) colorType = F_GREYSCALE;  /* all the way through */
    }
  }
    

  /* first thing to do is build an 8/24-bit Greyscale/TrueColor image
     (meaning: non-colormapped) */

  if (colorType == F_GREYSCALE) {   /* build an 8-bit Greyscale image */
    image8 = (byte *) malloc(w * h);
    if (!image8) FatalError("writeJPEG: unable to malloc image8\n");

    if (ptype == PIC8) {
      for (i=0,ip=image8,ep=inpix; i<w * h; i++, ip++, ep++)
	*ip = MONO(rmap[*ep], gmap[*ep], bmap[*ep]);
    }
    else {  /* PIC24 */
      for (i=0,ip=image8,ep=inpix; i<w*h; i++, ip++, ep+=3)
	*ip = MONO(ep[0],ep[1],ep[2]);
    }
  }

  else {    /* colorType = some color format */
    if (ptype == PIC8) {
      image24 = (byte *) malloc(w * h * 3);
      if (!image24) {  /* this simply isn't going to work */
	FatalError("writeJPEG: unable to malloc image24\n");
      }

      for (i=0, ip=image24, ep=inpix; i<w*h; i++, ep++) {
	*ip++ = rmap[*ep];
	*ip++ = gmap[*ep];
	*ip++ = bmap[*ep];
      }
    }

    else {  /* PIC24 */
      image24 = inpix;
    }
  }

  if (bwpic) free(bwpic);  /* won't need it anymore */

  /* in any event, we've got some valid image.  Do the JPEG Thing */
  rv = writeJFIF(fp);

  /* get rid of the greyscale/truecolor image */
  if (colorType == F_GREYSCALE) free(image8);
  else if (ptype==PIC8) free(image24);

  if (CloseOutFile(fp, filename, rv) == 0) {
    /* everything's cool! */
    DirBox(0);
  }

  if (gampic24) free(gampic24);

  SetCursors(-1);
}






/*********************************************/
/**** INTERFACE CODE FOR THE JPEG LIBRARY ****/
/*********************************************/



/********* JPEG DECOMPRESSION FUNCTIONS **********/

/**************************************************/
static void xv_jpeg_monitor(cinfo, loopcnt, looplimit)
     decompress_info_ptr cinfo;
     long loopcnt, looplimit;
{
  WaitCursor();

#ifdef FOO
  {
    int a,b;
    double percent;

    a = cinfo->completed_passes;
    b = cinfo->total_passes;

    percent = ((a + ((double) loopcnt / looplimit)) / (double) b) * 100.0;

    fprintf(stderr,"jpeg: %lf done.  loop: %ld, %ld  pass: %d, %d\n",
	    percent, loopcnt, looplimit, a, b);
  }
#endif
}


/**************************************************/
static void xv_process_comment(cinfo, length)
     decompress_info_ptr cinfo;
     long                length;
{
  char *sp, *oldsp;
  int   ch,hasnull;


  if (!loadComments) {
    loadComments = (char *) malloc(length + 1);
    if (loadComments) loadComments[0] = '\0';
  }
  else {
    loadComments = (char *) realloc(loadComments, 
				    strlen(loadComments) + length + 1);
  }
  
  if (!loadComments) FatalError("out of memory in xv_process_comment");

  oldsp = sp = loadComments + strlen(loadComments);

  hasnull = 0;
  while (length-- > 0) {
    ch = JGETC(cinfo);
    *sp++ = (char) ch;
    if (ch==0) hasnull = 1;
  }

  if (hasnull) sp = oldsp;     /* swallow comments that have nulls in them */

  *sp++ = '\0';
}


/**************************************************/
static void d_ui_method_selection(cinfo)
     decompress_info_ptr cinfo;
{
  int i;

  /* select output colorspace & quantization parameters */
  if (cinfo->jpeg_color_space == CS_GRAYSCALE) {
    cinfo->out_color_space = CS_GRAYSCALE;
    cinfo->quantize_colors = FALSE;

    SetISTR(ISTR_INFO,"Loading %ldx%ld Greyscale JPEG (%ld bytes)...",
	    cinfo->image_width, cinfo->image_height, filesize);

    /* fill in a greyscale colormap */
    for (i=0; i<256; i++) rmap[i] = gmap[i] = bmap[i] = i;
  }

  else {
    cinfo->out_color_space = CS_RGB;
    cinfo->quantize_colors = FALSE;   /* give 24-bit image to XV */

    SetISTR(ISTR_INFO,"Loading %ldx%ld Color JPEG (%ld bytes)...",
	    cinfo->image_width, cinfo->image_height, filesize);

  }

  jselwxv(cinfo);
}


/**************************************************/
static void output_init (cinfo)
     decompress_info_ptr cinfo;
{
  Width  = cinfo->image_width;
  Height = cinfo->image_height;

  if (cinfo->out_color_space == CS_GRAYSCALE) {
    pic8 = (byte *) malloc(Width * Height);
    if (!pic8) FatalError("Not enough memory for Image");
    CurImagePtr = pic8;
  }

  else {
    pic24 = (byte *) malloc(Width * Height * 3);
    if (!pic24) FatalError("Not enough memory for Image");
    CurImagePtr = pic24;
  }
}


/**************************************************/
static void put_color_map (cinfo, num_colors, colormap)
     decompress_info_ptr cinfo;
     int num_colors;
     JSAMPARRAY colormap;
{
  int i;

  for (i = 0; i < num_colors; i++) {
    rmap[i] = GETJSAMPLE(colormap[0][i]);
    gmap[i] = GETJSAMPLE(colormap[1][i]);
    bmap[i] = GETJSAMPLE(colormap[2][i]);
  }
}


/**************************************************/
static void put_pixel_rows (cinfo, num_rows, pixel_data)
     decompress_info_ptr cinfo;
     int                 num_rows;
     JSAMPIMAGE          pixel_data;
{
  JSAMPROW ptr0, ptr1, ptr2;
  long col;
  long width = cinfo->image_width;
  int row;
  static unsigned int totrows = 0;

  if (cinfo->out_color_space == CS_GRAYSCALE || 
      cinfo->quantize_colors == TRUE) {

    for (row = 0; row < num_rows; row++) {
      ptr0 = pixel_data[0][row];
      for (col = width; col > 0; col--) {
	*CurImagePtr++ = GETJSAMPLE(*ptr0++);
      }
      totrows++;
      if ((totrows & 0x1f) == 0) WaitCursor();
    }
  }

  else {
    for (row = 0; row < num_rows; row++) {
      ptr0 = pixel_data[0][row];
      ptr1 = pixel_data[1][row];
      ptr2 = pixel_data[2][row];
      for (col = width; col > 0; col--) {
	*CurImagePtr++ = GETJSAMPLE(*ptr0++);
	*CurImagePtr++ = GETJSAMPLE(*ptr1++);
	*CurImagePtr++ = GETJSAMPLE(*ptr2++);
      }
      totrows++;
      if ((totrows & 0x1f) == 0) WaitCursor();
    }
  }
}


/**************************************************/
static void output_term (cinfo)
     decompress_info_ptr cinfo;
{
  /* no work required */
}


/**************************************************/
static void jselwxv(cinfo)
     decompress_info_ptr cinfo;
{
  cinfo->methods->output_init = output_init;
  cinfo->methods->put_color_map = put_color_map;
  cinfo->methods->put_pixel_rows = put_pixel_rows;
  cinfo->methods->output_term = output_term;
}


/**************************************************/
static void JPEG_Message (msgtext)
     char *msgtext;
{
  char tempstr[200];

  sprintf(tempstr, msgtext,
	  e_methods.message_parm[0], e_methods.message_parm[1],
	  e_methods.message_parm[2], e_methods.message_parm[3],
	  e_methods.message_parm[4], e_methods.message_parm[5],
	  e_methods.message_parm[6], e_methods.message_parm[7]);
  SetISTR(ISTR_WARNING, tempstr);
}


/**************************************************/
static void JPEG_Error (msgtext)
     char *msgtext;
{
  char tempstr[200];

  sprintf(tempstr, msgtext,
	  e_methods.message_parm[0], e_methods.message_parm[1],
	  e_methods.message_parm[2], e_methods.message_parm[3],
	  e_methods.message_parm[4], e_methods.message_parm[5],
	  e_methods.message_parm[6], e_methods.message_parm[7]);
  SetISTR(ISTR_WARNING, tempstr);
  (*e_methods.free_all) ();	/* clean up memory allocation */
  longjmp(jmpState,1);
}








/*******************************************/
int LoadJFIF(fname, pinfo)
     char    *fname;
     PICINFO *pinfo;
/*******************************************/
{
  /* returns '1' on success, '0' on failure */
  int rtval;
  struct Decompress_info_struct    cinfo;
  struct Decompress_methods_struct dc_methods;

  /* Set up the input file */
  pinfo->type = PIC8;

#ifdef VMS
  if ((cinfo.input_file = fopen(fname, "r", "ctx=stm")) == NULL) return 0;
#else
  if ((cinfo.input_file = fopen(fname, "r")) == NULL) return 0;
#endif

  fseek(cinfo.input_file, 0L, 2);
  filesize = ftell(cinfo.input_file);
	    fseek(cinfo.input_file, 0L, 0);

  cinfo.output_file = NULL;	/* only wanna read */

  pic8 = pic24 = (byte *) NULL;
  loadComments = (char *) NULL;

  rmap = pinfo->r;  gmap = pinfo->g;  bmap = pinfo->b;

  /* Set up longjmp for error recovery out of JPEG_Error */
  rtval = setjmp(jmpState);
  if (rtval) {
    fclose(cinfo.input_file);	/* close input file */
    if (pic8)  free(pic8);   pic8  = (byte *) NULL;
    if (pic24) free(pic24);  pic24 = (byte *) NULL;
    if (loadComments) free(loadComments);
    return 0;
  }


  /* Initialize the system-dependent method pointers. */
  cinfo.methods  = &dc_methods;
  cinfo.emethods = &e_methods;
  e_methods.error_exit = JPEG_Error; /* provide my own error/message rtns */
  e_methods.trace_message = JPEG_Message;
  e_methods.trace_level = 0;	/* no tracing, thank you */

  e_methods.num_warnings = 0;        /* no warnings emitted yet */
  e_methods.first_warning_level = 0; /* display first corrupt-data warning */
  e_methods.more_warning_level = 3;  /* but suppress additional ones */

  jselmemmgr(&e_methods);	/* memory allocation routines */
  dc_methods.d_ui_method_selection = d_ui_method_selection;


  /* Set up default JPEG parameters. */
  j_d_defaults(&cinfo, TRUE);
  cinfo.desired_number_of_colors = 255;
  

  /* set up our progress-monitoring function */
  cinfo.methods->progress_monitor = xv_jpeg_monitor;

  cinfo.methods->process_comment  = xv_process_comment;
  

  /* Set up to read a JFIF or baseline-JPEG file. */
  /* A smarter UI would inspect the first few bytes of the input file */
  /* to determine its type. */
  jselrjfif(&cinfo);
  
  jpeg_decompress(&cinfo);

  fclose(cinfo.input_file);


  if (pic24) {
    pic8 = pic24;
    pinfo->type = PIC24;
  }

  pinfo->pic = pic8;
  pinfo->w = Width;  
  pinfo->h = Height;
  pinfo->frmType = F_JPEG;
  if (cinfo.jpeg_color_space == CS_GRAYSCALE) {
    sprintf(pinfo->fullInfo, "Greyscale JPEG. (%ld bytes)", filesize);
    pinfo->colType = F_GREYSCALE;
  }
  else {
    sprintf(pinfo->fullInfo, "Color JPEG. (%ld bytes)", filesize);
    pinfo->colType = F_FULLCOLOR;
  }

  sprintf(pinfo->shrtInfo, "%ldx%ld %s JPEG. ", cinfo.image_width, 
	  cinfo.image_height, 
	  (cinfo.out_color_space == CS_GRAYSCALE) ? "Greyscale " : "Color ");

  pinfo->comment = loadComments;
  
  if (pinfo->pic) return 1;
  return 0;
}






/********* JPEG COMPRESSION FUNCTIONS **********/


/**************************************************/
static void c_ui_method_selection(cinfo)
     compress_info_ptr cinfo;
{
  /* select output colorspace */
  if (colorType == F_GREYSCALE) {
    j_monochrome_default(cinfo);
  }
}


/**************************************************/
static void input_init (cinfo)
     compress_info_ptr cinfo;
{
  int w,h;

  if (colorType == F_GREYSCALE) {
    cinfo->input_components = 1;
    cinfo->in_color_space = CS_GRAYSCALE;
    CurImagePtr = image8;
  }

  else {
    cinfo->input_components = 3;
    cinfo->in_color_space = CS_RGB;
    CurImagePtr = image24;
  }

  if (savenormCB.val) { w = cWIDE;  h = cHIGH; }
                 else { w = eWIDE;  h = eHIGH; }

  cinfo->image_width  = w;
  cinfo->image_height = h;
  cinfo->data_precision = 8;
}


/**************************************************/
static void get_input_row(cinfo, pixel_row)
     compress_info_ptr cinfo;
     JSAMPARRAY        pixel_row;
{
  JSAMPROW ptr0, ptr1, ptr2;
  long col;
  static unsigned int totrows = 0;

  if (cinfo->input_components == 1) {
    ptr0 = pixel_row[0];
    for (col = cinfo->image_width; col > 0; col--) {
      *ptr0++ = *CurImagePtr++;
    }
    totrows++;
    if ((totrows & 0x1f) == 0) WaitCursor();
  }

  else {
    ptr0 = pixel_row[0];
    ptr1 = pixel_row[1];
    ptr2 = pixel_row[2];
    for (col = cinfo->image_width; col > 0; col--) {
      *ptr0++ = *CurImagePtr++;
      *ptr1++ = *CurImagePtr++;
      *ptr2++ = *CurImagePtr++;
    }
    totrows++;
    if ((totrows & 0x1f) == 0) WaitCursor();
  }
}


/**************************************************/
static void input_term (cinfo)
     compress_info_ptr cinfo;
{
  /* no work required */
}


/**************************************************/
static void jselrxv(cinfo)
     compress_info_ptr cinfo;
{
  cinfo->methods->input_init = input_init;
  cinfo->methods->get_input_row = get_input_row;
  cinfo->methods->input_term = input_term;
}



/*******************************************/
static int writeJFIF(fp)
     FILE *fp;
{
  int    i, rtval;
  struct Compress_info_struct    cinfo;
  struct Compress_methods_struct c_methods;
  char  *cmt, xvcmt[256];

  cmt = (char *) NULL;

  /* Set up longjmp for error recovery out of JPEG_Error */
  rtval = setjmp(jmpState);
  if (rtval) {
    if (picComments && cmt) free(cmt);
    return rtval;		/* no further cleanup needed */
  }
  
  /* Initialize the system-dependent method pointers. */
  cinfo.methods  = &c_methods;
  cinfo.emethods = &e_methods;
  e_methods.error_exit = JPEG_Error; /* provide my own error/message rtns */
  e_methods.trace_message = JPEG_Message;
  e_methods.trace_level = 0;	/* no tracing, thank you */

  e_methods.num_warnings = 0; /* no warnings emitted yet */
  e_methods.first_warning_level = 0; /* display first corrupt-data warning */
  e_methods.more_warning_level = 3; /* but suppress additional ones */

  jselmemmgr(&e_methods);	/* memory allocation routines */
  c_methods.c_ui_method_selection = c_ui_method_selection;

  /* Set up default JPEG parameters. */
  j_c_defaults(&cinfo, qDial.val, FALSE);

  cinfo.smoothing_factor = smDial.val;

  /* set up our progress-monitoring function */
  cinfo.methods->progress_monitor = xv_jpeg_monitor;

  sprintf(xvcmt, "%sXV %s  Quality = %d, Smoothing = %d\n",
	  CREATOR_STR, REVDATE, qDial.val, smDial.val);

  if (picComments) {   /* append XV comment */
    char *sp;  int done;
    i   = strlen(picComments);
    cmt = (char *) malloc(i + strlen(xvcmt) + 2 + 1);
    if (!cmt) FatalError("out of memory in writeJFIF()");

    strcpy(cmt, picComments);

    /* see if there's a line that starts with 'CREATOR: ' in the
       comments.  If there is, rip it out. */

    sp = cmt;  done = 0;
    while (!done && *sp) {
      if (strncmp(sp, CREATOR_STR, strlen(CREATOR_STR)) == 0) {
	char *sp1;
	sp1 = sp;
	/* find end of this line */
	while (*sp1 && *sp1 != '\n') sp1++;
	if (*sp1 == '\n') sp1++;               /* move past \n */

	/* move comments from sp1 and on down to sp */
	xvbcopy(sp1, sp, strlen(sp1) + 1);   /* +1 to copy the trailing \0 */

	done = 1;
      }
      else {   /* skip ahead to next line */
	while (*sp && *sp != '\n') sp++;
	if (*sp == '\n') sp++;
      }
    }

    /* count # of \n's at end of comment.  
       If none, add 2.   If one, add 1.  If two or more, add none. */

    sp = cmt + strlen(cmt);
    for (i=0; i<3 && i<strlen(cmt); i++) {
      sp--;
      if (*sp != '\n') break;
    }

    for ( ; i<2; i++) strcat(cmt, "\n");
    strcat(cmt, xvcmt);

    cinfo.comment_text = cmt;
  }
  else cinfo.comment_text = xvcmt;

    

  /* Select input and output */
  cinfo.input_file  = NULL;
  cinfo.output_file = fp;       /* already open */
  
  jselrxv(&cinfo);		/* we'll be reading from memory */
  jselwjfif(&cinfo);		/* and writing to JFIF file format */

  /* Do it! */
  jpeg_compress(&cinfo);
  
  if (picComments && cmt) free(cmt);

  return 0;
}


#endif  /* HAVE_JPEG */
