/*
 * xvrle.c - load routine for rle (Utah Raster Toolkit) format pictures
 *
 * LoadRLE(fname, numcols)  -  loads an RLE file
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


/* Definitions needed to parse RLE format */
/*   snarfed from rle_code.h, part of the Utah Raster Toolkit */
#define     LONG_OP             0x40
#define     RSkipLinesOp        1
#define     RSetColorOp         2
#define     RSkipPixelsOp       3
#define     RByteDataOp         5
#define     RRunDataOp          6
#define     REOFOp              7

#define     H_CLEARFIRST        0x1   /* clear framebuffer flag */
#define     H_NO_BACKGROUND     0x2   /* if set, no bg color supplied */
#define     H_ALPHA             0x4   /* if set, alpha channel (-1) present */
#define     H_COMMENT           0x8   /* if set, comments present */


#define GETINT(fp) (c=getc(fp), c1=getc(fp), (c1<<8) + c )

static void read_rle();
static int  rleError();



/*******************************************/
int LoadRLE(fname, pinfo)
     char    *fname;
     PICINFO *pinfo;
/*******************************************/
{
  FILE  *fp;
  int    c, c1, i, j, k;
  byte   bgcol[256];
  byte   maps[3][256];
  int    xpos, ypos, w, h, flags, ncolors, pixelbits, ncmap, cmaplen;
  int    cmtlen;
  byte  *img, *pic8;
  long filesize;
  char  *bname, *errstr;

  pinfo->type = PIC8;
  pinfo->pic     = (byte *) NULL;
  pinfo->comment = (char *) NULL;

  bname = BaseName(fname);

  /* open the stream */
  fp=fopen(fname,"r");
  if (!fp) return (rleError(bname, "unable to open file"));
  

  /* figure out the file size */
  fseek(fp, 0L, 2);
  filesize = ftell(fp);
  fseek(fp, 0L, 0);


  /* read the magic number */
  c = getc(fp);  c1 = getc(fp);
  if ((c != 0x52) || (c1 != 0xcc))
    return(rleError(bname, "unrecognized magic number"));


  xpos       = GETINT(fp);    /* read rest of header info */
  ypos       = GETINT(fp);
  w          = GETINT(fp);
  h          = GETINT(fp);
  flags      = getc(fp);
  ncolors    = getc(fp);
  pixelbits  = getc(fp);
  ncmap      = getc(fp);
  c          = getc(fp);
  cmaplen = (1L << c);

  if (DEBUG) {
    fprintf(stderr,"RLE: %dx%d image at %d,%d\n", w, h, xpos, ypos);
    fprintf(stderr,"flags: 0x%02x  (%s%s%s%s)\n",
	    flags, 
	    (flags & H_CLEARFIRST)    ? "CLEARFIRST " : "",
	    (flags & H_NO_BACKGROUND) ? "NO_BG " : "",
	    (flags & H_ALPHA)         ? "ALPHA " : "",
	    (flags & H_COMMENT)       ? "COMMENT" : "");

    fprintf(stderr, "%d bands, %d pixelbits, %d cmap bands, %d cmap entries\n",
	    ncolors, pixelbits, ncmap, cmaplen);
  }  

  if (!(flags & H_NO_BACKGROUND)) {
    if (DEBUG) fprintf(stderr, "background value: ");
    for (i=0; i<ncolors; i++) {
      bgcol[i] = getc(fp);
      if (DEBUG) fprintf(stderr, "0x%02x ", bgcol[i]);
    }
    if (DEBUG) fprintf(stderr,"\n");
  }
  else {
    getc(fp);  /* skip filler byte */
  }

  if ((ncolors % 2) == 0) getc(fp);     /* get on a word boundary */

  /* read colormap(s) */
  for (i=0; i<ncmap; i++) {
    for (j = 0; j<cmaplen; j++) {
      k = GETINT(fp);
      if (i<3) maps[i][j] = k>>8;
    }
  }

  if (DEBUG) {
    if (ncmap) {
      fprintf(stderr, "Colormap:\n");
      for (i=0; i<cmaplen; i++) {
	fprintf(stderr,"(");
	for (j=0; (j<ncmap && j<3); j++) {
	  fprintf(stderr, "%02x ", maps[j][i]);
	}
	fprintf(stderr,")  ");
      }
      fprintf(stderr,"\n\n");
    }
    else fprintf(stderr,"No colormap\n");
  }


  /* read (skip over, actually) the comments, if any */
  if (flags & H_COMMENT) {
    cmtlen = GETINT(fp);
    if (cmtlen) {
      pinfo->comment = (char *) malloc(cmtlen + 1);

      if (DEBUG) fprintf(stderr,"Comment: (%d bytes) '", cmtlen);
      for (i=0; i<cmtlen; i++) {
	c = getc(fp);
	if (c==EOF) break;
	if (pinfo->comment) {
	  if (c == '\0') c = '\n';     /* translate NUL to NL */
	  pinfo->comment[i] = (char) c;
	}
	if (DEBUG) fprintf(stderr,"%c",c);
      }
      if (pinfo->comment) pinfo->comment[i] = '\0';

      if (cmtlen % 2) getc(fp);    /* get on a word boundary */
      if (DEBUG) fprintf(stderr,"'\n\n");
    }
  }


  if (ferror(fp)) {
    fclose(fp);
    if (pinfo->comment) free(pinfo->comment);  pinfo->comment = (char *) NULL;
    return rleError(bname, "EOF reached in RLE header.\n");
  }


  /*
   * Acceptable cases:
   *   ncolors = 1, 3, or >3 (extra planes ignored)
   *   pixelbits = 8
   *   ncmap = 0    (interpreted as TrueColor/TrueGray)
   *           1    (TrueColor/TrueGray with a gamma curve)
   *    3 | ncolors (TrueColor with three gamma curves, or
   *                 PseudoColor if ncolors==1)
   */

  errstr = NULL;
  if (ncolors == 0 || ncolors == 2)
    errstr = "Unsupt. # of channels in RLE file.\n";

  if (pixelbits != 8)
    errstr = "Only 8-bit pixels supported in RLE files.\n";

  if (ncmap==0 || ncmap==1 || ncmap == 3 || ncmap == ncolors) { /* ok */ }
  else errstr = "Invalid # of colormap channels in RLE file.\n";

  if (w<1 || h<1)
    errstr = "Bogus size in RLE header.\n";


  if (errstr) {
    fclose(fp);
    if (pinfo->comment) free(pinfo->comment);  pinfo->comment = (char *) NULL;
    return rleError(bname, errstr);
  }


  /* allocate image memory */
  if (ncolors == 1) img = (byte *) calloc(w * h, 1);
               else img = (byte *) calloc(w * h * 3, 1);
  if (!img) {
    fclose(fp);
    if (pinfo->comment) free(pinfo->comment);  pinfo->comment = (char *) NULL;
    return rleError(bname, "unable to allocate image data.\n");
  }


  /* set background, if necessary */
  if ((flags & H_CLEARFIRST) && !(flags & H_NO_BACKGROUND)) {
    byte *ip;
    if (ncolors == 1) {
      for (i=0, ip=img; i<w*h; i++, ip++) *ip = bgcol[0];
    }
    else {
      for (i=0, ip=img; i<w*h; i++) 
	for (j=0; j<3; j++, ip++) *ip = bgcol[j];
    }
  }


  read_rle(fp, img, w, h, ncolors, ncmap);

  if (ferror(fp))    /* just a warning */
    rleError(bname, "RLE file appears to be truncated.");

  fclose(fp);


  /* apply gamma curve(s) to image (if grayscale or truecolor) */
  if (ncmap) {
    byte *ip;
    int   imagelen, cmask;
    imagelen = (ncolors==1) ? w*h : w*h*3;
    cmask = (cmaplen-1);

    if (ncmap == 1) {   /* single gamma curve */
      for (i=0, ip=img; i<imagelen; i++, ip++) *ip = maps[0][*ip & cmask];
    }

    else if (ncmap >= 3 && ncolors >=3) {   /* one curve per band */
      for (i=0, ip=img; i<w*h; i++) {
	*ip = maps[0][*ip & cmask];   ip++;
	*ip = maps[1][*ip & cmask];   ip++;
	*ip = maps[2][*ip & cmask];   ip++;
      }
    }
  }


  /* finally, convert into XV internal format */

  pinfo->pic = img;
  pinfo->w   = w;  
  pinfo->h   = h;
  pinfo->frmType = -1;    /* no default format to save in */

  if (ncolors == 1) {     /* grayscale or PseudoColor */
    pinfo->type = PIC8;
    if (ncmap == 1) {
      pinfo->colType = F_GREYSCALE;
      sprintf(pinfo->fullInfo, "Greyscale RLE.  (%ld bytes)", filesize);
      for (i=0; i<256; i++) 
	pinfo->r[i] = pinfo->g[i] = pinfo->b[i] = i;
    }
    else {
      pinfo->colType = F_FULLCOLOR;
      sprintf(pinfo->fullInfo, "PseudoColor RLE.  (%ld bytes)", filesize);
      for (i=0; i<256; i++) {
	pinfo->r[i] = maps[0][i];
	pinfo->g[i] = maps[1][i];
	pinfo->b[i] = maps[2][i];
      }
    }
    
    sprintf(pinfo->shrtInfo, "%dx%d RLE.",w, h);
  }

  else {                  /* true color */
    pinfo->type = PIC24;
    pinfo->colType = F_FULLCOLOR;
    sprintf(pinfo->fullInfo, "TrueColor RLE.  (%ld bytes)", filesize);
    sprintf(pinfo->shrtInfo, "%dx%d RLE.", w, h);
  }

  return 1;
}


/*******************************************/
static void read_rle(fp, img, w, h, ncolors, ncmap)
     FILE *fp;
     byte *img;
     int   w, h, ncolors, ncmap;
{
  int posx, posy, plane, bperpix, i, pixval, skipcalls;
  int opcode, operand, done, c, c1;    
  byte *ip;

  posx = posy = plane = done = skipcalls = 0;
  if (ncolors == 1) bperpix = 1;
               else bperpix = 3;


  while (!done && (opcode=getc(fp)) != EOF) {
    switch (opcode & 0x3f) {
    case RSkipLinesOp:
      if (opcode & LONG_OP) { getc(fp);  operand = GETINT(fp); }
      else operand = getc(fp);
      posx = 0;
      posy += operand;
      skipcalls++;
      if ((skipcalls & 0x7f)==0) WaitCursor();
      break;


    case RSetColorOp:
      operand = getc(fp);
      plane = operand;
      posx = 0;
      break;


    case RSkipPixelsOp:
      if (opcode & LONG_OP) { getc(fp);  operand = GETINT(fp); }
      else operand = getc(fp);
      
      posx += operand;
      break;


    case RByteDataOp:
      if (opcode & LONG_OP) { getc(fp);  operand = GETINT(fp); }
      else operand = getc(fp);

      ip = img + ((h-posy-1) * w*bperpix) + posx*bperpix + plane;
      operand++;

      for (i=0; i<operand; i++, ip+=bperpix) {
	c = getc(fp);
	if (plane<ncolors && posy<h && (posx+i < w)) *ip = c;
      }
      
      if (operand & 1) getc(fp);  /* word boundary */
      posx += operand;
      break;


    case RRunDataOp:
      if (opcode & LONG_OP) { getc(fp);  operand = GETINT(fp); }
      else operand = getc(fp);

      pixval = getc(fp);  getc(fp);
      operand++;

      ip = img + ((h-posy-1) * w*bperpix) + posx*bperpix + plane;

      for (i=0; i<operand; i++, ip+=bperpix) {
	if (plane<ncolors && posy<h && (posx+i < w)) *ip = pixval;
      }
      
      /*  if (operand & 1) getc(fp); */  /* word boundary */
      posx += operand;
      break;

    case REOFOp:
      done = 1;
      break;
    }
  }
}


/*******************************************/
static int rleError(fname,st)
     char *fname, *st;
{
  SetISTR(ISTR_WARNING,"%s:  %s", fname, st);
  return 0;
}

