/*
 * xvpm.c - load routine for 'pm' format pictures
 *
 * LoadPM();
 * WritePM(fp, pic, ptype, w, h, r,g,b, numcols, style, comment)
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
#include "pm.h"

pmpic  thePic;

#ifdef __STDC__
  static int pmError(char *, char *);
  static int flip4(int);
  static int getint32(FILE *);
  static void putint32(int, FILE *);
#else
  static int pmError();
  static int flip4();
  static int getint32();
  static void putint32();
#endif


/*******************************************/
int LoadPM(fname, pinfo)
     char    *fname;
     PICINFO *pinfo;
/*******************************************/
{
  /* returns '1' on success */

  FILE  *fp;
  byte  *pic8;
  int    isize,i,flipit,w,h;
  char  *bname;

  bname = BaseName(fname);
  thePic.pm_image = (char *) NULL;
  thePic.pm_cmt   = (char *) NULL;

  pinfo->pic = (byte *) NULL;
  pinfo->comment = (char *) NULL;


  fp=fopen(fname,"r");
  if (!fp) return( pmError(bname, "unable to open file") );

  /* read in the pmpic struct, one byte at a time */
  thePic.pm_id      = getint32(fp);
  thePic.pm_np      = getint32(fp);
  thePic.pm_nrow    = getint32(fp);
  thePic.pm_ncol    = getint32(fp);
  thePic.pm_nband   = getint32(fp);
  thePic.pm_form    = getint32(fp);
  thePic.pm_cmtsize = getint32(fp);

  if (ferror(fp)) return(pmError(bname, "error reading header"));

  flipit = 0;

  if (thePic.pm_id != PM_MAGICNO) {
    thePic.pm_id = flip4(thePic.pm_id);
    if (thePic.pm_id == PM_MAGICNO) flipit = 1;
    else thePic.pm_id = flip4(thePic.pm_id);
  }
  if (thePic.pm_id != PM_MAGICNO) return( pmError(bname, "not a PM file") );

  if (flipit) {
    thePic.pm_np      = flip4(thePic.pm_np);
    thePic.pm_nrow    = flip4(thePic.pm_nrow);
    thePic.pm_ncol    = flip4(thePic.pm_ncol);
    thePic.pm_nband   = flip4(thePic.pm_nband);
    thePic.pm_form    = flip4(thePic.pm_form);
    thePic.pm_cmtsize = flip4(thePic.pm_cmtsize);
    }

  w = thePic.pm_ncol;  h = thePic.pm_nrow;

  /* make sure that the input picture can be dealt with */
  if ( thePic.pm_nband!=1 || 
      (thePic.pm_form!=PM_I && thePic.pm_form!=PM_C) ||
      (thePic.pm_form==PM_I && thePic.pm_np>1) ||
      (thePic.pm_form==PM_C && (thePic.pm_np==2 || thePic.pm_np>4)) ) {
    fprintf(stderr,"PM picture not in a displayable format.\n");
    fprintf(stderr,"(ie, 1-plane PM_I, or 1-, 3-, or 4-plane PM_C)\n");

    return pmError(bname, "PM file in unsupported format");
  }	


  isize = pm_isize(&thePic);

  if (DEBUG) 
    fprintf(stderr,"%s: LoadPM() - loading a %dx%d %s pic, %d planes\n",
	    cmd, w, h, (thePic.pm_form==PM_I) ? "PM_I" : "PM_C", 
	    thePic.pm_np);

	      
  /* allocate memory for picture and read it in */
  thePic.pm_image = (char *) malloc(isize);
  if (thePic.pm_image == NULL) 
    return( pmError(bname, "unable to malloc PM picture") );

  if (fread(thePic.pm_image, (unsigned) isize, 1, fp) != 1)   {
    free(thePic.pm_image);
    return( pmError(bname, "file read error") );
  }


  /* alloc and read in comment, if any */
  if (thePic.pm_cmtsize>0) {
    thePic.pm_cmt = (char *) malloc(thePic.pm_cmtsize+1);
    if (thePic.pm_cmt) {
      thePic.pm_cmt[thePic.pm_cmtsize] = '\0';  /* to be safe */
      if (fread(thePic.pm_cmt, thePic.pm_cmtsize, 1, fp) != 1) {
	free(thePic.pm_cmt);
	thePic.pm_cmt = (char *) NULL;
      }
    }
  }
   
  fclose(fp);


  if (thePic.pm_form == PM_I) {
    int  *intptr;
    byte *pic24, *picptr;

    if ((pic24 = (byte *) malloc(w*h*3))==NULL) {
      if (thePic.pm_cmt) free(thePic.pm_cmt);
      return( pmError(bname, "unable to malloc 24-bit picture") );
    }
      
    intptr = (int *) thePic.pm_image;
    picptr = pic24;

    if (flipit) {    /* if flipit, integer is RRGGBBAA instead of AABBGGRR */
      for (i=w*h; i>0; i--, intptr++) {
	if ((i & 0x3fff) == 0) WaitCursor();
	*picptr++ = (*intptr>>24) & 0xff;
	*picptr++ = (*intptr>>16) & 0xff;
	*picptr++ = (*intptr>>8)  & 0xff;
      }
    }
    else {
      for (i=w*h; i>0; i--, intptr++) {
	if ((i & 0x3fff) == 0) WaitCursor();
	*picptr++ = (*intptr)     & 0xff;
	*picptr++ = (*intptr>>8)  & 0xff;
	*picptr++ = (*intptr>>16) & 0xff;
      }
    }

    free(thePic.pm_image);

    pinfo->pic  = pic24;
    pinfo->type = PIC24;
  }


  else if (thePic.pm_form == PM_C && thePic.pm_np>1) {
    byte *pic24, *picptr, *rptr, *gptr, *bptr;

    if ((pic24 = (byte *) malloc(w*h*3))==NULL) {
      if (thePic.pm_cmt) free(thePic.pm_cmt);
      return( pmError(bname, "unable to malloc 24-bit picture") );
    }

    rptr = (byte *) thePic.pm_image;
    gptr = rptr + w*h;
    bptr = rptr + w*h*2;
    picptr = pic24;
    for (i=w*h; i>0; i--) {
      if ((i & 0x3fff) == 0) WaitCursor();
      *picptr++ = *rptr++;
      *picptr++ = *gptr++;
      *picptr++ = *bptr++;
    }
    free(thePic.pm_image);

    pinfo->pic  = pic24;
    pinfo->type = PIC24;
  }
  

  else if (thePic.pm_form == PM_C && thePic.pm_np==1) {
    /* don't have to convert, just point pic at thePic.pm_image */
    pic8 = (byte *) thePic.pm_image;
    for (i=0; i<256; i++) 
      pinfo->r[i] = pinfo->g[i] = pinfo->b[i] = i;  /* build mono cmap */

    pinfo->pic  = pic8;
    pinfo->type = PIC8;
  }


  /* load up remaining pinfo fields */
  pinfo->w = thePic.pm_ncol;  pinfo->h = thePic.pm_nrow;
  pinfo->frmType = F_PM;
  pinfo->colType = (thePic.pm_form==PM_I || thePic.pm_np>1) 
                         ? F_FULLCOLOR : F_GREYSCALE;
  sprintf(pinfo->fullInfo,"PM, %s.  (%d plane %s)  (%d bytes)",
	  (thePic.pm_form==PM_I || thePic.pm_np>1) 
	        ? "24-bit color" : "8-bit greyscale",
	  thePic.pm_np, (thePic.pm_form==PM_I) ? "PM_I" : "PM_C",
	  isize + PM_IOHDR_SIZE + thePic.pm_cmtsize);

  sprintf(pinfo->shrtInfo, "%dx%d PM.", w,h);
  pinfo->comment = thePic.pm_cmt;

  return 1;
}


/*******************************************/
int WritePM(fp, pic, ptype, w, h, rmap, gmap, bmap, numcols, colorstyle,
	    comment)
     FILE *fp;
     byte *pic;
     int   ptype,w,h;
     byte *rmap, *gmap, *bmap;
     int   numcols, colorstyle;
     char *comment;
{
  /* writes a PM file to the already open stream
     'colorstyle' single-handedly determines the type of PM pic written
     if colorstyle==0, (Full Color) a 3-plane PM_C pic is written
     if colorstyle==1, (Greyscal) a 1-plane PM_C pic is written
     if colorstyle==0, (B/W stipple) a 1-plane PM_C pic is written */

  char  foo[256];
  int   i;
  byte *p;

  /* create 'comment' field */
  sprintf(foo,"CREATOR: XV %s\n", REVDATE);

  /* fill in fields of a pmheader */
  thePic.pm_id = PM_MAGICNO;
  thePic.pm_np = (colorstyle==0) ? 3 : 1;
  thePic.pm_ncol = w;
  thePic.pm_nrow = h;
  thePic.pm_nband = 1;
  thePic.pm_form  = PM_C;
  thePic.pm_cmtsize = (strlen(foo) + 1);   /* +1 to write trailing '\0' */
  if (comment) thePic.pm_cmtsize += (strlen(comment) + 1);

  putint32(thePic.pm_id, fp);
  putint32(thePic.pm_np, fp);
  putint32(thePic.pm_nrow, fp);
  putint32(thePic.pm_ncol, fp);
  putint32(thePic.pm_nband, fp);
  putint32(thePic.pm_form, fp);
  putint32(thePic.pm_cmtsize, fp);

  /* write the picture data */
  if (colorstyle == 0) {         /* 24bit RGB, organized as 3 8bit planes */

    if (ptype == PIC8) {
      WaitCursor();
      for (i=0,p=pic; i<w*h; i++, p++) putc(rmap[*p], fp);

      WaitCursor();
      for (i=0,p=pic; i<w*h; i++, p++) putc(gmap[*p], fp);

      WaitCursor();
      for (i=0,p=pic; i<w*h; i++, p++) putc(bmap[*p], fp);
    }

    else {  /* PIC24 */
      WaitCursor();
      for (i=0,p=pic; i<w*h; i++, p+=3) putc(*p, fp);

      WaitCursor();
      for (i=0,p=pic+1; i<w*h; i++, p+=3) putc(*p, fp);

      WaitCursor();
      for (i=0,p=pic+2; i<w*h; i++, p+=3) putc(*p, fp);
    }
  }


  else if (colorstyle == 1) {    /* GreyScale: 8 bits per pixel */
    byte rgb[256];
    
    if (ptype == PIC8) {
      for (i=0; i<numcols; i++) rgb[i] = MONO(rmap[i],gmap[i],bmap[i]);
      for (i=0, p=pic; i<w*h; i++, p++) {
	if ((i & 0x3fff) == 0) WaitCursor();
	putc(rgb[*p],fp);
      }
    }
    else {  /* PIC24 */
      for (i=0, p=pic; i<w*h; i++, p+=3) {
	if ((i & 0x3fff) == 0) WaitCursor();
	putc( MONO(p[0],p[1],p[2]), fp);
      }
    }
  }

  else /* (colorstyle == 2) */ { /* B/W stipple.  pic is 1's and 0's */
    /* note: pic has already been dithered into 8-bit image */
    for (i=0, p=pic; i<w*h; i++, p++) {
      if ((i & 0x3fff) == 0) WaitCursor();
      putc(*p ? 255 : 0,fp);
    }
  }

  if (comment) {
    fwrite(comment, strlen(comment), 1, fp);
    fwrite("\n", 1, 1, fp);
  }
  fwrite(foo, strlen(foo) + 1, 1, fp);  /* +1 to write trailing '\0' */

  if (ferror(fp)) return -1;

  return 0;
}


/*****************************/
static int pmError(fname, st)
     char *fname, *st;
{
  SetISTR(ISTR_WARNING,"%s:  %s", fname, st);
  if (thePic.pm_image != NULL) free(thePic.pm_image);
  return 0;
}


/*****************************/
static int flip4(i)
     int i;
{
  /* flips low-order 4 bytes around in integer */

  byte b0, b1, b2, b3;
  int  rv;

  b0 = (((u_long) i)) & 0xff;
  b1 = (((u_long) i) >>  8) & 0xff;
  b2 = (((u_long) i) >> 16) & 0xff;
  b3 = (((u_long) i) >> 24) & 0xff;

  rv = (((u_long) b0) << 24) |
       (((u_long) b1) << 16) |
       (((u_long) b2) <<  8) |
       (((u_long) b3));

  return rv;
}



static int getint32(fp)
     FILE *fp;
{
  int i;

  i  = (getc(fp) & 0xff) << 24;
  i |= (getc(fp) & 0xff) << 16;
  i |= (getc(fp) & 0xff) << 8;
  i |= (getc(fp) & 0xff);

  return i;
}



static void putint32(i, fp)
     int   i;
     FILE *fp;
{
  putc( ((i>>24) & 0xff), fp);
  putc( ((i>>16) & 0xff), fp);
  putc( ((i>> 8) & 0xff), fp);
  putc( ((i) & 0xff), fp);
}



