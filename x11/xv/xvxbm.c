/*
 * xvxbm.c - load routine for X11 Bitmap format pictures
 *
 * LoadXBM(fname)  -  loads an X11 Bitmap file\
 * WriteXBM(fp, pic, w, h)
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



/*
 * File Format:
 *   (format identifier:  "#define" as first couple chars in file)
 *
 * looks for first line beginning with '#define'
 *   reads "#define identifier width"  (identifier is ignored)
 * looks for next line beginning with '#define'
 *   reads "#define identifier height" (identifier is ignored)
 * looks for next occurence of characters '0x'
 *   read next two chars as two hex digits
 *   move forward to next occurence of '0x'
 *   repeat
 */
 

static int xbmError();


/*******************************************/
int LoadXBM(fname, pinfo)
     char    *fname;
     PICINFO *pinfo;
{
  /* returns '1' on success */

  FILE  *fp;
  int    c, c1;
  int    i, j, k, bit, w, h;
  byte  *pix, *pic8;
  long   filesize;
  char   line[256];
  byte   hex[256];
  char  *bname;

  k = 0;

  bname = BaseName(fname);
  fp=fopen(fname,"r");
  if (!fp) return (xbmError(bname, "couldn't open file"));

  fseek(fp, 0L, 2);
  filesize = ftell(fp);
  fseek(fp, 0L, 0);


  /* read width:  skip lines until we hit a #define */
  while (1) {
    if (!fgets(line,256,fp)) 
      return(xbmError(bname, "EOF reached in header info."));

    if (strncmp(line,"#define",7)==0) {
      if (sscanf(line,"#define %*s %d", &w) != 1) 
	return(xbmError(bname, "Unable to read 'width'"));
      else break;
    }
  }


  /* read height:  skip lines until we hit another #define */
  while (1) {
    if (!fgets(line,256,fp)) 
      return(xbmError(bname, "EOF reached in header info."));

    if (strncmp(line,"#define",7)==0) {
      if (sscanf(line,"#define %*s %d", &h) != 1) 
	return(xbmError(bname, "Unable to read 'height'"));
      else break;
    }
  }



  /* scan forward until we see the first '0x' */
  c = getc(fp);  c1 = getc(fp);
  while (c1!=EOF && !(c=='0' && c1=='x') ) { c = c1;  c1 = getc(fp); }

  if (c1==EOF) 
    return(xbmError(bname, "No bitmap data found"));


  pic8 = (byte *) calloc(w*h,1);
  if (!pic8) return(xbmError(bname, "couldn't malloc 'pic8'"));

  /* load up the pinfo structure */
  pinfo->pic = pic8;
  pinfo->w = w;     
  pinfo->h = h;
  pinfo->type = PIC8;
  pinfo->frmType = F_XBM;
  pinfo->colType = F_BWDITHER;
  sprintf(pinfo->fullInfo, "X11 Bitmap  (%ld bytes)", filesize);
  sprintf(pinfo->shrtInfo, "%dx%d X11 Bitmap.",w,h);
  pinfo->comment = (char *) NULL;

  /* B/W bitmaps have a two entry colormap */
  pinfo->r[0] = pinfo->g[0] = pinfo->b[0] = 255;     /* 0 = white */
  pinfo->r[1] = pinfo->g[1] = pinfo->b[1] = 0;       /* 1 = black */


  /* initialize the 'hex' array for zippy ASCII-hex -> int conversion */

  for (i=0; i<256; i++) hex[i]=0;
  for (i='0'; i<='9'; i++) hex[i] = i - '0';
  for (i='a'; i<='f'; i++) hex[i] = i + 10 - 'a';
  for (i='A'; i<='F'; i++) hex[i] = i + 10 - 'A';

  /* read/convert the image data */

  for (i=0, pix=pic8; i<h; i++)
    for (j=0,bit=0; j<w; j++, pix++, bit = (++bit)&7) {

      if (!bit) {
	/* get next byte from file.  we're already positioned at it */
	c = getc(fp);  c1 = getc(fp);
	if (c<0 || c1<0) { 
	  /* EOF: break out of loop */	  
	  c=c1='0'; i=h; j=w;
	  xbmError(bname, "The file would appear to be truncated.");
	}

	k = (hex[c] << 4) + hex[c1];

	/* advance to next '0x' */
	c = getc(fp);  c1 = getc(fp);
	while (c1!=EOF && !(c=='0' && c1=='x') ) { c = c1;  c1 = getc(fp); }
      }

      *pix = (k&1) ? 1 : 0;
      k = k >> 1;
    }

  fclose(fp);

  return 1;
}  



/*******************************************/
static int xbmError(fname, st)
     char *fname, *st;
{
  SetISTR(ISTR_WARNING,"%s:  %s", fname, st);
  return 0;
}


/*******************************************/
int WriteXBM(fp, pic, w, h, rmap, gmap, bmap, fname)
     FILE *fp;
     byte *pic;
     int   w,h;
     byte *rmap, *gmap, *bmap;
     char *fname;
{
  /* pic is expected to be an array of w*h bytes, each of which is either
     '0' or '1'.
     The 'darker' of {rmap,gmap,bmap}[0] and {rmap,gmap,bmap}[1] is 
     considered black, and the other one, white.
     Some sort of stippling algorithm should've
     been called already to produce pic, otherwise the output won't be at all
     useful */

  int   i,j,k,bit,len,nbytes,flipbw;
  byte *pix;
  char name[256], *foo;

  foo = BaseName(fname);
  strcpy(name, foo);

  foo = (char *) strchr(name,'.');
  if (foo) *foo='\0';                 /* truncated name at first '.' */

  fprintf(fp,"#define %s_width %d\n",name,w);  
  fprintf(fp,"#define %s_height %d\n",name,h);
  fprintf(fp,"static char %s_bits[] = {\n",name);

  fprintf(fp," ");

  /* set flipbw if color#0 is black */
  flipbw = (MONO(rmap[0],gmap[0],bmap[0]) < MONO(rmap[1],gmap[1],bmap[1]));

  nbytes = h * ((w+7)/8);   /* # of bytes to write */

  for (i=0, len=1, pix=pic; i<h; i++) {
    for (j=bit=k=0; j<w; j++,pix++) {
      k = (k>>1);
      if (*pix) k |= 0x80;
      bit++;
      if (bit==8) {
	if (flipbw) k = ~k;
	fprintf(fp,"0x%02x",(byte) k&0xff);
	nbytes--;  len += 4;
	if (nbytes) { fprintf(fp,",");  len++; }
	if (len>72) { fprintf(fp,"\n ");  len=1; }
	bit = k = 0;
      }
    }

    if (bit) {
      k = k >> (8-bit);
      if (flipbw) k = ~k;
      fprintf(fp,"0x%02x",(byte) k&0xff);
      nbytes--;  len += 4;
      if (nbytes) { fprintf(fp,",");  len++; }
      if (len>72) { fprintf(fp,"\n ");  len=1; }
    }
  }

  fprintf(fp,"};\n");

  if (ferror(fp)) return -1;
  return 0;
}



