/*
 * xvdflt.c - load routine for 'default' XV image
 *
 * LoadDfltPic()  -  loads up 'pic'  note:  can't fail(!)
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

#ifdef VMS
#  include "[.bitmaps]xvpic_logo_top."
#  include "[.bitmaps]xvpic_logo_bot."
#  include "[.bitmaps]xvpic_logo_out."
#  include "[.bitmaps]xv_jhb."
#  include "[.bitmaps]xv_cpyrt."
#  include "[.bitmaps]xv_rev."
#  include "[.bitmaps]xv_ver."
#  include "[.bitmaps]xf_left."
#  include "[.bitmaps]xf_right."
#  include "[.bitmaps]font5x9.h"
#else
#  include "bitmaps/xvpic_logo_top"
#  include "bitmaps/xvpic_logo_bot"
#  include "bitmaps/xvpic_logo_out"
#  include "bitmaps/xv_jhb"
#  include "bitmaps/xv_cpyrt"
#  include "bitmaps/xv_rev"
#  include "bitmaps/xv_ver"
#  include "bitmaps/xf_left"
#  include "bitmaps/xf_right"
#  include "bitmaps/font5x9.h"
#endif


#define DWIDE 480
#define DHIGH 270


/* local function defs */
#ifdef __STDC__
static void setcolor(PICINFO *, int, int, int, int);
static void gen_bg(byte *, PICINFO *);
static void drawstr(char *, int, int, byte *, int, int, int);
#else
static void setcolor(), gen_bg(), drawstr();
#endif


/*******************************************/
void LoadDfltPic(pinfo)
     PICINFO *pinfo;
/*******************************************/
{
  /* load up the stuff XV expects us to load up */

  char str[256];
  byte *dfltpic;
  int   i, j, k;

  dfltpic = (byte *) calloc(DWIDE * DHIGH,1);
  if (!dfltpic) FatalError("couldn't malloc 'dfltpic' in LoadDfltPic()");


  if (ncols) {    /* draw fish texture */
    for (i=k=0; i<DHIGH; i+=xf_left_height) {
      for (j=0; j<DWIDE; j+=xf_left_width) {
	k++;
	if (k&1) 
	  xbm2pic(xf_left_bits, xf_left_width, xf_left_height,
		  dfltpic, DWIDE, DHIGH, j + xf_left_width/2,
		  i + xf_left_height/2, 1);
      }
    }
  }



  xbm2pic(xvpic_logo_out_bits, xvpic_logo_out_width, xvpic_logo_out_height, 
	   dfltpic, DWIDE, DHIGH, DWIDE/2 + 10, 80, 103);

  xbm2pic(xvpic_logo_top_bits, xvpic_logo_top_width, xvpic_logo_top_height, 
	   dfltpic, DWIDE, DHIGH, DWIDE/2 + 10, 80, 100);

  xbm2pic(xvpic_logo_bot_bits, xvpic_logo_bot_width, xvpic_logo_bot_height, 
	   dfltpic, DWIDE, DHIGH, DWIDE/2 + 10, 80, 101);



  xbm2pic(xv_jhb_bits, xv_jhb_width, xv_jhb_height, 
	   dfltpic, DWIDE, DHIGH, DWIDE/2, 160, 102);

  xbm2pic(xv_cpyrt_bits, xv_cpyrt_width, xv_cpyrt_height, 
	   dfltpic, DWIDE, DHIGH, DWIDE/2, 213, 102);

  i = xv_ver_width + xv_rev_width + 30;

  xbm2pic(xv_ver_bits, xv_ver_width, xv_ver_height, 
	   dfltpic, DWIDE, DHIGH, DWIDE/2 - (i/2) + xv_ver_width/2, 230, 102);

  xbm2pic(xv_rev_bits, xv_rev_width, xv_rev_height, 
	   dfltpic, DWIDE, DHIGH, DWIDE/2 + (i/2) - xv_rev_width/2, 230, 102);


  strcpy(str,"UNREGISTERED COPY:  See 'License' for registration info.");

#ifdef REGSTR
  strcpy(str,REGSTR);
#endif

  drawstr(str, DWIDE/2, 258, dfltpic, DWIDE, DHIGH, 104);

  setcolor(pinfo, 0, 225,  150, 255);  /* top-left fish color */
  setcolor(pinfo, 15, 55,    0,  77);  /* bot-right fish color */
  setcolor(pinfo, 16, 150, 150, 255);  /* top-left background color */
  setcolor(pinfo, 63,   0,   0,  77);  /* bottom-right background color */

  if (ncols) gen_bg(dfltpic, pinfo);

  /* set up colormap */
  setcolor(pinfo, 100, 255,213, 25);   /* XV top half */
  setcolor(pinfo, 101, 255,000,000);   /* XV bottom half */
  setcolor(pinfo, 102, 255,208,000);   /* jhb + fish + revdate */
  setcolor(pinfo, 103, 220,220,220);   /* XV backlighting */
  setcolor(pinfo, 104, 255,255,255);   /* registration string */

  if (ncols==0) {
    setcolor(pinfo,   0, 0, 0, 0);
    setcolor(pinfo, 102,255,255,255);
    setcolor(pinfo, 103,255,255,255);
    setcolor(pinfo, 104,255,255,255);
  }

  pinfo->pic     = dfltpic;
  pinfo->w       = DWIDE;
  pinfo->h       = DHIGH;
  pinfo->type    = PIC8;
  pinfo->frmType = F_GIF;
  pinfo->colType = F_FULLCOLOR;

  if (ncols==0) pinfo->colType = F_BWDITHER;

  sprintf(pinfo->fullInfo, "<internal>");
  sprintf(pinfo->shrtInfo, "%dx%d internal image.",DWIDE, DHIGH);
  pinfo->comment = (char *) NULL;
}



/*******************************************/
void xbm2pic(bits, bwide, bhigh, pic, pwide, phigh, cx, cy, col)
     char *bits;
     byte *pic;
     int   bwide, bhigh, pwide, phigh, cx, cy, col;
/*******************************************/
{
  /* draws an X bitmap into an 8-bit 'pic'.  Only '1' bits from the bitmap
     are drawn (in color 'col').  '0' bits are ignored */

  int     i, j, k, bit, x, y;
  byte   *pptr, *bptr;

  y = cy - bhigh/2;

  for (i=0; i<bhigh; i++,y++) {
    if ( (y>=0) && (y<phigh) ) {
      pptr = pic + y * pwide;
      bptr = (byte *) bits + i * ((bwide+7)/8);
      x = cx - bwide/2;

      for (j=0,bit=0; j<bwide; j++, bit = (++bit)&7, x++) {
	if (!bit) k = *bptr++;
	if ( (k&1) && (x>=0) && (x<pwide))
	  pptr[x] = col;

	k = k >> 1;
      }
    }
  }
}  


/*******************************************/
static void setcolor(pinfo, i, rv, gv, bv)
     PICINFO *pinfo;
     int i, rv, gv, bv;
{
  pinfo->r[i] = rv;
  pinfo->g[i] = gv;
  pinfo->b[i] = bv;
}


/*******************************************/
static void gen_bg(dfltpic, pinfo)
     byte    *dfltpic;
     PICINFO *pinfo;
{
  int i,j,k, dr, dg, db;
  byte *pp;

  pp = dfltpic;
  for (i=0; i<DHIGH; i++)
    for (j=0; j<DWIDE; j++, pp++) {
      if (*pp == 0) {
	*pp = 16 + ((i+j) * 48) / (DHIGH + DWIDE);
      }
      else if (*pp == 1) {
	*pp = ((i+j) * 16) / (DHIGH + DWIDE);
      }
    }


  /* color gradient in cells 0-15 */
  for (i=1; i<15; i++) {
    dr = (int) pinfo->r[15] - (int) pinfo->r[0];
    dg = (int) pinfo->g[15] - (int) pinfo->g[0];
    db = (int) pinfo->b[15] - (int) pinfo->b[0];

    setcolor(pinfo, i, (int) pinfo->r[0] + (dr * i) / 15,
	               (int) pinfo->g[0] + (dg * i) / 15,
                       (int) pinfo->b[0] + (db * i) / 15);
  }

  /* color gradient in cells 16-63 */
  for (i=17, j=1; i<63; i++,j++) {
    dr = (int) pinfo->r[63] - (int) pinfo->r[16];
    dg = (int) pinfo->g[63] - (int) pinfo->g[16];
    db = (int) pinfo->b[63] - (int) pinfo->b[16];

    setcolor(pinfo, i, (int) pinfo->r[16] + (dr * j)/47,
                       (int) pinfo->g[16] + (dg * j)/47,
                       (int) pinfo->b[16] + (db * j)/47);
  }
}



/*******************************************/
static void drawstr(str, cx, cy, pic, pw, ph, col)
     char *str;
     byte *pic;
     int   cx, cy, pw, ph, col;
{
  /* draw string (in 5x9 font) centered around cx,cy, in color 'col' */

  int  i;

  i = strlen(str);
  if (!i) return;

  cx -= ((i-1) * 3);

  for ( ; *str; str++, cx+=6) {
    i = (byte) *str;
    if (i >= 32 && i < 128) 
      xbm2pic(font5x9[i - 32], 5, 9, pic, pw, ph, cx, cy, col);
  }
}
