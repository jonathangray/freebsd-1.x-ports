/*
 * xv24to8.c  -  contains the 24-to-8-bit Conv24to8() procedure
 *               and the 8-to-24-bit Conv8to24() procedure
 *
 * The Conv24to8 procedure takes a pointer to a 24-bit image (loaded
 * previously).  The image will be a w * h * 3 byte array of
 * bytes.  The image will be arranged with 3 bytes per pixel (in order
 * R, G, and B), pixel 0 at the top left corner.  (As normal.)
 * The procedure also takes a maximum number of colors to use (numcols)
 * and pointers to three 256-long arrays of bytes (to hold the returned
 * colormap)
 *
 * Note that Conv24to8() does NOT free the pic24 image under any circumstances
 *
 * The Conv24to8 procedure will set up the following:  it will allocate, make
 * & return 'pic8', a 'w' by 'h' (passed in values) 8-bit picture.
 * it will load up the rmap, gmap and bmap colormap arrays.  it will NOT 
 * calculate numcols, since the cmap sort procedure has to be called anyway
 *
 * Conv24to8 returns 'pic8' if successful, 'NULL' on failure (presumably on a 
 * malloc())
 *
 * The 'slow' code is based on Heckbert's Median Cut algorithm.
 *
 * contains:
 *   Cont24to8()
 *   Init24to8()
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

/*
 * Portions Copyright (C) 1989, 1991 by Jef Poskanzer.  See copyright notice
 * at beginning of relevant code.
 */

#include "xv.h"

#define	MAX_CMAP_SIZE	256
#define	COLOR_DEPTH	8
#define	MAX_COLOR	256
#define	B_DEPTH		5		/* # bits/pixel to use */
#define	B_LEN		(1<<B_DEPTH)
#define	C_DEPTH		3
#define	C_LEN		(1<<C_DEPTH)	/* # cells/color to use */

#define R2FACT 20 /* .300 * .300 * 256 = 23 */
#define G2FACT 39 /* .586 * .586 * 256 = 88 */
#define B2FACT 8  /* .114 * .114 * 256 =  3 */


typedef	struct colorbox {
  struct colorbox *next, *prev;
  int              rmin,rmax, gmin,gmax, bmin,bmax;
  int              total;
} CBOX;

typedef struct {
  int num_ents;
  int entries[MAX_CMAP_SIZE][2];
} CCELL;

static byte *pic8;
static byte *pic24;
static byte *rmap, *gmap, *bmap;      /* ptrs to computed colormap */
static int   num_colors, WIDE, HIGH;
static int   histogram[B_LEN][B_LEN][B_LEN];

CBOX   *freeboxes, *usedboxes;
CCELL **ColorCells;

#ifdef __STDC__
  static void   get_histogram(CBOX *);
  static CBOX  *largest_box(void);
  static void   splitbox(CBOX *);
  static void   shrinkbox(CBOX *);
  static void   assign_color(CBOX *, byte *, byte *, byte *);
  static CCELL *create_colorcell(int, int, int);
  static void   map_colortable(void);
  static int    quant_fsdither(void);
  static int    Quick24to8(byte *, int, int);
  static int    QuickCheck(byte *, int, int, int);
  static int    ppmquant(byte *, int, int, int);
#else
  static void   get_histogram();
  static CBOX  *largest_box();
  static void   splitbox();
  static void   shrinkbox();
  static void   assign_color();
  static CCELL *create_colorcell();
  static void   map_colortable();
  static int    quant_fsdither();
  static int    Quick24to8();
  static int    QuickCheck();
  static int    ppmquant();
#endif

static int    tbl1[512],     /* tables used in F-S Dithering */
              tbl3[512],     /* contain i/16, 3i/16, 5i/16, 7i/16, */
              tbl5[512],     /* (i=-256..255) respectively */
              tbl7[512];


/****************************/
void Init24to8()
/****************************/
{
  /* initialize Floyd-Steinberg division tables */
  /* make sure rounding is done correctly for negative values! */

  int i;

  for (i = -256; i < 0; i++) {
    tbl1[i+256] = -((8 -i  )/16);
    tbl3[i+256] = -((8 -3*i)/16);
    tbl5[i+256] = -((8 -5*i)/16);
    tbl7[i+256] = -((8 -7*i)/16);
  }

  for (i = 0; i < 256; i++) {
    tbl1[i+256] = (i  +8)/16;
    tbl3[i+256] = (3*i+8)/16;
    tbl5[i+256] = (5*i+8)/16;
    tbl7[i+256] = (7*i+8)/16;
  }

#ifdef FOO 
  for (i=0; i<512; i++) {
    printf("%3d:  tbl1=%3d, tbl3=%3d, tbl5=%3d, tbl7=%3d\n",
	   i-256, tbl1[i], tbl3[i], tbl5[i], tbl7[i]);
  }
#endif
}



/****************************/
byte *Conv24to8(p,w,h,nc,rm,gm,bm)
/****************************/
byte *p;
byte *rm, *gm, *bm;
int   w,h,nc;
{
  int   i, j;
  CBOX *box_list, *ptr;

  /* returns pointer to new 8-bit-per-pixel image (w*h) if successful, or
     NULL if unsuccessful */

  if (nc<=0) nc = 255;  /* 'nc == 0' breaks code */

  /* copy arguments to local-global variables */
  pic24 = p;  WIDE = w;  HIGH = h;  num_colors = nc;
  rmap = rm;  gmap = gm;  bmap = bm;

  if (!pic24) return NULL;

  /* allocate pic immediately, so that if we can't allocate it, we don't
     waste time running this algorithm */

  pic8 = (byte *) malloc(WIDE * HIGH);
  if (!pic8) {
    fprintf(stderr,"%s: Conv24to8() - failed to allocate 'pic8'\n",cmd);
    return pic8;
  }

  if (!noqcheck && QuickCheck(pic24,w,h,nc)) { 
    /* see if it's a <256 color RGB pic */
    SetISTR(ISTR_INFO,"No color compression was necessary.\n");
    return pic8;   
  }

  else if (conv24 == CONV24_FAST) {
    SetISTR(ISTR_INFO,"Doing 'quick' 24-bit to 8-bit conversion.");
    i = Quick24to8(pic24,w,h);
    if (i) { free(pic8);  pic8 = NULL; }
    return pic8;
  }

  else if (conv24 == CONV24_BEST) {
    SetISTR(ISTR_INFO,"Doing 'best' 24-bit to 8-bit conversion.");
    i = ppmquant(pic24,w,h,nc);
    if (i) { free(pic8);  pic8 = NULL; }
    return pic8;
  }

  else 
    SetISTR(ISTR_INFO,"Doing 'slow' 24-bit to 8-bit conversion.");



  /**** STEP 1:  create empty boxes ****/

  usedboxes = NULL;
  box_list = freeboxes = (CBOX *) malloc(num_colors * sizeof(CBOX));

  if (box_list == NULL) {
    fprintf(stderr,"%s: Conv24to8() - failed to allocate 'freeboxes'\n",cmd);
    free(pic8);  pic8 = NULL;
    return pic8;
  }

  for (i=0; i<num_colors; i++) {
    freeboxes[i].next = &freeboxes[i+1];
    freeboxes[i].prev = &freeboxes[i-1];
  }
  freeboxes[0].prev = NULL;
  freeboxes[num_colors-1].next = NULL;




  /**** STEP 2: get histogram, initialize first box ****/

  ptr = freeboxes;
  freeboxes = ptr->next;
  if (freeboxes) freeboxes->prev = NULL;

  ptr->next = usedboxes;
  usedboxes = ptr;
  if (ptr->next) ptr->next->prev = ptr;
	
  get_histogram(ptr);




  /**** STEP 3: continually subdivide boxes until no more free boxes remain */

  while (freeboxes) {
    ptr = largest_box();
    if (ptr) splitbox(ptr);
    else break;
  }



  
  /**** STEP 4: assign colors to all boxes ****/

  for (i=0, ptr=usedboxes; i<num_colors && ptr; i++, ptr=ptr->next) {
    assign_color(ptr, &rmap[i], &gmap[i], &bmap[i]);
  }

  /* We're done with the boxes now */
  num_colors = i;
  free(box_list);
  box_list = freeboxes = usedboxes = NULL;
 



  /**** STEP 5: scan histogram and map all values to closest color */

  /* 5a: create cell list as described in Heckbert[2] */

  ColorCells = (CCELL **) calloc(C_LEN*C_LEN*C_LEN, sizeof(CCELL *));


  /* 5b: create mapping from truncated pixel space to color table entries */

  map_colortable();




  /**** STEP 6: scan image, match input values to table entries */

  i = quant_fsdither();

  /* free everything that can be freed */
  for (j=0 ; j < C_LEN*C_LEN*C_LEN ; j++) {
    if (ColorCells[j] != NULL) free (ColorCells[j]);
  }
  free(ColorCells);

  if (i) { free(pic8);  pic8 = NULL; }  /* free 'pic' on failure */
  return pic8;
}


/****************************/
static void get_histogram(box)
     CBOX *box;
/****************************/
{
  int   i,j,r,g,b,*ptr;
  byte *p;

  box->rmin = box->gmin = box->bmin = 999;
  box->rmax = box->gmax = box->bmax = -1;
  box->total = WIDE * HIGH;

  /* zero out histogram */
  ptr = &histogram[0][0][0];
  for (i=B_LEN*B_LEN*B_LEN; i>0; i-- )  *ptr++ = 0;

  /* calculate histogram */
  p = pic24;
  for (i=0; i<HIGH; i++)
    for (j=0; j<WIDE; j++) {
      r = (*p++) >> (COLOR_DEPTH-B_DEPTH);
      g = (*p++) >> (COLOR_DEPTH-B_DEPTH);
      b = (*p++) >> (COLOR_DEPTH-B_DEPTH);

      if (r < box->rmin) box->rmin = r;
      if (r > box->rmax) box->rmax = r;

      if (g < box->gmin) box->gmin = g;
      if (g > box->gmax) box->gmax = g;

      if (b < box->bmin) box->bmin = b;
      if (b > box->bmax) box->bmax = b;
      histogram[r][g][b]++;
    }
}



/******************************/
static CBOX *largest_box()
/******************************/
{
  CBOX *tmp, *ptr;
  int   size = -1;

  tmp = usedboxes;
  ptr = NULL;

  while (tmp) {
    if ( (tmp->rmax > tmp->rmin  ||
	  tmp->gmax > tmp->gmin  ||
	  tmp->bmax > tmp->bmin)  &&  tmp->total > size ) {
      ptr = tmp;
      size = tmp->total;
    }
    tmp = tmp->next;
  }
  return(ptr);
}



/******************************/
static void splitbox(ptr)
     CBOX *ptr;
/******************************/
{
  int   hist2[B_LEN], first, last, i, rdel, gdel, bdel;
  CBOX *new;
  int  *iptr, *histp, ir, ig, ib;
  int  rmin,rmax,gmin,gmax,bmin,bmax;
  enum {RED,GREEN,BLUE} which;

  /*
   * see which axis is the largest, do a histogram along that
   * axis.  Split at median point.  Contract both new boxes to
   * fit points and return
   */

  first = last = 0;   /* shut RT hcc compiler up */

  rmin = ptr->rmin;  rmax = ptr->rmax;
  gmin = ptr->gmin;  gmax = ptr->gmax;
  bmin = ptr->bmin;  bmax = ptr->bmax;

  rdel = rmax - rmin;
  gdel = gmax - gmin;
  bdel = bmax - bmin;

  if      (rdel>=gdel && rdel>=bdel) which = RED;
  else if (gdel>=bdel)               which = GREEN;
  else                               which = BLUE;

  /* get histogram along longest axis */
  switch (which) {

  case RED:
    histp = &hist2[rmin];
    for (ir=rmin; ir<=rmax; ir++) {
      *histp = 0;
      for (ig=gmin; ig<=gmax; ig++) {
	iptr = &histogram[ir][ig][bmin];
	for (ib=bmin; ib<=bmax; ib++) {
	  *histp += *iptr;
	  ++iptr;
	}
      }
      ++histp;
    }
    first = rmin;  last = rmax;
    break;

  case GREEN:
    histp = &hist2[gmin];
    for (ig=gmin; ig<=gmax; ig++) {
      *histp = 0;
      for (ir=rmin; ir<=rmax; ir++) {
	iptr = &histogram[ir][ig][bmin];
	for (ib=bmin; ib<=bmax; ib++) {
	  *histp += *iptr;
	  ++iptr;
	}
      }
      ++histp;
    }
    first = gmin;  last = gmax;
    break;

  case BLUE:
    histp = &hist2[bmin];
    for (ib=bmin; ib<=bmax; ib++) {
      *histp = 0;
      for (ir=rmin; ir<=rmax; ir++) {
	iptr = &histogram[ir][gmin][ib];
	for (ig=gmin; ig<=gmax; ig++) {
	  *histp += *iptr;
	  iptr += B_LEN;
	}
      }
      ++histp;
    }
    first = bmin;  last = bmax;
    break;
  }


  /* find median point */
  {
    int sum, sum2;

    histp = &hist2[first];

    sum2 = ptr->total/2;
    histp = &hist2[first];
    sum = 0;
        
    for (i=first; i<=last && (sum += *histp++)<sum2; i++);
    if (i==first) i++;
  }


  /* Create new box, re-allocate points */
  
  new = freeboxes;
  freeboxes = new->next;
  if (freeboxes) freeboxes->prev = NULL;

  if (usedboxes) usedboxes->prev = new;
  new->next = usedboxes;
  usedboxes = new;

  {
    int sum1,sum2,j;
    
    histp = &hist2[first];
    sum1 = 0;
    for (j = first; j < i; ++j) sum1 += *histp++;
    sum2 = 0;
    for (j = i; j <= last; ++j) sum2 += *histp++;
    new->total = sum1;
    ptr->total = sum2;
  }


  new->rmin = rmin;  new->rmax = rmax;
  new->gmin = gmin;  new->gmax = gmax;
  new->bmin = bmin;  new->bmax = bmax;

  switch (which) {
  case RED:    new->rmax = i-1;  ptr->rmin = i;  break;
  case GREEN:  new->gmax = i-1;  ptr->gmin = i;  break;
  case BLUE:   new->bmax = i-1;  ptr->bmin = i;  break;
  }

  shrinkbox(new);
  shrinkbox(ptr);
}


/****************************/
static void shrinkbox(box)
     CBOX *box;
/****************************/
{
  int *histp,ir,ig,ib;
  int  rmin,rmax,gmin,gmax,bmin,bmax;

  rmin = box->rmin;  rmax = box->rmax;
  gmin = box->gmin;  gmax = box->gmax;
  bmin = box->bmin;  bmax = box->bmax;

  if (rmax>rmin) {
    for (ir=rmin; ir<=rmax; ir++)
      for (ig=gmin; ig<=gmax; ig++) {
	histp = &histogram[ir][ig][bmin];
	for (ib=bmin; ib<=bmax; ib++)
	  if (*histp++ != 0) {
	    box->rmin = rmin = ir;
	    goto have_rmin;
	  }
      }

  have_rmin:
    if (rmax>rmin)
      for (ir=rmax; ir>=rmin; --ir)
	for (ig=gmin; ig<=gmax; ig++) {
	  histp = &histogram[ir][ig][bmin];
	  for (ib=bmin; ib<=bmax; ib++)
	    if (*histp++ != 0) {
	      box->rmax = rmax = ir;
	      goto have_rmax;
	    }
	}
  }


 have_rmax:

  if (gmax>gmin) {
    for (ig=gmin; ig<=gmax; ig++)
      for (ir=rmin; ir<=rmax; ir++) {
	histp = &histogram[ir][ig][bmin];
	for (ib=bmin; ib<=bmax; ib++)
	  if (*histp++ != 0) {
	    box->gmin = gmin = ig;
	    goto have_gmin;
	  }
      }
  have_gmin:
    if (gmax>gmin)
      for (ig=gmax; ig>=gmin; --ig)
	for (ir=rmin; ir<=rmax; ir++) {
	  histp = &histogram[ir][ig][bmin];
	  for (ib=bmin; ib<=bmax; ib++)
	    if (*histp++ != 0) {
	      box->gmax = gmax = ig;
	      goto have_gmax;
	    }
	}
  }


 have_gmax:

  if (bmax>bmin) {
    for (ib=bmin; ib<=bmax; ib++)
      for (ir=rmin; ir<=rmax; ir++) {
	histp = &histogram[ir][gmin][ib];
	for (ig=gmin; ig<=gmax; ig++) {
	  if (*histp != 0) {
	    box->bmin = bmin = ib;
	    goto have_bmin;
	  }
	  histp += B_LEN;
	}
      }
  have_bmin:
    if (bmax>bmin)
      for (ib=bmax; ib>=bmin; --ib)
	for (ir=rmin; ir<=rmax; ir++) {
	  histp = &histogram[ir][gmin][ib];
	  for (ig=gmin; ig<=gmax; ig++) {
	    if (*histp != 0) {
	      bmax = ib;
	      goto have_bmax;
	    }
	    histp += B_LEN;
	  }
	}
  }

 have_bmax: return;
}



/*******************************/
static void assign_color(ptr,rp,gp,bp)
     CBOX *ptr;
     byte *rp,*gp,*bp;
/*******************************/
{

  int r,g,b;

  r = ((ptr->rmin + ptr->rmax) << (COLOR_DEPTH - B_DEPTH)) / 2;
  g = ((ptr->gmin + ptr->gmax) << (COLOR_DEPTH - B_DEPTH)) / 2;
  b = ((ptr->bmin + ptr->bmax) << (COLOR_DEPTH - B_DEPTH)) / 2;

  *rp = (byte) r;  *gp = (byte) g;  *bp = (byte) b;
}



/*******************************/
static CCELL *create_colorcell(r1,g1,b1)
     int r1,g1,b1;
/*******************************/
{
  register int    i;
  register CCELL *ptr;
  register byte  *rp,*gp,*bp;
  int             ir,ig,ib;
  long            dist, mindist, tmp;

  ir = r1 >> (COLOR_DEPTH-C_DEPTH);
  ig = g1 >> (COLOR_DEPTH-C_DEPTH);
  ib = b1 >> (COLOR_DEPTH-C_DEPTH);

  r1 &= ~1 << (COLOR_DEPTH-C_DEPTH);
  g1 &= ~1 << (COLOR_DEPTH-C_DEPTH);
  b1 &= ~1 << (COLOR_DEPTH-C_DEPTH);

  ptr = (CCELL *) malloc(sizeof(CCELL));
  *(ColorCells + ir*C_LEN*C_LEN + ig*C_LEN + ib) = ptr;
  ptr->num_ents = 0;

  /* step 1: find all colors inside this cell, while we're at
     it, find distance of centermost point to furthest
     corner */

  mindist = 2000000000;

  rp=rmap;  gp=gmap;  bp=bmap;
  for (i=0; i<num_colors; i++,rp++,gp++,bp++)
    if( *rp>>(COLOR_DEPTH-C_DEPTH) == ir  &&
        *gp>>(COLOR_DEPTH-C_DEPTH) == ig  &&
        *bp>>(COLOR_DEPTH-C_DEPTH) == ib) {

      ptr->entries[ptr->num_ents][0] = i;
      ptr->entries[ptr->num_ents][1] = 0;
      ++ptr->num_ents;

      tmp = *rp - r1;
      if (tmp < (MAX_COLOR/C_LEN/2)) tmp = MAX_COLOR/C_LEN-1 - tmp;
      dist = (tmp*tmp * R2FACT);

      tmp = *gp - g1;
      if (tmp < (MAX_COLOR/C_LEN/2)) tmp = MAX_COLOR/C_LEN-1 - tmp;
      dist += (tmp*tmp * G2FACT);

      tmp = *bp - b1;
      if (tmp < (MAX_COLOR/C_LEN/2)) tmp = MAX_COLOR/C_LEN-1 - tmp;
      dist += (tmp*tmp * B2FACT);

      if (dist < mindist) mindist = dist;
    }


  /* step 3: find all points within that distance to box */

  rp=rmap;  gp=gmap;  bp=bmap;
  for (i=0; i<num_colors; i++,rp++,gp++,bp++)
    if (*rp >> (COLOR_DEPTH-C_DEPTH) != ir  ||
	*gp >> (COLOR_DEPTH-C_DEPTH) != ig  ||
	*bp >> (COLOR_DEPTH-C_DEPTH) != ib) {

      dist = 0;

      if ((tmp = r1 - *rp)>0 || (tmp = *rp - (r1 + MAX_COLOR/C_LEN-1)) > 0 )
	dist += (tmp*tmp * R2FACT);

      if( (tmp = g1 - *gp)>0 || (tmp = *gp - (g1 + MAX_COLOR/C_LEN-1)) > 0 )
	dist += (tmp*tmp * G2FACT);

      if( (tmp = b1 - *bp)>0 || (tmp = *bp - (b1 + MAX_COLOR/C_LEN-1)) > 0 )
	dist += (tmp*tmp * B2FACT);

      if( dist < mindist ) {
	ptr->entries[ptr->num_ents][0] = i;
	ptr->entries[ptr->num_ents][1] = dist;
	++ptr->num_ents;
      }
    }


  /* sort color cells by distance, use cheap exchange sort */
  {
    int n, next_n;

    n = ptr->num_ents - 1;
    while (n>0) {
      next_n = 0;
      for (i=0; i<n; ++i) {
	if (ptr->entries[i][1] > ptr->entries[i+1][1]) {
	  tmp = ptr->entries[i][0];
	  ptr->entries[i][0] = ptr->entries[i+1][0];
	  ptr->entries[i+1][0] = tmp;
	  tmp = ptr->entries[i][1];
	  ptr->entries[i][1] = ptr->entries[i+1][1];
	  ptr->entries[i+1][1] = tmp;
	  next_n = i;
	}
      }
      n = next_n;
    }
  }
  return (ptr);
}




/***************************/
static void map_colortable()
/***************************/
{
  int    ir,ig,ib, *histp;
  CCELL *cell;

  histp  = &histogram[0][0][0];
  for (ir=0; ir<B_LEN; ir++)
    for (ig=0; ig<B_LEN; ig++)
      for (ib=0; ib<B_LEN; ib++) {
	if (*histp==0) *histp = -1;
	else {
	  int  i, j;
	  long dist, d2, tmp;
	  
	  cell = *(ColorCells +
		   ( ((ir>>(B_DEPTH-C_DEPTH)) << C_DEPTH*2)
		   + ((ig>>(B_DEPTH-C_DEPTH)) << C_DEPTH)
		   +  (ib>>(B_DEPTH-C_DEPTH)) ) );
		
	  if (cell==NULL)
	    cell = create_colorcell(ir<<(COLOR_DEPTH-B_DEPTH),
				    ig<<(COLOR_DEPTH-B_DEPTH),
				    ib<<(COLOR_DEPTH-B_DEPTH));

	  dist = 2000000000;
	  for (i=0; i<cell->num_ents && dist>cell->entries[i][1]; i++) {
	    j = cell->entries[i][0];
	    d2 = rmap[j] - (ir << (COLOR_DEPTH-B_DEPTH));
	    d2 = (d2 * d2 * R2FACT);
	    tmp = gmap[j] - (ig << (COLOR_DEPTH-B_DEPTH));
	    d2 += (tmp*tmp * G2FACT);
	    tmp = bmap[j] - (ib << (COLOR_DEPTH-B_DEPTH));
	    d2 += (tmp*tmp * B2FACT);
	    if( d2 < dist ) { dist = d2;  *histp = j; }
	  }
	}
	histp++;
      }
}



/*****************************/
static int quant_fsdither()
/*****************************/
{
  register int  *thisptr, *nextptr;
  int           *thisline, *nextline, *tmpptr;
  int            r1, g1, b1, r2, g2, b2;
  int            i, j, imax, jmax, oval;
  byte          *inptr, *outptr;
  int            lastline, lastpixel;

  imax = HIGH - 1;
  jmax = WIDE - 1;
  
  thisline = (int *) malloc(WIDE * 3 * sizeof(int));
  nextline = (int *) malloc(WIDE * 3 * sizeof(int));

  if (thisline == NULL || nextline == NULL) {
    fprintf(stderr,"%s: unable to allocate stuff for the 'dither' routine\n",
	    cmd);
    return 1;
  }


  inptr  = (byte *) pic24;
  outptr = (byte *) pic8;

  /* get first line of picture */
  for (j=WIDE * 3, tmpptr=nextline; j; j--)
    *tmpptr++ = (int) *inptr++;

  for (i=0; i<HIGH; i++) {
    /* swap thisline and nextline */
    tmpptr = thisline;  thisline = nextline;  nextline = tmpptr;
    lastline = (i==imax);

    if ((i&0x1f) == 0) WaitCursor();

    /* read in next line */
    if (!lastline)
      for (j=WIDE * 3, tmpptr=nextline; j; j--)
	*tmpptr++ = (int) *inptr++;

    /* dither this line and put it into the output picture */
    thisptr = thisline;  nextptr = nextline;

    for (j=0; j<WIDE; j++) {
      lastpixel = (j==jmax);

      r2 = *thisptr++;  g2 = *thisptr++;  b2 = *thisptr++;

      RANGE(r2, 0, MAX_COLOR-1);
      RANGE(g2, 0, MAX_COLOR-1);
      RANGE(b2, 0, MAX_COLOR-1);

      r1 = r2;  g1 = g2;  b1 = b2;

      r2 >>= (COLOR_DEPTH-B_DEPTH);
      g2 >>= (COLOR_DEPTH-B_DEPTH);
      b2 >>= (COLOR_DEPTH-B_DEPTH);

      if ( (oval=histogram[r2][g2][b2]) == -1) {
	int ci, cj;
	long dist, d2, tmp;
	CCELL *cell;

	cell = *( ColorCells + 
		( ((r2>>(B_DEPTH-C_DEPTH)) << C_DEPTH*2)
	        + ((g2>>(B_DEPTH-C_DEPTH)) << C_DEPTH )
		+  (b2>>(B_DEPTH-C_DEPTH)) ) );
	      
	if (cell==NULL) cell = create_colorcell(r1,g1,b1);

	dist = 2000000000;
	for (ci=0; ci<cell->num_ents && dist>cell->entries[ci][1]; ci++) {
	  cj = cell->entries[ci][0];
	  d2 = (rmap[cj] >> (COLOR_DEPTH-B_DEPTH)) - r2;
	  d2 = (d2*d2 * R2FACT);
	  tmp = (gmap[cj] >> (COLOR_DEPTH-B_DEPTH)) - g2;
	  d2 += (tmp*tmp * G2FACT);
	  tmp = (bmap[cj] >> (COLOR_DEPTH-B_DEPTH)) - b2;
	  d2 += (tmp*tmp * B2FACT);
	  if (d2<dist) { dist = d2;  oval = cj; }
	}
	histogram[r2][g2][b2] = oval;
      }

      *outptr++ = oval;

      r1 -= rmap[oval];  g1 -= gmap[oval];  b1 -= bmap[oval];
	
      /* don't use tables, because r1,g1,b1 could go negative */
      if (!lastpixel) {
	thisptr[0] += (r1<0) ? (r1*7-8)/16 : (r1*7+8)/16;
	thisptr[1] += (g1<0) ? (g1*7-8)/16 : (g1*7+8)/16;
	thisptr[2] += (b1<0) ? (b1*7-8)/16 : (b1*7+8)/16;
      }
	
      if (!lastline) {
	if (j) {
	  nextptr[-3] += (r1<0) ? (r1*3-8)/16 : (r1*3+8)/16;
	  nextptr[-2] += (g1<0) ? (g1*3-8)/16 : (g1*3+8)/16;
	  nextptr[-1] += (b1<0) ? (b1*3-8)/16 : (b1*3+8)/16;
	}

	nextptr[0] += (r1<0) ? (r1*5-8)/16 : (r1*5+8)/16;
	nextptr[1] += (g1<0) ? (g1*5-8)/16 : (g1*5+8)/16;
	nextptr[2] += (b1<0) ? (b1*5-8)/16 : (b1*5+8)/16;
	
	if (!lastpixel) {
	  nextptr[3] += (r1<0) ? (r1-8)/16 : (r1+8)/16;
	  nextptr[4] += (g1<0) ? (g1-8)/16 : (g1+8)/16;
	  nextptr[5] += (b1<0) ? (b1-8)/16 : (b1+8)/16;
	}
	nextptr += 3;
      }
    }
  }

  free(thisline);  free(nextline);
  return 0;
}










/************************************/
static int Quick24to8(p24,w,h)
byte *p24;
int   w,h;
{

  /* floyd-steinberg dithering.
   *
   * ----   x    7/16
   * 3/16  5/16  1/16
   *
   */

  /* called after 'pic8' has been alloced, pWIDE,pHIGH set up, mono/1-bit
     checked already */

  byte *pp;
  int  r1, g1, b1;
  int  *thisline, *nextline, *thisptr, *nextptr, *tmpptr;
  int  i, j, val, pwide3;
  int  imax, jmax;

  pp = pic8;  pwide3 = w * 3;  imax = h-1;  jmax = w-1;

  /* up to 256 colors:     3 bits R, 3 bits G, 2 bits B  (RRRGGGBB) */
#define RMASK 0xe0
#define R_SHIFT        0
#define GMASK 0xe0
#define G_SHIFT        3
#define BMASK 0xc0
#define B_SHIFT        6

  /* load up colormap */
  /* note that 0 and 255 of each color are always in the map; */
  /* intermediate values are evenly spaced. */
  for (i=0; i<256; i++) {
    rmap[i] = (((i<<R_SHIFT) & RMASK) * 255 + RMASK/2) / RMASK;
    gmap[i] = (((i<<G_SHIFT) & GMASK) * 255 + GMASK/2) / GMASK;
    bmap[i] = (((i<<B_SHIFT) & BMASK) * 255 + BMASK/2) / BMASK;
  }


  thisline = (int *) malloc(pwide3 * sizeof(int));
  nextline = (int *) malloc(pwide3 * sizeof(int));
  if (!thisline || !nextline) {
    fprintf(stderr,"%s: unable to allocate memory in Quick24to8()\n", cmd);
    return(1);
    }

  /* get first line of picture */
  for (j=pwide3, tmpptr=nextline; j; j--) *tmpptr++ = (int) *p24++;

  for (i=0; i<h; i++) {
    tmpptr = thisline;  thisline = nextline;  nextline = tmpptr;   /* swap */

    if ((i&0x1f) == 0) WaitCursor();

    if (i!=imax)   /* get next line */
      for (j=pwide3, tmpptr=nextline; j; j--)
	*tmpptr++ = (int) *p24++;

    for (j=0, thisptr=thisline, nextptr=nextline; j<w; j++,pp++) {
      r1 = *thisptr++;  g1 = *thisptr++;  b1 = *thisptr++;
      RANGE(r1,0,255);  RANGE(g1,0,255);  RANGE(b1,0,255);  

      /* choose actual pixel value */
      val = (((r1&RMASK)>>R_SHIFT) | ((g1&GMASK)>>G_SHIFT) | 
	     ((b1&BMASK)>>B_SHIFT));
      *pp = val;

      /* compute color errors */
      r1 -= rmap[val];
      g1 -= gmap[val];
      b1 -= bmap[val];

      /* Add fractions of errors to adjacent pixels */
      r1 += 256;              /* make positive for table indexing */
      g1 += 256;
      b1 += 256;

      if (j!=jmax) {  /* adjust RIGHT pixel */
	thisptr[0] += tbl7[r1];
	thisptr[1] += tbl7[g1];
	thisptr[2] += tbl7[b1];
      }
      
      if (i!=imax) {	/* do BOTTOM pixel */
	nextptr[0] += tbl5[r1];
	nextptr[1] += tbl5[g1];
	nextptr[2] += tbl5[b1];

	if (j>0) {  /* do BOTTOM LEFT pixel */
	  nextptr[-3] += tbl3[r1];
	  nextptr[-2] += tbl3[g1];
	  nextptr[-1] += tbl3[b1];
	}

	if (j!=jmax) {  /* do BOTTOM RIGHT pixel */
	  nextptr[3] += tbl1[r1];
	  nextptr[4] += tbl1[g1];
	  nextptr[5] += tbl1[b1];
	}
	nextptr += 3;
      }
    }
  }

  return 0;
}
      


/****************************/
static int QuickCheck(pic24,w,h,maxcol)
byte *pic24;
int   w,h,maxcol;
{
  /* scans picture until it finds more than 'maxcol' different colors.  If it
     finds more than 'maxcol' colors, it returns '0'.  If it DOESN'T, it does
     the 24-to-8 conversion by simply sticking the colors it found into
     a colormap, and changing instances of a color in pic24 into colormap
     indicies (in pic8) */

  unsigned long colors[256],col;
  int           i, nc, low, high, mid;
  byte         *p, *pix;

  if (maxcol>256) maxcol = 256;

  /* put the first color in the table by hand */
  nc = 0;  mid = 0;  

  for (i=w*h,p=pic24; i; i--) {
    col  = (((u_long) *p++) << 16);  
    col += (((u_long) *p++) << 8);
    col +=  *p++;

    /* binary search the 'colors' array to see if it's in there */
    low = 0;  high = nc-1;
    while (low <= high) {
      mid = (low+high)/2;
      if      (col < colors[mid]) high = mid - 1;
      else if (col > colors[mid]) low  = mid + 1;
      else break;
    }

    if (high < low) { /* didn't find color in list, add it. */
      if (nc>=maxcol) return 0;
      xvbcopy((char *) &colors[low], (char *) &colors[low+1],
	      (nc - low) * sizeof(u_long));
      colors[low] = col;
      nc++;
    }
  }


  /* run through the data a second time, this time mapping pixel values in
     pic24 into colormap offsets into 'colors' */

  for (i=w*h,p=pic24, pix=pic8; i; i--,pix++) {
    col  = (((u_long) *p++) << 16);  
    col += (((u_long) *p++) << 8);
    col +=  *p++;

    /* binary search the 'colors' array.  It *IS* in there */
    low = 0;  high = nc-1;
    while (low <= high) {
      mid = (low+high)/2;
      if      (col < colors[mid]) high = mid - 1;
      else if (col > colors[mid]) low  = mid + 1;
      else break;
    }

    if (high < low) {
      fprintf(stderr,"QuickCheck:  impossible situation!\n");
      exit(1);
    }
    *pix = mid;
  }

  /* and load up the 'desired colormap' */
  for (i=0; i<nc; i++) {
    rmap[i] =  colors[i]>>16;  
    gmap[i] = (colors[i]>>8) & 0xff;
    bmap[i] =  colors[i]     & 0xff;
  }

  return 1;
}



/***************************************************************/
byte *Conv8to24(pic8, w, h, rmap,gmap,bmap)
     byte *pic8, *rmap, *gmap, *bmap;
     int   w, h;
{
  /* converts an w*h 8-bit image (with colormap rmap,gmap,bmap) into a
   * 24-bit image.  Note, 'pic8' could be NULL
   *
   * returns pointer to new 24-bits-per-pixel image (w*h) if successful,
   * or NULL if unsuccessful
   */

  int   i;
  byte *pic24, *sp, *dp;

  pic24 = (byte *) malloc(w * h * 3);
  if (!pic24) return pic24;

  for (i=w*h, sp=pic8, dp=pic24; i; i--, sp++) {
    if ((i&0x1ffff)==0) WaitCursor();
    *dp++ = rmap[*sp];
    *dp++ = gmap[*sp];
    *dp++ = bmap[*sp];
  }

  return pic24;
}
  







/***************************************************************/
/* The following code based on code from the 'pbmplus' package */
/* written by Jef Poskanzer                                    */
/***************************************************************/


/* ppmquant.c - quantize the colors in a pixmap down to a specified number
**
** Copyright (C) 1989, 1991 by Jef Poskanzer.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
*/


#undef max
#define max(a,b) ((a) > (b) ? (a) : (b))
#undef min
#define min(a,b) ((a) < (b) ? (a) : (b))
#undef abs
#define abs(a) ((a) >= 0 ? (a) : -(a))
#undef odd
#define odd(n) ((n) & 1)

typedef unsigned char pixval;

#define PPM_MAXMAXVAL 255
typedef struct { pixval r, g, b; } pixel;

#define PPM_GETR(p) ((p).r)
#define PPM_GETG(p) ((p).g)
#define PPM_GETB(p) ((p).b)

#define PPM_ASSIGN(p,red,grn,blu) \
  { (p).r = (red); (p).g = (grn); (p).b = (blu); }

#define PPM_EQUAL(p,q) ( (p).r == (q).r && (p).g == (q).g && (p).b == (q).b )


/* Color scaling macro -- to make writing ppmtowhatever easier. */

#define PPM_DEPTH(newp,p,oldmaxval,newmaxval) \
    PPM_ASSIGN( (newp), \
	        (int) PPM_GETR(p) * (newmaxval) / (oldmaxval), \
	        (int) PPM_GETG(p) * (newmaxval) / (oldmaxval), \
	        (int) PPM_GETB(p) * (newmaxval) / (oldmaxval) )


/* Luminance macro. */

/* 
 * #define PPM_LUMIN(p) \
 *   ( 0.299 * PPM_GETR(p) + 0.587 * PPM_GETG(p) + 0.114 * PPM_GETB(p) )
 */

/* Luminance macro, using only integer ops.  Returns an int (*256)  JHB */
#define PPM_LUMIN(p) \
  ( 77 * PPM_GETR(p) + 150 * PPM_GETG(p) + 29 * PPM_GETB(p) )

/* Color histogram stuff. */

typedef struct colorhist_item* colorhist_vector;
struct colorhist_item { pixel color;
			int value;
		      };

typedef struct colorhist_list_item* colorhist_list;
struct colorhist_list_item { struct colorhist_item ch;
			     colorhist_list next;
			   };

typedef colorhist_list* colorhash_table;

typedef struct box* box_vector;
struct box {
  int index;
  int colors;
  int sum;
};


#define MAXCOLORS 32767
#define CLUSTER_MAXVAL 63

#define LARGE_LUM
#define REP_AVERAGE_PIXELS

#define FS_SCALE 1024

#define HASH_SIZE 6553

#define ppm_hashpixel(p) ((((int) PPM_GETR(p) * 33023 +    \
			    (int) PPM_GETG(p) * 30013 +    \
			    (int) PPM_GETB(p) * 27011) & 0x7fffffff)   \
			  % HASH_SIZE)



/*** function defs ***/

#ifdef __STDC__
  static colorhist_vector mediancut(colorhist_vector, int, int, int, int);
  static int              redcompare  (colorhist_vector, colorhist_vector);
  static int              greencompare(colorhist_vector, colorhist_vector);
  static int              bluecompare (colorhist_vector, colorhist_vector);
  static int              sumcompare  (box_vector, box_vector);
  static colorhist_vector ppm_computecolorhist(pixel **, int,int,int,int *);
  static colorhash_table  ppm_computecolorhash(pixel **, int,int,int,int *);
  static colorhist_vector ppm_colorhashtocolorhist(colorhash_table, int);
  static colorhash_table  ppm_alloccolorhash(void);
  static void             ppm_freecolorhist(colorhist_vector);
  static void             ppm_freecolorhash(colorhash_table);
#else
  static colorhist_vector mediancut();
  static int              redcompare(), greencompare(), bluecompare();
  static int              sumcompare();
  static colorhist_vector ppm_computecolorhist(), ppm_colorhashtocolorhist();
  static colorhash_table  ppm_computecolorhash(), ppm_alloccolorhash();
  static void             ppm_freecolorhist(), ppm_freecolorhash();
#endif


/****************************************************************************/
static int ppmquant(pic24, cols, rows, newcolors)
     byte *pic24;
     int  cols, rows, newcolors;
{
  pixel**          pixels;
  register pixel*  pP;
  int              row;
  register int     col, limitcol;
  pixval           maxval, newmaxval;
  int              colors;
  register int     index;
  colorhist_vector chv, colormap;
  colorhash_table  cht;
  int              i;
  unsigned char    *picptr;
  static char      *fn = "ppmquant()";

  index = 0;
  maxval = 255;

  /*
   *  reformat 24-bit pic24 image (3 bytes per pixel) into 2-dimensional
   *  array of pixel structures
   */

  if (DEBUG) fprintf(stderr,"%s: remapping to ppm-style internal fmt\n", fn);
  WaitCursor();
  
  pixels = (pixel **) malloc(rows * sizeof(pixel *));
  if (!pixels) FatalError("couldn't allocate 'pixels' array");
  for (row=0; row<rows; row++) {
    pixels[row] = (pixel *) malloc(cols * sizeof(pixel));
    if (!pixels[row]) FatalError("couldn't allocate a row of pixels array");

    for (col=0, pP=pixels[row]; col<cols; col++, pP++) {
      pP->r = *pic24++;
      pP->g = *pic24++;
      pP->b = *pic24++;
    }
  }
  if (DEBUG) fprintf(stderr,"%s: done format remapping\n", fn);


    

  /*
   *  attempt to make a histogram of the colors, unclustered.
   *  If at first we don't succeed, lower maxval to increase color
   *  coherence and try again.  This will eventually terminate, with
   *  maxval at worst 15, since 32^3 is approximately MAXCOLORS.
   */

  WaitCursor();
  for ( ; ; ) {
    if (DEBUG) fprintf(stderr, "%s:  making histogram\n", fn);

    chv = ppm_computecolorhist(pixels, cols, rows, MAXCOLORS, &colors);
    if (chv != (colorhist_vector) 0) break;
    
    if (DEBUG) fprintf(stderr, "%s: too many colors!\n", fn);
    newmaxval = maxval / 2;
    if (DEBUG) fprintf(stderr, "%s: rescaling colors (maxval=%d) %s\n",
		       fn, newmaxval, "to improve clustering");

    for (row=0; row<rows; ++row)
      for (col=0, pP=pixels[row]; col<cols; ++col, ++pP)
	PPM_DEPTH( *pP, *pP, maxval, newmaxval );
    maxval = newmaxval;
  }

  if (DEBUG) fprintf(stderr,"%s: %d colors found\n", fn, colors);



  /*
   * Step 3: apply median-cut to histogram, making the new colormap.
   */

  WaitCursor();
  if (DEBUG) fprintf(stderr, "%s: choosing %d colors\n", fn, newcolors);
  colormap = mediancut(chv, colors, rows * cols, maxval, newcolors);
  ppm_freecolorhist(chv);



  /*
   *  Step 4: map the colors in the image to their closest match in the
   *  new colormap, and write 'em out.
   */

  if (DEBUG) fprintf(stderr,"%s: mapping image to new colors\n", fn);
  cht = ppm_alloccolorhash();

  picptr = pic8;
  for (row = 0;  row < rows;  ++row) {
    col = 0;  limitcol = cols;  pP = pixels[row];

    if ((row & 0x1f) == 0) WaitCursor();
    do {
      int hash;
      colorhist_list chl;

      /* Check hash table to see if we have already matched this color. */

      hash = ppm_hashpixel(*pP);
      for (chl = cht[hash];  chl;  chl = chl->next)
	if (PPM_EQUAL(chl->ch.color, *pP)) {index = chl->ch.value; break;}

      if (!chl /*index = -1*/) {/* No; search colormap for closest match. */
	register int i, r1, g1, b1, r2, g2, b2;
	register long dist, newdist;

	r1 = PPM_GETR( *pP );
	g1 = PPM_GETG( *pP );
	b1 = PPM_GETB( *pP );
	dist = 2000000000;

	for (i=0; i<newcolors; i++) {
	  r2 = PPM_GETR( colormap[i].color );
	  g2 = PPM_GETG( colormap[i].color );
	  b2 = PPM_GETB( colormap[i].color );

	  newdist = ( r1 - r2 ) * ( r1 - r2 ) +
	            ( g1 - g2 ) * ( g1 - g2 ) +
	            ( b1 - b2 ) * ( b1 - b2 );

	  if (newdist<dist) { index = i;  dist = newdist; }
	}

	hash = ppm_hashpixel(*pP);
	chl = (colorhist_list) malloc(sizeof(struct colorhist_list_item));
	if (!chl) FatalError("ran out of memory adding to hash table");

	chl->ch.color = *pP;
	chl->ch.value = index;
	chl->next = cht[hash];
	cht[hash] = chl;
      }

      *picptr++ = index;

      ++col;
      ++pP;
    }
    while (col != limitcol);
  }

  /* rescale the colormap and load the XV colormap */
  for (i=0; i<newcolors; i++) {
    PPM_DEPTH(colormap[i].color, colormap[i].color, maxval, 255);
    rmap[i] = PPM_GETR( colormap[i].color );
    gmap[i] = PPM_GETG( colormap[i].color );
    bmap[i] = PPM_GETB( colormap[i].color );
  }

  /* free the pixels array */
  for (i=0; i<rows; i++) free(pixels[i]);
  free(pixels);

  /* free cht and colormap */
  ppm_freecolorhist(colormap);
  ppm_freecolorhash(cht);

  return 0;
}



/*
** Here is the fun part, the median-cut colormap generator.  This is based
** on Paul Heckbert's paper "Color Image Quantization for Frame Buffer
** Display", SIGGRAPH '82 Proceedings, page 297.
*/



/****************************************************************************/
static colorhist_vector mediancut( chv, colors, sum, maxval, newcolors )
     colorhist_vector chv;
     int colors, sum, newcolors;
     int maxval;
{
  colorhist_vector colormap;
  box_vector bv;
  register int bi, i;
  int boxes;

  bv = (box_vector) malloc(sizeof(struct box) * newcolors);
  colormap = (colorhist_vector) 
             malloc(sizeof(struct colorhist_item) * newcolors );

  if (!bv || !colormap) FatalError("unable to malloc in mediancut()");

  for (i=0; i<newcolors; i++)
    PPM_ASSIGN(colormap[i].color, 0, 0, 0);

  /*
   *  Set up the initial box.
   */
  bv[0].index = 0;
  bv[0].colors = colors;
  bv[0].sum = sum;
  boxes = 1;


  /*
   ** Main loop: split boxes until we have enough.
   */

  while ( boxes < newcolors ) {
    register int indx, clrs;
    int sm;
    register int minr, maxr, ming, maxg, minb, maxb, v;
    int halfsum, lowersum;

    /*
     ** Find the first splittable box.
     */
    for (bi=0; bv[bi].colors<2 && bi<boxes; bi++) ;
    if (bi == boxes) break;	/* ran out of colors! */

    indx = bv[bi].index;
    clrs = bv[bi].colors;
    sm = bv[bi].sum;

    /*
     ** Go through the box finding the minimum and maximum of each
     ** component - the boundaries of the box.
     */
    minr = maxr = PPM_GETR( chv[indx].color );
    ming = maxg = PPM_GETG( chv[indx].color );
    minb = maxb = PPM_GETB( chv[indx].color );

    for (i=1; i<clrs; i++) {
      v = PPM_GETR( chv[indx + i].color );
      if (v < minr) minr = v;
      if (v > maxr) maxr = v;

      v = PPM_GETG( chv[indx + i].color );
      if (v < ming) ming = v;
      if (v > maxg) maxg = v;

      v = PPM_GETB( chv[indx + i].color );
      if (v < minb) minb = v;
      if (v > maxb) maxb = v;
    }

    /*
     ** Find the largest dimension, and sort by that component.  I have
     ** included two methods for determining the "largest" dimension;
     ** first by simply comparing the range in RGB space, and second
     ** by transforming into luminosities before the comparison.  You
     ** can switch which method is used by switching the commenting on
     ** the LARGE_ defines at the beginning of this source file.
     */
    {
      /* LARGE_LUM version */

      pixel p;
      int rl, gl, bl;

      PPM_ASSIGN(p, maxr - minr, 0, 0);
      rl = PPM_LUMIN(p);

      PPM_ASSIGN(p, 0, maxg - ming, 0);
      gl = PPM_LUMIN(p);

      PPM_ASSIGN(p, 0, 0, maxb - minb);
      bl = PPM_LUMIN(p);

      if (rl >= gl && rl >= bl)
	qsort( (char*) &(chv[indx]), clrs, sizeof(struct colorhist_item),
	      redcompare );
      else if (gl >= bl)
	qsort( (char*) &(chv[indx]), clrs, sizeof(struct colorhist_item),
	      greencompare );
      else qsort( (char*) &(chv[indx]), clrs, sizeof(struct colorhist_item),
		 bluecompare );
    }

    /*
     ** Now find the median based on the counts, so that about half the
     ** pixels (not colors, pixels) are in each subdivision.
     */
    lowersum = chv[indx].value;
    halfsum = sm / 2;
    for (i=1; i<clrs-1; i++) {
      if (lowersum >= halfsum) break;
      lowersum += chv[indx + i].value;
    }

    /*
     ** Split the box, and sort to bring the biggest boxes to the top.
     */
    bv[bi].colors = i;
    bv[bi].sum = lowersum;
    bv[boxes].index = indx + i;
    bv[boxes].colors = clrs - i;
    bv[boxes].sum = sm - lowersum;
    ++boxes;
    qsort( (char*) bv, boxes, sizeof(struct box), sumcompare );
  }  /* while (boxes ... */

  /*
   ** Ok, we've got enough boxes.  Now choose a representative color for
   ** each box.  There are a number of possible ways to make this choice.
   ** One would be to choose the center of the box; this ignores any structure
   ** within the boxes.  Another method would be to average all the colors in
   ** the box - this is the method specified in Heckbert's paper.  A third
   ** method is to average all the pixels in the box.  You can switch which
   ** method is used by switching the commenting on the REP_ defines at
   ** the beginning of this source file.
   */
  
  for (bi=0; bi<boxes; bi++) {
    /* REP_AVERAGE_PIXELS version */
    register int indx = bv[bi].index;
    register int clrs = bv[bi].colors;
    register long r = 0, g = 0, b = 0, sum = 0;

    for (i=0; i<clrs; i++) {
      r += PPM_GETR( chv[indx + i].color ) * chv[indx + i].value;
      g += PPM_GETG( chv[indx + i].color ) * chv[indx + i].value;
      b += PPM_GETB( chv[indx + i].color ) * chv[indx + i].value;
      sum += chv[indx + i].value;
    }

    r = r / sum;  if (r>maxval) r = maxval;	/* avoid math errors */
    g = g / sum;  if (g>maxval) g = maxval;
    b = b / sum;  if (b>maxval) b = maxval;

    PPM_ASSIGN( colormap[bi].color, r, g, b );
  }

  free(bv);
  return colormap;
}


/**********************************/
static int redcompare(ch1, ch2)
     colorhist_vector ch1, ch2;
{
  return (int) PPM_GETR( ch1->color ) - (int) PPM_GETR( ch2->color );
}

/**********************************/
static int greencompare(ch1, ch2)
     colorhist_vector ch1, ch2;
{
  return (int) PPM_GETG( ch1->color ) - (int) PPM_GETG( ch2->color );
}

/**********************************/
static int bluecompare( ch1, ch2 )
     colorhist_vector ch1, ch2;
{
  return (int) PPM_GETB( ch1->color ) - (int) PPM_GETB( ch2->color );
}

/**********************************/
static int sumcompare( b1, b2 )
     box_vector b1, b2;
{
  return b2->sum - b1->sum;
}



/****************************************************************************/
static colorhist_vector 
  ppm_computecolorhist(pixels, cols, rows, maxcolors, colorsP)
     pixel** pixels;
     int cols, rows, maxcolors;
     int* colorsP;
{
  colorhash_table cht;
  colorhist_vector chv;

  cht = ppm_computecolorhash(pixels, cols, rows, maxcolors, colorsP);
  if (!cht) return (colorhist_vector) 0;

  chv = ppm_colorhashtocolorhist(cht, maxcolors);
  ppm_freecolorhash(cht);
  return chv;
}


/****************************************************************************/
static colorhash_table ppm_computecolorhash(pixels, cols, rows, 
					    maxcolors, colorsP )
     pixel** pixels;
     int cols, rows, maxcolors;
     int* colorsP;
{
  colorhash_table cht;
  register pixel* pP;
  colorhist_list chl;
  int col, row, hash;

  cht = ppm_alloccolorhash( );
  *colorsP = 0;

  /* Go through the entire image, building a hash table of colors. */
  for (row=0; row<rows; row++)
    for (col=0, pP=pixels[row];  col<cols;  col++, pP++) {
      hash = ppm_hashpixel(*pP);

      for (chl = cht[hash]; chl != (colorhist_list) 0; chl = chl->next)
	if (PPM_EQUAL(chl->ch.color, *pP)) break;
      
      if (chl != (colorhist_list) 0) ++(chl->ch.value);
      else {
	if ((*colorsP)++ > maxcolors) {
	  ppm_freecolorhash(cht);
	  return (colorhash_table) 0;
	}
	
	chl = (colorhist_list) malloc(sizeof(struct colorhist_list_item));
	if (!chl) FatalError("ran out of memory computing hash table");

	chl->ch.color = *pP;
	chl->ch.value = 1;
	chl->next = cht[hash];
	cht[hash] = chl;
      }
    }
  
  return cht;
}


/****************************************************************************/
static colorhash_table ppm_alloccolorhash()
{
  colorhash_table cht;
  int i;

  cht = (colorhash_table) malloc( HASH_SIZE * sizeof(colorhist_list) );
  if (!cht) FatalError("ran out of memory allocating hash table");

  for (i=0; i<HASH_SIZE; i++ )
    cht[i] = (colorhist_list) 0;

  return cht;
}


/****************************************************************************/
static colorhist_vector ppm_colorhashtocolorhist( cht, maxcolors )
     colorhash_table cht;
     int maxcolors;
{
  colorhist_vector chv;
  colorhist_list chl;
  int i, j;

  /* Now collate the hash table into a simple colorhist array. */
  chv = (colorhist_vector) malloc( maxcolors * sizeof(struct colorhist_item) );

  /* (Leave room for expansion by caller.) */
  if (!chv) FatalError("ran out of memory generating histogram");

  /* Loop through the hash table. */
  j = 0;
  for (i=0; i<HASH_SIZE; i++)
    for (chl = cht[i];  chl != (colorhist_list) 0;  chl = chl->next) {
      /* Add the new entry. */
      chv[j] = chl->ch;
      ++j;
    }

  return chv;
}


/****************************************************************************/
static void ppm_freecolorhist( chv )
     colorhist_vector chv;
{
  free( (char*) chv );
}


/****************************************************************************/
static void ppm_freecolorhash( cht )
     colorhash_table cht;
{
  int i;
  colorhist_list chl, chlnext;

  for (i=0; i<HASH_SIZE; i++)
    for (chl = cht[i];  chl != (colorhist_list) 0; chl = chlnext) {
      chlnext = chl->next;
      free( (char*) chl );
    }

  free( (char*) cht );
}
