/*
 * bggen.c  -  a program that generates backgrounds for use with XV
 *
 * by John Bradley, University of Pennsylvania
 *   (bradley@cis.upenn.edu)
 *
 *      Rev: 8/31/90
 *      Rev: 10/17/90  -  added '-w' option
 */

/*
 * Copyright 1989, 1990, 1991, 1992 by John Bradley and
 *                       The University of Pennsylvania
 *
 * Permission to use, copy, and distribute for non-commercial purposes,
 * is hereby granted without fee, providing that the above copyright
 * notice appear in all copies and that both the copyright notice and this
 * permission notice appear in supporting documentation.
 *
 * The software may be modified for your own purposes, but modified versions
 * may not be distributed.
 *
 * This software is provided "as is" without any expressed or implied warranty.
 *
 * The author may be contacted via:
 *    US Mail:   John Bradley
 *               GRASP Lab, Room 301C
 *               3401 Walnut St.
 *               Philadelphia, PA  19104
 *
 *    Phone:     (215) 898-8813
 *    EMail:     bradley@cis.upenn.edu
 */



#include <stdio.h>

#define DEFSIZE 1024
#define MAXCOLS  128
#define cols xv_cols

struct color { int r,g,b; int y; } cols[MAXCOLS], *cur, *nex;

int bmask[8] = { 0x80, 0xc0, 0xe0, 0xf0, 0xf8, 0xfc, 0xfe, 0xff };


main(argc,argv)
int    argc;
char **argv;
{
  int i,j,cnt,numcols;
  int high = DEFSIZE;
  int wide = 1;
  int bits = 8;
  int r, g, b;

#ifdef VMS
  getredirection(&argc, &argv);
#endif

  cnt = 0;  numcols = 0;
  for (i=1; i<argc; i++) {
    if (!strcmp(argv[i],"-s")) high = atoi(argv[++i]);

    else if (!strcmp(argv[i],"-w")) wide = atoi(argv[++i]);

    else if (!strcmp(argv[i],"-b")) bits = atoi(argv[++i]);

    else if (argv[i][0]=='-') break;     /* any other '-' option is unknown */

    else {
      switch (cnt) {
      case 0:  cols[numcols].r = atoi(argv[i]);  break;
      case 1:  cols[numcols].g = atoi(argv[i]);  break;
      case 2:  cols[numcols].b = atoi(argv[i]);  break;
      }
      cnt++;

      if (cnt==3) {
	if (numcols<MAXCOLS) numcols++;
	cnt = 0;
      }
    }
  }


  if (cnt || numcols==0 || high<1 || bits<1 || bits>8) {
    fprintf(stderr,"usage:  %s [-s size] [-w width] [-b bits] %s\n\n",
	    argv[0], "r1 g1 b1 [r2 g2 b2 ...]");
    fprintf(stderr,"\tThis will generate a WIDTHxSIZE vertical color band.\n");
    fprintf(stderr,"\t(Default: 1x%d)  To set your background\n",DEFSIZE);
    fprintf(stderr,"\t'bits' is the number of significant bits in the\n");
    fprintf(stderr,"\tcolor specifications.  (1-8)\n");
    fprintf(stderr,"\tpipe the resulting output into this cmd:\n");
    fprintf(stderr,"\t\t'xv -root -quit -slow24 -'\n\n");
    exit(1);
  }

  printf("P3 %d %d 255\n",wide,high);

  /* special case code for numcols==1 */

  if (numcols==1) {
    for (i=0; i<high; i++) 
      for (j=0; j<wide; j++)
	printf("%d %d %d\n",cols[0].r,cols[0].g,cols[0].b);
  }
  else {

    /* fill in 'y' field of cols[] */
    for (i=0; i<numcols; i++)
      cols[i].y = ((high-1) * i) / (numcols-1);

    cur = &cols[0];  nex = cur+1;

    for (i=0; i<high; i++) {
      /* advance to next pair of colors if we're outside region */
      while (nex->y < i) { cur++; nex++; }

      r = cur->r + ((nex->r - cur->r) * (i - cur->y)) / (nex->y - cur->y);
      g = cur->g + ((nex->g - cur->g) * (i - cur->y)) / (nex->y - cur->y);
      b = cur->b + ((nex->b - cur->b) * (i - cur->y)) / (nex->y - cur->y);

      r = r & bmask[bits-1];
      g = g & bmask[bits-1];
      b = b & bmask[bits-1];

      for (j=0; j<wide; j++)
	printf("%d %d %d\n",r,g,b);
    }
  }

  exit(0);
}




