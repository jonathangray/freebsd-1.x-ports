/*
 * Copyright 1989 Jon Bennett, Mike Bolotski, David Gagne, Dan Lovinger
 * Copyright 1986 Chris Gutherie
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of the copyright holders not be used in
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission.  The copyright holders make no
 * representations about the suitability of this software for any purpose.  It
 * is provided "as is" without express or implied warranty.
 *
 * THE COPYRIGHT HOLDERS DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 */

#include <X11/Xlib.h>

#include <stdio.h>
#include <math.h>

#include "defs.h"
#include "data.h"

#define W ship_width
#define H ship_height

#define PI    3.14159265358979323846
#define PI2   6.28318530717958647692

LOCAL u_char     shipmaps[MAXEMPIRES][VIEWS][W * H / 8];


#define S(x,y) src[(H-1-(y))*W+(x)]
#define D(x,y) dst[(H-1-(y))*W+(x)]

LOCAL void      Rotate90(src, dst)
  u_char          *src;
  u_char          *dst;
{
  int             x, y;

  for (y = 0; y < H; y++)
    for (x = 0; x < W; x++)
      D(x, y) = S(W - 1 - y, x);
}


/* reflect about horizontal axis */
LOCAL void      ReflectH(src, dst)
  u_char          *src;
  u_char          *dst;
{
  int             x, y;

  for (y = 0; y < H; y++)
    for (x = 0; x < W; x++)
      D(x, y) = S(x, H - 1 - y);
}


/* reflect about vertical axis */
LOCAL void      ReflectV(src, dst)
  u_char          *src;
  u_char          *dst;
{
  int             x, y;

  for (y = 0; y < H; y++)
    for (x = 0; x < W; x++)
      D(x, y) = S(W - 1 - x, y);
}


/* reflect about diagonal */
LOCAL void      ReflectD(src, dst)
  u_char          *src;
  u_char          *dst;
{
  int             x, y;

  for (y = 0; y < H; y++)
    for (x = 0; x < W; x++)
      D(x, y) = S(y, x);
}


LOCAL void      rotate(angle, src, dst)
  float           angle;
  u_char          *src;
  u_char          *dst;
{
  int             h_x = W / 2;
  int             h_y = H / 2;
  int             x, y;
  double          sint = sin(angle);
  double          cost = cos(angle);

  for (y = 0; y < H; y++) {
    for (x = 0; x < W; x++) {
      double          xd = (double) (x - h_x) + 0.5;
      double          yd = (double) (y - h_y) + 0.5;
      double          ox = xd * cost - yd * sint;
      double          oy = xd * sint + yd * cost;
      int             iox, ioy;
      double          eps;

      ox -= 0.5;
      oy -= 0.5;
      if (oy < 0) {
	double          aoy = -oy;

	ioy = (int) aoy;
	eps = aoy - ioy;
	if (eps > 0.99)
	  ioy++;
	ioy = -ioy;
      }
      else {
	ioy = (int) oy;
	eps = oy - ioy;
	if (eps > 0.99)
	  ioy++;
      }

      if (ox < 0) {
	double          aox = -ox;

	iox = (int) aox;
	eps = aox - iox;
	if (eps > 0.99)
	  iox++;
	iox = -iox;
      }
      else {
	iox = (int) ox;
	eps = ox - (double) iox;
	if (eps > 0.99)
	  iox++;
      }


      iox += h_x;
      ioy += h_y;

      if (iox >= W || iox < 0 || ioy >= H || ioy < 0)
	D(x, y) = 0;
      else
	D(x, y) = S(iox, ioy);
    }
  }
}


#define ONE  1
#define ZERO 0

/****************************************************************************
 *
 * BMparse:
 *
 ****************************************************************************/

#define BUFLEN 512

LOCAL void      BMparse(file, array)
  char           *file;
  u_char          *array;
{
  int             tmp;
  int             end;
  int             mask;
  int             r, c;
  int             w = 0, h = 0;		/* Better be 16 x 16. */
  char            line[BUFLEN];
  char           *word;
  char           *strtok();
  int             isChar;
  FILE           *fp = OpenLibraryFile(file);

  while ((h == 0) || (w == 0)) {
    if (!fgets(line, BUFLEN, fp)) {
      fprintf(stderr, "Ouch.  I did not read the bitmap dimensions.\n");
      exit(1);
    }
    if (sscanf(line, "%*s %*s %d", &tmp) == 1) {
      if (w == 0)
	w = tmp;
      else if (h == 0)
	h = tmp;
    }
  }
  if ((w != W) || (h != H)) {
    fprintf(stderr, "Damn, the bitmap is not 16 x 16!\n");
    exit(1);
  }
  r = 0;
  c = W - 1;
  isChar = 0;
  while (fgets(line, BUFLEN, fp) && (r < H)) {
    word = strtok(line, " \n\t,");
    while (word) {
      if (strcmp(word, "char") == 0)
	isChar = 1;
      else if (strcmp(word, "short") == 0)
	isChar = 0;
      else if (sscanf(word, "0x%0x", &tmp) == 1) {
	mask = 0x01;
	if ((isChar) && (c == W - 1))
	  end = 7;
	else
	  end = -1;

	for (; c > end; c--) {
	  array[r * W + c] = (mask & tmp) ? ONE : ZERO;
	  mask <<= 1;
	}
	if (c <= 0) {
	  c = W - 1;
	  ++r;
	}
      }
      word = strtok(NULL, " \n\t,");
    }
  }
}


LOCAL void      ArrayToBitmap(src, dst)
  u_char          *src;
  u_char          *dst;
{
  u_char          *s = src;
  u_char          *d = dst;
  u_char           cur = 0;
  int             bit = 0;
  int             count = 0;
  int             i;

  for (i = 0; i < W * H; i++) {
    if (*s++)
      cur |= 1 << bit;

    if (++bit == 8) {
      *d++ = cur;
      bit = 0;
      cur = 0;
      count++;
    }
  }
  if (count != W * H / 8)
    fprintf(stderr, "ArrayToBitmap sanity check\n"), exit(1);
}


#ifdef debug
LOCAL void      DumpBitmap(name, view, src)
  char           *name;
  int             view;
  u_char          *src;
{
  char            buf[100];
  int             i;
  FILE           *fp;
  u_char          *s = src;

  sprintf(buf, "%s.%d", name, view);
  if (!(fp = fopen(buf, "w")))
    fprintf(stderr, "Couldn't open file\n"), exit(1);

  fprintf(fp, "#define bogus_width  16\n");
  fprintf(fp, "#define bogus_height 16\n");
  fprintf(fp, "LOCAL char bogus_bits[] = {\n");

  for (i = 1; i < W * H / 8; i++, s++)
    fprintf(fp, "0x%x, ", (unsigned int) *s);
  fprintf(fp, "0x%x};\n", (unsigned int) *s);
  fclose(fp);
}

#endif

#ifdef debug
LOCAL void      PrintArray(a)
  u_char          *a;
{
  int             r, c;

  for (r = 0; r < W; r++) {
    for (c = 0; c < H; c++)
      printf("%c", a[r * W + c] ? '1' : '0');
    printf("\n");
  }
  printf("\n");
}

#endif

#define DST(i)  (&dst[(i) * W * H])

#define V4 (VIEWS/4)
#define V2 (VIEWS/2)

GLOBAL void     CreateShipBitMaps()
{
  int             empire, i;
  float           angle;
  double          views = (double) VIEWS;

  u_char          *dst = (u_char *) malloc(W * H * VIEWS);


  for (empire = 0; empire < numempires; empire++) {	/* for each empire */
    char           *name = empires[empire].iconname;

    if (!*name)
      continue;

    BMparse(name, DST(0));

    for (i = 1; i < 3; i++) {		/* first quadrant */
      angle = ((double) i) / views * PI2;
      rotate(angle, DST(0), DST(i));
    }
    ReflectD(DST(1), DST(V4 - 1));	/* 1 -> 3 */


    Rotate90(DST(0), DST(V4));

    for (i = 1; i < V4; i++)		/* 4th quadrant */
      ReflectH(DST(i), DST(V2 - i));
    ReflectH(DST(0), DST(V2));

    for (i = 1 + V2; i < VIEWS; i++)	/* 2nd and 3rd */
      ReflectV(DST(VIEWS - i), DST(i));

    for (i = 0; i < VIEWS; i++) {
      ArrayToBitmap(DST(i), shipmaps[empire][i]);

      /* PrintArray(DST(i));  */
    }

  }
  free(dst);
}

GLOBAL void     InitPixMaps(p)
  aPlayer        *p;
{
  int             i, j;

  for (i = 0; i < numempires; i++) {
    if (!*empires[i].iconname) {
      for (j = 0; j < VIEWS; j++)	/* hope that Pixmap=0 illegal */
	p->shippics[i][j] = 0;
    }
    else {

      for (j = 0; j < VIEWS; j++) {
	p->shippics[i][j] =
	  XCreatePixmapFromBitmapData(p->display, p->w, shipmaps[i][j],
				      ship_width, ship_height, p->shipCol[i],
				      p->backColor, p->depth);
	if (p->shippics[i][j] == 0)
	  fprintf(stderr, "InitPixMap sanity check\n"), exit(1);
      }
    }
  }
}
