/*
 * Copyright 1989 Jon Bennett, Mike Bolotski, David Gagne, Dan Lovinger
 * Copyright 1986 Chris Gutherie
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of the copyright holders not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.  The copyright holders make no
 * representations about the suitability of this software for any purpose.
 * It is provided "as is" without express or implied warranty.
 *
 * THE COPYRIGHT HOLDERS DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 */

#include <X11/Xlib.h>
#include <X11/Xos.h>

#include <stdio.h>
#include "defs.h"
#include "data.h"

typedef struct assoc {
  char           *str;
  int             bwDef;		/* normal and inverse */
  char           *colorDef;
  u_long           xcolor;
}               ASSOC;

#define BLACK   0
#define WHITE   1
#define NCOLORS        13

static ASSOC    assoc[NCOLORS] = {
  {"border", WHITE, "blue"},
  {"background", BLACK, "black"},
  {"text", WHITE, "white"},
  {"fed", WHITE, "yellow",},
  {"rom", WHITE, "red",},
  {"kli", WHITE, "green",},
  {"ori", WHITE, "blue",},
  {"warning", WHITE, "red"},
  {"unknown", WHITE, "light grey"},
  {"ralert", WHITE, "red"},
  {"yalert", WHITE, "yellow"},
  {"galert", WHITE, "green"},
  {"me", WHITE, "white"}
};

LOCAL int       getMonoColors(p, prog)
  register struct player *p;
  char           *prog;
{
  int             i;
  u_long           bw[2];

  u_long           white_pix = XWhitePixel(p->display, p->screen);
  u_long           black_pix = XBlackPixel(p->display, p->screen);
  int             inv = booleanDefault(p, prog, "reverseVideo");

  bw[WHITE] = inv ? black_pix : white_pix;
  bw[BLACK] = inv ? white_pix : black_pix;

  p->borderColor = bw[assoc[0].bwDef];
  p->backColor = bw[assoc[1].bwDef];
  p->textColor = bw[assoc[2].bwDef];
  p->shipCol[0] = bw[assoc[3].bwDef];
  p->shipCol[1] = bw[assoc[4].bwDef];
  p->shipCol[2] = bw[assoc[5].bwDef];
  p->shipCol[3] = bw[assoc[6].bwDef];
  p->warningColor = bw[assoc[7].bwDef];
  p->unColor = bw[assoc[8].bwDef];
  p->aColor[(int) Red] = bw[assoc[9].bwDef];
  p->aColor[(int) Yellow] = bw[assoc[10].bwDef];
  p->aColor[(int) Green] = bw[assoc[11].bwDef];
  p->myColor = bw[assoc[12].bwDef];

  for (i = 4; i < MAXEMPIRES; i++)
    p->shipCol[i] = p->shipCol[0];

  return white_pix == 0;
}



GLOBAL int      getColorDefs(p, prog)
  register struct player *p;
  char           *prog;
{
  int             i;
  char           *color;
  Colormap        default_colormap;
  XColor          def;

  if (p->mono)				/* b & w */
    return getMonoColors(p, prog);


  default_colormap = XDefaultColormap(p->display, p->screen);

  for (i = 0; i < NCOLORS; i++) {
    if (!(color = XGetDefault(p->display, PROGRAM_NAME, assoc[i].str)))
      color = assoc[i].colorDef;
    def.pixel = 0;
    XParseColor(p->display, default_colormap, color, &def);
    XAllocColor(p->display, default_colormap, &def);
    assoc[i].xcolor = def.pixel;
  }
  p->borderColor = assoc[0].xcolor;
  p->backColor = assoc[1].xcolor;
  p->textColor = assoc[2].xcolor;
  p->shipCol[0] = assoc[3].xcolor;
  p->shipCol[1] = assoc[4].xcolor;
  p->shipCol[2] = assoc[5].xcolor;
  p->shipCol[3] = assoc[6].xcolor;
  p->warningColor = assoc[7].xcolor;
  p->unColor = assoc[8].xcolor;
  p->aColor[(int) Red] = assoc[9].xcolor;
  p->aColor[(int) Yellow] = assoc[10].xcolor;
  p->aColor[(int) Green] = assoc[11].xcolor;
  p->myColor = assoc[12].xcolor;

  for (i = 4; i < MAXEMPIRES; i++)
    p->shipCol[i] = p->shipCol[0];
  return 0;
}

/* return 1 if field is present and is 'on'  otherwise return 0 */

GLOBAL int      booleanDefault(p, prog, def)
  register struct player *p;
  char           *prog, *def;
{
  char           *str;

  return (str = XGetDefault(p->display, prog, def)) && !strcmp(str, "on");
}

#define iswhite(c)   ((c) == ' ' || c == '\t' || (c) == ',')

GLOBAL int      arrayDefault(p, def, sizeP, sp)
  register struct player *p;
  char           *def;
  int            *sizeP;
  char           *sp;
{
  int             max;
  int             rc;
  char           *str;

  if (!(str = XGetDefault(p->display, PROGRAM_NAME, def)))
    return -1;

  max = *sizeP;
  *sizeP = 0;

  for (;;) {
    while (iswhite(*str))
      str++;
    if (*str == '\0')
      break;
    if (++(*sizeP) > max)
      return (-1);
    if (sscanf(str, "0x%x", &rc) != 1)
      return (-1);
    sp[*sizeP] = rc;
    while (!iswhite(*str) && *str != '\0')
      str++;
  }
  return (0);
}
