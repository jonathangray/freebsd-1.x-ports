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
#include <X11/cursorfont.h>
#include <X11/Xutil.h>

#include <stdio.h>

#include "defs.h"
#include "data.h"

#define	MIN(a,b)	(((a) < (b)) ? (a) : (b))
#define	MAX(a,b)	(((a) > (b)) ? (a) : (b))

/* NOTE: These should be set in player data? */
#define MAX_LABEL 6			/* the length of the longest slider
					   label */

#define STAT_WIDTH		160
#define STAT_SPACE		5


GLOBAL void     updateStats(p, window)
  register struct player *p;
  Window          window;
{
  Display        *display;
  GC              gc;
  unsigned int    normal_color;
  unsigned int    warning_color;
  int             box_left;
  int             box_top;
  int             box_width;
  int             box_height;
  int             increment;
  aSlider        *slider_ptr;
  int             i;

  display = p->display;
  gc = p->sgc;
  normal_color = p->myColor;
  warning_color = p->warningColor;
  box_left = (MAX_LABEL + 1) * fontWidth(p->dfont) + STAT_SPACE;
  box_top = STAT_SPACE;
  box_width = STAT_WIDTH - STAT_SPACE - box_left;
  box_height = fontHeight(p->dfont);
  increment = box_height + STAT_SPACE;

  for (i = 0, slider_ptr = sliders[p->p_empire]; i < NUM_SLIDERS; i++, slider_ptr++, box_top += increment) {
    int             value;
    short          *old_value_ptr;
    int             old_value;
    int             new_x;
    
    switch (i) {
      case 0:
	value = p->p_shield;
	old_value_ptr = &p->p_oldshield;
	break;
      case 1:
	value = p->p_damage;
	old_value_ptr = &p->p_olddamage;
	break;
      case 2:
	value = p->p_fuel;
	old_value_ptr = &p->p_oldfuel;
	break;
      case 3:
	value = p->p_speed;
	old_value_ptr = &p->p_oldspeed;
	break;
      case 4:
	value = p->p_wtemp;
	old_value_ptr = &p->p_oldwtemp;
	break;
      case 5:
	value = p->p_etemp;
	old_value_ptr = &p->p_oldetemp;
	break;
      default:
	fprintf(stderr, "Slider sanity check failed\n");
	value = 0;
	old_value_ptr = 0;
	break;
    }
    value/=slider_ptr->scale;
    if (value < slider_ptr->min)
      value = slider_ptr->min;
    else if (value > slider_ptr->max)
      value = slider_ptr->max;
    old_value = *old_value_ptr;
    if (value == old_value)
      continue;
    new_x = value * box_width / slider_ptr->diff;
    /* We don't try to do any optimization of redraws here. Remember, the
       colors may have changed. */

    XSetForeground(display, gc,
		   value < slider_ptr->low_red ||
		   value > slider_ptr->high_red ?
		   warning_color : normal_color);
    if (new_x > 0)
      XFillRectangle(display, window, gc, box_left, box_top, new_x, box_height);
    if (new_x < box_width)
      XClearArea(display, window, box_left + new_x, box_top,
		 box_width - new_x, box_height, False);
    *old_value_ptr = value;
  }
}

GLOBAL void     initStats(p, prog)
  register struct player *p;
  char           *prog;
{
  char           *str;
  int             stat_x;
  int             stat_y;
  unsigned int    dummy;
  int             uspec;

  if (!(str = XGetDefault(p->display, prog, "stats.geometry")))
    return;
  uspec = XParseGeometry(str, &stat_x, &stat_y, &dummy, &dummy);
  p->uspec = uspec;
  p->statX = CHECKSPEC(uspec, (XValue | XNegative)) ? -stat_x : stat_x;
  p->statY = CHECKSPEC(uspec, (YValue | YNegative)) ? -stat_y : stat_y;
}


GLOBAL Window   openStats(p)
  struct player  *p;
{
  Display        *display;
  int             stat_height;
  Window          window;
  XGCValues       values;
  XSizeHints      wininfo;

  display = p->display;
  stat_height = NUM_SLIDERS * (fontHeight(p->dfont) + STAT_SPACE) + STAT_SPACE;
  window = XCreateSimpleWindow(display, RootWindow(display, p->screen),
	p->statX, p->statY, (unsigned) STAT_WIDTH, (unsigned) stat_height, 2,
			       p->borderColor, p->backColor);
  XSelectInput(display, window, ExposureMask);
  XStoreName(display, window, "xtrek-stats");
  XDefineCursor(display, window,
		(Cursor) XCreateFontCursor(display, XC_crosshair));
  values.foreground = p->myColor;
  values.background = p->backColor;
  p->sgc = XCreateGC(display, window, GCForeground | GCBackground, &values);
  wininfo.x = p->statX;
  wininfo.y = p->statY;
  wininfo.width = STAT_WIDTH;
  wininfo.height = stat_height;
  wininfo.min_width = STAT_WIDTH;
  wininfo.min_height = stat_height;
  wininfo.max_width = STAT_WIDTH;
  wininfo.max_height = stat_height;
  if (p->uspec & (XValue | YValue | XNegative | YNegative))
    wininfo.flags = USPosition | PSize | PMinSize | PMaxSize;
  else
    wininfo.flags = PPosition | PSize | PMinSize | PMaxSize;
  XSetNormalHints(display, window, &wininfo);
  XMapWindow(display, window);
  return window;
}


GLOBAL void     redrawStats(player, window)
  register struct player *player;
  Window          window;
{
  int             font_width = fontWidth(player->dfont);
  aSlider        *slider_ptr = sliders[player->p_empire];
  Display        *display = player->display;
  GC              gc = player->sgc;
  int             text_left = MAX_LABEL * font_width + STAT_SPACE;
  int             text_top = STAT_SPACE + player->dfont->ascent;
  int             box_left = text_left + font_width - 1;
  int             box_top = STAT_SPACE - 1;
  int             box_width = STAT_WIDTH - STAT_SPACE - box_left;
  int             font_height = fontHeight(player->dfont);
  int             box_height = font_height + 1;
  int             counter = NUM_SLIDERS - 1;
  int             increment = font_height + STAT_SPACE;

  XClearWindow(display, window);
  XSetForeground(display, gc, player->myColor);

  for (;;) {
    int             length = slider_ptr->label_length;

    XDrawImageString(display, window, gc, text_left - length * font_width,
		     text_top, slider_ptr->label, length);
    XDrawRectangle(display, window, gc, box_left, box_top,
		   box_width, box_height);
    if (--counter < 0)
      break;
    slider_ptr++;
    text_top += increment;
    box_top += increment;
  }

  /* reset sliders */
#define BOGUS -1
  player->p_oldshield = BOGUS;
  player->p_olddamage = BOGUS;
  player->p_oldfuel = BOGUS;
  player->p_oldspeed = BOGUS;
  player->p_oldwtemp = BOGUS;
  player->p_oldetemp = BOGUS;
}


GLOBAL void     closeStats(player, window)
  register struct player *player;
  Window          window;
{
  XWindowAttributes wa;
  Display        *display = player->display;

  XFreeGC(display, player->sgc);
  XGetWindowAttributes(display, window, &wa);
  XDestroyWindow(display, window);
  player->statX = wa.x;
  player->statY = wa.y;
  player->statwin = 0;
}
