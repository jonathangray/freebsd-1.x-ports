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

#include <stdio.h>
#include <math.h>
#include <signal.h>

#include "defs.h"
#include "data.h"


GLOBAL void     dmessage(p)
  register struct player *p;
{
  if(!p) return;
  if (p->p_umsg.m_pending)
    return;
  if (mctl->mc_current == p->lastm) {
    if (!p->mdisplayed)
      return;
    XFillRectangle(p->display, p->messagew, p->cleargc,
		   p->dfont->ascent, 5, fontWidth(p->dfont) * p->lastcount,
		   fontHeight(p->dfont));
    p->mdisplayed = 0;
    return;
  }
  do {
    struct message *cur;
    char            buf[80];

    if (++(p->lastm) >= MAXMSG)
      p->lastm = 0;
    cur = &messages[p->lastm];
    if (!(cur->m_flags & MVALID))
      continue;
    if (!((cur->m_flags & MALL) || (cur->m_flags & MEMPIRE) &&
	  (cur->m_recpt & p->p_mask) || (cur->m_flags & MINDIV)
	  && (cur->m_recpt == p->p_no)))
      continue;
    if (p->mdisplayed) {
      XFillRectangle(p->display, p->messagew, p->cleargc, p->dfont->ascent,
		5, fontWidth(p->dfont) * p->lastcount, fontHeight(p->dfont));
      p->mdisplayed = 0;
    }
    (void) strcpy(buf, cur->m_data);
    p->lastcount = strlen(buf);
    if (p->lastcount > 80)
      p->lastcount = 80;
    XDrawImageString(p->display, p->messagew, p->dfgc, p->dfont->ascent,
		     5 + p->dfont->ascent, buf, p->lastcount);
    if (p->do_bell)
      XBell(p->display, p->screen);
    p->mdisplayed = 1;
    XFlush(p->display);
    return;
  } while (p->lastm != mctl->mc_current);
}




/*
 * * The warning in text will be printed in the warning window.
 */
GLOBAL void     warning(p, text)
  register struct player *p;
  register char  *text;
{
  Display        *display;
  Window          window;
  XFontStruct    *font;

  if (ISROBOT(p))
    return;
  display = p->display;
  window = p->warnw;
  font = p->dfont;
  p->warntimer = udcounter + WARNTIME;
  /* WARNTIME updates later the line will be cleared */
  if (p->warncount > 0)
    XFillRectangle(display, window, p->cleargc,
		   5, 5, fontWidth(font) * p->warncount, fontHeight(font));
  p->warncount = strlen(text);
  XDrawImageString(display, window, p->dfgc,
		   5, 5 + font->ascent, text, p->warncount);
  XFlush(display);
}


GLOBAL void     itoa(n, len, buf)
  int             n, len;
  char           *buf;
{
  char            sign;
  char           *ptr;

  if (n >= 0)
    sign = ' ';
  else {
    n = -n;
    sign = '-';
  }
  ptr = buf + len;
  do {
    *--ptr = n % 10 + '0';
    if (ptr <= buf)
      return;
    n /= 10;
  }
  while (n > 0);
  *--ptr = sign;
  while (ptr > buf)
    *--ptr = ' ';
}
