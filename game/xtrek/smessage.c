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
 * representations about the suitability of this software for any purpose. It
 * is provided "as is" without express or implied warranty.
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
#include <ctype.h>
#include <signal.h>

#include "defs.h"
#include "data.h"

#define fw5 (5 + font_width)
#define fa5 (5 + font->ascent)
#define BROADCAST_CHAR '!'
#define PLAYER_OFFSET 1000
#define EMPIRE_OFFSET 2000

/*
 * returns -1 if bad, empire number + EMPIRE_OFFSET, player number +
 * PLAYER_OFFSET, or 0 for bcast
 */

LOCAL int       getaddr(p, who)
  register struct player *p;
  char            who;
{
  char           *ptr;
  int             i;
  int             pnum;

  (void) sprintf(p->p_umsg.m_addrmsg, " %c%x->", empires[p->p_empire].code, p->p_no);
  ptr = &p->p_umsg.m_addrmsg[5];
  if (who == BROADCAST_CHAR) {
    strcpy(ptr, "ALL");
    return 0;
  }
  for (i = 0; i < numempires; i++)
    if (who == empires[i].code) {
      strcpy(ptr, empires[i].abbrev);
      return i + EMPIRE_OFFSET;
    }
  if ('0' <= who && who <= '9')
    pnum = who - '0';
  else if ('a' <= who && who <= 'f')
    pnum = who - 'a' + 10;
  else {
    warning(p, "Not legal recipient.");
    return -1;
  }
  if (!isAlive(&players[pnum])) {
    warning(p, "Player is not in game.");
    return -1;
  }
  (void) sprintf(ptr, "%c%x ", empires[players[pnum].p_empire].code, pnum);
  return PLAYER_OFFSET + pnum;
}


GLOBAL void     smessage(p, ichar)
  register struct player *p;
  char            ichar;
{
  Display        *display = p->display;
  Window          window = p->messagew;
  XFontStruct    *font = p->dfont;
  int             font_width = fontWidth(font);
  int             font_height = fontHeight(font);
  char            icharbuf[2];

  /* this is for XDrawImageString down below. A hack.  - dan */
  icharbuf[0] = ichar;
  icharbuf[1] = '\0';

  if (p->p_umsg.m_pending == 0) {
    p->p_umsg.m_pending = 1;
    if (p->mdisplayed) {
      XFillRectangle(display, window, p->cleargc, fw5, 5,
		     font_width * p->lastcount, font_height);
      p->mdisplayed = 0;
    }
    /* Put the proper recipient in the window */
    if ((p->p_umsg.m_addr = getaddr(p, ichar)) < 0) {
      p->p_umsg.m_pending = 0;		/* print error message */
      return;
    }
    XDrawImageString(display, window, p->dfgc, fw5, fa5,
		     p->p_umsg.m_addrmsg, UMSGADDRLEN);
    p->p_umsg.m_lcount = UMSGADDRLEN;
    return;
  }
  switch (ichar) {
    case '\b':
    case '\177':
      if (--p->p_umsg.m_lcount < UMSGADDRLEN) {
	p->p_umsg.m_lcount = UMSGADDRLEN;
	break;
      }
      XFillRectangle(display, window, p->cleargc,
		     font_width * p->p_umsg.m_lcount, 5, font_width, font_height);
      break;

    case '\027':			/* CTRL-w */
      {
	int             i = 0;

	/* back up over blanks */
	while (--p->p_umsg.m_lcount >= UMSGADDRLEN &&
	       isspace(p->p_umsg.m_buf[p->p_umsg.m_lcount - UMSGADDRLEN]))
	  i++;
	p->p_umsg.m_lcount++;
	/* back up over non-blanks */
	while (--p->p_umsg.m_lcount >= UMSGADDRLEN &&
	       !isspace(p->p_umsg.m_buf[p->p_umsg.m_lcount - UMSGADDRLEN]))
	  i++;
	p->p_umsg.m_lcount++;
	if (i > 0)
	  XFillRectangle(display, window, p->cleargc, fw5 * p->p_umsg.m_lcount, 5, font_width * i, font_height);
      }
      break;

    case '\025':			/* CTRL-u */
    case '\030':			/* CTRL-x */
      while (--p->p_umsg.m_lcount >= UMSGADDRLEN)
	XFillRectangle(display, window, p->cleargc, 5 + font_width * UMSGADDRLEN, 5, font_width * (p->p_umsg.m_lcount - UMSGADDRLEN), font_height);
      p->p_umsg.m_pending = 0;
      break;

    case '\033':			/* ESC */
      XFillRectangle(display, window, p->cleargc, 5, 5, font_width * p->p_umsg.m_lcount, font_height);
      p->mdisplayed = 0;
      p->p_umsg.m_pending = 0;
      break;

    case '\r':
      p->p_umsg.m_buf[p->p_umsg.m_lcount - UMSGADDRLEN] = 0;
      p->p_umsg.m_pending = 0;
      {
	int             addr;
	char           *addrmsg;

	addr = p->p_umsg.m_addr;
	addrmsg = p->p_umsg.m_addrmsg;
	if (addr == 0)
	  pmessage(p->p_umsg.m_buf, 0, MALL, addrmsg);
	else if (addr > EMPIRE_OFFSET)
	  pmessage(p->p_umsg.m_buf, addr - EMPIRE_OFFSET, MEMPIRE, addrmsg);
	else if (addr > PLAYER_OFFSET)
	  pmessage(p->p_umsg.m_buf, addr - PLAYER_OFFSET, MINDIV, addrmsg);
	else
	  fprintf(stderr, "smessage: sanity check failed\n");
      }

      XFillRectangle(display, window, p->cleargc, 5, 5, font_width * p->p_umsg.m_lcount, font_height);
      p->mdisplayed = 0;
      p->p_umsg.m_lcount = 0;
      break;

    default:
      if (p->p_umsg.m_lcount == UMSGLEN) {
	if (p->do_bell)
	  XBell(display, p->screen);
	break;
      }
      if (iscntrl(ichar))
	break;

      XDrawImageString(display, window, p->dfgc, font_width * p->p_umsg.m_lcount, fa5, icharbuf, 1);
      p->p_umsg.m_buf[(p->p_umsg.m_lcount++) - UMSGADDRLEN] = ichar;
      break;
  }
}


GLOBAL void     pmessage(str, recip, group, address)
  char           *str;
  int             recip;
  int             group;
  char           *address;
{
  int             current;
  register struct message *cur;

  current = mctl->mc_current;
  if (++current >= MAXMSG)
    current = 0;
  cur = &messages[current];
  mctl->mc_current = current;
  cur->m_no = current;
  cur->m_flags = group;
  cur->m_time = 0;
  cur->m_recpt = recip;
  cur->m_flags |= MVALID;
  (void) sprintf(cur->m_data, "%s %s", address, str);
}
