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
#include <X11/cursorfont.h>
#include <X11/Xutil.h>

#include <stdio.h>
#include <math.h>

#include "defs.h"
#include "data.h"
#include "keys.h"

LOCAL char     *AUTHOR[] = {
  "",
  "XTREK Release Version 6.1",
  "",
  "This version by",
  "Jon Bennett (jcrb@cs.cmu.edu)",
  "Mario Goertzel (mg2p+@andrew.cmu.edu)",
  "Dan Lovinger (dl2n+@andrew.cmu.edu)",
  "Rob Ryan (rr2b+@andrew.cmu.edu)",
};

typedef struct hline {
  char            key;
  char           *mess;
}               HelpLine;

LOCAL HelpLine  help_message[] = {
  {' ', "0-9 Set speed"},
  {FRACWARP_KEY, "Set fractional speed"},
  {SETCOURSE_KEY, "Set course"},

#ifdef TELEPORT_OPTION
  {TELEPORT_KEY, "Teleport"},
#endif					/* TELEPORT_OPTION */

#ifdef TURBO_OPTION
  {TURBO_KEY, "Turbo drive"},
#endif					/* TURBO_OPTION */

  {PHASER_KEY, "Fire phaser"},
  {TORP_KEY, "Launch torp"},
  {MINE_KEY, "Drop a mine"},
  {DET_OTHER_TORP_KEY, "Detonate enemy torps"},
  {DET_OWN_TORP_KEY, "Detonate own torps"},
  {DET_OWN_MINE_KEY, "Detonate own mines"},
  {REPAIR_KEY, "Enter repair mode"},
  {CLOAK_KEY, "Toggle cloaking device"},
  {TOGGLE_SHIELDS_KEY, "Toggle shields"},
  {SHIELDS_UP_KEY, "Put up shields"},
  {SHIELDS_DOWN_KEY, "Put down shields"},
  {LOCK_KEY, "Lock on to player or planet"},
  {FOLLOW_KEY, "Follow locked ship"},
  {BOMB_KEY, "Bomb planet below"},
  {BEAM_UP_KEY, "Beam up armies from planet"},
  {BEAM_DOWN_KEY, "Beam down armies to planet"},
  {COUP_KEY, "Coup a planet"},
  {INFO_KEY, "Get info on player/planet"},
  {ORBIT_KEY, "Orbit planet"},
  {QUIT_KEY, "Quit Xtrek"},
  {REPEAT_MESSAGE_KEY, "Review messages"},
  {TOGGLE_COPILOT_KEY, "(Dis)Allow copilots"},
  {PLAYER_LIST_KEY, "List player stats"},
  {PLANET_LIST_KEY, "List planet stats"},
  {SCORE_LIST_KEY, "List scores"},
  {TOGGLE_STAT_KEY, "Toggle status window"},
  {TOGGLE_SHOW_SHIELDS_KEY, "Toggle showing of shields"},
  {TOGGLE_MAP_MODE_KEY, "Toggle map window updating"},
  {HELP_KEY, "Toggle this window"},
  {WAR_KEY, "Toggle war window"},
  {TOGGLE_BELL_KEY, "Toggle bell mode"},
  {TOGGLE_NAME_MODE_KEY, "Toggle planet name mode"},
  {SETRHOSTILE_KEY, "Turn on HOSTILE mode"},
  {CLRRHOSTILE_KEY, "Turn off HOSTILE mode"},
  {ROBOT_HORDE_KEY, "Bring in robot horde"},
  {0, ""}
};

#define MAXHELPWIDTH 40
#define NCOL 4
#define NROW (int) ((((sizeof(help_message)/sizeof(HelpLine)) - 1.0)/ (float) NCOL) + 0.99)

GLOBAL void     fillhelp(p)
  register struct player *p;
{
  register int    i = 0, row, column;
  char            buf[100];

  for (column = 0; column < NCOL; column++)
    for (row = 1; row <= NROW; row++, i++) {
      if (help_message[i].key == 0)
	return;
      sprintf(buf, "%c  %s", help_message[i].key, help_message[i].mess);
      XDrawImageString(p->display, p->helpWin, p->dfgc,
		       fontWidth(p->dfont) * (MAXHELPWIDTH * column + 1),
	    fontHeight(p->dfont) * row + p->dfont->ascent, buf, strlen(buf));
    }
}

GLOBAL void     showMotd(p)
  register struct player *p;
{
  char            buf[BUFSIZ];
  FILE           *motd;
  int             i, length, top, center;

  /* Author Gratification */

  XClearWindow(p->display, p->w);
  for (i = 0; i < SIZEOF(AUTHOR); i++) {
    length = strlen(AUTHOR[i]);
    center = WINSIDE / 2 - (length * fontWidth(p->dfont)) / 2;
    XDrawImageString(p->display, p->w, p->dfgc, center,
	     i * fontHeight(p->dfont) + p->dfont->ascent, AUTHOR[i], length);
  }
  top = SIZEOF(AUTHOR) + 2;

  /* the follwing will print a motd */
  if (motd = fopen(MOTD, "r")) {
    for (i = top; fgets(buf, sizeof (buf), motd); i++) {
      length = strlen(buf);
      buf[length - 1] = NULL;
      if (length > 80)
	length = 80;
      XDrawImageString(p->display, p->w, p->dfgc, 20,
		   i * fontHeight(p->dfont) + p->dfont->ascent, buf, length);
    }
    (void) fclose(motd);
  }
}


GLOBAL void     CreateHelpWindow(p, RootWin)
  aPlayer        *p;
  Window          RootWin;
{
  Display        *display;
  Window          window;
  XSizeHints      wininfo;

  display = p->display;
  window = XCreateWindow(display, RootWin,
			 0, YOFF + WINSIDE + 2 * BORDER + 2 * MSGSIZE,
			 (unsigned) WINSIDE * 2 + 1 * BORDER,
			 (unsigned) (NROW + 2) * fontHeight(p->dfont), BORDER,
			 p->depth,
			 InputOutput, (Visual *) CopyFromParent, 0L,
			 (XSetWindowAttributes *) 0);
  XStoreName(display, window, "xtrek-help");
  XSetWindowBackground(display, window, p->backColor);
  XSetWindowBorder(display, window, p->borderColor);
  wininfo.x = 0;
  wininfo.y = YOFF + WINSIDE + BORDER * 2 + MSGSIZE * 2;
  wininfo.width = WINSIDE * 2 + BORDER;
  wininfo.height = fontHeight(p->dfont) * (NROW + 2);
  wininfo.min_width = WINSIDE * 2 + BORDER;
  wininfo.min_height = NROW * fontHeight(p->dfont);
  wininfo.max_width = WINSIDE * 2 + BORDER;
  wininfo.max_height = wininfo.min_height * 2;
  wininfo.flags = PPosition | PSize | PMinSize | PMaxSize;
  XSetNormalHints(display, window, &wininfo);
  p->helpWin = window;
}
