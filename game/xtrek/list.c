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

#include "defs.h"
#include "data.h"

/*
 * * Open a window which contains all the planets and their current *
 * statistics.  Players will not know about planets that their empire * has
 * not orbited.
 */

GLOBAL void     planetlist(p)
  register struct player *p;
{
  Display        *display = p->display;
  Window          window = p->planetw;
  GC              gc = p->dfgc;
  int             font_ascent = p->dfont->ascent;
  int             font_height = fontHeight(p->dfont);
  int             y = font_ascent + font_height * 2;
  char            buf[BUFSIZ];
  struct planet  *planet;

  (void) sprintf(buf, "  # Planet Name      own armies REPAIR FUEL");
  XDrawImageString(display, window, gc, 0, font_ascent, buf, strlen(buf));
  for (planet = &planets[0]; planet < &planets[MAXPLANETS]; planet++) {
    if (!(planet->pl_info & FLAG(p->p_empire)))
      (void) sprintf(buf, " %2d %-16s", planet->pl_no, planet->pl_name);
    else
      (void) sprintf(buf, " %2d %-16s %3s %3d    %6s %4s",
		     planet->pl_no,
		     planet->pl_name,
		     empires[planet->pl_owner].abbrev,
		     planet->pl_armies,
		     (planet->pl_flags & PLREPAIR ? "REPAIR" : "      "),
		     (planet->pl_flags & PLFUEL ? "FUEL" : "    "));
    XDrawImageString(display, window, gc, 0, y, buf, strlen(buf));
    y += font_height;
  }
  XFlush(display);
}



GLOBAL void     playerlist(p)
  register struct player *p;
{
  struct player  *player;
  Display        *display = p->display;
  Window          window = p->playerw;
  GC              gc = p->dfgc;
  int             font_height = fontHeight(p->dfont);
  int             y = p->dfont->ascent;

  XDrawImageString(display, window, p->dfgc, 0, font_height,
	   " # Team Name             Login    Display          dir spd", 58);
  y += font_height * 2;
  for (player = &players[0]; player < &players[MAXPLAYER]; player++) {
    char            buf[BUFSIZ];

    if (player->p_status != PAlive)
      continue;
    (void) sprintf(buf, " %1x  %1c   %-16.16s %-8s %-16.16s %3d %3.1f",
		   player->p_no, empires[player->p_empire].code,
		   player->p_name, player->p_login, player->p_monitor,
		   player->p_dir, (float) player->p_speed * 0.1);
    XDrawImageString(display, window, gc, 0, y, buf, strlen(buf));
    y += font_height;
  }
  XFlush(display);
}



LOCAL char     *hdr =
" # Name              Kills   Max Killed Skill    Torps Phasers Planets Bombed";

GLOBAL void     scorelist(p)
  register struct player *p;
{
  Display        *display = p->display;
  XFontStruct    *font = p->dfont;
  int             font_height = fontHeight(font);
  int             y = font_height * 2;
  int             empnum;
  Window          window = XCreateSimpleWindow(display, p->mapw,
					       10, 10, 77 * fontWidth(font),
				(nplayers + numempires + 4) * font_height, 2,
					       p->borderColor, p->backColor);

  XMapWindow(display, window);
  XDrawImageString(display, window, p->dfgc, 0, font_height, hdr, 77);

  {
    struct player  *player;

    for (player = players; player < &players[MAXPLAYER]; player++) {
      double          player_kills;
      int             player_losses;
      char            buf[BUFSIZ];

      if (player->p_status == PFree)
	continue;
      player_kills = player->p_stats.st_kills;
      player_losses = player->p_stats.st_losses;
      sprintf(buf, " %1x %-16.16s %6.2f %5.2f %6d %5.2f %7d %7d %7d %6d",
	      player->p_no,
	      player->p_name,
	      player_kills,
	      player->p_stats.st_maxkills,
	      player_losses,
	      player_kills / (player_losses+1),
	      player->p_stats.st_torps,
	      player->p_stats.st_phasers,
	      player->p_stats.st_planets,
	      player->p_stats.st_armsbomb);
      XDrawImageString(display, window, p->dfgc, 0, y, buf, strlen(buf));
      y += font_height;
    }
  }

  for (empnum = 0; empnum < numempires; empnum++) {
    char            buf[BUFSIZ];

    y += font_height;
    sprintf(buf, " %1x %-16.16s %6.2f %5.2f %6d %5.2f %7d %7d %7d %6d",
	    empnum,
	    empires[empnum].name,
	    empires[empnum].stats.kills,
	    empires[empnum].stats.maxkills,
	    empires[empnum].stats.losses,
	    empires[empnum].stats.kills / (empires[empnum].stats.losses+1),
	    empires[empnum].stats.torps,
	    empires[empnum].stats.phasers,
	    empires[empnum].stats.planets,
	    empires[empnum].stats.armsbomb);
    XDrawImageString(display, window, p->dfgc, 0, y, buf, strlen(buf));
  }

  XFlush(display);
  p->infomapped = 1;
  p->infow = window;
}
