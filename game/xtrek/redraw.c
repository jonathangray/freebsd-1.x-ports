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
#include <signal.h>
#include <math.h>

#include "defs.h"
#include "data.h"
#include "polar.h"

/* NOTE: This definition depends on MAXPLAYERS! */
static char    *shipnos = "0123456789abcdef";

static int      stlinecount = 1;

#define CLEARZONE(p, c, X, Y, W, H) \
 { (c)->x = (X)  ; (c)->y = (Y); (c)->width = (W); (c)->height=(H); \
   (p)->clearcount++; (c)++; }

#define CLEARLINE(p, x1, y1, x2, y2) \
 { int __c = p->clearlcount; \
    p->clearline[0][__c] = x1; p->clearline[1][__c] = y1; \
    p->clearline[2][__c] = x2; p->clearline[3][__c] = y2; \
    p->clearlcount = __c+1; \
 }


#define WINDOW_WIDTH WINSIDE
#define WINDOW_HEIGHT WINSIDE

#define BORDER_WIDTH 2
#define BORDER_HEIGHT 2

/*
 * Moved some local variables outside of DrawLocalWindow  to allow reuse by
 * DrawShips, DrawPlanets, DrawTorps
 */

static int      origin_x;
static int      origin_y;
static Display *display;
static XFontStruct *font;
static int      font_width;
static int      font_height;
static int      font_hwidth;
static int      font_hheight;
static XRectangle *clearzone;


/*
 * compute position of the end of the phasor and return in locations pointed
 * to by x_ref and y_ref.
 */
LOCAL void      phaserdist(p, php, x_ref, y_ref)
  struct player  *p;
  struct phaser  *php;
  int            *x_ref;
  int            *y_ref;

{
  int             dir = php->ph_dir;
  int             dist = p->ship.phaserdist;

  *x_ref = POLAR_X(dist, dir) + p->p_x;
  *y_ref = POLAR_Y(dist, dir) + p->p_y;
}


LOCAL void      drawPartialPhaser(p, j)
  struct player  *p, *j;
{
  int             sx, sy;
  int             ex, ey;
  struct phaser  *php;

  sx = j->p_x - p->p_x;
  sy = j->p_y - p->p_y;
  /* Now draw his phaser (if it exists) */
  php = &phasers[j->p_no];
  if (php->ph_status == PhMiss) {
    /* Here I will have to compute end coordinate */
    phaserdist(j, php, &ex, &ey);
    ex -= p->p_x;
    ey -= p->p_y;
  }
  else {
    int             target = php->ph_target;

    ex = (players[target].p_x) - p->p_x;
    ey = (players[target].p_y) - p->p_y;
  }
  /* phaser end points are now relative to our position, make them relative to
     window origin (so X clips them correctly) */
  sx = sx / SCALE + WINSIDE / 2;
  ex = ex / SCALE + WINSIDE / 2;
  if (sx < 0 && ex < 0 || sx > WINSIDE && ex > WINSIDE)
    return;
  sy = sy / SCALE + WINSIDE / 2;
  ey = ey / SCALE + WINSIDE / 2;
  if (sy < 0 && ey < 0 || sy > WINSIDE && ey > WINSIDE)
    return;
  {
    Display        *display = p->display;
    GC              gc = p->xfgc;

    XSetForeground(display, gc, phaserColor(php));
    XDrawLine(display, p->w, gc, sx, sy, ex, ey);
  }
  CLEARLINE(p, sx, sy, ex, ey);
}


LOCAL void      DrawPlanets(p)
  struct player  *p;
{
  struct planet  *planet;
  int             unscaled_x;
  int             unscaled_y;
  int             x;
  int             y;
  char            glyph;

  for (planet = &planets[0]; planet < &planets[MAXPLANETS]; planet++) {
    unscaled_x = planet->pl_x - origin_x;
    if ((unsigned) unscaled_x >= WINDOW_WIDTH * SCALE)
      continue;
    unscaled_y = planet->pl_y - origin_y;
    if ((unsigned) unscaled_y >= WINDOW_HEIGHT * SCALE)
      continue;
    XSetForeground(display, p->xfgc, planetColor(planet));
    x = unscaled_x / SCALE;
    y = unscaled_y / SCALE;
    glyph = planetGlyph(planet);
    XDrawString(display, p->w, p->xfgc, x - planet_hwidth,
		y - planet_hheight, &glyph, 1);

    if (p->namemode) {
      int             name_length = planet->pl_namelen;
      int             name_x = x - name_length * font_hwidth;
      int             name_y = y + planet_hheight;

      XDrawImageString(display, p->w, p->dfgc, name_x, name_y + font->ascent, planet->pl_name, name_length);
      CLEARZONE(p, clearzone, name_x, name_y, name_length * font_width, font_height);
    }
    CLEARZONE(p, clearzone, x - planet_hwidth, y - planet_hwidth, planet_width, planet_height);
  }
}

#ifdef OLDDRAWTORPS
LOCAL void      DrawTorps(p)
  struct player  *p;
{
  struct torp    *torp;
  int             unscaled_x;
  int             unscaled_y;
  int             x;
  int             y;

  for (torp = &torps[0]; torp < &torps[MAXPLAYER * MAXTORP]; torp++) {
    if (torp->t_status==TFree)
      continue;
    unscaled_x = torp->t_x - origin_x;
    if ((unsigned) unscaled_x >= WINDOW_WIDTH * SCALE)
      continue;
    unscaled_y = torp->t_y - origin_y;
    if ((unsigned) unscaled_y >= WINDOW_HEIGHT * SCALE)
      continue;
    XSetForeground(display, p->xfgc, torpColor(torp));
    x = unscaled_x / SCALE;
    y = unscaled_y / SCALE;
    if (torp->t_status == TExplode) {
      int             cl_scale = torp->t_range;

      if (cl_scale >= 800) {
	char            glyph;

	x -= ex_hwidth;
	y -= ex_hheight;
	if (cl_scale >= 2400)
	  cl_scale = 4;
	else if (cl_scale >= 1800)
	  cl_scale = 3;
	else if (cl_scale >= 1500)
	  cl_scale = 2;
	else if (cl_scale >= 1350)
	  cl_scale = 1;
	else
	  cl_scale = 0;
	glyph = EXP_GLYPHS_LEFT + cl_scale;
	XDrawString(display, p->w, p->xfgc, x, y, &glyph, 1);
	glyph = EXP_GLYPHS_RIGHT + cl_scale;
	XDrawString(display, p->w, p->xfgc, x + ex_hwidth, y, &glyph, 1);
	CLEARZONE(p, clearzone, x, y, ex_width, ex_height);
      }
      else {
	static char     glyph = CLOUD_GLYPH;

	x -= cloud_hwidth;
	y -= cloud_hheight;
	XDrawString(display, p->w, p->xfgc, x, y, &glyph, 1);
	CLEARZONE(p, clearzone, x, y, cloud_width, cloud_height);
      }
    }
    else if (torp->t_owner != p->p_no &&
	     ((torp->t_war & p->p_mask) ||
	      torp->t_mask & (p->p_hostile | p->p_swar))) {
      static char     glyph = ETorp_GLYPH;

      x -= etorp_hwidth;
      y -= etorp_hheight;
      XDrawString(display, p->w, p->xfgc, x, y, &glyph, 1);
      CLEARZONE(p, clearzone, x, y, etorp_width, etorp_height);
    }
    else {
      static char     glyph = MTorp_GLYPH;

      x -= mtorp_hwidth;
      y -= mtorp_hheight;
      XDrawString(display, p->w, p->xfgc, x, y, &glyph, 1);
      CLEARZONE(p, clearzone, x, y, mtorp_width, mtorp_height);
    }
  }
}
#else /* OLDDRAWTORPS */

LOCAL void      DrawTorps(p)
  struct player  *p;
{
  struct torp    *torp;
  int             unscaled_x;
  int             unscaled_y;
  int             x;
  int             y;

  for (torp = &torps[0]; torp < &torps[MAXPLAYER * MAXTORP]; torp++) {
    if (torp->t_status==TFree)
      continue;
    unscaled_x = torp->t_x - origin_x;
    if ((unsigned) unscaled_x >= WINDOW_WIDTH * SCALE)
      continue;
    unscaled_y = torp->t_y - origin_y;
    if ((unsigned) unscaled_y >= WINDOW_HEIGHT * SCALE)
      continue;
 /*   XSetForeground(display, p->xfgc, torpColor(torp)); */
    x = unscaled_x / SCALE;
    y = unscaled_y / SCALE;
    if (torp->t_status == TExplode) {
      int             cl_scale = torp->t_range;

      if (cl_scale >= 800) {
	char            glyph;

	x -= ex_hwidth;
	y -= ex_hheight;
	if (cl_scale >= 2400)
	  cl_scale = 4;
	else if (cl_scale >= 1800)
	  cl_scale = 3;
	else if (cl_scale >= 1500)
	  cl_scale = 2;
	else if (cl_scale >= 1350)
	  cl_scale = 1;
	else
	  cl_scale = 0;
	glyph = EXP_GLYPHS_LEFT + cl_scale;
	XDrawString(display, p->w, p->xfgc, x, y, &glyph, 1);
	glyph = EXP_GLYPHS_RIGHT + cl_scale;
	XDrawString(display, p->w, p->xfgc, x + ex_hwidth, y, &glyph, 1);
	CLEARZONE(p, clearzone, x, y, ex_width, ex_height);
      }
      else {
	static char     glyph = CLOUD_GLYPH;

	x -= cloud_hwidth;
	y -= cloud_hheight;
	XDrawString(display, p->w, p->xfgc, x, y, &glyph, 1);
	CLEARZONE(p, clearzone, x, y, cloud_width, cloud_height);
      }
    }
    else if (torp->t_owner != p->p_no &&
	     ((torp->t_war & p->p_mask) ||
	      torp->t_mask & (p->p_hostile | p->p_swar))) {
      static char     glyph = ETorp_GLYPH;

      x -= etorp_hwidth;
      y -= etorp_hheight;
      XDrawString(display, p->w, p->xfgc, x, y, &glyph, 1); 
      CLEARZONE(p, clearzone, x, y, etorp_width, etorp_height);
    }
    else {
      static char     glyph = MTorp_GLYPH;

      x -= mtorp_hwidth;
      y -= mtorp_hheight;
      XDrawString(display, p->w, p->xfgc, x, y, &glyph, 1); 
      CLEARZONE(p, clearzone, x, y, mtorp_width, mtorp_height);
    }
  }
}
#endif /* OLDDRAWTORPS */


LOCAL void      DrawShips(p)
  struct player  *p;
{
  struct player  *player;
  int             unscaled_x;
  int             unscaled_y;
  int             x;
  int             y;

  for (player = &players[0]; player < &players[MAXPLAYER]; player++) {
    if (player->p_status != PAlive && player->p_status != PExplode)
      continue;
    unscaled_x = player->p_x - origin_x;
    unscaled_y = player->p_y - origin_y;
    if (player != p) {
      if ((unsigned) unscaled_x >= WINDOW_WIDTH * SCALE ||
	  (unsigned) unscaled_y >= WINDOW_HEIGHT * SCALE) {
	switch (phasers[player->p_no].ph_status) {
	  case PhHit:
	  case PhMiss:
	    drawPartialPhaser(p, player);
	    break;
	  default:;
	}
	continue;
      }
      if (ISCLOAK(player)) {
	int             delta_x = unscaled_x - WINDOW_WIDTH * SCALE / 2;
	int             delta_y = unscaled_y - WINDOW_HEIGHT * SCALE / 2;
	int             distance_squared = delta_x * delta_x + delta_y * delta_y;
	int             max;

#define CLM (4000 * 4000)
#define LCLM (8000 * 8000)

	if (ISPLOCK(p) && player->p_no == p->p_playerl)
	  max = LCLM;
	else
	  max = CLM;
	if (distance_squared > max || RAND(max) < distance_squared)
	  continue;
      }
    }
    XSetForeground(display, p->xfgc, playerColor(player));
    x = unscaled_x / SCALE;
    y = unscaled_y / SCALE;
    switch (player->p_status) {
      case PAlive:
	{
	  int             oempire = player->p_empire;
	  int             r = rosette(player->p_dir);
	  Pixmap          pix = p->shippics[oempire][r];

	  if (pix) {			/* found a pixmap so use it */
	    XCopyArea(display, pix, p->w, p->monogc, 0, 0,
		 ship_width, ship_height, x - ship_hwidth, y - ship_hheight);
	  }
	  /* no pixmap, use font */
	  else {
	    int             nglyph = oempire >= DEFEMPIRES ? 0 : oempire;
	    char            glyph = SHIP_BASE + SHIP_MULT * nglyph + r;

	    XDrawString(display, p->w, p->xfgc,
			x - ship_hwidth, y - ship_hheight, &glyph, 1);
	  }
	}
	/* NOTE: This DrawImageString doesn't use the right GC */
	XDrawImageString(display, p->w, p->dfgc, x + ship_hwidth,
			 y - ship_hheight + font->ascent,
			 shipnos + player->p_no, 1);
	/* NOTE: fix to change shield color depending on damage.. */
	if (p->showShields && ISSHIELD(player)) {
	  char            glyph;

	  if (player->p_damage > p->ship.maxdamage / 2)
	    glyph = RShield_GLYPH;
	  else if (player->p_shield < p->ship.maxshields / 2)
	    glyph = YShield_GLYPH;
	  else
	    glyph = Shield_GLYPH;
	  /* if !self then glyph = Shield_GLYPH; old else for enemy shield
	     disp */
	  XDrawString(display, p->w, p->xfgc, x - shield_hwidth,
		      y - shield_hheight, &glyph, 1);
	}
	/* NOTE: move XTextWidth calc into player structure */

	CLEARZONE(p, clearzone, x + ship_hwidth, y - ship_hheight,
		  XTextWidth(font, shipnos + player->p_no, 1),
		  fontHeight(font));
	CLEARZONE(p, clearzone, x - shield_hwidth, y - shield_hheight,
		  shield_width, shield_height);
	break;
      case PExplode:
	{
	  char            glyph;
	  int             expl_x = x - ex_hwidth;
	  int             expl_y = y - ex_hheight;

	  glyph = EXP_GLYPHS_LEFT + ((10 - player->p_explode) / 2);
	  XDrawString(display, p->w, p->xfgc, expl_x, expl_y, &glyph, 1);
	  glyph = EXP_GLYPHS_RIGHT + ((10 - player->p_explode) / 2);
	  XDrawString(display, p->w, p->xfgc, x, expl_y, &glyph, 1);
	  CLEARZONE(p, clearzone, expl_x, expl_y, ex_width, ex_height);
	}
      default:;
    }
    /* Now draw his phaser (if it exists) */
    {
      int             end_x;
      int             end_y;
      struct phaser  *php = &phasers[player->p_no];

      switch (php->ph_status) {
	case PhMiss:
	  /* Here I will have to compute end coordinate */
	  {
	    int             temp_x;
	    int             temp_y;

	    phaserdist(player, php, &temp_x, &temp_y);
	    end_x = (temp_x - origin_x) / SCALE;
	    end_y = (temp_y - origin_y) / SCALE;
	  }
	  break;
	case PhHit:
	  {
	    int             target = php->ph_target;

	    end_x = (players[target].p_x - origin_x) / SCALE;
	    end_y = (players[target].p_y - origin_y) / SCALE;
	  }
	  break;
	default:
	  continue;
      }
      XSetForeground(display, p->xfgc, phaserColor(php));
      XDrawLine(display, p->w, p->xfgc, x, y, end_x, end_y);
      CLEARLINE(p, x, y, end_x, end_y);
    }
  }
}


LOCAL void      DrawLocalWindow(p)
  register struct player *p;
{
  /* Missing declarations! */

  clearzone = &p->clearzone[p->clearcount];

  origin_x = p->p_x - WINDOW_WIDTH * SCALE / 2;
  origin_y = p->p_y - WINDOW_HEIGHT * SCALE / 2;
  display = p->display;
  font = p->dfont;
  font_width = fontWidth(font);
  font_height = fontHeight(font);
  font_hwidth = font_width / 2;
  font_hheight = font_height / 2;

  DrawPlanets(p);			/* Draw Planets */
  DrawTorps(p);				/* draw torps */
  DrawShips(p);

  /* draw edges */
  XSetForeground(display, p->xfgc, p->warningColor);
  if (origin_y <= -SCALE)
    /* top edge */
  {
    /* Adding in GWIDTH and subtracting it back out may seem stupid, but it
       makes it so that the dividend is always positive. */

    int             left = (GWIDTH - origin_x) / SCALE - GWIDTH / SCALE - BORDER_WIDTH;
    int             top = (GWIDTH - origin_y) / SCALE - GWIDTH / SCALE - BORDER_WIDTH;

    XFillRectangle(display, p->w, p->xfgc, left, top,
		   GWIDTH / SCALE + BORDER_WIDTH * 2, BORDER_HEIGHT);
    CLEARZONE(p, clearzone, left, top, GWIDTH / SCALE + BORDER_WIDTH * 2,
	      BORDER_HEIGHT);
  }
  if (origin_x <= -SCALE)
    /* left edge */
  {
    int             left = (GWIDTH - origin_x) / SCALE - GWIDTH / SCALE - BORDER_WIDTH;
    int             top = (GWIDTH - origin_y) / SCALE - GWIDTH / SCALE;

    XFillRectangle(display, p->w, p->xfgc, left, top, BORDER_WIDTH,
		   GWIDTH / SCALE);
    CLEARZONE(p, clearzone, left, top, BORDER_WIDTH, GWIDTH / SCALE);
  }
  if (origin_x > GWIDTH - WINDOW_WIDTH * SCALE)
    /* right edge */
  {
    int             left = (GWIDTH - origin_x) / SCALE;
    int             top = (GWIDTH - origin_y) / SCALE - GWIDTH / SCALE;

    XFillRectangle(display, p->w, p->xfgc, left, top,
		   BORDER_WIDTH, GWIDTH / SCALE);
    CLEARZONE(p, clearzone, left, top, BORDER_WIDTH, GWIDTH / SCALE);
  }
  if (origin_y >= GWIDTH - (WINDOW_HEIGHT - 1) * SCALE)
    /* bottom edge */
  {
    int             left;
    int             top;

    left = (GWIDTH - origin_x) / SCALE - GWIDTH / SCALE - BORDER_WIDTH;
    top = (GWIDTH - origin_y) / SCALE;
    XFillRectangle(display, p->w, p->xfgc, left, top,
		   GWIDTH / SCALE + BORDER_WIDTH * 2, BORDER_HEIGHT);
    CLEARZONE(p, clearzone, left, top, GWIDTH / SCALE + BORDER_WIDTH * 2,
	      BORDER_HEIGHT);
  }
  /* Change border color to signify alert status */
  {
    AlertLevel      alert = p->p_alert;

    if (alert != p->oldalert) {
      if (!p->mono) {
	u_long           color = p->aColor[(int) alert];

	XSetWindowBorder(display, p->baseWin, color);
	XSetWindowBorder(display, p->mapw, color);
	XSetWindowBorder(display, p->w, color);
	XSetWindowBorder(display, p->warnw, color);
	XSetWindowBorder(display, p->messagew, color);
	XSetWindowBorder(display, p->tstatw, color);
	XSetWindowBorder(display, p->iconWin, color);
      }
      else {
	  Pixmap          tile = p->aTile[(int) alert];
	  XSetWindowBorderPixmap(display, p->baseWin, tile);
	  XSetWindowBorderPixmap(display, p->mapw, tile);
	  XSetWindowBorderPixmap(display, p->iconWin, tile);
	  XSetWindowBorderPixmap(display,p->messagew,tile);
	  XSetWindowBorderPixmap(display,p->w,tile);
	  XSetWindowBorderPixmap(display,p->warnw,tile);
	  XSetWindowBorderPixmap(display,p->tstatw,tile);
      }
      p->oldalert = alert;
    }
  }
}


GLOBAL void     map(p)
register struct player *p;
{
    Display        *display = p->display;
    Window          window = p->mapw;
  XFontStruct    *font = p->dfont;
  int             font_width = fontWidth(font);
  int             font_height = fontHeight(font);

  if (p->mclearcount) {
    XFillRectangles(display, window, p->cleargc, p->mclearzone, p->mclearcount);
    p->mclearcount = 0;
  }
  /* Draw Planets */
  {
    struct planet  *planet;

    for (planet = &planets[0]; planet < &planets[MAXPLANETS]; planet++) {
      int             x;
      int             y;
      char            glyph;

      if (!p->redrawall && !(planet->pl_flags & PLREDRAW))
	continue;
      XSetForeground(display, p->xfgc, planetColor(planet));
      x = planet->pl_x * WINSIDE / GWIDTH;
      y = planet->pl_y * WINSIDE / GWIDTH;
      glyph = mplanetGlyph(planet);
      XDrawString(display, window, p->xfgc, x - mplanet_hwidth,
		  y - mplanet_hheight, &glyph, 1);
      XDrawImageString(display, window, p->dfgc, x - (font_width * 3 >> 1),
		     y + mplanet_hheight + font->ascent, planet->pl_name, 3);
    }
  }
  p->redrawall = 0;

  /* Draw ships */
  {
    struct player  *player;

    for (player = &players[0]; player < &players[MAXPLAYER]; player++) {
      int             x;
      int             y;
      char           *str;
      int             count;

      if (player->p_status != PAlive || ISCLOAK(player))
	continue;
      x = player->p_x * WINSIDE / GWIDTH - font_width;
      y = player->p_y * WINSIDE / GWIDTH - font_hheight;
      /* NOTE: put these calculations into the player data */
      str = player->p_mapchars;
      XDrawImageString(display, window, p->dfgc, x, y + font->ascent, str, 2);
      count = p->mclearcount;
      p->mclearzone[count].x = x;
      p->mclearzone[count].y = y;
      p->mclearzone[count].width = font_width * 2;
      p->mclearzone[count].height = font_height;
      p->mclearcount = count + 1;
    }
  }
}


GLOBAL void     redraw(j)
  register struct player *j;
{
  static char    *buf = "                                                                        ";
  struct player  *p;

  /* erase warning line if necessary */
  if (j->warntimer <= udcounter && j->warncount > 0) {
    XClearWindow(j->display, j->warnw);
    j->warncount = 0;
  }
  /* clear rectangles */
  if (j->clearcount > 0) {
    XFillRectangles(j->display, j->w, j->cleargc, j->clearzone, j->clearcount);
    j->clearcount = 0;
  }
  /* NOTE: Can this be made into an XDrawLines? */
  {
    int             count;

    for (count = j->clearlcount; --count >= 0;)
      XDrawLine(j->display, j->w, j->cleargc,
		j->clearline[0][count], j->clearline[1][count],
		j->clearline[2][count], j->clearline[3][count]);
    j->clearlcount = 0;
  }

  if (!ISCLOAK(j) && j->mapmode && udcounter % 5 == 0 && !FAILED(j, LRS))
    map(j);

  if (udcounter % MESSTIME == 0)	/* Display new message every */
    dmessage(j);			/* MESSTIME/UPS seconds     */

  if (!FAILED(j, SRS))			/* redraw loc window */
    DrawLocalWindow(j);

  if (udcounter % 4 == 0) {		/* update every 4 window updt */
    statline(j, j, 1);
    p = &players[j->p_playerl];
    if (ISPLOCK(j) && !(ISCLOAK(p)))
      statline(p, j, 0);
    else
      XDrawImageString(j->display, j->tstatw, j->dfgc, j->ts_offset,
		       31 + j->dfont->ascent, buf, STATLINESIZE);
  }
  if (ISSHOWSTATS(j))
    updateStats(j, j->statwin);
}


GLOBAL void     intrupt()
{

  udcounter++;
  move();

  ++stlinecount;
}
