/* $Id: dbuff.h,v 1.1 1994/02/23 14:40:05 jkh Exp $
 *
 * XPilot, a multiplayer gravity war game.  Copyright (C) 1991-93 by
 *
 *      Bjørn Stabell        (bjoerns@staff.cs.uit.no)
 *      Ken Ronny Schouten   (kenrsc@stud.cs.uit.no)
 *      Bert Gÿsbers         (bert@mc.bio.uva.nl)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifndef	DBUFF_H
#define	DBUFF_H

#include <X11/Xlib.h>

#ifdef SPARC_CMAP_HACK
# ifdef sparc
#  if defined(SVR4) || defined(__svr4__)
#   include <sys/fbio.h>
#  else
#   include <sun/fbio.h>
#  endif
# else
#  undef SPARC_CMAP_HACK
# endif
#endif

typedef enum { PIXMAP_COPY, COLOR_SWITCH } dbuff_t;

typedef struct {
    Display		*display;
    dbuff_t		type;
    Colormap		cmap;
    unsigned long	drawing_planes;
    int			buffer;
    XColor		*colormaps[2];
    int			map_size;
    unsigned long	masks[2];
    unsigned long	*planes;
    unsigned long	pixel;
#ifdef SPARC_CMAP_HACK
    int			fbfd;
    struct fbcmap	hardcmap;
#endif
} dbuff_state_t;

dbuff_state_t *start_dbuff(Display *display, Colormap cmap,
			   dbuff_t type,
			   unsigned long planes, XColor *colors);
void dbuff_switch(dbuff_state_t *state);
void end_dbuff(dbuff_state_t *state);

#endif
