/* $Id: version.h,v 1.1 1994/02/23 14:40:08 jkh Exp $
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

#ifndef	VERSION_H
#define	VERSION_H

/*
 * Update these for each new released version.
 *
 * IMPORTANT!!!!!
 *
 * If you're working on an unofficial version, please change the
 * VERSION_STATUS field to avoid confusion.  Only the OFFICIAL
 * version can have a VERSION_STATUS field that is empty or contains
 * the word "beta" or "alpha".
 */
#define VERSION_MAJOR		"3"
#define VERSION_MINOR		"0"
#define VERSION_PATCHLEVEL	"7"
#define VERSION_STATUS		""

/*
 * Don't change these
 */
#define VERSION		\
	VERSION_MAJOR "." VERSION_MINOR "." VERSION_PATCHLEVEL VERSION_STATUS
#define TITLE		"XPilot " VERSION
#define AUTHORS		"Bjørn Stabell, Ken Ronny Schouten & Bert Gÿsbers"

#if defined(__hpux)
#   pragma COPYRIGHT_DATE	"1991-93"
#   pragma COPYRIGHT		AUTHORS
#   pragma VERSIONID		TITLE
#endif

#define COPYRIGHT	\
	"© 1991-93 by Bjørn Stabell, Ken Ronny Schouten & Bert Gÿsbers" 
#endif
