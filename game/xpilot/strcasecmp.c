/* $Id: strcasecmp.c,v 1.1 1994/02/23 14:40:08 jkh Exp $
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

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

#ifndef	lint
static char sourceid[] =
    "@(#)$Id: strcasecmp.c,v 1.1 1994/02/23 14:40:08 jkh Exp $";
#endif

/*
 * By Ian Malcom Brown.
 * Changes by BG: prototypes with const,
 * moved the ++ expressions out of the macro.
 */
int strcasecmp(const char *str1, const char *str2)
{
    int	c1, c2;


    do {
	c1 = *str1++;
	c2 = *str2++;
	c1 = tolower(c1);
	c2 = tolower(c2);
    } while (c1 != 0 && c2 != 0 && c1 == c2);

    return (c1 - c2);
}

/*
 * By Bert Gijsbers, derived from Ian Malcom Brown's strcasecmp().
 */
int strncasecmp(const char *str1, const char *str2, size_t n)
{
    int	c1, c2;

    do {
	if (n-- <= 0) {
	    return 0;
	}
	c1 = *str1++;
	c2 = *str2++;
	c1 = tolower(c1);
	c2 = tolower(c2);
    } while (c1 != 0 && c2 != 0 && c1 == c2);

    return (c1 - c2);
}
