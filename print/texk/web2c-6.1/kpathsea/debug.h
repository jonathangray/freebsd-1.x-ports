/* debug.h: Runtime tracing.

Copyright (C) 1993, 94 Karl Berry.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifndef KPATHSEA_DEBUG_H
#define KPATHSEA_DEBUG_H

/* If NO_DEBUG is defined, skip all this.  */
#ifndef NO_DEBUG

/* OK, we'll have tracing support.  */
#define DEBUG

/* Bit vector defining what we should trace.  */
extern unsigned kpathsea_debug;

/* Set a bit.  */
#define DEBUG_SET(bit) do { kpathsea_debug |= 1 << (bit); } while (0)

/* Test if a bit is on.  */
#define DEBUG_P(bit) (kpathsea_debug & (1 << (bit)))

#define DEBUG_STAT 0		/* stat calls */
#define DEBUG_DB_BUILD 1	/* hash table built from ls-R */

#endif /* not NO_DEBUG */

#endif /* not KPATHSEA_DEBUG_H */
