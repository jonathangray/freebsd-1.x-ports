/* $Id: types.h,v 1.1 1994/02/23 14:40:08 jkh Exp $
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

#ifndef	TYPES_H
#define	TYPES_H

#ifdef VMS
#include <types.h>
#else
#include <sys/types.h>
#endif

#ifdef VMS
typedef char byte;
#else
typedef signed char	byte;
#endif
typedef unsigned char	u_byte;

#ifdef VMS
#if !defined(CADDR_T) && !defined(__CADDR_T) && !defined(__SOCKET_TYPEDEFS)
typedef char *caddr_t; 
#define CADDR_T
#define __CADDR_T
#endif
#ifndef __SOCKET_TYPEDEFS
typedef unsigned short  u_short;
typedef unsigned short  u_long;
typedef unsigned short  u_char;
#define __SOCKET_TYPEDEFS
#endif
#endif


#if (_SEQUENT_)
typedef unsigned short	u_short;
#endif

typedef enum {true = 1, false = 0} bool;

typedef struct { float x, y; }	vector;
typedef vector			position;
typedef struct { int x, y; }	ivec;
typedef ivec			ipos;

/*
 * Modern environments use the fd_set type as argument to select.
 * This allows a select on more than 32 file descriptors.
 * To not get the rest of the pack into trouble this macro
 * is used to prevent any integer vs. pointer clashes
 * and to allow for C++ compilation.
 */
#if defined(__linux)
#define select_t fd_set
#define select(N, R, W, E, T)	select((N),		\
	(select_t*)(R), (select_t*)(W), (select_t*)(E), (T))
#else
#define select_t int
#endif

#endif
