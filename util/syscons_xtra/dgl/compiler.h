/*
 *  Copyright (C) 1991 By DeepCore Technologies
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 1, or any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *      DeepCore Technologies
 *	Att: Søren Schmidt 	Email:	sos@kmd-ac.dk
 *	Tritonvej 36		UUCP:	...uunet!dkuug!kmd-ac!sos
 *	DK9210 Aalborg SO	Phone:  +45 9814 8076
 */

#ifdef __GNUC__

static __inline__ void
outb(port, val)
     short port;
     char val;
{
  __asm__ volatile("out%B0 %0,%1" : :"a" (val), "d" (port));
}

static __inline__ void
outw(port, val)
     short port;
     short val;
{
  __asm__ volatile("out%W0 %0,%1" : :"a" (val), "d" (port));
}

static __inline__ unsigned int
inb(port)
     short port;
{
  unsigned int ret;
  __asm__ volatile("in%B0 %1,%0" : "=a" (ret) : "d" (port));
  return ret;
}

static __inline__ unsigned int
inw(port)
     short port;
{
  unsigned int ret;
  __asm__ volatile("in%W0 %1,%0" :
		   "=a" (ret) :
		   "d" (port));
  return ret;
}
#endif 


