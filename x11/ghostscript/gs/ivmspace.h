/* Copyright (C) 1992 Aladdin Enterprises.  All rights reserved.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* ivmspace.h */
/* Local/global space management */

/*
 * Attempting to store a reference to a local object into a global object
 * must produce an invalidaccess error.  We must check this in three
 * categories of places:
 *
 *	- The scanner, when it encounters a //name inside {}.
 *
 *	- All operators that allocate ref-containing objects and also
 *	store into them:
 *		packedarray  gstate  makepattern?
 *		makefont  scalefont  definefont
 *
 *	- All operators that store refs into existing objects:
 *		put(array)  putinterval  astore  copy(to array)
 *		def  store  put(dict)  copy(dict)
 *		dictstack  execstack  makeoperator
 *		currentgstate  defineusername?
 */

/* Test whether an object is local/global. */
#define r_local(rp) (r_type_attrs(rp) & a_local)
#define r_is_local(rp) r_has_attr(rp, a_local)
#define r_is_global(rp) !r_is_local(rp)
#define check_global(rf)\
  if ( !r_is_global(&rf) ) return_error(e_invalidaccess)

/* Get the local/global attribute of the current allocator, */
/* either 0 or a_local. */
extern uint alloc_current_local(P0());

/* Select the current local or global allocator. */
/* Return the previous state. */
extern uint alloc_select_local(P1(uint));
