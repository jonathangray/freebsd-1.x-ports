/* Copyright (C) 1992, 1993 Aladdin Enterprises.  All rights reserved.

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

/* gxrefct.h */
/* Definitions for reference-counted data structures */

#ifndef gxrefct_INCLUDED
#  define gxrefct_INCLUDED

/*
 * A reference-counted object must include the following header:
 *	rc_header rc;
 */
typedef struct rc_header_s {
	long ref_count;
	uint size;
} rc_header;

/* ------ Allocate/free ------ */

#define rc_alloc_n(vp, stype, ssize, mprocs, errstat, cname, rcinit)\
   {	if ( (vp = (stype *)(*(mprocs)->alloc)(1, ssize, cname)) == 0 )\
	   errstat;\
	vp->rc.ref_count = rcinit;\
	vp->rc.size = ssize;\
   }
#define rc_alloc_0(vp, stype, ssize, mprocs, errstat, cname)\
  rc_alloc_n(vp, stype, ssize, mprocs, errstat, cname, 0)
#define rc_alloc_1(vp, stype, ssize, mprocs, errstat, cname)\
  rc_alloc_n(vp, stype, ssize, mprocs, errstat, cname, 1)

#define rc_alloc_struct_0(vp, stype, mprocs, errstat, cname)\
  rc_alloc_0(vp, stype, sizeof(stype), mprocs, errstat, cname)
#define rc_alloc_struct_1(vp, stype, mprocs, errstat, cname)\
  rc_alloc_1(vp, stype, sizeof(stype), mprocs, errstat, cname)
#define rc_alloc_struct_n(vp, stype, mprocs, errstat, cname, rcinit)\
  rc_alloc_n(vp, stype, sizeof(stype), mprocs, errstat, cname, rcinit)

#define rc_free(vp, mprocs, cname)\
  (*(mprocs)->free)((char *)vp, 1, (vp)->rc.size, cname)

/* ------ Reference counting ------ */

/* Increment a reference count. */
#define rc_increment(vp)\
  if ( vp != 0 ) vp->rc.ref_count++

/* Increment a reference count, allocating the structure if necessary. */
#define rc_allocate(vp, stype, ssize, mprocs, errstat, cname)\
  if ( vp != 0 )\
	vp->rc.ref_count++;\
  else\
   {	rc_alloc_1(vp, stype, ssize, mprocs, errstat, cname);\
   }
#define rc_allocate_struct(vp, stype, mprocs, errstat, cname)\
  rc_allocate(vp, stype, sizeof(stype), mprocs, errstat, cname)

/* Guarantee that a structure is not shared. */
#define rc_unshare(vp, stype, mprocs, errstat, cname)\
  if ( vp == 0 || vp->rc.ref_count > 1 )\
   {	stype *new;\
	rc_alloc_struct_1(new, stype, mprocs, errstat, cname);\
	if ( vp ) vp->rc.ref_count--;\
	vp = new;\
   }

/* Decrement a reference count, freeing the structure if necessary. */
#define rc_decrement(vp, mprocs, cname)\
  if ( vp != 0 && !--(vp->rc.ref_count) )\
   {	rc_free(vp, mprocs, cname);\
	vp = 0;\
   }

/* Adjust a reference count either up or down. */
#define rc_adjust(vp, delta, mprocs, cname)\
  if ( vp != 0 && !(vp->rc.ref_count += delta) )\
   {	rc_free(vp, mprocs, cname);\
	vp = 0;\
   }

/* Assign a pointer, adjusting reference counts. */
#define rc_assign(vpto, vpfrom, mprocs, cname)\
  if ( vpto != vpfrom )\
   {	rc_decrement(vpto, mprocs, cname);\
	vpto = vpfrom;\
	rc_increment(vpto);\
   }

#endif					/* gxrefct_INCLUDED */
