/* Copyright (C) 1989, 1990, 1993 Aladdin Enterprises.  All rights reserved.

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

/* gstypes.h */
/* Miscellaneous common types for Ghostscript library */

/* Representation of a point. */
typedef struct gs_point_s {
	double x, y;
} gs_point;
typedef struct gs_int_point_s {
	int x, y;
} gs_int_point;
/* Representation of a rectangle. */
/* Note that rectangles are half-open, i.e.: their width is */
/* q.x-p.x and their height is q.y-p.y; they include the points */
/* (x,y) such that p.x<=x<q.x and p.y<=y<q.y. */
typedef struct gs_rect_s {
	gs_point p, q;			/* origin point, corner point */
} gs_rect;
typedef struct gs_int_rect_s {
	gs_int_point p, q;
} gs_int_rect;

/* So many routines use the graphics state */
/* that we may as well declare the abstract type here. */
struct gs_state_s;
typedef struct gs_state_s gs_state;

/*
 * Types for client-supplied allocate and free procedures.
 * For accountability, debugging, and error messages,
 * we pass an identifying string to alloc and free.
 * Note that the arguments are like calloc, not like malloc,
 * but an alloc procedure doesn't clear the block.
 */
typedef char *(*gs_proc_alloc_t)(P3(unsigned num_elements, unsigned element_size, const char *client_name));
typedef void (*gs_proc_free_t)(P4(char *data, unsigned num_elements, unsigned element_size, const char *client_name));
/*
 * These procedures are never supplied directly, only as elements of
 * a memory_procs structure.  Someday the memory manager will be a real
 * 'object'....
 */
typedef struct {
	gs_proc_alloc_t alloc;
	gs_proc_free_t free;
} gs_memory_procs;
/*
 * We define our own versions of malloc and free that conform
 * to the types gs_proc_alloc_t and gs_proc_free_t:
 */
char *gs_malloc(P3(uint, uint, const char *));
void gs_free(P4(char *, uint, uint, const char *));
extern const gs_memory_procs gs_default_memory_procs;
