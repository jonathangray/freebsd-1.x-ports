/* cursors.h - defines for cursor numbers (machine generated) */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)cursors.h	1.5 4/7/91 (UKC) */

/*  These #defines index an array - don't change them without changing
 *  the coressponding stuff in cursors.c.
 */
#define CU_DEAD		0
#define CU_DISPLAY	1
#define CU_DRAG_BOX	2
#define CU_MENU		3
#define CU_SBAR		4
#define CU_SC_PRESSED	5
#define CU_SCROLL_DOWN	6
#define CU_SCROLL_UP	7
#define CU_WAIT		8
#define CU_LINE		9
#define CU_DRAGBOX	10

void set_bm_cursor PROTO((int wn, int cno));
