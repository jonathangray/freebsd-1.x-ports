 /*
 * Copyright 1989 Jon Bennett, Mike Bolotski, David Gagne, Dan Lovinger
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of the copyright holders not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.  The copyright holders make no
 * representations about the suitability of this software for any purpose.
 * It is provided "as is" without express or implied warranty.
 *
 * THE COPYRIGHT HOLDERS DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 */

/*
 * This file defines macros to convert to and from polar coordinates.
 * Directions are represented with 0 as north, 64 as east, 128 as south, and
 * 192 as west. Right now they are slow.  This will be improved later.
 */

/*
#define POLAR_DISTANCE(X, Y) \
	   ((int) hypot ((double) (X), (double) (Y)))
*/

#ifndef ABS
#define ABS(x) (((x)<0)?-(x):(x))
#endif

#define POLAR_DISTANCE(X, Y) \
	   ((int) rhypot(ABS((long)X), ABS((long)Y)))

/*
#define POLAR_DIRECTION(X, Y) \
  (128 - (int) (atan2 ((double) (X), (double) (Y)) \
  * 40.74366543152520595683424342336368)) 
 */

#define POLAR_DIRECTION(X,Y)\
ratan((long)X,(long)Y) 

/* #define POLAR_X(DIST, DIR) \
         ((int) (sin ((DIR) * 0.02454369260617025967548940143187) * (DIST)))
#define POLAR_Y(DIST, DIR) \
(-(int) (cos ((DIR) * 0.02454369260617025967548940143187) * (DIST)))
*/ 
/*

#define POLAR_SINE(x) (sin((x) * 0.02454369260617025967548940143187))
 */

#define POLAR_SINE(x) (Sine[x])
/* */
#define POLAR_Y(DIST,DIR) (-(int)(POLAR_SINE((DIR)+64)*(DIST)))
#define POLAR_X(DIST,DIR) ((int)(POLAR_SINE(DIR)*(DIST)))

#define POLAR_ARCSINE(x) ((x<0)?-asintable[-(int)(x*2048)]:asintable[(int)(x*2048)])

/*
#define POLAR_ARCSINE(x) ((int) (asin(x) * 40.74366543152520595683424342336368))
*/
