/* mhole.h - header file for the mousehole package mhole.c*/

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)mhole.h	1.2 4/7/91 (UKC) */

void draw_mousehole PROTO((int wn, int x, int y, int width, int height));
void mhdraw PROTO((const char **caps));
