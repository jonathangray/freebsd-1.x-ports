/* wn_xrop.h - header file for wn_xrop.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)wn_xrop.h	1.2 4/7/91 (UKC) */

void _wn_z_rasterop PROTO((bitmap_t *sbm, int src_x, int src_y, int width, int height, bitmap_t *dbm, int dst_x, int dst_y, int ropfunc));
void _wn_xy_rasterop PROTO((bitmap_t *sbm, int src_x, int src_y, int width, int height, bitmap_t *dbm, int dst_x, int dst_y, int ropfunc));
void _wn_send_bm PROTO((bitmap_t *bm, int sx, int sy, int width, int height, Window win, GC gc, int dx, int dy, int ropfunc, int fg, int bg));
