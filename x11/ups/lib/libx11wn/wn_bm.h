/* wn_bm.h - header file for wn_bm.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)wn_bm.h	1.2 4/7/91 (UKC) */

void _wn_define_machine_bitmap_format PROTO((void));
struct pixrect *_wn_bm_to_pixrect PROTO((bitmap_t *bm));
void _wn_set_machine_format PROTO((bitmap_t *bm, format_t *fm, int flags));
void _wn_restore_format PROTO((bitmap_t *bm, format_t *fm));

#ifdef EOF
void wn_dump_bitmap PROTO((FILE *fp, bitmap_t *bm, int x, int y, int width, int height));
void _wn_dump_bits PROTO((FILE *fp, bitmap_t *bm, int orig_x, int orig_y, int width, int height, int margin));
void _wn_dump2_bits PROTO((FILE *fp, bitmap_t *bm1, int x1, int y1, bitmap_t *bm2, int x2, int y2, int width, int height, int margin));
#endif
