/*
 * Support drawing routines for \special commands generated
 * by epsf.sty and epsf.tex, e.g.,
 *   psfile=/u11/doug/foo.ps llx=165 lly=346 urx=461 ury=434 rwi=2663
 *
 * The approach is to fork ghostscript (GNU's PostScript
 * interpreter) and have it make a bitmap.
 *
 * Doug Bryan, dbryan@stanford.edu, January 1993.
 * No Copyright.  Public domain for all I care.  Offered as-is, as-always.
 *
 */

void epsfile(
#if NeedFunctionPrototypes
    char *  /* the special command to execute */
#endif
);

void psfig_setup(
#if NeedFunctionPrototypes
    char *  /* the special command to execute */
#endif
);

void psfig(
#if NeedFunctionPrototypes
    char *  /* the special command to execute */
#endif
);

/*
 * These are the routines called by tpic.c to handle specials of the form
 *      psfile=...
 */

void flush_gsbm_cache();
/*
 * The bitmaps for the PostScript specials are cached, so we don't  
 * re-execute ghostscript so often.  flush_gsbm_cache() frees the
 * cache.  Probably should only be called when changing dvi files.
 * Right now it's never called since I can't find where xdvi reads new
 * dvi files.  (I don't flush when rereading same dvi files, since the
 * likelyhood of figures/speicals changing is small.)
 */

void flush_one_gsbm();
/*
 * Free one cached ghostscript bitmap.
 */

extern void psfig_setup_angle ();
