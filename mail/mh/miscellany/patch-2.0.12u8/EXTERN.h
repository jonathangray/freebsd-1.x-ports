/* /usr/home/jtc/CVSROOT/mh/miscellany/patch-2.0.12u8/EXTERN.h,v 1.1.1.1 1993/01/30 04:40:51 jtc Exp
 *
 * EXTERN.h,v
 * Revision 1.1.1.1  1993/01/30  04:40:51  jtc
 * mh-6.8
 *
 * Revision 2.0  86/09/17  15:35:37  lwall
 * Baseline for netwide release.
 * 
 */

#ifdef EXT
#undef EXT
#endif
#define EXT extern

#ifdef INIT
#undef INIT
#endif
#define INIT(x)

#ifdef DOINIT
#undef DOINIT
#endif
