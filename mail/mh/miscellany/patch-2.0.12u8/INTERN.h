/* /usr/home/jtc/CVSROOT/mh/miscellany/patch-2.0.12u8/INTERN.h,v 1.1.1.1 1993/01/30 04:40:51 jtc Exp
 *
 * INTERN.h,v
 * Revision 1.1.1.1  1993/01/30  04:40:51  jtc
 * mh-6.8
 *
 * Revision 2.0  86/09/17  15:35:58  lwall
 * Baseline for netwide release.
 * 
 */

#ifdef EXT
#undef EXT
#endif
#define EXT

#ifdef INIT
#undef INIT
#endif
#define INIT(x) = x

#define DOINIT
