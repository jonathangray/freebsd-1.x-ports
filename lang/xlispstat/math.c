/* math - Elementwise arithmetic functions                             */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "xlisp.h"

/* external variables */

/* external functions */
extern LVAL subr_map_elements(), recursive_subr_map_elements();

/*************************************************************************/
/*************************************************************************/
/**                                                                     **/
/**                 Recursive Vectorized Math Functions                 **/
/**                                                                     **/
/*************************************************************************/
/*************************************************************************/

/* The basic math functions have been modified to operate element-wise   */
/* on compound data. The operation is recursive: if compound data items  */
/* contain compound data items the mapping proceeds down to the next     */
/* level.                                                                */

extern LVAL xadd(), xsub(), xmul(), xdiv(), xrem(), xmod(), xmin(), xmax(),
  xexpt(), xlog();

extern LVAL xlogand(), xlogior(), xlogxor(), xlognot();

extern LVAL xabs(), xadd1(), xsub1(), xsin(), xcos(), xtan(),
  xexp(), xsqrt(), xfix(), xfloat(), xrand(), xfloor(), xceil(), xround(),
  xasin(), xacos(), xatan(), xphase();

extern LVAL xminusp(), xzerop(), xplusp(), xevenp(), xoddp(), xlss(), xleq(),
  xequ(), xneq(), xgeq(), xgtr();

LVAL xsradd()   { return (recursive_subr_map_elements(xadd, xsradd)); }
LVAL xsrsub()   { return (recursive_subr_map_elements(xsub, xsrsub)); }
LVAL xsrmul()   { return (recursive_subr_map_elements(xmul, xsrmul)); }
LVAL xsrdiv()   { return (recursive_subr_map_elements(xdiv, xsrdiv)); }
LVAL xsrrem()   { return (recursive_subr_map_elements(xrem, xsrrem)); }
LVAL xsrmod()   { return (recursive_subr_map_elements(xmod, xsrmod)); }
LVAL xsrmin()   { return (recursive_subr_map_elements(xmin, xsrmin)); }
LVAL xsrmax()   { return (recursive_subr_map_elements(xmax, xsrmax)); }
LVAL xsrexpt()  { return (recursive_subr_map_elements(xexpt, xsrexpt));}
LVAL xsrlog()   { return (recursive_subr_map_elements(xlog, xsrlog)); }

LVAL xsrlogand()   { return (recursive_subr_map_elements(xlogand, xsrlogand)); }
LVAL xsrlogior()   { return (recursive_subr_map_elements(xlogior, xsrlogior)); }
LVAL xsrlogxor()   { return (recursive_subr_map_elements(xlogxor, xsrlogxor)); }
LVAL xsrlognot()   { return (recursive_subr_map_elements(xlognot, xsrlognot)); }

LVAL xsrabs()   { return (recursive_subr_map_elements(xabs, xsrabs)); }
LVAL xsradd1()  { return (recursive_subr_map_elements(xadd1, xsradd1)); }
LVAL xsrsub1()  { return (recursive_subr_map_elements(xsub1, xsrsub1)); }
LVAL xsrsin()   { return (recursive_subr_map_elements(xsin, xsrsin)); }
LVAL xsrcos()   { return (recursive_subr_map_elements(xcos, xsrcos)); }
LVAL xsrtan()   { return (recursive_subr_map_elements(xtan, xsrtan)); }
LVAL xsrexp()   { return (recursive_subr_map_elements(xexp, xsrexp)); }
LVAL xsrsqrt()  { return (recursive_subr_map_elements(xsqrt, xsrsqrt)); }
LVAL xsrfix()   { return (recursive_subr_map_elements(xfix, xsrfix)); }
LVAL xsrfloat() { return (recursive_subr_map_elements(xfloat, xsrfloat)); }
LVAL xsrrand()  { return (recursive_subr_map_elements(xrand, xsrrand)); }
LVAL xsrfloor() { return (recursive_subr_map_elements(xfloor, xsrfloor)); }
LVAL xsrceil()  { return (recursive_subr_map_elements(xceil, xsrceil)); }
LVAL xsrround() { return (recursive_subr_map_elements(xround, xsrround)); }
LVAL xsrasin()  { return (recursive_subr_map_elements(xasin, xsrasin)); }
LVAL xsracos()  { return (recursive_subr_map_elements(xacos, xsracos)); }
LVAL xsratan()  { return (recursive_subr_map_elements(xatan, xsratan)); }
LVAL xsrphase()  { return (recursive_subr_map_elements(xphase, xsrphase)); }

LVAL xsrminusp(){ return (recursive_subr_map_elements(xminusp, xsrminusp)); }
LVAL xsrzerop() { return (recursive_subr_map_elements(xzerop, xsrzerop)); }
LVAL xsrplusp() { return (recursive_subr_map_elements(xplusp, xsrplusp)); }
LVAL xsrevenp() { return (recursive_subr_map_elements(xevenp, xsrevenp)); }
LVAL xsroddp()  { return (recursive_subr_map_elements(xoddp, xsroddp)); }

LVAL xsrlss(){ return (recursive_subr_map_elements(xlss, xsrlss)); }
LVAL xsrleq(){ return (recursive_subr_map_elements(xleq, xsrleq)); }
LVAL xsrequ(){ return (recursive_subr_map_elements(xequ, xsrequ)); }
LVAL xsrneq(){ return (recursive_subr_map_elements(xneq, xsrneq)); }
LVAL xsrgeq(){ return (recursive_subr_map_elements(xgeq, xsrgeq)); }
LVAL xsrgtr(){ return (recursive_subr_map_elements(xgtr, xsrgtr)); }

