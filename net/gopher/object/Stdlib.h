/********************************************************************
 * lindner
 * 3.3
 * 1993/07/07 19:28:04
 * /home/mudhoney/GopherSrc/CVS/gopher+/object/Stdlib.h,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: Stdlib.h
 * Include stdlib.h on systems that have it.
 *********************************************************************
 * Revision History:
 * Stdlib.h,v
 * Revision 3.3  1993/07/07  19:28:04  lindner
 * Mods for SGIs
 *
 * Revision 3.2  1993/06/15  06:07:16  lindner
 * Mods for VMS
 *
 * Revision 3.1.1.1  1993/02/11  18:03:04  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.1  1992/12/10  23:27:52  lindner
 * gopher 1.1 release
 *
 *
 *********************************************************************/


#ifndef  GSTDLIB_H
#define  GSTDLIB_H

/**  These definitely need stdlib.h **/
#if defined (NeXT) || defined(__STDC__) || defined(VMS) ||defined(sgi)
#  ifndef sony_news   /** Ack! sonynews is STDC but no stdlib! **/
#    include <stdlib.h>
#  endif
#else

/** These definitely *don't* want stdlib.h **/
#  if !defined(mips) && !defined(sequent) && !defined(n16) && !defined(NeXT) &&      !defined(ultrix) && !defined(UMAX43) && !defined(sony_news)

#     include <stdlib.h>
#  endif
#endif


#endif /* GSTDLIB_H */
