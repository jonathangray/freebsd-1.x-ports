/********************************************************************
 * lindner
 * 3.3
 * 1993/07/27 00:30:13
 * /home/mudhoney/GopherSrc/CVS/gopher+/object/STAarray.h,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: STAarray.h
 * Header file, abstraction of a dynamic Strings array.
 *********************************************************************
 * Revision History:
 * STAarray.h,v
 * Revision 3.3  1993/07/27  00:30:13  lindner
 * plus patch from Mitra
 *
 * Revision 3.2  1993/03/26  19:51:05  lindner
 * Added STApop
 *
 * Revision 3.1.1.1  1993/02/11  18:03:07  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.1  1993/01/31  00:34:40  lindner
 * Initial revision
 *
 * Revision 1.1  1992/12/10  23:27:52  lindner
 * gopher 1.1 release
 *
 *
 *********************************************************************/

#ifndef STAARRAY_H
#define STAARRAY_H

#include "STRstring.h"
#include "DAarray.h"

typedef DynArray StrArray;
#define STAnew(a)        (DAnew((a),STRnew,STRinit,STRdestroy,STRcpy))
#define STAinit(a)       (DAinit((a)))
#define STAgetTop(a)     (DAgetTop(a))
#define STAgetEntry(a,b) (String*)(DAgetEntry(a,b))
/* Some places use STAgetEntry and then pretend they got a char* back)*/
#define STAgetText(a,b)	 (char*)(STRget(STAgetEntry(a,b)))
#define STApush(a,b)     (DApush((DynArray*)(a),(b)))
#define STApop(a)        (String*)(DApop(a))
#define STAdestroy(a)    (DAdestroy(a))
#define STAcpy(a,b)      (DAcpy(a,b))


#endif
