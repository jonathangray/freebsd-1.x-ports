/********************************************************************
 * lindner
 * 3.3
 * 1993/07/23 04:49:07
 * /home/mudhoney/GopherSrc/CVS/gopher+/object/VIews.h,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: VIews.h
 * Header file, abstraction of a gopher+ view.
 *********************************************************************
 * Revision History:
 * VIews.h,v
 * Revision 3.3  1993/07/23  04:49:07  lindner
 * Added PrettyView fcn
 *
 * Revision 3.2  1993/02/19  21:33:24  lindner
 * Gopher1.2b2 release
 *
 * Revision 3.1.1.1  1993/02/11  18:03:07  lindner
 * Gopher+1.2beta release
 *
 * Revision 2.1  1993/02/09  22:48:34  lindner
 * Changes for multilingual views
 *
 * Revision 1.1  1993/01/31  00:34:40  lindner
 * Initial revision
 *
 *
 *********************************************************************/

#ifndef VIEWS_H
#define VIEWS_H


#include "STRstring.h"
#include "boolean.h"

struct view_struct 
{
     String *Type;
     String *Lang;
     String *Size;
     String *comments;
};

typedef struct view_struct VIewobj;

#define VIgetType(a)     ((STRget((a)->Type)))
#define VIgetLang(a)     ((STRget((a)->Lang)))
#define VIgetSize(a)     ((STRget((a)->Size)))
#define VIgetComments(a) ((STRget((a)->comments)))
     
#define VIsetType(a,b)     (STRset((a)->Type,(b)))
#define VIsetLang(a,b)     (STRset((a)->Lang,(b)))
#define VIsetSize(a,b)     (STRset((a)->Size,(b)))
#define VIsetComments(a,b) (STRset((a)->comments,(b)))

/** Real view functions in VIews.c **/
VIewobj *VInew();
void     VIinit();
void     VIcpy();
void     VIdestroy();
void     VItoLine();
boolean  VIfromLine();
char    *VIgetViewnLang();
char    *VIgetPrettyView();
char    *VIprettyLang();

/*******************************************************/
/** This stuff is for a dynamic array of VIewobjs     **/

#include "DAarray.h"

typedef DynArray VIewArray;
#define VIAnew(a)        (DAnew((a),VInew,VIinit,VIdestroy,VIcpy))
#define VIAinit(a)       (DAinit((a)))
#define VIAgetTop(a)     (DAgetTop(a))
#define VIAgetEntry(a,b) (VIewobj*)(DAgetEntry(a,b))
#define VIApush(a,b)     (DApush((DynArray*)(a),(b)))
#define VIAdestroy(a)    (DAdestroy(a))
#define VIAcpy(a,b)      (DAcpy(a,b))

#endif
