/********************************************************************
 * lindner
 * 3.1.1.1
 * 1993/02/11 18:02:57
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopher/HTML.h,v
 * $Status: $
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: HTML.h
 * Header file for lame attempt at HTML
 *********************************************************************
 * Revision History:
 * HTML.h,v
 * Revision 3.1.1.1  1993/02/11  18:02:57  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.1  1992/12/10  23:32:16  lindner
 * gopher 1.1 release
 *
 * Revision 1.1  1992/12/10  06:16:51  lindner
 * Initial revision
 *
 *
 *********************************************************************/

#include "GDgopherdir.h"
#include <stdio.h>

struct HTMLstruct {
     GopherDirObj *Links;

     int          *Linklinenum;      /** Line # of the link **/
     int          *Linklinepos;      /** line position of the link **/
     int          Linkmaxsize;       /** Size of Links arrays **/

     FILE         *Filehtml;
     FILE         *Filetxt;

     String       *Filehtmlname;
     String       *Filetxtname;

     String       *Title;
};

typedef struct HTMLstruct HTMLObj;

#define HTMLGetLink(a,b)  (GDgetEntry(((a)->Links),(b)))  /** Returns a GopherObj**/
#define HTMLGetLinepos(a,b)    ((a)->Linklinepos[(b)])
#define HTMLGetNumLinks(a)     (GDgetNumitems((a)->Links))
#define HTMLGetLinenum(a,b)    ((a)->Linklinenum[(b)])
#define HTMLGetLinkMax(a)      ((a)->Linkmaxsize)
#define HTMLSetLinepos(a,b,c)  ((a)->Linklinepos[(b)] = (c))
#define HTMLSetLinenum(a,b,c)  ((a)->Linklinenum[(b)] = (c))
/** procedures defined in HTML.c **/

HTMLObj *HTMLnew( /*size*/);
void     HTMLInit();
void     HTMLDestroy();
void     HTMLgrow();
void     HTMLaddLink();



