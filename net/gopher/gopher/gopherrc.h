/********************************************************************
 * lindner
 * 3.2
 * 1993/02/19 21:10:54
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopher/gopherrc.h,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: gopherrc.h
 * Abstraction and macros for stuff in gopherrc.c
 *********************************************************************
 * Revision History:
 * gopherrc.h,v
 * Revision 3.2  1993/02/19  21:10:54  lindner
 * Updated declarations
 *
 * Revision 3.1.1.1  1993/02/11  18:03:00  lindner
 * Gopher+1.2beta release
 *
 * Revision 2.1  1993/02/09  22:35:54  lindner
 * initial rev.
 *
 *
 *
 *********************************************************************/

#ifndef GOPHERRC_H
#define GOPHERRC_H

#include "String.h"
#include "DAarray.h"
#include "GDgopherdir.h"



/*
 * stuff to map from a gopher+ view attribute to a command
 */

struct gopherrcmap_struct {
     String *view;
     
     String *displaycmd;
     String *printcmd;
};

typedef struct gopherrcmap_struct RCMapObj;

/** Macros and external functions ***/
#define   RCMgetView(a)         (STRget((a)->view))
#define   RCMgetDisplaycmd(a)   (STRget((a)->displaycmd))
#define   RCMgetPrintcmd(a)     (STRget((a)->printcmd))

#define   RCMsetView(a,b)       (STRset((a)->view,b))
#define   RCMsetDisplaycmd(a,b) (STRset((a)->displaycmd,b))
#define   RCMsetPrintcmd(a,b)   (STRset((a)->printcmd,b))

RCMapObj *RCMnew();
void     RCMdestroy();
void     RCMinit();
void     RCMdestroy();

/** Dynamic Array of the above... **/
/*** Definitions for an Array of extension objects ***/

typedef DynArray RCMAarray;
#define RCMAnew()            (RCMAarray *)(DAnew(20,RCMnew,NULL,RCMdestroy,RCMcpy))
#define RCMAdestroy(a)       (DAdestroy(a))
#define RCMAgetEntry(a,b)    (RCMapObj *)(DAgetEntry((DynArray*)a,b))
#define RCMAadd(a,b)         (DApush(a,b))
#define RCMAgetNumEntries(a) (DAgetTop(a))

int     RCMAviewSearch();
boolean RCMAfromLine();
void    RCMAtoFile();


/*
 * General gopherrc stuff.
 */

struct RC_struct {
     RCMAarray    *commands;
     GopherDirObj *Bookmarks;

     boolean      ChangedDefs;
};


typedef struct RC_struct RCobj;

#define RCgetBookmarkDir(a)    ((a)->Bookmarks)
#define RCsetBookmarkDir(a,b)  ((a)->Bookmarks=(b))
#define RCinitBookmarkDir(a,b) ((a)->Bookmarks=GDnew(b))
RCobj  *RCnew();
void    RCsetdefs();
void    RCfromFile();
void    RCtoFile();

#endif
