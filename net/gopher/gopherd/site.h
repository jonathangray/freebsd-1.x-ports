/********************************************************************
 * lindner
 * 3.2
 * 1993/08/20 18:03:17
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopherd/site.h,v
 * $Status: $
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: site.h
 * Main functions for the gopher client
 *********************************************************************
 * Revision History:
 * site.h,v
 * Revision 3.2  1993/08/20  18:03:17  lindner
 * Mods to allow gopherd.conf files control ftp gateway access
 *
 * Revision 3.1.1.1  1993/02/11  18:02:53  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.1  1992/12/10  23:13:27  lindner
 * gopher 1.1 release
 *
 *
 *********************************************************************/

/*
 * Definitions for security and hostname access
 */

#ifndef SITE_H
#define SITE_H

#include "STRstring.h"
#include "boolean.h"
#include "DAarray.h"

typedef int Accesslevel;
#define ACC_BROWSE (1<<0)
#define ACC_READ   (1<<1)
#define ACC_SEARCH (1<<2)
#define ACC_FTP    (1<<3)
#define ACC_UNKNOWN (-1)

#define ACC_FULL (ACC_BROWSE | ACC_READ | ACC_SEARCH | ACC_FTP)

struct Site_struct {
     String       *domain;
     Accesslevel  Level;
     boolean      isnum;
};

typedef struct Site_struct Site;
typedef DynArray SiteArray;

#define SiteArrgetEntry(a,b) (Site *)(DAgetEntry((DynArray*)a,b))
#define SiteArrDestroy(a) (DAdestroy(a))
#define SiteArrPush(a,b) (DApush((a),(b)))
#endif

/**  Functions declared in site.c **/
SiteArray       *SiteArrayNew();
void            SiteArrayAdd( /* sitearr, name, Level */);
Accesslevel     SiteAccess(/* sitearr, name */);

boolean         SiteArrCanRead(/* */);
boolean         SiteArrCanBrowse(/* */);
boolean         SiteArrCanSearch(/* */);

boolean         SiteProcessLine(/* sitearr, inputline, DefAccess */);
