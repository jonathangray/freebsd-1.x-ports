/********************************************************************
 * lindner
 * 3.5
 * 1993/08/23 19:37:32
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopherd/gopherdconf.h,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: gopherdconf.h
 * Header file for routines in gopherdconf.c
 *********************************************************************
 * Revision History:
 * gopherdconf.h,v
 * Revision 3.5  1993/08/23  19:37:32  lindner
 * Add a semicolon
 *
 * Revision 3.4  1993/08/23  18:46:20  lindner
 * Crude addition of a veronica top-level block
 *
 * Revision 3.3  1993/08/20  18:03:11  lindner
 * Mods to allow gopherd.conf files control ftp gateway access
 *
 * Revision 3.2  1993/03/24  20:25:46  lindner
 * Addition for secureusers file
 *
 * Revision 3.1.1.1  1993/02/11  18:02:52  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.2  1993/01/30  23:57:44  lindner
 * Added macros for Geog and Lang
 *
 * Revision 1.1  1992/12/10  23:13:27  lindner
 * gopher 1.1 release
 *
 *
 *********************************************************************/



#ifndef GOPHERDCONF_H
#define GOPHERDCONF_H

#include "boolean.h"
#include "STRstring.h"
#include "ext.h"       /** Filename extensions **/
#include "site.h"      /** Hostname/IP security **/


/**************************************************************************
 * A structure that defines all the changable parameters of a gopher server
 * Read from the file gopherd.conf
 */

struct gdconf_struct {
     ExtArray     *Extensions;
     SiteArray    *Sites;
     Accesslevel  Defaccess;       /*** Default access level for unknowns **/

     boolean      RunFromInetd;    /*** -I option **/
     boolean      Caching;         /*** -C option **/
     String       *Logfile;        /*** The filename for the logfile **/
     String       *BummerMsg;      /*** Message given to non-secure sites **/
     String       *Data_Dir;       /*** Where our data directory is **/
     String       *Hostname;       /*** A FQDN for the host **/
     int          Port;            /*** The Port we're running at **/
     boolean      chroot;          /*** Are we chroot()ing? **/

     boolean      Securityon;      /*** Are we restricting access or not? **/

     String       *Admin;          /*** Administrator info **/
     String       *AdminEmail;     /*** E-mail address of administrator **/
     
     String       *Site;           /*** Description of site **/
     String       *Org;            /*** Name of Organization **/
     String       *Loc;            /*** Location of site **/
     String       *Geog;           /*** Latitude and Longitude **/
     String       *Lang;           /*** Default language ***/

     String       *Tixfile;        /*** Tickets file ***/
     
     boolean      VeronicaIndex;   /*** Index it or not? **/
     int          TZ;              /*** Timezone **/
};

typedef struct gdconf_struct GDCobj;

#define GDCgetDefAccess(a)      ((a)->Defaccess)
#define GDCgetInetdActive(a)    ((a)->RunFromInetd)
#define GDCsetInetdActive(a,b)  ((a)->RunFromInetd=(b))
#define GDCgetCaching(a)        ((a)->Caching)

#define GDCgetLogfile(a)        STRget((a)->Logfile)
#define GDCsetLogfile(a,b)      STRset((a)->Logfile,(b))
#define GDCgetDatadir(a)        STRget((a)->Data_Dir)
#define GDCgetHostname(a)       STRget((a)->Hostname)
#define GDCsetHostname(a,b)     STRset((a)->Hostname,(b))
#define GDCgetPort(a)           ((a)->Port)
#define GDCgetchroot(a)         ((a)->chroot)
#define GDCgetBummerMsg(a)      STRget((a)->BummerMsg)
#define GDCsetBummerMsg(a,b)    STRset((a)->BummerMsg,(b))

#define GDCgetAdmin(a)       STRget((a)->Admin)
#define GDCsetAdmin(a,b)     STRset((a)->Admin,(b))
#define GDCgetAdminEmail(a)  STRget((a)->AdminEmail)
#define GDCsetAdminEmail(a,b) STRset((a)->AdminEmail,(b))
#define GDCgetSite(a)        STRget((a)->Site)
#define GDCsetSite(a,b)      STRset((a)->Site,(b))
#define GDCgetOrg(a)         STRget((a)->Org)
#define GDCsetOrg(a,b)       STRset((a)->Org,(b))
#define GDCgetLoc(a)         STRget((a)->Loc)
#define GDCsetLoc(a,b)       STRset((a)->Loc,(b))
#define GDCsetGeog(a,b)      STRset((a)->Geog,(b))
#define GDCgetGeog(a)        STRget((a)->Geog)
#define GDCgetLang(a)        STRget((a)->Lang)
#define GDCsetLang(a,b)      STRset((a)->Lang,(b))
#define GDCgetTixfile(a)        STRget((a)->Tixfile)
#define GDCsetTixfile(a,b)      STRset((a)->Tixfile,(b))
#define GDCgetShouldIndex(a)    ((a)->VeronicaIndex)
#define GDCsetShouldIndex(a,b)  ((a)->VeronicaIndex=(b))

#define GDCgetTZ(a)          (a)->TZ

/*
 * Real live functions in gopherdconf.c
 */

GDCobj       *GDCnew();
void         GDCinit(/* gdc, FILE* gdc.conf file */);
void         GDCdestroy(/* gdc */);

boolean      GDCignore(/* gdc, char *filename */);

boolean      GDCCanRead(/* gdc, hostname, ipnum */);
boolean      GDCCanBrowse(/* gdc, hostname, ipnum */);
boolean      GDCCanSearch(/* gdc, hostname, ipnum */);
boolean      GDCCanFTP(/* gdc, hostname, ipnum */);

boolean      GDCExtension(/*gdc, ext, Gtype, Prefix*/);


#endif /* GOPHERDCONF_H */
