/********************************************************************
 * lindner
 * 3.1
 * 1993/06/22 07:06:58
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopherd/serverutil.h,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: serverutil.h
 * declarations of utility functions
 *********************************************************************
 * Revision History:
 * serverutil.h,v
 * Revision 3.1  1993/06/22  07:06:58  lindner
 * New header file..
 *
 *
 *********************************************************************/

#ifndef G_SERVERUTIL_H
#define G_SERVERUTIL_H

void    GplusError( /* sockfd, errclass, text, moretext */);
void    Abortoutput( /* sockfd, errmsg */);
boolean Setuid_username( /*username */);
void    inet_netnames(/* sockfd, host_name, ipnum*/);
int     is_mail_from_line( /* line */);
char    *mtm_basename( /* string */);
boolean Cachetimedout();
boolean isadir();

#endif
