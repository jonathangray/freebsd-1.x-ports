/********************************************************************
 * lindner
 * 3.1
 * 1993/07/07 19:27:27
 * /home/mudhoney/GopherSrc/CVS/gopher+/object/Sockets.h,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: Sockets.h
 * Socket protos etc.
 *********************************************************************
 * Revision History:
 * Sockets.h,v
 * Revision 3.1  1993/07/07  19:27:27  lindner
 * Socket functions
 *
 *
 *********************************************************************/


#ifndef SOCKETS_H
#define SOCKETS_H

void SOCKlinger();
int  SOCKbind_to_port();
int  SOCKconnect();
int  SOCKlisten();
int  SOCKaccept();

#endif  /* SOCKETS_H */
