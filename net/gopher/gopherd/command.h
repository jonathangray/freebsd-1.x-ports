/********************************************************************
 * lindner
 * 3.4
 * 1993/07/23 03:11:22
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopherd/command.h,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992, 1993 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: command.h
 * Definitions and prototypes for CMD object.
 *********************************************************************
 * Revision History:
 * command.h,v
 * Revision 3.4  1993/07/23  03:11:22  lindner
 * Added CMDgetFile() fcn
 *
 * Revision 3.3  1993/04/09  16:50:26  lindner
 * nothing
 *
 * Revision 3.2  1993/03/24  20:18:50  lindner
 * Fixed bug in macro declaration
 *
 * Revision 3.1  1993/03/19  19:56:56  lindner
 * New CMD object
 *
 *
 *********************************************************************/


#ifndef COMMAND_H
#define COMMAND_H

#include "STRstring.h"
#include "tix.h"
#include "boolean.h"


struct command_struct {
     String  *datafromnet;

     boolean isGplus;

     char    *selstr;
     char    *command;
     char    *search;
     
     String  *secureuser;
     String  *ticket;

     String  *askfile;
};

typedef struct command_struct CMDobj;


#define CMDgetSelstr(a)    ((a)->selstr)
#define CMDsetSelstr(a,b)  ((a)->selstr=(b))
#define CMDgetCommand(a)   ((a)->command)
#define CMDsetCommand(a,b) ((a)->command=(b))

#define CMDgetSearch(a)    ((a)->search)
#define CMDsetSearch(a,b)  ((a)->search=(b))

#define CMDgetData(a)      (STRget((a)->datafromnet))
#define CMDsetData(a,b)    (STRset((a)->datafromnet, b))

#define CMDgetAskfile(a)   (STRget((a)->askfile))
#define CMDsetAskfile(a,b) (STRset((a)->askfile,(b)))

#define CMDisGplus(a)      ((a)->isGplus)
#define CMDsetGplus(a,b)   ((a)->isGplus=b)

#define CMDgetUser(a)      (STRget((a)->secureuser))
#define CMDsetUser(a,b)    (STRset((a)->secureuser,(b)))

#define CMDgetTicket(a)    (STRget((a)->ticket))
#define CMDsetTicket(a,b)  (STRset((a)->ticket,(b)))

CMDobj *CMDnew();
void    CMDdestroy();
void    CMDfromNet();
void    CMDgetXtra();
char   *CMDticketfromLine();
char   *CMDgetView();
char   *CMDgetFile();
#endif
