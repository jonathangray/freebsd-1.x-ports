/********************************************************************
 * lindner
 * 3.6
 * 1993/08/04 22:12:37
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopherd/globals.h,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: globals.h
 * Global variables for gopher server
 *********************************************************************
 * Revision History:
 * globals.h,v
 * Revision 3.6  1993/08/04  22:12:37  lindner
 * Mods to use Gpopen
 *
 * Revision 3.5  1993/07/27  05:27:44  lindner
 * Mondo Debug overhaul from Mitra
 *
 * Revision 3.4  1993/07/26  17:23:43  lindner
 * Mods for send_binary
 *
 * Revision 3.3  1993/07/20  23:56:21  lindner
 * Added Argv vars, peername, etc.
 *
 * Revision 3.2  1993/07/07  19:38:01  lindner
 * none
 *
 * Revision 3.1.1.1  1993/02/11  18:02:51  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.3  1993/01/30  23:57:44  lindner
 * New global "ASKfile
 *
 * Revision 1.2  1992/12/16  05:07:23  lindner
 * Removed SortDir, moved it into listdir() where it belongs.
 *
 * Revision 1.1  1992/12/10  23:13:27  lindner
 * gopher 1.1 release
 *
 *
 *********************************************************************/

/*
 * This is some funky defines that assures that global variables are
 * declared only once.  (when globals.c includes this file with EXTERN
 * defined.
 */

#ifndef EXTERN
#define EXTERN extern
#define INIT(x)
#else
#define EXTERN
#define INIT(x) = (x)
#endif

#include "boolean.h"

/**** Defines ****/
#define MAXLINE 512

/**** Globals.  ****/

EXTERN GDCobj    *Config;
EXTERN boolean   RunFromInetd INIT(FALSE);
EXTERN boolean   Caching INIT(TRUE);
EXTERN boolean   UsingHTML INIT(FALSE);
EXTERN int       LOGFileDesc INIT(-1);
EXTERN char      Data_Dir[256];
EXTERN char      *pname INIT(NULL);

EXTERN int       dochroot INIT(TRUE);   /*** Should we use chroot?? ***/
EXTERN char      *Zehostname INIT(NULL);      /** Holds name and domain **/
EXTERN int       GopherPort INIT(GOPHER_PORT);
EXTERN char      *EXECargs INIT(NULL);   /** used with exec type **/
EXTERN boolean   MacIndex      INIT(FALSE);

/*** What's being run ***/
EXTERN boolean   RunLS         INIT(FALSE);
EXTERN boolean   RunServer     INIT(TRUE);  /** Run server as default **/
EXTERN boolean   RunIndex      INIT(FALSE); /** Index server (gindexd) **/

EXTERN char **   Argv;
EXTERN char *   LastArgv;

/*** Incoming data from the client ***/
EXTERN char *ASKfile           INIT(NULL);

/*** Who's querying us.. ***/
EXTERN char    CurrentPeerName[256];
EXTERN char    CurrentPeerIP[16];


/*** Prototypes n' externals ****/

extern char *parse_input();
extern int do_command();
void intro_mesg(/* int */);
void listdir();
void printfile();
void send_binary();
FILE *specialfile();
char *fixfile();
FILE *Gpopen();

/*** From index.c ***/
void NeXTIndexQuery();
void WaisIndexQuery();
void GrepIndexQuery();
void ShellIndexQuery();

#include "serverutil.h"

/*** from ftp.c ***/
void TranslateResults();
int SendFtpQuery();
