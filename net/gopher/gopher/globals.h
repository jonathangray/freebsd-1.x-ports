/********************************************************************
 * lindner
 * 3.11
 * 1993/08/16 18:19:32
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopher/globals.h,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: globals.h
 * Global variables and #defines
 *********************************************************************
 * Revision History:
 * globals.h,v
 * Revision 3.11  1993/08/16  18:19:32  lindner
 * VMS special interrupt and subprocess handling vars
 *
 * Revision 3.10  1993/08/16  17:58:19  lindner
 * Removed REMOTEUSER ifdefs
 *
 * Revision 3.9  1993/08/03  20:48:21  lindner
 * Audio file fix from jqj
 *
 * Revision 3.8  1993/08/03  20:24:16  lindner
 * Bigger Better Badder Options, inspired by jqj
 *
 * Revision 3.7  1993/07/27  05:28:46  lindner
 * Mondo Debug overhaul from Mitra
 *
 * Revision 3.6  1993/07/23  04:36:59  lindner
 * Added a Jmpenv variable for longjmp
 *
 * Revision 3.5  1993/07/20  23:11:56  lindner
 * Got rid of versionline
 *
 * Revision 3.4  1993/06/08  06:33:07  lindner
 * Updated version number
 *
 * Revision 3.3  1993/04/15  21:28:27  lindner
 * Mods for remote access
 *
 * Revision 3.2  1993/03/24  16:57:51  lindner
 * New version#
 *
 * Revision 3.1.1.1  1993/02/11  18:02:57  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.5  1993/02/09  22:09:28  lindner
 * Added more fcns and upped version #
 *
 * Revision 1.4  1993/01/21  03:29:48  lindner
 * Updated version number for 1.12
 *
 * Revision 1.3  1993/01/08  19:44:21  lindner
 * Updated version number
 *
 * Revision 1.2  1992/12/31  04:04:51  lindner
 * Changes for VMS
 *
 * Revision 1.1  1992/12/10  23:32:16  lindner
 * gopher 1.1 release
 *
 *********************************************************************/

#include <setjmp.h>

#define MAXRESP      20                  /* Max size of a response list*/
#define WHOLELINE 80                   /* Used in ourutil.c */

/*
 * These are some funky defines that assures that global variables are
 * declared only once.  (when globals.c includes this file with EXTERN
 * defined.
 */

#ifndef EXTERN
#define EXTERN extern
#define INIT(x)
#else
#define EXTERN
#define INIT(x) =(x)
#endif

/*** Global variables ***/

EXTERN RCobj     *GlobalRC;

EXTERN boolean   ChangedDefs INIT(FALSE);

EXTERN char      *Searchstring INIT(NULL);
EXTERN int       iLevel INIT(0);
EXTERN BOOLEAN   SecureMode INIT(FALSE);
EXTERN BOOLEAN	 NoShellMode INIT(FALSE);
EXTERN BOOLEAN   RemoteUser INIT(FALSE);

EXTERN GopherDirObj *CurrentDir INIT(NULL);
EXTERN GopherDirObj *OldDirs[30];  /** Should be a stack... **/

EXTERN GopherDirObj *BookmarkDir INIT(NULL);

EXTERN char      USERCAP[WHOLELINE];    /* The validated user capability */
EXTERN int       SOUNDCHILD INIT(0);     /* The pid of the sound player child. */
EXTERN CursesObj *CursesScreen;

EXTERN char *Gopenfile INIT(NULL);

EXTERN jmp_buf Jmpenv;

#ifndef VMS
extern int errno;
#endif

/*** Externals ***/

#ifndef VMS
extern char **environ;                  /* User environment array */
extern char *sys_errlist[];
#endif

/*** VMS needs special interrupt and subprocess handling ***/

#ifdef VMS
void (*VMSsignal(/* int, void (*) () */)) (int);
#define signal(a,b) VMSsignal(a,b)
EXTERN boolean HadVMSInt INIT(FALSE);
int DCLsystem(/* char* */);
#define system(a) DCLsystem(a)
#endif

/*** Prototypes and forward declarations ***/

/*** Ourutils.c ***/
void display_file(/* char *Filename */);
void ZapCRLF( /* char *buffer */ );
int  outchar( /*char c*/ );        
void CursesErrorMsg( /* char* */);
void GetOneOption(/* char*, char* */);

int  process_request(/* ZeGopher*/);
int  Load_Dir(/*ZeGopher*/);
int  Load_Index();
int  Load_Index_or_Dir();
void GetOneOption(/* */);
void check_sock(/* int, char* */);
char *Choose_View(/* gs */);
char **AskBlock(/* gs */);


