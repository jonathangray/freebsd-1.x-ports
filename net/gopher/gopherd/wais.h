/********************************************************************
 * lindner
 * 3.1.1.1
 * 1993/02/11 18:02:53
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopherd/wais.h,v
 * $Status: $
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: wais.h
 * Header file for Wais stuff.
 *********************************************************************
 * Revision History:
 * wais.h,v
 * Revision 3.1.1.1  1993/02/11  18:02:53  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.1  1992/12/10  23:13:27  lindner
 * gopher 1.1 release
 *
 *
 *********************************************************************/




/* WIDE AREA INFORMATION SERVER SOFTWARE:
   No guarantees or restrictions.  See the readme file for the full standard
   disclaimer.

   This is part of the shell user-interface for the WAIS software.  Do with it
   as you please.

   jonathan@Think.COM

 * wais.h,v
 * Revision 3.1.1.1  1993/02/11  18:02:53  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.1  1992/12/10  23:13:27  lindner
 * gopher 1.1 release
 *
 * Revision 1.6  92/02/15  19:50:24  jonathan
 * Removed old cruft.  Added $Log for RCS
 * 
*/

#ifndef _H_WAIS
#define _H_WAIS

/* usefull definitions */

#define STRINGSIZE	256

#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>

/* from the IR directory */

#include <cutil.h>
/*#include <irdirent.h>*/
#include <ui.h>
#include <irfileio.h>
#include <sockets.h>
FILE *connect_to_server _AP((char* host_name,long port));

/* for this application */

#include "../ui/list.h"
#include "source.h"
#include "document.h"
#include "../ui/util.h"
#include "question.h"
#define CHARS_PER_PAGE 2000

#ifdef MAIN
#define ext
#else
#define ext extern
#endif

ext char* command_name;

ext int numtosave;

ext int NumQuestions;

ext Question the_Question;

ext SList Sources;
ext int NumSources;

ext int maxDocs;

#endif
