/********************************************************************
 * lindner
 * 3.13
 * 1993/08/16 18:04:15
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopher/gopherrc.c,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: gopherrc.c
 * Utilities to read and write the .gopherrc file 
 *********************************************************************
 * Revision History:
 * gopherrc.c,v
 * Revision 3.13  1993/08/16  18:04:15  lindner
 * Add a purge to gopherrc on VMS
 *
 * Revision 3.12  1993/08/16  17:58:22  lindner
 * Removed REMOTEUSER ifdefs
 *
 * Revision 3.11  1993/08/03  20:00:00  lindner
 * Change audio/mulaw to audio/basic for MIME
 *
 * Revision 3.10  1993/07/29  17:23:41  lindner
 * eliminate non-used variables
 *
 * Revision 3.9  1993/07/27  05:28:51  lindner
 * Mondo Debug overhaul from Mitra
 *
 * Revision 3.8  1993/07/26  15:37:27  lindner
 * fix warning message for hosed gopher.rc lines.
 *
 * Revision 3.7  1993/07/20  23:12:34  lindner
 * Mods to use patchlevel.h
 *
 * Revision 3.6  1993/07/07  19:44:27  lindner
 * Better MIME compatibility, now accepts text/plain
 *
 * Revision 3.5  1993/06/09  22:14:42  lindner
 * VMS updates
 *
 * Revision 3.4  1993/04/15  21:21:30  lindner
 * Code for remote users (Mitra)
 *
 * Revision 3.3  1993/03/24  16:58:47  lindner
 * Added RCprintCommand()
 *
 * Revision 3.2  1993/02/19  21:10:17  lindner
 * Generalized some routines.  Added global rc file.
 *
 * Revision 3.1.1.1  1993/02/11  18:03:00  lindner
 * Gopher+1.2beta release
 *
 * Revision 2.1  1993/02/09  22:35:15  lindner
 * Lots of changes.  RC processing now holds the global variables inside
 * its container.  Much more objecty
 *
 * Revision 1.8  1993/01/12  21:16:44  lindner
 * Fixed gopherrc for good!
 *
 * Revision 1.7  1993/01/09  02:53:55  lindner
 * Fixed spelling error on it's
 *
 * Revision 1.6  1993/01/09  02:31:09  lindner
 * Added #define to check for MAXPATHLEN
 *
 * Revision 1.5  1992/12/31  05:46:52  lindner
 * Merged 1.2.1.1 in main code sequence
 *
 * Revision 1.4  1992/12/15  16:54:38  lindner
 * Fixed typo in initial display message.  Also changed message
 * "Press any key to continue" to "Press RETURN to continue", since
 * we're not in raw() mode.
 *
 * Revision 1.3  1992/12/14  20:41:00  lindner
 * .gopherrc was being created with a random permissions mask.  Fixed it
 * Also made it so that gopherrc is created with mode 0644
 *
 * Revision 1.2.1.1  1992/12/31  05:42:26  lindner
 * VMS mods.
 *
 * Revision 1.2  1992/12/11  19:52:59  lindner
 * Added copyright notices when the users starts the client the first time.
 *
 * Revision 1.1  1992/12/10  23:32:16  lindner
 * gopher 1.1 release
 *
 * Revision 1.1  1992/12/10  06:16:51  lindner
 * Initial revision
 *
 *
 *********************************************************************/


#include "gopherrc.h"
#include "CURcurses.h"
#include "conf.h"
#include "globals.h"
#include "Malloc.h"
#include "patchlevel.h"
#include "compatible.h"

#include <stdio.h>

#ifndef VMS
#include <fcntl.h>
#include <pwd.h>
#endif

#ifdef VMS
#include <unixlib.h>
#define MAXPATHLEN FILENAME_MAX
#include <file.h>
#define GLOBALRC "[-.gopher]gopher.rc"
#endif

#ifndef MAXPATHLEN
#define MAXPATHLEN 256
#endif



/*
 * Definitions for the mapping object 
 */

RCMapObj *
RCMnew()
{
     RCMapObj *rcm;

     rcm = (RCMapObj*) malloc(sizeof(RCMapObj));
     
     rcm->view = STRnew();
     rcm->displaycmd = STRnew();
     rcm->printcmd = STRnew();

     return(rcm);
}

void
RCMdestroy(rcm)
  RCMapObj *rcm;
{
     STRdestroy(rcm->view);
     STRdestroy(rcm->displaycmd);
     STRdestroy(rcm->printcmd);
     free(rcm);
}

void
RCMinit(rcm)
  RCMapObj *rcm;
{
     STRinit(rcm->view);
     STRinit(rcm->displaycmd);
     STRinit(rcm->printcmd);
}

	  
void
RCMcpy(dest, orig)    
  RCMapObj *dest, *orig;
{
     STRcpy(dest->view, orig->view);
     STRcpy(dest->displaycmd, orig->displaycmd);
     STRcpy(dest->printcmd, orig->printcmd);
}



/* 
 * Substitue filename for %s in zeline and return the command in "line",
 * 
 */

static void
RCMsubst_percent_s(rcm, filename, line, zeline)
  RCMapObj *rcm;
  char *filename, *line, *zeline;
{
     
     while (1) {
	  if (*zeline=='\0') {
	       *line = '\0';
	       return;
	  }
	  
	  else if (*zeline == '%') {
	       if (*(zeline+1) == 's') {
		    strcpy(line, filename);
		    line += strlen(filename);
		    zeline+=2;
	       } else {
		    *line = *zeline;
		    line++;
		    zeline++;
	       }
	  }
	  else {
	       *line = *zeline;
	       line++;
	       zeline++;
	  }
     }

}



/* 
 * Construct a command line for displaying.
 */


void
RCMdisplayCommand(rcm, filename, line)
  RCMapObj *rcm;
  char *filename, *line;
{
     char *zeline;

     zeline  = RCMgetDisplaycmd(rcm);
     RCMsubst_percent_s(rcm, filename, line, zeline);
}




/* 
 * Construct a command line for printing.
 */


void
RCMprintCommand(rcm, filename, line)
  RCMapObj *rcm;
  char *filename, *line;
{
     char *zeline;

     zeline  = RCMgetPrintcmd(rcm);
     RCMsubst_percent_s(rcm, filename, line, zeline);
}

     

/*
 * Find the item that contains "view"
 */

int
RCMAviewSearch(rcma, view)
  RCMAarray *rcma;
  char      *view;
{
     int i;
     RCMapObj *temp;
     char viewstowage[64];
     char *tempview, *cp;

     tempview = view;

     if ((cp = strchr(view, ' ')) != NULL) {
	  strcpy(viewstowage, view);
	  viewstowage[cp - view] = '\0';
	  tempview = viewstowage;
     }


     /*** Linear search.  Ick. ***/
     
     for (i=0; i< RCMAgetNumEntries(rcma); i++) {
	  temp = RCMAgetEntry(rcma,i);
	  
	  if (strcasecmp(tempview, RCMgetView(temp))==0)
	       return(i);
     }
     return(-1);
}

/*
 * Map: "view","display command","print command"
 */

boolean
RCMAfromLine(rcma, line)
  RCMAarray *rcma;
  char *line;
{
     char *cp;
     RCMapObj *rcm;
     char *view, *dcmd, *pcmd;
     int num;

     view = line;

     cp = strchr(line, ',');
     if (cp == NULL)
	  return(FALSE);

     *cp = '\0';
     dcmd = cp+1;

     cp = strchr(dcmd, ',');
     if (cp == NULL)
	  return(FALSE);

     *cp = '\0';
     pcmd = cp+1;
     
     if ((num = RCMAviewSearch(rcma, view)) >=0)
	  rcm = RCMAgetEntry(rcma, num);
     else
	  rcm = RCMnew();

     RCMsetView(rcm, view);
     RCMsetDisplaycmd(rcm, dcmd);
     RCMsetPrintcmd(rcm, pcmd);

     if (num <0) {
	  RCMAadd(rcma, rcm);
	  RCMdestroy(rcm);
     }

     return(TRUE);
}


/*
 * Writes out the .gopherrc file.
 */

void
RCMAtoFile(rcma, fd)
  RCMAarray *rcma;
  int fd;
{
     int i;
     RCMapObj *rcm;
     char tmpstr[512];

     for (i = 0 ; i<RCMAgetNumEntries(rcma); i++) {
	  rcm = RCMAgetEntry(rcma, i);
	  sprintf(tmpstr, "map: %s,%s,%s\n", RCMgetView(rcm),
		  RCMgetDisplaycmd(rcm), RCMgetPrintcmd(rcm));
	  writestring(fd, tmpstr);
     }

}

/**********************************************************
*  Generic RC stuff follows
*
*/

RCobj *
RCnew()
{
     RCobj *rc;

     rc = (RCobj *) (malloc(sizeof(RCobj)));
     
     rc->commands  = RCMAnew();
     rc->Bookmarks = NULL;
     rc->ChangedDefs = FALSE;
     
     RCsetdefs(rc);
     return(rc);
}



static int
rcopen(flags) 
  int flags;
{
     char rcfilename[MAXPATHLEN];
     int rcfile;
#ifndef VMS
     struct passwd *pwdentry;
     pwdentry = getpwuid(geteuid());

     if (pwdentry == NULL)
	  return(-1);

     /** First, let's find out if there is a bookmark file **/

     strcpy(rcfilename,pwdentry->pw_dir);   /* Home directory */
     strcat(rcfilename,"/.gopherrc");

#else
     strcpy(rcfilename,"sys$login:gopherrc");
#endif  /* VMS */

     rcfile = open(rcfilename, flags,0644);

     return(rcfile);
}     


void
RCfromFile(rc, rcfd)
  RCobj *rc;
  int rcfd;
{
     char inputline[512];

     while (readline(rcfd, inputline, sizeof(inputline))) {

	  if (*inputline == '#')
	       continue;

	  ZapCRLF(inputline);
	  if (strncasecmp(inputline, "bookmarks:", 9)==0) {
	      
	       if (RCgetBookmarkDir(rc) == NULL)
		    RCinitBookmarkDir(rc, 32);
	       
	       GDfromLink(RCgetBookmarkDir(rc), rcfd, "localhost", 70, "");
	       
	       GDsetTitle(RCgetBookmarkDir(rc), "Bookmarks");
	  } else if (strncasecmp(inputline, "map: ", 5)==0) {
	       if (RCMAfromLine(rc->commands, inputline+5) == FALSE) {
		    /** Error parsing **/
		    printf("Warning, bad line in gopherrc: %s\n", inputline);
		    sleep(2);
	       }
	  } else
	       ;

     }
     ;
}

/*
 * Process the User's .gopherrc file and read it into rc
 */

void
RCfromUser(rc)
  RCobj *rc;
{
     int rcfile;
#ifndef VMS
     struct passwd *pwdentry;

     pwdentry = getpwuid(geteuid());

     if (pwdentry == NULL)
	  return;
#endif
     rcfile = rcopen(O_RDONLY);

     if (rcfile < 0) {
	  /*** Make the .gopherrc file ***/
	if (!NoShellMode) { /*  No point in telling a user with no shell 
				about a file they cant get at! */
	  rcfile = rcopen(O_CREAT);
	  close(rcfile);
	  printf("Welcome to the wonderful world of Gopher!\n\n");
	  printf("Gopher has limitations on its use and comes without\n");
	  printf("a warranty.  Please refer to the file 'Copyright' included\n");
	  printf("in the distribution.\n\n");
	  printf("Internet Gopher Information Client %s.%s patch%d\n",
		 GOPHER_MAJOR_VERSION, GOPHER_MINOR_VERSION, PATCHLEVEL);
	  printf("Copyright 1991,92,93 by the Regents of the University of Minnesota\n\n");
	  printf("Press RETURN to continue\n");
	  while ('\n' != getchar())
	       ;
	} /*NoShellMode*/
	  return;    /*** No such file ***/
     }
     else {
	  RCfromFile(GlobalRC, rcfile);
     }
     close(rcfile);
}


/*
 * Set up some useful definitions for RCfiles 
 */

void
RCsetdefs(rc)
  RCobj *rc;
{
     char tmpstr[512];
     int rcfile;
     
     sprintf(tmpstr, "Text,%s %%s,%s %%s", PAGER_COMMAND, PRINTER_COMMAND);
     RCMAfromLine(rc->commands, tmpstr);

     sprintf(tmpstr, "Text/plain,%s %%s,%s %%s", PAGER_COMMAND, PRINTER_COMMAND);
     RCMAfromLine(rc->commands, tmpstr);

     sprintf(tmpstr, "Audio/basic,%s,", PLAY_COMMAND);
     RCMAfromLine(rc->commands, tmpstr);

     sprintf(tmpstr, "Image,%s %%s,%s %%s", IMAGE_COMMAND, PRINTER_COMMAND);
     RCMAfromLine(rc->commands, tmpstr);

     sprintf(tmpstr, "Terminal/telnet,%s %%s,", TELNET_COMMAND);
     RCMAfromLine(rc->commands, tmpstr);
 
     sprintf(tmpstr, "Terminal/tn3270,%s %%s,", TN3270_COMMAND);
     RCMAfromLine(rc->commands, tmpstr);

     if (RemoteUser)
	  rcfile = open(REMOTERC, O_RDONLY);
     else
	  rcfile = open(GLOBALRC, O_RDONLY);

     if (rcfile > -1) {
	  RCfromFile(rc, rcfile);
	  
	  close(rcfile);
     }
}     


void
RCfromENV()
{
     ;
}


void
RCtoFile(rc)
  RCobj *rc;
{
     int rcfile;
     char rcfilename[512];
     char oldrcfilename[512];
#ifndef VMS
     struct passwd *pwdentry;

     pwdentry = getpwuid(geteuid());

     if (pwdentry == NULL)
	  return;
#endif /* not VMS */

     /* In NoShellMode we just stop writing of map: stuff */
     if (SecureMode)
	  return;

     /** First, let's find out if there is a rc file **/

#ifndef VMS
     strcpy(rcfilename,pwdentry->pw_dir);   /* Home directory */
     strcat(rcfilename,"/.gopherrc");
     strcpy(oldrcfilename,pwdentry->pw_dir); 
     strcat(oldrcfilename,"/.gopherrc~");

     rcfile = open(rcfilename, O_RDONLY);

     if (rcfile > -1)
	  if (rename(rcfilename, oldrcfilename)<0) {
	       CursesErrorMsg("Aborting gopher configuration save!!");
	       return;
	  }

     close(rcfile);

#else
     strcpy(rcfilename,"sys$login:gopherrc");
#endif
 
     rcfile = open(rcfilename, O_WRONLY|O_CREAT|O_TRUNC, 0644);

     if (rcfile < 0) {
#ifdef VMS
          fprintf(stderr,"Can't write gopher configuration file!!");
#else
	  CursesErrorMsg("Can't write gopher configuration file!!");
#endif
	  return;    
     }

     writestring(rcfile, "RCversion: 1.0\n");

     if (!NoShellMode)   RCMAtoFile(rc->commands, rcfile);
     
     if (RCgetBookmarkDir(GlobalRC) != NULL) {
	  writestring(rcfile, "\nbookmarks:\n");
	  GDtoLink(RCgetBookmarkDir(GlobalRC), rcfile);
     }

     writestring(rcfile, "\n");
     close(rcfile);
#ifdef VMS
     while (unlink("sys$login:gopherrc.;-1") == 0) ; /* purge/keep=1 */
#endif
}


/*
 * This makes a display command for the client.
 * It returns FALSE if it finds a method of displaying the item
 *
 * It returns TRUE and fills line with a command if one is found
 */

boolean 
RCdisplayCommand(rc, view, filename, line)
  RCobj *rc;
  char  *view, *filename, *line;
{
     int num;  

     num = RCMAviewSearch(rc->commands, view);

     if (num >=0) {
	  RCMdisplayCommand(RCMAgetEntry(rc->commands, num), filename, line);
	  return(TRUE);
     }
     else
	  return(FALSE);
}

/*
 * This makes a display command for the client.
 * It returns FALSE if it finds a method of printing the item
 *
 * It returns TRUE and fills line with a command if one is found
 */

boolean 
RCprintCommand(rc, view, filename, line)
  RCobj *rc;
  char  *view, *filename, *line;
{
     int num;  

     num = RCMAviewSearch(rc->commands, view);

     if (num >=0) {
	  RCMprintCommand(RCMAgetEntry(rc->commands, num), filename, line);
	  return(TRUE);
     }
     else
	  return(FALSE);
}

