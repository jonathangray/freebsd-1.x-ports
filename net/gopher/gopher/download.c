/********************************************************************
 * lindner
 * 3.9
 * 1993/08/16 17:57:58
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopher/download.c,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: download.c
 * Functions relating to downloading data
 *********************************************************************
 * Revision History:
 * download.c,v
 * Revision 3.9  1993/08/16  17:57:58  lindner
 * Fix for sys$scratch for VMS
 *
 * Revision 3.8  1993/08/09  20:39:55  lindner
 * fix for VMS
 *
 * Revision 3.7  1993/07/30  17:36:54  lindner
 * More secure download in /tmp
 *
 * Revision 3.6  1993/07/29  17:21:05  lindner
 * eliminate non-used variables
 *
 * Revision 3.5  1993/07/23  04:36:03  lindner
 * LocalFile mods
 *
 * Revision 3.4  1993/07/20  23:11:25  lindner
 * downloading now caches the downloaded file too
 *
 * Revision 3.3  1993/04/30  16:01:19  lindner
 * kermit binary mods
 *
 * Revision 3.2  1993/03/24  16:57:37  lindner
 * Fixes for new SaveFile()
 *
 * Revision 3.1.1.1  1993/02/11  18:02:57  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.4  1993/01/14  21:59:30  lindner
 * Filenames generated for zmodem now are a bit better.. should work better
 * on VMS
 *
 * Revision 1.3  1993/01/12  20:42:04  lindner
 * Added <stat.h> stuff for VMS, also changed text download for VMS from
 * cat to type
 *
 * Revision 1.2  1993/01/11  19:26:56  lindner
 * Mods to make it work under VMS
 *
 * Revision 1.1  1993/01/07  22:47:20  lindner
 * Initial revision
 *
 *
 *********************************************************************/




#include "gopher.h"
#ifdef VMS
#include <stat.h>
#else
#include <sys/stat.h>
#endif

static char *DLnames[] = {
     "Zmodem",
     "Ymodem",
     "Xmodem-1K",
     "Xmodem-CRC",
     "Kermit",
     "Text",
     NULL
     };

static char *DLcmds[] = { /* Cmds for ascii files: FILE */
     "sz ",
     "sb ",
     "sx -k ",
     "sx ",
     "kermit -q -s ",
#ifdef VMS
     "type ",
#else
     "cat -v ",
#endif
     NULL
     };

static char *DLcmdB[] = {     /* Cmds for binary files */
     "sz ",
     "sb ",
     "sx -k ",
     "sx ",
     "kermit -q -i -s ",
#ifdef VMS
     "type ",
#else
     "cat -v ",
#endif
     NULL
     };

void
Download_file(gs)
  GopherObj *gs;
{
     int    choice;
     char   tmpfilename[512], *cp;
     char   command[512];
     int    start, end;
     struct stat buf;

     switch (GSgetType(gs)) {
     case A_DIRECTORY:
     case A_CSO:
     case A_ERROR:
     case A_INDEX:
     case A_TELNET:
     case A_TN3270:
	  CursesErrorMsg("Sorry, can't download that!");
	  return;
     }

     choice = CURChoice(CursesScreen, GSgetTitle(gs), DLnames, 
			"Choose a download method", -1);
     
     if (choice == -1)
	  return;
     
     
     /*** Get a reasonable tmp file name ***/
     cp = GSgetPath(gs);
     if ((cp = strrchr(cp,'/')) != NULL)
	  strcpy(tmpfilename, cp+1);
     else
	  strcpy(tmpfilename,GSgetTitle(gs));

#ifdef VMS
     VMSfile(tmpfilename);
#else
     UNIXfile(tmpfilename);
#endif

     for (cp=tmpfilename; *cp != '\0'; cp++) {
	  switch (*cp) {
	  case ' ':
	  case '\"':
	  case '\'':
	       *cp = '_';
	  }
     }

#ifdef VMS
     if (chdir("SYS$SCRATCH")!=0) {
	  CursesErrorMsg("Can't write to SYS$SCRATCH!");
	  return;
     }
#else
     if (chdir("/tmp")!=0) {
	  CursesErrorMsg("Can't write to the /tmp directory!");
	  return;
     }
#endif

     /** Make sure we don't overwrite an existing file ... **/
     while (stat(tmpfilename, &buf) == 0) {
	  int len = strlen(tmpfilename);

	  if (tmpfilename[len-1] == '-') {
	       tmpfilename[len] = tmpfilename[len] + 1;
	  } else
	       strcat(tmpfilename, "-1");
     }

     /*** Retrieve the file ***/
     if (GSgetLocalFile(gs) == NULL) {
	  Save_file(gs, tmpfilename);
	  GSsetLocalFile(gs, tmpfilename);
     } else
	  strcpy(tmpfilename, GSgetLocalFile(gs));

     /*** Check to see which method they're using to download ***/
     
     if (stat(tmpfilename, &buf) < 0) {
	  CursesErrorMsg("File didn't transfer successfully");
	  return;
     }

     GSsetLocalFile(gs, tmpfilename);

     /*** Now start the download ... ***/
     switch (GSgetType(gs)) {
     case A_FILE:
	  strcpy(command, DLcmds[choice]);
	  break;
     default:
	  strcpy(command, DLcmdB[choice]);
     }
     strcat(command, tmpfilename);
     
     CURexit(CursesScreen);

     if (choice == 5) {
	  printf("Start your capture now...\n\n");
	  printf("Press <RETURN> when you're ready\n");
	  fflush(stdout);
	  getchar();
     } else {
	  printf("Start your download now...\n");
	  fflush(stdout);
     }

     start = time(NULL);

     if (system(command))
	  printf("\nDownload could not be completed, sorry... \n");
     else {
	  end = time(NULL);
	  if (end == start)
	       end++;
     
	  printf("\nDownload Complete. %d total bytes, %d bytes/sec\n",
		 buf.st_size, buf.st_size/(end-start));
     }

     printf("Press <RETURN> to continue");
     fflush(stdout);
     getchar();
     CURenter(CursesScreen);
     
}
