/********************************************************************
 * lindner
 * 3.34
 * 1993/08/23 02:32:28
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopher/gopher.c,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: gopher.c
 * Main functions for the gopher client
 *********************************************************************
 * Revision History:
 * gopher.c,v
 * Revision 3.34  1993/08/23  02:32:28  lindner
 * Don't connect if nothing typed in 'o'
 *
 * Revision 3.33  1993/08/19  20:31:10  lindner
 * remove excess variable
 *
 * Revision 3.32  1993/08/19  20:22:49  lindner
 * Mitra's Debug patch
 *
 * Revision 3.31  1993/08/16  18:10:19  lindner
 * Temporary code to fix DEC Alphas screen clearing, exit handler for VMS
 *
 * Revision 3.30  1993/08/16  17:58:20  lindner
 * Removed REMOTEUSER ifdefs
 *
 * Revision 3.29  1993/08/09  20:28:04  lindner
 * Mods for VMS for telnet dialog, argv[0]
 *
 * Revision 3.28  1993/08/09  20:17:59  lindner
 * Fixes for CMULIB and NETLIB for VMS
 *
 * Revision 3.27  1993/08/05  03:24:21  lindner
 * Fix for control-c on startup
 *
 * Revision 3.26  1993/08/04  22:08:47  lindner
 * Fix for problems with '=' and '?' and /bin/mail Gripe mods
 *
 * Revision 3.25  1993/08/03  20:48:27  lindner
 * Audio file fix from jqj
 *
 * Revision 3.24  1993/08/03  20:26:50  lindner
 * Don't allow securemode types to use o
 *
 * Revision 3.23  1993/08/03  20:24:18  lindner
 * Bigger Better Badder Options, inspired by jqj
 *
 * Revision 3.22  1993/08/03  04:43:56  lindner
 * Fix for VMS unresolved variables
 *
 * Revision 3.21  1993/07/30  17:37:24  lindner
 * SecureMode fix from Mitra
 *
 * Revision 3.20  1993/07/30  14:19:29  lindner
 * Mitra autoexit patch
 *
 * Revision 3.19  1993/07/30  14:12:18  lindner
 * Allow non-gplus stuff to cache
 *
 * Revision 3.18  1993/07/29  17:24:53  lindner
 * Removed dead variable, $ and ! are synonomous now
 *
 * Revision 3.17  1993/07/27  05:28:48  lindner
 * Mondo Debug overhaul from Mitra
 *
 * Revision 3.16  1993/07/27  02:02:22  lindner
 * More comments, GDdelete method
 *
 * Revision 3.15  1993/07/26  20:29:40  lindner
 * fix memory usage
 *
 * Revision 3.14  1993/07/26  15:35:56  lindner
 * fix for longjmp params
 *
 * Revision 3.13  1993/07/23  04:42:58  lindner
 * error checking and longjmp'ing
 *
 * Revision 3.12  1993/07/20  23:15:35  lindner
 * Mods to cache askdata..
 *
 * Revision 3.11  1993/07/07  19:42:59  lindner
 * fix for SGIs
 *
 * Revision 3.10  1993/06/29  06:18:09  lindner
 * Prettier error msg for VMS
 *
 * Revision 3.9  1993/06/22  06:13:28  lindner
 * took back fix for perror.h etc.
 *
 * Revision 3.8  1993/06/11  16:25:43  lindner
 * Many bug fixes, open new gopher, shell escape, etc.
 *
 * Revision 3.7  1993/04/30  16:04:48  lindner
 * Cleared gripe memory, fixed bug for long names in  check_sock
 *
 * Revision 3.6  1993/04/13  04:56:54  lindner
 * New code for REMOTEUSERS from Mitra
 * Better error messages for socket connections from jqj
 *
 * Revision 3.5  1993/03/24  16:59:54  lindner
 * Major changes to the way things are displayed
 *
 * Revision 3.4  1993/03/18  23:32:07  lindner
 * commented out forkoff stuff for now...
 *
 * Revision 3.3  1993/02/19  21:08:04  lindner
 * Updated most routines to get defaults from gopher+ attribute types,
 * Allow commands to be pipelines and almost have forking working :-)
 *
 * Fixed problems with bookmarks.  Still need to work on g+ bookmarks
 *
 * Revision 3.2  1993/02/11  18:25:45  lindner
 * Fixed helpfile display.
 *
 * Revision 3.1.1.1  1993/02/11  18:02:58  lindner
 * Gopher+1.2beta release
 *
 * Revision 2.1  1993/02/09  22:34:31  lindner
 * Mostly changes for RC stuff.
 *
 * Revision 1.16  1993/01/31  00:08:12  lindner
 * Mucho changes for gopher+, Gripe() fcn, better describe_gopher()
 * Now retrieves directories using gopher+ if available.
 *
 * Revision 1.15  1993/01/17  03:46:46  lindner
 * Fixes for tmpname memory leak.
 *
 * Revision 1.14  1993/01/15  20:14:27  lindner
 * Added -T to the usage line
 *
 * Revision 1.13  1993/01/14  22:00:21  lindner
 * added ^R and ^W to redraw the screen for VMS
 * Old search terms don't persist now when highlighting text.
 *
 * Revision 1.12  1993/01/13  16:18:11  lindner
 * Put back in 's' save patch...  Sigh...
 *
 * Revision 1.11  1993/01/12  22:38:56  lindner
 * Rescinded changes for sound on VMS, it doesn't work!
 *
 * Revision 1.10  1993/01/12  21:42:14  lindner
 * Fixed problem with viewing files in secure mode.
 * Fixed problems with suck_sound in VMS.
 * Fixed problems with \n on the system command in VMS
 *
 * Revision 1.9  1993/01/12  17:30:17  lindner
 * Fixed problem with 's' key for save_file.
 *
 * Revision 1.8  1993/01/09  02:33:46  lindner
 * Fixed definitions for controlc() and sizechange()
 *
 * Revision 1.7  1993/01/09  02:18:40  lindner
 * Changed (void*)-1 constructs to SIG_ERR
 *
 * Revision 1.6  1993/01/08  19:25:02  lindner
 * Securemode users can't display graphics now, they get a message instead..
 *
 * Revision 1.5  1993/01/07  22:49:40  lindner
 * Added option -T to set initial type.  Added 'D' command for downloading
 *
 * Revision 1.4  1992/12/31  06:34:49  lindner
 * Okay, okay, Save_File is really Save_file
 *
 * Revision 1.3  1992/12/31  05:38:01  lindner
 * Removed getfile() replaced with Save_File().
 *
 * Revision 1.2  1992/12/31  04:34:38  lindner
 * Added VMS support from fogel and jqj.
 *
 * Revision 1.1  1992/12/10  23:32:16  lindner
 * gopher 1.1 release
 *
 *********************************************************************/


#include "gopher.h"
#include "Stdlib.h"
#include "Debug.h"

#include <errno.h>

void describe_gopher();
extern int  twirl();


/*
** Open a connection to another host using telnet or tn3270
*/

void
do_tel_3270(ZeGopher)
  GopherStruct *ZeGopher;
{
     char *Dialogmess[9];

     char sMessage1[128];
     char sMessage2[128];
     char sTelCmd[128]; 

     /* retrieve the gopher information for the telnet command*/

     clear();
#ifdef VMS
     refresh();
#endif
     Dialogmess[0] = "Warning!!!!!, you are about to leave the Internet";
     Dialogmess[1] = "Gopher program and connect to another host. If";
     Dialogmess[2] = "you get stuck press the control key and the";
#if defined(VMS) && defined(MULTINET)
     Dialogmess[3] = "^ key, and then type q.";
#else
     Dialogmess[3] = "] key, and then type quit";
#endif
     Dialogmess[4] = "";
     
     if (GSgetPort(ZeGopher) != 0)
	  sprintf(sMessage1,"Connecting to %.40s, port %d using %s.", 
		  GSgetHost(ZeGopher),GSgetPort(ZeGopher), 
		  (GSgetType(ZeGopher) == A_TN3270) ? "tn3270" : "telnet");
     else
	  sprintf(sMessage1, "Connecting to %.40s using %s.",
		  GSgetHost(ZeGopher),
		  (GSgetType(ZeGopher) == A_TN3270) ? "tn3270" : "telnet");

     Dialogmess[5] = sMessage1;

     if (*GSgetPath(ZeGopher) != '\0')
	  sprintf(sMessage2,"Use the account name \"%.40s\" to log in",
		  GSgetPath(ZeGopher));
     else
	  sMessage2[0] = '\0';

     Dialogmess[6] = "";
     Dialogmess[7] = sMessage2;
     Dialogmess[8] = NULL;

     if (CURDialog(CursesScreen, GSgetTitle(ZeGopher), Dialogmess) <0)
	  return;

     CURexit(CursesScreen);

     if (GSgetType(ZeGopher) == 'T') {
	  /**** A TN3270 connection ****/
	  RCdisplayCommand(GlobalRC, "Terminal/tn3270", GSgetHost(ZeGopher), sTelCmd);
     } else {

	  RCdisplayCommand(GlobalRC, "Terminal/telnet", GSgetHost(ZeGopher), sTelCmd);

	  if (GSgetPort(ZeGopher) != 0 && GSgetPort(ZeGopher) != 23) 
#if defined(VMS) && (defined(MULTINET) || defined(CMUIP))
               sprintf(sTelCmd+strlen(sTelCmd),
		       "/PORT=%d", GSgetPort(ZeGopher));
#else
	       sprintf(sTelCmd+strlen(sTelCmd), " %d", GSgetPort(ZeGopher));
#endif
     }
     
     CURexit(CursesScreen);
     system(sTelCmd);
     CURenter(CursesScreen);
     return;
}


void
Gripe(gs)
 GopherObj *gs;
{
     char *gripeprompt[10];
     char *gripemess[10];
     char *cp, email[128], mailcmd[256];
     int i;
     FILE *f;

     GSgetginfo(gs);

     if (!GSisGplus(gs) || GSgetAdmin(gs) == NULL) {
	  CursesErrorMsg("Can't find an administrator for this item, sorry!");
	  return;
     }

     cp = strchr(GSgetAdmin(gs),'<');
     
     if (cp == NULL)
	  return;

     strncpy(email, cp+1, sizeof(email));
     cp = strrchr(email, '>');
     if (cp != NULL)
	  *cp = '\0';

     /** Empty out the array **/
     for (i=0; i< 10; i++) {
	  cp = (char *) malloc(80);
	  bzero(cp,80);
	  gripeprompt[i] = "";
	  gripemess[i] = cp;
     }

     gripeprompt[0] = "Subject";
     gripeprompt[1] = "Problem";
     gripeprompt[9] = NULL;

     if (CURRequest(CursesScreen, GSgetAdmin(gs), gripeprompt, gripemess)!=0) {
	  return;
     }

     sprintf(mailcmd, "%s %s", MAIL_COMMAND, email);
     
     f = popen(mailcmd, "w");
     if (f == NULL) {
	  CursesErrorMsg("Cannot send mail...");
	  return;
     }

     fprintf(f, "Subject: %s\n\n", gripemess[0]);
     for (i=1; i< 10; i++) {
	  fprintf(f, "%s\n", gripemess[i]);
	  free(gripemess[i]);
     }
     pclose(f);
}

/*
** do_index gets keywords from the user to search for.  It returns
** it to the calling process.  This storage is volotile. Callers should
** make a copy if they want to call do_index multiple times.
*/

char* do_index(ZeGopher)
  GopherObj *ZeGopher;
{
     static char *inputline = NULL;
     static char *prompt[2];
     static char *response[2];

     if (inputline == NULL) {
	  inputline = (char *) malloc(sizeof(char)*256);
	  if (inputline == NULL)
	       perror("Out of memory"), exit(-1);
	  *inputline = '\0';
     }

     prompt[0] = "Words to search for";
     prompt[1] = NULL;

     response[0] = inputline;
     response[1] = NULL;

     if (CURRequest(CursesScreen, GSgetTitle(ZeGopher),prompt, response) == -1 )
	  return(NULL);

     if (*inputline == '\0')
	  return(NULL);
     else
	  return(inputline);
}


/*
 * this procedure just retrieves binary data from the socket and
 * pumps it into a "play" process.
 */

#define BUFSIZE 1400  /* A pretty good value for ethernet */

#ifndef VMS
void
suck_sound(sockfd)
  int sockfd;
{
     FILE *Play;
     int j;
     char buf[BUFSIZE];
     char playCmd[BUFSIZE];

     if (! RCdisplayCommand(GlobalRC, "Audio/basic", "-", playCmd)) {
	  /*** Hey! no play command, bummer ***/
	  CursesErrorMsg("Sorry, this machine doesn't support sounds");

	  return;
     }

     Play = popen(playCmd, "w");
	  
     
     while(1) {
          j = read(sockfd, buf, BUFSIZE);
	  
	  if (j == 0)
	       break;
	  
	  fwrite(buf, 1, j, Play);
     }
}
#endif

/*
 * fork off a sound process to siphon the data across the net.
 * So the user can listen to tunage while browsing the directories.
 */

void
do_sound(ZeGopher)
  GopherStruct *ZeGopher;
{
#ifdef VMS
     CursesErrorMsg("Sorry, this machine doesn't support sounds");
#else
     int sockfd;
     BOOLEAN Waitforchld = FALSE;

     if ((sockfd = GSconnect(ZeGopher)) <0) {
	  check_sock(sockfd, GSgetHost(ZeGopher), GSgetPort(ZeGopher));
	  return;
     }

     /** Send out the request **/
     GStransmit(ZeGopher, sockfd, NULL, NULL, NULL);
     
     /** Okay, it's cool, we can fork off **/

     if (SOUNDCHILD != 0)
	  Waitforchld = TRUE;

     if ( (SOUNDCHILD = fork()) < 0)
	  ;/* Fork Error */
     
     else if (SOUNDCHILD == 0) {  /* Child Process */
	  wait(SIGCHLD);
	  suck_sound(sockfd);
	  exit(0);
     }
     
     /* Parent Process */
     
     closenet(sockfd);
     return;
#endif  /* not VMS */
}



/*
 * Replace the searched words with backspaces and underline characters.
 */

static char sGBoldoutput[20];  /*** Used for stripping weird stuff from
				    term strings ***/
static int iGposition = 0;     /*** Pointer into the Boldoutput string **/

/*** Used by tputs() ***/

int
Boldoutchar(c)
  char c;
{
     sGBoldoutput[iGposition++] = c;
     return(c);
}


void
Boldit(inputline, outputline, MungeSearchstr)
  char *inputline, *outputline, *MungeSearchstr;
{
     char words[20][40];  /** A reasonable guess **/
     int numchars, lowwordnum, wordcount, i;
     char *cp, *lowword;

     outputline[0] = '\0';

     bzero(outputline, 512);
     
     while (isspace(*MungeSearchstr)) /** Strip off spaces **/
	  MungeSearchstr++;
	  
     for (wordcount=0; wordcount<20; wordcount++) {

	  while (isspace(*MungeSearchstr)) /** Strip off spaces **/
	       MungeSearchstr++;
	  
	  numchars = sreadword(MungeSearchstr, words[wordcount], 40);
	  MungeSearchstr += numchars;
	  if (numchars == 0)
	       break;
	  if (strcmp(words[wordcount], "and")==0 ||
	      strcmp(words[wordcount], "or")==0 ||
	      strcmp(words[wordcount], "not")==0) {
	       words[wordcount][0] = '\0';
	       wordcount--;
	  }
     }


     /** Find the first word in the line **/

     while (*inputline!='\0') {
	  lowword = NULL;

	  for (i=0; i< wordcount; i++) {
	       cp = strcasestr(inputline, words[i]);
	       if (cp != NULL)
		    if (cp < lowword || lowword == NULL) {
			 lowword = cp;
			 lowwordnum = i;
		    }
	  }

	  if (lowword == NULL) {
	       strcpy(outputline, inputline);
	       return;
	  }
	  else {
	       strncpy(outputline, inputline, lowword - inputline);
	       outputline += (lowword - inputline);
	       inputline = lowword;
	       
	       iGposition = 0;
	       tputs(CURgetHighon(CursesScreen), 1, Boldoutchar);
	       sGBoldoutput[iGposition] = '\0';
	       strcpy(outputline, sGBoldoutput);
	       outputline += strlen(sGBoldoutput);

	       strncpy(outputline, inputline, strlen(words[lowwordnum]));
	       inputline += strlen(words[lowwordnum]);
	       outputline += strlen(words[lowwordnum]);


	       iGposition = 0;
	       tputs(CURgetHighoff(CursesScreen), 1, Boldoutchar);
	       sGBoldoutput[iGposition] = '\0';
	       strcpy(outputline, sGBoldoutput);
	       outputline += strlen(sGBoldoutput);

	  }
     }
}


/**
*** Show file takes a gopher text thing, writes it to a file
*** and passes it to your favorite pager.
**/

void
showfile(ZeGopher)
  GopherObj *ZeGopher;
{
     char    *tmpfilename = NULL;
     FILE    *tmpfile;
     char    inputline[512];
     char    outputline[512];
     char    *view = NULL;
     boolean WritePipe = FALSE,
             ForkOff   = FALSE;
     boolean GS2FileSucceeded = TRUE;
     int     Child;

     DebugGSplusPrint(ZeGopher, "showfile:start");

     view = Choose_View(ZeGopher);
     Debug("showfile:Choose_View returned view=%s\n",view);

     if (view == NULL)
	  return;

     RCdisplayCommand(GlobalRC, view, "", inputline);
     Debug("showfile:inputline=%s\n",inputline);

     if (GSgetLocalFile(ZeGopher) == NULL) {

	  if (*inputline == '|') {
	       if ((tmpfile = popen(inputline+1, "w")) == NULL)
		    fprintf(stderr, "Couldn't execute %s\n",inputline), exit(-1);
	       WritePipe = TRUE;
	       CURexit(CursesScreen);
	  } else {
	       /** Open a temporary file **/
	       tmpfilename = tempnam("/tmp","gopher");

	       if ((tmpfile = fopen(tmpfilename, "w")) == NULL)
		    fprintf(stderr, "Couldn't make a tmp file!\n"), exit(-1);
	       GSsetLocalFile(ZeGopher, tmpfilename);
	  } /* | */
	  
	  if (inputline[strlen(inputline)-1] == '&') {
	       inputline[strlen(inputline)-1] = '\0';
	       ForkOff = TRUE;
	  }

#ifndef VMS
	  /* This is the child process, so exit.. */
	  if (ForkOff) {
	       if ((Child = fork()) < 0) {
		    ;/** Fork Error ***/
		    CursesErrorMsg("Fork Error!");
	       }
	       else if (Child >0) {
		    /*** Parent Process ***/
		    ;
		    CURenter(CursesScreen);
		    return;
	       }
	  }  /* Forkoff */

#endif
	  
 	  Debug("showfile: view=%s ",view);
 	  Debug("command=%s ",inputline);
 	  Debug("file=%s",tmpfilename);
 	  Debug("/%s\n",GSgetLocalFile(ZeGopher));
	  
	  GS2FileSucceeded = GStoFile(ZeGopher, tmpfile, view, twirl);

	  if (!GS2FileSucceeded) {
	       GSsetLocalFile(ZeGopher, NULL);
	  }

	  if (WritePipe)
	       pclose(tmpfile);  /* data went down pipe - displayed there */
	  else
	       fclose(tmpfile);  /* data is in tmpfile, will display below */

     } /* GopehrPluss || LocalFile = NULL */

     /*** Bolding...... */
     if (GSgetType(ZeGopher) == -1 && GS2FileSucceeded) {
	  if ((inputline[0] == '.') && (inputline[1] == '\0'))
	       ;/*break;*/
	  else {
	       /*** Underline searched words, except and, or and not ***/
	       if (Searchstring != NULL) {
		    Boldit(inputline, outputline, Searchstring);
	       }
	       else
		    strcpy(outputline, inputline);
	       fputs(outputline, tmpfile);
		    fputc('\n', tmpfile);
	  }
     }

     if (!WritePipe && GS2FileSucceeded)
	  GSdisplay(ZeGopher, view);

/*     if (!ForkOff) {
	  printf("\nPress <RETURN> to continue: ");
	  fflush(stdin);
	  fflush(stdin);
	  getchar();
	  CURenter(CursesScreen);
     }*/

     if (tmpfilename!=NULL) free(tmpfilename);

     if (ForkOff)
	  exit(-1);

     CURenter(CursesScreen);
     return;
}


/*
** Pushgopher takes a GopherThing pointer and adds it to it's stack.
**
** Ick this must be fixed!
*/

void
pushgopher(ZeDir)
  GopherDirObj *ZeDir;
{

     OldDirs[iLevel]= ZeDir;
     iLevel ++;
}

/*
** If the stack is empty, popgopher returns a -1
*/

int
popgopher(ZeDir)
  GopherDirObj **ZeDir;
{

     if (iLevel == 0)
	  return(-1);

     iLevel --;

     *ZeDir =  OldDirs[iLevel];

     return(0);
}


#ifdef VMS
#include <perror.h>
#else
extern int h_errno;
extern int sys_nerr;
extern char *sys_errlist[];
extern int  errno;
#endif

void check_sock(sockfd, host, port)
  int sockfd;
  char *host;
  int port;
{
     char DispString[WHOLELINE];
     char DispString2[WHOLELINE];
     char *DispStrings[4];

     /* NULL DispStrings entries here, so can override below */
     DispStrings[3] = NULL;

     if (sockfd <0) {
	  sprintf(DispString, "Cannot connect to host %.40s, port %d.", host, port);
	  switch (sockfd) {
	  case -2:
	       DispStrings[2] = "Hostname is unknown.";
	       break;
	  case -3:
	       DispStrings[2] = "Unable to allocate a socket.";
	       break;
	  case -4:
#if defined(VMS) && defined(MULTINET)
	       sprintf(DispString2, "%.78s.", vms_errno_string());
	       DispStrings[2] = DispString2;
#else
	       if (errno > 0 && errno <= sys_nerr) {
		    sprintf(DispString2, "Connection failed: %s.",
#ifdef VMS
			    strerror(errno));
#else
			    sys_errlist[errno]);
#endif
		    DispStrings[2] = DispString2;
	       } else
		    DispStrings[2] = "Connection to remote host failed.";
#endif
	       break;
	  default:
	       DispStrings[2] = "Unknown error.";
	  }
	  DispStrings[0] = DispString;
	  DispStrings[1] = "";
	  DispStrings[3] = NULL;

	  CURDialog(CursesScreen, "Network Error", DispStrings);
     }
}


BOOLEAN
ReallyQuit()
{
     char yesno[3];
	       
     yesno[0] = 'y';
     yesno[1] = '\0';
     
     CURgetYesorNo(CursesScreen, "Really quit (y/n) ?", yesno);
     if (*yesno == 'y') {
	  return(TRUE);
     }
     
     return(FALSE);
}
     

void
CleanupandExit(exitval)
  int exitval;
{
     GopherDirObj *gd;
#ifdef VMS
     extern boolean DidCleanup;
#endif

#if defined(VMS) && defined(__ALPHA)
    /*
     *  Temporary workaround for problems with
     *  Curses library functions on Alphas.
     */
     extern void VMSClearandHome();
     
     CURexit(CursesScreen);
     VMSClearandHome();
#else
     CURexit(CursesScreen);
#endif
     if (ChangedDefs)
	  RCtoFile(GlobalRC);

/*     RCdestroy(GlobalRC);*/

     do {
	  if (CurrentDir != NULL)
	       GDdestroy(CurrentDir);
     }
     while (popgopher(&CurrentDir) != -1);
     
     gd = RCgetBookmarkDir(GlobalRC);
     if (gd != NULL)
	  GDdestroy(gd);

#ifdef VMS
     DidCleanup = TRUE;
#endif
     exit(exitval);
}



/**************
** This bit of code catches control-c's, it cleans up the curses stuff.
*/
void
controlc(sig)
  int sig;
{

#ifdef VMS
     if (!CursesScreen->inCurses) {
          /** Reprime the signal and set flag **/
          if (signal(SIGINT, controlc) == SIG_ERR)
	       perror("signal died:\n"), exit(-1);
 	  HadVMSInt = TRUE;
          return;
     }
#endif

     if (CurrentDir == NULL || GDgetNumitems(CurrentDir) <= 0) {
          CURexit(CursesScreen);
	  fprintf(stderr,
		  "gopher: Nothing received for main menu, can't continue\n");
	  exit(1);
     }

     if (ReallyQuit())
     {
	  CleanupandExit(0);
     }
     else {
	  CURresize(CursesScreen);
	  scline(-1, 1, CurrentDir);
	  /** Interrupt search, go back a level?? **/
     }

     /*
      * Reprime the signals...
      */

     if (signal(SIGINT, controlc) == SIG_ERR)
	  perror("signal died:\n"), exit(-1);


     /** Really should be siglongjmp **/
     longjmp(Jmpenv,1);
}


     

/**************
** This bit of code catches window size change signals
**/

void
sizechange(sig)
  int sig;
{
     int lines, cols;
     
#ifdef  TIOCGWINSZ
     static struct      winsize zewinsize;        /* 4.3 BSD window sizing */
#endif

     lines = LINES;
     cols  = COLS;
     
#ifdef  TIOCGWINSZ
     if (ioctl(0, TIOCGWINSZ, (char *) &zewinsize) == 0) {
	  lines = zewinsize.ws_row;
	  cols  = zewinsize.ws_col;
     } else {
#endif
	  /* code here to use sizes from termcap/terminfo, not yet... */
	  ;
#ifdef  TIOCGWINSZ
     }

     if (lines != LINES || cols != COLS) {
	  LINES = lines;
	  COLS  = cols;
	  CURresize(CursesScreen);
     
	  scline(-1, 1, CurrentDir);
     }

     if (signal(SIGWINCH, sizechange)==SIG_ERR)
	  perror("signal died:\n"), exit(-1);

	    
#endif

}



/**********
**
** Set up all the global variables.
**
***********/

void
Initialize()
{

     Debug("Initialize\n",NULL)
     GlobalRC        = RCnew();

     /** get defaults from the rc file **/

     RCfromUser(GlobalRC);


     /*** Set up the curses environment ***/
     
     CursesScreen = CURnew();

     if (strcmp(CURgetTerm(CursesScreen), "unknown")==0)
	  fprintf(stderr, "I don't understand your terminal type\n"), exit(-1);


     /*** Make a signal handler for window size changes ***/

#ifdef SIGWINCH
     CURsetSIGWINCH(CursesScreen, sizechange);
     if (signal(SIGWINCH, sizechange)==SIG_ERR)
	  perror("signal died:\n"), exit(-1);

#endif

     if (signal(SIGINT, controlc) == SIG_ERR)
	  perror("signal died:\n"), exit(-1);

     if (signal(SIGPIPE, CleanupandExit) == SIG_ERR)
	  perror("signal died:\n"), exit(-1);

     if (signal(SIGTERM, CleanupandExit) == SIG_ERR)
	  perror("signal died:\n"), exit(-1);

     /*** Initialize international languages ***/
#ifdef LC_ALL
     setlocale(LC_ALL, "");
#endif
       

     /*** Init MainWindow ****/
     CURenter(CursesScreen);
}



/*
 * This stuff will set the options in a nice way...
 */

static char *OptionMenu[] =
{"General Options",
 "Edit Display Applications",
 "Edit Printing Applications",
 "Add a New Application Type",
  NULL
};

static char *OptionNewApp[] = 
{"Content Type Name",
 "Display Application",
 "Printing Application",
 NULL
 };
 

static char *GlobalOptions[MAXRESP];

void
SetOptions()
{
     static char *Responses[MAXRESP];
     static int inited = FALSE;
     char *zetitle;
     int i, choice;
     RCMapObj *rcm;
     int numoptions;


     if (SecureMode || NoShellMode) {
	  CursesErrorMsg("Sorry, you are not allowed to set options in secure mode.");
	  return;
     }


     if (inited == FALSE) {
	  
	  for (i=0; i< MAXRESP; i++) {
	       Responses[i] = (char *) malloc(sizeof(char) * MAXSTR);
	  }
	  Responses[i] = NULL;
	  inited = TRUE;
     }

     
     /** Options menu **/

     choice = CURChoice(CursesScreen, "Gopher Options", OptionMenu,
		   "Your Choice?", -1);

     if (choice == -1)
	  return;

     switch (choice) {

     case 0:
	  /** General Options **/
	  CursesErrorMsg("Sorry, no general options.... yet");
	  break;

     case 1:
     case 2:
	  /** Display/Print Applications **/
	  numoptions = RCMAgetNumEntries(GlobalRC->commands);
	  if (numoptions >= MAXRESP)
	       numoptions = MAXRESP - 1;
	  
	  for (i = 0 ; i<numoptions; i++) {
	       rcm = RCMAgetEntry(GlobalRC->commands, i);
	       GlobalOptions[i] = RCMgetView(rcm);
	       if (choice == 1)
		    strcpy(Responses[i], RCMgetDisplaycmd(rcm));
	       else if (choice ==2)
		    strcpy(Responses[i], RCMgetPrintcmd(rcm));
	  }
	  GlobalOptions[i] = NULL;

	  if (choice == 1)
	       zetitle = "Edit Display Applications";
	  else if (choice == 2)
	       zetitle = "Edit Printing Applications";

	  if (CURRequest(CursesScreen, zetitle, GlobalOptions, 
			 Responses) == 0) {
	       char tmpstr[512];
	       
	       while (i-- > 0) {
		    rcm = RCMAgetEntry(GlobalRC->commands, i);
		    if (choice == 1)
			 /** Display Applications **/
			 sprintf(tmpstr, "%s,%s,%s", GlobalOptions[i], 
				 Responses[i], RCMgetPrintcmd(rcm));
		    else
			 /** Print Applications **/
			 sprintf(tmpstr, "%s,%s,%s", GlobalOptions[i], 
				 RCMgetDisplaycmd(rcm), Responses[i]);
			 
		    RCMAfromLine(GlobalRC->commands, tmpstr);
	       }
	       ChangedDefs = TRUE;
	  }
	  break;
     case 3:
	  /** Add a new application **/
	  
	  for (i=0; i < MAXRESP; i++)
	       if (Responses[i] != NULL)
		    *(Responses[i]) = '\0';
	  
	  if (CURRequest(CursesScreen, "Add a new Application Type", 
			 OptionNewApp, Responses) == 0) {
	       char tmpstr[512];
	       
	       sprintf(tmpstr, "%s,%s,%s", Responses[0], Responses[1],
		       Responses[2]);
	       RCMAfromLine(GlobalRC->commands, tmpstr);
	       ChangedDefs = TRUE;
	  }
	  break;
	  
     }  /** End of switch on option type **/
}



GopherDirObj *
GDdeleteGS(gd,j) 
	GopherDirObj	*gd;
	int		j;	/* Number of GS to delete */
/* Return 1 if last item - handle delete with care outside of this */
{
	       GopherDirObj *tempgd;
	       int i;
	       if (GDgetNumitems(gd) == 1) {
		    /* Last item in the directory */
		    return(NULL);
		}

	       tempgd = GDnew(GDgetNumitems(gd)+1);

	       for (i=0; i<GDgetNumitems(gd); i++) {
		    if (i != j)
			 GDaddGS(tempgd, GDgetEntry(gd, i));
	       }
	       GDsetTitle(tempgd, GDgetTitle(gd));

	       if (GDgetCurrentItem(gd) > GDgetNumitems(tempgd))
		    GDsetCurrentItem(tempgd, GDgetNumitems(tempgd));
	       else
	            GDsetCurrentItem(tempgd, GDgetCurrentItem(gd));

	       GDdestroy(gd);
	       return(tempgd);
}

int
main(argc, argv)
  int argc;
  char *argv[];
{
     BOOLEAN bDone = FALSE;
     char sTmp[80];
     GopherStruct *RootGophers[2];
     int numhosts = 2;
     int TypedChar;
     /*** for getopt processing ***/
     int c;
     extern char *optarg;
     extern int optind;
     int errflag =0, i;

     int Garbled = TRUE;
     boolean Bkmarksfirst = FALSE;
     

#ifdef VMS
	  argv[0] = "gopher";
#endif

	/* DEBUG=TRUE; */
     Debug("main:A\r\n",NULL)
     for (i=0; i<2; i++) {
	  RootGophers[i] = GSnew();
	  GSsetType (RootGophers[i], A_DIRECTORY);   
	  GSsetPath (RootGophers[i],"");
     }
     
     Debug("main:B\r\n",NULL)
     /** Should generalize this to >2 hosts .... Sigh... ***/
     GSsetHost (RootGophers[0], CLIENT1_HOST);
     GSsetPort (RootGophers[0], CLIENT1_PORT);

     GSsetHost (RootGophers[1], CLIENT2_HOST);
     GSsetPort (RootGophers[1], CLIENT2_PORT);

     if (CLIENT2_PORT == 0)
	  numhosts = 1;

     sTmp[0] = '\0';

     while ((c = getopt(argc, argv, "DsSbrp:t:T:")) != -1)
	  switch (c) {
	  case 's':
	       SecureMode = TRUE;
	       break;
	  case 'S':	/* Similar to secure, but assumes own Unix account */
			/* No change in behaviour if this not set */
		NoShellMode = TRUE;
		break;
	  case 'p':
	       GSsetPath(RootGophers[0], optarg);
	       GSsetPath(RootGophers[1], optarg);
	       break;
	  case 'T':
	       GSsetType(RootGophers[0], *optarg);
	       GSsetType(RootGophers[1], *optarg);
	       break;
	  case 't':
	       GSsetTitle(RootGophers[0], optarg);
	       GSsetTitle(RootGophers[1], optarg);
	       break;
	  case 'D':
	       DEBUG = TRUE;
	       Debug("gopher starting - debug on\n",NULL)
	       break;
	  case 'r':
	       RemoteUser = TRUE;
	       break;
	  case 'b':
	       Bkmarksfirst = TRUE;
	       break;
	  case '?':
	       errflag++;
	  }


     if (errflag) {
	  
	  fprintf(stderr, "Usage: %s [-sSbDr] [-T type] [-p path] [-t title] [hostname port]+\n", argv[0]);
	  fprintf(stderr, "     -s      secure mode, users without own account\n");
	  fprintf(stderr, "     -S      secure mode, users with own account\n");
	  fprintf(stderr, "     -p path specify path to initial item\n");
	  fprintf(stderr, "     -T      Type of initial item\n");
	  fprintf(stderr, "     -b      Bookmarks first\n");
	  fprintf(stderr, "     -r      Remote user\n");
	  fprintf(stderr, "     -D      Debug mode\n");
	  exit(-1);
     }

     /**** Get host #1 from the command line ****/
     
     if (optind < argc) {
	  GSsetHost(RootGophers[0], argv[optind]);
	  GSsetHost(RootGophers[1], "");  /** Nuke the 2nd alternative host **/
	  GSsetPort(RootGophers[1], 0);
	  numhosts = 1;
	  optind++;
     }
     if (optind < argc) {
	  GSsetPort(RootGophers[0], atoi(argv[optind]));
	  optind++;
     }

     /*** Get host #2 from the command line... ***/
     if (optind < argc) {
	  GSsetHost(RootGophers[1], argv[optind]);
	  numhosts = 2;
	  optind++;
     }
     if (optind < argc) {
	  GSsetPort(RootGophers[1], atoi(argv[optind]));
	  optind++;
     }

     /*** If the title hasn't been set, then add a default title **/
     if (GSgetTitle(RootGophers[0]) == NULL) {
	  sprintf(sTmp, "Root gopher server: %.59s", GSgetHost(RootGophers[0]));
	  GSsetTitle(RootGophers[0], sTmp);
	  sprintf(sTmp, "Root gopher server: %.59s", GSgetHost(RootGophers[1]));
	  GSsetTitle(RootGophers[1], sTmp);
     }

     /*** Set up global variables, etc. ***/

     Initialize();

     if (Bkmarksfirst) {
	  CurrentDir = RCgetBookmarkDir(GlobalRC);
	  
	  if (CurrentDir != NULL)
	       GDaddGS(CurrentDir, RootGophers[0]);
     } else {
	  int rnum;

	  srand(time(NULL));
	  rnum = rand() % numhosts;
	  process_request(RootGophers[rnum]);

          /* just process the command line entry */
          if( GSgetType(RootGophers[0]) != A_DIRECTORY ) {
	       refresh();
	       CURexit(CursesScreen);
	       exit(0);
          }
     }

#ifdef DEBUGGING
	if (CurrentDir == NULL)  {
	     Debug("CurrentDir=NULL\n",NULL);
	}
	else
	     if (GDgetNumitems(CurrentDir) <=0) {
		  Debug("GDgetNumitems<=0\n", NULL);
	     }
#endif

     if (CurrentDir == NULL || GDgetNumitems(CurrentDir) <= 0) {
	  /*
	   * We didn't get anything from that gopher server.  Either
	   * it is down, doesn't exist, or is empty or otherwise
	   * busted.
           *
           * Try any alternative hosts that are available..
	   */

	  while (optind < argc && CurrentDir == NULL) {
	       Debug("Trying alternate site\n",0);
	       GSsetHost(RootGophers[0], argv[optind]);
	       optind++;
	       GSsetPort(RootGophers[0], atoi(argv[optind]));
	       optind++;
	       process_request(RootGophers[0]);
	  }
     }	  

     if (CurrentDir == NULL || GDgetNumitems(CurrentDir) <= 0) {
#ifdef DEBUGGING
	if (CurrentDir == NULL)  {
	     Debug("B: CurrentDir=NULL\n",0);
	}
	else if (GDgetNumitems(CurrentDir) <=0) {
	     Debug("B: GDgetNumitems<=0\n",0);
	}
#endif	

	CURexit(CursesScreen);
	fprintf(stderr,
		"%s: Nothing received for main menu, can't continue\n", argv[0]);
	exit(1);
   }

     while (bDone == FALSE)
     {
	  /* Set up a longjmp for control-cs **/
#ifdef VMS
	  setjmp(Jmpenv);
#else
	  if (setjmp(Jmpenv))
	       CursesErrorMsg("Interrupted...");
#endif
#if defined(VMS) && defined(__ALPHA)
	 /*
	  *  Temporary workaround for problems with
	  *  Curses library functions on Alphas.
	  */
	  CURexit(CursesScreen);
	  CURenter(CursesScreen);
#endif

	  GetMenu(CurrentDir, &TypedChar, Garbled);

	  Garbled = TRUE;

	  switch(TypedChar)
	  {
	  case '\r':
	  case '\n':
	       /*** Select the designated item ***/
	       if (process_request(GDgetEntry(CurrentDir,GDgetCurrentItem(CurrentDir)-1))==1)
		    ;  /** Don't do anything if we failed, I guess **/
	       break;
	       
	  case '\014': /* ^L */
#ifdef VMS
	  case '\022': /* ^R */
	  case '\027': /* ^W */
	       CURexit(CursesScreen);
	       CURenter(CursesScreen);
#endif
	       /*** Redraw the screen ***/
	       break;

	  case '\0':
	       /*** What the heck? ***/
	       CursesErrorMsg("Strange Error occurred!");
	       break;
	       
	  case 'u':
	  case 'U': 
#ifdef AUTOEXITONU
	  case 'q':
#endif
	  {
	       GopherDirObj *tempGdir;

	       /*** Don't highlight texts which aren't search hits ***/
	       Searchstring = NULL;

	       /*** Go up a directory level ***/
	       tempGdir = CurrentDir;
	       /** Don't destroy root level directory, or bookmarks **/
	       if (popgopher(&CurrentDir)==0 && tempGdir != CurrentDir) {
		    if (tempGdir != RCgetBookmarkDir(GlobalRC))
			 GDdestroy(tempGdir);
#ifdef AUTOEXITONU
	       } else {
	       		bDone = TRUE;
	       		CURexit(CursesScreen);
#endif
	       }
	  }
	       break;

	  case 'g':
	       /*** Gripe! ***/
	       Gripe(GDgetEntry(CurrentDir,GDgetCurrentItem(CurrentDir)-1));
	       break;

	  case 's':  /*** Save a file directly ***/
	       if (SecureMode || NoShellMode) {
		    CursesErrorMsg("Sorry, can't save files in securemode");
		    break;
	       }
	       Save_file(GDgetEntry(CurrentDir,GDgetCurrentItem(CurrentDir)-1), NULL,NULL);
	       break;
	       
	  case 'D':
	       Download_file(GDgetEntry(CurrentDir,GDgetCurrentItem(CurrentDir)-1));
	       break;

	  case 'o': /** Open a new session **/
	  {
	       GopherObj *tempgs = GSnew();
	       char *prompts[4];
	       char *responses[4];
	       int i;

	       if (SecureMode) {
		    CursesErrorMsg("Sorry, you're not allowed to do this");
		    break;
	       }

	       prompts[0]="Hostname";
	       prompts[1]="Port";
	       prompts[2]="Selector (Optional)";
	       prompts[3]=NULL;
	       
	       for (i=0; i<3; i++) {
		    responses[i] = (char*)malloc(80*sizeof(char));
		    *responses[i] = '\0';
	       }
	       strcpy(responses[1], "70");
		    
	       responses[3] = NULL;
	       
	       if ((CURRequest(CursesScreen, "Connect to a new Gopher Server",
			      prompts, responses) == -1) || (responses[0] == '\0'))
		    
		    break;

	       GSsetType(tempgs, A_DIRECTORY);
	       GSsetTitle(tempgs, responses[0]);
	       GSsetHost(tempgs, responses[0]);
	       GSsetPort(tempgs, atoi(responses[1]));
	       GSsetPath(tempgs, responses[2]);
	       
	       Load_Dir(tempgs);

	       GSdestroy(tempgs);
	       for (i=0; i<3; i++)
		    free(responses[i]);
	       break;
	  }
	       

	  case 'v':  /** View bookmark list **/
	  {
	       if (RCgetBookmarkDir(GlobalRC) == NULL) {
		    CursesErrorMsg("No bookmarks are defined");
		    break;
	       }

	       /** Don't push an empty gopher directory... **/
	       if (CurrentDir != NULL)
		    pushgopher(CurrentDir); 
	       
	       CurrentDir = RCgetBookmarkDir(GlobalRC);

	       break;
	  }	       

	  case 'a': /** add current item as a bookmark **/
	  {
	       GopherObj *tmpgs;
	       char newtitle[256];
	       
	       if (RCgetBookmarkDir(GlobalRC) == NULL) {
 		    RCsetBookmarkDir(GlobalRC,GDnew(32));
		    GDsetTitle(RCgetBookmarkDir(GlobalRC), "Bookmarks");
	       }

	       tmpgs = GSnew();
	       GScpy(tmpgs, GDgetEntry(CurrentDir, GDgetCurrentItem(CurrentDir)-1));
	       
	       strcpy(newtitle, GSgetTitle(tmpgs));
	       if (CURGetOneOption(CursesScreen, "Name for this bookmark? ", newtitle) <0)
		    break;
	       if (*newtitle == '\0')
		    break;

	       GSsetTitle(tmpgs, newtitle);
	       GDaddGS(RCgetBookmarkDir(GlobalRC), tmpgs);
	       GSdestroy(tmpgs);

	       ChangedDefs = TRUE;

	       break;
	  }

	  case 'A': /*** Add current directory/search as a bookmark **/
	  {
	       GopherObj *tmpgs;
	       char newtitle[256];

	       if (RCgetBookmarkDir(GlobalRC) == NULL) {
		    RCsetBookmarkDir(GlobalRC,GDnew(32));
		    GDsetTitle(RCgetBookmarkDir(GlobalRC), "Bookmarks");
	       }

	       if (CurrentDir == RCgetBookmarkDir(GlobalRC)) {
		    CursesErrorMsg("Sorry, can't make a bookmark of bookmarks");
		    break;
	       }
	       
	       tmpgs = GSnew();
	       if (iLevel == 0)
		    GScpy(tmpgs, RootGophers[0]);
	       else
		    GScpy(tmpgs, GDgetEntry(OldDirs[iLevel-1], 
					    GDgetCurrentItem(OldDirs[iLevel-1])-1));
	       
	       strcpy(newtitle, GDgetTitle(CurrentDir));
	       if (CURGetOneOption(CursesScreen, "Name for this bookmark? ", newtitle)<0)
		    break;
	       if (*newtitle == '\0')
		    break;

	       GSsetTitle(tmpgs, newtitle);

	       /*** Freeze the search, if there was one. ***/
	       if (GSgetType(tmpgs) == '7') {
		    char ickypath[512];
		    strcpy(ickypath, GSgetPath(tmpgs));
		    strcat(ickypath, "\t");
		    strcat(ickypath, Searchstring);
		    GSsetPath(tmpgs, ickypath);
		    GSsetType(tmpgs, '1');
	       }

	       GDaddGS(RCgetBookmarkDir(GlobalRC), tmpgs);

	       ChangedDefs = TRUE;

	       break;
	  }

	  case 'd':  /*** Delete a bookmark ***/
	  {
	       GopherDirObj *tempgd;

		tempgd=GDdeleteGS(CurrentDir,(GDgetCurrentItem(CurrentDir)-1));
		if (tempgd == NULL) {
		    Debug("Last item - pip up a directory",NULL)
		    tempgd = CurrentDir;
		    if (popgopher(&CurrentDir)==0 && tempgd != CurrentDir) {
			Debug("Don't destroy root level directory, or bookmarks",NULL) 
			 if (tempgd != RCgetBookmarkDir(GlobalRC)) {
			      GDdestroy(tempgd);
			 }
		    } else {
			 CursesErrorMsg("Sorry, can't delete top level directory.");
			 scline(-1, 1, CurrentDir);
		    }
		    ChangedDefs = TRUE;
	        } else {
	       if (CurrentDir == RCgetBookmarkDir(GlobalRC))
		    RCsetBookmarkDir(GlobalRC, tempgd);
	       
	       CurrentDir = tempgd;
	       
	       ChangedDefs = TRUE;
		}
	       break;
	  }

	  case '$':
	  case '!':
	       if (SecureMode || NoShellMode) {
		    CursesErrorMsg("Sorry, can't spawn in securemode");
		    break;
	       }
	       /*** Spawn to shell or DCL ***/
	       CURexit(CursesScreen);
#ifdef VMS
	       printf("Spawning DCL subprocess.  Logout to return to Gopher.\n");
	       fflush(stdout);
	       i = system("");
#else
	       printf("Spawning your default shell.  Use 'exit' to return to Gopher.\n\n");
		    fflush(stdout);
		    system(getenv("SHELL"));
#endif
		    CURenter(CursesScreen);
#ifdef VMS
	       if (i != SS$_NORMAL)
		    CursesErrorMsg("Spawn to DCL failed!");
#endif
	       break;
	       

	       /*** Pop back to the main menu ***/
	  case 'M':
	  case 'm': 
	  {
	       GopherDirObj *tempGdir = NULL;
	       
	       while (popgopher(&CurrentDir) != -1) {
		    if (tempGdir != NULL)
			 GDdestroy(tempGdir);
		    tempGdir = CurrentDir;
	       }

	  }

	       break;

#ifndef AUTOEXITONU
	  case 'q':
	       /*** Quit the program ***/
	       if (TRUE == ReallyQuit()) {
		    bDone = TRUE;
		    CURexit(CursesScreen);
		    break;
	       }

	       break;
#endif /*AUTOEXITONU*/
#ifdef VMS
	  case '\032': /* ^Z */
#endif
	  case 'Q':
	       /*** Quit the program, don't ask ***/
	       bDone = TRUE;
	       CURexit(CursesScreen);
	       break;
	       
	  case 'O':
	       /*** Change various program things ***/
	       SetOptions();
	       break;
		      
	  case '=':
	       describe_gopher("Gopher Item Information", 
			       GDgetEntry(CurrentDir, GDgetCurrentItem(CurrentDir)-1));
	       break;

	  case '^':
	  {
	       if (iLevel == 0)
		    describe_gopher("Gopher Directory Information",
				    RootGophers[0]);
	       else
		    describe_gopher("Gopher Directory Information",
				    GDgetEntry(OldDirs[iLevel-1],
					       GDgetCurrentItem(OldDirs[iLevel-1])-1));
	       break;
	  }
			       

	  case '?':
	  {
	       /*** Display help file ***/
	       GopherObj *tmpgs;

	       tmpgs = GSnew();
	       GSsetType(tmpgs, A_FILE);
	       GSsetTitle(tmpgs, "Gopher Help File");
	       GSsetLocalFile(tmpgs, GOPHERHELP);
	       
	       CURexit(CursesScreen);
	       showfile(tmpgs);
	       CURenter(CursesScreen);

	       GSsetLocalFile(tmpgs, NULL);
	       GSdestroy(tmpgs);
	       break;
	  }

	  default:
	       Debug("main beep %d\r\n",TypedChar)
	       CURBeep(CursesScreen);
	       Garbled = FALSE;
	       break;

	  } /* switch */
     }      /* while */

     CleanupandExit(0);

}


int
Load_Index(ZeGopher)
  GopherObj *ZeGopher;
{
     Draw_Status("Searching Text...");
     refresh();

     return(Load_Index_or_Dir(ZeGopher, Searchstring));
}

int
Load_Dir(ZeGopher)
  GopherObj *ZeGopher;
{
     Searchstring= NULL;
     return(Load_Index_or_Dir(ZeGopher, NULL));
}


int
Load_Index_or_Dir(ZeGopher, Searchmungestr)
  GopherObj *ZeGopher;
  char      *Searchmungestr;
{
     int failed = 0;
     int sockfd;
     int i, numbytes;
     static char DirTitle[512];
     GopherDirObj *NewDir = NULL;

     DebugGSplusPrint(ZeGopher,"Load_Index_or_Dir");
     NewDir = GDnew(32);


     Draw_Status("Connecting..."); refresh();

     if ((sockfd = GSconnect(ZeGopher)) <0) {
	  check_sock(sockfd, GSgetHost(ZeGopher), GSgetPort(ZeGopher));
	  failed = 1;
     }
     else {
	  if (GSgetType(ZeGopher) == A_DIRECTORY) {
	       Draw_Status("Retrieving Directory..."); refresh();
	       GStransmit(ZeGopher, sockfd, NULL, "$", NULL);
	  }
	  else if (GSgetType(ZeGopher) == A_INDEX) {
	       Draw_Status("Searching..."); refresh();
	       GStransmit(ZeGopher, sockfd, Searchmungestr, "$", NULL);
	  }

	  numbytes = GSrecvHeader(ZeGopher, sockfd);

	  if (numbytes == 0) {

	       Gplus_Error(sockfd);
	       failed = 1;
	       return(failed);
	  }

	  if (GSisGplus(ZeGopher))
	       GDplusfromNet(NewDir, sockfd, twirl);
	  else
	       GDfromNet(NewDir, sockfd, twirl);

	  if (GDgetNumitems(NewDir) <= 0) {
	       CursesErrorMsg("Nothing available.");
	       failed = 1;
	       GDdestroy(NewDir);
	       closenet(sockfd);
	       return(failed);
	  }

	  if (GSgetType(ZeGopher) == A_INDEX) {
	       sprintf(DirTitle, "%s: %s", GSgetTitle(ZeGopher), Searchmungestr);
	       GDsetTitle(NewDir, DirTitle);
	  }
          else
	       GDsetTitle(NewDir, GSgetTitle(ZeGopher));

	  /** Don't push an empty gopher directory... **/
	  if (CurrentDir != NULL)
	       pushgopher(CurrentDir); 

	  CurrentDir = NewDir;
	  
     }
     i = closenet(sockfd);
     return(failed);
}

/*
 * This dispathes the appropriate fcns depending on what the object is.. 
 */

int
process_request(ZeGopher)
  GopherObj *ZeGopher;
{
     int failed=0;
     char **askdata = NULL;

     if (GSisAsk(ZeGopher)) {
	  char **askdata = AskBlock(ZeGopher);
	  
	  if (askdata == NULL)
	       return;
	  GSsetAskdata(ZeGopher, askdata);
     }

     /** Decide what to do with it.. **/

     Debug("process_request type=%c\n",GSgetType(ZeGopher));

     switch(GSgetType(ZeGopher)) {
     case -1:
	  break;

     case A_FILE:
	  Draw_Status("Receiving Information...");
	  refresh();
	  showfile(ZeGopher);
	  break;

     case A_GIF:
     case A_IMAGE:
	  Draw_Status("Receiving Information...");
	  refresh();
	  if (RemoteUser)
		Download_file(ZeGopher);
	  else
		showfile(ZeGopher);
	  break;
     case A_MIME:
	  if (!SecureMode) {
	       Draw_Status("Receiving Information...");
	       refresh();
	       showfile(ZeGopher);
	  }
	  else
	       CursesErrorMsg("Sorry Cannot display this file anonymously");
	  break;

     case A_MACHEX:
     case A_PCBIN:
     case A_UNIXBIN:
	if (RemoteUser) {
		Draw_Status("Receiving Information...");
		refresh();
		Download_file(ZeGopher);
	}
	else
	  if (!SecureMode || NoShellMode)
	       Save_file(ZeGopher, NULL);
	  else
	       CursesErrorMsg("Sorry, cannot transfer files in securemode");
	  break;
	  
     case A_DIRECTORY:
	  Draw_Status("Receiving Directory...");
	  refresh();
	  failed = Load_Dir(ZeGopher);
	  break;
		    
     case A_TELNET:
     case A_TN3270:
	  do_tel_3270(ZeGopher);
	  break;

     case A_INDEX:
	  refresh();
	  Searchstring = do_index(ZeGopher);
	  Draw_Status("Searching Text...");
	  if (Searchstring != NULL)
	       failed=Load_Index(ZeGopher);
	  else
	       failed = 1;
	  break;
	  
     case A_CSO:
	  do_cso(ZeGopher);
	  break;

     case A_SOUND:
	  Draw_Status("Receiving Sound...");
	  refresh();
	  do_sound(ZeGopher);
	  break;

     case A_HTML:
	  Draw_Status("Receiving HTML page...");
	  refresh();
	  do_html(ZeGopher);
	  break;
     }
     return(failed);
}


/*
 * Provide information about a gopher item...
 * Should use curses...  But....
 */

void
describe_gopher(banner, ZeGopher)
  char *banner;
  GopherStruct *ZeGopher;
{
     char *tmpfilename;
     FILE *tmpfile;
     Blockobj *bl;
     int i,j, views;
     GopherObj *infogs;

     infogs = GSnew();
     GSsetType(infogs, A_FILE);
     GSsetTitle(infogs, "Link Info");

     CURexit(CursesScreen);	/* do this *before* possible exit() below */

     GSgetginfo(ZeGopher);

     Gopenfile = tmpfilename = tempnam("/tmp", "gopher");
     GSsetLocalFile(infogs, tmpfilename);

     if ((tmpfile = fopen(tmpfilename, "w")) == NULL)
	   fprintf(stderr, "Couldn't make a tmp file!\n"), exit(-1);

     free(tmpfilename);

     GStoLink(ZeGopher, fileno(tmpfile));

     if (GSisGplus(ZeGopher)) {
	  GopherObj *server;

	  server = GSnew();
	  GScpy(server, ZeGopher);
	  GSsetPath(server, "");

	  fputc('\n', tmpfile);
	  /** Search out for specific blocks **/
	  for (i=0; i<GSgetNumBlocks(ZeGopher); i++) {
	       if (strcmp(BLgetName(GSgetBlock(ZeGopher, i)),"ABSTRACT")==0){
		    bl = GSgetBlock(ZeGopher, i);
		    fprintf(tmpfile, "%s\n---------\n\n",BLgetName(bl));
		    for (j=0; j < BLgetNumLines(bl); j++) {
			 fprintf(tmpfile, "%s\n", BLgetLine(bl, j));
		    }
		    fprintf(tmpfile, "\n");
	       }
	  }

	  fprintf(tmpfile, "Size       Language      Document Type\n");
	  fprintf(tmpfile, "---------- ------------- ----------------------------\n");
	  
	  for (views=0; views< GSgetNumViews(ZeGopher); views++) {
	       char *cp; 

	       cp = VIgetSize(GSgetView(ZeGopher,views)); 
	       if (cp != NULL)
		    fprintf(tmpfile, "%s", cp);
	       else
		    cp = "";

	       for (i=strlen(cp); i<11; i++)
		    fputc(' ', tmpfile);

	       
	       cp = VIprettyLang(GSgetView(ZeGopher,views), "En_US"); 
	       if (cp != NULL)
		    fprintf(tmpfile, "%s", cp);
	       else
		    cp = "";
	       for (i=strlen(cp); i<14; i++)
		    fputc(' ', tmpfile);

	       cp = VIgetType(GSgetView(ZeGopher,views)); 
	       fprintf(tmpfile, "%s\n", cp);
	  }

	  fprintf(tmpfile, "\n\nServer Information\n");
	  GSdestroy(server);
     }

     fclose(tmpfile);

     showfile(infogs);

     CURenter(CursesScreen); /* do this after unlink fails */

     GSdestroy(infogs);
     return;
}
