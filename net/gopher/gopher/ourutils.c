/********************************************************************
 * lindner
 * 3.26
 * 1993/08/19 20:51:52
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopher/ourutils.c,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992,1993 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: ourutils.c
 * stuff that doesn't really fit anywhere else.
 *********************************************************************
 * Revision History:
 * ourutils.c,v
 * Revision 3.26  1993/08/19  20:51:52  lindner
 * secure patch from mitra
 *
 * Revision 3.25  1993/08/19  20:30:39  lindner
 * Fix problem with selecting ask item more than once
 *
 * Revision 3.24  1993/08/19  20:22:56  lindner
 * Mitra's Debug patch
 *
 * Revision 3.23  1993/08/16  18:01:10  lindner
 * Fix for VMSfile, mods for DEC alpha VMS
 *
 * Revision 3.22  1993/08/16  17:58:24  lindner
 * Removed REMOTEUSER ifdefs
 *
 * Revision 3.21  1993/08/06  14:39:57  lindner
 * One more small security patch
 *
 * Revision 3.20  1993/08/05  20:40:43  lindner
 * Added warning message..
 *
 * Revision 3.19  1993/08/04  22:07:23  lindner
 * Use /bin/mail instead of ucbmail
 *
 * Revision 3.18  1993/08/03  04:43:59  lindner
 * Fix for VMS unresolved variables
 *
 * Revision 3.17  1993/07/30  17:31:56  lindner
 * Mods to support AskP:, AskL:
 *
 * Revision 3.16  1993/07/29  17:20:19  lindner
 * eliminate non-used variables
 *
 * Revision 3.15  1993/07/27  06:14:17  lindner
 * Bug fix, use readrecvbuf, not readn
 *
 * Revision 3.14  1993/07/27  05:28:57  lindner
 * Mondo Debug overhaul from Mitra
 *
 * Revision 3.13  1993/07/27  02:04:13  lindner
 * More comments
 *
 * Revision 3.12  1993/07/26  20:30:05  lindner
 * fix memory usage
 *
 * Revision 3.11  1993/07/26  15:35:28  lindner
 * Fix for Unixfile to remove parens
 *
 * Revision 3.10  1993/07/23  04:38:43  lindner
 * Mods to make client view selection pretty
 *
 * Revision 3.9  1993/07/20  23:14:59  lindner
 * Mods to cache askdata..
 *
 * Revision 3.8  1993/07/07  19:43:42  lindner
 * Added TPU support for pager in VMS
 *
 * Revision 3.7  1993/06/29  06:18:52  lindner
 * recasting MIME types
 *
 * Revision 3.6  1993/04/15  21:27:26  lindner
 * NOMAIL option, Remote user stuff, and bug in Ask
 *
 * Revision 3.5  1993/03/26  19:44:36  lindner
 * Fix for CSO stuff and saving binary files
 *
 * Revision 3.4  1993/03/24  17:01:27  lindner
 * Major changes to the way things are displayed/saved
 *
 * Revision 3.3  1993/03/18  23:32:41  lindner
 * Added Note: support and default values to AskBlock
 *
 * Revision 3.2  1993/02/19  21:12:16  lindner
 * Generalized on gopher+ stuff, strip long filenames to reasonable length
 *
 * Revision 3.1.1.1  1993/02/11  18:02:58  lindner
 * Gopher+1.2beta release
 *
 * Revision 2.1  1993/02/09  22:37:50  lindner
 * Much new in the way of display routines.
 *
 * Revision 1.11  1993/01/31  00:10:44  lindner
 * Checks for multiple views when saving.
 *
 * Revision 1.10  1993/01/17  04:30:38  lindner
 * VMSfile() truncates to a legal VMS filename length.
 *
 * Revision 1.9  1993/01/14  21:16:58  lindner
 * New routines to make nice filenames for Unix and VMS.
 * Add return to make screen look nice for VMS pagers.
 *
 * Revision 1.8  1993/01/12  21:45:17  lindner
 * Removed \n from system() calls.
 * Added filter to Save_file for directories and searches
 *
 * Revision 1.7  1993/01/12  18:19:32  lindner
 * Fixed Save_file call in display_file
 *
 * Revision 1.6  1993/01/09  01:37:32  lindner
 * Changed Save_file so that it takes a default filename.
 * Removed some dead code...
 *
 * Revision 1.5  1992/12/31  05:50:03  lindner
 * Added mods for VMS
 *
 * Revision 1.4  1992/12/11  21:19:51  lindner
 * Really fixed RealName this time..  Gotta test these things..
 *
 * Revision 1.3  1992/12/11  20:59:48  lindner
 * Fixed problem with syntax error RealName, grrr
 *
 * Revision 1.2  1992/12/11  20:54:38  lindner
 * Mailing items with quotes in their name now works, added fflush(stdin)
 * everywhere to fix some input processing problems.
 *
 * Revision 1.1  1992/12/10  23:32:16  lindner
 * gopher 1.1 release
 *
 *********************************************************************/


#include "gopher.h"
#ifndef VMS
#include <pwd.h>
#endif
#ifdef mips
char *getenv();
#endif
#include "Debug.h"

extern int twirl();

int 
outchar(c)
  char c;
{
        /** output the given character.  From tputs... **/
        /** Note: this CANNOT be a macro!              **/

        putc(c, stdout);
	return(c);
}


/*
 * Function that twirls on the screen..
 *
 * pct is an optional val that is to be displayed.
 */

int
twirl(pct)
  int pct;
{
     static int twirlnum = 0;
     static char *twirls = "-/|\\";

     addch('\b');
     addch(*(twirls + (twirlnum++ % 4 )));
     refresh();

     return(0);
}



/*
** This procedure sends a file through the mail.
*/

void
mail_file(Filename, Realname)
  char *Filename, *Realname;
{
     static char *SaveName = NULL;
     char command[512];
     FILE *infile, *mailit;
     char buf[512];
     
     if (SaveName==NULL) {
	  if ((SaveName = (char *) malloc(sizeof(char)*256)) == NULL)
	       perror("Out of memory!");
	  *SaveName = '\0';
     }

     if (CURGetOneOption(CursesScreen, "Mail current document to:", SaveName)<0)
	  return;
     
     if (*SaveName == '\0')
	  return;

/*
 * You should be very careful when you add characters to this list..
 * 
 * It could create a nasty security hole!
 */
     
#define ACHARS "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789%.@!-_"
		    
     if (SecureMode)
	  if ( strspn(SaveName, ACHARS) != strlen(SaveName)||
	      *SaveName == '-') {
	       Debug("mail_file - beep - bad char",NULL)
	       puts(CURgetBell(CursesScreen));
	       fflush(stdout);
	       Draw_Status("Address syntax rejected...");
	       refresh();
	       return;
	  }

     if (strchr(Realname, '\"') != NULL)
	  Realname = "Gopher File";
     
     Draw_Status("Mailing File...");
     refresh();
     
#ifdef VMS
     sprintf(command, "%s/subject=\"%s\" %s %s", 
           MAIL_COMMAND, Realname, Filename, SaveName);
     system(command);
#else
     sprintf(command, "%s %s", 
	     MAIL_COMMAND, SaveName);
     
     infile = fopen(Filename, "r");
     mailit = popen(command, "w");
     
     fprintf(mailit, "Subject: %s\n\n", Realname);

     while (readline(fileno(infile), buf, sizeof(buf))) {
	  twirl();
	  fprintf(mailit, "%s", buf);
     }

     pclose(mailit);
     fclose(infile);

#endif

}

/*
 *  This fcn allows one to choose a view out of a potential nine views.
 *  returns false if fails
 */

char *
Choose_View(gs)
  GopherObj *gs;
{
     int viewno;
     static char viewnlang[64];
     char *view;

     if (GSisGplus(gs)) {
	  char *choices[10], *views[10];

	  GSgetginfo(gs);
	  
	  if (GSgetNumViews(gs) >1) {
	       int choice, default_choice = 0;

	       for (viewno=0; viewno <GSgetNumViews(gs); viewno++) {
		    views[viewno] =  strdup(VIgetViewnLang(GSgetView(gs, 
					    viewno), viewnlang));

		    choices[viewno] = strdup(VIgetPrettyView(GSgetView(gs,viewno),viewnlang));
		    if (!strcmp(VIgetType(GSgetView(gs, viewno)), "text/plain"))
			 default_choice = viewno;
	       }

	       choices[viewno] = NULL;
	       choice = CURChoice(CursesScreen, GSgetTitle(gs), choices,
				  "Choose a document type", default_choice);
	       Debug("Choice was %d\n", choice);
	       if (choice == -1)
		    return(NULL);

	       sprintf(viewnlang, "%s", views[choice]);

	       for (viewno = 0; viewno <GSgetNumViews(gs); viewno++)  {
		    free(views[viewno]);

		    free(choices[viewno]);

	       }
	       return(viewnlang);
	  }
	  else if (GSgetNumViews(gs) == 1)
	       return(VIgetViewnLang(GSgetView(gs, 0), viewnlang));
	  else
	       return("");
     }
     else {
	  switch (GSgetType(gs)) {
	  case A_FILE:
	  case A_CSO:
	       view = "text/plain";
	       break;
	  case A_GIF:
	       view = "image/gif";
	       break;
	  case A_IMAGE:
	       view = "image";
	       break;
	  case A_SOUND:
	       view = "audio/basic";
	       break;
	  case A_TN3270:
	       view = "terminal/tn3270";
	       break;
	  case A_TELNET:
	       view = "terminal/telnet";
	       break;
	  case A_UNIXBIN:
	  case A_PCBIN:
	  case A_MACHEX:
	       view = "Binary";
	       break;
	  }
	  return(view);
     }
}

/*
 * Crude implementation of the ASK block stuff.
 */

char **
AskBlock(gs)
  GopherObj *gs;
{
     int Asknum;
     char *choices[20];
     Blockobj *bl;
     char **responses;
     char **asktypes;
     int  *curtypes;
     char askline[256];
     char *defaultval;
     int i,j;

     GSgetginfo(gs);
	  
     bl = GSfindBlock(gs, "ASK");

     if (bl == NULL)
	  return(NULL);

     if ((responses = GSgetAskdata(gs)) == NULL) {
	  responses = (char **) malloc(25 * sizeof(char*));
     }
     asktypes  = (char **) malloc(25 * sizeof(char*));
     curtypes  = (int *)   malloc(25 * sizeof(int));

     for (Asknum=0; Asknum <BLgetNumLines(bl); Asknum++) {
	  char *askprompt, *cp;

	  strcpy(askline, BLgetLine(bl, Asknum));
	  /*** find the type of question ***/
	  askprompt = strchr(askline, ':');
	  if (askprompt == NULL) {
	       continue;
	  }
	  *(askprompt+1) = '\0';
	  askprompt+=2;
	  
	  /*** Zap the tabs, and load it up.. ***/
	  cp = strchr(askprompt, '\t');
	  if (cp != NULL) {
	       defaultval = cp+1;
	       *cp = '\0';
	  } else
	       defaultval = NULL;

	  asktypes[Asknum] = strdup(askline);
	  choices[Asknum] = strdup(askprompt);

	  if (strncasecmp(askline, "Note:", 5) == 0)
	       curtypes[Asknum] = CUR_LABEL;
	  else if (strncasecmp(askline, "AskP:", 5)==0) {
	       curtypes[Asknum] = CUR_PASSWD;
	  } else {
	       curtypes[Asknum] = CUR_PROMPT;
	  }

	  responses[Asknum] = (char *) malloc(40);
	  responses[Asknum][0] = '\0';
	  if (defaultval != NULL)
	       strncpy(responses[Asknum], defaultval, 39);
     }
     
     curtypes[Asknum] = NULL;
     asktypes[Asknum] = NULL;
     choices[Asknum] = NULL;
     responses[Asknum] = NULL;
     
     if (CURRequest_things(CursesScreen, GSgetTitle(gs), choices, responses,
			   curtypes))
	  return(NULL);
     else {
	  /*** Free memory ***/
	  free(curtypes);

	  for (i=Asknum-1; i >= 0; i--) {
	       if (choices[i] != NULL) free(choices[i]);
	       /** Okay, eliminate Note: entries block stuff **/
	       if (strcmp(asktypes[i], "Note:")==0)
		    /*** Remove it from the askdata.. **/
		    for (j=i; j<Asknum; j++)
			 responses[j] = responses[j+1];

	       else if (strcmp(asktypes[i], "AskL:")==0) {
		    /*** move the stuff up... ***/
		    for (j=Asknum; j>i; j--) {
			 responses[j] = responses[j-1];
		    }
		    /*** add '1' as the line count.. ***/
		    responses[i] = "1";  /*** Hack alert!!! ***/
		    responses[Asknum+1] = NULL;
	       }

	       if (asktypes[i] != NULL) free(asktypes[i]);
	  }
	  return(responses);
     }
}

/*
 * Handle a gplus error
 */
#define GERRMSGSIZE 25

void
Gplus_Error(sockfd)
  int sockfd;
{
     char inputline[256];
     char *errmsg[GERRMSGSIZE];
     int i;
     
     Debug("Gplus_Error on %d...", sockfd);
     for (i=0; i < GERRMSGSIZE;i++) {
	  char *cp;

	  if (readline(sockfd, inputline, sizeof(inputline)) <=0)
	       break;
	  ZapCRLF(inputline);
	  if (strcmp(inputline, ".")==0)
	       break;
	  
	  cp = inputline;
	  while ((cp = strchr(cp, '\t')) != NULL)
	       *cp = ' ';

	  errmsg[i] = strdup(inputline);
     }
     
     errmsg[i] = NULL;
     
     CURDialog(CursesScreen, "Gopher Transmission Error", errmsg);
     for (i=0; ; i++) {
	  if (errmsg[i] == NULL)
	       break;
	  else
	       free(errmsg[i]);
     }

}



/* Read from socket fd to file f until EOF */
boolean
CopyFile(fd, f)
  int fd;
  FILE *f;
{
     int numread;
     char buf[1024];

	       
     while ((numread = readrecvbuf(fd, buf, sizeof buf)) > 0) {
	  if (fwrite(buf, numread, 1, f) == 0) {
	       closenet(fd);
	       return(FALSE);
	  }
     }
}


/*
 * This fcn transfers a file from a gopher server into f.
 *
 */

boolean
GStoFile(gs, f, view, twirlfn)
  GopherObj *gs;
  FILE *f;
  char *view;
  int (*twirlfn)();
{
     int numread, sockfd;
     char buf[1024];
     int bytemethod;
     int line = 0, i;

     /*** Check for gopher+ and multiple views ***/

     Debug("GStoFile, view %s",view);
     if ((sockfd = GSconnect(gs)) <0) {
	  check_sock(sockfd, GSgetHost(gs), GSgetPort(gs));
	  return(FALSE);
     }

     /** Send out the request **/
     GStransmit(gs, sockfd, NULL, "+", view);
     bytemethod = GSrecvHeader(gs, sockfd);

     if (bytemethod == 0) {
	  Gplus_Error(sockfd);
	  
	  return(FALSE);
     }
	  

     Draw_Status("Receiving file...");
     refresh();
     

     if (GSisText(gs, view)) {
	  while (readline(sockfd, buf, sizeof(buf)) > 0) {
	       ZapCRLF(buf);
	       line ++;
	       if (*buf == '.' && *(buf+1) == '\0')
		    break;



	       if (GSgetType(gs) == A_CSO) {
		    if (*buf == '2')
			 break;
		    
		    if ((*buf >= '3') && (*buf <= '9'))  {
			 fprintf(f, "%s\n", GSgetPath(gs));
			 fprintf(f, "%s\n", buf+4);
			 break;
		    }
		    if (*buf == '-') {


			 if (buf[1] >= '3' && buf[1] <= '9') {
			      fprintf(f, "%s\n", GSgetPath(gs));
			      fprintf(f, "%s\n", buf+5);
			 }
			 else {
			      char *colonpos = strchr(buf+5,':');
			      if (colonpos != NULL && *(colonpos-1) != i) {
				   fprintf(f, "-------------------------------------------------------\n");
				   i = *(colonpos-1);
			      }
			      fputs((colonpos ? colonpos+1 : buf+6), f);
			      fputc('\n', f);
			 }
		    }
	       }
	       else {
		    fputs(buf, f);
		    putc('\n', f);
	       }
	       if ((line % 25) == 0)
		    twirlfn();
	  }
     }
     else {
	       
	  while ((numread = readrecvbuf(sockfd, buf, sizeof buf)) > 0) {
	       if (fwrite(buf, numread, 1, f) == 0) {
		    CursesErrorMsg("Problems Writing");
		    closenet(sockfd);
		    return(FALSE);
	       }
	       twirlfn();
	  }
     }

     if (GSgetType(gs) == A_CSO)
	  writestring(sockfd, "quit\r\n");

     closenet(sockfd);
     return(TRUE);
}


/*
 * This function makes a nice vms filename.
 */

void
VMSfile(fname)
  char *fname;
{
#ifndef VMS
     return;
#else
     char    *cp, *dot, *end;
     int     j, k;

     /*** Trim off date-size suffix, if present ***/
     end = fname + strlen(fname);
     if ((*(end - 1) == ']') && ((cp = strrchr(fname, '[')) != 0) &&
         (cp > fname) && *(--cp) == ' ')
	  *cp = '\0';

     /*** Replace illegal or problem characters ***/
     dot = fname + strlen(fname);
     for (cp = fname; cp < dot; cp++) {

	  /** Replace with underscores **/
	  if (*cp == ' ' || *cp == '/' || *cp == ':' ||
	      *cp == '[' || *cp == ']')
	       *cp = '_';

	  /** Replace with dashes **/
	  else if (*cp == '!' || *cp == '?' || *cp == '\'' || 
	           *cp == ',' || *cp == ':' || *cp == '\"' ||
	           *cp == '+' || *cp == '@' || *cp == '\\' ||
	           *cp == '(' || *cp == ')' || *cp == '=' ||
	           *cp == '<' || *cp == '>' || *cp == '#' ||
	           *cp == '%' || *cp == '*' || *cp == '`' ||
	           *cp == '~' || *cp == '^' || *cp == '|' ||
		   *cp == '/')
	       *cp = '-';
     }

     /** Collapse any serial underscores **/
     cp = fname + 1;
     j = 0;
     while (cp < dot) {
	  if (fname[j] == '_' && *cp == '_')
	       cp++;
	  else
	       fname[++j] = *cp++;
     }
     fname[++j] = '\0';

     /** Collapse any serial dashes **/
     dot = fname + (strlen(fname));
     cp = fname + 1;
     j = 0;
     while (cp < dot) {
          if (fname[j] == '-' && *cp == '-')
	       cp++;
	  else
	       fname[++j] = *cp++;
     }
     fname[++j] = '\0';

     /** Trim any trailing or leading **/
     /** underscrores or dashes       **/
     cp = fname + (strlen(fname)) - 1;
     while (*cp == '_' || *cp == '-')
          *cp-- = '\0';
     if (fname[0] == '_' || fname[0] == '-') {
          dot = fname + (strlen(fname));
          cp = fname;
          while ((*cp == '_' || *cp == '-') && cp < dot)
	       cp++;
	  j = 0;
          while (cp < dot)
	       fname[j++] = *cp++;
	  fname[j] = '\0';
     }

     /** Replace all but the last period with _'s, or second **/
     /** to last if last is followed by a terminal Z or z,   **/
     /** e.g., convert foo.tar.Z to                          **/
     /**               foo.tar_Z                             **/
     j = strlen(fname) - 1;
     if ((dot = strrchr(fname, '.')) != 0) {
	  if (((fname[j] == 'Z' || fname[j] == 'z') && fname[j-1] == '.') &&
	      (((cp = strchr(fname, '.')) != NULL) && cp < dot)) {
	       *dot = '_';
	       dot = strrchr(fname, '.');
	  }
	  cp = fname;
	  while ((cp = strchr(cp, '.')) != NULL && cp < dot)
	       *cp = '_';

          /** But if the root is > 39 characters, move **/
          /** the period appropriately to the left     **/
	  while (dot - fname > 39) {
	       *dot = '\0';
	       if ((cp = strrchr(fname, '_')) != NULL) {
		    *cp  = '.';
		    *dot = '_';
	       } 
	       else if ((cp = strrchr(fname, '-')) != NULL) {
		    *cp  = '.';
		    *dot = '_';
	       }
	       else {
		    *dot = '_';
		    j = strlen(fname);
		    fname[j+1] = '\0';
		    while (j > 39)
			 fname[j--] = fname[j];
		    fname[j] = '.';
	       }
               dot = strrchr(fname, '.');
	  }

          /** Make sure the extension is < 40 characters **/
          if ((fname + strlen(fname) - dot) > 39)
	       *(dot+40) = '\0';

	  /** Trim trailing dashes or underscores **/
	  j = strlen(fname) - 1;
	  while (fname[j] == '_' || fname[j] == '-')
	       fname[j--] = '\0';
     }
     else {
	  /** No period, so put one on the end, or after   **/
	  /** the 39th character, trimming trailing dashes **/
	  /** or underscrores                              **/
	  if (strlen(fname) > 39)
	       fname[39] = '\0';
	  j = strlen(fname) - 1;
	  while ((fname[j] == '_') || (fname[j] == '-'))
	       j--;
	  fname[++j] = '.';
	  fname[++j] = '\0';
     }

     /** Make sure the rest of the original string in nulled, **/
     /** to avoid problems with the CURwgetstr() line editor  **/
     cp = fname + strlen(fname);
     while (cp < end)
          *cp++ = '\0';

	  
#endif
}
     


/*
 * This function makes a nice UNIX filename
 */

void
UNIXfile(fname)
  char *fname;
{
     char *cp;

     for (cp = fname; *cp != '\0'; cp++) {
	  switch (*cp) {
	  case '\'':
	  case '\"':
	  case '/':
	  case ' ':
	  case '(':
	  case ')':

	       *cp = '-';
	  }
     }

     if (strlen(fname) >50) {
	  fname[40] = '\0';
     }
}


/*
 * This procedure prompts the user for a name, checks for pipe characters
 * and ~ characters in the filename and saves the file to disk.
 * 
 * If infile is NULL then contact the server and get the file.
 * 
 * If saveto is non-NULL then don't prompt the user for a filename
 * to save into, just "do it".
 */

void
Save_file(gs, saveto)
  GopherObj *gs;
  char *saveto;
{
     char    Userfilename[128];
     char    *cp, *view;
     char    buf[1024];
     boolean Openpipe = FALSE;
     FILE    *f;
     char    *infile = GSgetLocalFile(gs);


     switch (GSgetType(gs)) {
     case A_DIRECTORY:
     case A_INDEX:
	  if (saveto == NULL)
	       return;
     }

     /*** Construct a nice default filename ***/
     
     if (saveto == NULL) {
  	/* It shouldnt ever come here if Secure, but I've noticed calls 
     	to this in the code */
  	if (NoShellMode || SecureMode) {
		CursesErrorMsg("You cant do that");
		return;
  	}
	  /*** Let them use the Title ***/
	  strcpy(Userfilename, GSgetTitle(gs));

#ifdef VMS
	  VMSfile(Userfilename);
#else
	  UNIXfile(Userfilename);
#endif

	  if (CURGetOneOption(CursesScreen, "Save in file: ", Userfilename)<0)
	       return;

	  saveto = Userfilename;
     }

     if (*saveto == '\0')
	  return;

#ifndef VMS
     if (*saveto == '|') {
	  /** Open a pipe! **/
	  Openpipe = TRUE;
	  saveto++;
     }
     
     if (*saveto == '~') {
	  /*** Save in our home directory ***/
	  if (*(saveto + 1) == '/' && (cp = getenv("HOME")) != NULL) {
	       /*** Expand ~ to the home directory ***/
	       strcpy(buf,cp);
	       buf[strlen(cp)]='/';
	       strcpy(buf+strlen(cp) +1, saveto+2);
	       strcpy(saveto, buf);
	  } else {
	  /*** Save in someone else's home directory ***/
	       struct passwd *pass;

	       cp = strchr(saveto,'/');
	       *cp = '\0';
	       pass = getpwnam(saveto+1);
	       if (pass != NULL) {
		    /** align in prep for the home dir **/
		    strcpy(buf,pass->pw_dir);
		    buf[strlen(pass->pw_dir)]='/';
		    strcpy(buf+strlen(pass->pw_dir)+1,cp+1);
		    strcpy(saveto,buf);
		    
	       } else {
		    char tmpstr[256];
		    sprintf(tmpstr, "No such user '%s'", saveto+1);
		    CursesErrorMsg(tmpstr);
		    return;
	       }
	  }
     }
#endif

     if (Openpipe)
	  f = popen(saveto, "w+");
     else
	  f = fopen(saveto, "w+");

     if (f == NULL) {
	  char tempstr[128];

	  sprintf(tempstr, "Couldn't create '%s'", saveto);
	  CursesErrorMsg(tempstr);
	  return;
     }
     
     if (infile != NULL) {
	  /** We've already retrieved it, copy it over... **/
	  int oldfd, cc;

	  oldfd = open(infile, O_RDONLY);
	  if (oldfd <0) {
	       CursesErrorMsg("Can't open old file..");
	       if (Openpipe)
		    pclose(f);
	       else
		    fclose(f);
	       return;
	  }
	  
	  while ( (cc=read(oldfd, buf, sizeof buf)) > 0) {
	       if (fwrite(buf, cc, 1, f) <= 0)
		    CursesErrorMsg("Problems Writing");
	       twirl();
	  }
	  if (Openpipe)
	       pclose(f);
	  else
	       fclose(f);

	 close(oldfd);
	 return;

     } else {
	  /** We don't have the file yet, let's get it... **/
	  view = Choose_View(gs);
	  
	  if (view == NULL)
	       return;
	  
	  GStoFile(gs, f, view, twirl);  /* false if fails */
	  if (Openpipe)
	       pclose(f);
	  else
	       fclose(f);
     }
}



/*
** This procedure exits out of the curses environment and
** displays the file indicated by pathname to the screen
** using a pager command of some sort 
*/

void
GSdisplay(gs, view)
  GopherObj *gs;
  char      *view;
{
     char command[MAXSTR];
     int ch;
     char *file = GSgetLocalFile(gs);

#ifdef VMS
#include descrip
     int return_status;
     /** Keep DECC from complaining **/
     struct dsc$descriptor_s  command_desc;
     command_desc.dsc$w_length  = strlen(command);
     command_desc.dsc$b_class   = DSC$K_CLASS_S;
     command_desc.dsc$b_dtype   = DSC$K_DTYPE_T;
     command_desc.dsc$a_pointer = command;
#endif


     /** Search for a helper application **/
     RCdisplayCommand(GlobalRC, view, file, command);

     Debug("Displaying view %s ", view);
     Debug("command %s ", command);
     Debug("file %s", file);
     Debug("/%s \n", GSgetLocalFile(gs));

     if (strlen(command) == 0) {
	  if (SecureMode || NoShellMode)
	       CursesErrorMsg("Sorry, no viewer for the file....");
	  else
	       Save_file(gs, NULL);

	  return;
     }

     CURexit(CursesScreen);

#ifdef VMS
     if (strncasecmp("TPU",command,3) == 0) {
	  
	  /** the /READ qualifier is used to notify the TPU procedure
	    that we want "secure" mode  **/
	  if (SecureMode)
	       strcat(command,"/READ");
	  return_status = TPU$TPU(&command_desc);
	  if (!return_status) exit(return_status);
     } else
#endif

     if (strcmp(command, "builtin") != 0) {
	  system(command);
     }
     else
	  Ourpager(GSgetLocalFile(gs));


     printf("\nPress <RETURN> to continue");

     if (!SecureMode && !NoShellMode)
	  printf(",\n   <m> to mail, <D> to download, <s> to save, or <p> to print:");
#ifndef NOMAIL
     else
	  printf(", <m> to mail, <D> to download:");
#endif

     fflush(stdin);
     fflush(stdout);
     noecho();
     cbreak();

     ch = 0;
     while (ch == 0) {
	  ch=getchar();
#ifdef VMS
	  if (HadVMSInt)
	       break;
#endif

	  if (SecureMode || NoShellMode) {
	       switch (ch) {
	       case '\n':
	       case '\r':
	       case KEY_ENTER:
	       case ' ':
		    break;
#ifndef NOMAIL
	       case 'm':
#ifdef VMS
                    (void) getchar();
#endif
		    CURenter(CursesScreen);
		    mail_file(GSgetLocalFile(gs), GSgetTitle(gs));
		    break;
#endif
	       case 'D':
#ifdef VMS
		    (void) getchar();
#endif
		    CURenter(CursesScreen);
		    Download_file(gs);
		    break;

	       default:
		    puts(CURgetBell(CursesScreen));
		    fflush(stdout);
		    ch=0;
		    break;
	       }
	  }
	  else {

	       switch(ch) {
	       case '\n':
	       case '\r':
	       case ' ' :
		    break;
	       case 's':
#ifdef VMS
                    (void) getchar();
#endif
		    CURenter(CursesScreen);
		    Draw_Status("Saving File...");
		    refresh();

 		    if (!(SecureMode || NoShellMode))
			 Save_file(gs,NULL);
		    break;

	       case 'p':
#ifdef VMS
                    (void) getchar();
#endif
		    if (!RCprintCommand(GlobalRC, view, GSgetLocalFile(gs), command)) {
			 CursesErrorMsg("Sorry, cannot print this document");
			 return;
		    }
		    system(command);
		    break;
		    
	       case 'm':
#ifdef VMS
                    (void) getchar();
#endif
                    CURenter(CursesScreen);
		    mail_file(GSgetLocalFile(gs), GSgetTitle(gs));
		    break;

	       case 'D':
#ifdef VMS
		    (void) getchar();
#endif
		    CURenter(CursesScreen);
		    Download_file(gs);
		    break;
		    
	       default:
		    Debug("GSdisplayAfter - beep\r\n",NULL)
		    Debug("GSdisplayAfter - beep2\r\n",NULL)
		    puts(CURgetBell(CursesScreen));
		    fflush(stdout);
		    ch=0;
		    break;
	       }
	  }
     }
     
     tputs(CURgetCLS(CursesScreen),1,outchar);
     fflush(stdout);
}


/*
** This mini pager is intended for people worried about shell escapes from
** more or less or whatever
*/


/*Ourpager(filename)
  char *filename;
{
     FILE *InFile;
     int i;
     char inputline[512], *cp;
     int Done = FALSE;
     char ZeTypedChar;
     
     if ((InFile = fopen(filename, "r")) == NULL)
	  return;

     while (Done == FALSE) {
	  tputs(CURgetCLS(CursesScreen),1,outchar);

	  for (i=0 ; i < LINES-1; i++) {
	       cp = fgets(inputline, 512, InFile);
	       ZapCRLF(inputline);
	       puts(inputline);
	  }

	  printf("----Press <ENTER> for next page, q to exit------");
	  
	  cbreak();
	  ZeTypedChar = getchar();
	  
	  if ((ZeTypedChar == 'q') || (cp == NULL)) {
	       printf("\n");
	       Done = TRUE;
	  }
     }

     cbreak();
     fclose(InFile);
}
*/



/*
 * This allows the user to add or change data in a form.
 *
 * It returns true if the user wants to keep the changes
 * It returns false if the user wants to abort
 *
 */

BOOLEAN
NewForm(ZeForm)
  Form *ZeForm;
{
     static char printstring[WHOLELINE];
     int         i;          /** Acme Buggy whips and integers **/
     char        ch;

     while (1) {

	  DisplayForm(ZeForm);

	  sprintf(printstring, "Press <ESC> to accept fields and continue");

	  CURcenterline(CursesScreen,"Press <ESC> to accept fields and continue", LINES-3);
	  CURcenterline(CursesScreen,"or press <CTRL-A> to abort", LINES-2);

	  refresh();

	  /*** Now get some user input ***/

	  for (i=0; i< ZeForm->numfields; i++) {

	       move(ZeForm->yloc[i],ZeForm->xloc[i] + strlen(ZeForm->tags[i])+2);
	       refresh();
	       echo();

	       ch = CURwgetstr(CursesScreen, stdscr, ZeForm->values[i],80, FALSE);

	       /*** ESC key ***/
	       if (ch == '\033') {
		    noecho();
		    return(TRUE);
	       }

	       /*** CTRL-A ***/
	       else if (ch == '\001') {
		    noecho();
		    return(FALSE);
	       }

	       noecho();
	  }
	  
     }

}




/*
 * This Displays a form and a "menu".  It displays the Form
 * and then it waits for a keypress.  It returns the value of the
 * key that is pressed.
 */

void
DisplayForm(ZeForm)
  Form *ZeForm;
{
     int         availlines;
     int         optionlen;
     int         i;          /** Acme Buggy whips and integers **/


     clear();
     refresh();

     Draw_Banner();
     CURcenterline(CursesScreen, ZeForm->Title, 2);
     
     availlines = LINES - 6;
     
     /*** Print out the options according to the Form structure ***/
     
     for (i=0; i<ZeForm->numfields; i++) {
	  optionlen = strlen((ZeForm->tags)[i]);
	  mvaddstr(ZeForm->yloc[i],ZeForm->xloc[i], 
		   ZeForm->tags[i]);
	  addch(':');
	  addch(' ');
	  
	  /*** Now add the current value ***/
	  addstr((ZeForm->values)[i]);
     }
     
}

/*
 * This displays a single line message using CUR routines
 */
     
void
CursesMsg(Message, title)
  char *Message;
  char *title;
{
     char *mess[2];
     
     mess[0] = Message;
     mess[1] = NULL;

     Debug("CursesMsg - beep\r\n",NULL)
     CURBeep(CursesScreen);

     CURDialog(CursesScreen, title,mess);

     return;

}


/*
 * This does the same as above, but puts a window title on the mess 
 */

void
CursesErrorMsg(Message)
  char *Message;
{
     CursesMsg(Message, "Gopher Error");
}




