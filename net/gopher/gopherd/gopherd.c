/********************************************************************
 * lindner
 * 3.43
 * 1993/08/23 20:10:39
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopherd/gopherd.c,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: gopherd.c
 * Routines to implement the gopher server.
 *********************************************************************
 * Revision History:
 * gopherd.c,v
 * Revision 3.43  1993/08/23  20:10:39  lindner
 * Additional colon, gopher+ error fix
 *
 * Revision 3.42  1993/08/23  19:38:23  lindner
 * Yet another fix for mindexd troubles..
 *
 * Revision 3.41  1993/08/23  18:46:11  lindner
 * Crude addition of a veronica top-level block
 *
 * Revision 3.40  1993/08/23  02:34:30  lindner
 * Optional date and time
 *
 * Revision 3.39  1993/08/20  18:03:00  lindner
 * Mods to allow gopherd.conf files control ftp gateway access
 *
 * Revision 3.38  1993/08/19  20:52:25  lindner
 * Mitra comments
 *
 * Revision 3.37  1993/08/19  20:25:50  lindner
 * Mitra's Debug patch
 *
 * Revision 3.36  1993/08/12  06:27:35  lindner
 * Get rid of errant message when using inetd
 *
 * Revision 3.35  1993/08/11  22:47:44  lindner
 * Fix for gopher0 clients on gopher+ server
 *
 * Revision 3.34  1993/08/11  21:34:05  lindner
 * Remove extensions from titles for files with multiple views.
 * Move CMDfromNet() to *after* the chroot() and setuid()
 *
 * Revision 3.33  1993/08/11  14:39:24  lindner
 * Fix for send_binary bug
 *
 * Revision 3.32  1993/08/11  02:27:40  lindner
 * Fix for wais gateway and Unix client
 *
 * Revision 3.31  1993/08/10  20:26:57  lindner
 * Fixed bogus reading of .cache+ files
 *
 * Revision 3.30  1993/08/06  14:42:49  lindner
 * fix for mindex
 *
 * Revision 3.29  1993/08/06  14:30:40  lindner
 * Fixes for better security logging
 *
 * Revision 3.28  1993/08/05  20:47:16  lindner
 * Log execution of programs
 *
 * Revision 3.27  1993/08/02  17:59:26  lindner
 * Fix for Debug syntax error when using DL
 *
 * Revision 3.26  1993/07/29  20:49:25  lindner
 * removed dead vars, test for non-existant binary files, Dump_Core thing..
 *
 * Revision 3.25  1993/07/27  20:16:03  lindner
 * Fixed bug logging directory transactions
 *
 * Revision 3.24  1993/07/27  06:13:40  lindner
 * Bug fixes for redeffed .cache stuff
 *
 * Revision 3.23  1993/07/27  05:27:46  lindner
 * Mondo Debug overhaul from Mitra
 *
 * Revision 3.22  1993/07/27  01:52:59  lindner
 * More comments, don't let server die if fork error..
 *
 * Revision 3.21  1993/07/26  20:33:02  lindner
 * Fix from guyton, cachefd can be zero
 *
 * Revision 3.20  1993/07/26  17:24:02  lindner
 * Faster .cache output
 *
 * Revision 3.19  1993/07/26  15:32:26  lindner
 * mods for application/gopher-menu and faster .cache sending
 *
 * Revision 3.18  1993/07/23  03:24:22  lindner
 * fix for ppoen and NeXTs with overwritten envp
 * mucho mods for looking up filenames,
 * enhanced item info for waissrc: and others.
 * update for Text/plain & other MIME types.
 *
 * Revision 3.17  1993/07/20  23:53:51  lindner
 * LOGGOpher changes, Argv mucking, and Version Number enhancements
 *
 * Revision 3.16  1993/07/13  03:57:29  lindner
 * Fix for gopherls, improved mailfile handling, iteminfo on 3b2
 *
 * Revision 3.15  1993/07/10  04:22:36  lindner
 * fix for gopherls
 *
 * Revision 3.14  1993/07/08  17:54:40  lindner
 * fix for .src files that already end with .src
 *
 * Revision 3.13  1993/07/07  19:33:00  lindner
 * Sockets.c update, fix for compressed binarys, exec: fixes
 *
 * Revision 3.12  1993/06/11  16:59:57  lindner
 * gzip support, less lookups, etc.
 *
 * Revision 3.11  1993/04/15  22:20:08  lindner
 * CAPFILES mods
 *
 * Revision 3.10  1993/04/15  04:48:07  lindner
 * Debug code from Mitra
 *
 * Revision 3.9  1993/04/10  06:06:11  lindner
 * Admit1 Fixes for combined public/authed server
 *
 * Revision 3.8  1993/04/07  05:54:39  lindner
 * Fixed dreaded .mindex addition problem
 *
 * Revision 3.7  1993/03/26  19:47:50  lindner
 * Hacks to support wais gateway and gplus indexing
 *
 * Revision 3.6  1993/03/25  21:36:30  lindner
 * Mods for directory/recursive etal
 *
 * Revision 3.5  1993/03/24  22:08:59  lindner
 * Removed unused variable
 *
 * Revision 3.4  1993/03/24  20:23:17  lindner
 * Lots of bug fixes, compressed file support, linger fixes, etc.
 *
 * Revision 3.3  1993/03/01  02:22:40  lindner
 * Mucho additions for admit1 stuff..
 *
 * Revision 3.2  1993/02/19  21:21:11  lindner
 * Fixed problems with signals, problems with gethostbyaddr() and
 * inconsisent behavior that depended on the order of files in a directory.
 *
 * Revision 3.1.1.1  1993/02/11  18:02:55  lindner
 * Gopher+1.2beta release
 *
 * Revision 2.8  1993/02/09  22:14:57  lindner
 * many additions for gopher+ stuff..
 *
 * Revision 2.7  1993/01/30  23:57:44  lindner
 * Added extradata handling, language parsing, Fixed extension adding.
 * Fixes for new secure data method.
 * Enhanced link file processing
 * New fcn GDfromSelstr
 *
 * Revision 2.6  1993/01/12  22:53:28  lindner
 * Merged in 1.2.1.6 into mainline release
 *
 * Revision 2.5  1993/01/12  22:48:56  lindner
 * Fixes for mindexd
 *
 * Revision 2.4  1992/12/29  22:13:14  lindner
 * Further refinements to gopher+ stuff.
 * Integrated mindex into the server.
 *
 * Revision 2.3  1992/12/22  21:59:25  lindner
 * Merged in mtm patches to move kernal code to kernutils.c (rev 1.2.1.x)
 *
 * Revision 2.2  1992/12/22  18:56:38  lindner
 * First working gopher+ server with multiple views.  Added Addextension()
 * to get the right filename on the backend.  More mods to GDfromUFS()
 *
 * Revision 2.1  1992/12/21  20:09:46  lindner
 * Added much code and made many changes to move to gopher+
 * Added parse_request, removed a lot of html stuff
 * Added GDfromHTML, etc..  Lots o' stuff..
 *
 * Revision 1.2.1.6  1993/01/09  02:37:29  lindner
 * Added ftp gateway logging
 * Changed gethostbyaddr() to work on UNICOS
 *
 * Revision 1.2.1.5  1993/01/05  21:32:25  lindner
 * Fixed printfile() so that it deals with files with a period on a line
 * all by itself correctly.  Also removed one writestring() call.
 *
 * Revision 1.2.1.4  1993/01/05  21:10:38  lindner
 * Removed setuid() call that broke when using -u and chroot()
 *
 * Revision 1.2.1.3  1992/12/22  21:55:30  lindner
 * fixed typo, int was meant to be double..  Grrrr
 *
 * Revision 1.2.1.2  1992/12/22  21:48:15  lindner
 * Added extern maxload, fixes bug with previous version...
 *
 * Revision 1.2.1.1  1992/12/21  20:51:52  lindner
 * Moved loadrestrict stuff to kernutils.c, also the server
 * can now restrict for load average from standalone mode.
 *
 * Revision 1.2  1992/12/13  06:13:59  lindner
 * Added code to do readline timeouts <mtm>
 *
 * Revision 1.1  1992/12/10  23:13:27  lindner
 * gopher 1.1 release
 *
 *********************************************************************/


/* Originally derived from an 
 * Example of server using TCP protocol
 * pp 284-5 Stevens UNIX Network Programming
 */


#include "gopherd.h"
#include "command.h"
#include "patchlevel.h"
#include "Debug.h"

#undef stat /** Stupid openers thing..**/

GopherDirObj *GDfromSelstr();
GopherDirObj *GDfromUFS();

static	char*	Gdefusername = NULL;


extern char *getdesc();
extern double maxload;
void Process_Side();

#include "STAarray.h"
#include "STRstring.h"
#include "Sockets.h"

/* This function is called on a read timeout from the network */

#include <setjmp.h>
jmp_buf env;

SIGRETTYPE read_timeout(sig)
  int sig;
{
     longjmp(env,1);
}

SIGRETTYPE Dump_Core(sig)
  int sig;
{
     rchdir("/");
     err_dump("Caught signal, dumping core..");
}


/*
 * This routine finds out the hostname of the server machine.
 * It uses a couple of methods to find the fully qualified 
 * Domain name
 */

char *
GetDNSname(backupdomain)
  char *backupdomain;
{
     static char DNSname[MAXHOSTNAMELEN];
     struct hostent *hp;
     char *cp;

     cp = GDCgetHostname(Config);
     if (*cp != '\0')
	  return(cp);

     DNSname[0] = '\0';
     /* Work out our fully-qualified name, for later use */
     
     if (gethostname(DNSname, MAXHOSTNAMELEN) != 0) {
	  fprintf(stderr, "Cannot determine the name of this host\n");
	  exit(-1);
     }

     /* Now, use gethostbyname to (hopefully) do a nameserver lookup */
     hp = gethostbyname( DNSname);

     /*
      ** If we got something, and the name is longer than hostname, then
      ** assume that it must the the fully-qualified hostname
      */
     if ( hp!=NULL && strlen(hp->h_name) > strlen(DNSname) ) 
	  strncpy( DNSname, hp->h_name, MAXHOSTNAMELEN );
     else
	  strcat(DNSname, backupdomain);

     return(DNSname);
}


/*
 * Tries to figure out what the currently connected port is.
 * 
 * If it's a socket then it will return the port of the socket, 
 * if it isn't a socket then it returns -1.
 */

int 
GetPort(fd)
  int fd;
{
     struct sockaddr_in serv_addr;

     int length = sizeof(serv_addr);
     
     /** Try to figure out the port we're running on. **/
     
     if (getsockname(fd, (struct sockaddr *) &serv_addr,&length) == 0)
	  return(ntohs(serv_addr.sin_port));
     else
	  return(-1);

}


void
main(argc, argv, envp)
  int 	argc;
  char 	*argv[];
  char  *envp[];
{
     int                childpid;
     int                sockfd, newsockfd;
     int                clilen;
     struct sockaddr_in cli_addr;
     boolean            OptionsRead = FALSE;
     int i;
 
     /*** for getopt processing ***/
     int c;
     extern char *optarg;
     extern int optind;
     int errflag =0;


     Argv = argv;

#ifndef NeXT
     /* NeXTs don't like their envp to be overwritten... */

     for (i=0; envp[i] != NULL; i++)
	  ;
#endif

     if (i > 0)
	  LastArgv = envp[i - 1] + strlen(envp[i - 1]);
     else
	  LastArgv = argv[argc - 1] + strlen(argv[argc - 1]);

     pname = argv[0];
     strcpy(Data_Dir, DATA_DIRECTORY);
     err_init();	/* openlog() early - before we chroot() of course */

     /*** Check argv[0], see if we're running as gopherls, etc. ***/

     RunServer = RunLS = RunIndex = FALSE;

     if (strstr(argv[0], "gopherls") != NULL) {
	  RunLS = TRUE;
     } else if (strstr(argv[0], "gindexd") != NULL) {
	  RunIndex = TRUE;
	  dochroot = FALSE;
     } else 
	  RunServer = TRUE;  /** Run the server by default **/

     Config = GDCnew();  /** Set up the general configuration **/
 
     while ((c = getopt(argc, argv, "mCDIcL:l:o:u:")) != -1)
	  switch (c) {
	  case 'D':
	       DEBUG = TRUE;
	       break;

	  case 'I':
	       RunFromInetd = TRUE;
	       break;

	  case 'C':
	       Caching = FALSE;
	       break;

	  case 'm':
		if (RunIndex)
			MacIndex = TRUE;
		break;

	  case 'c':
	       dochroot = FALSE;
	       if (!RunFromInetd) {
		    printf("Not using chroot() - be careful\n");
		    if ( getuid() == 0 || geteuid() == 0 )
			 printf("You should run without root perms\n");
	       }
	       break;

	  case 'L':  /** Load average at which to restrict usage **/
	       maxload = atof(optarg);
	       break;

	  case 'l':  /** logfile name **/
	       if (*optarg == '/')
		    GDCsetLogfile(Config, optarg);
	       else {
		    char tmpstr[256];
		    
		    getwd(tmpstr);
		    strcat(tmpstr, "/");
		    strcat(tmpstr, optarg);
		    
		    GDCsetLogfile(Config, tmpstr);
	       }
	       break;
	       
	  case 'o': /** option file **/
	       if (*optarg == '/')
		    GDCfromFile(Config, optarg);
	       else {
		    char tmpstr[256];
		    getwd(tmpstr);
		    strcat(tmpstr, "/");
		    strcat(tmpstr, optarg);
		    GDCfromFile(Config, tmpstr);
	       }
	       OptionsRead = TRUE;
	       break;

	  case 'u':
	       {
		    struct passwd *pw = getpwnam( optarg );
		    if ( !pw ) {
			 fprintf(stderr,
			      "Could not find user '%s' in passwd file\n",
			      optarg);
			 errflag++;
		    } else {
			 Gdefusername = strdup(optarg);
			 if (!RunFromInetd) {
			      printf("Running as user '%s'\n",
				   optarg);
			 }
		    }
	       }
	       break;

	  case '?':
	  case 'h':
	       errflag++;
	       break;
	  }
     if (errflag) {
	  /* Distribution has Usage for older versions of gopher, doesnt
	   * reflect options in newer versions */
	  fprintf(stderr, "Usage: %s [-mCDIc] [-u username] [-s securityfile] [-l logfile] [ -L loadavg ] <datadirectory> <port>\n", argv[0]);
	  fprintf(stderr, "   -C  turns caching off\n");
	  fprintf(stderr, "   -D  enables copious debugging info\n");
	  fprintf(stderr, "   -I  enable \"inetd\" mode\n");
	  fprintf(stderr, "   -c  disable chroot(), use secure open routines instead\n");
	  fprintf(stderr, "   -u  specifies the username for use with -c\n");
	  fprintf(stderr, "   -o  override the default options file '%s'\n", CONF_FILE);
	  fprintf(stderr, "   -l  specifies the name of a logfile\n");
	  fprintf(stderr, "   -L  specifies maximum load to run at\n");
	  fprintf(stderr, "   -m  specifies MacIndex, whatever that is\n");
		  
	  exit(-1);
     }

     if ( getuid() == 0 && Gdefusername == NULL && !RunFromInetd)
	  printf("Warning! You should run the server with the -u option!\n");


     if (optind < argc) {
	  strcpy(Data_Dir, argv[optind]);
	  optind++;
	  Debug("main: Setting data to Data Directory is %s\n",Data_Dir);
     } else if (RunLS)
	  strcpy(Data_Dir, "/");

     Debug("main: Data Directory is %s\n",Data_Dir);

     if (optind < argc) {
	  GopherPort = atoi(argv[optind]);
	  optind++;
	  Debug("main: Setting port to %d\n",GopherPort);
     }
     Debug("main: Port is %d\n",GopherPort);

     /** Read the options in, if not overridden **/
     if (OptionsRead == FALSE)
	  GDCfromFile(Config, CONF_FILE);

     /*** Make sure we do a tzset before doing a chroot() ***/
     tzset();
     

     if (RunLS) {
	  Zehostname =GetDNSname(DOMAIN_NAME);
	  Caching = FALSE;

	  fflush(stdout);
	  uchdir(Data_Dir);
	  /* chroot lines added, jdc */
 	  if (dochroot) {
	       if (chroot(Data_Dir))
		    Abortoutput(sockfd, "Data_Dir dissappeared!"), exit(-1);
	  }
	  
	  listdir(fileno(stdout), "/", FALSE, NULL, NULL);
	  exit(0);
     }

     if (!RunFromInetd) {
	  printf("Internet Gopher Server %s.%s patch %d\n", GOPHER_MAJOR_VERSION, GOPHER_MINOR_VERSION, PATCHLEVEL);
	  printf("Copyright 1991,92,93 the Regents of the University of Minnesota\n");
	  printf("See the file 'Copyright' for conditions of use\n");
	  printf("Data directory is %s\n", Data_Dir);
	  printf("Port is %d\n", GopherPort);
     }

     if (*GDCgetLogfile(Config) != '\0' && !RunFromInetd)
	  printf("Logging to File %s\n", GDCgetLogfile(Config));

     /*
      * Would like to setuid() here, but have to wait until after the
      * bind() in case we're going to be running on a privileged port.
      */

     if (uchdir(Data_Dir)) {
	  if (!RunIndex) {
	       fprintf(stderr, "Cannot change to data directory!! %s \n",Data_Dir);
	       exit(-1);
	  }
     }

     if (dochroot && getuid() != 0) {
	  fprintf(stderr, "Gopherd uses the privileged call chroot().  Please become root.\n");
	  exit(-1);
     }

     fflush(stderr);
     fflush(stdout);

     if (DEBUG == FALSE && RunFromInetd==FALSE)
	  daemon_start(TRUE);

#ifdef DEBUGGING
     (void) signal(SIGUSR1, Dump_Core);
#endif

     /*** Hmmm, does this look familiar? :-) ***/


     err_init();	/* does this look familiar too?? :-) */

     /** Ask the system what host we're running on **/

     Zehostname = GetDNSname(DOMAIN_NAME);
     Debug("I think your hostname is %s\n", Zehostname);

     if (RunFromInetd) {
	  /** Ask the system which port we're running on **/
	  int newport=0;
	  if ((newport =GetPort(0)) !=0)
	       GopherPort=newport;

	  /*** Do the stuff for inetd ***/

	  while(do_command(fileno(stdout))!=0);	/* process the request */
	  exit(0);
     }

     /** Set our cmd string **/

     ServerSetArgv("waiting for connection");
     

     /** Open a TCP socket (an internet stream socket **/
     sockfd = SOCKbind_to_port(GopherPort);

     listen(sockfd, 5);
     
     for ( ; ; ) {
	  /*
	   * Wait for a connection from a client process.
	   * This is an example of a concurrent server.
	   */
	  
	  clilen = sizeof(cli_addr);
	  while (1) {
	       newsockfd = accept(sockfd, (struct sockaddr *) &cli_addr,
				  &clilen);

	       if (newsockfd >= 0)
		    break;
	       else if (errno != EINTR)  /** Restart accept if we hit a
					     SIGCHLD signal..**/
		    err_dump("server: accept error");
	  }
		    

	  SOCKlinger(sockfd, FALSE);

	  
	  if ( (childpid = fork()) < 0) {
	       /** Problems forking..  **/
	       writestring(sockfd, "3Problems forking!\tmoo\terror.host\t0\r\n.\r\n");

	  }
	  
	  else if (childpid == 0) {	/* Child process */
	       close(sockfd);		/* close original socket */

	       while(do_command(newsockfd)!=0);	/* process the request */
	       exit(0);
	  }
	  /** clean up any zombie children **/
	  sig_child();

	  close(newsockfd); 		/* parent process */
     }
}





#define NO_SUBJECT "<no subject>"

void
process_mailfile(sockfd, Mailfname)
  int sockfd;
  char *Mailfname;
{
     FILE *Mailfile;
     char Zeline[MAXLINE];
     char outputline[MAXLINE];
     char Title[MAXLINE];
     long Startbyte=0, Endbyte=0, Bytecount=0;
     boolean flagged = 0;
     char *p;

     Debug("process_mailfile %s\n",Mailfname);

     Mailfile = rfopen(Mailfname, "r");

     if (Mailfile == NULL) {
	  Abortoutput(sockfd, "Cannot access file");
	  return;
     }

     while (fgets(Zeline, MAXLINE, Mailfile) != NULL) {
	  if (strncmp(Zeline, "Subject: ", 9)==0 && (!flagged)) {
	       flagged =1;
	       /* trim out the white space.. */
	       p = Zeline + 8;
	       while ((*p == ' ')||(*p == '\t'))
		    p++;
	       if (*p == '\n')
		    strcpy(Title, NO_SUBJECT);
	       else
		    strcpy(Title,p);
	       
	       ZapCRLF(Title);
	       Debug("Found title %s\n", Title);
	  }
	  
	  if (strcmp(Zeline, "\n") ==0 && (!flagged)) {
	       flagged = 1;
	       strcpy(Title, NO_SUBJECT);
	       Debug("No subject found - using default\n",0);
	  }
	  
	  if (is_mail_from_line(Zeline)==0) {
	       Endbyte = Bytecount;
	       flagged =0;

	       if (Endbyte != 0) {
		    sprintf(outputline, "0%s\tR%d-%d-%s\t%s\t%d\r\n", 
			    Title, Startbyte, Bytecount, Mailfname,
			    Zehostname, GopherPort);
		    if (writestring(sockfd, outputline) < 0)
			 LOGGopher(sockfd, "Client went away"), exit(-1);
		    Startbyte=Bytecount;
		    *Title = '\0';
	       }
	  }

	  Bytecount += strlen(Zeline);
     }

     if (*Title != '\0') {
	  sprintf(outputline, "0%s\tR%d-%d-%s\t%s\t%d\r\n", 
		  Title, Startbyte, Bytecount, Mailfname, 
		  Zehostname, GopherPort);
	  if (writestring(sockfd, outputline)<0)
	       LOGGopher(sockfd, "Client went away"),exit(-1);
     }	  

}



boolean
Can_Read(sockfd)
  int sockfd;
{
     if (RunLS == TRUE)
	  return(TRUE);
     return(GDCCanRead(Config, CurrentPeerName, CurrentPeerIP));
}

boolean
Can_Browse(sockfd)
  int sockfd;
{
     if (RunLS == TRUE)
	  return(TRUE);
     return(GDCCanBrowse(Config, CurrentPeerName, CurrentPeerIP));
}

boolean
Can_FTP(sockfd)
  int sockfd;
{
     if (RunLS == TRUE)
	  return(TRUE);
     return(GDCCanFTP(Config, CurrentPeerName, CurrentPeerIP));
}

boolean
Can_Search(sockfd)
  int sockfd;
{
     if (RunLS == TRUE)
	  return(TRUE);
     return(GDCCanSearch(Config, CurrentPeerName, CurrentPeerIP));
}



char *
AddExtension(cmd, view)
  CMDobj *cmd;
  char *view;
{
     char *filename = CMDgetFile(cmd);
     char *newselstr;
     int  flen=0, newlen=0;
     char *newfile;

     if (filename == NULL)
	  return(CMDgetSelstr(cmd));

     newfile = EXAfindFile(Config->Extensions, filename, view);
     
     flen = strlen(filename);
     newlen = strlen(newfile);

     if (newlen > flen) {
	  /*** Add the found extensions... ***/
	  newselstr = (char *)malloc(MAXPATHLEN);
	  
	  strcpy(newselstr, CMDgetSelstr(cmd));
	  strcat(newselstr, newfile+flen);
	  Debug("New long file is %s", newselstr);
	  return(newselstr);
     }
     return(CMDgetSelstr(cmd));
}




int
do_command(sockfd)
  int sockfd;
{
     char    logline[MAXLINE];
     char    *view     = NULL;
     char    *Selstr   = NULL;
     CMDobj  *cmd;
     char    *filter   = NULL;
     TixObj  *tix      = NULL;

     cmd = CMDnew();

     /*** Reopen the log file ***/

     if (*GDCgetLogfile(Config) != '\0') {
	  LOGFileDesc = uopen(GDCgetLogfile(Config), O_WRONLY | O_APPEND |O_CREAT, 0644);
	  
	  if (LOGFileDesc == -1) {
	       printf("Can't open the logfile: %s\n", GDCgetLogfile(Config));
	       exit(-1);
	  }
     }

     if(LoadTooHigh()) {
	  Abortoutput(sockfd, "System is too busy right now. Please try again later.");
	  exit(-1);
     }


     (void) signal(SIGALRM,read_timeout);
     (void) alarm(READTIMEOUT);

     if(setjmp(env)) {
	  LOGGopher(sockfd,"readline: Timed out!");
	  Abortoutput(sockfd,"readline: Timed out!");
	  exit(-1);
     }

     /*** Find out who's knockin' ***/
     inet_netnames(sockfd, CurrentPeerName, CurrentPeerIP);
     
     ServerSetArgv("input from %s", CurrentPeerName);

#ifdef UMNDES
     if (CMDgetTicket(cmd) != NULL) {
	  /*** Test the ticket, and set the appropriate user ***/
	  tix=ValidTicket(sockfd, cmd);

	  if (tix == NULL)
	       return(0);
	  
	  if (Setuid_username(TIXgetUser(tix))==FALSE)
	       printf("Failed to set username to %s\n", TIXgetUser(tix));
	  Debug("We're now running as uid %d\n", geteuid());
     } else
	  /*** No ticket given, default to the defuser ***/
	  if (Gdefusername && Setuid_username(Gdefusername) == FALSE) {
	       Debug("Couldn't change uid to %s\n",Gdefusername);
	       Abortoutput(sockfd, "Can't set UID!"), exit(-1);
	  }
#else  /* UMNDES */
     if (Gdefusername && Setuid_username(Gdefusername)== FALSE)
	  Abortoutput(sockfd, "Can't set UID!"), exit(-1);

#endif  /* UMNDES */

     /** Change our root directory **/
     if ( dochroot ) {
	  /** Change back to root for a bit **/
	  int tempuid = geteuid();
	  
	  seteuid(0);

	  if (chroot(Data_Dir))
	       Abortoutput(sockfd, "Data_Dir dissappeared!"), exit(-1);
	  
	  seteuid(tempuid);

	  uchdir("/");	/* needed after chroot */
     }



     CMDfromNet(cmd, sockfd);
     ASKfile = CMDgetAskfile(cmd);  /* Ickkk!  This is a global used only
				       by specialfile and Specialclose */

     if (RunIndex) {
	  /*** Run like the old gindexd thing. ***/
	  
	  uchdir("/");
	  strcpy(Data_Dir, "/");

	  if (*CMDgetSelstr(cmd) == '\t')
	       Do_IndexTrans(sockfd, Data_Dir, CMDgetSearch(cmd)+1);
	  else
	       Do_IndexTrans(sockfd, Data_Dir, CMDgetSearch(cmd));

	  Do_IndexTrans(sockfd, Data_Dir, CMDgetSearch(cmd));
	  return(0);
     }

     



     /** Extract the view if it exists **/
     if (CMDgetCommand(cmd) != NULL) {
	  char *command = CMDgetCommand(cmd);
	  if (*command == '+' && strlen(command)>1)
	       view = command+1;
	  else if (*command == '!') {
	       item_info(cmd, sockfd);
	       if (*(command+1) != '\0')
		    filter = command + 1;
	       return(0);
	  }
	  else if (*command == '$') {
	       if (*(command+1) != '\0')
		    filter = command + 1;
	       view = "application/gopher+-menu";
	       *command = '+';
	  }
	  else
	       ; /*** Error ***/
     }
     
     if (strncmp(CMDgetSelstr(cmd), "waisdocid:",10)==0)
	  view = "Text/plain";

     /*** Root level null selector string.. ***/
     if (!view && strlen(CMDgetSelstr(cmd)) == 0) 
	  view = "application/gopher-menu";

     /*** Try to find a view if not supplied ***/
     if (view == NULL) {
	  GopherDirObj *gd;
	  int num,i;

	  /** Get gopher directory containing item in question **/
	  gd = GDfromSelstr(cmd, sockfd);
    

	  if (gd != NULL) {
	       num = GDSearch(gd, CMDgetSelstr(cmd));
	       if (num >=0) {
		    GopherObj *gs;
	       
		    gs= GDgetEntry(gd, num);

		    if (GSgplusInited(gs) == FALSE) {
			 view = "";
		    }

		    /**  If only one view, take it **/
		    else if (GSgetNumViews(gs) == 1)
			 view = VIgetViewnLang(GSgetView(gs, 0),(char*)malloc(128));
		    else {
			 /*** Hmmm, let's choose one.. ***/
			 for (i=0; i<GSgetNumViews(gs); i++) {
			      char *tmpview;

			      tmpview = VIgetType(GSgetView(gs,i));
			      if (GSgetType(gs) == '0') {
				   if (strcasecmp(tmpview, "Text/plain")==0) {
					view = VIgetViewnLang(GSgetView(gs, i),(char*)malloc(128));
					break;
				   }
			      }
			      if (GSgetType(gs) == 'I') {
				   if (strcmp(tmpview, "image/gif")==0) {
					view = VIgetViewnLang(GSgetView(gs, i),(char*)malloc(128));
					break;
				   }
			      }

			 }
			 if (view == NULL)
			      /** Give up, take the first view... **/
			      view = VIgetViewnLang(GSgetView(gs,0), (char*)malloc(128));
		    }
		    /** We should have a view by now **/
	       }
	  }
     }


     /** Decide whether to add extensions of not .. **/
     if (view != NULL )
	  Selstr = AddExtension(cmd, view);
     else
	  Selstr = CMDgetSelstr(cmd);


     /*** With the funky new capability system we can just check the
          first letter(s), end decide what the object refers to. ***/

     ServerSetArgv("%s to %s", Selstr, CurrentPeerName);

     switch (*Selstr) {
     case '\0':
     case '\t':

	  /*** The null capability, so it's not a file, probably wants
	       to talk to a directory server ***/

	  /*** we'll just do a "list" of the root directory, with no user
	       capability.  ***/


	  listdir(sockfd, "/", CMDisGplus(cmd), view, filter);
	  LOGGopher(sockfd, "Root Connection");
	  break;

     case 'h':
	  /*** A raw html file ***/
	  /*** Turn off html'ing and just dump the file ***/
	  UsingHTML = FALSE;

     case '0':
     case '9':
     case 's':
     case 'I':
     case 'g':
	  /*** It's some kind of file ***/

	  /*** Is it binary??  ***/
	  if (view == NULL)  {
	       if (*Selstr != '0')
		    send_binary(sockfd, Selstr+1, CMDisGplus(cmd));
	       else
		    printfile(sockfd, Selstr+1, 0, -1, CMDisGplus(cmd));
	  } else {
	       if (GSisText(NULL, view))
		    printfile(sockfd, Selstr+1, 0, -1, CMDisGplus(cmd));
	       else
		    send_binary(sockfd, Selstr+1, CMDisGplus(cmd));
	  }
	  /*** Log it ***/
	  LOGGopher(sockfd, "retrieved file %s", Selstr+1 );
	  break;


     case '1':
	  /*** It's a directory capability ***/
	  listdir(sockfd, Selstr+1, CMDisGplus(cmd), view, filter);

	  /** Log it **/
	  LOGGopher(sockfd, "retrieved directory %s", Selstr+1);
	  break;

     case '7':
	  /*** It's an index capability ***/
	  if (Can_Search(sockfd) == FALSE) {
	       Abortoutput(sockfd, GDCgetBummerMsg(Config));
	       LOGGopher(sockfd, "Denied access for %s", Selstr+1);
	       break;
	  }

	  Do_IndexTrans(sockfd, Selstr+1, cmd);

	  break;


     case 'm':
	  if (strncmp(Selstr, "mindex:", 7)==0) {
	       /*** First test for multiple indexes ***/
	       if (Can_Search(sockfd) == FALSE) {
		    Abortoutput(sockfd, GDCgetBummerMsg(Config));
		    LOGGopher(sockfd, "Denied access for %s", Selstr+1);
		    break; 
	       }
	       do_mindexd(sockfd, Selstr+7, CMDgetSearch(cmd), CMDisGplus(cmd),
			  view);
	       break;
	  }


	  /*** This is an internal identifier ***/
	  /*** The m paired with an Objtype of 1 makes a mail spool file
	    into a directory.
	    ***/
	  if (Can_Browse(sockfd) == FALSE) {
	       Abortoutput(sockfd,  GDCgetBummerMsg(Config));
	       LOGGopher(sockfd,"Denied access for %s", Selstr+1);
	       break;
	  }

	  process_mailfile(sockfd, Selstr + 1);
	  writestring(sockfd, ".\r\n");

	  /** Log it **/
	  LOGGopher(sockfd, "retrieved maildir %s", Selstr+1 );

	  break;

     case 'R':
	  /*** This is an internal identifier ***/
	  /*** The R defines a range  ****/
	  /*** The format is R<startbyte>-<endbyte>-<filename> **/
     {
	  int startbyte, endbyte;
	  char *cp, *oldcp;

	  cp = strchr(Selstr+1, '-');
	  
	  if (cp == NULL) {
	       Abortoutput(sockfd, "Range specifier error");
	       break;
	  }
	  
	  *cp = '\0';
	  startbyte = atoi(Selstr+1);
	  oldcp = cp+1;

	  cp = strchr(oldcp, '-');
	  
	  if (cp == NULL) {
	       Abortoutput(sockfd, "Range specifier error");
	       exit(-1);
	  }

	  *cp = '\0';
	  endbyte = atoi(oldcp);
	  oldcp = cp + 1;

	  Debug("Start: %d, ", startbyte);
	  Debug("End: %d, ", endbyte);
	  Debug("File: %s\n", oldcp);

	  printfile(sockfd, oldcp, startbyte, endbyte, CMDisGplus(cmd));

	  /*** Log it ***/
	  LOGGopher(sockfd, "retrieved range %d - %d of file %s", startbyte, endbyte, oldcp);
	  break;
     }

     case 'f':
	  if (Can_FTP(sockfd) == FALSE) {
	       Abortoutput(sockfd,  GDCgetBummerMsg(Config));
	       LOGGopher(sockfd, "Denied access for %s", Selstr);
	       break;
	  }

	  if (strncmp(Selstr, "ftp:",4)==0){

	       LOGGopher(sockfd, "retrieved %s", Selstr);

	       SendFtpQuery(sockfd, Selstr+4);
	       break;
	  }
	  break;


     case 'e':
	  if (Can_Browse(sockfd) == FALSE) {
	       Abortoutput(sockfd,  GDCgetBummerMsg(Config));
	       LOGGopher(sockfd, "Denied access for %s", Selstr);
	       break;
	  }

	  if (strncmp(Selstr, "exec:", 5)==0) {
	       /* args are between colons */
	       char *args, *command;
	       
	       command = strrchr(Selstr + 5, ':');
	       if (command == NULL)
		    break;

	       if (*(Selstr+4) == ':' && *(Selstr+5) == ':')
		    args = NULL;
	       else
		    args = Selstr+5;

	       *command = '\0';
	       command++;
	       
	       EXECargs = args;

	       printfile(sockfd, command, 0, -1, CMDisGplus(cmd));
	       LOGGopher(sockfd, "Executed %s %s", command, args);
	  }
	  break;

     case 'w':
     {
	  if (strncmp(Selstr, "waissrc:", 8) == 0) {
	       char waisfname[512];  /*** Ick this is gross ***/

	       if (Can_Search(sockfd) == FALSE) {
		    Abortoutput(sockfd,  GDCgetBummerMsg(Config));
		    LOGGopher(sockfd, "Denied access for %s", Selstr);
		    break;
	       }
	       strcpy(waisfname, Selstr+8);
	       if (strlen(waisfname) <= 4 ||
		   strncmp(&waisfname[strlen(waisfname)-4],".src",4) )
		    strcat(waisfname, ".src");
	       SearchRemoteWAIS(sockfd, waisfname, cmd, view);
	       break;
	  }
	  else if (strncmp(Selstr, "waisdocid:", 10) == 0) {
	       if (Can_Browse(sockfd) == FALSE) {
		    Abortoutput(sockfd,  GDCgetBummerMsg(Config));
		    LOGGopher(sockfd, "Denied access for %s", Selstr);
		    break;
	       }
	       Fetchdocid(sockfd, Selstr+10);
	       break;
	  }
     }


     default:
	  /*** Hmmm, must be an old link... Let's see if it exists ***/

	  switch (isadir(Selstr)) {
	  case -1:
	       /* no such file */
	       sprintf(logline, "'%s' does not exist", Selstr);
	       Abortoutput(sockfd, logline);
	       break;

	  case 0:
	       /* it's a file */
	       printfile(sockfd, Selstr, 0, -1, CMDisGplus(cmd));
	       
	       /* Log it... */
	       LOGGopher(sockfd, "retrieved file %s", Selstr);

	       break;

	  case 1:
	       /* it's a directory */
	       listdir(sockfd, Selstr, CMDisGplus(cmd), view, filter);

	       /* Log it */
	       LOGGopher(sockfd, "retrieved directory %s", Selstr);

	       break;
	  }
     }

     return(0);
}


/*
 * This function tries to find out what type of file a pathname is.
 */

void
Getfiletypes(newpath, filename, gs)
  char *newpath;
  char *filename;
  GopherObj *gs;
{
     int Zefilefd;
     static char Zebuf[256];
     char *cp;
     static char Selstr[512];

     
     switch (isadir(filename)) {
     case -1:
	  GSsetType(gs,'3');
	  return;

     case 1:
	  GSsetType(gs,A_DIRECTORY);
	  *Selstr = '1';
	  strcpy(Selstr +1, newpath);
	  GSsetPath(gs, Selstr);
	  return;
     default:

	  /*** The default is a generic text file ***/
	  GSsetType(gs, A_FILE);

	  *Selstr = '0';
	  strcpy(Selstr + 1, newpath);

	  /*** Test and see if the thing exists... and is readable ***/
	  
	  if ((Zefilefd = ropen(filename, O_RDONLY)) < 0) {
	       GSsetType(gs, '3');
	       return;
	  }
	  
	  if (read(Zefilefd, Zebuf, sizeof(Zebuf)) <0) {
	       GSsetType(gs, '3');
	       return;
	  }
	  close(Zefilefd);
	  
	  /*** Check the first few bytes for sound data ***/
	  
	  cp = Zebuf;

	  if (strncmp(cp, ".snd", 4)==0) {
	       GSsetType(gs, A_SOUND);
	       *Selstr = 's';
	       strcpy(Selstr+1, newpath);
	  }

	  /*** Check and see if it's mailbox data ***/
	  
	  if (is_mail_from_line(Zebuf)==0) {
	       GSsetType(gs, A_DIRECTORY);
	       *Selstr = 'm';
	       strcpy(Selstr+1, newpath);
	       GSsetGplus(gs, FALSE);  /** Not yet.. **/
	  }
	  

	  /*** Check for uuencoding data ***/

	  if (strncmp(cp,"begin",6) == 0)  {
	       GSsetType(gs, '6');
	       *Selstr = '6';
	       strcpy(Selstr+1, newpath);
	  }
	  
	  /*** Check for GIF magic code ***/
	  
	  if (strncmp(cp, "GIF", 3) == 0) {
	       GSsetType(gs, 'I');
 	       *Selstr = '9';
 	       strcpy(Selstr + 1, newpath);
 	  }

	  GSsetPath(gs, Selstr);

     }
}

/*
 * Add a default view if none exists..
 */

void
AddDefaultView(gs, size, lang)
  GopherObj *gs;
  int size;
  char *lang;
{

     if (lang == NULL)
	  lang = GDCgetLang(Config);
     
     switch (GSgetType(gs)) {
     case A_FILE:
	  GSaddView(gs, "Text/plain", lang, size);
	  break;
     case A_DIRECTORY:
	  GSaddView(gs, "application/gopher-menu", lang, size);
	  GSaddView(gs, "application/gopher+-menu", lang, size);
	  break;
     case A_MACHEX:
	  GSaddView(gs, "application/mac-binhex40", lang, size);
	  break;
     case A_PCBIN:
	  GSaddView(gs, "application/octet-stream", lang, size);
	  break;
     case A_CSO:
	  GSaddView(gs, "application/qi", lang, 0);
	  break;
     case A_INDEX:
	  GSaddView(gs, "application/gopher-menu", lang, size);
	  GSaddView(gs, "application/gopher+-menu", lang, size);
	  break;
     case A_TELNET:
	  GSaddView(gs, "application/telnet", lang, 0);
	  break;
     case A_SOUND:
	  GSaddView(gs, "audio/basic", lang, size);
	  break;
     case A_UNIXBIN:
	  GSaddView(gs, "application/octet-stream", lang, size);
	  break;
     case A_GIF:
	  GSaddView(gs, "image/gif", lang, size);
	  break;	
     case A_HTML:
	  GSaddView(gs, "application/HTML", lang, size);
	  break;
     case A_TN3270:
	  GSaddView(gs, "application/tn3270", lang, 0);
	  break;
     case A_MIME:
	  GSaddView(gs, "multipart/mixed", lang, size);
	  break;
     case A_IMAGE:
	  GSaddView(gs, "image", lang, size);
	  break;
     }
}


#ifdef ADD_DATE_AND_TIME

void
GSaddDateNsize(gs, statbuf)
  GopherObj *gs;
  struct stat statbuf;
{
     int           fd, i;
     char         longname[256];
     char         *cdate, *ti, *fp;
     
     switch (GSgetType(gs)) {
     case '1': /*** It's a directory ***/
     case '7': /*** It's an index ***/
     case 'f': /*** ftp link ***/
     case 'e': /*** exec link ***/
     case 'h': /*** www link ***/
     case 'w': /*** wais link ***/
     case 'm':
	  break;
     default:
     {
	  cdate= ctime( &statbuf.st_mtime); /* last mod time */
	  cdate[ 7]= 0; cdate[10]= 0; cdate[24]= 0;
	  sprintf( longname, "%s  [%s%s%s, %ukb]", GSgetTitle(ge),
		  cdate+8,cdate+4,cdate+22, (statbuf.st_size+1023) / 1024);
	  GSsetTitle(gs,longname);
     }
	  break;
     }
}     
#else
void
GSaddDateNsize(a,b)
  GopherObj *a;
  struct stat b;
{
     ;
}

#endif /* ADD_DATE_AND_TIME */


/*
 * Add a DL description if it's there ...
 */

GStitlefromDL(gs, filename)
  GopherObj *gs;
  char *filename;
{
#ifdef DL
     char               dlpath[2];    /*** for DL**/
     char               *dlout;

     /* Process a "dl" description if there is one! */
     
     dlpath[0] = '.';
     dlpath[1] = '\0';
     dlout = getdesc(NULL,dlpath,filename,0);
     
     Debug("dl: %s", dlpath);
     Debug(" %s", filename);
     Debug(" %s\n", dlout);

     if (dlout != NULL) {
	  GSsetTitle(gs, dlout);
     }
#endif
     ;
}


/*
 * Load up a gopher directory from the file system 
 */

GopherDirObj *
GDfromUFS(pathname, sockfd, isGplus)
  char *pathname;
  int sockfd;
  boolean isGplus;
{
     DIR                *ZeDir;
     char               filename[256];
     static char        newpath[512];
     static GopherObj   *Gopherstow = NULL;
     static Extobj      *ext = NULL;
     GopherObj          *gs;
     struct dirent      *dp;
     GopherDirObj       *gd;
     struct stat        statbuf;
     boolean            AddItem = TRUE;
     static char        Pathp[512];
     StrArray           *Linkfiles;
     int                i, cachefd;

     Linkfiles = STAnew(10);

     Debug("GDfromUFS:%s\r\n",pathname)
     Debug("GDfromUFS:Config=%d\r\n",Config)
     /*** Make our gopherobj ****/
     if (Gopherstow == NULL)
	  Gopherstow = GSnew();

     gs = Gopherstow;
     if (ext == NULL)
	  ext = EXnew();

     if (isGplus && GSgplusInited(gs) == FALSE)
	  GSplusnew(gs);

     gd = GDnew(32);

     if (rchdir(pathname)<0) {
	  perror("SOL dude");
	  exit(-1);
     }
     

     if (Caching && Cachetimedout(".cache+", CACHE_TIME, ".")==FALSE) {
	  if ((cachefd = ropen(".cache+", O_RDONLY)) >=0) {
	       GDplusfromNet(gd, cachefd, NULL);
	       close(cachefd);
	       return(gd);
	  }
     }

     /* open "." since we just moved there - makes it work when not
	chroot()ing and using relative paths */
     if ((ZeDir = uopendir(".")) == NULL) {
	  char tmpstr[256];
	  getwd(tmpstr);
	  printf("Current Dir is %s\n", tmpstr);
	  fflush(stdout);
	  perror("SOL dude");
	  sprintf(tmpstr, "Cannot access directory '%s'", pathname);
	  Abortoutput(sockfd, tmpstr);
	  return(NULL);
     }

     for (dp = readdir(ZeDir); dp != NULL; dp = readdir(ZeDir)) {
#ifdef CAPFILES
	  char capfile[MAXPATHLEN];
	  FILE *SideFile;

	  strcpy(capfile,".cap/");
	  strcat(capfile, dp->d_name);
#endif

          strcpy(newpath, pathname);
	  strcpy(filename, dp->d_name);
          if (newpath[strlen(newpath)-1] != '/')
               strcat(newpath, "/");
          strcat(newpath, dp->d_name);


	  AddItem = TRUE;
	  gs = Gopherstow;

	  if (strcmp(filename, ".")==0 ||
	      strcmp(filename, "..")==0 ||
	      strncmp(filename, ".cache", 6) ==0||
	      GDCignore(Config,filename))
	       continue;
	  else if (filename[0] == '.' && 
		   strncmp(filename, ".cache",6) != 0 &&
		   isadir(filename)==0) {
	       
	       String *temp;
	       
	       /*** This is a link file, process it after other files ***/
	       
	       temp = STRnew();
	       STRset(temp, filename);
	       
	       STApush(Linkfiles, temp);
	       STRdestroy(temp);
	       continue;
	  } else if (filename[0] == '.') {
	       continue;
	  }
	  
	  ustat(filename, &statbuf);
	  
	  gs = Gopherstow;
	  GSinit(gs);
	  GSsetHost(gs, Zehostname);
	  GSsetPort(gs, GopherPort);
	  GSsetGplus(gs, TRUE);
	  
	  Getfiletypes(newpath, filename, gs);

	  if (GSgetType(gs) == '3')
	       continue;
	  
	  GStitlefromDL(gs, filename); /** Check DL database for a good name**/

	  /* Strip any Decoder extensions from newpath before processing them
	 	filename needs to remain as is for type checking*/
	  
	  if (EXAcasedSearch(Config->Extensions, ext, filename, EXT_DECODER)) {
	       char *foo = GSgetPath(gs);

	       newpath[strlen(newpath) - strlen(EXgetExt(ext))] = '\0';
	       foo[strlen(foo) - strlen(EXgetExt(ext))] = '\0';
	       filename[strlen(filename) - strlen(EXgetExt(ext))] = '\0';
	       
	  }
	  if (GSgetTitle(gs) == NULL)
	       GSsetTitle(gs, filename);
	  
	  GSaddDateNsize(gs, statbuf);

	  /*** Add views, prefixes et al.. ***/
	  if (GDCBlockExtension(Config, filename, ext)) {
	       char *tmpstr = GSgetPath(gs);
	       int num;

	       /** Strip off the extension from the path **/
	       tmpstr[strlen(tmpstr) - strlen(EXgetExt(ext))]='\0';

	       num = GDSearch(gd, GSgetPath(gs));
	       if (num != -1) {
		    gs = GDgetEntry(gd, num);
		    AddItem = FALSE;
	       } 
	       
	       if (strcasecmp(EXgetBlockname(ext), "ASK") == 0) {
		    GSsetAsk(gs, TRUE);
	       }


	       if (isGplus) {
		    GSaddBlock(gs,EXgetBlockname(ext), fixfile(newpath));
	       }
	       if (AddItem == TRUE)
		    GSsetType(gs, '\0');
	  }

	  else if (GDCViewExtension(Config, filename, &ext)) {
	       char *Prefix;
	       int  num;
	       
	       Prefix = EXgetPrefix(ext);

	       strcpy(Pathp, Prefix);
	       strcpy(Pathp+strlen(Prefix), newpath);
	       

	       /*** Strip extension off of pathname***/
	       Pathp[strlen(Prefix)+strlen(newpath)-strlen(EXgetExt(ext))]= '\0';
	       GSsetPath(gs, Pathp);
	       GSsetType(gs, EXgetObjtype(ext));
	       /*** Strip extension off of title***/
	       filename[strlen(filename)-strlen(EXgetExt(ext))]= '\0';
	       
	       /**search for existing entry to add a view to **/
	       num = GDSearch(gd, Pathp);
	       if (num != -1) {
		    if (GSgetType(GDgetEntry(gd,num)) == '\0') {
			 GSsetHost(gs, NULL);
			 GSsetTitle(gs, filename);
			 GSmerge(GDgetEntry(gd, num), gs);
		    }
		    gs = GDgetEntry(gd, num);
		    AddItem = FALSE;
	       } 

	       /** Oh say can we hack, by the dawns early day :-) **/
	       if (strcasecmp(EXgetExt(ext), ".mindex")==0) {
		    GSsetGplus(gs, FALSE);
	       }

	       if (isGplus) {
		    char *lang;

		    lang = EXgetVLang(ext);
		    if (lang == NULL || strcmp(lang, "")==0)
			 lang = GDCgetLang(Config);
		    GSaddView(gs, EXgetView(ext), lang, statbuf.st_size);
	       }
	  } 
	  else if (AddItem) {
	       int num;
	       char type;
	       char *path;

	       num = GDSearch(gd, GSgetPath(gs));
	       if (num != -1) {
		    type = GSgetType(gs);
		    path = GSgetPath(gs);
		    gs = GDgetEntry(gd, num);
		    GSsetType(gs, type);
		    GSsetPath(gs, path);
		    AddItem = FALSE;
	       } 
	       if (isGplus && GSisGplus(gs))
		    AddDefaultView(gs, statbuf.st_size, NULL);
	  }

#ifdef CAPFILES
	  if ((SideFile = rfopen(capfile, "r"))!=0) {
	       Debug("cap file name: %s\n", capfile);
	       Process_Side(SideFile, gs);
	       fclose (SideFile);
	  }
#endif
	  
	  if (isGplus) {
	       char tmpstr[256];
	       char timeval[16];
	       struct tm *tmthing;
	       
	       if (GSgplusInited(gs) == FALSE)
		    GSplusnew(gs);
	       
	       /*** Add admin, abstract entries, etal ***/
	       sprintf(tmpstr, "%s <%s>", 
		       GDCgetAdmin(Config), GDCgetAdminEmail(Config));
	       GSsetAdmin(gs, tmpstr);
	       
	       /** Set mod date entry **/
	       tmthing = localtime(&(statbuf.st_mtime));
	       strftime(timeval,sizeof(timeval), "%Y%m%d%H%M%S", tmthing);
	       sprintf(tmpstr,"%s<%s>", asctime(tmthing),timeval);
	       *(strchr(tmpstr, '\n')) = ' ';
	       
	       GSsetModDate(gs, tmpstr);
	  }
	  
	  /*** Add the entry to the directory ***/
	  if (AddItem) {
	       GDaddGS(gd, gs);
	  } else
	       AddItem = TRUE;
	  
	  
     }

     for (i=0 ; i<STAgetTop(Linkfiles); i++) {
	  int linkfd; 

	  linkfd = uopen(STRget(STAgetEntry(Linkfiles,i)), O_RDONLY);
     
	  if (linkfd >0) {
	       GDfromLink(gd, linkfd, Zehostname, GopherPort, pathname, CurrentPeerName);
	       close(linkfd);
	  }
     }

     closedir(ZeDir);
     
     GDsort(gd);
     
     return(gd);
     
}



GopherDirObj *
GDfromSelstr(cmd,sockfd)
  CMDobj *cmd;
  int sockfd;
{
     char *it = NULL;
     char *cp;
     GopherDirObj *gd;
     char directory[512];

     it = CMDgetFile(cmd);

     if (it == NULL)
	  return(NULL);
     else
	  strcpy(directory, it);

     cp = strrchr(directory, '/');
     if (cp != NULL)
	  *(cp+1) = '\0';
     
     if (rchdir(directory)<0) {
	  char tmpstr[512];
	  sprintf(tmpstr, "- Cannot access directory '%s'", directory);
	  Abortoutput(sockfd, tmpstr);
	  return(NULL);
     }
     
     gd = GDfromUFS(directory, sockfd, TRUE); /** Returns NULL if error **/
     
     return(gd);
}


/*
 * Send item information to client
 */
item_info(cmd, sockfd)
  CMDobj *cmd;
  int sockfd;
{
     GopherDirObj *gd;
     GopherObj *gs;
     int num;
     char tmpstr[256];


     gd = GDfromSelstr(cmd,sockfd);

     /** For now, strip off first character and find the directory above **/
     /* Note that GDfromSelstr will return NULL (i hope) if cant find dir */

     switch (*CMDgetSelstr(cmd)) {
     case '/':
     case '\0':
	  gs = GSnew();
	  GSsetHost(gs, Zehostname);
	  GSsetPort(gs, GopherPort);
	  GSsetPath(gs, "");
	  GSsetTitle(gs, GDCgetSite(Config));
	  GSsetType(gs, '1');
	  GSsetGplus(gs, TRUE);

	  GSsendHeader(sockfd, -1);
	  writestring(sockfd, "+INFO ");
	  GStoNet(gs,sockfd);
	  sprintf(tmpstr, "+ADMIN:\r\n Admin: %s <%s>\r\n", GDCgetAdmin(Config),
		  GDCgetAdminEmail(Config));
	  writestring(sockfd, tmpstr);
	  sprintf(tmpstr, " Site: %s\r\n", GDCgetSite(Config));
	  writestring(sockfd, tmpstr);
	  sprintf(tmpstr, " Org: %s\r\n", GDCgetOrg(Config));
	  writestring(sockfd, tmpstr);
	  sprintf(tmpstr, " Loc: %s\r\n", GDCgetLoc(Config));
	  writestring(sockfd, tmpstr);
	  sprintf(tmpstr, " Geog: %s\r\n", GDCgetGeog(Config));
	  writestring(sockfd, tmpstr);
	  sprintf(tmpstr, " Version: U of Minnesota Unix %s.%s pl%d\r\n",
		  GOPHER_MAJOR_VERSION, GOPHER_MINOR_VERSION, PATCHLEVEL);
	  writestring(sockfd, tmpstr);
	  writestring(sockfd, "+VERONICA:\r\n treewalk:");
	  if (GDCgetShouldIndex(Config) == TRUE)
	       writestring(sockfd, " yes");
	  else
	       writestring(sockfd, " no");

	  writestring(sockfd, "\r\n+VIEWS:\r\n");
	  writestring(sockfd, " application/gopher-menu: <0k>\r\n");
	  writestring(sockfd, " application/gopher+-menu: <0k>\r\n");
	  writestring(sockfd, " Directory/recursive: <0k>\r\n");
	  writestring(sockfd, " Directory+/recursive: <0k>\r\n");

	  break;

     default:
	  num = GDSearch(gd, CMDgetSelstr(cmd));
	  uchdir("/");


	  if (num < 0) {
	       GplusError(sockfd, 1, "Cannot find item information for that item");
	       return;
	  }
	  else {
	       gs = GDgetEntry(gd, num);
	       GSsendHeader(sockfd, -1);
	       GSplustoNet(gs, sockfd, NULL);
	  }
	  break;

     }
     writestring(sockfd, ".\r\n");
}
     

/*
** This function lists out what is in a particular directory.
** it also outputs the contents of link files.
**
** It also checks for the existance of a .cache file if caching is
** turned on...
**
** Ack is this ugly.
*/

void
listdir(sockfd, pathname, isgplus, view, filter)
  int sockfd;
  char *pathname;
  boolean isgplus;
  char *view;
  char *filter;
{
     GopherDirObj *gd;
     boolean      attrlist = FALSE;
     boolean      Recurse  = FALSE;
     char         *filtereddata[16], **filtered=filtereddata;
     int          i=0;
     StrArray     *RecurseDirs;
     String       *stapath;
     String       *pushstring = STRnew();

     if (uchdir("/"))
	  perror("SOL dude");

     if (filter != NULL) {
	  while (*filter != '\0') {
	       if (*filter=='+') {
		    *filter = '\0';
		    filtered[i] = filter+1;
		    filter++; i++;
	       }
	       filter++;
	  }
	  filtered[i] = NULL;
     } else
	  filtered = NULL;


     if (view != NULL) {
	  if (strncmp(view, "application/gopher+-menu",24) == 0)
	       attrlist = TRUE;
	  if (strncmp(view, "Directory+/recursive", 20)==0)
	       Recurse = TRUE;
	  if (strncmp(view, "Directory/recursive", 19)==0)
	       Recurse = TRUE;
     }
     if (Recurse)
	  RecurseDirs = STAnew(32);

     if (Can_Browse(sockfd) == FALSE) {
	  Abortoutput(sockfd,  GDCgetBummerMsg(Config));
	  LOGGopher(sockfd, "Denied access for %s", pathname);
	  return;
     }

     if (rchdir(pathname)<0) {
	  if (errno == EACCES)
#ifdef UMNDES
	       PleaseAuthenticate(sockfd);
#endif
	  ;
	  Abortoutput(sockfd, "- Cannot access that directory");
	  return;
     }

     if (isgplus)
	  GSsendHeader(sockfd, -1);


     do {
	  Debug("Sending %s\n", pathname);

	  if (Recurse) {
	       rchdir("/");
	       if (rchdir(pathname)<0) {
		    continue;
	       }
	  }


	  if (Caching) {

	       if (!attrlist && Cachetimedout(".cache", CACHE_TIME, ".")==FALSE) {
		    /*** Old style cache  ***/
		    send_binary(sockfd, ".cache", FALSE);
		    writestring(sockfd, ".\r\n");
		    return;
	       } else if (Cachetimedout(".cache+", CACHE_TIME, ".")==FALSE) {
		    /*** Gopher+ cache. ***/
		    
		    if (strcmp(view, "application/gopher+-menu")==0) {
			 send_binary(sockfd, ".cache+", FALSE);
			 writestring(sockfd, ".\r\n");
			 return;
		    } else if (strcmp(view, "application/gopher-menu")==0) {
			 send_binary(sockfd, ".cache", FALSE);
			 writestring(sockfd, ".\r\n");
			 return;
		    }
	       }
	  }

	  /** If we didn't cache then we have to load up the directory **/
	  gd = GDfromUFS(pathname, sockfd, attrlist);

	  if (gd == NULL) {
	       /** Should generate an error message here **/
	       return;
	  }
	  
	  rchdir("/");
	  
	  if (attrlist)
	       GDplustoNet(gd, sockfd,filtered);
	  else
	       GDtoNet(gd, sockfd);


	  /*
	   * Write out the cache... *After* we send out the data to the net.
	   */
	  if (Caching) {
	       int cachefd;
	       char cachefile[MAXPATHLEN];
	       
	       strcpy(cachefile, pathname);
	       strcat(cachefile, "/.cache");
	       
	       cachefd = ropen(cachefile, O_WRONLY|O_CREAT|O_TRUNC, 0644);
	       
	       if (cachefd >= 0) {
		    Debug("Caching directory... into %s\n",cachefile);
		    GDtoNet(gd, cachefd);
		    close(cachefd);
	       }
	       
	       if (attrlist) {
		    strcat(cachefile, "+");
		    
		    cachefd = ropen(cachefile, O_WRONLY|O_CREAT|O_TRUNC,0644);
		    
		    if (cachefd >= 0) {
			 GDplustoNet(gd, cachefd,filtered);
			 close(cachefd);
		    }
	       }
	  }

	  
	  if (Recurse) {
	       GopherObj *gs;
	       /** Push entries on the stack **/
	       for (i=0; i< GDgetNumitems(gd); i++) {
		    gs = GDgetEntry(gd, i);
		    if ((GSgetType(gs) == A_DIRECTORY) &&
			(strcmp(GSgetHost(gs), Zehostname) == 0)) {
			 STRset(pushstring,  GSgetPath(gs));
			 STApush(RecurseDirs, pushstring);
		    }
	       }

	       do {
		    stapath = STApop(RecurseDirs);
		    if (stapath == NULL) {
			 Recurse = FALSE;  /** Done **/
			 break;
		    }
		    pathname = STRget(stapath);
		    
		    if (*pathname == 'm')
			 process_mailfile(sockfd, pathname+1);
	       } while (*pathname == 'm');

	       pathname++;

	  }

     } while (Recurse);

     writestring(sockfd, ".\r\n");

}


/*
 * This processes a file containing any subset of
 * Type, Name, Path, Port or Host, and returns pointers to the
 * overriding data that it finds.
 *
 * The caller may choose to initialise the pointers - so we don't
 * touch them unless we find an over-ride.
 */

void
Process_Side(sidefile, Gopherp)
  FILE *sidefile;
  GopherObj *Gopherp;
{
     char inputline[MAXLINE];
     char *cp;


     inputline[0] = '\0';

     for (;;) {
	  for (;;) {
	       cp = fgets(inputline, 1024, sidefile);
	       if (inputline[0] != '#' || cp == NULL)
		    break;
	  }
	  
	  /*** Test for EOF ***/
	  if (cp==NULL)
	       break;
	  
	  ZapCRLF(inputline);  /* should zap tabs as well! */

	  /*** Test for the various field values. **/
	  
	  if (strncmp(inputline, "Type=", 5)==0) {
	       GSsetType(Gopherp, inputline[5]);
	       if (inputline[5] == '7') {
		    /*** Might as well set the path too... ***/
		    *(GSgetPath(Gopherp)) = '7';
	       }
	       if (inputline[5] == '9') {
		    /*** Might as well set the path too... ***/
		    *(GSgetPath(Gopherp)) = '9';
	       }
	  }

	  else if (strncmp(inputline, "Name=", 5)==0) {
	       GSsetTitle(Gopherp, inputline+5);
	  }

	  else if (strncmp(inputline, "Host=", 5)==0) {
	       GSsetHost(Gopherp, inputline+5);
	  }

	  else if (strncmp(inputline, "Port=", 5)==0) {
	       GSsetPort(Gopherp, atoi(inputline+5));
	  }

	  else if (strncmp(inputline, "Path=", 5)==0) {
	       GSsetPath(Gopherp, inputline+5);
	  }

	  else if (strncmp(inputline, "Numb=", 5)==0) {
	       GSsetNum(Gopherp, atoi(inputline+5));
	  }

	  else if (strncmp(inputline, "Name=", 5)==0) {
	       GSsetTitle(Gopherp, inputline+5);
	  }

	  else if (strncmp(inputline, "Abstract=", 9)==0) {
	       GSsetAbstract(Gopherp, inputline+9);
	  }

     }

     fclose(sidefile);
}





/*
** This function opens the specified file, starts a zcat if needed,
** and barfs the file across the socket.
**
** It now also checks and sees if access is allowed
**
**  This also used the global variable ASKfile
**
*/

void
printfile(sockfd, pathname, startbyte, endbyte, Gplus)
  int     sockfd;
  char    *pathname;
  int     startbyte, endbyte;
  boolean Gplus;
{
     FILE *ZeFile;
     char inputline[MAXPATHLEN];
     FILE *pp;


     /*** Check and see if the peer has permissions to read files ***/
     
     if (Can_Read(sockfd) == FALSE) {
	  if (writestring(sockfd, GDCgetBummerMsg(Config)) <0)
	       LOGGopher(sockfd, "Client went away"), exit(-1);
	  LOGGopher(sockfd,"Denied access for %s", pathname);
	  Abortoutput(sockfd, "\r\nBummer.....\r\n.\r\n");
	  return;
     }


     if ( (ZeFile = rfopen(pathname, "r")) == NULL) {
	  /*
	   * The specified file does not exist
	   */
	  char notexistline[256];
	  sprintf(notexistline, "'%s' does not exist!!", pathname);
	  Abortoutput(sockfd, notexistline);

	  return;
     }

     if (startbyte != 0)
	  fseek(ZeFile, startbyte, 0);
     
     if (pp = specialfile(sockfd, ZeFile, pathname)) {	/* Ick uses global ASKfile*/
	  fclose(ZeFile);
	  ZeFile = pp;
     }

     if (Gplus)
	  GSsendHeader(sockfd, -1);

     while (fgets(inputline, sizeof(inputline), ZeFile) != NULL) {

	  ZapCRLF(inputline);

	  /** Period on a line by itself, double it.. **/
	  if (*inputline == '.' && inputline[1] == '\0') {
	       inputline[1] = '.';
	       inputline[2] = '\0';
	  }

	  strcat(inputline, "\r\n");
	  if (writestring(sockfd, inputline) <0)
	       LOGGopher(sockfd, "Client went away"), exit(-1);

	  if (endbyte >0) {
	       if (ftell(ZeFile) >= endbyte)
		    break;
	  }
     }

     Specialclose(ZeFile);

     if (writestring(sockfd, ".\r\n")<0)
	  LOGGopher(sockfd, "Client went away"), exit(-1);
}


#define BUFSIZE 1523  /* A pretty good value for ethernet */

void
send_binary(sockfd, filename, isGplus)
  int sockfd;
  char *filename;
  boolean isGplus;
{

     FILE *sndfile,*pp;
     unsigned char in[BUFSIZE];
     register int j;
     int gotbytes, size;
     struct stat buf;

     /** Don't check decoder/extension if .cache **/
     if (strncmp(filename, ".cache",6)!=0) {

	  if (Can_Read(sockfd) == FALSE) {
	       if (writestring(sockfd, GDCgetBummerMsg(Config)) <0)
		    LOGGopher(sockfd, "Client went away"), exit(-1);
	       writestring(sockfd, "\r\nBummer.....\r\n.\r\n");
	       LOGGopher(sockfd, "Denied access for %s", filename);
	       return;
	  }
	  
	  if (strcmp(filename, "-") == 0) {
	       /*** Do some live digitization!! **/
	       sndfile = popen("record -", "r");
	  }
	  else
	       sndfile = rfopen(filename, "r");
	  
	  if (!sndfile) {
	       /*
		* The specified file does not exist
		*/
	       char notexistline[256];
	       sprintf(notexistline, "'%s' does not exist!!", filename);
	       Abortoutput(sockfd, notexistline);
	       
	       return;
	  }

	  if (pp = specialfile(sockfd, sndfile, filename)) {
	       fclose(sndfile);
	       sndfile = pp;
	  }
     } else
	  sndfile = rfopen(filename, "r");


     if ((isGplus) && strcmp(filename, "-") == 0) 
	  GSsendHeader(sockfd, -2);
     else if (isGplus) {
	  rstat(filename, &buf);
	  size = buf.st_size;
	  GSsendHeader(sockfd, size);
     }


     while(1) {
	  gotbytes = fread(in, 1, BUFSIZE, sndfile);
	  
	  if (gotbytes == 0)
	       break;       /*** end of file or error... ***/

          j = writen(sockfd, in, gotbytes);

	  if (j == 0)
	       break;       /*** yep another error condition ***/

     }
     Specialclose(sndfile);
}

