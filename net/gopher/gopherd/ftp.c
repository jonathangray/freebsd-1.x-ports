/********************************************************************
 * lindner
 * 3.12
 * 1993/08/23 21:43:21
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopherd/ftp.c,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: ftp.c
 * Routines to translate gopher protocol to ftp protocol.
 *********************************************************************
 * Revision History:
 * ftp.c,v
 * Revision 3.12  1993/08/23  21:43:21  lindner
 * Fix for bug with symlinks
 *
 * Revision 3.11  1993/08/23  20:51:48  lindner
 * Multiple fixes from Matti Aarnio
 *
 * Revision 3.10  1993/08/11  14:41:48  lindner
 * Fix for error logging and uninitialized gs
 *
 * Revision 3.9  1993/08/11  14:08:39  lindner
 * Fix for hanging ftp gateway connections
 *
 * Revision 3.8  1993/08/03  20:43:54  lindner
 * Fix for sites that have @ -> for symbolic links
 *
 * Revision 3.7  1993/08/03  06:40:11  lindner
 * none
 *
 * Revision 3.6  1993/08/03  06:14:12  lindner
 * Fix for extra slashes
 *
 * Revision 3.5  1993/07/30  19:21:03  lindner
 * Removed debug stuff, fix for extra slashes
 *
 * Revision 3.4  1993/07/30  18:38:59  lindner
 * Move 3.3.1 to main trunk
 *
 * Revision 3.3.1.7  1993/07/29  21:42:21  lindner
 * Fixes for Symbolic links, plus removed excess variables
 *
 * Revision 3.3.1.6  1993/07/27  05:27:42  lindner
 * Mondo Debug overhaul from Mitra
 *
 * Revision 3.3.1.5  1993/07/26  17:18:55  lindner
 * Removed extraneous abort printf
 *
 * Revision 3.3.1.4  1993/07/26  15:34:21  lindner
 * Use tmpnam() and bzero(), plus ugly fixes..
 *
 * Revision 3.3.1.3  1993/07/23  03:12:29  lindner
 * Small fix for getreply, reformatting..
 *
 * Revision 3.3.1.2  1993/07/07  19:39:48  lindner
 * Much prettification, unproto-ed, and use Sockets.c routines
 *
 * Revision 3.3.1.1  1993/06/22  20:53:21  lindner
 * Bob's ftp stuff
 *
 * Revision 3.3  1993/04/15  04:48:09  lindner
 * Debug code from Mitra
 *
 * Revision 3.2  1993/03/24  20:15:06  lindner
 * FTP gateway now gets filetypes from gopherd.conf
 *
 * Revision 3.1.1.1  1993/02/11  18:02:51  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.2.1 1993/04/02 12:00:01  alberti
 * Robert Alberti, alberti@boombox.micro.umn.edu, 02. Apr 93
 * Extensively rewritten tell different server types apart
 *
 * Andi Karrer, karrer@bernina.ethz.ch, 21. Feb 93
 * Added support for VMS FTP servers that do not understand the FTP PASV
 * command (Wollongong, Multinet). Also present these servers output
 * in a Unix fashion.
 * Also works with Ultrix 4.2 FTP servers whose PASV command is broken.
 *
 * Revision 1.2  1993/01/11  23:18:09  lindner
 * Changed password to be the host of the remote client, not the gateway.
 *
 * Revision 1.1  1992/12/10  23:13:27  lindner
 * gopher 1.1 release
 *
 *
 *********************************************************************/


/* -------------------------------------------------
 *     g2fd.c          Gopher to FTP gateway daemon.
 *     Version 0.3 Hacked up: April 1992.  Farhad Anklesaria.
 *     Based on a Perl story by John Ladwig.
 *     Based on a Perl story by Farhad Anklesaria.
 *
 *      Modified by Greg Smith, Bucknell University, 24 Nov 1992
 *      to handle multiline status replies.
 *
 ---------------------------------------------------- */

#include "gopherd.h"
#include <signal.h>

#include <stdio.h>
#include "ext.h"
#include "Debug.h"

#define GETFILE 0
#define GETBINARY 1
#define GETDIR 2

#define FTPISUNKNOWN 0
#define FTPISPASV   1
#define FTPISVMS    3

#define FTPISNOVELL 61
#define FTPISPORT   7
#define FTPISUNIX   8

#define DUMP 1
#define KEEP 0

char    *ftptmp;

#define SLEN 256

static int sockfd;          /* I hate globals, but these are ubiquitous comm */
static int ftp_control;     /* channels and not about to change, so... */


/*** Some forward declarations ***/
boolean NotText();
int     Abort();
boolean IsBinaryType();
void    Cleanup();
void    FailErr();
int     getreply();
char    *WalkDirs();

/*********** Common Utility for I/O ***************/

int
getreply(level,inputline,maxlen)
  int level;
  char *inputline;
  int maxlen;
{
     int i, j, errval, origval;
     char errcode[4];
     
     i=readline(ftp_control, inputline, maxlen);
     
     strncpy(errcode, inputline, 3);
     errcode[3] = '\0';

     origval = errval = atoi(errcode);
     
     /* If this is not in an expected range */
     if (errval > level && inputline[3] != '-')           
	  Abort(":-(",inputline);    /* display and abort */
     else if (errval > level)
	  FtpAbort(":-(", inputline);
     
     if (inputline[3] == '-') /* if a continuance line */
	  while (errval == origval)
	  {
	       i = readline(ftp_control, inputline, maxlen);
	       
	       for (j = 0; (j < 4) && isspace(inputline[j]); j++);
	       
	       if (j == 4) continue;
	       
	       strncpy(errcode, inputline, 3);
	       errval = atoi(errcode);
	       
	       if (inputline[3] == '-') 
		    continue;
	       
	       if ((inputline[3] == ' ') && (errval == origval))
		    errval = 0;
	  } 
     
     return (0);
}

/***************** Connect, Login, and Disconnection routines *********/

/* 
  Establish connection, validate DNS name,
  connect and return control pointer
*/

int
FtpConnect(host)
  char *host; 
{
     int newcontrol;
     GopherObj *gs;
     
     /*** hook onto the gs code to do our ftp socket connects ***/
     gs = GSnew();
     
     /** Get ready for some cleanup action **/
     signal(SIGPIPE, Cleanup);
     signal(SIGINT, Cleanup);
     signal(SIGALRM, Cleanup);
     
     /*** Open an ftp control connection ***/
     GSsetHost(gs, host);
     GSsetPort(gs, 21);
     
     newcontrol = GSconnect(gs);
     if (newcontrol < 0)
	  Abort("FtpConnect","Unable to connect to ftp server!");
     
     return(newcontrol);
}

void
FtpLogin()
{
     char      inputline[512];
     char      ipnum[64];
     /*** Send username ***/
     
     writestring(ftp_control, "USER anonymous\r\n");
     
     getreply(399,inputline,sizeof(inputline));
     
     writestring(ftp_control, "PASS -gopher@");
     inet_netnames(sockfd, inputline, ipnum);
     writestring(ftp_control, inputline);
     writestring(ftp_control, "\r\n");
     
     getreply(399, inputline,sizeof(inputline));
}

void 
QuitClose(ftp_data)
  int ftp_data;
{
     
     writestring(ftp_control, "QUIT\r\n");
     
     close(ftp_data);
     close(ftp_control);
}

/***************** Issue Query routines *********************/
int
PASVQuery(ftptype, path)
  int ftptype;
  char *path;
{
     int       termCh;
     char      *cp;
     char      inputline[512];
     char      buf[1600];     /*** Nice MTU size ***/
     int       tmpfd;
     int       ftp_data;
     int       ftp_dataport, nRead;
     int       getting;	
     static GopherObj *gs = NULL;
     
     if (gs == NULL)
	  gs = GSnew();
     else
	  GSinit(gs);


     /**** Send PASV and set up the data port ***/
     writestring(ftp_control, "PASV\r\n");
     
     getreply(299, inputline,sizeof(inputline));
     
     /*** Find out the port number of the data connection ***/
     inputline[strlen(inputline)] = '\0';  /** Zap the right paren **/
     cp = strrchr(inputline, ',');         /** lower order octet **/
     if (cp == NULL)  Abort("PVQ2","cannot ftp!");
     *cp = '\0';
     cp ++;
     ftp_dataport = atoi(cp);
     
     cp = strrchr(inputline, ',');         /** upper octet **/
     if (cp == NULL)  Abort("PVQ3"," cannot ftp!");
     *cp = '\0';
     cp ++;
     ftp_dataport = atoi(cp) * 256 + ftp_dataport;
     
     GSsetPort(gs, ftp_dataport);
     ftp_data = GSconnect(gs);
     if (ftp_data < 0)
	  Abort("PVQ4","Unable to establish data connection!");
     
     termCh = path[strlen(path)-1]; /* Grab possible end char: / etc */
     
     if (termCh == '/') {     /* We have a directory */
	  getting = GETDIR;       /* Specify it's not a file */
	  
	  switch (ftptype)
          {
          case FTPISNOVELL:
	       writestring(ftp_control, "LIST ");
	       break;
          default:
	       writestring(ftp_control, "LIST -LF "); /* Issue a directory request */
	       break;
          }
	  
	  if (strlen(path) > 0) /* Tack directory name onto LIST -LF command */
	       writestring(ftp_control, path);
	  
	  writestring(ftp_control, "\r\n");
	  
	  getreply(199, inputline,sizeof(inputline));
	  
     }
     else
     {                     /* We have a file */
	  getting = GETFILE;      /* Set flag */
	  
	  if ((getting = IsBinaryType(path)) == GETBINARY)
	  {
	       writestring(ftp_control, "TYPE I\r\n");
	       getreply(299, inputline,sizeof(inputline));
	  }
	  
	  writestring(ftp_control, "RETR ");
	  writestring(ftp_control, path);
	  writestring(ftp_control, "\r\n");
	  
	  getreply(199, inputline, sizeof(inputline));
     }
     
     /*** Transfer the data... ***/
     
     ftptmp = tempnam("/tmp", "gftp");
     
     tmpfd = uopen(ftptmp, O_RDWR|O_CREAT,0755);
     
     if (tmpfd < 0)
          Abort("PVQ8","Sorry, out of tmp transfer space!");
     
     while ((nRead = read(ftp_data, buf, sizeof buf)) > 0)
	  writen(tmpfd, buf, nRead);
     
     QuitClose(ftp_data);
     
     return(getting);
}

void
xLateBinary(fp)
  FILE *fp;
{
     int fd, nRead;
     char buf[BUFSIZ];

     /* Read/write file, gotta make this a memory xfer */
     Debug("Binary file\n",0);
     
     fd = fileno(fp);
     
     while ((nRead = read(fd, buf, sizeof buf)) > 0)
	  writen(sockfd, buf, nRead);
     
     fclose(fp);
}

/*--------------------------------*/
/* Used in xLateText below --------*/
boolean
NotText(buf)
  char * buf;
{
     int max;   char *c;
     
     if ((max = strlen(buf)) >= (BUFSIZ - 50)) max = BUFSIZ - 50;
     for (c = buf; c < (buf + max); c++) {
	  if (*c > '~') return(TRUE);
     }
     return(FALSE);
}

/*--------------------------------*/

int xLateText(fp)
  FILE *fp;
{
     char buf[BUFSIZ];
     int checkIt;
     
     
     while (fgets(buf, sizeof buf, fp) != NULL)
     {
	  Debug(">%s",buf);
	  
	  if (checkIt)
	  { /* Just peek at it once */
	       checkIt = 0;
	       
	       if (NotText(buf))
	       {
		    Abort("xLT","Sorry.  File does not appear to contain text.");
		    fclose(fp);
		    return(0);
	       }
	  }
	  
	  ZapCRLF(buf);
	  
	  FailErr(writestring(sockfd, buf));
	  FailErr(writestring(sockfd, "\r\n"));
     }
     
     fclose(fp);
     FailErr(writestring(sockfd, ".\r\n"));
     return(1);
}



/*********************************************************************
   xLateList grabs LIST output, trims any
   trailing spaces, and passes it to GopherList 
   Gopherlist clips the front off of the buf line 
   and puts the display name in theName 
*/

void
xLateList(ftptype, host, path, fp) 
  int ftptype;                      
  char *host;         
  char *path;
  FILE *fp;
{
     char *buf;
     char *theName;
     char outputline[512];
     
     buf = (char *)malloc(BUFSIZ);
     theName = (char *)malloc(BUFSIZ);

     /* fill buf with data from ftp server */
     while (fgets(buf, BUFSIZ, fp) != NULL)
     {
	  ZapCRLF(buf);
	  
	  /* GopherList will Clip the long-list buf into a name-only buf */
	  
	  if (GopherList(buf, theName, ftptype) == -1) continue;
	  
	  sprintf(outputline, "%s\tftp:%s@%s%s\t%s\t%d\r\n",
                  theName,host, path, buf, Zehostname, GopherPort);
	  
	  Debug("{%s",outputline);
	  
	  FailErr(writestring(sockfd, outputline));
     }
     
     fclose(fp);
     free(buf);
     free(theName);
     FailErr(writestring(sockfd, ".\r\n"));
}

void
xLateDir(ftptype, host, path, fp)
  int ftptype;
  char *host;
  char *path;
  FILE *fp;
{
     char *buf;
     char outputline[512];
     char *theName;
     
     buf = (char *)malloc(BUFSIZ);
     theName = (char *)malloc(BUFSIZ);
     
     while (fgets(buf, BUFSIZ, fp) != NULL)
     {
	  Debug("]%s\n",buf);
	  
	  ZapCRLF(buf);
	  
	  GopherType(buf, theName, ftptype);
	  
	  switch(ftptype)
	  {
	  case FTPISNOVELL:
	       sprintf(outputline, "%s\tftp:%s@%s%s\t%s\t%d\r\n", theName,
		       host, path, &buf[FTPISNOVELL], Zehostname, GopherPort);
	       break;
	  case FTPISPORT:
	       sprintf(outputline, "%s\tftp:%s@%s%s\t%s\t%d\r\n", theName,
		       host, path, &buf[FTPISPORT], Zehostname, GopherPort);
	       break;
	  case FTPISUNIX:
	  case FTPISUNKNOWN:
	       sprintf(outputline, "%s\tftp:%s@%s%s\t%s\t%d\r\n", theName,
		       host, path, &buf[FTPISUNIX], Zehostname, GopherPort);
	       break;
	  default:
	       sprintf(outputline, "%s\tftp:%s@%s%s\t%s\t%d\r\n", theName,
		       host, path, buf, Zehostname, GopherPort);
	       break;
	  }
	  
	  FailErr(writestring(sockfd, outputline));
     }
     
     fclose(fp);
     free(buf);
     free(theName);
     FailErr(writestring(sockfd, ".\r\n"));
}

/*--------------------------------*/

FILE *OpenOrDie(file, mode)
  char *file, *mode;
{
     FILE *fp, *fopen();
     if ((fp = ufopen(file, mode,0755)) != NULL) {
	  return(fp);
     } else {
	  Cleanup();
	  exit(-1);
     }
     return(NULL);  /** Shouldn't get here **/
}

/*--------------------------------*/
void
xLateResults(ftptype, getting, host, path)
  int ftptype;
  int getting;
  char *host;
  char *path;
{
     FILE  *fp;
     
     signal(SIGPIPE, Cleanup);
     signal(SIGINT, Cleanup);
     
     fp = OpenOrDie(ftptmp, "r");
     
     switch(getting)
     {
     case GETFILE:
	  xLateText(fp);
	  break;
     case GETBINARY:
	  xLateBinary(fp);
	  break;
     case GETDIR:
	  switch(ftptype)
	  {
	  case FTPISUNKNOWN:
	  case FTPISVMS:
	       Debug("xLate: DIR case\n",0);
	       xLateDir(ftptype, host, path, fp);
	       break;
	  default:
	       Debug("xLate: LIST case\n",0);
	       xLateList(ftptype, host, path, fp);
	       break;
	  }


	  break;
     default:
	  Abort("XR:","Shouldn't get here!\n");
     }
     
     Cleanup();
}

/*--------------------------------*/
  
int
Abort(where, complaint)
  char *where;
  char *complaint;
{
     int i;
     char inputline[BUFSIZ], errmsg[BUFSIZ+20];
     
     strcpy(inputline, complaint);
     
     ZapCRLF(inputline);
     sprintf(errmsg, "%c%s:%s\t\terror.host\t1\r\n",A_ERROR, where, inputline);
     
     writestring(sockfd, errmsg);
     bzero(inputline, BUFSIZ);
     writestring(sockfd, ".\r\n");

     Cleanup();
     exit(1);
     ;
}

int
FtpAbort(where, complaint)
  char *where;
  char *complaint;
{
     int i;
     char inputline[BUFSIZ], errmsg[BUFSIZ+20];
     
     strcpy(inputline, complaint);
     
     while (i)
     {
	  ZapCRLF(inputline);
	  sprintf(errmsg, "%c%s:%s\t\terror.host\t1\r\n",A_ERROR, where, &inputline[4]);
	  
	  writestring(sockfd, errmsg);
	  bzero(inputline, BUFSIZ);
	  i=readline(ftp_control, inputline, BUFSIZ);
	  if (i <= 0) break;
     }
     
     writestring(sockfd, ".\r\n");
     fflush(stdout);
     Cleanup();
     exit(1);
}

/*--------------------------------*/

boolean
IsBinaryType(path)
  char *path;
{
     char Gtype;
     Extobj *ext;
     
     if (GDCViewExtension(Config, path, &ext)) {
	  Gtype = EXgetObjtype(ext);
	  
	  if(Gtype == A_FILE || Gtype == A_MACHEX) 
	       return FALSE;
	  else
	       return TRUE;
     } else
	  return(FALSE);
     
}


/*--------------------------------*/
void
TrimEnd(bufptr)
  char *bufptr;
{
     int last;
     for (last = strlen(bufptr) - 1; isspace(bufptr[last]) ; bufptr[last--] = '\0')
	  ;
}

int
Vaxinate(bufptr)
  char *bufptr;
{
     int last;
     
     /* strip ugly VMS version numbers */
     for (last = strlen(bufptr) - 1; (last>1) && (isdigit(bufptr[last]) || bufptr[last] == ';') ; last--)
          bufptr[last] = '\0';
     
     /* if bufptr ends in ".dir", it's a directory, replace ".dir" with "/" */
     if((last > 3) && (strncmp(bufptr + last - 3, ".dir", 4) == 0))
     {
	  last -= 3;
	  bufptr[last] = '/';
	  bufptr[last+1]  = '\0';
     }
     return(last);
}

int
GopherType(bufptr, theName, ftptype)
  char *bufptr;
  char *theName;
  int   ftptype;
{
     int last;
     
     
     if (ftptype == FTPISVMS) 
	  last = Vaxinate(bufptr);
     else
	  last = strlen(bufptr)-1;
     
     
     if (bufptr[last] == '/')
     {
	  sprintf(theName, "%c%s", A_DIRECTORY, bufptr);
	  theName[strlen(theName)-1] = '\0';
	  return(A_DIRECTORY);
     }
     
     if ((bufptr[last] == '*') || (bufptr[last] == '@'))     /* Hack out * and @ */
	  bufptr[last] = '\0';
     
     
     return(GopherFile(bufptr, theName, ftptype));
}



/* At this point we're looking at a file */

int
GopherFile(buf, theName, ftptype)
  char *buf;
  char *theName;
  int ftptype;
{
     char	Gtype;
     int	last;
     char	tmpName[SLEN];	
     Extobj     *ext;
     
     last = strlen(buf) -1;
     
     strcpy(tmpName, buf);
     if (buf[last] == '/') {
	  tmpName[last] = '\0';
	  sprintf(theName, "%c%s", A_DIRECTORY, tmpName);
	  return;
     }
     if ((buf[last] == '*') || (buf[last] == '@')) {	/* Hack out * and @ */
	  buf[last] = '\0';
	  tmpName[last] = '\0';
     }
     
     /* At this point we're looking at a file */
     if (GDCViewExtension(Config, buf, &ext)) {  
	  Gtype = EXgetObjtype(ext);
	  
	  sprintf(theName, "%c%s", Gtype, tmpName);
	  return;
     }
     
     sprintf(theName, "%c%s", A_FILE, tmpName);
     return;		/* Some other and hopefully text file */
     ;
}

int
ParseUnixList(bufptr, IntName, theName, ftptype, kind)
  char *bufptr;
  char *IntName;
  char *theName;
  int ftptype;
  int kind;
{
     int i, end;
     int gap;
     char *dirname, *alias;
     
     end = strlen(bufptr);
     
     for (i= 0, gap=0; (gap < kind) && (i < end); gap++)
     {
	  /* Skip chars to white */
	  for (;(!isspace(bufptr[i])) && (i < end); i++);
	  
	  if (i >= end) 
	       Abort ("PUL","said it was Unix but wasn't");
	  
	  /* Skip white to chars */
	  for (;isspace(bufptr[i]) && (i < end); i++);
	  
	  if (i >= end) Abort ("PUL","said Unix but wasn't");
     }
     
     /* Point at supposed start-of-fileIntName */
     dirname = alias = &bufptr[i]; 
     
     if (dirname[strlen(dirname)-1] == '/')
	  dirname[strlen(dirname)-1] = '\0';

     switch(bufptr[0])
     {
     case 'l': /* Link? Skip to REAL IntName! */
	  /* [mea] Or do you ? Handle the symlink semantics ??
	     Is it really a directory, or a file ?       */

 	  /* Data is of   foobar@ -> barfoo format, that is, separator is
 	     5 characters "@ -> " */
	  

	  for (dirname = alias ; (*dirname != '\0') && (dirname != NULL);
	       ++dirname) {
	       if (strncmp(dirname, "@ -> ",5) == 0)
		    break;
	  }
	  if (dirname == NULL)
	       return(-1); /* No real DirName?  Hm.  Oh well */
	  
	  /*Internal name in 'IntName' */
 	  strncpy(IntName,alias,dirname-alias);
 	  IntName[dirname-alias] = 0; /* Make sure it terminates */

	  /* Display name in theName */
	  sprintf(theName, "%c%s", A_DIRECTORY, IntName );

	  if (IntName[strlen(IntName)-1] == '@')
	       IntName[strlen(IntName)-1] = '\0';

	  /* Tag slash on end */
	  sprintf(bufptr, "%s/", IntName);
	  
	  return(A_DIRECTORY);
	  break;
	  
     case 'd': /* Now treat as regular directory */
	  /* Display name in theName */
	  sprintf(theName, "%c%s", A_DIRECTORY, dirname);

	  /*Internal name in 'IntName' */
	  strcpy(IntName,dirname); 

	  /* Tag slash on end */
	  sprintf(bufptr, "%s/", IntName);
	  return(A_DIRECTORY);
	  break;
	  
     default: 
	  /* Determine type of file */
	  strcpy(IntName, dirname); 
	  GopherType(IntName, theName, ftptype);
	  strcpy(bufptr, theName+1);
	  break;
     }
     return(i);
}



/*************************************************************
 Takes the LIST output and 
 truncates it to just the name
 Handles special cases for different
 formats of LIST output
******************************/

int
GopherList(bufptr, theName, ftptype) 
  char *bufptr;
  char *theName;
  int   ftptype;
{
     int i;
     char *IntName;
     
     IntName = (char *)malloc(BUFSIZ);
     
     /* Skip 'total' line */
     if (strncmp(bufptr, "total", 5) == 0) return (-1); 

     /* Trim whitespaces */
     TrimEnd(bufptr); 
     
     switch (ftptype)
     {
     case FTPISUNIX:
	  i = ParseUnixList(bufptr, IntName, theName, ftptype, FTPISUNIX);
	  break;
     case FTPISPORT:
	  i = ParseUnixList(bufptr, IntName, theName, ftptype, FTPISPORT);
	  break;
     case FTPISNOVELL:
	  strcpy(IntName,&bufptr[FTPISNOVELL]);
	  
	  if (bufptr[0] == 'd')
	  {
	       sprintf(theName, "%c%s", A_DIRECTORY, IntName);
	       sprintf(bufptr, "%s/", IntName);
	       return(A_DIRECTORY);
	  }
	  else
	  {
	       i = GopherType(IntName, theName, ftptype);
	       sprintf(bufptr, "%c%s", i,IntName);
	  }
	  break;
	  
     default:
	  break;
     }
     return(i);
}


/*--------------------------------*/

void
Cleanup()
{
     unlink(ftptmp);
     Debug("Cleaning up %s\n", ftptmp);
     
     exit(1);
}

/*--------------------------------*/
void
FailErr(result)
  int result;
{
     if (result < 0) {
	  Cleanup();
     }
}

/*--------------------------------*/
/* CWD into proper directory */

char *
WalkDirs(path)
  char *path;
{
     char *beg, *end;
     char vmspath[256], inputline[512];
     
     /* path looks like '/dir/dir.../dir/' now. Because Wollongong
      * wants "CWD dir/dir/dir" and Multinet wants "CWD dir.dir.dir"
      * so we do the CWD in a stepwise fashion. Oh well...
      */
     
     beg = path+1;
     for (end = beg; (*end != '\0') && (*end != '/'); ++end); 
     
     while (*end != '\0')
     {
	  bzero(vmspath, 256);
	  strncpy(vmspath, beg, end-beg);
	  
	  writestring(ftp_control, "CWD ");
	  writestring(ftp_control, vmspath);
	  writestring(ftp_control, "\r\n");
	  
	  getreply(299,inputline,sizeof(inputline));
	  
	  beg=end+1; /* Skip slash */
	  for (end = beg; (*end != '\0') && (*end != '/'); ++end); 
     }
     
     return(beg);
}

int
PortxFerDir(ftptype, path)
  int ftptype;
  char *path;
{
     char inputline[512];
     
     WalkDirs(path); /* Advance down directories to bottom */
     
     switch(ftptype) /* Send appropriate directory command */
     {
     case FTPISUNKNOWN:
	  Debug("PxFD: LIST -LF\n",0);
	  writestring(ftp_control, "LIST -LF\r\n");
	  break;
     case FTPISVMS:
	  Debug("PxFD: NLST\n",0);
	  writestring(ftp_control, "NLST\r\n");
	  break;
     case FTPISUNIX:
     case FTPISPORT:
	  Debug("PxFD: LIST -LF\n",0);
	  writestring(ftp_control, "LIST -LF\r\n");
	  break;

     default:
	  Debug("PxFD: LIST\n",0);
	  writestring(ftp_control, "LIST\r\n");
	  break;
     }
     
     getreply(299,inputline,sizeof inputline);
     
     return(GETDIR);
}

int
PortxFerFile(path)
  char *path;
{
     char *fname;
     char inputline[512];
     int getting;
     
     if ((getting = IsBinaryType(path)) == GETBINARY)
     {
	  writestring(ftp_control, "TYPE I\r\n");
	  
	  Debug("Send: TYPE I\n",0);
	  
	  getreply(299,inputline,sizeof(inputline));
     }
     
     fname = WalkDirs(path);
     
     writestring(ftp_control, "RETR ");
     writestring(ftp_control, fname);
     writestring(ftp_control, "\r\n");
     
     Debug("Send: RETR %s CRLF\n", fname);
     
     getreply(199,inputline,sizeof(inputline));
     return(getting);
}


int
PORTQuery(ftptype, path)
  int ftptype;
  char *path;
{
     int       termCh, i;
     char      inputline[512];
     char      buf[1600];     /*** Nice MTU size ***/
     int       tmpfd;
     int       ftp_data;
     int       ftp_dataport, nRead;
     int       getting;
     struct sockaddr_in we;
     
     if ((ftp_dataport = SOCKlisten(&we)) < 0)
	  Abort("PQ1","No clue what socket error!");
     
     sprintf(inputline, "PORT %d,%d,%d,%d,%d,%d\r\n",
	     (htonl(we.sin_addr.s_addr) >> 24) & 0xFF,
	     (htonl(we.sin_addr.s_addr) >> 16) & 0xFF,
	     (htonl(we.sin_addr.s_addr) >>  8) & 0xFF,
	     (htonl(we.sin_addr.s_addr)      ) & 0xFF,
	     (htons(we.sin_port)        >>  8) & 0xFF,
	     (htons(we.sin_port)             ) & 0xFF);
     
     writestring(ftp_control, inputline); /* Send PORT command */
     
     getreply(299,inputline,sizeof(inputline));
     
     termCh = path[strlen(path)-1]; /* Grab possible end char: / etc  */
     
     if (termCh == '/') /* Directory case */
	  getting = PortxFerDir(ftptype, path);
     else
	  getting = PortxFerFile(path);
     
      ftp_data = SOCKaccept(ftp_dataport, we);
     
     if (ftp_data < 0)
	  Abort("PQ3","Unable to establish data connection!");
     
     /*** Transfer the data... ***/
     ftptmp = tempnam("/tmp", "gftp");
     tmpfd = uopen(ftptmp, O_RDWR|O_CREAT,0755);
     
     if (tmpfd < 0)
	  Abort("PQ4","Sorry, out of tmp transfer space!");
     
     while ((nRead = read(ftp_data, buf, sizeof buf)) > 0)
     {
	  if ((termCh == '/') && (ftptype==FTPISVMS))
	       for (i=0; i<nRead; i++) buf[i] = tolower(buf[i]);
	  
	  writen(tmpfd, buf, nRead);
     }
     
     QuitClose(ftp_data);
     
     return(getting);
}

int
FtpPreAnalyze()
{
     char inputline[512];
     int i;
     
     /*** Strip off the connection message ***/
     getreply(299,inputline,sizeof(inputline));
     
     for (i = 4; i < strlen(inputline); i++) /* Scan first line for OS */
     {
	  if (strncmp(inputline+i, "NetWare", 7) == 0)
	       return(FTPISNOVELL);
	  if (strncmp(inputline+i, "SunOS 4.1", 9) == 0)
	       return(FTPISUNIX);
	  if (strncmp(inputline+i, "ULTRIX", 6) == 0)
	       return(FTPISUNIX);
     }
     return (FTPISUNKNOWN);
}

int
AnalyzeType()
{
     int ftpflag;
     int resplen, i;
     int has_pasv, has_port;
     char  *typeptr;
     char inputline[2048];
     
     writestring(ftp_control, "SYST\r\n");
     
     getreply(999,inputline,sizeof(inputline));
     
     typeptr = inputline+4;
     inputline[3] = 0;
     ftpflag = atoi(inputline);
     
     switch (ftpflag)
     {
     case 215:
	  if (strncmp(typeptr, "MTS", 3) == 0)
	       return(FTPISVMS);
	  if (strncmp(typeptr, "VMS", 3) == 0)
	       return(FTPISVMS);
	  if (strncmp(typeptr, "UNIX", 4) == 0)
	       return(FTPISUNIX);
     case 500:
     case 502:
     default:
	  break;
     }
     
     Debug("Didn't have SYST capabaility, checking HELP commands\n",0);
     
     writestring(ftp_control, "HELP\r\n");
     
     resplen = readline(ftp_control,inputline,sizeof(inputline));
     
     Debug("%s", inputline);
     
     has_pasv = FALSE;
     has_port = FALSE;
     
     if ((resplen) && (strncmp(inputline, "214", 3) != 0))
     {
	  Debug("FTP said %s\nA server that doesnt do HELP?\n", inputline);
	  resplen = 0;
     }
     
     bzero(inputline, sizeof(inputline));
     
     while((resplen > 0) && (strncmp(inputline, "214", 3) != 0))
     {
	  for (i = 0; i < resplen-4; i++)  /* search till next-to-end */
	  {
	       if (   (inputline[i] == 'P')  /* If we don't see a 'P' test no further */
		   && (inputline[i+4] != '*') ) /*And not neutralized by an asterisk */
	       {
		    if (strncmp(inputline+i, "PASV", 4) == 0) /*If it's PASV */
			 has_pasv = TRUE; /* It has PASV */
		    
		    if (strncmp(inputline+i, "PORT", 4) == 0) /*If it's PORT */
			 has_port = TRUE;
	       }
	  }
	  resplen = readline(ftp_control, inputline,sizeof(inputline)); 

	  /* No need to check further if it has both, clear buffer */
	  if (has_pasv && has_port)
	       while (strncmp(inputline, "214", 3) != 0)
		    resplen = getreply(399, inputline, sizeof(inputline));
     }
     
     Debug("PASV = %d, ", has_pasv);
     Debug("%s\n", (has_pasv ? "PASV detected" : "PASV not implemented\n"));
     Debug("PORT = %d, ", has_port);
     Debug("%s\n", (has_port ? "PORT detected" : "PORT not implemented\n"));
     
     if (has_port) return (FTPISPORT);
     if (has_pasv) return (FTPISPASV);
     
     return(FTPISUNKNOWN);
}

/*
 * This function parses the ftp: string, i.e. ftp:moo.com@/pub/ or
 * URL syntax: ftp://moo.com/pub/
 */

char *
ParseQuery(query)
  char *query;
{
     int i;
     int sLen;
     char termCh;
     
     /* Find @ or ? and parse out, abort if not present */
     for (i = 0; (query[i] != '\0') && (query[i] != '@') && (query[i] != '?'); i++);
     
     if ((i < 2) || (query[i] == '\0'))
	  Abort("ParseQ","    Invalid ftp query.");
     
     sLen = strlen(query);/* Point at end of string */
     termCh = query[sLen - 1];    /* Grab possible end char: / etc */
     
     if ((termCh == '*') || (termCh == '@'))
	  query[sLen - 1] = '\0'; /* Clip invalid end chars */
     
     query[i] = '\0';
     
     i++;
     
     return(&query[i]);
}

int
SendFtpQuery(ctrl_sockfd, query)
  int ctrl_sockfd;
  char *query;
{
     int ftptype;
     int getting;
     char *path;
     
     sockfd = ctrl_sockfd; /* Make it global, makes life easier */
     
     path = ParseQuery(query);
     
     Debug(":Query=%s\n", query);
     Debug(":Path =%s\n", path);
     
     ftp_control = FtpConnect(query); /* Use query str to make control conn*/
     Debug("Connected\n",0);
     ftptype= FtpPreAnalyze();
     
     FtpLogin();
     
     if (ftptype == FTPISUNKNOWN)
	  ftptype = AnalyzeType();
     
     if (!ftptype)
	  Abort ("SFQ1","This server thinks it's an FTP server, but doesn't do PORT or PASV?!\n");
     
     switch(ftptype)
     {
     case FTPISPASV:
	  Debug("About to PASVQuery\n",0);
	  getting = PASVQuery(ftptype, path);
	  break;
     default:
	  Debug("About to PORTQuery %d\n", ftptype);
	  getting = PORTQuery(ftptype, path);
	  break;
     }
     
     xLateResults(ftptype, getting, query, path);
     return(ftptype);
 
}

