/********************************************************************
 * lindner
 * 3.11
 * 1993/08/23 18:34:28
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopherd/index.c,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: index.c
 * Routines to deal with various types of indexes.
 *********************************************************************
 * Revision History:
 * index.c,v
 * Revision 3.11  1993/08/23  18:34:28  lindner
 * Add fixfile() call
 *
 * Revision 3.10  1993/08/06  14:30:44  lindner
 * Fixes for better security logging
 *
 * Revision 3.9  1993/08/05  20:44:02  lindner
 * Use Gpopen instead of popen, remove extra filtering code
 *
 * Revision 3.8  1993/08/04  22:12:43  lindner
 * Mods to use Gpopen
 *
 * Revision 3.7  1993/07/27  05:27:50  lindner
 * Mondo Debug overhaul from Mitra
 *
 * Revision 3.6  1993/07/26  15:31:13  lindner
 * mods for application/gopher-menu
 *
 * Revision 3.5  1993/07/20  23:56:44  lindner
 * LOGGopher mods
 *
 * Revision 3.4  1993/04/15  21:41:38  lindner
 * Added $ to list of naughty characters (just in case)
 *
 * Revision 3.3  1993/04/09  15:54:29  lindner
 * Fixes for indexes with gopher+
 *
 * Revision 3.2  1993/03/26  19:47:06  lindner
 * First crack at gopherplussing Indexing
 *
 * Revision 3.1.1.1  1993/02/11  18:02:52  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.3  1993/01/30  23:57:44  lindner
 * Removed html code, moved parsing of the inputline to gopherd.c
 *
 * Revision 1.2  1992/12/14  21:36:05  lindner
 * Fixed problem in ShellIndexQuery, cp wasn't being incremented.
 * Also added special character elimination from GrepIndexQuery
 *
 * Revision 1.1  1992/12/10  23:13:27  lindner
 * gopher 1.1 release
 *
 *
 *********************************************************************/

#include "gopherd.h"
#include "command.h"
#include "Debug.h"
#include <stdio.h>

#define WAISTYPE 1
#define NEXTTYPE 2
#define SHLLTYPE 3
#define GREPTYPE 4


void 
Do_IndexTrans(sockfd, IndexDirectory, cmd)
  int sockfd;
  char *IndexDirectory;
  CMDobj *cmd;
{
     char *cp = NULL;
     char *dbName = NULL;
     char INDEXHost[256], INDEXPath[256];  /** Hard coded limits, ugh! **/
     int  INDEXPort=0;
     int  Index_type=0;
     char *SearchString = CMDgetSearch(cmd);
     char *view = NULL;
     boolean isgplus;

     Debug("Index Dir is %s\n", IndexDirectory);

     Index_type = Find_index_type(IndexDirectory);

     if (CMDgetCommand(cmd) != NULL) {
	  if (*CMDgetCommand(cmd) == '+')
	       view = CMDgetCommand(cmd) + 1;
	  else if (*CMDgetCommand(cmd) == '$')
	       view = "application/gopher+-menu";

	  if (*view == '\0')
	       view = "application/gopher-menu";
     }

     isgplus = CMDisGplus(cmd);

     Debug("Index type is %d\n", Index_type);

     if (Index_type < 0) {
	  /**** Error condition, unknown index type... ****/
	  Abortoutput(sockfd, "Unknown index type");
	  return;
     }
	  
     if (Index_type == WAISTYPE) {
	  /*** The selector string has both the directory and the dbname... ***/
	  cp = strrchr(IndexDirectory, '/');
	  
	  if (cp == NULL)
	       dbName = "index";
	  else {
	       dbName= cp+1;
	       *cp='\0';
	  }
     }

     if (Read_hostdata(IndexDirectory, INDEXHost, &INDEXPort, INDEXPath, dbName) <0) {
	  LOGGopher(sockfd, "Malformed hostdata file");
	  writestring(sockfd, "0Error on server, malformed hostdata\t\t\t1\r\n.\r\n");
	  return;
     }

     /* Doctor up the indexdirectory path if we're not running chroot()
      * we use fixfile to keep things secure....
      */

     if (!dochroot)
	  IndexDirectory = fixfile(IndexDirectory);

     /** And call the appropriate query function **/

     switch (Index_type) {

     case NEXTTYPE:
	  
	  NeXTIndexQuery(sockfd, SearchString, IndexDirectory, NULL, 
			 INDEXHost, INDEXPort, INDEXPath, isgplus, view);
	  break;

     case WAISTYPE:
	  WaisIndexQuery(sockfd, IndexDirectory, SearchString, dbName, 
			 INDEXHost, INDEXPort, INDEXPath, isgplus, view);
	  break;

     case GREPTYPE:
	  GrepIndexQuery(sockfd, IndexDirectory, SearchString, 
			 INDEXHost, INDEXPort, INDEXPath);
	  break;
	  
     case SHLLTYPE:
	  ShellIndexQuery(sockfd, IndexDirectory, SearchString);
	  break;
     }
     
     /** Log it here so we get the query in the logfile **/

     if (dbName)
	  LOGGopher(sockfd,"search %s/%s for %s", IndexDirectory, 
		    dbName, SearchString);
     else
	  
	  LOGGopher(sockfd, "search %s for %s", IndexDirectory, SearchString);
}



/*
 * Try to figure out what each type of object is
 *
 * index types are 
 *   Error       == -1
 *   WAIS        == 1
 *   NeXT        == 2
 *   ShellScript == 3
 *   Grep        == 4
 */

int
Find_index_type(gopherpath)
  char *gopherpath;
{
     char Teststr[512];
     FILE *Testfile;

     strcpy(Teststr, gopherpath);
     strcat(Teststr, "/.index/index.ixif");

     Testfile = rfopen(Teststr, "r");
     if (Testfile != NULL) {
	  /*** Next Index ***/
	  fclose(Testfile);
	  return(NEXTTYPE);
     }


     strcpy(Teststr, gopherpath);
     strcat(Teststr, ".inv");

     Testfile = rfopen(Teststr, "r");
     if (Testfile != NULL) {
	  /*** WAIS Index ***/
	  fclose(Testfile);
	  return(WAISTYPE);
     }


     strcpy(Teststr, gopherpath);

     if (isadir(Teststr) == 1) {
	  return(GREPTYPE);
     }
     
     Testfile = rfopen(Teststr, "r");
     if (Testfile != NULL) {
	  /** Shell script? **/
	  if (getc(Testfile) == '#')
	       if (getc(Testfile) == '!') {
		    fclose(Testfile);
		    return(SHLLTYPE);
	       }
     }

     return(-1);
}
     

/*
 * Read in the data from a hostdata file...
 * 
 * Try "<dbname>.hostdata" first, fall back to "hostdata" otherwise
 */

int
Read_hostdata(IndexDirectory, INDEXHost, INDEXPort, INDEXPath, dbName)
  char *IndexDirectory;
  char *INDEXHost, *INDEXPath;
  int  *INDEXPort;
  char *dbName;
{
     FILE *Hostfile;
     char hostdataName[256];

     /** Read in the proper hostdata file.... **/

     rchdir(IndexDirectory);  /** Change into the index directory **/

     sprintf(hostdataName, "%s.hostdata", dbName);  /* try idx.hostdata */
     if ((Hostfile = ufopen(hostdataName, "r")) == NULL)
	  Hostfile = ufopen("hostdata", "r");

     if (Hostfile == NULL) {
	  /*** Use the current host/port as the default ***/
	  fclose(Hostfile);
	  strcpy(INDEXHost, Zehostname);
	  *INDEXPort = GopherPort;
	  strcpy(INDEXPath, Data_Dir);
     } 
     else {
	  char tempbuf[255];

	  if (fgets(INDEXHost, 64, Hostfile) == NULL)
	       return(-1);
	  
	  ZapCRLF(INDEXHost);
	  
	  if (fgets(tempbuf, 255, Hostfile) == NULL)
	       return(-1);
	  
	  if ((*INDEXPort=atoi(tempbuf))==0)
	       return(-1);
	  
	  if (fgets(INDEXPath, 256, Hostfile) == NULL)
	       return(-1);
	  
	  ZapCRLF(INDEXPath);
	  fclose(Hostfile);
     }
     
     return(0);
}


/*
 * This is a searching function that runs grep across files
 * in a single directory...
 */

void
GrepIndexQuery(sockfd, Indexdir, Searchstr, INDEXHost, INDEXPort, INDEXPath)
  int sockfd;
  char *Indexdir;
  char *Searchstr;
  char *INDEXHost;
  int INDEXPort;
  char *INDEXPath;
{
     FILE *moocow;
     char command[512];
     char inputline[512];
     char *cp;
     GopherObj *gs;
     GopherDirObj *gd;

     gs = GSnew();
     gd = GDnew(32);

     cp = Searchstr;
     while (*cp != '\0') {
	  if (*cp == ';' ||*cp == '"' || *cp == '`' || *cp == '$')
	       *cp = '.';
	  cp++;
     }

     sprintf(command, "egrep \"%s\" \"%s\"/*", Searchstr, Indexdir);
     Debug("Grep command is %s\n", command);

     moocow = Gpopen(sockfd, command, "r");
     
     if (moocow == NULL) {
	  writestring(sockfd, ".\r\n");
	  LOGGopher(sockfd, "Couldn't open grep command");
	  return;
     }

     while (fgets(inputline, 512, moocow)) {
	  ZapCRLF(inputline);
	  GSsetType(gs, '0');

	  cp = strstr(inputline, INDEXPath) + strlen(INDEXPath);
	  GSsetTitle(gs, cp);

	  cp = strchr(inputline, ':');
	  *cp='\0';
	  cp =strstr(inputline, INDEXPath) + strlen(INDEXPath);
	  GSsetPath(gs, cp);
	  GSsetHost(gs, INDEXHost);
	  GSsetPort(gs, INDEXPort);

	  GDaddGS(gd, gs);
     }
     if (UsingHTML)
	  GDtoNetHTML(gd, sockfd);
     else {
	  GDtoNet(gd, sockfd);
	  writestring(sockfd, ".\r\n");
     }

     pclose(moocow);
}



/*
 * This starts up a shell script that's defined to be an index gateway
 * 
 * The shell script should write out standard gopher directory protocol.
 */

void
ShellIndexQuery(sockfd, Script, Searchstring)
  int sockfd;
  char *Script;
  char *Searchstring;
{
     GopherDirObj *gd;
     char Command[512];
     FILE  *Searchprocess;
     char *cp;

     gd = GDnew(32);

     sprintf(Command, "\"%s\" \"%s\"", Script, Searchstring);

     Searchprocess = Gpopen(sockfd, Command, "r");

     if (Searchprocess == NULL) {
	  writestring(sockfd, ".\r\n");
	  return;
     }

     GDfromNet(gd, fileno(Searchprocess), NULL);
     if (UsingHTML)
	  GDtoNetHTML(gd, sockfd);
     else {
	  GDtoNet(gd, sockfd);
	  writestring(sockfd, ".\r\n");
     }

     pclose(Searchprocess);
     GDdestroy(gd);
}

	  
#ifndef WAISSEARCH

void
WaisIndexQuery(sockfd, index_directory, SearchWords, new_db_name, INDEXHost, INDEXPort, INDEXPath)
  int sockfd;
  char *index_directory;
  char *SearchWords;
  char *new_db_name;
  char *INDEXHost;
  int  INDEXPort;
  char *INDEXPath;
{
     Abortoutput(sockfd, "Sorry, this isn't a WAIS index...");
     return;
}
#endif 

#ifndef NEXTSEARCH
void
NeXTIndexQuery(sockfd, SearchWords, ZIndexDirectory, DatabaseNm, INDEXHost, INDEXPort)
  int sockfd;
  char *SearchWords;
  char *ZIndexDirectory;
  char *DatabaseNm;  /*** Not used by the next indexer... ***/
  char *INDEXHost;
  int INDEXPort;
{
     Abortoutput(sockfd, "This isn't a NeXT... is it?");
     return;
}
#endif
