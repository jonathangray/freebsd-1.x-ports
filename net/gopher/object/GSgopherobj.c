/********************************************************************
 * lindner
 * 3.16
 * 1993/08/19 20:24:11
 * /home/mudhoney/GopherSrc/CVS/gopher+/object/GSgopherobj.c,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: GSgopherobj.c
 * Implement gopher directory functions.
 *********************************************************************
 * Revision History:
 * GSgopherobj.c,v
 * Revision 3.16  1993/08/19  20:24:11  lindner
 * Mitra's Debug patch
 *
 * Revision 3.15  1993/07/29  20:02:55  lindner
 * Removed dead variables
 *
 * Revision 3.14  1993/07/27  20:17:25  lindner
 * Fix improper bracketed debug output
 *
 * Revision 3.13  1993/07/27  05:30:23  lindner
 * Mondo Debug overhaul from Mitra
 *
 * Revision 3.12  1993/07/27  00:30:09  lindner
 * plus patch from Mitra
 *
 * Revision 3.11  1993/07/23  04:50:17  lindner
 * Mods to allow abstract/admin setting in .names files
 *
 * Revision 3.10  1993/07/21  03:31:09  lindner
 * Askdata can be stored locally, plus GSfromLink doesn't core dump with mangled items
 *
 * Revision 3.9  1993/07/14  20:37:11  lindner
 * Negative numbering patches
 *
 * Revision 3.8  1993/07/07  19:30:12  lindner
 * Split off socket based fcns to Sockets.c
 *
 * Revision 3.7  1993/07/06  20:22:40  lindner
 * Added listener and accept fcns
 *
 * Revision 3.6  1993/06/22  06:07:14  lindner
 * Added Domain= hacks..
 *
 * Revision 3.5  1993/04/15  17:44:52  lindner
 * Fixed link processing, mods from Mitra
 *
 * Revision 3.4  1993/03/26  19:50:46  lindner
 * Mitra fixes for better/clearer fromNet code
 *
 * Revision 3.3  1993/03/24  17:05:33  lindner
 * Additions for Localfile for each GopherObj
 *
 * Revision 3.2  1993/03/18  22:15:50  lindner
 * filtering, memory leaks fixed, GSmerge problems
 *
 * Revision 3.1.1.1  1993/02/11  18:03:06  lindner
 * Gopher+1.2beta release
 *
 * Revision 2.4  1993/02/09  22:47:16  lindner
 * New GSisText() More fixes for gopher+
 *
 * Revision 2.3  1993/01/31  00:22:51  lindner
 * Added GSplusfromNet(), updated gplus for VIewobjs and BLobjs
 * Better GSplustoNet(), New fcn GSmerge()
 * GSfromLink parses differently
 * GStoLink adds a couple more entries.
 *
 * Revision 2.2  1993/01/14  21:23:38  lindner
 * Merged in 1.1 mods to mainline.
 *
 * Revision 2.1  1993/01/14  21:19:43  lindner
 * Addition of Gopher+ structures and routines (first cut)
 *
 * Revision 1.1.1.3  1993/01/11  19:54:06  lindner
 * Fixed syntax in UCX #defines. (DuH!)
 *
 * Revision 1.1.1.2  1993/01/08  19:00:00  lindner
 * Mods for UCX
 *
 * Revision 1.1.1.1  1992/12/31  05:01:31  lindner
 * Chnages for VMS.
 *
 * Revision 1.1  1992/12/10  23:27:52  lindner
 * gopher 1.1 release
 *
 *
 *********************************************************************/

#include "GSgopherobj.h"

#if defined(mips) && defined(ultrix)   /*** Gross hack, yuck! ***/
#define _SIZE_T
#endif

#include "String.h"
#include "STRstring.h"
#include <stdio.h>
#include "compatible.h"
#include <errno.h>
#include "Malloc.h"
#include "Sockets.h"
#include "Debug.h"

/*
 * Make a new gopherobj, set the fields accordingly...
 */

GopherObj *
GSnewSet(objtype, Title, Selstr, Host, Port)
  char objtype;
  char *Title;
  char *Selstr;
  char *Host;
  int Port;
{
     GopherObj *temp;

     temp = (GopherObj *) malloc(sizeof(GopherObj));
     temp->Selstr   = STRnewSet(Selstr);
     temp->Title    = STRnewSet(Title);
     temp->Host     = STRnewSet(Host);
     temp->iPort    = Port;
     GSsetNum(temp, -1);
     GSsetWeight(temp, 0);

     return(temp);
}


/*
 * Make a new gopherobj...  Should reuse destroyed GopherObjs...
 */

GopherObj *
GSnew()
{
     GopherObj *temp;

     temp = (GopherObj *) malloc(sizeof(GopherObj));
     temp->Selstr    = STRnew();
     temp->Title     = STRnew();
     temp->Host      = STRnew();
     temp->Localfile = STRnew();
     temp->gplus     = NULL;
     GSinit(temp);
     return(temp);
}

/*
 * Initialize the gopherplus components of the object
 * (Only called for a gplus item
 */

void
GSplusnew(gs)
  GopherObj *gs;
{
     if (gs->gplus == NULL) {
	  gs->gplus = (GplusObj *) malloc(sizeof(GplusObj));
     }
     
     gs->gplus->Admin    = STRnew();
     gs->gplus->ModDate  = STRnew();

     gs->gplus->Views    = VIAnew(10);
     gs->gplus->OtherBlocks = BLAnew(5);
     gs->gplus->Askdata  = NULL;
}


/*** Destroy gopher object ***/

void
GSdestroy(gs)
  GopherObj *gs;
{
     STRdestroy(gs->Selstr);
     STRdestroy(gs->Title);
     STRdestroy(gs->Host);
     if (GSgetLocalFile(gs) != NULL)
	  unlink(GSgetLocalFile(gs));
     STRdestroy(gs->Localfile);

     GSplusdestroy(gs);
     free(gs);
}
     

/*** Destroy Gopher+ attributes ***/

void
GSplusdestroy(gs)
  GopherObj *gs;
{
     if (gs->gplus != NULL) {
	  STRdestroy(gs->gplus->Admin);
	  STRdestroy(gs->gplus->ModDate);
	  VIAdestroy(gs->gplus->Views);
	  BLAdestroy(gs->gplus->OtherBlocks);

	  (void*)GSsetAskdata(gs, NULL);

	  free(gs->gplus);
     }
}


/*
 * Clear out all the crud 
 */

void
GSinit(gs)
  GopherObj *gs;
{
     GSsetType(gs, '\0');
     
     STRinit(gs->Title);
     STRinit(gs->Selstr);
     STRinit(gs->Host);
     STRinit(gs->Localfile);

     gs->iPort = 0;
     GSsetNum(gs, 0);
     GSsetWeight(gs, 0);

     GSsetGplus(gs,FALSE);  /** Default is no gplus **/
     GSsetAsk(gs, FALSE);

     GSplusInit(gs);
}


/*
 * Clear out gopher+ crud if it exists
 */

void
GSplusInit(gs)
  GopherObj *gs;
{
     if (gs->gplus != NULL) {
	  STRinit(gs->gplus->Admin);
	  STRinit(gs->gplus->ModDate);
	  VIAinit(gs->gplus->Views);
	  STAinit(gs->gplus->OtherBlocks);

     }
}

void
GSsetstringAsk(gs,ask)
  GopherObj *gs;
  char *ask;
{
     GSsetBlock(gs,"ASK",ask,TRUE);
     GSsetAsk(gs,TRUE);
}


void
GSsetAbstract(gs, abstract)
  GopherObj *gs;
  char *abstract;
{
     GSsetBlock(gs,"ABSTRACT",abstract,TRUE);
}

void
GSsetBlock(gs, blockname, text, appendIfPoss)
  GopherObj *gs;
  char *blockname;
  char *text;
  boolean appendIfPoss;
{
     Blockobj *bl;

     Debug("GSsetBlock:%s;",blockname);
     Debug("%s\r\n", text);
     /* Change so if appendIfPos is set it appends rather than makes new*/
     bl = BLnew();

     BLsetName(bl, blockname);
     BLaddText(bl, text);
     
#ifdef DEBUGGING
     if (DEBUG)
	  BLtoNet(bl, fileno(stderr));
#endif
     if (gs->gplus == NULL)
	  GSplusnew(gs);
     
     BLApush(gs->gplus->OtherBlocks, bl);
     BLdestroy(bl);
}

/*
 * Set the Askdata, destroy if necessary..
 */

char **
GSsetAskdata(gs, askdata)
  GopherObj *gs;
  char **askdata;
{
     if (!GSgplusInited(gs))
	  return(NULL);

     /** Destroy data if necessary **/
     if (gs->gplus->Askdata != NULL) {
	  int i=0;
	  char **Askmoo = gs->gplus->Askdata;
	  
	  while (Askmoo[i] != NULL) {
	       free(Askmoo[i++]);
	  }
     }

     gs->gplus->Askdata = askdata;

     return(askdata);

}


/*
 * Set the administrator line as defined in Gopher+ protocol
 * Name <email>
 */

void
GSsetAdmin(gs, admin)
  GopherObj *gs;
  char *admin;
{
     if (gs->gplus == NULL)
	  GSplusnew(gs);

     STRset(gs->gplus->Admin, admin);
}     

void
GSsetModDate(gs, str)
  GopherObj *gs;
  char *str;
{
     if (gs->gplus == NULL) GSplusnew(gs);
     STRset(gs->gplus->ModDate, str);
}     



/** Add a view for a specific gopherobj **/

void
GSaddView(gs, view, language, estsize)
  GopherObj *gs;
  char *view;
  char *language;
  int estsize;
{
     char tmpstr[64];
     VIewobj *temp;

     if (estsize > 1024)
	  sprintf(tmpstr, "%dk", estsize/1024);
     else
      	  sprintf(tmpstr, ".%dk", (estsize*10)/1024);

     temp = VInew();

     VIsetType(temp, view);
     VIsetLang(temp, language);
     VIsetSize(temp, tmpstr);

     VIApush(gs->gplus->Views, temp);
     VIdestroy(temp);
}


void
GSaddBlock(gs, Blockname, file)
  GopherObj *gs;
  char *Blockname;
  char *file;
{
     Blockobj *bl;

     bl = BLnew();
     BLsetName(bl, Blockname);
     BLsetFile(bl, file);
     ;
     if (gs->gplus == NULL)
	  GSplusnew(gs);

     BLApush(gs->gplus->OtherBlocks, bl);
}



/*
 * Send a gopher reference into an fd
 */

void
GStoNet(gs, sockfd)
  GopherObj *gs;
  int sockfd;
{
     static char buf[1024];

     buf[0] = GSgetType(gs);

     if (buf[0] == '\0')	/* For example a .names with no Type */
	buf[0] = '3'; 

     sprintf(buf + 1, "%s\t%s\t%s\t%d",
	     GSgetTitle(gs),
	     GSgetPath(gs),
	     GSgetHost(gs),
	     GSgetPort(gs));

     if (GSisAsk(gs))
	  strcat(buf, "\t?\r\n");
     else if (GSisGplus(gs))
	  strcat(buf, "\t+\r\n");
     else
	  strcat(buf, "\r\n");
     
     writestring(sockfd, buf);
     
     Debug("GStoNet:%s", buf);

}


/*
 * Send a long gopher data descriptor
 *
 * filter is a character array of blocks to send
 */

void
GSplustoNet(gs, sockfd, filter)
  GopherObj *gs;
  int sockfd;
  char **filter;
{
     int     i;
     char    tmpstr[256];
     boolean sendviews, sendadmin, sendothers;

     if (filter == NULL)
	  sendviews = sendadmin = sendothers = TRUE;
     else {
	  sendviews = sendadmin = sendothers = FALSE;
	  for (i=0; filter[i] != NULL ;i++) {
	       if (strcasecmp(filter[i], "VIEWS")==0)
		    sendviews = TRUE;
	       else if (strcasecmp(filter[i], "ADMIN")==0)
		    sendadmin = TRUE;
	       else 
		    sendothers = TRUE;
	  }
     }    
     
     
     /** Send out the old style INFO stuff **/
     writestring(sockfd, "+INFO: ");
     GStoNet(gs,sockfd);

     if (GSgplusInited(gs)) {
	  /*** Should special case for local filename.... ***/

	  if (GSgetAdmin(gs) != NULL && GSgetModDate(gs) != NULL && sendadmin){
	       writestring(sockfd, "+ADMIN:\r\n Admin: ");
	       writestring(sockfd, GSgetAdmin(gs));
	       writestring(sockfd, "\r\n Mod-Date: ");
	       writestring(sockfd, GSgetModDate(gs));
	       writestring(sockfd, "\r\n");
	  }
	  if (GSgetNumViews(gs) > 0 && sendviews) {
	       writestring(sockfd, "+VIEWS:\r\n");
	       for (i=0 ; i< GSgetNumViews(gs); i++) {
		    VItoLine(GSgetView(gs, i), tmpstr);
		    writestring(sockfd, tmpstr);
		    writestring(sockfd, "\r\n");
	       }
	  }
	  if (sendothers)
	       for (i=0; i< GSgetNumBlocks(gs); i++)
		    BLtoNet(GSgetBlock(gs, i),sockfd);
     }
}


/*** GSplusfromnet() assumes that the leading +INFO text has been read from
     the file descriptor.

     It returns 1  if there's more data (and another INFO block)
                0  if there's EOF
		SOFTERROR caller can keep reading
		HARDERROR caller should stop reading
 ***/

int
GSplusfromNet(gs, fd)
  GopherObj *gs;
  int fd;
{
     int result,readok,i;
     boolean nextinfo = FALSE;
     char plusfield;
     Blockobj *bl;
     char inputline[512];
     
     if (gs->gplus == NULL)
	  GSplusnew(gs);

     bl = BLnew();

     /** get the gopher-data descriptor **/
     readok = GSfromNet(gs, fd);

     if (readok == HARDERROR)
	  return(HARDERROR);

     /** If readok is softerror we still need to look for blocks to throw
         away any data before next item **/

     /** Now start looking for blocks.  Process blocks if we know how.  **/
     /** State: _GotGREF_ **/

     if ((result = readrecvbuf(fd, &plusfield, 1))<0)
	  return(HARDERROR);

     while (!nextinfo) {
	  if (result == 0) { 	       /*** We're done ***/
	       BLdestroy(bl);

	       if (readok == SOFTERROR)
		    return(SOFTERROR);
	       else
		    return(FOUNDEOF);
	  }
	  switch (plusfield) {

	  case '.':
	       readline(fd, inputline, sizeof(inputline));
	       result = FOUNDEOF;
	       break;

	  case '+':
	       /*** State _NewBlock_ ***/

	       readtotoken(fd,inputline, sizeof(inputline), ':');

	       if (strcasecmp(inputline, "INFO")==0) {
		    /** Get rid of the space. **/
		    readrecvbuf(fd, &plusfield,1);
		    BLdestroy(bl);
		    if (readok == SOFTERROR)
			 return(SOFTERROR);
		    else
			 return(MORECOMING);
	       }

	       /** Specialized fromNets here **/

	       BLinit(bl);
	       if ((result=BLfromNet(bl, fd, inputline)) <0) {
		    /** Bad read **/
		    BLdestroy(bl);
		    return(HARDERROR);
	       }
	       else if (result == FOUNDEOF)   /** EOF, block read ok **/
		    nextinfo = TRUE;
	       else 
		    nextinfo = FALSE;


	       /*** See what the block is. Transform it if necessary ***/
	       if (strcasecmp(BLgetName(bl), "VIEWS")==0) {
		    VIAfromBL(gs->gplus->Views, bl);
	       } else if (strcasecmp(BLgetName(bl), "ADMIN")==0) {
		    for (i=0; i<BLgetNumLines(bl); i++) {
			 char *cp;
			 
			 cp = BLgetLine(bl, i);
			 
			 if (strncasecmp(cp, "Admin: ",7)==0)
			      GSsetAdmin(gs, cp+7);
			 if (strncasecmp(cp, "Mod-Date: ", 10)==0)
			      GSsetModDate(gs, cp+10);
		    }
	       } else
		    BLApush(gs->gplus->OtherBlocks, bl);

	       break;  /** '+' **/

	  default: /*** Hmmm plusfield wasn't a plus or '.' **/
	       return(HARDERROR);

	  } /** switch plusfield **/

     }   /** While **/

     BLdestroy(bl);

     return(FOUNDEOF);
}


void
GStoNetHTML(gs, sockfd)
  GopherObj *gs;
  int sockfd;
{
     static char buf[1024];
     static char pathbuf[1024];

     buf[0] = '\0';
     pathbuf[0] = '\0';

     /** Convert Path so that spaces are %20 **/
     Tohexstr(GSgetPath(gs), pathbuf);

     sprintf(buf, "<A HREF=http://%s:%d/%s>%s</A>",
	     GSgetHost(gs),
	     GSgetPort(gs),
	     pathbuf,
	     GSgetTitle(gs));

     writestring(sockfd, buf);

     Debug("HTML: %s\n", buf);
     
     if (GSgetWeight(gs) != 0) {
	  sprintf(buf, "Score: %d\r\n", GSgetWeight(gs));
	  writestring(sockfd, buf);
     }
     else
	  writestring(sockfd, "\r\n");

}


/*
 * Fill in a GopherObj, given an HREF= link from a WWW anchor..
 * So far only works with http
 */

void
GSfromHREF(gs, href)
  GopherObj *gs;
  char *href;
{
     char *cp;
     char *host;
     char path[1024];
     
     Debug("GSfromHREF %s\r\n",href)

     if (strncasecmp(href, "http://", 7)==0) {
	  host = href +7;
	  cp  = strchr(href+7, '/');
	  if (cp == NULL)
	       return;
	  
	  *cp = '\0';
	  strcpy(path, "GET ");
	  strcat(path, cp+1);
	  GSsetPath(gs, path);

	  GSsetType(gs, 'h');

	  GSsetPath(gs, path);
	  
	  cp = strchr(host, ':');
	  if (cp==NULL) 
	       GSsetPort(gs, 80);  /** default WWW port **/
	  else {
	       GSsetPort(gs, atoi(cp+1));
	       *cp = '\0';
	  }

	  GSsetHost(gs, host);
     }
}
	  


/* GSfromNet - no comments on the original, so this is my (Mitra's) guess
   GSfromNet reads a gopher style line, it is called from:
   GDfromNet - in which case the gopher line is the whole item or;
   GSplusfromNet - in which case it is the part after the +INFO.
   It should return after reading the whole line, however in gopher+1.2b2
   this is not always the case, especially if it sees a Type=3
   returns:
       1      blank line (I think?)
       0      ok
      -1 HARDERROR    readfield etc error - give up
      -2 SOFTERROR    unrecognized or unhandleable type - skip on to next one
*/




extern int readfield();
extern int readline();

int
GSfromNet(gs, sockfd)
  GopherObj *gs;
  int sockfd;
{
     char foo[1024], *cp;
     
     if (readfield(sockfd, foo, 1024)<= 0) {
	  /* EOF or error */
	  return(HARDERROR);
     }

     GSsetType(gs, foo[0]);

     /** Get the kind of file from the first character **/
     /** Filter out files that we can't deal with **/

     switch (GSgetType(gs)) {
       case A_FILE:
       case A_DIRECTORY:
       case A_MACHEX:
       case A_PCBIN:
       case A_CSO:
       case A_INDEX:
       case A_TELNET:
       case A_SOUND:
       case A_UNIXBIN:
       case A_GIF:
       case A_HTML:
       case A_TN3270:
       case A_MIME:
       case A_IMAGE:
	  break;
       case A_EOI:
	  if (foo[1] == '\r' && foo[2] == '\n')
	       return(1);
     default:
	  /** Can't handle this type **/
	  readline(sockfd, foo, 1024);/** Cleanup **/
	  return(SOFTERROR);
     }

     /** Suck off the User Displayable name **/
     GSsetTitle(gs, foo+1);
     
     /** Suck off the Pathname **/
     if (readfield(sockfd, foo, 1024) == 0)
	  return(HARDERROR);
     GSsetPath(gs, foo);

     /** Suck off the hostname **/
     if (readfield(sockfd, foo, 1024) == 0)
	  return(HARDERROR);
     GSsetHost(gs, foo);

     if (readline(sockfd, foo, 1024)==0)
	  return(HARDERROR); 

     GSsetPort(gs, 0);

     /** Get the port number **/
     if ((cp = strchr(foo, '\t')) != NULL) {
	  *cp = '\0';
	  switch (*(cp+1)) {
	  case '?':
	       GSsetAsk(gs, TRUE);
	  case '+':
	       GSsetGplus(gs, TRUE);
	       break;
	  }
     }

     GSsetPort(gs, atoi(foo));
     DebugGSplusPrint(gs,"GSfromNet end:");

     return(0);
}


/** Copy a GopherObj ***/

void
GScpy(dest, orig)
  GopherObj *dest, *orig;
{
     dest->sFileType = orig->sFileType;
     dest->iPort     = orig->iPort;
     dest->Itemnum   = orig->Itemnum;

     GSsetTitle(dest, GSgetTitle(orig));
     GSsetPath(dest, GSgetPath(orig));
     GSsetHost(dest, GSgetHost(orig));
     
     GSsetGplus(dest, GSisGplus(orig));
     GSsetAsk(dest, GSisAsk(orig));

     GSsetLocalFile(dest, GSgetLocalFile(orig));
     GSpluscpy(dest, orig);
     
}

void
GSpluscpy(dest, orig)
  GopherObj *dest, *orig;
{
     if (GSgplusInited(orig)) {
	  if (!GSgplusInited(dest))
	       GSplusnew(dest);
	  GSsetAdmin(dest, GSgetAdmin(orig));
	  GSsetModDate(dest, GSgetModDate(orig));
	  VIAcpy(dest->gplus->Views, orig->gplus->Views);
	  BLAcpy(dest->gplus->OtherBlocks, orig->gplus->OtherBlocks);
	  
     }
}

/** GSmerge combines 2 gopher objects overwriting fields defined in overlay **/
/* Called by GDaddGSmerge to merge gs into directory, that is only called 
   by function to do this from a link */
void
GSmerge(gs, overlay)
  GopherObj *gs, *overlay;
{
     char *tempstr;

     DebugGSplusPrint(gs,"GSmerge: Original");
     DebugGSplusPrint(overlay,"GSmerge: Overlay");

     if (GSgetHost(overlay) == NULL) {

	  if (GSgetType(overlay) != '\0') {
	       /* Just setting Type wont work, since they interelated with Path
	        * so set first char of path as well */
		GSsetType(gs, GSgetType(overlay));
		tempstr = GSgetPath(gs);
		tempstr[0] = GSgetType(overlay);
		GSsetPath(gs, tempstr);
	  }
	  if (GSgetTitle(overlay) != NULL)
	       GSsetTitle(gs, GSgetTitle(overlay));
	  /* Dont set path - that is the key to the merge, and in the overlay
	     most probably has the first char set to ' '  ????*/
	  if (GSgetHost(overlay) != NULL)
	       GSsetHost(gs, GSgetHost(overlay));
	  if (GSgetPort(overlay) != 0)
	       GSsetPort(gs, GSgetPort(overlay));
	  if (GSgetNum(overlay) != -1)
	       GSsetNum(gs, GSgetNum(overlay));
	  if (GSgetWeight(overlay) != 0)
	       GSsetWeight(gs, GSgetWeight(overlay));
	  if (GSgetAdmin(overlay) != NULL)
               GSsetAdmin(gs, GSgetAdmin(overlay));
          if (overlay->gplus != NULL)
          {
              if (!GSgplusInited(gs))
                    GSplusnew(gs);
               BLAcpy(gs->gplus->OtherBlocks,
                        overlay->gplus->OtherBlocks);
	 }
     }
     DebugGSplusPrint(gs,"GSmerge: Result");
}


/** Compare two GopherObjs ***/

int
GScmp(gs1, gs2)
  GopherObj *gs1, *gs2;
{
     if (GSgetTitle(gs1) == NULL)
	  return(1);
     if (GSgetTitle(gs2) == NULL)
	  return(-1);

     return(strcmp(GSgetTitle(gs1), GSgetTitle(gs2)));
}


/*********** The following functions implement the gopher/gopher+
  protocol, mostly
  GSconnect(), then GStransmit(), GSsendHeader() GSrecvHeader();
************/



/* GSconnect performs a connection to socket 'service' on host
 * 'host'.  Host can be a hostname or ip-address.  If 'host' is null, the
 * local host is assumed.   The parameter full_hostname will, on return,
 * contain the expanded hostname (if possible).  Note that full_hostname is a
 * pointer to a char *, and is allocated by connect_to_gopher()
 *
 * Errors:
 *
 * -1 get service failed
 * -2 get host failed
 * -3 socket call failed
 * -4 connect call failed
 */

int
GSconnect(gs)
  GopherObj *gs;
{
     int sockfd; 

     sockfd = SOCKconnect(GSgetHost(gs), GSgetPort(gs));
     
     return(sockfd);
}


/*
 * GStransmit sends the request from the client to the server
 *
 * All parameters are optional except for gs and sockfd.
 * 
 * the rest pertain to gopher+ transmission.
 */

void 
GStransmit(gs, sockfd, search, command, view)
  GopherObj *gs;
  int sockfd;
  char *search;
  char *command;
  char *view;
{
     char *cp;
     char **ask = GSgetAskdata(gs);
     int i;

     writestring(sockfd, GSgetPath(gs));
     if (search != NULL) {
	  writestring(sockfd, "\t");
	  writestring(sockfd, search);
     }

     /** Only send if gplus **/
     if  (!GSisGplus(gs)) {
	  writestring(sockfd, "\r\n");
	  return;
     }

     if (command != NULL) {
	  writestring(sockfd, "\t");
	  writestring(sockfd, command);
     }	  

     if (view != NULL) {
	  writestring(sockfd, view);
     }
     if (ask == NULL) 
	  writestring(sockfd, "\r\n");
     else {
	  writestring(sockfd, "\t1\r\n");

	  GSsendHeader(sockfd, -1);
	  
	  for (i=0; ;i++) {
	       cp = ask[i];

	       if (cp == NULL)
		    break;

	       writestring(sockfd, cp);
	       writestring(sockfd, "\r\n");
	  }
	  writestring(sockfd, ".\r\n");
     }
}



/*
 * GSsendHeader generates an appropriate header on sockfd
 *
 */
void
GSsendHeader(sockfd, size)
  int sockfd;
  int size;
{
     char sizestr[64];

     sprintf(sizestr, "+%d\r\n", size);
     writestring(sockfd, sizestr);
}


/** GSsendErrorHeader sends an error message header/message to the client **/
void
GSsendErrorHeader(gs,sockfd,errortype,errormsg)
  GopherObj *gs;
  int       sockfd;
  int       errortype;
  char      *errormsg;
{
     char tmpstr[512];

     sprintf(tmpstr, "-%d %s\r\n", errortype, errormsg);
     writestring(sockfd, tmpstr);
}



/*
 * GSrecvHeader will retrieve a gopher+ header, if it exists
 *
 * It returns the expected number of bytes that are headed our
 * way, if it can.
 * 
 * Otherwise it returns -1 to indicate to read until \r\n.\r\n
 * or -2 to indicate to read to EOF
 *
 * If it encounters an error, it returns 0 and sets errno to the
 * gopher error class.
 */

int
GSrecvHeader(gs, sockfd)
  GopherObj *gs;
  int sockfd;
{
     char headerline[128];

     Debug("GSrecvHeader\n",0);
     if (GSisGplus(gs)) {
	  readline(sockfd, headerline, sizeof(headerline));
	  ZapCRLF(headerline);
	  if (*headerline == '+') {
	       if (*(headerline+1) == '-')
		    return(- (atoi(headerline+2)));
	       else
		    return(atoi(headerline+1));
	  } 
	  else if (*headerline == '-') {
	       /*** Oh no! an error! ***/
	       errno = atoi(headerline+1);
	       return(0);
	  }
     }
     /*** Guess if we're running old style gopher ***/
     else {
	  switch (GSgetType(gs)) {
	  case A_SOUND:
	  case A_IMAGE:
	  case A_GIF:
	  case A_UNIXBIN:
	  case A_PCBIN:
	       return(-2);
	       break;
	  default:
	       return(-1);
	  }
     }
}


/*
 * This routine will load up the item information from a gopher item
 * if the item hasn't transferred it already...
 */

void
GSgetginfo(gs)
  GopherObj *gs;
{
     int sockfd, bytes;
     char inputline[256];

     if (!GSisGplus(gs))
	  return;

     if (GSgplusInited(gs))
	  return;


     GSplusnew(gs);

     /** Send out the request **/
     if ((sockfd = GSconnect(gs)) <0) {
	  /*check_sock(sockfd, GSgetHost(gs), GSgetPort(gs));*/
	  return;
     }
     
     GStransmit(gs, sockfd, NULL, "!", NULL, NULL);
     bytes = GSrecvHeader(gs,sockfd);

     /***  Read off the first info block ***/
     readtotoken(sockfd, inputline, sizeof(inputline), ' ');

     GSplusfromNet(gs, sockfd);
     
}


/*
 * GSfromLink takes an open file descriptor and starts reading from it.
 * It starts reading.
 *
 * It reads until it finds a line it recognizes, then
 *
 * It keeps going until it finds
 *   eof, a non-recognized line, as long as there is a valid Path= line
 *
 * returns -1 on an error, 0 for EOF, 1 for success
 */

#define	G_PATH	1
#define	G_TYPE  2
#define	G_NAME	4
#define	G_PORT	8
#define	G_HOST	16
#define	G_ALL (G_PATH | G_TYPE | G_NAME | G_PORT | G_HOST)

int
GSfromLink(gs, fd, host, port, directory, peer)
  GopherObj *gs;
  int       fd;
  char      *host;
  int       port;
  char      *directory;
  char      *peer;
{
     int doneflags = 0;
     char buf[1024];
     int bytesread;
     boolean BadDomain = FALSE;   /** For use with the Domain= line **/

     Debug("GSfromLink...\n",0);
     while ((bytesread = readline(fd, buf, sizeof(buf)))>0) {

	  Debug("%s",buf);

	  if (buf[0] == '#') {
	       if (doneflags & G_PATH)
		    break;   /* comment */
	       else
		    continue;
	  }

	  ZapCRLF(buf);


	  if (strncmp(buf, "Type=", 5)==0) {
	       GSsetType(gs, buf[5]);
	       if (buf[6] == '+')
		    GSsetGplus(gs, TRUE);
	       doneflags |= G_TYPE;
	  }

	  else if (strncmp(buf, "Name=", 5)==0) {
	       GSsetTitle(gs, buf+5);
	       doneflags |= G_NAME;
	  }

	  else if (strncmp(buf, "Path=", 5)==0) {
	       if (strncmp(buf+5, "~/",2) == 0 ||
		   strncmp(buf+5, "./",2) == 0) {
		    char tmpstr[256];
		    
		    *tmpstr = '.';
		    strcpy(tmpstr+1, directory);
		    if (directory[strlen(directory)-1] == '/')
			 strcat(tmpstr, buf+7);
		    else
			 strcat(tmpstr, buf+6);
		    GSsetPath(gs, tmpstr);
	       } else
		    GSsetPath(gs, buf+5);
	       doneflags |= G_PATH;
	  }
 	  
	  else if (strncmp(buf, "Host=", 5)==0) {
	       if (buf[5] == '+' && buf[6] == '\0')
		    GSsetHost(gs, host);
	       else
		    GSsetHost(gs, buf+5);

	       doneflags |= G_HOST;
	  }

	  else if (strncmp(buf, "Port=", 5)==0) {
	       if (buf[5] == '+' && buf[6] == '\0')
		    GSsetPort(gs, port);
	       else
		    GSsetPort(gs, atoi(buf+5));

	       doneflags |= G_PORT;
	  }

	  else if (strncmp(buf, "Numb=", 5)==0)
	       GSsetNum(gs, atoi(buf+5));

	  else if (strncmp(buf, "Abstract=", 9)==0)
	       GSsetAbstract(gs, buf+9);
	  else if (strncmp(buf, "Admin=", 6) == 0)
	       GSsetAdmin(gs, buf +6);

	  else if (strncmp(buf, "Domain=", 7) ==0 && peer != NULL) {
	       /** Check to see if the peer matches the domain **/
	       int peerlen   = strlen(peer);
	       int domainlen = strlen(buf+7);
	       
	       if (domainlen > peerlen) {
		    BadDomain = TRUE;
	       } else if (strncasecmp(buf+7, peer + peerlen - domainlen, domainlen)== 0) {
		    /** Domains match, do it! **/
		    BadDomain = FALSE;
	       } else
		    BadDomain = TRUE;
	  }

	  else
	       break;  /*** Unknown name/item ***/
     }

     Debug("Done with this link item\n",0);

     if (BadDomain)
	  return(SOFTERROR);

     if (bytesread == 0) {
	  if (doneflags & G_PATH)
	       return(FOUNDEOF);  /** Found the eof, plus there's a g item **/
	  else 
	       return(HARDERROR); /** Mangled item, plus eof, stop the game **/
     }

     if (doneflags & G_PATH)
	  return(MORECOMING);  /** Found item, more coming. **/
     else
	  return(SOFTERROR);   /** Mangled item, more coming.. **/
}



void
GStoLink(gs, fd)
  GopherObj *gs;
  int fd;
{
     char gtype[2];
     char portnum[16];
     
     gtype[0] = GSgetType(gs);
     gtype[1] = '\0';

     writestring(fd, "#");
     writestring(fd, "\nType=");
     writestring(fd, gtype);
     if (GSisGplus(gs))
	  writestring(fd, "+");
     writestring(fd, "\nName=");
     writestring(fd, GSgetTitle(gs));
     writestring(fd, "\nPath=");
     writestring(fd, GSgetPath(gs));
     writestring(fd, "\nHost=");
     writestring(fd, GSgetHost(gs));
     writestring(fd, "\nPort=");
     sprintf(portnum, "%d", GSgetPort(gs));
     writestring(fd, portnum);
     writestring(fd, "\n");
     if (GSisGplus(gs) && GSgplusInited(gs)) {
	  writestring(fd, "Admin=");
	  writestring(fd, GSgetAdmin(gs));
	  writestring(fd, "\nModDate=");
	  writestring(fd, GSgetModDate(gs));
	  writestring(fd, "\n");
     }
}


boolean
GSisText(gs, view)
  GopherObj *gs;
  char *view;
{
     char viewstowage[64], *cp;

     strcpy(viewstowage, view);
     if ((cp=strchr(viewstowage, ' '))!=NULL) {
	  *cp = '\0';
	  view = viewstowage;
     }

     if (view == NULL) {
	  switch (GSgetType(gs)) {
	  case A_FILE:
	  case A_MACHEX:
	       return(TRUE);

	  default:
	       return(FALSE);
	  }
     }
     else {
	  if (strncasecmp(view, "Text",4) == 0 ||
	      strncasecmp(view, "application/postscript", 21)==0)
	       return(TRUE);
	  else
	       return(FALSE);
     }
}

#ifdef DEBUGGING
void
GSplusPrint(gs,head)
  GopherObj *gs;
  char *head;
{
     int i;
     int oldDebug = DEBUG;
     DEBUG=FALSE;
     fprintf(stderr,"%s: Type=%c,Title=%s,Path=%s,Host=%s,Port=%d,Num=%d,Weight=%d,Plus=%d,Ask=%d\r\n",
	     head,
	     GSgetType(gs),
	     GSgetTitle(gs),
	     GSgetPath(gs),
	     GSgetHost(gs),
	     GSgetPort(gs),
	     GSgetNum(gs),
	     GSgetWeight(gs),
	     GSisGplus(gs),
	     GSisAsk(gs)
	     );
     
     if (GSgplusInited(gs))
	  for (i=0; i< GSgetNumBlocks(gs); i++) {
	       BLtoNet(GSgetBlock(gs, i), fileno(stderr));
	  }
     fprintf(stderr,"===============\r\n");
     DEBUG = oldDebug;
}
#endif



