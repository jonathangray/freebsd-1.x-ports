/********************************************************************
 * lindner
 * 3.13
 * 1993/08/23 20:56:34
 * /home/mudhoney/GopherSrc/CVS/gopher+/object/GDgopherdir.c,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: GDgopherdir.c
 * Implement gopher directory routines
 *********************************************************************
 * Revision History:
 * GDgopherdir.c,v
 * Revision 3.13  1993/08/23  20:56:34  lindner
 * Fix for empty directory in g+ client
 *
 * Revision 3.12  1993/08/19  20:51:33  lindner
 * Mitra comments
 *
 * Revision 3.11  1993/08/19  20:24:04  lindner
 * Mitra's Debug patch
 *
 * Revision 3.10  1993/07/29  20:02:16  lindner
 * Removed dead variables
 *
 * Revision 3.9  1993/07/27  05:30:22  lindner
 * Mondo Debug overhaul from Mitra
 *
 * Revision 3.8  1993/07/27  00:30:08  lindner
 * plus patch from Mitra
 *
 * Revision 3.7  1993/07/14  20:37:08  lindner
 * Negative numbering patches
 *
 * Revision 3.6  1993/06/22  06:07:17  lindner
 * Added Domain= hacks..
 *
 * Revision 3.5  1993/04/15  21:35:12  lindner
 * Debug code, better .Link processing, better GDfromNet()
 *
 * Revision 3.4  1993/03/26  19:50:44  lindner
 * Mitra fixes for better/clearer fromNet code
 *
 * Revision 3.3  1993/03/24  17:04:49  lindner
 * bad strcmp() can check unmalloced() mem, fixed
 *
 * Revision 3.2  1993/03/18  22:13:29  lindner
 * filtering, compression fixes
 *
 * Revision 3.1  1993/02/11  18:03:01  lindner
 * Initial revision
 *
 * Revision 2.1  1993/02/09  22:46:50  lindner
 * Many additions for gopher+
 *
 * Revision 1.4  1993/01/31  00:22:51  lindner
 * Changed GDaddGS to merge entries with the same path.
 * Added GDplusfromNet() to siphon data from network.
 * GDfromLink now knows about ~/ inside of Path=
 * Changed GDSearch to ignore leading character.
 *
 * Revision 1.3  1992/12/19  04:44:09  lindner
 * Added GDplustoNet()
 *
 * Revision 1.2  1992/12/16  20:37:04  lindner
 * Added function GDsearch(), does a linear search of a gopher directory
 *
 * Revision 1.1  1992/12/10  23:27:52  lindner
 * gopher 1.1 release
 *
 *
 *********************************************************************/


#include "GDgopherdir.h"
#include "Malloc.h"


#include "String.h"
#include <stdio.h>
#include "Debug.h"

/***********************************************************************
** Stuff for GopherDirObjs
**
***********************************************************************/


GopherDirObj*
GDnew(size)
  int size;
{
     GopherDirObj *temp;
     
     temp = (GopherDirObj*) malloc(sizeof(GopherDirObj));

     temp->Gophers = DAnew(size, GSnew, GSinit, GSdestroy, GScpy);

     temp->Title = STRnew();
     temp->currentitem = 1;

     GDinit(temp);
     return(temp);
}


void
GDdestroy(gd)
  GopherDirObj *gd;
{
     DAdestroy(gd->Gophers);
     
     STRdestroy(gd->Title);
     free(gd);
}


void
GDinit(gd)
  GopherDirObj *gd;
{
     DAinit(gd->Gophers);
     STRinit(gd->Title);
}



/** This proc adds a GopherObj to a gopherdir.
    It will attempt to merge two items if need be..
 **/
void
GDaddGSmerge(gd, gs)
  GopherDirObj *gd;
  GopherObj *gs;
{
	int num;
 
	num = GDSearch(gd, GSgetPath(gs));

	if (num == -1)
	     DApush(gd->Gophers, gs);
 	else {
	     GSmerge(GDgetEntry(gd, num),gs);
	}
}


/*
 * This one never tries to merge 
 */

void
GDaddGS(gd, gs)
  GopherDirObj *gd;
  GopherObj *gs;
{
 
	if (GSgetType(gs) != 'X')
	     DApush(gd->Gophers, gs);
}


/*
 * Really weird!!!  We need this for qsort,  don't know why we can't use
 * GScmp...
 */

#define sgn(a)	((a) == 0 ? 0 : (a) < 0 ? -1 : 1)

int
GSqsortcmp(gs1, gs2)
  GopherObj **gs1, **gs2;
{
     if (GSgetTitle(*gs1) == NULL)
	  return(1);
     if (GSgetTitle(*gs2) == NULL)
	  return(-1);
     
     /** No numbering set on either entry, or both numbered
         entries have the same number   **/

     if (GSgetNum(*gs1) == GSgetNum(*gs2))
	  return(strcmp(GSgetTitle(*gs1), GSgetTitle(*gs2)));

     /** If the signs are equal, compare the numbers conventionally **/
     
     /** N.B. If the signs ARE equal, they cannot be 0 (otherwise we would **/
     /** have had the above case, because only the sign of 0 is 0)  */
     if (sgn(GSgetNum(*gs1)) == sgn(GSgetNum(*gs2)))
          return(GSgetNum(*gs1) < GSgetNum(*gs2) ? -1 : 1);

     /** The signs must be different, so we can use a conventional test, **/
     /** remembering only to say positive numbers go before negative ones **/
     return(GSgetNum(*gs1) > GSgetNum(*gs2) ? -1 : 1);
}

/*
 * Sorts a gopher directory
 */

void
GDsort(gd)
  GopherDirObj *gd;
{

     DAsort(gd->Gophers, GSqsortcmp);
}


void
GDtoNet(gd, sockfd)
  GopherDirObj *gd;
  int sockfd;
{
     int i;
     Debug("GDplustoNet\n",0);
     for (i=0; i< GDgetNumitems(gd); i++) {
	  GStoNet(GDgetEntry(gd, i), sockfd);
     }	  

}


void
GDplustoNet(gd, sockfd, filter)
  GopherDirObj *gd;
  int          sockfd;
  char         **filter;
{
     int i;

     for (i=0; i< GDgetNumitems(gd); i++) {
	  GSplustoNet(GDgetEntry(gd, i), sockfd,filter);
     }	  
}

void
GDtoNetHTML(gd, sockfd)
  GopherDirObj *gd;
  int sockfd;
{
     int i;
     
     writestring(sockfd, "<MENU>\r\n");
     
     for (i=0; i< GDgetNumitems(gd); i++) {
	  writestring(sockfd, "<LI>");
	  GStoNetHTML(GDgetEntry(gd, i), sockfd);
     }	  
     writestring(sockfd, "</MENU>");
}

/*
 * Gopher+ counterpart to GDfromNet()
 * returns number of items found
 */


int
GDplusfromNet(gd, fd, eachitem)
  GopherDirObj *gd;
  int fd;
  int (*eachitem)();
{
     static GopherObj *TempGopher;
     static char ZesTmp[1024];
     int j, result;
     char inputline[256];

     Debug("GDplusfromNet:: start\r\n",0)
     if (TempGopher == NULL)
	  TempGopher = GSnew();

     /** State: _begin_ **/

     result = readrecvbuf(fd, inputline, 1);
     if (result <=0)
	  return(0);
     else if (*inputline == '.') {
	  /*** Read off the rest of the junk... ***/
	  readline(fd,inputline,sizeof(inputline));
	  return(0);
     }
     else if (*inputline != '+')
	  return(0);

     Debug("after readrecvbuf",0)
     /** State _FirstPlus_ **/

     result = readtotoken(fd, inputline, sizeof(inputline), ':');
     if (result <=0)
	  return(result);

     Debug("after readtotoken",0)
     if (strcmp(inputline, "INFO")!=0) {
	  return(0);
     }
     Debug("after INFO",0)
     /** Read the space **/
     readrecvbuf(fd, inputline, 1);


     /*** State _FirstINFO_ ***/

     for (j=0; ; j++) {

     	  Debug("for start",0);
	  ZesTmp[0] = '\0';
	  
	  GSinit(TempGopher);
	  result = GSplusfromNet(TempGopher, fd);
	  
	  switch (result) {
	  case MORECOMING:
	       GDaddGS(gd, TempGopher);
	       if (eachitem != NULL) 
		    eachitem();
	       break;

	  case FOUNDEOF:
	       GDaddGS(gd, TempGopher);
	       return(j+1);

	  case HARDERROR:  /** Give up reading - bad read or protocol error **/
	       return(j);

	  case SOFTERROR:  /** This one was bad, but we can try for next **/
	       j= j-1;
	       if (j<0) j=0;
	       break;
	  }
	       
     } /* for */

     /** Never get here **/
} 

/*
 * Fill up a GopherDirObj with GopherObjs, given a gopher directory coming
 * from sockfd.
 *
 * For each GopherObj retrieved, eachitem() is executed.
 *
 */

void
GDfromNet(gd, sockfd, eachitem)
  GopherDirObj *gd;
  int sockfd;
  int (*eachitem)();
{
     static GopherObj *TempGopher;
     static char ZesTmp[1024];
     int i;

     Debug("GDfromNet...",0);
     if (TempGopher == NULL)
	  TempGopher = GSnew();

     for (; ;) {

	  ZesTmp[0] = '\0';
	  
	  GSinit(TempGopher);
	  i = GSfromNet(TempGopher, sockfd);
	  
         /* In gopher+1.2b2 this routine clears up if GSfromNet returns 
            a failure, better to clear up in GSfromNet so that the 
            system returns in a known state - note that other callers of 
            GSfromNet didn't clean up and crashed! */
	  
	  switch (i) {

	  case 0:
	       GDaddGS(gd, TempGopher);
	       if (eachitem != NULL) eachitem();
	       break;

	  case 1:  /* . on a line by itself, nothing more */
	       return;

	  case SOFTERROR:  /** Unknown object type **/
	       break;

	  case HARDERROR:
	       return;
	  }
     }
} 


/*
 * Given an open file descriptor and an inited GopherDirobj,
 *   read in gopher links, and add them to a gopherdir
 */

void
GDfromLink(gd, fd, host, port, directory, peer)
  GopherDirObj *gd;
  int          fd;
  char         *host;       /** Current Host **/
  int          port;        /** Current port **/
  char         *directory;  /** Current directory **/
  char         *peer;       /** connected client **/
{
     GopherObj *gs;
     int result;

     gs = GSnew();


     while (1) {
	  result = GSfromLink(gs, fd, host, port,directory, peer);

	  if (result == HARDERROR)
	       break;
	  if (result == SOFTERROR)
	       continue;

	  if (*GSgetPath(gs) == '.')
	       GDaddGSmerge(gd, gs);
	  else
	       GDaddGS(gd, gs);

	  if (result == FOUNDEOF)
	       break;

	  GSinit(gs);
     }
	  
     GSdestroy(gs);
}


void
GDtoLink(gd, fd)
  GopherDirObj *gd;
  int fd;
{
     int i;

     for (i=0; i< GDgetNumitems(gd); i++) {
	  GStoLink(GDgetEntry(gd, i), fd);
     }	  

}

/***  Search for a specific gopher item ***/
/* Allow text to end in .Z - gd will never have a .Z item in it due to fix 
   in gopherd/gopherd */
int
GDSearch(gd, text)
  GopherDirObj *gd;
  char         *text; 	/* Note first char is G0 type and is ignored*/
{
     int i;
     int WasCtrlZ = 0;
     GopherObj *gs;
     int len;

     Debug("GDSearch: %s;\n",text);

     if (gd == NULL)
	  return(-1);

     if (text == NULL)
	  return(-1);

     len = strlen(text);
     if (len >2) {
	  WasCtrlZ = (strcmp(text+strlen(text)-2,".Z")  == 0);
	  if (WasCtrlZ) 
	       text[strlen(text)-2] = '\0';
     }
	
     for (i=0; i< GDgetNumitems(gd); i++) {
	  gs = GDgetEntry(gd, i);

	  if (len >1 && strcmp(text+1, GSgetPath(gs)+1) == 0) {
	       if (WasCtrlZ) 
		    text[strlen(text)-2] = '.';
	       Debug("Matched\n",0);
	       return(i);
	  }
     }
     if (WasCtrlZ) 
	  text[strlen(text)-2] = '.';
     Debug("GDsearch: No Match\n",0);
     return(-1);
}

