/********************************************************************
 * lindner
 * 3.5
 * 1993/07/27 05:27:35
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopherd/NeXTindex.c,v
 * $Status: $
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: NeXTindex.c
 * index interface to the NeXT text indexing routines. 
 *********************************************************************
 * Revision History:
 * NeXTindex.c,v
 * Revision 3.5  1993/07/27  05:27:35  lindner
 * Mondo Debug overhaul from Mitra
 *
 * Revision 3.4  1993/07/26  15:31:09  lindner
 * mods for application/gopher-menu
 *
 * Revision 3.3  1993/04/09  16:24:49  lindner
 * Fixes for Gopher+ operation
 *
 * Revision 3.2  1993/03/26  19:46:49  lindner
 * First crack at gopherplussing Indexing
 *
 * Revision 3.1.1.1  1993/02/11  18:02:50  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.1  1992/12/10  23:13:27  lindner
 * gopher 1.1 release
 *
 *
 *********************************************************************/

#include <sys/stat.h>
#include "text/wftable.h"
#include "text/ix.h"

#include "gopherd.h"
#include "Debug.h"

int
myInterruptRoutine()
{
     /* for now, always return 0 so the search is not interrupted */
     return(0);
}

void
NeXTIndexQuery(sockfd, SearchWords, ZIndexDirectory, DatabaseNm, INDEXHost, INDEXPort, INDEXPath, isgplus, view)
  int sockfd;
  char *SearchWords;
  char *ZIndexDirectory;
  char *DatabaseNm;  /*** Not used by the next indexer... ***/
  char *INDEXHost;
  int INDEXPort;
  char *INDEXPath;
  boolean isgplus;
  char *view;
{
     unsigned long i;
     char          *cp;
     int           j;
     Index         *workingIndex;
     RefList       theRefList;
     RefList       *ptrtheRefList;
     Reference     *MyReference;
     FileCell      *f;
     char          tempstr[40];
     char          outputline[1024];
     GopherObj     *gs;
     GopherDirObj  *gd;
     boolean       plusdirs = FALSE;


     if (view != NULL) {
	  if (strcasecmp(view, "application/gopher+-menu")==0)
	       plusdirs = TRUE;
     }


     gs = GSnew();
     gd = GDnew(32);

     Debug("Nextindexer called: Search %s,", SearchWords);
     Debug(" Indexdir %s\r\n", ZIndexDirectory);

     /*** Try to open the index a couple of times ***/
     if (!dochroot)
	  ZIndexDirectory = fixfile(ZIndexDirectory);

     for (j=0; j< 4; j++) {
	  
	  workingIndex = ixOpen( ZIndexDirectory, "r" );
	  if (workingIndex != NULL)
	       break;
	  else
	       usleep (50);
     }

     if ( workingIndex != 0 ) {
	  theRefList = ixIndexQuery(workingIndex, SearchWords, ixSearchByFullWord,
				    ixMatchContent, ixLiteralString,
				    (myInterruptRoutine));

	  for( i=0; i < theRefList.n; i++ ){
	       MyReference = &(theRefList.r[i]);
	       f = (*MyReference).f;
	       
	       /*** The Selector String ***/
	       /*** So far we only index text files, so put a 0 in front ***/

	       if (strstr((*f).file, ".cache") != NULL) {
		    continue;
	       }
	       
	       GSsetType(gs, '0');
	       /*** Process the description field, remove any crud, replace
                    with spaces. ***/
               {
			char *moo = f->desc;
			while (*moo != '\0') {
				if (!isprint(*moo))
					*moo = ' ';
				moo++;
			}
		}
		
	       GSsetTitle(gs, (f->desc)+1);
	       GSsetHost(gs, INDEXHost);
	       GSsetPort(gs, INDEXPort);

	       cp = strstr(f->file, INDEXPath);
	       if (cp == NULL)
		    sprintf(outputline, "0/%s", f->file);
	       else
		    sprintf(outputline, "0/%s", cp);

	       if (MacIndex)
			GSsetPath(gs, f->file);
	       else
	       		GSsetPath(gs, outputline);
	       GSsetWeight(gs, (int)(MyReference->weight * 1000.0));
	       GDaddGS(gd, gs);
	  }

	  if (isgplus) {
	       GSsendHeader(sockfd, -1);
	  }

	  if (plusdirs)
	       GDplustoNet(gd, sockfd, NULL);
	  else
	       GDtoNet(gd, sockfd);

	  writestring(sockfd, ".\r\n");
	  
     }
     else {
	  fprintf(stderr,"can't open working index\n" );
     }

     GSdestroy(gs);
     GDdestroy(gd);
     
     /* all done.... close the index file */
}

