/********************************************************************
 * lindner
 * 3.9
 * 1993/08/23 18:33:35
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopherd/Waisindex.c,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: Waisindex.c
 * Routines to translate wais indexes on disk to gopher
 *********************************************************************
 * Revision History:
 * Waisindex.c,v
 * Revision 3.9  1993/08/23  18:33:35  lindner
 * Fix bug with DGs patch
 *
 * Revision 3.8  1993/08/23  02:34:58  lindner
 * Limit # of hits
 *
 * Revision 3.7  1993/07/27  05:27:37  lindner
 * Mondo Debug overhaul from Mitra
 *
 * Revision 3.6  1993/07/26  15:31:10  lindner
 * mods for application/gopher-menu
 *
 * Revision 3.5  1993/07/20  23:55:38  lindner
 * LOGGopher mods
 *
 * Revision 3.4  1993/06/11  16:49:10  lindner
 * Fix for freeWAIS and Gopher+ requests
 *
 * Revision 3.3  1993/04/09  16:24:03  lindner
 * Hacks to support WAIS GIF, TIFF, DVI types
 *
 * Revision 3.2  1993/03/26  19:46:52  lindner
 * First crack at gopherplussing Indexing
 *
 * Revision 3.1.1.1  1993/02/11  18:02:50  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.5  1993/01/30  23:55:36  lindner
 * Better error messages, changed a uchdir to a rchdir
 *
 * Revision 1.4  1993/01/05  02:41:28  lindner
 * .cap files are now ignored by the indexer
 *
 * Revision 1.3  1993/01/01  00:12:41  lindner
 * Fixed parameters to GDnew()
 *
 * Revision 1.2  1992/12/21  20:36:44  lindner
 * Added #include for cutil.h (from dgg)
 *
 * Revision 1.1  1992/12/10  23:13:27  lindner
 * gopher 1.1 release
 *
 *
 *********************************************************************/

#if defined(WAISSEARCH)

/* WIDE AREA INFORMATION SERVER SOFTWARE
   No guarantees or restrictions.  See the readme file for the full standard
   disclaimer.    
   Brewster@think.com

   Heavily hacked by Paul Lindner (lindner@boombox.micro.umn.edu)
   Do you even recognize this Brewster? :-)

*/

int ShowDate = 0;

#define _search_c

#include "gopherd.h"
#include "Debug.h"

#if defined(_AIX)
#define ANSI_LIKE
#endif

#include "../ir/irext.h"
#include "../ir/irsearch.h"
#include "../ir/docid.h"
#include "../ir/irtfiles.h"
#include "../ir/cutil.h"    /** fix for -DBIO wais needs.. **/
#include <math.h>


FILE *logfile = NULL; /* the logfile */
char *log_file_name = NULL;

static char *DefaultDB = "index";
static char *MonthStr[] = {
     "Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Sept", "Oct",
      "Nov", "Dec"
};

#if defined(void)
#undef void
#endif



int
Process_Veronica(besthit, gs)
  hit *besthit;
  GopherObj *gs;
{
     FILE *ZeFile;
     char veronicabuf[1024];
     char *data, *cp;

     /*** Open up the file and seek to the right position ***/

     ZeFile = ufopen(besthit->filename, "r");

     if (ZeFile == NULL)
	  return(-1);

     fseek(ZeFile, besthit->start_character, 0);

     bzero(veronicabuf, sizeof(veronicabuf));
     fread(veronicabuf, 1, besthit->end_character - besthit->start_character,
	   ZeFile);
     veronicabuf[besthit->end_character - besthit->start_character+1] = '\0';
     
     
     data = veronicabuf;
     GSsetType(gs, *data);
     
     ZapCRLF(data);
     
     cp = strchr(data, '\t');
     *cp = '\0';
     GSsetTitle(gs, data+1);
     
     data = cp+1;
     cp = strchr(data, '\t');
     *cp = '\0';
     GSsetPath(gs, data);
     
     data = cp + 1;
     cp = strchr(data, '\t');
     *cp = '\0';
     GSsetHost(gs, data);
     
     GSsetPort(gs, atoi(cp+1));

     fclose(ZeFile);
     return(0);
}

void
WaisIndexQuery(sockfd, index_directory, SearchWords, new_db_name, INDEXHost, INDEXPort, INDEXPath, isgplus, view)
  int sockfd;
  char *index_directory;
  char *SearchWords;
  char *new_db_name;
  char *INDEXHost;
  int  INDEXPort;
  char *INDEXPath;
  boolean isgplus;
  char *view;
{ 
     database*            db;
     long                 maxRawScore, normalScore, i;
     char                 *cp, *Selstrout;
     char                 dateline[10];
     query_parameter_type parameters;
     boolean              search_result;
     char                 score[6];
     char                 ReturnLine[512];
     boolean              plusdirs=FALSE;
                                        
     char                 * sidename;    /* mtm 11-23-92 */
     FILE                 * SideFile = NULL;  /* mtm 11-23-92 */

     GopherDirObj         *gd;
     GopherObj            *gs;
     

     gs = GSnew();
     gd = GDnew(32);

     if (view != NULL) {
	  if (strcasecmp(view, "application/gopher+-menu")==0)
	       plusdirs = TRUE;
     }

     Debug("WaisIndexQuery: IndexPath: %s\n", INDEXPath);

#ifdef DEBUGGING
     if (DEBUG)  {
	  fprintf(stderr, "IndexPath: %s\n", INDEXPath);
	  logfile = stderr;   /** Log wais error messages to console **/
     }     else {
	  logfile = ufopen("/dev/null", "w+");
     }
#endif

     if (new_db_name == NULL) {
	  new_db_name = DefaultDB;
     }
     if (view != NULL) {
	  if (strcasecmp(view, "application/gopher+-menu")==0)
	       plusdirs = TRUE;
     }


     if (rchdir(index_directory)) {
	  char tmpstr[512];

	  sprintf(tmpstr, "Couldn't change to index directory '%s'",index_directory);
	  Abortoutput(sockfd, tmpstr);
	  return;
     }

     if (SearchWords != NULL && strlen(SearchWords) == 0) {
	  EveryWAISdocument(new_db_name);
	  return;
     }

     db = openDatabase(new_db_name, false, true);
     
     if (db == NULL) {
	  sprintf(ReturnLine, "Failed to open database %s in index dir %s", new_db_name, index_directory);
	  Abortoutput(sockfd, ReturnLine);
	  writestring(sockfd, ".\r\n"); /** be polite **/
	  return;
     }
     
#ifdef BIO            /* dgg */
{
     char *cp= read_delimiters( db);  /* use data-specific delim, available */

     if (cp != NULL) {
	  strcpy( gDelimiters, cp);
	  wordDelimiter= wordbreak_user;
     }
     else
	  wordDelimiter= wordbreak_notalnum;
}
#endif
 
/* check query string for ">max_retrieve" number */
{
     char *hitcp;
     int  ihit;

     hitcp = strrchr( SearchWords, '>');

     if ((hitcp != NULL) && (hitcp > SearchWords) && isdigit(*(++hitcp)) ) {
	  ihit= atoi( hitcp);
	  if (ihit>0) {
	       parameters.max_hit_retrieved = ihit;
	       *(--hitcp) = '\0';  /* drop rest of string */
	  }
     }
}

     parameters.max_hit_retrieved = 256;

     set_query_parameter(SET_MAX_RETRIEVED_MASK, &parameters);
     
     search_result = false;
     search_result |= search_for_words(SearchWords, db, 0);
     
     if (search_result == true) {
	  /* the search went ok */
	  hit best_hit;
	  
	  finished_search_word(db);
	  Debug("WaisIndexQuery:After finished_search\n",0);

	  uchdir(Data_Dir); /* necessary to find side files */

	  if (view != NULL)
	       GSsendHeader(sockfd, -1);

	  for (i = 0; i < parameters.max_hit_retrieved; i++){ 
	       if (0 != next_best_hit(&best_hit, db))
		    break;		/* out of hits */
	       if (i == 0)
		    maxRawScore = best_hit.weight;
	       if (best_hit.weight > 0 && 
		   strstr(best_hit.filename, ".cache")==NULL &&
		   strstr(best_hit.filename, ".cap/")==NULL){
		    long lines,length;

		    char** type = NULL;
		    
		    normalScore = (long)floor((((double)best_hit.weight) /
					       ((double)maxRawScore)) *	
					      (MAX_NORMAL_SCORE + 1));

		    if (normalScore > MAX_NORMAL_SCORE)
			 normalScore = MAX_NORMAL_SCORE;
		    

		    /*** Strip off the first part of the path in the filename*/
		    /*** Plus it gets rid of weird automount things... ***/
		    Selstrout =strstr(best_hit.filename, INDEXPath);
		    if (Selstrout == NULL)
			 Selstrout = "Error in Hostdata!";
		    else
			 Selstrout += strlen(INDEXPath);
		    

                    sprintf(score,"%3d ",best_hit.weight);

		    /*  Causes freewais to core dump... 
                    waislog(0,99,"%s: Score %3d:%s",SearchWords,best_hit.weight,Selstrout);*/
		    
		    /** Make the outgoing string **/

		    ZapCRLF(best_hit.headline);
		    
		    /*** Remove the gopher data directory pathname if
		         it's there from the headline
		    ***/

		    if ((cp = strstr(best_hit.headline, INDEXPath)) != NULL) {
			 /*** Dangerous.... ***/
			 strcpy(cp, cp+strlen(INDEXPath));
		    } 
			 
		    if ((strstr(best_hit.type, "PS") != NULL)
			|| (strstr(best_hit.type, "DVI") != NULL)
			|| (strstr(best_hit.type, "GIF") != NULL)
			|| (strstr(best_hit.type, "TIFF") != NULL))
			 GSsetType(gs, A_IMAGE);
		    else
			 GSsetType(gs, A_FILE);

		    GSsetTitle(gs, best_hit.headline);
		    GSsetHost(gs, INDEXHost);
		    GSsetPort(gs, INDEXPort);

		         /* removed "/" from following line (before %s) . 
			    Was getting double slash at least with w8b5bio; 
			    mtm 11-23-92 */

		    sprintf(ReturnLine, "R%d-%d-%s",
			    best_hit.start_character, best_hit.end_character,
			    Selstrout);
		    
		    if (!MacIndex)
			GSsetPath(gs, ReturnLine);
		    else
			GSsetPath(gs, Selstrout);
		    GSsetWeight(gs, best_hit.weight);
		    
                    /* 
		     * Find and process sidefile. 
		     * Allow worst case name length. 
		     */

		    if((sidename = (char *) malloc((unsigned) 
		        strlen(Selstrout) + 
	                strlen("/.cap/") + 1)) != NULL) {
		      if((cp = mtm_basename(Selstrout)) != Selstrout) {
			/*  turn "/foo/bar/baz" into "/foo/bar/.cap/baz" */
			strncpy(sidename,Selstrout,(cp - Selstrout));
			*(sidename + (cp - Selstrout)) = '\0';
			strcat(sidename,".cap/");
			strcat(sidename,cp);
		      }
		      else {
		      /* root of the gopher tree, this is easier... */
			strcpy(sidename,"/.cap/");
			strcat(sidename,Selstrout);
		      }
		      if ((SideFile = rfopen(sidename, "r")) != NULL) {
			   Debug("Side file name: %s\n", sidename);
			Process_Side(SideFile, gs);
		      }
		      free(sidename);
		    }
		    
		    Debug("Doc type is %s\n", best_hit.type);
		    if (strcmp(best_hit.type, "GOPHER")==0) {
			 Debug("Got a veronica style thing %s\n",best_hit.headline);
			Process_Veronica(&best_hit, gs);
		   }			
		    
		    if (isgplus)
			 GSsendHeader(sockfd, -1);

		    if (plusdirs)
			 GSplustoNet(gs, sockfd, NULL);
		    else
			 GStoNet(gs,sockfd);
	       }
	       
	       Debug("WaisIndexQuery ReturnLine=%s\r\n", ReturnLine);
	       Debug("WaisIndexQuery: End=%d;", best_hit.end_character);
	       Debug("Length=%d;", best_hit.document_length);
	       Debug("lines=%d\r\n", best_hit.number_of_lines);
	  }
     }
     else {
	  /* something went awry in the search */
	  LOGGopher(sockfd, "Something went wrong in the search!");
	  writestring(sockfd, ".\r\n"); /*** be polite, don't screw up the client**/
	  return;
     }
     finished_best_hit(db);

     writestring(sockfd, ".\r\n");

     /* free everything */
     closeDatabase(db);
     return;
}

EveryWAISdocument(sockfd, db, INDEXHost, INDEXPort, INDEXPath)
  int sockfd;
  char *db;
  char *INDEXHost;
  int  INDEXPort;
  char *INDEXPath;
{
     FILE         *dbcatalog;
     char         db_name[MAXPATHLEN];
     char         inputline[512];
     String       *Headline;
     String       *Filename;
     int          StartByte, EndByte;
     GopherObj    *gs;
     GopherDirObj *gd;
     boolean      Headlineset = FALSE;
     boolean      DocIDset    = FALSE;

     gs = GSnew();
     gd = GDnew(32);
     Headline = STRnew();
     Filename = STRnew();

     strcpy(db_name, db);
     strcat(db_name, ".cat");

     dbcatalog = rfopen(db_name, "r");
     
     while (fgets(inputline, sizeof(inputline), dbcatalog) != NULL) {
	  if (strncmp(inputline, "Headline: ", 10)==0) {
	       STRset(Headline, inputline +10);
	       Headlineset = TRUE;
	  }
	  else if (strncmp(inputline, "DocID: ", 7)==0) {
	       char *cp;

	       StartByte = atoi(inputline);
	       cp = strchr(inputline+7, ' ');
	       if (cp == NULL) break;

	       cp++;
	       EndByte = atoi(cp);

	       cp = strchr(inputline+7, ' ');
	       cp++;
	       if (cp == NULL) break;

	       cp =strstr(cp, INDEXPath);
	       if (cp == NULL) break;
	       
	       STRset(Filename, cp);

	       DocIDset = TRUE;
	  }
	  
	  if (DocIDset == TRUE && Headlineset == TRUE) {
	       char tmppath[512];
	       Extobj *ext = EXnew();

	       if (GDCBlockExtension(Config, STRget(Filename), ext)) {
		    GSsetType(gs, EXgetObjtype(ext));
	       } else
		    GSsetType(gs, '0');
		    
	       EXdestroy(ext);

	       sprintf(tmppath, "R%d-%d-%s", StartByte, EndByte, STRget(Filename));

	       GSsetTitle(gs, STRget(Headline));
	       GSsetHost(gs, INDEXHost);
	       GSsetPort(gs, INDEXPort);
	       GSsetPath(gs, tmppath);

	       GDaddGS(gd, gs);

	       DocIDset = FALSE;
	       Headlineset = FALSE;
	  }
     }	  
}

#endif /** WAISSEARCH **/
