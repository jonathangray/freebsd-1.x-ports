/********************************************************************
 * lindner
 * 3.4
 * 1993/07/07 19:42:15
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopher/cso.c,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: cso.c
 * Functions to support CSO qi/ph servers
 *********************************************************************
 * Revision History:
 * cso.c,v
 * Revision 3.4  1993/07/07  19:42:15  lindner
 * fix for cancel from cso screen
 *
 * Revision 3.3  1993/06/08  06:31:01  lindner
 * Fixed infamous cached cso search, added fishing mode
 *
 * Revision 3.2  1993/05/20  06:00:38  lindner
 * Better cso support, fixed bug with unchangable search
 *
 * Revision 3.1.1.1  1993/02/11  18:02:57  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.4  1993/01/08  19:43:01  lindner
 * dialog box cancels automatically if the user doesn't enter anything.
 *
 * Revision 1.3  1992/12/31  05:53:01  lindner
 * Mods for VMS
 *
 * Revision 1.2  1992/12/28  19:02:58  lindner
 * Changed field selection criteria to be based on "Lookup"
 * not "Indexed".  Removed old dead static variables.
 * Changed the name of the popup box from "Ph Query" to the
 * name of the gopher item.
 *
 * Revision 1.1  1992/12/10  23:32:16  lindner
 * gopher 1.1 release
 *
 *********************************************************************/
 
#include "gopher.h"

void 
do_cso(ZeGopher)
  GopherStruct *ZeGopher;
{
     char inputline[1024], *cp;
     int sockfd, len, numfields=0;
     char *Fields[50];
     char *Responses[50];
     char query[512], tmpquery[256];
     int i;

     Draw_Status("Fetching Fields...");
     refresh();

     /*** Fetch the indexed fields from the server ***/
     if ((sockfd = GSconnect(ZeGopher)) <0) {
	  check_sock(sockfd, GSgetHost(ZeGopher), GSgetPort(ZeGopher));
	  return;
     }

     writestring(sockfd, "fields\r\n");
     
     while (1) {
	  len = readline(sockfd, inputline, 1024);
	  twirl();
	  if ((len <= 0) || (strncmp(inputline, "200", 3)==0))
	       break;

	  cp = inputline;
	  if (strstr(inputline, "Lookup") == NULL)
	       continue;
	  
	  cp = strrchr(inputline,':');
	  *cp = '\0';
	  cp --;
	  cp = strrchr(inputline, ':') + 1;
	  
	  if (numfields < (LINES-10)) {
	       /*** Put name at the top ***/
	       
	       if (strcmp(cp, "name") == 0 && numfields != 0) {
		    Fields[numfields] = Fields[0];
		    Fields[0] = strdup(cp);
	       }
	       else {
		    Fields[numfields] = strdup(cp);
	       }
	       Responses[numfields] = (char *) malloc(sizeof(char)*80);
	       *Responses[numfields] = '\0';
	       *(Responses[numfields]+1) = '\0';
	       numfields++;
	  }
     }
     Fields[numfields] = NULL;
     Responses[numfields] = NULL;

     writestring(sockfd, "quit\r\n");
     /** Read the stupid bye message **/
     readline(sockfd, inputline, 1024);
     closenet(sockfd);

     /*** Do cso stuff until user presses CTRL-G ***/

     while (1) {
	  clear();
	  Draw_Status("...");
	  
	  refresh();

	  if (CURRequest(CursesScreen, GSgetTitle(ZeGopher), Fields, Responses) < 0) {

	       /*** Free the memory that we just allocated for fields ***/
	       
	       for (i=0; i<numfields; i++)
		    free(Fields[i]);
	       return;
	  }
     
	  strcpy(query, "query ");
	  
	  for (i=0; i<numfields; i++) {
	       if (*Responses[i] != '\0') {
		    cp = strrchr(Responses[i], ' ');
		    while (cp != NULL) {
			 sprintf(tmpquery, "%s=%s ", Fields[i], cp+1);
			 strcat(query, tmpquery);
			 *cp = '\0';
			 cp = strrchr(Responses[i], ' ');
		    }
		    
		    sprintf(tmpquery, "%s=%s ", Fields[i], Responses[i]);
		    strcat(query, tmpquery);
	       }
	  }
	  if (strlen(query) > 6)
	       GSsetPath(ZeGopher, query);
	  else {
	       for (i=0; i<numfields; i++)
		    free(Fields[i]);
	       return;
	  }
	  
	  Draw_Status("Searching...");
	  refresh();
	  showfile(ZeGopher, NULL);      /* Receive response as a file */
	  unlink(GSgetLocalFile(ZeGopher));
	  GSsetLocalFile(ZeGopher, NULL);
	  
     }	  
}
