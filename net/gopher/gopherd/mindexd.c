/********************************************************************
 * lindner
 * 3.6
 * 1993/08/06 14:36:14
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopherd/mindexd.c,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: mindexd.c
 * Routines to implement a fanout search server
 *********************************************************************
 * Revision History:
 * mindexd.c,v
 * Revision 3.6  1993/08/06  14:36:14  lindner
 * Fix for mindexd and gplus...
 *
 * Revision 3.5  1993/07/29  21:21:27  lindner
 * Removed excess variables
 *
 * Revision 3.4  1993/07/27  05:27:53  lindner
 * Mondo Debug overhaul from Mitra
 *
 * Revision 3.3  1993/07/20  23:52:43  lindner
 * Add .mindex if we can't open the file
 *
 * Revision 3.2  1993/07/13  03:58:44  lindner
 * fixed bug closing an fclose
 *
 * Revision 3.1.1.1  1993/02/11  18:02:56  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.1  1992/12/29  23:27:21  lindner
 * Initial revision
 *
 *
 *********************************************************************/

#include "gopherd.h"
#include "Debug.h"

#define MAXSLAVES 64    

struct one_slave {
   int  port;
   char host[128];
   char pathname[512];
   int  sockfd;
   int  pid;
};

struct one_slave slaves[MAXSLAVES];

int LastSlave;


/*  read the config file and fill the slaves struct, returns
    the number of slave servers */
int
getconfig( the_filename )
  char *the_filename;
{
    char *cp;
    char *local;
    int i;
    FILE *fp;
    char    line[1024];      /* line buffer for reading config */
    char    *linep;         /* pointer to 'line' */


    local = the_filename;

    if ((fp = rfopen(local, "r")) == NULL) {
	 /** Try adding .mindex to the filename **/
	 strcat(local, ".mindex");
	 if ((fp = rfopen(local, "r")) == NULL) {
	      exit(1);
	 }
    }

    LastSlave = -1;
    for (;;) {
        if (fgets(line, sizeof line, fp) == NULL)
               break;  /* done */

        if ((i = strlen(line)))
               line[i-1] = '\0';  /* remove trailing newline */
        if (line[0] == '#' || line[0] == '\0')
               continue;       /* skip comment lines */
        else {
	     char *pathpart;

               LastSlave++;
               linep = line;
               /* the port number is seperated from the host name by a space */
	       /* The path is everything after the port */
	     
 	       pathpart = strchr(strchr(linep, ' ')+1, ' ');
	       *pathpart = '\0';
	       pathpart++;
	       cp = strchr(linep, ' ');
               slaves[LastSlave].port = atoi(cp+1);
               *cp = '\0'; /* trim off stuff after the hostname */
               cp = slaves[LastSlave].host;
               strcpy(cp, linep);
	       cp = slaves[LastSlave].pathname;
	       if (pathpart != NULL) 
		    strcpy(cp, pathpart);
	       else
		    cp = "";
        }
    }
    fclose(fp);

    return( LastSlave );
}


/* take the query the client passed us, and send it on to the slave index
   servers, gather up their responses and return them to our client 

  still need to add code to handle the case where a slave dies on us
*/
void
HandleQuery(sockfd, queryline )
  int  sockfd;
  char *queryline;
{
     char      answerline[512];
     int       length_answer;
     int       i;
     int       childpid;
     GopherObj *gs;

     gs = GSnew();

     Debug("Number slaves is %d",LastSlave);
     Debug(", query is %s\n", queryline);

     /* send queries to all the slaves and gather up responces*/
     for (i = 0; i < ( LastSlave + 1); i++ ) {

        /* fork child processes to handle each of the slaves */

        if ( (childpid = fork()) < 0)
            err_dump("server: fork error");

        else if (childpid == 0) {     /* Child process */
	     GSsetType(gs, '7');
	     GSsetTitle(gs,"");
	     GSsetHost(gs, slaves[i].host);
	     GSsetPort(gs, slaves[i].port);
	     GSsetPath(gs, slaves[i].pathname);


	     slaves[i].sockfd = GSconnect(gs);
	     writestring(slaves[i].sockfd, slaves[i].pathname);
	     writestring(slaves[i].sockfd, "\t");
	     writestring(slaves[i].sockfd, queryline );
	     writestring(slaves[i].sockfd, "\r\n");
	     for(;;) {
		  length_answer = readline(slaves[i].sockfd, answerline, 512);
		  if (length_answer > 0) {
		       if (answerline[0] == '.') {
			    close(slaves[i].sockfd);
			    exit(0);
		       }
		       else {
			    writestring(sockfd, answerline);
		       }
		  }
	     }
        } else {  /** Parent process **/
	     slaves[i].pid = childpid;
	}
	
	
   }
     
     /* make sure all the children are done */
     while (wait((int * ) 0) != -1) 
	  ;
     
     /* all done now, tell the client we are finished */
     writestring(sockfd, ".\r\n");
     
}



int
do_mindexd(sockfd, config_filename, search, isgplus, view)
  int sockfd;
  char *config_filename;
  char *search;
  int  isgplus;
  char *view;
{

     if (isgplus) 
	  GSsendHeader(sockfd, -1);

     if ((LastSlave = getconfig(config_filename)) == -1) 
         err_dump("can't read config file" );
     
     HandleQuery(sockfd, search);
     close(sockfd); 
     return(0);
}



