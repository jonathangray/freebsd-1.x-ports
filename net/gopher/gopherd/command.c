/********************************************************************
 * lindner
 * 3.6
 * 1993/07/29 20:13:32
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopherd/command.c,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992, 1993 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: command.c
 * Routines to parse commands from the client.
 *********************************************************************
 * Revision History:
 * command.c,v
 * Revision 3.6  1993/07/29  20:13:32  lindner
 * Removed dead variables
 *
 * Revision 3.5  1993/07/27  05:27:40  lindner
 * Mondo Debug overhaul from Mitra
 *
 * Revision 3.4  1993/07/23  03:10:53  lindner
 * Added CMDgetFile() fcn
 *
 * Revision 3.3  1993/04/09  16:50:24  lindner
 * nothing
 *
 * Revision 3.2  1993/03/24  20:19:40  lindner
 * Fixed memory leak
 *
 * Revision 3.1  1993/03/19  19:56:53  lindner
 * New CMD object
 *
 *
 *********************************************************************/



#include "Malloc.h"
#include "String.h"
#include <stdio.h>

#include "command.h"
#include "openers.h"
#include "Debug.h"

CMDobj *
CMDnew()
{
     CMDobj *temp;

     temp = (CMDobj *) malloc(sizeof(CMDobj));

     temp->selstr  = temp->command = temp->search = NULL;

     temp->datafromnet   = STRnew();
     temp->askfile       = STRnew();

     temp->secureuser    = STRnew();
     temp->ticket        = STRnew();

     CMDsetGplus(temp, FALSE);
     
     return(temp);
}


void
CMDdestroy(cmd)
  CMDobj *cmd;
{
     STRdestroy(cmd->datafromnet);
     STRdestroy(cmd->askfile);
     STRdestroy(cmd->ticket);
     STRdestroy(cmd->secureuser);

     free(cmd);
}


void
CMDfromNet(cmd, sockfd)
  CMDobj *cmd;
  int    sockfd;
{
     char *cp;
     char inputline[512];
     char *incoming;
     char *field1=NULL, *field2=NULL, *field3 = NULL;
     char *extradata = NULL;
     int length;
     
     length = readline(sockfd, inputline, sizeof(inputline));

     /** Set the alarm signal for about an hour, just in case.. **/
     (void) alarm(60 * 60);


     if (length <= 0) {
	  close(sockfd);
	  err_quit("getcommand: readline error");
     }

     ZapCRLF(inputline);

     Debug("Received: %s\n", inputline);

     CMDsetData(cmd, inputline);

     cp = CMDgetData(cmd);


     cp = CMDticketfromLine(cmd, cp);

     CMDsetSelstr(cmd, cp);
     

     /** Find the first field, if it exists... **/
     cp = strchr(cp, '\t');
     if (cp != NULL) {
	  *cp = '\0';
	  cp++;
	  
	  field1 = cp;
	  
	  /** find the second field, if it exists **/
	  cp = strchr(cp, '\t');
	  if (cp != NULL) {
	       *cp = '\0';
	       cp++;
	       field2 = cp;
	  } else {
	       /** find the third field, if it exists **/
	       if (cp != NULL) 
		    cp = strchr(cp, '\t');
	       if (cp != NULL) {
		    *cp = '\0';
		    cp++;
		    field3 = cp;
	       }
	  }

     }
     /** Okay, now decide which field is the search and 
       which is the command */
     
     if (*inputline == '7' || strncmp(inputline, "waissrc:",8)==0 ||
	 strncmp(inputline, "mindex:",7) ==0) {
	  
	  CMDsetSearch(cmd, field1);
	  CMDsetCommand(cmd, field2);
     } else {
	  CMDsetCommand(cmd, field1);
	  CMDsetSearch(cmd, NULL);
     }

     /** Get the extra data if it exists... **/
     if (field3 != NULL)
	  extradata = field3;
     else if	 (CMDgetSearch(cmd) == NULL && field2 != NULL)
	  extradata = field2;

     if (extradata != NULL) {
	  char *tmp = tempnam(NULL, "gdata");
	  Debug("Ask data is in %s\n", tmp);
	  CMDsetAskfile(cmd, tmp);
	  CMDgetXtra(cmd, sockfd, atoi(extradata));
	  free(tmp);
     }

     if (CMDgetCommand(cmd) != NULL && *CMDgetCommand(cmd) != '\0')
	  CMDsetGplus(cmd, TRUE);

     Debug("Command:: ",0);
     Debug("selstr %s, ", CMDgetSelstr(cmd));
     Debug("command %s, ", CMDgetCommand(cmd));
     Debug("search %s, ", CMDgetSearch(cmd));
     Debug("extradata %s, ", CMDgetAskfile(cmd));
     Debug("user %s, ", CMDgetUser(cmd));
     Debug("ticket %s, ", CMDgetTicket(cmd));
}


/*
 * Retrieve extra data from the client request..  This stuff is optional
 *
 */

void
CMDgetXtra(cmd, fd, extradata)
  CMDobj *cmd;
  int fd;
  int extradata;
{
     FILE *retrfile;
     char inputline[512];

     /** Siphon off data if it's there.. **/

     /** A ticket? **/
     if ((extradata & 0x2) == 0x2) {
	  ;
     }

     /** An ask block **/
     if ((extradata & 0x1) == 0x1) {
	  retrfile = ufopen(CMDgetAskfile(cmd), "w",0777);

	  /** Okay, the next line is either +-1, or +bytes .. **/
	  readline(fd, inputline, sizeof(inputline));
	  ZapCRLF(inputline);
	  if (strcmp(inputline, "+-1")==0) {
	       while (readline(fd, inputline, sizeof(inputline))>0)  {
		    ZapCRLF(inputline);
		    ZapCRLF(inputline);
		    if (*inputline == '.' && *(inputline+1) == '\0')
			 break;
		    fputs(inputline, retrfile);
		    putc('\n', retrfile);
	       }
	  }
     }
     fclose(retrfile);
}



char *
CMDticketfromLine(cmd, input)
  CMDobj *cmd;
  char   *input;
{
     char *cp;
     char *originput = input;

     if (strncmp(input, "*UMNDES ",8)!=0)
	  return(originput);
     
     input+=8;
     
     cp = strchr(input, ' ');
     if (cp == NULL)
	  return(originput);

     *cp = '\0';
     
     CMDsetUser(cmd,input);

     if (strlen(CMDgetUser(cmd)) == 0)
	  return(originput);

     /** Special cases for icky turbogopher **/

     if (strlen(cp+1) < 16)  {
	  input = strchr(cp+1, '\t');
	  *(input-1) = '\0';
	  CMDsetTicket(cmd, cp+1);
	  return(input);
     } else if (*(cp+17) == ' ') {
	  *(cp+17) = '\0';
	  CMDsetTicket(cmd, cp+1);
	  return(cp+18);
     } else {
	  CMDsetTicket(cmd, cp+1);
	  *(CMDgetTicket(cmd)+17) = '\0';
	  return(cp+17);
     }

}

/** Find an associated file from the selector string... Yuck! **/

char *
CMDgetFile(cmd)
  CMDobj *cmd;
{
     char *selstr = CMDgetSelstr(cmd);
     char *cp = NULL;

     switch (*selstr) {
     case '0':
     case '1':
     case '7':
     case '9':
     case 's':
     case 'I':
	  return(selstr+1);

     case 'm':
	  if (strncmp(selstr,"mindex:",7)==0)
	       return(selstr+7);
	  else
	       return(selstr+1);

     case 'R':
	  cp = strchr(selstr, '-');
	  if (cp == NULL)  break;

	  cp++;
	  cp = strchr(cp, '-');
	  if (cp == NULL) break;
	  
	  return(cp+1);


	  /*** Exec: ***/
     case 'e':
	  cp = strrchr(selstr, ':');
	  if (cp!=NULL)
               return(cp+1);
	  else
               return(NULL);
	  break;

	  /** WAIS docid **/
     case 'w':
	  if (strncmp(selstr, "waissrc:",8)==0)
	       return(strchr(selstr, ':')+1);
	  
     default:
	  if (*selstr == '\0') {
	       return("//");
	  } else {
	       return(NULL);
	  }
     }

     return(NULL);
     ;
}
