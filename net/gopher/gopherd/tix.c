/********************************************************************
 * lindner
 * 3.4
 * 1993/07/27 05:28:02
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopherd/tix.c,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992, 1993 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: tix.c
 * Routines to grant and validate tickets.
 *********************************************************************
 * Revision History:
 * tix.c,v
 * Revision 3.4  1993/07/27  05:28:02  lindner
 * Mondo Debug overhaul from Mitra
 *
 * Revision 3.3  1993/07/07  19:34:14  lindner
 * renamed encrypt() to UMNDESencrypt()
 *
 * Revision 3.2  1993/04/10  06:08:36  lindner
 * Randomizer fixes
 *
 * Revision 3.1  1993/03/19  20:00:19  lindner
 * New TIX object
 *
 *
 *********************************************************************/


#include "tix.h"
#include "Malloc.h"
#include "String.h"
#include "Debug.h"

TixObj *
TIXnew()
{
     TixObj *temp;
     int i;

     temp = (TixObj *) malloc(sizeof(TixObj));
     
     for (i=0; i<8; i++)
	  temp->password[i] = ' ';
     temp->password[8] = '\0';
     
     temp->username = STRnew();
     temp->ticket   = 0;
     
     return(temp);
}
     
void
TIXdestroy(tix)
  TixObj *tix;
{
     STRdestroy(tix->username);
     
     free(tix);
}

void
TIXinit(tix)
  TixObj *tix;
{
     int i;

     STRinit(tix->username);

     for (i=0; i<8; i++)
	  tix->password[i] = ' ';
     tix->password[8] = '\0';

     tix->ticket = 0;
}

void
TIXcpy(dest, orig)
  TixObj *dest, *orig;
{
     STRcpy(dest->username, orig->username);
     strncpy(dest->password, orig->password,8);
     dest->ticket = orig->ticket;
}


/*
 * Okay, we're done with the old ticket, go on to the next
 */

void
TIXusedTicket(tix)
  TixObj *tix;
{
     tix->ticket++;
}



char *
TIXgetCryptedTicket(tix)
  TixObj *tix;
{
     char crypted[17];
     char cleardec[9];
     char clearhex[17];
     char password[17];
     char *cp;
     long out[3];
     
     sprintf(cleardec, "%-8d", TIXgetPlainTicket(tix));
     Hexall(cleardec, clearhex);
     Hexall(TIXgetPass(tix),password);

     Debug("Encrypting %s ", cleardec);
     Debug(" (%s)", clearhex);
     Debug(" with key %s", TIXgetPass(tix));
     Debug(" (%s)", password);

     UMNDESencrypt(clearhex, password, out);
     sprintf(crypted, "%.8lX%.8lX", out[0], out[1]);

     return(strdup(crypted));
}


/*
 * Set up a random ticket
 */

void	  
TIXrandomTicket(tix)
  TixObj *tix;
{
     int t;
     
     srand(time(NULL));
     t = rand();

     while (t > 88888888)
	  t /= 10;
     
     Debug("T is %ld\n", t);

     TIXsetPlainTicket(tix, t);
#ifdef DEBUGGING
     if (DEBUG) {
	  printf("Using default encrypted tix value 1520\n");
	  TIXsetPlainTicket(tix, 1520);
     }
#endif
}


boolean
TIXvalid(tix, crypted)
  TixObj *tix;
  char   *crypted;
{
     char cleardec[9];  /** Ticket in ascii **/
     char clearhex[17]; /** ticket in hex   **/
     char tixfromnet[17];
     char password[17];
     char *cp;
     long out[3];
     
     sprintf(cleardec, "%-8d", TIXgetPlainTicket(tix));
     Hexall(cleardec, clearhex);
     Hexall(TIXgetPass(tix),password);
     
#ifdef DEBUGGING
     if (DEBUG) {
	  printf("Testing user %s, pass: %s (%s), ticket %d (%s)\n",
		 TIXgetUser(tix),TIXgetPass(tix), password,
		 TIXgetPlainTicket(tix), clearhex);
     }
#endif

     
     decrypt(crypted, password, out);
     sprintf(tixfromnet, "%.8lX%.8lX", out[0], out[1]);

     Debug("Decrypted ticket from net is: %s\n", tixfromnet);

     if (strncmp(tixfromnet, clearhex,8) == 0)
	  return(TRUE);
     else
	  return(FALSE);
}     


void
TIXtoFile(tix, fd)
  TixObj *tix;
  int    fd;
{
     char outputline[512];

     sprintf(outputline, "%s:%s:%d\n", TIXgetUser(tix), TIXgetPass(tix),
	     TIXgetPlainTicket(tix));

     writestring(fd, outputline);
}


boolean
TIXfromFile(tix, fd)
  TixObj *tix;
  int    fd;
{
     char inputline[512];
     int  numbytes;
     char *pass, *tick;

     numbytes = readline(fd, inputline, sizeof(inputline));
     if (numbytes <3)
	  return(FALSE);
     
     pass = strchr(inputline, ':');
     if (pass == NULL)
	  return(FALSE);
     *pass = '\0';
     pass++;

     tick = strchr(pass, ':');
     if (tick == NULL)
	  return(FALSE);
     *tick = '\0';
     tick++;

     TIXsetUser(tix, inputline);
     TIXsetPass(tix, pass);
     TIXsetPlainTicket(tix, atoi(tick));

     return(TRUE);
}



/**************************
 * TIXA stuff follows
 */

boolean
TIXAfromFile(tixa, fd)
  TixArray *tixa;
  int      fd;
{
     TixObj *tix;

     tix = TIXnew();

     while (1) {
	  if (TIXfromFile(tix, fd) == FALSE)
	       break;
	  
	  TIXAadd(tixa, tix);
	  TIXinit(tix);
     }
     if (TIXAgetNumEntries(tixa) == 0)
	  return(FALSE);
     else
	  return(TRUE);
}

void
TIXAtoFile(tixa, fd)
  TixArray *tixa;
  int fd;
{
     int i;

     for (i=0; i<TIXAgetNumEntries(tixa); i++)
	  TIXtoFile(TIXAgetEntry(tixa, i), fd);
}



int
TIXAsearchUser(tixa, username)
  TixArray *tixa;
  char *username;
{
     int i;

     for (i=0; i<TIXAgetNumEntries(tixa); i++) {
	  char *tixuser  = TIXgetUser(TIXAgetEntry(tixa,i));

	  if (tixuser == NULL)
	       return(-1);

	  if (strcmp(username, tixuser) == 0)
	       return(i);
     }
     /** didn't find it **/
     return(-1);
}
     

boolean 
TIXAvalid(tixa, username, cryptedticket)
  TixArray *tixa;
  char     *username;
  char     *cryptedticket;
{
     int i;
     boolean status;

     i = TIXAsearchUser(tixa, username);
     if (i<0)
	  return(FALSE);

     status = TIXvalid(TIXAgetEntry(tixa, i), cryptedticket);

     return(status);
}


