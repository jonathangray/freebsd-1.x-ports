/********************************************************************
 * lindner
 * 3.1
 * 1993/03/19 20:00:22
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopherd/tix.h,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992, 1993 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: tix.h
 * Definitions and prototypes of object TIX
 *********************************************************************
 * Revision History:
 * tix.h,v
 * Revision 3.1  1993/03/19  20:00:22  lindner
 * New TIX object
 *
 *
 *********************************************************************/

#ifndef TIX_H
#define TIX_H

#include "STRstring.h"
#include "boolean.h"
#include "DAarray.h"

struct tix_struct {
     String *username;
     char   password[9];  /** max of eight char password +NULL **/
     long   ticket;
};

typedef struct tix_struct TixObj;

#define TIXgetUser(a)    (STRget((a)->username))
#define TIXsetUser(a,b)  (STRset((a)->username,(b)))

#define TIXgetPass(a)    ((a)->password)
#define TIXsetPass(a,b)  (strncpy((a)->password,(b),strlen(b))) 
                              /** don't copy \0, preserve spaces... **/

#define TIXgetPlainTicket(a)   ((a)->ticket)
#define TIXsetPlainTicket(a,b) ((a)->ticket=b)


TixObj *TIXnew();
void    TIXdestroy();
void    TIXinit();
void    TIXcpy();
void    TIXusedTicket();
char *  TIXgetCryptedTicket();
void    TIXrandomTicket();
boolean TIXvalid();
void    TIXtoFile();

/*** Definitions for an array of Tickets ***/

typedef DynArray TixArray;
#define TIXAnew()    (ExtArray *)(DAnew(10, TIXnew,TIXinit,TIXdestroy, TIXcpy))
#define TIXAdestroy(a)       (DAdestroy(a))
#define TIXAgetEntry(a,b)    (TixObj*)(DAgetEntry((DynArray*)a,b))
#define TIXAadd(a,b)         (DApush(a,b))
#define TIXAgetNumEntries(a) (DAgetTop(a))

boolean TIXAfromFile();
void    TIXAtoFile();
int     TIXAsearchUser();

#endif
