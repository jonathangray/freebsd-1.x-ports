/*

	IDL:    Bernard Sufrin, Oxford
		X toolkit base
		@(#)idlxt.c	2.1 93/03/07 00:58:09
*/

#       include <stdio.h>
#       include <X11/Intrinsic.h>
#       include <X11/StringDefs.h>
#       include <X11/Xaw/Command.h>
#       include <X11/Xaw/Box.h>
#       include "idlbase.h"

static  int        suspicious;

/*      Threshold  beyond which mere suspicion that
	the client has diedd turns into certainty (see below).
*/
#define CERTAINTY 80000

void IdlEvents(client_data, fid, id)
  XtPointer client_data;
  int       *fid;
  XtInputId *id;
{ int waiting;
  while (waiting=idlcaninput ()) idlevent();
  idlflush();
  /*
	For some reason Xt wakes us up here with 0-length caninput()
	whenever we send a message back to our client. This is not appropriate
	behaviour. This mess is to ensure that we don't leave uncleaned-up
	servers buzzing if the client dies intestate. It would be much more
	straightforward to ask if there's anything at the other
	end of the pipe, but SunOs and X documentation being what they are
	dictate that expediency is the better part of elegance.
  */
  if (waiting==0)
     { suspicious++;
       if (suspicious>CERTAINTY) /* wait a while before believing it */
	{ fprintf(stderr, "[Client probably died]\n");
	  exit(200);
	}
     }
  else
     { suspicious=0; }
}

void IdlErrors(client_data, fid, id)
  XtPointer client_data;
  int       *fid;
  XtInputId *id;
{ fprintf(stderr, "[Warning: client side pipe closed]\n");
  exit(2);
}

#include <signal.h>
void Handler(sig, code, scp, addr)
     int sig, code;
     struct sigcontext *scp;
     char *addr;
{  fprintf(stderr, "[Unexpected Signal %d @0x%x in Server]\n", sig, scp->sc_pc);
   abort();
}

extern
void    XtIdlMainLoop(app_context, topLevel)
	XtAppContext    app_context;
	Widget          topLevel;
{    int i;
     for (i=1; i<31; i++) if (i!=6) signal(i, Handler);


     {   XtInputId idi=XtAppAddInput(app_context, idlinputchannel(),
				   XtInputReadMask, IdlEvents,
				  NULL);
	 XtInputId ide=XtAppAddInput(app_context, idlinputchannel(),
				   XtInputExceptMask, IdlErrors,
				   NULL);
	 while (idlcontinue)
	 { XEvent e;
	   XtAppNextEvent(app_context, &e);
	   XtDispatchEvent(&e);
	 };


	XtRemoveInput(idi);
	XtRemoveInput(ide);
    }
}

