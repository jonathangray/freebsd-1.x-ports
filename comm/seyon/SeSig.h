
/*
 * This file is part of the Seyon, Copyright (c) 1992-1993 by Muhammad M.
 * Saggaf. All rights reserved.
 *
 * See the file COPYING (1-COPYING) or the manual page seyon(1) for a full
 * statement of rights and permissions for this program.
 */

/*
 * sighandler.h -
 *	Defines the interface to signalling handling in an Xt safe way
 * $Id: SeSig.h,v 1.1 1994/02/24 22:13:06 jkh Exp $
 * $Log: SeSig.h,v $
 * Revision 1.1  1994/02/24 22:13:06  jkh
 * Initial revision
 *
 * Revision 1.1  92/12/10  08:50:54  ware
 * Initial revision [Modfied by me --M.S.]
 *
 */

typedef void    (*XoSignalCallbackProc) (
#if NeedFunctionPrototypes
					  int signo,	/* the signal number */
					  XtPointer client_data	/* closure */
#endif
);

/*
 * Private structure used to store the information about the currently
 * installed signal handlers
 */

typedef struct _xo_signal_data_ {
  XoSignalCallbackProc handler;	/* function to execute */
  XtPointer       client_data;	/* data to pass */
} _XoSignalData;

extern XoSignalCallbackProc XoAppAddSignal(
#if NeedFunctionPrototypes
				XtAppContext context,	/* application context */
				int sig,	/* which signal */
				XoSignalCallbackProc handler,	/* the handler */
				XtPointer client_data	/* private data */
#endif
);

extern void     XoAppRemoveSignal(
#if NeedFunctionPrototypes
				   XtAppContext context,	/* application context */
				   int sig	/* which signal */
#endif
);

extern void     XoAppIgnoreSignal(
#if NeedFunctionPrototypes
				   XtAppContext context,	/* application context */
				   int sig	/* which signal */
#endif
);
