/* File: sather/sys/C/rt_sux_.c
   Author: Heinz W. Schmidt
   Created: Wed May 22 14:12:40 1991
   Copyright (C) International Computer Science Institute, 1991, 1992, 1993 

   COPYRIGHT NOTICE: This code is provided "AS IS" WITHOUT ANY WARRANTY
   and is subject to the terms of the SATHER LIBRARY GENERAL PUBLIC
   LICENSE contained in the file: "sather/doc/license.txt" of the Sather
   distribution. The license is also available from ICSI, 1947 Center
   St., Suite 600, Berkeley CA 94704, USA.

   Changes: Heinz W. Schmidt (hws@csis.dit.csiro.au)
            Oscar Bosman (oscar@csis.dit.csiro.au)
   (c) Commonwealth Scientific and Industrial Research Organisation (CSIRO),
   Australia, 1992, 1993.
   The modifications are provided "AS IS" WITHOUT ANY WARRANTY and are subject
   to the terms of the SATHER LIBRARY GENERAL PUBLIC LICENCE referred to above.
 
 ** RCS: $Id: rt_sux_.c,v 1.1 1994/02/12 03:23:35 hsu Exp $
 ** HISTORY:
 ** Last edited: Oct 24 19:23 1993 (hws)
 **  Oct 24 16:54 1993 (hws): Optionally use SIGABRT instead of SIGQUIT with strict ANSI.
 **  Sep  3 11:46 1993 (oscar): SGI don't like ; after function decl.
 **  May 19 20:14 1993 (hws): improve continuation message
 **  May 12 18:31 1993 (hws): allow user to resume after exception under sdb
 **  May  4 15:52 1993 (hws): adapt signal callsfor ANSI C
 **  Apr  3 21:28 1993 (hws): all uppercase Sater interface removed
 **  Jun 29 23:21 1992 (hws): use mapped (c_name) sux22_create SUX::create
*/ 
/* We cannot compile the complete runtime system on some suns due to asm */
/* memory problems. */

#include "all_.h"
#include <memory.h>
#include <string.h>
#include <stdio.h>

#include "macros_.h"

#include <signal.h>

/* Sys V have variation in codes and names. We always initialize at runtime. */
/* EH_N_SIGNALS number of signals */
/* EH_SUX_ATOMIC whether SUX instances have no pointers in them */

#define EH_N_SUX_SIGNALS 32
#define EH_N_SIGNALS 64
#define EH_SUX_ATOMIC 1
#define EH_UNDEF (-1)
#define EH_SIG_ERR (int (*)())-1

char *eh_signal_type_[EH_N_SIGNALS];
char *eh_signal_doc_[EH_N_SIGNALS];

char *eh_signal_type_str_(b)
     int b;
{ return(eh_signal_type_[b]); }

char *eh_signal_doc_str_(b)
{ return(eh_signal_doc_[b]); }

eh_signal_setup_(code,type,doc)
     int code;
     char *type,*doc;
{ 
  /* fprintf(stderr,"Setup signal %d, %s, %s\n", code, type, doc); */
  eh_signal_type_[code] = type;
  eh_signal_doc_[code] = doc;
}

void *eh_old_sigquit_hdlr;
int TRACE_BACK_;
int EH_TRACE_LEVEL_ = 0;

/* ANSI C DOES NOT SUPPORT SIGQUIT. USE ABORT SIGNAL INSTEAD. */

#ifdef __STDC__
#   ifndef SIGQUIT
#       define SIGQUIT SIGABRT
#   endif
#endif

void eh_restore_sigquit()
{  
   fprintf(stderr, "Runtime-system default handler: TRACE_BACK_ = ");
   switch (TRACE_BACK_) {
   case EH_NO_TRACE_:			/* restore old quit handler and quit */
     fprintf(stderr, "EH_NO_TRACE_ (quit like that).\n");
     signal(SIGQUIT, eh_old_sigquit_hdlr); 
     kill(getpid (), SIGQUIT); 
     break; 
   case EH_TRACE_:			/* quit like above with trace */
     fprintf(stderr, "EH_TRACE_ (quit with trace).\n");
     signal(SIGQUIT, eh_old_sigquit_hdlr); 
/*   display_tracestk_(); */ /* provide trace back and quit */
     kill(getpid (), SIGQUIT); 
     break; 
   case EH_INTERACT_:			/* interactive */
     fprintf(stderr, "EH_INTERACT_ (type what next).\n");
     printf("\nContinue? (y or n): ");
     fflush(stdout);
     {
       int c;

       while (1)
	 {
	   c = fgetc (stdin);
	   if (c == -1 || c == 'y' || c == 'n')
	     break;
	   printf("\nWhat? (y or n): ");
	   fflush(stdout);
	 }
       if (c == 'n') {
	 signal(SIGQUIT, eh_old_sigquit_hdlr); 
	 kill(getpid (), SIGQUIT);
       } 
     }
     break;       
   default:				/* resume N times and then quit */
     fprintf(stderr, "EH_TRACE_LIM_ = %d (continue at most %d times).\n", EH_TRACE_LIM_, EH_TRACE_LIM_);
     if ( EH_TRACE_LEVEL_ > EH_TRACE_LIM_ ) {
       signal(SIGQUIT, eh_old_sigquit_hdlr);
       kill(getpid (), SIGQUIT); break;
     } else EH_TRACE_LEVEL_++;
     break;
   }
 }

#include <signal.h>
/* * Last edited: Mar 29 21:20 1992 (hws) */

extern ptr sux22_create();

void eh_signal_sux_(b)  /* called by system when terminating signal detected */
     int b;
{
  /*  fprintf(stderr,"Going to signal %d\n", b); */
  EH_THROW(sux22_create(S_ptr_VOID_,b));
  /* We can end up here now, for instance user can continue under sdb */
}

void eh_expect_signal_(b)
     int b;
{
  if ( b<1||b>(EH_N_SUX_SIGNALS-1) ) 
    { fprintf(stderr,"(C::eh_signal_sux_) signal code out of range %d.\n",b);
      exit(-1);
    }
  if (eh_signal_type_[b]==(char *)0) 
    { fprintf(stderr,"(C::eh_signal_sux_) non-existent signal %d.\n",b); 
      exit(-1);
    }
  if (b==SIGQUIT) eh_old_sigquit_hdlr=signal(b,eh_signal_sux_); 
    else signal(b,eh_signal_sux_);
}

int sighup_ = EH_UNDEF;
int sigint_ = EH_UNDEF;
int sigquit_ = EH_UNDEF;
int sigill_ = EH_UNDEF;
int sigtrap_ = EH_UNDEF;
int sigiot_ = EH_UNDEF;
int sigabrt_ = EH_UNDEF;
int sigemt_ = EH_UNDEF;
int sigfpe_ = EH_UNDEF;
int sigkill_ = EH_UNDEF;
int sigbus_ = EH_UNDEF;
int sigsegv_ = EH_UNDEF;
int sigsys_ = EH_UNDEF;
int sigpipe_ = EH_UNDEF;
int sigalrm_ = EH_UNDEF;
int sigterm_ = EH_UNDEF;
int sigurg_ = EH_UNDEF;
int sigstop_ = EH_UNDEF;
int sigtstp_ = EH_UNDEF;
int sigcont_ = EH_UNDEF;
int sigchld_ = EH_UNDEF;
int sigcld_ = EH_UNDEF;
int sigttin_ = EH_UNDEF;
int sigttou_ = EH_UNDEF;
int sigio_ = EH_UNDEF;
int sigpoll_ = EH_UNDEF;

eh_signals_init_()
{
  int i;
  for (i = 0; i < EH_N_SIGNALS; i++)
    eh_signal_type_[i] = (char *)0;

  /* Sather runtime signals */
  eh_signal_setup_(OUT_OF_MEM_ERR_,     "RT_OUTOFMEM",    OUT_OF_MEM_MSG_);

  eh_signal_setup_(NO_ATTR_TAB_ERR_,    "RT_MISATTBL",   NO_ATTR_TAB_MSG_);
  eh_signal_setup_(NO_FEAT_TAB_ERR_,    "RT_MISFEATBL",  NO_FEAT_TAB_MSG_);
  eh_signal_setup_(B_FILE_MISSING_ERR_, "RT_MISBFILE",   B_FILE_MISSING_MSG_);
  eh_signal_setup_(NO_DES_TAB_ERR_,     "RT_MISDESTBL",  NO_DES_TAB_MSG_);
  eh_signal_setup_(MISS_DISP_ERR_,      "RT_MISDISPREF", MISS_DISP_MSG_);

  eh_signal_setup_(VOID_OBJ_ERR_,       "RT_VOIDOBJ",    VOID_ERR_MSG_);
  eh_signal_setup_(INVALD_CLS_ERR_,     "RT_BADCLASS",   INVALD_CLS_MSG_);
  eh_signal_setup_(INVALD_FEAT_ERR_,    "RT_BADFEAT",    INVALD_FEAT_MSG_);
  eh_signal_setup_(DISP_NEW_ERR_,       "RT_BADNEWARGS", DISP_NEW_MSG_);
  eh_signal_setup_(ILL_DISP_ERR_,       "RT_BADDISPREF", ILL_DISP_MSG_);

  eh_signal_setup_(INVALD_ATTR_ERR_,    "RT_BADATTR",    INVALD_ATTR_MSG_);
  eh_signal_setup_(ARR_BOUND_ERR_,      "RT_BOUNDARR",   ARR_BOUND_MSG_);
  eh_signal_setup_(TYPE_MISMATCH_ERR_,  "RT_TYPVIOL",    TYPE_MISMATCH_MSG_);
  eh_signal_setup_(EH_VOID_OBJ_ERR_,    "RT_VOIDEXC",    EH_VOID_OBJ_MSG_);
  eh_signal_setup_(EH_VOID_TYPE_ERR_,   "RT_BADEXCTYP",  EH_VOID_TYPE_MSG_);
  eh_signal_setup_(TYPE_ASSERT_ERR_,    "RT_SPECVIOL",   TYPE_ASSERT_MSG_);

  /* Unix system signals */
#ifdef SIGHUP
  eh_signal_setup_(SIGHUP,"SIGHUP","hangup");
  sighup_ = SIGHUP;
#endif
#ifdef SIGINT
  eh_signal_setup_(SIGINT,"SIGINT","interrupt"); 
  sigint_ = SIGINT;
#endif
#ifdef SIGQUIT
  eh_signal_setup_(SIGQUIT,"SIGQUIT","quit"); 
  sigquit_ = SIGQUIT;
#endif
#ifdef SIGILL
  eh_signal_setup_(SIGILL,"SIGILL","illegal instruction (not reset when caught)");
  sigill_ = SIGILL;
#endif
#ifdef SIGTRAP
  eh_signal_setup_(SIGTRAP,"SIGTRAP","trace trap (not reset when caught)");
  sigtrap_ = SIGTRAP;
#endif
#ifdef SIGIOT
  eh_signal_setup_(SIGIOT,"SIGIOT","IOT instruction");
  sigiot_ = SIGIOT;
#endif
#ifdef SIGABRT
  eh_signal_setup_(SIGABRT,"SIGABRT","used by abort, replace SIGIOT in the future");
  sigabrt_ = SIGABRT;
#endif
#ifdef SIGEMT
  eh_signal_setup_(SIGEMT,"SIGEMT","EMT instruction");
  sigemt_ = SIGEMT;
#endif
#ifdef SIGFPE
  eh_signal_setup_(SIGFPE,"SIGFPE","floating point exception");
  sigfpe_ = SIGFPE;
#endif
#ifdef SIGKILL
  eh_signal_setup_(SIGKILL,"SIGKILL","kill (cannot be caught or ignored)");
  sigkill_ = SIGKILL;
#endif
#ifdef SIGBUS
  eh_signal_setup_(SIGBUS,"SIGBUS","bus error");
  sigbus_ = SIGBUS;
#endif
#ifdef SIGSEGV
  eh_signal_setup_(SIGSEGV,"SIGSEGV","segmentation violation");
  sigsegv_ = SIGSEGV;
#endif
#ifdef SIGSYS
  eh_signal_setup_(SIGSYS,"SIGSYS","bad argument to system call");
  sigsys_ = SIGSYS;
#endif
#ifdef SIGPIPE
  eh_signal_setup_(SIGPIPE,"SIGPIPE","write on a pipe with no one to read it");
  sigpipe_ = SIGPIPE;
#endif
#ifdef SIGALRM
  eh_signal_setup_(SIGALRM,"SIGALRM","alarm clock");
  sigalrm_ = SIGALRM;
#endif
#ifdef SIGTERM
  eh_signal_setup_(SIGTERM,"SIGTERM","software termination signal from kill");
  sigterm_ = SIGTERM;
#endif
#ifdef SIGURG
  eh_signal_setup_(SIGURG,"SIGURG","urgent condition on IO channel");
  sigurg_ = SIGURG;
#endif
#ifdef SIGSTOP
  eh_signal_setup_(SIGSTOP,"SIGSTOP","sendable stop signal not from tty");
  sigstop_ = SIGSTOP;
#endif
#ifdef SIGTSTP
  eh_signal_setup_(SIGTSTP,"SIGTSTP","stop signal from tty");
  sigtstp_ = SIGTSTP;
#endif
#ifdef SIGCONT
  eh_signal_setup_(SIGCONT,"SIGCONT","continue a stopped process");
  sigcont_ = SIGCONT;
#endif
#ifdef SIGCHLD
  eh_signal_setup_(SIGCHLD,"SIGCHLD","to parent on child stop or exit");
  sigchld_ = SIGCHLD;
#endif
#ifdef SIGCLD
  eh_signal_setup_(SIGCLD,"SIGCLD","System V name for SIGCHLD");
  sigcld_ = SIGCLD;
#endif
#ifdef SIGTTIN
  eh_signal_setup_(SIGTTIN,"SIGTTIN","to readers pgrp upon background tty read");
  sigttin_ = SIGTTIN;
#endif
#ifdef SIGTTOU
  eh_signal_setup_(SIGTTOU,"SIGTTOU","like TTIN for output if (tp->t_local&LTOSTOP)");
  sigttou_ = SIGTTOU;
#endif
#ifdef SIGIO
  eh_signal_setup_(SIGIO,"SIGIO","input/output possible signal");
  sigio_ = SIGIO;
#endif
#ifdef SIGPOLL
  eh_signal_setup_(SIGPOLL,"SIGPOLL","System V name for SIGIO");
  sigpoll_ = SIGPOLL;
#endif
}

/* EOF rt_sux */
