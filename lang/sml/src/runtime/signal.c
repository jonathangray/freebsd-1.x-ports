/* signal.c
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * Machine independent signal support.
 */

#ifdef THINK_C
#include <signal.h>
#include <setjmp.h>
#else
#ifdef SOLARIS
#include <signal.h>
#include <siginfo.h>
#include <ucontext.h>
#else
#include <sys/signal.h>
#endif
#endif
#include "ml_os.h"
#include "ml_types.h"
#include "request.h"
#include "ml_signal.h"
#include "prim.h"
#include "ml_state.h"
#include "fpregs.h"
#include "sync.h"
#ifdef AIX
#include <fptrap.h>
#include <fpxcp.h>
#include <sys/except.h>
#endif AIX

#include "cstack.h"

#ifdef HPPA
# define sc_pc  sc_pcoq_head
# define sc_npc sc_pcoq_tail
#endif


/* Purpose of the signal flags in the MLState_t vector:
 *   inML          : This flag is set when we are executing ML code.  
 *   handlerPending: This flag is set when a handler trap is pending,
 *                   and cleared when handler trap is occurs.	    
 *   inSigHandler	: This flag is set when a handler trap occurs and
 *                   is cleared when the ML handler returns.	   
 *   maskSignals   : When set, signals are masked.
 *   ioWaitFlag    : This flag is set when we are waiting for a	    
 * 	          blocking I/O operation to be ready.
 *   NumPendingSigs: This is the total number of signals pending.
 *   SigTbl[]      : The number of pending signals of each type
 *   SigCode       : The signal to be passed to ML
 *   SigCount      : The number of pending signals of type sigCode
 */

extern MLState_ptr find_self();
extern void mp_shutdown();
extern spin_lock_t siginfo_lock;

#ifdef THINK_C
#include "MacOS.dep.h"
static char	unix2ml[32]	/* map UNIX signal codes to ML signal codes */
  = {
    ML_NOSIG,	  ML_SIGQUIT,	ML_NOSIG,     ML_NOSIG,		/* 0-3 */    /* 1=SIGABRT=>QUIT? */
    ML_SIGINT,	  ML_NOSIG,	ML_SIGTERM,   ML_NOSIG,		/* 4-7 */
    ML_SIGHUP,	  ML_SIGALRM,	ML_SIGUSR1,   ML_SIGUSR2,	/* 8-11 */
    ML_SIGQUIT,	  ML_SIGTSTP,	ML_SIGCONT,   ML_SIGURG,	/* 12-15 */
    ML_SIGCHLD,	  ML_SIGIO,	ML_SIGWINCH,  ML_NOSIG,		/* 16-19 */
    ML_NOSIG,	  ML_NOSIG,	ML_NOSIG,     ML_NOSIG,		/* 20-23 */
    ML_NOSIG,	  ML_NOSIG,	ML_NOSIG,     ML_NOSIG,		/* 24-27 */
    ML_NOSIG,	  ML_NOSIG,	ML_NOSIG,     ML_NOSIG		/* 28-31 */
  };
#else
static char	unix2ml[]	/* map UNIX signal codes to ML signal codes */
  = {
    ML_NOSIG,	  ML_SIGHUP,	ML_SIGINT,    ML_SIGQUIT,	/* 0-3 */
    ML_NOSIG,	  ML_NOSIG,	ML_NOSIG,     ML_NOSIG,		/* 4-7 */
    ML_NOSIG,	  ML_NOSIG,	ML_NOSIG,     ML_NOSIG,		/* 8-11 */
    ML_NOSIG,	  ML_NOSIG,	ML_SIGALRM,   ML_SIGTERM,	/* 12-15 */
#ifdef HPUX
    ML_SIGUSR1,	  ML_SIGUSR2,	ML_SIGCHLD,   ML_NOSIG,		/* 16-19 */
    ML_NOSIG,	  ML_NOSIG,	ML_SIGIO,     ML_SIGWINCH,	/* 20-23 */
    ML_NOSIG,	  ML_SIGTSTP,	ML_SIGCONT,   ML_NOSIG,		/* 24-27 */
    ML_NOSIG,	  ML_SIGURG,	ML_NOSIG,     ML_NOSIG,		/* 28-31 */
#else /* !HPUX */
#if defined(RISCos) || defined(SGI)
    ML_SIGUSR1,	  ML_SIGUSR2,	ML_SIGCHLD,   ML_NOSIG,		/* 16-19 */
    ML_NOSIG,	  ML_SIGTSTP,	ML_NOSIG,     ML_SIGIO,		/* 20-23 */
    ML_SIGURG,	  ML_SIGWINCH,	ML_SIGVTALRM, ML_SIGPROF,	/* 24-27 */
    ML_SIGCONT,	  ML_NOSIG,	ML_NOSIG,     ML_NOSIG,		/* 28-31 */
#else /* !HPUX && !SGI */
#ifdef AUX
    ML_SIGUSR1,	  ML_SIGUSR2,	ML_SIGCHLD,   ML_NOSIG,		/* 16-19 */
    ML_SIGTSTP,	  ML_NOSIG,	ML_NOSIG,     ML_NOSIG,		/* 20-23 */
    ML_NOSIG,	  ML_NOSIG,	ML_SIGVTALRM, ML_SIGPROF,	/* 24-27 */
    ML_SIGWINCH,  ML_SIGCONT,	ML_SIGURG,    ML_SIGIO		/* 28-31 */
#else /* !HPUX && !SGI && !AUX */
#ifdef AIX
    ML_SIGURG,	  ML_NOSIG,	ML_SIGTSTP,   ML_SIGCONT,	/* 16-19 */
    ML_SIGCHLD,   ML_NOSIG,     ML_NOSIG,     ML_SIGIO,		/* 20-23 */
    ML_NOSIG,     ML_NOSIG,     ML_NOSIG,     ML_NOSIG,		/* 24-27 */
    ML_SIGWINCH,  ML_NOSIG,     ML_SIGUSR1,   ML_SIGUSR2,	/* 28-31 */
    ML_SIGPROF,   ML_NOSIG,     ML_SIGVTALRM, ML_NOSIG,         /* 32-35 */
    ML_NOSIG,     ML_NOSIG,     ML_NOSIG,     ML_NOSIG,         /* 36-39 */
    ML_NOSIG,     ML_NOSIG,     ML_NOSIG,     ML_NOSIG,         /* 40-43 */
    ML_NOSIG,     ML_NOSIG,     ML_NOSIG,     ML_NOSIG,         /* 44-47 */
    ML_NOSIG,     ML_NOSIG,     ML_NOSIG,     ML_NOSIG,         /* 48-51 */
    ML_NOSIG,     ML_NOSIG,     ML_NOSIG,     ML_NOSIG,         /* 52-55 */
    ML_NOSIG,     ML_NOSIG,     ML_NOSIG,     ML_NOSIG,         /* 56-59 */
    ML_NOSIG,     ML_NOSIG,     ML_NOSIG,     ML_NOSIG,         /* 60-63 */
#else /* !HPUX && !SGI && !AUX && !AIX */
#ifdef SOLARIS
    ML_SIGUSR1,   ML_SIGUSR2,   ML_SIGCHLD,   ML_NOSIG,         /* 16-19 */
    ML_SIGWINCH,  ML_SIGURG,    ML_SIGIO,     ML_NOSIG,         /* 20-23 */
    ML_SIGTSTP,   ML_SIGCONT,   ML_NOSIG,     ML_NOSIG,         /* 24-27 */
    ML_SIGVTALRM, ML_SIGPROF,   ML_NOSIG,     ML_NOSIG,         /* 28-31 */
    ML_NOSIG,     ML_NOSIG,                                     /* 32-33 */
#else /* !HPUX && !SGI && !AUX && !AIX && !SOLARIS */
    ML_SIGURG,	  ML_NOSIG,	ML_SIGTSTP,   ML_SIGCONT,	/* 16-19 */
    ML_SIGCHLD,	  ML_NOSIG,	ML_NOSIG,     ML_SIGIO,		/* 20-23 */
    ML_NOSIG,	  ML_NOSIG,	ML_SIGVTALRM, ML_SIGPROF,	/* 24-27 */
    ML_SIGWINCH,  ML_NOSIG,	ML_SIGUSR1,   ML_SIGUSR2	/* 28-31 */
#endif
#endif
#endif
#endif
#endif
  };
#endif

static struct siginfo_t {	/* Info about the ML signals */
    char	    unix_code;	    /* the unix signal code of this signal */
    char	    state;	    /* the state of this signal. */
    char            default_action; /* what to do when disabled */
} siginfo[NUM_ML_SIGS] =
{
    { SIGHUP,	    ML_SIG_DISABLED, DFL_TERM_NO_CORE },
    { SIGINT,	    ML_SIG_DISABLED, DFL_TERM_NO_CORE },
    { SIGQUIT,	    ML_SIG_DISABLED, DFL_NO_HANDLER },
    { SIGALRM,	    ML_SIG_DISABLED, DFL_TERM_NO_CORE },
    { SIGTERM,	    ML_SIG_DISABLED, DFL_TERM_NO_CORE },
    { SIGURG,	    ML_SIG_DISABLED, DFL_IGNORE },
#ifdef HPUX
    { SIGCHLD,	    ML_SIG_DISABLED, DFL_NO_HANDLER },
#else
    { SIGCHLD,	    ML_SIG_DISABLED, DFL_IGNORE },
#endif
    { SIGIO,	    ML_SIG_DISABLED, DFL_IGNORE },
    { SIGWINCH,	    ML_SIG_DISABLED, DFL_IGNORE },
    { SIGUSR1,	    ML_SIG_DISABLED, DFL_TERM_NO_CORE },
    { SIGUSR2,	    ML_SIG_DISABLED, DFL_TERM_NO_CORE },
    { SIGTSTP,	    ML_SIG_DISABLED, DFL_NO_HANDLER },
    { SIGCONT,	    ML_SIG_DISABLED, DFL_NO_HANDLER },
    { SIG_NOT_UNIX, ML_SIG_DISABLED, DFL_IGNORE },	
    { SIGVTALRM,    ML_SIG_DISABLED, DFL_TERM_NO_CORE },	
    { SIGPROF,      ML_SIG_DISABLED, DFL_NO_HANDLER },	
    /* ML_SIGGC (garbage collection) */
};

#ifdef SOLARIS
static sigset_t SIGMASKALL = { 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF};
sigset_t emptyMask;
#else
#define SIGMASKALL		0xffffffff	/* mask all signals */
#endif

/* sig_setup:
 * This (machine independent) function is called by the (machine dependent) function
 * ghandle to set things up for handling a signal in ML.  Since this is called by
 * ghandle, it is atomic w.r.t. signals.
 */
void sig_setup (msp)
    MLState_ptr msp;
{
    register int    nextSig, oldmask;

#ifndef THINK_C
#ifdef SOLARIS
    sigset_t oldset;
    sigprocmask(SIG_SETMASK,&SIGMASKALL,&oldset);
#else
    oldmask  = sigblock(SIGMASKALL);
#endif
#endif
    nextSig = msp->SigCode;

  /* determine which signal to handle */
    do {
	if ((nextSig += 1) == NUM_ML_SIGS)
	    nextSig = 0;
    } while (msp->SigTbl[nextSig] == 0);

  /* record the signal and count */
    msp->SigCode	 = nextSig;
    msp->SigCount	 = msp->SigTbl[nextSig];
    msp->NumPendingSigs  -= msp->SigCount;
    msp->SigTbl[nextSig] = 0;

    msp->handlerPending  = 0;
    msp->inSigHandler    = 1;

#ifndef THINK_C
#ifdef SOLARIS
    sigprocmask(SIG_SETMASK,&oldset,0);
#else
    sigsetmask(oldmask);
#endif
#endif

} /* end of sig_setup */


/* FORCE_TRAP:
 * Set the ML heap limit to 0 so that next limit check will trap back to C.
 * Called from within signal handlers.
 * Implementation is machine-dependent:
 * the heap limit usually lives in a register, which may or may not
 * be accessible through scp; on some machines, it lives in the ML
 * stack frame or in a global.
 * Where possible, we adjust the limit directly from within the handler.
 * Otherwise, we set the handler's return pc to point at a routine
 * adjust_limit (see *.prim.s) that adjusts the limit upon return
 * from the signal handler.  In this case, we store the real return pc
 * in global saved_pc; this solution is inadequate for multiprocessors. */
#ifdef THINK_C
#define FORCE_TRAP(msp,scp) (scp)->sc_regs[5] = 0
#else
#ifdef MIPS
#define FORCE_TRAP(msp,scp) (scp)->sc_regs[19] = 0 
#else
#ifdef I386
#define FORCE_TRAP(msp,scp) (msp)->MLframe[3] = 0
#else
#ifdef HPPA
#define FORCE_TRAP(msp,scp) (scp)->sc_gr19 = 0
#else
#ifdef C
#define FORCE_TRAP(msp,scp) plimit = 0
#else
#ifdef AIX
#define FORCE_TRAP(msp,scp) (scp)->sc_jmpbuf.jmp_context.gpr[15] = 0
#else
int saved_pc;              /* saved_pc to restore in adjust_limit */
# ifdef SPARC
#ifdef SOLARIS
#define FORCE_TRAP(msp,ucp) (ucp)->uc_mcontext.gregs[REG_G4] = 0
#else
#define FORCE_TRAP(msp,scp) {                                            \
     register int pc = (scp)->sc_pc;                                     \
     (scp)->sc_pc = (int)adjust_limit;                                   \
     if (pc+4 != (scp)->sc_npc) {                                        \
	/* the pc is pointing to a delay slot, so back-up to the branch. \
	 * Note: this relies on the assumption that the branch doesn't   \
	 * have a side-effect that interferes with the delay slot. */    \
	pc -= 4;                                                         \
     };                                                                  \
     (scp)->sc_npc = ((int)adjust_limit)+4;                              \
     saved_pc = pc;                                                      \
     }
# endif
# else
#define FORCE_TRAP(msp,scp) {                 \
     register int pc = (scp)->sc_pc;          \
     (scp)->sc_pc = (int)adjust_limit;        \
     saved_pc = pc;                           \
    }
#endif
#endif
#endif
#endif
#endif
#endif
#endif

/* sig_handler:
 * The C signal handler for signals that are to be passed to the ML handler.
 */
#ifdef SOLARIS
SIGH_RET_TYPE sig_handler(sig,info,ctxt)
     int sig;
     struct siginfo *info;
     struct ucontext *ctxt;
#else
SIGH_RET_TYPE sig_handler (sig, code, scp)
    int		    sig, code;
    struct sigcontext *scp;
#endif
{
    extern int	    adjust_limit[];
    MLState_ptr     msp = find_self();
    int             ml_sig;

    ml_sig = unix2ml[sig];
    if ((siginfo[ml_sig].state) == ML_SIG_DISABLED) {
	if (siginfo[ml_sig].default_action == DFL_TERM_NO_CORE) {
	    mp_shutdown (msp, 1);
	}
    }
    else {
    /* record the signal */
      msp->NumPendingSigs++;
      msp->SigTbl[ml_sig]++;

      if (!msp->maskSignals) {
	if (msp->ioWaitFlag) {
	  /* We were waiting for a blocking I/O operation when the signal occurred,
	   * so longjmp out of the operation (see io_wait() in "cfuns.c"). */
#ifdef THINK_C
	     longjmp (msp->SysCallEnv, 1);
#else
#ifdef SOLARIS
	    siglongjmp (msp->SysCallEnv, 1);
#else
	    _longjmp (msp->SysCallEnv, 1);
#endif
#endif
 	  }
	else if (msp->inML && (!msp->handlerPending) && 
		 (! msp->inSigHandler)) {
	    msp->handlerPending	= 1;
#ifdef SOLARIS
	    FORCE_TRAP(msp,ctxt);
#else
	    FORCE_TRAP(msp,scp);
#endif
	  }
      }
    }
} /* end of sig_handler */


#if (MAX_PROCS > 1)
/* The signal handler for GC synchronization signals (SIGUSR1). */
SIGH_RET_TYPE gc_sync_handler (sig, code, scp)
     int  sig, code;
     struct sigcontext *scp;
{
  extern int adjust_limit[];
  extern int should_exit;
  MLState_ptr     msp = find_self();

#ifdef MP_DEBUG
  pchatting("[gsynch signal received]\n");
#endif MP_DEBUG
  if (should_exit) 
    mp_shutdown (msp,0);

  if (msp->ioWaitFlag) {
#ifdef THINK_C
     longjmp (msp->SysCallEnv, 1);
#else
    _longjmp (msp->SysCallEnv, 1);
#endif
  }
  else if (msp->inML) {
    FORCE_TRAP(msp,scp);
  }
}
#endif /* (MAX_PROCS > 1) */


/* handlesys:
 * The handler for SIGSYS.
 */
SIGH_RET_TYPE handlesys ()
{
  /* Long jump to back to ml_syscall. */
#ifdef THINK_C
     longjmp ((find_self())->SysCallEnv, 1);
#else
#ifdef SOLARIS
    siglongjmp ((find_self())->SysCallEnv, 1);
#else
    _longjmp ((find_self())->SysCallEnv, 1);
#endif
#endif
}


/* handleprof:
 * The handler for profile signals.
 */
SIGH_RET_TYPE handleprof ()
{
   extern ML_val_t current0[], *times0[];
   int curr = INT_MLtoC(current0[1]);
   ML_val_t *times_array = times0[1];
   times_array[curr] = INT_incr(times_array[curr],1);
 /* possibility of a slight inaccuracy here:
    if current==2 (meaning "Garbage Collection"), then 
    it could be that times0[1] is in the middle of being forwarded,
    in which case the wrong version of the times array might be incremented.
    However, I don't think this can lead to any bug other than a missed
    tick attributable to g.c. */
}


/* gcsignal:
 * Record a garbage collection signal (if enabled).  Return true, if a signal
 * was recorded.
 */
int gcsignal (msp)
    MLState_ptr msp;
{
    if (siginfo[ML_SIGGC].state == ML_SIG_ENABLED) {
	msp->NumPendingSigs++;
	msp->SigTbl[ML_SIGGC]++;
	return 1;
    }
    else
	return 0;

} /* end of gcsignal */

/* turn_off_signals: tell the OS to ignore all signals.  This is used
 * while a proc is sleeping so that it doesn't react to signals that
 * are sent while it is sleeping.
 */
void turn_off_signals(msp)
     MLState_ptr msp;
{
  int ml_sig, sig;

  for (ml_sig = 0; ml_sig < NUM_ML_SIGS; ml_sig++) {
    sig = siginfo[ml_sig].unix_code;
    if (sig != SIGUSR1)
      SETSIG(sig,SIG_IGN,SIGMASKALL);
  }
}

extern int request_fault[];

#ifdef SOLARIS
static SIGH_RET_TYPE trap_handler(sig,info,ctxt)
     int sig;
     struct siginfo *info;
     struct ucontext *ctxt;
#else
static SIGH_RET_TYPE trap_handler(sig,code,scp)
     int sig,code;
     struct sigcontext *scp;
#endif
{
    MLState_ptr MLState = find_self();

#ifdef BSD386
    /* Due to an OS bug, SIGBUS is generated by an INTO instruction */
    if (sig == SIGBUS) {
       sig = SIGFPE; 
       code = FPE_INTOVF_TRAP;
       /* PC no longer needs to be adjusted by +1? */
    }
#endif

    if (!MLState->inML) 
#ifdef SOLARIS
	die ("bogus signal not in ML: (%d, %#x)\n", sig, info->si_code);
#else
	die ("bogus signal not in ML: (%d, %#x)\n", sig, code);
#endif
    

#ifdef MIPS
    make_exn_code (MLState, scp, sig, code? code : scp->sc_fpc_csr);
#else
#ifdef SOLARIS
    make_exn_code (MLState, sig, info->si_code);
#else
    make_exn_code (MLState, scp, sig, code);
#endif SOLARIS
#endif MIPS


#ifdef C
    request = REQ_FAULT
    saveregs (MLState);
#else
#ifdef AIX
    {
	/*
	** On the RS6000 the exception bits are sticky.
	** We  need to reset them before exit from here.
	*/
	struct mstsave 	* scj = &(scp->sc_jmpbuf.jmp_context);
	fp_ctx_t	flt_context;
    
	scj->xer       	&= 0x3fffffff;
        fp_sh_trap_info(scp,&flt_context);
	fp_sh_set_stat(scp,(flt_context.fpscr & ~flt_context.trap));

        scp->sc_jmpbuf.jmp_context.iar 	= (int)request_fault;
	return;
    }
#else   
#ifdef SOLARIS
    ctxt->uc_mcontext.gregs[REG_PC] = (int)request_fault;
    ctxt->uc_mcontext.gregs[REG_nPC] = (ctxt->uc_mcontext.gregs[REG_PC]) + 4;
    ctxt->uc_mcontext.fpregs.fpu_qcnt = 0;
#else
    scp->sc_pc = (int)request_fault;
#ifdef SPARC
    scp->sc_npc = scp->sc_pc + 4;
#else
#if defined(HPPA) && defined(HPUX)
    scp->sc_pcoq_tail = scp->sc_pc + 4;
    scp->sc_pcsq_tail = scp->sc_pcsq_head = pointer2space(request_fault);
    if (sig == SIGFPE && code == 0xd) {
       /* this is a bit weird.  integer division is done by a function call
	  to C code (millicode really), which requires saving some
	  registers.  the /0 trap happens in that millicode. but
	  request_fault assumes the registers have ML values.  so we have
	  to restore what was saved before doing the division. */

       char *sp = scp->sc_gr30;
       scp->sc_gr8 = * (int *) (sp + callersave_offset(8));
       scp->sc_gr9 = * (int *) (sp + callersave_offset(9));
       scp->sc_gr10 = * (int *) (sp + callersave_offset(10));
       scp->sc_gr11 = * (int *) (sp + callersave_offset(11));
       scp->sc_gr29 = * (int *) (sp + othersave_offset(0));
       scp->sc_gr31 = * (int *) (sp + othersave_offset(1));
       scp->sc_gr26 = * (int *) (sp + othersave_offset(2));
       scp->sc_gr24 = * (int *) (sp + othersave_offset(4));
       scp->sc_gr23 = * (int *) (sp + othersave_offset(5));
       
    }
    scp->sc_sl.sl_ss.ss_frstat = 0xe;
#endif
#endif
#endif
#endif
#endif

} /* trap_handler */

/* setup_signals:  setup the C signal handlers for ML catch-able signals.
 * Also, if this is the first time a proc has called it, then we need to
 * install any C signal handlers needed by the runtime.  Note that on
 * MP systems, SIGUSR1 is unavailable for use.
 */
void setup_signals (msp, first_time)
     MLState_ptr msp;
     int         first_time;
{
    int			ml_sig, sig;

  /* set up the ML signals according to their state */
    for (ml_sig = 0;  ml_sig < NUM_ML_SIGS;  ml_sig++) {
	sig = siginfo[ml_sig].unix_code;
#if (MAX_PROCS > 1)
	if (sig != SIG_NOT_UNIX && sig != SIGUSR1)
#else
	if (sig != SIG_NOT_UNIX)
#endif
	    switch (siginfo[ml_sig].default_action) {
  	      case DFL_NO_HANDLER:
	        if (siginfo[ml_sig].state == ML_SIG_ENABLED) {
		  SETSIG (sig, sig_handler, SIGMASKALL);
		} else {
		  SETSIG (sig, SIG_DFL, SIGMASKALL);
		}
		break;
	      case DFL_TERM_NO_CORE: /* fall through */
	      case DFL_IGNORE:
		SETSIG (sig, sig_handler, SIGMASKALL);
		break;
	      }
    }

    if (first_time) {
#ifdef SOLARIS
      SETSIG (SIGPIPE, SIG_IGN, emptyMask);  /* will force an EPIPE error instead */
#else
      SETSIG (SIGPIPE, SIG_IGN, 0);  /* will force an EPIPE error instead */
#endif
      SETSIG (SIGSYS, handlesys, SIGMASKALL);
      SETSIG (SIGVTALRM, handleprof, SIGMASKALL);
#if (MAX_PROCS > 1)
      SETSIG(SIGUSR1, gc_sync_handler, SIGMASKALL);
#endif

    /* setup the machine dependent signals. */
      SETSIG(SIGFPE, trap_handler, SIGMASKALL);

#ifdef SPARC
#ifdef MACH
  /* MACH on sun-4s use SIGILL for some causes of Overflow */
    SETSIG (SIGILL, trap_handler, SIGMASKALL);
#endif
    set_fsr (0x0d000000); /* enable FP exceptions NV, OF, & DZ; disable UF */
#endif

#ifdef MIPS
    SETSIG(SIGTRAP, trap_handler, SIGMASKALL);
    set_fsr();  /* enable floating-point exceptions */
#endif

#ifdef BSD386
    /* Due to OS bug, SIGBUS is generated by an INTO instruction. */
    SETSIG(SIGBUS, trap_handler, SIGMASKALL);
#endif   

#ifdef AUX
  /* A/UX uses SIGILL for integer overflow traps */
    SETSIG (SIGILL, trap_handler, SIGMASKALL);
#endif

#if defined(M68) || defined(I386)
#ifdef THINK_C
    SETSIG (SIGTRAP, trap_handler, SIGMASKALL);
    init_TRAPS();
    signal(SIGFPE,  e_raise);
    SETSIG (SIGFPE, trap_handler, SIGMASKALL);
    signal(SIGABRT, e_raise);
    signal(SIGINT,  e_raise);
    signal(SIGTERM, e_restart_handler);
#endif
    fpenable();
#endif

#ifdef AIX
    SETSIG(SIGTRAP,trap_handler,SIGMASKALL);
    fp_clr_flag(FP_ALL_XCP);	/* clear all floating exception bits */
    fp_enable_all();		/* enable all FE exceptions */
    fp_disable(TRP_INEXACT);	/* see me if you want this -lg */
#endif AIX
    }

#ifdef HPPA
    set_fsr();
#endif 

} /* end of setup_signals */


/* enable_sig:
 */
void enable_sig (ml_sig, enable)
    int		    ml_sig, enable;
{
    int		    sig;

    while (!(try_spin_lock(siginfo_lock))) /* spin */;

    siginfo[ml_sig].state = (enable ? ML_SIG_ENABLED : ML_SIG_DISABLED);
    spin_unlock(siginfo_lock);
    if (((sig = siginfo[ml_sig].unix_code) != SIG_NOT_UNIX) &&
	(siginfo[ml_sig].default_action == DFL_NO_HANDLER)) {
	if (enable) {
	    SETSIG (sig, sig_handler, SIGMASKALL);
	} else {
	    SETSIG (sig, SIG_IGN, SIGMASKALL);
	}
    }

} /* end of enable_sig */


/* make_ml_sigh_arg:
 * Build the argument record for the ML signal handler.  It has the type
 *
 *   val sigHandler : (int * int * unit cont) -> 'a
 *
 * The first arg is the signal code, the second is the signal count and the
 * third is the resumption continuation.  The ML signal handler should never
 * return.
 *
 * Layout of the resumption continuation:
 *
 *                  resumption continuation
 *                            |
 *                            v
 *   +------------------+----+-+-+-+-+-+~+---------~
 *   | STRING floatregs |desc|1|2|3|4| |B| live regs
 *   +------------------+----+-+-+-+-+|+~+---------~
 *           ^                        |
 *           |________________________|
 *
 * At least 4K avail. heap assumed.
 */
ML_val_t make_ml_sigh_arg (msp)
     MLState_ptr msp;
{
    ML_val_t	resume_c, arg;
    int		i, n, mask;
    ML_val_t 	fpregs;

  /* save floating point registers. */
#if (NSAVED_FPREGS > 0)
#ifdef I386	/* Need to save entire FP state. */
    ML_alloc_write (msp, 0, MAKE_DESC(FP_STATE_SIZE, TAG_string));
    msp->ml_allocptr += sizeof(long);
    fpregs = PTR_CtoML(msp->ml_allocptr);
    savefpregs (msp->ml_allocptr);
    msp->ml_allocptr += FP_STATE_SIZE;
#else
    savefpregs (msp);
    fpregs = PTR_CtoML(msp->ml_allocptr + sizeof(long));
    msp->ml_allocptr += (NSAVED_FPREGS*2 + 1) * sizeof(long);
#endif I386
#else
    fpregs = ML_unit;
#endif

  /* allocate the closure for resume */
    ML_alloc_write (msp, 1, PTR_CtoML(sigh_resume));
    ML_alloc_write (msp, 2, INT_CtoML(msp->mask));
    ML_alloc_write (msp, 3, msp->ml_pc);
    ML_alloc_write (msp, 4, msp->ml_exncont);
    ML_alloc_write (msp, 5, fpregs);
    n = 6; 
    /*
     * note that varptr and (if defined) icount are 
     * shared between mainline ML code and exn handler, so
     * are not saved/restored here.
     */

#if defined(BASE_INDX)
    ML_alloc_write (msp, n, msp->ml_baseptr);
    n++;
#endif
#if defined(C)
      mask = Cmask;
      ML_alloc_write (msp, n, INT_CtoML(Cmask));
      n++;
#else /* !defined(C) */
    mask = msp->mask;
#endif
    for (i = 0;  mask != 0;  i++) {
	if (mask & 0x1) {
	    ML_alloc_write (msp, n, msp->ml_roots[ArgRegMap[i]]);
	    n++;
	}
	mask >>= 1;
    }
    ML_alloc_write (msp, 0, MAKE_DESC(n-1, TAG_record));
    resume_c = ML_alloc(msp, n-1);

  /* allocate the ML signal handler's argument record */
    REC_ALLOC3(msp, arg,
	INT_CtoML(msp->SigCode), INT_CtoML(msp->SigCount), resume_c);

    return arg;

} /* end of make_ml_sigh_arg. */


/* load_resume_state:
 * Load the ML state vector with the state preserved in a resumption continuation
 * built by make_ml_sigh_arg.
 */
void load_resume_state (msp)
     MLState_ptr msp;
{

#if (CALLEESAVE > 0)
    register ML_val_t *p = (ML_val_t *)(PTR_MLtoC(msp->ml_closure));
#else
    register ML_val_t *p = (ML_val_t *)(PTR_MLtoC(msp->ml_cont));
#endif
    register int    i, n, mask;

    mask = msp->mask = INT_MLtoC(p[1]);
    msp->ml_pc = p[2];
    msp->ml_exncont = p[3];
#if (NSAVED_FPREGS > 0)
    restorefpregs(p[4]);
#endif
    n = 5;
#if defined(BASE_INDX)
    msp->ml_baseptr = p[n];
    n++;
#endif
#if defined(C)
  /** NOTE: this probably doesn't work anymore!!! **/
    mask = ((unsigned int) p[n]) >> 1;
    n++;
#endif
    for (i = 0;  mask != 0;  i++) {
	if (mask & 0x1) {
	    msp->ml_roots[ArgRegMap[i]] = p[n++];
	}
	mask >>= 1;
    }

} /* end of load_resume_state. */




