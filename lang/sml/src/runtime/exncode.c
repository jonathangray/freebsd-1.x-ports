/* exncode.c
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * NOTE: this file should be merged with signal.c
 */

#ifdef MIPS
#ifdef sony_news
#include <machine/cpu.h>   /* for EXC_OV */
#else
#ifndef SGI
#include <mips/cpu.h>   /* for EXC_OV */
#else
#include <sys/sbd.h>   /* for EXC_OV */
#endif
#endif
#endif
#include <signal.h>
#ifdef SOLARIS
#include <siginfo.h>
#endif
#ifdef DYNIX
#include <sys/types.h>
#include <machine/fpu.h>
#endif
#include "tags.h"
#include "ml_types.h"

extern int	div_e0[], overflow_e0[];

#ifdef HPUX
#  define CODE_FLTDIV		0x0400
#  ifdef HPPA
#    define CODE_FLTOVF         12
#  else
#    define CODE_FLTOVF		0x1000
#    define CODE_FLTUND		0x0800
#    ifndef FPE_FLTOPERR_TRAP
#      define FPE_FLTOPERR_TRAP   0x2000
#    endif
#  endif
#endif

#ifdef NeXT
/* these are taken from the 0.9 include files */
#  define CODE_OVERFLOW		0x1c
#  define CODE_ZERODIV		0x14
#  define CODE_FLTDIV		0xc8
#  define CODE_FLTOVF		0xd4
#  define CODE_FLTUND		0xcc
#endif

#ifdef THINK_C
#include "MacOS.dep.h"
#  define CODE_OVERFLOW		0x1c
#  define CODE_ZERODIV		0x14
/* these are taken from the NEXT defn above  -- e, check this! */
#  define CODE_FLTDIV		0xc8
#  define CODE_FLTOVF		0xd4
#  define CODE_FLTUND		0xcc
#endif

/* make_exn_code:
 * Map a UNIX (signal, code) pair to an ML (exception, arg) pair.
 */
#if defined(AIX)
#include <fpxcp.h>

void make_exn_code(msp,scp,sig,code)
MLState_ptr 		msp;
struct sigcontext	*scp;
int			sig, code;
	{
 	switch (sig) 
		{
		case SIGTRAP:
			{
			fp_ctx_t	flt_context;

			fp_sh_trap_info(scp,&flt_context);
                        if (flt_context.trap & FP_DIV_BY_ZERO) 
				{
                                msp->fault_exn = PTR_CtoML(div_e0+1);
                                return;
				}
                        else 	{
                                msp->fault_exn = PTR_CtoML(overflow_e0+1);
                                return;
                       		}
			}
                default:
                        die("Unknown signal %d\n", sig);
		}
	}
#else
#ifdef SOLARIS
void make_exn_code (MLState,sig,code)
    MLState_ptr MLState;
    int		sig, code;
#else
void make_exn_code (MLState,scp,sig,code)
    MLState_ptr MLState;
    struct sigcontext *scp;
    int		sig, code;
#endif
{
    switch (sig) {
#ifdef THINK_C
    case SIGTRAP:
#else
      case SIGFPE:
#endif
#if defined(HPUX) && !defined(HPPA)
	if (code == 5) {
	    MLState->fault_exn = PTR_CtoML(div_e0+1);
	    return;
	}
	else
	    code &= 0x3c00;  /* grab exception status */
#endif /* HPUX && !HPPA */

#ifdef HPPA
	/* The cause of the floating exception could be
	** dumped into any of the floating exception registers 
	*/
	{
	    struct save_state *ss;
	    
	    ss = &(scp->sc_sl.sl_ss);
	    if ((ss->ss_frexcp1 & 0x40000000) ||
		(ss->ss_frexcp2 & 0x40000000) ||
		(ss->ss_frexcp3 & 0x40000000) ||
		(ss->ss_frexcp4 & 0x40000000) ||
		(ss->ss_frexcp5 & 0x40000000) ||
		(ss->ss_frexcp6 & 0x40000000) ||
		(ss->ss_frexcp7 & 0x40000000)) 
		{
		    MLState->fault_exn = PTR_CtoML(div_e0+1);
		    return;
		}
	    else if ((ss->ss_frexcp1 & 0x20000000) ||
		     (ss->ss_frexcp2 & 0x20000000) ||
		     (ss->ss_frexcp3 & 0x20000000) ||
		     (ss->ss_frexcp4 & 0x20000000) ||
		     (ss->ss_frexcp5 & 0x20000000) ||
		     (ss->ss_frexcp6 & 0x20000000) ||
		     (ss->ss_frexcp7 & 0x20000000)) 
		
		{
		    MLState->fault_exn = PTR_CtoML(overflow_e0+1);
		    return;
		}
	}
#endif /* HPPA */

#ifdef AUX
	if (code == 5) { /* integer divide by zero */
	    MLState->fault_exn = PTR_CtoML(div_e0+1);
	}
	else
	    die("unexpected floating point error: %d\n", code);
	return;
#else

/* MACH maps mips exceptions to the corresponding BSD exceptions; it
  does it right */

#if defined(MIPS) && !defined(MACH) 
	if (code == EXC_OV) {
	    MLState->fault_exn = PTR_CtoML(overflow_e0+1);
	}
	else {
	  /* code contains the FP control/status register */
	    if (code & 0x4000) /* bit-14 is overflow */
		MLState->fault_exn = PTR_CtoML(overflow_e0+1);
	    else if (code & 0x18000) /* bit-15 is divide by zero,
				       bit-16 is 0/0 */
		MLState->fault_exn = PTR_CtoML(div_e0+1);
	    else if (code & 0x2000 /* bit-13 is underflow */)
		die("underflow should not trap\n");
#ifdef SGI
	    else if (code == 14) /* no known symbolic constant for this */
		MLState->fault_exn = PTR_CtoML(div_e0+1);
#endif
	    else 
	        die("strange floating point error, %d\n",code);
	}
	return;
#else
	switch (code) {
#ifdef CODE_OVERFLOW
	case CODE_OVERFLOW:
#endif
#ifdef FPE_TRAPV_TRAP
	case FPE_TRAPV_TRAP:
#endif
#ifdef FPE_INTOVF_TRAP
	case FPE_INTOVF_TRAP:
#endif
#ifdef FPE_FLTOVF_TRAP
	case FPE_FLTOVF_TRAP:
#endif
#ifdef FPE_FLTOVF_FAULT
	case FPE_FLTOVF_FAULT:
#endif
#ifdef K_INTOVF
	  case K_INTOVF:
#endif
#ifdef CODE_FLTOVF
	  case CODE_FLTOVF:
#endif
#ifdef K_FLTOVF
	  case K_FLTOVF:
#endif
#if   defined(SPARC) && defined (MACH)
	case 0x8:
#endif 
#if defined(HPPA)
	case 14:
#endif
#ifdef FPE_INTOVF
	case FPE_INTOVF:
#endif
#ifdef FPE_FLTOVF
	case FPE_FLTOVF:
#endif
	    MLState->fault_exn = PTR_CtoML(overflow_e0+1);
	    return;
#ifdef CODE_ZERODIV
	  case CODE_ZERODIV:
#endif
#ifdef FPE_INTDIV_TRAP
	  case FPE_INTDIV_TRAP:
#endif
#ifdef FPE_FLTDIV_TRAP
	  case FPE_FLTDIV_TRAP:
#endif
#ifdef FPE_FLTDIV_FAULT
	  case FPE_FLTDIV_FAULT:
#endif
#ifdef FPE_FLTOPERR_TRAP	/* sun-3&4s generate this on 0.0/0.0 */
	  case FPE_FLTOPERR_TRAP:
#endif
#ifdef FPE_FLTINV_TRAP          /* I387's generate this on 0.0/0.0 */
          case FPE_FLTINV_TRAP:
#endif
#ifdef CODE_FLTDIV
	  case CODE_FLTDIV:
#endif
#ifdef K_INTDIV
	  case K_INTDIV:
#endif
#ifdef K_FLTDIV
	  case K_FLTDIV:
#endif
#if    defined(SPARC) && defined(MACH)
	  case 0x82:
#endif
#if    defined(HPPA) && defined(HPUX)
	  case 13:   /* what's the proper symbolic name for this? XXX */
	    
#endif
#ifdef FPE_INTDIV
	  case FPE_INTDIV:
#endif
#ifdef FPE_FLTDIV
	  case FPE_FLTDIV:
#endif
	    MLState->fault_exn = PTR_CtoML(div_e0+1);
	    return;
#ifdef CODE_FLTUND
	  case CODE_FLTUND:
#endif
#ifdef K_FLTUND
	  case K_FLTUND:
#endif
#ifdef FPE_FLTUND_TRAP
	  case FPE_FLTUND_TRAP:
#endif
#ifdef FPE_FLTUND_FAULT
	  case FPE_FLTUND_FAULT:
#endif
#ifdef FPE_FLTUND
	  case FPE_FLTUND:
#endif
		die("underflow should not trap\n");
	  default:
	        die("strange floating point error, %#x\n", code);
	}
#endif MIPS
#endif AUX

#ifndef THINK_C
      case SIGEMT:
	die("Floating point EMT trap (68881 not installed?)\n");
#endif

#ifdef HPUX
      case SIGILL:
	if (code == 7) {
	    MLState->fault_exn = PTR_CtoML(overflow_e0+1);
	    return;
	}
	else
	    die ("exnCode: code was %d\n",code);
#endif HPUX

#ifdef AUX
      case SIGILL:
	if (code == 7) { /* integer overflow trap */
	    MLState->fault_exn = PTR_CtoML(overflow_e0+1);
	}
	else
	    die ("unexpected illegal instruction trap: %d\n",code);
	return;
#endif AUX

#if     defined(SPARC) && defined(MACH)
      case SIGILL:
	if (code == 0x87) {
	      MLState->fault_exn = PTR_CtoML(overflow_e0+1);
	      return;
	    } else
	      die ("SIGILL code 0x%x\n",code);
#endif  /* defined(SPARC) && defined(MACH) */
#ifdef MIPS
      case SIGTRAP:
	switch (code) {
	  case BRK_OVERFLOW:
	    MLState->fault_exn = PTR_CtoML(overflow_e0+1);
	    return;
	  case BRK_DIVZERO:
	    MLState->fault_exn = PTR_CtoML(div_e0+1);
	    return;
	  default:
	    die("illegal BREAK instruction");
	}
#endif MIPS
    } /* end of switch */

} /* end of make_exn_code */
#endif AIX
