/* run_ml.c
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * These are routines for handling the ML/C interface.
 */

#include "ml_state.h"
#include "ml_types.h"
#include "request.h"
#include "cause.h"
#include "prim.h"
#include "sync.h"
#include "eventchk.h"

/* This table maps the register numbers of the code generator to the proper
 * indices of the root vector.  The order of entries in this table must
 * respect both the MLState vector layout and the order of the miscregs in
 * the C-machine implementation.  The pc, varptr, and exncont are not included.
 */
int		ArgRegMap[N_ARG_REGS] = {
	LINK_INDX, CLOSURE_INDX, ARG_INDX, CONT_INDX,	/* the standard arg registers */
#ifndef C
#  if defined(RS6000)
         9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23
#  endif
#  if defined(SPARC)
	/* misc. regs = %g2-%g3, %o1-%o2, %l0-%l7, %i4; */
	 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 8
#  endif
#  if defined(M68)
	  5
#  endif
#  if defined(VAX)
	 5, 6, 7, 8, 9
#  endif
#  if defined(MIPS)
	 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18
#  endif
#  if defined(HPPA)
	 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19
#  endif
#  if defined(NS32)
	 ??
#  endif
#  if defined(I386)
	 7, 8, 9
#  endif
#else /* !C */
	 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
	 22, 23, 24, 25, 26
#endif /* !C */
    };

void raise_ml_exn ();
static void uncaught ();

extern void check_heap ();
extern void callgc0 ();

#ifdef C
jmp_buf top_level;
#endif

/* run_ml:
 */
static void run_ml (msp)
     MLState_ptr msp;
{
#ifdef C
  top: if (setjmp(top_level))
          goto top;
#endif
    while (1) {

#ifdef ICOUNT
/*      chatting("icount = %d\n", (msp->ml_icount));   */
        msp->ml_icount = (ML_val_t)((int)(msp->ml_icount)+372);
        while ((int)(msp->ml_icount) >= 1000000)
	  {extern int icountM;
           msp->ml_icount = (ML_val_t)((int)(msp->ml_icount)-1000000);
	   icountM += 1;
	  }
#endif
#ifdef MP_DEBUG
	pchatting(msp,"[req = %d]\n",msp->request);
#endif MP_DEBUG
	switch (msp->request) {
	  case REQ_RETURN:
#ifdef GCMON
	    dumpMon(msp);
#endif GCMON
	    return;

	  case REQ_RUN:
	    break;

	  case REQ_EXN: /* an uncaught exception */
	    uncaught (msp->ml_arg);
	    return;

	  case REQ_FAULT: { /* a hardware fault */
		raise_ml_exn (msp);
	    } break;

	  case REQ_GC:
#ifdef C
	    die("internal error: requested gc");
#else
	    callgc0 (msp, CAUSE_GC, 
		     (msp->ml_limitptr + 4096) - (msp->ml_allocptr) + 4);
#endif
	    gcsignal(msp);
	    break;

	  case REQ_CALLC: {
		int	    (*f)();
		ML_val_t    arg;

#if (CALLEESAVE > 0)
		msp->ml_closure = ML_unit;
                msp->ml_pc = msp->ml_cont;
#else 
		msp->ml_closure = msp->ml_cont;
		msp->ml_pc = msp->ml_linkreg = CODE_ADDR(msp->ml_cont);
#endif

		msp->mask = CONT_ARGS_MASK;
		check_heap (msp, 32768);

		f   = (int (*)())REC_SELPTR(msp->ml_arg, 0);
		arg = REC_SEL(msp->ml_arg, 1);
		(*f)(msp, arg);
#ifdef C
		msp->request = REQ_RUN;
		goto top;
#endif
	    } break;

	  case REQ_SIGNAL: {
		extern ML_val_t make_ml_sigh_arg(),sighandler0[];

		sig_setup(msp);
		check_heap(msp, 4096);

		msp->ml_arg     = make_ml_sigh_arg(msp);
		msp->ml_cont    = PTR_CtoML(sigh_return_c);
		msp->ml_exncont = PTR_CtoML(handle_v+1);
		msp->ml_closure = sighandler0[1];
		msp->ml_pc  = msp->ml_linkreg = CODE_ADDR(msp->ml_closure);
#ifdef C
		msp->request = REQ_RUN;
		goto top;
#endif
	    } break;

	  case REQ_SIG_RETURN: {
	      /* Handle the return from the ML signal handler.  The ml_arg
	       * contains the unit resumption continuation (2-arg fn).
	       */
		register ML_val_t kont = msp->ml_arg;

	      /* throw to the cont that we have received as an argument. */
		msp->ml_arg     = ML_unit;
#if (CALLEESAVE > 0)
		msp->ml_closure = kont;
		msp->ml_cont    = ML_unit;
#else 
		msp->ml_closure = ML_unit;
		msp->ml_cont    = kont;
#endif
		msp->ml_pc = msp->ml_linkreg = CODE_ADDR(kont);
		msp->ml_exncont = ML_unit;
	      /* Note that we have finished handling the signal */
		msp->inSigHandler = 0;
#ifdef C
		msp->request = REQ_RUN;
		goto top;
#endif
	    } break;

	  case REQ_SIG_RESUME:
	    load_resume_state(msp);
	    break;

	  default:
	    die ("internal error: unknown request code = %d\n", msp->request);
	    break;
	} /* end of switch */

	msp->request = REQ_GC;
        restoreregs(msp);

	MAYBE_EVENTCHK();
    }

} /* end of run_ml. */


/* initialization for a new processor running ML code */
void proc_body (msp)
    MLState_ptr msp;
{
    extern spin_lock_t MLproc_lock;

    setup_signals(msp, TRUE);
  /* spin until we get our id */
    while (msp->self == 0)
	continue;
#ifdef MP_DEBUG
    pchatting(msp,"[releasing lock]\n");
#endif MP_DEBUG
    spin_unlock (MLproc_lock); /* implicitly handed to the child by the parent */
    run_ml (msp);
  /* should never return */
    die ("proc returned after run_ml() in proc_body().\n");

} /* end of proc_body */


/* raise_ml_exn:
 * Modify the ML state, so that the given exception will be raised when ML is resumed.
 */
void raise_ml_exn (msp)
     MLState_ptr msp;
{
    ML_val_t	        exn = msp->fault_exn;
    register ML_val_t	kont = msp->ml_exncont;

    msp->ml_arg		= exn;
#if (CALLEESAVE > 0)
    msp->ml_closure	= kont;
#else 
    msp->ml_cont	= kont;
#endif
    msp->ml_pc = msp->ml_linkreg = CODE_ADDR(kont);

} /* end of raise_ml_exn. */


int profile_array[201] = {MAKE_DESC(200,TAG_array),
1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1, 
1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1, 
1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1, 
1,1,1,1,1,1,1,1,1,1, 

1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1, 
1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1, 
1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1, 
1,1,1,1,1,1,1,1,1,1 };

/* apply_ml_fn:
 * Apply the ML closure f to arg and return the result.
 */
ML_val_t apply_ml_fn (msp, f, arg)
    MLState_ptr     msp;
    ML_val_t	    f, arg;
{
    register int	i;

  /* clear the ML root registers */
    for (i = 0;  i < NROOTS;  i++)
	msp->ml_roots[i] = ML_unit;

  /* initialize the calling context */
    msp->ml_exncont	= PTR_CtoML(handle_v+1);
    msp->ml_varptr      = PTR_CtoML(profile_array+1);
    msp->ml_arg		= arg;
    msp->ml_cont	= PTR_CtoML(return_c);
    msp->ml_closure	= f;
    msp->ml_pc = msp->ml_linkreg = CODE_ADDR(f);
    msp->request	= REQ_RUN;

    run_ml (msp);

    return msp->ml_arg;

} /* end of apply_ml_fn */


/* restart_ml:
 * Restart an exported ML system.
 */
void restart_ml ()
{
    extern MLState_ptr mp_init();

    MLState_ptr		msp = mp_init(TRUE);

    restart_gc (msp);
    setup_signals (msp, TRUE);

    msp->ml_arg = ML_true;
    msp->request = REQ_RUN;
    run_ml (msp);
      
#ifdef ADVICE
    call_endadvice(msp);
#endif

    mp_shutdown(msp,0);

} /* end of restart_ml */


/* uncaught:
 * Handle an uncaught exception.
 */
static void uncaught (e)
    ML_val_t	e;
{
    ML_val_t	val = REC_SEL(e, 1);
    ML_val_t	name = REC_SEL(REC_SEL(e, 0), 0);

    chatting("Uncaught exception %.*s with ", OBJ_LEN(name), PTR_MLtoC(name));

    if (! OBJ_isBOXED(val))
	chatting("%d\n", INT_MLtoC(val));
    else {
	int tag = OBJ_TAG(val);
	if ((tag == TAG_string) || (tag == TAG_emb_string))
	    chatting("\"%.*s\"\n", OBJ_LEN(val), PTR_MLtoC(val));
	else
	    chatting("<unknown>\n");
    }

    mp_shutdown(find_self(),1);

} /* end of uncaught */
