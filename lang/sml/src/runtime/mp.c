/* mp.c
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * Routines to deal with multiple processors.  These routines are
 * essentially no-ops on all machines except for the SGI.  Routines
 * will be added for Mach based systems in the future.
 */
#ifdef THINK_C
#include <unix.h>
#endif THINK_C
#ifdef SGI
#include <sys/types.h>
#include <sys/prctl.h>
#include <unistd.h>
#endif SGI
#include <signal.h>
#include "ml_state.h"
#include "ml_types.h"
#include "sync.h"
#include "request.h"
#include "cause.h"
#include "prim.h"

#define RETURN(r) {        \
   MLState->ml_arg = (r);  \
   return;}

extern void state_init();
extern void callgc0();

#define refcell(z)	\
    ML_val_t z[2] = {(ML_val_t)MAKE_DESC(1, TAG_array), INT_CtoML(0)}

refcell(active_procs0);
#define active_procs (active_procs0[1])

static double state_vectors[((sizeof(MLState_t)*MAX_PROCS)+7)/8];
#if (MAX_PROCS > 1)
volatile int should_exit = FALSE;
#else /* (MAX_PROCS == 1) */
int should_exit = FALSE;
#endif
MLState_t *MLproc = (MLState_t *)state_vectors;
MLState_ptr Exporters_State = (MLState_ptr)0;

spin_lock_t MLproc_lock;
spin_lock_t siginfo_lock;

#if (MAX_PROCS > 1)
/******************************************************/
/* OS-dependent routines for processes                */
/******************************************************/
#ifdef SGI
/****************/
/* SGI Solution */
/****************/
void block(p)
     pid_t p;
{
  int error,res;

  if ((res = blockproc(p)) == -1) {
    error = oserror();
    chatting("blockproc failed with error %d on proc %d\n",error,p);
    die("%s\n",strerror(error));
  }
}

void unblock(p)
     pid_t p;
{
  int error,res;

  if ((res = unblockproc(p)) == -1) {
    error = oserror();
    chatting("unblockproc failed with error %d on proc %d\n",error,p);
    die("%s\n",strerror(error));
  }
}
  
void signalproc(p) 
     pid_t p;
{
  kill(p,SIGUSR1);
}

int new_proc(child_state)
     MLState_t *child_state;
{
  int ret, error;
  extern void proc_body();

  ret = sproc(proc_body,PR_SALL,child_state);
  if (ret == -1) {
    error = oserror();
    chatting("[warning acquireProc: %s]\n",strerror(error));
  } 
  return ret;
}
#endif SGI

#else /* (MAX_PROCS == 1) */
/**************************/
/* Uni-processor solution */
/**************************/
int new_proc(child_state)
     MLState_t *child_state;
{
  /* always fails */
  return (-1);
}

void block()
{
  die("block called on non-mp system\n");
}

void unblock()
{
  die("unblock called on non-mp system\n");
}

void signalproc()
{
  die("signalproc called on non-mp system\n");
}
#endif /* (MAX_PROCS > 1) */

void
dump_proc_states()
{
  int i,j;
  MLState_t *p;

  for (i=0; i < MAX_PROCS; i++) {
    p = &(MLproc[i]);
    chatting("ml_allocptr   = %x\n",p->ml_allocptr);
    chatting("ml_limitptr   = %x\n",p->ml_limitptr);
    chatting("ml_storeptr   = %x\n",p->ml_storeptr);
    for (j=0; j < NROOTS; j++) 
      chatting("ml_roots[%d] = %x\n",j,p->ml_roots[j]);
    chatting("inML          = %x\n",p->inML);
    chatting("request       = %x\n",p->request);
    chatting("handlerPending= %x\n",p->handlerPending);
    chatting("inSigHandler  = %x\n",p->inSigHandler);
    chatting("maskSignals   = %x\n",p->maskSignals);
    chatting("NumPendingSigs= %x\n",p->NumPendingSigs);
    chatting("ioWaitFlag    = %x\n",p->ioWaitFlag);
    chatting("GCpending     = %x\n",p->GCpending);
    chatting("self          = %x\n",p->self);
    chatting("state         = %x\n",p->state);
    chatting("alloc_boundary= %x\n",p->alloc_boundary);
    chatting("---------------------------------------\n");
  }
}

/* mp_shutdown : sets should_exit, wakes up any suspended procs, and
 * signals running procs so they'll all exit.
 */
void
mp_shutdown(MLState, exit_value)
     MLState_ptr MLState;
     int exit_value;
{
  int i;
  MLState_ptr p;

#if (MAX_PROCS > 1)
  if (!should_exit) {
    should_exit = TRUE;
    for (i=0; i < MAX_PROCS; i++) {
      p = &(MLproc[i]);
      if (p->state == MLPROC_SUSPENDED) {
	unblock(p->self);
      } else if ((p->state == MLPROC_RUNNING) && 
		 (p->self != MLState->self)) {
	signalproc(p->self);
      }
    }
  }
#endif
  exit(exit_value);
}

/* check_suspended : checks to make sure every other proc is not running.
 * raises an exception otherwise.
 */
void
check_suspended(MLState)
     MLState_ptr MLState;
{
  int i;
  MLState_ptr p;

  for (i=0; i < MAX_PROCS; i++) {
    p = &(MLproc[i]);
    if ((p != MLState) && (p->state == MLPROC_RUNNING))  {
      chatting("[warning: procs are running!!!]\n");
      raise_syserror(MLState,0);
    }
  }
}


/* release_proc : unit -> 'a */
void
ml_release_proc (MLState)
     MLState_ptr MLState;
{
  int i;
  extern void turn_off_signals();

#ifdef MP_DEBUG
  pchatting(MLState,"[entering release_proc]\n");
#endif MP_DEBUG
  while (!try_spin_lock(MLproc_lock)) {
    if (MLState->GCpending)
      MLState->mask=CONT_ARGS_MASK;
      callgc0(MLState, CAUSE_GC, 0, CONT_ARGS_MASK);
  }
#ifdef MP_DEBUG
  pchatting(MLState, "[have lock]\n");
#endif MP_DEBUG
  if (active_procs != INT_CtoML(1)) {
    active_procs = INT_CtoML(INT_MLtoC(active_procs) - 1);
    turn_off_signals(MLState);
    MLState->state = MLPROC_SUSPENDED;
    for (i=0; i < NROOTS; i++)
      MLState->ml_roots[i] = ML_unit;
    MLState->ml_varptr = ML_unit;
    MLState->handlerPending = FALSE;
    MLState->inSigHandler = FALSE;
    MLState->maskSignals = FALSE;
    MLState->NumPendingSigs = 0;
    MLState->ioWaitFlag = 0;
    MLState->GCpending = FALSE;
    MLState->mask = 0;
    MLState->amount = 0;
    MLState->SigCode = 0;
    MLState->SigCount = 0;
    for (i=0; i < NUM_ML_SIGS; i++)
      MLState->SigTbl[i] = 0;
    MLState->fault_exn = ML_unit;
#ifdef MP_DEBUG  
    pchatting(MLState,"[releasing lock and suspending self]\n");
#endif MP_DEBUG
    spin_unlock(MLproc_lock);
    block(MLState->self);

    if (should_exit)
      mp_shutdown(MLState, 0);

/* must install any C handlers necessary to deal with signals again -- in case
 * they've changed since we've been asleep. 
 */
    setup_signals (MLState, FALSE); 

#ifdef MP_DEBUG
    pchatting(MLState,"[resumed]\n");
#endif MP_DEBUG
  } else {
    spin_unlock(MLproc_lock);
    RETURN(ML_unit);
  }
}


void
init_proc_state (p)
     MLState_ptr p;
{
  int i;
  
  p->ml_allocptr = 0;
  p->ml_limitptr = 0;
  p->ml_storeptr = 0;
  for (i=0; i < NROOTS; i++)
    p->ml_roots[i] = ML_unit;
  p->ml_varptr = ML_unit;
  p->inML = FALSE;
  p->request = REQ_RUN;
  p->handlerPending = FALSE;
  p->inSigHandler = FALSE;
  p->maskSignals = FALSE;
  p->NumPendingSigs = 0;
  p->ioWaitFlag = 0;
  p->GCpending = FALSE;
  p->self = 0;
  p->state = MLPROC_NO_PROC;
  p->alloc_boundary = 0;
  p->max_allocptr = 0;
  p->mask = 0;
  p->amount = 0;
  p->SigCode = 0;
  p->SigCount = 0;
  for (i=0; i < NUM_ML_SIGS; i++)
    p->SigTbl[i] = 0;
  p->fault_exn = ML_unit;
}

MLState_ptr mp_init (restarted)
     int restarted;
{
  int i;
  MLState_ptr MLState,p;

  should_exit = FALSE;
  active_procs = INT_CtoML(1);
  sync_init(restarted);
  MLproc_lock = runtime_spin_lock();
  siginfo_lock = runtime_spin_lock();
  if (!restarted) {
    MLState = (MLState_t *)(&(MLproc[0]));
    init_proc_state(MLState);
    MLState->ml_storeptr = (int)STORLST_nil;
  } else {
    MLState = Exporters_State;
  }

  /* Initialize other proc's states */
  for (i=0; i < MAX_PROCS; i++) {
    p = (&(MLproc[i]));
    if (p != MLState) {
      init_proc_state(p);
      p->ml_storeptr = (int)STORLST_nil;
    }
  }
  MLState->state = MLPROC_RUNNING;
  MLState->self = getpid();
  MLState->request = REQ_RETURN;
  return MLState;
}


/* Find pointer to own state vector:  Note this is very
   expensive (involves a system call -- getpid) so it should
   be avoided at all costs.  In the future we might replace
   this with a [tricky] machine-dependent way of finding the
   per-proc state (like using the stack of the proc.)
*/
MLState_ptr find_self ()
{
#if (MAX_PROCS > 1)
  int i;
  int id = getpid();

  for (i=0; MLproc[i].self != id; i++);
  return (&(MLproc[i]));
#else
  return (&(MLproc[0]));
#endif /* (MAX_PROCS > 1) */
}

/* acquire_proc : ('a * (unit -> unit)) -> bool */
void
ml_acquire_proc(MLState,arg)
     MLState_ptr MLState;
     ML_val_t arg;
{
#if (MAX_PROCS > 1)
  volatile ML_val_t var_value = REC_SEL(arg,0);
  volatile ML_val_t fn = REC_SEL(arg,1);
#else /* (MAX_PROCS == 1) */
  ML_val_t var_value = REC_SEL(arg,0);
  ML_val_t fn = REC_SEL(arg,1);
#endif 
  int i;
  MLState_ptr p;

#ifdef MP_DEBUG
  pchatting(MLState,"[entering acquire_proc]\n");
#endif MP_DEBUG
  if (active_procs == INT_CtoML(MAX_PROCS)) {
#ifdef MP_DEBUG
    pchatting(MLState,"[active_procs maxed]\n");
#endif MP_DEBUG
    RETURN(ML_false);
  }
  while (!try_spin_lock(MLproc_lock)) {
    if (MLState->GCpending) {
      MLState->mask = CONT_ARGS_MASK;
      callgc0(MLState, CAUSE_GC, 0, CONT_ARGS_MASK);
      var_value = REC_SEL((REC_SEL(MLState->ml_arg,1)),0);
      fn = REC_SEL((REC_SEL(MLState->ml_arg, 1)),1);
    }
  }
#ifdef MP_DEBUG
  pchatting(MLState,"[got lock]\n");
#endif MP_DEBUG
  i = 0;
  while ((i < MAX_PROCS) && (MLproc[i].state != MLPROC_SUSPENDED)) i++;
  if (i == MAX_PROCS) {
    i = 0;
    while ((i < MAX_PROCS) && (MLproc[i].state != MLPROC_NO_PROC)) i++;
    if (i == MAX_PROCS) {
      spin_unlock(MLproc_lock);
#ifdef MP_DEBUG
      pchatting(MLState,"[lock released, no procs]\n");
#endif MP_DEBUG
      RETURN(ML_false);
    }
  }
  active_procs = INT_incr(active_procs,1);
  p = &(MLproc[i]);
  p->ml_exncont = PTR_CtoML(handle_v+1);
  p->ml_arg = ML_unit;
  p->ml_cont = PTR_CtoML(return_c);
  p->ml_closure = fn;
  p->ml_pc = CODE_ADDR(fn);
  p->request = REQ_RUN;
  p->ml_varptr = var_value;
  if (p->state == MLPROC_NO_PROC) {
    p->state = MLPROC_RUNNING;
    if (((p->self) = new_proc(p)) != -1) {
      /* implicit handoff of MLproc_lock to child
	 so that handlers for GC signals may be 
	 installed before someone butts in. */
#ifdef MP_DEBUG
      pchatting(MLState,"[new proc %d]\n",p->self);
#endif MP_DEBUG
      RETURN(ML_true);
    } else {
      p->self = 0;
      p->state = MLPROC_NO_PROC;
      active_procs = INT_CtoML(INT_MLtoC(active_procs) - 1);
      spin_unlock(MLproc_lock);
      RETURN(ML_false);
    }
  } else {
    p->state = MLPROC_RUNNING;
    unblock(p->self);
    spin_unlock(MLproc_lock);
    RETURN(ML_true);
  }
}

void
ml_max_procs(MLState,arg)
     MLState_ptr MLState;
     ML_val_t    arg;
{
  RETURN(INT_CtoML(MAX_PROCS));
}
