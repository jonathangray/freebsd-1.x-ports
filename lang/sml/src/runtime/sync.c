/* sync.c
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * Provides synchronization primitves for processors.  The try_spin_lock
 * and spin_unlock routines are intended to work on runtime spin locks
 * only.  
 *
 * ml_spin_locks can "share" in the sense that calls to ml_spin_lock
 * can return the same lock (currently happens only on SGI where we
 * have a limited number of hardware locks.)  Runtime spin locks never
 * share.
 */
#include "ml_state.h"
#include "ml_types.h"
#include "tags.h"

#if (MAX_PROCS > 1)
#ifdef SGI
/*******************************************************************/
/* SGI spin locks -- we use the SGI's hardware locks.              */
/*******************************************************************/
#include <sys/types.h>
#include <ulocks.h>
#define MAX_LOCKS 4000

usptr_t *sync_arena;
typedef ulock_t spin_lock_t;

int           max_locks = MAX_LOCKS;
spin_lock_t   locks[MAX_LOCKS];


#define syscall(x,f,v)    if (((x) = (f)) == (v)) { \
			    error = oserror(); \
                            chatting("f failed with error %d\n",error); \
                            die("%s\n",strerror(error)); } else

void sync_init(restarted)
    int restarted;
{
  int error;
  int ret;
  int i, done = FALSE;
  int proc_share, k;
  extern MLState_t *MLProc;
  MLState_ptr p;

  if (!restarted) { 
    syscall(ret, usconfig(CONF_LOCKTYPE, US_NODEBUG), -1);
    syscall(ret, usconfig(CONF_ARENATYPE, US_SHAREDONLY), -1);
  } 
  syscall(sync_arena,usinit(tmpnam(0)),0);
  max_locks = 0;
  for (max_locks = 0; (max_locks < MAX_LOCKS) && (!done); max_locks++) {
    if ((locks[max_locks] = usnewlock(sync_arena)) == 0)
      done = TRUE;
  }
  proc_share = max_locks / MAX_PROCS;
  for (i=0, k=0; i < MAX_PROCS; i++) {
    p = &(MLproc[i]);
    p->lock_base = k;
    p->lock_ptr = k;
    k += proc_share;
    p->lock_top = k - 1;
  }
}

spin_lock_t runtime_spin_lock()
{
  int error;
  spin_lock_t ret;
  syscall(ret,usnewlock(sync_arena),0);
  return ret;
}

void ml_spin_lock (msp)
    MLState_ptr msp;
{
  ML_val_t l = (ML_val_t)&(locks[msp->lock_ptr]);
  msp->lock_ptr++;
  if (msp->lock_ptr > msp->lock_top) {
    msp->lock_ptr = msp->lock_base;
  }
  msp->ml_arg = l;
  return;
}
  
int try_spin_lock(lock)
     spin_lock_t lock;
{
  int error, ret;
  syscall(ret, uscsetlock(lock, 1), -1);
  return(ret);
}

void spin_unlock(lock)
     spin_lock_t lock;
{
  int error, ret;
  syscall(ret, usunsetlock(lock), -1);
}

#endif SGI
#else /* (MAX_PROCS == 1) */
/*******************************************************************/
/* Default case (i.e. MAX_PROCS == 1)                              */
/*******************************************************************/
typedef int *spin_lock_t;

void sync_init (restarted)
    int restarted;
{
    /* nothing */
}

/* These are spin locks to be used by the runtime -- just an int pointer,
   initialized to TRUE.
*/
static int runtime_lock;

spin_lock_t runtime_spin_lock()
{
  spin_lock_t lock;

  runtime_lock = TRUE;
  return (&runtime_lock);
}

/* Allocate a 1-element bool array on the heap. */
void ml_spin_lock(msp)
     MLState_ptr msp;
{
    ML_alloc_write (msp, 0, MAKE_DESC(4, TAG_bytearray));
    ML_alloc_write (msp, 1, ML_true);
    msp->ml_arg = ML_alloc(msp, 1);
    return;
}

int try_spin_lock(lock)
     spin_lock_t lock;
{
  int old = *lock;
  *lock = FALSE;
  return old;
}

void spin_unlock(lock)
     spin_lock_t lock;
{ 
  *lock = TRUE;
}

#endif /* (MAX_PROCS > 1) */
