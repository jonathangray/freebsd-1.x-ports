/* callgc.c
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 */

#include "ml_os.h"
#include "ml_state.h"
#include "ml_types.h"
#include "tags.h"
#include "cause.h"
#include "request.h"
#include "sync.h"
#ifndef THINK_C
#include <sys/signal.h>
#else
#include <signal.h>
#endif

#define refcell(z)	\
    ML_val_t z[2] = {(ML_val_t)MAKE_DESC(1,TAG_array), INT_CtoML(0)};

refcell(collected0)
refcell(collectedfrom0)
refcell(times0)
refcell(current0)
refcell(gcmessages0)
refcell(majorcollections0)
refcell(minorcollections0)
refcell(pstruct0)
refcell(ratio0)
refcell(sighandler0)
refcell(softmax0)
refcell(lastratio0)

#define collected (collected0[1])
#define collectedfrom (collectedfrom0[1])
#define current (current0[1])
#define gcmessages (gcmessages0[1])
#define majorcollections (majorcollections0[1])
#define minorcollections (minorcollections0[1])
#define pstruct (pstruct0[1])
#define ratio (ratio0[1])
#define softmax (softmax0[1])
#define lastratio (lastratio0[1])

#define DEFAULT_CHUNK_SIZE (1024 * 512)               /* 1/2 Meg */
#define MIN_CHUNK_SIZE     (1024 * 128)               /* 1/8 Meg */
#define MAX_CHUNK_SIZE     (1024 * 1024)              /* 1 meg   */
#define DEFAULT_HEAP_SIZE ((MAX_PROCS) * 1024 * 1024) /* 1 meg per proc */

int chunk_size = DEFAULT_CHUNK_SIZE;
#if (MAX_PROCS > 1)
volatile int gcMaster = 0;
#else /* (MAX_PROCS == 1) */
int gcMaster = 0;
#endif
extern spin_lock_t MLproc_lock;
extern MLState_t *MLproc;
extern void block();
extern void unblock();

int		arenabase;               /* bottom of the heap */
int		arenasize = 0;           /* heap starts empty */
int		new_size = DEFAULT_HEAP_SIZE;
int		arstart;                 /* beginning of allocation arena */
int		arend;                   /* end of main arena, and the heap */
int		old_high;                /* marks end of persistent heap */
int		new_high;
int		new_new_high;
int		lastbreak;
int             shouldFlush1 = FALSE;
int             shouldFlush2 = FALSE;
int             flushFrom1, flushFrom2;
int             flushTo1, flushTo2;

static int	pagesize;

ML_val_t	store_preserve = INT_CtoML(0);
ML_val_t        empty_store_lists[1] = {0};
int		preserving = 0;

void callgc0 ();
static void callgc ();
static int **getmore_die ();
static int **getmore_must ();
#ifdef NeXT
static int brk(), sbrk();
#endif

/* divideAllocArea:
 * Divide the allocation area among the processor states.  Each non-running
 * proc state receives a minimum-size chunk (128k).  The rest of the area
 * is divided equally among the running proc states.  However, each proc's
 * area is further subdivided into chunks (between 128k and 512k in size).
 * The proc starts off with the first chunk in its area.
 *
 * When a proc faults, it tries to get another chunk from its own area.
 * If it fails, it tries to "steal" a chunk from another proc's area.
 * If this fails, it forces the other procs to do a GC and re-divide the
 * allocation area.
 */
void divideAllocArea(MLState, allocStart, allocLimit)
     MLState_ptr MLState;
     int allocStart, allocLimit;
{
  MLState_t *p;
  int i;
  int live_procs = 0;
  int procHeapSize;
  int next_chunk = allocStart & (~3);
  int max_chunk_size, min_chunk_size;

#if (MAX_PROCS > 1)
  max_chunk_size = ((allocLimit - allocStart) / MAX_PROCS) & (~3);
  max_chunk_size = (max_chunk_size > MAX_CHUNK_SIZE) ? MAX_CHUNK_SIZE :
    max_chunk_size;
  min_chunk_size = (max_chunk_size < MIN_CHUNK_SIZE) ? max_chunk_size :
    MIN_CHUNK_SIZE;
  chunk_size = (DEFAULT_CHUNK_SIZE < max_chunk_size) ? DEFAULT_CHUNK_SIZE :
    max_chunk_size;
  chunk_size = (chunk_size > min_chunk_size) ? chunk_size :
    min_chunk_size;
#else
  chunk_size = ((allocLimit - allocStart) / MAX_PROCS) & (~3);
  min_chunk_size = 0;  /* to avoid complaints about uninitialized vars */
#endif

  /* Give non-running procs only minimum-size chunk of allocation space */
  for (i = 0; i < MAX_PROCS; i++) {
    p = &(MLproc[i]);
    if (p->state != MLPROC_RUNNING) {
      p->ml_allocptr = next_chunk;
      p->alloc_boundary = next_chunk + min_chunk_size;
      p->ml_limitptr = p->alloc_boundary - 4096;
      p->max_allocptr = next_chunk;
      next_chunk += min_chunk_size;
    } else 
      live_procs++;
  }
  
  procHeapSize = ((allocLimit - next_chunk) / live_procs) & (~3);
  chunk_size = (procHeapSize < chunk_size) ? procHeapSize : chunk_size;
  
  /* give each proc an allocation area and a starting chunk within it */
  for (i=0; i < MAX_PROCS; i++) {
    p = &(MLproc[i]);
    if (p->state == MLPROC_RUNNING) {
      p->ml_allocptr = next_chunk;
      p->max_allocptr = next_chunk;
      p->alloc_boundary = next_chunk + procHeapSize;
      p->ml_limitptr = next_chunk + chunk_size - 4096;
      next_chunk += procHeapSize;
    }
  }

#ifdef MP_GC_DEBUG
    chatting("[alloc area: %x-%x, chunk_size = %x]\n",
	     allocStart, allocLimit, chunk_size);

  for (i=0; i < MAX_PROCS; i++) {
    p = &(MLproc[i]);
    chatting("[MLproc[%d]: alloc = %d, limit = %d,\n",
	     i, p->ml_allocptr,p->ml_limitptr);
    chatting("            (%dk), bound = %d (%dk)]\n",
	     (p->ml_limitptr - p->ml_allocptr)/1024,
	     p->alloc_boundary,
	     (p->alloc_boundary - p->ml_allocptr)/1024);
  }
#endif MP_GC_DEBUG

#ifdef MP_DEBUG
  /* sanity check */
  if (next_chunk > allocLimit) 
    die("next_chunk = %x, allocLimit = %x\n",next_chunk,allocLimit);
#endif MP_DEBUG
}


/* new_chunk:
 * Try to find a new chunk instead of forcing a GC sync -- return TRUE
 * if successful.
 */
int new_chunk(MLState)
     MLState_ptr MLState;
{
  MLState_t *p;
  int found_space = FALSE;
  int i;
  int local_chunk_size;
  int amount = MLState->amount + 4096;
  int cur_limitptr = MLState->ml_limitptr;
  int max_limitptr = MLState->alloc_boundary - 4096;
  
  local_chunk_size = (chunk_size < amount) ? amount : chunk_size;

  if ((cur_limitptr + local_chunk_size) > max_limitptr) {
  /* not enough space for a chunk, but maybe enough space for amount */
    if ((cur_limitptr + amount) > max_limitptr) {
    /* not enough space in our allocation area, so look for a chunk to steal */
      for (i=0; (i < MAX_PROCS) && (!found_space); i++) {
	p = &(MLproc[i]);
	if (p != MLState) {
	  if ((p->alloc_boundary - local_chunk_size) > 
	      (p->ml_limitptr + 4096)) {
	    /* we can steal a full chunk from p */
	    MLState->alloc_boundary = p->alloc_boundary;
	    MLState->ml_limitptr = p->alloc_boundary - 4096;
	    MLState->ml_allocptr = p->alloc_boundary - local_chunk_size;
	    p->alloc_boundary = MLState->ml_allocptr;
	    found_space = TRUE;
#ifdef MP_GC_DEBUG
	    chatting("[stole chunk from MLproc[%d]]\n",i);
#endif MP_GC_DEBUG

	  } else if ((p->alloc_boundary - amount) >
		     (p->ml_limitptr + 4096)) {
	    /* we can steal the amount needed from p */
	    MLState->alloc_boundary = p->alloc_boundary;
	    MLState->ml_limitptr = p->alloc_boundary - 4096;
	    MLState->ml_allocptr = p->alloc_boundary - amount;
	    p->alloc_boundary = MLState->ml_allocptr;
	    found_space = TRUE;
#ifdef MP_GC_DEBUG
	    chatting("[stole amount from MLproc[%d]]\n",i);
#endif MP_GC_DEBUG
	  }
	}
      }
    } else {
      /* enough space for amount -- just set limit to end of our area */
      MLState->ml_limitptr = max_limitptr;
      found_space = TRUE;

#ifdef MP_GC_DEBUG
      chatting("[found amount]\n");
#endif MP_GC_DEBUG

    }
  } else {
  /* enough space for a chunk */
    MLState->ml_limitptr = cur_limitptr + local_chunk_size;
    found_space = TRUE;

#ifdef MP_GC_DEBUG
    chatting("[found chunk]");
#endif MP_GC_DEBUG

  }

#ifdef MP_GC_DEBUG
  if (found_space)
    chatting("[alloc = %x, limit = %x, bound = %x]\n",
	     MLState->ml_allocptr, MLState->ml_limitptr, 
	     MLState->alloc_boundary);
#endif MP_GC_DEBUG

  return (found_space);
}


/* init_gc:
 */
void init_gc (MLState)
     MLState_ptr MLState;
{
    pagesize		 = getpagesize();
    arenabase		 = sbrk(0);
    lastbreak		 = arenabase;
    increase_heapsize();
    old_high		 = arenabase;
#ifdef CACHE_SIZE
    arstart              = arenabase+arenasize-CACHE_SIZE;
#else
    arstart              = ((arenabase+arenasize/2)+3)&(~3);
#endif
    collected		 = INT_CtoML(0);
    collectedfrom	 = INT_CtoML(0);
    minorcollections	 = INT_CtoML(0);
    majorcollections	 = INT_CtoML(0);

    divideAllocArea(MLState, arstart, arenabase+arenasize);
    lastratio            = INT_CtoML(0);
}

/* restart_gc:
 */
void restart_gc(MLState)
     MLState_ptr MLState;
{
    int		live_size = old_high - arenabase;
    int		a = 0;
    ML_val_t	x = gcmessages;

    resettimers();
    lastbreak = EDATA;
#ifdef THINK_C
    gcmessages = INT_CtoML(2);
#else
    gcmessages = INT_CtoML(0);
#endif
    new_size = compute_new_size(live_size);
    do {
	increase_heapsize();
	if (arenasize == a)
	    die("Can't get enough memory to start ML\n");
	a = arenasize;
    } while (arenasize < 3*live_size);
    gcmessages = x;
    lastratio = INT_CtoML(arenasize/(live_size/100));
#ifdef ADVICE
    ostime=zero; otime=zero; ogtime=zero;
    getting_advice=1;
    initadvice();
#endif
    divideAllocArea(MLState, arstart, lastbreak);
} /* end of restart_gc */


/* check_heap:
 * Check the heap to insure that there is a sufficient amount of available
 * memory in the allocation arena.  If not, then do a garbage collection and
 * return 1, otherwise return 0.
 * NOTE: if a garbage collection is done, then any roots in C variables (other
 * than the ML state vector) are obsolete.
 */
int check_heap (MLState,amount)
    MLState_ptr MLState;
    int		amount;
{
    register int    top = MLState->ml_limitptr;

    if ((MLState->ml_allocptr + amount) >= top) {
	if (gcmessages >= INT_CtoML(3))
	    chatting("[check_heap: %d bytes available, %d required]\n",
		(top + 4096) - MLState->ml_allocptr, amount+4096);

	callgc0 (MLState, CAUSE_GC, amount);
	return 1;
    }
    else
	return 0;

} /* end of check_heap */


/* collect_roots:
 * Collect all of the roots from the active processors (and their storelists)
 * and pass them off to callgc.
 */
void collect_roots (MLState, cause)
    MLState_ptr MLState;
    int		cause;
{
    int		i;
    int         *roots[((NROOTS+5)*MAX_PROCS)+5];
    int         mask, j;
    MLState_t   *p;
    ML_val_t    store_lists[MAX_PROCS+1];
    ML_val_t    *storeptr = store_lists;
    int         max_allocptr = MLState->ml_allocptr;
    int		**rootsptr = roots;
    int         total_amount = 0;
    ML_val_t	currentsave = current;

    current = INT_CtoML(2);

    start_gc_timer();
    *rootsptr++ = (int *) (times0+1);
    *rootsptr++ = (int *) &pstruct;
    *rootsptr++ = (int *) &store_preserve;
    *rootsptr++ = (int *) &(sighandler0[1]);

    for (j=0; j < MAX_PROCS; j++) {
      p = &(MLproc[j]);
      /* add running proc's roots */
      if (p->state == MLPROC_RUNNING) {
#ifdef MP_DEBUG
	pchatting(MLState,"[adding %d's roots]\n",p->self);
#endif MP_DEBUG
	*rootsptr++ = (int *) &(p->ml_pc);
	*rootsptr++ = (int *) &(p->ml_exncont);
	*rootsptr++ = (int *) &(p->ml_varptr);
#ifdef BASE_INDX
	*rootsptr++ = (int *) &(p->ml_baseptr);
#endif
#ifdef GLOBAL_INDX
	*rootsptr++ = (int *) &(p->ml_globalptr);
#endif
/*	chatting("\n[pc 0x%x, 0x%x]",p->ml_pc,p->mask); */
	mask = p->mask;
	for (i = 0;  mask != 0;  i++, mask >>= 1) {
	  if ((mask & 1) != 0)
	    *rootsptr++ = (int *)&(p->ml_roots[ArgRegMap[i]]);
	}
	total_amount += p->amount;
      }
      max_allocptr = (p->max_allocptr > max_allocptr) ? p->max_allocptr :
	max_allocptr;
      /* add the proc's store list to the vector of store lists */
      if (p->ml_storeptr != (int)STORLST_nil) {
	*storeptr++ = (ML_val_t)p->ml_storeptr;
	p->ml_storeptr = (int)STORLST_nil;
      }
    }
    *rootsptr = 0;
    *storeptr = 0;
#ifdef MP_DEBUG
    pchatting(MLState,"[callgc]\n");
#endif MP_DEBUG
    shouldFlush1 = FALSE;
    shouldFlush2 = FALSE;
    callgc(MLState, cause, roots, &max_allocptr, store_lists, total_amount);
    divideAllocArea(MLState, max_allocptr, arend);
    current = currentsave;

    stop_gc_timer();

} /* end of collect_roots */


#if (MAX_PROCS > 1)
/* Force other procs to synchronize for GC */
void
gcMaster_sync(MLState, cause)
     MLState_ptr MLState;
     int cause;
{
  int i;
  MLState_t *p;
  int live_procs = 0;

  /* send a GC sync signal to other running procs */
  for (i=0; i < MAX_PROCS; i++) {
    p = &(MLproc[i]);
    if ((p->self != MLState->self) && (p->state == MLPROC_RUNNING)) {
      p->GCpending = TRUE;
#ifdef MP_DEBUG
      pchatting(MLState,"[signalling %d]\n",p->self);
#endif MP_DEBUG
      signalproc(p->self);
      live_procs++;
    }
  }
  /* wait for others to check in */
  for (i=0; i < live_procs; i++)
    block(MLState->self);
  /* all have checked in -- do the gc.  At this point, all other
     procs are blocked. */
#ifdef MP_DEBUG
  pchatting(MLState,"[gc sync complete]\n");
#endif MP_DEBUG
  collect_roots(MLState, cause);
  /* wake others up for cache flush */
  for (i=0; i < MAX_PROCS; i++) {
    p = &(MLproc[i]);
    if ((p->self != MLState->self) && (p->state == MLPROC_RUNNING)) {
      p->GCpending = FALSE;
#ifdef MP_DEBUG
      pchatting(MLState,"[waking %d]\n",p->self);
#endif MP_DEBUG
      unblock(p->self);
    }
  }
  for (i=0; i < live_procs; i++)
    block(MLState->self);
#ifdef MP_DEBUG
  pchatting(MLState,"[master resuming.]\n");
#endif MP_DEBUG
} /* end gcMaster_sync */


/* synchronize with the GC master and wait for it to finish GC. */
void gcSlave_sync(MLState)
     MLState_ptr MLState;
{
#ifdef MP_DEBUG
  pchatting(MLState,"[syncing with master.]\n");
#endif MP_DEBUG
  unblock(gcMaster);
  /* Wait until master wakes us -- GC will be done at this point */
  block(MLState->self);
  /* Flush i-cache if necessary and inform Master when through */
  if (shouldFlush1) 
    FlushICache(flushFrom1, flushTo1);
  if (shouldFlush2)
    FlushICache(flushFrom2, flushTo2);
  unblock(gcMaster);
#ifdef MP_DEBUG
  pchatting(MLState,"[slave resuming.]\n");
#endif MP_DEBUG
} /* end gcSlave_sync */


void
callgc0 (MLState, cause, amount)
     MLState_ptr MLState;
     int cause, mask;
{
  extern int should_exit;

  if (should_exit) 
    mp_shutdown(MLState, 0);

  if (MLState->max_allocptr < MLState->ml_allocptr)
    MLState->max_allocptr = MLState->ml_allocptr;
  MLState->amount = amount;

  /* Try to grab the MLproc_lock, but check for other pending GC's */
#ifdef MP_DEBUG
  pchatting(MLState,"[entered callgc0]\n");
#endif MP_DEBUG
  while ((!try_spin_lock(MLproc_lock)) &&
	 (!MLState->GCpending)) /* spin */ ;

  if (MLState->GCpending) {
    /* We failed to acquire the lock, but a GC master has set our GCpending
       flag, so synchronize with the master. */
    gcSlave_sync(MLState);
  } else {
  /* We succeeded in acquiring the lock, so we have control over GC.
     If cause != CAUSE_GC, then we have to do a synch.  Otherwise,
     try to just get a new chunk before resorting to a synch. */

    if ((cause != CAUSE_GC) || (! new_chunk(MLState))) {
#ifdef MP_DEBUG
      pchatting(MLState,"[setting self to master.]\n");
#endif MP_DEBUG
      gcMaster = MLState->self;
      gcMaster_sync(MLState, cause);
      gcMaster = 0;
    }
#ifdef MP_DEBUG
    pchatting(MLState,"[releasing lock]\n");
#endif MP_DEBUG
    spin_unlock(MLproc_lock);
  }
}
#else /* (MAX_PROCS <= 1) */
void
callgc0 (MLState, cause, amount)
     MLState_ptr MLState;
     int cause;
{
  if (MLState->max_allocptr < MLState->ml_allocptr)
    MLState->max_allocptr = MLState->ml_allocptr;
  MLState->amount = amount;
  collect_roots(MLState, cause);
}
#endif MAX_PROCS

/* callgc:
 */
static void callgc (MLState, cause, misc_roots, arptr, store_lists, amount)
    MLState_ptr MLState;
    int		cause;		/* the reason for doing GC */
    int		***misc_roots;	/* vector of ptrs to extra root words */
    int		*arptr;		/* place to put new freespace pointer */
    ML_val_t	*store_lists;	/* vector of list of refs stored into */
    int         amount;         /* amount of space requested */
{
    int amount_desired;

    arend = arenabase+arenasize;
    if (cause == CAUSE_GC)
        amount_desired = amount;
    else if (cause == CAUSE_BLAST)
 	amount_desired = 4 + arend - (*arptr);
    else
	amount_desired = 0;
    if (arstart == *arptr)
	new_high = old_high; /* no minor needed */
    else  {
	if (gcmessages >= INT_CtoML(3))
	    chatting("\n[Minor collection...");
	gc (MLState,
	    arstart, arend,
	    old_high,arstart,
	    old_high,
	    &new_high,
	    misc_roots,store_lists,
	    &shouldFlush1,
	    getmore_die, 0);
	{
	    int a = new_high-old_high, b =(*arptr)-arstart;
	    if (gcmessages >= INT_CtoML(3)) {
              int d = (b+50)/100;
              int p = d > 0 ? a/d : 0;
		chatting(" %d%% used (%d/%d), %d msec]\n",
                  p, a, b, check_gc_timer());
            }
	    collected = INT_incr(collected, (a+512)/1024); /* round to nearest K */
	    collectedfrom = INT_incr(collectedfrom, (b+512)/1024);
	    minorcollections = INT_incr(minorcollections, 1);
	}

#ifdef GCMON
    gcmonMinor(arstart,*arptr,old_high,new_high);
#endif
  /* flush i-cache from old_high to new_high -- save values so other procs
     can flush their caches. */

	shouldFlush1 = TRUE;

    if (shouldFlush1) {
      flushFrom1 = old_high;
      flushTo1   = new_high - old_high;
      FlushICache(flushFrom1, flushTo1);
    }

#ifdef GCDEBUG
	checkup (MLState, arstart, new_high);
	clear (new_high, arenabase+arenasize);
#endif
    }

    {
	int need_major = 0;
	int was_preserving;
	int gamma = INT_MLtoC(ratio);

	if (gamma < 3) gamma = 3;

	if ((cause == CAUSE_EXPORT) || (cause == CAUSE_BLAST) || (cause == CAUSE_MAJOR))
	    need_major = 1;
	else {
	    int cut = arenasize-arenasize/gamma;
	    int max = INT_MLtoC(softmax);
	    int halfmax = max/2;
	    int halfsize = arenasize/2;
	    cut = (cut<halfmax ? cut : halfmax);
	    cut = (cut>halfsize ? cut : halfsize);
	    if (new_high+amount_desired > arenabase+cut)
		need_major = 1;
	    else {
		int live_size = amount_desired+new_high-old_high;
#ifdef ADVICE
		if (((arenabase+arenasize-new_high)/2 <= amount_desired*3+100)
		|| (minorcollections > old_minorcount+200))
		    need_major = 1;
#else
		new_size = compute_new_size(live_size);
		if (new_size > arenasize
		&& (increase_heapsize()-new_high)/2 <= amount_desired)
		    need_major = 1;
#endif ADVICE
	     /*	lastratio = INT_CtoML(arenasize/(live_size/100)); */
	   }
	}
	if (cause == CAUSE_BLAST)
	    old_high = new_high;
	if (need_major) {
	    int		msec0;
	    if (gcmessages >= INT_CtoML(1)) {
		chatting("\n[Major collection...");
		msec0 = check_gc_timer();
	    }
	    was_preserving=preserving; preserving=0;
	    if (gc(MLState,
		   arenabase, old_high,
		   old_high, arenabase+arenasize,
		   new_high,
		   &new_new_high,
		   misc_roots, empty_store_lists,
		   &shouldFlush2,
		   getmore_must, (cause == CAUSE_BLAST) ? &(MLState->ml_arg) : 0))
	    {
#ifdef GCMON
		gcmonMajor (arenabase, old_high, old_high, new_new_high);
#endif
		moveback (old_high, new_new_high, arenabase, misc_roots);
	      /* flush i-cache from arenabase to arenabase+new_new_high-old_high if necessary */

		/* jgm: looks like the hooks I put in to gc.c don't catch
		 * all code string movements -- doing major collections
		 * repeatedly while another processor is running/allocating 
		 * causes a segmentation violation that goes away when we
		 * always do a cache flush.
		 */
		shouldFlush2 = TRUE;

		if (shouldFlush2) {
		  flushFrom2 = arenabase;
		  flushTo2   = new_new_high-old_high;
		  FlushICache (flushFrom2, flushTo2);
		}
		{
		    int a = new_new_high-new_high, b = new_high-arenabase;
                  if (gcmessages >= INT_CtoML(1)) {
                    int d = (b+50)/100;
                    int p = d > 0 ? a/d : 0;
                    chatting(" %d%% used (%d/%d), %d msec]\n",
                          p, a, b, check_gc_timer()-msec0);
                  }
                  collected = INT_incr(collected,(a+512)/1024);
                  collectedfrom = INT_incr(collectedfrom,(b+512)/1024);
		  majorcollections = INT_incr(majorcollections,1);
		}
		{
		    int live_size = amount_desired+new_new_high-old_high;
		    old_high = arenabase+new_new_high-old_high;
#ifdef ADVICE
		    new_size = ask_new_size(live_size);
#else
		    new_size = compute_new_size(live_size);
#endif
		    if (new_size > arenasize) {
			int end = increase_heapsize();
			if ((end-old_high)/2 <= amount_desired)
			    die("\nRan out of memory\n");
		    }
#ifdef ADVICE
		    else if (new_size < arenasize)
			decrease_heapsize();
		    old_size = arenasize;
		    old_minorcount = minorcollections;
#else
		    else if (new_size < (arenasize/4)*3)
			decrease_heapsize();
#endif
		    lastratio = INT_CtoML(arenasize/(live_size/100));
		}
	    }
	    else {
		if (gcmessages >= INT_CtoML(1))
		    chatting("abandoned]\n");
	    }
 	    preserving=was_preserving;
	}
	else
	    old_high=new_high;
    }
    arend = arenabase+arenasize;
#ifdef HPPA
    /* on the HP the high bits of pointers have segment bits.
       we can't add them without overflow. */
    arstart = (arend/2 + old_high/2 + 3) & (~3);
#else
    arstart = (((arend+old_high)/2)+3)&(~3);
#endif
    (*arptr) = arstart;

} /* end of callgc */


/* getmore_die:
 */
static int **getmore_die ()
{
    die("bug: insufficient to_space\n");
}

int amount_desired;

/* decrease_heapsize:
 */
int decrease_heapsize ()
{
    int		p = arenabase+new_size;
    p = (p + pagesize-1 ) & ~(pagesize-1);
    if (p < lastbreak) {
	brk(p);
	arenasize = p-arenabase;
	if (gcmessages >= INT_CtoML(2))
	    chatting ("\n[Decreasing heap to %dk]\n",arenasize/1024);
	lastbreak = p;
    }
    return lastbreak;
}

/* increase_heapsize:
 * Assume that new_size > arenasize.
 */
int increase_heapsize ()
{
    int		p = arenabase+new_size;

  RESTART:;
    p = (p + pagesize-1 ) & ~(pagesize-1);
    if (p == lastbreak) {
	if (gcmessages >= INT_CtoML(2))
	    chatting("\nWarning: can't increase heap\n");
	return p;
    }
    else if (brk(p)) {
	if (gcmessages >= INT_CtoML(3))
	    chatting("\nWarning: must reduce heap request\n");
	p = (lastbreak+(p-pagesize))/2;
	goto RESTART;
    }
    else {
	lastbreak=p;
	arenasize = p-arenabase;
	if (gcmessages >= INT_CtoML(2))
            chatting("\n[Increasing heap to %dk]\n",arenasize/1024);
        return p;
    }
}

int compute_new_size (live_size) 
    int		live_size;
{
    int		new_size;
    int		gamma = INT_MLtoC(ratio);
    int		max = INT_MLtoC(softmax);

    if (gamma < 3)
	gamma = 3;
    if (2000000000 / gamma < live_size)
	new_size = 2000000000;
    else
	new_size = live_size*gamma;
    if (max < new_size) {
	int new = 3*live_size;
	new_size = ((new > max) ? new : max);
    }
    return new_size;
}

/* getmore_must:
 */
static int **getmore_must ()
{
    int		oldsize = arenasize;
    int		live_size = amount_desired+arenasize+arenabase-old_high;
    int		r;

    new_size = compute_new_size(live_size);
    r = increase_heapsize();
#ifdef ADVICE
    while (oldsize == arenasize) {
	chatting ("\nCan't get more memory; waiting\n");
	sleep (10);
	chatting("Trying again\n");
	r = increase_heapsize();
    }
#else
    if (oldsize == arenasize)
	die("\nRan out of memory");
#endif
    return (int **)r;
} /* end of getmore_must */


#ifdef GETSTORELIST
/* uniq:
 * THIS COULD BE PUT IN ml_getstorelist (in cfuns.c).
 */
ML_val_t uniq (arg)
    ML_val_t	arg;
{
    register ML_val_t *p, q;

    for (q = arg;  q != STORLST_nil;  q = STORLST_next(q)) {
	if (STORLST_index(q) == -1) {
	    (PTR_MLtoC(q))[-1] = STORLST_objdesc(q);
	    (PTR_MLtoC(STORLST_obj(q)))[-1] = 0;
	}
    }

    for (q = arg;  q != INT_CtoML(0);  q = REC_SEL(q, 2)) {
	if ((STORLST_objdesc(q) != 0)
	&& (STORLST_index(q) >= 0)
	&& (STORLST_index(STORLST_obj(q)) != 0)) {
	    (PTR_MLtoC(q))[-1] = STORLST_index(STORLST_obj(q));
	    (PTR_MLtoC(STORLST_obj(q)))[1] = 0;
	}
    }

    for (p = &arg;  *p != STORLST_nil;  p = &(STORLST_next(*p))) {
	if ((PTR_MLtoC(STORLST_obj(*p)))[1] == 0) {
	    (PTR_MLtoC(STORLST_obj(*p)))[STORLST_index(*p)] = OBJ_DESC(*p);
	    (PTR_MLtoC(*p))[-1] = MAKE_DESC(3, TAG_record);
	}
    }

    return arg;

} /* end of uniq */
#endif GETSTORELIST


#ifdef NeXT
/**
 ** This implements sbrk/brk using vm_allocate/vm_deallocate.
 ** Arguments are assumed to be page multiples, and the argument
 ** to brk is assumed to be just after the desired breakpoint.
 **
 ** No relationship between the mapped region and the rest of the
 ** process image is guaranteed, but it is expected that the region
 ** will follow the end of data/bss.
 **
 ** Works with NeXT Mach (software release 0.9).  5/15/89
 **
 ** James William O'Toole Jr.
 **
 **/

#include <c.h>			/* TRUE and FALSE */
#ifdef NeXT_3_0
#include <mach/kern_return.h>	/* KERN_whatever */
#include <mach/mach.h>
#else
#include <sys/kern_return.h>	/* KERN_whatever */
#include <mach.h>
#endif

extern vm_task_t task_self_;

int mach_sbrk_needsinit = TRUE;
int mach_maplimit = 0;
int mach_brkpt = 0;
int mach_quant = 0x800000;
int big_heap = 0x2000000;

static int sbrk(incr)
    int incr;
{
    if (incr)
	die("sbrk called with nonzero value");
    if (mach_sbrk_needsinit != FALSE) {
	if (vm_allocate(task_self_, &mach_brkpt, big_heap, TRUE) != KERN_SUCCESS)
	    die("vm_allocate failed");
	mach_maplimit = mach_brkpt + big_heap;
	mach_sbrk_needsinit = FALSE;
    }
    return(mach_brkpt);
}

static int brk(pos)
    int pos;
{
    if (pos > mach_maplimit)
	return KERN_FAILURE;
    else
	return KERN_SUCCESS;
}

#endif NeXT


/** GCDEBUG routines **/

#ifdef GCDEBUG
clear(low,high) int *low, *high;
{int *i;
 chatting("clearing new space...  ");
 for(i=low; i<high; i++) *i=0;
 chatting("done\n");
}

int *descriptor;
checkup (MLState, low, high)
    MLState_ptr MLState;
    int *low,*high;
{int *i,*j;
 chatting("checking to_space...  ");
 i = low;
 while (i < high) {
   descriptor = i;
   switch(OBJ_TAG(i)) {
        case TAG_backptr:
		chatting("Uncool backpointer at %#x in to_space\n",i);
		mp_shutdown(MLState,0);
		break;
	case TAG_emb_string: case TAG_emb_reald:
		chatting("Uncool embedded tag at %#x in to_space\n",i);
		mp_shutdown(MLState,0);
		break;
	case TAG_string:
	case TAG_bytearray:
		i += (OBJ_LEN(i)+7)>>2;
		break;
	case TAG_reald:
		i += 3;
		break;
	case TAG_realdarray:
		i += (OBJ_LEN(i)<<1)+1;
		break;
	case TAG_record:
	case TAG_pair:
	case TAG_array:
		j = i + OBJ_LEN(i) + 1;
		while(++i < j) {
		    if (! OBJ_isBOXED(*i))
			continue;
		    else if ((int*)*i > high) {
			chatting("Uncool pointer %#x at %#x\n", *i,i);
			chatting("Descriptor is at %#x\n",descriptor);
		    }
		    else if ((int*)*i < low) {
			chatting("Uncool pointer %#x at %#x\n", *i,i);
			chatting("Descriptor is at %#x\n",descriptor);
		    }
		}
		break;
	case TAG_forwarded:
		chatting("Uncool forwarding tag at %#x in to_space\n",i);
		mp_shutdown(MLState,0);
		break;
	default: /* function pointers */
		chatting("Unexpected even tag %d at %#x in to_space\n",
			 OBJ_LEN(i),i);
		mp_shutdown(MLState,0);
		break;
	      }
 }
 chatting("done\n");
}
#endif	/* GCDEBUG */
