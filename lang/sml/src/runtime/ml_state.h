/* ml_state.h
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * This defines the layout of the ML state vector.  The ML state consists of
 * the current allocation pointer (ml_allocptr), the heap limit pointer (ml_limitptr),
 * the store list pointer (ML_storeptr), and a set of root registers (ml_roots[]).
 * There are six or more distinguished root registers, which are arranged in a machine
 * dependent fashion.
 */

#ifndef _ML_STATE_
#define _ML_STATE_
#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif TRUE
#include "mask.h"
#include <setjmp.h>
#include "ml_signal.h"
#include "mp.h"

/* NROOTS gives the size of the variable-size portion (roots[]) of the
 * ML state vector. Note that the name "roots" is slightly misleading;
 * while every entry in the vector must be saved over calls to C, not 
 * every entry is a valid root on every entry to C.  
 * The valididity of most entries is indicated using the register map
 * convention (via ArgRegMap); these entries are valid (and live)
 * iff the corresponding bit in the register mask is set (see 
 * cps/generic.sml).
 * N_ARG_REGS gives the number of such entries.
 * The pc, exncont, varptr, and baseptr (if defined) are 
 * always valid roots, and the icounter (if defined) never is.
 */

#ifndef C
#if defined(RS6000)
#   define NROOTS	    24	       /* r6-r28,unused_slot */
#   define N_ARG_REGS       19	       /* (misc regs + 4) */
#   define LINK_INDX        0
#   define CLOSURE_INDX	    1
#   define ARG_INDX	    2
#   define CONT_INDX	    3
#   define EXN_INDX	    4
#   define VAR_INDX         5
#   define BASE_INDX	    6
#   define PC_INDX	    8
#endif

#if defined(SPARC)
#   define NROOTS	    21		
#   define N_ARG_REGS       17          /* exclude baseptr */
#   define PC_INDX	    0
#   define EXN_INDX	    7
#   define ARG_INDX	    1
#   define CONT_INDX	    2
#   define CLOSURE_INDX	    3
#   define BASE_INDX	    4
#   define VAR_INDX         6
#   define LINK_INDX        5
#endif

#if defined(MIPS)
#   define NROOTS	    21		/* $2-$4, $30, pc, $5-$18,$20,$24 */
#   define N_ARG_REGS       17          /* see run_ml.c */
#   define PC_INDX	    4
#   define EXN_INDX	    3		/* $30 */
#   define ARG_INDX	    0		/* $2 */
#   define CONT_INDX	    1		/* $3 */
#   define CLOSURE_INDX	    2		/* $4 */
#   define VAR_INDX         19          /* $20 */
#   define BASE_INDX	    20          /* $24 */
#   define LINK_INDX        5
/* #   define ICOUNT_INDX   ??    */        
/* #   define ICOUNT           1   */
#endif	

#if defined(HPPA)
#   define NROOTS	    21		
#   define N_ARG_REGS       16          /* 4 + length miscregs */
#   define PC_INDX	    0		/* = gclink */
#   define EXN_INDX	    7
#   define ARG_INDX	    1
#   define CONT_INDX	    2
#   define CLOSURE_INDX	    3
#   define BASE_INDX	    4
#   define VAR_INDX         6
#   define LINK_INDX        5		/* = stdlink */
#endif

#if defined(M68)
#   define NROOTS	    8		/* d7, a0-a4, d3, pc */
#   define N_ARG_REGS       5
#   define PC_INDX	    7
#   define EXN_INDX	    0		/* d7 */
#   define ARG_INDX	    1		/* a0 */
#   define CONT_INDX	    2		/* a1 */
#   define CLOSURE_INDX	    3		/* a2 */
#   define VAR_INDX         6           /* d3 */
#   define LINK_INDX        4
#endif

#if defined(VAX)
#   define NROOTS	    12		/* r0-r7, r10, r13, pc (r15),varptr */
#   define N_ARG_REGS       9
#   define PC_INDX	    10		/* a.k.a. r15 */
#   define EXN_INDX	    0		/* r13 */
#   define ARG_INDX	    1		/* r0 */
#   define CONT_INDX	    2		/* r1 */
#   define CLOSURE_INDX	    3		/* r2 */
#   define VAR_INDX         11          /* varptr */
#   define LINK_INDX        4
#endif

#  if defined(NS32)
#   define NROOTS
#   define N_ARG_REGS       ?
#   define PC_INDX	    ?
#   define EXN_INDX	    ?		/* ?? */
#   define ARG_INDX	    ?		/* r0 */
#   define CONT_INDX	    ?		/* r1 */
#   define CLOSURE_INDX	    ?		/* r2 */
#endif

#if defined(I386)
#   define NROOTS	    10
#   define N_ARG_REGS	    7
#   define EXN_INDX	    0		/* 8(esp) */
#   define ARG_INDX	    1		/* ebp	   */
#   define CONT_INDX	    2		/* esi	   */
#   define CLOSURE_INDX	    3		/* 16(esp) */
#   define VAR_INDX	    4		/* 28(esp) */ 
#   define LINK_INDX	    5		/* 20(esp) */
#   define PC_INDX	    6		/* eip	   */
/*  define MISC1	    7 */	/* ebx	   */
/*  define MISC2	    8 */	/* ecx	   */
/*  define MISC3	    9 */	/* edx	   */
#endif

#else /* !C */
#   define NROOTS          27          /* r5-r31 */
#   define N_ARG_REGS      25
#   define EXN_INDX        0           /* r5 */
#   define PC_INDX         1           /* r6 */
#   define CLOSURE_INDX    2           /* r7 */
#   define ARG_INDX        3           /* r8 */
#   define CONT_INDX       4           /* r9 */
#   define LINK_INDX       5
#endif /* !C */


/* All ML values are represented by a (32-bit) word.  A value is either a tagged
 * integer (unboxed), or a pointer to a heap object (boxed).
 */
typedef unsigned int *ML_val_t;

typedef struct {
    int		ml_allocptr;		/* the pointer to the next word to allocate */
    int		ml_limitptr;		/* the heap limit pointer */
    int		ml_storeptr;		/* the list of store operations */
    ML_val_t	ml_roots[NROOTS];	/* the root registers */
#   define      ml_linkreg      ml_roots[LINK_INDX]
#   define	ml_pc		ml_roots[PC_INDX]
#   define	ml_exncont	ml_roots[EXN_INDX]
#   define	ml_arg		ml_roots[ARG_INDX]
#   define	ml_cont		ml_roots[CONT_INDX]
#   define	ml_closure	ml_roots[CLOSURE_INDX]
#ifdef VAR_INDX
#   define      ml_varptr       ml_roots[VAR_INDX]
#endif
#ifdef BASE_INDX
#   define	ml_baseptr	ml_roots[BASE_INDX]
#endif
#ifdef ICOUNT_INDX
#   define      ml_icount       ml_roots[ICOUNT_INDX]
#endif
#ifdef I386
    int		tempmem;	      /* See I386.prim.s */
    int		tempmem2;
#endif
    int         inML;                 /* in ML or C? */
    int         request;              /* runtime service request code */
    int         handlerPending;       /* signal handler pending? */
    int         inSigHandler;         /* in a signal handler?    */
    int         maskSignals;          /* signals masked?         */
    int         NumPendingSigs;       /* number of pending signals */
    int         ioWaitFlag;           /* waiting for an IO operation? */
    int         GCpending;            /* indicates if GC is pending */
#if defined(I386) 
    int*        MLframe;              /* base address of ML stack frame */
#else
#if (!defined(MIPS) && !defined(C))   /* see lg */
    int         unused;
#endif
#endif
    int         self;                 /* proc id */
    int         state;                /* RUNNING, SUSPENDED, or NO_PROC */
    int         alloc_boundary;       /* end of arena for this proc */
    int         max_allocptr;         /* max value of allocptr for this proc */
    int         mask;                 /* live reg mask -- used in gc */
    int         amount;               /* amount of space requested in gc */
    int         SigCode;              /* last signal that hit         */
    int         SigCount;             /* number of times signal has hit */
    int         SigTbl[NUM_ML_SIGS];  /* status of the ML signals */
#ifdef SOLARIS
    sigjmp_buf  SysCallEnv;           /* used to lngjmp out of IO operations */
#else
    jmp_buf     SysCallEnv;           /* used to lngjmp out of IO operations */
#endif SOLARIS
    ML_val_t    fault_exn;
#ifdef SGI
    int         lock_base;            /* base of lock allocation arena */
    int         lock_top;             /* top of lock allocation arena */
    int         lock_ptr;             /* next lock to return on allocation */
#endif SGI
} MLState_t;

typedef MLState_t *MLState_ptr;

#define ALL_ARGS_MASK	((1 << N_ARG_REGS)-1)
#define CONT_ARGS_MASK	contmask

/* This table maps the register numbers of the argument registers used by the
 * code generator to the proper indices of the root vector.  The standard closure
 * is in slot-0, the standard arg in slot-1 and the standard continuation is in
 * slot-2.
 */
extern int	ArgRegMap[N_ARG_REGS];

/* Table is defined in ml_run.c */

#endif /* !_ML_STATE_ */
