/* SPARC.prim.s
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * AUTHOR:  John Reppy
 *	    Cornell University
 *	    Ithaca, NY 14853
 *	    jhr@cs.cornell.edu
 */

#ifdef SOLARIS
# include <sys/asm_linkage.h>
# include <sys/trap.h>
#else
# include <machine/asm_linkage.h>
# include <machine/trap.h>
#endif
#include "tags.h"
#include "request.h"
#include "mask.h"

#ifdef __STDC__
#define CONCAT(ex,suffix)	ex ## suffix
#else
#define CONCAT(ex,suffix)	ex/**/suffix
#endif

#ifdef SOLARIS
# define XID(id)	id
#else
# define XID(id)	CONCAT(_,id)
#endif

/* SPARC runtime code for ML.  Registers are used as follows:  */

/* IMPORTANT NOTE:
	many of these registers are in "register pairs",  which is used
	to advantage when loading and storing them from/to the ML State.
	Beware if you re-arrange these register assignments 
	(look for ldd and std instructions).  The same goes for the
	offsets in the ML state vector */

/* Note: don't add spaces before the register names in theses
 *	 macro's.
 */
#define exncont g7   /* exception handler (ml_exncont)	*/
#define allocptr g6  /* freespace pointer (ml_allocptr) */
#define storeptr g5  /* store pointer     (ml_storeptr) */
#define limit g4     /* heap limit pointer (ml_limitptr)*/
#define stdarg i0    /* standard argument (ml_arg)  	*/
#define stdcont i1   /* standard continuation (ml_cont) */
#define stdclos i2   /* standard closure  (ml_clos)	*/
#define baseptr i3   /* base code pointer (ml_roots[])  */
#define varptr i5    /* var pointer       (ml_varptr)   */
#define stdlink g1
#define miscreg0 g2  /* miscellaneous registers (ml_roots[]) */
#define miscreg1 g3  /* the first few of these may be callee-save */
#define miscreg2 o0
#define miscreg3 o1
#define miscreg4 l0
#define miscreg5 l1
#define miscreg6 l2
#define miscreg7 l3
#define miscreg8 l4
#define miscreg9 l5
#define miscreg10 l6
#define miscreg11 l7
#define miscreg12 i4
#define tmp1 o2
#define tmp2 o3
#define tmp3 o4
#define tmp4 o5      /* also used to pass register mask to g.c. */
#define gclink o7    /* link register for return from g.c.  (ml_pc) */

/* %o2 and %o3 are also used as for multiply and divide */

/*
 * %o6 = %sp (not used by ML)
 * %i6 = %fp (not used by ML)
 * %i7 = return address to C code (not used by ML)
*/


/* offsets in the MLState structure */

#define allocptr_offset	 0
#define limit_offset	 4
#define storeptr_offset	 8
#define gclink_offset	12
#define stdarg_offset	16
#define stdcont_offset	20
#define stdclos_offset	24
#define baseptr_offset	28
#define stdlink_offset  32
#define varptr_offset	36
#define exncont_offset	40
#define miscreg12_offset 44
#define miscreg0_offset	48
#define miscreg1_offset	52
#define miscreg2_offset	56
#define miscreg3_offset	60
#define miscreg4_offset	64
#define miscreg5_offset	68
#define miscreg6_offset	72
#define miscreg7_offset	76
#define miscreg8_offset	80
#define miscreg9_offset	84
#define miscreg10_offset 88
#define miscreg11_offset 92
#define inML 96
#define request 100
#define handlerPending 104
#define inSigHandler 108
#define maskSignals 112
#define NumPendingSigs 116
#define ioWaitFlag 120
#define GCpending 124
#define saved_pc 128
#define mask_ 148


/* The ML stack frame has the following layout (set up by restoreregs):
 *
 *			+-----------------+
 *	%fp = %sp+94:	| empty           |
 *			+-----------------+
 *	%sp+92:		| temp for cvti2d |
 *			+-----------------+
 *	%sp+88:		| addr of _startgc|
 *			+-----------------+
 *	%sp+84:		| ptr to MLState  |
 *			+-----------------+
 *	%sp+80:		| temp for floor  |
 *			+-----------------+
 *	%sp+76:		| addr of _ml_div |
 *			+-----------------+
 *	%sp+72:		| addr of _ml_mul |
 *			+-----------------+
 *	%sp+68:		|    saved %g6    |
 *			+-----------------+
 *	%sp+64:		|    saved %g7    |
 *			+-----------------+
 *			|  space to save  |
 *			|  in and local   |
 *	%sp:		|    registers    |
 *			+-----------------+
 *
 * The size of the frame is
 */
#define ML_FRAMESIZE (WINDOWSIZE+36)
#define MLSTATE_OFFSET 84
#define STARTGC_OFFSET 88


#define ML_CODE_HDR(name)			\
	    .global name;			\
	    .align  4;				\
	    .word   TAG_backptr;		\
    name:


#if (CALLEESAVE > 0)
#define CONTINUE				\
            jmp     %stdcont;			\
            subcc   %allocptr,%limit,%g0
#else
#define CONTINUE				\
	    ld	    [%stdcont],%stdlink;	\
	    jmp	    %stdlink;			\
            subcc   %allocptr,%limit,%g0
#endif

#define CHECKLIMIT(mask)		\
	    1: ble  2f;			\
	       nop;			\
	       set mask,%tmp4;		\
 	       set 1b,%gclink;		\
	       ba   XID(saveregs);		\
	       nop;			\
	    2:
	       
	.seg	"text"

	.global XID(request_fault), XID(restoreregs), XID(sigh_resume)

/* sigh_return_a:
 * The return continuation for the ML signal handler.
 */
ML_CODE_HDR(XID(sigh_return_a))
	set	contmask,%tmp4
	ba	set_request
	set	REQ_SIG_RETURN,%tmp3	/* delay slot */

/* sigh_resume:
 * Resume execution at the point at which a handler trap occurred.  This is a
 * standard two-argument function, thus the closure is in ml_cont (%stdcont).
 */
XID(sigh_resume):
	set	callcc_mask,%tmp4
	ba	set_request
	set	REQ_SIG_RESUME,%tmp3	/* delay slot */

ML_CODE_HDR(XID(handle_a))
	set	exnmask,%tmp4
	ba	set_request
	set	REQ_EXN,%tmp3		/* delay slot */

ML_CODE_HDR(XID(return_a))
	set	contmask,%tmp4
	ba	set_request
	set	REQ_RETURN,%tmp3		/* delay slot */

XID(request_fault):
	set	exnmask,%tmp4
	ba	set_request
	set	REQ_FAULT,%tmp3		/* delay slot */

ML_CODE_HDR(XID(callc_a))
	CHECKLIMIT(closmask)
	set	closmask,%tmp4
	set	REQ_CALLC,%tmp3
	/* fall through */

set_request:			      /* a quick return to run_ml() */
	ld	[%sp+MLSTATE_OFFSET],%tmp2 /* get MLState ptr from stack */
	st	%tmp4,[%tmp2+mask_]    /* save the register mask */
	st	%tmp3,[%tmp2+request]
	st	%g0,[%tmp2+inML]		/* note that we have left ML code */
	dec	4,%allocptr			/* adjust store pointer */
	st	%allocptr,[%tmp2]		/* save allocptr */
	st	%storeptr,[%tmp2+8]		/* save storeptr */
	std	%stdarg,[%tmp2+16]		/* save %stdarg, %stdcont */
	st	%stdclos,[%tmp2+24]		/* save closure */
	st	%varptr,[%tmp2+36]		/* save varptr */
	st	%exncont,[%tmp2+40]		/* save exncont */

#if (CALLEESAVE > 0)
        std     %miscreg0,[%tmp2+miscreg0_offset]
#endif
#if (CALLEESAVE > 2)
        std     %miscreg2,[%tmp2+miscreg2_offset]
#endif
#if (CALLEESAVE > 4)
        std     %miscreg4,[%tmp2+miscreg4_offset]
#endif
#if (CALLEESAVE > 6)
        std     %miscreg6,[%tmp2+miscreg6_offset]
#endif
#if (CALLEESAVE > 8)
        std     %miscreg8,[%tmp2+miscreg8_offset]
#endif
	
        ldd	[%sp+64],%g6		/* restore C registers %g6 & %g7. */
	ret
	restore				/* restore C register window (delay slot) */

	.global	XID(saveregs)		/* for access from debugger */
XID(saveregs):
	ld	[%sp+MLSTATE_OFFSET],%tmp2 /* get MLState ptr from stack */
	st	%tmp4,[%tmp2+mask_]	/* save register mask */
	tst	%limit
	bne	1f
	set	REQ_SIGNAL,%limit		/* delay slot */
	st	%limit,[%tmp2+request]
1:
	st	%g0,[%tmp2+inML]		/* note that we have left ML code */
	st	%gclink,[%tmp2+gclink_offset]	/* resume pc */
	add	%baseptr,-4096,%baseptr		/* adjust the base code ptr (sub 4096) */
	dec	4,%allocptr			/* adjust store pointer */
	st	%allocptr,[%tmp2]		/* save allocptr */
	st	%storeptr,[%tmp2+storeptr_offset] /* save storeptr */
	std	%stdarg,[%tmp2+stdarg_offset]	/* save %stdarg, %stdcont */
	std	%stdclos,[%tmp2+stdclos_offset]	/* save %stdclos, %baseptr */
	st	%varptr,[%tmp2+varptr_offset]
	st	%stdlink,[%tmp2+stdlink_offset]
	st	%exncont,[%tmp2+exncont_offset]
	std	%miscreg0,[%tmp2+miscreg0_offset]
	std	%miscreg2,[%tmp2+miscreg2_offset]
	std	%miscreg4,[%tmp2+miscreg4_offset]
	std	%miscreg6,[%tmp2+miscreg6_offset]
	std	%miscreg8,[%tmp2+miscreg8_offset]
	std	%miscreg10,[%tmp2+miscreg10_offset]
	st	%miscreg12,[%tmp2+miscreg12_offset]
	ldd	[%sp+64],%g6		/* restore C registers %g6 & %g7. */
	ret
	restore				/* restore C register window (delay slot) */

XID(restoreregs):
	save	%sp,-SA(ML_FRAMESIZE),%sp
	st	%i0,[%sp+MLSTATE_OFFSET]	/* save MLState ptr on stack */
	set	XID(saveregs),%tmp2
	st	%tmp2,[%sp+STARTGC_OFFSET]
	mov	%i0,%tmp2			/* transfer MLState ptr to %tmp2 */
	std	%g6,[%sp+64]		/* save C registers %g6 & %g7 */
	set	_ml_mul,%tmp1		/* set pointer to ml_mul */
	st	%tmp1,[%sp+72]
	set	_ml_div,%tmp1		/* set pointer to ml_div */
	st	%tmp1,[%sp+76]
	ld	[%tmp2+allocptr_offset],%allocptr
	ld	[%tmp2+limit_offset],%limit
	ld	[%tmp2+storeptr_offset],%storeptr
	ld	[%tmp2+gclink_offset],%gclink
	ldd	[%tmp2+stdarg_offset],%stdarg      /* stdarg and stdcont */
	ldd	[%tmp2+stdclos_offset],%stdclos     /* stdclos and baseptr */
	ld 	[%tmp2+varptr_offset],%varptr
	ld	[%tmp2+stdlink_offset],%stdlink
	ld	[%tmp2+exncont_offset],%exncont	/* restore exnptr */
	ldd	[%tmp2+miscreg0_offset],%miscreg0
	ldd	[%tmp2+miscreg2_offset],%miscreg2
	ldd	[%tmp2+miscreg4_offset],%miscreg4
	ldd	[%tmp2+miscreg6_offset],%miscreg6
	ldd	[%tmp2+miscreg8_offset],%miscreg8
	ldd	[%tmp2+miscreg10_offset],%miscreg10
	ld	[%tmp2+miscreg12_offset],%miscreg12
	inc	4,%allocptr		/* adjust store pointer */
	sub	%baseptr,-4096,%baseptr	/* adjust the base code ptr (add 4096) */
	set	1,%tmp1			/* note that we have entered ML code */
	st	%tmp1, [%tmp2+inML]
	ld	[%tmp2+GCpending],%tmp1	/* check for pending GC sync */
	tst	%tmp1
	bne	3f
	ld	[%tmp2+NumPendingSigs],%tmp1	/* check for pending signals */
	tst	%tmp1
	bne	2f
	nop
1:
	.global	XID(go)
XID(go):
	jmp	%gclink			/* invoke the ML code */
	subcc	%allocptr,%limit,%g0		/* delay slot */
2:				     	/* there are pending signals */
	ld	[%tmp2+maskSignals],%tmp1	/* check if signals are masked */
	tst	%tmp1
	bne	1b
	ld	[%tmp2+inSigHandler],%tmp1
	tst	%tmp1			/* check if we are currently handling a signal */
	bne	1b
	set	1,%tmp1			/* (delay slot) */
	st	%tmp1,[%tmp2+handlerPending]	/* note that a handler trap is pending */
3:	set	0,%limit
	ba	1b
	nop


/* adjust_limit:
 * Adjust the heap limit pointer so that a trap will be generated on the next limit
 * check and then continue executing ML code.
 * NOTE: this code cannot trash any registers (other than %limit) or the condition code.
 * To achieve this we work inside a new register window.
 */

/* MP Note : This will have to be changed for MAX_PROCS > 1 cases, since
 * the saved_pc cannot be put in a global runtime variable.
 */
#if (MAX_PROCS > 1)
	???
#else
	.global	XID(adjust_limit)
XID(adjust_limit):
	save	%sp,-SA(WINDOWSIZE),%sp
	sethi	%hi(XID(saved_pc)),%o3
	ld	[%o3+%lo(XID(saved_pc))],%o0
	set	0,%limit
	jmp	%o0
	restore				/* (delay slot) */
#endif

/* array : (int * 'a) -> 'a array
 * Allocate and initialize a new array.	 This can cause GC.
 */
ML_CODE_HDR(XID(array_a))
1:
	ld	[%stdarg],%tmp2		/* get length into %tmp2 */
	ld	[%stdarg+4],%tmp1	/* get default into %tmp1 */
	sra	%tmp2,1,%tmp2		/* convert to sparc int */
	sll	%tmp2,width_tags,%tmp3	/* build the tag in %tmp3 */
	or	%tmp3,TAG_array,%tmp3
	sll	%tmp2,2,%tmp2		/* scale length to bytes */
	add	%allocptr,%tmp2,%tmp4
	set	1b,%gclink
	dec	4,%tmp2			/* length-- */
	subcc	%tmp4,%limit,%g0		/* check the heap limit */
	bgt	XID(saveregs)
	set	closmask,%tmp4			/* delay slot */
	st	%tmp3,[%allocptr-4]		/* store the tag */
	mov	%allocptr,%stdarg			/* result := object addr. */
2:					/* initialization loop */
	st	%tmp1,[%allocptr]		    /* store default. */
	deccc	4,%tmp2			    /* length-- */
	bge	2b
	inc	4,%allocptr			    /* freeptr++ (delay slot) */
	/* end loop */
	inc	4,%allocptr			/* freeptr++ */
	CONTINUE

/* create_b : int -> bytearray
 * create_r : int -> realarray
 * create_s : int -> string
 * Create bytearray, realarray or string of given length.  Note that the length
 * is the number of array elements.  These can cause GC.
 */
ML_CODE_HDR(XID(create_r_a))
1:	sra	%stdarg,1,%tmp2	    /* %tmp2 = length (sparc int) */
	sll	%tmp2,width_tags,%tmp1
	or	%tmp1,TAG_realdarray,%tmp3	    /* build the descriptor in %tmp3 */
	sll	%tmp2,3,%tmp2	    /* %tmp2 = length in bytes (no tag) */
/* Here %tmp3 holds the descriptor and %tmp2 holds the object length in bytes */
	add	%allocptr,%tmp2,%tmp4
	set	1b,%gclink
	add	%tmp2,4,%tmp2	    /* %tmp2 = length including tag*/
	subcc	%tmp4,%limit,%g0		/* check the heap limit */
	bgt	XID(saveregs)
	set	closmask,%tmp4			/* delay slot */
	st	%tmp3,[%allocptr-4]	    /* store the tag */
	mov	%allocptr,%stdarg		    /* result := object addr */
	add	%tmp2,%allocptr,%allocptr	    /* freeptr += length */
	CONTINUE


ML_CODE_HDR(XID(create_b_a))
1:	sra	%stdarg,1,%tmp2	    /* %tmp2 = length (sparc int) */
	sll	%tmp2,width_tags,%tmp1
	or	%tmp1,TAG_bytearray,%tmp3	    /* build the descriptor in %tmp3 */
	add	%tmp2,3,%tmp2	    /* %tmp2 = length in words */
	sra	%tmp2,2,%tmp2
	sll	%tmp2,2,%tmp2	    /* %tmp2 = length in bytes (no tag) */
/* Here %tmp3 holds the descriptor and %tmp2 holds the object length in bytes */
	add	%allocptr,%tmp2,%tmp4
	set	1b,%gclink
	add	%tmp2,4,%tmp2	    /* %tmp2 = length including tag*/
	subcc	%tmp4,%limit,%g0		/* check the heap limit */
	bgt	XID(saveregs)
	set	closmask,%tmp4			/* delay slot */
	st	%tmp3,[%allocptr-4]	    /* store the tag */
	mov	%allocptr,%stdarg		    /* result := object addr */
	add	%tmp2,%allocptr,%allocptr	    /* freeptr += length */
	CONTINUE

ML_CODE_HDR(XID(create_s_a))
1:	sra	%stdarg,1,%tmp2	    /* %tmp2 = length (sparc int) */
	sll	%tmp2,width_tags,%tmp1
	or	%tmp1,TAG_string,%tmp3  /* build the descriptor in %tmp3 */
	add	%tmp2,3,%tmp2	    /* %tmp2 = length in words */
	sra	%tmp2,2,%tmp2
	sll	%tmp2,2,%tmp2	    /* %tmp2 = length in bytes (no tag) */
/* Here %tmp3 holds the descriptor and %tmp2 holds the object length in bytes */
	add	%allocptr,%tmp2,%tmp4
	set	1b,%gclink
	add	%tmp2,4,%tmp2	    /*  %tmp2 = length including tag*/
	subcc	%tmp4,%limit,%g0		/* check the heap limit */
	bgt	XID(saveregs)
	set	closmask,%tmp4			/* delay slot */
	st	%tmp3,[%allocptr-4]	    /* store the tag */
	mov	%allocptr,%stdarg		    /* result := object addr */
	add	%tmp2,%allocptr,%allocptr	    /* freeptr += length */
	CONTINUE

/* create_v_a : int * 'a list -> 'a vector
 * 	creates a vector with elements taken from a list.
 *	n.b. The frontend ensures that list cannot be nil.
 */
ML_CODE_HDR(XID(create_v_a))
#define ML_NIL 1
#define ML_LIST_HD(p) [p]
#define ML_LIST_TL(p) [p+4]
1:					/* jump back here after GC trap */
	ld 	[%stdarg],%tmp4		/* tmp1 := tagged length */
	sra	%tmp4,1,%tmp4		/* tmp1 := untagged length */
	sll	%tmp4,width_tags,%tmp2	/* build descriptor in tmp2 */
	or	%tmp2,TAG_record,%tmp2	/* tag field */
	sll	%tmp4,2,%tmp4		/* tmp1:=length in bytes */
	add	%allocptr,%tmp4,%tmp4
	set	1b,%gclink
	subcc	%tmp4,%limit,%g0		/* check the heap limit */
	bgt	XID(saveregs)
	set	closmask,%tmp4			/* delay slot */
	st	%tmp2,[%allocptr-4]		/* store descriptor */
	ld	[%stdarg+4],%tmp4		/* tmp2 := list */
	mov	%allocptr,%stdarg			/* return val = arg of continuation */
2:
	ld	ML_LIST_HD(%tmp4),%tmp2	/* %tmp2 := hd(%tmp4) */
	ld 	ML_LIST_TL(%tmp4),%tmp4 	/* %tmp4 := tl(%tmp4) */
	st	%tmp2,[%allocptr]		/* update vector */
	cmp	%tmp4,ML_NIL		/* if (%tmp4 == nil) then exit */
	bne	2b
	add	%allocptr,4,%allocptr		/* next index (delay slot) */
4:		
	add	%allocptr,4,%allocptr		/* adjust heap pointer for descriptor */
	CONTINUE
	
/* floor : real -> int
 * Return the floor of the argument or else raise Float("floor") if out of range.
 * We implement the range check by using an integer comparison with the high 32
 * bits of the real value (which contains the biased exponent).
 * (double)(2^30)   == [0x41d00000, 0x0]
 * (double)(-2^30)  == [0xc1d00000, 0x0]
 */
ML_CODE_HDR(XID(floor_a))
	ld	[%stdarg],%f0	    /* fetch arg into %f0, %f1. */
	ld	[%stdarg+4],%f1
	ld	[%stdarg],%tmp2	    /* %tmp2 gets high word. */
	tst	%tmp2		    /* negative ? */
	blt	1f
	nop
				/* handle positive case */
	set	0x41d00000,%tmp3	    /* %tmp3 = 2^30 */
	cmp	%tmp2,%tmp3		    /* if %tmp2 >= 2^30 then range error */
	bge	out_of_range
	nop
	fdtoi	%f0,%f2		    /* cvt to int (round towards 0) */
	st	%f2,[%sp+80]
	ld	[%sp+80],%tmp2	    /* %tmp2 gets int result (via stack temp). */
	ba	2f
	nop
1:				/* handle negative case. */
	set	0xc1d00000,%tmp3	    /* %tmp3 = -2^30 */
	cmp	%tmp2,%tmp3		    /* if %tmp2 < -2^30 then range error */
	bge	out_of_range	    /* not bl because of sign. */
	nop
	fdtoi	%f0,%f2		    /* cvt to int (round towards 0) */
	st	%f2,[%sp+80]
	fitod	%f2,%f4		    /* cvt back to real to check for fraction */
	fcmpd	%f0,%f4		    /* same value? */
	ld	[%sp+80],%tmp2	    /* %tmp2 gets int result (via stack temp). */
	fbe	2f		    /* check result of fcmpd */
	nop
	dec	%tmp2		    /* push one lower */
2:				/* cvt result to ML int, and continue */
	add	%tmp2,%tmp2,%tmp2
	add	%tmp2,1,%stdarg
	CONTINUE

out_of_range:			/* out of range */
	t	ST_INT_OVERFLOW		/* generate an Overflow exn.  We do this */
					/* via a trap to produce a SIGOVFL */


/* logb : real -> int
 * Extract and unbias the exponent, return 0 for a zero exponent.
 * The IEEE bias is 1023.
 */
ML_CODE_HDR(XID(logb_a))
	ld	[%stdarg],%tmp2		/* extract exponent. */
	srl	%tmp2,20,%tmp2
	andcc	%tmp2,0x7ff,%tmp2		/* if (exp == 0) */
	beq	2f
	nop
	sll	%tmp2,1,%tmp2		/* else unbias and cvt to ML int. */
	sub	%tmp2,2045,%stdarg		/* 2(n-1023)+1 == 2n-2045. */
1:	CONTINUE
2:	ba	1b
	set	1,%stdarg			/* return ML zero (delay slot) */


/* scalb : (real * int) -> real
 * Scale the first argument by 2 raised to the second argument.	 Raise
 * Float("underflow") or Float("overflow") as appropriate.
 */

ML_CODE_HDR(XID(scalb_a))
	CHECKLIMIT(closmask)
	ld	[%stdarg+4],%tmp1   /* %tmp1 gets scale (second arg) */
	sra	%tmp1,1,%tmp1	    /* cvt scale to sparc int */
	ld	[%stdarg],%stdarg   /* %stdarg gets real (first arg) */
	ld	[%stdarg],%tmp4	    /* %tmp4 gets high word of real value. */
	set	0x7ff00000,%tmp2    /* %tmp2 gets exponent mask. */
	andcc	%tmp4,%tmp2,%tmp3   /* extract exponent into %tmp3. */
	beq	1f		    /* if 0 then return same */
	nop
	srl	%tmp3,20,%tmp3	    /* cvt exp to int (delay slot). */
	addcc	%tmp3,%tmp1,%tmp1	    /* %tmp1 = exp + scale */
	ble	under		    /* if new exp <= 0 then underflow */
	nop
	cmp	%tmp1,2047	    /* if new exp >= 2047 then overflow */
	bge	over
	nop
	andn	%tmp4,%tmp2,%tmp4   /* mask out old exponent. */
	sll	%tmp1,20,%tmp1	    /* shift new exp to exponent position. */
	or	%tmp4,%tmp1,%tmp4   /* set new exponent. */
	ld	[%stdarg+4],%tmp1   /* %tmp1 gets low word of real value. */
	st	%tmp4,[%g6]	    /* allocate the new real value */
	st	%tmp1,[%g6+4]
7:	set	DESC_reald,%tmp1
	st	%tmp1,[%g6-4]
	mov	%g6,%stdarg	    /* set result. */
	inc	12,%g6		    /* storeptr += 3 */
1:	CONTINUE

over:				/* handle overflow */
	t	ST_INT_OVERFLOW	    /* generate an Overflow exn.  We do this */
	/* never get here */	    /* via a trap to produce a SIGOVFL */

under:				/* handle underflow */
	st	%g0,[%allocptr]
	st	%g0,[%allocptr+4]
	ba	7b
	nop

/** Integer multiplication and division routines **
 *
 * The arguments are %o2, %o3 and the result is in %o2.
 * Note: this code assumes that .mul and .div don't use %g4 (the limitptr).
 */
	.global .mul, .div

/* ml_mul:
 * multiply %o2 by %o3, returning the result in %o2
 * Note: this code assumes that .mul doesn't trash any global or input
 * registers.
 */
_ml_mul:
	save	%sp,-SA(WINDOWSIZE),%sp
	mov	%i2,%o0
	call	.mul
	mov	%i3,%o1			/* (delay slot) */
	bnz	1f			/* if z is clear, then overflow */
	restore %o0,0,%o2		/* result in %o2 (delay slot) */
	retl
	nop
1:					/* handle overflow. */
	t	ST_INT_OVERFLOW		/* generate an Overflow exn.  We do this */
					/* via a trap to produce a SIGOVFL */

/* ml_div:
 * divide %o2 by %o3, returning the result in %o2.
 * Note: .div uses %g1, %g2 and %g3, so we must save them.  We do this using the
 * locals of the new window, since .div is a leaf routine.
 */
_ml_div:
	save	%sp,-SA(WINDOWSIZE),%sp
	addcc	%i3,%g0,%o1		/* %o1 is divisor (and check for zero) */
	bz	1f
				    /* save %g1, %g2 and %g3 (using new window) */
	mov	%g1,%l1			/* (delay slot) */
	mov	%g2,%l2
	mov	%g3,%l3
	call	.div
	mov	%i2,%o0			/* (delay slot) */
				    /* restore %g1, %g2 and %g3 */
	mov	%l3,%g3
	mov	%l2,%g2
	mov	%l1,%g1
	ret
	restore %o0,0,%o2		/* result in %o2 (delay slot) */
1:				    /* handle zero divide */
	restore				/* restore ML window */
	t	ST_DIV0			/* generate a Div exn.  We do this via a */
					/* trap to produce a SIGDIV */


/* try_lock : spin_lock -> bool
 * low-level test-and-set style primitive for mutual-exclusion among 
 * processors.
 */
ML_CODE_HDR(XID(try_lock_a))
#if (MAX_PROCS > 1)
	???
#else (MAX_PROCS == 1)
	ld	[%stdarg],%tmp1	/* load previous value into %tmp1 */
	set	1,%tmp2		/* ML_false */
	st	%tmp2,[%stdarg]	/* store ML_false into the lock */
	mov	%tmp1,%stdarg		/* return previous value of lock */
	CONTINUE
#endif

/* unlock : releases a spin lock 
 */
ML_CODE_HDR(XID(unlock_a))
#if (MAX_PROCS > 1)
	???
#else (MAX_PROCS == 1)
	set	3,%tmp1		/* store ML_true ... */
	st	%tmp1,[%stdarg]	/* into the lock */
	set	1,%stdarg		/* return unit */
	CONTINUE
#endif


/* set_fsr:
 * Load the floating-point status register with the given word.
 */
	.global	XID(set_fsr)
XID(set_fsr):
	set	fsrtmp,%o1
	st	%o0,[%o1]
	retl
	ld	[%o1],%fsr		/* (delay slot) */
	.seg	"data"
fsrtmp:	.word	0


/* void FlushICache (char *addr, int nbytes)
 */
	
	.global	XID(FlushICache)
XID(FlushICache):
	and	%o1,0x1F,%o2	/* m <- (nbytes % (32-1)) >> 2 (use %o2 for m) */
	srl	%o2,2,%o2
	srl	%o1,5,%o1	/* i <- (nbytes >> 5) */
/* FLUSH4 implements: if (m > 0) { FLUSH addr; addr += 4; m--;} else goto L_test */
#define FLUSH4					\
		tst	%o2;			\
		ble	L_test;			\
		nop;				\
		iflush	%o0;			\
		inc	4,%o0;			\
		dec	1,%o2
	FLUSH4
	FLUSH4
	FLUSH4
	FLUSH4
	FLUSH4
	FLUSH4
	FLUSH4
				/* addr is 32-byte aligned here */
L_test:
	tst	%o1
	be	L_exit
	nop
L_loop:				/* flush 32 bytes per iteration */
	iflush	%o0
	iflush	%o0+8
	iflush	%o0+16
	iflush	%o0+24
	deccc	1,%o1		/* if (--i > 0) goto L_loop */
	bg	L_loop
	inc	32,%o0		/* addr += 32 (delay slot) */
L_exit:
	retl
	nop


/* this bogosity is for export.c */
	.global XID(startptr)
XID(startptr):
#ifdef SOLARIS
	.long	_start
#else
	.long	start
#endif
