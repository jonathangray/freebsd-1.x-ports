/* I386.prim.s, by Mark Leone (mleone@cs.cmu.edu)
 *
 * The 386 registers are used as follows:
 *
 * EAX - temp1 (see the code generator, i386/i386.sml)
 * EBX - misc1
 * ECX - misc2
 * EDX - misc3
 * ESI - standard continuation (ml_cont, see ml_state.h)
 * EBP - standard argument (ml_arg)
 * EDI - free space pointer (ml_allocptr)
 * ESP - stack pointer
 * EIP - program counter (ml_pc)
 * 
 * Other values, which on most architectures would reside in registers,
 * are stored on the stack:
 * 
 * 8(ESP) - exception handler continuation (ml_exncont)
 * 12(ESP) - data limit (ml_limitptr)
 * 16(ESP) - standard closure (ml_closure)
 * 20(ESP) - link register (ml_linkreg)
 * 24(ESP) - store pointer (ml_storeptr)
 * 28(ESP) - var pointer (ml_varptr)
 *
 * Note that the garbage collector only preserves root registers 
 * (EBX, ECX, EDX, ESI, EBP, EIP).
 *
 * The ML state vector has the following layout on the 386 (see "ml_state.h"):
 *
 *		+----------------------+
 * MLState -->	| ml_allocptr  EDI     |
 *		|----------------------|
 *	      4 | ml_limitptr  12(ESP) |
 *		|----------------------|
 *	      8 | ml_storeptr  24(ESP) |
 *		|----------------------|
 *	     12 | ml_exncont	8(ESP) | <-- ml_roots
 *		|----------------------|
 *	     16 | ml_arg       EBP     |
 *		|----------------------|
 *	     20 | ml_cont      ESI     |
 *		|----------------------|
 *	     24 | ml_closure   16(ESP) |
 *		|----------------------|
 *	     28 | ml_varptr    20(ESP) |
 *		|----------------------|
 *	     32 | ml_linkreg   28(ESP) |
 *		|----------------------|
 *	     36 | ml_pc	       EIP     |
 *		|----------------------|
 *	     40 | misc1	       EBX     |
 *		|----------------------|
 *	     44 | misc2	       ECX     |
 *		|----------------------|
 *	     48 | misc3	       EDX     |
 *		|----------------------|
 *	     52 | tempmem	       |
 *		|----------------------|
 *	     56 | tempmem2	       |
 *		|----------------------|
 *	     60 | inML		       |
 *		|----------------------|
 *	     64 | request	       |
 *		|----------------------|
 *	     68 | handlerPending       |
 *		|----------------------|
 *	     72 | inSigHandler	       |
 *		|----------------------|
 *	     76 | maskSignals	       |
 *		|----------------------|
 *	     80 | NumPendingSigs       |
 *		|----------------------|
 *	     84 | ioWaitFlag	       |
 *		|----------------------|
 *	     88 | GCpending	       |
 *		|----------------------|
 *	     92 | MLframe	       |
 *		|----------------------|
 *			  ....
 *		|----------------------|
 *	    112 | mask		       |
 *		|----------------------|

 */

#include "mask.h"
#include "tags.h"
#include "request.h"

/* Registers (see i386/i386.sml): */
#define temp		%eax
#define misc1		%ebx
#define misc2		%ecx
#define misc3		%edx
#define cont		%esi
#define arg		%ebp
#define allocptr	%edi

/* Stack frame (see i386/i386.sml): */
#define tempmem		0(%esp)
#define tempmem2	4(%esp)
#define exncont		8(%esp)
#define limitptr	12(%esp)
#define closure		16(%esp)
#define linkreg		20(%esp)
#define storeptr	24(%esp)
#define varptr		28(%esp)
#define start_gc	32(%esp)
#define mask		36(%esp)
#define mlstate_ptr	40(%esp)
#define ML_STATE_OFFSET 40
#define ML_FRAME_SIZE	44

/* Offsets of fields in MLState (see ml_state.h): */
#define ml_allocptr	0
#define ml_limitptr	4
#define ml_storeptr	8
#define ml_exncont	12
#define ml_arg		16
#define ml_cont		20
#define ml_closure	24
#define ml_varptr	28
#define ml_linkreg	32
#define ml_pc		36
#define ml_misc1	40
#define ml_misc2	44
#define ml_misc3	48
#define ml_tempmem	52
#define ml_tempmem2	56
#define inML		60
#define request		64
#define handlerPending	68
#define inSigHandler	72
#define maskSignals	76
#define NumPendingSigs	80
#define ioWaitFlag	84
#define GCpending	88
#define MLframe		92
#define ml_mask		112

/* Following are to make Sequent dynix3 assembler happy. */
#ifdef DYNIX
#define lea leal
#define stosl stosd
#define via *
#else
#define via
#endif

/*
 * 386 function call conventions:  [true for gcc and dynix3 cc; 
 *                                  untested for others]
 * Caller save registers: eax, ecx, edx
 * Callee save registers: ebx, esi, edi, and ebp. 
 * Floating point state is caller-save.
 * Arguments passed on stack.  Rightmost argument pushed first.
 * Word-sized result returned in %eax.
 */

#define CALLEE_SAVE	\
	pushl	%ebx;	\
	pushl	%esi;	\
	pushl	%edi;	\
	pushl	%ebp	

#define CALLEE_RESTORE	\
	popl	%ebp;	\
	popl	%edi;	\
	popl	%esi;	\
	popl	%ebx 

/* MOVE copies one memory location to another, using a specified temporary. */

#define MOVE(src,tmp,dest)	\
	movl	src, tmp;	\
	movl	tmp, dest

#define ML_CODE_HDR(name)					\
	    .globl name;					\
	    .align  2;						\
	    .word   TAG_backptr;				\
    name:

#define CONTINUE						\
	movl	(cont), temp;					\
	movl	temp, linkreg;	  /* Not really a register */	\
	cmpl	limitptr, allocptr;				\
	jmp     via temp

#define CHECKLIMIT(lab, maskval)	\
	jbe	9f;			\
	lea	lab, temp;		\
	movl	$maskval, mask;		\
	jmp	via start_gc;		\
 9:

/**********************************************************************/
	.text

ML_CODE_HDR(_sigh_return_a)
	movl	mlstate_ptr, temp
	movl	$REQ_SIG_RETURN, request(temp)
	jmp	_quicksave

/* sigh_resume:
 * Resume execution at the point at which a handler trap occurred.  This is a
 * standard two-argument function, thus the closure is in ml_cont.
 */
	.globl	_sigh_resume
_sigh_resume:
	movl	mlstate_ptr, temp
	movl	$REQ_SIG_RESUME, request(temp)
	movl	$callcc_mask, ml_mask(temp)
	jmp	_quicksave

ML_CODE_HDR(_handle_a)
	movl	mlstate_ptr, temp
	movl	$REQ_EXN, request(temp)
	movl	$exnmask, ml_mask(temp)
	jmp	_quicksave

ML_CODE_HDR(_return_a)
	movl	mlstate_ptr, temp
	movl	$REQ_RETURN, request(temp)
	movl	$contmask, ml_mask(temp)
	jmp	_quicksave

/* Request a fault.  The floating point coprocessor must be reset
   (thus trashing the FP registers) since we don't know whether a 
   value has been pushed into the temporary "register".	 This is OK 
   because no floating point registers will be live at the start of 
   the exception handler. */

	.globl	_request_fault
_request_fault:
	call	_fpenable		/* Doesn't trash any general regs. */
	movl	mlstate_ptr, temp
	movl	$REQ_FAULT, request(temp)
	movl	$exnmask, ml_mask(temp)
	jmp	_quicksave

ML_CODE_HDR(_callc_a)
1:	CHECKLIMIT(1b, closmask)
	movl	mlstate_ptr, temp
	movl	$REQ_CALLC, request(temp)
	movl	$closmask, ml_mask(temp)
	/* fall into quicksave */

	.globl _quicksave		/* For debugging. */
_quicksave:
	movl	$0, inML(temp)		/* note that we have left ML code */

	/* Save registers. */
	movl	allocptr, ml_allocptr(temp)
	movl	arg, ml_arg(temp)
	movl	cont, ml_cont(temp)

#define	temp2 allocptr
	/* Save pseudo-registers before the stack frame is popped. 
	   (Don't need to save limitptr.) */
	MOVE(tempmem, temp2, ml_tempmem(temp))
	MOVE(tempmem2,temp2, ml_tempmem2(temp))
	MOVE(exncont, temp2, ml_exncont(temp)) 
	MOVE(closure, temp2, ml_closure(temp))
	MOVE(linkreg, temp2, ml_linkreg(temp))
	MOVE(storeptr,temp2, ml_storeptr(temp))
	MOVE(varptr,  temp2, ml_varptr(temp))
	MOVE(mask,    temp2, ml_mask(temp))
#undef	temp2	

	/* Pop the stack frame and return to run_ml(). */
	addl	$ML_FRAME_SIZE, %esp
	CALLEE_RESTORE
	ret

	.globl	_saveregs
_saveregs:
	pushl	temp			/* Contains "resume" address. */
	movl	ML_STATE_OFFSET+4(%esp), temp
	popl	ml_pc(temp)
	movl	$0, inML(temp)		/* note that we have left ML code */
	cmpl	$0, limitptr
	jne	1f
	movl	$REQ_SIGNAL, request(temp)
1:
	/* Save registers. */
	movl	allocptr, ml_allocptr(temp)
	movl	arg, ml_arg(temp)
	movl	cont, ml_cont(temp)
	movl	misc1, ml_misc1(temp)
	movl	misc2, ml_misc2(temp)
	movl	misc3, ml_misc3(temp)

#define	temp2 allocptr
	/* Save pseudo-registers before the stack frame is popped. 
	   (Don't need to save limitptr.) */
	MOVE(tempmem, temp2, ml_tempmem(temp))
	MOVE(tempmem2,temp2, ml_tempmem2(temp))
	MOVE(exncont, temp2, ml_exncont(temp)) 
	MOVE(closure, temp2, ml_closure(temp))
	MOVE(linkreg, temp2, ml_linkreg(temp))
	MOVE(storeptr,temp2, ml_storeptr(temp))
	MOVE(varptr,  temp2, ml_varptr(temp))
	MOVE(mask,    temp2, ml_mask(temp))
#undef	temp2	

	/* Pop the stack frame and return to run_ml(). */
	addl	$ML_FRAME_SIZE, %esp
	CALLEE_RESTORE
	ret

	.globl	_restoreregs
_restoreregs:
	movl	4(%esp), temp		/* Get argument (MLState ptr). */
	CALLEE_SAVE

#define temp2	%ebx
	/* Allocate and initialize the ML stack frame. */
	subl	$ML_FRAME_SIZE, %esp
	MOVE(	ml_tempmem(temp),  temp2, tempmem)
	MOVE(	ml_tempmem2(temp), temp2, tempmem2)
	MOVE(	ml_exncont(temp),  temp2, exncont) 
	MOVE(	ml_limitptr(temp), temp2, limitptr)
	MOVE(	ml_closure(temp),  temp2, closure)
	MOVE(	ml_linkreg(temp),  temp2, linkreg) 
	MOVE(	ml_storeptr(temp), temp2, storeptr)
	MOVE(	ml_varptr(temp),   temp2, varptr)
	movl	$_saveregs, start_gc
	movl	temp, mlstate_ptr
#undef	temp2

	/* Load ML registers. */
	movl	ml_allocptr(temp), allocptr
	movl	ml_cont(temp), cont
	movl	ml_arg(temp), arg
	movl	ml_misc1(temp), misc1
	movl	ml_misc2(temp), misc2
	movl	ml_misc3(temp), misc3

	movl	%esp, MLframe(temp)     /* Note frame ptr for signal handler. */
	movl	$1,inML(temp)
	cmpl	$0, GCpending(temp)
	jne	adjust
	cmpl	$0, NumPendingSigs(temp)	/* Any pending signals? */
	jne	pending
jmp_ml:
	cmpl	limitptr, allocptr
	jmp	via ml_pc(temp)		/* Jump to ML code. */
pending:
	cmpl	$0, maskSignals(temp)	/* Are signals masked? */
	jne	jmp_ml
	cmpl	$0, inSigHandler(temp)	/* Currently handling a signal? */
	jne	jmp_ml
	movl	$1,handlerPending(temp)	/* A handler trap is now pending */
adjust:
	movl	$0,limitptr		/* Force a trap on next limit check */
	jmp	jmp_ml			/* Jump to ML code. */

/* ----------------------------------------------------------------------
 * array : (int * 'a) -> 'a array
 * Allocate and initialize a new array.	 This can cause GC.
 */

ML_CODE_HDR(_array_a)
#define length	%ecx			/* %ecx must be a misc register! */
1:	movl	(arg), length		/* Get length of array (tagged). */
	sarl	$1, length		/* Remove tag bit. */

	/* Check heap limit. The number of bytes needed is 4*length+4 */
	lea	4(allocptr,length,4), temp
	cmpl	limitptr, temp
	CHECKLIMIT(1b, closmask)

	movl	length, temp		/* Construct descriptor for array. */
	sall	$width_tags, temp
	orl	$TAG_array, temp
	stosl				/* Store descriptor, inc. allocptr. */
	movl	4(arg), temp		/* Get initial value for array elts. */
	movl	allocptr, arg		/* Pointer to pass to continuation. */

	/* The length is in %ecx, the initial value is in temp (%eax), 
	   and the allocation pointer is in %edi, so we can use REP STOSL
	   to allocate and initialize the array. */
	rep
	stosl
	CONTINUE	
#undef length

/* create_b : int -> bytearray
 * create_r : int -> realarray
 * create_s : int -> string
 * Create bytearray, realarray or string of given length.  Note that the length
 * is the number of array elements.  These can cause GC.
 */

#define BYTE_VECTOR(tag)						      \
1:	/* Check heap limit.  The number of bytes needed		      \
	   is < length+8  (4 bytes for descriptor, up to 3 bytes	      \
	   padding for alignment). */					      \
	movl	arg, temp;		/* Can't untag arg because of GC. */  \
	sarl	$1, temp;		/* Untagged # of bytes in vector. */  \
									      \
	lea	8(allocptr,temp), temp;					      \
	cmpl	limitptr, temp;						      \
	CHECKLIMIT(1b, closmask);					      \
									      \
	sarl	$1, arg;		/* Untag arg (it's ok now). */	      \
	movl	arg, temp;		/* Create descriptor. */	      \
	sall	$width_tags, temp;					      \
	orl	tag, temp;						      \
	stosl;				/* Store descriptor, inc. allocptr */ \
									      \
	addl	$3, arg;		/* Round up length to mult of 4. */   \
	andl	$~3, arg;						      \
	addl	allocptr, arg;		/* arg=new alloc, alloc=old alloc. */ \
	xchgl	allocptr, arg;		/* Return pointer to 1st element. */  \
	CONTINUE

ML_CODE_HDR(_create_b_a)
 BYTE_VECTOR($TAG_bytearray)

ML_CODE_HDR(_create_s_a)
 BYTE_VECTOR($TAG_string)

ML_CODE_HDR(_create_r_a)
1:	/* Check heap limit.  Need 4+length*8 bytes. */
	movl	arg, temp
	sarl	$1, temp		/* Number of elements. */
	lea	 4(allocptr,temp,8), temp
	cmpl	 limitptr, temp
	CHECKLIMIT(1b, closmask)

	sarl	$1, arg			/* Untag arg. */
	movl	arg, temp		/* Create descriptor. */
	sall	$width_tags, temp
	orl	$TAG_realdarray, temp
	stosl				/* Store descriptor, inc. allocptr. */

	lea	(allocptr,arg,8), arg	/* arg=new alloc, alloc=old alloc. */
	xchgl	allocptr, arg;		/* Return pointer to 1st element. */
	CONTINUE

/* create_v_a : int * 'a list -> 'a vector
 *	creates a vector with elements taken from a list.
 *	n.b. The frontend ensures that list cannot be nil.
 */

ML_CODE_HDR(_create_v_a)
#define ML_NIL			$1
#define ML_LIST_HD(listp)	(listp)
#define ML_LIST_TL(listp)	4(listp)
1:
	/* Check heap limit.  Need 4+length*4 bytes. */
	movl	(arg), temp
	sarl	$1, temp		/* Number of elements. */
	lea	 4(allocptr,temp,4), temp
	cmpl	 limitptr, temp
	CHECKLIMIT(1b, closmask)

	movl	(arg), temp		/* Create descriptor. */
	sarl	$1, temp
	sall	$width_tags, temp
	orl	$TAG_record, temp
	stosl				/* Store descriptor, inc. allocptr. */

	movl	4(arg), misc1		/* misc1 used as pointer into list. */
	movl	allocptr, arg		/* Return pointer to 1st element. */
2:	movl	ML_LIST_HD(misc1), temp
	stosl				/* Store element, inc. allocptr. */
	movl	ML_LIST_TL(misc1), misc1
	cmpl	ML_NIL, misc1
	jne	2b
	CONTINUE
	
/* try_lock: spin_lock -> bool. 
 * low-level test-and-set style primitive for mutual-exclusion among 
 * processors.	For now, we only provide a uni-processor trivial version.
 */
ML_CODE_HDR(_try_lock_a)
#if (MAX_PROCS > 1)
	???
#else (MAX_PROCS == 1)
	movl	(arg), temp		/* Get old value of lock. */
	movl	$1, (arg)		/* Set the lock to ML_false. */
	movl	temp, arg		/* Return old value of lock. */
	CONTINUE
#endif

/* unlock : releases a spin lock 
 */
ML_CODE_HDR(_unlock_a)
#if (MAX_PROCS > 1)
	???
#else (MAX_PROCS == 1)
	movl	$3, (arg)		/* Store ML_true into lock. */
	movl	$1, arg			/* Return unit. */
	CONTINUE
#endif


/********************* Floating point functions. *********************/

#define FPOP	fstp %st	/* Pop the floating point register stack. */

	.data
	.align	2
maxint_plus1:
	.long	0x40000000	/* Used in floor_a to check for overflow. */
minint_sub1:
	.long	0xbfffffff

/* Temporary storage for the old and new floating point control
   word.  We don't use the stack to for this, since doing so would 
   change the offsets of the pseudo-registers. */
old_controlwd:	
	.word	0
new_controlwd:	
	.word	0
	.text
	.align 2

/*
 * Initialize the 80387 floating point coprocessor.  First, the floating
 * point control word is initialized (undefined fields are left
 * unchanged).	Rounding control is set to "nearest" (although floor_a
 * needs "toward negative infinity").  Precision control is set to
 * "double".  The precision, underflow, and denormal exceptions are
 * masked.  The overflow, zero divide, and invalid operation exceptions
 * are not masked.  Next, seven of the eight available entries on the
 * floating point register stack are claimed (see i386/i386.sml).
 *
 * NB: this cannot trash any registers because it's called from request_fault.
 */
	.globl	_fpenable
_fpenable:
	finit
	subl	$4, %esp	/* Temp space.	Keep stack aligned. */
	fstcw	(%esp)		/* Store FP control word. */
	andw	$0xf0c0, (%esp)	/* Keep undefined fields, clear others. */
	orw	$0x0232, (%esp)	/* Set fields (see above). */
	fldcw	(%esp)		/* Install new control word. */
	addl	$4, %esp
	fldz			/* Push a zero onto the register stack. */
	fld	%st		/* Copy it 6 times. */
	fld	%st
	fld	%st
	fld	%st
	fld	%st
	fld	%st
	ret

/* Save the state of the floating point unit. */
	.globl _savefpregs
_savefpregs:
	movl	4(%esp), temp		/* Get pointer argument. */
	fsave	(temp)
	ret

/* Restore the state of the floating point unit. */
	.globl	_restorefpregs
_restorefpregs:
	movl	4(%esp), temp		/* Arg is an ML string. */
	frstor	(temp)
	ret

/* floor : real -> int
   Return the nearest integer that is less or equal to the argument,
   or else raise Float("floor") if out of range.  This could be made 
   more efficient using integer comparisons to check for overflow. */

ML_CODE_HDR (_floor_a)
	fstcw	old_controlwd		/* Get FP control word. */
	movw	old_controlwd, %ax
	andw	$0xf3ff, %ax		/* Clear rounding field. */
	orw	$0x0400, %ax		/* Round towards neg. infinity. */
	movw	%ax, new_controlwd
	fldcw	new_controlwd		/* Install new control word. */

	fldl	(arg)			/* Load argument. */
	ficoml	maxint_plus1		/* Compare: arg >= maxint + 1? */
	fstsw	%ax			/* Get FP status word. */
	sahf				/* Copy to integer status word. */
	jae	floor_err
	ficoml	minint_sub1		/* Compare: arg <= maxint + 1? */
	fstsw	%ax			/* Get FP status word. */
	sahf				/* Copy to integer status word. */
	jbe	floor_err

	subl	$4, %esp
	fistpl	(%esp)			/* Round, store, and pop. */
	popl	arg
	sall	$1, arg			/* Tag the resulting integer. */
	incl	arg

	fldcw	old_controlwd		/* Restore old FP control word. */
	CONTINUE
floor_err:
	FPOP				/* Discard argument. */
	fldcw	old_controlwd		/* Restore old FP control word. */
	int	$4			/* Signal an overflow. */

/* logb : real -> int
   Extract and unbias the exponent of the argument.  This could probably
   be made more efficient by using integer instructions to do the
   extraction.	NB: We assume the first floating point "register" is
   caller-save, so we can use it here (see i386/i386.sml). */

ML_CODE_HDR (_logb_a)
	fldl	(arg)			/* Load argument into temporary. */
	fstp	%st(1)			/* Store in first "register". */
	fxtract				/* Decompose into exp, fraction */
	FPOP				/* Discard fraction. */
	subl	$4, %esp
	fistl	(%esp)			/* Store exponent */
	popl	arg
	sall	$1, arg			/* Tag the resulting integer */
	incl	arg
	CONTINUE

/* scalb : (real * int) -> real
 * Scale the first argument by 2 raised to the second argument.	 Raise
 * Float("underflow") or Float("overflow") as appropriate.
 * NB: We assume the first floating point "register" is
 * caller-save, so we can use it here (see i386/i386.sml). */

ML_CODE_HDR (_scalb_a)
1:	CHECKLIMIT(1b, closmask)
	pushl	4(arg)			/* Get copy of scalar. */
	sarl	$1, (%esp)		/* Untag it. */
	fildl	(%esp)			/* Load it ... */
	fstp	%st(1)			/* ... into 1st FP reg. */
	addl	$4, %esp		/* Discard copy of scalar. */

	movl	(arg), temp		/* Get pointer to real. */
	fldl	(temp)			/* Load it into temp. */

	fscale				/* Multiply exponent by scalar. */
	movl	$DESC_reald, (allocptr)
	fstpl	4(allocptr)		/* Store resulting float. */
	addl	$4, allocptr		/* Allocate word for tag. */
	movl	allocptr, arg		/* Return a pointer to the float. */
	addl	$8, allocptr		/* Allocate room for float. */
	CONTINUE

/* this bogosity is for export.c */
	.globl	_startptr
_startptr: 
	.long	start
