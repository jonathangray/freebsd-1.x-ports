/* MIPS.prim.s
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
*/

#include "mask.h"
#include "tags.h"
#include "request.h"
#include "fpregs.h"

/*
 * The MIPS registers are used as follows; the names in parentheses are the
 * MLState field names (see ml_state.h):
 */

#define	      zero 0
/* assembler-temp  1  							 */
#define     stdarg 2 	/*  	standard arg  (ml_arg)		 	  */
#define    stdcont 3 	/*      standard continuation (ml_cont) 	  */
#define    stdclos 4 	/*      standard closure (ml_closure)             */
#define    stdlink 5	/* 	ptr to just-entered std function (ml_link) */
#define   miscreg0 6    /*  miscellaneous registers, 0..12 (ml_roots[]) */
#define   miscreg1 7
#define   miscreg2 8
#define   miscreg3 9
#define  miscreg4 10
#define  miscreg5 11
#define  miscreg6 12
#define  miscreg7 13
#define  miscreg8 14
#define  miscreg9 15
#define miscreg10 16
#define miscreg11 17
#define miscreg12 18
#define     limit 19 	/*      end of heap - 4096  (ml_limitptr)  	   */
#define    varptr 20 	/*      per-thread var pointer (ml_varptr) */
#define exhausted 21 	/*      arith temp; also, heap-limit comparison flag */
#define  storeptr 22 	/*   	store pointer  (ml_storeptr) 		*/
#define  allocptr 23 	/* 	freespace pointer  (ml_allocptr) */
#define   basereg 24 	/*       pointer to base of code object+32764 (ml_roots[]) */
#define    ptrtmp 25 	/*       internal temporary */
/*                26		reserved for operating system     */
/*                27		reserved for operating system     */
/*  globalptr     28		reserved for C and assembler */
/*  stackptr      29          stack pointer 			*/
#define   exncont 30 	/*       exception handler (ml_exncont) */
#define    gclink 31 	/* 	resumption point for restoreregs (ml_pc) */

#define atmp1 miscreg9
#define atmp2 miscreg10
#define atmp3 miscreg11
#define atmp4 miscreg12

#define allocptr_offset 	 0	/* offsets in MLState */
#define limit_offset		 4
#define storeptr_offset		 8
#define stdarg_offset		12
#define stdcont_offset		16
#define stdclos_offset		20
#define exncont_offset		24
#define gclink_offset		28
#define	miscreg_offset(i)	36+4*(i)
#define stdlink_offset		32
#define varptr_offset		88
#define basereg_offset		92

#define inML                    96
#define request                100
#define handlerPending         104
#define inSigHandler           108
#define maskSignals            112
#define NumPendingSigs         116
#define ioWaitFlag             120
#define GCpending              124
#define mask_		       144

#define ML_CODE_HDR(name)					\
	    .globl name;					\
	    .align  2;	/* actually 4-byte alignment */		\
	    .word   TAG_backptr;				\
    name:

#if (CALLEESAVE > 0)
#define CONTINUE						\
	    slt		$exhausted,$allocptr,$limit;	\
            j		$stdcont;
#else
#define CONTINUE						\
	    lw		$stdlink,0($stdcont);			\
	    slt		$exhausted,$allocptr,$limit;	\
	    j		$stdlink
#endif

#define CHECKLIMIT(mask)					\
	    bnez	$exhausted,3f;			\
	    lw		$exhausted,4($sp);		\
	    li		$ptrtmp,mask;			\
	    move	$gclink,$stdlink;				\
	    j		$exhausted;			\
	 3:

	.text

	.globl	saveregs
	.globl	handle_c
	.globl	return_c
	.globl	restoreregs
	.globl	savefpregs
	.globl	restorefpregs


#define regspace	40
#define argbuild	16
#define framesize	(regspace+argbuild) /* must be multiple of 8 */
#define frameoffset	(0)
#define MLSTATE_OFFSET  (0)
#define STARTGC_OFFSET (4)


/* sig_return : ('a cont * 'a) -> 'b
 */
ML_CODE_HDR(sigh_return_a)
	li	$ptrtmp,contmask
	li	$atmp1,REQ_SIG_RETURN
	b	set_request

/* sigh_resume:
 * Resume execution at the point at which a handler trap occurred.  
 */
	.globl	sigh_resume
sigh_resume:
	li	$ptrtmp,callcc_mask
	li	$atmp1,REQ_SIG_RESUME
	b	set_request

ML_CODE_HDR(handle_a) /* exception handler for ML functions called from C */
	li	$ptrtmp,exnmask
	li	$atmp1,REQ_EXN
	b	set_request

ML_CODE_HDR(return_a) /* continuation for ML functions called from C */
	li	$ptrtmp,contmask
	li	$atmp1,REQ_RETURN
	b	set_request

	.globl  request_fault
request_fault:
	li	$ptrtmp,exnmask
	li	$atmp1,REQ_FAULT
	b	set_request
	
ML_CODE_HDR(callc_a)
	CHECKLIMIT(closmask)
	li	$ptrtmp,closmask
	li	$atmp1,REQ_CALLC
	/* fall through */

set_request:
	move	$exhausted,$ptrtmp	/* save the register mask */
	lw	$ptrtmp,MLSTATE_OFFSET($sp)	/* save the minimal ML state */
	sw	$exhausted,mask_($ptrtmp)
	sw	$atmp1,request($ptrtmp)
	sw	$zero,inML($ptrtmp)	/* note that we have left ML */
	sw	$allocptr,allocptr_offset($ptrtmp)
	sw	$storeptr,storeptr_offset($ptrtmp)
	sw	$stdarg,stdarg_offset($ptrtmp)
	sw	$stdcont,stdcont_offset($ptrtmp)
	sw	$stdclos,stdclos_offset($ptrtmp)
	sw	$exncont,exncont_offset($ptrtmp)
#if CALLEESAVE > 0
	sw	$miscreg0,miscreg_offset(0)($ptrtmp)
#endif
#if CALLEESAVE > 1
	sw	$miscreg1,miscreg_offset(1)($ptrtmp)
#endif
#if CALLEESAVE > 2
	sw	$miscreg2,miscreg_offset(2)($ptrtmp)
#endif
#if CALLEESAVE > 3
	sw	$miscreg3,miscreg_offset(3)($ptrtmp)
#endif
#if CALLEESAVE > 4
	sw	$miscreg4,miscreg_offset(4)($ptrtmp)
#endif
#if CALLEESAVE > 5
	sw	$miscreg5,miscreg_offset(5)($ptrtmp)
#endif
#if CALLEESAVE > 6
	sw	$miscreg6,miscreg_offset(6)($ptrtmp)
#endif
#if CALLEESAVE > 7
	sw	$miscreg7,miscreg_offset(7)($ptrtmp)
#endif
#if CALLEESAVE > 8
	sw	$miscreg8,miscreg_offset(8)($ptrtmp)
#endif
	sw	$varptr,varptr_offset($ptrtmp)
restore_c_regs:
	lw	$31,argbuild+36($sp)	/* restore the C registers */
	lw	$30,argbuild+32($sp)
        lw      $23,argbuild+28($sp)
        lw      $22,argbuild+24($sp)
        lw      $21,argbuild+20($sp)
        lw      $20,argbuild+16($sp)
        lw      $19,argbuild+12($sp)
        lw      $18,argbuild+8($sp)
        lw      $17,argbuild+4($sp)
        lw      $16,argbuild($sp)
	addu	$sp,framesize		/* discard the stack frame */
	j	$31			/* return to run_ml() */

startgcptr: .word saveregs
	.globl	saveregs
	.ent	saveregs
saveregs:
	move	$exhausted,$ptrtmp	/* save the register mask */
	lw	$ptrtmp,MLSTATE_OFFSET($sp)	/* save the ML state */
	sw	$exhausted,mask_($ptrtmp)
	bnez	$limit,1f
	li      $limit,REQ_SIGNAL
	sw	$limit,request($ptrtmp)
1:	
	sw	$zero,inML($ptrtmp)		/* note that we have left ML */
	sub     $basereg, 32764			/* adjust baseReg */
	sw	$allocptr,allocptr_offset($ptrtmp)
	sw	$storeptr,storeptr_offset($ptrtmp)
	sw	$stdarg,stdarg_offset($ptrtmp)
	sw	$stdcont,stdcont_offset($ptrtmp)
	sw	$stdclos,stdclos_offset($ptrtmp)
	sw	$gclink,gclink_offset($ptrtmp)
	sw	$exncont,exncont_offset($ptrtmp)
	sw	$miscreg0,miscreg_offset(0)($ptrtmp)	/* save misc. roots */
	sw	$miscreg1,miscreg_offset(1)($ptrtmp)
	sw	$miscreg2,miscreg_offset(2)($ptrtmp)
	sw	$miscreg3,miscreg_offset(3)($ptrtmp)
	sw	$miscreg4,miscreg_offset(4)($ptrtmp)
	sw	$miscreg5,miscreg_offset(5)($ptrtmp)
	sw	$miscreg6,miscreg_offset(6)($ptrtmp)
	sw	$miscreg7,miscreg_offset(7)($ptrtmp)
	sw	$miscreg8,miscreg_offset(8)($ptrtmp)
	sw	$miscreg9,miscreg_offset(9)($ptrtmp)
	sw	$miscreg10,miscreg_offset(10)($ptrtmp)
	sw	$miscreg11,miscreg_offset(11)($ptrtmp)
	sw	$miscreg12,miscreg_offset(12)($ptrtmp)
	sw	$stdlink,stdlink_offset($ptrtmp)
	sw	$basereg,basereg_offset($ptrtmp)		/* base reg */
	sw	$varptr,varptr_offset($ptrtmp)
	b	restore_c_regs

	.end	saveregs

	.ent	restoreregs
restoreregs:
	subu	$sp,framesize		/* allocate a stack frame */
					/* save the C registers */
.frame $sp,framesize,$zero
.mask 0xc0ff0000,frameoffset
	lw	$5,startgcptr
	sw	$4,MLSTATE_OFFSET($sp)	/* save MLState ptr for return to C */
	sw	$5,STARTGC_OFFSET($sp) /* so ML can find saveregs! */
	sw	$31,argbuild+36($sp)
	sw	$30,argbuild+32($sp)
        sw      $23,argbuild+28($sp)
        sw      $22,argbuild+24($sp)
        sw      $21,argbuild+20($sp)
        sw      $20,argbuild+16($sp)
        sw      $19,argbuild+12($sp)
        sw      $18,argbuild+8($sp)
        sw      $17,argbuild+4($sp)
        sw      $16,argbuild($sp)
					
	move    $ptrtmp,$4             /* MLState ptr should be in */
                                       /* C standard arg thanks to run_ml */
	lw	$allocptr,allocptr_offset($ptrtmp)
	lw	$limit,limit_offset($ptrtmp)
	lw	$storeptr,storeptr_offset($ptrtmp)

	li	$atmp1,1
.set	noreorder			/* the order here is important */
	sw	$atmp1,inML($ptrtmp)	/* note that we are entering ML code */
	lw	$atmp1,GCpending($ptrtmp)	/* check for gc sync */
	nop
	beqz	$atmp1,6f
	nop
	li	$limit,0		/* adjust the limit register */
6:
	lw	$atmp1,NumPendingSigs($ptrtmp)	/* check for pending signals */
	nop				/* (load delay slot) */
	bnez	$atmp1,1f
	nop				/* (branch delay slot) */
8:	lw	$stdarg,stdarg_offset($ptrtmp)
	lw	$stdcont,stdcont_offset($ptrtmp)
	lw	$stdclos,stdclos_offset($ptrtmp)
	lw	$exncont,exncont_offset($ptrtmp)
	lw	$miscreg0,miscreg_offset(0)($ptrtmp)
	lw	$miscreg1,miscreg_offset(1)($ptrtmp)
	lw	$miscreg2,miscreg_offset(2)($ptrtmp)
	lw	$miscreg3,miscreg_offset(3)($ptrtmp)
	lw	$miscreg4,miscreg_offset(4)($ptrtmp)
	lw	$miscreg5,miscreg_offset(5)($ptrtmp)
	lw	$miscreg6,miscreg_offset(6)($ptrtmp)
	lw	$miscreg7,miscreg_offset(7)($ptrtmp)
	lw	$miscreg8,miscreg_offset(8)($ptrtmp)
	lw	$miscreg9,miscreg_offset(9)($ptrtmp)
	lw	$miscreg10,miscreg_offset(10)($ptrtmp)
	lw	$miscreg11,miscreg_offset(11)($ptrtmp)
	lw	$miscreg12,miscreg_offset(12)($ptrtmp)
	lw	$stdlink,stdlink_offset($ptrtmp)
	lw	$varptr,varptr_offset($ptrtmp)
	lw 	$basereg,basereg_offset($ptrtmp)
	lw	$gclink,gclink_offset($ptrtmp)
	add     $basereg,32764		/* adjust baseReg */
	slt	$exhausted,$allocptr,$limit
	.end	restoreregs
	.globl	go
	.ent	go
go:	j	$gclink			/* jump to ML code */
	nop
	.end	go
1:				      /* there are pending signals, */
	lw	$atmp1,maskSignals($ptrtmp)	/* are signal masked? */
	nop				/* (load delay slot) */
	bnez	$atmp1,8b
	nop				/* (branch delay slot) */
	lw	$atmp1,inSigHandler($ptrtmp)	/* check for a pending handler */
	nop				/* (load delay slot) */
	bnez	$atmp1,8b              
        li	$atmp1,1		/* (branch delay slot) */
	sw	$atmp1,handlerPending($ptrtmp)	/* note the pending handler */
	li	$limit,0		/* trap on the next limit check. */
	beqz 	$zero,8b
	nop
.set	reorder

	.text
	.ent savefpregs			/* Only called from signal.c */
	.set reorder
savefpregs:
	li      $15,MAKE_DESC(NSAVED_FPREGS*8, TAG_string)
	lw 	$14,allocptr_offset($4)
        sw      $15,0($14)      	/* string tag */
	swc1	$f20,4($14)		/* fpr20 */
	swc1	$f21,8($14)
	swc1	$f22,12($14)		/* fpr22 */
	swc1	$f23,16($14)
	swc1	$f24,20($14)		/* fpr24 */
	swc1	$f25,24($14)
	swc1	$f26,28($14)		/* fpr26 */
	swc1	$f27,32($14)
	swc1	$f28,36($14)		/* fpr28 */
	swc1	$f29,40($14)
	j 	$31			/* return */
	.end	savefpregs

	.ent	restorefpregs		/* Only called from signal.c */
	.set 	reorder
restorefpregs:				/* floats address passed as parm */
	lwc1	$f20,	0($4)		/* retrieve float registers */
	lwc1	$f21,	4($4)
	lwc1	$f22,	8($4)
	lwc1	$f23,	12($4)
	lwc1	$f24,	16($4)
	lwc1	$f25,	20($4)
	lwc1	$f26,	24($4)
	lwc1	$f27,	28($4)
	lwc1	$f28,	32($4)
	lwc1	$f29,	36($4)
	j	$31
	.end 	restorefpregs

/* try_lock : spin_lock -> bool
 * low-level primitive for mutual-exclusion among processors -- note,
 * the "atomicity" guaranteed by try_lock is only between processors and
 * not between signals.  
 */
ML_CODE_HDR(try_lock_a)
#if (MAX_PROCS > 1)
#ifdef SGI
.set noreorder
	lw	$stdarg,0($stdarg)
	nop
	sll	$stdarg,1
	xori	$stdarg,3
	andi	$stdarg,3
.set reorder
	CONTINUE
#endif SGI
#else (MAX_PROCS == 1)
	lw	$atmp1,0($stdarg)
	li	$atmp2,1		/* ML_false */
	sw	$atmp2,0($stdarg)
	move	$stdarg,$atmp1
	CONTINUE
#endif (MAX_PROCS > 1)

ML_CODE_HDR(unlock_a)
#if (MAX_PROCS > 1)
#ifdef SGI
	sw	$zero,0($stdarg)
	li	$stdarg,1
	CONTINUE
#endif SGI
#else (MAX_PROCS == 1)
	li	$atmp1,3		/* ML_true */
	sw	$atmp1,0($stdarg)
	li	$stdarg,1		/* just return unit */
	CONTINUE
#endif (MAX_PROCS > 1)

/* array : (int * 'a) -> 'a array
 * Allocate and initialize a new array.	 This can cause GC.
 */
ML_CODE_HDR(array_a)
	lw	$atmp1,0($stdarg)	/* tagged length in $atmp1 */
	lw	$atmp4,4($stdarg)		/* get initial value in $atmp4 */
	sra	$atmp1,1		/* untagged length */
	sll	$atmp2,$atmp1,width_tags /* build descriptor in $atmp2 */
	ori	$atmp2,TAG_array
	sll	$atmp1,2		/* get length in bytes into $atmp1 */
	sub	$atmp3,$limit,$allocptr	/* subtract allocptr */
	sub	$atmp3,$atmp3,$atmp1	/* subtract requested bytes */
	move	$gclink,$stdlink
	li	$ptrtmp,closmask
	blez	$atmp3,saveregs		/* do we have enough? */
	sw	$atmp2,0($allocptr)	/* store the descriptor */
	add	$allocptr,4		/* points to new object */
	add	$atmp3,$atmp1,$allocptr	/* beyond last word of new array */
	move	$stdarg,$allocptr	/* put ptr in return register */
					/* (return val = arg of continuation) */
2:					/* loop */
	sw	$atmp4,0($allocptr)	  /* store the value */
        addi	$allocptr,4		  /* on to the next word */
	bne	$allocptr,$atmp3,2b	  /* if not off the end, repeat */
					/* end loop */
        CONTINUE

/* create_b : int -> bytearray
 * create_r : int -> realarray
 * create_s : int -> string
 * Create bytearray or string of given length.	This can cause GC.
 */
ML_CODE_HDR(create_r_a)
	sra	$atmp1,$stdarg,1	/* $atmp1 = length */
	sll	$atmp1,3
	addi	$atmp1,4		/* length in bytes (including desc) */

	sub	$atmp3,$limit,$allocptr	/* subtract allocptr */
	sub	$atmp3,$atmp3,$atmp1	/* subtract requested bytes */
	move	$gclink,$stdlink
	li	$ptrtmp,closmask
	blez	$atmp3,saveregs		/* do we have enough? */
	sra	$atmp2,$stdarg,1	/* build descriptor in atmp2 */
	sll	$atmp2,width_tags
	ori	$atmp2,TAG_realdarray
	sw	$atmp2,0($allocptr)	/* store descriptor */
	addi	$stdarg,$allocptr,4	/* pointer to new string */
	add	$allocptr,$atmp1		/* advance allocptr */
	CONTINUE

ML_CODE_HDR(create_b_a)
	sra	$atmp1,$stdarg,1		/* $atmp1 = length */
	addi	$atmp1,$atmp1,7
	sra	$atmp1,2			/* length in words (including desc) */
	sll	$atmp1,2 			/* length in bytes (including desc) */

	sub	$atmp3,$limit,$allocptr	/* subtract allocptr */
	sub	$atmp3,$atmp3,$atmp1	/* subtract requested bytes */
	move	$gclink,$stdlink
	li	$ptrtmp,closmask
	blez	$atmp3,saveregs		/* do we have enough? */
	sra	$atmp2,$stdarg,1	/* build descriptor in atmp2 */
	sll	$atmp2,width_tags
	ori	$atmp2,TAG_bytearray
	sw	$atmp2,0($allocptr)	/* store descriptor */
	addi	$stdarg,$allocptr,4	/* pointer to new string */
	add	$allocptr,$atmp1		/* advance allocptr */
	CONTINUE

ML_CODE_HDR(create_s_a)
	sra	$atmp1,$stdarg,1		/* $atmp1 = length */
	addi	$atmp1,$atmp1,7
	sra	$atmp1,2			/* length in words (including desc) */
	sll	$atmp1,2 			/* length in bytes (including desc) */

	sub	$atmp3,$limit,$allocptr	/* subtract allocptr */
	sub	$atmp3,$atmp3,$atmp1	/* subtract requested bytes */
	move	$gclink,$stdlink
	li	$ptrtmp,closmask
	blez	$atmp3,saveregs		/* do we have enough? */
	sra	$atmp2,$stdarg,1	/* build descriptor in atmp2 */
	sll	$atmp2,width_tags
	ori	$atmp2,TAG_string
	sw	$atmp2,0($allocptr)	/* store descriptor */
	addi	$stdarg,$allocptr,4	/* pointer to new string */
	add	$allocptr,$atmp1		/* advance allocptr */
	CONTINUE

/* create_v_a : int * 'a list -> 'a vector
 * 	creates a vector with elements taken from a list.
 *	n.b. The frontend ensures that list cannot be nil.
 */
ML_CODE_HDR(create_v_a)
#define ML_NIL 		1
#define ML_LIST_HD(p)	0(p)
#define ML_LIST_TL(p) 	4(p)
	lw	$atmp1,0($stdarg)	/* atmp1 := tagged length */
	sra	$atmp1,1		/* untagged length */
	sll	$atmp2,$atmp1,width_tags/* build descriptor in $atmp2 */
	ori	$atmp2,TAG_record	/* tag field */
	sll	$atmp1,2		/* get length in bytes into $atmp1 */
	sub	$atmp3,$limit,$allocptr	/* subtract allocptr */
	sub	$atmp3,$atmp3,$atmp1	/* subtract requested bytes */
	move	$gclink,$stdlink
	li	$ptrtmp,closmask
	blez	$atmp3,saveregs		/* do we have enough? */
	sw	$atmp2,0($allocptr)	/* store the descriptor */
	addi	$allocptr,4		/* points to new object */
        lw      $atmp2, 4($stdarg)      /* atmp2 := list */
	move	$stdarg,$allocptr	/* return val = arg of continuation */
        li      $atmp3, ML_NIL          /* atmp3 := NIL */
3:					/* loop */
        lw      $atmp4, ML_LIST_HD($atmp2) /* atmp4 := data */
        sw      $atmp4, 0($allocptr)     /* update vector */
        lw      $atmp2, ML_LIST_TL($atmp2)
        addi    $allocptr, 4            /* next index */
        bne     $atmp2, $atmp3, 3b      /* reached end? */
        CONTINUE

#ifdef MIPSEL
#define BIGPART 4
#else
#define BIGPART 0
#endif
#define LITTLEPART (4-BIGPART)

/* Floating exceptions raised (assuming ROP's are never passed to functions):
 *	DIVIDE BY ZERO - (div)
 *	OVERFLOW/UNDERFLOW - (add,div,sub,mul) as appropriate
 *
 * floor raises integer overflow if the float is out of 32-bit range,
 * so the float is tested before conversion, to make sure it is in (31-bit)
 * range */
.set noreorder
maxint:	.double	1073741824.0
.set reorder
ML_CODE_HDR(floor_a)
	lwc1	$f4,LITTLEPART($stdarg)	/* get least significant word */
	lwc1	$f5,BIGPART($stdarg)	/* get most significant word */
	mtc1	$zero,$f2			/* ($f2,$f3) := maxint */
 	lui	$atmp3,0x41d0
	mtc1	$atmp3,$f3
	abs.d	$f6,$f4
	c.le.d	$f6,$f2
	cfc1	$atmp3,$31			/* grab fpa control register */
	bc1f	over
	ori	$atmp2,$atmp3,0x03		/* set rounding bits to 11 */
	ctc1	$atmp2,$31			/* return fpa control register */
	cvt.w.d $f6,$f4			/* convert to integer */
	ctc1	$atmp3,$31			/* return fpa control register */
	mfc1	$stdarg,$f6		/* get in std argument register */
	add	$stdarg,$stdarg		/* make room for tag bit */
	add	$stdarg,1		/* add the tag bit */
	CONTINUE


ML_CODE_HDR(logb_a)
	lw 	$stdarg,BIGPART($stdarg)/* most significant part */
	srl 	$stdarg,20		/* throw out 20 low bits */
	andi	$stdarg,0x07ff		/* clear all but 11 low bits */
	sub 	$stdarg,1023		/* subtract 1023 */
	sll 	$stdarg,1		/* make room for tag bit */
	add	$stdarg,1		/* add the tag bit */
	CONTINUE

ML_CODE_HDR(scalb_a)
	CHECKLIMIT(closmask)
	lw 	$atmp1,4($stdarg)	/* get tagged n */
	sra	$atmp1,1		/* get real n */
	beqz	$atmp1,9f		/* if zero, return the old float */
	lw	$ptrtmp,0($stdarg)	/* get pointer to float */
	lw 	$atmp2,BIGPART($ptrtmp)	/* most significant part */
	srl 	$atmp2,20		/* throw out 20 low bits */
	andi	$atmp2,0x07ff		/* clear all but 11 low bits */
	add	$atmp3,$atmp2,$atmp1	/* new := old + n */
	blt	$atmp3,1,under		/* punt if underflow */
	bgt	$atmp3,2046,over	/* or overflow */
	xor	$atmp3,$atmp2		/* at3 = new xor old */
	sll	$atmp3,20		/* put exponent in right position */
	lw	$atmp2,BIGPART($ptrtmp)	/* most significant word */
	xor	$atmp2,$atmp3		/* change to new exponent */
	sw	$atmp2,BIGPART+4($allocptr)	/* save */
	lw 	$atmp2,LITTLEPART($ptrtmp) /* get least significant word */
	sw	$atmp2,LITTLEPART+4($allocptr)	/* save lsw */
8:	li	$atmp4,DESC_reald        /* make descriptor */
	sw	$atmp4,0($allocptr)	/* save descriptor */
	add	$stdarg,$allocptr,4	/* get pointer to new float */
	add	$allocptr,12		/* point to new free word */
        CONTINUE

9:	lw	$stdarg,0($stdarg)	/* get old float */
	CONTINUE

over:	li	$atmp3,0x7fffffff
	add	$atmp3,$atmp3		/* generate overflow exception */

under:	sw	$zero,4($allocptr)		/* return 0.0 */
	sw	$zero,8($allocptr)
	b	8b

/* set_fsr:
 * Turn on floating-point overflow, underflow and zero-divide exceptions.
 */
	.globl	set_fsr
	.ent	set_fsr
set_fsr:
	cfc1	$atmp1,$31		/* grab fpa control register */
	ori 	$atmp1,$atmp1,0xe00	/* set V, O, and Z bits */
	ctc1	$atmp1,$31		/* return fpa control register */
	j	$31
	.end	set_fsr

/* this bogosity is for export.c */
	.globl	startptr
startptr: .word    __start
