/** IBM RS6000 assembly routines **/

#include "mask.h"
#include "tags.h"
#include "request.h"
#include "fpregs.h"

/* MLState vector
 *              +----------------------+
 *  MLState --> | ml_allocptr (14)     |
 *              +----------------------+
 *      +4:     | ml_limitptr (15)     |
 *              +----------------------+
 *      +8:     | ml_storeptr (16)     |
 *              +----------------------+
 *      +12:    | ml_stdlink (17)      |
 *              +----------------------+
 *      +16:    | ml_closure (18)      |
 *              +----------------------+
 *      +20:    | ml_arg (19)          |   
 *              +----------------------+
 *      +24:    | ml_cont (20)         |
 *              +----------------------+
 *      +28:    | ml_exn (21)          |
 *              +----------------------+
 *      +32:    | ml_varptr (22)       |
 *              +----------------------+
 *      +36:    | ml_basereg (23)      |
 *              +----------------------+
 *      +40:    | xxx (not used)       |
 *              +----------------------+
 *      +44:    | gclink (28)          |
 *              +----------------------+
 *      +48:    | misc_regs 24-27,3-13 |
 *		.		       .
 *		.		       .
 *		+----------------------+
 *		| inML		       |
 *		.		       .
 */

/* stackframe layout:
 * Note: The offset of cvti2d tmp is used in rs6000.sml
 *       float load/store offset is hardwired in rs6000instr.sml
 *
 * 	        +-------------------+
 *   sp--> 0(sp)| mlstate addr      |
 *	        +-------------------+
 *	   4(sp)| _startgc addr	    |
 *	        +-------------------+
 *	   8(sp)| cvti2d const	    |
 *	        +-------------------+
 *	  16(sp)| cvti2d tmp2	    |
 *	      	+-------------------+
 *	  24(sp)| float load/store  |
 *		+-------------------+
 *	  32(sp)| floor tmp	    |
 *		+-------------------+
 *  argblock(sp)| C calleesave regs |
 *	        .		    .
 *		.		    .
 */

/** register usage **/

#define		sp		1
#define 	stackptr	sp
#define		allocptr	14
#define 	limitptr	15
#define 	storeptr	16
#define		stdlink		17
#define 	stdclos		18
#define 	stdarg		19
#define 	stdcont		20
#define 	exncont		21
#define 	varptr		22
#define 	basereg		23
#define		miscreg0	24
#define		miscreg1	25
#define 	miscreg2	26
#define		miscreg3	27
#define		miscreg4	3
#define		miscreg5	4
#define		miscreg6	5
#define		miscreg7	6
#define		miscreg8	7
#define		miscreg9	8
#define		miscreg10	9
#define		miscreg11	10
#define		miscreg12	11
#define		miscreg13	12
#define		miscreg14	13
#define		gclink		28

#define       	atmp1 29
#define       	atmp2 30
#define       	atmp3 31
#define 	atmp4 miscreg13

/** MLState offsets **/
#define		NROOTS			24	/* from ml_state.h */

#define 	allocptr_offset 	0
#define 	limitptr_offset		4
#define 	storeptr_offset 	8
#define 	stdlink_offset		12
#define 	stdclos_offset  	16
#define		stdarg_offset   	20
#define 	stdcont_offset  	24
#define		exncont_offset  	28
#define 	varptr_offset		32
#define 	basereg_offset		36
#define 	used_slot_offset	40
#define		gclink_offset		44
#define		miscreg_offset(n) 	(48+4*(n)) 

#define 	inML_offset		(12+NROOTS*4)
#define 	request_offset		(inML_offset+4)
#define 	handlerPending_offset  	(inML_offset+8)
#define 	inSigHandler_offset    	(inML_offset+12)
#define 	maskSignals_offset     	(inML_offset+16)
#define 	NumPendingSigs_offset  	(inML_offset+20)
#define 	ioWaitFlag_offset      	(inML_offset+24)
#define 	GCpending_offset	(inML_offset+28)
#define 	mask_offset		(inML_offset+52)


#define argblock 		40
#define savearea		(23*4+4)	/* lr,cr,1,2,13-31,padding */
#define framesize		(argblock+savearea)
#define MLSTATE_OFFSET 		0	
#define STARTGC_OFFSET		4
#define CVTI2D_OFFSET		8
#define FLOOR_OFFSET		32

/** offsets in condition register CR.0 **/

#define CR0_LT 0
#define CR0_GT 1
#define CR0_EQ 2
#define CR0_SO 3
	
#define CR0	0

/** special registers **/

#define SPR_LR	8

#define ML_CODE_HDR(name)			\
	    .globl name;			\
	    .align  2;				\
	    .long   TAG_backptr;		\
    name:

#if (CALLEESAVE > 0)
#define CONTINUE					\
	    cmpl	CR0,limitptr,allocptr;		\
	    mtlr	stdcont;			\
	    br
#endif

#define CHECKLIMIT(mask,label)	 			\
	    bbt		CR0_GT, label;			\
	    l		atmp2,STARTGC_OFFSET(stackptr);	\
	    lil		atmp1,mask;			\
	    mtspr	SPR_LR, atmp2;			\
	    ai		gclink,stdlink,0;		\
	    br		;				\
    label:




/* create table of contents entries. */
	.toc
	.csect 	prim.s[BS]
	.extern .saveregs

	.csect  [RO]
	.toc
cvti2d_CONST:	
	.tc 	fd43300000_80000000[TC],1127219200,-2147483648
T.saveregs:	
	.tc	saveregs[TC],.saveregs


	.csect	[PR]
/* sig_return : ('a cont * 'a) -> 'b
 */
ML_CODE_HDR(sigh_return_a)
	lil 	atmp1,contmask
	lil 	atmp4,REQ_SIG_RETURN
	b	set_request

	.globl 	sigh_resume
sigh_resume:
	lil	atmp1,callcc_mask
	lil	atmp4, REQ_SIG_RESUME
        b	set_request

		 /* exception handler for ML functions called from C */
ML_CODE_HDR(handle_a)
	lil	atmp1,exnmask
	lil	atmp4,REQ_EXN
	b	set_request


		/* continuation for ML functions called from C */		
ML_CODE_HDR(return_a)
	lil	atmp1,contmask
	lil	atmp4,REQ_RETURN
	b	set_request


	.globl request_fault

request_fault:
	lil	atmp1,exnmask
	lil	atmp4,REQ_FAULT
	b	set_request

ML_CODE_HDR(callc_a)
	CHECKLIMIT(closmask,callc_v_limit) 
	lil	atmp1,closmask
	lil	atmp4,REQ_CALLC
	b	set_request

set_request:
	l	atmp3,MLSTATE_OFFSET(sp)	/* save the minimal ML state */
	st	atmp4,request_offset(atmp3)	/* request */
	st	atmp1,mask_offset(atmp3)	/* mask */
	lil	0,0				/* reg 0 <- 0 */
	st	0,inML_offset(atmp3)		/* note that we have left ML */
	st	allocptr,allocptr_offset(atmp3)
	st	storeptr,storeptr_offset(atmp3)
	st	stdarg,stdarg_offset(atmp3)
	st	stdcont,stdcont_offset(atmp3)
	st	stdclos,stdclos_offset(atmp3)
	st	exncont,exncont_offset(atmp3)
#if CALLEESAVE > 0
	st	miscreg0,miscreg_offset(0)(atmp3)
#endif
#if CALLEESAVE > 1
	st	miscreg1,miscreg_offset(1)(atmp3)
#endif
#if CALLEESAVE > 2
	st	miscreg2,miscreg_offset(2)(atmp3)
#endif
#if CALLEESAVE > 3
	??
#endif
	st	varptr,varptr_offset(atmp3)

restore_c_regs:
 	l	2, (argblock+4)(sp) 
	l	13, (argblock+8)(sp)
	l	14, (argblock+12)(sp)
	l 	15, (argblock+16)(sp)
	l	16, (argblock+20)(sp)
	l	17, (argblock+24)(sp)
	l	18, (argblock+28)(sp)
	l 	19, (argblock+32)(sp)
	l 	20, (argblock+36)(sp)
	l	21, (argblock+40)(sp)
	l 	22, (argblock+44)(sp)
	l 	23, (argblock+48)(sp)
	l 	24, (argblock+52)(sp)
	l	25, (argblock+56)(sp)
	l 	26, (argblock+60)(sp)
	l	27, (argblock+64)(sp)
	l	28, (argblock+68)(sp)
	l 	29, (argblock+72)(sp)
	l	30, (argblock+76)(sp)
	l 	31, (argblock+80)(sp)
	l	0, (argblock+84)(sp)
	mtlr    0
	l	0, (argblock+88)(sp)
	mtcr    0
	ai	sp,sp,framesize 
	br

	.globl .saveregs
	.csect [PR]
.saveregs:
	l	atmp3,MLSTATE_OFFSET(sp)
	st	atmp1,mask_offset(atmp3)
 	cmpi	CR0,limitptr,0
	bbf	CR0_EQ, saveregs_1
	lil	limitptr,REQ_SIGNAL
	st	limitptr, request_offset(atmp3)	
saveregs_1:
	lil	0,0
	st	0,inML_offset(atmp3)
	ai	basereg, basereg, -32764
	st	allocptr,allocptr_offset(atmp3)
	st	storeptr,storeptr_offset(atmp3)
	st	stdarg,stdarg_offset(atmp3)
	st	stdcont,stdcont_offset(atmp3)
	st	stdclos,stdclos_offset(atmp3)
	st	gclink,gclink_offset(atmp3)
	st	exncont,exncont_offset(atmp3)
	st	miscreg0,miscreg_offset(0)(atmp3)	/* save misc. roots */
	st	miscreg1,miscreg_offset(1)(atmp3)
	st	miscreg2,miscreg_offset(2)(atmp3)
	st	miscreg3,miscreg_offset(3)(atmp3)
	st	miscreg4,miscreg_offset(4)(atmp3)
	st	miscreg5,miscreg_offset(5)(atmp3)
	st	miscreg6,miscreg_offset(6)(atmp3)
	st	miscreg7,miscreg_offset(7)(atmp3)
	st	miscreg8,miscreg_offset(8)(atmp3)
	st	miscreg9,miscreg_offset(9)(atmp3)
	st	miscreg10,miscreg_offset(10)(atmp3)
	st	miscreg11,miscreg_offset(11)(atmp3)
	st	miscreg12,miscreg_offset(12)(atmp3)
	st	miscreg13,miscreg_offset(13)(atmp3)
	st	miscreg14,miscreg_offset(14)(atmp3)
	st	stdlink,stdlink_offset(atmp3)
	st	basereg,basereg_offset(atmp3)		/* base reg */
	st	varptr,varptr_offset(atmp3)
	b	restore_c_regs


	.globl 	.restoreregs
.restoreregs:
	ai	sp,sp,-framesize
	l	0,T.saveregs(2)
	st	3, MLSTATE_OFFSET(sp)
	st	0, STARTGC_OFFSET(sp)
	lfd	0, cvti2d_CONST(2)
	stfd	0, CVTI2D_OFFSET(sp)

	st	2, argblock+4(sp)
	st	13, argblock+8(sp)
	st	14, argblock+12(sp)
	st 	15, argblock+16(sp)
	st	16, argblock+20(sp)
	st	17, argblock+24(sp)
	st	18, argblock+28(sp)
	st 	19, argblock+32(sp)
	st 	20, argblock+36(sp)
	st	21, argblock+40(sp)
	st 	22, argblock+44(sp)
	st 	23, argblock+48(sp)
	st 	24, argblock+52(sp)
	st	25, argblock+56(sp)
	st 	26, argblock+60(sp)
	st	27, argblock+64(sp)
	st	28, argblock+68(sp)
	st 	29, argblock+72(sp)
	st	30, argblock+76(sp)
	st 	31, argblock+80(sp)
	mflr    0
	st	0,  argblock+84(sp)
	mfcr	0
	st	0,  argblock+88(sp)
	
	and	atmp1,3,3			/* atmp1 := MLState pointer */

	l	allocptr,allocptr_offset(atmp1)
	l	limitptr,limitptr_offset(atmp1)
	l	storeptr,storeptr_offset(atmp1)

	lil	atmp2,1
				  	  /* the order here is important */
	st	atmp2,inML_offset(atmp1)  /* note that we are entering ML code */
					  /* check for gc sync */
	l	atmp2,GCpending_offset(atmp1)	
	cmpi    CR0,atmp2,0
	bbt	CR0_EQ,restoreregs_6
	lil	limitptr,0		  /* adjust the limit register */

restoreregs_6:
	l	atmp2,NumPendingSigs_offset(atmp1)/* check for pending signals */
	cmpi	CR0,atmp2,0
	bbf	CR0_EQ,restoreregs_1

restoreregs_8:
	l	stdarg,stdarg_offset(atmp1)
	l	stdcont,stdcont_offset(atmp1)
	l	stdclos,stdclos_offset(atmp1)
	l	exncont,exncont_offset(atmp1)
	l	miscreg0,miscreg_offset(0)(atmp1)
	l	miscreg1,miscreg_offset(1)(atmp1)
	l	miscreg2,miscreg_offset(2)(atmp1)
	l	miscreg3,miscreg_offset(3)(atmp1)
	l	miscreg4,miscreg_offset(4)(atmp1)
	l	miscreg5,miscreg_offset(5)(atmp1)
	l	miscreg6,miscreg_offset(6)(atmp1)
	l	miscreg7,miscreg_offset(7)(atmp1)
	l	miscreg8,miscreg_offset(8)(atmp1)
	l	miscreg9,miscreg_offset(9)(atmp1)
	l	miscreg10,miscreg_offset(10)(atmp1)
	l	miscreg11,miscreg_offset(11)(atmp1)
	l	miscreg12,miscreg_offset(12)(atmp1)
	l	miscreg13,miscreg_offset(13)(atmp1)
	l	miscreg14,miscreg_offset(14)(atmp1)
	l	stdlink,stdlink_offset(atmp1)
	l	varptr,varptr_offset(atmp1)
	l 	basereg,basereg_offset(atmp1)
	l	gclink,gclink_offset(atmp1)
	ai      basereg,basereg,32764		/* adjust baseReg */
	cmpl	CR0,limitptr,allocptr
	mtlr	gclink

	mtfsfi  3,0			/* Ensure that no exceptions are set */
	mtfsfi  2,0
	mtfsfi  1,0
	mtfsfi  0,0
	lil	0,0
	mtxer   0
					/* fall through */
	.globl	go 
go:	

	br				/* jump to ML code */

restoreregs_1:			        /* there are pending signals, */
	l	atmp2,maskSignals_offset(atmp1)	/* are signal masked? */
	cmpi	CR0,atmp2,0
	bbf	CR0_EQ,restoreregs_8

					/* check for a pending handler */
	l	atmp2,inSigHandler_offset(atmp1) 
	cmpi	CR0,atmp2,0
        lil	atmp2,1			/* This seems unnecessary */
	bbf	CR0_EQ, restoreregs_8
	st	atmp2,handlerPending_offset(atmp1)
					/* note the pending handler */
	lil	limitptr,0		/* trap on the next limit check. */
	b	restoreregs_8


	.globl	.savefpregs
	.align 2

ML_CODE_HDR(array_a)
	l	atmp2,0(stdarg)		/* atmp2 := tagged length */
	l	atmp4,4(stdarg)		/* atmp4 := get initial value */
	srai	atmp2,atmp2,1		/* atmp2 := untagged length */
	sli	atmp3,atmp2,width_tags  /* atmp3 := build descriptor */
	oril	atmp3,atmp3,TAG_array
	sli	atmp2,atmp2,2		/* atmp2 := length in bytes */
	a     	0,atmp2,allocptr
	cmpl	CR0,limitptr,0		/* enough space */
	ai	gclink,stdlink,0
	lil	atmp1,closmask
	bbt	CR0_LT,.saveregs
	st	atmp3,0(allocptr)	/* store the descriptor */
	ai	allocptr,allocptr,4	/* points to new object */
	a	atmp1,atmp2,allocptr	/* beyond last word of new array */
	ai	stdarg,allocptr,0	/* put ptr in return register */
					/* (return val = arg of continuation) */
array_a_2:				/* loop */
	st	atmp4,0(allocptr)	/* store the value */
        ai	allocptr,allocptr,4	/* on to the next word */
	cmp	CR0,allocptr,atmp1
	bbf	CR0_EQ, array_a_2	/* if not off the end, repeat */
					/* end loop */
	CONTINUE

ML_CODE_HDR(create_b_a)
	srai	atmp2,stdarg,1		/* atmp1 = length */
	ai	atmp2,atmp2,7
	srai	atmp2,atmp2,2		/* length in words (including desc) */
	sli	atmp2,atmp2,2		/* length in bytes (including desc) */
	a	atmp3,atmp2,allocptr
	cmpl	CR0,limitptr,atmp3
	ai	gclink,stdlink,0
	lil	atmp1,closmask
        bbt     CR0_LT,.saveregs		/* do we have enough? */

	srai	atmp3,stdarg,1		/* build descriptor in atmp3 */
	sli	atmp3,atmp3,width_tags
	oril	atmp3,atmp3,TAG_bytearray
	st	atmp3,0(allocptr)	/* store descriptor */
	ai	stdarg,allocptr,4	/* pointer to new string */
	a	allocptr,allocptr,atmp2	/* advance allocptr */
	CONTINUE

ML_CODE_HDR(create_r_a)
	srai	atmp2,stdarg,1		/* atmp1 = length */
	sli	atmp2,atmp2,3
	ai	atmp2,atmp2,4		/* length in bytes (including desc) */

	a	atmp3,atmp2,allocptr
	cmpl	CR0,limitptr,atmp3
	ai	gclink,stdlink,0
	lil	atmp1,closmask
	bbt     CR0_LT,.saveregs	/* do we have enough? */
	srai	atmp1,stdarg,1		/* build descriptor in atmp1 */
	sli	atmp1,atmp1,width_tags
	oril	atmp1,atmp1,TAG_realdarray
	st	atmp1,0(allocptr)	/* store descriptor */
	ai	stdarg,allocptr,4	/* pointer to new string */
	a	allocptr,allocptr,atmp2	/* advance allocptr */
	CONTINUE

/*
** create_s_a: int -> string
*/
ML_CODE_HDR(create_s_a)
	srai	atmp2,stdarg,1		/* atmp1 = length */
	ai	atmp2,atmp2,7
	srai	atmp2,atmp2,2		/* length in words (including desc) */
	sli	atmp2,atmp2,2		/* length in bytes (including desc) */

	a	atmp3,atmp2,allocptr
	cmpl	CR0,limitptr,atmp3
	ai	gclink,stdlink,0
	lil	atmp1,closmask
	bbt	CR0_LT,.saveregs
	srai	atmp1,stdarg,1		/* build descriptor in atmp2 */
	sli	atmp1,atmp1,width_tags
	oril	atmp1,atmp1,TAG_string
	st	atmp1,0(allocptr)	/* store descriptor */
	ai	stdarg,allocptr,4	/* pointer to new string */
	a	allocptr,allocptr,atmp2		/* advance allocptr */
	CONTINUE

ML_CODE_HDR(create_v_a)
#define ML_NIL 		1
#define ML_LIST_HD(p)	0(p)
#define ML_LIST_TL(p) 	4(p)
	l	atmp1,0(stdarg)		/* atmp1 := tagged length */
	srai	atmp1,atmp1,1		/* untagged length */
	sli	atmp2,atmp1,width_tags	/* build descriptor in atmp2 */
	oril	atmp2,atmp2,TAG_record	/* tag field */
	sli	atmp3,atmp1,2		/* get length in bytes into atmp1 */
	a	0,atmp3,allocptr
	cmpl	CR0,limitptr,0
	ai	gclink,stdlink,0
	lil	atmp1,closmask
	bbt	CR0_LT,.saveregs		/* do we have enough? */
	st	atmp2,0(allocptr)	/* store the descriptor */
	ai	allocptr,allocptr,4	/* points to new object */
        l       atmp2, 4(stdarg)        /* atmp2 := list */
	ai	stdarg,allocptr,0	/* return val = arg of continuation */
        lil     atmp1, ML_NIL           /* atmp3 := NIL */
create_v_v_1:				/* loop */
        l       atmp4, ML_LIST_HD(atmp2)/* atmp4 := data */
        st      atmp4, 0(allocptr)      /* update vector */
        l       atmp2, ML_LIST_TL(atmp2)
        ai      allocptr, allocptr, 4   /* next index */
	cmp	CR0,atmp2,atmp1
	bbf	CR0_EQ, create_v_v_1	/* reached end? */
	CONTINUE



	.csect	[RO]
	.toc
floor_CONST:	.tc	fd43300800_0[TC],0x43300800, 0
floor_MAXINT:	.tc	fd41d00000_0[TC],0x41d00000, 0

	.csect 	[PR]
	/*
	** floor_a : real -> int
	**	The overflow test here is not quite 
	**	accurate. MINIT = -MAXINT instead of 
	**      MININT = -(MAXINT + 1). Should fix this
	**	eventually!!
	**
	**	This code essentially loads 1.0*2^52 into 
	**	register f3. A floating add will internally 
	**	perform an exponent alignment, which will 
	**	bring the required bits into the mantissa.
	*/
ML_CODE_HDR(floor_a)
	lfd	1, 0(stdarg)		
	lfd	0, floor_MAXINT(2)
	fabs	5, 1
	fcmpo	CR0,5,0
	bbt	CR0_GT,floor_overflow

	/*
	** Neat thing here is that this code works for
	** both +ve and -ve floating point numbers.
	*/
	mffs	0
	stfd	0,0(allocptr)	/* steal the allocptr for a second */
	l 	0, 4(allocptr)
	mtfsb1	30
	mtfsb1 	31
	lfd	3,floor_CONST(2)	
	fa	6,1,3
	stfd	6,FLOOR_OFFSET(sp)
	l	stdarg,FLOOR_OFFSET+4(sp)
	a	stdarg,stdarg,stdarg
	ai	stdarg,stdarg,1
	
	andil.	0,0, 0xf
	mtfs	0
	CONTINUE

floor_overflow:
	teq	0,0



ML_CODE_HDR(logb_a)
	l 	stdarg,0(stdarg)  	/* most significant part */
	srai 	stdarg,stdarg,20	/* throw out 20 low bits */
	andil.	stdarg,stdarg,0x07ff	/* clear all but 11 low bits */
	bbt	CR0_EQ, logb_2
	ai	stdarg,stdarg,-1023	/* subtract 1023 */
	sli	stdarg,stdarg,1		/* make room for tag bit */
	ai	stdarg,stdarg,1		/* add the tag bit */
	CONTINUE
logb_2:
	lil	stdarg,1
	CONTINUE


/*
** scalb : real * int -> real
**	scalb(x,y) = x * 2^y
*/
ML_CODE_HDR(scalb_a)
	CHECKLIMIT(closmask,scalb_v_limit)
	l	atmp1,4(stdarg)		/* atmp1 := y */
	srai	atmp1,atmp1,1		/* atmp1 := machine int y */
	l	stdarg,0(stdarg)	/* stdarg := x */
	l	atmp2,0(stdarg)		/* atmp2 := MSW(x) */
	liu	0,0x7ff0		/* r0 := 0x7ff0,0000 */
	and.	atmp3,atmp2,0		/* atmp3 := atmp2 & 0x7ff00000 */
	bbt	CR0_EQ,scalb_all_done
	
	srai	atmp3,atmp3,20		/* atmp3 := ieee(exp) */
	a.	atmp1,atmp1,atmp3	/* scale exponent */
	bbt	CR0_LT,scalb_underflow

	cmpi	CR0,atmp1,2047		/* max. ieee(exp) */
	bbf	CR0_LT,scalb_overflow

	sfi	0,0,-1			/* r0 := not(r0) */
	and	atmp2,atmp2,0		/* atmp2 := high mantessa bits + sign */
	sli	atmp1,atmp1,20		/* atmp1 := new exponent */
	or	atmp1,atmp1,atmp2	/* atmp1 := new MSB(x) */
	l 	atmp2, 4(stdarg)	

scalb_write_out:
	st	atmp1, 4(allocptr)
	st	atmp2, 8(allocptr)
	lil	atmp3, DESC_reald
	st	atmp3, 0(allocptr)
	ai	stdarg,allocptr,4
	ai	allocptr,allocptr,12

scalb_all_done:
	CONTINUE

scalb_underflow:
	lil	atmp1,0
	lil	atmp2,0
	b	scalb_write_out

scalb_overflow:
	mtfsb1 	3



ML_CODE_HDR(try_lock_a)
	l	atmp1,0(stdarg)
	lil	atmp2,1			/* ML_false */
	st	atmp2,0(stdarg)
	ai	stdarg,atmp1,0
	CONTINUE


ML_CODE_HDR(unlock_a)
	lil	atmp1,3			/* ML_true */
	st	atmp1,0(stdarg)
	lil	stdarg,1		/* just return unit */
	CONTINUE



/* savefpregs and restorefpregs are called from C only. */
#define ctmp1 12
#define ctmp2 11
#define ctmp3 10


.savefpregs:
	lil	ctmp1, MAKE_DESC(NSAVED_FPREGS*8, TAG_string)	
	l	ctmp2, allocptr_offset(3)
	st	ctmp1, 0(ctmp2)		/* tag string */
	stfd	14, 4(ctmp2)
	stfd	15, 12(ctmp2)
	stfd	16, 20(ctmp2)
	stfd	17, 28(ctmp2)
	stfd	18, 36(ctmp2)
	stfd	19, 44(ctmp2)
	stfd	20, 52(ctmp2)
	stfd	21, 60(ctmp2)
	stfd	22, 68(ctmp2)
	stfd	23, 76(ctmp2)
	stfd	24, 84(ctmp2)
	stfd	25, 92(ctmp2)
	stfd	26, 100(ctmp2)
	stfd	27, 108(ctmp2)
	stfd	28, 116(ctmp2)
	stfd	29, 124(ctmp2)
	stfd	30, 132(ctmp2)
	stfd	31, 140(ctmp2)

	br

	.globl .restorefpregs
	.align 2
.restorefpregs:
	lfd	14, 0(3)
	lfd	15, 8(3)
	lfd	16, 16(3)
	lfd	17, 24(3)
	lfd	18, 32(3)
	lfd	19, 40(3)
	lfd	20, 48(3)
	lfd	21, 56(3)
	lfd	22, 64(3)
	lfd	23, 72(3)
	lfd	24, 80(3)
	lfd	25, 88(3)
	lfd	26, 96(3)
	lfd	27, 104(3)
	lfd	28, 112(3)
	lfd	29, 120(3)
	lfd	30, 128(3)
	lfd	31, 136(3)
	br


/* iCacheLineSize
 */
	.globl .iCacheLineSize
.iCacheLineSize:
	clcs	3,0xc
	br


/* 
** flushICache(start,size)
**	- based on i-cache line size.
*/
	.globl .FlushICache
.FlushICache:
	mfcr	0			/* save condition register into 0 */
	a	ctmp1,3,4		/* ctmp1 := final address */

					/* align address to cache line size */
	clcs    ctmp3,0xc		/* ctmp3 := I-cache line size */
	ai	ctmp3,ctmp3,-1		/* ctmp3 := ctmp3 -1  */
	sfi	ctmp3,ctmp3,-1		/* ctmp3 := 1's complement(ctmp3) */
	and	3,3,ctmp3
	clcs	ctmp3, 0xc		/* ctmp3 := I-cache line size */
	
FlushICache_1:
	clf	0,3			/* clear cache line */
	a 	3,3,ctmp3		/* addr := addr + icache line size */

	cmp	CR0,3,ctmp1		/* done ? */
	bbt	CR0_LT,FlushICache_1	/* repeat if < */

	mtcr	0			/* restore condition register */
	br				/* return */

