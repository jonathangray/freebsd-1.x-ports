; Copyright 1992 Scott Draves 

; a module compiles into this:
; fn () => (["a", "b"], fn [a, b] => top-level-thing)
; though it seems to actually look more like
; fn () => (fn [a, b] => top-level-thing, ["a", "b"])

#include <machine/trap.h>
#include "tags.h"
#include "request.h"
#include "mask.h"
#include "cstack.h"

; HPPA runtime code for ML.  Registers are used as follows:

#define zero      %r0
#define sp        %r30  		/* stack */
#define dp        %r27  		/* global data */
#define exncont   %r29  		/* exception handler (ml_exncont) */
#define allocptr  %r23  		/* freespace pointer (ml_allocptr) */
#define storeptr  %r22  		/* store pointer     (ml_storeptr) */
#define limit     %r19  		/* heap limit pointer (ml_limitptr)*/
#define stdarg    %r2   		/* standard argument (ml_arg)  	*/
#define stdcont   %r3   		/* standard continuation (ml_cont) */
#define stdclos   %r4   		/* standard closure  (ml_clos)	*/
#define baseptr   %r24  		/* base code pointer (ml_roots[])  */
#define varptr    %r20  		/* var pointer       (ml_varptr)   */
#define stdlink   %r5
#define miscreg0  %r6   		/* misc registers (ml_roots[]) */
#define miscreg1  %r7   		
#define miscreg2  %r8
#define miscreg3  %r9
#define miscreg4  %r10
#define miscreg5  %r11
#define miscreg6  %r12
#define miscreg7  %r13
#define miscreg8  %r14
#define miscreg9  %r15
#define miscreg10 %r16
#define miscreg11 %r17
#define exhausted %r18
#define tmp1      %r18
#define tmp2      %r21
#define maskreg   %r25
#define tmp3      %r25
#define tmp4      %r26
#define gclink    %r31 			/* link register  (ml_pc) */
#define rp	  %r2   		/* return pointer */
#define milli_rp  %r31
#define milli_rtn %r29
#define fpsr	  %fr0L

; offsets in the MLState structure

#define allocptr_offset	   0
#define limit_offset	   4
#define storeptr_offset	   8

#define gclink_offset	  12
#define stdarg_offset	  16
#define stdcont_offset	  20
#define stdclos_offset	  24
#define baseptr_offset	  28
#define stdlink_offset    32
#define varptr_offset	  36
#define exncont_offset	  40
#define	miscreg_offset(i) 44+4*(i)

#define inML            96
#define request        100
#define handlerPending 104
#define inSigHandler   108
#define maskSignals    112
#define NumPendingSigs 116
#define ioWaitFlag     120
#define GCpending      124
#define saved_pc       128
#define mask_offset    148

; C arguments
#define c_arg0	%r26
#define c_arg1	%r25
#define c_arg2	%r24
#define c_arg3	%r23



/* 0+ in c_reg_offset is because HP's retarded assembler can't
   handle expressions that begin with a paren
*/


/* concat(aaa,bbb) => aaabbb
   we use this instead of local labels because HP's "special" assembler
   doesn't support them
*/

#define concat(x, y) x/**/y

#define ML_CODE_HDR(name) 	 \
	.export  name,DATA	!\
	.align   4		!\
	.word    TAG_backptr	!\
	.label   name

#if (CALLEESAVE > 0)
#define CONTINUE			  		 \
	comclr,>=	allocptr, limit, exhausted 	!\
	ldi		1, exhausted	     	    	!\
	bv,n		zero(stdcont)			!\
	nop
#else
#define CONTINUE			  		 \
	ldw		0(stdcont), gclink 	   	!\
	comclr,>=	allocptr, limit, exhausted  	!\
	ldi		1, exhausted        	     	!\
	bv,n		zero(gclink)
#endif

#define CHECKLIMIT(name,mask)				 	 \
	combf,=,n	zero,exhausted,concat(L$$,name)  	!\
	ldw		startgc_offset(sp), tmp1          	!\
	ldi		mask, maskreg		           	!\
	copy		stdlink, gclink		            	!\
	bv,n		zero(tmp1)		             	!\
	.label concat(L$$,name)


;======================= data quadrand ========================
		.data

;;; sigh_return_c:
; The return continuation for the ML signal handler.

	ML_CODE_HDR(sigh_return_a)
	ldi	contmask, maskreg
	b	set_request
	ldi	REQ_SIG_RETURN, tmp4

;;; sigh_resume:
; Resume execution at the point at which a handler trap occurred.  This is a
; standard two-argument function, thus the closure is in ml_cont (%stdcont).

	.export sigh_resume,DATA
sigh_resume
	ldi     callcc_mask,maskreg
	b	set_request
	ldi	REQ_SIG_RESUME, tmp4

	ML_CODE_HDR(handle_a)
	ldi	exnmask, maskreg
	b	set_request
	ldi	REQ_EXN, tmp4


	ML_CODE_HDR(return_a)
	ldi	contmask, maskreg
	b	set_request
	ldi	REQ_RETURN, tmp4

	.export request_fault,DATA
request_fault
	ldi	exnmask, maskreg
	b	set_request
	ldi	REQ_FAULT, tmp4


	ML_CODE_HDR(callc_a)
	CHECKLIMIT(callc_a,closmask)
	ldi	closmask, maskreg
	ldi	REQ_CALLC, tmp4
	; fall through

	.export set_request,DATA
set_request					; a quick return to run_ml()
	ldw	mlstate_offset(sp), tmp2	; get MLState ptr from stack
	stw	tmp4, request(tmp2)
	stw	maskreg, mask_offset(tmp2)
	stw	zero, inML(tmp2)
	stw	allocptr, allocptr_offset(tmp2)
	stw	storeptr, storeptr_offset(tmp2)
	stw	stdarg, stdarg_offset(tmp2)
	stw	stdcont, stdcont_offset(tmp2)
	stw	stdclos, stdclos_offset(tmp2)
	stw	exncont, exncont_offset(tmp2)
#if CALLEESAVE > 0
	stw	miscreg0, miscreg_offset(0)(tmp2)
#endif
#if CALLEESAVE > 1
	stw	miscreg1,miscreg_offset(1)(tmp2)
#endif
#if CALLEESAVE > 2
	stw	miscreg2,miscreg_offset(2)(tmp2)
#endif
#if CALLEESAVE > 3
	???
#endif
	stw	varptr, varptr_offset(tmp2)


	.export restore_c_regs,DATA
restore_c_regs
	ldw	c_reg_offset(3)(sp), %r3	; restore all the c callee save regs
	ldw	c_reg_offset(4)(sp), %r4
	ldw	c_reg_offset(5)(sp), %r5
	ldw	c_reg_offset(6)(sp), %r6
	ldw	c_reg_offset(7)(sp), %r7
	ldw	c_reg_offset(8)(sp), %r8
	ldw	c_reg_offset(9)(sp), %r9
	ldw	c_reg_offset(10)(sp), %r10
	ldw	c_reg_offset(11)(sp), %r11
	ldw	c_reg_offset(12)(sp), %r12
	ldw	c_reg_offset(13)(sp), %r13
	ldw	c_reg_offset(14)(sp), %r14
	ldw	c_reg_offset(15)(sp), %r15
	ldw	c_reg_offset(16)(sp), %r16
	ldw	c_reg_offset(17)(sp), %r17
	ldw	c_reg_offset(18)(sp), %r18
	ldw	rp_offset - ml_framesize(sp), rp ; reach over our frame into caller's
	ldsid	(rp), %r1
	mtsp	%r1, %sr1
	ble	0(%sr1, rp)
	ldo	-ml_framesize(sp), sp		; delete our frame


	; call this for gc
	.export saveregs,DATA
saveregs
	ldw	mlstate_offset(sp), tmp2
	stw	maskreg, mask_offset(tmp2)
	comibf,= 0, limit, saveregs1
	ldi	REQ_SIGNAL, limit
	stw	limit, request(tmp2)
saveregs1
	stw	zero, inML(tmp2)		; no longer in ML
	stw	gclink, gclink_offset(tmp2)
	; bias base register XXX
	stw	allocptr, allocptr_offset(tmp2)
	stw	storeptr, storeptr_offset(tmp2)
	stw	stdarg, stdarg_offset(tmp2)
	stw	stdcont, stdcont_offset(tmp2)
	stw	stdclos, stdclos_offset(tmp2)
	stw	exncont, exncont_offset(tmp2)
	stw	miscreg0, miscreg_offset(0)(tmp2)
	stw	miscreg1, miscreg_offset(1)(tmp2)
	stw	miscreg2, miscreg_offset(2)(tmp2)
	stw	miscreg3, miscreg_offset(3)(tmp2)
	stw	miscreg4, miscreg_offset(4)(tmp2)
	stw	miscreg5, miscreg_offset(5)(tmp2)
	stw	miscreg6, miscreg_offset(6)(tmp2)
	stw	miscreg7, miscreg_offset(7)(tmp2)
	stw	miscreg8, miscreg_offset(8)(tmp2)
	stw	miscreg9, miscreg_offset(9)(tmp2)
	stw	miscreg10, miscreg_offset(10)(tmp2)
	stw	miscreg11, miscreg_offset(11)(tmp2)
	stw	stdlink, stdlink_offset(tmp2)
	stw	baseptr, baseptr_offset(tmp2)
	b	restore_c_regs
	stw	varptr, varptr_offset(tmp2)



;;; restoreregs

	.export restoreregs,ENTRY
restoreregs
	stw	rp, rp_offset(sp)	; save return addr in caller's frame
	ldo	ml_framesize(sp), sp	; create a new frame for ourselves
	stw	%r3, c_reg_offset(3)(sp); save all the c callee save regs
	stw	%r4, c_reg_offset(4)(sp)
	stw	%r5, c_reg_offset(5)(sp)
	stw	%r6, c_reg_offset(6)(sp)
	stw	%r7, c_reg_offset(7)(sp)
	stw	%r8, c_reg_offset(8)(sp)
	stw	%r9, c_reg_offset(9)(sp)
	stw	%r10, c_reg_offset(10)(sp)
	stw	%r11, c_reg_offset(11)(sp)
	stw	%r12, c_reg_offset(12)(sp)
	stw	%r13, c_reg_offset(13)(sp)
	stw	%r14, c_reg_offset(14)(sp)
	stw	%r15, c_reg_offset(15)(sp)
	stw	%r16, c_reg_offset(16)(sp)
	stw	%r17, c_reg_offset(17)(sp)
	stw	%r18, c_reg_offset(18)(sp)
	stw	c_arg0, mlstate_offset(sp); save msp, our sole C argument
	ldil	L%saveregs, %r3
	ldo	R%saveregs(%r3), %r3
	ldil	L%ml_mul, %r4
	ldo	R%ml_mul(%r4), %r4
	ldil	L%ml_div, %r5
	ldo	R%ml_div(%r5), %r5
	stw	%r3, startgc_offset(sp)
	stw	%r4, mul_offset(sp)
	stw	%r5, div_offset(sp)
	copy	c_arg0, tmp1			; msp -> tmp1
	ldw	stdarg_offset(tmp1), stdarg
	ldw	stdcont_offset(tmp1), stdcont
	ldw	stdclos_offset(tmp1), stdclos
	ldw	exncont_offset(tmp1), exncont
	ldw	stdlink_offset(tmp1), stdlink
	ldw	varptr_offset(tmp1), varptr
	ldw	baseptr_offset(tmp1), baseptr
	ldw	gclink_offset(tmp1), gclink

	ldw	allocptr_offset(tmp1), allocptr	; restore ML regs
	ldw	limit_offset(tmp1), limit
	ldw	storeptr_offset(tmp1), storeptr

	ldw	miscreg_offset(0)(tmp1), miscreg0
	ldw	miscreg_offset(1)(tmp1), miscreg1
	ldw	miscreg_offset(2)(tmp1), miscreg2
	ldw	miscreg_offset(3)(tmp1), miscreg3
	ldw	miscreg_offset(4)(tmp1), miscreg4
	ldw	miscreg_offset(5)(tmp1), miscreg5
	ldw	miscreg_offset(6)(tmp1), miscreg6
	ldw	miscreg_offset(7)(tmp1), miscreg7
	ldw	miscreg_offset(8)(tmp1), miscreg8
	ldw	miscreg_offset(9)(tmp1), miscreg9
	ldw	miscreg_offset(10)(tmp1), miscreg10
	ldw	miscreg_offset(11)(tmp1), miscreg11
	ldi	1, tmp2

	; bias base register XXX

	stw	tmp2, inML(tmp1)		; we are now "in ML"
	ldw	GCpending(tmp1), tmp2
	addi,=	0, tmp2, zero
	ldi	0, limit			; nullified if GCpending == 0
	ldw	NumPendingSigs(tmp1), tmp2
	comibt,=,n	0, tmp2, restoreregs1
	ldw	maskSignals(tmp1), tmp2
	ldw	inSigHandler(tmp1), tmp3
	or	tmp2, tmp3, tmp2
	comibf,=,n	0, tmp2, restoreregs1	; both false => jump
	ldi	1, tmp2
	stw	tmp2, handlerPending(tmp1)
	ldi	0, limit
restoreregs1
	comclr,>= allocptr, limit, exhausted
	ldi	1, exhausted
	bv,n	0(gclink)
	



;;; array : (int * 'a) -> 'a array
; Allocate and initialize a new array.	 This can cause GC.

	ML_CODE_HDR(array_a)
	ldw	0(stdarg), tmp1		; tmp1 = length
	ldw	4(stdarg), tmp2		; tmp2 = init value
	extrs	tmp1, 30, 31, tmp1	; tmp1 = tmp1 >> 1
	copy	stdlink, gclink
	zdep	tmp1, 29, 30, tmp4	; tmp4 = tmp1 << 2
	add	allocptr, tmp4, tmp4	; tmp4 += allocptr (points to last word)
	comb,>=	tmp4, limit, saveregs
	ldi	closmask, maskreg
	ldi	TAG_array, tmp3		; tmp3 = TAG_tags
	dep	tmp1, 31-width_tags, 32-width_tags, tmp3 ; tmp3 |= tmp1 << width_tags
	stwm	tmp3, 4(allocptr)	; *allocptr++ = tmp3
	copy	allocptr, stdarg
L$array_a_loop
	comb,<	allocptr, tmp4, L$array_a_loop	; while (allocptr < tmp4)
	stwm	tmp2, 4(allocptr)		; *allocptr++ = tmp2
	CONTINUE



;;; create_b : int -> bytearray
;;; create_r : int -> realarray
;;; create_s : int -> string
; Create bytearray, realarray or string of given length.  
; Note that the length  is the number of array elements.  
; These can cause GC.

;; create_r_a
	ML_CODE_HDR(create_r_a)
	extrs	stdarg, 30, 31, tmp1	; tmp1 = stdarg >> 1
	zdep	tmp1, 28, 29, tmp4	; tmp4 = tmp1 << 3
	copy	stdlink, gclink
	add	allocptr, tmp4, tmp2
	comb,>=	tmp2, limit, saveregs
	ldi	closmask, maskreg
	ldi	TAG_realdarray, tmp3
	dep	tmp1, 31-width_tags, 32-width_tags, tmp3
	stwm	tmp3, 4(allocptr)
	copy	allocptr, stdarg
	ldo	4(tmp2), allocptr
	CONTINUE

;; create_b_a
	ML_CODE_HDR(create_b_a)
	extrs	stdarg, 30, 31, tmp1	; tmp1 = stdarg >> 1
	ldo	3(tmp1), tmp4		; tmp4 = tmp1 + 3
	depi	0, 31, 2, tmp4		; tmp4 = tmp4 & ~3
	copy	stdlink, gclink
	add	allocptr, tmp4, tmp2
	comb,>=	tmp2, limit, saveregs
	ldi	closmask, maskreg
	ldi	TAG_bytearray, tmp3
	dep	tmp1, 31-width_tags, 32-width_tags, tmp3
	stwm	tmp3, 4(allocptr)
	copy	allocptr, stdarg
	ldo	4(tmp2), allocptr
	CONTINUE

;; create_s_a
	ML_CODE_HDR(create_s_a)
	extrs	stdarg, 30, 31, tmp1	; tmp1 = stdarg >> 1
	ldo	3(tmp1), tmp4		; tmp4 = tmp1 + 3
	depi	0, 31, 2, tmp4		; tmp4 = tmp4 & ~3
	copy	stdlink, gclink
	add	allocptr, tmp4, tmp2
	comb,>=	tmp2, limit, saveregs
	ldi	closmask, maskreg
	ldi	TAG_string, tmp3
	dep	tmp1, 31-width_tags, 32-width_tags, tmp3
	stwm	tmp3, 4(allocptr)
	copy	allocptr, stdarg
	ldo	4(tmp2), allocptr
	CONTINUE


;;; create_v_a : int * 'a list -> 'a vector
; creates a record with elements taken from a list.
; n.b. The frontend ensures that list cannot be nil.
; see array_a for line by line comments

	ML_CODE_HDR(create_v_a)
	ldw	0(stdarg), tmp1
	ldw	4(stdarg), tmp2
	extrs	tmp1, 30, 31, tmp1
	copy	stdlink, gclink
	zdep	tmp1, 29, 30, tmp4
	add	allocptr, tmp4, tmp4
	comb,>=	tmp4, limit, saveregs
	ldi	closmask, maskreg
	ldi	TAG_record, tmp3
	dep	tmp1, 31-width_tags, 32-width_tags, tmp3
	stwm	tmp3, 4(allocptr)
	copy	allocptr, stdarg
L$create_v_a_loop
	ldw	0(tmp2), tmp3
	ldw	4(tmp2), tmp2
	comb,<	allocptr, tmp4, L$create_v_a_loop
	stwm	tmp3, 4(allocptr)
	CONTINUE

	
;;; floor : real -> int
; Overflow conditions are not quite correct.
; Should be fixed eventually.

floor_MAXINT	.double 1073741824.0

;; floor_a	
   ML_CODE_HDR(floor_a)
   fldws        0(stdarg), %fr4L
   fldws        4(stdarg), %fr4R

   ldil	  	L%floor_MAXINT,tmp2
   ldo	  	R%floor_MAXINT(tmp2),tmp2
   fldds     	0(tmp2), %fr6		; fr6 := MAXINT
   fabs,dbl  	%fr4, %fr5
   fcmp,dbl,< 	%fr5, %fr6
   ftest
   b		L$floor_overflow
   nop

   ldi	        -104,tmp2		; set rounding mode towards -infinity
   ldi		0x60e,tmp1
   stw		tmp1,-104(sp)
   fldwx	tmp2(sp),%fr0L

   fcnvfx,dbl,sgl %fr4, %fr4R 
   ldi		0xe,tmp1
   stw		tmp1,-104(sp)
   fldwx	tmp2(sp),%fr0L
   fstwx        %fr4R,tmp2(sp)
   ldw		-104(sp),stdarg
   add          stdarg,stdarg,stdarg
   ldo		1(stdarg),stdarg
   CONTINUE

L$floor_overflow
   ldil		L%0x7fffffff,tmp1
   ldo		R%0x7fffffff(tmp1),tmp1
   addo		tmp1,tmp1,zero




;;; logb : real -> int
; Extract and unbias the exponent, return 0 for a zero exponent.
; The IEEE bias is 1023.

   ML_CODE_HDR(logb_a)
   ldw		0(stdarg), stdarg
   extru	stdarg, 11, 11, stdarg
   add		stdarg, stdarg, stdarg
   ldo		1-1023-1023(stdarg), stdarg
   CONTINUE



;;; scalb : (real * int) -> real 		
;(scalb(x,y) = x * 2 ^ y)
   ML_CODE_HDR(scalb_a)
   CHECKLIMIT(scalb_a,closmask)
   ldw		4(stdarg),tmp1		; tmp1 := ML int y
   extrs   	tmp1,30,31,tmp1		; tmp1 := machine int y
   ldw		0(stdarg),stdarg	; stdarg := x
   ldw		0(stdarg),tmp3		; tmp3 := MSW(*x)

   extru,<> 	tmp3,11,11,tmp2		; tmp2 := ieee(exp)
   b		scalb_all_done
   nop

   add,>   	tmp2,tmp1,tmp2
   b		scalb_underflow
   nop

   ldi		2047,tmp4
   combt,< 	tmp4,tmp2,scalb_overflow
   nop
	
   dep 		tmp2,11,11,tmp3
   ldw		4(stdarg),tmp4	
	
scalb_write_out
   stw		tmp3,4(allocptr)
   stw		tmp4,8(allocptr)
   ldi		DESC_reald,tmp3
   stw		tmp3,0(allocptr)
   addi		0x4,allocptr,stdarg
   addi		0xc,allocptr,allocptr

scalb_all_done
   CONTINUE

scalb_underflow
   add		0,0,tmp3
   add		0,0,tmp4
   b		scalb_write_out
   nop

scalb_overflow
   ldil		L%0x7fffffff,tmp1
   ldo		R%0x7fffffff(tmp1),tmp1
   addo		0,tmp1,tmp1

/*
  Integer multiplication and division routines
 
  these funny functions take their args in tmp1 and tmp2, and the
  return address in tmp4.  it leaves its return value in tmp3.
 
  presumably it's ok to save callersave regs into our stack from
  because we can't switch threads in the middle of straightline code
 
  XXX this is completely horrid.
  XXX multiplication doesn't detect overflow

*/

#define milli_save(name,milli)				 \
	.import	milli,MILLICODE				!\
	.export	name,DATA				!\
	.label name					!\
	stw	%r8, callersave_offset(8)(sp)		!\
	stw	%r9, callersave_offset(9)(sp)		!\
	stw	%r10, callersave_offset(10)(sp)		!\
	stw	%r11, callersave_offset(11)(sp)		!\
	stw	milli_rtn, othersave_offset(0)(sp)	!\
	stw	milli_rp, othersave_offset(1)(sp)	!\
	stw	c_arg0, othersave_offset(2)(sp)		!\
	stw	c_arg2, othersave_offset(4)(sp) 	!\
	stw	c_arg3, othersave_offset(5)(sp) 	!\
        stw     %r19, 0-96(sp)                          !\
        stw     %r20, 0-100(sp)                         !\
        stw     %r22, 0-104(sp)                         !\
	copy	tmp1, c_arg0				!\
	ldil	L%milli, c_arg2  			!\
	ldo	R%milli(c_arg2), c_arg2			!\
	ldil	L%milli_glue, c_arg3 			!\
	ldo	R%milli_glue(c_arg3), c_arg3		!\
	ldsid	(c_arg3), %r8				!\
	mtsp	%r8, %sr1				!\
	ble	0(%sr1, c_arg3)				!\
	copy	tmp2, c_arg1

#define milli_restore					 \
	copy	milli_rtn, tmp3				!\
	ldw	callersave_offset(8)(sp), %r8		!\
	ldw	callersave_offset(9)(sp), %r9		!\
	ldw	callersave_offset(10)(sp), %r10		!\
	ldw	callersave_offset(11)(sp), %r11		!\
	ldw	othersave_offset(0)(sp), milli_rtn	!\
	ldw	othersave_offset(1)(sp), milli_rp	!\
	ldw	othersave_offset(2)(sp), c_arg0		!\
	ldw	othersave_offset(4)(sp), c_arg2		!\
        ldw     0-96(sp), %r19                          !\
        ldw     0-100(sp),%r20                          !\
        ldw     0-104(sp),%r22                          !\
	bv	zero(tmp4)				!\
	ldw	othersave_offset(5)(sp), c_arg3

;; ml_mul
	milli_save(ml_mul,$$mulI)
	milli_restore

;; ml_div
	milli_save(ml_div,$$divI)
	milli_restore


;;; try_lock : spin_lock -> bool
; low-level test-and-set style primitive for mutual-exclusion among 
; processors.

	ML_CODE_HDR(try_lock_a)
        ldw	0(stdarg),tmp1
	ldi     1,tmp2			/* ML_false */
	stw	tmp2,0(stdarg)
	add	tmp1,zero,stdarg
	CONTINUE


;; unlock : releases a spin lock 

	ML_CODE_HDR(unlock_a)
	ldi	3,tmp1			/* ML_true */
	stw	tmp1,0(stdarg)		
	ldi	1,stdarg		/* return unit */
	CONTINUE


;======================== code quadrant =======================	
		.code

	.export milli_glue,CODE
milli_glue
	copy	milli_rp, %r9
	bl	L$milli_glue, milli_rp
	copy	zero, zero
	ldsid	(%r9), c_arg3
	mtsp	c_arg3, %sr1
	be,n	0(%sr1, %r9)
L$milli_glue
	bv,n	zero(c_arg2)


;;; set_fsr - set IEEE floating point enables.

    .export set_fsr
set_fsr
    .proc
    .callinfo

    .enter 
    ldi		0xe,tmp1
    stw		tmp1,-104(sp)
    ldi		-104,tmp1
    fldwx	tmp1(sp),%fr0L
    .leave

    .procend



;;; flush_icache

    .export flush_icache
flush_icache
    .proc
    .callinfo

    .enter
    ldsid	(26), 23	; get space id from short pointer
    mtsp	23, 2		; stick it in scratch space reg

    depi	0,31,4,26	; align address to cache line
    addi	15,25,25	; align size upwards
    depi	0,31,4,25		    
    ldi		16,22		; r22 := minimum cache line size
    ldi	-16,21			; r21 := -(minimum cache line size)

fic_loop
    fdc	0(2,26)
    sync
    fic,m	22(2,26)	; fic can't use short pointer so
				; use the space reg set up above

    nop				; 7 cycle delay. See programming note
    nop				; for SYNC in arch. ref. manual.
    nop
    nop
    nop
    nop
    nop

    addb,>=	21,25,fic_loop	; add stride to count, branch
    nop
    .leave

    .procend


	.export pointer2space
pointer2space
	.proc
	.callinfo
	.entry
	bv	0(2)
	ldsid	(26), 28
	.exit
	.procend

	.end			; End of program

