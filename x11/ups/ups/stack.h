/* stack.h - header file for stack.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)stack.h	1.8 20/5/92 (UKC) */

typedef struct {
	int si_signo;
	taddr_t si_fp;
	preamble_t si_prbuf;
} siginfo_t;

/*  Element of the linked list describing the stack.
 */
typedef struct stackst {
	func_t *stk_func;
	taddr_t stk_pc;			/* saved program counter */
	taddr_t stk_fp;			/* frame pointer */
	taddr_t stk_sp;			/* stack pointer (only for $sp) */
#ifdef ARCH_VAX
	taddr_t stk_ap;			/* argument pointer */
#else
#define stk_ap	stk_fp
#endif
	int stk_lnum;			/* source lnum corresponding to stk_pc */
	siginfo_t *stk_siginfo;		/* signal that cause func call, if not 0 */
	short stk_bad;			/* stack corrupted after this frame */
	struct stackst *stk_inner;	/* stack_t describing next frame in */
	struct stackst *stk_outer;	/* stack_t describing next frame out */
#ifdef CHECK_STACK
	short stk_isfree;		/* for debugging */
	short stk_stamp;		/* stamp for debugging */
#endif
} stack_t;

stack_t *build_stack_trace PROTO((proc_t proc));
taddr_t reg_addr PROTO((stack_t *stk, int reg));
stack_t *new_stk PROTO((void));
void free_stk PROTO((stack_t *stk));

extern func_t Dummyfunc;
