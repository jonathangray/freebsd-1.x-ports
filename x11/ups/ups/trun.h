/* trun.h - target control header file */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)trun.h	1.7 4/7/91 (UKC) */

typedef enum rtypeen {
	RT_STEP,
	RT_NEXT,
	RT_CONT
} rtype_t;

/*  Define KNOW_ASM if we know the assembler for this machine.
 */
#if defined(ARCH_VAX) || defined(ARCH_SUN3) || defined(ARCH_MIPS) || defined(ARCH_CLIPPER) || defined(ARCH_SUN4)
#define KNOW_ASM
#endif

/*  Set FORGET_ASM to make ups forget any knowledge of assembler it might
 *  have had.
 */
#ifdef FORGET_ASM
#undef KNOW_ASM
#endif

#ifdef PROC_H_INCLUDED
proc_t run_target PROTO((proc_t proc, breakpoint_t stop_bp, rtype_t rtype));
#endif
