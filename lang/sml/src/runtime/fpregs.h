/* fpregs.h
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 */
 
#ifndef C
#if defined(SPARC)
#	define NSAVED_FPREGS	0
#endif
#if defined(M68)
#	define NSAVED_FPREGS 	6
#endif
#if defined(VAX)
#	define NSAVED_FPREGS	0
#endif
#if defined(MIPS)
#	define NSAVED_FPREGS	5
#endif
#if defined(HPPA)
#	define NSAVED_FPREGS	0
#endif
#if defined(RS6000)
#	define NSAVED_FPREGS 	18
#endif
#if defined(I386)
#	define NSAVED_FPREGS	8
#	define FP_STATE_SIZE	108
#endif

#endif
