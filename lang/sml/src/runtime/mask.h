/* mask.h 
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * CALLEESAVE = 1 case is avoided, this is consistant with the 
 * the code in cps directory(closure.sml generic.sml etc)
 *
 * Some assemblers don't like <<.
 * The dynix3 assembler doesn't like parentheses.
 */

#ifdef CALLEESAVE
#if defined(M68) || defined(VAX)
#define PWR_2_CALLEESAVE 1
#else 
#define PWR_2_CALLEESAVE (1<<(CALLEESAVE))
#endif
#else
#if defined(SPARC) || defined(MIPS) || defined(RS6000) || defined(HPPA)
#define CALLEESAVE 3
#define PWR_2_CALLEESAVE 8
#else
#define CALLEESAVE 0
#define PWR_2_CALLEESAVE 1
#endif
#endif

#ifndef HPPA_ASM_HACK
# ifdef HPPA
#  define HPPA_ASM_HACK 0+
# else
#  define HPPA_ASM_HACK
# endif
#endif



				/* ((1 << (CALLEESAVE + 4)) - 1) */
#if defined(DYNIX) && defined(ASM)
#define closmask [[PWR_2_CALLEESAVE * 16] - 1]	
#else
#define closmask HPPA_ASM_HACK((PWR_2_CALLEESAVE * 16) - 1)	
#endif

/*
 * We avoid saying:
 *	if (CALLEESAVE == 0) ..
 * since this gives cpp running on HPUX trouble.
 */
#if CALLEESAVE
				/* ((1 << (CALLEESAVE + 4)) - 0x10 + 0xc) */

#if defined(DYNIX) && defined(ASM)
#define contmask [[PWR_2_CALLEESAVE * 16] - 0x10 + 0xc]
#else
#define contmask HPPA_ASM_HACK((PWR_2_CALLEESAVE * 16) - 0x10 + 0xc)
#endif
#define exnmask closmask

#else

#if defined(DYNIX) && defined(ASM)
#define contmask [0xd]
#else
#define contmask HPPA_ASM_HACK(0xd)
#endif
#define exnmask contmask
#endif

#if CALLEESAVE
#define callcc_mask closmask
#else
#define callcc_mask contmask
#endif
