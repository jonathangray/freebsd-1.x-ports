/* mreg.h - machine dependent register information */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)mreg.h	1.12 4/5/92 (UKC) */

#ifdef ARCH_SUN3
#define N_SUN_GREGS		14
#endif

#ifdef ARCH_SUN386
#define N_SUN_GREGS		12	/* probably too high */
#endif

#ifdef ARCH_SUN4
#define N_SUN_GREGS		32
#endif

#ifdef ARCH_BSDI386
/*  The first eight values are the general register numbers used in the
 *  symbol table.  These are mapped to a u area offset by the trapregs
 *  and syscallregs arrays in set_uarea_reg_offsets in proc.c.
 *
 *  UR_FP, UR_SP and UR_PC are also mapped by the *regs arrays - don't
 *  change their values without changing the arrays.
 */
#define N_UAREA_GREGS		8
#define UR_SP			4
#define UR_FP			5
#define UR_PC			N_UAREA_GREGS
#define N_UREGS			(N_UAREA_GREGS + 1)
#endif

#ifdef ARCH_CLIPPER
#define N_UAREA_GREGS		14	/* Clipper has r0..r13 as general regs */

#define UR_FP		(N_UAREA_GREGS)		/* fp is r14 */
#define UR_SP		(N_UAREA_GREGS + 1)	/* sp is r15 */
#define UR_PC		(N_UAREA_GREGS + 2)
#define N_UREGS		(N_UAREA_GREGS + 3)
#endif

#ifdef ARCH_MIPS
#define N_UAREA_GREGS		NGP_REGS + NFP_REGS	/* from ptrace.h */

#define UR_SP		MIPS_SP_REGNO		/* from mips_frame.h */
#define UR_FP		N_UAREA_GREGS		/* we fake this on the fly */
#define UR_PC		(N_UAREA_GREGS + 1)
#define N_UREGS		(N_UAREA_GREGS + 2)
#endif

#ifdef ARCH_VAX
#define N_UAREA_GREGS		12	/* VAX has r0..r11 as general registers */

#define UR_AP	N_UAREA_GREGS
#define UR_FP	(N_UAREA_GREGS + 1)
#define UR_SP	(N_UAREA_GREGS + 2)
#define UR_PC	(N_UAREA_GREGS + 3)
#define UR_PSL	(N_UAREA_GREGS + 4)
#define N_UREGS	(N_UAREA_GREGS + 5)
#endif /* ARCH_VAX */

/*  Some architectures (currently VAX and MIPS under Ultrix) have
 *  the registers accessible via ptrace READU calls in the u area.
 *
 *  We try to abstract out some common code for these, and define
 *  UAREA_REGS for #ifdefs.
 */
#if defined(OS_BSDI) || defined(ARCH_VAX) || defined(ARCH_MIPS) || defined(ARCH_CLIPPER)
#define UAREA_REGS
#endif

#ifdef UAREA_REGS
typedef struct uregst {
	short ur_uaddr;
	short ur_is_current;
	taddr_t ur_value;
} ureg_t;

int reg_to_uarea_index PROTO((int reg));

#ifdef ARCH_BSDI386
typedef int (*get_uarea_word_func_t)PROTO((int arg, int offset));
void set_uarea_reg_offsets PROTO((ureg_t *ur,
				  get_uarea_word_func_t get_uarea_word_func,
				  int arg));
#else
void set_uarea_reg_offsets PROTO((ureg_t *ur));
#endif

#endif /* UAREA_REGS */

#ifdef OS_SUNOS
typedef struct sunregsst {
	struct regs sr_regs;
#ifdef ARCH_SUN4
	struct rwindow sr_rwindow;	/* MUST be just after sr_regs - see gsr */
#endif
#if defined(ARCH_SUN3) || defined(ARCH_SUN4) 
	struct fpu sr_fpu;
	bool sr_need_fpu;
#endif
#ifdef ARCH_SUN3
	enum { FPT_SOFT, FPT_68881, FPT_FPA } sr_fptype;
#endif
} sunregs_t;

taddr_t get_sun_regval PROTO((sunregs_t *sr, int pid, int reg));
#endif
