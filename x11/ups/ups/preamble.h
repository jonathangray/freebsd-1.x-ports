/* preamble.h - the structure used for recognising function preamble code */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)preamble.h	1.7 15/9/92 (UKC) */

#ifdef ARCH_386
/*  Max number of registers that can be saved on function entry.
 *  We are conservative here - the real number is probably 3 or 4.
 */
#define N_PUSHABLE_REGS	8
#endif

typedef struct preamblest {
	unsigned pr_bpt_offset;	/* offset from func addr of first bpt loc */
	unsigned pr_rsave_mask;	/* register save mask */
	int pr_rsave_offset;	/* offset from fp of start of reg save area */
#ifdef ARCH_CLIPPER
	int pr_frame_size;	/* frame size for functions with no fp */
#endif
#if defined(ARCH_MIPS) || defined(ARCH_CLIPPER) || defined(ARCH_SUN3)
	unsigned pr_fpreg_rsave_mask;	/* floating point reg rsave mask */
	int pr_fpreg_rsave_offset;	/* floating point reg save offset */
#endif
#ifdef ARCH_386
	char pr_regtab[N_PUSHABLE_REGS];/* which order regs saved */
#endif
} preamble_t;

#if defined(ARCH_MIPS) && (defined(OS_ULTRIX) || defined(OS_RISCOS) \
							|| defined(OS_NEWSOS))
#define FU_PREAMBLE(f)	((preamble_t *)(f)->fu_preamble_id)
#else
#define FU_PREAMBLE(f)	((preamble_t *)(((f)->fu_preamble_id != NULL) \
				    ? ((f)->fu_preamble_id) :  get_startup_code(f)))
#endif
