/* mips_frame.h - MIPS stack frame info */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)mips_frame.h	1.2 4/7/91 (UKC) */

/*  Information about the MIPS virtual stack frame.
 *
 *  These fields are copied from the PDR (see <syms.h>)
 */
typedef struct framest {
	long fr_frame_size;		/* Stack frame size */
	long fr_reg_offset;		/* Offset from fp to registers */
	long fr_reg_mask;		/* Saved integer registers mask */
	short fr_pcreg;			/* Register used to save the pc */
	short fr_spreg;			/* Register used for sp (usually r29) */
} frame_t;

#define MIPS_SP_REGNO		29	/* r29 is used as the sp on the MIPS */
#define MIPS_SAVEDPC_REGNO	31	/* r31 is used to save the pc */

#ifdef SYMTAB_H_INCLUDED
frame_t *get_frame_info PROTO((func_t *f));
#endif

int get_frame_size PROTO((unsigned long addr));
