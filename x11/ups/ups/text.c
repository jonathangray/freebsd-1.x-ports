/* text.c - address <-> line number mapping and function startup code handling */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_text_c_sccsid[] = "@(#)text.c	1.21 20/5/92 (UKC)";

#include <mtrprog/ifdefs.h>


#ifdef ARCH_VAX
#include <sys/types.h>
#include <machine/frame.h>
#endif

#ifdef ARCH_SUN386
#include <machine/reg.h>
#endif

#include <local/ukcprog.h>

#include "ups.h"
#include "symtab.h"
#include "text.h"
#include "preamble.h"

/*  Return an lno pointer given a function and an address.
 *  Return NULL if no line number information is available.
 *
 *  This function searches for the first lno that has an address larger
 *  than text_addr, and returns the one before that.
 */
lno_t *
addr_to_lno(f, text_addr)
func_t *f;
taddr_t text_addr;
{
	lno_t *ln, *last;
	preamble_t *pr;

	pr = FU_PREAMBLE(f);
	if (text_addr < f->fu_addr + pr->pr_bpt_offset)
		return NULL;
	last = NULL;
	for (ln = FU_LNOS(f); ln != NULL; ln = ln->ln_next) {
		if (ln->ln_addr > text_addr)
			return last;
		last = ln;
	}
	return last;
}

/*  Return the lnum that matches address text_addr.
 *
 *  This function is used to matching RBRAC addresses to line numbers.
 *  Some compilers emit several lnos for a for loop, all with the same
 *  source line number.  For the end of a block we want a source line
 *  number near the end of the loop, hence the odd way this routine works.
 */
int
rbrac_addr_to_lnum(f, text_addr)
func_t *f;
taddr_t text_addr;
{
	int max_lnum;
	lno_t *ln;
	preamble_t *pr;

	pr = FU_PREAMBLE(f);

	if (text_addr < f->fu_addr + pr->pr_bpt_offset)
		return 0;

	max_lnum = 0;
	for (ln = FU_LNOS(f); ln != NULL; ln = ln->ln_next) {
		if (ln->ln_addr > text_addr)
			break;
		if (ln->ln_num > max_lnum)
			max_lnum = ln->ln_num;
	}
	return max_lnum;
}

int
addr_to_lnum(f, text_addr)
func_t *f;
taddr_t text_addr;
{
	lno_t *lno;

	lno = addr_to_lno(f, text_addr);
	return (lno != NULL) ? lno->ln_num : 0;
}

/*  Map line number lnum to an address, given that the line is within
 *  function f.
 *
 *  Return 0 if there is no line number information, 1 if we can't find
 *  the line number.
 */
taddr_t
lnum_to_addr(f, lnum)
func_t *f;
int lnum;
{
	register struct lnost *ln;

	if ((f->fu_flags & FU_NOSYM) || FU_LNOS(f) == NULL)
		return 0;
	for (ln = FU_LNOS(f); ln != NULL; ln = ln->ln_next)
		if (ln->ln_num == lnum)
			return ln->ln_addr;
	return 1;
}

#ifdef ARCH_MIPS
/*  There is no DS3100 version of get_startup_code() - the stuff is
 *  set up by skim_te_symtab() in st_te.c.
 */
#endif /* ARCH_MIPS */

#ifdef ARCH_386
/*  Some 80386 opcodes that get_startup_code() must recognise at the start
 *  of functions.
 *
 *  The 80386 appears to read its opcodes as a byte stream (like a VAX)
 *  but we want to treat the first four opcodes of a function as a single
 *  integer.  Hence the SW_MOVL_ESP_EBP and SW_SUBL_IMM_ISP definitions.
 */
#define PUSHL_EBP	0x55
#define JMP_FAR		0xe9	/* + 4 byte operand */
#define JMP_SHORT	0xeb

#define SUBL_IMM_ESP		0x81ec	/* four byte operand */
#define SUBL_IMM_ESP_SHORT	0x83ec	/* one byte operand */

/*  No, I don't have the faintest idea why opcodes should be different
 *  across operating systems using the same chip ...
 */
#ifdef OS_SUNOS
#define MOVL_ESP_EBP	0x8bec
#endif

#ifdef OS_BSDI
#define MOVL_ESP_EBP	0x89e5
#endif

#define SWAP_SHORT(x)		((x >> 8) | (((x) & 0xff) << 8))
#define SW_MOVL_ESP_EBP		SWAP_SHORT(MOVL_ESP_EBP)
#define SW_SUBL_IMM_ESP		SWAP_SHORT(SUBL_IMM_ESP)
#define SW_SUBL_IMM_ESP_SHORT	SWAP_SHORT(SUBL_IMM_ESP_SHORT)

/*  80386 version of get_startup_code().
 */
preamble_id_t
get_startup_code(f)
func_t *f;
{
	/*  Worst case is
	 *
	 *	pushl %ebp		1 byte
	 *	movl %esp, %ebp		2 bytes
	 *	subl xxx, %esp		5 bytes
	 *	8 * pushl <reg>		8 bytes
	 *
	 *  I don't think we'd ever see 8 register pushes, but
	 *  who cares, PTRACE_READ is reasonably fast.
	 */
	unsigned char textbuf[1 + 2 + 5 + 8], *text;
	taddr_t addr;
	preamble_t *pr;
	int count;

	pr = (preamble_t *)e_malloc(sizeof(preamble_t));

	textfile_tread(f->fu_symtab_id, f->fu_addr,
					     (char *)textbuf, sizeof textbuf);
	text = textbuf;

	/*  The 80386 C compiler without -O is not very clever - it
	 *  jumps to the end of the function and back because at the
	 *  end of the function it knows how much stack space to grab
	 *  and which registers to save.  Thus we check for a jump
	 *  and follow it.
	 */
	if (*text == JMP_SHORT) {
		addr = f->fu_addr + 2 + text[1];
		pr->pr_bpt_offset = 2;
	}
	else if (*text == JMP_FAR) {
		addr = f->fu_addr + 5 + *(unsigned *)&text[1];
		pr->pr_bpt_offset = 5;
	}
	else {
		addr = 0;
		pr->pr_bpt_offset = 0;
	}

	if (addr != 0) {
		textfile_tread(f->fu_symtab_id, addr,
					(char *)textbuf, sizeof(textbuf));
		text = textbuf;
	}

	if ((*(unsigned *)text & 0xffffff) ==
					(PUSHL_EBP | (SW_MOVL_ESP_EBP << 8))) {
		text += 3;
		if (pr->pr_bpt_offset == 0)
			pr->pr_bpt_offset = 3;
	}
	else
		f->fu_flags |= FU_NO_FP;

	if (*(unsigned short *)text == SW_SUBL_IMM_ESP) {
		pr->pr_rsave_offset = -*(int *)&text[2];
		text += 6;
	}
	else if (*(unsigned short *)text == SW_SUBL_IMM_ESP_SHORT) {
		pr->pr_rsave_offset = -*(unsigned char *)&text[2];
		text += 3;
	}
	else
		pr->pr_rsave_offset = 0;
	
	count = 0;
	pr->pr_rsave_mask = 0;
	for (pr->pr_rsave_mask = 0; (*text & ~7) == 0x50; ++text) {
#ifdef OS_SUNOS
		static int regs[] = { EAX, ECX, EDX, EBX, ESP, EBP, ESI, EDI };
#endif
		int reg;

		if (text >= textbuf + sizeof(textbuf))
			panic("pushl botch in gsf");

#ifdef OS_SUNOS
		reg = regs[*text & 7];
#endif
#ifdef OS_BSDI
		reg = *text & 7;
#endif

		pr->pr_rsave_mask |= 1 << reg;
		pr->pr_regtab[count++] = reg;
	}

	f->fu_preamble_id = (preamble_id_t)pr;
	return (preamble_id_t)pr;
}
#endif /* ARCH_386 */

#ifdef ARCH_CLIPPER
/*  Some Clipper opcodes needed by get_startup_code().
 */
#define PUSHW_R14_R15		(0x1400 | (15 << 4) | 14)
#define PUSHW_RX_R15		(0x1400 | (15 << 4))
#define MOVW_R15_R14		(0x8400 | (15 << 4) | 14)
#define NOOP_0			0x0000
#define SUBI_R15_LONG_FMT	(0xa330 | 15)
#define SUBI_R15_SHORT_FMT	(0xa3b0 | 15)
#define SUBQ_X_R15		(0xa200 | 15)
#define SAVEWX			0xb400
#define SAVEWX_WORD1		0x0000
#define SAVEDX			0xb420
#define SAVEDX_WORD1		0x0000

/*  Clipper version of get_startup_code.
 */
preamble_id_t
get_startup_code(f)
func_t *f;
{
	unsigned short textbuf[20];	/* an overestimate */
	unsigned short *text;
	preamble_t *pr;

	pr = (preamble_t *)e_malloc(sizeof(preamble_t));

	/*  The following stuff is *very* compiler dependent and liable
	 *  to change with compiler releases.
	 */

	/*  Get the first few instructions
	 *
	 *  We read from the text file as the process may not have started yet.
	 */
	textfile_tread(f->fu_symtab_id,
				f->fu_addr, (char *)textbuf, sizeof(textbuf));

	text = textbuf;
	
	pr->pr_frame_size = 0;

	/*  Functions compiled with -g have the instructions
	 *
	 *	pushw r14, r15		(r14 is the fp, r15 the sp)
	 *	noop 0
	 *	movw r15, r14
	 *
	 *  at the start to build the frame.
	 */
	if (*text == PUSHW_R14_R15 && text[1] == NOOP_0 && text[2] == MOVW_R15_R14)
		text += 3;
	else {
		/*  No frame pointer.
		 */
		f->fu_flags |= FU_NO_FP;
	}

	if (*text == SUBI_R15_LONG_FMT) {
		int operand;

		operand = text[1] | (text[2] << 16);
		if (operand < 0)
			panic("negative (long fmt) frame size");
		pr->pr_frame_size += operand;
		text += 3;
	}

	if (*text == SUBI_R15_SHORT_FMT) {
		if ((short)text[1] < 0)
			panic("negative frame size");
		pr->pr_frame_size += text[1];
		text += 2;
	}
	else if ((*text & 0xff0f) == SUBQ_X_R15)
		pr->pr_frame_size += (*text++ >> 4) & 0xf;

	pr->pr_rsave_offset = -pr->pr_frame_size;
	pr->pr_rsave_mask = 0;

	/*  The compiler sometimes emits a some pushw rx, r15 instructions
	 *  rather than a single register savewn, so check for these.
	 */
	while ((*text & 0xfff0) == PUSHW_RX_R15) {
		pr->pr_rsave_mask |= (1 << (*text++ & 0xf));
		pr->pr_frame_size += 4;
		pr->pr_rsave_offset -= 4;
	}

	if ((*text & 0xfff0) == SAVEWX && text[1] == SAVEWX_WORD1) {
		int i, basereg, regbytes;

		basereg = *text & 0xf;
		text += 2;
		for (i = basereg; i < 15; ++i)
			pr->pr_rsave_mask |= (1 << i);

		regbytes = (15 - basereg) * 4;

		pr->pr_frame_size += regbytes;

		/*  Registers are saved below the frame pointer, so the
		 *  offset is negative.
		 */
		pr->pr_rsave_offset -= regbytes;
	}

	pr->pr_fpreg_rsave_mask = 0;
	pr->pr_fpreg_rsave_offset = 0;

	if ((*text & 0xfff8) == SAVEDX && text[1] == SAVEDX_WORD1) {
		int i, basereg, regbytes;

		basereg = *text & 7;
		text += 2;
		for (i = basereg; i < 8; ++i)
			pr->pr_fpreg_rsave_mask |= 1 << i;
		regbytes = (8 - basereg) * 8;

		pr->pr_frame_size += regbytes;
		pr->pr_fpreg_rsave_offset = pr->pr_rsave_offset - regbytes;
	}

	pr->pr_bpt_offset = (text - textbuf) * sizeof(textbuf[0]);

	if (pr->pr_bpt_offset > sizeof(textbuf))
		panic("startup botch in gsc");

	f->fu_preamble_id = (preamble_id_t)pr;
	return (preamble_id_t)pr;
}
#endif

#ifdef ARCH_SUN4
#define SAVE_MASK	((3 << 30) | (077 << 19))
#define SAVE		((2 << 30) | (074 << 19))

/*  SPARC version of get_startup_code().
 */
preamble_id_t
get_startup_code(f)
func_t *f;
{
	preamble_t *pr;
#define NWORDS	3
	int words[NWORDS];
	int i;

	pr = (preamble_t *)e_malloc(sizeof(preamble_t));

	textfile_tread(f->fu_symtab_id, f->fu_addr, (char *)words, sizeof(words));

	for (i = 0; i < NWORDS; ++i)
		if ((words[i] & SAVE_MASK) == SAVE)
			break;
	if (i == NWORDS) {
		f->fu_flags |= FU_NO_FP;
		pr->pr_bpt_offset = 0;
	}
	else {
		pr->pr_bpt_offset = (i + 1) * 4;
	}

	pr->pr_rsave_mask = 0;
	pr->pr_rsave_offset = 0;

	f->fu_preamble_id = (preamble_id_t)pr;
	return (preamble_id_t)pr;
}
#endif /* ARCH_CLIPPER || ARCH_SUN4 */

#if 0
/*  Null version of get_startup_code().  Used at the start of a port
 *  to a new architecture to make ups compile.
 */
preamble_id_t
get_startup_code(f)
func_t *f;
{
	preamble_t *pr;

	pr = (preamble_t *)e_malloc(sizeof(preamble_t));

	pr->pr_rsave_mask = 0;
	pr->pr_rsave_offset = 0;
	pr->pr_bpt_offset = 0;

	f->fu_preamble_id = (preamble_id_t)pr;
	return (preamble_id_t)pr;
}
#endif

#ifdef ARCH_VAX
/*  VAX version of get_startup_code().
 */
preamble_id_t
get_startup_code(f)
func_t *f;
{
	unsigned short rsavemask;
	preamble_t *pr;

	pr = (preamble_t *)e_malloc(sizeof(preamble_t));

	textfile_tread(f->fu_symtab_id,
				f->fu_addr, (char *)&rsavemask, sizeof(rsavemask));
	pr->pr_rsave_mask = rsavemask;
	pr->pr_rsave_offset = sizeof(struct frame);
	pr->pr_bpt_offset = (f->fu_language == LANG_FORTRAN) ? 0 : 2;

	f->fu_preamble_id = (preamble_id_t)pr;
	return (preamble_id_t)pr;
}
#endif /* ARCH_VAX */

#ifdef ARCH_SUN3
/*  A few 68xxx opcodes that get_startup_code() must recognise at the start
 *  of functions.
 */
#define LINK_A6		0x4e56
#define ADDL_IMM_A7	0xdffc
#define MOVEML		0x48d7
#define PEA_ABSW	0x4878
#define TRAP_0		0x4e40
#define FMOVEM_A6	0xf236

/*  Sun 2/3 version of get_startup_code().  Knows too much about
 *  68xxx opcodes and function preambles.
 */
preamble_id_t
get_startup_code(f)
func_t *f;
{
	unsigned short textbuf[12];	/* need at most 12 shorts */
	unsigned short *text;
	preamble_t *pr;

	pr = (preamble_t *)e_malloc(sizeof(preamble_t));

	/*  The following stuff is *very* compiler dependent and liable
	 *  to change with compiler releases.
	 */

	/*  Get the first few instructions
	 *
	 *  We read from the text file as the process may not have started yet.
	 */
	textfile_tread(f->fu_symtab_id,
				f->fu_addr, (char *)textbuf, sizeof(textbuf));

	text = textbuf;

	/*  System call stubs are a special case - they save no registers
	 *  and breakpoints should be placed at the function address.
	 */
	if (*text == PEA_ABSW) {
		pr->pr_rsave_mask = 0;
		pr->pr_bpt_offset = 0;
		f->fu_flags |= FU_NO_FP;
		return (preamble_id_t)pr;
	}

	/*  Get the link command and offset.
	 *  The cast to short is there as we want sign extension in this
	 *  case, as the displacement operand of link is a 16 bit signed
	 *  number.
	 */
	if (*text == LINK_A6) {
		pr->pr_rsave_offset = (short)text[1];
		text += 2;
	}
	else
		f->fu_flags |= FU_NO_FP;

	/*  Get the add which moves the stack pointer over the locals
	 *  and registers.
	 *
	 *  This add is not present for optimised code - the addition is
	 *  done in the link instruction.
	 */
	if (*text == ADDL_IMM_A7) {
		if (pr->pr_rsave_offset != 0) {
			/*  Non zero link offset *and* addition - surely not.
			 */
			panic("bad startup code in get_startup_code");
		}
		pr->pr_rsave_offset = (text[1] << 16) | text[2];
		text += 3;
	}

	/*  Get the register save command.
	 *  Not always present for optimised code.
	 */
	if (*text == MOVEML) {
		pr->pr_rsave_mask = text[1];
		text += 2;
	}
	else {
		/*  No registers saved in this function.
		 */
		pr->pr_rsave_mask = 0;
	}

	/*  Check for a floating point register save instruction.
	 */
	if (*text == FMOVEM_A6) {
		pr->pr_fpreg_rsave_mask = text[1] & 0xff;
		pr->pr_fpreg_rsave_offset = (text[3] << 16) | text[4];
		text += 5;
	}
	else {
		/*  No floating point registers saved in this function.
		 */
		pr->pr_fpreg_rsave_mask = 0;
	}

	pr->pr_bpt_offset = (text - textbuf) * sizeof(textbuf[0]);

	f->fu_preamble_id = (preamble_id_t)pr;
	return (preamble_id_t)pr;
}
#endif /* ARCH_SUN3 */

/*  Determine the minimum displacement into the function code at which a
 *  breakpoint can be placed. This should be after the registers have
 *  been saved if they are saved. Return the address.
 *
 *  If the function has line number information, just return the address
 *  of the first line.
 */
taddr_t
min_bpt_addr(f)
func_t *f;
{
	preamble_t *pr;

	pr = FU_PREAMBLE(f);
	if (FU_LNOS(f) != NULL)
		return FU_LNOS(f)->ln_addr;
	else
		return f->fu_addr + pr->pr_bpt_offset;
}

/*  Set *p_addr to the address corresponding to line lnum in function f.
 *  Return 0 for success, -1 and an error message otherwise.
 */
int
map_lnum_to_addr(f, lnum, p_addr)
func_t *f;
int lnum;
taddr_t *p_addr;
{
	taddr_t addr;

	if ((addr = lnum_to_addr(f, lnum)) == 1) {
		errf("No executable code at line %d of %s", lnum,
							f->fu_fil->fi_name);
		return -1;
	}
	if (addr == 0) {
		errf("No line number information for %s", f->fu_name);
		return -1;
	}
	if (addr < min_bpt_addr(f))
		addr = min_bpt_addr(f);
	*p_addr = addr;
	return 0;
}

