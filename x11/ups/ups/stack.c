/* stack.c - build a stack trace by examining the target */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_stack_c_sccsid[] = "@(#)stack.c	1.32 20/5/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <sys/types.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef ARCH_MIPS
#ifdef ARCH_CLIPPER
#include <machine/machparam.h>
#include <machine/vmparam.h>	/* for USRSTACK */
#else
#include <machine/frame.h>
#endif /* !ARCH_CLIPPER */
#endif /* !ARCH_MIPS */

#ifdef ARCH_BSDI386
#include <machine/vmparam.h>	/* for USRSTACK */
#include <sys/param.h>
#include <sys/user.h>
#endif

#include <local/ukcprog.h>
#include <mtrprog/alloc.h>

#include "ups.h"
#include "symtab.h"
#include "proc.h"
#include "data.h"
#include "text.h"
#include "preamble.h"
#include "stack.h"

#ifdef ARCH_MIPS
#include "mips_frame.h"
#endif /* ARCH_MIPS */

#ifdef ARCH_BSDI386
/*  There is no <machine/frame.h> on the BSDi box.  This is what a
 *  stack frame looks like.  Don't add to this - the signal handling
 *  code in build_stack_trace depends on the size of this struct.
 */
struct frame {
	struct frame *fr_savfp;
	int fr_savpc;
};
#endif

#define HAS_FP(f)	((((f)->fu_preamble_id == NULL) && get_startup_code(f)), \
						((f)->fu_flags & FU_NO_FP) == 0)

func_t Dummyfunc = {
	FU_NOSYM | FU_DONE_LNOS | FU_DONE_BLOCKS,
	"{DUMMY}"
};

static siginfo_t *make_siginfo PROTO((int signo));
static stack_t *make_stk PROTO((func_t *f, taddr_t pc, int lnum, stack_t *last));
#ifndef ARCH_MIPS
static stack_t *discard_junk_frames PROTO((stack_t *stk));
#endif

ALLOC_NEW_FREE(extern,stack_t,stk,stk_inner)

static stack_t *
make_stk(f, pc, lnum, last)
func_t *f;
taddr_t pc;
int lnum;
stack_t *last;
{
	stack_t *stk;

	stk = new_stk();

	stk->stk_bad = FALSE;
	stk->stk_pc = pc;
	stk->stk_siginfo = NULL;
	stk->stk_func = f;
	stk->stk_lnum = lnum;
	stk->stk_inner = last;
#ifdef CHECK_STACK
	stk->stk_isfree = FALSE;
	stk->stk_stamp = 0;
#endif
	return stk;
}

static siginfo_t *
make_siginfo(signo)
int signo;
{
	siginfo_t *si;

	si = (siginfo_t *)e_malloc(sizeof(siginfo_t));
	si->si_signo = signo;
	si->si_fp = 0;

	/*  We leave si_prbuf uninitialised, as it is not used unless
	 *  si_fp is non zero.
	 */
	
	return si;
}

#ifdef CHECK_STACK
static void
check_stack()
{
	register stack_t *stk;
	static int stamp = 0;
	int maxstamp;

	maxstamp = stamp++;
	for (stk = Outer_stack; stk != NULL; stk = stk->stk_inner) {
		if (stk->stk_isfree)
			panic("found free stk in stack");
		if (stk->stk_stamp > maxstamp)
			panic("cycle detected in stack");
		stk->stk_stamp = stamp;
	}
}
#endif

/*  FRAGILE CODE
 *
 *  Build a stack trace from a core file or process.
 *
 *  If ups breaks on when you get a new release of an OS, this is the
 *  place to look.  This function has lots of machine and compiler
 *  dependent assumptions about stack layout (especially when signals
 *  are involved).
 *
 *  Note that the stk_outer links are not set by this function.
 */
#ifdef ARCH_SUN4
stack_t *
build_stack_trace(proc)
proc_t proc;
{
	int lnum;
	taddr_t pc, savpc, fp;
	stack_t *stk, *last;
	func_t *f;
	struct frame fbuf;
	
	pc = proc_getreg(proc, REG_PC);
	fp = proc_getreg(proc, REG_FP);
	savpc = proc_getreg(proc, 31);

	for (stk = last = NULL; ; last = stk) {
		if ((f = addr_to_func(pc)) == NULL)
			f = &Dummyfunc;

		if (strcmp(f->fu_name, "_sigtramp") == 0 && last != NULL) {
			struct { int signo, code; taddr_t scp; } sigargs;
			struct sigcontext scbuf;

			/*  From looking at stack dumps ...
			 */
			if (dread(fp + 0x40, (char *)&sigargs, sizeof(sigargs)) != 0)
				break;
			last->stk_siginfo = make_siginfo(sigargs.signo);
			last->stk_siginfo->si_fp = fp;

			if (dread(sigargs.scp, (char *)&scbuf, sizeof(scbuf)) != 0)
				break;
			if (dread(fp, (char *)&fbuf, sizeof(fbuf)) != 0)
				break;

			fp = (taddr_t)fbuf.fr_savfp;
			pc = (taddr_t)scbuf.sc_pc;
			if ((f = addr_to_func(pc)) == NULL)
				f = &Dummyfunc;
			if (HAS_FP(f))
				savpc = fbuf.fr_savpc;
		}

		/*  The Sun 4 is unusual in that saved pc values point
		 *  to the address of the call instruction, rather than
		 *  the one after the call.  Thus we don't have to offset
		 *  the pc to get the right line number.
		 */
		lnum = addr_to_lnum(f, pc);

		stk = make_stk(f, pc, lnum, last);
		stk->stk_fp = fp;
		stk->stk_sp = fp;

		/*  If we are not the innermost function, adjust stk_pc so that it
		 *  points to the location where the called function will jump to
		 *  on return.
		 */
		if (last != NULL && last->stk_siginfo == NULL)
			stk->stk_pc += 8;

		if (fp == TADDR_NULL)
			break;

		/*  Get the next stack frame
		 *
		 *  The pc > f->fu_addr test is to cover the case where
		 *  a signal has interrupted a function before the frame
		 *  setup preamble has executed.
		 */
		if (HAS_FP(f) && pc > f->fu_addr) {
			if (dread(fp, (char *)&fbuf, sizeof(fbuf)) != 0)
				break;
			fp = (taddr_t)fbuf.fr_savfp;
			pc = savpc;
			savpc = fbuf.fr_savpc;
		}
		else {
			if (last == NULL)
				pc = proc_getreg(proc, 15);
			else {
				/*  We can't cope with non frame pointer
				 *  function that calls another function.
				 */
				if (last->stk_siginfo == NULL)
					break;
				pc = savpc;
				savpc = fbuf.fr_savpc;
			}
		}
	}

	if (fp != TADDR_NULL && stk != NULL)
		stk->stk_bad = TRUE;

	return discard_junk_frames(stk);
}
#endif /* ARCH_SUN4 */

#ifdef ARCH_MIPS
stack_t *
build_stack_trace(proc)
proc_t proc;
{
	int lnum;
	bool stack_ok;
	stack_t *stk, *last;
	taddr_t pc, sp;

	pc = proc_getreg(proc, REG_PC);
	sp = proc_getreg(proc, REG_SP);

	stack_ok = FALSE;

	for (stk = last = NULL; sp != 0; last = stk) {	/* BUG: need proper test */
		func_t *f;
		frame_t *fr;
		taddr_t fp, saved_pc;

		if ((f = addr_to_func(pc)) == NULL)
			break;

		fr = get_frame_info(f);

		/*  We take a frame with pcreg == 0 as the end of the stack.
		 *  This is from observation, not from any documentation.
		 */
		if (fr->fr_pcreg == 0) {
			stack_ok = TRUE;
			break;
		}

		if ((fr->fr_reg_mask & (1 << MIPS_SAVEDPC_REGNO)) == 0 &&
						strcmp(f->fu_name, "sigvec") == 0) {
			struct sigcontext *sc;
			int signo;

			/*  We set sc just so the address expressions below
			 *  work.  We never dereference it.  This isn't very
			 *  portable, but then neither is this function.
			 */
			sc = (struct sigcontext *)(sp + 9 * sizeof(taddr_t));

			if (proc_read_data(proc, (taddr_t)&sc->sc_pc,	
						(char *)&pc, sizeof(pc)) != 0)
				break;
			
			if ((f = addr_to_func(pc)) == NULL)
				break;
			fr = get_frame_info(f);

			if (proc_read_data(proc, (taddr_t)&sc->sc_regs[4],
						(char *)&signo, sizeof(signo)) != 0)
				break;
			if (signo == 0)
				signo = -1;
			if (last != NULL)
				last->stk_siginfo = make_siginfo(signo);

			if (proc_read_data(proc,
					   (taddr_t)&sc->sc_regs[MIPS_SP_REGNO],
					   (char *)&sp, sizeof(sp)) != 0)
				break;

			if (proc_read_data(proc, (taddr_t)&sc->sc_regs[fr->fr_pcreg],
					   (char *)&saved_pc, sizeof(int)) != 0)
				break;
		}
		else
			saved_pc = 0;

		if (fr->fr_spreg != MIPS_SP_REGNO)
			panic("can't cope with spreg != SP_REG in bst");
		fp = sp + fr->fr_frame_size;

		lnum = addr_to_lnum(f, (last == NULL) ? pc : pc - 1);

		stk = make_stk(f, pc, lnum, last);
		stk->stk_fp = fp;
		stk->stk_sp = sp;

		/*  We only drop the sp over the function's stack frame
		 *  if the function has had a chance to set up the frame.
		 */
		if (pc > f->fu_addr)
			sp += fr->fr_frame_size;

		if (pc > f->fu_addr &&
				    fr->fr_reg_mask & (1 << MIPS_SAVEDPC_REGNO)) {
			if (fr->fr_pcreg != MIPS_SAVEDPC_REGNO)
				panic("savpc botch in bst");
			if (dread(fp + fr->fr_reg_offset, (char *)&pc, 4) != 0)
				break;
		}
		else {
			/*  The pc was not saved ... therefore this must
			 *  be a leaf function, which means that it must
			 *  be the innermost function on the stack (modulo
			 *  signals).
			 */
			if (saved_pc != 0)
				pc = saved_pc;
			else if (last == NULL)
				pc = proc_getreg(proc, fr->fr_pcreg);
			else
				panic("leaf func botch in bst");
		}
	}

	if (!stack_ok && stk != NULL)
		stk->stk_bad = TRUE;

	return stk;
}
#endif /* !ARCH_MIPS */

#if !defined(ARCH_MIPS) && !defined(ARCH_SUN4)
stack_t *
build_stack_trace(proc)
proc_t proc;
{
#ifdef OS_SUNOS
	struct sigframest {
		taddr_t sf_savfp;
		taddr_t sf_savpc;
		int sf_signo;
		int sf_code;
		taddr_t sf_scp;
	} sigframe;
#endif /* OS_SUNOS */
#ifdef ARCH_CLIPPER
	struct frame {
		taddr_t fr_savfp;
		taddr_t fr_savpc;
	};
	preamble_t *pr;
	bool must_find_fp;
#endif
	struct sigcontext scbuf;
	int lnum;
	bool normal_frame;
	taddr_t pc, fp, sp;
#ifdef ARCH_VAX
	taddr_t ap;
#endif
	register stack_t *stk;
	func_t *f;
	stack_t *last;
	struct frame fbuf;
#ifdef ARCH_CLIPPER
	extern func_t *Main_func;
#endif
	
	pc = proc_getreg(proc, REG_PC);
	fp = proc_getreg(proc, REG_FP);
#ifdef ARCH_VAX
	ap = proc_getreg(proc, REG_AP);
#endif
	sp = proc_getreg(proc, REG_SP);

#ifdef DEBUG_STACK
	if (Debug_flags & DBFLAG_STACK)
		fprintf(stderr, "bst: pc=0x%x fp=0x%x sp=0x%x\n", pc, fp, sp);
#endif

#ifdef ARCH_CLIPPER
	must_find_fp = FALSE;
#endif
	for (stk = last = NULL; fp != NULL; last = stk) {
		normal_frame = TRUE;

		if ((f = addr_to_func(pc)) == NULL) {
#ifdef ARCH_CLIPPER
			if (last != NULL && last->stk_func == Main_func) {
				fp = NULL;
				break;
			}
#endif /* ARCH_CLIPPER */
			f = &Dummyfunc;
		}
#if defined(ARCH_VAX) || defined(ARCH_CLIPPER) || defined(ARCH_BSDI386)
		if (*f->fu_name == '[' && strcmp(f->fu_name, "[start]") == 0) {
			fp = NULL;
			break;
		}
#endif

		lnum = addr_to_lnum(f, (last == NULL) ? pc : pc - 1);

#ifdef OS_SUNOS
		if (strcmp(f->fu_name, "_sigtramp") == 0 && last != NULL) {
			int signo;
			taddr_t scp;

			if (dread(last->stk_fp, (char *)&sigframe,
							sizeof(sigframe)) != 0)
				break;
			signo = sigframe.sf_signo;
			scp = sigframe.sf_scp;

			last->stk_siginfo = make_siginfo(signo);

			if (dread(scp, (char *)&scbuf, sizeof(scbuf)) != 0)
				break;
			pc = (taddr_t)scbuf.sc_pc;
			if ((f = addr_to_func(pc)) == NULL)
				f = &Dummyfunc;
			lnum = addr_to_lnum(f, pc);
		}
#endif /* OS_SUNOS */

#ifdef DEBUG_STACK
		if (Debug_flags & DBFLAG_STACK)
			fprintf(stderr, "bst: pc=0x%x fname=%s fp=%x\n",
								pc, f->fu_name, fp);
#endif

		stk = make_stk(f, pc, lnum, last);

		/*  Get the next stack frame
		 */
#if defined(ARCH_SUN3) || defined(ARCH_SUN386)
		/*  Special case code for functions with no frame pointer
		 *  setup code on Suns.
		 *
		 *  The pc > f->fu_addr test is to cover the case where
		 *  a signal has interrupted a function before the frame
		 *  setup preamble has executed.
		 */
		if (!(HAS_FP(f) && pc > f->fu_addr) &&
				     (last == NULL || last->stk_siginfo != NULL)) {
			taddr_t offset;
			int res;

			if (last != NULL && last->stk_siginfo != NULL)
				sp = (taddr_t)scbuf.sc_sp;

			/*  A function with no stack setup code may still
			 *  push things on the stack, so the sp may point
			 *  a few words above the return address.
			 *
			 *  Without interpreting the code of the function,
			 *  we don't know how may words have been pushed,
			 *  so we punt and look down the stack until we
			 *  find an address that points inside a function.
			 *
			 *  BUG: this code will break if, say, a register
			 *  containing a text address has been pushed.
			 *  We should either own up and put "(probably)"
			 *  after the function name in the stack trace if
			 *  we had to use an offset, or verify that the
			 *  address we found points just after a function call.
			 */
			for (offset = 0; offset < 6 * sizeof(taddr_t); offset += sizeof(taddr_t)) {
				res = dread(sp + offset, (char *)&pc, sizeof(pc));
				if (res != 0 || addr_to_func(pc) != NULL)
					break;
			}
			stk->stk_fp = sp + offset;	/* for saved regs */
			stk->stk_sp = sp;
			if (res != 0)
				break;
			normal_frame = FALSE;
		}
#endif /* ARCH_SUN3 || ARCH_SUN386 */

#if defined(ARCH_SUN386)
		/*  Skip a frame if this is a signal frame.  I don't know
		 *  why this is necessary on the 386i and not on the Sun 3,
		 *  but it makes the stack trace work.
		 */
		if (last != NULL && last->stk_siginfo != NULL) {
			if (dread(fp, (char *)&fbuf, sizeof(fbuf)) != 0)
				break;
			fp = (taddr_t) fbuf.fr_savfp;
		}
#endif

#ifdef ARCH_CLIPPER
		pr = FU_PREAMBLE(f);
		if (!HAS_FP(f)) {
			int res;
			unsigned short text[2];

			stk->stk_sp = sp;
			sp += pr->pr_frame_size;
			stk->stk_fp = sp;
			if (dread(sp, (char *)&pc, sizeof(pc)) != 0)
				break;
			sp += 4;

			/*  If the function pushed arguments on the
			 *  stack the pc should be pointing at the
			 *  code to drop the sp back over the pushed
			 *  arguments.  Check for this, and adjust
			 *  the sp accordingly if we find the instruction.
			 *
			 *  We're looking for addq $n, sp or addi $n, sp.
			 *
			 *  BUG: the proc_read_text/textfile_tread stuff
			 *       is bogus: proc_read_text should check
			 *       for proc == NULL itself.  Would need to
			 *	 do something about the symtab_id argument.
			 */
			if (proc != NULL)
				res = proc_read_text(proc, pc,
						     (char *)text, sizeof(text));
			else
				res = textfile_tread(f->fu_symtab_id, pc,
						     (char *)text, sizeof(text));
			if (res != 0)
				break;
			if ((*text & 0xff0f) == 0x820f)
				sp += (*text >> 4) & 0xf;
			else if (*text == 0x83bf)
				sp += (short)text[1];
			
			normal_frame = FALSE;
			must_find_fp = TRUE;
		}
#endif

		if (normal_frame) {
#ifdef ARCH_CLIPPER
			if (must_find_fp) {
				taddr_t fpaddr;

				fpaddr = reg_addr(last, 14); /* fp is r14 */
				if (fpaddr != 0 && proc_read_data(proc,
						   fpaddr, (char *)&fp, 4) != 0)
					break;
			}
			must_find_fp = FALSE;
#endif
#ifdef ARCH_VAX
			stk->stk_ap = ap;
#endif
			stk->stk_fp = fp;
			stk->stk_sp = sp;
			if (dread(fp, (char *)&fbuf, sizeof(fbuf)) != 0)
				break;
			sp = fp + sizeof(fbuf);

#ifdef DEBUG_STACK
			if (Debug_flags & DBFLAG_STACK) {
				fprintf(stderr,
					"bst: frame fp=0x%x: fp=0x%x pc=0x%x\n",
					fp, fbuf.fr_savfp, fbuf.fr_savpc);
			}
#endif

			fp = (taddr_t)fbuf.fr_savfp;
#ifdef ARCH_VAX
			ap = (taddr_t)fbuf.fr_savap;
#endif
			pc = fbuf.fr_savpc;
		}

#ifdef ARCH_BSDI386
		/*  If the pc points at the signal trampoline code in the u
		 *  area, we assume that the target has taken a signal.
		 */
#define u ((struct user *)USRSTACK)
		if (pc >= (taddr_t)u->u_pcb.pcb_sigc &&
		    pc < (taddr_t)u->u_pcb.pcb_sigc + sizeof u->u_pcb.pcb_sigc){
#undef u
			struct { int signo, code; taddr_t scp; } sigargs;
			taddr_t scp;

			if (proc_read_data(proc, sp,
				     (char *)&sigargs, sizeof(sigargs)) != 0)
				break;
			if (proc_read_data(proc, sigargs.scp,
					    (char *)&scbuf, sizeof(scbuf)) != 0)
				break;

			stk->stk_siginfo = make_siginfo(sigargs.signo);

			sp = scbuf.sc_sp;
			pc = scbuf.sc_pc;
			fp = scbuf.sc_fp;
		}
#endif
#ifdef ARCH_CLIPPER
		/*  If the pc points into the u area, we assume that
		 *  the target has taken a signal, and that the pc is
		 *  in the signal trampoline code.
		 */
		if (pc >= (taddr_t)USRSTACK &&
				      pc < (taddr_t)USRSTACK + UPAGES * NBPG) {
			taddr_t scp;
			siginfo_t *si;
			int signo;

			if (proc_read_data(proc, sp,
					     (char *)&scp, sizeof(scp)) != 0)
				break;
			if (proc_read_data(proc, scp,
					    (char *)&scbuf, sizeof(scbuf)) != 0)
				break;
			if (proc_read_data(proc, scp - 20,
					    (char *)&signo, sizeof(signo)) != 0)
				break;

			si = make_siginfo(signo);
			si->si_fp = scp - 80; /* From looking at stack dumps */
			si->si_prbuf.pr_rsave_mask = (1 << 14) - 1; /* savew0 */
			si->si_prbuf.pr_rsave_offset = 0;
			stk->stk_siginfo = si;

			sp = scbuf.sc_sp;
			pc = scbuf.sc_pc;
		}
#endif
#ifdef ARCH_VAX
		/*  Check for signal generated stack frames on the VAX.
		 *
		 *  Assumption: if this is a callg frame rather than a calls
		 *  one, then this frame was generated by a signal.
		 *  Furthur assumptions come from poking around in VAX
		 *  stack dumps.
		 *
		 *  As the man says, you are not expected to understand this.
		 */
		if (!fbuf.fr_s) {
			taddr_t scp;
			int signo;

			/*  The hairiest bit of all: get the pc from
			 *  the arguments of the signal catching routine.
			 *  What we are getting is roughly
			 *
			 *	((struct sigcontext *)ap[3])->sc_pc
			 *
			 *  The 3 comes from the fact that scp is documented
			 *  in sigvec(2) as the third argument to a signal
			 *  catcher.  This would make you expect ap[2], but
			 *  VAX args are always shifted by one.
			 */
			if (dread(ap + 3 * sizeof(taddr_t), (char *)&scp,
								sizeof(scp)) != 0)
				break;
			if (dread(scp, (char *)&scbuf, sizeof(scbuf)) != 0)
				break;
			pc = scbuf.sc_pc;

			/*  The signal number is the first argument of the signal
			 *  catcher.
			 */
			if (dread(ap + sizeof(taddr_t), (char *)&signo,
								sizeof(signo)) != 0)
				break;

			/*  Now skip a frame.
			 */
			if (dread(fp, (char *)&fbuf, sizeof(fbuf)) != 0)
				break;
			fp = (taddr_t)fbuf.fr_savfp;
			ap = (taddr_t)fbuf.fr_savap;
			stk->stk_siginfo = make_siginfo(signo);
		}
#endif /* ARCH_VAX */
	}
	if (fp != NULL && stk != NULL)
		stk->stk_bad = TRUE;

	return discard_junk_frames(stk);
}
#endif /* !ARCH_SUN4 && !ARCH_MIPS */

#ifndef ARCH_MIPS
/*  Discard any frames outside the main function.
 *
 *  Must do it this way round as the main function may be called
 *  recursively.
 */
static stack_t *
discard_junk_frames(stk)
stack_t *stk;
{
	extern func_t *Main_func;

	if (Main_func != NULL) {
		stack_t *outer, *next;

		outer = stk;
		while (stk != NULL && !stk->stk_bad && stk->stk_func != Main_func)
			stk = stk->stk_inner;
		if (stk != NULL && stk->stk_func == Main_func) {
			for (; outer != stk; outer = next) {
				next = outer->stk_inner;
				free_stk(outer);
			}
		}
		stk = outer;
	}

	return stk;
}
#endif

taddr_t
reg_addr(stk, reg)
stack_t *stk;
int reg;
{
#ifdef ARCH_SUN4
	if (stk == NULL)
		return 0;
	else if (reg < 32) {
		taddr_t fp;
		extern proc_t Current_proc;	/* TEMPORARY */

		if (reg < 8)
			panic("reg_addr: can't do global regs");

		if (reg < 16) {
			stk = stk->stk_inner;
			reg += 16;
		}

		/*  Integer register.  Just return the address in the
		 *  register window (as it appears on the stack).
		 *
		 *  On the SPARC, a called function conceptually saves
		 *  the caller's local and in registers on the stack,
		 *  as the first 16 words of it's stack frame (the
		 *  kernel hides the register window gunk from us).
		 */
		if (stk == NULL)
			fp = proc_getreg(Current_proc, REG_SP);
		else if (stk->stk_siginfo != NULL && stk->stk_siginfo->si_fp != 0)
			fp = stk->stk_siginfo->si_fp;
		else if (HAS_FP(stk->stk_func))
			fp = stk->stk_fp;
		else
			return reg_addr(stk->stk_inner, reg);
		return fp + (reg - 16) * 4;
	}
	else if (reg < 64) {
#define STF	((3 << 30) | (044 << 19))
#define NINST	32		/* Max 32 fp regs (paranoia) */
#define SPREG	14		/* SPARC reg %o6 */
		int text[NINST];
		int i;

		/*  Floating point register.  Search back through the
		 *  instructions for the floating point save.
		 */

		stk = stk->stk_outer;

		reg -= 32;

		if (textfile_tread(stk->stk_func->fu_symtab_id,
				   stk->stk_pc - sizeof(text),
				   (char *)text,
				   sizeof(text)) != 0) {
			return 0;	/* BUG: not right */
		}
		
		for (i = NINST - 1; i > 0; --i) {
			int inst, val, expected;

			inst = text[i];
			val = inst & ~((1 << 12) - 1);
			expected = STF | (reg << 25) | (SPREG << 14) | (1 << 13);
			if (val == expected) {
				return stk->stk_inner->stk_fp +
							(inst & ((1 << 12) - 1));
			}
		}
	}
	else
		panic("reg_addr: reg > 63");

	return 0;
#else
	preamble_t *pr;
	taddr_t regaddr;
	unsigned rmask;
	siginfo_t *si;
#ifdef ARCH_386
	int i;
#else
	unsigned mask;
#endif

#ifdef ARCH_SUN3
	/*  Registers 18 to 25 are the 68881 floating point registers.
	 */
	if (reg >= 18 && reg < 26) {
		reg -= 18;
		rmask = 0x80 >> reg;
		si = NULL;
		while (stk != NULL) {
			taddr_t fp;

			if (si == NULL && stk->stk_siginfo != NULL &&
						stk->stk_siginfo->si_fp != 0) {
				si = stk->stk_siginfo;
				fp = si->si_fp;
				pr = &si->si_prbuf;
			}
			else {
				fp = stk->stk_fp;
				pr = FU_PREAMBLE(stk->stk_func);
				stk = stk->stk_inner;
			}
			if (pr->pr_fpreg_rsave_mask & rmask) {
				regaddr = fp + pr->pr_fpreg_rsave_offset;
				for (mask = 0x80; mask != rmask; mask >>= 1)
					if (pr->pr_fpreg_rsave_mask & mask)
						regaddr += 12;
				return regaddr;
			}
		}
		return 0;
	}
#endif /* ARCH_MIPS */
#ifdef ARCH_MIPS
	/*  Registers 32 to 63 are the MIPS floating point registers.
	 */
	if (reg >= 32 && reg < 64) {
		rmask = 1 << (reg - 32);
		si = NULL;
		while (stk != NULL) {
			taddr_t fp;

			if (si == NULL && stk->stk_siginfo != NULL &&
						stk->stk_siginfo->si_fp != 0) {
				si = stk->stk_siginfo;
				fp = si->si_fp;
				pr = &si->si_prbuf;
			}
			else {
				fp = stk->stk_fp;
				pr = FU_PREAMBLE(stk->stk_func);
				stk = stk->stk_inner;
			}
			if (pr->pr_fpreg_rsave_mask & rmask) {
				regaddr = fp + pr->pr_fpreg_rsave_offset;
				for (mask = 1 << 31; mask != rmask; mask >>= 1)
					if (pr->pr_fpreg_rsave_mask & mask)
						regaddr -= 4;
				return regaddr;
			}
		}
		return 0;
	}
#endif /* ARCH_MIPS */
#ifdef ARCH_CLIPPER
	/*  Registers 16 to 32 are the Clipper fp registers.
	 */
	if (reg >= 16 && reg < 32) {
		if (reg & 1)
			panic("odd reg in gra");
		reg = (reg - 16) / 2;
		rmask = 1 << reg;
		si = NULL;
		while (stk != NULL) {
			taddr_t fp;

			if (si == NULL && stk->stk_siginfo != NULL &&
						stk->stk_siginfo->si_fp != 0) {
				si = stk->stk_siginfo;
				fp = si->si_fp;
				pr = &si->si_prbuf;
			}
			else {
				fp = stk->stk_fp;
				pr = FU_PREAMBLE(stk->stk_func);
				stk = stk->stk_inner;
			}
			if (pr->pr_fpreg_rsave_mask & rmask) {
				regaddr = fp + pr->pr_fpreg_rsave_offset;
				for (mask = 0x1; mask != rmask; mask <<= 1)
					if (pr->pr_fpreg_rsave_mask & mask)
						regaddr += 8;
				return regaddr;
			}
		}
		return 0;
	}
#endif

	if (reg < 0 || reg > 31)
		panic("regno botch in gra");

	rmask = 1 << reg;
	si = NULL;
	while (stk != NULL) {
		taddr_t fp;

		if (si == NULL && stk->stk_siginfo != NULL &&
						stk->stk_siginfo->si_fp != 0) {
			si = stk->stk_siginfo;
			fp = si->si_fp;
			pr = &si->si_prbuf;
		}
		else {
			fp = stk->stk_fp;
			pr = FU_PREAMBLE(stk->stk_func);
			stk = stk->stk_inner;
		}

		if (pr->pr_rsave_mask & rmask) {
			regaddr = fp + pr->pr_rsave_offset;
#ifdef ARCH_386
			for (i = 0; i < N_PUSHABLE_REGS; ++i) {
				regaddr -= 4;
				if (pr->pr_regtab[i] == reg)
					return regaddr;
			}
			panic("reg botch in gra");
			return 0;	/* to satisfy gcc */
#else
#ifdef ARCH_MIPS
			for (mask = 1 << 31; mask != rmask; mask >>= 1)
				if (pr->pr_rsave_mask & mask)
					regaddr -= 4;
#else
			for (mask = 0x1; mask != rmask; mask <<= 1)
				if (pr->pr_rsave_mask & mask)
					regaddr += 4;
#endif
			return regaddr;
#endif
		}
	}

	return 0;
#endif /* !ARCH_SUN4 */
}
