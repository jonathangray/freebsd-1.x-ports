/* trun_ss.c - machine target execution control using single stepping */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_trun_ss_c_sccsid[] = "@(#)trun_ss.c	1.17 20/5/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <signal.h>

#include <local/ukcprog.h>
#include "ups.h"
#include "symtab.h"
#include "proc.h"
#include "breakpoint.h"
#include "trun.h"
#include "text.h"
#include "exec.h"
#include "obj_stack.h"
#include "obj_signal.h"
#include "obj_bpt.h"

extern proc_t Current_proc;	/* BUG: gross, etc, etc */

#ifndef KNOW_ASM
#ifdef ARCH_MIPS
static void get_jump_info PROTO((proc_t proc, taddr_t pc,
				 bool *p_stepped_into_func, int *p_rlink_reg));
#endif

/*  Target execution control.  Sit in a loop continuing the target process
 *  proc until we should stop, at which point we refresh the display.
 *  We return the new value of proc (a process may die, or we may need to
 *  start one, so the value can change).
 *
 *  rtype is the way in which we should run the target - RT_CONT, RT_STEP, or
 *  RT_NEXT.
 *
 *  If stop_bp is zero, then we should stop at any breakpoint we hit.  Otherwise
 *  all breakpoints other than stop_bp are ignored.
 *
 *  Warning: this is a hairy namei() type loop, full of breaks and continues.
 */
proc_t
run_target(proc, stop_bp, rtype)
proc_t proc;
breakpoint_t stop_bp;
rtype_t rtype;
{
	stopres_t whystopped;
	cont_type_t cont_type;
	breakpoint_t bp, tmp_bp;
	lno_t *lno;
	bool stepped_into_func, stop, bp_wants_stop, at_right_stack_level;
	int sig, orig_lnum;
	taddr_t orig_fp, jsr_fp, last_sp, jsr_sp, sp;
	taddr_t last_pc, pc, retaddr;
	func_t *orig_f, *last_f, *f;
#ifdef ARCH_MIPS
	int rlink_reg;
#endif

	if (rtype != RT_CONT && stop_bp != 0)
		panic("stop_bp set illegally in run_target");

	cont_type = (rtype == RT_CONT) ? PR_CONT : PR_STEP;
	tmp_bp = 0;
	jsr_sp = 0;
	jsr_fp = 0;

	if (proc != 0) {
		taddr_t atol_pc;	/* pc to give to addr_to_lnum */

		sp = proc_getreg(proc, REG_SP);
		if (rtype == RT_CONT) {
			pc = atol_pc = proc_getreg(proc, REG_PC);
			orig_fp = proc_getreg(proc, REG_FP);
			orig_f = f = addr_to_func(pc);
		}
		else {
			taddr_t fp;

			fp = proc_getreg(proc, REG_FP);
			get_current_func(&f, &orig_fp, &pc, &atol_pc);
			orig_f = f;

			if (fp != orig_fp) {
				stop_bp = addr_to_breakpoint(pc);
				if (stop_bp == NULL) {
					tmp_bp = add_breakpoint(pc);
					stop_bp = tmp_bp;
				}
				if (!breakpoint_is_installed(stop_bp)) {
					if (install_breakpoint(stop_bp, proc) != 0)
						panic("can't install stop_bp");
				}
				jsr_fp = orig_fp;
				cont_type = PR_CONT;
			}
		}
		orig_lnum = (orig_f != NULL) ? addr_to_lnum(orig_f, atol_pc) : 0;
	}
	else {
		/*  No process.  Set sp so that the test "current_sp >= old_sp"
		 *  always succeeds.
		 */
		sp = 0;
		orig_fp = 0;

		/*  The following lines were added because gcc complained
		 *  about them being used unintialised.  I'm not sure if
		 *  these are the right values - this code has not been
		 *  used since these were added.
		 */
		orig_lnum = 0;
		orig_f = f = NULL;
		pc = 0;
	}
	
	bp = 0; /* to satisfy gcc */
	sig = 0; /* to satisfy gcc */

	for(;;) {
		/*  Start, continue or single step the target
		 */
		if (proc == 0) {
			if (rtype != RT_CONT)
				panic("bad rtype in run_target");
			proc = start_target(&whystopped);
			Current_proc = proc;	/* BUG: fix this */
			if (proc == 0)
				break;
			sig = 0;
		}
		else {
			int res;

			if (cont_type == PR_CONT)
				res = install_all_breakpoints(proc);
			else
				res = uninstall_all_breakpoints(proc);
			if (res != 0) {
				whystopped = SR_FAILED;
				break;
			}
			whystopped = proc_cont(proc, get_restart_sig(proc),
									cont_type);

		}
		
		/*  Set stop to indicate whether we should stop now. 
		 *  If we aren't stopping, refresh the display if necessary.
		 */
		switch(whystopped) {
		case SR_DIED:
		case SR_USER:
		case SR_FAILED:
			stop = TRUE;
			break;
		case SR_SSTEP:
			stop = FALSE;	/* but see later */

			/*  If we have arrived at the location of a breakpoint,
			 *  we execute the code associated with it, as a
			 *  next or step to a line with a breakpoint before
			 *  it conceptually executes the code of the breakpoint.
			 *
			 *  Note that we use addr_to_breakpoint here rather
			 *  the get_breakpoint_at_addr because we act on
			 *  a breakpoint even if it's not installed.
			 */
			bp = addr_to_breakpoint(proc_getreg(proc, REG_PC));
			if (bp != NULL) {
				execute_bp_code(bp, proc_getreg(proc, REG_FP),
						     proc_getreg(proc, REG_AP));
				whystopped = proc_get_stopres(proc);
				if (whystopped == SR_DIED)
					stop = TRUE;
			}
			break;
		case SR_SIG:
			sig = proc_get_lastsig(proc);
			if (sig_kills_target_by_default(sig) &&
					proc_get_sigstate(proc, sig) == SGH_DEFAULT)
				stop = TRUE;
			else if (rtype == RT_CONT && stop_bp == 0)
				stop = sig_stops_target(sig);
			else
				stop = FALSE;
			if (!stop && sig_causes_refresh(sig))
				refresh_target_display(proc, whystopped);
			break;
		case SR_BPT:
			bp = get_breakpoint_at_addr(proc, proc_getreg(proc, REG_PC));
			if (bp == NULL)
				panic("bpt botch in rt");
			bp_wants_stop = execute_bp_code(bp,
							proc_getreg(proc, REG_FP),
							proc_getreg(proc, REG_AP));
			whystopped = proc_get_stopres(proc);
			if (whystopped == SR_DIED) {
				stop = TRUE;
				break;
			}

			if (stop_bp == 0)
				stop = bp_wants_stop;
			else if (bp != stop_bp)
				stop = FALSE;
			else if (rtype == RT_STEP || rtype == RT_NEXT)
				stop = FALSE;
			else
				stop = proc_getreg(proc, REG_FP) >= orig_fp;
			break;
		default:
			panic("stopres botch in rt");
			stop = 0; /* to satisfy gcc */
		}

		/*  Get the simple cases out of the way.
		 */
		if (stop)
			break;
		if (rtype == RT_CONT)
			continue;
		
		/*  At this point, the following assertions hold:
		 *
		 *	We aren't stopping
		 *	rtype is RT_NEXT or RT_STEP
		 *	whystopped is SR_SSTEP or SR_BPT or SR_SIG.
		 *
		 *  First, check the above.
		 */
		if (stop || (rtype != RT_NEXT && rtype != RT_STEP) ||
			    (whystopped != SR_SSTEP &&
			     whystopped != SR_BPT && whystopped != SR_SIG))
			panic("assertion failed in run_target");


		/*  Find out some things about the target state.
		 */
		last_f = f;
		last_pc = pc;
		last_sp = sp;
		pc = proc_getreg(proc, REG_PC);
		sp = proc_getreg(proc, REG_SP);

		/*  If we have nexted or stepped off the end of main,
		 *  we are back in start(), so finish things off by
		 *  doing a cont.  We test for this by seeing if the
		 *  stack pointer is below the stack pointer of main.
		 */
		if (sp > proc_get_base_sp(proc)) {
			cont_type = PR_CONT;
			continue;
		}
		
		f = addr_to_func(pc);
		lno = (f != NULL) ? addr_to_lno(f, pc) : NULL;

		/*  If we hit stop_bp and we are at the right stack level,
		 *  remove and zero stop_bp.
		 */
		if (jsr_fp != 0)
			at_right_stack_level = proc_getreg(proc, REG_FP) >= jsr_fp;
		else
			at_right_stack_level = sp >= jsr_sp;
		if (whystopped == SR_BPT && bp == stop_bp && at_right_stack_level) {
			uninstall_breakpoint(stop_bp);
			stop_bp = 0;
			jsr_sp = 0;
			if (tmp_bp != 0) {
				remove_breakpoint(tmp_bp);
				tmp_bp = 0;
			}
			cont_type = PR_STEP;
		}
		 
		/*  If we aren't at the top level, we can just carry on.
		 */
		if (stop_bp != 0)
			continue;
		
		/*  Have we just stepped into a function?
		 *  The test is complicated by recursion - we may have
		 *  changed functions even though f hasn't changed.
		 */
#ifdef ARCH_MIPS
		get_jump_info(proc, last_pc, &stepped_into_func, &rlink_reg);
#else
		stepped_into_func = sp < last_sp && (f != last_f || pc <= last_pc);
#endif

		/*  If we have stepped into a function, next over it if we
		 *  are nexting or can't step through it.
		 */
		if (whystopped == SR_SIG &&
					proc_get_sigstate(proc, sig) == SGH_CAUGHT)
			retaddr = proc_get_retaddr_after_sig(proc);
		else if (stepped_into_func && (rtype == RT_NEXT ||
					       f == NULL || 
					       (f->fu_flags & FU_NOSYM) ||
					       open_source_file(f->fu_fil, TRUE) != 0
					      )) {
#ifdef ARCH_MIPS
			retaddr = proc_get_retaddr_after_jsr(proc, rlink_reg);
#else
			retaddr = proc_get_retaddr_after_jsr(proc);
#endif
		}
		else
			retaddr = 0;
		
		if (retaddr != 0) {
			stop_bp = addr_to_breakpoint(retaddr);
			if (stop_bp == NULL) {
				tmp_bp = add_breakpoint(retaddr);
				stop_bp = tmp_bp;
			}
			if (!breakpoint_is_installed(stop_bp)) {
				if (install_breakpoint(stop_bp, proc) != 0)
					panic("can't install stop_bp");
			}
			jsr_sp = sp;
			cont_type = PR_CONT;
			continue;
		}

		/*  We are at the top level, nexting or stepping.
		 *  If we are at a known source line, and the line number
		 *  or function is different from the originals, we stop.
		 */
		if (lno != NULL && (f != orig_f || lno->ln_num != orig_lnum)) {
			if (lno->ln_addr != pc) {
				if (f == orig_f)
					orig_lnum = lno->ln_num;
				continue;
			}
			stop = TRUE; /* unnecessary, but tidy */
			break;
		}
	}

	if (whystopped == SR_DIED && proc != 0) {
		proc_free(proc);
		proc = 0;
		Current_proc = proc;	/* BUG: fix this */
#ifdef OS_SUNOS_4
		unload_shared_library_symtabs();
#endif
	}

	/*  If we added a temporary breakpoint ourself, remove it.
	 */
	if (tmp_bp != 0)
		remove_breakpoint(tmp_bp);

	refresh_target_display(proc, whystopped);
	return proc;
}

#ifdef ARCH_MIPS
static void
get_jump_info(proc, pc, p_stepped_into_func, p_rlink_reg)
proc_t proc;
taddr_t pc;
bool *p_stepped_into_func;
int *p_rlink_reg;
{
#define JAL_CODE	0x0c000000
#define SPECIAL_CODE	0x00000000
#define JALR_CODE	0x00000009
	taddr_t opcode;

	if (proc_read_text(proc, pc, (char *)&opcode, 4) != 0)
		panic("prt failed in gji");
	
	if ((opcode & 0xfc000000) == JAL_CODE) {
		*p_stepped_into_func = TRUE;
		*p_rlink_reg = 31;
	}
	else if ((opcode & 0xfc00001f) == (SPECIAL_CODE | JALR_CODE)) {
		*p_stepped_into_func = TRUE;
		*p_rlink_reg = (opcode >> 11) & 0x1f;
	}
	else {
		*p_stepped_into_func = FALSE;
	}
}
#endif /* ARCH_MIPS */
#endif /* !KNOW_ASM */
