/* trun_j.c - target control when we know where the jumps are */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_trun_j_c_sccsid[] = "@(#)trun_j.c	1.25 20/5/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <sys/types.h>
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>

#include <local/ukcprog.h>

#include "ups.h"
#include "symtab.h"
#include "proc.h"
#include "breakpoint.h"
#include "as.h"

#include "trun.h"
#include "data.h"
#include "exec.h"
#include "obj_stack.h"
#include "text.h"
#include "obj_signal.h"
#include "obj_bpt.h"

#ifdef KNOW_ASM

/*  Needed for temporary gross hack.
 */
extern proc_t Current_proc;

static proc_t continue_target PROTO((proc_t proc, breakpoint_t stop_bp,
							stopres_t *p_whystopped));
static int jump_needed PROTO((jump_t *j));
static jump_t *get_lno_jumps PROTO((func_t *f, lno_t *lno, int want_calls,
					     taddr_t *p_addr, taddr_t *p_nextaddr));
static stopres_t cont_target_over_sigs PROTO((proc_t proc));
static proc_t bump_target PROTO((proc_t proc, rtype_t rtype,
							stopres_t *p_whystopped));

/*  If we can't remove a breakpoint at the end of a next, step,
 *  we store in Stuck_bp so that next time run_target() is invoked it can
 *  have another go at clearing it.
 */
static breakpoint_t Stuck_bp = 0;

static proc_t
continue_target(proc, stop_bp, p_whystopped)
proc_t proc;
breakpoint_t stop_bp;
stopres_t *p_whystopped;
{
	stopres_t whystopped;
	int stop, sig;
	bool should_uninstall_stop_bp;
	breakpoint_t bp;
	taddr_t orig_fp;

	orig_fp = 0;	/* but may be changed below */
	should_uninstall_stop_bp = FALSE;

	if (proc != 0) {
		if (stop_bp != 0) {
			if (!breakpoint_is_installed(stop_bp)) {
				if (install_breakpoint(stop_bp, proc) != 0)
					return proc;
				should_uninstall_stop_bp = TRUE;
			}

			if (proc_getreg(proc, REG_PC) == breakpoint_to_addr(stop_bp))
				orig_fp = proc_getreg(proc, REG_FP);
		}
		else {
			if (install_all_breakpoints(proc) != 0)
				return proc;
		}
	}

	do {
		if (proc == 0) {
			proc = start_target(&whystopped);
			if (proc == 0)
				break;
		}
		else
			whystopped = proc_cont(proc, get_restart_sig(proc), PR_CONT);
		
		switch(whystopped) {
		case SR_DIED:
		case SR_USER:
		case SR_FAILED:
			stop = TRUE;
			break;
		case SR_SSTEP:
			/*  This should never happen, but it seems to
			 *  because of an oddity with signals, ptrace and
			 *  instructions which branch to themselves.
			 */
			stop = FALSE;
			break;
		case SR_SIG:
			sig = proc_get_lastsig(proc);
			if (sig_kills_target_by_default(sig) &&
					proc_get_sigstate(proc, sig) == SGH_DEFAULT)
				stop = TRUE;
			else
				stop = stop_bp == 0 && sig_stops_target(sig);
			if (!stop && sig_causes_refresh(sig))
				refresh_target_display(proc, whystopped);
			break;
		case SR_BPT:
			bp = get_breakpoint_at_addr(proc, proc_getreg(proc, REG_PC));

			Current_proc = proc;	/* BUG: fix this */

			stop = execute_bp_code(bp, proc_getreg(proc, REG_FP),
						   proc_getreg(proc, REG_AP));
			whystopped = proc_get_stopres(proc);
			if (whystopped != SR_BPT)
				stop = TRUE;
			if (stop_bp != 0)
				stop = bp == stop_bp &&
					proc_getreg(proc, REG_FP) >= orig_fp;
			break;
		default:
			panic("stopres botch in ct");
			stop = 0; /* to satisfy gcc */
		}
	} while (!stop);
	
	if (should_uninstall_stop_bp && breakpoint_is_installed(stop_bp)) {
		if (uninstall_breakpoint(stop_bp) != 0)
			Stuck_bp = stop_bp;
	}

	*p_whystopped = whystopped;
	return proc;
}

/*  Return TRUE if a jump is needed.  The only jumps that aren't needed
 *  are calls to a known address where the known address is within a
 *  function for which we don't have symbol table information.
 *
 *  You might think we could also skip branches to a destination within
 *  the line, but we can't because these jumps might jump over the
 *  breakpoint we have set.
 */
static int
jump_needed(j)
register jump_t *j;
{
	func_t *f;

	if (j->ju_type == JT_CALL && j->ju_dstaddr != 0) {
		f = addr_to_func(j->ju_dstaddr);
		return f != NULL && FU_LNOS(f) != NULL;
	}

	return TRUE;
}

static jump_t *
get_lno_jumps(f, lno, want_calls, p_addr, p_nextaddr)
func_t *f;
lno_t *lno;
int want_calls;
taddr_t *p_addr, *p_nextaddr;
{
	taddr_t addr, nextaddr;
	jump_t *jumps, *src, *dst;
	char *text;
	int len;

	if (lno == NULL || f == NULL)
		panic("lno or f NULL in get_lno_jumps");
	addr = lno->ln_addr;
	if (lno->ln_next != NULL)
		nextaddr = lno->ln_next->ln_addr;
	else
		nextaddr = get_addr_lim(f);

	len = nextaddr - addr;
	text = e_malloc(len);
	if (textfile_tread(f->fu_symtab_id, addr, text, len) != 0) {
		errf("Can't read text of %s (%m)", f->fu_name);
		return NULL;
	}
	jumps = get_jumps(addr, text, len, want_calls, TRUE);
	free(text);

	src = dst = jumps;
	for(;;) {
		if (jump_needed(src)) {
			if (src != dst)
				*dst = *src;
			++dst;
		}
		if (src->ju_type == JT_END)
			break;
		++src;
	}

	*p_addr = addr;
	*p_nextaddr = nextaddr;
	return jumps;
}

static stopres_t
cont_target_over_sigs(proc)
proc_t proc;
{
	stopres_t whystopped;
	taddr_t lastsp, lastpc;
	int lastsig;

	lastsp = lastpc = 0;
	lastsig = 0;

	for (;;) {
		taddr_t sp, pc;
		int sig;

		whystopped = proc_cont(proc, get_restart_sig(proc), PR_CONT);
		if (whystopped != SR_SIG)
			break;

		sig = proc_get_lastsig(proc);
		pc = proc_getreg(proc, REG_PC);
		sp = proc_getreg(proc, REG_SP);

		if (pc == lastpc && sp == lastsp && sig == lastsig)
			break;
		lastpc = pc;
		lastsp = sp;
		lastsig = sig;

		if (sig_kills_target_by_default(sig) &&
				proc_get_sigstate(proc, sig) == SGH_DEFAULT)
			break;

		if (sig_causes_refresh(sig))
			refresh_target_display(proc, whystopped);
	}
	return whystopped;
}

static proc_t
bump_target(proc, rtype, p_whystopped)
proc_t proc;
rtype_t rtype;
stopres_t *p_whystopped;
{
	static char error_stackfile[] = "ups_stack.error";
	taddr_t addrlim;
	jump_t *jumps, *j;
	lno_t *lno, *old_lno;
	func_t *f, *old_f;
	taddr_t fp, orig_fp, adjusted_pc, pc, last_pc, addr, nextaddr, bpt_addr;
	stopres_t whystopped;
	breakpoint_t bp, tmp_bp;

	get_current_func(&f, &orig_fp, &pc, &adjusted_pc);

	/*  We want the adjusted pc (see get_current_func) for the
	 *  bpt_addr == pc test below.  This is to avoid a false
	 *  positive when the saved pc after a function call points
	 *  at the next source line.
	 */
	pc = adjusted_pc;

	if (f == NULL || (lno = addr_to_lno(f, pc)) == NULL) {
		errf("Can't step in function with no line number information");
		return proc;
	}
	addrlim = get_addr_lim(f);
	jumps = get_lno_jumps(f, lno, rtype == RT_STEP,  &addr, &nextaddr);

	if (jumps == NULL || uninstall_all_breakpoints(proc) != 0)
		return proc;
	
	bp = 0;
	for(;;) {
		/*  Find the first jump instruction after or at the pc.
		 */
		for (j = jumps; j->ju_type != JT_END && j->ju_addr < pc; ++j)
			;
		bpt_addr = j->ju_type != JT_END ? j->ju_addr : nextaddr;
		if (bpt_addr < pc) {
			dump_stack_to_file(error_stackfile);
			panic("bpt addr botch");
		}

		/*  If we are not already at the jump, set a breakpoint
		 *  at the jump and continue the target to let it get there.
		 *  Check that we got to the breakpoint we were expecting.
		 */
		if (bpt_addr == pc) {
			whystopped = SR_BPT;
			fp = proc_getreg(proc, REG_FP);
		}
		else {
			bp = add_breakpoint(bpt_addr);
			if (install_breakpoint(bp, proc) != 0) {
				whystopped = SR_FAILED;
				break;
			}
			for(;;) {
				whystopped = cont_target_over_sigs(proc);
				if (whystopped != SR_BPT) {
					fp = 0; /* to satisfy gcc */
					break;
				}
				fp = proc_getreg(proc, REG_FP);
				if (fp >= orig_fp)
					break;
			}
			if (remove_breakpoint(bp) != 0)
				break;
			bp = 0;
			if (whystopped != SR_BPT)
				break;
			pc = proc_getreg(proc, REG_PC);
			if (pc != bpt_addr)
				panic("unexpected pc in bt");
		}


		/*  If we have reached the next line, we've finished the
		 *  next or step.
		 *
		 *  As we fell through to this line rather than jumping,
		 *  if there is a breakpoint at the line, execute the
		 *  code associated with the breakpoint.
		 */
		if (bpt_addr == nextaddr && (rtype == RT_STEP || fp >= orig_fp)) {
			breakpoint_t bp_at_nextline;

			bp_at_nextline = addr_to_breakpoint(pc);
			if (bp_at_nextline != NULL) {
				execute_bp_code(bp_at_nextline, fp,
							proc_getreg(proc, REG_AP));
				whystopped = proc_get_stopres(proc);
			}
			break;
		}

		/*  OK, we are now sitting at a jump instruction of some sort.
		 *  Single step to execute that instruction and find out where we
		 *  jumped to.
		 */
		whystopped = proc_cont(proc, get_restart_sig(proc), PR_STEP);
		if (whystopped != SR_SSTEP)
			break;

		last_pc = pc;
		pc = proc_getreg(proc, REG_PC);
		fp = proc_getreg(proc, REG_FP);

		/*  If we are still at the same line and stack level, continue.
		 */
		if (pc >= addr && pc < nextaddr && fp <= orig_fp) {
			if (pc < last_pc)
				break;
			else
				continue;
		}
		
		/*  Off the line - find the new lno and func.
		 *  We save the old lno first because we want to check
		 *  that we've moved onto a different source line (you
		 *  can get multiple lnos with the same source line
		 *  number).
		 */
		old_lno = lno;
		old_f = f;
		if (pc == nextaddr && lno->ln_next != NULL)
			lno = lno->ln_next;
		else {
			if (pc < f->fu_addr || pc >= addrlim) {
				f = addr_to_func(pc);
				if (f == NULL)
					break;
				addrlim = get_addr_lim(f);
			}
			lno = addr_to_lno(f, pc);

			/*  If we don't know which lno we're at after
			 *  a branch, stop.
			 *
			 *  This can happen when returning to function
			 *  with no symbol table information (a RET
			 *  instruction is treated as a branch).
			 *
			 *  A call to a function usually puts the pc in
			 *  the function prologue, so we don't mind not
			 *  knowing which lno we're at after a call.
			 *  We move the pc onwards below in this case.
			 */
			if (j->ju_type != JT_CALL && lno == NULL)
				break;
		}

		/*  We're done if we're at a source line boundary with
		 *  a different source line number than the current lno.
		 *
		 *  Note that it is possible to jump from within a source
		 *  line to the start of the line.  One example of this is
		 *  the SPARC, where we jump over the code for the body of
		 *  a switch statement to the switch itself.  The compiler
		 *  emits two line number symbols referring the the source
		 *  line containing the switch, and we jump from the first
		 *  to the start of the second.
		 */
		if (lno != NULL) {
			bool on_boundary, new_func, new_lnum, new_level;
			
			on_boundary = pc == lno->ln_addr;
			new_func = f != old_f;
			new_lnum = lno->ln_num != old_lno->ln_num;
			new_level = fp != orig_fp;

			if (on_boundary && (new_func || new_lnum || new_level))
				break;
		}
		
		/*  If we have just stepped into a function, move the pc
		 *  up to the minimum breakpoint address and stop.
		 */
		if (j->ju_type == JT_CALL) {
			taddr_t call_bpt_addr;
			breakpoint_t bp_at_start_of_func;

			call_bpt_addr = min_bpt_addr(f);
			bp_at_start_of_func = addr_to_breakpoint(call_bpt_addr);

			if (pc == call_bpt_addr)
				whystopped = SR_BPT;
			else {
				if (bp_at_start_of_func != 0) {
					bp = bp_at_start_of_func;
					tmp_bp = 0;
				}
				else {
					bp = add_breakpoint(call_bpt_addr);
					tmp_bp = bp;
				}
				if (install_breakpoint(bp, proc) != 0)
					break;
				whystopped = proc_cont(proc, 0, PR_CONT);
				if (tmp_bp != 0 && remove_breakpoint(tmp_bp) != 0)
					break;
				bp = 0;
			}
			if (whystopped != SR_BPT)
				break;
			if (proc_getreg(proc, REG_PC) != call_bpt_addr) {
				dump_stack_to_file(error_stackfile);
				panic("pc escaped in bump_target");
			}
			if (bp_at_start_of_func != NULL) {
				execute_bp_code(bp_at_start_of_func,
							proc_getreg(proc, REG_FP),
							proc_getreg(proc, REG_AP));
				whystopped = proc_get_stopres(proc);
			}
			break;
		}
		
		/*  Move focus to the new line.
		 */
		jumps = get_lno_jumps(f, lno, rtype == RT_STEP, &addr, &nextaddr);
		orig_fp = proc_getreg(proc, REG_FP);
		if (jumps == NULL)
			break;
	}
	if (bp != 0 && remove_breakpoint(bp) != 0)
		Stuck_bp = bp;
	*p_whystopped = whystopped;
	return proc;
}

proc_t
run_target(proc, stop_bp, rtype)
proc_t proc;
breakpoint_t stop_bp;
rtype_t rtype;
{
	stopres_t whystopped;

	if (proc == 0 && rtype != RT_CONT)
		panic("bad rtype in run_target");

	if (Stuck_bp != 0) {
		if (remove_breakpoint(Stuck_bp) == 0)
			Stuck_bp = 0;
		else
			return proc;
	}

	/*  Set whystopped to anything other than SR_DIED or SR_SIG.
	 */
	whystopped = SR_BPT;

	if (rtype == RT_CONT)
		proc = continue_target(proc, stop_bp, &whystopped);
	else {
		proc = bump_target(proc, rtype, &whystopped);
		if (whystopped != SR_DIED) {
			taddr_t sp;

			sp = proc_getreg(proc, REG_SP);
			if (sp > proc_get_base_sp(proc))
				proc = continue_target(proc, (breakpoint_t)0,
									&whystopped);
		}
	}

	if (whystopped == SR_DIED) {
		proc_free(proc);
		proc = 0;
#ifdef OS_SUNOS_4
		unload_shared_library_symtabs();
#endif
	}

	refresh_target_display(proc, whystopped);
	return proc;
}
#endif /* KNOW_ASM */
