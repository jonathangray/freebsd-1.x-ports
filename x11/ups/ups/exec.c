/* exec.c - code for starting and managing target execution */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_exec_c_sccsid[] = "@(#)exec.c	1.34 17/9/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/errno.h>
extern int errno;

#include <local/wn.h>
#include <local/arg.h>
#include <local/obj/obj.h>

#include <local/ukcprog.h>

#include "ups.h"
#include "symtab.h"
#include "ci.h"
#include "cursors.h"
#include "proc.h"
#include "core.h"
#include "text.h"
#include "exec.h"
#include "breakpoint.h"
#include "obj_stack.h"
#include "obj_target.h"
#include "obj_signal.h"
#include "obj_buildf.h"
#include "obj_bpt.h"
#include "trun.h"
#include "printf.h"
#include "ui.h"
#include "menudata.h"
#include "tdr.h"
#include "state.h"

/*  Process we are currently debugging.  BUG: GET RID OF THIS - IT'S GROSS.
 */
proc_t Current_proc;

/*  Temporary hack.
 */
#define proc_is_process(proc)	((proc) != 0)

void
do_menu_target_command(rv)
int rv;
{
	proc_t proc;

	proc = Current_proc;
	switch(rv) {
	case MR_TGT_CONT:
	case MR_TGT_RUN:
		proc = run_target(proc, (breakpoint_t)0, RT_CONT);
		break;
	case MR_TGT_STEP:
		proc = run_target(proc, (breakpoint_t)0, RT_STEP);
		break;
	case MR_TGT_NEXT:
		proc = run_target(proc, (breakpoint_t)0, RT_NEXT);
		break;
	case MR_TGT_STOP:
		errf("The target is not currently running"); 
		break;
	case MR_WRITE_CORE:
		write_target_core();
		break;
	case MR_TGT_KILL:
		proc_terminate(proc);
		refresh_target_display(proc, SR_DIED);
		proc_free(proc);
		proc = 0;
#ifdef OS_SUNOS_4
		unload_shared_library_symtabs();
#endif
		break;
	default:
		panic("bad cmd in dmtc");
	}
	Current_proc = proc;
}

void
write_target_core()
{
	if (Current_proc != 0 || !proc_is_process(Current_proc))
		proc_write_corefile(Current_proc, "ups_core");
	else
		errf("Target not running");
}

#ifndef OS_SUNOS
/* ARGSUSED */
#endif
int
attach_to_process(name, pid)
const char *name;
int pid;
{
#ifndef OS_SUNOS
	errf("This machine does not support attaching to running processes");
	return -1;
#else
	stopres_t whystopped;

	if (Current_proc != 0)
		panic("proc != 0 in attach_to_process");

	Current_proc = proc_attach(name, pid, &whystopped);

	/* Check that we attached sucessfully.
	 */
	if (Current_proc == 0) {
		errf("Can't attach to process %d (%m)", pid);
		return -1;
	}
	if (whystopped == SR_DIED) {
		errf("Process %d died on being attached to!", pid);
		proc_free(Current_proc);
		Current_proc = 0;
		return -1;
	}

#ifdef OS_SUNOS_4
	load_shared_library_symtabs();
	recalculate_bpt_addrs();
#endif

	reinitialise_bpt_code_data();

	refresh_target_display(Current_proc, whystopped);
	proc_set_base_sp(Current_proc, get_outer_fp());

	return 0;
#endif /* !OS_SUNOS */
}

void
kill_or_detach_from_target()
{
	if (proc_is_attached(Current_proc))
		proc_detach(Current_proc, proc_get_lastsig(Current_proc));
	else
		proc_terminate(Current_proc);
	proc_free(Current_proc);
	Current_proc = 0;
#ifdef OS_SUNOS_4
	unload_shared_library_symtabs();
#endif
}

void
exec_to_lnum(f, lnum)
func_t *f;
int lnum;
{
	cursor_t old_cursor;
	taddr_t addr;
	breakpoint_t bp, tmp_bp;

	if (get_target_state() == TS_STOPPED_AND_CANT_CONTINUE) {
		errf("Can't continue target");
		return;
	}

	if (map_lnum_to_addr(f, lnum, &addr) != 0)
		return;

	bp = addr_to_breakpoint(addr);
	if (bp != NULL)
		tmp_bp = NULL;
	else
		tmp_bp = bp = add_breakpoint(addr);

	set_target_state(TS_RUNNING);

	old_cursor = 0;		/* to satisfy gcc */

	if (td_have_window()) {
		old_cursor = wn_get_window_cursor(WN_STDWIN);
		set_bm_cursor(WN_STDWIN, CU_WAIT);
		update_target_menu_state();
	}
		
	Current_proc = run_target(Current_proc, bp, RT_CONT);

	if (td_have_window()) {
		wn_define_cursor(WN_STDWIN, old_cursor);
		update_target_menu_state();
	}

	if (tmp_bp != NULL)
		remove_breakpoint(tmp_bp);
}

/*  BUG: this function has the side effect of updating Current_proc.
 */
void
refresh_target_display(proc, whystopped)
proc_t proc;
stopres_t whystopped;
{
	breakpoint_t bp;
	taddr_t pc;
	bool old;
	objid_t obj_to_make_visible;

	if (td_have_window())
		wn_updating_off(WN_STDWIN);
	old = td_set_obj_updating(OBJ_UPDATING_OFF);

	if (whystopped == SR_DIED) {
		set_target_state(TS_NOTR);
		close_target_display();
		obj_to_make_visible = NULL;
	}
	else {
		int sig;
		tstate_t tstate;

		sig = proc_get_lastsig(proc);
		tstate = sig_is_fatal(sig) ? TS_STOPPED_AND_CANT_CONTINUE
					   : TS_STOPPED_AND_CAN_CONTINUE;
		set_target_state(tstate);

		pc = proc_getreg(proc, REG_PC);
		bp = (whystopped == SR_BPT) ? addr_to_breakpoint(pc) : 0;

		/*  This is gross, but rebuild_display() calls dread(), which
		 *  uses Current_proc, which may have changed.
		 *  Must think of a better way to do this some time.
		 */
		Current_proc = proc;

		obj_to_make_visible = rebuild_display(proc);
	}

	td_set_obj_updating(old);

	if (obj_to_make_visible != NULL)
		ensure_visible(obj_to_make_visible);

	if (td_have_window())
		wn_updating_on(WN_STDWIN);
}

/*  Start the target process running.
 */
proc_t
start_target(p_whystopped)
stopres_t *p_whystopped;
{
	proc_t proc;
	const char *path, **argv, **envp;
	long rdlist;
	taddr_t stop_addr;
	extern func_t *Main_func;

	if (setup_args(&path, &argv, &envp, &rdlist) != 0) {
		*p_whystopped = SR_DIED;
		return 0;
	}

	proc = proc_start(path, argv, envp, rdlist, p_whystopped);
	if (proc == 0)
		return 0;
	
	stop_addr = min_bpt_addr(Main_func);

	/*  We want the target stopped at the start of main so we can
	 *  get the minimum sp value (proc_set_base_sp) - this is used
	 *  to decide when we have stepped off the end of main.
	 *
	 *  Under SunOS we also need to load shared library information
	 *  after the target has started and the runtime linking has been
	 *  done.  In this case (OS_SUNOS_4) we have to first stop right
	 *  at the start of main, then set another breakpoint after the
	 *  stack setup code (the usual place) and continue the target to
	 *  that.  The reason for this is that run-time linking invoked
	 *  from [start] will overwrite the first user instruction in main
	 *  if it refers to a shared library global, blowing away any
	 *  breakpoint that has been written there.  We assume that the
	 *  stack setup code will not contain such an instruction.
	 */
	if (*p_whystopped == SR_SSTEP) {
#ifdef OS_SUNOS_4
		*p_whystopped = proc_execto(proc, path, Main_func->fu_addr);
		if (*p_whystopped == SR_BPT && Main_func->fu_addr != stop_addr)
			*p_whystopped = proc_execto(proc, path, stop_addr);
#else
		*p_whystopped = proc_execto(proc, path, stop_addr);
#endif
	}

	if (*p_whystopped == SR_DIED) {
		proc_free(proc);
#ifdef OS_SUNOS_4
		unload_shared_library_symtabs();
#endif
		return 0;
	}

	if (*p_whystopped != SR_BPT)
		panic("proc not started properly on start_target");

	proc_set_base_sp(proc, proc_getreg(proc, REG_SP));

#ifdef OS_SUNOS_4
	/*  BUG: MUST find a way to avoid this kind of crap.
	 *  load_shared_library_symtabs() calls dread() which
	 *  uses Current_proc, so set it.
	 */
	Current_proc = proc;
	load_shared_library_symtabs();
	recalculate_bpt_addrs();
#endif /* OS_SUNOS_4 */

	reinitialise_bpt_code_data();

	if (install_all_breakpoints(proc) != 0)
		panic("can't install breakpoints");

	/*  If we have a breakpoint at stop_addr, we don't need to continue
	 *  the target - we are already at the breakpoint.
	 */
	if (get_breakpoint_at_addr(proc, stop_addr) == NULL)
		*p_whystopped = proc_cont(proc, 0, PR_CONT);

	if (*p_whystopped == SR_DIED) {
		proc_free(proc);
		proc = 0;
#ifdef OS_SUNOS_4
		unload_shared_library_symtabs();
#endif
	}

	return proc;
}

ci_exec_result_t
call_target_function(code_id, addr, args, nwords, p_res)
code_id_t code_id;
taddr_t addr;
taddr_t *args;
int nwords;
taddr_t *p_res;
{
	int argno, retval, argslots, type_index;
	const char *mesg;
	int argsizes_buf[40], *argsizes;
#define MAX_ARGSIZES	(sizeof argsizes_buf / sizeof *argsizes_buf)

	if (addr == STOP_ADDR)
		return STOP;
	if (addr == PRINTF_ADDR)
		return ups_printf(Current_proc, code_id, args, nwords);

	if (nwords > MAX_ARGSIZES)
		panic("nwords too large in ctf");

	type_index = nwords;
	argsizes = argsizes_buf;

	for (argno = 0; argno < nwords; argno += argslots, ++type_index) {
		type_t *type;
		int val, i;

		type = (type_t *)args[type_index];
		switch (type->ty_code) {
		case DT_PTR_TO:
			switch (type->ty_base->ty_code) {
			case TY_U_STRUCT:
			case TY_U_UNION:
				val = 0;
				break;
			default:
				val = ci_typesize((lexinfo_t *)NULL, type->ty_base);
				break;
			}
			argslots = 1;
			break;
		case DT_ARRAY_OF:
			val = ci_typesize((lexinfo_t *)NULL, type);
			argslots = 1;
			break;
		case TY_FLOAT:
			val = 0;
			argslots = sizeof(float) / sizeof(long);
			break;
		case TY_DOUBLE:
			val = 0;
			argslots = sizeof(double) / sizeof(long);
			break;
		default:
			val = 0;
			argslots = 1;
			break;
		}
		for (i = 0; i < argslots; i++)
			*argsizes++ = val;
	}

	retval = proc_call_func(Current_proc, code_id, addr,
					args, argsizes_buf, nwords, p_res, &mesg);
	if (mesg != NULL)
		errf("Call of function %s failed: %s (%m)",
				addr_to_func(addr)->fu_name, mesg);

	return (retval == 0) ? CI_ER_CONTINUE : CI_ER_INDIRECT_CALL_FAILED;
}

int
get_restart_sig(proc)
proc_t proc;
{
	int sig;
	
	sig = proc_get_lastsig(proc);
	return (sig != 0 && accept_signal(sig)) ? sig : 0;
}
