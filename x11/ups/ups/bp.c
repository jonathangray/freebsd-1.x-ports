/* bp.c - low level breakpoint list handling */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_bp_c_sccsid[] = "@(#)bp.c	1.15 26/4/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <stdlib.h>

#include <local/ukcprog.h>

#include "ups.h"
#include "proc.h"
#include "breakpoint.h"

typedef struct bpst {
	struct bpst *bp_next;	/* next entry in the list */
	struct bpst *bp_prev;	/* previous entry in the list */
	proc_t bp_proc;		/* process that bpt is (will be) inserted in */
	taddr_t bp_addr;	/* the text address of the breakpoint */
	opcode_t bp_code;	/* the opcode that has been replaced */
	long bp_user_data;	/* user data - not used by us */
} bp_t;

/*  The HLH Clipper compiler won't accept the Bphead initialisation
 *  below (it doesn't enter names as defined until the end of the
 *  declarator line).  So we add this extern declaration as a workaround.
 *
 *  Other compilers do this as well (cx), and the extern doesn't do any
 *  harm so we don't #ifdef it.
 */
extern bp_t Bphead;

static bp_t Bphead = { &Bphead, &Bphead };

/*  Create an uninstalled breakpoint at address addr.
 */
breakpoint_t
add_breakpoint(addr)
taddr_t addr;
{
	bp_t *bp;

	bp = (bp_t *) e_malloc(sizeof(bp_t));

	bp->bp_next = Bphead.bp_next;
	bp->bp_prev = &Bphead;
	Bphead.bp_next->bp_prev = bp;
	Bphead.bp_next = bp;

	bp->bp_addr = addr;
	bp->bp_proc = 0;
	bp->bp_user_data = 0;
	return (breakpoint_t)bp;
}

void
set_breakpoint_data(breakpoint, data)
breakpoint_t breakpoint;
long data;
{
	((bp_t *)breakpoint)->bp_user_data = data;
}

long
get_breakpoint_data(breakpoint)
breakpoint_t breakpoint;
{
	return ((bp_t *)breakpoint)->bp_user_data;
}

/*  Remove a breakpoint. Uninstall it first if necessary.
 */
int
remove_breakpoint(breakpoint)
breakpoint_t breakpoint;
{
	register bp_t *bp;

	bp = (bp_t *)breakpoint;
	if (bp->bp_proc != 0)
		if (uninstall_breakpoint((breakpoint_t)bp) != 0)
			return -1;

	bp->bp_prev->bp_next = bp->bp_next;
	bp->bp_next->bp_prev = bp->bp_prev;

	free((char *)bp);

	return 0;
}

/*  Return non zero if breakpoint is installed.
 */
int
breakpoint_is_installed(breakpoint)
breakpoint_t breakpoint;
{
	return ((bp_t *)breakpoint)->bp_proc != 0;
}

/*  Install a breakpoint (e.g. write TRAP opcode into target).
 */
int
install_breakpoint(breakpoint, proc)
breakpoint_t breakpoint;
proc_t proc;
{
	bp_t *bp;

	bp = (bp_t *)breakpoint;
	if (proc_tswap(proc, bp->bp_addr, (opcode_t)0, &bp->bp_code) != 0) {
		errf("Can't install breakpoint (%m)");
		return -1;
	}
	bp->bp_proc = proc;
	return 0;
}

/*  Uninstall a breakpoint.
 */
int
uninstall_breakpoint(breakpoint)
breakpoint_t breakpoint;
{
	bp_t *bp;

	bp = (bp_t *)breakpoint;
	if (proc_tswap(bp->bp_proc, bp->bp_addr, bp->bp_code,
							(opcode_t *)NULL) != 0) {
		errf("Can't uninstall breakpoint (%m)");
		return -1;
	}
	bp->bp_proc = 0;
	return 0;
}

/*  Uninstall any breakpoints installed in proc.
 */
int
uninstall_all_breakpoints(proc)
proc_t proc;
{
	register bp_t *bp;

	for (bp = Bphead.bp_next; bp != &Bphead; bp = bp->bp_next)
		if (bp->bp_proc == proc)
			if (uninstall_breakpoint((breakpoint_t)bp))
				return -1;
	return 0;
}

/*  Install any uninstalled breakpoints.
 */
int
install_all_breakpoints(proc)
proc_t proc;
{
	register bp_t *bp;

	for (bp = Bphead.bp_next; bp != &Bphead; bp = bp->bp_next)
		if (bp->bp_proc == 0)
			if (install_breakpoint((breakpoint_t)bp, proc))
				return -1;
	return 0;
}

/*  If there is a breakpoint installed at addr in proc, return the breakpoint.
 *  Otherwise, return 0.
 */
breakpoint_t
get_breakpoint_at_addr(proc, addr)
proc_t proc;
taddr_t addr;
{
	register bp_t *bp;

	for (bp = Bphead.bp_next; bp != &Bphead; bp = bp->bp_next)
		if (bp->bp_proc == proc && bp->bp_addr == addr)
			return (breakpoint_t)bp;
	return NULL;
}

/*  Return the breakpoint corresponding to address addr, even if breakpoint
 *  is not currently installed.
 *
 *  If there is no breakpoint at addr, return 0.
 */
breakpoint_t
addr_to_breakpoint(addr)
taddr_t addr;
{
	register bp_t *bp;

	for (bp = Bphead.bp_next; bp != &Bphead; bp = bp->bp_next)
		if (bp->bp_addr == addr)
			return (breakpoint_t)bp;
	return NULL;
}

taddr_t
breakpoint_to_addr(breakpoint)
breakpoint_t breakpoint;
{
	return ((bp_t *)breakpoint)->bp_addr;
}

/*  Run through the breakpoint list marking breakpoints as not installed.
 *  Called when the target process exits.
 */
void
mark_breakpoints_as_uninstalled(proc)
proc_t proc;
{
	register bp_t *bp;

	for (bp = Bphead.bp_next; bp != &Bphead; bp = bp->bp_next)
		if (bp->bp_proc == proc)
			bp->bp_proc = 0;
}

