/* proc.c - process manipulation and examination */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_proc_c_sccsid[] = "@(#)proc.c	1.34 17/9/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/wait.h>
#include <sys/stat.h>
#ifdef OS_ULTRIX
#include <dirent.h>	/* needed for user.h */
#endif

#ifdef __STDC__
#include <unistd.h>
#endif

#if defined(OS_ULTRIX) || defined(ARCH_MIPS)
/*  The Ultrix system header files <setjmp.h> and <machine/pcb.h> clash
 *  over the #defines for some symbols, so we #undef them.  Hmmm ...
 */
#undef JB_S0
#undef JB_S1
#undef JB_S2
#undef JB_S3
#undef JB_S4
#undef JB_S5
#undef JB_S6
#undef JB_S7
#undef JB_S8
#undef JB_SP
#undef JB_PC
#undef NJBREGS
#endif

#include <sys/param.h>
#include <signal.h>
#include <sys/dir.h>		/* VAX needs this for user.h */
#include <sys/user.h>
#include <machine/reg.h>
#include <errno.h>
extern int errno;	/* some versions on errno.h don't declare this */

#ifdef ARCH_VAX
#include <machine/frame.h>
#endif
#include <local/ukcprog.h>

#ifdef ARCH_MIPS
#include "mips_frame.h"
#endif
#include "ups.h"
#include "ui.h"
#include "ci.h"
#include "proc.h"
#include "breakpoint.h"
#include "mreg.h"
#include "core.h"		/* Temporary kludge - see proc_getreg */
#include <local/arg.h>
#include "debug.h"

#if defined(OS_ULTRIX) && defined(ARCH_MIPS)
#define MIPS_ULTRIX
#endif

#ifdef ARCH_SUN4
#define NO_SINGLE_STEP
#include <machine/cpu.h>
#include "as.h"			/* For get_next_pc() */
static int sparc_u_offset PROTO((void));
int gethostid PROTO((void));
#endif

#if defined(OS_SUNOS) || defined(MIPS_ULTRIX) || defined(OS_RISCOS)
#include <sys/ptrace.h>
#endif

#ifndef OS_SUNOS

/*  Word size (and alignment) for ptrace read/write data requests.
 *
 *  Used in proc_read_data and proc_write_data.
 */
#define WORDSIZE	4

#if defined(ARCH_MIPS) && (defined(OS_ULTRIX) || defined(OS_RISCOS))
/* Request values for the ptrace system call
 */
enum ptracereq {
	PTRACE_TRACEME = 	PT_TRACE_ME,	/* by tracee to begin tracing */
	PTRACE_PEEKTEXT =	PT_READ_I,	/* read word from text segment */
	PTRACE_PEEKDATA =	PT_READ_D,	/* read word from data segment */
	PTRACE_PEEKUSER =	PT_READ_U,	/* read word from user struct */
	PTRACE_POKETEXT =	PT_WRITE_I,	/* write word into text segment */
	PTRACE_POKEDATA =	PT_WRITE_D,	/* write word into data segment */
	PTRACE_POKEUSER =	PT_WRITE_U,	/* write word into user struct */
	PTRACE_CONT =		PT_CONTINUE,	/* continue process */
	PTRACE_KILL =		PT_KILL,	/* terminate process */
	PTRACE_SINGLESTEP =	PT_STEP,	/* single step process */

	PTRACE_ATTACH =		20,		/* attach to running process */
	PTRACE_GETREGS =	21,		/* get general registers */
	PTRACE_GETFPREGS =	22,		/* get floating point registers */
	PTRACE_SETREGS =	24,		/* get general registers */
	PTRACE_SETFPREGS =	25,		/* get floating point registers */
	PTRACE_SETWATCHPOINT =	26,		/* set data watchpoint */
	PTRACE_DELWATCHPOINT =	27,		/* delete data watchpoint */
	PTRACE_DETACH =		28,		/* detach from process */
	PTRACE_GETFD =		29		/* get fd of attached process */
};
#else
/* Request values for the ptrace system call
 */
enum ptracereq {
	PTRACE_TRACEME = 	0,	/* by tracee to begin tracing */
	PTRACE_CHILDDONE =	0,	/* tracee is done with his half */
	PTRACE_PEEKTEXT =	1,	/* read word from text segment */
	PTRACE_PEEKDATA =	2,	/* read word from data segment */
	PTRACE_PEEKUSER =	3,	/* read word from user struct */
	PTRACE_POKETEXT =	4,	/* write word into text segment */
	PTRACE_POKEDATA =	5,	/* write word into data segment */
	PTRACE_POKEUSER =	6,	/* write word into user struct */
	PTRACE_CONT =		7,	/* continue process */
	PTRACE_KILL =		8,	/* terminate process */
	PTRACE_SINGLESTEP =	9	/* single step process */
};
#endif /* !(OS_ULTRIX && OS_MIPS) */
#endif /* !OS_SUNOS */

#ifndef WSTOPSIG
#define WSTOPSIG(w)	((w).w_stopsig)
#endif

typedef enum ptracereq ptracereq_t;

#ifdef PTRACE_DEBUG
static int ptrace_debug PROTO((int request, int pid, int addr, int data));
#define ptrace ptrace_debug
#endif

#ifdef OS_SUNOS
int ptrace PROTO((ptracereq_t req, int pid, char *addr, int data, char *addr2));
#define std_ptrace(req, pid, addr, data)	ptrace(req, pid, addr, data, (char *)NULL)
#else
int ptrace PROTO((ptracereq_t req, int pid, char *addr, int data));
#define std_ptrace(req, pid, addr, data)	ptrace(req, pid, addr, data)
#endif

/*  Return the offset into the u area of member.
 *  Used when doing PEEKUSER ptrace requests.
 */
#ifdef ARCH_MIPS
#define U_OFFSET(member)	((char *)((int)&((struct user *)NULL)->member / 4))
#else
#define U_OFFSET(member)	((char *)&((struct user *)NULL)->member)
#endif

#ifdef OS_SUNOS_4
#define u_tsize		u_procp->p_tsize
#define u_dsize		u_procp->p_dsize
#define u_ssize		u_procp->p_ssize
#endif /* OS_SUNOS_4 */

/*  Macros for getting/setting registers in a Sun regs structure.
 */
#ifdef ARCH_SUN3
#define IS_FLOAT_REG(regno)	((regno) >= 16)
#define FRAME_REG(sr)		((sr)->sr_regs.r_areg[6])
#define INTEGER_REG(sr, regno)	((sr)->sr_regs.r_dreg[regno])
#endif

#ifdef ARCH_SUN386
#define FRAME_REG(sr)		((sr)->sr_regs.r_reg[EBP])
#define INTEGER_REG(sr, regno)	((sr)->sr_regs.r_reg[regno])
#endif

#ifdef ARCH_SUN4
#define IS_FLOAT_REG(regno)	((regno) >= 32 && (regno) < 64)
#define FLOAT_REG_OFFSET	32
#define FRAME_REG(sr)		((sr)->sr_rwindow.rw_fp)
#define INTEGER_REG(sr, regno)	((&(sr)->sr_regs.r_y)[regno])
#endif

#if defined(OS_SUNOS) || defined(OS_BSDI)
typedef int wait_arg_t;
#define WAIT_STATUS_IS_INT
#else
typedef union wait wait_arg_t;
#endif

typedef struct iproc {
	int ip_pid;
	char *ip_textfile;
	taddr_t ip_base_sp;
	taddr_t ip_restart_pc;
	short ip_lastsig;
	short ip_attached;
	stopres_t ip_whystopped;
#ifdef ARCH_CLIPPER
	taddr_t ip_fpreg_sp;
	ureg_t ip_fp_uregs[16];
#endif
#ifdef OS_SUNOS
	sunregs_t ip_sunregs;
#endif
#ifdef UAREA_REGS	/* Currently VAX and MIPS under Ultrix */
	ureg_t ip_uregs[N_UREGS];
#endif
} iproc_t;

static int e_ptrace PROTO((ptracereq_t req, int pid, char *addr, int data));
static int update_regs PROTO((iproc_t *ip));
static void stop_target PROTO((void));
static void wait_for_target PROTO((iproc_t *ip));
static iproc_t *make_iproc PROTO((const char *textfile, int pid, int attached));

#ifdef UAREA_REGS
static int get_uarea_reg PROTO((iproc_t *ip, int ri, taddr_t *p_value));
static int set_uarea_reg PROTO((iproc_t *ip, int ri, taddr_t value));
#endif

#ifdef OS_SUNOS
static int set_sun_regval PROTO((sunregs_t *sr, int pid,
						int regno, taddr_t val));
#endif

#ifdef ARCH_CLIPPER
int get_clipper_fp_reg PROTO((iproc_t *ip, int regno, taddr_t *p_val));
#endif

#ifdef ARCH_SUN3
static void convert_68881_reg PROTO((unsigned *rwords, bool is_double,
							fpval_t *p_val));
#endif

#ifndef OS_SUNOS
static int get_words PROTO((int pid, ptracereq_t ptrace_req,
			    taddr_t addr, char *buf, int nbytes));
#endif

#ifdef NO_SINGLE_STEP
static void single_step_target PROTO((iproc_t *ip, int sig));
#endif

static int wait_with_intr PROTO((wait_arg_t *p_status));
static char *get_restart_pc PROTO((iproc_t *ip));

#ifdef ARCH_BSDI386
static int get_uarea_word_with_ptrace PROTO((int pid, int offset));
#endif

/*  Breakpoint opcodes for the various machines.
 */
#ifdef ARCH_SUN3
#define BPT				0x4e4f		/* trap #15 */
#define BPT_PC_OFFSET			2
#define PUT_OPCODE_IN_WORD(w, data)	(((data) << 16) | ((w) & 0xffff))
#define GET_OPCODE_FROM_WORD(w)		(((w) >> 16) & 0xffff)
#endif /* ARCH_SUN3 */

/*  This is just to make ups compile - the values are wrong.
 */
#ifdef ARCH_BSDI386
#define BPT				0xcc		/* int $3 */
#define BPT_PC_OFFSET			1
#define PUT_OPCODE_IN_WORD(w, data)	(((w) & ~0xff) | (data & 0xff))
#define GET_OPCODE_FROM_WORD(w)		((w) & 0xff)
#endif

#ifdef ARCH_SUN386
#define BPT				0xcc		/* int $3 */
#define BPT_PC_OFFSET			1
#endif

#ifdef ARCH_SUN4
#define BPT				0x91d02001	/* What gdb uses */
#define BPT_PC_OFFSET			0		/* WRONG */
#define PUT_OPCODE_IN_WORD(w, data)	(data)
#define GET_OPCODE_FROM_WORD(w)		(w)
#endif

#ifdef ARCH_VAX
#define BPT				0x03		/* bpt */
#define BPT_PC_OFFSET			0
#endif

#ifdef ARCH_CLIPPER
#define PUT_OPCODE_IN_WORD(w, data)	(((data) << 16) | ((w) & 0xffff))
#define GET_OPCODE_FROM_WORD(w)		(((w) >> 16) & 0xffff)
#define BPT				0x0900		/* what dbx uses */
#define BPT_PC_OFFSET			0
#endif

#ifdef ARCH_MIPS
#define BPT				0x0d		/* break 0 */
#define BPT_PC_OFFSET			0
#define PUT_OPCODE_IN_WORD(w, data)	(data)
#define GET_OPCODE_FROM_WORD(w)		(w)
#endif

#if defined(ARCH_VAX) || defined(ARCH_SUN386)
#define PUT_OPCODE_IN_WORD(w, data)	(((w) & ~0xff) | (data & 0xff))
#define GET_OPCODE_FROM_WORD(w)		((w) & 0xff)
#endif

/*  Special value for ptrace restart requests meaning restart the target
 *  from where it stopped.
 */
#define RESTART_FROM_WHERE_STOPPED	((taddr_t)1)

/*  Hook for debugging - set by make_proc()
 */
static iproc_t *Ip_for_ups;

#ifdef PTRACE_DEBUG

#undef ptrace

static int
ptrace_debug(request, pid, addr, data)
int request, pid, addr, data;
{
	static char *reqnames[] = {
		"TRACEME", "PEEKTEXT", "PEEKDATA", "PEEKUSER", "POKETEXT",
		"POKEDATA", "POKEUSER", "CONT", "KILL", "SINGLESTEP"
	};
	char buf[50], ebeforebuf[20], eafterbuf[20], *reqname, *ebefore, *eafter;
	int errno_before, errno_after, res;
	static bool first_call = TRUE, no_debug;

	if ((Debug_flags & DBFLAG_PTRACE) == 0)
		return ptrace(request, pid, addr, data);

	if (request < 0 || request > 9) {
		sprintf(buf, "<req %d>", request);
		reqname = buf;
	}
	else
		reqname = reqnames[request];

	errno_before = errno;
	res = ptrace(request, pid, addr, data);
	errno_after = errno;

	if (errno_before != 0) {
		sprintf(ebeforebuf, "[errno=%d]", errno_before);
		ebefore = ebeforebuf;
	}
	else
		ebefore = "";

	if (errno_after != 0) {
		sprintf(eafterbuf, "[errno=%d]", errno_after);
		eafter = eafterbuf;
	}
	else
		eafter = "";

	fprintf(stderr, "\tproc %d%s: ptrace(%s, %d, 0x%x, 0x%x)",
							getpid(), ebefore,
							reqname, pid, addr, data);
	if (res != 0 || errno_after != 0)
		fprintf(stderr, " -> 0x%x%s", res, eafter);
	fputc('\n', stderr);

	fflush(stderr);
	
	errno = errno_after;
	return res;
}

#define ptrace ptrace_debug

#endif /* PTRACE_DEBUG */

/*  Call ptrace(2), but abort if errno gets set.
 */
static int
e_ptrace(req, pid, addr, data)
ptracereq_t req;
int pid;
char *addr;
int data;
{
	int res;

	errno = 0;
	res = std_ptrace(req, pid, addr, data);
	if (errno != 0)
		panic("ptrace failed in e_ptrace");
	return res;
}


#ifdef UAREA_REGS

#ifndef ARCH_BSDI386
/*  Set the offsets into the u area for the registers.
 *  Called once on startup.
 *  Not static because we also need this information set up for core
 *  files - thus we are called from core.c
 */
void
set_uarea_reg_offsets(ur)
ureg_t *ur;
{
#ifdef ARCH_VAX
#define RUADDR(rno)	(ctob(UPAGES) + (rno * 4))
	static taddr_t rmap[] = {
		RUADDR(R0),  RUADDR(R1),  RUADDR(R2),  RUADDR(R3),
		RUADDR(R4),  RUADDR(R5),  RUADDR(R6),  RUADDR(R7),
		RUADDR(R8),  RUADDR(R9),  RUADDR(R10), RUADDR(R11),
		RUADDR(AP),  RUADDR(FP),  RUADDR(SP),  RUADDR(PC),
		RUADDR(PS),
	};
#undef RUADDR
	taddr_t *rptr;

	rptr = rmap;
	while (rptr < rmap + N_UREGS)
		ur++->ur_uaddr = *rptr++;
#endif /* ARCH_VAX */
#ifdef ARCH_MIPS
	int i;

	for (i = 0; i < NGP_REGS + NFP_REGS; ++i)
		ur[i].ur_uaddr = i;

	if (i != UR_FP)
		panic("regno botch in suro");

	/*  This ureg is the frame pointer `register'.  We evaluate this
	 *  on the demand, so there is no u area address.  For safety,
	 *  make sure that any attempt to use it causes an error.
	 */
	ur[UR_FP].ur_uaddr = 20000;

	ur[UR_PC].ur_uaddr = PC;
#endif /* ARCH_MIPS */
#ifdef ARCH_CLIPPER
	int i;

	for (i = 0; i < 16; ++i)
		ur[i].ur_uaddr = (int)U_OFFSET(u_pcb.pcb_regs[i]);
	ur[UR_PC].ur_uaddr = (int)U_OFFSET(u_pcb.pcb_cxt.cxt_pc);
#endif
}
#endif /* !ARCH_BSDI386 */

/*  Set *p_val to the current value of u area register ri.
 *  ri is a general purpose register number, or one of UR_PC, UR_AP, UR_FP
 *  and UR_SP.  See mreg.h
 */
static int
get_uarea_reg(ip, ri, p_val)
iproc_t *ip;
int ri;
taddr_t *p_val;
{
	ureg_t *ur;

	ur = ip->ip_uregs + ri;
	if (!ur->ur_is_current) {
		errno = 0;
#ifdef ARCH_MIPS
		if (ri == UR_FP) {
			taddr_t sp, pc;

			if (get_uarea_reg(ip, UR_SP, &sp) != 0)
				return -1;
			if (get_uarea_reg(ip, UR_PC, &pc) != 0)
				return -1;
			ur->ur_value = sp + get_frame_size(pc);
		}
		else {
			ur->ur_value = std_ptrace(PTRACE_PEEKUSER, ip->ip_pid,
							(char *)ur->ur_uaddr, 0);
		}
#else
		ur->ur_value = std_ptrace(PTRACE_PEEKUSER, ip->ip_pid,
							(char *)ur->ur_uaddr, 0);
#endif
		if (errno != 0)
			return -1;
		ur->ur_is_current = TRUE;
	}
	*p_val = ur->ur_value;
	return 0;
}

static int
set_uarea_reg(ip, ri, value)
iproc_t *ip;
int ri;
taddr_t value;
{
	ureg_t *ur;

	ur = ip->ip_uregs + ri;
	errno = 0;
	e_ptrace(PTRACE_POKEUSER, ip->ip_pid, (char *)ur->ur_uaddr, (int)value);
	ur->ur_is_current = FALSE;
	return (errno == 0) ? 0 : -1;
}

/*  Convert machine independent register number regno to u area register number.
 */
int
reg_to_uarea_index(regno)
int regno;
{
	switch(regno) {
	case REG_PC:
		return UR_PC;
	case REG_SP:
		return UR_SP;
	case REG_AP:
#ifdef ARCH_VAX
		return UR_AP;
	case REG_CONDITION_CODES:
		return UR_PSL;
#endif
	case REG_FP:
		return UR_FP;
	default:
		if (regno < 0 || regno >= N_UAREA_GREGS)
			panic("bad regno in proc_getreg");
		return regno;
	}
	/* NOTREACHED */
}
#endif /* UAREA_REGS */

#ifdef ARCH_CLIPPER
int
get_clipper_fp_reg(ip, regno, p_val)
iproc_t *ip;
int regno;
taddr_t *p_val;
{
	ureg_t *ur;

	if (ip->ip_fpreg_sp == 0) {
		static unsigned short text[] = {
			0xb420,		/* saved0 */
			0x0000,		/* second word of saved0 */
			0x0900,		/* BPT */
			0x0000,		/* nop */
		};
		taddr_t pc, saved_restart_pc, saved_sp, sp, new_sp, word1, word2;
		int errno1, errno2, i;

		saved_sp = sp = proc_getreg((proc_t)ip, REG_SP);

		/*  Back off to a word aligned place at least four halfwords
		 *  before the current pc.
		 */
		saved_restart_pc = ip->ip_restart_pc;
		pc = (ip->ip_restart_pc - 8) & ~3;

		word1 = std_ptrace(PTRACE_PEEKTEXT, ip->ip_pid, (char *)pc, 0);
		errno1 = errno;
		word2 = std_ptrace(PTRACE_PEEKTEXT, ip->ip_pid, (char *)(pc + 4), 0);
		errno2 = errno;

		std_ptrace(PTRACE_POKETEXT, ip->ip_pid, (char *)pc, *(int *)text);

		/*  At this point we can still back out.  Beyond this point
		 *  we have trodden on the text, so any errors are fatal.
		 */
		if (errno != 0 || errno1 != 0 || errno2 != 0) {
			errf("Can't insert fp register grabbing code (%m)");
			return -1;
		}

		e_ptrace(PTRACE_POKETEXT, ip->ip_pid,
						(char *)(pc + 4), *(int *)&text[2]);

		/*  Ensure that the stack pointer is eight byte aligned
		 */
		if (sp & 7) {
			sp &= ~7;
			if (proc_setreg((proc_t)ip, REG_SP, sp) != 0)
				panic("can't set sp in gifr");
		}
		
		e_ptrace(PTRACE_CONT, ip->ip_pid, (char *)pc, 0);
		wait_for_target(ip);
		new_sp = proc_getreg((proc_t)ip, REG_SP);

		/*  Make sure we've hit the breakpoint, and that the sp
		 *  has been moved by the saved0.
		 */
		if (ip->ip_whystopped != SR_BPT && ip->ip_whystopped != SR_SSTEP)
			panic("ws botch in gcfr");

		e_ptrace(PTRACE_POKETEXT, ip->ip_pid, (char *)pc, word1);
		e_ptrace(PTRACE_POKETEXT, ip->ip_pid, (char *)(pc + 4), word2);

		/*  Sometimes the pc doesn't advance.  I don't know why.
		 */
		if (ip->ip_restart_pc == pc) {
			errf("Error running fp regno grabbing code");
			ip->ip_restart_pc = saved_restart_pc;
			return -1;
		}

		if (ip->ip_restart_pc != pc + 4)
			panic("pc botch in gcfr");
		if (new_sp != sp - 4 * 16)
			panic("sp botch in gcfr");

		ip->ip_restart_pc = saved_restart_pc;

		ip->ip_fpreg_sp = sp - 8 * 8;	/* 8 eight byte doubles */
		proc_setreg((proc_t)ip, REG_SP, saved_sp);

		for (i = 0; i < 16; ++i)
			ip->ip_fp_uregs[i].ur_is_current = FALSE;
	}

	ur = &ip->ip_fp_uregs[regno];
	if (!ur->ur_is_current) {
		if (proc_read_data((proc_t)ip, ip->ip_fpreg_sp + regno * 4,
						    (char *)&ur->ur_value, 4) != 0) {
			errf("Can't read fp register value from the stack (%m)");
			return -1;
		}
		ur->ur_is_current = TRUE;
	}

	*p_val = ur->ur_value;
	return 0;
}
#endif /* ARCH_CLIPPER */

#ifdef OS_SUNOS
/*  Return the current value of Sun register regno.
 *  regno is a machine independent register number.
 */
taddr_t
get_sun_regval(sr, pid, reg)
sunregs_t *sr;
int pid, reg;
{
	switch(reg) {
	case REG_PC:
		return sr->sr_regs.r_pc;
	case REG_SP:
		return sr->sr_regs.r_sp;
	case REG_FP:
	case REG_AP:
		return FRAME_REG(sr);
#ifdef ARCH_SUN4
	case REG_CONDITION_CODES:
		return sr->sr_regs.r_psr;
#endif
	default:
#ifdef ARCH_SUN4
		if (reg == REG_FP_CONDITION_CODES || (reg >= 32 && reg < 64)) {
			if (sr->sr_need_fpu) {
				e_ptrace(PTRACE_GETFPREGS, pid,
							    (char *)&sr->sr_fpu, 0);
				sr->sr_need_fpu = FALSE;
			}
			if (reg == REG_FP_CONDITION_CODES)
				return sr->sr_fpu.fpu_fsr;
			return sr->sr_fpu.fpu_regs[reg - FLOAT_REG_OFFSET];
		}
#endif
		if (reg < 0 || reg >= N_SUN_GREGS)
			panic("bad reg in gsr");
#ifdef ARCH_SUN4
		if (reg == 0)
			return 0;
#endif
		return INTEGER_REG(sr, reg);
	}
}

static int
set_sun_regval(sr, pid, regno, val)
sunregs_t *sr;
int pid, regno;
taddr_t val;
{
	switch(regno) {
	case REG_PC:
		sr->sr_regs.r_pc = val;
		break;
	case REG_FP:
	case REG_AP:
		FRAME_REG(sr) = val;
		break;
	case REG_SP:
		sr->sr_regs.r_sp = val;
		break;
	default:
#ifdef ARCH_SUN4
		if (IS_FLOAT_REG(regno)) {
			if (sr->sr_need_fpu) {
				e_ptrace(PTRACE_GETFPREGS, pid,
						       (char *)&sr->sr_fpu, 0);
				sr->sr_need_fpu = FALSE;
			}
			sr->sr_fpu.fpu_regs[regno - FLOAT_REG_OFFSET] = val;

			if (std_ptrace(PTRACE_SETFPREGS, pid,
						(char *)&sr->sr_fpu, 0) != 0) {
				sr->sr_need_fpu = TRUE;
				return -1;
			}
			return 0;
		}
#endif
		if (regno < 0 || regno >= N_SUN_GREGS)
			panic("bad regno in ssr");
		INTEGER_REG(sr, regno) = val;
		break;
	}

	errno = 0;
	e_ptrace(PTRACE_SETREGS, pid, (char *)sr, 0);
	if (errno != 0)
		return -1;

#ifdef ARCH_SUN4
	if (ptrace(PTRACE_WRITEDATA, pid, (char *)sr->sr_regs.r_sp,
		          sizeof(sr->sr_rwindow), (char *)&sr->sr_rwindow) != 0)
		return -1;
#endif

	return 0;
}

#endif /* OS_SUNOS */

#ifdef ARCH_BSDI386
static int
get_uarea_word_with_ptrace(pid, offset)
int pid, offset;
{
	return e_ptrace(PTRACE_PEEKUSER, pid, (char *)offset, 0);
}
	
void
set_uarea_reg_offsets(ur, get_uarea_word, arg)
ureg_t *ur;
get_uarea_word_func_t get_uarea_word;
int arg;
{
	static int trapregs[N_UREGS] = {
		tEAX, tECX, tEDX, tEBX, tESP, tEBP, tESI, tEDI, tEIP
	};
	static int syscallregs[N_UREGS] = {
		sEAX, sECX, sEDX, sEBX, sESP, sEBP, sESI, sEDI, sEIP
	};
	taddr_t addr;
	int flags, offset, i, *regmap;

	flags = (*get_uarea_word)(arg, (int)U_OFFSET(u_pcb.pcb_flags));
	regmap = ((flags & FM_TRAP) != 0) ? trapregs : syscallregs;

	addr = (*get_uarea_word)(arg, (int)U_OFFSET(u_kproc.kp_proc.p_regs));
	offset = addr - USRSTACK;

	for (i = 0; i < N_UREGS; ++i)
		ur[i].ur_uaddr = offset + 4 * regmap[i];
}
#endif /* ARCH_BSDI386 */

/*  Update the stored machine register values.  This is called after the
 *  target has been run and has thus changed the register values.
 *  For the VAX, we just mark our cached values as invalid.
 *
 *  The pc is a special case, as when we hit a breakpoint some machines
 *  report the pc as pointing after the trap opcode.  Thus if we are at
 *  a breakpoint we adjust the pc value.  The VAX kernel seems to do
 *  this for us.
 *
 *  Return TRUE if the target stopped because it hit a breakpoint.
 */
static int
update_regs(ip)
iproc_t *ip;
{
	taddr_t pc;
	breakpoint_t bp;
#ifdef OS_SUNOS
	sunregs_t *sr;
#endif

#ifdef UAREA_REGS
	ureg_t *ur;

	for (ur = ip->ip_uregs + N_UREGS; ur >= ip->ip_uregs; --ur)
		ur->ur_is_current = FALSE;

#ifdef ARCH_BSDI386
	set_uarea_reg_offsets(ip->ip_uregs,
				      get_uarea_word_with_ptrace, ip->ip_pid);
#endif

#ifdef ARCH_CLIPPER
	ip->ip_fpreg_sp = 0;
#endif

	if (get_uarea_reg(ip, UR_PC, &pc) != 0)
		panic("can't get pc in ur");
	bp = get_breakpoint_at_addr((proc_t)ip, pc - BPT_PC_OFFSET);
	if (bp != NULL) {
		pc -= BPT_PC_OFFSET;
		ip->ip_uregs[UR_PC].ur_value = pc;
	}
#endif /* UAREA_REGS */

#ifdef OS_SUNOS
	sr = &ip->ip_sunregs;
	e_ptrace(PTRACE_GETREGS, ip->ip_pid, (char *)&sr->sr_regs, 0);
#ifdef ARCH_SUN4
	if (ptrace(PTRACE_READDATA, ip->ip_pid, (char *)sr->sr_regs.r_sp,
		        sizeof(sr->sr_rwindow), (char *)&sr->sr_rwindow) != 0) {
		panic("rwindow read botch in ur");
	}
#endif
	pc = (taddr_t)sr->sr_regs.r_pc;
	bp = get_breakpoint_at_addr((proc_t)ip, pc - BPT_PC_OFFSET);
	if (bp != NULL) {
		pc -= BPT_PC_OFFSET;
		sr->sr_regs.r_pc = pc;
	}
#if defined(ARCH_SUN3) || defined(ARCH_SUN4)
	sr->sr_need_fpu = TRUE;
#endif
#endif /* OS_SUNOS */
	ip->ip_restart_pc = pc;
	return bp != NULL;
}
	
jmp_buf Longjmp_env;

/*  A pointer to this is passed to wn_set_abort_func() by wait_for_target().
 */
static void
stop_target()
{
	longjmp(Longjmp_env, 1);
}

static int
wait_with_intr(p_status)
wait_arg_t *p_status;
{
	if (setjmp(Longjmp_env) == 1)
		return 0;

	return wait(p_status);
}

/*  Wait for process ip to stop.  If the process didn't die, update the
 *  stored register values.   Set ip_stopres to the reason why the
 *  process stopped, and ip_restart_pc to the address the process
 *  should be restarted from.
 */
static void
wait_for_target(ip)
iproc_t *ip;
{
	static int want_errors = -1;
	stopres_t whystopped;
	int pid, at_bpt;
	bool user_stopped_target;
	wait_arg_t status;

	if (want_errors == -1)
		want_errors = getenv("WANT_ERRORS") != NULL;

	user_stopped_target = FALSE;
	for (;;) {
		abort_func_t oldfunc;

		if ((Debug_flags & DBFLAGS_NO_STOP) != 0)
			pid = wait(&status);
		else {
			oldfunc = set_user_abort_func(stop_target);
			pid = wait_with_intr(&status);
			(void) set_user_abort_func(oldfunc);
		}

		if (pid == 0) {
			if (user_wants_stop()) {
				kill(ip->ip_pid, SIGTRAP);
				user_stopped_target = TRUE;
			}
			continue;
		}
		if (pid == ip->ip_pid)
			break;
		else if (pid == -1) {
			if (errno != EINTR)
				errf("Wait returned -1 (%m)");
		}
		else {
			if (want_errors)
				errf("Wait returned bad pid %d (expected %d)",
								pid, ip->ip_pid);
		}
	}

	if (WIFSTOPPED(status)) {
		ip->ip_lastsig = 0;
		at_bpt = update_regs(ip);
		if (WSTOPSIG(status) != SIGTRAP) {
			whystopped = SR_SIG;
			ip->ip_lastsig = WSTOPSIG(status);
		}
		else if (user_stopped_target)
			whystopped = SR_USER;
		else if (at_bpt)
			whystopped = SR_BPT;
		else
			whystopped = SR_SSTEP;
	}
	else {
		ip->ip_lastsig = -1;
		ip->ip_restart_pc = 0;
		whystopped = SR_DIED;
		mark_breakpoints_as_uninstalled((proc_t)ip);
	}
	ip->ip_whystopped = whystopped;
}

static char *
get_restart_pc(ip)
iproc_t *ip;
{
#ifdef ARCH_CLIPPER
	if (ip->ip_fpreg_sp != 0)
		return (char *)ip->ip_restart_pc;
#endif
	if (ip->ip_whystopped != SR_BPT)
		return (char *)1;

	return (char *)ip->ip_restart_pc;
}

/*  Make a new iproc structure for a running process pid.
 *  textfile is the name of the binary file.
 *  attached is TRUE if we attached to this process with a PTRACE_ATTACH.
 */
static iproc_t *
make_iproc(textfile, pid, attached)
const char *textfile;
int pid, attached;
{
	iproc_t *ip;

	ip = (iproc_t *) e_malloc(sizeof(iproc_t));
	ip->ip_pid = pid;
	ip->ip_textfile = strsave(textfile);
	ip->ip_attached = attached;
#if defined(UAREA_REGS) && !defined(ARCH_BSDI386)
	set_uarea_reg_offsets(ip->ip_uregs);
#endif
#ifdef ARCH_CLIPPER
	ip->ip_fpreg_sp = 0;
#endif
#ifdef ARCH_SUN3
	ip->ip_sunregs.sr_fptype = FPT_68881;	/* BUG: just for testing */
#endif
#ifdef OS_SUNOS
	/*  Purify doesn't seem to know that PTRACE_GETREGS sets
	 *  all of a regs structure.
	 */
	memset((char *)&ip->ip_sunregs, '\0', sizeof(ip->ip_sunregs));
#endif
	Ip_for_ups = ip;	/* for debugging only */
	return ip;
}

proc_t
proc_start(path, argv, envp, rdlist, p_whystopped)
const char *path, **argv, **envp;
long rdlist;
stopres_t *p_whystopped;
{
	iproc_t *ip;
	int pid;

	fflush(stdout);
	fflush(stderr);

	if ((pid = vfork()) == 0) {
		arg_do_redirs_in_child(rdlist);
		if (std_ptrace(PTRACE_TRACEME, 0, (char *)NULL, 0) != 0)
			panic("ptrace TRACEME request failed in child");
		execve(path, (char **)argv, (char **)envp);
		perror(path);
		_exit(1);
	}

	arg_tidy_redirs_in_parent(rdlist);
	if (pid == -1) {
		errf("Can't fork to run %s (%m)", path);
		*p_whystopped = SR_DIED;
		return 0;
	}

	ip = make_iproc(path, pid, FALSE);

	wait_for_target(ip);

	if (ip->ip_whystopped == SR_DIED) {
		errf("Can't start %s", path);
		proc_free((proc_t)ip);
		*p_whystopped = SR_DIED;
		return 0;
	}

	*p_whystopped = ip->ip_whystopped;
	return (proc_t)ip;
}

stopres_t
proc_execto(proc, path, addr)
proc_t proc;
const char *path;
taddr_t addr;
{
	breakpoint_t bp;
	iproc_t *ip;

	ip = (iproc_t *)proc;

	bp = add_breakpoint(addr);

	if (install_breakpoint(bp, proc) != 0) {
		errf("Can't install initial breakpoint in %s (%m)", path);

		if (remove_breakpoint(bp) != 0)
			panic("can't back out uninstalled bpt");

		if (!ip->ip_attached)
			proc_terminate(proc);

		return SR_DIED;
	}

	ip->ip_lastsig = 0;
	do {
		proc_cont((proc_t)ip, ip->ip_lastsig, PR_CONT);
	} while (ip->ip_whystopped == SR_SIG);

	if (remove_breakpoint(bp) != 0)
		panic("can't uninstall initial breakpoint");
	
	return ip->ip_whystopped;
}

void
proc_set_base_sp(proc, sp)
proc_t proc;
taddr_t sp;
{
	((iproc_t *)proc)->ip_base_sp = sp;
}

taddr_t
proc_get_base_sp(proc)
proc_t proc;
{
	return ((iproc_t *)proc)->ip_base_sp;
}

#ifdef OS_SUNOS
proc_t
proc_attach(path, pid, p_whystopped)
const char *path;
int pid;
stopres_t *p_whystopped;
{
	iproc_t *ip;

	if (std_ptrace(PTRACE_ATTACH, pid, (char *)NULL, 0) != 0)
		return 0;
	ip = make_iproc(path, pid, TRUE);
	wait_for_target(ip);
	*p_whystopped = ip->ip_whystopped;
	return (proc_t)ip;
}
#endif /* OS_SUNOS */

/*  Kill off the target process
 */
void
proc_terminate(proc)
proc_t proc;
{
	iproc_t *ip;
	wait_arg_t status;

	ip = (iproc_t *) proc;
	std_ptrace(PTRACE_KILL, ip->ip_pid, (char *)NULL, 0);
	if (ip->ip_attached) {
		/*  wait() hangs on a PTRACE_ATTACH process which
		 *  has been killed, so fake the status.
		 */
#ifdef WAIT_STATUS_IS_INT
		status = SIGKILL;
#else
		status.w_T.w_Termsig = SIGKILL;
		status.w_T.w_Retcode = 0;
#endif
	}
	else
		wait_for_target(ip);
}

void
proc_free(proc)
proc_t proc;
{
	/*  BUG: we test for !=0 because we are sometimes called
	 *       more that once.  We need to sort this stuff out
	 *       properly.
	 */
	if (proc != 0) {
		free(((iproc_t *)proc)->ip_textfile);
		free((char *)(iproc_t *)proc);
	}
}

/*  Detach from a process which we earlier attached to.
 *  Leave the process running.
 */
void
proc_detach(proc, sig)
proc_t proc;
int sig;
{
	iproc_t *ip;

	ip = (iproc_t *) proc;
	if (!ip->ip_attached)
		panic("proc_detach called but not attached to proc");
	if (uninstall_all_breakpoints(proc) != 0)
		panic("can't uninstall breakpoints in proc");
#ifdef OS_SUNOS
	(void) e_ptrace(PTRACE_DETACH, ip->ip_pid, get_restart_pc(ip), sig);
#endif /* OS_SUNOS */
}

#ifdef NO_SINGLE_STEP
static void
single_step_target(ip, sig)
iproc_t *ip;
int sig;
{
	proc_t proc;
	taddr_t npc;
	breakpoint_t bp;
	bool must_remove_bp, must_uninstall_bp;

	proc = (proc_t)ip;

	/*  Ask an as_*.c routine where it thinks the pc is going.
	 */
	npc = get_next_pc(proc, ip->ip_restart_pc);

	bp = addr_to_breakpoint(npc);
	must_remove_bp = bp == NULL;
	if (bp == NULL)
		bp = add_breakpoint(npc);

	must_uninstall_bp = !breakpoint_is_installed(bp);
	if (must_uninstall_bp && install_breakpoint(bp, proc) != 0)
		panic("can't install bp in sst");

	e_ptrace(PTRACE_CONT, ip->ip_pid, get_restart_pc(ip), sig);
	wait_for_target(ip);

	if (ip->ip_whystopped != SR_DIED) {
		if (must_uninstall_bp && uninstall_breakpoint(bp) != 0)
			panic("can't uninstall bp in sst");
		if (must_remove_bp && remove_breakpoint(bp) != 0)
			panic("can't remove bp in sst");
		if (ip->ip_restart_pc == npc && ip->ip_whystopped == SR_BPT)
			ip->ip_whystopped = SR_SSTEP;
	}
}
#endif

/*  Restart process proc.  Either continue or single step it, depending on ctype.
 *  *p_user_stopped_target is set if the user asks to stop the target.
 *
 *  If the process was stopped at a breakpoint, we remove the breakpoint,
 *  single step the process and replace the breakpoint.  If ctype was PR_CONT
 *  and the single step stopped normally we than continue the process with
 *  a PR_CONT.
 *
 *  The intended effect is that breakpoints that are at the current pc value
 *  are ignored.
 *
 *  We return the reason why the process stopped - one of SR_SIG, SR_DIED,
 *  SR_SSTEP, SR_BPT and SR_FAILED.  We map SIGTRAP into SR_BPT - higher level
 *  software is not aware SIGTRAP is special.
 */
stopres_t
proc_cont(proc, sig, ctype)
proc_t proc;
int sig;
cont_type_t ctype;
{
	iproc_t *ip;
	ptracereq_t req;
	breakpoint_t bp;

	ip = (iproc_t *) proc;

	/*  Step over bpt if there is one at ip->ip_restart_pc.
	 */
	if ((bp = get_breakpoint_at_addr(proc, ip->ip_restart_pc)) != NULL) {
		if (uninstall_breakpoint(bp) != 0)
			return SR_FAILED;
#ifdef NO_SINGLE_STEP
		single_step_target(ip, sig);
#else
		(void) e_ptrace(PTRACE_SINGLESTEP, ip->ip_pid,
						get_restart_pc(ip), sig);
		wait_for_target(ip);
#endif
		if (install_breakpoint(bp, proc) != 0)
			panic("can't install breakpoint in proc_cont");
		if (ip->ip_whystopped != SR_SSTEP || ctype == PR_STEP)
			return ip->ip_whystopped;
		if (ctype != PR_CONT)
			panic("bad ctype in proc_cont");
		req = PTRACE_CONT;
		sig = 0;
	}
	else {
		switch(ctype) {
		case PR_CONT:
			req = PTRACE_CONT;
			break;
		case PR_STEP:
			req = PTRACE_SINGLESTEP;
			break;
		default:
			panic("bad ctype in proc_cont");
			req = (ptracereq_t)0; /* to satisfy gcc */
		}
	}
#ifdef NO_SINGLE_STEP
	if (req == PTRACE_SINGLESTEP)
		single_step_target(ip, sig);
	else {
		e_ptrace(req, ip->ip_pid, get_restart_pc(ip), sig);
		wait_for_target(ip);
	}
#else
	e_ptrace(req, ip->ip_pid, get_restart_pc(ip), sig);
	wait_for_target(ip);
#endif
	return ip->ip_whystopped;
}

/*  Return TRUE if we attached to process proc with a PTRACE_ATTACH.
 */
int
proc_is_attached(proc)
proc_t proc;
{
	return ((iproc_t *)proc)->ip_attached;
}

/*  Return the signal that last stopped the target.
 */
int
proc_get_lastsig(proc)
proc_t proc;
{
	return (proc != NULL) ? ((iproc_t *)proc)->ip_lastsig : core_get_lastsig();
}

/*  Return the reason why the process stopped.  This is the reason for the
 *  most recent stop.
 */
stopres_t
proc_get_stopres(proc)
proc_t proc;
{
	return ((iproc_t *)proc)->ip_whystopped;
}

/*  Return the filename of the binary of process proc.
 */
const char *
proc_get_textfile(proc)
proc_t proc;
{
	return ((iproc_t *)proc)->ip_textfile;
}

#ifdef ARCH_SUN3
static void
convert_68881_reg(rwords, is_double, p_val)
unsigned *rwords;
bool is_double;
fpval_t *p_val;
{
	int exponent;
	unsigned *w;

	exponent = ((rwords[0] >> 16) & 0x7fff) - ((1 << 14) - 1);

	w = (unsigned *)&p_val->d;

	/*  Transfer exponent and sign bit.
	 */
	w[0] = (exponent + ((1 << 10) - 1)) << 20;
	w[0] |= rwords[0] & 0x80000000;

	/*  Copy mantissa into first 32 bit word
	 */
	w[0] |= (rwords[1] & 0x7fffffff) >> 11;

	/*  Copy rest of mantissa (that will fit) into second 32 bit word.
	 */
	w[1] = ((rwords[1] & 0x7ff) << (32 - 11)) | (rwords[2] >> 11); 

	if (!is_double)
		p_val->f = p_val->d;
}
#endif
		
int
proc_read_fpval(proc, addr, is_double, p_val)
proc_t proc;
taddr_t addr;
bool is_double;
fpval_t *p_val;
{
#ifdef ARCH_SUN3
	unsigned rwords[3];

	if (proc_read_data(proc, addr, (char *)rwords, sizeof(rwords)) != 0)
		return -1;
	convert_68881_reg(rwords, is_double, p_val);
	return 0;
#else
	if (is_double)
		return proc_read_data(proc, addr, (char *)&p_val->d, sizeof(double));
	else
		return proc_read_data(proc, addr, (char *)&p_val->f, sizeof(float));
#endif
}

int
proc_read_fpreg(proc, regno, is_double, p_val)
proc_t proc;
int regno;
bool is_double;
fpval_t *p_val;
{
#ifdef ARCH_SUN3
	iproc_t *ip;
	sunregs_t *sr;

	/*  The f68881 has eight registers, which are numbered 18..25 in
	 *  the symbol table.
	 */
	regno -= 18;
	if (regno < 0 || regno >= 8)
		panic("bad regno in rf");
	
	ip = (iproc_t *)proc;
	sr = &ip->ip_sunregs;
	if (sr->sr_need_fpu) {
		e_ptrace(PTRACE_GETFPREGS, ip->ip_pid, (char *)&sr->sr_fpu, 0);
		sr->sr_need_fpu = FALSE;
	}

	convert_68881_reg((unsigned *)sr->sr_fpu.f_fpstatus.fps_regs[regno].fp,
								is_double, p_val);
	return 0;
#else
	if (is_double) {
		int regno2;
		taddr_t *buf = (taddr_t *)&p_val->d;

		regno2 = regno + 1;
#ifdef ARCH_BSDI386
		if (regno == 3)
			regno2 = 6;
#endif
		if (proc_readreg(proc, regno, buf) != 0 ||
		    proc_readreg(proc, regno2, buf + 1) != 0)
			return -1;
	}
	else {
		if (proc_readreg(proc, regno, (taddr_t *)&p_val->f) != 0)
			return -1;
	}
	return 0;
#endif
}

/*  Get the value currently stored in register regno of process proc.
 *  Return 0 and set *p_val for success, -1 for failure.
 *
 *  regno is either a general purpose register, or one of REG_PC, REG_SP,
 *  REG_FP and REG_AP.  See mreg.h.
 */
int
proc_readreg(proc, regno, p_val)
proc_t proc;
int regno;
taddr_t *p_val;
{
	iproc_t *ip;

	/*  Temporary kludge until we can to this properly - if
	 *  proc is null go for the core file.
	 */
	if (proc == NULL) {
#ifdef ARCH_CLIPPER
		if (regno >= 16 && regno < 32) {
			errf("Sorry, can't get fp registers from core files on the Clipper");
			return -1;
		}
#endif
		return core_getreg(regno, p_val);
	}

	ip = (iproc_t *)proc;
#ifdef OS_SUNOS
	*p_val = get_sun_regval(&ip->ip_sunregs, ip->ip_pid, regno);
	return 0;
#else
#ifdef UAREA_REGS
#ifdef ARCH_CLIPPER
	if (regno >= 16 && regno < 32)
		return get_clipper_fp_reg((iproc_t *)proc, regno - 16, p_val);
#endif
	return get_uarea_reg((iproc_t *)proc, reg_to_uarea_index(regno), p_val);
#else
	panic("don't know how to get a register value for this machine");
	return -1;	/* to satisfy gcc */
#endif /* !UAREA_REGS */
#endif /* !ARCH_SUN3 */
}

/*  Interface to proc_readreg() that aborts on failure.
 */
taddr_t
proc_getreg(proc, regno)
proc_t proc;
int regno;
{
	taddr_t val;

	if (proc_readreg(proc, regno, &val) != 0)
		panic("proc_readreg failed");
	return val;
}

int
proc_setreg(proc, regno, value)
proc_t proc;
int regno;
taddr_t value;
{
	iproc_t *ip;

	ip = (iproc_t *)proc;
#ifdef OS_SUNOS
	return set_sun_regval(&ip->ip_sunregs, ip->ip_pid, regno, value);
#else
#ifdef UAREA_REGS
	return set_uarea_reg((iproc_t *)proc, reg_to_uarea_index(regno), value);
#else
	panic("don't know how to get a register value for this machine");
	return 0;	/* to satisfy gcc */
#endif /* !UAREA_REGS */
#endif /* !ARCH_SUN3 */
}

/*  Some architectures pass some function arguments in registers.
 */
#ifdef ARCH_MIPS
#define N_REG_ARGS	4
#define RETURN_REGNO	2
#endif /* ARCH_MIPS */

#ifdef ARCH_CLIPPER
#define N_REG_ARGS	2
#endif

#ifdef ARCH_SUN4
#define N_REG_ARGS	6
#define RETURN_REGNO	8	/* %o0 */
#define ALIGN_STACK(n)	((n) & ~07)
#endif

#ifndef N_REG_ARGS
#define N_REG_ARGS	0
#endif

#ifndef RETURN_REGNO
#define RETURN_REGNO	0
#endif

#ifndef ALIGN_STACK
#define ALIGN_STACK(n)	((n) & ~03)
#endif

/*  Call a function in the target.
 *
 *  There must be no breakpoints installed when this routine is called
 */
int
proc_call_func(proc, code_id, addr, args, argsizes, nargs, p_res, p_mesg)
proc_t proc;
code_id_t code_id;
taddr_t addr;
taddr_t *args;
int *argsizes;
int nargs;
taddr_t *p_res;
const char **p_mesg;
{
	taddr_t saved_restart_pc;
	taddr_t sp, retpc;
	iproc_t *ip;
	int i;
#ifdef ARCH_VAX
	struct frame frame;
	taddr_t fp, ap, saved_sp, saved_r0, saved_r1;
	int psw;
	unsigned short regmask;
#endif
#ifdef OS_SUNOS
	sunregs_t sunregs;
#endif
#ifdef ARCH_SUN4
	int *oregs;
#endif
#ifdef ARCH_MIPS
	taddr_t regtab[NGP_REGS + NFP_REGS]; 	/* Not all used */
	static int saveregs = -1;
#endif /* ARCH_MIPS */
#ifdef ARCH_CLIPPER
	taddr_t saved_sp, saved_regs[N_REG_ARGS];
#endif
#ifdef ARCH_BSDI386
	taddr_t saved_regs[N_UREGS];
#endif
	breakpoint_t breakpoint;
	taddr_t realargs[40];
	
	if (nargs > sizeof realargs / sizeof *realargs)
		panic("nargs too large in proc_call_func");

	*p_mesg = NULL;

	ip = (iproc_t *)proc;

	/*  We can only call a target function if we are currently stopped
	 *  at a breakpoint.  This is because on the SPARC you can't always
	 *  feed a pc value back to PTRACE_CONT - if for example the target
	 *  is stopped at a delay slot then the only thing that works is
	 *  the special pc value 1 meaning `restart from where you stopped'.
	 *  Calling a target function would change the `where you last stopped'
	 *  remembered state and we would be unable to subseqently continue
	 *  the target.
	 *
	 *  The condition should never be true, as target functions can
	 *  only be called from breakpoint code and we don't allow target
	 *  function calls in display area expressions.  Better safe than
	 *  sorry though.
	 */
	if (ip->ip_whystopped != SR_BPT) {
		*p_mesg = "Can't call target function - target not stopped at breakpoint";
		return -1;
	}

#ifdef ARCH_MIPS
	/*  Save registers that the Assembly Language Guide
	 *  says are not preserved across function calls.
	 *  This is because we can call a target function
	 *  at an arbitrary point, and the surrounding code
	 *  will not be preprared for registers to change.
	 *
	 *  We save all registers expect r0 (wired to zero),
	 *  and r16..r23 (s0..s7).
	 *
	 *  Similarly, we save the non-preserved floating point
	 *  registers (f0..f19).
	 */
	if (saveregs == -1)
		saveregs = getenv("DONTSAVEREGS") == NULL;

	if (saveregs) {
		for (i = 1; i < 16; ++i)
			regtab[i] = proc_getreg(proc, i);
		for (i = 23; i < 32 + 20; ++i)
			regtab[i] = proc_getreg(proc, i);
		/*  Make the sp four byte aligned.  It probably already is.
		 */
		sp = regtab[29] & ~3;
	}
	else {
		sp = proc_getreg(proc, REG_SP) & ~3;
	}
#endif /* ARCH_MIPS */

#ifdef ARCH_VAX
	if (proc_read_text(proc, addr, (char *)&regmask, sizeof(regmask)) != 0) {
		*p_mesg = "Can't read register mask from text";
		return -1;
	}
	addr += sizeof(regmask);

	fp = proc_getreg(proc, REG_FP);
	psw = proc_getreg(proc, REG_CONDITION_CODES) & 0xffff;

	/*  Get the values of the registers that won't be preserved by the
	 *  calls instruction.
	 */
	saved_r0 = proc_getreg(proc, 0);
	saved_r1 = proc_getreg(proc, 1);
	saved_sp = proc_getreg(proc, REG_SP);

	/*  Do the calls type stack pointer four byte alignment.
	 */
	sp = saved_sp & ~03;
#endif
#ifdef OS_SUNOS
	sunregs = ip->ip_sunregs;
	sp = sunregs.sr_regs.r_sp;
#endif
#ifdef ARCH_CLIPPER
	for (i = 0; i < N_REG_ARGS; ++i)
		saved_regs[i] = proc_getreg(proc, i);
	sp = saved_sp = proc_getreg(proc, REG_SP);
#endif
#ifdef ARCH_BSDI386
	for (i = 0; i < N_UREGS; ++i)
		if (get_uarea_reg(ip, i, &saved_regs[i]) != 0)
			panic("can't get reg in pcf");
	sp = proc_getreg(proc, REG_SP);
#endif

	retpc = saved_restart_pc = ip->ip_restart_pc;

	/*  Make copies of the targets of pointers and arrays.
	 *
	 *  These are indicated by a non zero size.
	 */
	for (i = 0; i < nargs; ++i) {
		size_t size;

		size = argsizes[i];
		if (size > 0 && ci_is_ci_addr(code_id, args[i])) {
			sp = ALIGN_STACK(sp - size);
			if (proc_write_data(proc, sp, (char *)args[i], size) != 0) {
				*p_mesg = "Can't write data to the stack";
				return -1;
			}
			realargs[i] = sp;
		}
		else
			realargs[i] = args[i];
	}

	/*  Push the arguments.  We want the last argument pushed first,
	 *  so work backwards through the array of arguments.
	 *
	 *  On some machines only some arguments go on the stack - the
	 *  rest go in registers.  We delay setting the registers until
	 *  we have successfully set the return breakpoint (see below).
	 *  
	 */
#ifdef ARCH_SUN4
	/*  Try to end up with the stack pointer eight byte aligned
	 *  (after the 0x5c subtraction below.
	 */
	if (nargs < N_REG_ARGS || nargs % 2 == 0)
		sp -= 4;
#endif
	for (i = nargs - 1; i >= N_REG_ARGS; --i) {
		sp -= 4;
		if (proc_write_data(proc, sp, (char *)&realargs[i], 4) != 0) {
			*p_mesg = "Can't write argument to the stack";
			return -1;
		}
	}
#ifdef ARCH_SUN4
	/*  The SPARC compilers expect to find the seventh parameter
	 *  (the first one not passed in a register) at 0x5c(fp).
	 */
	sp -= 0x5c;
#endif

#ifdef ARCH_VAX
	/*  Push the arg count.
	 */
	sp -= 4;
	if (proc_write_data(proc, sp, (char *)&nargs, sizeof(nargs)) != 0) {
		*p_mesg = "Can't write the arg count to the stack";
		return -1;
	}

	ap = sp;

	/*  Save the registers mentioned in the function's save mask.
	 */
	for (i = 11; i >= 0; --i) {
		if (regmask & (1 << i)) {
			taddr_t regval;

			regval = proc_getreg(proc, i);
			sp -= 4;
			if (proc_write_data(proc, sp, (char *)&regval, 4) != 0) {
				*p_mesg = "Can't push register value";
				return -1;
			}
		}
	}

	/*  Construct a calls stack frame.
	 *
	 *  We don't use the bitfields defined in <sys/frame.h> because they
	 *  just have a :1 for a field which is specified as "must be zero"
	 *  by the VAX architecture manual.
	 */
	frame.fr_handler = 0;
	(&frame.fr_handler)[1] = (1 << 29) | (regmask << 16) | psw;
	frame.fr_savap = proc_getreg(proc, REG_AP);
	frame.fr_savfp = proc_getreg(proc, REG_FP);
	frame.fr_savpc = retpc;

	/*  Push frame onto the stack.
	 */
	sp -= sizeof(frame);
	if (proc_write_data(proc, sp, (char *)&frame, sizeof(frame)) != 0) {
		*p_mesg = "Can't push stack frame";
		return -1;
	}
#endif /* ARCH_VAX */
#if defined(ARCH_SUN3) || defined(ARCH_SUN386) || defined(ARCH_CLIPPER) || defined(ARCH_BSDI386)
	sp -= 4;
	if (proc_write_data(proc, sp, (char *)&retpc, sizeof(retpc)) != 0) {
		*p_mesg = "Can't push return address";
		return -1;
	}
#endif

	/*  Put a breakpoint where we've arranged for the function to
	 *  return to, unless there is already a breakpoint installed there.
	 */
	if (get_breakpoint_at_addr(proc, retpc) != NULL)
		breakpoint = NULL;
	else {
		breakpoint = add_breakpoint(retpc);
		if (install_breakpoint(breakpoint, proc) != 0) {
			*p_mesg = "Can't insert breakpoint at return address";
			return -1;
		}
	}

	/*  Up to this point, we haven't been committed, as all we've been
	 *  doing is pushing things beyond the end of the stack.
	 *
	 *  Now we're reasonably confident that the call will succeed, so
	 *  sync the target registers with our idea of their values.
	 */
#ifdef ARCH_VAX
	proc_setreg(proc, REG_AP, ap);
	proc_setreg(proc, REG_FP, fp);
	proc_setreg(proc, REG_SP, sp);
#endif /* ARCH_VAX */
#if defined(ARCH_SUN3) || defined(ARCH_SUN386) || defined(ARCH_BSDI386)
	proc_setreg(proc, REG_SP, sp);
#endif
#ifdef ARCH_MIPS
	/*  The first four integer arguments go in registers, but there
	 *  is space allocated for them on the stack.
	 *
	 *  BUG: we assume that all the arguments are integer (not
	 *       floating point).
	 */
	sp -= N_REG_ARGS * 4;
	if (proc_setreg(proc, MIPS_SP_REGNO, sp) != 0) {
		*p_mesg = "Can't set stack pointer for target function call";
		return -1;
	}
	
	for (i = 0; i < nargs && i < N_REG_ARGS; ++i) {
		if (proc_setreg(proc, 4 + i, realargs[i]) != 0)
			panic("regno write failed in cf");
	}

	/*  Arrange for the function to return to where we've set
	 *  the breakpoint.
	 */
	if (proc_setreg(proc, MIPS_SAVEDPC_REGNO, retpc) != 0)
		panic("regno write failed in cf");
#endif
#ifdef ARCH_CLIPPER
	if (proc_setreg(proc, REG_SP, sp) != 0) {
		*p_mesg = "Can't set stack pointer for target function call";
		return -1;
	}
	for (i = 0; i < nargs && i < N_REG_ARGS; ++i) {
		if (proc_setreg(proc, i, realargs[i]) != 0)
			panic("regno write failed in cf");
	}
#endif
#ifdef ARCH_SUN4
	oregs = &ip->ip_sunregs.sr_regs.r_o0;
	for (i = 0; i < nargs && i < N_REG_ARGS; ++i)
		oregs[i] = realargs[i];
	oregs[6] = sp;
	oregs[7] = retpc - 8;	/* Normal func return is jmp savpc + 8 */
	if (std_ptrace(PTRACE_SETREGS, ip->ip_pid,
					(char *)&ip->ip_sunregs.sr_regs, 0) != 0) {
		*p_mesg = "Can't set registers for target function call";
		return -1;
	}
		
#endif

	/*  Jump to the start of the function.
	 */
	(void) e_ptrace(PTRACE_CONT, ip->ip_pid, (char *)addr, 0);
	wait_for_target(ip);

	/*  If we installed a breakpoint ourself, uninstall it.
	 */
	if (ip->ip_whystopped != SR_DIED && breakpoint != NULL) {
		if (remove_breakpoint(breakpoint) != 0)
			return -1;
	}

	/*  If we didn't get back to where we expected, stop now.
	 */
	if (ip->ip_whystopped != SR_BPT || ip->ip_restart_pc != retpc)
		return -1;

	*p_res = proc_getreg(proc, RETURN_REGNO);

	/*  Copy back any stuff we copied into the target's core.
	 */
	for (i = 0; i < nargs; ++i) {
		if (realargs[i] != args[i])
			proc_read_data(proc, realargs[i], (char *)args[i],
								       argsizes[i]);
	}

	/*  Restore the registers we saved.
	 */
#ifdef ARCH_VAX
	proc_setreg(proc, REG_PC, saved_restart_pc);
	proc_setreg(proc, REG_SP, saved_sp);
	proc_setreg(proc, 0, saved_r0);
	proc_setreg(proc, 1, saved_r1);
#endif
#ifdef OS_SUNOS
	(void) e_ptrace(PTRACE_SETREGS, ip->ip_pid, (char *)&sunregs, 0);
	ip->ip_sunregs = sunregs;
#endif
#ifdef ARCH_MIPS
	if (saveregs) {
		for (i = 1; i < 16; ++i) {
			if (proc_setreg(proc, i, regtab[i]) != 0)
				panic("can't restore registers in cf");
		}
		for (i = 24; i < 32 + 20; ++i) {
			if (proc_setreg(proc, i, regtab[i]) != 0)
				panic("can't restore registers in cf");
		}
	}
#endif
#ifdef ARCH_CLIPPER
	for (i = 0; i < N_REG_ARGS; ++i) {
		if (proc_setreg(proc, i, saved_regs[i]) != 0)
			panic("can't restore regs in cf");
	}
	if (proc_setreg(proc, REG_SP, saved_sp) != 0)
		panic("can't restore sp in cf");
#endif
#ifdef ARCH_BSDI386
	for (i = 0; i < N_UREGS; ++i)
		if (set_uarea_reg(ip, i, saved_regs[i]) != 0)
			panic("can't restore regs in cf");
#endif

	/*  Wait for target will have set ip_restart_pc to retpc, because
	 *  that is where the function returned.  We put this back.
	 */
	ip->ip_restart_pc = saved_restart_pc;

	return 0;
}

/*  Return the saved pc after a signal.  Only works immediately after a
 *  proc_cont(..., PR_STEP) which has returned SR_SIG for a signal which
 *  is caught.
 *
 *  Used to put a breakpoint after the return from the signal handler.
 */
taddr_t
proc_get_retaddr_after_sig(proc)
proc_t proc;
{
#if defined(ARCH_SUN3) || defined(ARCH_SUN386) || defined(ARCH_VAX)
	return proc_getreg(proc, REG_PC);
#endif
#if defined(ARCH_MIPS) || defined(ARCH_SUN4) || defined(ARCH_CLIPPER)
	panic("gras NYI on MIPS");
	return 0;	/* to satisfy gcc */
#endif
}

/*  Return the saved pc after a jsr.  Must be called immediatly after a jsr.
 *  Used to put a breakpoint at the return address.
 */
#ifdef ARCH_MIPS
taddr_t
proc_get_retaddr_after_jsr(proc, rlink_reg)
proc_t proc;
int rlink_reg;
{
	return proc_getreg(proc, rlink_reg);
}
#else
taddr_t
proc_get_retaddr_after_jsr(proc)
proc_t proc;
{
#if defined(ARCH_SUN3) || defined(ARCH_SUN386) || defined(ARCH_CLIPPER) || defined(ARCH_BSDI386)
	taddr_t sp, retaddr;

	sp = proc_getreg(proc, REG_SP);
	if (proc_read_data(proc, sp, (char *)&retaddr, sizeof(retaddr)) == -1)
		panic("bad sp in pgraj");
	return retaddr;
#else
#ifdef ARCH_VAX
	taddr_t sp;
	struct frame frbuf;

	sp = proc_getreg(proc, REG_SP);
	if (proc_read_data(proc, sp, (char *)&frbuf, sizeof(frbuf)) == -1)
		panic("bad sp in pgraj");
	return frbuf.fr_savpc;
#else
	panic("graj NYI");
	return 0;
#endif	/* !ARCH_VAX */
#endif  /* !ARCH_{SUN{3,386},CLIPPER} */
}
#endif /* !ARCH_MIPS */

/*  Insert opcode into the text area at addr.  If p_old_opcode is
 *  non NULL we ignore the supplied opcode and insert the appropriate
 *  breakpoint opcode for the machine.  Otherwise insert opcode
 *  and set *p_old_opcode to the old opcode.
 *
 *  We treat it as a fatal error if we are installing a breakpoint
 *  and one is already there, or if we are installing other than
 *  a breakpoint and there isn't one already there.
 */
int
proc_tswap(proc, addr, opcode, p_old_opcode)
proc_t proc;
taddr_t addr;
opcode_t opcode, *p_old_opcode;
{
#if defined(ARCH_SUN386) || defined(ARCH_CLIPPER)
	int byte_shift;
	unsigned mask;
#endif
	int pid, temp;
	opcode_t old_opcode;

	pid = ((iproc_t *)proc)->ip_pid;

	if (p_old_opcode != NULL)
		opcode = BPT;

#ifdef ARCH_CLIPPER
	if ((addr & 1) != 0)
		panic("unaligned addr passes to proc_tswap");
	byte_shift = (addr & 03) * 8;
	mask = 0xffff << byte_shift;
	addr &= ~03;
#endif
#ifdef ARCH_SUN386
	byte_shift = (addr & 03) * 8;
	mask = 0xff << byte_shift;
	addr &= ~03;
#endif
#ifdef ARCH_MIPS
	if ((addr & 3) != 0)
		panic("unaligned addr passed to proc_tswap");
#endif

	errno = 0;
	temp = std_ptrace(PTRACE_PEEKTEXT, pid, (char *)addr, 0);

#if defined(ARCH_SUN386) || defined(ARCH_CLIPPER)
	old_opcode = ((unsigned)temp & mask) >> byte_shift;
	temp = ((unsigned)temp & ~mask) | (opcode << byte_shift);
#else
	old_opcode = GET_OPCODE_FROM_WORD(temp);
	temp = PUT_OPCODE_IN_WORD(temp, opcode);
#endif

	(void) std_ptrace(PTRACE_POKETEXT, pid, (char *)addr, temp);
	{
		int check;

		check = std_ptrace(PTRACE_PEEKTEXT, pid, (char *)addr, 0);
		if (check != temp)
			panic("ptrace botch");
	}

	if (errno != 0)
		return -1;
	
	if ((opcode == BPT) == (old_opcode == BPT)) {
		if (opcode == BPT)
			panic("duplicate breakpoint in tswap");
		else
			panic("vanished breakpoint in tswap");
	}

	if (p_old_opcode != NULL)
		*p_old_opcode = old_opcode;
	return 0;
}

int
proc_write_corefile(proc, name)
proc_t proc;
const char *name;
{
#ifdef OS_SUNOS
	if (std_ptrace(PTRACE_DUMPCORE, ((iproc_t *)proc)->ip_pid,
							(char *)name, 0) == 0)
		return 0;

	errf("Can't dump core file to %s (%m)", name);
#else
	errf("Sorry, this machine/OS does not support core snapshots");
#endif
	return -1;
}

#ifdef ARCH_SUN4
/*  The Sun 4m CPU has an extra 1024 byte field at the start of the
 *  u area.  This means we have to add or subtract this amount if we
 *  are running on a different CPU architecture than the one we were
 *  compiled on.
 */
static int
sparc_u_offset()
{
	static int offset;
	static bool need_offset = TRUE;

	if (need_offset) {
		int cputype;

		cputype = (gethostid() >> 24) & CPU_ARCH;

#ifdef SUN4M_ARCH
		offset = (cputype == SUN4M_ARCH) ? 0 : -1024;
#else
		offset = (cputype == 0x70) ? 1024 : 0;
#endif
		
		if ((Debug_flags & DBFLAG_MISC) != 0)
			printf("sparc_u_offset = %d\n", offset);
		
		need_offset = FALSE;
	}

	return offset;
}
#endif

/*  Return the current state of signal handling for signal sig in process
 *  proc.  Returns SGH_CAUGHT, SGH_IGNORED or SGH_DEFAULT.
 *
 *  You might think we could return the address of the signal handler for
 *  caught signals, but on the Suns at least the returned address is that
 *  of a library routine which handles all caught signals.
 */
sigstate_t
proc_get_sigstate(proc, sig)
proc_t proc;
int sig;
{
	caddr_t uaddr;
	taddr_t handler;
	sigstate_t res;
	iproc_t *ip;

	if (sig < 1 || sig >= NSIG)
		panic("sig botch in gss");
	
	ip = (iproc_t *)proc;

#ifdef ARCH_MIPS
	uaddr = (char *)(SIG_BASE + sig);
#else
#ifdef ARCH_BSDI386
	uaddr = U_OFFSET(u_sigacts.ps_sigact[sig]);
#else
#ifdef ARCH_SUN4
	uaddr = U_OFFSET(u_signal[sig]) + sparc_u_offset();
#else
	uaddr = U_OFFSET(u_signal[sig]);
#endif
#endif
#endif

	errno = 0;
	handler = e_ptrace(PTRACE_PEEKUSER, ip->ip_pid, (char *)uaddr, 0);
	
	if (handler == (taddr_t)SIG_IGN)
		res = SGH_IGNORED;
	else if (handler == (taddr_t)SIG_DFL)
		res = SGH_DEFAULT;
	else
		res = SGH_CAUGHT;

	return res;
}

#ifndef OS_SUNOS
static int
get_words(pid, ptrace_req, addr, buf, nbytes)
int pid;
ptracereq_t ptrace_req;
taddr_t addr;
char *buf;
int nbytes;
{
	char *optr;
	int word, trailing_nbytes;
	taddr_t lim;

	optr = buf;

	/*  Round down the address to a four byte alignment
	 */
	if ((addr & (WORDSIZE - 1)) != 0) {
		taddr_t aligned;
		int offset, count;

		offset = addr & (WORDSIZE - 1);
		aligned = addr - offset;
		errno = 0;
		word = std_ptrace(ptrace_req, pid, (char *)aligned, 0);
		if (errno != 0)
			return -1;

		count = WORDSIZE - offset;
		if (count > nbytes)
			count = nbytes;
		memcpy(buf, (char *)&word + offset, count);

		optr += count;
		addr += count;
		nbytes -= count;
	}

	/*  At this point addr is on a 32 bit word boundary.
	 */

	trailing_nbytes = nbytes & 03;
	nbytes -= trailing_nbytes;

	lim = addr + nbytes;
	errno = 0;

	/*  Copy the whole words into the buffer.  We still have to use
	 *  memcpy because optr might not be four byte aligned (in our
	 *  address space) and some machines (e.g. MIPS) would object.
	 */
	while (addr < lim) {
		word = std_ptrace(ptrace_req, pid, (char *)addr, 0);
		memcpy(optr, (char *)&word, WORDSIZE);
		addr += WORDSIZE;
		optr += WORDSIZE;
	}

	if (trailing_nbytes != 0) {
		word = std_ptrace(ptrace_req, pid, (char *)addr, 0);
		memcpy(optr, (char *)&word, trailing_nbytes);
	}

	return errno != 0 ? -1 : 0;
}
#endif

/*  This is just for the $debug command "dumpu".  This is the only function
 *  that exports the concept of the u area outside this source file.
 *
 *  As this is just for debugging, we don't try to cope with non aligned
 *  tranfers.
 */
int
proc_read_uarea(proc, addr, buf, nbytes)
proc_t proc;
taddr_t addr;
char *buf;
int nbytes;
{
#ifdef ARCH_MIPS
#define USCALE	4
#else
#define USCALE	1
#endif
	int *optr;
	iproc_t *ip;
	taddr_t lim;

	ip = (iproc_t *)proc;

	addr /= USCALE;
	lim = addr + nbytes / USCALE;

	if ((addr & 03) != 0 || (nbytes & 03) != 0)
		panic("alignment botch in pra");
	
	optr = (int *)buf;
	for (; addr < lim; addr += 4 / USCALE) {
		errno = 0;
		*optr++ = std_ptrace(PTRACE_PEEKUSER, ip->ip_pid, (char *)addr, 0);
		if (errno != 0)
			return -1;
	}
	return 0;
}

/*  Read nbytes bytes into buf starting at address addr in the text area of process
 *  proc.
 *  The byte count is returned or -1 case of error.
 */
int
proc_read_text(proc, addr, buf, nbytes)
proc_t proc;
taddr_t addr;
char *buf;
int nbytes;
{
	int pid;

	pid = ((iproc_t *)proc)->ip_pid;
#ifdef OS_SUNOS
	if (ptrace(PTRACE_READTEXT, pid, (char *)addr, nbytes, buf) == 0)
		return 0;
	else
		return -1;
#else
	return get_words(pid, PTRACE_PEEKTEXT, addr, buf, nbytes);
#endif /* !OS_SUNOS */
}

/*  Read nbytes of data into buf from process proc, starting at target
 *  address addr.
 *  Return the number of bytes read, or -1 if there was an error.
 *  We never return a short count - the return value is always nbytes or -1.
 */
int
proc_read_data(proc, addr, buf, nbytes)
proc_t proc;
taddr_t addr;
char *buf;
int nbytes;
{
	int pid;

	/*  Temporary kludge until we can do this properly - if
	 *  proc is null go for the core file.
	 */
	if (proc == 0)
		return core_dread(addr, buf, nbytes);

	pid = ((iproc_t *)proc)->ip_pid;
#ifdef PURIFY
	/*  Purify doesn't know about ptrace ...
	 */
	memset(buf, '\0', nbytes);
#endif
#ifdef OS_SUNOS
	if (ptrace(PTRACE_READDATA, pid, (char *)addr, nbytes, buf) == 0)
		return 0;
	else
		return -1;
#else
	return get_words(pid, PTRACE_PEEKDATA, addr, buf, nbytes);
#endif /* !OS_SUNOS */
}

int
proc_write_data(proc, addr, buf, nbytes)
proc_t proc;
taddr_t addr;
const char *buf;
int nbytes;
{
#ifndef OS_SUNOS
	const char *iptr;
	taddr_t lim;
	int word, trailing_nbytes;
#endif
	int pid;

	pid = ((iproc_t *)proc)->ip_pid;
#ifdef OS_SUNOS
	if (ptrace(PTRACE_WRITEDATA, pid, (char *)addr, nbytes, (char *)buf) == 0)
		return 0;
	else
		return -1;
#else
	iptr = buf;

	/*  Round down the address to a four byte alignment
	 */
	if ((addr & (WORDSIZE - 1)) != 0) {
		taddr_t aligned;
		int offset, count;

		offset = addr & (WORDSIZE - 1);
		aligned = addr - offset;

		errno = 0;
		word = std_ptrace(PTRACE_PEEKDATA, pid, (char *)aligned, 0);
		if (errno != 0)
			return -1;

		count = WORDSIZE - offset;
		if (count > nbytes)
			count = nbytes;
		memcpy((char *)&word + offset, buf,  count);

		errno = 0;
		std_ptrace(PTRACE_POKEDATA, pid, (char *)aligned, word);
		if (errno != 0)
			return -1;

		iptr += count;
		addr += count;
		nbytes -= count;
	}

	/*  At this point addr is on a 32 bit word boundary.
	 */

	trailing_nbytes = nbytes & 03;
	nbytes -= trailing_nbytes;

	lim = addr + nbytes;
	errno = 0;

	/*  Copy the whole words into the buffer.  We still have to use
	 *  memcpy because iptr might not be four byte aligned (in our
	 *  address space) and some machines (e.g. MIPS) would object.
	 */
	while (addr < lim) {
		memcpy((char *)&word, iptr, WORDSIZE);
		std_ptrace(PTRACE_POKEDATA, pid, (char *)addr, word);
		addr += WORDSIZE;
		iptr += WORDSIZE;
	}

	if (trailing_nbytes != 0) {
		word = std_ptrace(PTRACE_PEEKDATA, pid, (char *)addr, 0);
		memcpy((char *)&word, iptr, trailing_nbytes);
		std_ptrace(PTRACE_POKEDATA, pid, (char *)addr, word);
	}

	return errno != 0 ? -1 : 0;
#endif /* !OS_SUNOS */
}
