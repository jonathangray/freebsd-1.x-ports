/* proc.h - public header file for proc.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)proc.h	1.14 17/9/92 (UKC) */

#define PROC_H_INCLUDED

/*  Type for a target opcode.  Used in proc_tswap().
 *  Would like to use unsigned short here, but we can't use types
 *  with promotions when mixing prototypes with old style function
 *  declarations.
 */
typedef unsigned opcode_t;

/*  Which way to continue a process for proc_cont().
 */
typedef enum cont_typeen {
	PR_STEP,	/* single step target */
	PR_CONT		/* continue target until signal or exit */
} cont_type_t;

/*  What state signal handling is in for a given signal.
 */
typedef enum sigstateen {
	SGH_DEFAULT,	/* SIG_DFL - default */
	SGH_IGNORED,	/* SIG_IGN - signal is ignored */
	SGH_CAUGHT	/* signal is caught */
} sigstate_t;

/*  Why the process stopped after proc_cont() or proc_start().
 */
typedef enum stopresen {
	SR_SIG,		/* got a signal */
	SR_BPT,		/* hit a breakpoint */
	SR_DIED,	/* process exited */
	SR_SSTEP,	/* stopped after a single step */
	SR_USER,	/* user requested stops */
	SR_FAILED	/* couldn't restart target (breakpoint problems) */
} stopres_t;

enum which_regset {
	SUN_GEN_REGS,
	SUN_FP_REGS,
	SUN_FPA_REGS
};

typedef union {
	float f;
	double d;
} fpval_t;

/*  Register type for proc_readreg().  Positive values are general
 *  purpose registers.
 *
 *  Not all of these are valid for all architectures.  For example,
 *  the MIPS chip has no condition codes, and the VAX is the only
 *  machine with an argument pointer (REG_AP) distinct from the
 *  frame pointer.
 */
#define REG_PC			(-1)	/* program counter */
#define REG_SP			(-2)	/* stack pointer */
#define REG_AP			(-3)	/* argument pointer (VAX only) */
#define REG_FP			(-4)	/* frame pointer */
#define REG_CONDITION_CODES	(-5)	/* condition codes */
#define REG_FP_CONDITION_CODES	(-6)	/* floating point condition codes */

#define REG_MIN_REGNO		(-6)

/*  Types of the routines.
 */
proc_t proc_start PROTO((const char *path, const char **argv, const char **envp,
			 long rdlist, stopres_t *p_whystopped));
stopres_t proc_execto PROTO((proc_t proc, const char *path, taddr_t addr));
void proc_set_base_sp PROTO((proc_t proc, taddr_t sp));
taddr_t proc_get_base_sp PROTO((proc_t proc));
proc_t proc_attach PROTO((const char *path, int pid, stopres_t *p_whystopped));
void proc_terminate PROTO((proc_t proc));
void proc_free PROTO((proc_t proc));
void proc_detach PROTO((proc_t proc, int sig));
stopres_t proc_cont PROTO((proc_t proc, int sig, cont_type_t ctype));
int proc_is_attached PROTO((proc_t proc));
int proc_get_lastsig PROTO((proc_t proc));
stopres_t proc_get_stopres PROTO((proc_t proc));
const char *proc_get_textfile PROTO((proc_t proc));
taddr_t proc_getreg PROTO((proc_t proc, int reg));
int proc_read_fpval PROTO((proc_t proc, taddr_t addr, bool want_double,
								fpval_t *p_val));
int proc_read_fpreg PROTO((proc_t proc, int reg, bool want_double, fpval_t *p_val));
int proc_readreg PROTO((proc_t proc, int reg, taddr_t *p_regval));
int proc_setreg PROTO((proc_t proc, int reg, taddr_t regval));

#ifdef ARCH_MIPS
taddr_t proc_get_retaddr_after_jsr PROTO((proc_t proc, int rlink_reg));
#else
taddr_t proc_get_retaddr_after_jsr PROTO((proc_t proc));
#endif

taddr_t proc_get_retaddr_after_sig PROTO((proc_t proc));
int proc_tswap PROTO((proc_t proc, taddr_t addr, opcode_t opcode,
						 opcode_t *p_old_opcode));
sigstate_t proc_get_sigstate PROTO((proc_t proc, int sig));
int proc_read_uarea PROTO((proc_t proc, taddr_t addr, char *buf, int nbytes));
int proc_read_data PROTO((proc_t proc, taddr_t addr, char *buf, int nbytes));
int proc_write_data PROTO((proc_t proc, taddr_t addr, const char *buf, int nbytes));
int proc_read_text PROTO((proc_t proc, taddr_t addr, char *buf, int nbytes));

int proc_write_corefile PROTO((proc_t proc, const char *name));

#ifdef CI_H_INCLUDED
int proc_call_func PROTO((proc_t proc, code_id_t code_id,
			  taddr_t addr, taddr_t *args, int *argsizes, int nargs,
			  taddr_t *p_res, const char **p_mesg));
#endif
