/* core.c - code for reading and writing core files */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_core_c_sccsid[] = "@(#)core.c	1.23 15/9/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>

#ifdef __STDC__
#include <unistd.h>
#endif

#ifdef ARCH_SUN386
#include <machine/tss.h>	/* to stop cc complaining about incomplete types */
#endif

#include <a.out.h>
#include <sys/param.h>
#include <sys/dir.h>	/* vax needs this for user.h */
#include <signal.h>
#include <errno.h>

#ifndef OS_BSDI
#include <sys/vmparam.h>
#endif

#ifndef OS_SUNOS
#include <sys/user.h>
#endif

#ifdef ARCH_MIPS
#include <machine/reg.h>
#endif

#ifdef OS_SUNOS

#ifdef OS_SUNOS_4
#define FPU
#define c_fpstatus	c_fpu.f_fpstatus
#define c_fparegs	c_fpu.f_fparegs
#endif /* OS_SUNOS_4 */

#include <sys/core.h>
#define c_sunregs	c_regs

#ifdef ARCH_SUN386
#define TEXTADDR	0x10d0		/* Virtual address of start of text */
#define N_DATADDR(a)	(btop(TEXTADDR) + btop(a.a_text + PAGESIZE - 1))
#endif

#define DATADDR(c)	N_DATADDR(c.c_aouthdr)
#endif /* OS_SUNOS */

#include <local/ukcprog.h>
#include <mtrprog/utils.h>

#include "ups.h"
#include "mreg.h"
#include "proc.h"
#include "core.h"
#ifdef ARCH_MIPS
#include "mips_frame.h"	/* mreg.h needs this for MIPS_SP_REGNO */
#endif

#undef USERCORE
#if defined(OS_BSDI) || defined(OS_ULTRIX) || defined(ARCH_CLIPPER) || \
    defined(ARCH_VAX) || defined(OS_RISCOS) || defined(OS_NEWSOS)
#define USERCORE
#endif

#ifdef USERCORE
struct core {
	union c_headerun {
		struct user cu_u;
		char cu_dummy[ctob(UPAGES)];
	} c_un;
	int c_regarr[1];
	int c_len;
	int c_dsize;
	int c_ssize;
	int c_signo;
	char *c_cmdname;
	ureg_t *c_uregs;
};

#undef DATADDR

#ifdef ARCH_MIPS
#define DATADDR(c)	USRDATA		/* from <machine/vmparam.h> */
#endif

#ifdef ARCH_BSDI386
#define DATADDR(c)	(c).c_un.cu_u.u_kproc.kp_eproc.e_vm.vm_daddr
#endif

#ifndef DATADDR
#define DATADDR(c)	ctob((c).c_un.cu_u.u_tsize)
#endif

#define CORE_NAMELEN	MAXNAMLEN
#endif /* USERCORE */

/*  Information about a core file.
 *
 *  Machine independent apart from the regs fields.
 */
struct coredescst {
	int co_fd;
	int co_lastsig;
	off_t co_data_fpos;
	off_t co_data_lim;
	off_t co_stack_fpos;
	off_t co_stack_lim;
	taddr_t co_min_data_addr;
	taddr_t co_max_data_addr;
	taddr_t co_min_stack_addr;
	taddr_t co_max_stack_addr;
#ifdef USERCORE
	ureg_t *co_uregs;
#endif /* USERCORE */
#ifdef OS_SUNOS
	sunregs_t co_sunregs;
#endif /* OS_SUNOS */
} Coredesc = { -1 };

static int get_core_header PROTO((int cfd, struct core *cp,
				  const char *corename, int want_messages));
static const char *is_corefile PROTO((const char *corename, struct stat *st));
static int core_matches_text PROTO((const char *corename, const char *textname,
				    const char *cmdname, time_t core_mtime,
				    int user_gave_core));

#ifdef ARCH_BSDI386
static int get_uword PROTO((int arg, int offset));
#endif

#define IS_STACK_ADDR(co, addr) (addr >= (co).co_min_stack_addr && \
						addr < (co).co_max_stack_addr)

#define IS_DATA_ADDR(co, addr) (addr >= (co).co_min_data_addr && \
						addr < (co).co_max_data_addr)

#define DADDR_TO_COREPOS(co, addr) ((off_t)(addr - (co).co_min_data_addr) \
							+ (co).co_data_fpos)

#define SADDR_TO_COREPOS(co, addr) ((co).co_stack_lim - ((off_t)USRSTACK - (off_t)addr))

int
core_getreg(regno, p_val)
int regno;
taddr_t *p_val;
{
#ifdef OS_SUNOS
	*p_val = get_sun_regval(&Coredesc.co_sunregs, 0, regno);
	return 0;
#else /* !OS_SUNOS */
#ifdef USERCORE
	off_t fpos;
#ifdef ARCH_MIPS
	int mreg;

	switch (regno) {
	case REG_PC:
		mreg = EF_EPC;
		break;
	case REG_SP:
		mreg = EF_SP;
		break;
	case REG_FP:
	case REG_AP:
		mreg = EF_SP;	/* for now */
		break;
	default:
		if (regno < 0 || regno >= 32)
			panic("bad reg in core_getreg");
		mreg = regno + 3;	/* look at <machine/reg.h> */
		break;
	}
	fpos = (off_t)&CORE_REG(mreg, 0);
#else /* ARCH_MIPS */
	fpos = Coredesc.co_uregs[reg_to_uarea_index(regno)].ur_uaddr;
#endif /* !ARCH_MIPS */
	if (lseek(Coredesc.co_fd, fpos, L_SET) == -1)
		return -1;
	return (read(Coredesc.co_fd, (char *)p_val, 4) == 4) ? 0 : -1;
#else
	panic("core_getreg NYI");
	return 0;	/* to satisfy gcc */
#endif /* !USERCORE*/
#endif /* !OS_SUNOS */
}

#ifdef USERCORE
#define CORE_HEADER_SIZE	ctob(UPAGES)
#else
#define CORE_HEADER_SIZE	sizeof(struct core)
#endif

static int
get_core_header(cfd, cp, corename, want_messages)
int cfd;
struct core *cp;
const char *corename;
int want_messages;
{
#ifdef OS_SUNOS
	if (read(cfd, (char *)cp, CORE_HEADER_SIZE) != CORE_HEADER_SIZE) {
		if (want_messages)
			errf("Can't read core file %s (%m)", corename);
		return -1;
	}
	if (cp->c_magic != CORE_MAGIC) {
		if (want_messages)
			errf("%s is not a core file", corename);
		return -1;
	}
		
#endif /* OS_SUNOS */
#ifdef USERCORE
	if (read(cfd, (char *)&cp->c_un, CORE_HEADER_SIZE) != CORE_HEADER_SIZE) {
		if (want_messages)
			errf("Can't read core file %s (%m)", corename);
		return -1;
	}
	cp->c_dsize = ctob(cp->c_un.cu_u.u_dsize);
	cp->c_ssize = ctob(cp->c_un.cu_u.u_ssize);
	cp->c_len = sizeof(cp->c_un);
	cp->c_uregs = (ureg_t *) e_malloc(sizeof(ureg_t) * N_UREGS);
	cp->c_signo = 0;
#ifdef OS_BSDI
	cp->c_cmdname = cp->c_un.cu_u.u_kproc.kp_proc.p_comm;
	set_uarea_reg_offsets(cp->c_uregs, get_uword, (int)cp);
#else
	cp->c_cmdname = cp->c_un.cu_u.u_comm;
	set_uarea_reg_offsets(cp->c_uregs);
#endif
#endif /* USERCORE */
	return 0;
}

#ifdef ARCH_BSDI386
static int
get_uword(arg, offset)
int arg, offset;
{
	struct core *cp;

	cp = (struct core *)arg;

	if (offset < 0 || offset >= sizeof(cp->c_un.cu_u))
		panic("get_uword offset botch");
	
	return *(int *)&cp->c_un.cu_dummy[offset];
}
#endif

static const char *
is_corefile(corename, st)
const char *corename;
struct stat *st;
{
	if (stat(corename, st) < 0)
		return "Can't stat %s (%m)";
	if ((st->st_mode & S_IFMT) != S_IFREG)
		return "%s is not a regular file";
	if (st->st_size <= CORE_HEADER_SIZE)
		return "%s is not a core file";
	return NULL;
}

static int
core_matches_text(corename, textname, cmdname, core_mtime, user_gave_core)
const char *corename, *textname, *cmdname;
time_t core_mtime;
int user_gave_core;
{
	struct stat tstat;
	const char *basetext;	/* last component of text name */
	int basetext_len, cmdname_len;
	int len;		/* #chars to compare with core command name */

	if (stat(textname, &tstat) < 0) {
		if (user_gave_core)
			errf("Can't stat %s (%m)", textname);
		return FALSE;
	}
	if ((basetext = strrchr(textname, '/')) == NULL)
		basetext = textname;
	else
		++basetext;
	basetext_len = strlen(basetext);
	cmdname_len = strlen(cmdname);
	len = (basetext_len < cmdname_len) ? basetext_len : cmdname_len;
	if (cmdname != NULL && strncmp(basetext, cmdname, len) != 0) {
		if (user_gave_core)
			errf("Warning: %s dumped from %s not %s",
						corename, cmdname, textname);
		else
			return FALSE;
	}
	if (core_mtime < tstat.st_mtime) {
		if (user_gave_core)
			errf("Warning: core file %s is older than %s",
							corename, textname);
		else
			return FALSE;
	}
	return TRUE;
}

/*  Check that corename is a core dump and was dumped from textname.
 *  If the core file is OK, initialise the fields of Coredesc from it.
 *
 *  If user_gave_core is FALSE, we are silent about most things.
 *  Otherwise we complain about errors, but we accept (with a warning)
 *  out of date core files and core files which don't seem to have been
 *  dumped from textname.
 *
 *  The data_addr argument is the address in memory of the start of
 *  the text segment.  It is only used on systems where we can't get
 *  this information from the core file.
 *
 *  On the Sun (where we can attach to a running process, we treat a numeric
 *  corename as a process id, and check that we can send a signal to that process.
 */
int
open_corefile(corename, textname, user_gave_core, data_addr)
const char *corename, *textname;
int user_gave_core;
taddr_t data_addr;
{
	struct core c;
	const char *efmt, *warn;
	int cfd;
	struct stat cstat;
#ifdef OS_SUNOS
	sunregs_t *sr;
#endif

	if (is_number(corename)) {
#ifdef OS_SUNOS
		if (kill(atoi(corename), 0) == 0)
			return 0;
		if (errno == EPERM)
			errno = EACCES;
		errf("Can't send signal to process %s (%m)", corename);
		return -1;
#else
		errf("Can't attach to processes on machines other than the Sun");
		return -1;
#endif /* !OS_SUNOS */
	}
	if ((efmt = is_corefile(corename, &cstat)) != NULL) {
		if (user_gave_core)
			errf(efmt, corename);
		return -1;
	}

	if ((cfd = open(corename, O_RDONLY)) == -1) {
		warn = user_gave_core ? "" : "warning: ";
		errf("%scan't open core file %s (%m)", warn, corename);
		return -1;
	}

	if (get_core_header(cfd, &c, corename, user_gave_core) != 0) {
		close(cfd);
		return -1;
	}

	if (!core_matches_text(corename, textname, c.c_cmdname, cstat.st_mtime,
							user_gave_core))
		return -1;

	/*  Core is OK, so initialise Coredesc.
	 */

	/*  Set the layout fields of coredesc
	 */
	Coredesc.co_data_fpos = c.c_len;
	Coredesc.co_data_lim = c.c_len + c.c_dsize;
	Coredesc.co_stack_fpos = Coredesc.co_data_lim;
	Coredesc.co_stack_lim = Coredesc.co_stack_fpos + c.c_ssize;

	/*  Set the address range fields.
	 */
#ifdef ARCH_CLIPPER
	Coredesc.co_min_data_addr = data_addr;
#else
	Coredesc.co_min_data_addr = (taddr_t)DATADDR(c);
#endif
	Coredesc.co_max_data_addr = (taddr_t)(Coredesc.co_min_data_addr +
							     c.c_dsize - 1);
	Coredesc.co_min_stack_addr = (taddr_t)(USRSTACK - c.c_ssize);
	Coredesc.co_max_stack_addr = (taddr_t)(USRSTACK - 1);

	Coredesc.co_lastsig = c.c_signo;
	Coredesc.co_fd = cfd;

	/*  Set the registers (machine dependent)
	 */
#ifdef USERCORE
	Coredesc.co_uregs = c.c_uregs;		/* pointer assignment */
#endif /* USERCORE */
#ifdef OS_SUNOS
	sr = &Coredesc.co_sunregs;
	sr->sr_regs = c.c_sunregs;		/* structure assignment */
#ifdef ARCH_SUN4
	sr->sr_fpu = c.c_fpu;			/* ditto */
	sr->sr_need_fpu = FALSE;
	core_dread((taddr_t)sr->sr_regs.r_sp,
				(char *)&sr->sr_rwindow, sizeof(sr->sr_rwindow));
#endif /* ARCH_SUN4 */
#endif /* OS_SUNOS */

	return 0;
}

int
core_get_lastsig()
{
	return Coredesc.co_lastsig;
}

taddr_t
get_max_stack_addr()
{
	return (taddr_t)USRSTACK;
}

int
core_dread(addr, buf, nbytes)
taddr_t addr;
char *buf;
int nbytes;
{
	off_t corefile_pos;

	if (IS_STACK_ADDR(Coredesc, addr))
		corefile_pos = SADDR_TO_COREPOS(Coredesc, addr);
	else if (IS_DATA_ADDR(Coredesc, addr))
		corefile_pos = DADDR_TO_COREPOS(Coredesc, addr);
	else
		return -1;
	if (lseek(Coredesc.co_fd, corefile_pos, L_SET) == -1)
		return -1;
	return (read(Coredesc.co_fd, buf, nbytes) == nbytes) ? 0 : -1;
}
