/* data.c - higher level routines to read and write target data */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_data_c_sccsid[] = "@(#)data.c	1.8 15/9/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <sys/types.h>
#include <signal.h>
#include <setjmp.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include <local/ukcprog.h>
#include <mtrprog/utils.h>

#include "ups.h"
#include "data.h"
#include "proc.h"
#include "obj_stack.h"
#include "obj_target.h"
#include "state.h"

/*  Process we are currently debugging.  BUG: GET RID OF THIS - IT'S GROSS.
 */
extern proc_t Current_proc;

static volatile int Got_sigill;

static jmp_buf Sigill_env;

static void catch_sigill PROTO((int sig));
static int charval PROTO((int n));

static void
catch_sigill(unused_sig)
int unused_sig;
{
	Got_sigill = 1;
	longjmp(Sigill_env, 1);
}

/*  Extract a floating point value from the value_t union.
 *
 *  On the VAX (and possibly other machines) certain bit patterns cause
 *  exceptions when treated as a floating point value, so we bracket
 *  the extraction with a handler for these exceptions.
 */
const char *
get_real(vl, want_hex, is_float)
value_t vl;
bool want_hex, is_float;
{
	static char buf[60];
	double d;
	void (*old_sigill_func)PROTO((int sig));

	if (want_hex) {
		if (is_float)
			sprintf(buf, "<0x%08x>", vl.vl_int);
		else
			sprintf(buf, "<0x%08x%08x>",
					vl.vl_ints[DOUBLE_MSW],
					vl.vl_ints[DOUBLE_LSW]);
		return buf;
	}

	old_sigill_func = signal(SIGILL, catch_sigill);
	Got_sigill = FALSE;
	
	if (setjmp(Sigill_env) == 0)
		d = is_float ? vl.vl_float : vl.vl_double;
	else
		d = 0;

	(void) signal(SIGILL, old_sigill_func);

	if (Got_sigill) {
		/*  Note: get_real checks for this output when reading
		 *        a value so the exact format of the output
		 *	  is important.
		 */
		if (is_float)
			sprintf(buf, "<illegal float 0x%08x>", vl.vl_int);
		else
			sprintf(buf, "<illegal double 0x%x%08x>",
						vl.vl_ints[DOUBLE_MSW],
						vl.vl_ints[DOUBLE_LSW]);
	}
	else {
		char *s;

		if (is_float)
			sprintf(buf, "%.6g", d);
		else
			sprintf(buf, "%.12g", d);
		
		/*  We always want a decimal point even if the value
		 *  is integral.
		 */
		s = buf;
		if (*s == '-')
			++s;
		while (isdigit(*s))
			++s;
		if (*s == '\0')
			strcpy(s, ".0");
	}

	return buf;
}

/*  Read a string from the data area.
 */
int
dgets(addr, optr, max_nbytes)
taddr_t addr;
register char *optr;
int max_nbytes;
{
	char ibuf[4], *olim;
	register char *iptr;

	olim = optr + max_nbytes - 1;
	iptr = ibuf + sizeof(ibuf);
	while (optr < olim) {
		if (iptr == ibuf + sizeof(ibuf)) {
			if (dread(addr, ibuf, sizeof(ibuf)) != 0)
				return -1;
			iptr = ibuf;
			addr += sizeof(ibuf);
		}
		if (*iptr == '\0')
			break;
		*optr++ = *iptr++;
	}
	*optr++ = '\0';
	return 0;
}

/*  Nominal address of the registers. Recognised by dread()
 *
 *  Just pick a value that can't be confused with a stack or data address.
 *
 *  BUG: this is unsafe now that we have mmap() and shared libraries -
 *  something could actually be mapped at this address.
 */
#define REG_ADDR		((taddr_t)(0xf0000000 - REG_MIN_REGNO))

/*  Maximum number of registers.  Used only for limiting the range of
 *  pseudo addresses that will be interpreted as register numbers.
 */
#define MAX_REGS		256

/*  Is addr an encoded register address?
 */
#define IS_REG_ADDR(addr)	(addr >= (REG_ADDR + REG_MIN_REGNO) && \
						addr < REG_ADDR + MAX_REGS)

/*  Convert a register number to an "address" that will be recognised by
 *  dread().
 */
taddr_t
regno_to_addr(regno)
int regno;
{
	if (regno > MAX_REGS)
		panic("bad reg addr n rta");
	return REG_ADDR + regno;
}

int
dread_fpval(addr, is_reg, is_double, buf)
taddr_t addr;
bool is_reg, is_double;
char *buf;
{
	int res;
	fpval_t fpval;

	if (IS_REG_ADDR(addr)) {
		res = proc_read_fpreg(Current_proc, (int)(addr - REG_ADDR),
							is_double, &fpval);
	}
	else if (is_reg) {
		/*  A register that's been saved on the stack.
		 */
		res = proc_read_fpval(Current_proc, addr, is_double, &fpval);
	}
	else
		return dread(addr, buf, is_double ? sizeof(double) : sizeof(float));

	if (res != 0)
		return -1;

	if (is_double)
		*(double *)buf = fpval.d;
	else
		*(float *)buf = fpval.f;

	return 0;
}

/*  Read n bytes into buf from the data or stack area of the target process.
 *  We deduce from addr whether we are supposed to read the stack or
 *  the data area.
 *
 *  Return 0 for success, -1 for failure.
 */
int
dread(addr, buf, nbytes)
taddr_t addr;
char *buf;
int nbytes;
{
	taddr_t regval;
	int regno;

	if (IS_REG_ADDR(addr)) {
		regno = addr - REG_ADDR;

		if (proc_readreg(Current_proc, regno, &regval) != 0)
			return -1;

		switch(nbytes) {
		case 8:
			/*  We assume this is a pair of registers
			 */
			memcpy(buf, (char *)&regval, 4);
			if (proc_readreg(Current_proc, regno + 1, &regval) != 0)
				return -1;
			memcpy(buf + 4, (char *)&regval, 4);
			break;
		case 4:
			memcpy(buf, (char *)&regval, 4);
			break;
		case 2:
#ifdef IS_BIG_ENDIAN
			memcpy(buf, ((char *)&regval) + 2, 2);
#else
			memcpy(buf, (char *)&regval, 2);
#endif
			break;
		case 1:
#ifdef IS_BIG_ENDIAN
			*buf = ((char *)&regval)[3];
#else
			*buf = regval;
#endif
			break;
		default:
			panic("nbytes botch in dread");
			break;
		}
		return 0;
	}

	return proc_read_data(Current_proc, addr, buf, nbytes);
}

int
dwrite(addr, buf, nbytes)
taddr_t addr;
const char *buf;
int nbytes;
{
	taddr_t regval;
	int regno;

	if (Current_proc == 0)
		panic("dwrite called on a core file");

	if (IS_REG_ADDR(addr)) {
		regno = addr - REG_ADDR;
#ifdef ARCH_SUN3
		/* BUG */
		if (regno >= 14) {
			errf("\bcan't handle 68881/fpa register floats yet");
			return -1;
		}
#endif
		switch(nbytes) {
		case 8:
			/*  We assume this is a pair of registers
			 */
			memcpy((char *)&regval, buf + 4, 4);
			if (proc_setreg(Current_proc, regno + 1, regval) != 0)
				return -1;

			memcpy((char *)&regval, buf, 4);
			break;
		case 4:
			memcpy((char *)&regval, buf, 4);
			break;
		case 2:
			regval = 0;
#ifdef IS_BIG_ENDIAN
			memcpy(((char *)&regval) + 2, buf, 2);
#else
			memcpy((char *)&regval, buf, 2);
#endif
			break;
		case 1:
			regval = 0;
#ifdef IS_BIG_ENDIAN
			((char *)&regval)[3] = *buf;
#else
			regval = *buf;
#endif
			break;
		default:
			panic("nbytes botch in dw");
			regval = 0; /* to satisfy gcc */
			break;
		}
		return proc_setreg(Current_proc, regno, regval);
	}

	return proc_write_data(Current_proc, addr, buf, nbytes);
}

void
dump_uarea_to_file(name)
const char *name;
{
	taddr_t addr, val;
	proc_t proc;
	FILE *fp;

	if (get_target_state() == TS_NOTR) {
		errf("Target not running");
		return;
	}
	if ((proc = Current_proc) == NULL) {
		errf("Can't dump core file u areas (yet)");
		return;
	}

	if ((fp = fopen_new_file(name)) == NULL)
		return;
	errf("Dumping u area to file %s", name);

	for (addr = 0; proc_read_uarea(proc, addr, (char *)&val, 4) == 0; addr += 4)
		fprintf(fp, "%04x: %08x %10d\n", addr, val, val);

	if (fclose_new_file(fp, name) == 0)
		errf("Dump of u area complete");
}

static int
charval(n)
int n;
{
	 n &= 0xff;
	 return (isascii(n) && isprint(n)) ? n : '?';
}

void
dump_stack_to_file(name)
const char *name;
{
	FILE *ofp;
	taddr_t sp, fp, ap, pc, addr;
	int val, nzeroes;
	const char *label;
	bool is_labelled;

	if (get_target_state() == TS_NOTR) {
		errf("Target not running");
		return;
	}

	pc = proc_getreg(Current_proc, REG_PC);
	sp = proc_getreg(Current_proc, REG_SP);
	fp = proc_getreg(Current_proc, REG_FP);
	ap = proc_getreg(Current_proc, REG_AP);
	if (sp % sizeof(val) != 0) {
		errf("SP (0x%x) not a multiple of %d", sp, sizeof(val));
		return;
	}

	if ((ofp = fopen_new_file(name)) == NULL)
		return;
	errf("Dumping stack to file %s", name);

	fprintf(ofp, "sp:%08x fp:%08x ap:%08x pc:%08x\n", sp, fp, ap, pc);

	nzeroes = 0;
	for (addr = sp; dread(addr, (char *)&val, 4) == 0; addr += 4) {

		/*  On the BSDi box, dread() goes on and on succeeding,
		 *  generating huge stack dumps, hence this boredom theshold.
		 */
		if (val != 0) {
			nzeroes = 0;
		}
		else {
			if (++nzeroes == 1000) {
				fputs(
				   "Giving up after 1000 consecutive zeroes\n", 
									ofp);
				break;
			}
		}

		label = stack_addr_label(addr, &is_labelled);
		(void) fprintf(ofp, is_labelled ? "%32s " : "%-32s ", label);
		(void) fprintf(ofp, "%8x |%8x|  |%12d|  |%c%c%c%c|\n",
				addr, val, val,
				charval(val),
				charval(val >> 8),
				charval(val >> 16),
				charval(val >> 24));
	}

	if (fclose_new_file(ofp, name) == 0)
		errf("Stack dump complete");
}
