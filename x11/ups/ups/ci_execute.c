/* ci_execute.c - execute interpreter code */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_ci_execute_c_sccsid[] = "@(#)ci_execute.c	1.20 17/9/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <sys/types.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <setjmp.h>
#include <signal.h>

#include <local/ukcprog.h>

#include "ci.h"
#include "ci_machine.h"

#ifdef CXSTATS
static void print_stats PROTO((unsigned long *counts, int ncounts));
#endif
static void spfailed PROTO((long expected, long got, long location));
static void tracelink PROTO((machine_t *ma, unsigned char *savpc,
								unsigned char *pc));
static void traceunlink PROTO((machine_t *ma, char *fp, unsigned char *pc));
static void catch_fpe PROTO((int sig));

static jmp_buf Sigfpe_env;

/*  BUG: we take no precautions to ensure that the C interpreter address
 *       range doesn't overlap other address ranges.  This means that this
 *       routine may give the wrong answer.
 */
bool
ci_is_ci_addr(code_id, addr)
code_id_t code_id;
unsigned long addr;
{
	machine_t *ma;

	ma = (machine_t *)code_id;
	return (char *)addr >= ma->ma_data && (char *)addr < ma->ma_maxsp;
}

void
ci_initialise_code(code_id, reset_data)
code_id_t code_id;
bool reset_data;
{
	machine_t *ma;
	reg_reloc_t *rr;

	ma = (machine_t *)code_id;

	for (rr = ma->ma_reg_relocs; rr != NULL; rr = rr->rr_next) {
		unsigned long val;
		textword_t *p;

		val = (*ma->ma_get_regaddr_proc)(ma->ma_get_regaddr_proc_arg,
								     rr->rr_regno);

		/*  Do what ci_code_long() does.
		 */
		p = ma->ma_text + rr->rr_addr;
		p[0] = val & 0xff;
		p[1] = (val >> 8) & 0xff;
		p[2] = (val >> 16) & 0xff;
		p[3] = (val >> 24) & 0xff;
	}

	ma->ma_pc = ma->ma_text + ma->ma_entry_point;
	ma->ma_sp = (stackword_t *)(ma->ma_data + ma->ma_data_size +
							    ma->ma_bss_size +
							    ma->ma_stack_size);
	ma->ma_maxsp = (char *)ma->ma_sp;
	ma->ma_minsp = (unsigned long *)((char *)ma->ma_sp - ma->ma_stack_size);
	ma->ma_fp = NULL;
	ma->ma_funclevel = 0;

	if (reset_data) {
		memcpy(ma->ma_data, ma->ma_data_copy, ma->ma_data_size);
		memset(ma->ma_data + ma->ma_data_size, 0, ma->ma_bss_size);
	}
}

static void
catch_fpe(sig)
int sig;
{
	longjmp(Sigfpe_env, 1);
}

ci_exec_result_t
ci_execute_code(code_id, procfp, procap, readproc, writeproc, indirect_call_proc)
code_id_t code_id;
textaddr_t procfp, procap;
ci_readproc_t readproc;
ci_writeproc_t writeproc;
ci_indirect_call_proc_t indirect_call_proc;
{
	union {
		unsigned char d_uchar;
		char d_char;
		unsigned short d_ushort;
		short d_short;
		unsigned long d_ulong;
		long d_long;
		float d_float;
		double d_double;
	} d;
	union {
		double rv_double;
		float rv_float;
		stackword_t rv_word;
	} retval;
	machine_t *ma;
	textword_t *text;
	register textword_t *pc;
	register stackword_t *sp;
	char *data, *fp, *minsp, *maxsp;
	int i, ncase, minval, switchval, nfuncs;	/* for switch ops */
	int offset, width, mask;			/* for bitfield ops */
	int checkval;					/* for checksp ops */
	int count;					/* for multi_arrow */
	long nbytes;
	stackword_t addr;
	textaddr_t *funcaddrs;
	int nargs, func_index;
	ci_exec_result_t res;
#ifdef CXSTATS
	unsigned long opcounts[(int)OC_LAST_OPCODE];
#endif
	void (*oldfpe)PROTO((int sig));
#ifdef ARCH_VAX
	void (*oldill)PROTO((int sig));
#endif

	ma = (machine_t *)code_id;
	pc = ma->ma_pc;
	sp = ma->ma_sp;
	minsp = (char *)ma->ma_minsp;
	maxsp = ma->ma_maxsp;
	fp = ma->ma_fp;
	text = ma->ma_text;
	data = ma->ma_data;
	funcaddrs = ma->ma_funcaddrs;
	nfuncs = ma->ma_nfuncs;

	nargs = 0;	/* to satisfy gcc (I don't know why it complains) */

#ifdef CXSTATS
	memset((char *)opcounts, 0, sizeof(opcounts));
#endif

#define IS_CI_ADDR(addr)	((char *)(addr) >= data && (char *)(addr) < maxsp)
#define READDATA(addr, item)	(*readproc)(addr, (char *)&item, sizeof(item))
#define WRITEDATA(addr, item)	(*writeproc)(addr, (const char *)&item, sizeof(item))

#define PUSH(val, sp)		(*--(sp) = (val))
#define POP(sp)			(*(sp)++)

	/*  Catch divisions by zero etc in the target code.
	 */
	oldfpe = signal(SIGFPE, catch_fpe);
#ifdef ARCH_VAX
	/*  The VAX sends a SIGILL signal for certain illegal floating
	 *  point values.
	 */
	oldill = signal(SIGILL, catch_fpe);
#endif
	if (setjmp(Sigfpe_env) != 0) {
		res = CI_ER_ARITHMETIC_EXCEPTION;
		goto quit;
	}
		
	for (;;) {
#ifdef CXSTATS
		++opcounts[*pc];
#endif
		switch ((opcode_t)*pc++) {

		case OC_PUSH_WORD_RETVAL:
			PUSH(retval.rv_word, sp);
			break;
		case OC_PUSH_FLOAT_RETVAL:
			sp -= FLOAT_NSLOTS;
			*(float *)sp = retval.rv_float;
			break;
		case OC_PUSH_DOUBLE_RETVAL:
			sp -= DOUBLE_NSLOTS;
			*(double *)sp = retval.rv_double;
			break;

		case OC_TRACERET_FLOAT:
			traceunlink(ma, fp, pc);
		case OC_RET_FLOAT:
			retval.rv_float = *(float *)sp;
			goto unlink;

		case OC_TRACERET_DOUBLE:
			traceunlink(ma, fp, pc);
		case OC_RET_DOUBLE:
			retval.rv_double = *(double *)sp;
			goto unlink;

		case OC_TRACERET:
			traceunlink(ma, fp, pc);
			goto unlink;

		case OC_TRACERET_WORD:
			traceunlink(ma, fp, pc);
		case OC_RET_WORD:
			retval.rv_word = *sp;
		case OC_RET:
unlink:			sp = (stackword_t *)fp;
			fp = (char *)POP(sp);
			pc = (textword_t *)POP(sp);
			break;

		case OC_PUSH_STRUCTRET_ADDR:
			offset = ((unsigned long *)fp)[2];
			addr = ((unsigned long *)fp)[offset + 2];
			PUSH(addr, sp);
			break;

		case OC_TRACERET_STRUCT:
			traceunlink(ma, fp, pc);
		case OC_RET_STRUCT:
			nbytes = GETLONG(pc);
			addr = *sp;
			sp = (stackword_t *)fp;
			fp = (char *)POP(sp);
			pc = (textword_t *)POP(sp);
			if (*sp != 0)
				memcpy((char *)sp[*sp], (char *)addr, nbytes);
			break;

		case OC_CALL_B:
			func_index = *pc++;
func_call:
			nargs = *pc++;
			if (func_index < 0) {
				/* BUG: what if the func returns double?
				 */
				res = (*indirect_call_proc)(code_id,
							    funcaddrs[func_index],
							    sp, nargs,
							    &retval.rv_word);
				if (res != CI_ER_CONTINUE)
					goto quit;
			}
			else if (func_index < nfuncs) {
				PUSH((stackword_t)pc, sp);
				pc = text + funcaddrs[func_index];
			}
			else {
				typedef stackword_t (*lfaddr_t)PROTO((stackword_t,
								      ...));
				lfaddr_t lfaddr;
				stackword_t rvword;

				lfaddr = (lfaddr_t)funcaddrs[func_index];
#ifdef CXSTATS
				if (lfaddr == (lfaddr_t)exit &&
							  getenv("CXSTATS") != NULL)
					print_stats(opcounts, (int)OC_LAST_OPCODE);
#endif
				switch (nargs) {
				case 0:
					rvword = (*(stackword_t (*)PROTO((void)))lfaddr)();
					break;
				case 1:
					rvword = (*lfaddr)(sp[0]);
					break;
				case 2:
					rvword = (*lfaddr)(sp[0], sp[1]);
					break;
				case 3:
					rvword = (*lfaddr)(sp[0], sp[1], sp[2]);
					break;
				case 4:
					rvword = (*lfaddr)(sp[0], sp[1], sp[2],
							   sp[3]);
					break;
				case 5:
					rvword = (*lfaddr)(sp[0], sp[1], sp[2],
							   sp[3], sp[4]);
					break;
				case 6:
					rvword = (*lfaddr)(sp[0], sp[1], sp[2],
							   sp[3], sp[4], sp[5]);
					break;
				case 7:
					rvword = (*lfaddr)(sp[0], sp[1], sp[2],
							   sp[3], sp[4], sp[5],
							   sp[6]);
					break;
				case 8:
					rvword = (*lfaddr)(sp[0], sp[1], sp[2],
							   sp[3], sp[4], sp[5],
							   sp[6], sp[7]);
					break;
				case 9:
					rvword = (*lfaddr)(sp[0], sp[1], sp[2],
							   sp[3], sp[4], sp[5],
							   sp[6], sp[7], sp[8]);
					break;
				case 10:
					rvword = (*lfaddr)(sp[0], sp[1], sp[2],
							   sp[3], sp[4], sp[5],
							   sp[6], sp[7], sp[8],
							   sp[9]);
					break;
				default:
					if (nargs > 30)
						panic("too many args");
					rvword = (*lfaddr)(sp[0],  sp[1],  sp[2],
							   sp[3],  sp[4],  sp[5],
							   sp[6],  sp[7],  sp[8],
							   sp[9],  sp[10], sp[11],
							   sp[12], sp[13], sp[14],
							   sp[15], sp[16], sp[17],
							   sp[18], sp[19], sp[20],
							   sp[21], sp[22], sp[23],
							   sp[24], sp[25], sp[26],
							   sp[27], sp[28], sp[29]);
					break;
				}
				/* BUG: what if the function returned double? */
				retval.rv_word = rvword;
			}
			break;
		case OC_CALL_W:
			func_index = GETWORD(pc);
			pc += 2;
			goto func_call;
		case OC_CALL_L:
			func_index = GETLONG(pc);
			pc += 4;
			goto func_call;
		case OC_CALL_INDIRECT:
			func_index = POP(sp);
			goto func_call;

		case OC_TRACELINK_B:
			tracelink(ma, (unsigned char *)*sp, pc);
		case OC_LINK_B:
			PUSH((stackword_t)fp, sp);
			fp = (char *)sp;
			sp = (stackword_t *)((char *)sp - *pc++);
			if ((char *)sp - *pc++ < minsp) {
				res = CI_ER_STACK_OVERFLOW;
				--pc;
				goto quit;
			}
			break;

		case OC_TRACELINK_W:
			tracelink(ma, (unsigned char *)*sp, pc);
		case OC_LINK_W:
			PUSH((stackword_t)fp, sp);
			fp = (char *)sp;
			sp = (stackword_t *)((char *)sp - GETWORD(pc));
			pc += 2;
			if ((char *)sp - *pc++ < minsp)
				panic("stack overflow");
			break;

		case OC_TRACELINK_L:
			tracelink(ma, (unsigned char *)*sp, pc);
		case OC_LINK_L:
			PUSH((stackword_t)fp, sp);
			fp = (char *)sp;
			sp = (stackword_t *)((char *)sp - GETLONG(pc));
			pc += 4;
			if ((char *)sp - *pc++ < minsp)
				panic("stack overflow");
			break;

		case OC_POPMANY_B:
			sp = (stackword_t *)((char *)sp + *pc++);
			break;
		case OC_POPMANY_W:
			sp = (stackword_t *)((char *)sp + GETWORD(pc));
			pc += 2;
			break;
		case OC_POPMANY_L:
			sp = (stackword_t *)((char *)sp + GETLONG(pc));
			pc += 4;
			break;

		case OC_PROC_MEMCPY_B:
			nbytes = *pc++;
			goto proc_memcpy;
		case OC_PROC_MEMCPY_W:
			nbytes = GETWORD(pc);
			pc += 2;
			goto proc_memcpy;
		case OC_PROC_MEMCPY_L:
			nbytes = GETLONG(pc);
			pc += 4;
proc_memcpy:
			if (IS_CI_ADDR(sp[1])) {
				if (IS_CI_ADDR(*sp))
					memcpy((char *)sp[1], (char *)*sp, nbytes);
				else {
					if ((*readproc)(*sp, (char *)sp[1],
								     nbytes) != 0) {
						res = CI_ER_READDATA_FAILED;
						goto quit;
					}
				}
			}
			else {
				if (IS_CI_ADDR(*sp)) {
					if ((*writeproc)(sp[1], (char *)*sp,
								     nbytes) != 0) {
						res = CI_ER_WRITEDATA_FAILED;
						goto quit;
					}
				}
				else {
					char *buf;

					buf = e_malloc(nbytes);
					if ((*readproc)(*sp, buf, nbytes) != 0) {
						res = CI_ER_READDATA_FAILED;
						goto quit;
					}
					if ((*writeproc)(sp[1], buf, nbytes) != 0) {
						res = CI_ER_WRITEDATA_FAILED;
						goto quit;
					}
					free(buf);
				}
			}

			sp += 2;
			break;

		case OC_MEMCPY_B:
			memcpy((char *)sp[1], (char *)*sp, (size_t)*pc++);
			sp += 2;
			break;
		case OC_MEMCPY_W:
			memcpy((char *)sp[1], (char *)*sp, (size_t)GETWORD(pc));
			pc += 2;
			sp += 2;
			break;
		case OC_MEMCPY_L:
			memcpy((char *)sp[1], (char *)*sp, (size_t)GETLONG(pc));
			pc += 4;
			sp += 2;
			break;
		
		case OC_RESERVE_BYTES:
			nbytes = GETLONG(pc);
			pc += 4;
			sp -= nbytes / 4;
			sp[-1] = (long)sp;
			--sp;
			break;

		case OC_PROC_PUSH_BYTES:
			nbytes = GETLONG(pc);
			addr = *sp;
			sp -= nbytes / 4 - 1;

			if (IS_CI_ADDR(*sp))
				memcpy((char *)sp, (char *)addr, nbytes);
			else {
				if ((*readproc)((unsigned)addr, (char *)sp, nbytes) != 0){
					res = CI_ER_READDATA_FAILED;
					goto quit;
				}
			}

			pc += 4;
			break;
		
		case OC_PUSH_BYTES:
			nbytes = GETLONG(pc);
			addr = *sp;
			sp -= nbytes / 4 - 1;
			memcpy((char *)sp, (char *)addr, nbytes);
			pc += 4;
			break;
		
		case OC_EXTRACT_UNSIGNED_BITFIELD:
			offset = *pc++;
			width = *pc++;
			*sp = (*sp >> offset) & ((1 << width) - 1);
			break;
		case OC_EXTRACT_SIGNED_BITFIELD:
			offset = *pc++;
			width = *pc++;
			*sp = (*sp >> offset) & ((1 << width) - 1);
			if (*sp & (1 << (width - 1)))
				*sp |= ~0 << width;
			break;
		case OC_INSERT_SIGNED_BITFIELD:
		case OC_INSERT_UNSIGNED_BITFIELD:
			offset = *pc++;
			width = *pc++;
			mask = ((1 << width) - 1) << offset;
			sp[1] = (sp[1] & ~mask) | ((*sp << offset) & mask);
			++sp;
			break;

		case OC_PROC_ASSIGN_BYTE:
			d.d_char = *sp;
			if (IS_CI_ADDR(sp[1]))
				*(char *)sp[1] = d.d_char;
			else {
				if (WRITEDATA(sp[1], d.d_char) != 0) {
					res = CI_ER_WRITEDATA_FAILED;
					goto quit;
				}
			}
			sp += 2;
			break;
		case OC_PROC_ASSIGN_WORD:
			d.d_short = *sp;
			if (IS_CI_ADDR(sp[1]))
				*(short *)sp[1] = d.d_short;
			else {
				if (WRITEDATA(sp[1], d.d_short) != 0) {
					res = CI_ER_WRITEDATA_FAILED;
					goto quit;
				}
			}
			sp += 2;
			break;
		case OC_PROC_ASSIGN_LONG:
			d.d_long = *sp;
			if (IS_CI_ADDR(sp[1]))
				*(long *)sp[1] = d.d_long;
			else {
				if (WRITEDATA(sp[1], d.d_long) != 0) {
					res = CI_ER_WRITEDATA_FAILED;
					goto quit;
				}
			}
			sp += 2;
			break;
		case OC_PROC_ASSIGN_FLOAT:
			d.d_float = *(float *)sp;
			if (IS_CI_ADDR(sp[FLOAT_NSLOTS]))
				*(float *)sp[FLOAT_NSLOTS] = d.d_float;
			else {
				if (WRITEDATA(sp[FLOAT_NSLOTS], d.d_float) != 0) {
					res = CI_ER_WRITEDATA_FAILED;
					goto quit;
				}
			}
			sp += FLOAT_NSLOTS + 1;
			break;
		case OC_PROC_ASSIGN_DOUBLE:
			d.d_double = *(double *)sp;
			if (IS_CI_ADDR(sp[DOUBLE_NSLOTS]))
				*(double *)sp[DOUBLE_NSLOTS] = d.d_double;
			else {
				if (WRITEDATA(sp[DOUBLE_NSLOTS], d.d_double) != 0) {
					res = CI_ER_WRITEDATA_FAILED;
					goto quit;
				}
			}
			sp += DOUBLE_NSLOTS + 1;
			break;

		case OC_ASSIGN_BYTE:
			*(char *)sp[1] = *sp;
			sp += 2;
			break;
		case OC_ASSIGN_WORD:
			*(short *)sp[1] = *sp;
			sp += 2;
			break;
		case OC_ASSIGN_LONG:
			*(long *)sp[1] = *sp;
			sp += 2;
			break;
		case OC_ASSIGN_FLOAT:
			*(float *)sp[FLOAT_NSLOTS] = *(float *)sp;
			sp += FLOAT_NSLOTS + 1;
			break;
		case OC_ASSIGN_DOUBLE:
			*(double *)sp[DOUBLE_NSLOTS] = *(double *)sp;
			sp += DOUBLE_NSLOTS + 1;
			break;

		case OC_PROC_ASSIGN_AND_PUSH_BYTE:
			d.d_char = *sp;
			if (IS_CI_ADDR(sp[1]))
				*(char *)sp[1] = d.d_char;
			else {
				if (WRITEDATA(sp[1], d.d_char) != 0) {
					res = CI_ER_WRITEDATA_FAILED;
					goto quit;
				}
			}
			sp[1] = d.d_char;
			++sp;
			break;
		case OC_PROC_ASSIGN_AND_PUSH_WORD:
			d.d_short = *sp;
			if (IS_CI_ADDR(sp[1]))
				*(short *)sp[1] = d.d_short;
			else {
				if (WRITEDATA(sp[1], d.d_short) != 0) {
					res = CI_ER_WRITEDATA_FAILED;
					goto quit;
				}
			}
			sp[1] = d.d_short;
			++sp;
			break;
		case OC_PROC_ASSIGN_AND_PUSH_LONG:
			d.d_long = *sp;
			if (IS_CI_ADDR(sp[1]))
				*(long *)sp[1] = d.d_long;
			else {
				if (WRITEDATA(sp[1], d.d_long) != 0) {
					res = CI_ER_WRITEDATA_FAILED;
					goto quit;
				}
			}
			sp[1] = d.d_long;
			++sp;
			break;
		case OC_PROC_ASSIGN_AND_PUSH_FLOAT:
			d.d_float = *(float *)sp;
			if (IS_CI_ADDR(sp[FLOAT_NSLOTS]))
				*(float *)sp[FLOAT_NSLOTS] = d.d_float;
			else {
				if (WRITEDATA(sp[FLOAT_NSLOTS], d.d_float) != 0) {
					res = CI_ER_WRITEDATA_FAILED;
					goto quit;
				}
			}
			++sp;
			*(float *)sp = d.d_float;
			break;
		case OC_PROC_ASSIGN_AND_PUSH_DOUBLE:
			d.d_double = *(double *)sp;
			if (IS_CI_ADDR(sp[DOUBLE_NSLOTS]))
				*(double *)sp[DOUBLE_NSLOTS] = d.d_double;
			else {
				if (WRITEDATA(sp[DOUBLE_NSLOTS], d.d_double) != 0) {
					res = CI_ER_WRITEDATA_FAILED;
					goto quit;
				}
			}
			++sp;
			*(double *)sp = d.d_double;
			break;

		case OC_ASSIGN_AND_PUSH_BYTE:
			*(char *)sp[1] = *sp;
			sp[1] = *sp;
			++sp;
			break;
		case OC_ASSIGN_AND_PUSH_WORD:
			*(short *)sp[1] = *sp;
			sp[1] = *sp;
			++sp;
			break;
		case OC_ASSIGN_AND_PUSH_LONG:
			*(long *)sp[1] = *sp;
			sp[1] = *sp;
			++sp;
			break;
		case OC_ASSIGN_AND_PUSH_FLOAT:
			d.d_float = *(float *)sp;
			*(float *)sp[FLOAT_NSLOTS] = d.d_float;
			++sp;
			*(float *)sp = d.d_float;
			break;
		case OC_ASSIGN_AND_PUSH_DOUBLE:
			d.d_double = *(double *)sp;
			*(double *)sp[DOUBLE_NSLOTS] = d.d_double;
			++sp;
			*(double *)sp = d.d_double;
			break;

		case OC_PROC_DEREF_SIGNED_BYTE:
			if (IS_CI_ADDR(*sp))
				d.d_char = *(char *)*sp;
			else {
				if (READDATA(*sp, d.d_char) != 0) {
					res = CI_ER_READDATA_FAILED;
					goto quit;
				}
			}
			*sp = d.d_char;
			break;
		case OC_PROC_DEREF_SIGNED_WORD:
			if (IS_CI_ADDR(*sp))
				d.d_short = *(short *)*sp;
			else {
				if (READDATA(*sp, d.d_short) != 0) {
					res = CI_ER_READDATA_FAILED;
					goto quit;
				}
			}
			*sp = d.d_short;
			break;
		case OC_PROC_DEREF_SIGNED_LONG:
			if (IS_CI_ADDR(*sp))
				d.d_long = *(long *)*sp;
			else {
				if (READDATA(*sp, d.d_long) != 0) {
					res = CI_ER_READDATA_FAILED;
					goto quit;
				}
			}
			*sp = d.d_long;
			break;
		case OC_PROC_DEREF_FLOAT:
			if (IS_CI_ADDR(*sp))
				d.d_float = *(float *)*sp;
			else {
				if (READDATA(*sp, d.d_float) != 0) {
					res = CI_ER_READDATA_FAILED;
					goto quit;
				}
			}
			sp -= FLOAT_NSLOTS - 1;
			*(float *)sp = d.d_float;
			break;
		case OC_PROC_DEREF_DOUBLE:
			if (IS_CI_ADDR(*sp))
				d.d_double = *(double *)*sp;
			else {
				if (READDATA(*sp, d.d_double) != 0) {
					res = CI_ER_READDATA_FAILED;
					goto quit;
				}
			}
			sp -= DOUBLE_NSLOTS - 1;
			*(double *)sp = d.d_double;
			break;

		case OC_PROC_DEREF_UNSIGNED_BYTE:
			if (IS_CI_ADDR(*sp))
				d.d_uchar = *(unsigned char *)*sp;
			else {
				if (READDATA(*sp, d.d_uchar) != 0) {
					res = CI_ER_READDATA_FAILED;
					goto quit;
				}
			}
			*sp = d.d_uchar;
			break;
		case OC_PROC_DEREF_UNSIGNED_WORD:
			if (IS_CI_ADDR(*sp))
				d.d_ushort = *(unsigned short *)*sp;
			else {
				if (READDATA(*sp, d.d_ushort) != 0) {
					res = CI_ER_READDATA_FAILED;
					goto quit;
				}
			}
			*sp = d.d_ushort;
			break;
		case OC_PROC_DEREF_UNSIGNED_LONG:
			if (IS_CI_ADDR(*sp))
				d.d_ulong = *(unsigned long *)*sp;
			else {
				if (READDATA(*sp, d.d_ulong) != 0) {
					res = CI_ER_READDATA_FAILED;
					goto quit;
				}
			}
			*sp = d.d_ulong;
			break;

		case OC_DEREF_SIGNED_BYTE:
			*sp = *(char *)*sp;
			break;
		case OC_DEREF_SIGNED_WORD:
			*sp = *(short *)*sp;
			break;
		case OC_DEREF_SIGNED_LONG:
			*sp = *(long *)*sp;
			break;

		case OC_DEREF_UNSIGNED_BYTE:
			*sp = *(unsigned char *)*sp;
			break;
		case OC_DEREF_UNSIGNED_WORD:
			*sp = *(unsigned short *)*sp;
			break;
		case OC_DEREF_UNSIGNED_LONG:
			*sp = *(unsigned long *)*sp;
			break;
		case OC_DEREF_FLOAT:
			sp -= FLOAT_NSLOTS - 1;
			*(float *)sp = *(float *)sp[FLOAT_NSLOTS - 1];
			break;
		case OC_DEREF_DOUBLE:
			sp -= DOUBLE_NSLOTS - 1;
			*(double *)sp = *(double *)sp[DOUBLE_NSLOTS - 1];
			break;
		
		case OC_MULTI_ARROW:
			offset = sp[0];
			addr = sp[1];
			count = sp[2];

			if (count < 0) {
				res = CI_ER_BAD_MA_COUNT;
				goto quit;
			}

			while (--count >= 0) {
				if (IS_CI_ADDR(addr)) {
					addr = *(unsigned long *)(addr + offset);
				}
				else {
					if (READDATA(addr + offset,
							     d.d_ulong) != 0) {
						res = CI_ER_READDATA_FAILED;
						goto quit;
					}
					addr = d.d_ulong;
				}
			}

			sp += 2;
			*sp = addr;

			break;

		case OC_NEG_CONSTPUSH_B:
			PUSH(-*pc++, sp);
			break;
		case OC_NEG_CONSTPUSH_W:
			PUSH(-GETWORD(pc), sp);
			pc += 2;
			break;
		case OC_NEG_CONSTPUSH_L:
			PUSH(-GETLONG(pc), sp);
			pc += 4;
			break;

		case OC_CONSTPUSH_B:
			PUSH(*pc++, sp);
			break;
		case OC_CONSTPUSH_W:
			PUSH(GETWORD(pc), sp);
			pc += 2;
			break;
		case OC_CONSTPUSH_L:
			PUSH(GETLONG(pc), sp);
			pc += 4;
			break;
		
		case OC_PUSH_FLOAT_CONST:
			sp -= FLOAT_NSLOTS;
			memcpy((char *)sp, pc, FLOAT_NBYTES);
			pc += FLOAT_NBYTES;
			break;
		case OC_PUSH_DOUBLE_CONST:
			sp -= DOUBLE_NSLOTS;
			memcpy((char *)sp, pc, DOUBLE_NBYTES);
			pc += DOUBLE_NBYTES;
			break;

		case OC_CHECK_SP_B:
			checkval = *pc++;
			if (fp - (char *)sp != checkval)
				spfailed(checkval, fp - (char *)sp, (pc - text) - 2);
			break;
		case OC_CHECK_SP_W:
			checkval = GETWORD(pc);
			if (fp - (char *)sp != checkval)
				spfailed(checkval, fp - (char *)sp, (pc - text) - 1);
			pc += 2;
			break;
		case OC_CHECK_SP_L:
			checkval = GETLONG(pc);
			if (fp - (char *)sp != checkval)
				spfailed(checkval, fp - (char *)sp, (pc - text) - 1);
			pc += 4;
			break;

		case OC_PROC_PUSH_FP_ADDR_B:
			PUSH((stackword_t)(procfp + *pc++), sp);
			break;
		case OC_PROC_PUSH_FP_ADDR_W:
			PUSH((stackword_t)(procfp + GETWORD(pc)), sp);
			pc += 2;
			break;
		case OC_PROC_PUSH_FP_ADDR_L:
			PUSH((stackword_t)(procfp + GETLONG(pc)), sp);
			pc += 4;
			break;

		case OC_PROC_PUSH_AP_ADDR_B:
			PUSH((stackword_t)(procap + *pc++), sp);
			break;
		case OC_PROC_PUSH_AP_ADDR_W:
			PUSH((stackword_t)(procap + GETWORD(pc)), sp);
			pc += 2;
			break;
		case OC_PROC_PUSH_AP_ADDR_L:
			PUSH((stackword_t)(procap + GETLONG(pc)), sp);
			pc += 4;
			break;

		case OC_PUSH_STACKADDR_B:
			PUSH((stackword_t)(fp + *pc++), sp);
			break;
		case OC_PUSH_STACKADDR_W:
			PUSH((stackword_t)(fp + GETWORD(pc)), sp);
			pc += 2;
			break;
		case OC_PUSH_STACKADDR_L:
			PUSH((stackword_t)(fp + GETLONG(pc)), sp);
			pc += 4;
			break;

		case OC_PUSH_UNSIGNED_BYTE_AT_ADDR_B:
			addr = *pc++;
			PUSH(*(unsigned char *)addr, sp);
			break;
		case OC_PUSH_UNSIGNED_BYTE_AT_ADDR_W:
			PUSH(*(unsigned char *)GETWORD(pc), sp);
			pc += 2;
			break;
		case OC_PUSH_UNSIGNED_BYTE_AT_ADDR_L:
			PUSH(*(unsigned char *)GETLONG(pc), sp);
			pc += 4;
			break;

		case OC_PUSH_UNSIGNED_SHORT_AT_ADDR_B:
			addr = *pc++;
			PUSH(*(unsigned short *)addr, sp);
			break;
		case OC_PUSH_UNSIGNED_SHORT_AT_ADDR_W:
			PUSH(*(unsigned short *)GETWORD(pc), sp);
			pc += 2;
			break;
		case OC_PUSH_UNSIGNED_SHORT_AT_ADDR_L:
			PUSH(*(unsigned short *)GETLONG(pc), sp);
			pc += 4;
			break;

		case OC_PUSH_UNSIGNED_LONG_AT_ADDR_B:
			addr = *pc++;
			PUSH(*(unsigned long *)addr, sp);
			break;
		case OC_PUSH_UNSIGNED_LONG_AT_ADDR_W:
			PUSH(*(unsigned long *)GETWORD(pc), sp);
			pc += 2;
			break;
		case OC_PUSH_UNSIGNED_LONG_AT_ADDR_L:
			PUSH(*(unsigned long *)GETLONG(pc), sp);
			pc += 4;
			break;

		case OC_PUSH_UNSIGNED_BYTE_AT_STACKADDR_B:
			PUSH(*(unsigned char *)(fp + *pc++), sp);
			break;
		case OC_PUSH_UNSIGNED_BYTE_AT_STACKADDR_W:
			PUSH(*(unsigned char *)(fp + GETWORD(pc)), sp);
			pc += 2;
			break;
		case OC_PUSH_UNSIGNED_BYTE_AT_STACKADDR_L:
			PUSH(*(unsigned char *)(fp + GETLONG(pc)), sp);
			pc += 4;
			break;

		case OC_PUSH_UNSIGNED_SHORT_AT_STACKADDR_B:
			PUSH(*(unsigned short *)(fp + *pc++), sp);
			break;
		case OC_PUSH_UNSIGNED_SHORT_AT_STACKADDR_W:
			PUSH(*(unsigned short *)(fp + GETWORD(pc)), sp);
			pc += 2;
			break;
		case OC_PUSH_UNSIGNED_SHORT_AT_STACKADDR_L:
			PUSH(*(unsigned short *)(fp + GETLONG(pc)), sp);
			pc += 4;
			break;

		case OC_PUSH_UNSIGNED_LONG_AT_STACKADDR_B:
			PUSH(*(unsigned long *)(fp + *pc++), sp);
			break;
		case OC_PUSH_UNSIGNED_LONG_AT_STACKADDR_W:
			PUSH(*(unsigned long *)(fp + GETWORD(pc)), sp);
			pc += 2;
			break;
		case OC_PUSH_UNSIGNED_LONG_AT_STACKADDR_L:
			PUSH(*(unsigned long *)(fp + GETLONG(pc)), sp);
			pc += 4;
			break;

		case OC_PUSH_SIGNED_BYTE_AT_ADDR_B:
			addr = *pc++;
			PUSH(*(char *)addr, sp);
			break;
		case OC_PUSH_SIGNED_BYTE_AT_ADDR_W:
			PUSH(*(char *)GETWORD(pc), sp);
			pc += 2;
			break;
		case OC_PUSH_SIGNED_BYTE_AT_ADDR_L:
			PUSH(*(char *)GETLONG(pc), sp);
			pc += 4;
			break;

		case OC_PUSH_SIGNED_SHORT_AT_ADDR_B:
			addr = *pc++;
			PUSH(*(short *)addr, sp);
			break;
		case OC_PUSH_SIGNED_SHORT_AT_ADDR_W:
			PUSH(*(short *)GETWORD(pc), sp);
			pc += 2;
			break;
		case OC_PUSH_SIGNED_SHORT_AT_ADDR_L:
			PUSH(*(short *)GETLONG(pc), sp);
			pc += 4;
			break;

		case OC_PUSH_SIGNED_LONG_AT_ADDR_B:
			addr = *pc++;
			PUSH(*(long *)addr, sp);
			break;
		case OC_PUSH_SIGNED_LONG_AT_ADDR_W:
			PUSH(*(long *)GETWORD(pc), sp);
			pc += 2;
			break;
		case OC_PUSH_SIGNED_LONG_AT_ADDR_L:
			PUSH(*(long *)GETLONG(pc), sp);
			pc += 4;
			break;
		
		case OC_PUSH_FLOAT_AT_ADDR_B:
			sp -= FLOAT_NSLOTS;
			addr = *pc++;
			memcpy((char *)sp, (char *)addr, FLOAT_NBYTES);
			break;
		case OC_PUSH_FLOAT_AT_ADDR_W:
			sp -= FLOAT_NSLOTS;
			memcpy((char *)sp, (char *)GETWORD(pc), FLOAT_NBYTES);
			pc += 2;
			break;
		case OC_PUSH_FLOAT_AT_ADDR_L:
			sp -= FLOAT_NSLOTS;
			memcpy((char *)sp, (char *)GETLONG(pc), FLOAT_NBYTES);
			pc += 4;
			break;

		case OC_PUSH_DOUBLE_AT_ADDR_B:
			sp -= DOUBLE_NSLOTS;
			addr = *pc++;
			memcpy((char *)sp, (char *)addr, DOUBLE_NBYTES);
			break;
		case OC_PUSH_DOUBLE_AT_ADDR_W:
			sp -= DOUBLE_NSLOTS;
			memcpy((char *)sp, (char *)GETWORD(pc), DOUBLE_NBYTES);
			pc += 2;
			break;
		case OC_PUSH_DOUBLE_AT_ADDR_L:
			sp -= DOUBLE_NSLOTS;
			memcpy((char *)sp, (char *)GETLONG(pc), DOUBLE_NBYTES);
			pc += 4;
			break;

		case OC_PUSH_SIGNED_BYTE_AT_STACKADDR_B:
			PUSH(*(char *)(fp + *pc++), sp);
			break;
		case OC_PUSH_SIGNED_BYTE_AT_STACKADDR_W:
			PUSH(*(char *)(fp + GETWORD(pc)), sp);
			pc += 2;
			break;
		case OC_PUSH_SIGNED_BYTE_AT_STACKADDR_L:
			PUSH(*(char *)(fp + GETLONG(pc)), sp);
			pc += 4;
			break;

		case OC_PUSH_SIGNED_SHORT_AT_STACKADDR_B:
			PUSH(*(short *)(fp + *pc++), sp);
			break;
		case OC_PUSH_SIGNED_SHORT_AT_STACKADDR_W:
			PUSH(*(short *)(fp + GETWORD(pc)), sp);
			pc += 2;
			break;
		case OC_PUSH_SIGNED_SHORT_AT_STACKADDR_L:
			PUSH(*(short *)(fp + GETLONG(pc)), sp);
			pc += 4;
			break;

		case OC_PUSH_SIGNED_LONG_AT_STACKADDR_B:
			PUSH(*(long *)(fp + *pc++), sp);
			break;
		case OC_PUSH_SIGNED_LONG_AT_STACKADDR_W:
			PUSH(*(long *)(fp + GETWORD(pc)), sp);
			pc += 2;
			break;
		case OC_PUSH_SIGNED_LONG_AT_STACKADDR_L:
			PUSH(*(long *)(fp + GETLONG(pc)), sp);
			pc += 4;
			break;

		case OC_PUSH_FLOAT_AT_STACKADDR_B:
			sp -= FLOAT_NSLOTS;
			memcpy((char *)sp, fp + *pc++, FLOAT_NBYTES);
			break;
		case OC_PUSH_FLOAT_AT_STACKADDR_W:
			sp -= FLOAT_NSLOTS;
			memcpy((char *)sp, fp + GETWORD(pc), FLOAT_NBYTES);
			pc += 2;
			break;
		case OC_PUSH_FLOAT_AT_STACKADDR_L:
			sp -= FLOAT_NSLOTS;
			memcpy((char *)sp, fp + GETLONG(pc), FLOAT_NBYTES);
			pc += 4;
			break;

		case OC_PUSH_DOUBLE_AT_STACKADDR_B:
			sp -= DOUBLE_NSLOTS;
			memcpy((char *)sp, fp + *pc++, DOUBLE_NBYTES);
			break;
		case OC_PUSH_DOUBLE_AT_STACKADDR_W:
			sp -= DOUBLE_NSLOTS;
			memcpy((char *)sp, fp + GETWORD(pc), DOUBLE_NBYTES);
			pc += 2;
			break;
		case OC_PUSH_DOUBLE_AT_STACKADDR_L:
			sp -= DOUBLE_NSLOTS;
			memcpy((char *)sp, fp + GETLONG(pc), DOUBLE_NBYTES);
			pc += 4;
			break;

		case OC_SWITCH_ON_TABLE:
			ncase = GETWORD(pc);
			pc += 2;
			minval = GETLONG(pc);
			switchval = POP(sp) - minval;
			if ((unsigned)switchval >= ncase)
				switchval = ncase;
			pc += 4 + switchval * 2;
			pc += (short)GETWORD(pc);
			break;

		case OC_SWITCH_ON_CHAIN_B:
			ncase = GETWORD(pc);
			pc += 2;
			minval = GETLONG(pc);
			pc += 4;
			switchval = POP(sp) - minval;
			for (i = 0; i < ncase; ++i) {
				int caseval;

				caseval = *pc++;
				if (caseval == switchval) {
					pc += (short)GETWORD(pc);
					break;
				}
				pc += 2;
			}
			if (i == ncase)
				pc += (short)GETWORD(pc);
			break;

		case OC_SWITCH_ON_CHAIN_W:
			ncase = GETWORD(pc);
			pc += 2;
			minval = GETLONG(pc);
			pc += 4;
			switchval = POP(sp) - minval;
			for (i = 0; i < ncase; ++i) {
				int caseval;

				caseval = GETWORD(pc);
				pc += 2;
				if (caseval == switchval) {
					pc += (short)GETWORD(pc);
					break;
				}
				pc += 2;
			}
			if (i == ncase)
				pc += (short)GETWORD(pc);
			break;

		case OC_SWITCH_ON_CHAIN_L:
			ncase = GETWORD(pc);
			pc += 2;
			minval = GETLONG(pc);
			pc += 4;
			switchval = POP(sp) - minval;
			for (i = 0; i < ncase; ++i) {
				int caseval;

				caseval = GETLONG(pc);
				pc += 4;
				if (caseval == switchval) {
					pc += (short)GETWORD(pc);
					break;
				}
				pc += 2;
			}
			if (i == ncase)
				pc += (short)GETWORD(pc);
			break;

		case OC_UNRESOLVED_JUMP:
			panic("bad opcode: unresolved jump");
			break;
		case OC_JUMP:
			pc += (short)GETWORD(pc);
			break;
		case OC_JUMP_IF_NON_ZERO:
			pc += (POP(sp) != 0) ? (short)GETWORD(pc) : 2;
			break;
		case OC_JUMP_IF_ZERO:
			pc += (POP(sp) == 0) ? (short)GETWORD(pc) : 2;
			break;

		case OC_DUP:
			--sp;
			*sp = sp[1];
			break;
		case OC_DUP_BACK_ONE:
			sp[2] = *sp;
			break;
		case OC_RESERVE_SLOT:
			--sp;
			break;
		case OC_POP:
			++sp;
			break;

		case OC_CVT_TO_BOOL:
			*sp = *sp != 0;
			break;

		case OC_BITWISE_NOT:
			*sp = ~*sp;
			break;
		case OC_LOGICAL_NOT:
			*sp = !*sp;
			break;
		case OC_UNARY_MINUS:
			*sp = -*sp;
			break;

		case OC_FLOAT_UNARY_MINUS:
			*(float *)sp = -*(float *)sp;
			break;
		case OC_ADD_FLOATS:
			*(float *)&sp[FLOAT_NSLOTS] += *(float *)sp;
			sp += FLOAT_NSLOTS;
			break;
		case OC_SUB_FLOATS:
			*(float *)&sp[FLOAT_NSLOTS] -= *(float *)sp;
			sp += FLOAT_NSLOTS;
			break;
		case OC_MUL_FLOATS:
			*(float *)&sp[FLOAT_NSLOTS] *= *(float *)sp;
			sp += FLOAT_NSLOTS;
			break;
		case OC_CHKDIV_FLOATS:
			if (*(float *)sp == 0.0) {	/* BUG: exact cmp */
				res = CI_ER_DIVISION_BY_ZERO;
				goto quit;
			}
			/* fall through */
		case OC_DIV_FLOATS:
			*(float *)&sp[FLOAT_NSLOTS] /= *(float *)sp;
			sp += FLOAT_NSLOTS;
			break;

		case OC_DOUBLE_UNARY_MINUS:
			*(double *)sp = -*(double *)sp;
			break;
		case OC_ADD_DOUBLES:
			*(double *)&sp[DOUBLE_NSLOTS] += *(double *)sp;
			sp += DOUBLE_NSLOTS;
			break;
		case OC_SUB_DOUBLES:
			*(double *)&sp[DOUBLE_NSLOTS] -= *(double *)sp;
			sp += DOUBLE_NSLOTS;
			break;
		case OC_MUL_DOUBLES:
			*(double *)&sp[DOUBLE_NSLOTS] *= *(double *)sp;
			sp += DOUBLE_NSLOTS;
			break;

		case OC_CHKDIV_DOUBLES:
			if (*(double *)sp == 0.0) {	/* BUG: exact cmp */
				res = CI_ER_DIVISION_BY_ZERO;
				goto quit;
			}
			/* fall through */
		case OC_DIV_DOUBLES:
			*(double *)&sp[DOUBLE_NSLOTS] /= *(double *)sp;
			sp += DOUBLE_NSLOTS;
			break;

		case OC_BITWISE_AND:
			sp[1] &= *sp;
			++sp;
			break;
		case OC_BITWISE_XOR:
			sp[1] ^= *sp;
			++sp;
			break;
		case OC_BITWISE_OR:
			sp[1] |= *sp;
			++sp;
			break;
		case OC_MUL_UNSIGNED:
			sp[1] *= *sp;
			++sp;
			break;
		case OC_MUL_SIGNED:
			sp[1] = (long)sp[1] * (long)*sp;
			++sp;
			break;
		case OC_CHKDIV_UNSIGNED:
			if (*sp == 0) {
				res = CI_ER_DIVISION_BY_ZERO;
				goto quit;
			}
			/* fall through */
		case OC_DIV_UNSIGNED:
			sp[1] /= *sp;
			++sp;
			break;
		case OC_CHKDIV_SIGNED:
			if (*sp == 0) {
				res = CI_ER_DIVISION_BY_ZERO;
				goto quit;
			}
			/* fall through */
		case OC_DIV_SIGNED:
			sp[1] = (long)sp[1] / (long)*sp;
			++sp;
			break;
		case OC_MOD:
			sp[1] %= *sp;
			++sp;
			break;
		case OC_ADD:
			sp[1] += *sp;
			++sp;
			break;
		case OC_SUB:
			sp[1] -= *sp;
			++sp;
			break;
		case OC_LSHIFT:
			sp[1] <<= *sp;
			++sp;
			break;
		case OC_RSHIFT:
			sp[1] >>= *sp;
			++sp;
			break;

		case OC_IS_EQUAL:
			sp[1] = sp[1] == *sp;
			++sp;
			break;
		case OC_NOT_EQUAL:
			sp[1] = sp[1] != *sp;
			++sp;
			break;

		case OC_LESS_SIGNED:
			sp[1] = (long)sp[1] < (long)*sp;
			++sp;
			break;
		case OC_GREATER_SIGNED:
			sp[1] = (long)sp[1] > (long)*sp;
			++sp;
			break;
		case OC_LESS_OR_EQUAL_SIGNED:
			sp[1] = (long)sp[1] <= (long)*sp;
			++sp;
			break;
		case OC_GREATER_OR_EQUAL_SIGNED:
			sp[1] = (long)sp[1] >= (long)*sp;
			++sp;
			break;

		case OC_LESS_UNSIGNED:
			sp[1] = sp[1] < *sp;
			++sp;
			break;
		case OC_GREATER_UNSIGNED:
			sp[1] = sp[1] > *sp;
			++sp;
			break;
		case OC_LESS_OR_EQUAL_UNSIGNED:
			sp[1] = sp[1] <= *sp;
			++sp;
			break;
		case OC_GREATER_OR_EQUAL_UNSIGNED:
			sp[1] = sp[1] >= *sp;
			++sp;
			break;
		
		case OC_FLOAT_IS_EQUAL:
			sp[2 * FLOAT_NSLOTS - 1] =
					*(float *)&sp[FLOAT_NSLOTS] == *(float *)sp;
			sp += 2 * FLOAT_NSLOTS - 1;
			break;
		case OC_FLOAT_NOT_EQUAL:
			sp[2 * FLOAT_NSLOTS - 1] =
					*(float *)&sp[FLOAT_NSLOTS] != *(float *)sp;
			sp += 2 * FLOAT_NSLOTS - 1;
			break;
		case OC_FLOAT_LESS:
			sp[2 * FLOAT_NSLOTS - 1] =
					*(float *)&sp[FLOAT_NSLOTS] < *(float *)sp;
			sp += 2 * FLOAT_NSLOTS - 1;
			break;
		case OC_FLOAT_GREATER:
			sp[2 * FLOAT_NSLOTS - 1] =
					*(float *)&sp[FLOAT_NSLOTS] > *(float *)sp;
			sp += 2 * FLOAT_NSLOTS - 1;
			break;
		case OC_FLOAT_LESS_OR_EQUAL:
			sp[2 * FLOAT_NSLOTS - 1] =
					*(float *)&sp[FLOAT_NSLOTS] <= *(float *)sp;
			sp += 2 * FLOAT_NSLOTS - 1;
			break;
		case OC_FLOAT_GREATER_OR_EQUAL:
			sp[2 * FLOAT_NSLOTS - 1] =
					*(float *)&sp[FLOAT_NSLOTS] >= *(float *)sp;
			sp += 2 * FLOAT_NSLOTS - 1;
			break;

		case OC_DOUBLE_IS_EQUAL:
			sp[2 * DOUBLE_NSLOTS - 1] =
				     *(double *)&sp[DOUBLE_NSLOTS] == *(double *)sp;
			sp += 2 * DOUBLE_NSLOTS - 1;
			break;
		case OC_DOUBLE_NOT_EQUAL:
			sp[2 * DOUBLE_NSLOTS - 1] =
				     *(double *)&sp[DOUBLE_NSLOTS] != *(double *)sp;
			sp += 2 * DOUBLE_NSLOTS - 1;
			break;
		case OC_DOUBLE_LESS:
			sp[2 * DOUBLE_NSLOTS - 1] =
				     *(double *)&sp[DOUBLE_NSLOTS] < *(double *)sp;
			sp += 2 * DOUBLE_NSLOTS - 1;
			break;
		case OC_DOUBLE_GREATER:
			sp[2 * DOUBLE_NSLOTS - 1] =
				     *(double *)&sp[DOUBLE_NSLOTS] > *(double *)sp;
			sp += 2 * DOUBLE_NSLOTS - 1;
			break;
		case OC_DOUBLE_LESS_OR_EQUAL:
			sp[2 * DOUBLE_NSLOTS - 1] =
				     *(double *)&sp[DOUBLE_NSLOTS] <= *(double *)sp;
			sp += 2 * DOUBLE_NSLOTS - 1;
			break;
		case OC_DOUBLE_GREATER_OR_EQUAL:
			sp[2 * DOUBLE_NSLOTS - 1] =
				     *(double *)&sp[DOUBLE_NSLOTS] >= *(double *)sp;
			sp += 2 * DOUBLE_NSLOTS - 1;
			break;
		
		case OC_CVT_FLOAT_TO_DOUBLE:
			sp -= DOUBLE_NSLOTS - FLOAT_NSLOTS;
			*(double *)sp = *(float *)&sp[DOUBLE_NSLOTS - FLOAT_NSLOTS];
			break;
		case OC_CVT_LONG_TO_DOUBLE:
			sp -= DOUBLE_NSLOTS - 1;
			*(double *)sp = sp[DOUBLE_NSLOTS - 1];
			break;

		case OC_CVT_FLOAT_TO_LONG:
			sp -= FLOAT_NSLOTS - 1;
			*sp = *(float *)&sp[FLOAT_NSLOTS - 1];
			break;
		case OC_CVT_DOUBLE_TO_LONG:
			sp[DOUBLE_NSLOTS - 1] = *(double *)sp;
			sp += DOUBLE_NSLOTS - 1;
			break;

		case OC_CVT_LONG_TO_FLOAT:
			sp -= FLOAT_NSLOTS - 1;
			*(float *)sp = sp[FLOAT_NSLOTS - 1];
			break;
		case OC_CVT_DOUBLE_TO_FLOAT:
			*(float *)&sp[DOUBLE_NSLOTS - FLOAT_NSLOTS] = *(double *)sp;
			sp += DOUBLE_NSLOTS - FLOAT_NSLOTS;
			break;
		
		case OC_TRAP:
			res = CI_ER_TRAP;
			goto quit;

		default:
			panic("illegal opcode");
			break;
		}
	}

quit:
	ma->ma_pc = pc - 1;
	ma->ma_sp = sp;
	ma->ma_fp = fp;

#ifdef CXSTATS
	if (getenv("CXSTATS") != NULL)
		print_stats(opcounts, (int)OC_LAST_OPCODE);
#endif

	signal(SIGFPE, oldfpe);
#ifdef ARCH_VAX
	signal(SIGILL, oldill);
#endif

	return res;
}

static void
tracelink(ma, savpc, pc)
machine_t *ma;
unsigned char *savpc, *pc;
{
	long from_pc, to_pc;

	from_pc = savpc - ma->ma_text;
	to_pc = (pc - ma->ma_text) - 1;

	if (from_pc < 0 || from_pc >= ma->ma_text_size)
		panic("from_pc botch in tul");
	if (to_pc < 0 || to_pc >= ma->ma_text_size)
		panic("from_pc botch in tul");

	printf("%d->%d call: %d -> %d\n", ma->ma_funclevel, ma->ma_funclevel + 1,
								   from_pc, to_pc);
	++ma->ma_funclevel;
	fflush(stdout);
}

static void
traceunlink(ma, fp, pc)
machine_t *ma;
char *fp;
unsigned char *pc;
{
	long from_pc, to_pc;

	from_pc = (pc - ma->ma_text) - 1;
	to_pc = ((unsigned char **)fp)[1] - ma->ma_text;

	if (from_pc < 0 || from_pc >= ma->ma_text_size)
		panic("from_pc botch in tul");
	if (to_pc < 0 || to_pc >= ma->ma_text_size)
		panic("from_pc botch in tul");

	printf("%d->%d ret: %d -> %d\n", ma->ma_funclevel, ma->ma_funclevel - 1,
					from_pc, to_pc);
	--ma->ma_funclevel;
	fflush(stdout);
}

static void
spfailed(expected, got, location)
long expected, got, location;
{
	errf("SP botch (expected %d, got %d) pc=%d",
		    expected, got, location);
	panic("sp botch");
}

#ifdef CXSTATS
static void
print_stats(counts, ncounts)
unsigned long *counts;
int ncounts;
{
	int i;

	for (i = 0; i < ncounts; ++i)
		printf("%3d: %ld\n", i, counts[i]);
}
#endif
