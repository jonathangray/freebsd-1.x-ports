/* as_sparc.c - find the jumps in SPARC machine code */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_as_sparc_c_sccsid[] = "@(#)as_sparc.c	1.11 12/9/92 (UKC)";

#include <mtrprog/ifdefs.h>

#ifdef ARCH_SUN4
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include <local/ukcprog.h>
#include "ups.h"
#include "as.h"
#include "debug.h"
#include "proc.h"


typedef unsigned long psr_t, fpsr_t;

/*  A SPARC instruction.
 */
typedef union sparc_instun {
	unsigned word;
	struct {
		unsigned op:2;
		unsigned disp30:30;
	} fmt1;
	struct {
		unsigned op:2;
		unsigned rd:5;
		unsigned op2:3;
		unsigned imm22:22;
	} fmt2_sethi;
	struct {
		unsigned op:2;
		unsigned a:1;
		unsigned cond:4;
		unsigned op2:3;
		unsigned disp22:22;
	} fmt2;
	struct {
		unsigned op:2;
		unsigned rd:5;
		unsigned op3:6;
		unsigned rs1:5;
		unsigned i:1;
		unsigned asi:8;
		unsigned rs2:5;
	} fmt3r;
	struct {
		unsigned op:2;
		unsigned rd:5;
		unsigned op3:6;
		unsigned rs1:5;
		unsigned i:1;
		unsigned simm13:13;
	} fmt3i;
	struct {
		unsigned op:2;
		unsigned rd:5;
		unsigned op3:6;
		unsigned rs1:5;
		unsigned opf:9;
		unsigned rs2:5;
	} fmt3f;
} sparc_inst_t;

typedef enum { UNIMP = 0, BICC = 2, SETHI = 4, FBFCC = 6, CBCCC = 7 } btype_t;

/*  Some opcodes that we special case.
 */
#define OR	002
#define SUBCC	024
#define TICC	072
#define JMPL	070
#define RESTORE	075
#define FPOP1	064
#define FPOP2	065

typedef enum {
	CC_N,	CC_E,	CC_LE,	CC_L,	CC_LEU,	CC_LU,	CC_NEG,	CC_VS,
	CC_A,	CC_NE,	CC_G,	CC_GE,	CC_GU,	CC_GEU,	CC_POS,	CC_VC
} cond_t;

typedef enum {
	FC_N,	FC_NE,	FC_LG,	FC_UL, FC_L,	FC_UG,	FC_G,	FC_U,
	FC_A,	FC_E,	FC_UE,	FC_GE, FC_UGE,	FC_LE,	FC_ULE,	FC_O
} fp_cond_t;

static int sign_ext PROTO((unsigned u, int bit));
static int cond_holds PROTO((psr_t psr, cond_t cond));
static int fp_cond_holds PROTO((fpsr_t psr, fp_cond_t cond));

static int
sign_ext(u, bit)
unsigned u;
int bit;
{
	if  (u & (1 << (bit - 1)))
		u |= ~(~0 & ((1 << bit) - 1));
	return  u;
}

const char *
disassemble_one_instruction(addr, ctext, p_buf)
taddr_t addr;
const char *ctext, **p_buf;
{
	typedef struct fpopmapst {
		const char *fm_name;
		int fm_opf;
		bool fm_allregs;
	} fpopmap_t;
	static const char *regnames[] = {
		"%g0",	"%g1",	"%g2",	"%g3",	"%g4",	"%g5",	"%g6",	"%g7",	
		"%o0",	"%o1",	"%o2",	"%o3",	"%o4",	"%o5",	"%sp",	"%o7",	
		"%l0",	"%l1",	"%l2",	"%l3",	"%l4",	"%l5",	"%l6",	"%l7",	
		"%i0",	"%i1",	"%i2",	"%i3",	"%i4",	"%i5",	"%fp",	"%i7",	
	};
	static const char *condnames[16] = {
		"bn",	"be",	"ble",	"bl",	"bleu",	"blu",	"bneg",	"bvs",
		"ba",	"bne",	"bg",	"bge",	"bgu",	"bgeu",	"bpos",	"bvc"
	};
	static const char *fcondnames[16] = {
		"fbn",	"fbne",	"fblg",	"fbul",	"fbl",	"fbug",	"fbg",	"fbu",
		"fba",	"fbe",	"fbue",	"fbge",	"fbuge","fble",	"fbule","o"
	};
	static const char *cbcondnames[16] = {
		"cbn",	"cb123","cb12",	"cb13",	"cb1",	"cb23",	"cb2",	"cb3",
		"cba",	"cb0",	"cb03",	"cb02",	"cb023","cb01",	"cb013","cb012"
	};
	static const char *opnames2[64] = {
		"add",	"and",	"or",	"xor",	"sub",	"andn",	"orn",	"xnor",
		"addx",	NULL,	NULL,	NULL,	"subx",	NULL,	NULL,	NULL,
		"addcc","andcc","orcc",	"xorcc","subcc","andncc","orncc","xnorcc",
		"addxcc",NULL,	NULL,	NULL,	"subxcc",NULL,	NULL,	NULL,
		"taddcc","tsubcc","taddcctv","tsubcctv","mulscc","sll","srl","sra",
		"rdy",	"rdpsr","rdwim","rdtbr",NULL,	NULL,	NULL,	NULL,
		"wry",	"wrpsr","wrwim","wrtby","fpop1","fpop2","cpop1","cpop2",
		"jmpl",	"rett",	"ticc",	"iflush","save","restore",NULL,	NULL,
	};
	static const char *opnames3[64] = {
		"ld",	"ldub",	"lduh",	"ldd",	"st",	"stb",	"sth",	"std",
		NULL,	"ldsb",	"ldsh",	NULL,	NULL,	"ldstub",NULL,	"swap",
		"lda",	"lduba","lduha","ldda",	"sta",	"stba",	"stha",	"stda",
		NULL,	"ldsba","ldsha",NULL,	NULL,	"ldstuba",NULL,	"swapa",
		"ldf",	"ldfsr",NULL,	"lddf",	"stf",	"stfsr","stdfq","stdf",
		NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
		"ldc",	"ldcsr",NULL,	"lddc",	"stc",	"stcsr","stdcq","stdc",
		NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
	};
	static fpopmap_t fpop1_map[] = {
		"fmovs",	0001,	FALSE,
		"fnegs",	0005,	FALSE,
		"fabss",	0011,	FALSE,
		"fsqrts",	0051,	FALSE,
		"fsqrtd",	0052,	FALSE,
		"fsqrtx",	0053,	FALSE,
		"fadds",	0101,	TRUE,
		"faddd",	0102,	TRUE,
		"faddx",	0103,	TRUE,
		"fsubs",	0105,	TRUE,
		"fsubd",	0106,	TRUE,
		"fsubx",	0107,	TRUE,
		"fmuls",	0111,	TRUE,
		"fmuld",	0112,	TRUE,
		"fmulx",	0113,	TRUE,
		"fdivs",	0115,	TRUE,
		"fdivd",	0116,	TRUE,
		"fdivx",	0117,	TRUE,
		"fitos",	0304,	TRUE,
		"fdtos",	0306,	FALSE,
		"fxtos",	0307,	FALSE,
		"fitod",	0310,	FALSE,
		"fstod",	0311,	FALSE,
		"fxtod",	0313,	FALSE,
		"fxtod",	0313,	FALSE,
		"fitox",	0314,	FALSE,
		"fstox",	0315,	FALSE,
		"fdtox",	0316,	FALSE,
		"fstoi",	0321,	FALSE,
		"fdtoi",	0322,	FALSE,
		"fxtoi",	0323,	FALSE,
		NULL,		0,	FALSE,
	};
	static fpopmap_t fpop2_map[] = {
		"fcmps",	0121,	TRUE,
		"fcmpd",	0122,	TRUE,
		"fcmpx",	0123,	TRUE,
		"fcmpes",	0125,	TRUE,
		"fcmped",	0126,	TRUE,
		"fcmpex",	0127,	TRUE,
		NULL,		0,	FALSE,
	};
		
	static const char **ctabs[8] = {
		NULL, NULL, condnames, NULL, NULL, NULL, fcondnames, cbcondnames
	};
	static char buf[60];
	static char srcops[30];
	sparc_inst_t si;
	const char *opname, *sep, *regname;
	int opcode, reg2;
	taddr_t operand;

	si = *(sparc_inst_t *)ctext;

	switch (si.fmt1.op) {
	case 0:
		switch ((btype_t)si.fmt2.op2) {
		case UNIMP:
			operand = si.fmt2_sethi.imm22;
			if (operand < 10)
				sprintf(buf, "unimp %d", operand);
			else
				sprintf(buf, "unimp 0x%x", operand);
			break;

		case CBCCC:
		case FBFCC:
		case BICC:
			operand = addr + (sign_ext(si.fmt2.disp22, 22) << 2);

			if (ctabs[si.fmt2.op2] != NULL)
				strcpy(buf, ctabs[si.fmt2.op2][si.fmt2.cond]);
			else
				sprintf(buf, "<b%d>%s", si.fmt2.op2,
						condnames[si.fmt2.cond] + 1);

			sprintf(buf + strlen(buf), "%s\t%s",
				(si.fmt2.a != 0) ? ",a" : "",
				addr_to_func_and_offset(operand, TRUE));
			break;

		case SETHI:
			if (si.fmt2_sethi.rd == 0 && si.fmt2_sethi.imm22 == 0)
				strcpy(buf, "nop");
			else
				sprintf(buf, "sethi\t%%hi(0x%x), %s",
						si.fmt2_sethi.imm22 << 10,
						regnames[si.fmt2_sethi.rd]);
			break;
		}
		break;
	case 1:
		operand = addr + (si.fmt1.disp30 << 2);
		sprintf(buf, "call\t%s", addr_to_func_and_offset(operand, TRUE));
		break;
	case 2:
		opcode = si.fmt3i.op3;
		opname = opnames2[opcode];

		if (opname == NULL) {
			if (Debug_flags & DBFLAG_DBXASM)
				strcpy(buf, "badop");
			else
				sprintf(buf, "<bad fmt2 opcode 0x%8x", si.word);
			break;
		}

		if (opcode == FPOP1 || opcode == FPOP2) {
			fpopmap_t *fm;
			int opf;

			opf = si.fmt3f.opf;
			fm = (opcode == FPOP1) ? fpop1_map : fpop2_map;
			for (; fm->fm_name != NULL; ++fm)
				if (fm->fm_opf == opf)
					break;
			if (fm->fm_name == NULL)
				sprintf(buf, "<bad fmt3 fp opcode 0x%8x", si.word);
			else if (fm->fm_allregs)
				sprintf(buf, "%s\t%%f%d, %%f%d, %%f%d", fm->fm_name,
					si.fmt3f.rs1, si.fmt3f.rs2, si.fmt3f.rd);
			else
				sprintf(buf, "%s\t%%f%d, %%f%d", fm->fm_name,
						si.fmt3f.rs1, si.fmt3f.rd);
			break;
		}

		if (opcode == TICC) {
			char condname[8];

			strcpy(condname, condnames[si.fmt2.cond]);
			*condname = 't';
			opname = condname;
		}

		if (opcode == JMPL) {
			if (si.fmt3i.i && si.fmt3i.simm13 == 8 &&
					si.fmt3i.rs1 == 31 && si.fmt3i.rd == 0) {
				strcpy(buf, "ret");
				break;
			}
			if (si.fmt3i.rd == 0) {
				if (si.fmt3i.i) {
					sprintf(buf, "jmp\t%s + %d",
						regnames[si.fmt3i.rs1],
						sign_ext(si.fmt3i.simm13, 13));
				}
				else  {
					sprintf(buf, "jmp\t%s + %s",
						regnames[si.fmt3r.rs1],
						regnames[si.fmt3r.rs2]);
				}
				break;
			}
			sep = " + ";
		}
		else
			sep = ", ";
		
		if (opcode == RESTORE && si.fmt3r.i == 0 && si.fmt3r.rs1 == 0 &&
					si.fmt3r.rs2 == 0 && si.fmt3r.rd == 0) {
			strcpy(buf, "restore");
			break;
		}

		if (si.fmt3i.i) {
			if (opcode == OR && si.fmt3i.rs1 == 0)
				sprintf(buf, "mov\t%d, %s",
						sign_ext(si.fmt3i.simm13, 13),
						regnames[si.fmt3i.rd]);
			else if (opcode == SUBCC && si.fmt3i.rd == 0)
				sprintf(buf, "cmp\t%s, %d",
						regnames[si.fmt3i.rs1],
						sign_ext(si.fmt3i.simm13, 13));
			else
				sprintf(buf, "%s\t%s%s%d, %s", opname,
						regnames[si.fmt3i.rs1], sep,
						sign_ext(si.fmt3i.simm13, 13),
						regnames[si.fmt3i.rd]);
		}
		else {
			if (opcode == OR && si.fmt3i.rs1 == 0)
				sprintf(buf, "mov\t%s, %s",
						regnames[si.fmt3r.rs2],
						regnames[si.fmt3r.rd]);
			else if (opcode == SUBCC && si.fmt3i.rd == 0)
				sprintf(buf, "cmp\t%s, %s",
						regnames[si.fmt3r.rs1],
						regnames[si.fmt3r.rs2]);
			else
				sprintf(buf, "%s\t%s%s%s, %s", opname,
						regnames[si.fmt3r.rs1], sep,
						regnames[si.fmt3r.rs2],
						regnames[si.fmt3r.rd]);
		}
		break;
	case 3:
		opname = opnames3[si.fmt3i.op3];
		reg2 = si.fmt3r.rs2;
		if (opname == NULL) {
			if (Debug_flags & DBFLAG_DBXASM)
				strcpy(buf, "badop");
			else
				sprintf(buf, "<bad fmt3 opcode 0x%8x>", si.word);
			break;
		}
		if (si.fmt3i.i) {
			sprintf(srcops, "[%s + %d]",
						regnames[si.fmt3i.rs1],
						sign_ext(si.fmt3i.simm13, 13));
		}
		else if (si.fmt3r.asi != 0) {
			if (reg2 == 0) {
				sprintf(srcops, "[%s] 0x%02x",
						regnames[si.fmt3r.rs1],
						si.fmt3r.asi);
			}
			else {
				sprintf(srcops, "[%s + %s] 0x%02x",
						regnames[si.fmt3r.rs1],
						regnames[si.fmt3r.rs2],
						si.fmt3r.asi);
			}
		}
		else {
			if (reg2 == 0)
				sprintf(srcops, "[%s]", regnames[si.fmt3r.rs1]);
			else {
				sprintf(srcops, "[%s + %s]",
						regnames[si.fmt3r.rs1],
						regnames[si.fmt3r.rs2]);
			}
		}

		if (opname[strlen(opname) - 1] == 'f') {
			static char fbuf[5];	/* '%', 'f', '1', '4', '\0' */

			sprintf(fbuf, "%%f%d", si.fmt3r.rd);
			regname = fbuf;
		}
		else
			regname = regnames[si.fmt3r.rd];

		if (*opname == 's')
			sprintf(buf, "%s\t%s, %s", opname, regname, srcops);
		else
			sprintf(buf, "%s\t%s, %s", opname, srcops, regname);

		break;
	}

	*p_buf = buf;
	return ctext + 4;
}

static int
fp_cond_holds(psr, fp_cond)
psr_t psr;
fp_cond_t fp_cond;
{
	bool e, g, l, u;
	int cc;

	cc = (psr >> 10) & 3;
	e = cc == 0;
	l = cc == 1;
	g = cc == 2;
	u = cc == 3;

	switch (fp_cond) {
		case FC_N:	return 0;
		case FC_NE:	return l | g | u;
		case FC_LG:	return l | g;
		case FC_UL:	return l | u;
		case FC_L:	return l;
		case FC_UG:	return g | u;
		case FC_G:	return g;
		case FC_U:	return u;
		case FC_A:	return 1;
		case FC_E:	return e;
		case FC_UE:	return u | e;
		case FC_GE:	return e | g;
		case FC_UGE:	return e | g | u;
		case FC_LE:	return e | l;
		case FC_ULE:	return e | l | u;
		case FC_O:	return e | l | g;
	}

	panic("cond botch in fch");
	return 0;	/* to satisfy gcc */
}

static int
cond_holds(psr, cond)
psr_t psr;
cond_t cond;
{
	bool n, z, v, c;

	n = (psr & (1 << 23)) != 0;
	z = (psr & (1 << 22)) != 0;
	v = (psr & (1 << 21)) != 0;
	c = (psr & (1 << 20)) != 0;

	switch (cond) {
		case CC_N:	return 0;
		case CC_E:	return z;
		case CC_LE:	return z | (n ^ v);
		case CC_L:	return n ^ v;
		case CC_LEU:	return c | z;
		case CC_LU:	return c;
		case CC_NEG:	return n;
		case CC_VS:	return v;
		case CC_A:	return TRUE;
		case CC_NE:	return !z;
		case CC_G:	return !(z | (n ^ v));
		case CC_GE:	return !(n ^ v);
		case CC_GU:	return !(c | z);
		case CC_GEU:	return !c;
		case CC_POS:	return !n;
		case CC_VC:	return !v;
	}

	panic("cond botch in ch");
	return 0;	/* to satisfy gcc */
}

taddr_t
get_next_pc(proc, addr)
proc_t proc;
taddr_t addr;
{
	sparc_inst_t si;
	psr_t psr;
	int offset;

	if (proc_read_text(proc, addr, (char *)&si, sizeof(si)) != 0)
		panic("prt failed in gnp");

	offset = 0;

	switch (si.fmt1.op) {
	case 0:
		/*  Conditional annulled branches land up one instruction
		 *  further on than normal.
		 */
		if (si.fmt2.a && si.fmt2.cond != CC_A)
			offset = 4;

		switch ((btype_t)si.fmt2.op2) {
		case CBCCC:
			panic("cbcc NYI in gnp");
			break;
		case FBFCC:
			psr = proc_getreg(proc, REG_FP_CONDITION_CODES);
			if (fp_cond_holds(psr, (fp_cond_t)si.fmt2.cond))
				return addr +
					    (sign_ext(si.fmt2.disp22, 22) << 2);
			break;
		case BICC:
			psr = proc_getreg(proc, REG_CONDITION_CODES);
			if (cond_holds(psr, (cond_t)si.fmt2.cond))
				return addr +
					    (sign_ext(si.fmt2.disp22, 22) << 2);
			break;
		default:
			break;
		}
		break;
	case 1:
		return addr + (si.fmt1.disp30 << 2);
	case 2:
		if (si.fmt3i.op3 == JMPL) {
			int val;

			val = proc_getreg(proc, (int)si.fmt3i.rs1);
			if (si.fmt3i.i)
				val += sign_ext(si.fmt3i.simm13, 13);
			else
				val += proc_getreg(proc, (int)si.fmt3r.rs2);
			
			return val;
		}
		break;
	case 3:
		break;
	}

	return addr + offset + 4;
}

jump_t *
get_jumps(addr, ctext, len, want_calls, want_branches)
taddr_t addr;
const char *ctext;
int len, want_calls, want_branches;
{
	static jump_t *jtab;
	static int jtab_size = 0;
	jumptype_t jtype;
	sparc_inst_t *p_si, *lim, si;
	bool unconditional;
	int njumps;

	if (jtab_size == 0) {
		jtab_size = 16;
		jtab = (jump_t *)e_malloc((jtab_size + 1) * sizeof(jump_t));
	}

	if (((int)ctext & 03) != 0 || (len & 03) != 0)
		panic("align/len botch in gj");
	p_si = (sparc_inst_t *)ctext;
	lim = (sparc_inst_t *)(ctext + len);

	njumps = 0;

	for (; p_si < lim; ++p_si, addr += 4) {
		taddr_t jdest;

		si = *p_si;
		switch (si.fmt1.op) {
		case 0:
			switch ((btype_t)si.fmt2.op2) {
			case CBCCC:
			case FBFCC:
			case BICC:
				jdest = addr + (sign_ext(si.fmt2.disp22, 22) << 2);
				unconditional = FALSE;
				break;
			default:
				continue;
			}
			jtype = JT_BRANCH;
			break;
		case 1:
			jdest = addr + (si.fmt1.disp30 << 2);
			unconditional = TRUE;
			jtype = JT_CALL;
			break;
		case 2:
			if (si.fmt3i.op3 == JMPL) {
				jdest = 0;
				jtype = (si.fmt3i.rd != 0) ? JT_CALL : JT_BRANCH;
				unconditional = TRUE;
			}
			else
				continue;
			break;
		default:
			continue;
		}

		if ((want_calls && jtype == JT_CALL) ||
		    			   (want_branches && jtype == JT_BRANCH)) {
			if (njumps >= jtab_size) {
				jtab_size *= 2;
				jtab = (jump_t *)e_realloc((char *)jtab,
						  (jtab_size + 1) * sizeof(jump_t));
			}
			jtab[njumps].ju_addr = addr;
			jtab[njumps].ju_type = jtype;
			jtab[njumps].ju_dstaddr = jdest;
			jtab[njumps].ju_unconditional = unconditional;
			++njumps;
		}
	}
	jtab[njumps].ju_type = JT_END;
	return jtab;
}
#endif /* ARCH_SUN4 */
