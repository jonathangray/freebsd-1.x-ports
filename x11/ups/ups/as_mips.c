/* as_mips.c - find the jumps in MIPS machine code */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_as_mips_c_sccsid[] = "@(#)as_mips.c	1.8 20/5/92 (UKC)";

#include <mtrprog/ifdefs.h>

#ifdef ARCH_MIPS
#include <machine/inst.h>

#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include <local/ukcprog.h>
#include "ups.h"
#include "as.h"


static const char *regname PROTO((int regno));

static const char *
regname(regno)
int regno;
{
	static const char *regnames[] = {
		"r0",	"r1",	"r2",	"r3",	"r4",	"r5",	"r6",	"r7",
		"r8",	"r9",	"r10",	"r11",	"r12",	"r13",	"r14",	"r15",
		"r16",	"r17",	"r18",	"r19",	"r20",	"r21",	"r22",	"r23",
		"r24",	"r25",	"r26",	"r27",	"r28",	"sp",	"r30",	"r31"
	};

	if (regno < 0 || regno >= 32)
		panic("bad regno in rn");
	return regnames[regno];
}

const char *
disassemble_one_instruction(addr, ctext, p_buf)
taddr_t addr;
const char *ctext, **p_buf;
{
	static const char *opnames[64] = {
		NULL,	NULL,	"j",	"jal",	"beq",	"bne",	"blez",	"bgtz",
		"addi",	"addiu","slti",	"sltiu","andi",	"ori",	"xori",	"lui",
		"cop0",	"cop1",	"cop2",	"cop3",	NULL,	NULL,	NULL,	NULL,
		NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
		"lb",	"lh",	"lwl",	"lw",	"lbu",	"lhu",	"lwr",	NULL,
		"sb",	"sh",	"swl",	"sw",	NULL,	NULL,	"swr",	NULL,
		"lwc0",	"lwc1",	"lwc2",	"lwc3",	NULL,	NULL,	NULL,	NULL,
		"swc0",	"swc1",	"swc2",	"swc3",	NULL,	NULL,	NULL,	NULL,
	};
	static const char *specnames[64] = {
		"sll",	NULL,	"srl",	"sra",	"sllv",	NULL,	"srlv",	"srav",
		"jr",	"jalr",	NULL,	NULL,	"syscall","break",NULL,	NULL,
		"mfhi",	"mthi",	"mflo",	"mtlo",	NULL,	NULL,	NULL,	NULL,
		"mult",	"multu","div",	"divu",	NULL,	NULL,	NULL,	NULL,
		"add",	"addu",	"sub",	"subu",	"and",	"or",	"xor",	"nor",
		NULL,	NULL,	"slt",	"sltu",	NULL,	NULL,	NULL,	NULL,	
		NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
		NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
	};
	static const char *cop1_opnames[64] = {
		"fadd",	"fsub",	"fmul",	"fdiv",	NULL, "fabs",	"fmov",	"fneg",
		NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
		NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
		NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
		"cvt.d","cvt.s",NULL,	NULL,	"cvt.w",NULL,	NULL,	NULL,
		NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
		"c.f",	"c.un",	"c.eq",	"c.ueq","c.olt","c.ult","c.ole","c.ule",
		"c.sf","c.ngle","c.seq","c.ngl","c.lt",	"c.nge","c.le",	"c.ngt"
	};
	static const char *cop1_fmts[16] = {
		"s",	"d", 	"<2>",	"<3>",	"<4>",	"<5>",	"<6>",	"<7>",
		"<8>",	"<9>", 	"<10>",	"<11>",	"<12>",	"<13>",	"<14>",	"<15>"
	};
	union mips_instruction inst;
	static char buf[100];
	taddr_t jdest;
	const char *opname, *fmt;

	inst = *(union mips_instruction *)ctext;

	opname = opnames[inst.j_format.opcode];
	switch (inst.j_format.opcode) {
	case spec_op:
		opname = specnames[inst.r_format.func];
		switch (inst.r_format.func) {
		case sll_op: case srl_op: case sra_op:
			sprintf(buf, "%s\t%s,%s,%d", opname,
						regname(inst.r_format.rd),
						regname(inst.r_format.rt),
						inst.r_format.re);
			break;
		case sllv_op: case srlv_op: case srav_op:
			sprintf(buf, "%s\t%s,%s,%s", opname,
						regname(inst.r_format.rd),
						regname(inst.r_format.rt),
						regname(inst.r_format.rs));
			break;
		case jr_op:
			sprintf(buf, "%s\t%s", opname, regname(inst.r_format.rs));
			break;
		case jalr_op:
			sprintf(buf, "%s\t%s,%s",
					opname,
					regname(inst.r_format.rd),
					regname(inst.r_format.rs));
			break;
		case syscall_op:
			strcpy(buf, opname);
			break;
		case break_op:
			sprintf(buf, "%s\t%d", opname, (inst.word >> 6) & 0xfffff);
			break;
		case mfhi_op: case mflo_op:
			sprintf(buf, "%s\t%s", opname, regname(inst.r_format.rd));
			break;
		case mult_op: case multu_op: case div_op: case divu_op:
			sprintf(buf, "%s\t%s,%s", opname,
						regname(inst.r_format.rs),
						regname(inst.r_format.rt));
			break;
		case mthi_op: case mtlo_op:
			sprintf(buf, "%s\t%s", opname, regname(inst.r_format.rs));
			break;
		case add_op: case addu_op: case sub_op: case subu_op:
		case and_op: case or_op: case xor_op: case nor_op:
		case slt_op: case sltu_op:
			sprintf(buf, "%s\t%s,%s,%s", opname,
						regname(inst.r_format.rd),
						regname(inst.r_format.rs),
						regname(inst.r_format.rt));
			break;
		default:
			if (opname != NULL) {
				sprintf(buf, "%s\t<rs=%d,rd=%d,re=%d>",
						opname,
						inst.r_format.rs,
						inst.r_format.rd,
						inst.r_format.re);
			}
			else {
				sprintf(buf, "<unimplemented special %d (0x%x)>",
						inst.r_format.func,
						inst.word);
				opname = "was_null";	/* see end of func */
			}
			break;
		}
		break;
	case bcond_op:
		jdest = addr + 4 + (inst.i_format.simmediate << 2);
		switch (inst.i_format.rt) {
			case bltz_op:	opname = "bltz";	break;
			case bgez_op:	opname = "bgez";	break;
			case bltzal_op:	opname = "bltz";	break;
			case bgezal_op:	opname = "bltz";	break;
			default:	opname = "<unknown>";	break;
		}
		sprintf(buf, "%s\t$%d,%s", opname, regname(inst.i_format.rs),
					addr_to_func_and_offset(jdest, TRUE));
		break;
	case j_op:
	case jal_op:
		jdest = (addr & (3 << 30)) | (inst.j_format.target << 2);
		sprintf(buf, "%s\t%s", opname, addr_to_func_and_offset(jdest, TRUE));
		break;
	case beq_op:
	case bne_op:
		jdest = addr + 4 + (inst.i_format.simmediate << 2);
		sprintf(buf, "%s\t%s,%s,%s", opname,
					regname(inst.i_format.rs),
					regname(inst.i_format.rt),
					addr_to_func_and_offset(jdest, TRUE));
		break;
	case blez_op:
	case bgtz_op:
		jdest = addr + 4 + (inst.i_format.simmediate << 2);
		sprintf(buf, "%s\t%s", opname, addr_to_func_and_offset(jdest, TRUE));
		break;
	case addi_op:
	case slti_op:
		sprintf(buf, "%s\t%s,%s,%d", opname,
					regname(inst.i_format.rt),
					regname(inst.i_format.rs),
					inst.i_format.simmediate);
	case ori_op:	case andi_op:	case xori_op:
	case sltiu_op:	case addiu_op:	case lui_op:
		sprintf(buf, "%s\t%s,%s,%d", opname,
					regname(inst.u_format.rt),
					regname(inst.u_format.rs),
					inst.u_format.uimmediate);
		break;
	case cop0_op:
		switch (inst.word & 0x1f) {
			case tlbr_op:	opname = "tlbr";	break;
			case tlbwi_op:	opname = "tlbwi";	break;
			case tlbwr_op:	opname = "tlbwr";	break;
			case tlbp_op:	opname = "tlbp";	break;
			case rfe_op:	opname = "rfe";		break;
			default:	opname = NULL;		break;
		}
		if (opname != 0)
			strcpy(buf, opname);
		else {
			sprintf(buf, "Unknown cop0 opcode %d>", inst.word & 0x1f);
			opname = "was_null";
		}
		break;
	case cop1_op:
#define ft rt
#define fs rd
#define fd re
		if ((inst.word & (1 << 22)) == 0) {
			sprintf(buf, "<cop1 op %x>", inst.word);
			break;
		}
		opname = cop1_opnames[inst.f_format.func];
		fmt = cop1_fmts[inst.f_format.fmt];
		switch (inst.f_format.func) {
		case fabs_op:
			sprintf(buf, "%s.%s f%d,f%d", opname, fmt,
							inst.f_format.fd,
							inst.f_format.fs);
			break;
		case fadd_op: case fsub_op: case fmpy_op: case fdiv_op:
			sprintf(buf, "%s.%s\tf%d,f%d,f%d", opname, fmt,
							inst.f_format.fd,
							inst.f_format.fs,
							inst.f_format.ft);
			break;
		default:
			if (opname != NULL) {
				sprintf(buf, "%s.%s\tf%d,f%d", opname, fmt,
							inst.f_format.fs,
							inst.f_format.ft);
			}
			else {
				sprintf(buf, "<unknown cop1 op %d>",
							inst.f_format.func);
				opname = "was_null";
			}
		}
		break;
	case lb_op: case lh_op: case lwl_op: case lw_op:
	case lbu_op: case lhu_op: case lwr_op:
	case sb_op: case sh_op: case swl_op: case sw_op: case swr_op:
		sprintf(buf, "%s\t%s,%d(%s)", opname,
					regname(inst.i_format.rt),
					inst.i_format.simmediate,
					regname(inst.i_format.rs));
		break;
	case lwc0_op: case lwc1_op: case lwc2_op: case lwc3_op:
	case swc0_op: case swc1_op: case swc2_op: case swc3_op:
		sprintf(buf, "%s\tf%d,%d(%s)", opname,
					inst.i_format.rt,
					inst.i_format.simmediate,
					regname(inst.i_format.rs));
		break;
	default:
		sprintf(buf, "<unknown opcode 0x%x>", inst.word);
		opname = "was_null";
		break;
	}

	if (opname == NULL)
		panic("bad opname in doa");
	if (strlen(buf) >= sizeof(buf))
		panic("buffer overflow in doa");
	
	*p_buf = buf;
	return ctext + 4;
}

jump_t *
get_jumps(addr, ctext, len, want_calls, want_branches)
taddr_t addr;
const char *ctext;
int len, want_calls, want_branches;
{
#define TARGET_MASK	((1 << 26) - 1)
	static jump_t *jtab;
	static int jtab_size = 0;
	jumptype_t jtype;
	const int *lim, *text;
	int njumps;

	if (jtab_size == 0) {
		jtab_size = 16;
		jtab = (jump_t *)e_malloc((jtab_size + 1) * sizeof(jump_t));
	}

	if (((int)ctext & 03) != 0 || (len & 03) != 0)
		panic("align/len botch in gj");
	text = (int *)ctext;
	lim = (int *)(ctext + len);

	njumps = 0;

	for (; text < lim; ++text) {
		int inst;
		taddr_t jdest;

		inst = *text;
		switch (inst >> 26) {
		case j_op:
			jdest = (addr & (3 << 30)) | ((inst & TARGET_MASK) << 2);
			jtype = JT_BRANCH;
			break;
		case jal_op:
			jdest = (addr & (3 << 30)) | ((inst & TARGET_MASK) << 2);
			jtype = JT_CALL;
			break;
		case beq_op:
		case bne_op:
		case blez_op:
		case bgtz_op:
		case bcond_op:
			jdest = addr + 4 + ((inst & 0xffff) << 2);
			jtype = JT_BRANCH;
			break;
		case cop0_op:
		case cop1_op:
		case cop2_op:
		case cop3_op:
			if ((inst & 0x03fe0000) != 0x01000000)
				continue;
			jdest = addr + 4 + ((inst & 0xffff) << 2);
			jtype = JT_BRANCH;
			break;
		case spec_op:
			switch (inst & 0x3f) {
			case jr_op:
				jdest = 0;
				jtype = JT_BRANCH;
				break;
			case jalr_op:
				jdest = 0;
				jtype = JT_CALL;
				break;
			default:
				continue;
			}
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
			jtab[njumps].ju_addr = addr + ((char *)text - ctext);
			jtab[njumps].ju_type = jtype;
			jtab[njumps].ju_dstaddr = jdest;
			jtab[njumps].ju_unconditional = FALSE;	/* don't know */
			++njumps;
		}
	}
	jtab[njumps].ju_type = JT_END;
	return jtab;
}
#endif /* ARCH_MIPS */
