/* as_clipper.c - find the jumps in Clipper machine code */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_as_clipper_c_sccsid[] = "@(#)as_clipper.c	1.9 20/5/92 (UKC)";

#include <mtrprog/ifdefs.h>

#ifdef ARCH_CLIPPER
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include <local/ukcprog.h>
#include "ups.h"
#include "as.h"
#include "debug.h"


typedef unsigned short iword_t;

typedef enum optypeen {
	NOTIMP,		/* unimplmented instruction */
	BYTE_OP,	/* second byte is literal argument */
	REG,		/* third and fourth nybbles are integer registers */
	REV_REG,	/* fourth and third nybbles are integer registers */
	SREG,		/* third and fourth nybbles are float registers */
	DREG,		/* third and fourth nybbles are double registers */
	ONEREG,		/* fourth nybble is integer register */
	XXREG,		/* regs from name, e.g. movpw -> mov px,wy */
	REV_XXREG,	/* as XXREG, but regnos in the opposite order */
	IMM,		/* 16 or 32 bit immediate */
	IMMSHORT,	/* 16 bit immediate */
	QUICK,		/* third nybble is literal, fourth is integer reg */
	ADDR,		/* address */
	TEXT_ADDR,	/* text address */
	BRANCH_ADDR,	/* text address, but get branch opcode first */
	FBRANCH_ADDR,	/* like BRANCH_ADDR, but for bf* */
	MACRO		/* macro */
} optype_t;
	
typedef struct opdescst {
	const char *od_opname;
	optype_t od_optype;
} opdesc_t;

static void disassemble_macro PROTO((char *buf, unsigned word1, unsigned word2));

opdesc_t Opdescs[256] = {
	/* 0000 0000 */	"noop",		BYTE_OP,
	/* 0000 0001 */	NULL,		NOTIMP,
	/* 0000 0010 */	NULL,		NOTIMP,
	/* 0000 0011 */	NULL,		NOTIMP,
	/* 0000 0100 */	NULL,		NOTIMP,
	/* 0000 0101 */	NULL,		NOTIMP,
	/* 0000 0110 */	NULL,		NOTIMP,
	/* 0000 0111 */	NULL,		NOTIMP,
	/* 0000 1000 */	NULL,		NOTIMP,
	/* 0000 1001 */	NULL,		NOTIMP,
	/* 0000 1010 */	NULL,		NOTIMP,
	/* 0000 1011 */	NULL,		NOTIMP,
	/* 0000 1100 */	NULL,		NOTIMP,
	/* 0000 1101 */	NULL,		NOTIMP,
	/* 0000 1110 */	NULL,		NOTIMP,
	/* 0000 1111 */	NULL,		NOTIMP,

	/* 0001 0000 */	"movwp",	REV_XXREG,
	/* 0001 0001 */	"movpw",	XXREG,
	/* 0001 0010 */	"calls",	BYTE_OP,
#define OC_RET	0x13
	/* 0001 0011 */	"ret",		ONEREG,
	/* 0001 0100 */	"pushw",	REV_REG,
	/* 0001 0101 */	NULL,		NOTIMP,
	/* 0001 0110 */	"popw",		REG,
	/* 0001 0111 */	NULL,		NOTIMP,
	/* 0001 1000 */	NULL,		NOTIMP,
	/* 0001 1001 */	NULL,		NOTIMP,
	/* 0001 1010 */	NULL,		NOTIMP,
	/* 0001 1011 */	NULL,		NOTIMP,
	/* 0001 1100 */	NULL,		NOTIMP,
	/* 0001 1101 */	NULL,		NOTIMP,
	/* 0001 1110 */	NULL,		NOTIMP,
	/* 0001 1111 */	NULL,		NOTIMP,

	/* 0010 0000 */	"adds",		SREG,
	/* 0010 0001 */	"subs",		SREG,
	/* 0010 0010 */	"addd",		DREG,
	/* 0010 0011 */	"subd",		DREG,
	/* 0010 0100 */	"movs",		SREG,
	/* 0010 0101 */	"cmps",		SREG,
	/* 0010 0110 */	"movd",		DREG,
	/* 0010 0111 */	"cmpd",		DREG,
	/* 0010 1000 */	"muls",		SREG,
	/* 0010 1001 */	"divs",		SREG,
	/* 0010 1010 */	"muld",		DREG,
	/* 0010 1011 */	"divd",		DREG,
	/* 0010 1100 */	"movsw",	XXREG,
	/* 0010 1101 */	"movws",	XXREG,
	/* 0010 1110 */	"movdl",	XXREG,
	/* 0010 1111 */	"movld",	XXREG,

	/* 0011 0000 */	"shaw",		REG,
	/* 0011 0001 */	"shal",		REG,
	/* 0011 0010 */	"shlw",		REG,
	/* 0011 0011 */	"shll",		REG,
	/* 0011 0100 */	"rotw",		REG,
	/* 0011 0101 */	"rotl",		REG,
	/* 0011 0110 */	NULL,		NOTIMP,
	/* 0011 0111 */	NULL,		NOTIMP,
	/* 0011 1000 */	"shai",		IMMSHORT,
	/* 0011 1001 */	"shali",	IMMSHORT,
	/* 0011 1010 */	"shli",		IMMSHORT,
	/* 0011 1011 */	"shlli",	IMMSHORT,
	/* 0011 1100 */	"roti",		IMMSHORT,
	/* 0011 1101 */	"rotli",	IMMSHORT,
	/* 0011 1110 */	NULL,		NOTIMP,
	/* 0011 1111 */	NULL,		NOTIMP,

	/* 0100 0000 */	NULL,		NOTIMP,
	/* 0100 0001 */	NULL,		NOTIMP,
	/* 0100 0010 */	NULL,		NOTIMP,
	/* 0100 0011 */	NULL,		NOTIMP,
#define OC_CALL_REL	0x44
	/* 0100 0100 */	">call",	TEXT_ADDR,
#define OC_CALL		0x45
	/* 0100 0101 */	">call",	TEXT_ADDR,
	/* 0100 0110 */	NULL,		NOTIMP,
	/* 0100 0111 */	NULL,		NOTIMP,
#define OC_B_REL	0x48
	/* 0100 1000 */	"<b*>",		BRANCH_ADDR,
#define OC_B		0x49
	/* 0100 1001 */	"<b*>",		BRANCH_ADDR,
	/* 0100 1010 */	NULL,		NOTIMP,
	/* 0100 1011 */	NULL,		NOTIMP,
#define OC_BF_REL	0x4c
	/* 0100 1100 */	"<bf*>",	FBRANCH_ADDR,
#define OC_BF		0x4d
	/* 0100 1101 */	"<bf*>",	FBRANCH_ADDR,
	/* 0100 1110 */	NULL,		NOTIMP,
	/* 0100 1111 */	NULL,		NOTIMP,

	/* 0101 0000 */	NULL,		NOTIMP,
	/* 0101 0001 */	NULL,		NOTIMP,
	/* 0101 0010 */	NULL,		NOTIMP,
	/* 0101 0011 */	NULL,		NOTIMP,
	/* 0101 0100 */	NULL,		NOTIMP,
	/* 0101 0101 */	NULL,		NOTIMP,
	/* 0101 0110 */	NULL,		NOTIMP,
	/* 0101 0111 */	NULL,		NOTIMP,
	/* 0101 1000 */	NULL,		NOTIMP,
	/* 0101 1001 */	NULL,		NOTIMP,
	/* 0101 1010 */	NULL,		NOTIMP,
	/* 0101 1011 */	NULL,		NOTIMP,
	/* 0101 1100 */	NULL,		NOTIMP,
	/* 0101 1101 */	NULL,		NOTIMP,
	/* 0101 1110 */	NULL,		NOTIMP,
	/* 0101 1111 */	NULL,		NOTIMP,

	/* 0110 0000 */	"loadw",	ADDR,
	/* 0110 0001 */	"loadw",	ADDR,
	/* 0110 0010 */	"loada",	ADDR,
	/* 0110 0011 */	"loada",	ADDR,
	/* 0110 0100 */	"loads",	ADDR,
	/* 0110 0101 */	"loads",	ADDR,
	/* 0110 0110 */	"loadd",	ADDR,
	/* 0110 0111 */	"loadd",	ADDR,
	/* 0110 1000 */	"loadb",	ADDR,
	/* 0110 1001 */	"loadb",	ADDR,
	/* 0110 1010 */	"loadbu",	ADDR,
	/* 0110 1011 */	"loadbu",	ADDR,
	/* 0110 1100 */	"loadh",	ADDR,
	/* 0110 1101 */	"loadh",	ADDR,
	/* 0110 1110 */	"loadhu",	ADDR,
	/* 0110 1111 */	"loadhu",	ADDR,

	/* 0111 0000 */	">storw",	ADDR,
	/* 0111 0001 */	">storw",	ADDR,
	/* 0111 0010 */	"tsts",		ADDR,
	/* 0111 0011 */	NULL,		NOTIMP,
	/* 0111 0100 */	">stors",	ADDR,
	/* 0111 0101 */	">stors",	ADDR,
	/* 0111 0110 */	">stord",	ADDR,
	/* 0111 0111 */	">stord",	ADDR,
	/* 0111 1000 */	">storb",	ADDR,
	/* 0111 1001 */	">storb",	ADDR,
	/* 0111 1010 */	">storb",	ADDR,
	/* 0111 1011 */	">storbu",	ADDR,
	/* 0111 1100 */	">storh",	ADDR,
	/* 0111 1101 */	">storh",	ADDR,
	/* 0111 1110 */	">storhu",	ADDR,
	/* 0111 1111 */	">storhu",	ADDR,

	/* 1000 0000 */	"addw",		REG,
	/* 1000 0001 */	NULL,		NOTIMP,
	/* 1000 0010 */	"addq",		QUICK,
	/* 1000 0011 */	"addi",		IMM,
	/* 1000 0100 */	"movw",		REG,
	/* 1000 0101 */	NULL,		NOTIMP,
	/* 1000 0110 */	"loadq",	QUICK,
	/* 1000 0111 */	"loadi",	IMM,
	/* 1000 1000 */	"andw",		REG,
	/* 1000 1001 */	NULL,		NOTIMP,
	/* 1000 1010 */	NULL,		NOTIMP,
	/* 1000 1011 */	"andi",		IMM,
	/* 1000 1100 */	"orw",		REG,
	/* 1000 1101 */	NULL,		NOTIMP,
	/* 1000 1110 */	NULL,		NOTIMP,
	/* 1000 1111 */	"ori",		IMM,

	/* 1001 0000 */	"addwc",	REG,
	/* 1001 0001 */	"subwc",	REG,
	/* 1001 0010 */	NULL,		NOTIMP,
	/* 1001 0011 */	"negw",		REG,
	/* 1001 0100 */	NULL,		NOTIMP,
	/* 1001 0101 */	NULL,		NOTIMP,
	/* 1001 0110 */	NULL,		NOTIMP,
	/* 1001 0111 */	NULL,		NOTIMP,
	/* 1001 1000 */	"mulw",		REG,
	/* 1001 1001 */	"mulwx",	REG,
	/* 1001 1010 */	"mulwu",	REG,
	/* 1001 1011 */	"mulwux",	REG,
	/* 1001 1100 */	"divw",		REG,
	/* 1001 1101 */	"modw",		REG,
	/* 1001 1110 */	"divwu",	REG,
	/* 1001 1111 */	"modwu",	REG,

	/* 1010 0000 */	"subw",		REG,
	/* 1010 0001 */	NULL,		NOTIMP,
	/* 1010 0010 */	"subq",		QUICK,
	/* 1010 0011 */	"subi",		IMM,
	/* 1010 0100 */	"cmpw",		REG,
	/* 1010 0101 */	NULL,		NOTIMP,
	/* 1010 0110 */	"cmpq",		QUICK,
	/* 1010 0111 */	"cmpi",		IMM,
	/* 1010 1000 */	"xorw",		REG,
	/* 1010 1001 */	NULL,		NOTIMP,
	/* 1010 1010 */	NULL,		NOTIMP,
	/* 1010 1011 */	"xori",		IMM,
	/* 1010 1100 */	"notw",		REG,
	/* 1010 1101 */	NULL,		NOTIMP,
	/* 1010 1110 */	"notq",		QUICK,
	/* 1010 1111 */	NULL,		NOTIMP,

	/* 1011 0000 */	NULL,		NOTIMP,
	/* 1011 0001 */	NULL,		NOTIMP,
	/* 1011 0010 */	NULL,		NOTIMP,
	/* 1011 0011 */	NULL,		NOTIMP,
	/* 1011 0100 */	"<macro>",	MACRO,
	/* 1011 0101 */	NULL,		NOTIMP,
	/* 1011 0110 */	"<pmac>",	MACRO,
	/* 1011 0111 */	NULL,		NOTIMP,
	/* 1011 1000 */	NULL,		NOTIMP,
	/* 1011 1001 */	NULL,		NOTIMP,
	/* 1011 1010 */	NULL,		NOTIMP,
	/* 1011 1011 */	NULL,		NOTIMP,
	/* 1011 1100 */	NULL,		NOTIMP,
	/* 1011 1101 */	NULL,		NOTIMP,
	/* 1011 1110 */	NULL,		NOTIMP,
	/* 1011 1111 */	NULL,		NOTIMP,
};

jump_t *
get_jumps(addr, ctext, len, want_calls, want_branches)
taddr_t addr;
const char *ctext;
int len, want_calls, want_branches;
{
	static jump_t *jtab;
	static int jtab_size = 0;
	const iword_t *text, *lim, *old_text;
	jumptype_t jtype;
	int njumps;

	if (jtab_size == 0) {
		jtab_size = 16;
		jtab = (jump_t *)e_malloc((jtab_size + 1) * sizeof(jump_t));
	}

	if (((int)ctext & 1) != 0 || (len & 1) != 0)
		panic("align/len botch in gj");
	text = (iword_t *)ctext;
	lim = (iword_t *)(ctext + len);

	njumps = 0;
	for (; text <= lim; addr += (char *)text - (char *)old_text) {
		opdesc_t *od;
		unsigned inst;
		int opcode, reg1;
		taddr_t operand;

		old_text = text;
		inst = *text++;
		reg1 = (inst >> 4) & 0xf;
		opcode = inst >> 8;
		od = &Opdescs[opcode];

		operand = 0;
		switch (od->od_optype) {
		case IMM: case IMMSHORT:
			text += (reg1 == 11) ? 1 : 2;
			break;
		case MACRO:
			++text;
			break;
		case FBRANCH_ADDR: case BRANCH_ADDR: case ADDR: case TEXT_ADDR:
			if ((inst & 0x100) == 0)
				break;
			switch (reg1) {
			case 1:
				operand = addr + (text[0] | text[1] << 16);
				text += 2;
				break;
			case 3:
				operand = text[0] | (text[1] << 16);
				text += 2;
				break;
			case 6:
				text += 3;
				break;
			case 9:
				operand = addr + (short)*text++;
				break;
			case 10:
				++text;
				break;
			case 11:
				operand = (short)*text++;
				break;
			case 13:
			case 14:
				++text;
				break;
			default:
				break;
			}
			break;
		default:
			break;
		}

		switch (opcode) {
		case OC_RET: case OC_B_REL: case OC_BF_REL: case OC_B: case OC_BF:
			jtype = JT_BRANCH;
			break;
		case OC_CALL: case OC_CALL_REL:
			jtype = JT_CALL;
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
			jtab[njumps].ju_dstaddr = operand;
			jtab[njumps].ju_unconditional = FALSE;	/* don't know */
			++njumps;
		}
	}

	jtab[njumps].ju_type = JT_END;
	return jtab;
}

static const char *
addr_to_funcname(addr, operand)
taddr_t addr, operand;
{
	const char *res;

	if (Debug_flags & DBFLAG_DBXASM) {
		static char buf[20];

		if (addr != 0)
			sprintf(buf, "%d(pc)", operand);
		else
			sprintf(buf, "%d", operand);
		return buf;
	}
	else
		res = addr_to_func_and_offset(addr + operand, TRUE);
	
	return res;
}

const char *
disassemble_one_instruction(addr, ctext, p_buf)
taddr_t addr;
const char *ctext, **p_buf;
{
	static const char *branch_names[16] = {
		"b",	"bclt",	"bcle",	"bceq",	"bcgt",	"bcge",	"bcne",	"bcltu",
		"bcleu","bcgtu","bcgeu","bv",	"bnv",	"bn",	"bnn",	"bfn"
	};
	static const char *regnames[16] = {
		"r0",	"r1",	"r2",	"r3",	"r4",	"r5",	"r6",	"r7",
		"r8",	"r9",	"r10",	"r11",	"r12",	"r13",	"r14",	"r15"
	};
	opdesc_t *od;
	const char *opname, *regname;
	const iword_t *text;
	register unsigned inst, lowbyte, highbyte;
	int ch1, ch2, reg1, reg2, len, operand, optype, endch;
	bool dbx;
	static char buf[80], abuf[50];

	text = (const iword_t *)ctext;

	dbx = (Debug_flags & DBFLAG_DBXASM) != 0;

	inst = *text++;
	highbyte = inst >> 8;
	lowbyte = inst & 0xff;
	reg1 = lowbyte >> 4;
	reg2 = lowbyte & 0xf;

	od = &Opdescs[inst >> 8];
	opname = od->od_opname;
	optype = od->od_optype;
	if (opname == NULL && od->od_optype != NOTIMP)
		panic("opname botch in doa");

	switch (optype) {
	case NOTIMP:
		if (dbx) {
			/*  Don't ask.
			 */
			if ((inst & 0xf) == 0) {
				sprintf(buf, ".word 0x%x%02x",
						    (inst & 0xf0) >> 4, inst >> 8);
			}
			else {
				sprintf(buf, ".word 0x%x%x%02x",
					inst & 0xf, (inst & 0xf0) >> 4, inst >> 8);
			}
		}
		else
			sprintf(buf, "<unimplemented opcode 0x%04x>", inst);
		break;
	case BYTE_OP:
		if (dbx)
			operand = (lowbyte >> 4) | ((lowbyte & 0xf) << 4);
		else
			operand = lowbyte;
		sprintf(buf, "%s\t%d", opname, operand);
		break;
	case REV_REG:
		sprintf(buf, "%s\t%s, %s", opname, regnames[reg2], regnames[reg1]);
		break;
	case REG:
		sprintf(buf, "%s\t%s, %s", opname, regnames[reg1], regnames[reg2]);
		break;
	case SREG:
	case DREG:
		sprintf(buf, "%s\tf%d, f%d", opname, reg1, reg2);
		break;
	case ONEREG:
		sprintf(buf, "%s\t%s", opname, regnames[reg2]);
		break;
	case REV_XXREG:
		reg1 = lowbyte & 0xf;
		reg2 = lowbyte >> 4;
		/*  FALL THROUGH */
	case XXREG:
		if ((len = strlen(opname)) < 2)
			panic("len botch in doa");
		ch1 = opname[len - 2];
		ch2 = opname[len - 1];

		if (ch1 == 'w' || ch1 == 'l')
			ch1 = 'r';
		if (ch2 == 'w' || ch2 == 'l')
			ch2 = 'r';
		if (dbx) {
			if (ch1 == 's' || ch1 == 'd')
				ch1 = 'f';
			if (ch2 == 's' || ch2 == 'd')
				ch2 = 'f';
		}
		if (ch1 == 'p' && reg1 == 0)
			sprintf(buf, "%s\tpsw, %c%d", opname, ch2, reg2);
		else if (ch2 == 'p' && reg2 == 0)
			sprintf(buf, "%s\t%c%d, psw", opname, ch1, reg1);
		else
			sprintf(buf, "%s\t%c%d, %c%d", opname, ch1, reg1, ch2, reg2);
		break;
	case IMM:
		if (reg1 == 11)
			operand = (short)*text++;
		else if (reg1 == 3) {
			operand = *text | (text[1] << 16);
			text += 2;
		}
		else {
			sprintf(buf, "%s\t%d<illegal mode %d>, %s", opname,
					      (short)*text++, reg1, regnames[reg2]);
			break;
		}
		sprintf(buf, "%s\t$%d, %s", opname, operand, regnames[reg2]);
		break;
	case IMMSHORT:
		if (reg1 != 11) {
			sprintf(buf, "%s\t%d<illegal mode %d>, %s", opname,
					      (short)*text++, reg1, regnames[reg2]);
			break;
		}
		sprintf(buf, "%s\t$%d, %s", opname, (short)*text++, regnames[reg2]);
		break;
	case QUICK:
		sprintf(buf, "%s\t$%d, %s", opname, reg1, regnames[reg2]);
		break;
	case FBRANCH_ADDR:
		if (reg2 < 2)
			opname = (reg2 != 0) ? "bfbad" : "bfany";
		else {
			static char fbuf[20];

			sprintf(fbuf, "bf<cond %d>", reg2);
			opname = fbuf;
		}
		optype = TEXT_ADDR;
		goto text_addr;
	case BRANCH_ADDR:
		opname = branch_names[reg2];
		optype = TEXT_ADDR;
		goto text_addr;
	case ADDR:
	case TEXT_ADDR:
text_addr:
		if ((inst & 0x100) == 0)
			sprintf(abuf, "(%s)", regnames[reg1]);
		else {
			switch (reg1) {
			case 10:
				inst = *text++;
				sprintf(abuf, "%d(%s)", (short)inst >> 4,
								regnames[reg2]);
				reg2 = inst & 0xf;
				break;
			case 6:
				sprintf(abuf, "%d(%s)", text[1] | (text[2] << 16),
								    regnames[reg2]);
				reg2 = *text & 0xf;
				text += 3;
				break;
			case 11:
			case 3:
				if (reg1 == 11)
					operand = (short)*text++;
				else {
					operand = text[0] | (text[1] << 16);
					text += 2;
				}
				if (optype == TEXT_ADDR)
					strcpy(abuf, addr_to_funcname(0, operand));
				else
					sprintf(abuf, dbx ? "%d" : "0x%x", operand);
				break;
			case 9:
				strcpy(abuf, addr_to_funcname(addr, (short)*text++));
				break;
			case 1:
				operand = (text[0] | text[1] << 16);
				text += 2;
				if (optype == TEXT_ADDR) {
					strcpy(abuf, addr_to_funcname(addr,operand));
				}
				else {
					if (dbx)
						sprintf(abuf, "%d(pc)", operand);
					else
						sprintf(abuf, "0x%x", addr+operand);
				}
				break;
			case 14:
				inst = *text++;
				sprintf(abuf, "[%s](%s)",
							regnames[(inst >> 4) & 0xf],
							regnames[reg2]);
				reg2 = inst & 0xf;
				break;
			case 13:
				inst = *text++;
				sprintf(abuf, "[%s](pc)",
							regnames[(inst >> 4) & 0xf]);
				reg2 = inst & 0xf;
				break;
			default:
				sprintf(abuf, "<bad addrmode %d>", reg1);
				break;
			}
		}
		switch (od->od_optype) {
		case BRANCH_ADDR:
		case FBRANCH_ADDR:
			sprintf(buf, "%s\t%s", opname, abuf);
			break;
		case ADDR:
		case TEXT_ADDR:
			endch = opname[strlen(opname) - 1];
			if (endch == 'd' || endch == 's') {
				static char rbuf[4];	/* 'r', '1', '2', '\0' */

				sprintf(rbuf, "%c%d", dbx ? 'f' : endch, reg2);
				regname = rbuf;
			}
			else
				regname = regnames[reg2];

			if (*opname == '>')
				sprintf(buf, "%s\t%s, %s", ++opname, regname, abuf);
			else
				sprintf(buf, "%s\t%s, %s", opname, abuf, regname);
			break;
		default:
			panic("ot botch in doa");
		}
		break;
	case MACRO:
		disassemble_macro(buf, inst, *text++);
		break;
	default:
		panic("unknown optype in doa");
		break;
	}

	*p_buf = buf;
	return (char *)text;
}

static void
disassemble_macro(buf, word1, word2)
char *buf;
unsigned word1, word2;
{
	static const char *cnvs[16] = {
		"sw",	"rsw",	"tsw",	"ws",	"dw",	"rdw",	"tdw",	"wd",
		"sd",	"ds",	NULL,	NULL,	NULL,	NULL,	NULL,	NULL
	};
	int lowbyte, reg1, reg2, dch, sch;
	bool dbx;

	dbx = (Debug_flags & DBFLAG_DBXASM) != 0;

	if (dbx)
		dch = sch = 'f';
	else {
		dch = 'd';
		sch = 'f';
	}

	lowbyte = word1 & 0xff;
	reg1 = (word2 >> 4) & 0xf;
	reg2 = word2 & 0xf;

	if ((lowbyte >> 4) == 3 && cnvs[lowbyte & 0xf] != NULL) {
		const char *s, *str;
		int ch1, ch2;

		str = cnvs[lowbyte & 0xf];

		s = str + (strlen(str) - 2);
		ch1 = s[0];
		ch2 = s[1];

		if (dbx) {
			if (ch1 == 'd' || ch1 == 's')
				ch1 = 'f';
			if (ch2 == 'd' || ch2 == 's')
				ch2 = 'f';
		}
		if (ch1 == 'w')
			ch1 = 'r';
		if (ch2 == 'w')
			ch2 = 'r';

		sprintf(buf, "cnv%s\t%c%d, %c%d", str, ch1, reg1, ch2, reg2);
		return;
	}
	if ((word1 & ~0x1f) == 0xb400) {
		sprintf(buf, "%sw%d", (word1 & 0x10) ? "rest" : "save", word1 & 0xf);
		return;
	}

	switch (lowbyte) {
	case 0x00:
		sprintf(buf, "movus\tr%d, r%d", reg1, reg2);
		break;
	case 0x01:
		sprintf(buf, "movsu\tr%d, r%d", reg1, reg2);
		break;
	case 0x02:
		sprintf(buf, "saveur\tr%d", reg1);
		break;
	case 0x03:
		sprintf(buf, "restur\tr%d", reg1);
		break;
	case 0x04:
		sprintf(buf, "reti\tr%d", reg1);
		break;
	case 0x05:
		strcpy(buf, "wait");
		break;
	case 0x0e:
		strcpy(buf, "initc");
		break;
	case 0x0d:
		strcpy(buf, "movc");
		break;
	case 0x0f:
		strcpy(buf, "cmpc");
		break;
	case 0x20: case 0x21: case 0x22: case 0x23:
	case 0x24: case 0x25: case 0x26: case 0x27:
		sprintf(buf, "saved%d", lowbyte & 0x7);
		break;
	case 0x28: case 0x29: case 0x2a: case 0x2b:
	case 0x2c: case 0x2d: case 0x2e: case 0x2f:
		sprintf(buf, "restd%d", lowbyte & 0x7);
		break;
	case 0x3a:
		sprintf(buf, "negs %c%d, %c%d", sch, reg1, sch, reg2);
		break;
	case 0x3b:
		sprintf(buf, "negd %c%d, %c%d", dch, reg1, dch, reg2);
		break;
	case 0x3c:
		sprintf(buf, "scalbs\tr%d, %c%d", reg1, sch, reg2);
		break;
	case 0x3d:
		sprintf(buf, "scalbd\tr%d, %c%d", reg1, dch, reg2);
		break;
	case 0x3e:
		strcpy(buf, "trapfn");
		break;
	case 0x3f:
		sprintf(buf, "loadfs\tw%d,r%d", reg1, reg2);
		break;
	default:
		sprintf(buf, "<unimplemented macro instruction %04x:%04x>",
								word1, word2);
		break;
	}
}
#endif /* ARCH_CLIPPER */
