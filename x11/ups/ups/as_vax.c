/* as_vax.c - disassemble VAX machine code */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_as_vax_c_sccsid[] = "@(#)as_vax.c	1.17 29/6/92 (UKC)";

#include <mtrprog/ifdefs.h>

#ifdef ARCH_VAX
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include <local/ukcprog.h>
#include "ups.h"
#include "as.h"


static const char *disassemble_one_inst_and_get_operand PROTO((taddr_t addr,
			const char *text, const char **p_buf,
			int *p_val_is_constant, int *p_val));
static const char *address_mode PROTO((taddr_t addr, const char *text, char *buf,
				       int access_type, int optype,
				       int *p_val_is_constant, int *p_val));

/*  Mask to get the instruction type (call, branch, other)
 */
#define ITMASK	 0x3

#define BRA	 0x0	/* branch type instruction */
#define CALL	 0x1	/* subroutine call type instruction */
#define RET	 0x2	/* return from subroutine type instruction */
#define OTHER	 0x3	/* all other instructions */

/*  Flags OR'ed into the type mask
 */
#define DBL	 0x4	/* BRA instruction has displ.b[bw] as last opcode */

typedef unsigned char itype_t;

/*  This table of VAX opcodes and operands is taken from the VAX11/780
 *  Architecture Handbook Vol 1 1977-78.  It is indexed by opcode.
 *
 *  There are a few differences between this table and the book:
 *
 *	All occurences of 'x' or 'y' as an operand type have been
 *	replaced by the actual operand type for the particular
 *	instance of the opcode (i.e. 'l' for MOVL, 'b' for MOVB etc).
 *
 *	There are two new access type letters, 'f' and 'F', which are used
 *	instead of 'a' when an address should be printed as a function name.
 *	'f' means that the address should be printed as a function name
 *	only if it exactly matches a known function address. 'F' means
 *	that the address should always be printed as a function name,
 *	with an offset if necessary.
 *
 *	The last operand of EMUL in the book was given as "prod.wg".
 *	This was assumed to be a misprint for "prod.wq".
 *
 *	The operands of the CASE instruction have a special marker, '*',
 *	before the '.' of the last operand to mark the CASE opcodes
 *	as special (they have a variable number of operands).
 *
 *  The operand names ("src", "dst" etc) are included for completeness -
 *  they are not currently used.
 */
static struct {
	itype_t ot_itype;
	const char *ot_opdesc;
} Optable[] = {
/*00*/	OTHER,		"halt",
/*01*/	OTHER,		"nop",
/*02*/	RET,		"rei",
/*03*/	OTHER,		"bpt",
/*04*/	RET,		"ret",
/*05*/	RET,		"rsb",
/*06*/	OTHER,		"ldpctx",
/*07*/	OTHER,		"svpctx",

/*08*/	OTHER,		"cvtps srclen.rw, srcaddr.ab, dstlen.rw, dstaddr.ab",
/*09*/	OTHER,		"cvtsp srclen.rw, srcaddr.ab, dstlen.rw, dstaddr.ab",
/*0a*/	OTHER,		"index subscript.rl, low.rl, high.rl, size.rl, indexin.rl, indexout.rl",
/*0b*/	OTHER,		"crc tbl.ab, inicrc.rl, strlen.rw, stream.ab, dst.wl",
/*0c*/	OTHER,		"prober mode.rb, len.rw, base.ab",
/*0d*/	OTHER,		"probew mode.rb, len.rw, base.ab",
/*0e*/	OTHER,		"insque entry.ab, pred.ab",
/*0f*/	OTHER,		"remque entry.ab, addr.wl",

/*10*/	BRA|DBL,	"bsbb displ.bb",
/*11*/	BRA|DBL,	"brb displ.bb",
/*12*/	BRA|DBL,	"bneq displ.bb",
/*13*/	BRA|DBL,	"beql displ.bb",
/*14*/	BRA|DBL,	"bgtr displ.bb",
/*15*/	BRA|DBL,	"bleq displ.bb",
/*16*/	CALL,		"jsb dst.Fb",
/*17*/	BRA,		"jmp dst.ab",

/*18*/	BRA|DBL,	"bgeq displ.bb",
/*19*/	BRA|DBL,	"blss displ.bb",
/*1a*/	BRA|DBL,	"bgtru displ.bb",
/*1b*/	BRA|DBL,	"blequ displ.bb",
/*1c*/	BRA|DBL,	"bvc displ.bb",
/*1d*/	BRA|DBL,	"bvs displ.bb",
/*1e*/	BRA|DBL,	"bcc displ.bb",
/*1f*/	BRA|DBL,	"bcs displ.bb",

/*20*/	OTHER,		"addp4 addlen.rw, addaddr.ab, sumlen.rw, sumaddr.ab",
/*21*/	OTHER,		"addp6 add1len.rw, add1addr.ab, add2len.rw, add2addr.ab, sumlen.rw, sumaddr.ab",
/*22*/	OTHER,		"subp4 sublen.rw, subaddr.ab, diflen.rw, difaddr.ab",
/*23*/	OTHER,		"subp6 sublen.rw, subaddr.ab, minlen.rw, minaddr.ab, diflen.rw, difaddr.ab",
/*24*/	OTHER,		"cvtpt srclen.rw, srcaddr.ab, tbladdr.ab, dstlen.rw, dstaddr.ab",
/*25*/	OTHER,		"mulp mulrlen.rw, mulraddr.ab, muldlen.rw, muladdr.ab, prodlen.rw, prodaddr.ab",
/*26*/	OTHER,		"cvttp srclen.rw, srcaddr.ab, tbladdr.ab, dstlen.rw, dstaddr.ab",
/*27*/	OTHER,		"divp divrlen.rw, divraddr.ab, divdlen.rw, divaddr.ab, quolen.rw, quoaddr.ab",

/*28*/	OTHER,		"movc3 len.rw, srcaddr.ab, dstaddr.ab",
/*29*/	OTHER,		"cmpc3 len.rw, src1addr.ab, src2addr.ab",
/*2a*/	OTHER,		"scanc len.rw, addr.ab, tbladdr.ab, mas.rb",
/*2b*/	OTHER,		"spanc len.rw, addr.ab, tbladdr.ab, mas.rb",
/*2c*/	OTHER,		"movc5 srclen.rw, srcaddr.ab, fill.rb, dstlen.rw, dstaddr.ab",
/*2d*/	OTHER,		"cmpc5 len.rw, src1addr.ab, fill.rb, src2len.rw, src2addr.ab",
/*2e*/	OTHER,		"movtc srclen.rw, srcaddr.ab, fill.rb, tbladdr.ab, dstlen.rw, dstaddr.ab",
/*2f*/	OTHER,		"movtuc srclen.rw, srcaddr.ab, esc.rb, tbladdr.ab, dstlen.rw, dstaddr.ab",

/*30*/	BRA|DBL,	"bsbw displ.bw",
/*31*/	BRA|DBL,	"brw displ.bw",
/*32*/	OTHER,		"cvtwl src.rw, dst.wl",
/*33*/	OTHER,		"cvtwb src.rw, dst.wl",
/*34*/	OTHER,		"movp len.rw, srcaddr.ab, dstaddr.ab",
/*35*/	OTHER,		"cmpp3 len.rw, src1addr.ab, src2addr.ab",
/*36*/	OTHER,		"cvtpl srclen.rw, srcaddr.ab, dst.wl",
/*37*/	OTHER,		"cmpp4 src1len.rw, src1addr.ab, src2len.rw, src2addr.ab",

/*38*/	OTHER,		"editpc srclen.rw, srcaddr.ab, pattern.ab, dstaddr.ab",
/*39*/	OTHER,		"matchc len1.rw, addr1.ab, len2.rw, addr2.ab",
/*3a*/	OTHER,		"locc char.ab, len.rw, addr.ab",
/*3b*/	OTHER,		"skpc char.ab, len.rw, addr.ab",
/*3c*/	OTHER,		"movzwl src.rw, dst.wl",
/*3d*/	OTHER,		"acbw limit.rw, add.rw, index.mw, displ.bw",
/*3e*/	OTHER,		"movaw src.aw, dst.wl",
/*3f*/	OTHER,		"pushaw src.aw",

/*40*/	OTHER,		"addf2 add.rf, sum.mf",
/*41*/	OTHER,		"addf3 add1.rf, add2.rf, sum.wf",
/*42*/	OTHER,		"subf2 sub.rf, dif.mf",
/*43*/	OTHER,		"subf3 sub.rf, min.rf, dif.wf",
/*44*/	OTHER,		"mulf2 mulr.rf, prod.mf",
/*45*/	OTHER,		"mulf3 mulr.rf, muld.rf, prod.wf",
/*46*/	OTHER,		"divf2 divr.rf, quo.mf",
/*47*/	OTHER,		"divf3 divr.rf, divd.rf, quo.wf",

/*48*/	OTHER,		"cvtfb src.rf, dst.wb",
/*49*/	OTHER,		"cvtfw src.rf, dst.ww",
/*4a*/	OTHER,		"cvtfl src.rf, dst.wl",
/*4b*/	OTHER,		"cvtrfl src.rf, dst.wf",
/*4c*/	OTHER,		"cvtbf src.rb, dst.wf",
/*4d*/	OTHER,		"cvtwf src.rw, dst.wf",
/*4e*/	OTHER,		"cvtlf src.rl, dst.wf",
/*4f*/	OTHER,		"acbf limit.rf, add.rf, index.mf, displ.bw",

/*50*/	OTHER,		"movf src.rf, dst.wf",
/*51*/	OTHER,		"cmpf src1.rf, src2.rf",
/*52*/	OTHER,		"mnegf src.rf, dst.wf",
/*53*/	OTHER,		"tstf src.rf",
/*54*/	OTHER,		"emodf mulr.rf, mulrx.rb, muld.rf, int.wl, fract.wf",
/*55*/	OTHER,		"polyf arg.rf, degree.rf, tbladdr.ab",
/*56*/	OTHER,		"cvtfd src.rf, dst.wd",
/*57*/	OTHER,		"dummy",

/*58*/	OTHER,		"adawi add.rw, sum.mw",
/*59*/	OTHER,		"dummy",
/*5a*/	OTHER,		"dummy",
/*5b*/	OTHER,		"dummy",
/*5c*/	OTHER,		"dummy",
/*5d*/	OTHER,		"dummy",
/*5e*/	OTHER,		"dummy",
/*5f*/	OTHER,		"dummy",

/*60*/	OTHER,		"addd2 add.rd, sum.md",
/*61*/	OTHER,		"addd3 add1.rd, add2.rd, sum.wd",
/*62*/	OTHER,		"subd2 sub.rd, dif.md",
/*63*/	OTHER,		"subd3 sub.rd, min.rd, dif.wd",
/*64*/	OTHER,		"muld2 mulr.rd, prod.md",
/*65*/	OTHER,		"muld3 mulr.rd, muld.rd, prod.wd",
/*66*/	OTHER,		"divd2 divr.rd, quo.md",
/*67*/	OTHER,		"divd3 divr.rd, divd.rd, quo.wd",

/*68*/	OTHER,		"cvtdb src.rd, dst.wb",
/*69*/	OTHER,		"cvtdw src.rd, dst.ww",
/*6a*/	OTHER,		"cvtdl src.rd, dst.wl",
/*6b*/	OTHER,		"cvtrdl src.rd, dst.wl",
/*6c*/	OTHER,		"cvtbd src.rb, dst.wd",
/*6d*/	OTHER,		"cvtwd src.rw, dst.wd",
/*6e*/	OTHER,		"cvtld src.rl, dst.wd",
/*6f*/	BRA|DBL,	"acbd limit.rd, add.rd, index.md, displ.bw",

/*70*/	OTHER,		"movd src.rd, dst.wd",
/*71*/	OTHER,		"cmpd src1.rd, src2.rd",
/*72*/	OTHER,		"mnegd src.rd, dst.wd",
/*73*/	OTHER,		"tstd src.rd",
/*74*/	OTHER,		"emodd mulr.rd, mulrx.rb, muld.rd, int.wl, fract.wd",
/*75*/	OTHER,		"polyd arg.rd, degree.rd, tbladdr.ab",
/*76*/	OTHER,		"cvtdf src.rd, dst.wf",
/*77*/	OTHER,		"dummy",

/*78*/	OTHER,		"ashl cnt.rb, src.rl, dst.wl",
/*79*/	OTHER,		"ashq cnt.rb, src.rq, dst.wq",
/*7a*/	OTHER,		"emul mulr.rl, muld.rl, add.rl, prod.wq", /* prog.wg in manual */
/*7b*/	OTHER,		"ediv divr.rl, divd.rq, quo.wl, rem.wl",
/*7c*/	OTHER,		"clrq dst.wq",
/*7d*/	OTHER,		"movq src.rq, dst.wq",
/*7e*/	OTHER,		"movaq src.aq, dst.wl",
/*7f*/	OTHER,		"pushaq src.aq",

/*80*/	OTHER,		"addb2 add.rb, sum.mb",
/*81*/	OTHER,		"addb3 add1.rb, add2.rb, sum.wb",
/*82*/	OTHER,		"subb2 sub.rb, dif.mb",
/*83*/	OTHER,		"subb3 sub.rb, min.rb, dif.wb",
/*84*/	OTHER,		"mulb2 mulr.rb, prod.mb",
/*85*/	OTHER,		"mulb3 mulr.rb, muld.rb, prod.wb",
/*86*/	OTHER,		"divb2 divr.rb, quo.mb",
/*87*/	OTHER,		"divb3 divr.rb, divd.rb, quo.wb",

/*88*/	OTHER,		"bisb2 mask.rb, dst.mb",
/*89*/	OTHER,		"bisb3 mask.rb, src.rb, dst.wb",
/*8a*/	OTHER,		"bicb2 mask.rb, dst.mb",
/*8b*/	OTHER,		"bicb3 mask.rb, src.rb, dst.wb",
/*8c*/	OTHER,		"xorb2 mask.rb, dst.mb",
/*8d*/	OTHER,		"xorb3 mask.rb, src.rb, dst.wb",
/*8e*/	OTHER,		"mnegb src.rb, dst.wb",
/*8f*/	BRA,		"caseb selector.rb, base.rb, limit.rb, displ*.bw",

/*90*/	OTHER,		"movb src.rb, dst.wb",
/*91*/	OTHER,		"cmpb src1.rb, src2.rb",
/*92*/	OTHER,		"mcomb src.rb, dst.wb",
/*93*/	OTHER,		"bitb mask.rb, src.rb",
/*94*/	OTHER,		"clrb dst.wb",
/*95*/	OTHER,		"tstb src.rb",
/*96*/	OTHER,		"incb sum.mb",
/*97*/	OTHER,		"decb dif.mb",

/*98*/	OTHER,		"cvtbl src.rb, dst.wl",
/*99*/	OTHER,		"cvtbw src.rb, dst.ww",
/*9a*/	OTHER,		"movzbl src.rb, dst.wl",
/*9b*/	OTHER,		"movzbw src.rb, dst.ww",
/*9c*/	OTHER,		"rotl cnt.rb, src.rl, dst.wl",
/*9d*/	BRA|DBL,	"acbb limit.rb, add.rb, index.mb, displ.bw",
/*9e*/	OTHER,		"movab src.ab, dst.wl",
/*9f*/	OTHER,		"pushab src.ab",

/*a0*/	OTHER,		"addw2 add.rw, sum.mw",
/*a1*/	OTHER,		"addw3 add1.rw, add2.rw, sum.ww",
/*a2*/	OTHER,		"subw2 sub.rw, dif.mw",
/*a3*/	OTHER,		"subw3 sub.rw, min.rw, dif.ww",
/*a4*/	OTHER,		"mulw2 mulr.rw, prod.mw",
/*a5*/	OTHER,		"mulw3 mulr.rw, muld.rw, prod.ww",
/*a6*/	OTHER,		"divw2 divr.rw, quo.mw",
/*a7*/	OTHER,		"divw3 divr.rw, divd.rw, quo.ww",

/*a8*/	OTHER,		"bisw2 mask.rw, dst.mw",
/*a9*/	OTHER,		"bisw3 mask.rw, src.rw, dst.ww",
/*aa*/	OTHER,		"bicw2 mask.rw, dst.mw",
/*ab*/	OTHER,		"bicw3 mask.rw, src.rw, dst.ww",
/*ac*/	OTHER,		"xorw2 mask.rw, dst.mw",
/*ad*/	OTHER,		"xorw3 mask.rw, src.rw, dst.ww",
/*ae*/	OTHER,		"mnegw src.rw, dst.ww",
/*af*/	BRA,		"casew selector.rw, base.rw, limit.rw, displ*.bw",

/*b0*/	OTHER,		"movw src.rw, dst.ww",
/*b1*/	OTHER,		"cmpw src1.rw, src2.rw",
/*b2*/	OTHER,		"mcomw src.rw, dst.ww",
/*b3*/	OTHER,		"bitw mask.rw, src.rw",
/*b4*/	OTHER,		"clrw dst.ww",
/*b5*/	OTHER,		"tstw src.rw",
/*b6*/	OTHER,		"incw sum.mw",
/*b7*/	OTHER,		"decw dif.mw",

/*b8*/	OTHER,		"bispsw mask.rw",
/*b9*/	OTHER,		"bicpsw mask.rw",
/*ba*/	OTHER,		"popr mask.rw",
/*bb*/	OTHER,		"pushr mask.rw",
/*bc*/	OTHER,		"chmk code.rw",
/*bd*/	OTHER,		"chme code.rw",
/*be*/	OTHER,		"chms code.rw",
/*bf*/	OTHER,		"chmu code.rw",

/*c0*/	OTHER,		"addl2 add.rl, sum.ml",
/*c1*/	OTHER,		"addl3 add1.rl, add2.rl, sum.wl",
/*c2*/	OTHER,		"subl2 sub.rl, dif.ml",
/*c3*/	OTHER,		"subl3 sub.rl, min.rl, dif.wl",
/*c4*/	OTHER,		"mull2 mulr.rl, prod.ml",
/*c5*/	OTHER,		"mull3 mulr.rl, muld.rl, prod.wl",
/*c6*/	OTHER,		"divl2 divr.rl, quo.ml",
/*c7*/	OTHER,		"divl3 divr.rl, divd.rl, quo.wl",

/*c8*/	OTHER,		"bisl2 mask.rl, dst.ml",
/*c9*/	OTHER,		"bisl3 mask.rl, src.rl, dst.wl",
/*ca*/	OTHER,		"bicl2 mask.rl, dst.ml",
/*cb*/	OTHER,		"bicl3 mask.rl, src.rl, dst.wl",
/*cc*/	OTHER,		"xorl2 mask.rl, dst.ml",
/*cd*/	OTHER,		"xorl3 mask.rl, src.rl, dst.wl",
/*ce*/	OTHER,		"mnegl src.rl, dst.wl",
/*cf*/	BRA,		"casel selector.rl, base.rl, limit.rl, displ*.bw",

/*d0*/	OTHER,		"movl src.rl, dst.wl",
/*d1*/	OTHER,		"cmpl src1.rl, src2.rl",
/*d2*/	OTHER,		"mcoml src.rl, dst.wl",
/*d3*/	OTHER,		"bitl mask.rl, src.rl",
/*d4*/	OTHER,		"clrl dst.wl",
/*d5*/	OTHER,		"tstl src.rl",
/*d6*/	OTHER,		"incl sum.ml",
/*d7*/	OTHER,		"decl dif.ml",

/*d8*/	OTHER,		"adwc add.rl, sum.ml",
/*d9*/	OTHER,		"sbwc sub.rl, dif.ml",
/*da*/	OTHER,		"mtpr src.rl, regnumber.rl",
/*db*/	OTHER,		"mfpr regnumber.rl, dst.wl",
/*dc*/	OTHER,		"movpsl dst.wl",
/*dd*/	OTHER,		"pushl src.rl",
/*de*/	OTHER,		"moval src.al, dst.wl",
/*df*/	OTHER,		"pushal src.al",

/*e0*/	BRA|DBL,	"bbs pos.rl, base.ab, displ.bb",
/*e1*/	BRA|DBL,	"bbc pos.rl, base.ab, displ.bb",
/*e2*/	BRA|DBL,	"bbss pos.rl, base.ab, displ.bb",
/*e3*/	BRA|DBL,	"bbcs pos.rl, base.ab, displ.bb",
/*e4*/	BRA|DBL,	"bbsc pos.rl, base.ab, displ.bb",
/*e5*/	BRA|DBL,	"bbcc pos.rl, base.ab, displ.bb",
/*e6*/	BRA|DBL,	"bbssi pos.rl, base.ab, displ.bb",
/*e7*/	BRA|DBL,	"bbcci pos.rl, base.ab, displ.bb",

/*e8*/	BRA|DBL,	"blbs src.rl, displ.bb",
/*e9*/	BRA|DBL,	"blbc src.rl, displ.bb",
/*ea*/	OTHER,		"ffs startpos.rl, size.rb, base.ab, findpos.wl",
/*eb*/	OTHER,		"ffc startpos.rl, size.rb, base.ab, findpos.wl",
/*ec*/	OTHER,		"cmpv pos.rl, size.rb, base.vb, dst.wl",
/*ed*/	OTHER,		"cmpzv pos.rl, size.rb, base.vb, dst.wl",
/*ee*/	OTHER,		"extv pos.rl, size.rb, base.vb, dst.wl",
/*ef*/	OTHER,		"extzv pos.rl, size.rb, base.vb, dst.wl",

/*f0*/	OTHER,		"insv src.rl, pos.rl, size.rb, base.ab",
/*f1*/	BRA|DBL,	"acbl limit.rl, add.rl, index.ml, displ.bw",
/*f2*/	BRA|DBL,	"aoblss limit.rl, index.ml, displ.bb",
/*f3*/	BRA|DBL,	"aobleq limit.rq, index.ml, displ.bb",
/*f4*/	BRA|DBL,	"sobgeq index.ml, displ.bb",
/*f5*/	BRA|DBL,	"sobgtr index.ml, displ.bb",
/*f6*/	OTHER,		"cvtlb src.rl, dst.wb",
/*f7*/	OTHER,		"cvtlw src.rl, dst.ww",

/*f8*/	OTHER,		"ashp cnt.rb, srclen.rw, srcaddr.ab, round.rb, dstlen.rw, dstaddr.ab",
/*f9*/	OTHER,		"cvtlp src.rl, dstlen.rw, dstaddr.ab",
/*fa*/	CALL,		"callg arglist.ab, dst.Fb",
/*fb*/	CALL,		"calls numarg.rl, dst.Fb",
/*fc*/	OTHER,		"xfc",
/*fd*/	OTHER,		"escd",
/*fe*/	OTHER,		"esce",
/*ff*/	OTHER,		"escf",
};

/*  Maximum length of a string representing a VAX address mode.
 */
#define MAX_ADDRMODE_LEN	25

static const char *
address_mode(addr, text, buf, access_type, optype, p_val_is_constant, p_val)
taddr_t addr;
register const char *text;
char *buf;
int access_type, optype;
int *p_val_is_constant, *p_val;
{
	static const char *regnames[] = {
		"r0",  "r1",  "r2",  "r3",  "r4",  "r5",  "r6",  "r7",
		"r8",  "r9",  "r10", "r11", "ap",  "fp",  "sp",  "pc" 
	};
	const char *dispfmt, *regname;
	char rbuf[MAX_ADDRMODE_LEN + 1];
	int regno, addrmode, val;

	if (access_type == 'b') {
		switch(optype) {
		case 'b':
			val = addr + 1 + *text;
			if (buf != NULL)
				(void) sprintf(buf, "%x", val);
			++text;
			break;
		case 'w':
			val = addr + sizeof(short) + *(short *)text;
			if (buf != NULL)
				(void) sprintf(buf, "%x", val);
			text += sizeof(short);
			break;
		default:
			panic("bad operand type");
		}
		*p_val_is_constant = TRUE;
		*p_val = val;
		return text;
	}
	if (strchr("afFmrvw", access_type) == NULL)
		panic("bad access type");
		
	regno = *text & 0xf;
	addrmode = (*text++ & 0xf0) >> 4;
	*p_val_is_constant = FALSE;	/* default - maty be overwritten */

	dispfmt = regname = 0; /* to satisfy gcc */

	if (addrmode > 7 && regno == 15) {
		if (buf != NULL)
			dispfmt = (addrmode & 1) ? "*%x" : "%x"; 
		switch (addrmode) {
		case 0x8:
			switch(optype) {
			case 'b':
				val = *text;
				*p_val_is_constant = TRUE;
				if (buf != NULL)
					sprintf(buf, "$%x", val);
				++text;
				break;
			case 'f':
				if (buf != NULL)
					sprintf(buf, "$%g", *(float *)text);
				text += sizeof(float);
				break;
			case 'd':
				if (buf != NULL)
					sprintf(buf, "$%g", *(double *)text);
				text += sizeof(double);
				break;
			case 'l':
				val = *(int *)text;
				*p_val_is_constant = TRUE;
				if (buf != NULL)
					sprintf(buf, "$%x", val);
				text += sizeof(int);
				break;
			case 'q':
				if (buf != NULL)
					sprintf(buf, "$%08x%08x", ((int *)text)[0],
								  ((int *)text)[1]);
				text += sizeof(int) * 2;
				break;
			case 'w':
				val = *(short *)text;
				*p_val_is_constant = TRUE;
				if (buf != NULL)
					sprintf(buf, "$%x", val);
				text += sizeof(short);
				break;
			default:
				panic("bad optype in address_mode");
				break;
			}
			break;
		case 0x9:
			if (buf != NULL)
				(void) sprintf(buf, "*$%x", *(int *)text);
			text += sizeof(int);
			break;
		case 0xa:
		case 0xb:
			val = addr + 1 + sizeof(char) + *text;
			if (buf != NULL)
				(void) sprintf(buf, dispfmt, val);
			++text;
			*p_val_is_constant = (addrmode & 1) == 0;
			break;
		case 0xc:
		case 0xd:
			val = addr + 1 + sizeof(short) + *(short *)text;
			if (buf != NULL)
				(void) sprintf(buf, dispfmt, val);
			text += sizeof(short);
			*p_val_is_constant = (addrmode & 1) == 0;
			break;
		case 0xe:
			val = addr + 1 + sizeof(int) + *(int *)text;
			if (buf != NULL) {
				if (access_type == 'f' || access_type == 'F')
					(void) strcpy(buf, addr_to_func_and_offset(val, access_type == 'F'));
				else
					(void) sprintf(buf, "%x", val);
			}
			text += sizeof(int);
			*p_val_is_constant = TRUE;
			break;
		case 0xf:
			val = addr + 1 + sizeof(int) + *(int *)text;
			if (buf != NULL)
				(void) sprintf(buf, "*%x", val);
			text += sizeof(int);
			break;
		}
	}
	else {
		if (buf != NULL) {
			regname = regnames[regno];
			dispfmt = (addrmode & 1) ? "*%d(%s)" : "%d(%s)"; 
		}
		switch (addrmode) {
		case 0x0:
		case 0x1:
		case 0x2:
		case 0x3:
			val = text[-1] & 0x3f;
			if (buf != NULL)
				(void) sprintf(buf, "$%x", val);
			*p_val_is_constant = TRUE;
			break;
		case 0x4:
			text = address_mode(addr, text, rbuf, access_type, optype,
							p_val_is_constant, &val);
			*p_val_is_constant = FALSE;
			if (buf != NULL) {
				(void) sprintf(buf, "%.*s[%s]",
					       MAX_ADDRMODE_LEN - strlen(regname),
					       rbuf, regname);
				buf[MAX_ADDRMODE_LEN] = '\0';
			}
			break;
		case 0x5:
			if (buf != NULL)
				(void) sprintf(buf, "%s", regname);
			break;
		case 0x6:
			if (buf != NULL)
				(void) sprintf(buf, "(%s)", regname);
			break;
		case 0x7:
			if (buf != NULL)
				(void) sprintf(buf, "-(%s)", regname);
			break;
		case 0x8:
			if (buf != NULL)
				(void) sprintf(buf, "(%s)+", regname);
			break;
		case 0x9:
			if (buf != NULL)
				(void) sprintf(buf, "*(%s)+", regname);
			break;
		case 0xa:
		case 0xb:
			if (buf != NULL)
				(void) sprintf(buf, dispfmt, *text, regname);
			++text;
			break;
		case 0xc:
		case 0xd:
			if (buf != NULL)
				(void) sprintf(buf, dispfmt, *(short *)text, regname);
			text += sizeof(short);
			break;
		case 0xe:
		case 0xf:
			if (buf != NULL)
				(void) sprintf(buf, dispfmt, *(int *)text, regname);
			text += sizeof(int);
			break;
		}
	}
	*p_val = val;
	return text;
}

static const char *
disassemble_one_inst_and_get_operand(addr, text, p_buf, p_val_is_constant, p_val)
taddr_t addr;
const char *text, **p_buf;
int *p_val_is_constant, *p_val;
{
	static char buf[512];
	int opcode;
	int val, val_is_constant;
	char optype, access_type;
	const char *operands, *next_operand, *save_text, *cptr;
	char *bufp;

	opcode = *text++ & 0xff;
	++addr;

	if (p_buf != NULL) {
		bufp = buf;
		cptr = Optable[opcode &0xff].ot_opdesc;
		while (*cptr != ' ' && *cptr != '\0')
			*bufp++ = *cptr++;
		if (*cptr == ' ') {
			*bufp++ = '\t';
			++cptr;
		}
		operands = cptr;
	}
	else {
		bufp = NULL;
		if ((operands = strchr(Optable[opcode].ot_opdesc, ' ')) == NULL)
			operands = "";
	}

	val_is_constant = FALSE;
	for (; *operands != '\0'; operands = next_operand) {
		if ((next_operand = strchr(operands + 1, ',')) == NULL)
			next_operand = operands + strlen(operands);
		access_type = next_operand[-2];
		optype = next_operand[-1];
		if (strchr("bdflqw", optype) == NULL)
			panic("unknown operand type");

		save_text = text;
		if (next_operand[-4] == '*') {
			int i, case_limit;

			/* Special case - the case statement has an inline
			 * array of 16 bit displacements.
			 */
			if (bufp != NULL)
				*--bufp = '\0'; /* squish ',' */

			if (!val_is_constant)
				panic("bad limit operand in case statement");
			case_limit = val;

			for (i = 0; i <= case_limit; ++i) {
				if (bufp != NULL) {
					(void) strcpy(bufp, "\n\t\t");
					bufp += strlen(bufp);
				}
				text = address_mode(addr, text, bufp,
						    access_type, optype,
						    &val_is_constant, &val);
				if (bufp != NULL)
					bufp += strlen(bufp);
			}
		}
		else {
			text = address_mode(addr, text, bufp,
					    access_type, optype,
					    &val_is_constant, &val);
			if (bufp != NULL) {
				bufp += strlen(bufp);
				if (*next_operand != '\0')
					*bufp++ = ',';
			}
		}
		addr += text - save_text;
	}
	if (bufp != NULL) {
		*bufp = '\0';
		*p_buf = buf;
	}
	*p_val = val;
	*p_val_is_constant = val_is_constant;
	return text;
}

const char *
disassemble_one_instruction(addr, text, p_buf)
taddr_t addr;
const char *text, **p_buf;
{
	int val, val_is_constant;

	return disassemble_one_inst_and_get_operand(addr, text, p_buf,
							&val_is_constant, &val);
}

jump_t *
get_jumps(addr, text, len, want_calls, want_branches)
taddr_t addr;
const char *text;
int len, want_calls, want_branches;
{
	static jump_t *jtab;
	static int jtab_size = 0;
	taddr_t dstaddr;
	const char *lim, *new_text;
	int itype, idesc, val, val_is_constant;
	int njumps;

	if (jtab_size == 0) {
		jtab_size = 16;
		jtab = (jump_t *)e_malloc((jtab_size + 1) * sizeof(jump_t));
	}
	lim = text + len;
	njumps = 0;
	while (text < lim) {
		idesc = Optable[*text & 0xff].ot_itype;
		itype = idesc & ITMASK;
		new_text = disassemble_one_inst_and_get_operand(addr, text,
					(const char **)NULL, &val_is_constant, &val);
		if ((want_calls && itype == CALL) ||
		    (want_branches && (itype == BRA || itype == RET))) {
			if (njumps >= jtab_size) {
				jtab_size *= 2;
				jtab = (jump_t *)e_realloc((char *)jtab,
						  (jtab_size + 1) * sizeof(jump_t));
			}
			jtab[njumps].ju_addr = addr;
			jtab[njumps].ju_type = (itype == CALL) ? JT_CALL : JT_BRANCH;
			jtab[njumps].ju_unconditional = FALSE;	/* don't know */
			dstaddr = 0;
			if (val_is_constant &&
				((itype == BRA && (idesc & DBL)) || itype == CALL))

				jtab[njumps].ju_dstaddr = val;
			else
				jtab[njumps].ju_dstaddr = 0;
			++njumps;
		}
		addr += new_text - text;
		text = new_text;
	}
	jtab[njumps].ju_type = JT_END;
	return jtab;
}
#endif /* ARCH_VAX */
