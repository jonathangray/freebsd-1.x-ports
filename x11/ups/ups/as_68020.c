/* as_68020.c - disassemble MC68020 machine code */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_as_68020_c_sccsid[] = "@(#)as_68020.c	1.16 26/7/92 (UKC)";

#include <mtrprog/ifdefs.h>

#ifdef ARCH_SUN3
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <local/ukcprog.h>
#include "ups.h"
#include "as.h"


/*  Instruction type.  Indicated in the opcode table in the opcode string -
 *  if it starts with "*b " or "*c " then the instruction is a branch or
 *  call.  If it doesn't start with "*" the the instruction is not a jump.
 */
typedef enum itypeen {
	IT_NOTJUMP,
	IT_BRANCH,
	IT_CALL
} itype_t;

typedef unsigned short word_t;

typedef struct xmaskst {
	word_t xm_mask;
	word_t xm_val;
} xmask_t;

typedef struct opcodest {
	const char *oc_const_name;
	char *oc_name;
	short oc_nwords;
	xmask_t *oc_xmask;
	word_t oc_word;
	word_t oc_mask;
} opcode_t;

typedef enum opsizeen { SZ_UNKNOWN, SZ_BYTE, SZ_WORD, SZ_LONG } opsize_t;

typedef struct dstatest {
	int ds_want_output;		/* In */
	word_t *ds_text;		/* In */
	taddr_t ds_addr;		/* In */

	opsize_t ds_size;		/* Out */
	itype_t ds_itype;		/* Out */
	int ds_offset;			/* Out */
	int ds_dstaddr_known;		/* Out */
	taddr_t ds_dstaddr;		/* Out */
} dstate_t;

typedef struct fieldst {
	const char *fe_name;
	int fe_wordnum;
	int fe_startbit;
	int fe_stopbit;
	const char *(*fe_func) PROTO((dstate_t *ds, int fieldval, const char **ftab));
	const char **fe_tab;
} field_t;

static word_t *get_displacement PROTO((word_t *text, int dispsize, int *p_res, int *p_bad_address));
static word_t *ext_ea PROTO((word_t *text, char *buf, const char *reg, int want_output, int *p_bad_address));
static const char *ff_ea PROTO((dstate_t *ds, int fieldval, const char **unused_ftab));
static const char *ff_moveas PROTO((dstate_t *ds, int fieldval, const char **ftab));
static const char *ff_movs PROTO((dstate_t *ds, int unused_fieldval, const char **unused_ftab));
static const char *ff_hexbyte PROTO((dstate_t *ds, int fieldval, const char **unused_ftab));
static const char *ff_num PROTO((dstate_t *ds, int fieldval, const char **unused_ftab));
static const char *ff_byte PROTO((dstate_t *ds, int unused_fieldval, const char **unused_ftab));
static const char *ff_word PROTO((dstate_t *ds, int unused_fieldval, const char **unused_ftab));
static const char *ff_long PROTO((dstate_t *ds, int unused_fieldval, const char **unused_ftab));
static const char *ff_disp16 PROTO((dstate_t *ds, int fieldval, const char **unused_ftab));
static const char *ff_disp PROTO((dstate_t *ds, int fieldval, const char **unused_ftab));
static const char *ff_data PROTO((dstate_t *ds, int unused_fieldval, const char **unused_ftab));
static const char *ff_size_w PROTO((dstate_t *ds, int unused_fieldval, const char **unused_ftab));
static const char *ff_size PROTO((dstate_t *ds, int fieldval, const char **ftab));
static const char *ff_tab PROTO((dstate_t *unused_ds, int fieldval, const char **ftab));
static const char *ff_regmask PROTO((dstate_t *ds, int fieldval, const char **unused_ftab));
static const char *ff_ctrlreg PROTO((dstate_t *unused_ds, int fieldval, const char **unused_ftab));
static field_t *lookup_field PROTO((const char *name));
static void disassemble_inst_internal PROTO((dstate_t *ds, const char **p_buf));

#ifdef TESTING
static const char *addr_to_func_and_offset PROTO((taddr_t addr, int flag));
static void check_fieldnames_unique PROTO((void));
static void check_fields_in_name_defined PROTO((char *name, word_t mask));
static void check_fields_defined PROTO((void));
static int hit_xmask PROTO((xmask_t *xmasks, word_t word));
static void check_opcodes_unique PROTO((void));
#endif

/*  Used in get_jumps()
 */
#define RTS		0x4e75

#define JMP_SWITCH	0x4efb	/* jmp pc@(d,reg) */

#define W12(b11,b10,b9,b8, b7,b6,b5,b4, b3,b2,b1,b0) \
		(b11<<11 | b10<<10 | b9<<9 | b8<<8 | b7<<7 | b6<<6 | \
		 b5<<5 | b4<<4 | b3<<3 | b2<<2 | b1<<1 | b0)

#define O(b11,b10,b9,b8, b7,b6,b5,b4, b3,b2,b1,b0) \
		W12((b11&1), (b10&1), (b9&1), (b8&1), (b7&1), (b6&1), \
		    (b5&1), (b4&1), (b3&1), (b2&1), (b1&1), (b0&1)) , \
		W12((b11/2), (b10/2), (b9/2), (b8/2), (b7/2), (b6/2), \
		    (b5/2), (b4/2),  (b3/2), (b2/2), (b1/2), (b0/2))

#define _ 2

static xmask_t Size76_xm[] = {
	W12(0,0,0,0, 1,1,0,0, 0,0,0,0),	W12(0,0,0,0, 1,1,0,0, 0,0,0,0),
	0, 0,
};

static xmask_t Cas_xm[] = {
	W12(0,1,1,0, 0,0,0,0, 0,0,0,0), W12(0,0,0,0, 0,0,0,0, 0,0,0,0),
	0, 0,
};

static xmask_t Ea_noreg_xm[] = {
	W12(0,0,0,0, 0,0,1,1, 1,0,0,0), W12(0,0,0,0, 0,0,0,0, 0,0,0,0),
	W12(0,0,0,0, 0,0,1,1, 1,0,0,0), W12(0,0,0,0, 0,0,0,0, 1,0,0,0),
	0, 0,
};

static opcode_t Opcodes_0000[] = {
	"*b rtm {d/a3}{2_0}",NULL, 	1,NULL,		O(0,1,1,0, 1,1,0,0, _,_,_,_),
	"*c callm #{byte},{ea}",NULL,	2,NULL,		O(0,1,1,0, 1,1,_,_, _,_,_,_),

	"{1:chkcmp11_11}{sz10_9} {ea},{1:d/a15}{1:14_12}",NULL,
					2,NULL,		O(0,_,_,0, 1,1,_,_, _,_,_,_),

	"or #{byte},ccr",NULL,		2,NULL,		O(0,0,0,0, 0,0,1,1, 1,1,0,0),
	"or #{word},sr",NULL,		2,NULL,		O(0,0,0,0, 0,1,1,1, 1,1,0,0),
	"or{sz} #{data},{ea}",NULL,	1,Size76_xm,	O(0,0,0,0, _,_,_,_, _,_,_,_),

	"movp{w/l6} d{2_0},({word},a{11_9})",NULL,
					2,NULL,		O(_,_,_,1, 0,_,0,0, 1,_,_,_),
	"movp{w/l6} d{2_0},({word},a{11_9})",NULL,	
					2,NULL,		O(_,_,_,1, 1,_,0,0, 1,_,_,_),

	"b{bitop7_6} d{11_9},{ea}",NULL,1,NULL,		O(_,_,_,1, _,_,_,_, _,_,_,_),

	"and #{byte},ccr",NULL,		2,NULL,		O(0,0,1,0, 0,0,1,1, 1,1,0,0),
	"and #{word},sr",NULL,		2,NULL,		O(0,0,1,0, 0,1,1,1, 1,1,0,0),
	"and{sz} #{data},{ea}",NULL,	1,Size76_xm,	O(0,0,1,0, _,_,_,_, _,_,_,_),

	"sub{sz} #{data},{ea}",NULL,	1,Size76_xm,	O(0,1,0,0, _,_,_,_, _,_,_,_),
	"add{sz} #{data},{ea}",NULL,	1,Size76_xm,	O(0,1,1,0, _,_,_,_, _,_,_,_),

	"b{bitop7_6} #{byte},{ea}",NULL,2,NULL,		O(1,0,0,0, _,_,_,_, _,_,_,_),

	"cas2{szb10_9} d{1:2_0}:d{2:2_0},d{1:8_6}:d{2:8_6},{1:d/a15}{1:14_12} {2:d/a15}{2:14_12}",NULL,
					3,Cas_xm,	O(1,_,_,0, 1,1,1,1, 1,1,0,0),
	"cas{szb10_9} d{1:2_0},d{1:8_6},{ea}",NULL,
					2,Cas_xm,	O(1,_,_,0, 1,1,_,_, _,_,_,_),

	"eor #{byte},ccr",NULL,		2,NULL,		O(1,0,1,0, 0,0,1,1, 1,1,0,0),
	"eor #{word},sr",NULL,		2,NULL,		O(1,0,1,0, 0,1,1,1, 1,1,0,0),
	"eor{sz} #{data},{ea}",NULL,	1,Size76_xm,	O(1,0,1,0, _,_,_,_, _,_,_,_),

	"cmp{sz} #{data},{ea}",NULL,	1,Size76_xm,	O(1,1,0,0, _,_,_,_, _,_,_,_),

	"movs{sz} {movs_operands}",NULL,2,Size76_xm,	O(1,1,1,0, _,_,_,_, _,_,_,_),
	{ NULL }
};

static opcode_t Opcodes_0001[] = {
	"mov{szc13_12} {moveas11_0}",NULL,
					1,NULL,		O(_,_,_,_, _,_,_,_, _,_,_,_),
	{ NULL }
};

static opcode_t Opcodes_0010[] = {
	"mov{szc13_12} {ea},a{11_9}",NULL,
					1,NULL,		O(_,_,_,0, 0,1,_,_, _,_,_,_),
	"mov{szc13_12} {moveas11_0}",NULL,
					1,NULL,		O(_,_,_,_, _,_,_,_, _,_,_,_),
	{ NULL }
};

static opcode_t Opcodes_0011[] = {
	"mov{szc13_12} {ea},a{11_9}",NULL,
					1,NULL,		O(_,_,_,0, 0,1,_,_, _,_,_,_),
	"mov{szc13_12} {moveas11_0}",NULL,
					1,NULL,		O(_,_,_,_, _,_,_,_, _,_,_,_),
	{ NULL }
};

static opcode_t Opcodes_0100[] = {
	"mov sr,{ea}",NULL,		1,NULL,		O(0,0,0,0, 1,1,_,_, _,_,_,_),
	"negx{sz} {ea}",NULL,		1,Size76_xm,	O(0,0,0,0, _,_,_,_, _,_,_,_),
	"chk{l/w7} {ea},d{11_9}",NULL,	1,NULL,		O(_,_,_,1, _,0,_,_, _,_,_,_),
	"mov ccr,{ea}",NULL, 		1,NULL,		O(0,0,1,0, 1,1,_,_, _,_,_,_),
	"clr{sz} {ea}",NULL,		1,Size76_xm,	O(0,0,1,0, _,_,_,_, _,_,_,_),
	"mov {ea},ccr",NULL,		1,NULL,		O(0,1,0,0, 1,1,_,_, _,_,_,_),
	"neg{sz} {ea}",NULL,		1,Size76_xm,	O(0,1,0,0, _,_,_,_, _,_,_,_),
	"mov {ea},sr",NULL,		1,NULL,		O(0,1,1,0, 1,1,_,_, _,_,_,_),
	"not{sz} {ea}",NULL,		1,Size76_xm,	O(0,1,1,0, _,_,_,_, _,_,_,_),
	"linkl a{2_0},#{long}",NULL,	3,NULL,		O(1,0,0,0, 0,0,0,0, 1,_,_,_),
	"nbcd {ea}",NULL,		1,NULL,		O(1,0,0,0, 0,0,_,_, _,_,_,_),
	"swap d{2_0}",NULL,		1,NULL,		O(1,0,0,0, 0,1,0,0, 0,_,_,_),
	"bkpt #{2_0}",NULL, 		1,NULL,		O(1,0,0,0, 0,1,0,0, 1,_,_,_),
	"pea {ea}",NULL,		1,NULL,		O(1,0,0,0, 0,1,_,_, _,_,_,_),
	"extw d{2_0}",NULL,		1,NULL,		O(1,0,0,0, 1,0,0,0, 0,_,_,_),
	"extl d{2_0}",NULL,		1,NULL,		O(1,0,0,0, 1,1,0,0, 0,_,_,_),
	"extbl d{2_0}",NULL,		1,NULL,		O(1,0,0,1, 1,1,0,0, 0,_,_,_),
	"lea {ea},a{11_9}",NULL,	1,Ea_noreg_xm,	O(_,_,_,1, 1,1,_,_, _,_,_,_),
	"movem{w/l6} {regmask},{ea}",NULL,
					2,Ea_noreg_xm,	O(1,0,0,0, 1,_,_,_, _,_,_,_),
	"movem{w/l6} {ea},{regmask}",NULL,
					2,Ea_noreg_xm,	O(1,1,0,0, 1,_,_,_, _,_,_,_),
	"illegal",NULL,			1,NULL,		O(1,0,1,0, 1,1,1,1, 1,1,0,0),
	"tas {ea}",NULL,		1,NULL,		O(1,0,1,0, 1,1,_,_, _,_,_,_),
	"tst{sz} {ea}",NULL,		1,Size76_xm,	O(1,0,1,0, _,_,_,_, _,_,_,_),
	"{mul/div6}{1:u/s11}{forcel}{1:w/l10} {ea},d{1:2_0}:d{1:14_12}",NULL,
					2,NULL,		O(1,1,0,0, 0,_,_,_, _,_,_,_),
	"*c trap #{3_0}",NULL,		1,NULL,		O(1,1,1,0, 0,1,0,0, _,_,_,_),
	"link a{2_0},#{word}",NULL, 	2,NULL,		O(1,1,1,0, 0,1,0,1, 0,_,_,_),
	"unlk a{2_0}",NULL,		1,NULL,		O(1,1,1,0, 0,1,0,1, 1,_,_,_),
	"mov a{2_0},usp",NULL, 		1,NULL,		O(1,1,1,0, 0,1,1,0, 0,_,_,_),
	"mov usp,a{2_0}",NULL,  	1,NULL,		O(1,1,1,0, 0,1,1,0, 1,_,_,_),
	"reset",NULL,			1,NULL,		O(1,1,1,0, 0,1,1,1, 0,0,0,0),
	"nop",NULL,			1,NULL,		O(1,1,1,0, 0,1,1,1, 0,0,0,1),
	"stop #{word}",NULL,		2,NULL,		O(1,1,1,0, 0,1,1,1, 0,0,1,0),
	"*b rte",NULL,			1,NULL,		O(1,1,1,0, 0,1,1,1, 0,0,1,1),
	"*b rtd #{word}",NULL,		2,NULL,		O(1,1,1,0, 0,1,1,1, 0,1,0,0),
	"*b rts",NULL,			1,NULL,		O(1,1,1,0, 0,1,1,1, 0,1,0,1),
	"*c trapv",NULL,		1,NULL,		O(1,1,1,0, 0,1,1,1, 0,1,1,0),
	"*b rtr",NULL,			1,NULL,		O(1,1,1,0, 0,1,1,1, 0,1,1,1),

	"movc {1:d/a15}{1:14_12},{1:ctrlreg}",NULL,
					2,NULL,		O(1,1,1,0, 0,1,1,1, 1,0,1,0),
	"movc {1:ctrlreg},{1:d/a15}{1:14_12}",NULL,		
					2,NULL,		O(1,1,1,0, 0,1,1,1, 1,0,1,1),

	"*c jsr {ea}",NULL,		1,NULL,		O(1,1,1,0, 1,0,_,_, _,_,_,_),
	"*b jmp {ea}",NULL,		1,NULL,		O(1,1,1,0, 1,1,_,_, _,_,_,_),
	{ NULL }
};

static opcode_t Opcodes_0101[] = {
	"*b db{condb11_8} d{2_0},{disp16}",NULL,
					2,NULL,		O(_,_,_,_, 1,1,0,0, 1,_,_,_),
	"*c trap{cond11_8}",NULL,	1,NULL,		O(_,_,_,_, 1,1,1,1, 1,1,0,0),
	"*c trap{cond11_8}w #{word}",NULL,
					2,NULL,		O(_,_,_,_, 1,1,1,1, 1,1,1,0),
	"*c trap{cond11_8}l #{long}",NULL,
					3,NULL,		O(_,_,_,_, 1,1,1,1, 1,1,1,1),
	"s{cond11_8} {ea}",NULL, 	1,NULL,		O(_,_,_,_, 1,1,_,_, _,_,_,_),
	"addq{sz} #{qdata11_9},{ea}",NULL,
					1,Size76_xm,	O(_,_,_,0, _,_,_,_, _,_,_,_),
	"subq{sz} #{qdata11_9},{ea}",NULL,
					1,Size76_xm,	O(_,_,_,1, _,_,_,_, _,_,_,_),
	{ NULL }
};

static opcode_t Opcodes_0110[] = {
	"*b bra{disp}",NULL,		1,NULL,		O(0,0,0,0, _,_,_,_, _,_,_,_),
	"*c bsr{disp}",NULL,		1,NULL,		O(0,0,0,1, _,_,_,_, _,_,_,_),
	"*b b{cond11_8}{disp}",NULL,	1,NULL,		O(_,_,_,_, _,_,_,_, _,_,_,_),
	{ NULL }
};

static opcode_t Opcodes_0111[] = {
	"moveq #{x7_0},d{11_9}",NULL,	1,NULL,		O(_,_,_,0, _,_,_,_, _,_,_,_),
	{ NULL }
};

static opcode_t Opcodes_1000[] = {
	"div{u/s8}{szw} {ea},d{11_9}",NULL,
					1,NULL,		O(_,_,_,_, 1,1,_,_, _,_,_,_),

	"sbcd d{2_0},d{11_9}",NULL,	1,NULL,		O(_,_,_,1, 0,0,0,0, 0,_,_,_),
	"sbcd a{2_0}@-,a{11_9}@-",NULL,	1,NULL,		O(_,_,_,1, 0,0,0,0, 1,_,_,_),

	"pack d{2_0},d{11_9} #{word}",NULL,
					2,NULL,		O(_,_,_,1, 0,1,0,0, 0,_,_,_),
	"pack a{2_0}@-,a{11_9}@- #{word}",NULL,
					2,NULL,		O(_,_,_,1, 0,1,0,0, 1,_,_,_),
	"unpk d{2_0},d{11_9} #{word}",NULL,
					2,NULL,		O(_,_,_,1, 1,0,0,0, 0,_,_,_),
	"unpk a{2_0}@-,a{11_9}@- #{word}",NULL,
					2,NULL,		O(_,_,_,1, 1,0,0,0, 1,_,_,_),

	"or{sz} {ea},d{11_9}",NULL,	1,Size76_xm,	O(_,_,_,0, _,_,_,_, _,_,_,_),
	"or{sz} d{11_9},{ea}",NULL,	1,Size76_xm,	O(_,_,_,1, _,_,_,_, _,_,_,_),
	{ NULL }
};

static opcode_t Opcodes_1001[] = {
	"sub{w/l8} a{11_9},{ea}",NULL,	1,NULL,		O(_,_,_,_, 1,1,_,_, _,_,_,_),
	"subx{sz} d{11_9},d{2_0}",NULL,	1,Size76_xm,	O(_,_,_,1, _,_,0,0, 0,_,_,_),
	"subx{sz} a{11_9}@-,a{2_0}@-",NULL,
					1,Size76_xm,	O(_,_,_,1, _,_,0,0, 1,_,_,_),
	"sub{sz} {ea},d{11_9}",NULL,	1,Size76_xm,	O(_,_,_,0, _,_,_,_, _,_,_,_),
	"sub{sz} d{11_9},{ea}",NULL,	1,Size76_xm,	O(_,_,_,1, _,_,_,_, _,_,_,_),
	{ NULL }
};

static opcode_t Opcodes_1010[] = {
	{ NULL }
};

static opcode_t Opcodes_1011[] = {
	"cmp{w/l8} {ea},a{11_9}",NULL,	1,NULL,		O(_,_,_,_, 1,1,_,_, _,_,_,_),
	"cmpm{sz} a{11_9}@+,a{2_0}@+",NULL,1,Size76_xm,	O(_,_,_,1, _,_,0,0, 1,_,_,_),
	"cmp{sz} {ea},d{11_9}",NULL,	1,Size76_xm,	O(_,_,_,0, _,_,_,_, _,_,_,_),
	"eor{sz} d{11_9},{ea}",NULL,	1,Size76_xm,	O(_,_,_,1, _,_,_,_, _,_,_,_),
	{ NULL }
};

static opcode_t Opcodes_1100[] = {
	"mul{u/s8}{szw} {ea},d{11_9}",NULL,
					1,NULL,		O(_,_,_,_, 1,1,_,_, _,_,_,_),
	"abcd d{11_9},d{2_0}",NULL,	1,NULL,		O(_,_,_,1, 0,0,0,0, 0,_,_,_),
	"abcd a{11_9}@-,a{2_0}@-",NULL,	1,NULL,		O(_,_,_,1, 0,0,0,0, 1,_,_,_),
	"exg d{11_9},d{2_0}",NULL,	1,NULL,		O(_,_,_,1, 0,1,0,0, 0,_,_,_),
	"exg a{11_9},a{2_0}",NULL,	1,NULL,		O(_,_,_,1, 0,1,0,0, 1,_,_,_),
	"exg d{11_9},a{2_0}",NULL,	1,NULL,		O(_,_,_,1, 1,0,0,0, 1,_,_,_),
	"and{sz} {ea},d{11_9}",NULL,	1,Size76_xm,	O(_,_,_,0, _,_,_,_, _,_,_,_),
	"and{sz} d{11_9},{ea}",NULL,	1,Size76_xm,	O(_,_,_,1, _,_,_,_, _,_,_,_),
	{ NULL }
};

static opcode_t Opcodes_1101[] = {
	"add{w/l8} {ea},a{11_9}",NULL,	1,NULL,		O(_,_,_,_, 1,1,_,_, _,_,_,_),
	"addx{sz} d{11_9},d{2_0}",NULL,	1,Size76_xm,	O(_,_,_,1, _,_,0,0, 0,_,_,_),
	"addx{sz} a{11_9}@-,-a{2_0}@-",NULL,
					1,Size76_xm,	O(_,_,_,1, _,_,0,0, 1,_,_,_),
	"add{sz} {ea},d{11_9}",NULL,	1,Size76_xm,	O(_,_,_,0, _,_,_,_, _,_,_,_),
	"add{sz} d{11_9},{ea}",NULL,	1,Size76_xm,	O(_,_,_,1, _,_,_,_, _,_,_,_),
	{ NULL }
};

static opcode_t Opcodes_1110[] = {
	"bf{bfop10_9} {ea}\\{{1:i/d11}{1:10_6}:{1:i/d5}{1:4_0}}",NULL,
					2,NULL,		O(1,_,_,0, 1,1,_,_, _,_,_,_),
	"bf{bfopd10_9} {ea}\\{{1:i/d11}{1:10_6}:{1:i/d5}{1:4_0}},d{1:14_12}",NULL,
					2,NULL,		O(1,_,_,1, 1,1,_,_, _,_,_,_),
	"{sro10_9}{r/l8} {ea}",NULL,	1,Size76_xm,	O(0,_,_,_, 1,1,_,_, _,_,_,_),
	"{sro4_3}{r/l8}{sz} #{qdata11_9},d{2_0}",NULL,
					1,Size76_xm,	O(_,_,_,_, _,_,0,_, _,_,_,_),
	"{sro4_3}{r/l8}{sz} d{11_9},d{2_0}",NULL,
					1,Size76_xm,	O(_,_,_,_, _,_,1,_, _,_,_,_),
	{ NULL }
};

static opcode_t Opcodes_1111[] = {
	"cpGEN cp{11_9} {ea}",NULL, 	2,NULL,		O(_,_,_,0, 0,0,_,_, _,_,_,_),
	"*b cpDB<{1:5_0}> cp{11_9},d{2_0},{disp16}",NULL,
					3,NULL,		O(_,_,_,0, 0,1,0,0, 1,_,_,_),
	"*c cpTRAP<{1:5_0}> cp{11_9},d{2_0}",NULL,
					3,NULL,		O(_,_,_,0, 0,1,1,1, 1,_,_,_),
	"cpS<{1:5_0}> cp{11_9},{ea}",NULL,
					2,NULL,		O(_,_,_,0, 0,1,_,_, _,_,_,_),
	"*b cpB<{5_0}>{w/l6} cp{11_9},{data}",NULL, 
					2,NULL,		O(_,_,_,0, 1,_,_,_, _,_,_,_),
	"cpSAVE cp{11_9},{ea}",NULL, 	1,NULL,		O(_,_,_,1, 0,0,_,_, _,_,_,_),
	"cpRESTORE cp{11_9},{ea}",NULL,	1,NULL,		O(_,_,_,1, 0,1,_,_, _,_,_,_),
	{ NULL }
};

#undef _
#undef O

static opcode_t *Opcodes[16] = {
	Opcodes_0000,
	Opcodes_0001,
	Opcodes_0010,
	Opcodes_0011,
	Opcodes_0100,
	Opcodes_0101,
	Opcodes_0110,
	Opcodes_0111,
	Opcodes_1000,
	Opcodes_1001,
	Opcodes_1010,
	Opcodes_1011,
	Opcodes_1100,
	Opcodes_1101,
	Opcodes_1110,
	Opcodes_1111,
};

static const char Bads[] = "<Bad size>";

static const char *Ft_sizes[] =		{ "b",	"w",	"l",	Bads	};
static const char *Ft_sizes_b[] =	{ "b",	Bads,	"w",	"l"	};
static const char *Ft_sizes_c[] =	{ Bads,	"b",	"l",	"w"	};

static const char *Ft_condb[] = {
	"t",  "ra",  "hi", "ls", "cc", "cs", "ne", "eq",
	"vc", "vs", "pl", "mi", "ge", "lt", "gt", "le",
};

static const char *Ft_cond[] = {
	"t",  "f",  "hi", "ls", "cc", "cs", "ne", "eq",
	"vc", "vs", "pl", "mi", "ge", "lt", "gt", "le",
};

static const char *Ft_qdata[] = { "0x8", "0x1", "0x2", "0x3",
				  "0x4", "0x5", "0x6", "0x7" };

static const char *Ft_l_or_l[] = { "l", "l" };

static const char *Ft_l_or_w[] = { "l", "w" };
static const char *Ft_w_or_l[] = { "w", "l" };
static const char *Ft_d_or_a[] = { "d", "a" };
static const char *Ft_u_or_s[] = { "u", "s" };
static const char *Ft_i_or_d[] = { "#", "d" };
static const char *Ft_r_or_l[] = { "r", "l" };

static const char *Ft_chkcmp[] = { "cmp2", "chk2" };
static const char *Ft_muldiv[] = { "mul", "div" };
static const char *Ft_bitop[] = { "tst", "chg", "clr", "set" };
static const char *Ft_bfopd[] = { "extu", "exts", "ffo", "ins" };
static const char *Ft_sro[] = { "as", "ls", "rox", "ro" };

static const char *Regnames[] = {
	"d0", "d1", "d2", "d3", "d4", "d5", "d6", "d7",
	"a0", "a1", "a2", "a3", "a4", "a5", "a6", "sp"
};

static word_t *
get_displacement(text, dispsize, p_res, p_bad_address)
word_t *text;
int dispsize;
int *p_res, *p_bad_address;
{
	switch (dispsize) {
	case 0:
	case 1:
		*p_res = 0;
		break;
	case 2:
		*p_res = (short)*text++;
		break;
	case 3:
		*p_res = *(int *)text;
		text += 2;
		break;
	default:
		*p_bad_address = TRUE;
	}
	return text;
}

/*  Masks for fields in the ea extension word.
 *
 *  The first group is common to the brief and full forms.
 */
#define EM_REG(e)	(((e) & 0xf000) >> 12)
#define EM_IS_LONG(e)	( (e) & W12(1,0,0,0, 0,0,0,0, 0,0,0,0))
#define EM_SCALE(e)	(((e) & W12(0,1,1,0, 0,0,0,0, 0,0,0,0)) >> 9)
#define EM_IS_FULL(e)	( (e) & W12(0,0,0,1, 0,0,0,0, 0,0,0,0))

/*  Mask for the brief form.
 */
#define EM_DISP(e)	((e) & 0xff)

/*  Mask for the full form.
 */
#define EM_BS(e)	( (e) & W12(0,0,0,0, 1,0,0,0, 0,0,0,0))
#define EM_IS(e)	( (e) & W12(0,0,0,0, 0,1,0,0, 0,0,0,0))
#define EM_BD_SIZE(e)	(((e) & W12(0,0,0,0, 0,0,1,1, 0,0,0,0)) >> 4)
#define EM_I_OR_IS(e)	( (e) & W12(0,0,0,0, 0,0,0,0, 0,1,1,1))

static word_t *
ext_ea(text, buf, reg, want_output, p_bad_address)
word_t *text;
char *buf;
const char *reg;
int want_output, *p_bad_address;
{
	word_t eword;
	static const char *scales[] = { "", ":2", ":4", ":8" };
	const char *extreg, *scale;
	char bdi[50];
	int bdisp, odisp, bdi_disp, w_or_l;

	*p_bad_address = FALSE;
	eword = *text++;

	if (EM_IS_FULL(eword)) {
		text = get_displacement(text, (int)EM_BD_SIZE(eword), &bdisp,
								p_bad_address);
		if (*p_bad_address)
			return text;
		text = get_displacement(text, (int)EM_I_OR_IS(eword) & 3, &odisp,
								p_bad_address);
		if (*p_bad_address)
			return text;
	}

	if (want_output) {
		extreg = Regnames[EM_REG(eword)];
		w_or_l = EM_IS_LONG(eword) ? 'l' : 'w';
		scale = scales[EM_SCALE(eword)];

		if (EM_IS_FULL(eword)) {
			bdi_disp = ((EM_I_OR_IS(eword) & 7) > 4) ? odisp : bdisp;

			/*  Build the base displacement and index part.
			 */
			if (EM_IS(eword))
				(void) sprintf(bdi, "(%s)", hex(bdi_disp));
			else
				(void) sprintf(bdi, "(%s,%s:%c%s)", hex(bdi_disp),
							     extreg, w_or_l, scale);
			
			if ((EM_I_OR_IS(eword) & 3) == 0)
				(void) sprintf(buf, "%s@%s", reg, bdi);
			else {
				if (EM_I_OR_IS(eword) & 4)
					(void) sprintf(buf, "%s@(%s)@%s",
							       reg, hex(bdisp), bdi);
				else
					(void) sprintf(buf, "%s@%s@(%s)",
							       reg, bdi, hex(odisp));
			}
		}
		else {
			(void) sprintf(buf, "%s@(%s,%s:%c%s)",
						reg, hex((char)EM_DISP(eword)),
						extreg, w_or_l, scale);
		}
	}
	return text;
}

#define EA_SIZE		60

/* ARGSUSED */
static const char *
ff_ea(ds, fieldval, unused_ftab)
dstate_t *ds;
int fieldval;
const char **unused_ftab;
{
	int mode, regno, want_output, had_error, val_known;
	taddr_t val;
	word_t *text, *origtext;
	const char *extreg0, *areg;
	char buf[EA_SIZE];

	mode = (fieldval >> 3) & 07;
	regno = fieldval & 07;
	areg = Regnames[regno | 0x8];
	text = origtext = ds->ds_text + ds->ds_offset;

	val = 0; /* to satisfy gcc */
	val_known = FALSE;
	extreg0 = NULL;

	want_output = ds->ds_want_output;

	switch(mode) {
	case 0:
		if (want_output)
			(void) sprintf(buf, "d%d", regno);
		break;
	case 1:
		if (want_output)
			(void) strcpy(buf, areg);
		break;
	case 2:
		if (want_output)
			(void) sprintf(buf, "%s@", areg);
		break;
	case 3:
		if (want_output)
			(void) sprintf(buf, "%s@+", areg);
		break;
	case 4:
		if (want_output)
			(void) sprintf(buf, "%s@-", areg);
		break;
	case 5:
		val = (short)*text++;
		if (want_output)
			(void) sprintf(buf, "%s@(%s)", areg, hex((int)val));
		break;
	case 6:
		extreg0 = areg;
		break;
	case 7:
		switch(regno) {
		case 0:
			val = (short)*text++;
			val_known = TRUE;
			if (want_output)
				(void) sprintf(buf, "%s:w",
						addr_to_func_and_offset(val, FALSE));
			break;
		case 1:
			val = *(int *)text;
			val_known = TRUE;
			if (want_output)
				(void) sprintf(buf, "%s:l",
						addr_to_func_and_offset(val, FALSE));
			text += 2;
			break;
		case 2:
			val = ds->ds_addr + 2 + (short)*text++;
			val_known = TRUE;
			if (want_output)
				(void) strcpy(buf,
						addr_to_func_and_offset(val, FALSE));
			break;
		case 3:
			extreg0 = "pc";
			break;
		case 4:
			had_error = FALSE;
			switch (ds->ds_size) {
			case SZ_BYTE:
				val = (char)*text++;
				break;
			case SZ_WORD:
				val = (short)*text++;
				break;
			case SZ_LONG:
				val = *(int *)text;
				text += 2;
				break;
			default:
				if (want_output)
					(void) strcpy(buf,
						      "<bad size in address mode>");
				had_error = TRUE;
				break;
			}
			if (want_output && !had_error)
				(void) sprintf(buf, "#%s", hex((int)val));
			break;
		default:
			if (want_output)
				(void) strcpy(buf, "<bad address mode>");
			break;
		}
	}

	if (val_known && ds->ds_itype != IT_NOTJUMP) {
		ds->ds_dstaddr = val;
		ds->ds_dstaddr_known = TRUE;
	}

	if (extreg0 != NULL) {
		int bad_address;

		text = ext_ea(text, buf, extreg0, ds->ds_want_output, &bad_address);
		if (bad_address && want_output)
			(void) strcpy(buf, "<bad address mode>");
	}

	ds->ds_offset += text - origtext;
	return buf;
}

static const char *
ff_moveas(ds, fieldval, ftab)
dstate_t *ds;
int fieldval;
const char **ftab;
{
	char buf[EA_SIZE * 2];
	int ea1, ea2;

	ea1 = fieldval & 077;
	ea2 = ((fieldval & 07000) >> 9) | ((fieldval & 00700) >> 3);
	if (ds->ds_want_output) {
		(void) strcpy(buf, ff_ea(ds, ea1, ftab));
		(void) strcat(buf, ",");
		(void) strcat(buf, ff_ea(ds, ea2, ftab));
		return buf;
	}
	else {
		(void) ff_ea(ds, ea1, ftab);
		(void) ff_ea(ds, ea2, ftab);
		return NULL;
	}
}

/* ARGSUSED */
static const char *
ff_movs(ds, unused_fieldval, unused_ftab)
dstate_t *ds;
int unused_fieldval;
const char **unused_ftab;
{
	static char buf[150];
	const char *ea, *reg;

	ea = ff_ea(ds, (int)(ds->ds_text[0] & 0x3f), (const char **)0);
	if (ds->ds_want_output) {
		reg = Regnames[ds->ds_text[1] >> 12];

		if (ds->ds_text[1] & 0x800)
			(void) sprintf(buf, "%s,%s", reg, ea);
		else
			(void) sprintf(buf, "%s,%s", ea, reg);
		return buf;
	}
	return NULL;
}

/* ARGSUSED */
static const char *
ff_hexbyte(ds, fieldval, unused_ftab)
dstate_t *ds;
int fieldval;
const char **unused_ftab;
{
	static char buf[20];

	if (ds->ds_want_output) {
		(void) sprintf(buf, "0x%x", (char)fieldval);
		return buf;
	}
	return NULL;
}

/* ARGSUSED */
static const char *
ff_num(ds, fieldval, unused_ftab)
dstate_t *ds;
int fieldval;
const char **unused_ftab;
{
	static char buf[20];

	if (ds->ds_want_output) {
		(void) sprintf(buf, "%d", fieldval);
		return buf;
	}
	return NULL;
}

/* ARGSUSED */
static const char *
ff_byte(ds, unused_fieldval, unused_ftab)
dstate_t *ds;
int unused_fieldval;
const char **unused_ftab;
{
	if (ds->ds_want_output)
		return ff_num(ds, (char)ds->ds_text[1], (const char **)0);
	return NULL;
}

/* ARGSUSED */
static const char *
ff_word(ds, unused_fieldval, unused_ftab)
dstate_t *ds;
int unused_fieldval;
const char **unused_ftab;
{
	if (ds->ds_want_output)
		return ff_num(ds, (short)ds->ds_text[1], (const char **)0);
	return NULL;
}

/* ARGSUSED */
static const char *
ff_long(ds, unused_fieldval, unused_ftab)
dstate_t *ds;
int unused_fieldval;
const char **unused_ftab;
{
	if (ds->ds_want_output)
		return ff_num(ds, *(int *)(ds->ds_text + 1), (const char **)0);
	return NULL;
}

/* ARGSUSED */
static const char *
ff_disp16(ds, fieldval, unused_ftab)
dstate_t *ds;
int fieldval;
const char **unused_ftab;
{
	static int want_offset_addrs = -1;


	ds->ds_dstaddr = ds->ds_addr + 2 + (short)fieldval;
	ds->ds_dstaddr_known = TRUE;
	if (ds->ds_want_output) {
		if (want_offset_addrs == -1)
			want_offset_addrs = getenv("OFFSET_ADDRS") != NULL; 
		return addr_to_func_and_offset(ds->ds_dstaddr, want_offset_addrs);
	}
	return NULL;
}

/* ARGSUSED */
static const char *
ff_disp(ds, fieldval, unused_ftab)
dstate_t *ds;
int fieldval;
const char **unused_ftab;
{
	static int want_offset_addrs = -1;
	static char buf[50];
	char *cptr;
	int val;

	cptr = buf;
	if (fieldval == 0) {
		val = (short)ds->ds_text[1];
		++ds->ds_offset;
	}
	else if (fieldval == 0xff) {
		val = *(int *)(ds->ds_text + 1);
		ds->ds_offset += 2;
		*cptr++ = 'l';
	}
	else {
		val = (char)fieldval;
		*cptr++ = 's';
	}
	ds->ds_dstaddr = ds->ds_addr + 2 + val;
	ds->ds_dstaddr_known = TRUE;
	if (ds->ds_want_output) {
		if (want_offset_addrs == -1)
			want_offset_addrs = getenv("OFFSET_ADDRS") != NULL; 
		*cptr++ = ' ';
		*cptr++ = ' ';
		(void) strcpy(cptr, addr_to_func_and_offset(ds->ds_dstaddr,
								want_offset_addrs));
		return buf;
	}
	return NULL;
}

/* ARGSUSED */
static const char *
ff_data(ds, unused_fieldval, unused_ftab)
dstate_t *ds;
int unused_fieldval;
const char **unused_ftab;
{
	char buf[20];
	int val;

	switch (ds->ds_size) {
	case SZ_BYTE:
		val = (char)ds->ds_text[1];
		ds->ds_offset += 1;
		break;
	case SZ_WORD:
		val = (short)ds->ds_text[1];
		ds->ds_offset += 1;
		break;
	case SZ_LONG:
		val = *(int *)(ds->ds_text + 1);
		ds->ds_offset += 2;
		break;
	default:
		if (ds->ds_want_output) {
			(void) strcpy(buf, "<unknown operand size>");
			return buf;
		}
		val = 0; /* to satisfy gcc */
		break;
	}
	if (ds->ds_want_output) {
		(void) sprintf(buf, "0x%x", val);
		return buf;
	}
	return NULL;
}

/* ARGSUSED */
static const char *
ff_size_w(ds, unused_fieldval, unused_ftab)
dstate_t *ds;
int unused_fieldval;
const char **unused_ftab;
{
	ds->ds_size = SZ_WORD;
	return "";
}

static const char *
ff_size(ds, fieldval, ftab)
dstate_t *ds;
int fieldval;
const char **ftab;
{
	static opsize_t sztab[] = { SZ_BYTE, SZ_WORD, SZ_LONG };
	static const char sizechars[] = "bwl";
	const char *sz, *pos;

	sz = ftab[fieldval];

	pos = strchr(sizechars, *sz);
	if (sz[1] != '\0' || pos == NULL)
		panic("bad size in ff_size");
	if (ds->ds_size != SZ_UNKNOWN)
		panic("multiple sizes in ff_size");
	ds->ds_size = sztab[pos - sizechars];
	return sz;
}

/* ARGSUSED */
static const char *
ff_tab(unused_ds, fieldval, ftab)
dstate_t *unused_ds;
int fieldval;
const char **ftab;
{
	return ftab[fieldval];
}

/* ARGSUSED */
static const char *
ff_regmask(ds, fieldval, unused_ftab)
dstate_t *ds;
int fieldval;
const char **unused_ftab;
{
	char buf[3 * 16 + 1];
	char *cptr;
	int i;

	if (ds->ds_want_output) {
		cptr = buf;
		for (i = 0; i < 8; ++i, fieldval >>= 1) {
			if (fieldval & 1) {
				(void) sprintf(cptr, "/d%d", i);
				cptr += 3;
			}
		}
		for (i = 0; i < 8; ++i, fieldval >>= 1) {
			if (fieldval & 1) {
				(void) sprintf(cptr, "/a%d", i);
				cptr += 3;
			}
		}
		*cptr = '\0';
		return buf + 1;
	}
	return NULL;
}

/* ARGSUSED */
static const char *
ff_ctrlreg(unused_ds, fieldval, unused_ftab)
dstate_t *unused_ds;
int fieldval;
const char **unused_ftab;
{

	static char buf[100];

	switch(fieldval) {
		case 0x000:	return "SFC";
		case 0x001:	return "DFC";
		case 0x002:	return "CACR";
		case 0x800:	return "USP";
		case 0x801:	return "VBR";
		case 0x802:	return "CAAR";
		case 0x803:	return "MSP";
		case 0x804:	return "ISP";
	}
	(void) sprintf(buf, "<bad control regnum %s>", hex(fieldval));
	return buf;
}

static field_t Fieldtab[] = {
	"ea",			0,	5,0,	ff_ea,		NULL,
	"moveas11_0",		0,	11,0,	ff_moveas,	NULL,

	"sz",			0,	7,6,	ff_size,	Ft_sizes,
	"sz10_9",		0,	10,9,	ff_size,	Ft_sizes,
	"szb10_9",		0,	10,9,	ff_size,	Ft_sizes_b,
	"szc13_12",		0,	13,12,	ff_size,	Ft_sizes_c,
	"szw",			1,	0,0,	ff_size_w,	NULL,/* BOGUS */
	"forcel",		1,	0,0,	ff_size,	Ft_l_or_l,/* BOGUS */

	"byte",			1,	7,0,	ff_byte,	NULL,
	"word",			1,	15,0,	ff_word,	NULL,
	"long",			1,	-1,-1,	ff_long,	NULL,
	"data",			1,	-1,-1,	ff_data,	NULL,
	"disp",			0,	7,0,	ff_disp,	NULL,
	"disp16",		1,	15,0,	ff_disp16,	NULL,

	"x7_0",			0,	7,0,	ff_hexbyte,	NULL,
	"11_9",			0,	11,9,	ff_num,		NULL,
	"7_0",			0,	7,0,	ff_num,		NULL,
	"5_0",			0,	5,0,	ff_num,		NULL,
	"3_0",			0,	3,0,	ff_num,		NULL,
	"2_0",			0,	2,0,	ff_num,		NULL,
	"1:14_12",		1,	14,12,	ff_num,		NULL,
	"1:10_6",		1,	10,6,	ff_num,		NULL,
	"1:8_6",		1,	8,6,	ff_num,		NULL,
	"1:5_0",		1,	5,0,	ff_num,		NULL,
	"1:4_0",		1,	4,0,	ff_num,		NULL,
	"1:2_0",		1,	2,0,	ff_num,		NULL,
	"2:14_12",		2,	14,12,	ff_num,		NULL,
	"2:8_6",		2,	8,6,	ff_num,		NULL,
	"2:2_0",		2,	2,0,	ff_num,		NULL,

	"qdata2_0",		0,	2,0,	ff_tab,		Ft_qdata,
	"qdata11_9",		0,	11,9,	ff_tab,		Ft_qdata,

	"1:w/l10",		1,	10,10,	ff_tab,		Ft_w_or_l,

	"u/s8",			0,	8,8,	ff_tab,		Ft_u_or_s,
	"1:u/s11",		1,	11,11,	ff_tab,		Ft_u_or_s,
	"l/w7",			0,	7,7,	ff_size,	Ft_l_or_w,
	"w/l6",			0,	6,6,	ff_size,	Ft_w_or_l,
	"w/l8",			0,	8,8,	ff_size,	Ft_w_or_l,
	"d/a3",			0,	3,3,	ff_tab,		Ft_d_or_a,
	"1:d/a15",		1,	15,15,	ff_tab,		Ft_d_or_a,
	"2:d/a15",		1,	15,15,	ff_tab,		Ft_d_or_a,
	"i/d5",			0,	5,5,	ff_tab,		Ft_i_or_d,
	"1:i/d5",		1,	5,5,	ff_tab,		Ft_i_or_d,
	"1:i/d11",		1,	11,11,	ff_tab,		Ft_i_or_d,
	"r/l8",			0,	8,8,	ff_tab,		Ft_r_or_l,

	"cond11_8",		0,	11,8,	ff_tab,		Ft_cond,	
	"condb11_8",		0,	11,8,	ff_tab,		Ft_condb,	

	"bitop7_6",		0,	7,6,	ff_tab,		Ft_bitop,
	"bfop10_9",		0,	10,9,	ff_tab,		Ft_bitop,
	"bfopd10_9",		0,	10,9,	ff_tab,		Ft_bfopd,
	"sro10_9",		0,	10,9,	ff_tab,		Ft_sro,
	"sro4_3",		0,	4,3,	ff_tab,		Ft_sro,
	"1:chkcmp11_11",	1,	11,11,	ff_tab,		Ft_chkcmp,	
	"mul/div6",		0,	6,6,	ff_tab,		Ft_muldiv,	

	"movs_operands",	0,	5,0,	ff_movs,	NULL,
	"regmask",		1,	15,0,	ff_regmask,	NULL,
	"1:ctrlreg",		1,	11,0,	ff_ctrlreg,	NULL,
};

#define FIELDTAB_SIZE	(sizeof(Fieldtab) / sizeof(Fieldtab[0]))

static field_t *
lookup_field(name)
const char *name;
{
	int i;

	for (i = 0; i < FIELDTAB_SIZE; ++i)
		if (strcmp(Fieldtab[i].fe_name, name) == 0)
			return Fieldtab + i;
	return NULL;
}

#define LBRACE	'{'	/* To avoid upsetting % in vi */
#define RBRACE	'}'	/* To avoid upsetting % in vi */

static void
disassemble_inst_internal(ds, p_buf)
register dstate_t *ds;
const char **p_buf;
{
	static char buf[128];
	opcode_t *oc;
	word_t word, mask;
	char *cptr, *end, *olim, *optr;
	const char *field;
	field_t *fe;
	int want_output, width, fieldval;

	if ((ds->ds_addr % 1) != 0)
		panic("odd addr in doi");
	
	want_output = ds->ds_want_output;

	ds->ds_dstaddr_known = FALSE;
	ds->ds_size = SZ_UNKNOWN;

	/*  Find the opcode definition.
	 */
	word = *ds->ds_text & 0xfff;
	for (oc = Opcodes[*ds->ds_text >> 12]; oc->oc_const_name != NULL; ++oc)
		if ((word & ~oc->oc_mask) == oc->oc_word)
			break;
	if (oc->oc_const_name == NULL) {
		ds->ds_offset = 1;
		if (ds->ds_want_output)
			(void) sprintf(buf, ".word\t0x%x", (short)*ds->ds_text);
		return;
	}

	/*  The oc_const_name field is of type "const char *" and is initialised
	 *  by a string constant.  We are going to modify it below, so
	 *  make a writeable copy.
	 *
	 *  BUG: the modification of the string is a flawed optimisation.
	 *       It saves us from looking up, say "ea" again for a
	 *       particular instruction.  What would be nice is a data
	 *	 structure where we only have to lookup "ea" once and
	 *	 then remember it for *all* instructions.
	 */
	if (oc->oc_name == NULL)
		oc->oc_name = strsave(oc->oc_const_name);

	ds->ds_offset = oc->oc_nwords;

	optr = buf;
	olim = buf + sizeof(buf) - 1;

	cptr = oc->oc_name;
	if (*cptr != '*')
		ds->ds_itype = IT_NOTJUMP;
	else {
		switch(cptr[1]) {
		case 'b':
			ds->ds_itype = IT_BRANCH;
			break;
		case 'c':
			ds->ds_itype = IT_CALL;
			break;
		default:
			panic("bad char in doi");
		}
		if (cptr[2] != ' ')
			panic("missing space in doi");
		cptr += 3;
	}

	for (; *cptr != '\0'; ++cptr) {
		if (*cptr != LBRACE || (cptr != oc->oc_name && cptr[-1] == '\\')) {
			if (want_output && optr < olim)
				*optr++ = *cptr;
			continue;
		}

		if ((end = strchr(cptr, RBRACE)) == NULL)
			panic("bad opdef");

		*end = '\0';
		if (cptr[1] & 0x80)
			fe = Fieldtab + (cptr[1] & 0x7f);
		else {
			if ((fe = lookup_field(cptr + 1)) == NULL)
				panic("undefined field name");
			if (fe - Fieldtab > 0x7f)
				panic("field index too large");
			cptr[1] = 0x80 | (fe - Fieldtab);
		}
		*end = RBRACE;
		cptr = end;

		width = fe->fe_startbit - fe->fe_stopbit + 1;
		mask = ((1 << width) - 1);
		fieldval = (ds->ds_text[fe->fe_wordnum] >> fe->fe_stopbit) & mask;

		field = (*fe->fe_func)(ds, fieldval, fe->fe_tab);
		if (want_output && optr != olim) {
			if (optr + strlen(field) < olim) {
				(void) strcpy(optr, field);
				optr += strlen(field);
			}
			else {
				*optr = '\0';
				optr = olim;
			}
		}
	}
	if (want_output) {
		*optr = '\0';
		*p_buf = buf;

		/*  More munging to look like dbx.
		 */
		if ((cptr = strchr(buf, ' ')) != NULL) {
			while (*cptr == ' ')
				++cptr;
			cptr[-1] = '\t';
		}
	}
}

const char *
disassemble_one_instruction(addr, ctext, p_buf)
taddr_t addr;
const char *ctext, **p_buf;
{
	dstate_t dsbuf;

	dsbuf.ds_want_output = TRUE;
	dsbuf.ds_text = (word_t *)ctext;
	dsbuf.ds_addr = addr;
	disassemble_inst_internal(&dsbuf, p_buf);
	return (char *)(dsbuf.ds_text + dsbuf.ds_offset);
}

jump_t *
get_jumps(addr, ctext, len, want_calls, want_branches)
taddr_t addr;
const char *ctext;
int len, want_calls, want_branches;
{
	static jump_t *jtab;
	static int jtab_size = 0;
	const char *junk_buf;
	dstate_t dsbuf;
	word_t *lim;
	int njumps;

	if (jtab_size == 0) {
		jtab_size = 16;
		jtab = (jump_t *)e_malloc((jtab_size + 1) * sizeof(jump_t));
	}
	if (len % sizeof(word_t) != 0)
		panic("bad len in get_jumps");
	njumps = 0;

	dsbuf.ds_want_output = FALSE;
	dsbuf.ds_text = (word_t *)ctext;
	dsbuf.ds_addr = addr;
	lim = dsbuf.ds_text + len / sizeof(word_t);

	while (dsbuf.ds_text < lim) {

		/*  On Suns, we can't tell where the text of a function
		 *  ends if it is the last function in a segment, and
		 *  the contents of the padding to the next boundary
		 *  can confuse us (e.g. with bad address mode diagnostics).
		 *
		 *  Thus this hack: if this opcode is an rts and we have
		 *  not yet seen any jumps, frig things so the loop will
		 *  exit after this instruction.
		 */
		if (njumps == 0 && *dsbuf.ds_text == RTS)
			lim = dsbuf.ds_text + 1;
		
		/*  Ugly feature of 68k intruction set - you can get
		 *  an inline table of jump addresses.  We cannot
		 *  determine the length of this table without very
		 *  ugly hacks, so we punt if we see the sort of
		 *  jump that switch does.  Just drop out of the
		 *  loop after the instruction.
		 */
		if (*dsbuf.ds_text == JMP_SWITCH)
			lim = dsbuf.ds_text + 1;

		disassemble_inst_internal(&dsbuf, &junk_buf);
		if ((want_calls && dsbuf.ds_itype == IT_CALL) ||
		    (want_branches && dsbuf.ds_itype == IT_BRANCH)) {
			if (njumps >= jtab_size) {
				jtab_size *= 2;
				jtab = (jump_t *)e_realloc((char *)jtab,
						  (jtab_size + 1) * sizeof(jump_t));
			}
			jtab[njumps].ju_addr = dsbuf.ds_addr;
			jtab[njumps].ju_type = (dsbuf.ds_itype == IT_CALL) ?
								JT_CALL : JT_BRANCH;
			jtab[njumps].ju_unconditional = FALSE;	/* punt */
			if (dsbuf.ds_dstaddr_known)
				jtab[njumps].ju_dstaddr = dsbuf.ds_dstaddr;
			else
				jtab[njumps].ju_dstaddr = 0;
			++njumps;
		}

		dsbuf.ds_addr += dsbuf.ds_offset * sizeof(word_t);
		dsbuf.ds_text += dsbuf.ds_offset;
	}
	jtab[njumps].ju_type = JT_END;
	return jtab;
}

#ifdef TESTING
#include <stdio.h>

static const char *
addr_to_func_and_offset(addr, flag)
taddr_t addr;
int flag;
{
	panic("not used with -DTESTING");
}

static void
error(fmt, a1, a2, a3, a4, a5)
const char *fmt;
int a1, a2, a3, a4, a5;
{
	fprintf(stderr, "Fatal internal error: ");
	fprintf(stderr, fmt, a1, a2, a3, a4, a5);
	fputs("Aborting... ", stderr);
	fflush(stderr);
	abort();
}

static void
check_fieldnames_unique()
{
	int i, j;

	for (i = 0; i < FIELDTAB_SIZE; ++i)
		for (j = 0; j < FIELDTAB_SIZE; ++j)
			if (i != j &&
			    strcmp(Fieldtab[i].fe_name, Fieldtab[j].fe_name) == 0)
				printf("Fields %d and %d both called %s\n", i, j,
								Fieldtab[i].fe_name);
}

static void
check_fields_in_name_defined(name, mask)
const char *name;
word_t mask;
{
	char *s, *end;
	field_t *fe;
	int width;
	unsigned int val;

	for (s = name; *s != '\0'; ++s) {
		if (*s != LBRACE || (s != name && s[-1] == '\\'))
			continue;

		if ((end = strchr(s, RBRACE)) == NULL) {
			printf("Bad opdef %s\n", name);
			return;
		}

		*end = '\0';
		if ((fe = lookup_field(s + 1)) == NULL)
			printf("Undefined field name %s\n", s + 1);
		*end = RBRACE;
		s = end;

		if (fe == NULL || fe->fe_wordnum != 0)
			continue;
		
		if (fe->fe_startbit > 15 || fe->fe_startbit < 0 ||
		    fe->fe_stopbit > 15 || fe->fe_stopbit < 0 ||
		    fe->fe_startbit < fe->fe_stopbit) {
			printf("Startbit (%d) or stopbit (%d) bad in field %s\n",
				      fe->fe_startbit, fe->fe_stopbit, fe->fe_name);
			continue;
		}

		width = fe->fe_startbit - fe->fe_stopbit + 1;
		val = ((1 << width) - 1) << fe->fe_stopbit;

		if ((mask & val) != val)
			printf("Some of the bits for {%s} in %s are fixed\n",
								fe->fe_name, name);
		mask &= ~val;
	}

	if (mask != 0)
		printf("Mask for %s has free variable bits (0x%x)\n", name, mask);
}

static void
check_fields_defined()
{
	int msb;
	opcode_t *oc;

	for (msb = 0; msb < 0x10; ++msb) {
		for (oc = Opcodes[msb]; oc->oc_name != NULL; ++oc) {
			oc->oc_name = strsave(oc->oc_const_name);
			check_fields_in_name_defined(oc->oc_name, oc->oc_mask);
		}
	}
}

static int
hit_xmask(xmasks, word)
register xmask_t *xmasks;
word_t word;
{
	for (; xmasks->xm_mask != 0; ++xmasks)
		if ((xmasks->xm_mask & word) == xmasks->xm_val)
			return TRUE;
	return FALSE;
}

static void
check_opcodes_unique()
{
	register opcode_t *oc, *octab;
	opcode_t **mtab;
	char *fmt;
	register word_t word;
	word_t m1, m2;
	int msb, nmatches, max_size, i;

	max_size = 0;
	for (msb = 0; msb < 0x10; ++msb) {
		for (oc = Opcodes[msb]; oc->oc_const_name != NULL; ++oc)
			;
		if (oc - Opcodes[msb] > max_size)
			max_size = oc - Opcodes[msb];
	}
	mtab = (opcode_t **)malloc((max_size + 1) * sizeof(opcode_t *));
	if (mtab == NULL)
		panic("malloc failed in cou");

	for (msb = 0; msb < 0x10; ++msb) {
		octab = Opcodes[msb];
		for (word = 0; word < 0x1000; ++word) {
			nmatches = 0;
			for (oc = octab; oc->oc_const_name !=  NULL; ++oc) {
				if ((word & ~oc->oc_mask) == oc->oc_word &&
				    (nmatches == 0 || oc->oc_xmask == NULL ||
					         !hit_xmask(oc->oc_xmask, word)))
					mtab[nmatches++] = oc;
			}

			m1 = mtab[0]->oc_mask;
			for (i = 1; i < nmatches; ++i) {
				m2 = mtab[i]->oc_mask;
				if (m1 == m2 || (m1 | m2) != m2)
					break;
			}

			if (i < nmatches) {
				printf("0x%01x%03x: %s", msb, word,
							mtab[0]->oc_const_name); 
				for (i = 1; i < nmatches; ++i) {
					m2 = mtab[i]->oc_mask;
					if (m1 == m2)
						fmt = " {%s}";
					else if ((m1 | m2) == m2)
						fmt = " %s";
					else if ((m1 | m2) == m1)
						fmt = " (%s)";
					else
						fmt = " [%s]";
					printf(fmt, mtab[i]->oc_const_name);
				}
				putchar('\n');
			}
		}
	}
}

main()
{
	check_opcodes_unique();
	check_fieldnames_unique();
	check_fields_defined();
}
#endif /* TESTING */
#endif /* ARCH_SUN3 */
