/* ci_machine.h - opcodes and definitions for the C interpreter machine */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)ci_machine.h	1.12 17/9/92 (UKC) */

typedef unsigned char textword_t;

typedef unsigned long stackword_t;

typedef unsigned long textaddr_t;

#define FLOAT_NBYTES	sizeof(float)
#define DOUBLE_NBYTES	sizeof(double)

#define FLOAT_NSLOTS	(FLOAT_NBYTES / sizeof(stackword_t))
#define DOUBLE_NSLOTS	(DOUBLE_NBYTES / sizeof(stackword_t))

/*  See ci_execute.c for the meanings of the individual opcodes.
 *
 *  Warning: the code generation generates byte, word and long forms
 *           of some opcodes by arithmetic on these enum values.
 *	     The order of opcodes that are grouped between --- comments
 *	     is significant, and no new opcodes should be inserted into
 *	     such groups.
 */
typedef enum opcodeen {
/* ----------------------- */
	OC_CALL_B,
	OC_CALL_W,
	OC_CALL_L,
/* ----------------------- */
	OC_POPMANY_B,
	OC_POPMANY_W,
	OC_POPMANY_L,
/* ----------------------- */
	OC_TRACELINK_B,
	OC_TRACELINK_W,
	OC_TRACELINK_L,
/* ----------------------- */
	OC_LINK_B,
	OC_LINK_W,
	OC_LINK_L,
/* ----------------------- */
	OC_PROC_MEMCPY_B,
	OC_PROC_MEMCPY_W,
	OC_PROC_MEMCPY_L,
/* ----------------------- */
	OC_MEMCPY_B,
	OC_MEMCPY_W,
	OC_MEMCPY_L,
/* ----------------------- */
	OC_PUSH_BYTES,
	OC_PROC_PUSH_BYTES,
	OC_RESERVE_BYTES,
/* ----------------------- */
	OC_PROC_ASSIGN_BYTE,
	OC_PROC_ASSIGN_WORD,
	OC_PROC_ASSIGN_LONG,
	OC_PROC_ASSIGN_FLOAT,
	OC_PROC_ASSIGN_DOUBLE,
/* ----------------------- */
	OC_ASSIGN_BYTE,
	OC_ASSIGN_WORD,
	OC_ASSIGN_LONG,
	OC_ASSIGN_FLOAT,
	OC_ASSIGN_DOUBLE,
/* ----------------------- */
	OC_PROC_ASSIGN_AND_PUSH_BYTE,
	OC_PROC_ASSIGN_AND_PUSH_WORD,
	OC_PROC_ASSIGN_AND_PUSH_LONG,
	OC_PROC_ASSIGN_AND_PUSH_FLOAT,
	OC_PROC_ASSIGN_AND_PUSH_DOUBLE,
/* ----------------------- */
	OC_ASSIGN_AND_PUSH_BYTE,
	OC_ASSIGN_AND_PUSH_WORD,
	OC_ASSIGN_AND_PUSH_LONG,
	OC_ASSIGN_AND_PUSH_FLOAT,
	OC_ASSIGN_AND_PUSH_DOUBLE,
/* ----------------------- */
	OC_PROC_DEREF_SIGNED_BYTE,
	OC_PROC_DEREF_SIGNED_WORD,
	OC_PROC_DEREF_SIGNED_LONG,
	OC_PROC_DEREF_FLOAT,
	OC_PROC_DEREF_DOUBLE,
/* ----------------------- */
	OC_PROC_DEREF_UNSIGNED_BYTE,
	OC_PROC_DEREF_UNSIGNED_WORD,
	OC_PROC_DEREF_UNSIGNED_LONG,
/* ----------------------- */
	OC_DEREF_SIGNED_BYTE,
	OC_DEREF_SIGNED_WORD,
	OC_DEREF_SIGNED_LONG,
	OC_DEREF_FLOAT,
	OC_DEREF_DOUBLE,
/* ----------------------- */
	OC_DEREF_UNSIGNED_BYTE,
	OC_DEREF_UNSIGNED_WORD,
	OC_DEREF_UNSIGNED_LONG,
/* ----------------------- */
	OC_NEG_CONSTPUSH_B,
	OC_NEG_CONSTPUSH_W,
	OC_NEG_CONSTPUSH_L,
/* ----------------------- */
	OC_CONSTPUSH_B,
	OC_CONSTPUSH_W,
	OC_CONSTPUSH_L,
/* ----------------------- */
	OC_PUSH_FLOAT_CONST,
	OC_PUSH_DOUBLE_CONST,
/* ----------------------- */
	OC_PUSH_ADDR_B,
	OC_PUSH_ADDR_W,
	OC_PUSH_ADDR_L,
/* ----------------------- */
	OC_CHECK_SP_B,
	OC_CHECK_SP_W,
	OC_CHECK_SP_L,

/* ----------------------- */
	OC_PROC_PUSH_FP_ADDR_B,
	OC_PROC_PUSH_FP_ADDR_W,
	OC_PROC_PUSH_FP_ADDR_L,
/* ----------------------- */
	OC_PROC_PUSH_AP_ADDR_B,
	OC_PROC_PUSH_AP_ADDR_W,
	OC_PROC_PUSH_AP_ADDR_L,
/* ----------------------- */
	OC_PUSH_STACKADDR_B,
	OC_PUSH_STACKADDR_W,
	OC_PUSH_STACKADDR_L,
/* ----------------------- */

/* ----------------------- */
	OC_PUSH_SIGNED_BYTE_AT_STACKADDR_B,
	OC_PUSH_SIGNED_BYTE_AT_STACKADDR_W,
	OC_PUSH_SIGNED_BYTE_AT_STACKADDR_L,

	OC_PUSH_SIGNED_SHORT_AT_STACKADDR_B,
	OC_PUSH_SIGNED_SHORT_AT_STACKADDR_W,
	OC_PUSH_SIGNED_SHORT_AT_STACKADDR_L,

	OC_PUSH_SIGNED_LONG_AT_STACKADDR_B,
	OC_PUSH_SIGNED_LONG_AT_STACKADDR_W,
	OC_PUSH_SIGNED_LONG_AT_STACKADDR_L,

	OC_PUSH_FLOAT_AT_STACKADDR_B,
	OC_PUSH_FLOAT_AT_STACKADDR_W,
	OC_PUSH_FLOAT_AT_STACKADDR_L,

	OC_PUSH_DOUBLE_AT_STACKADDR_B,
	OC_PUSH_DOUBLE_AT_STACKADDR_W,
	OC_PUSH_DOUBLE_AT_STACKADDR_L,
/* ----------------------- */
	OC_PUSH_UNSIGNED_BYTE_AT_STACKADDR_B,
	OC_PUSH_UNSIGNED_BYTE_AT_STACKADDR_W,
	OC_PUSH_UNSIGNED_BYTE_AT_STACKADDR_L,

	OC_PUSH_UNSIGNED_SHORT_AT_STACKADDR_B,
	OC_PUSH_UNSIGNED_SHORT_AT_STACKADDR_W,
	OC_PUSH_UNSIGNED_SHORT_AT_STACKADDR_L,

	OC_PUSH_UNSIGNED_LONG_AT_STACKADDR_B,
	OC_PUSH_UNSIGNED_LONG_AT_STACKADDR_W,
	OC_PUSH_UNSIGNED_LONG_AT_STACKADDR_L,
/* ----------------------- */

/* ----------------------- */
	OC_PUSH_UNSIGNED_BYTE_AT_ADDR_B,
	OC_PUSH_UNSIGNED_BYTE_AT_ADDR_W,
	OC_PUSH_UNSIGNED_BYTE_AT_ADDR_L,

	OC_PUSH_UNSIGNED_SHORT_AT_ADDR_B,
	OC_PUSH_UNSIGNED_SHORT_AT_ADDR_W,
	OC_PUSH_UNSIGNED_SHORT_AT_ADDR_L,

	OC_PUSH_UNSIGNED_LONG_AT_ADDR_B,
	OC_PUSH_UNSIGNED_LONG_AT_ADDR_W,
	OC_PUSH_UNSIGNED_LONG_AT_ADDR_L,
/* ----------------------- */
	OC_PUSH_SIGNED_BYTE_AT_ADDR_B,
	OC_PUSH_SIGNED_BYTE_AT_ADDR_W,
	OC_PUSH_SIGNED_BYTE_AT_ADDR_L,

	OC_PUSH_SIGNED_SHORT_AT_ADDR_B,
	OC_PUSH_SIGNED_SHORT_AT_ADDR_W,
	OC_PUSH_SIGNED_SHORT_AT_ADDR_L,

	OC_PUSH_SIGNED_LONG_AT_ADDR_B,
	OC_PUSH_SIGNED_LONG_AT_ADDR_W,
	OC_PUSH_SIGNED_LONG_AT_ADDR_L,

	OC_PUSH_FLOAT_AT_ADDR_B,
	OC_PUSH_FLOAT_AT_ADDR_W,
	OC_PUSH_FLOAT_AT_ADDR_L,

	OC_PUSH_DOUBLE_AT_ADDR_B,
	OC_PUSH_DOUBLE_AT_ADDR_W,
	OC_PUSH_DOUBLE_AT_ADDR_L,
/* ----------------------- */

/* ----------------------- */
	OC_INSERT_SIGNED_BITFIELD,
	OC_INSERT_UNSIGNED_BITFIELD,
/* ----------------------- */
	OC_EXTRACT_SIGNED_BITFIELD,
	OC_EXTRACT_UNSIGNED_BITFIELD,
/* ----------------------- */

	OC_TRACERET,
	OC_TRACERET_WORD,
	OC_TRACERET_FLOAT,
	OC_TRACERET_DOUBLE,
	OC_TRACERET_STRUCT,

	OC_RET,
	OC_RET_WORD,
	OC_RET_FLOAT,
	OC_RET_DOUBLE,
	OC_RET_STRUCT,

	OC_PUSH_STRUCTRET_ADDR,

	OC_MULTI_ARROW,

	OC_UNRESOLVED_JUMP,
	OC_JUMP,
	OC_JUMP_IF_NON_ZERO,
	OC_JUMP_IF_ZERO,

	OC_SWITCH_ON_TABLE,
	OC_SWITCH_ON_CHAIN_B,
	OC_SWITCH_ON_CHAIN_W,
	OC_SWITCH_ON_CHAIN_L,

	OC_PUSH_WORD_RETVAL,
	OC_PUSH_FLOAT_RETVAL,
	OC_PUSH_DOUBLE_RETVAL,
	OC_CALL_INDIRECT,
	OC_TRAP,

	OC_DUP,
	OC_DUP_BACK_ONE,
	OC_RESERVE_SLOT,
	OC_POP,

	OC_CVT_TO_BOOL,

	OC_BITWISE_AND,
	OC_BITWISE_XOR,
	OC_BITWISE_OR,
	OC_BITWISE_NOT,
	OC_LOGICAL_NOT,
	OC_LSHIFT,
	OC_RSHIFT,

	OC_MOD,

/* ----------------------- */
	OC_UNARY_MINUS,
	OC_FLOAT_UNARY_MINUS,
	OC_DOUBLE_UNARY_MINUS,
/* ----------------------- */
	OC_MUL_SIGNED,
	OC_MUL_FLOATS,
	OC_MUL_DOUBLES,
/* ----------------------- */
	OC_DIV_SIGNED,
	OC_DIV_FLOATS,
	OC_DIV_DOUBLES,
/* ----------------------- */
	OC_CHKDIV_SIGNED,
	OC_CHKDIV_FLOATS,
	OC_CHKDIV_DOUBLES,
/* ----------------------- */
	OC_ADD,
	OC_ADD_FLOATS,
	OC_ADD_DOUBLES,
/* ----------------------- */
	OC_SUB,
	OC_SUB_FLOATS,
	OC_SUB_DOUBLES,
/* ----------------------- */
	OC_IS_EQUAL,
	OC_FLOAT_IS_EQUAL,
	OC_DOUBLE_IS_EQUAL,
/* ----------------------- */
	OC_NOT_EQUAL,
	OC_FLOAT_NOT_EQUAL,
	OC_DOUBLE_NOT_EQUAL,
/* ----------------------- */
	OC_LESS_SIGNED,
	OC_FLOAT_LESS,
	OC_DOUBLE_LESS,
/* ----------------------- */
	OC_GREATER_SIGNED,
	OC_FLOAT_GREATER,
	OC_DOUBLE_GREATER,
/* ----------------------- */
	OC_LESS_OR_EQUAL_SIGNED,
	OC_FLOAT_LESS_OR_EQUAL,
	OC_DOUBLE_LESS_OR_EQUAL,
/* ----------------------- */
	OC_GREATER_OR_EQUAL_SIGNED,
	OC_FLOAT_GREATER_OR_EQUAL,
	OC_DOUBLE_GREATER_OR_EQUAL,
/* ----------------------- */
	OC_MUL_UNSIGNED,
	OC_DIV_UNSIGNED,
	OC_CHKDIV_UNSIGNED,
	OC_LESS_UNSIGNED,
	OC_GREATER_UNSIGNED,
	OC_LESS_OR_EQUAL_UNSIGNED,
	OC_GREATER_OR_EQUAL_UNSIGNED,

	OC_CVT_LONG_TO_DOUBLE,
	OC_CVT_FLOAT_TO_DOUBLE,
	OC_CVT_LONG_TO_FLOAT,
	OC_CVT_DOUBLE_TO_FLOAT,
	OC_CVT_FLOAT_TO_LONG,
	OC_CVT_DOUBLE_TO_LONG,

	OC_LAST_OPCODE		/* for sizing arrays of opcodes, etc */
} opcode_t;

#define BYTE_FORM_OFFSET	0
#define SHORT_FORM_OFFSET	1
#define LONG_FORM_OFFSET	2
#define FLOAT_FORM_OFFSET	3
#define DOUBLE_FORM_OFFSET	4

#define N_OPCODE_SIZES		3

#define BYTE_FORM(o)		((opcode_t)((int)(o) + BYTE_FORM_OFFSET))
#define SHORT_FORM(o)		((opcode_t)((int)(o) + SHORT_FORM_OFFSET))
#define LONG_FORM(o)		((opcode_t)((int)(o) + LONG_FORM_OFFSET))

#define MAX_BYTE	0xff
#define MAX_WORD	0xffff

typedef struct reg_relocst {
	long rr_addr;		/* relative to ma_text */
	int rr_regno;
	struct reg_relocst *rr_next;
} reg_reloc_t;

typedef struct machinest {
	textword_t *ma_text;
	char *ma_data;
	char *ma_data_copy;
	textaddr_t ma_entry_point;
	textword_t *ma_pc;
	stackword_t *ma_sp;
	char *ma_maxsp;
	unsigned long *ma_minsp;
	char *ma_fp;
	textaddr_t *ma_funcaddrs;
	int ma_nfuncs;
	long ma_text_size;
	long ma_data_size;
	long ma_bss_size;
	long ma_stack_size;
	long ma_funclevel;	/* used only when function call tracing is on */
	unsigned long (*ma_get_regaddr_proc)PROTO((char *arg, int regno));
	char *ma_get_regaddr_proc_arg;
	reg_reloc_t *ma_reg_relocs;
	alloc_id_t ma_alloc_id;
} machine_t;

/*  The data space looks like this.
 *
 *			 ----------------------- -----------
 *	   ma_minsp --> |     			|	|
 *			|			|	|
 *			|	stack		|	|-- ma_stack_size
 *			|			|	|
 *	   ma_minsp --> |     			|	|
 *			|-----------------------|-----------
 *			|			|	|
 *			|			|	|
 *			|			|	|
 *			|	bss		|	|-- ma_bss_size
 *			|			|	|
 *			|			|	|
 *	     		|     			|	|
 *			|-----------------------|-----------
 *			|			|	|
 *			|			|	|
 *			|  initialised data	|	|-- ma_data_size
 *			|			|	|
 *	    ma_data --> |     			|	|
 *			 ----------------------- -----------
 *
 *   The text is in a seperate chunk of memory, and looks like this:
 *
 *			 -----------------------|-----------
 *			|			|	|
 *   ma_entry_point -->	|			|	|
 *			|  text			|	|-- ma_text_size
 *			|			|	|
 *	    ma_text --> |     			|	|
 *			 ----------------------- -----------
 */

/*  The (unsigned) casts here are because unsigned char promotes to
 *  int under ANSI C, and the bitwise operators are (I think) not as
 *  well defined for int as for unsigned.  The casts also suppress
 *  buggy warning messages from the Ultrix C compiler.
 */
#define GETWORD(pc)	((unsigned)*(pc) | ((unsigned)(pc)[1] << 8))
#define GETLONG(pc)	((unsigned)*(pc) | ((unsigned)(pc)[1] << 8) | \
			 ((unsigned)(pc)[2] << 16) | ((unsigned)(pc)[3] << 24))
