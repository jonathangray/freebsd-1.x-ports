/* ci_opcodes.c - routines to write and disassemble interpreter code */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_ci_opcodes_c_sccsid[] = "@(#)ci_opcodes.c	1.18 26/7/92 (UKC)";

#include <stdio.h>

#include <string.h>

#include <local/ukcprog.h>
#include <mtrprog/so.h>

#include "ups.h"
#include "symtab.h"
#include "ci.h"
#include "ci_parse.h"
#include "ci_types.h"
#include "ci_machine.h"
#include "ci_compile.h"
#include "ci_util.h"

static void show_generic PROTO((machine_t *ma, opcode_t byte_opcode,
					const char *opname, textword_t **p_pc));

void
ci_code_byte(tx, byte)
text_t *tx;
long byte;
{
	if ((textword_t)byte != byte)
		ci_panic("byte out of range in cb");
	
	if (tx->tx_pc >= tx->tx_pclim) {
		do {
			tx->tx_pclim *= 2;
		} while (tx->tx_pc >= tx->tx_pclim);
		tx->tx_text = (textword_t *)e_realloc((char *)tx->tx_text,
				  (size_t)(tx->tx_pclim * sizeof(textword_t)));
	}
	tx->tx_text[tx->tx_pc++] = byte;
}

void
ci_code_word(tx, word)
text_t *tx;
long word;
{
	ci_code_byte(tx, word & 0xff);
	ci_code_byte(tx, (word >> 8) & 0xff);
}

void
ci_code_long(tx, l)
text_t *tx;
long l;
{
	ci_code_word(tx, l & 0xffff);
	ci_code_word(tx, (l >> 16) & 0xffff);
}

void
ci_code_opcode(tx, opcode)
text_t *tx;
opcode_t opcode;
{
	int tmpspace;

	if (tx->tx_flags & CI_CP_CHECKSP) {
		switch (opcode) {

		/*  Avoid infinite recursion.
		 */
		case OC_CHECK_SP_B:
		case OC_CHECK_SP_W:
		case OC_CHECK_SP_L:
			break;
		
		/*  We put a trap at location zero, and we don't want the
		 *  trap preceded by a checksp.  In general, we want
		 *  a trap at a given location to really result in a trap
		 *  opcode at that location.
		 */
		case OC_TRAP:
			break;

		/*  We don't want a check here, because the frame pointer
		 *  is not yet set up - the link instruction is just about
		 *  to to that.
		 */
		case OC_LINK_B:
		case OC_LINK_W:
		case OC_LINK_L:
		case OC_TRACELINK_B:
		case OC_TRACELINK_W:
		case OC_TRACELINK_L:
			break;
		
		default:
			ci_code_generic_opcode(tx, OC_CHECK_SP_B, tx->tx_sp);
		}
	}

	tmpspace = 0;
	switch (opcode) {
	case OC_BITWISE_AND:
	case OC_BITWISE_XOR:
	case OC_BITWISE_OR:
	case OC_MUL_SIGNED:
	case OC_MUL_UNSIGNED:
	case OC_DIV_SIGNED:
	case OC_DIV_UNSIGNED:
	case OC_CHKDIV_SIGNED:
	case OC_CHKDIV_UNSIGNED:
	case OC_MOD:
	case OC_ADD:
	case OC_SUB:
	case OC_LSHIFT:
	case OC_RSHIFT:
	case OC_IS_EQUAL:
	case OC_NOT_EQUAL:
	case OC_LESS_UNSIGNED:
	case OC_GREATER_UNSIGNED:
	case OC_LESS_OR_EQUAL_UNSIGNED:
	case OC_GREATER_OR_EQUAL_UNSIGNED:
	case OC_LESS_SIGNED:
	case OC_GREATER_SIGNED:
	case OC_LESS_OR_EQUAL_SIGNED:
	case OC_GREATER_OR_EQUAL_SIGNED:
	case OC_POP:
	case OC_JUMP_IF_ZERO:
	case OC_JUMP_IF_NON_ZERO:
	case OC_PROC_ASSIGN_AND_PUSH_BYTE:
	case OC_PROC_ASSIGN_AND_PUSH_WORD:
	case OC_PROC_ASSIGN_AND_PUSH_LONG:
	case OC_PROC_ASSIGN_AND_PUSH_FLOAT:
	case OC_PROC_ASSIGN_AND_PUSH_DOUBLE:
	case OC_ASSIGN_AND_PUSH_BYTE:
	case OC_ASSIGN_AND_PUSH_WORD:
	case OC_ASSIGN_AND_PUSH_LONG:
	case OC_ASSIGN_AND_PUSH_FLOAT:
	case OC_ASSIGN_AND_PUSH_DOUBLE:
	case OC_CALL_INDIRECT:
	case OC_SWITCH_ON_TABLE:
	case OC_SWITCH_ON_CHAIN_B:
	case OC_SWITCH_ON_CHAIN_W:
	case OC_SWITCH_ON_CHAIN_L:
	case OC_INSERT_SIGNED_BITFIELD:
	case OC_INSERT_UNSIGNED_BITFIELD:
	case OC_RET_WORD:
	case OC_TRACERET_WORD:
		tx->tx_sp -= sizeof(stackword_t);
		break;

	case OC_PROC_MEMCPY_B:
	case OC_PROC_MEMCPY_W:
	case OC_PROC_MEMCPY_L:
	case OC_MEMCPY_B:
	case OC_MEMCPY_W:
	case OC_MEMCPY_L:
		tx->tx_sp -= 2 * sizeof(stackword_t);
		break;

	case OC_PROC_ASSIGN_BYTE:
	case OC_PROC_ASSIGN_WORD:
	case OC_PROC_ASSIGN_LONG:
	case OC_ASSIGN_BYTE:
	case OC_ASSIGN_WORD:
	case OC_ASSIGN_LONG:
	case OC_MULTI_ARROW:
		tx->tx_sp -= 2 * sizeof(stackword_t);
		break;
	
	case OC_PROC_ASSIGN_FLOAT:
	case OC_ASSIGN_FLOAT:
		tx->tx_sp -= sizeof(stackword_t) + FLOAT_NBYTES;
		break;
	case OC_PROC_ASSIGN_DOUBLE:
	case OC_ASSIGN_DOUBLE:
		tx->tx_sp -= sizeof(stackword_t) + DOUBLE_NBYTES;
		break;

	case OC_NEG_CONSTPUSH_B:
	case OC_NEG_CONSTPUSH_W:
	case OC_NEG_CONSTPUSH_L:
	case OC_CONSTPUSH_B:
	case OC_CONSTPUSH_W:
	case OC_CONSTPUSH_L:

	case OC_PROC_PUSH_FP_ADDR_B:
	case OC_PROC_PUSH_FP_ADDR_W:
	case OC_PROC_PUSH_FP_ADDR_L:
	case OC_PROC_PUSH_AP_ADDR_B:
	case OC_PROC_PUSH_AP_ADDR_W:
	case OC_PROC_PUSH_AP_ADDR_L:
	case OC_PUSH_STACKADDR_B:
	case OC_PUSH_STACKADDR_W:
	case OC_PUSH_STACKADDR_L:

	case OC_PUSH_SIGNED_BYTE_AT_ADDR_B:
	case OC_PUSH_SIGNED_BYTE_AT_ADDR_W:
	case OC_PUSH_SIGNED_BYTE_AT_ADDR_L:
	case OC_PUSH_SIGNED_SHORT_AT_ADDR_B:
	case OC_PUSH_SIGNED_SHORT_AT_ADDR_W:
	case OC_PUSH_SIGNED_SHORT_AT_ADDR_L:
	case OC_PUSH_SIGNED_LONG_AT_ADDR_B:
	case OC_PUSH_SIGNED_LONG_AT_ADDR_W:
	case OC_PUSH_SIGNED_LONG_AT_ADDR_L:

	case OC_PUSH_UNSIGNED_BYTE_AT_ADDR_B:
	case OC_PUSH_UNSIGNED_BYTE_AT_ADDR_W:
	case OC_PUSH_UNSIGNED_BYTE_AT_ADDR_L:
	case OC_PUSH_UNSIGNED_SHORT_AT_ADDR_B:
	case OC_PUSH_UNSIGNED_SHORT_AT_ADDR_W:
	case OC_PUSH_UNSIGNED_SHORT_AT_ADDR_L:
	case OC_PUSH_UNSIGNED_LONG_AT_ADDR_B:
	case OC_PUSH_UNSIGNED_LONG_AT_ADDR_W:
	case OC_PUSH_UNSIGNED_LONG_AT_ADDR_L:

	case OC_PUSH_SIGNED_BYTE_AT_STACKADDR_B:
	case OC_PUSH_SIGNED_BYTE_AT_STACKADDR_W:
	case OC_PUSH_SIGNED_BYTE_AT_STACKADDR_L:
	case OC_PUSH_SIGNED_SHORT_AT_STACKADDR_B:
	case OC_PUSH_SIGNED_SHORT_AT_STACKADDR_W:
	case OC_PUSH_SIGNED_SHORT_AT_STACKADDR_L:
	case OC_PUSH_SIGNED_LONG_AT_STACKADDR_B:
	case OC_PUSH_SIGNED_LONG_AT_STACKADDR_W:
	case OC_PUSH_SIGNED_LONG_AT_STACKADDR_L:

	case OC_PUSH_UNSIGNED_BYTE_AT_STACKADDR_B:
	case OC_PUSH_UNSIGNED_BYTE_AT_STACKADDR_W:
	case OC_PUSH_UNSIGNED_BYTE_AT_STACKADDR_L:
	case OC_PUSH_UNSIGNED_SHORT_AT_STACKADDR_B:
	case OC_PUSH_UNSIGNED_SHORT_AT_STACKADDR_W:
	case OC_PUSH_UNSIGNED_SHORT_AT_STACKADDR_L:
	case OC_PUSH_UNSIGNED_LONG_AT_STACKADDR_B:
	case OC_PUSH_UNSIGNED_LONG_AT_STACKADDR_W:
	case OC_PUSH_UNSIGNED_LONG_AT_STACKADDR_L:

	case OC_PUSH_WORD_RETVAL:
	case OC_PUSH_STRUCTRET_ADDR:

	case OC_DUP:
	case OC_RESERVE_SLOT:
		tx->tx_sp += sizeof(stackword_t);
		break;
	
	case OC_CALL_B:
	case OC_CALL_W:
	case OC_CALL_L:
		tmpspace = sizeof(stackword_t);
		break;
	
	case OC_CHECK_SP_B:
	case OC_CHECK_SP_W:
	case OC_CHECK_SP_L:
	case OC_CVT_TO_BOOL:
	case OC_LOGICAL_NOT:
	case OC_BITWISE_NOT:
	case OC_UNARY_MINUS:
	case OC_FLOAT_UNARY_MINUS:
	case OC_DOUBLE_UNARY_MINUS:
	case OC_DUP_BACK_ONE:
	case OC_TRACERET:
	case OC_TRACERET_STRUCT:
	case OC_RET:
	case OC_RET_STRUCT:

	case OC_PROC_DEREF_SIGNED_BYTE:
	case OC_PROC_DEREF_SIGNED_WORD:
	case OC_PROC_DEREF_SIGNED_LONG:
	case OC_PROC_DEREF_UNSIGNED_BYTE:
	case OC_PROC_DEREF_UNSIGNED_WORD:
	case OC_PROC_DEREF_UNSIGNED_LONG:
	case OC_DEREF_SIGNED_BYTE:
	case OC_DEREF_SIGNED_WORD:
	case OC_DEREF_SIGNED_LONG:
	case OC_DEREF_UNSIGNED_BYTE:
	case OC_DEREF_UNSIGNED_WORD:
	case OC_DEREF_UNSIGNED_LONG:

	/* Stack effect of these depends on the operand
	 */
	case OC_POPMANY_B:
	case OC_POPMANY_W:
	case OC_POPMANY_L:
	case OC_PUSH_BYTES:
	case OC_PROC_PUSH_BYTES:
	case OC_RESERVE_BYTES:

	case OC_LINK_B:
	case OC_LINK_W:
	case OC_LINK_L:
	case OC_TRACELINK_B:
	case OC_TRACELINK_W:
	case OC_TRACELINK_L:

	case OC_TRAP:
	case OC_UNRESOLVED_JUMP:
	case OC_JUMP:

	case OC_EXTRACT_SIGNED_BITFIELD:
	case OC_EXTRACT_UNSIGNED_BITFIELD:
		break;
	
	case OC_PROC_DEREF_FLOAT:
	case OC_DEREF_FLOAT:
	case OC_CVT_LONG_TO_FLOAT:
		tx->tx_sp += FLOAT_NBYTES - sizeof(stackword_t);
		break;

	case OC_PROC_DEREF_DOUBLE:
	case OC_DEREF_DOUBLE:
	case OC_CVT_LONG_TO_DOUBLE:
		tx->tx_sp += DOUBLE_NBYTES - sizeof(stackword_t);
		break;

	case OC_CVT_FLOAT_TO_LONG:
		tx->tx_sp -= FLOAT_NBYTES - sizeof(stackword_t);
		break;
	case OC_CVT_DOUBLE_TO_LONG:
		tx->tx_sp -= DOUBLE_NBYTES - sizeof(stackword_t);
		break;
	
	case OC_PUSH_FLOAT_RETVAL:
	case OC_PUSH_FLOAT_CONST:
	case OC_PUSH_FLOAT_AT_STACKADDR_B:
	case OC_PUSH_FLOAT_AT_STACKADDR_W:
	case OC_PUSH_FLOAT_AT_STACKADDR_L:
	case OC_PUSH_FLOAT_AT_ADDR_B:
	case OC_PUSH_FLOAT_AT_ADDR_W:
	case OC_PUSH_FLOAT_AT_ADDR_L:
		tx->tx_sp += FLOAT_NBYTES;
		break;
	case OC_PUSH_DOUBLE_RETVAL:
	case OC_PUSH_DOUBLE_CONST:
	case OC_PUSH_DOUBLE_AT_STACKADDR_B:
	case OC_PUSH_DOUBLE_AT_STACKADDR_W:
	case OC_PUSH_DOUBLE_AT_STACKADDR_L:
	case OC_PUSH_DOUBLE_AT_ADDR_B:
	case OC_PUSH_DOUBLE_AT_ADDR_W:
	case OC_PUSH_DOUBLE_AT_ADDR_L:
		tx->tx_sp += DOUBLE_NBYTES;
		break;
	
	case OC_MUL_FLOATS:
	case OC_DIV_FLOATS:
	case OC_CHKDIV_FLOATS:
	case OC_ADD_FLOATS:
	case OC_SUB_FLOATS:
	case OC_RET_FLOAT:
	case OC_TRACERET_FLOAT:
		tx->tx_sp -= FLOAT_NBYTES;
		break;

	case OC_MUL_DOUBLES:
	case OC_DIV_DOUBLES:
	case OC_CHKDIV_DOUBLES:
	case OC_ADD_DOUBLES:
	case OC_SUB_DOUBLES:
	case OC_RET_DOUBLE:
	case OC_TRACERET_DOUBLE:
		tx->tx_sp -= DOUBLE_NBYTES;
		break;
	
	case OC_FLOAT_IS_EQUAL:
	case OC_FLOAT_NOT_EQUAL:
	case OC_FLOAT_LESS:
	case OC_FLOAT_LESS_OR_EQUAL:
	case OC_FLOAT_GREATER:
	case OC_FLOAT_GREATER_OR_EQUAL:
		tx->tx_sp -= (2 * FLOAT_NBYTES) - sizeof(stackword_t);
		break;

	case OC_DOUBLE_IS_EQUAL:
	case OC_DOUBLE_NOT_EQUAL:
	case OC_DOUBLE_LESS:
	case OC_DOUBLE_LESS_OR_EQUAL:
	case OC_DOUBLE_GREATER:
	case OC_DOUBLE_GREATER_OR_EQUAL:
		tx->tx_sp -= (2 * DOUBLE_NBYTES) - sizeof(stackword_t);
		break;
	
	case OC_CVT_DOUBLE_TO_FLOAT:
		tx->tx_sp -= DOUBLE_NBYTES - FLOAT_NBYTES;
		break;
	case OC_CVT_FLOAT_TO_DOUBLE:
		tx->tx_sp += DOUBLE_NBYTES - FLOAT_NBYTES;
		break;

	default:
		ci_panic("unknown opcode in ci_code_opcode");
	}

	if (tx->tx_sp + tmpspace > tx->tx_max_sp)
		tx->tx_max_sp = tx->tx_sp + tmpspace;

	ci_code_byte(tx, (int)opcode);
}

static void
show_generic(ma, byte_opcode, opname, p_pc)
machine_t *ma;
opcode_t byte_opcode;
const char *opname;
textword_t **p_pc;
{
	textword_t *pc;
	long offset, val;
	int typec;

	pc = *p_pc;
	switch (pc[-1] - (int)byte_opcode) {
	case 0:
		val = *pc++;
		typec = 'b';
		break;
	case 1:
		val = GETWORD(pc);
		typec = 'w';
		pc += 2;
		break;
	case 2:
		val = GETLONG(pc);
		typec = 'l';
		pc += 4;
		break;
	default:
		ci_panic("opcode botch in pgo");
		val = 0;	/* to satisfy gcc */
		typec = 0;	/* to satisfy gcc */
	}
	*p_pc = pc;
	
	printf("%s %d.%c", opname, val, typec);

	offset = val - (unsigned)ma->ma_data;
	if (offset >= 0 && offset < ma->ma_data_size)
		printf(" (&data[%d])", offset);

	offset -= ma->ma_data_size;
	if (offset >= 0 && offset < ma->ma_bss_size)
		printf(" (&bss[%d])", offset);
}

void
ci_disassemble(parse_id, code_id)
parse_id_t parse_id;
code_id_t code_id;
{
	machine_t *ma;
	textword_t *text;
	taddr_t *funcaddrs;
	textword_t *pclim;
	textword_t *pc;
	func_t *f, *funclist;
	lno_t *ln;
	float fval;
	double dval;
#define JUMPDEST(pc, text)		((pc + (short)GETWORD(pc)) - text)

	ma = (machine_t *)code_id;
	text = ma->ma_text;
	funcaddrs = ma->ma_funcaddrs;
	pclim = ma->ma_text + ma->ma_text_size;
	funclist = ((parse_res_t *)parse_id)->pr_funcs;

	pc = text; 
	f = NULL;
	ln = NULL;
	while (pc < pclim) {
		func_t *exactf;
		taddr_t addr;
		int minval, ncase, i;
		opcode_t opcode;

		addr = (taddr_t)(pc - text);

		if (addr == ma->ma_entry_point)
			puts("\n\n<entry_point>");

		if ((exactf = ci_addr_to_func(funclist, addr)) != NULL) {
			f = exactf;
			if (f->fu_fil != NULL && f->fu_fil->fi_so == NULL)
				f->fu_fil->fi_so = so_open_file(f->fu_fil->fi_name, 
							   (so_line_callback_t)NULL);
			printf("\n\n%s\n", f->fu_name);
			ln = f->fu__lnos;
		}

		for (; ln != NULL && ln->ln_addr <= addr; ln = ln->ln_next) {
			if (ln->ln_addr == addr) {
				fputs("\n#", stdout);
				if (f->fu_fil->fi_so != NULL && ln->ln_num > 0 &&
				       ln->ln_num <= so_get_nlines(f->fu_fil->fi_so))
					puts(so_getline(f->fu_fil->fi_so,
						        ln->ln_num - 1));
				else
					printf("#%s,%d\n",
					            f->fu_fil->fi_name, ln->ln_num);
			}
		}

		printf("%8d: ", addr);
		switch((opcode_t)*pc++) {
		case OC_SWITCH_ON_TABLE:
			ncase = GETWORD(pc);
			pc += 2;
			minval = GETLONG(pc);
			pc += 4;
			printf("switch_on_table #%d,+%d\n", ncase, minval);
			for (i = 0; i < ncase; ++i) {
				printf("\t\tcase %d: jump %d\n",
						i + minval, JUMPDEST(pc, text));
				pc += 2;
			}
			printf("\t\tdefault: jump %d", JUMPDEST(pc, text));
			pc += 2;
			break;

		case OC_INSERT_UNSIGNED_BITFIELD:
			printf("insert_unsigned_bitfield #%d,#", *pc++);
			printf("%d", *pc++);
			break;
		case OC_INSERT_SIGNED_BITFIELD:
			printf("insert_signed_bitfield #%d,#", *pc++);
			printf("%d", *pc++);
			break;
		case OC_EXTRACT_UNSIGNED_BITFIELD:
			printf("extract_unsigned_bitfield #%d,#", *pc++);
			printf("%d", *pc++);
			break;
		case OC_EXTRACT_SIGNED_BITFIELD:
			printf("extract_signed_bitfield #%d,#", *pc++);
			printf("%d", *pc++);
			break;

		case OC_SWITCH_ON_CHAIN_B:
		case OC_SWITCH_ON_CHAIN_W:
		case OC_SWITCH_ON_CHAIN_L:
			opcode = (opcode_t)pc[-1];
			ncase = GETWORD(pc);
			pc += 2;
			minval = GETLONG(pc);
			pc += 4;
			printf("switch_on_chain #%d,+%d\n", ncase, minval);
			for (i = 0; i < ncase; ++i) {
				int val;

				switch (opcode) {
				case OC_SWITCH_ON_CHAIN_B:
					val = *pc++;
					break;
				case OC_SWITCH_ON_CHAIN_W:
					val = GETWORD(pc);
					pc += 2;
					break;
				case OC_SWITCH_ON_CHAIN_L:
					val = GETLONG(pc);
					pc += 4;
					break;
				default:
					ci_panic("bad opcode");
					val = 0;	/* to satisfy gcc */
				}
				printf("\t\tcase %d: jump %d\n",
						val + minval, JUMPDEST(pc, text));
				pc += 2;
			}
			printf("\t\tdefault: jump %d", JUMPDEST(pc, text));
			pc += 2;
			break;

		case OC_NEG_CONSTPUSH_B:
		case OC_NEG_CONSTPUSH_W:
		case OC_NEG_CONSTPUSH_L:
			show_generic(ma, OC_NEG_CONSTPUSH_B, "neg_constpush", &pc);
			break;
		case OC_CONSTPUSH_B:
		case OC_CONSTPUSH_W:
		case OC_CONSTPUSH_L:
			show_generic(ma, OC_CONSTPUSH_B, "constpush", &pc);
			break;
		
		case OC_PUSH_FLOAT_CONST:
			memcpy((char *)&fval, pc, FLOAT_NBYTES);
			pc += FLOAT_NBYTES;
			printf("push_float_const %f", fval);
			break;
		case OC_PUSH_DOUBLE_CONST:
			memcpy((char *)&dval, pc, DOUBLE_NBYTES);
			pc += DOUBLE_NBYTES;
			printf("push_double_const %f", dval);
			break;

		case OC_PUSH_SIGNED_BYTE_AT_ADDR_B:
		case OC_PUSH_SIGNED_BYTE_AT_ADDR_W:
		case OC_PUSH_SIGNED_BYTE_AT_ADDR_L:
			show_generic(ma, OC_PUSH_SIGNED_BYTE_AT_ADDR_B,
						"push_signed_byte_at_addr", &pc);
			break;
		case OC_PUSH_SIGNED_SHORT_AT_ADDR_B:
		case OC_PUSH_SIGNED_SHORT_AT_ADDR_W:
		case OC_PUSH_SIGNED_SHORT_AT_ADDR_L:
			show_generic(ma, OC_PUSH_SIGNED_SHORT_AT_ADDR_B,
						"push_signed_short_at_addr", &pc);
			break;
		case OC_PUSH_SIGNED_LONG_AT_ADDR_B:
		case OC_PUSH_SIGNED_LONG_AT_ADDR_W:
		case OC_PUSH_SIGNED_LONG_AT_ADDR_L:
			show_generic(ma, OC_PUSH_SIGNED_LONG_AT_ADDR_B,
						"push_signed_long_at_addr", &pc);
			break;
		case OC_PUSH_FLOAT_AT_ADDR_B:
		case OC_PUSH_FLOAT_AT_ADDR_W:
		case OC_PUSH_FLOAT_AT_ADDR_L:
			show_generic(ma, OC_PUSH_FLOAT_AT_ADDR_B,
					"push_float_at_addr", &pc);
			break;
		case OC_PUSH_DOUBLE_AT_ADDR_B:
		case OC_PUSH_DOUBLE_AT_ADDR_W:
		case OC_PUSH_DOUBLE_AT_ADDR_L:
			show_generic(ma, OC_PUSH_DOUBLE_AT_ADDR_B,
					"push_double_at_addr", &pc);
			break;

		case OC_PUSH_SIGNED_BYTE_AT_STACKADDR_B:
		case OC_PUSH_SIGNED_BYTE_AT_STACKADDR_W:
		case OC_PUSH_SIGNED_BYTE_AT_STACKADDR_L:
			show_generic(ma, OC_PUSH_SIGNED_BYTE_AT_STACKADDR_B,
					"push_signed_byte_at_stackaddr", &pc);
			break;
		case OC_PUSH_SIGNED_SHORT_AT_STACKADDR_B:
		case OC_PUSH_SIGNED_SHORT_AT_STACKADDR_W:
		case OC_PUSH_SIGNED_SHORT_AT_STACKADDR_L:
			show_generic(ma, OC_PUSH_SIGNED_SHORT_AT_STACKADDR_B,
					"push_signed_short_at_stackaddr", &pc);
			break;
		case OC_PUSH_SIGNED_LONG_AT_STACKADDR_B:
		case OC_PUSH_SIGNED_LONG_AT_STACKADDR_W:
		case OC_PUSH_SIGNED_LONG_AT_STACKADDR_L:
			show_generic(ma, OC_PUSH_SIGNED_LONG_AT_STACKADDR_B,
					"push_signed_long_at_stackaddr", &pc);
			break;
		case OC_PUSH_FLOAT_AT_STACKADDR_B:
		case OC_PUSH_FLOAT_AT_STACKADDR_W:
		case OC_PUSH_FLOAT_AT_STACKADDR_L:
			show_generic(ma, OC_PUSH_FLOAT_AT_STACKADDR_B,
					"push_float_at_stackaddr", &pc);
			break;
		case OC_PUSH_DOUBLE_AT_STACKADDR_B:
		case OC_PUSH_DOUBLE_AT_STACKADDR_W:
		case OC_PUSH_DOUBLE_AT_STACKADDR_L:
			show_generic(ma, OC_PUSH_DOUBLE_AT_STACKADDR_B,
					"push_double_at_stackaddr", &pc);
			break;

		case OC_PUSH_UNSIGNED_BYTE_AT_ADDR_B:
		case OC_PUSH_UNSIGNED_BYTE_AT_ADDR_W:
		case OC_PUSH_UNSIGNED_BYTE_AT_ADDR_L:
			show_generic(ma, OC_PUSH_UNSIGNED_BYTE_AT_ADDR_B,
						"push_unsigned_byte_at_addr", &pc);
			break;
		case OC_PUSH_UNSIGNED_SHORT_AT_ADDR_B:
		case OC_PUSH_UNSIGNED_SHORT_AT_ADDR_W:
		case OC_PUSH_UNSIGNED_SHORT_AT_ADDR_L:
			show_generic(ma, OC_PUSH_UNSIGNED_SHORT_AT_ADDR_B,
						"push_unsigned_short_at_addr", &pc);
			break;
		case OC_PUSH_UNSIGNED_LONG_AT_ADDR_B:
		case OC_PUSH_UNSIGNED_LONG_AT_ADDR_W:
		case OC_PUSH_UNSIGNED_LONG_AT_ADDR_L:
			show_generic(ma, OC_PUSH_UNSIGNED_LONG_AT_ADDR_B,
						"push_unsigned_long_at_addr", &pc);
			break;

		case OC_PUSH_UNSIGNED_BYTE_AT_STACKADDR_B:
		case OC_PUSH_UNSIGNED_BYTE_AT_STACKADDR_W:
		case OC_PUSH_UNSIGNED_BYTE_AT_STACKADDR_L:
			show_generic(ma, OC_PUSH_UNSIGNED_BYTE_AT_STACKADDR_B,
					"push_unsigned_byte_at_stackaddr", &pc);
			break;
		case OC_PUSH_UNSIGNED_SHORT_AT_STACKADDR_B:
		case OC_PUSH_UNSIGNED_SHORT_AT_STACKADDR_W:
		case OC_PUSH_UNSIGNED_SHORT_AT_STACKADDR_L:
			show_generic(ma, OC_PUSH_UNSIGNED_SHORT_AT_STACKADDR_B,
					"push_unsigned_short_at_stackaddr", &pc);
			break;
		case OC_PUSH_UNSIGNED_LONG_AT_STACKADDR_B:
		case OC_PUSH_UNSIGNED_LONG_AT_STACKADDR_W:
		case OC_PUSH_UNSIGNED_LONG_AT_STACKADDR_L:
			show_generic(ma, OC_PUSH_UNSIGNED_LONG_AT_STACKADDR_B,
					"push_unsigned_long_at_stackaddr", &pc);
			break;

		case OC_PROC_MEMCPY_B:
		case OC_PROC_MEMCPY_W:
		case OC_PROC_MEMCPY_L:
			show_generic(ma, OC_PROC_MEMCPY_B, "proc_memcpy", &pc);
			break;
		case OC_MEMCPY_B:
		case OC_MEMCPY_W:
		case OC_MEMCPY_L:
			show_generic(ma, OC_MEMCPY_B, "memcpy", &pc);
			break;

		case OC_CHECK_SP_B:
		case OC_CHECK_SP_W:
		case OC_CHECK_SP_L:
			show_generic(ma, OC_CHECK_SP_B, "\t\t\t\tcheck_sp", &pc);
			break;

		case OC_PROC_PUSH_FP_ADDR_B:
		case OC_PROC_PUSH_FP_ADDR_W:
		case OC_PROC_PUSH_FP_ADDR_L:
			show_generic(ma, OC_PROC_PUSH_FP_ADDR_B,"proc_push_fp_addr",&pc);
			break;

		case OC_PROC_PUSH_AP_ADDR_B:
		case OC_PROC_PUSH_AP_ADDR_W:
		case OC_PROC_PUSH_AP_ADDR_L:
			show_generic(ma, OC_PROC_PUSH_AP_ADDR_B,"proc_push_ap_addr",&pc);
			break;

		case OC_PUSH_STACKADDR_B:
		case OC_PUSH_STACKADDR_W:
		case OC_PUSH_STACKADDR_L:
			show_generic(ma, OC_PUSH_STACKADDR_B, "push_stack_addr", &pc);
			break;

		case OC_CALL_INDIRECT:
			printf("call_indirect #%d", *pc++);
			break;
		
		case OC_RESERVE_BYTES:
			printf("reserve_bytes %d.l", GETLONG(pc));
			pc += 4;
			break;
		case OC_PUSH_BYTES:
			printf("push_bytes %d.l", GETLONG(pc));
			pc += 4;
			break;
		case OC_PROC_PUSH_BYTES:
			printf("proc_push_bytes %d.l", GETLONG(pc));
			pc += 4;
			break;

		case OC_TRACERET_STRUCT:
			printf("traceret_struct %d.l", GETLONG(pc));
			pc += 4;
			break;
		case OC_RET_STRUCT:
			printf("ret_struct %d.l", GETLONG(pc));
			pc += 4;
			break;

		case OC_CALL_B:
			printf("call %d.b #%d", *pc, pc[1]);
			pc += 2;
			break;
		case OC_CALL_W:
			printf("call %d.w #%d", GETWORD(pc), pc[2]);
			pc += 3;
			break;
		case OC_CALL_L:
			printf("call %d.l #%d", GETLONG(pc), pc[4]);
			pc += 5;
			break;
		case OC_POPMANY_B:
		case OC_POPMANY_W:
		case OC_POPMANY_L:
			show_generic(ma, OC_POPMANY_B, "popmany", &pc);
			break;

		case OC_LINK_B:
			printf("link %d.b,%d", *pc, pc[1]);
			pc += 2;
			break;
		case OC_LINK_W:
			printf("link %d.w,%d", GETWORD(pc), pc[2]);
			pc += 3;
			break;
		case OC_LINK_L:
			printf("link %d.l,%d", GETLONG(pc), pc[4]);
			pc += 5;
			break;

		case OC_TRACELINK_B:
			printf("tracelink %d.b,%d", *pc, pc[1]);
			pc += 2;
			break;
		case OC_TRACELINK_W:
			printf("tracelink %d.w,%d", GETWORD(pc), pc[2]);
			pc += 3;
			break;
		case OC_TRACELINK_L:
			printf("tracelink %d.l,%d", GETLONG(pc), pc[4]);
			pc += 5;
			break;

		case OC_UNRESOLVED_JUMP:
			printf("<unresolved jump> %d", JUMPDEST(pc, text));
			pc += 2;
			break;
		case OC_JUMP:
			printf("jump %d", JUMPDEST(pc, text));
			pc += 2;
			break;
		case OC_JUMP_IF_ZERO:
			printf("jump_if_zero %d", JUMPDEST(pc, text));
			pc += 2;
			break;
		case OC_JUMP_IF_NON_ZERO:
			printf("jump_if_non_zero %d", JUMPDEST(pc, text));
			pc += 2;
			break;

		case OC_PROC_ASSIGN_AND_PUSH_BYTE:
			fputs("proc_assign_and_push_byte", stdout);
			break;
		case OC_PROC_ASSIGN_AND_PUSH_WORD:
			fputs("proc_assign_and_push_word", stdout);
			break;
		case OC_PROC_ASSIGN_AND_PUSH_LONG:
			fputs("proc_assign_and_push_long", stdout);
			break;
		case OC_PROC_ASSIGN_AND_PUSH_FLOAT:
			fputs("proc_assign_and_push_float", stdout);
			break;
		case OC_PROC_ASSIGN_AND_PUSH_DOUBLE:
			fputs("proc_assign_and_push_double", stdout);
			break;

		case OC_ASSIGN_AND_PUSH_BYTE:
			fputs("assign_and_push_byte", stdout);
			break;
		case OC_ASSIGN_AND_PUSH_WORD:
			fputs("assign_and_push_word", stdout);
			break;
		case OC_ASSIGN_AND_PUSH_LONG:
			fputs("assign_and_push_long", stdout);
			break;
		case OC_ASSIGN_AND_PUSH_FLOAT:
			fputs("assign_and_push_float", stdout);
			break;
		case OC_ASSIGN_AND_PUSH_DOUBLE:
			fputs("assign_and_push_double", stdout);
			break;

		case OC_PROC_ASSIGN_BYTE:
			fputs("proc_assign_byte", stdout);
			break;
		case OC_PROC_ASSIGN_WORD:
			fputs("proc_assign_word", stdout);
			break;
		case OC_PROC_ASSIGN_LONG:
			fputs("proc_assign_long", stdout);
			break;
		case OC_PROC_ASSIGN_FLOAT:
			fputs("proc_assign_float", stdout);
			break;
		case OC_PROC_ASSIGN_DOUBLE:
			fputs("proc_assign_double", stdout);
			break;

		case OC_ASSIGN_BYTE:
			fputs("assign_byte", stdout);
			break;
		case OC_ASSIGN_WORD:
			fputs("assign_word", stdout);
			break;
		case OC_ASSIGN_LONG:
			fputs("assign_long", stdout);
			break;
		case OC_ASSIGN_FLOAT:
			fputs("assign_float", stdout);
			break;
		case OC_ASSIGN_DOUBLE:
			fputs("assign_double", stdout);
			break;

		case OC_LESS_UNSIGNED:
			fputs("less_unsigned", stdout);	
			break;
		case OC_GREATER_UNSIGNED:
			fputs("greater_unsigned", stdout);
			break;
		case OC_LESS_OR_EQUAL_UNSIGNED:
			fputs("less_or_equal_unsigned", stdout);
			break;
		case OC_GREATER_OR_EQUAL_UNSIGNED:
			fputs("greater_or_equal_unsigned", stdout);
			break;

		case OC_LESS_SIGNED:	
			fputs("less_signed", stdout);	
			break;
		case OC_GREATER_SIGNED:
			fputs("greater_signed", stdout);
			break;
		case OC_LESS_OR_EQUAL_SIGNED:
			fputs("less_or_equal_signed", stdout);
			break;
		case OC_GREATER_OR_EQUAL_SIGNED:
			fputs("greater_or_equal_signed", stdout);
			break;
		
		case OC_FLOAT_IS_EQUAL:
			fputs("float_is_equal", stdout);
			break;
		case OC_FLOAT_NOT_EQUAL:
			fputs("float_not_equal", stdout);
			break;
		case OC_FLOAT_LESS:
			fputs("float_less", stdout);
			break;
		case OC_FLOAT_GREATER:
			fputs("float_greater", stdout);
			break;
		case OC_FLOAT_LESS_OR_EQUAL:
			fputs("float_less_or_equal", stdout);
			break;
		case OC_FLOAT_GREATER_OR_EQUAL:
			fputs("float_greater_or_equal", stdout);
			break;

		case OC_DOUBLE_IS_EQUAL:
			fputs("double_is_equal", stdout);
			break;
		case OC_DOUBLE_NOT_EQUAL:
			fputs("double_not_equal", stdout);
			break;
		case OC_DOUBLE_LESS:
			fputs("double_less", stdout);
			break;
		case OC_DOUBLE_GREATER:
			fputs("double_greater", stdout);
			break;
		case OC_DOUBLE_LESS_OR_EQUAL:
			fputs("double_less_or_equal", stdout);
			break;
		case OC_DOUBLE_GREATER_OR_EQUAL:
			fputs("double_greater_or_equal", stdout);
			break;

		case OC_PROC_DEREF_UNSIGNED_BYTE:
			fputs("proc_deref_unsigned_byte", stdout);
			break;
		case OC_PROC_DEREF_UNSIGNED_WORD:
			fputs("proc_deref_unsigned_word", stdout);
			break;
		case OC_PROC_DEREF_UNSIGNED_LONG:
			fputs("proc_deref_unsigned_long", stdout);
			break;
		case OC_PROC_DEREF_SIGNED_BYTE:
			fputs("proc_deref_signed_byte", stdout);
			break;
		case OC_PROC_DEREF_SIGNED_WORD:
			fputs("proc_deref_signed_word", stdout);
			break;
		case OC_PROC_DEREF_SIGNED_LONG:
			fputs("proc_deref_signed_long", stdout);
			break;
		case OC_PROC_DEREF_FLOAT:
			fputs("proc_deref_float", stdout);
			break;
		case OC_PROC_DEREF_DOUBLE:
			fputs("proc_deref_double", stdout);
			break;

		case OC_DEREF_UNSIGNED_BYTE:
			fputs("deref_unsigned_byte", stdout);
			break;
		case OC_DEREF_UNSIGNED_WORD:
			fputs("deref_unsigned_word", stdout);
			break;
		case OC_DEREF_UNSIGNED_LONG:
			fputs("deref_unsigned_long", stdout);
			break;
		case OC_DEREF_SIGNED_BYTE:
			fputs("deref_signed_byte", stdout);
			break;
		case OC_DEREF_SIGNED_WORD:
			fputs("deref_signed_word", stdout);
			break;
		case OC_DEREF_SIGNED_LONG:
			fputs("deref_signed_long", stdout);
			break;
		case OC_DEREF_FLOAT:
			fputs("deref_float", stdout);
			break;
		case OC_DEREF_DOUBLE:
			fputs("deref_double", stdout);
			break;

		case OC_FLOAT_UNARY_MINUS:
			fputs("float_unary_minus", stdout);
			break;
		case OC_DOUBLE_UNARY_MINUS:
			fputs("double_unary_minus", stdout);
			break;

		case OC_CVT_FLOAT_TO_DOUBLE:
			fputs("cvt_float_to_double", stdout);
			break;
		case OC_CVT_LONG_TO_DOUBLE:
			fputs("cvt_long_to_double", stdout);
			break;
		case OC_CVT_LONG_TO_FLOAT:
			fputs("cvt_long_to_float", stdout);
			break;
		case OC_CVT_DOUBLE_TO_FLOAT:
			fputs("cvt_double_to_float", stdout);
			break;
		case OC_CVT_DOUBLE_TO_LONG:
			fputs("cvt_double_to_long", stdout);
			break;
		case OC_CVT_FLOAT_TO_LONG:
			fputs("cvt_float_to_long", stdout);
			break;

		case OC_PUSH_STRUCTRET_ADDR:
			fputs("push_structret_addr", stdout);
			break;
		case OC_PUSH_WORD_RETVAL:
			fputs("push_word_retval", stdout);
			break;
		case OC_PUSH_FLOAT_RETVAL:
			fputs("push_float_retval", stdout);
			break;
		case OC_PUSH_DOUBLE_RETVAL:
			fputs("push_double_retval", stdout);
			break;

		case OC_DIV_SIGNED:	fputs("div_signed", stdout);	break;
		case OC_CHKDIV_SIGNED:	fputs("chkdiv_signed", stdout);	break;
		case OC_DIV_UNSIGNED:	fputs("div_unsigned", stdout);	break;
		case OC_CHKDIV_UNSIGNED:fputs("chkdiv_unsigned", stdout);break;
		case OC_DIV_FLOATS:	fputs("div_floats", stdout);	break;
		case OC_CHKDIV_FLOATS:	fputs("chkdiv_floats", stdout);	break;
		case OC_DIV_DOUBLES:	fputs("div_doubles", stdout);	break;
		case OC_CHKDIV_DOUBLES:	fputs("chkdiv_doubles", stdout);break;
		case OC_MUL_SIGNED:	fputs("mul_signed", stdout);	break;
		case OC_MUL_UNSIGNED:	fputs("mul_unsigned", stdout);	break;
		case OC_MUL_FLOATS:	fputs("mul_floats", stdout);	break;
		case OC_MUL_DOUBLES:	fputs("mul_doubles", stdout);	break;
		case OC_ADD:		fputs("add", stdout);		break;
		case OC_ADD_FLOATS:	fputs("add_floats", stdout);	break;
		case OC_ADD_DOUBLES:	fputs("add_doubles", stdout);	break;
		case OC_SUB:		fputs("sub", stdout);		break;
		case OC_SUB_FLOATS:	fputs("sub_floats", stdout);	break;
		case OC_SUB_DOUBLES:	fputs("sub_doubles", stdout);	break;

		case OC_TRAP:		fputs("trap", stdout);		break;
		case OC_TRACERET:	fputs("traceret", stdout);	break;
		case OC_TRACERET_WORD:	fputs("traceret_word", stdout);	break;
		case OC_TRACERET_FLOAT:	fputs("traceret_float", stdout);break;
		case OC_TRACERET_DOUBLE:fputs("traceret_double", stdout);break;
		case OC_MULTI_ARROW:	fputs("multi_arrow", stdout);	break;
		case OC_RET:		fputs("ret", stdout);		break;
		case OC_RET_WORD:	fputs("ret_word", stdout);	break;
		case OC_RET_FLOAT:	fputs("ret_float", stdout);	break;
		case OC_RET_DOUBLE:	fputs("ret_double", stdout);	break;
		case OC_RESERVE_SLOT:	fputs("reserve_slot", stdout);	break;
		case OC_POP:		fputs("pop", stdout);		break;
		case OC_DUP_BACK_ONE:	fputs("dup_back_one", stdout);	break;
		case OC_DUP:		fputs("dup", stdout);		break;
		case OC_UNARY_MINUS:	fputs("unary_minus", stdout);	break;
		case OC_BITWISE_NOT:	fputs("bitwise_not", stdout);	break;
		case OC_CVT_TO_BOOL:	fputs("cvt_to_bool", stdout);	break;
		case OC_LOGICAL_NOT:	fputs("logical_not", stdout);	break;
		case OC_BITWISE_AND:	fputs("bitwise_and", stdout);	break;
		case OC_BITWISE_XOR:	fputs("bitwise_xor", stdout);	break;
		case OC_BITWISE_OR:	fputs("bitwise_or", stdout);	break;

		case OC_MOD:		fputs("mod", stdout);		break;
		case OC_LSHIFT:		fputs("lshift", stdout);	break;
		case OC_RSHIFT:		fputs("rshift", stdout);	break;
		case OC_IS_EQUAL:	fputs("is_equal", stdout);	break;
		case OC_NOT_EQUAL:	fputs("not_equal", stdout);	break;
		default:
			printf("<unknown opcode %d>", pc[-1]);
			break;
		}
		putchar('\n');
	}
	fflush(stdout);
}

void
ci_code_generic_opcode(tx, byte_opcode, arg)
text_t *tx;
opcode_t byte_opcode;
stackword_t arg;
{
	if (arg <= MAX_BYTE) {
		ci_code_opcode(tx, BYTE_FORM(byte_opcode));
		ci_code_byte(tx, (long)arg);
	}
	else if (arg < MAX_WORD) {
		ci_code_opcode(tx, SHORT_FORM(byte_opcode));
		ci_code_word(tx, (long)arg);
	}
	else {
		ci_code_opcode(tx, LONG_FORM(byte_opcode));
		ci_code_long(tx, (long)arg);
	}
}
