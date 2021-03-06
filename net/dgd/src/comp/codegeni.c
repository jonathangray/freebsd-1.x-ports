# include "comp.h"
# include "str.h"
# include "array.h"
# include "object.h"
# include "xfloat.h"
# include "interpret.h"
# include "data.h"
# include "fcontrol.h"
# include "table.h"
# include "node.h"
# include "control.h"
# include "codegen.h"

# define LINE_CHUNK	128

typedef struct _linechunk_ {
    char info[LINE_CHUNK];		/* chunk of line number info */
    struct _linechunk_ *next;		/* next in list */
} linechunk;

static linechunk *lline, *tline;	/* line chunk list */
static linechunk *fline;		/* free line chunk list */
static int lchunksz = LINE_CHUNK;	/* line chunk size */
static unsigned short line;		/* current line number */
static int line_info_size;		/* size of all line info */

/*
 * NAME:	line->byte()
 * DESCRIPTION:	output a line description byte
 */
static void line_byte(byte)
int byte;
{
    if (lchunksz == LINE_CHUNK) {
	register linechunk *l;

	/* new chunk */
	if (fline != (linechunk *) NULL) {
	    /* from free list */
	    l = fline;
	    fline = l->next;
	} else {
	    l = ALLOC(linechunk, 1);
	}
	l->next = (linechunk *) NULL;
	if (tline != (linechunk *) NULL) {
	    tline->next = l;
	} else {
	    lline = l;
	}
	tline = l;
	lchunksz = 0;
    }

    tline->info[lchunksz++] = byte;
    line_info_size++;
}

/*
 * NAME:	line->fix()
 * DESCRIPTION:	Fix the new line number.  Return 0 .. 2 for simple offsets,
 *		3 for special ones.
 */
static int line_fix(newline)
register unsigned short newline;
{
    register short offset;

    if (newline == 0) {
	/* nothing changes */
	return 0;
    }
    offset = newline - line;
    if (line != 0 && offset >= 0 && offset <= 2) {
	/* simple offset */
	line = newline;
	return offset;
    }

    if (offset >= -64 && offset <= 63) {
	/* one byte offset */
	line_byte(offset + 128 + 64);
    } else {
	/* two byte offset */
	offset += 16384;
	line_byte((offset >> 8) & 127);
	line_byte(offset);
    }
    line = newline;
    return 3;
}

/*
 * NAME:	line->make()
 * DESCRIPTION:	put line number info after the function
 */
static void line_make(buf)
register char *buf;
{
    register linechunk *l;

    /* collect all line blocks in one large block */
    for (l = lline; l != tline; l = l->next) {
	memcpy(buf, l->info, LINE_CHUNK);
	buf += LINE_CHUNK;
    }
    memcpy(buf, l->info, lchunksz);

    /* add blocks to free list */
    tline->next = fline;
    fline = lline;
    /* clear blocks */
    lline = (linechunk *) NULL;
    tline = (linechunk *) NULL;
    lchunksz = LINE_CHUNK;
    line = 0;
    line_info_size = 0;
}

/*
 * NAME:	line->clear()
 * DESCRIPTION:	clean up line number info chunks
 */
static void line_clear()
{
    register linechunk *l, *f;

    for (l = fline; l != (linechunk *) NULL; ) {
	f = l;
	l = l->next;
	FREE(f);
    }
    fline = (linechunk *) NULL;
}


# define CODE_CHUNK	128

typedef struct _codechunk_ {
    char code[CODE_CHUNK];		/* chunk of code */
    struct _codechunk_ *next;		/* next in list */
} codechunk;

static codechunk *lcode, *tcode;	/* code chunk list */
static codechunk *fcode;		/* free code chunk list */
static int cchunksz = CODE_CHUNK;	/* code chunk size */
static unsigned short here;		/* current offset */
static char *last_instruction;		/* address of last instruction */

/*
 * NAME:	code->byte()
 * DESCRIPTION:	output a byte of code
 */
static void code_byte(byte)
char byte;
{
    if (cchunksz == CODE_CHUNK) {
	register codechunk *l;

	/* new chunk */
	if (fcode != (codechunk *) NULL) {
	    /* from free list */
	    l = fcode;
	    fcode = l->next;
	} else {
	    l = ALLOC(codechunk, 1);
	}
	l->next = (codechunk *) NULL;
	if (tcode != (codechunk *) NULL) {
	    tcode->next = l;
	} else {
	    lcode = l;
	}
	tcode = l;
	cchunksz = 0;
    }
    tcode->code[cchunksz++] = byte;
    here++;
}

/*
 * NAME:	code->word()
 * DESCRIPTION:	output a word of code
 */
static void code_word(word)
unsigned short word;
{
    code_byte(word >> 8);
    code_byte(word);
}

/*
 * NAME:	code->instr()
 * DESCRIPTION:	generate an instruction code
 */
static void code_instr(i, line)
register int i;
register unsigned short line;
{
    code_byte(i | (line_fix(line) << I_LINE_SHIFT));
    last_instruction = &tcode->code[cchunksz - 1];
}

/*
 * NAME:	code->kfun()
 * DESCRIPTION:	generate code for a builtin kfun
 */
static void code_kfun(kf, line)
int kf;
unsigned short line;
{
    code_instr(I_CALL_KFUNC, line);
    code_byte(kf);
}

/*
 * NAME:	code->make()
 * DESCRIPTION:	create function code block
 */
static char *code_make(depth, nlocals, size)
unsigned short depth, *size;
int nlocals;
{
    register codechunk *l;
    register char *code;

    code = ALLOC(char, *size = 5 + here + line_info_size);
    *code++ = depth >> 8;
    *code++ = depth;
    *code++ = nlocals;
    *code++ = here >> 8;
    *code++ = here;

    /* collect all code blocks in one large block */
    for (l = lcode; l != tcode; l = l->next) {
	memcpy(code, l->code, CODE_CHUNK);
	code += CODE_CHUNK;
    }
    memcpy(code, l->code, cchunksz);
    code += cchunksz;
    line_make(code);
    code -= 5 + here;

    /* add blocks to free list */
    tcode->next = fcode;
    fcode = lcode;
    /* clear blocks */
    lcode = (codechunk *) NULL;
    tcode = (codechunk *) NULL;
    cchunksz = CODE_CHUNK;

    here = 0;
    return code;
}

/*
 * NAME:	code->clear()
 * DESCRIPTION:	clean up the code chunks
 */
static void code_clear()
{
    register codechunk *l, *f;

    for (l = fcode; l != (codechunk *) NULL; ) {
	f = l;
	l = l->next;
	FREE(f);
    }
    fcode = (codechunk *) NULL;

    line_clear();
}


# define JUMP_CHUNK	128

typedef struct _jmplist_ {
    unsigned short where;		/* where to jump from */
    short offset;			/* where to jump to */
    struct _jmplist_ *next;		/* next in list */
} jmplist;

typedef struct _jmpchunk_ {
    jmplist jump[JUMP_CHUNK];		/* chunk of jumps */
    struct _jmpchunk_ *next;		/* next in list */
} jmpchunk;

static jmpchunk *ljump;			/* list of jump chunks */
static jmpchunk *fjump;			/* list of free jump chunks */
static int jchunksz = JUMP_CHUNK;	/* size of jump chunk */
static jmplist *true_list;		/* list of true jumps */
static jmplist *false_list;		/* list of false jumps */
static jmplist *break_list;		/* list of break jumps */
static jmplist *continue_list;		/* list of continue jumps */

/*
 * NAME:	jump->offset()
 * DESCRIPTION:	generate a jump offset slot
 */
static jmplist *jump_offset(list)
jmplist *list;
{
    register jmplist *j;

    if (jchunksz == JUMP_CHUNK) {
	register jmpchunk *l;

	/* new chunk */
	if (fjump != (jmpchunk *) NULL) {
	    /* from free list */
	    l = fjump;
	    fjump = l->next;
	} else {
	    l = ALLOC(jmpchunk, 1);
	}
	l->next = ljump;
	ljump = l;
	jchunksz = 0;
    }
    j = &ljump->jump[jchunksz++];
    j->where = here;
    j->next = list;
    code_word(0);	/* empty space in code block filled in later */

    return j;
}

/*
 * NAME:	jump()
 * DESCRIPTION:	create a jump
 */
static jmplist *jump(i, list)
int i;
jmplist *list;
{
    code_instr(i, 0);
    return jump_offset(list);
}

/*
 * NAME:	jump->resolve()
 * DESCRIPTION:	resolve all jumps in a jump list
 */
static void jump_resolve(list, where)
register jmplist *list;
register unsigned short where;
{
    while (list != (jmplist *) NULL) {
	list->offset = where - list->where;
	list = list->next;
    }
}

/*
 * NAME:	jump->make()
 * DESCRIPTION:	fill in all jumps in a code block
 */
static void jump_make(code)
register char *code;
{
    register jmpchunk *l;
    register jmplist *j;
    register int i;
    jmplist *jmpjmp;

    i = jchunksz;
    jmpjmp = (jmplist *) NULL;
    for (l = ljump; l != (jmpchunk *) NULL; ) {
	jmpchunk *f;

	/*
	 * fill in jump addresses in code block
	 */
	j = &l->jump[i];
	do {
	    --j;
	    code[j->where    ] = j->offset >> 8;
	    code[j->where + 1] = j->offset;
	    if ((code[j->where + j->offset] & I_INSTR_MASK) == I_JUMP) {
		/*
		 * add to jump-to-jump list
		 */
		j->next = jmpjmp;
		jmpjmp = j;
	    }
	} while (--i > 0);
	i = JUMP_CHUNK;
	f = l;
	l = l->next;
	f->next = fjump;
	fjump = f;
    }

    for (j = jmpjmp; j != (jmplist *) NULL; j = j->next) {
	register unsigned short where, to;
	register short offset;

	/*
	 * replace jump-to-jump by a direct jump to destination
	 */
	where = j->where;
	offset = j->offset;
	while ((code[to = where + offset] & I_INSTR_MASK) == I_JUMP &&
	       offset != -1) {
	    /*
	     * Change to jump across the next jump.  If there is a loop, it
	     * will eventually result in a jump to itself.
	     */
	    to++;
	    offset = (code[to] << 8) | UCHAR(code[to + 1]);
	    offset += to - where;
	    code[where    ] = offset >> 8;
	    code[where + 1] = offset;
	    offset += where - to;
	    where = to;
	}
	/*
	 * jump to final destination
	 */
	offset += where - j->where;
	code[j->where    ] = offset >> 8;
	code[j->where + 1] = offset;
    }

    ljump = (jmpchunk *) NULL;
    jchunksz = JUMP_CHUNK;
}

/*
 * NAME:	jump->clear()
 * DESCRIPTION:	clean up the jump chunks
 */
static void jump_clear()
{
    register jmpchunk *l, *f;

    for (l = fjump; l != (jmpchunk *) NULL; ) {
	f = l;
	l = l->next;
	FREE(f);
    }
    fjump = (jmpchunk *) NULL;
}


static void cg_expr P((node*, bool));
static void cg_cond P((node*, bool));
static void cg_stmt P((node*));

static int nvars;		/* number of local variables */

/*
 * NAME:	codegen->aggr()
 * DESCRIPTION:	generate code for an aggregate
 */
static int cg_aggr(n)
register node *n;
{
    register int i;

    if (n == (node *) NULL) {
	return 0;
    }
    for (i = 1; n->type == N_PAIR; i++) {
	cg_expr(n->r.right, FALSE);
	n = n->l.left;
    }
    cg_expr(n, FALSE);
    return i;
}

/*
 * NAME:	codegen->map_aggr()
 * DESCRIPTION:	generate code for a mapping aggregate
 */
static int cg_map_aggr(n)
register node *n;
{
    register int i;

    if (n == (node *) NULL) {
	return 0;
    }
    for (i = 2; n->type == N_PAIR; i += 2) {
	cg_expr(n->r.right->r.right, FALSE);
	cg_expr(n->r.right->l.left, FALSE);
	n = n->l.left;
    }
    cg_expr(n->r.right, FALSE);
    cg_expr(n->l.left, FALSE);
    return i;
}

/*
 * NAME:	codegen->funargs()
 * DESCRIPTION:	generate code for function arguments
 */
static int cg_funargs(n)
register node *n;
{
    register int i;

    if (n == (node *) NULL) {
	return 0;
    }
    for (i = 1; n->type == N_PAIR; i++) {
	cg_expr(n->l.left, FALSE);
	n = n->r.right;
    }
    if (n->type == N_SPREAD) {
	cg_expr(n->l.left, FALSE);
	code_instr(I_SPREAD, n->line);
	code_byte(n->mod);
    } else {
	cg_expr(n, FALSE);
    }
    return i;
}

/*
 * NAME:	codegen->lvalue()
 * DESCRIPTION:	generate code for an lvalue
 */
static void cg_lvalue(n)
register node *n;
{
    if (n->type == N_CAST) {
	n = n->l.left;
    }
    switch (n->type) {
    case N_LOCAL:
	code_instr(I_PUSH_LOCAL_LVALUE, n->line);
	code_byte(nvars - (int) n->r.number - 1);
	break;

    case N_GLOBAL:
	code_instr(I_PUSH_GLOBAL_LVALUE, n->line);
	code_word((int) n->r.number);
	break;

    case N_INDEX:
	switch (n->l.left->type) {
	case N_LOCAL:
	    code_instr(I_PUSH_LOCAL_LVALUE, n->l.left->line);
	    code_byte(nvars - (int) n->l.left->r.number - 1);
	    break;

	case N_GLOBAL:
	    code_instr(I_PUSH_GLOBAL_LVALUE, n->l.left->line);
	    code_word((int) n->l.left->r.number);
	    break;

	case N_INDEX:
	    cg_expr(n->l.left->l.left, FALSE);
	    cg_expr(n->l.left->r.right, FALSE);
	    code_instr(I_INDEX_LVALUE, n->l.left->line);
	    break;

	default:
	    cg_expr(n->l.left, FALSE);
	    break;
	}
	cg_expr(n->r.right, FALSE);
	code_instr(I_INDEX_LVALUE, n->line);
	break;
    }
}

/*
 * NAME:	codegen->cast()
 * DESCRIPTION:	generate code for a cast
 */
static void cg_cast(type)
unsigned short type;
{
    code_instr(I_CAST, 0);
    if ((type & T_REF) != 0) {
	type = T_ARRAY;
    }
    code_byte(type);
}

/*
 * NAME:	codegen->fetch()
 * DESCRIPTION:	generate code for a fetched lvalue
 */
static void cg_fetch(n)
node *n;
{
    cg_lvalue(n);
    code_instr(I_FETCH, 0);
    if (n->type == N_CAST) {
	cg_cast(n->mod);
    }
}

/*
 * NAME:	codegen->asgnop()
 * DESCRIPTION:	generate code for an assignment operator
 */
static void cg_asgnop(n, op)
register node *n;
int op;
{
    cg_fetch(n->l.left);
    cg_expr(n->r.right, FALSE);
    code_kfun(op, n->line);
    if (n->l.left->mod != T_MIXED && n->r.right->mod == T_MIXED) {
	cg_cast(n->l.left->mod);
    }
    code_instr(I_STORE, 0);
}

/*
 * NAME:	codegen->expr()
 * DESCRIPTION:	generate code for an expression
 */
static void cg_expr(n, pop)
register node *n;
register bool pop;
{
    register jmplist *jlist, *j2list;
    register unsigned short i;
    long l;

    switch (n->type) {
    case N_ADD:
	cg_expr(n->l.left, FALSE);
	if (n->r.right->type == N_FLOAT) {
	    if (NFLT_ISONE(n->r.right)) {
		code_kfun(KF_ADD1, n->line);
		break;
	    }
	    if (NFLT_ISMONE(n->r.right)) {
		code_kfun(KF_SUB1, n->line);
		break;
	    }
	}
	cg_expr(n->r.right, FALSE);
	code_kfun(KF_ADD, n->line);
	break;

    case N_ADD_INT:
	cg_expr(n->l.left, FALSE);
	if (n->r.right->type == N_INT) {
	    if (n->r.right->l.number == 1) {
		code_kfun(KF_ADD1_INT, n->line);
		break;
	    }
	    if (n->r.right->l.number == -1) {
		code_kfun(KF_SUB1_INT, n->line);
		break;
	    }
	}
	cg_expr(n->r.right, FALSE);
	code_kfun(KF_ADD_INT, n->line);
	break;

    case N_ADD_EQ:
	cg_asgnop(n, KF_ADD);
	break;

    case N_ADD_EQ_INT:
	cg_asgnop(n, KF_ADD_INT);
	break;

    case N_ADD_EQ_1:
	cg_fetch(n->l.left);
	code_kfun(KF_ADD1, 0);
	code_instr(I_STORE, 0);
	break;

    case N_ADD_EQ_1_INT:
	cg_fetch(n->l.left);
	code_kfun(KF_ADD1_INT, 0);
	code_instr(I_STORE, 0);
	break;

    case N_AGGR:
	if (n->mod == T_MAPPING) {
	    i = cg_map_aggr(n->l.left);
	    code_instr(I_MAP_AGGREGATE, n->line);
	} else {
	    i = cg_aggr(n->l.left);
	    code_instr(I_AGGREGATE, n->line);
	}
	code_word(i);
	break;

    case N_AND:
	cg_expr(n->l.left, FALSE);
	cg_expr(n->r.right, FALSE);
	code_kfun(KF_AND, n->line);
	break;

    case N_AND_INT:
	cg_expr(n->l.left, FALSE);
	cg_expr(n->r.right, FALSE);
	code_kfun(KF_AND_INT, n->line);
	break;

    case N_AND_EQ:
	cg_asgnop(n, KF_AND);
	break;

    case N_AND_EQ_INT:
	cg_asgnop(n, KF_AND_INT);
	break;

    case N_ASSIGN:
	cg_lvalue(n->l.left);
	cg_expr(n->r.right, FALSE);
	code_instr(I_STORE, n->line);
	break;

    case N_CAST:
	cg_expr(n->l.left, FALSE);
	cg_cast(n->mod);
	break;

    case N_CATCH:
	jlist = jump((pop) ? I_CATCH | I_POP_BIT : I_CATCH, (jmplist *) NULL);
	cg_expr(n->l.left, TRUE);
	code_instr(I_PUSH_ZERO, n->line);
	code_instr(I_RETURN, 0);
	jump_resolve(jlist, here);
	return;

    case N_COMMA:
	cg_expr(n->l.left, TRUE);
	cg_expr(n->r.right, pop);
	return;

    case N_DIV:
	cg_expr(n->l.left, FALSE);
	cg_expr(n->r.right, FALSE);
	code_kfun(KF_DIV, n->line);
	break;

    case N_DIV_INT:
	cg_expr(n->l.left, FALSE);
	cg_expr(n->r.right, FALSE);
	code_kfun(KF_DIV_INT, n->line);
	break;

    case N_DIV_EQ:
	cg_asgnop(n, KF_DIV);
	break;

    case N_DIV_EQ_INT:
	cg_asgnop(n, KF_DIV_INT);
	break;

    case N_EQ:
	cg_expr(n->l.left, FALSE);
	cg_expr(n->r.right, FALSE);
	code_kfun(KF_EQ, n->line);
	break;

    case N_EQ_INT:
	cg_expr(n->l.left, FALSE);
	cg_expr(n->r.right, FALSE);
	code_kfun(KF_EQ_INT, n->line);
	break;

    case N_FLOAT:
	if (n->r.flow == 0L) {
	    code_instr(I_PUSH_FLOAT2, n->line);
	    code_word(n->l.fhigh);
	} else {
	    code_instr(I_PUSH_FLOAT6, n->line);
	    code_word(n->l.fhigh);
	    code_word((int) (n->r.flow >> 16));
	    code_word((int) n->r.flow);
	}
	break;

    case N_FUNC:
	i = cg_funargs(n->l.left);
	switch (n->r.number >> 24) {
	case KFCALL:
	    code_kfun((int) n->r.number, n->line);
	    if (PROTO_CLASS(KFUN((short) n->r.number).proto) & C_VARARGS) {
		code_byte(i);
	    }
	    break;

	case DFCALL:
	    if (((n->r.number >> 8) & 0xff) == 0) {
		code_instr(I_CALL_AFUNC, n->line);
		code_byte((int) n->r.number);
	    } else {
		code_instr(I_CALL_DFUNC, n->line);
		code_word((int) n->r.number);
	    }
	    code_byte(i);
	    break;

	case FCALL:
	    code_instr(I_CALL_FUNC, n->line);
	    code_word((int) n->r.number);
	    code_byte(i);
	    break;
	}
	break;

    case N_GE:
	cg_expr(n->l.left, FALSE);
	cg_expr(n->r.right, FALSE);
	code_kfun(KF_GE, n->line);
	break;

    case N_GE_INT:
	cg_expr(n->l.left, FALSE);
	cg_expr(n->r.right, FALSE);
	code_kfun(KF_GE_INT, n->line);
	break;

    case N_GLOBAL:
	code_instr(I_PUSH_GLOBAL, n->line);
	code_word((int) n->r.number);
	break;

    case N_GT:
	cg_expr(n->l.left, FALSE);
	cg_expr(n->r.right, FALSE);
	code_kfun(KF_GT, n->line);
	break;

    case N_GT_INT:
	cg_expr(n->l.left, FALSE);
	cg_expr(n->r.right, FALSE);
	code_kfun(KF_GT_INT, n->line);
	break;

    case N_INDEX:
	cg_expr(n->l.left, FALSE);
	cg_expr(n->r.right, FALSE);
	code_instr(I_INDEX, n->line);
	break;

    case N_INT:
	if (n->l.number == 0) {
	    code_instr(I_PUSH_ZERO, n->line);
	} else if (n->l.number == 1) {
	    code_instr(I_PUSH_ONE, n->line);
	} else if (n->l.number >> 7 == 0 || n->l.number >> 7 == -1) {
	    code_instr(I_PUSH_INT1, n->line);
	    code_byte((int) n->l.number);
	} else {
	    code_instr(I_PUSH_INT4, n->line);
	    code_word((int) (n->l.number >> 16));
	    code_word((int) n->l.number);
	}
	break;

    case N_LAND:
	if (!pop) {
	    jlist = true_list;
	    true_list = (jmplist *) NULL;
	    cg_cond(n, TRUE);
	    code_instr(I_PUSH_ZERO, 0);
	    j2list = jump(I_JUMP, (jmplist *) NULL);
	    jump_resolve(true_list, here);
	    true_list = jlist;
	    code_instr(I_PUSH_ONE, 0);
	    jump_resolve(j2list, here);
	} else {
	    jlist = false_list;
	    false_list = (jmplist *) NULL;
	    cg_cond(n->l.left, FALSE);
	    cg_expr(n->r.right, TRUE);
	    jump_resolve(false_list, here);
	    false_list = jlist;
	}
	return;

    case N_LE:
	cg_expr(n->l.left, FALSE);
	cg_expr(n->r.right, FALSE);
	code_kfun(KF_LE, n->line);
	break;

    case N_LE_INT:
	cg_expr(n->l.left, FALSE);
	cg_expr(n->r.right, FALSE);
	code_kfun(KF_LE_INT, n->line);
	break;

    case N_LOCAL:
	code_instr(I_PUSH_LOCAL, n->line);
	code_byte(nvars - (int) n->r.number - 1);
	break;

    case N_LOCK:
	code_instr(I_LOCK, 0);
	cg_expr(n->l.left, pop);
	code_instr(I_RETURN, 0);
	return;

    case N_LOR:
	if (!pop) {
	    jlist = false_list;
	    false_list = (jmplist *) NULL;
	    cg_cond(n, FALSE);
	    code_instr(I_PUSH_ONE, 0);
	    j2list = jump(I_JUMP, (jmplist *) NULL);
	    jump_resolve(false_list, here);
	    false_list = jlist;
	    code_instr(I_PUSH_ZERO, 0);
	    jump_resolve(j2list, here);
	} else {
	    jlist = true_list;
	    true_list = (jmplist *) NULL;
	    cg_cond(n->l.left, TRUE);
	    cg_expr(n->r.right, TRUE);
	    jump_resolve(true_list, here);
	    true_list = jlist;
	}
	return;

    case N_LSHIFT:
	cg_expr(n->l.left, FALSE);
	cg_expr(n->r.right, FALSE);
	code_kfun(KF_LSHIFT, n->line);
	break;

    case N_LSHIFT_INT:
	cg_expr(n->l.left, FALSE);
	cg_expr(n->r.right, FALSE);
	code_kfun(KF_LSHIFT_INT, n->line);
	break;

    case N_LSHIFT_EQ:
	cg_asgnop(n, KF_LSHIFT);
	break;

    case N_LSHIFT_EQ_INT:
	cg_asgnop(n, KF_LSHIFT_INT);
	break;

    case N_LT:
	cg_expr(n->l.left, FALSE);
	cg_expr(n->r.right, FALSE);
	code_kfun(KF_LT, n->line);
	break;

    case N_LT_INT:
	cg_expr(n->l.left, FALSE);
	cg_expr(n->r.right, FALSE);
	code_kfun(KF_LT_INT, n->line);
	break;

    case N_LVALUE:
	cg_lvalue(n->l.left);
	break;

    case N_MOD:
	cg_expr(n->l.left, FALSE);
	cg_expr(n->r.right, FALSE);
	code_kfun(KF_MOD, n->line);
	break;

    case N_MOD_INT:
	cg_expr(n->l.left, FALSE);
	cg_expr(n->r.right, FALSE);
	code_kfun(KF_MOD_INT, n->line);
	break;

    case N_MOD_EQ:
	cg_asgnop(n, KF_MOD);
	break;

    case N_MOD_EQ_INT:
	cg_asgnop(n, KF_MOD_INT);
	break;

    case N_MULT:
	cg_expr(n->l.left, FALSE);
	cg_expr(n->r.right, FALSE);
	code_kfun(KF_MULT, n->line);
	break;

    case N_MULT_INT:
	cg_expr(n->l.left, FALSE);
	cg_expr(n->r.right, FALSE);
	code_kfun(KF_MULT_INT, n->line);
	break;

    case N_MULT_EQ:
	cg_asgnop(n, KF_MULT);
	break;

    case N_MULT_EQ_INT:
	cg_asgnop(n, KF_MULT_INT);
	break;

    case N_NE:
	cg_expr(n->l.left, FALSE);
	cg_expr(n->r.right, FALSE);
	code_kfun(KF_NE, n->line);
	break;

    case N_NE_INT:
	cg_expr(n->l.left, FALSE);
	cg_expr(n->r.right, FALSE);
	code_kfun(KF_NE_INT, n->line);
	break;

    case N_NOT:
	cg_expr(n->l.left, FALSE);
	code_kfun(KF_NOT, n->line);
	break;

    case N_NOTF:
	cg_expr(n->l.left, FALSE);
	code_kfun(KF_NOTF, n->line);
	break;

    case N_NOTI:
	cg_expr(n->l.left, FALSE);
	code_kfun(KF_NOTI, n->line);
	break;

    case N_OR:
	cg_expr(n->l.left, FALSE);
	cg_expr(n->r.right, FALSE);
	code_kfun(KF_OR, n->line);
	break;

    case N_OR_INT:
	cg_expr(n->l.left, FALSE);
	cg_expr(n->r.right, FALSE);
	code_kfun(KF_OR_INT, n->line);
	break;

    case N_OR_EQ:
	cg_asgnop(n, KF_OR);
	break;

    case N_OR_EQ_INT:
	cg_asgnop(n, KF_OR_INT);
	break;

    case N_PAIR:
	cg_expr(n->l.left, pop);
	cg_expr(n->r.right, TRUE);
	return;

    case N_QUEST:
	if (n->r.right->l.left != (node *) NULL) {
	    jlist = false_list;
	    false_list = (jmplist *) NULL;
	    cg_cond(n->l.left, FALSE);
	    cg_expr(n->r.right->l.left, pop);
	    if (n->r.right->r.right != (node *) NULL) {
		j2list = jump(I_JUMP, (jmplist *) NULL);
		jump_resolve(false_list, here);
		false_list = jlist;
		cg_expr(n->r.right->r.right, pop);
		jump_resolve(j2list, here);
	    } else {
		jump_resolve(false_list, here);
		false_list = jlist;
	    }
	} else {
	    jlist = true_list;
	    true_list = (jmplist *) NULL;
	    cg_cond(n->l.left, TRUE);
	    if (n->r.right->r.right != (node *) NULL) {
		cg_expr(n->r.right->r.right, pop);
	    }
	    jump_resolve(true_list, here);
	    true_list = jlist;
	}
	return;

    case N_RANGE:
	cg_expr(n->l.left, FALSE);
	n = n->r.right;
	if (n->l.left != (node *) NULL &&
	    (n->l.left->type != N_INT || n->l.left->l.number != 0)) {
	    cg_expr(n->l.left, FALSE);
	    if (n->r.right != (node *) NULL) {
		cg_expr(n->r.right, FALSE);
		code_kfun(KF_RANGEFT, n->line);
	    } else {
		code_kfun(KF_RANGEF, n->line);
	    }
	} else if (n->r.right != (node *) NULL) {
	    cg_expr(n->r.right, FALSE);
	    code_kfun(KF_RANGET, n->line);
	} else {
	    code_kfun(KF_RANGE, n->line);
	}
	break;

    case N_RSHIFT:
	cg_expr(n->l.left, FALSE);
	cg_expr(n->r.right, FALSE);
	code_kfun(KF_RSHIFT, n->line);
	break;

    case N_RSHIFT_INT:
	cg_expr(n->l.left, FALSE);
	cg_expr(n->r.right, FALSE);
	code_kfun(KF_RSHIFT_INT, n->line);
	break;

    case N_RSHIFT_EQ:
	cg_asgnop(n, KF_RSHIFT);
	break;

    case N_RSHIFT_EQ_INT:
	cg_asgnop(n, KF_RSHIFT_INT);
	break;

    case N_STR:
	l = ctrl_dstring(n->l.string);
	if ((l & 0x01000000L) && (unsigned short) l < 256) {
	    code_instr(I_PUSH_STRING, n->line);
	    code_byte((int) l);
	} else if ((unsigned short) l < 256) {
	    code_instr(I_PUSH_NEAR_STRING, n->line);
	    code_byte((int) (l >> 16));
	    code_byte((int) l);
	} else {
	    code_instr(I_PUSH_FAR_STRING, n->line);
	    code_byte((int) (l >> 16));
	    code_word((int) l);
	}
	break;

    case N_SUB:
	if ((n->l.left->type == N_INT && n->l.left->l.number == 0) ||
	    (n->l.left->type == N_FLOAT && NFLT_ISZERO(n->l.left))) {
	    cg_expr(n->r.right, FALSE);
	    code_kfun(KF_UMIN, n->line);
	} else {
	    cg_expr(n->l.left, FALSE);
	    if (n->r.right->type == N_FLOAT) {
		if (NFLT_ISONE(n->r.right)) {
		    code_kfun(KF_SUB1, n->line);
		    break;
		}
		if (NFLT_ISMONE(n->r.right)) {
		    code_kfun(KF_ADD1, n->line);
		    break;
		}
	    }
	    cg_expr(n->r.right, FALSE);
	    code_kfun(KF_SUB, n->line);
	}
	break;

    case N_SUB_INT:
	if (n->l.left->type == N_INT && n->l.left->l.number == 0) {
	    cg_expr(n->r.right, FALSE);
	    code_kfun(KF_UMIN_INT, n->line);
	} else {
	    cg_expr(n->l.left, FALSE);
	    if (n->r.right->type == N_INT) {
		if (n->r.right->l.number == 1) {
		    code_kfun(KF_SUB1_INT, n->line);
		    break;
		}
		if (n->r.right->l.number == -1) {
		    code_kfun(KF_ADD1_INT, n->line);
		    break;
		}
	    }
	    cg_expr(n->r.right, FALSE);
	    code_kfun(KF_SUB_INT, n->line);
	}
	break;

    case N_SUB_EQ:
	cg_asgnop(n, KF_SUB);
	break;

    case N_SUB_EQ_INT:
	cg_asgnop(n, KF_SUB_INT);
	break;

    case N_SUB_EQ_1:
	cg_fetch(n->l.left);
	code_kfun(KF_SUB1, 0);
	code_instr(I_STORE, 0);
	break;

    case N_SUB_EQ_1_INT:
	cg_fetch(n->l.left);
	code_kfun(KF_SUB1_INT, 0);
	code_instr(I_STORE, 0);
	break;

    case N_TOFLOAT:
	cg_expr(n->l.left, FALSE);
	code_kfun(KF_TOFLOAT, n->line);
	break;

    case N_TOINT:
	cg_expr(n->l.left, FALSE);
	code_kfun(KF_TOINT, n->line);
	break;

    case N_TST:
	cg_expr(n->l.left, FALSE);
	code_kfun(KF_TST, n->line);
	break;

    case N_TSTF:
	cg_expr(n->l.left, FALSE);
	code_kfun(KF_TSTF, n->line);
	break;

    case N_TSTI:
	cg_expr(n->l.left, FALSE);
	code_kfun(KF_TSTI, n->line);
	break;

    case N_UPLUS:
	cg_expr(n->l.left, pop);
	return;

    case N_XOR:
	if (n->r.right->type == N_INT && n->r.right->l.number == -1) {
	    cg_expr(n->l.left, FALSE);
	    code_kfun(KF_NEG, n->line);
	} else {
	    cg_expr(n->l.left, FALSE);
	    cg_expr(n->r.right, FALSE);
	    code_kfun(KF_XOR, n->line);
	}
	break;

    case N_XOR_INT:
	if (n->r.right->type == N_INT && n->r.right->l.number == -1) {
	    cg_expr(n->l.left, FALSE);
	    code_kfun(KF_NEG_INT, n->line);
	} else {
	    cg_expr(n->l.left, FALSE);
	    cg_expr(n->r.right, FALSE);
	    code_kfun(KF_XOR_INT, n->line);
	}
	break;

    case N_XOR_EQ:
	if (n->r.right->type == N_INT && n->r.right->l.number == -1) {
	    cg_fetch(n->l.left);
	    code_kfun(KF_NEG, 0);
	    code_instr(I_STORE, 0);
	} else {
	    cg_asgnop(n, KF_XOR);
	}
	break;

    case N_XOR_EQ_INT:
	if (n->r.right->type == N_INT && n->r.right->l.number == -1) {
	    cg_fetch(n->l.left);
	    code_kfun(KF_NEG_INT, 0);
	    code_instr(I_STORE, 0);
	} else {
	    cg_asgnop(n, KF_XOR_INT);
	}
	break;

    case N_MIN_MIN:
	cg_fetch(n->l.left);
	code_kfun(KF_SUB1, 0);
	code_instr(I_STORE, 0);
	code_kfun((n->mod == T_INT) ? KF_ADD1_INT : KF_ADD1, 0);
	break;

    case N_MIN_MIN_INT:
	cg_fetch(n->l.left);
	code_kfun(KF_SUB1_INT, 0);
	code_instr(I_STORE, 0);
	code_kfun(KF_ADD1_INT, 0);
	break;

    case N_PLUS_PLUS:
	cg_fetch(n->l.left);
	code_kfun(KF_ADD1, 0);
	code_instr(I_STORE, 0);
	code_kfun((n->mod == T_INT) ? KF_SUB1_INT : KF_SUB1, 0);
	break;

    case N_PLUS_PLUS_INT:
	cg_fetch(n->l.left);
	code_kfun(KF_ADD1_INT, 0);
	code_instr(I_STORE, 0);
	code_kfun(KF_SUB1_INT, 0);
	break;
    }

    if (pop) {
	*last_instruction |= I_POP_BIT;
    }
}

/*
 * NAME:	codegen->cond()
 * DESCRIPTION:	generate code for a condition
 */
static void cg_cond(n, jmptrue)
register node *n;
register bool jmptrue;
{
    register jmplist *jlist;

    for (;;) {
	switch (n->type) {
	case N_INT:
	    if (jmptrue) {
		if (n->l.number != 0) {
		    true_list = jump(I_JUMP, true_list);
		}
	    } else if (n->l.number == 0) {
		false_list = jump(I_JUMP, false_list);
	    }
	    break;

	case N_LAND:
	    if (jmptrue) {
		jlist = false_list;
		false_list = (jmplist *) NULL;
		cg_cond(n->l.left, FALSE);
		cg_cond(n->r.right, TRUE);
		jump_resolve(false_list, here);
		false_list = jlist;
	    } else {
		cg_cond(n->l.left, FALSE);
		cg_cond(n->r.right, FALSE);
	    }
	    break;

	case N_LOR:
	    if (!jmptrue) {
		jlist = true_list;
		true_list = (jmplist *) NULL;
		cg_cond(n->l.left, TRUE);
		cg_cond(n->r.right, FALSE);
		jump_resolve(true_list, here);
		true_list = jlist;
	    } else {
		cg_cond(n->l.left, TRUE);
		cg_cond(n->r.right, TRUE);
	    }
	    break;

	case N_NOT:
	    jlist = true_list;
	    true_list = false_list;
	    false_list = jlist;
	    cg_cond(n->l.left, !jmptrue);
	    jlist = true_list;
	    true_list = false_list;
	    false_list = jlist;
	    break;

	case N_TST:
	    n = n->l.left;
	    continue;

	case N_COMMA:
	    cg_expr(n->l.left, TRUE);
	    n = n->r.right;
	    continue;

	default:
	    cg_expr(n, FALSE);
	    if (jmptrue) {
		true_list = jump(I_JUMP_NONZERO | I_POP_BIT, true_list);
	    } else {
		false_list = jump(I_JUMP_ZERO | I_POP_BIT, false_list);
	    }
	    break;
	}
	break;
    }
}

typedef struct {
    unsigned short where;		/* where to jump to */
    jmplist *jump;			/* list of unresolved jumps */
} case_label;

static case_label *switch_table;	/* label table for current switch */

/*
 * NAME:	codegen->switch_int()
 * DESCRIPTION:	generate single label code for a switch statement
 */
static void cg_switch_int(n)
register node *n;
{
    register node *m;
    register int i, size, sz;
    case_label *table;

    /*
     * switch expression
     */
    cg_expr(n->r.right->l.left, FALSE);

    /*
     * switch table
     */
    code_instr(I_SWITCH | I_POP_BIT, 0);
    code_byte(0);
    m = n->l.left;
    size = n->mod;
    sz = n->r.right->mod;
    if (m->l.left == (node *) NULL) {
	/* explicit default */
	m = m->r.right;
    } else {
	/* implicit default */
	size++;
    }
    code_word(size);
    code_byte(sz);

    table = switch_table;
    switch_table = ALLOCA(case_label, size);
    switch_table[0].jump = jump_offset((jmplist *) NULL);
    i = 1;
    do {
	register long l;

	l = m->l.left->l.number;
	switch (sz) {
	case 4:
	    code_word((int) (l >> 16));
	case 2:
	    code_word((int) l);
	    break;

	case 3:
	    code_byte((int) (l >> 16));
	    code_word((int) l);
	    break;

	case 1:
	    code_byte((int) l);
	    break;
	}
	switch_table[i++].jump = jump_offset((jmplist *) NULL);
	m = m->r.right;
    } while (i < size);

    /*
     * generate code for body
     */
    cg_stmt(n->r.right->r.right);

    /*
     * resolve jumps
     */
    if (size > n->mod) {
	/* default: across switch */
	switch_table[0].where = here;
    }
    for (i = 0; i < size; i++) {
	jump_resolve(switch_table[i].jump, switch_table[i].where);
    }
    AFREE(switch_table);
    switch_table = table;
}

/*
 * NAME:	codegen->switch_range()
 * DESCRIPTION:	generate range label code for a switch statement
 */
static void cg_switch_range(n)
register node *n;
{
    register node *m;
    register int i, size, sz;
    case_label *table;

    /*
     * switch expression
     */
    cg_expr(n->r.right->l.left, FALSE);

    /*
     * switch table
     */
    code_instr(I_SWITCH | I_POP_BIT, 0);
    code_byte(1);
    m = n->l.left;
    size = n->mod;
    sz = n->r.right->mod;
    if (m->l.left == (node *) NULL) {
	/* explicit default */
	m = m->r.right;
    } else {
	/* implicit default */
	size++;
    }
    code_word(size);
    code_byte(sz);

    table = switch_table;
    switch_table = ALLOCA(case_label, size);
    switch_table[0].jump = jump_offset((jmplist *) NULL);
    i = 1;
    do {
	register long l;

	l = m->l.left->l.number;
	switch (sz) {
	case 4:
	    code_word((int) (l >> 16));
	case 2:
	    code_word((int) l);
	    break;

	case 3:
	    code_byte((int) (l >> 16));
	    code_word((int) l);
	    break;

	case 1:
	    code_byte((int) l);
	    break;
	}
	l = m->l.left->r.number;
	switch (sz) {
	case 4:
	    code_word((int) (l >> 16));
	case 2:
	    code_word((int) l);
	    break;

	case 3:
	    code_byte((int) (l >> 16));
	    code_word((int) l);
	    break;

	case 1:
	    code_byte((int) l);
	    break;
	}
	switch_table[i++].jump = jump_offset((jmplist *) NULL);
	m = m->r.right;
    } while (i < size);

    /*
     * generate code for body
     */
    cg_stmt(n->r.right->r.right);

    /*
     * resolve jumps
     */
    if (size > n->mod) {
	/* default: across switch */
	switch_table[0].where = here;
    }
    for (i = 0; i < size; i++) {
	jump_resolve(switch_table[i].jump, switch_table[i].where);
    }
    AFREE(switch_table);
    switch_table = table;
}

/*
 * NAME:	codegen->switch_str()
 * DESCRIPTION:	generate code for a string switch statement
 */
static void cg_switch_str(n)
register node *n;
{
    register node *m;
    register int i, size;
    case_label *table;

    /*
     * switch expression
     */
    cg_expr(n->r.right->l.left, FALSE);

    /*
     * switch table
     */
    code_instr(I_SWITCH | I_POP_BIT, 0);
    code_byte(2);
    m = n->l.left;
    size = n->mod;
    if (m->l.left == (node *) NULL) {
	/* explicit default */
	m = m->r.right;
    } else {
	/* implicit default */
	size++;
    }
    code_word(size);

    table = switch_table;
    switch_table = ALLOCA(case_label, size);
    switch_table[0].jump = jump_offset((jmplist *) NULL);
    i = 1;
    if (m->l.left->type == N_INT) {
	/*
	 * 0
	 */
	code_byte(0);
	switch_table[i++].jump = jump_offset((jmplist *) NULL);
	m = m->r.right;
    } else {
	/* no 0 case */
	code_byte(1);
    }
    while (i < size) {
	register long l;

	l = ctrl_dstring(m->l.left->l.string);
	code_byte((int) (l >> 16));
	code_word((int) l);
	switch_table[i++].jump = jump_offset((jmplist *) NULL);
	m = m->r.right;
    }

    /*
     * generate code for body
     */
    cg_stmt(n->r.right->r.right);

    /*
     * resolve jumps
     */
    if (size > n->mod) {
	/* default: across switch */
	switch_table[0].where = here;
    }
    for (i = 0; i < size; i++) {
	jump_resolve(switch_table[i].jump, switch_table[i].where);
    }
    AFREE(switch_table);
    switch_table = table;
}

/*
 * NAME:	codegen->stmt()
 * DESCRIPTION:	generate code for a statement
 */
static void cg_stmt(n)
register node *n;
{
    register node *m;
    register jmplist *jlist, *j2list;
    register short offset;

    while (n != (node *) NULL) {
	if (n->type == N_PAIR) {
	    m = n->l.left;
	    n = n->r.right;
	} else {
	    m = n;
	    n = (node *) NULL;
	}
	switch (m->type) {
	case N_BLOCK:
	    if (m->mod == N_BREAK) {
		jlist = break_list;
		break_list = (jmplist *) NULL;
		cg_stmt(m->l.left);
		if (break_list != (jmplist *) NULL) {
		    jump_resolve(break_list, here);
		}
		break_list = jlist;
	    } else {
		jlist = continue_list;
		continue_list = (jmplist *) NULL;
		cg_stmt(m->l.left);
		if (continue_list != (jmplist *) NULL) {
		    jump_resolve(continue_list, here);
		}
		continue_list = jlist;
	    }
	    break;

	case N_BREAK:
	    break_list = jump(I_JUMP, break_list);
	    break;

	case N_CASE:
	    switch_table[m->mod].where = here;
	    cg_stmt(m->l.left);
	    break;

	case N_CONTINUE:
	    continue_list = jump(I_JUMP, continue_list);
	    break;

	case N_DO:
	    offset = here;
	    cg_stmt(m->r.right);
	    jlist = true_list;
	    true_list = (jmplist *) NULL;
	    cg_cond(m->l.left, TRUE);
	    jump_resolve(true_list, offset);
	    true_list = jlist;
	    break;

	case N_FOR:
	    if (m->r.right != (node *) NULL) {
		jlist = jump(I_JUMP, (jmplist *) NULL);
		offset = here;
		cg_stmt(m->r.right);
		jump_resolve(jlist, here);
	    } else {
		/* empty loop body */
		offset = here;
	    }
	    jlist = true_list;
	    true_list = (jmplist *) NULL;
	    cg_cond(m->l.left, TRUE);
	    jump_resolve(true_list, offset);
	    true_list = jlist;
	    break;

	case N_FOREVER:
	    offset = here;
	    if (m->l.left != (node *) NULL) {
		cg_expr(m->l.left, TRUE);
	    }
	    cg_stmt(m->r.right);
	    jump_resolve(jump(I_JUMP, (jmplist *) NULL), offset);
	    break;

	case N_IF:
	    if (m->r.right->l.left != (node *) NULL) {
		if (m->r.right->l.left->type == N_BREAK) {
		    jlist = true_list;
		    true_list = break_list;
		    cg_cond(m->l.left, TRUE);
		    break_list = true_list;
		    true_list = jlist;
		    if (m->r.right->r.right != (node *) NULL) {
			/* else */
			cg_stmt(m->r.right->r.right);
		    }
		    break;
		} else if (m->r.right->l.left->type == N_CONTINUE) {
		    jlist = true_list;
		    true_list = continue_list;
		    cg_cond(m->l.left, TRUE);
		    continue_list = true_list;
		    true_list = jlist;
		    if (m->r.right->r.right != (node *) NULL) {
			/* else */
			cg_stmt(m->r.right->r.right);
		    }
		    break;
		}
	    }
	    jlist = false_list;
	    false_list = (jmplist *) NULL;
	    cg_cond(m->l.left, FALSE);
	    cg_stmt(m->r.right->l.left);
	    if (m->r.right->r.right != (node *) NULL) {
		/* else */
		if (m->r.right->l.left != (node *) NULL &&
		    (m->r.right->l.left->flags & (F_BREAK | F_CONT | F_RETURN)))
		{
		    jump_resolve(false_list, here);
		    false_list = jlist;
		    cg_stmt(m->r.right->r.right);
		} else {
		    j2list = jump(I_JUMP, (jmplist *) NULL);
		    jump_resolve(false_list, here);
		    false_list = jlist;
		    cg_stmt(m->r.right->r.right);
		    jump_resolve(j2list, here);
		}
	    } else {
		/* no else */
		jump_resolve(false_list, here);
		false_list = jlist;
	    }
	    break;

	case N_PAIR:
	    cg_stmt(m);
	    break;

	case N_POP:
	    cg_expr(m->l.left, TRUE);
	    break;

	case N_RETURN:
	    cg_expr(m->l.left, FALSE);
	    code_instr(I_RETURN, m->line);
	    break;

	case N_SWITCH_INT:
	    cg_switch_int(m);
	    break;

	case N_SWITCH_RANGE:
	    cg_switch_range(m);
	    break;

	case N_SWITCH_STR:
	    cg_switch_str(m);
	    break;
	}
    }
}


static int nfuncs;		/* # functions generated */

/*
 * NAME:	codegen->init()
 * DESCRIPTION:	initialize the code generator
 */
void cg_init(inherited)
bool inherited;
{
    nfuncs = 0;
}

/*
 * NAME:	codegen->compiled()
 * DESCRIPTION:	return FALSE to signal that the code is interpreted, and not
 *		compiled
 */
bool cg_compiled()
{
    return FALSE;
}

/*
 * NAME:	codegen->function()
 * DESCRIPTION:	generate code for a function
 */
char *cg_function(fname, n, nvar, npar, depth, size)
string *fname;
node *n;
int nvar, npar;
unsigned short depth, *size;
{
    char *prog;

    nvars = nvar;
    cg_stmt(n);
    prog = code_make(depth + nvar, nvar - npar, size);
    jump_make(prog + 5);
    nfuncs++;

    return prog;
}

/*
 * NAME:	codegen->nfuncs()
 * DESCRIPTION:	return the number of functions generated
 */
int cg_nfuncs()
{
    return nfuncs;
}

/*
 * NAME:	codegen->clear()
 * DESCRIPTION:	clean up code generator
 */
void cg_clear()
{
    jump_clear();
    code_clear();
}
