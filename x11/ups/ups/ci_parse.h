/* ci_parse.h - header file for ci_parse.y */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)ci_parse.h	1.7 26/7/92 (UKC) */

typedef enum {
	OP_ASSIGN, OP_MUL_ASSIGN, OP_DIV_ASSIGN, OP_MOD_ASSIGN,
	OP_PLUS_ASSIGN, OP_MINUS_ASSIGN,
	OP_LSHIFT_ASSIGN, OP_RSHIFT_ASSIGN,
	OP_BITWISE_AND_ASSIGN, OP_BITWISE_XOR_ASSIGN, OP_BITWISE_OR_ASSIGN,

	OP_LOGICAL_OR, OP_LOGICAL_AND, OP_LOGICAL_NOT,

	OP_BITWISE_OR, OP_BITWISE_XOR, OP_BITWISE_AND, OP_BITWISE_NOT,
	
	OP_IS_EQUAL, OP_NOT_EQUAL,
	OP_LESS, OP_GREATER, OP_LESS_OR_EQUAL, OP_GREATER_OR_EQUAL,

	OP_LSHIFT, OP_RSHIFT,

	OP_PLUS, OP_MINUS, OP_MUL, OP_DIV, OP_MOD,

	OP_PREINC, OP_PREDEC, OP_POSTINC, OP_POSTDEC,

	OP_ADDRESS_OF, OP_DEREF,

	OP_UNARY_PLUS, OP_UNARY_MINUS,

	OP_COMMA,

	OP_CVT_TO_CHAR, OP_CVT_TO_UCHAR,
	OP_CVT_TO_SHORT, OP_CVT_TO_USHORT,
	OP_CVT_TO_INT, OP_CVT_TO_UINT,
	OP_CVT_TO_LONG, OP_CVT_TO_ULONG,
	OP_CVT_TO_DOUBLE, OP_CVT_TO_FLOAT,
	OP_CVT_TO_PTR, OP_CVT_TO_VOID,

	OP_LAST
} optype_t;

typedef enum {
	ET_CHAR_CONST,
	ET_INT_CONST,
	ET_FLOATING_CONST,
	ET_STRING_CONST,
	ET_ENUM_CONST,
	ET_VAR,
	ET_FUNCNAME,
	ET_UNDEF_VAR,
	ET_FUNC_CALL,
	ET_DOT,
	ET_MULTI_ARROW,
	ET_BINARY,
	ET_UNARY,
	ET_PROMOTION,
	ET_CAST,
	ET_ASSIGNMENT_CONVERSION,
	ET_SCALE,
	ET_SIZEOF,
	ET_CONDITIONAL
} expr_type_t;

typedef struct expr_listst {
	struct exprst *el_expr;
	struct expr_listst *el_next;
} expr_list_t;

typedef struct string_constst {
	const char *sc_val;
	long sc_size;
} string_const_t;

typedef enum {
	IT_INTVAL,
	IT_FLOATVAL,
	IT_STRINGVAL,
	IT_VARADDR,
	IT_FUNCADDR,
	IT_EXPR,
	IT_LIST
} inittype_t;

typedef struct initialiserst {
	inittype_t in_inittype;
	union {
		long inu_intval;
		double inu_floatval;
		struct initialiserst *inu_list;
		struct {
			union {
				var_t *v;
				string_const_t *sc;
			} addr;
			long offset;
		} inu_addr;
		struct {
			lexinfo_t *lexinfo;
			const char *name;
		} inu_func;
		struct exprst *inu_expr;
	} in_u;
	struct initialiserst *in_next;
} initialiser_t;

#define in_intval	in_u.inu_intval
#define in_floatval	in_u.inu_floatval
#define in_list		in_u.inu_list

#define in_addrvar	in_u.inu_addr.addr.v
#define in_stringval	in_u.inu_addr.addr.sc
#define in_offset	in_u.inu_addr.offset

#define in_func_name	in_u.inu_func.name
#define in_func_lexinfo	in_u.inu_func.lexinfo

#define in_expr		in_u.inu_expr

typedef struct initexprst {
	bool ie_is_list;
	union {
		struct exprst *ieu_expr;
		struct initexprst *ieu_list;
	} in_u;
	struct initexprst *ie_next;
} initexpr_t;
#define ie_expr	in_u.ieu_expr
#define ie_list	in_u.ieu_list

/*  Constant expression values.  Note that we make some assumptions
 *  about types sizes etc here - see ci_constexpr.c.
 */
typedef union {
	double cv_double;
	float cv_float;
	int cv_int;
	unsigned cv_unsigned;
} constval_t;

typedef struct declaratorst {
	type_t *dr_type;
	initexpr_t *dr_initialiser;
	struct declaratorst *dr_next;
} declarator_t;

typedef struct declarationst {
	class_t dn_class;
	type_t *dn_basetype;
	qualifiers_t dn_qualifiers;
	declarator_t *dn_declarators;
	struct declarationst *dn_next;
} declaration_t;

typedef struct func_call_exprst {
	struct exprst *fce_func;
	expr_list_t *fce_expr_list;
} func_call_expr_t;

typedef struct dot_expr_st {
	struct exprst *de_aggr;
	var_t *de_member;
} dot_expr_t;

typedef struct multi_arrow_expr_st {
	struct exprst *ma_aggr;
	struct exprst *ma_index;
	var_t *ma_member;
} multi_arrow_expr_t;

typedef struct scale_exprst {
	optype_t sc_op;
	struct exprst *sc_expr;
	long sc_factor;
} scale_expr_t;

typedef struct binary_exprst {
	optype_t be_op;
	struct exprst *be_left;
	struct exprst *be_right;
} binary_expr_t;

typedef struct unary_exprst {
	optype_t ue_op;
	struct exprst *ue_expr;
} unary_expr_t;

typedef struct cond_exprst {
	struct exprst *co_cond;
	struct exprst *co_if_true;
	struct exprst *co_if_false;
} cond_expr_t;

typedef struct sizeof_exprst {
	struct exprst *sz_expr;
	type_t *sz_type;
	taddr_t sz_size;
} sizeof_expr_t;

typedef struct {
	lexinfo_t *co_lexinfo;
	union {
		long cou_integer_val;
		double cou_floating_val;
		string_const_t cou_string_val;
	} co_u;
} constant_t;

#define co_integer_val	co_u.cou_integer_val
#define co_floating_val	co_u.cou_floating_val
#define co_string_val	co_u.cou_string_val

typedef enum {
	NT_VARNAME,
	NT_TAG,
	NT_ENUM_CONST,
	NT_TYPEDEF_NAME,
	NT_FUNCNAME
} nametype_t;

/*  An variable name, function name, typedef name or enum constant.
 */
typedef struct namedescst {
	const char *nd_name;
	nametype_t nd_nametype;
	lexinfo_t *nd_lexinfo;
	union {
		var_t *ndu_var;
		enum_member_t *ndu_enum_member;
		func_t *ndu_func;
		typedef_t *ndu_typedef;
	} nd_u;
} namedesc_t;

#define nd_var		nd_u.ndu_var
#define nd_enum_member	nd_u.ndu_enum_member
#define nd_func		nd_u.ndu_func
#define nd_typedef	nd_u.ndu_typedef

typedef struct exprst {
	expr_type_t ex_exprtype;
	type_t *ex_type;
	lexinfo_t *ex_lexinfo;
	bool ex_is_lvalue;
	bool ex_is_constant;
	union {
		func_call_expr_t *exu_func_call_expr;
		dot_expr_t *exu_dot_expr;
		multi_arrow_expr_t *exu_multi_arrow_expr;
		var_t *exu_var;
		enum_member_t *exu_enum_member;
		long exu_integer_constant_val;
		double exu_floating_constant_val;
		string_const_t *exu_string_constant_val;
		binary_expr_t *exu_binary_expr;
		unary_expr_t *exu_unary_expr;
		scale_expr_t *exu_scale_expr;
		cond_expr_t *exu_cond_expr;
		sizeof_expr_t *exu_sizeof_expr;
		const char *exu_undef_name;
	} ex_u;
} expr_t;
#define ex_func_call_expr		ex_u.exu_func_call_expr
#define ex_dot_expr			ex_u.exu_dot_expr
#define ex_multi_arrow_expr		ex_u.exu_multi_arrow_expr
#define ex_enum_member			ex_u.exu_enum_member
#define ex_var				ex_u.exu_var
#define ex_integer_constant_val		ex_u.exu_integer_constant_val
#define ex_floating_constant_val	ex_u.exu_floating_constant_val
#define ex_string_constant_val		ex_u.exu_string_constant_val
#define ex_binary_expr			ex_u.exu_binary_expr
#define ex_unary_expr			ex_u.exu_unary_expr
#define ex_scale_expr			ex_u.exu_scale_expr
#define ex_cond_expr			ex_u.exu_cond_expr
#define ex_sizeof_expr			ex_u.exu_sizeof_expr
#define ex_undef_name			ex_u.exu_undef_name

#define NO_LABELDESC	0xffff
#define MAX_LABELDESC	0xfffe

typedef unsigned int labeldesc_t;

typedef struct goto_labelst {
	const char *gl_name;
	lexinfo_t *gl_lexinfo;
	bool gl_used;
	bool gl_defined;
	labeldesc_t gl_labeldesc;		/* used in ci_compile.c */
	struct goto_labelst *gl_next;
} goto_label_t;

typedef struct labeled_stmst {
	goto_label_t *ls_goto_label;
	struct statementst *ls_stm;
} labeled_stm_t;

typedef struct case_labeled_stmst {
	labeldesc_t cs_labeldesc;
	long cs_val;
	expr_t *cs_expr;
	struct statementst *cs_stm;
} case_labeled_stm_t;

typedef struct compound_stmst {
	block_t *co_block;
	struct statementst *co_statements;
} compound_stm_t;

typedef struct if_stmst {
	expr_t *is_expr;
	struct statementst *is_ifpart;
	struct statementst *is_elsepart;
} if_stm_t;

typedef struct switch_stmst {
	expr_t *ss_expr;
	struct statementst *ss_stm;
	case_labeled_stm_t **ss_cstab;
	case_labeled_stm_t *ss_default_cs;
	int ss_ncase;
} switch_stm_t;

typedef struct while_stm_st {
	expr_t *ws_expr;
	struct statementst *ws_stm;
} while_stm_t;

typedef struct for_stm_st {
	expr_t *fs_init;
	expr_t *fs_test;
	expr_t *fs_reinit;
	struct statementst *fs_stm;
} for_stm_t;

typedef enum statement_typeen {
	STT_LABELED,
	STT_CASE_LABELED,
	STT_EXPR,
	STT_COMPOUND,
	STT_IF,
	STT_SWITCH,
	STT_WHILE,
	STT_DO,
	STT_FOR,
	STT_GOTO,
	STT_CONTINUE,
	STT_BREAK,
	STT_RETURN
} statement_type_t;

typedef struct statementst {
	statement_type_t st_type;
	union {
		labeled_stm_t *stu_labeled;
		case_labeled_stm_t *stu_case;
		expr_t *stu_expr;
		compound_stm_t *stu_compound;
		if_stm_t *stu_if;
		switch_stm_t *stu_switch;
		while_stm_t *stu_while;
		for_stm_t *stu_for;
		goto_label_t *stu_goto_label;
	} st_u;
	lexinfo_t *st_lexinfo;
	struct statementst *st_next;
} statement_t;

/*  Short names for the union members.
 */
#define st_labeled	st_u.stu_labeled
#define st_case		st_u.stu_case
#define st_expr		st_u.stu_expr
#define st_compound	st_u.stu_compound
#define st_if		st_u.stu_if
#define st_switch	st_u.stu_switch
#define st_while	st_u.stu_while
#define st_for		st_u.stu_for
#define st_goto_label	st_u.stu_goto_label

typedef struct { int bi_dummy; } *builtins_id_t;

typedef struct parse_resst {
	alloc_id_t pr_alloc_id;
	func_t *pr_funcs;
	block_t *pr_block;
} parse_res_t;
