/* ci_parse.y - yacc grammar for ups expressions */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

%{

char ups_ci_parse_y_sccsid[] = "@(#)ci_parse.y	1.12 12/9/92 (UKC)";

#include <setjmp.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <local/ukcprog.h>

#include "ups.h"
#include "symtab.h"
#include "ci.h"
#include "ci_parse.h"
#include "ci_types.h"
#include "ci_util.h"
#include "ci_decl.h"
#include "ci_func.h"
#include "ci_expr.h"
#include "ci_stm.h"
#include "ci_lex.h"

int yyparse PROTO((void));

static int Switch_level = 0;
static int Loop_level = 0;

#define YYMAXDEPTH	400

%}

%union {
	int undef;
	constant_t *constant;
	statement_t *statement;
	expr_list_t *expr_list;
	expr_t *expr;
	type_t *type;
	class_t class;
	typecode_t typecode;
	qualifiers_t qualifier;
	func_t *function;
	declaration_t *declaration;
	declarator_t *declarator;
	var_t *varlist;
	enum_member_t *enum_member;
	identifier_t *identifier;
	identifier_list_t *identifier_list;
	optype_t optype;
	initexpr_t *initialiser;
	lexinfo_t *lexinfo;
}

/*  C keywords.
 */
%token <lexinfo> FOR BREAK CONTINUE RETURN
%token IF ELSE WHILE DO
%token SWITCH CASE DEFAULT GOTO
%token SIZEOF
%token AUTO REGISTER STATIC EXTERN TYPEDEF
%token VOID CHAR SHORT INT LONG FLOAT DOUBLE SIGNED UNSIGNED
%token CONST VOLATILE
%token STRUCT UNION ENUM

/*  Single character symbols
 */
%token AND		/*	'&'	*/
%token TILDE		/*	'~'	*/
%token NOT		/*	'!'	*/
%token LESSTHAN		/*	'<'	*/
%token GREATERTHAN	/*	'>'	*/
%token XOR		/*	'^'	*/
%token OR		/*	'|'	*/
%token PLUS		/*	'+'	*/
%token MINUS		/*	'-'	*/
%token SLASH		/*	'/'	*/
%token PERCENT		/*	'%'	*/
%token STAR		/*	'*'	*/
%token DOT		/*	'.'	*/
%token COLON		/*	':'	*/
%token QUERY		/*	'?'	*/
%token SEMI		/*	';'	*/
%token COMMA		/*	','	*/
%token LPAREN		/*	'('	*/
%token RPAREN		/*	')'	*/
%token LBRACE		/*	'{'	*/
%token RBRACE		/*	'}'	*/
%token LBRAC		/*	'['	*/
%token RBRAC		/*	']'	*/
%token EQUALS		/*	'='	*/

/*  Two character operators.
 */
%token STAR_EQUALS SLASH_EQUALS PERCENT_EQUALS PLUS_EQUALS MINUS_EQUALS
%token LSHIFT_EQUALS RSHIFT_EQUALS AND_EQUALS XOR_EQUALS OR_EQUALS
%token ANDAND OROR EQEQ NOTEQ GTEQ LESSEQ LSHIFT RSHIFT
%token PLUSPLUS MINUSMINUS
%token ARROW ELLIPSIS

%token <constant> INTEGER_CONSTANT CHARACTER_CONSTANT FLOATING_CONSTANT
%token <constant> STRING_CONSTANT
%token <identifier> IDENTIFIER TYPEDEF_NAME

%token BADTOK				/*  Returned by yylex for illegal input */

%start translation_unit

%type <undef> translation_unit external_declaration function_definition

%type <qualifier> type_qualifier type_qualifier_list

%type <statement> statement statement_list compound_statement
%type <statement> expression_statement
%type <statement> continue_statement break_statement return_statement
%type <statement> labeled_statement goto_statement
%type <statement> switch_statement case_labeled_statement
%type <statement> if_statement while_statement do_statement for_statement

%type <expr> expression opt_expression
%type <expr> assignment_expression conditional_expression
%type <expr> constant_expression logical_or_expression logical_and_expression
%type <expr> inclusive_or_expression exclusive_or_expression
%type <expr> and_expression equality_expression relational_expression
%type <expr> shift_expression
%type <expr> additive_expression multiplicative_expression
%type <expr> cast_expression unary_expression postfix_expression
%type <expr> primary_expression
%type <expr> constant

%type <identifier_list> identifier_list

%type <expr_list> opt_argument_expression_list argument_expression_list

%type <lexinfo> for

%type <function> function_declaration

%type <declaration> declaration_specifiers specifier_qualifier_list
%type <declaration> struct_declaration
%type <declaration> parameter_type_list parameter_list parameter_declaration
%type <declaration> struct_declaration_list

%type <declarator> init_declarator init_declarator_list
%type <declarator> struct_declarator struct_declarator_list
%type <declarator> declarator abstract_declarator

%type <typecode> struct_or_union

%type <enum_member> enumerator enumerator_list

%type <type> direct_declarator direct_abstract_declarator
%type <type> pointer

%type <type> type_name type_specifier
%type <type> enum_specifier struct_or_union_specifier 

%type <class> storage_class_specifier
%type <initialiser> initialiser initialiser_list

%type <optype>	assignment_operator

%%

translation_unit
	: external_declaration {
		$$ = 0;
	}
	| translation_unit external_declaration {
		$$ = 0;
	}

external_declaration
	: function_definition {
		if (Switch_level != 0 || Loop_level != 0)
			ci_panic("level botch in ed");
		$$ = 0;
	}
	| declaration {
		if (Switch_level != 0 || Loop_level != 0)
			ci_panic("level botch in ed");
		$$ = 0;
	}

function_definition
	: function_declaration {
		ci_check_func_decls($1);
	} compound_statement {
		ci_end_func($3);
	}

	| function_declaration declaration_list {
		ci_check_func_decls($1);
	} compound_statement {
		ci_end_func($4);
	}

function_declaration
	: declarator {
		declaration_t *dn;

		dn = ci_make_declaration(CL_DECL, ci_code_to_type(TY_INT), 0, $1);
		$$ = ci_start_func(dn);
	}
	| declaration_specifiers declarator {
		declaration_t *dn;

		ci_fix_signed_and_unsigned($1);
		dn = ci_make_declaration($1->dn_class, $1->dn_basetype,
					 $1->dn_qualifiers, $2);
		$$ = ci_start_func(dn);
	}

declaration
	: declaration_specifiers SEMI {
		ci_fix_signed_and_unsigned($1);
	}
	| declaration_specifiers init_declarator_list SEMI {
		ci_fix_signed_and_unsigned($1);
		$1->dn_declarators = $2;
		ci_handle_declaration($1);
	}

declaration_list
	: declaration
	| declaration_list declaration

declaration_specifiers
	: storage_class_specifier {
		$$ = ci_make_declaration($1, ci_code_to_type(TY_INT), 0,
							(declarator_t *)NULL);
	}
	| storage_class_specifier declaration_specifiers {
		ci_set_storage_class($2, $1);
		$$ = $2;
	}

	| type_specifier {
		$$ = ci_make_declaration(CL_NOCLASS, $1, 0, (declarator_t *)NULL);
	}
	| type_specifier declaration_specifiers {
		ci_add_type_specifier($2, $1);
		$$ = $2;
	}

	| type_qualifier {
		$$ = ci_make_declaration(CL_NOCLASS, (type_t *)NULL, $1,
							(declarator_t *)NULL);
	}
	| type_qualifier declaration_specifiers {
		$2->dn_qualifiers |= $1;
		$$ = $2;
	}

storage_class_specifier
	: AUTO		{ $$ = CL_AUTO;		}
	| REGISTER	{ $$ = CL_NOCLASS;	}
	| STATIC	{ $$ = CL_STAT;		}
	| EXTERN	{ $$ = CL_DECL;		}
	| TYPEDEF	{ $$ = CL_TYPEDEF;	}

type_specifier
	: VOID				{ $$ = ci_code_to_type(TY_VOID);	}
	| CHAR				{ $$ = ci_code_to_type(TY_CHAR);	}
	| SHORT				{ $$ = ci_code_to_type(TY_SHORT);	}
	| INT				{ $$ = ci_code_to_type(TY_INT);		}
	| LONG				{ $$ = ci_code_to_type(TY_LONG);	}
	| FLOAT				{ $$ = ci_code_to_type(TY_FLOAT);	}
	| DOUBLE			{ $$ = ci_code_to_type(TY_DOUBLE);	}
	| SIGNED			{ $$ = ci_code_to_type(TY_SIGNED);	}
	| UNSIGNED			{ $$ = ci_code_to_type(TY_UNSIGNED);	}
	| struct_or_union_specifier	{ $$ = $1;				}
	| enum_specifier		{ $$ = $1;				}
	| TYPEDEF_NAME			{ $$ = ci_lookup_typedef($1->id_name);	}

type_qualifier
	: CONST		{ $$ = QU_CONST; }
	| VOLATILE 	{ $$ = QU_VOLATILE; }

struct_or_union_specifier
	: struct_or_union LBRACE struct_declaration_list RBRACE {
		$$ = ci_build_aggr_or_enum_def($1, (identifier_t *)NULL, AE_COMPLETE,
					      $3, (enum_member_t *)NULL);
	}
	| struct_or_union IDENTIFIER LBRACE struct_declaration_list RBRACE {
		$$ = ci_build_aggr_or_enum_def($1, $2, AE_COMPLETE,
					      $4, (enum_member_t *)NULL);
	}
	| struct_or_union IDENTIFIER {
		typecode_t typecode;

		typecode = ($1 == TY_STRUCT) ? TY_U_STRUCT : TY_U_UNION;
		$$ = ci_build_aggr_or_enum_def(typecode, $2, AE_INCOMPLETE,
					      (declaration_t *)NULL,
					      (enum_member_t *)NULL);
	}

struct_or_union
	: STRUCT	{ $$ = TY_STRUCT;	}
	| UNION		{ $$ = TY_UNION;		}

struct_declaration_list
	: struct_declaration {
		$$ = $1;
	}
	| struct_declaration_list struct_declaration {
		$2->dn_next = $1;
		$$ = $2;
	}

init_declarator_list
	: init_declarator {
		$$ = $1;
	}
	| init_declarator_list COMMA init_declarator {
		$3->dr_next = $1;
		$$ = $3;
	}

init_declarator
	: declarator {
		$$ = $1;
	}
	| declarator EQUALS initialiser {
		$1->dr_initialiser = $3;
		$$ = $1;
	}

struct_declaration
	: specifier_qualifier_list struct_declarator_list SEMI {
		ci_fix_signed_and_unsigned($1);
		$$ = ci_make_declaration($1->dn_class, $1->dn_basetype,
					 $1->dn_qualifiers, $2);
	}

specifier_qualifier_list
	: type_specifier {
		$$ = ci_make_declaration(CL_NOCLASS, $1, 0, (declarator_t *)NULL);
	}
	| type_specifier specifier_qualifier_list {
		ci_add_type_specifier($2, $1);
		$$ = $2;
	}
	| type_qualifier {
		$$ = ci_make_declaration(CL_NOCLASS, (type_t *)NULL, $1,
							(declarator_t *)NULL);
	}
	| type_qualifier specifier_qualifier_list {
		$2->dn_qualifiers |= $1;
		$$ = $2;
	}

struct_declarator_list
	: struct_declarator {
		$$ = $1;
	}
	| struct_declarator_list COMMA struct_declarator {
		$3->dr_next = $1;
		$$ = $3;
	}

struct_declarator
	: declarator {
		$$ = $1;
	}
	| COLON constant_expression {
		$$ = ci_make_declarator(ci_make_expr_bitfield_type((type_t *)NULL,
									       $2));
	}
	| declarator COLON constant_expression {
		$$ = ci_make_declarator(ci_make_expr_bitfield_type($1->dr_type, $3));
	}

enum_specifier
	: ENUM LBRACE enumerator_list RBRACE {
		$$ = ci_build_aggr_or_enum_def(TY_ENUM, (identifier_t *)NULL,
					   AE_COMPLETE,
					   (declaration_t *)NULL, $3);
	}
	| ENUM IDENTIFIER LBRACE enumerator_list RBRACE {
		$$ = ci_build_aggr_or_enum_def(TY_ENUM, $2,
					   AE_COMPLETE,
					   (declaration_t *)NULL, $4);
	}
	| ENUM IDENTIFIER {
		$$ = ci_build_aggr_or_enum_def(TY_U_ENUM, $2,
					   AE_INCOMPLETE,
					   (declaration_t *)NULL,
					   (enum_member_t *)NULL);
	}

enumerator_list
	: enumerator {
		if ($1->em_expr_id == NULL)
			$1->em_val = 0;
		$1->em_next = NULL;
		$$ = $1;
	}
	| enumerator_list COMMA enumerator {
		if ($3->em_expr_id == NULL)
			$3->em_val = $1->em_val + 1;
		$3->em_next = $1;
		$$ = $3;
	}

enumerator
	: IDENTIFIER {
		$$ = ci_build_enum_member($1, (expr_t *)NULL);
	}
	| IDENTIFIER EQUALS constant_expression {
		$$ = ci_build_enum_member($1, $3);
	}

declarator
	: direct_declarator {
		$$ = ci_make_declarator($1);
	}
	| pointer direct_declarator {
		type_t *ptype;

		ptype = ci_push_types($2, $1);
		$$ = ci_make_declarator(ptype);
	}

direct_declarator
	: IDENTIFIER {
		type_t *type;
		
		type = ci_make_type(Parse_alloc_id, TY_IDENTIFIER);
		type->ty_identifier = $1;
		$$ = type;
	}
	| LPAREN declarator RPAREN {
		$$ = $2->dr_type;
	}

	| direct_declarator LBRAC RBRAC {
		$$ = ci_make_array_type($1, (expr_t *)NULL);
	}
	| direct_declarator LBRAC constant_expression RBRAC {
		$$ = ci_make_array_type($1, $3);
	}

	| direct_declarator LPAREN RPAREN {
		$$ = ci_make_funcret_type($1, FDT_IDLIST, (declaration_t *)NULL,
							(identifier_list_t *)NULL);
	}
	| direct_declarator LPAREN parameter_type_list RPAREN {
		$$ = ci_make_funcret_type($1, FDT_TYPELIST, $3,
							(identifier_list_t *)NULL);
	}

	| direct_declarator LPAREN identifier_list RPAREN {
		$$ = ci_make_funcret_type($1, FDT_IDLIST, (declaration_t *)NULL, $3);
	}

pointer
	: STAR {
		$$ = ci_make_pointer((type_t *)NULL, 0);
	}
	| STAR type_qualifier_list {
		$$ = ci_make_pointer((type_t *)NULL, $2);
	}
	| STAR pointer {
		$$ = ci_make_pointer($2, 0);
	}
	| STAR type_qualifier_list pointer {
		$$ = ci_make_pointer($3, $2);
	}

type_qualifier_list
	: type_qualifier {
		$$ = $1;
	}
	| type_qualifier_list type_qualifier {
		$$ = $1 | $2;
	}

parameter_type_list
	: parameter_list {
		$$ = $1;
	}
	| parameter_list COMMA ELLIPSIS  {
		declaration_t *dn;

		dn = ci_make_declaration(CL_ARG, ci_code_to_type(TY_ELLIPSIS), 0,
					 ci_make_declarator((type_t *)NULL));
		dn->dn_next = $1;
		$$ = dn;
	}

parameter_list
	: parameter_declaration {
		$$ = $1;
	}
	| parameter_list COMMA parameter_declaration {
		$3->dn_next = $1;
		$$ = $3;
	}

parameter_declaration
	: declaration_specifiers declarator {
		ci_fix_signed_and_unsigned($1);
		$$ = ci_make_declaration($1->dn_class, $1->dn_basetype,
					 $1->dn_qualifiers, $2);
	}
	| declaration_specifiers {
		ci_fix_signed_and_unsigned($1);
		$$ = ci_make_declaration($1->dn_class, $1->dn_basetype,
					 $1->dn_qualifiers,
					 ci_make_declarator((type_t *)NULL));
	} 
	| declaration_specifiers abstract_declarator {
		ci_fix_signed_and_unsigned($1);
		$$ = ci_make_declaration($1->dn_class, $1->dn_basetype,
					 $1->dn_qualifiers, $2);
	}

identifier_list
	: IDENTIFIER {
		identifier_list_t *idl = NEW(identifier_list_t);

		idl->idl_id = $1;
		idl->idl_next = NULL;
		$$ = idl;
	}
	| identifier_list COMMA IDENTIFIER {
		identifier_list_t *idl = NEW(identifier_list_t);

		idl->idl_id = $3;
		idl->idl_next = $1;
		$$ = idl;
	}

initialiser
	: assignment_expression {
		$$ = ci_make_initexpr(FALSE, $1, (initexpr_t *)NULL);
	}
	| LBRACE initialiser_list RBRACE {
		$$ = ci_make_initexpr(TRUE, (expr_t *)NULL, $2);
	}
	| LBRACE initialiser_list COMMA RBRACE {
		$$ = ci_make_initexpr(TRUE, (expr_t *)NULL, $2);
	}

initialiser_list
	: initialiser {
		$$ = $1;
	}
	| initialiser_list COMMA initialiser {
		$3->ie_next = $1;
		$$ = $3;
	}

type_name
	: specifier_qualifier_list {
		ci_fix_signed_and_unsigned($1);
		$$ = $1->dn_basetype;
	}
	| specifier_qualifier_list abstract_declarator {
		ci_fix_signed_and_unsigned($1);
		$$ = ci_push_types($1->dn_basetype, $2->dr_type);
	}

abstract_declarator
	: pointer {
		$$ = ci_make_declarator($1);
	}
	| direct_abstract_declarator {
		$$ = ci_make_declarator($1);
	}
	| pointer direct_abstract_declarator {
		$1->ty_base = $2;
		$$ = ci_make_declarator($1);
	}

direct_abstract_declarator
	: LPAREN abstract_declarator RPAREN {
		$$ = $2->dr_type;
	}

	| LBRAC RBRAC {
		$$ = ci_make_array_type((type_t *)NULL, (expr_t *)NULL);
	}
	| LBRAC constant_expression RBRAC {
		$$ = ci_make_array_type((type_t *)NULL, $2);
	}
	| direct_abstract_declarator LBRAC RBRAC {
		$$ = ci_make_array_type($1, (expr_t *)NULL);
	}
	| direct_abstract_declarator LBRAC constant_expression RBRAC {
		$$ = ci_make_array_type($1, $3);
	}

	| LPAREN RPAREN {
		$$ = ci_make_funcret_type((type_t *)NULL, FDT_IDLIST,
						(declaration_t *)NULL,
						(identifier_list_t *)NULL);
	}
	| LPAREN parameter_type_list RPAREN {
		$$ = ci_make_funcret_type((type_t *)NULL, FDT_TYPELIST, $2,
						(identifier_list_t *)NULL);
	}
	| direct_abstract_declarator LPAREN RPAREN {
		$$ = ci_make_funcret_type($1, FDT_IDLIST, (declaration_t *)NULL,
						(identifier_list_t *)NULL);
	}

	| direct_abstract_declarator LPAREN parameter_type_list RPAREN {
		$$ = ci_make_funcret_type($1, FDT_TYPELIST, $3,
						(identifier_list_t *)NULL);
	}
		
statement
	: labeled_statement
	| case_labeled_statement
	| expression_statement
	| compound_statement
	| if_statement
	| switch_statement
	| while_statement
	| do_statement
	| for_statement
	| goto_statement
	| return_statement
	| continue_statement
	| break_statement

labeled_statement
	: IDENTIFIER COLON statement {
		$$ = ci_make_labeled_statement($1, $3);
	}

case_labeled_statement
	: CASE constant_expression COLON statement {
		$$ = ci_make_case_labeled_statement(Switch_level != 0, $2, $4);
	}
	| DEFAULT COLON statement {
		$$ = ci_make_case_labeled_statement(Switch_level != 0,
								(expr_t *)NULL, $3);
	}

expression_statement
	: opt_expression SEMI {
		$$ = ci_make_expression_statement($1);
	}

compound_statement
	: start_block compound_statement_declarations RBRACE {
		$$ = ci_end_compound_statement((statement_t *)NULL);
	}
	| start_block compound_statement_declarations statement_list RBRACE {
		$$ = ci_end_compound_statement($3);
	}

compound_statement_declarations
	: /* empty */
	| declaration_list

start_block
	: LBRACE {
		ci_start_block(TRUE);
	}

statement_list
	: statement {
		$1->st_next = NULL;
		$$ = $1;
	}
	| statement_list statement {
		$2->st_next = $1;
		$$ = $2;
	}

if_statement
	: IF LPAREN expression RPAREN statement {
		$$ = ci_make_if_statement($3, $5, (statement_t *)NULL);
	}
	| IF LPAREN expression RPAREN statement ELSE statement {
		$$ = ci_make_if_statement($3, $5, $7);
	}

switch
	: SWITCH {
		++Switch_level;
	}

switch_statement
	: switch LPAREN expression RPAREN statement {
		--Switch_level;
		$$ = ci_make_switch_statement($3, $5);
	}

while
	: WHILE {
		++Loop_level;
	}

while_statement
	: while LPAREN expression RPAREN statement {
		--Loop_level;
		$$ = ci_make_while_statement(STT_WHILE, $3, $5);
	}

do
	: DO {
		++Loop_level;
	}

do_statement
	: do statement WHILE LPAREN expression RPAREN SEMI {
		--Loop_level;
		$$ = ci_make_while_statement(STT_DO, $5, $2);
	}

for
	: FOR {
		++Loop_level;
		$$ = $1;
	}

for_statement
	: for LPAREN opt_expression SEMI opt_expression SEMI opt_expression RPAREN
									statement {
		--Loop_level;
		$$ = ci_make_for_statement($3, $5, $7, $9, $1);
	}

opt_expression
	: /* nothing */ {
		$$ = NULL;
	}
	| expression {
		$$ = $1;
	}

goto_statement
	: GOTO IDENTIFIER SEMI {
		$$ = ci_make_goto_statement($2);
	}

continue_statement
	: CONTINUE SEMI {
		$$ = ci_make_continue_statement(Loop_level != 0, $1);
	}
	
break_statement
	: BREAK SEMI {
		$$ = ci_make_break_statement(Loop_level + Switch_level != 0, $1);
	}
	
return_statement
	: RETURN opt_expression SEMI {
		$$ = ci_make_return_statement($2, $1);
	}

expression
	: assignment_expression {
		$$ = $1;
	}
	| expression COMMA assignment_expression {
		$$ = ci_make_comma_expr($1, $3);
	}

assignment_expression
	: conditional_expression
	| unary_expression assignment_operator assignment_expression {
		$$ = ci_make_assignment_expr($2, $1, $3);
	}

assignment_operator
	: EQUALS		{ $$ = OP_ASSIGN;		}
	| STAR_EQUALS		{ $$ = OP_MUL_ASSIGN;		}
	| SLASH_EQUALS		{ $$ = OP_DIV_ASSIGN;		}
	| PERCENT_EQUALS	{ $$ = OP_MOD_ASSIGN;		}
	| PLUS_EQUALS		{ $$ = OP_PLUS_ASSIGN;		}
	| MINUS_EQUALS		{ $$ = OP_MINUS_ASSIGN;		}
	| LSHIFT_EQUALS		{ $$ = OP_LSHIFT_ASSIGN;	}
	| RSHIFT_EQUALS		{ $$ = OP_RSHIFT_ASSIGN;	}
	| AND_EQUALS		{ $$ = OP_BITWISE_AND_ASSIGN;	}
	| XOR_EQUALS		{ $$ = OP_BITWISE_XOR_ASSIGN;	}
	| OR_EQUALS		{ $$ = OP_BITWISE_OR_ASSIGN;	}

conditional_expression
	: logical_or_expression
	| logical_or_expression QUERY expression COLON conditional_expression {
		$$ = ci_make_conditional_expression($1, $3, $5);
	}

/*  In K&RII, this is given as "constant_expression: conditional_expression"
 *  but we want to give a better error message than "syntax error" for
 *  non constant expressions where constant expressions are needed.
 *  Thus we do checks in the semantic code rather than in the syntax.
 */
constant_expression
	: assignment_expression {
		$$ = $1;
	}

logical_or_expression
	: logical_and_expression
	| logical_or_expression OROR logical_and_expression {
		$$ = ci_make_logical_expr(OP_LOGICAL_OR, "||", $1, $3);
	}

logical_and_expression
	: inclusive_or_expression
	| logical_and_expression ANDAND inclusive_or_expression {
		$$ = ci_make_logical_expr(OP_LOGICAL_AND, "&&", $1, $3);
	}

inclusive_or_expression
	: exclusive_or_expression
	| inclusive_or_expression OR exclusive_or_expression {
		$$ = ci_make_bitwise_expr(OP_BITWISE_OR, "|", $1, $3);
	}

exclusive_or_expression
	: and_expression
	| exclusive_or_expression XOR and_expression {
		$$ = ci_make_bitwise_expr(OP_BITWISE_XOR, "^", $1, $3);
	}

and_expression
	: equality_expression
	| and_expression AND equality_expression {
		$$ = ci_make_bitwise_expr(OP_BITWISE_AND, "&", $1, $3);
	}

equality_expression
	: relational_expression
	| equality_expression EQEQ relational_expression {
		$$ = ci_make_comparison_expr(OP_IS_EQUAL, "==", $1, $3);
	}
	| equality_expression NOTEQ relational_expression {
		$$ = ci_make_comparison_expr(OP_NOT_EQUAL, "!=", $1, $3);
	}

relational_expression
	: shift_expression
	| relational_expression LESSTHAN shift_expression {
		$$ = ci_make_comparison_expr(OP_LESS, "<", $1, $3);
	}
	| relational_expression GREATERTHAN shift_expression {
		$$ = ci_make_comparison_expr(OP_GREATER, ">", $1, $3);
	}
	| relational_expression LESSEQ shift_expression {
		$$ = ci_make_comparison_expr(OP_LESS_OR_EQUAL, "<=", $1, $3);
	}
	| relational_expression GTEQ shift_expression {
		$$ = ci_make_comparison_expr(OP_GREATER_OR_EQUAL, ">=", $1, $3);
	}

shift_expression
	: additive_expression
	| shift_expression LSHIFT additive_expression {
		$$ = ci_make_shift_expr(OP_LSHIFT, "<<", $1, $3);
	}
	| shift_expression RSHIFT additive_expression {
		$$ = ci_make_shift_expr(OP_RSHIFT, ">>", $1, $3);
	}

additive_expression
	: multiplicative_expression
	| additive_expression PLUS multiplicative_expression {
		$$ = ci_make_add_or_subtract_expr(OP_PLUS, "+", $1, $3);
	}
	| additive_expression MINUS multiplicative_expression {
		$$ = ci_make_add_or_subtract_expr(OP_MINUS, "-", $1, $3);
	}


multiplicative_expression
	: cast_expression
	| multiplicative_expression STAR cast_expression {
		$$ = ci_make_mul_or_div_expr(OP_MUL, "*", $1, $3);
	}
	| multiplicative_expression SLASH cast_expression {
		$$ = ci_make_mul_or_div_expr(OP_DIV, "/", $1, $3);
	}
	| multiplicative_expression PERCENT cast_expression {
		$$ = ci_make_mod_expr(OP_MOD, "%", $1, $3);
	}

cast_expression
	: unary_expression
	| LPAREN type_name RPAREN cast_expression {
		$$ = ci_make_cast_expr($2, $4);
	}

unary_expression
	: postfix_expression {
		$$ = $1;
	}
	| PLUSPLUS unary_expression {
		$$ = ci_make_inc_or_dec_expr(OP_PREINC, $2, "prefix ++");
	}
	| MINUSMINUS unary_expression {
		$$ = ci_make_inc_or_dec_expr(OP_PREDEC, $2, "prefix --");
	}
	| STAR cast_expression {
		$$ = ci_make_deref_expr($2);
	}
	| AND cast_expression {
		$$ = ci_make_address_of_expr($2);
	}
	| PLUS cast_expression {
		$$ = ci_make_unary_plus_expr($2);
	}
	| MINUS cast_expression {
		$$ = ci_make_unary_minus_expr($2);
	}
	| TILDE cast_expression {
		$$ = ci_make_bitwise_not_expr($2);
	}
	| NOT cast_expression {
		$$ = ci_make_logical_not_expr($2);
	}
	| SIZEOF unary_expression {
		$$ = ci_make_sizeof_expr($2, (type_t *)NULL);
	}
	| SIZEOF LPAREN type_name RPAREN {
		$$ = ci_make_sizeof_expr((expr_t *)NULL, $3);
	}

/*  We lose the K&RII unary_operator production - it is moved up into
 *  the unary_expression production.
 */

postfix_expression
	: primary_expression
	| postfix_expression LBRAC expression RBRAC {
		/*  Rewrite as *($1 + $3).
		 */
		$$ = ci_make_deref_expr(ci_make_add_or_subtract_expr(OP_PLUS, "+",
									   $1, $3));
	}
	| postfix_expression LPAREN opt_argument_expression_list RPAREN {
		$$ = ci_make_func_call_expr($1, $3);
	}
	| postfix_expression DOT IDENTIFIER {
		$$ = ci_make_dot_expr($1, $3, "dot");
	}
	| postfix_expression ARROW LBRAC expression RBRAC IDENTIFIER {
		$$ = ci_make_multi_arrow_expr($1, $4, $6);
	}
	| postfix_expression ARROW IDENTIFIER {
		/*  Rewrite as (*$1).$3
		 */
		$$ = ci_make_dot_expr(ci_make_deref_expr($1), $3, "arrow");
	}
	| postfix_expression PLUSPLUS {
		$$ = ci_make_inc_or_dec_expr(OP_POSTINC, $1, "postfix ++");
	}
	| postfix_expression MINUSMINUS {
		$$ = ci_make_inc_or_dec_expr(OP_POSTDEC, $1, "postfix --");
	}

primary_expression
	: IDENTIFIER {
		$$ = ci_make_identifier_expr($1);
	}
	| constant {
		$$ = $1;
	}
	| STRING_CONSTANT {
		$$ = ci_make_string_constant_expr($1->co_lexinfo,
						  &$1->co_string_val);
	}
	| LPAREN expression RPAREN {
		$$ = $2;
	}

opt_argument_expression_list
	: /* empty */ {
		$$ = NULL;
	}
	| argument_expression_list {
		$$ = $1;
	}

argument_expression_list
	: assignment_expression {
		expr_list_t *el = NEW(expr_list_t);

		el->el_expr = $1;
		el->el_next = NULL;
		$$ = el;
	}
	| argument_expression_list COMMA assignment_expression {
		expr_list_t *el = NEW(expr_list_t);

		el->el_expr = $3;
		el->el_next = $1;
		$$ = el;
	}

constant
	: INTEGER_CONSTANT {
		$$ = ci_make_integer_constant_expr(ET_INT_CONST,
						   $1->co_lexinfo,
						   $1->co_integer_val);
	}
	| CHARACTER_CONSTANT {
		$$ = ci_make_integer_constant_expr(ET_CHAR_CONST,
						   $1->co_lexinfo,
						   $1->co_integer_val);
	}
	| FLOATING_CONSTANT {
		$$ = ci_make_floating_constant_expr($1->co_lexinfo,
						    $1->co_floating_val);
	}
	/*  We lose the K&R2 terminal ENUMERATION_CONSTANT - this is
	 *  taken care of in the action for identifier.
	 */

%%

/*  Resolve the C typedef ambiguity - return IDENTIFIER or TYPEDEF
 *  as the type of the string name.
 */
token_t
name_type(name)
const char *name;
{
	return (ci_lookup_typedef(name) != NULL) ? TYPEDEF_NAME : IDENTIFIER;
}

parse_id_t
ci_parse_file(parse_id, filename, block, flags, report_error_func,
				resolve_name_func, getline_func, getline_arg)
parse_id_t parse_id;
const char *filename;
block_t *block;
unsigned long flags;
ci_report_error_func_t report_error_func;
ci_resolve_name_func_t resolve_name_func;
const char *(*getline_func)PROTO((char *arg));
char *getline_arg;
{
	lex_env_t lebuf;
	int res;

	lebuf.le_report_func = report_error_func;
	lebuf.le_getline = getline_func;
	lebuf.le_getline_arg = getline_arg;
	lebuf.le_filename = filename;
	lebuf.le_had_error = FALSE;
	lebuf.le_lnum = -1;
	lebuf.le_lptr = "";
	lebuf.le_abort_parse = FALSE;

	Lex_env = &lebuf;
	ci_set_diag_handler(ci_lex_error, (char *)&lebuf);
	ci_start_parse_tree((parse_res_t *)parse_id, resolve_name_func,
								block, flags);
	Loop_level = Switch_level = 0;

	if ((flags & CI_CP_DONT_PANIC) == 0)
		res = yyparse();
	else {
		extern bool ci_Catching_panics;
		extern jmp_buf ci_Catch_panic_env;

		if (ci_Catching_panics)
			panic("catch_panics botch");
		ci_Catching_panics = TRUE;

		if (setjmp(ci_Catch_panic_env) != 0)
			res = -1;
		else
			res = yyparse();
		
		ci_Catching_panics = FALSE;
	}

	ci_set_diag_handler((diag_handler_func_t)NULL, (char *)NULL);
	Lex_env = NULL;

	/*  We call yylex with Lex_env NULL, to make it put out a newline
	 *  when debugging.
	 */
	(void) yylex();

	return (parse_id_t)ci_end_parse_tree(res == 0 && !lebuf.le_had_error);
}
