/* HISTORY:
 ** Last edited: Oct 30 11:17 1993 (hws)
 **  Oct 15 19:05 1993 (hws): fix protect return a stmt NOT A STMT_LIST!
 **  Oct  3 12:14 1993 (hws): 'abstract' classes.
 **  Sep 15 00:22 1993 (hws): don't #undef free in ANSI C (SUN gcc at least mem faults)
 **  Sep  5 02:34 1993 (hws): little interface all.h (rather than all_.h) for ANSI C
 **  Mar  7 22:33 1993 (hws): make invariants named features
 **  Feb 23 00:15 1993 (hws): add invariants, assertions and old
 **  Feb 21 15:56 1993 (hws): eliminate debug, make assert like Sather 1
 **  Feb 21 15:55 1993 (hws): add require, ensure, define, undefine
 **  Sep 19 20:37 1992 (hws): tell line number of class begin
 **  Mar 15 11:58 1992 (hws): integrated exceptions from local compiler.
 **  Nov 27 08:06 1991 (hws): eliminated '=' sign in alias notation.
/*
/* YACC (or Bison or BYACC) grammar for the version of Sather written in 
   Sather. Tries to do minimal work in parser (eg. uses IDENTIFIER as
   high up as possible, etc.)

  Authors: Steve Omohundro (om@icsi.berkeley.edu) 
          Chu-Cheow Lim (clim@ICSI.Berkeley.EDU)
  Copyright (C) International Computer Science Institute, 1991, 1992, 1993 

  COPYRIGHT NOTICE: This code is provided "AS IS" WITHOUT ANY WARRANTY
  and is subject to the terms of the SATHER LIBRARY GENERAL PUBLIC
  LICENSE contained in the file: "sather/doc/license.txt" of the Sather
  distribution. The license is also available from ICSI, 1947 Center
  St., Suite 600, Berkeley CA 94704, USA.

  Changes: Heinz W. Schmidt (hws@csis.dit.csiro.au)
  (c) Commonwealth Scientific and Industrial Research Organisation (CSIRO),
  Australia, 1992, 1993.
  The modifications are provided "AS IS" WITHOUT ANY WARRANTY and are subject
  to the terms of the SATHER LIBRARY GENERAL PUBLIC LICENCE referred to above.
 
  $Id: david.y,v 1.1 1994/02/12 03:21:52 hsu Exp $
*/
%{

#include "all.h"

#include <stdio.h>

#if defined(rs6000) || defined(alpha)
#  undef free
#endif

extern ptr id_exprob_create();
extern ptr char_const_exprob_create();
extern ptr int_const_exprob_create();
extern ptr real_const_exprob_create();
extern ptr bool_const_exprob_create();
extern ptr str_const_exprob_create();

extern ptr op_exprob_create_unary();
extern ptr op_exprob_create_binary();

extern ptr aref_exprob_create();
extern ptr id_args_exprob_create();
extern ptr expr_args_exprob_create();
extern ptr typespec_args_exprob_create();

extern ptr exprob_put_name();

extern ptr lst_exprob_create();
extern ptr lst_exprob_push();
extern ptr lst_exprob_append();
extern ptr lst_stmtob_create();
extern ptr lst_stmtob_push();
extern ptr lst_stmtob_append();
extern ptr lst_when_stmtob_create();
extern ptr lst_when_stmtob_push();
extern ptr lst_declob_create();
extern ptr lst_declob_append();
extern ptr lst_typeob_create();
extern ptr lst_typeob_push();
extern ptr lst_featob_create();
extern ptr lst_featob_append();
extern ptr lst_featob_push();

extern ptr lst_int_create();
extern ptr lst_int_push();

extern ptr assert_stmtob_create();
extern ptr when_stmtob_create();
extern ptr switch_stmtob_create();
extern ptr loop_stmtob_create();
extern ptr elsif_stmtob_create();
extern ptr except_stmtob_create();
extern ptr cond_stmtob_create();
extern ptr assign_stmtob_create();
extern ptr call_stmtob_create();
extern ptr break_stmtob_create();
extern ptr return_stmtob_create();

extern ptr local_decl_stmtob_create();
extern ptr local_decl_stmtob_create_lst();

extern ptr simple_typeob_create();
extern ptr dispatch_typeob_create();
extern ptr param_typeob_create();

extern ptr const_decl_featob_create_lst();
extern ptr shared_decl_featob_create_lst();
extern ptr attr_decl_featob_create_lst();
extern ptr param_declob_create_lst();

extern ptr cinh_featob_create();
extern ptr rout_featob_create();
extern ptr alias_featob_create();
extern ptr rout_specob_create();

extern ptr any_declob_create();
extern int any_declob_ith_name();
extern ptr any_declob_get_type_spec();

extern ptr main_process_classdef();
extern ptr classob_table_install();
extern ptr classob_create();
extern ptr lst_featob_mark_private();
extern ptr lst_featob_mark_readonly();

extern ptr globals_classes_defs();

extern int str_table_index_of_str();
extern ptr str_table_at_index();
extern ptr str_buffer_create();
extern ptr str_buffer_strval();
extern char str_buffer_equal();
extern ptr str_buffer_terminate();
extern int str_buffer_length();
extern void str_buffer_init();
extern ptr str_buffer_push();
extern char str_buffer_pop();

extern int globals_curr_lineno;

/*
  Partial list of Sather constants used in "lexer.h".
  Refer to Sather definitions in "constants.sa".
*/

#define ABSTRACT_IND 66
#define AGAINST_IND 70
#define AND_IND 1
#define ASSERT_IND 2
#define ATTR_IND 72
#define BOOL_IND 42
#define BREAK_IND 3
#define CASE_IND 19
#define CLASS_IND 4
#define CONSTANT_IND 5
#define DEFINE_IND 62
#define ELSE_IND 7
#define ELSIF_IND 8
#define END_IND 9
#define ENSURE_IND 65
#define EXCEPTION_IND 60
#define FALSE_IND 35 
#define IF_IND 10
#define INVARIANT_IND 67
#define IS_IND 12
#define LOOP_IND 13
#define NEW_IND 31
#define NOT_IND 14
#define OR_IND 15
#define PRIVATE_IND 16
#define PROTECT_IND 69
#define RAISE_IND 68
#define READONLY_IND 73
#define REQUIRE_IND 64
#define RES_IND 33
#define RETURN_IND 17
#define SHARED_IND 18
#define THEN_IND 20
#define TRUE_IND 36
#define TYPECASE_IND 71
#define UNDEFINE_IND 63
#define UNDEF_TYPE_IND 55
#define UNTIL_IND 21
#define WHEN_IND 22
#define WHILE_IND 74

/* DPS - added */
#define INCLUDE_IND 11

#define NOT_OP_IND 1
#define LT_OP_IND 2
#define GT_OP_IND 3
#define LE_OP_IND 4
#define GE_OP_IND 5
#define EQ_OP_IND 6
#define NE_OP_IND 7
#define AND_OP_IND 8
#define OR_OP_IND 9
#define UMINUS_OP_IND 10
#define UPLUS_OP_IND 11
#define EXP_OP_IND 12
#define PLUS_OP_IND 13
#define MINUS_OP_IND 14
#define MULT_OP_IND 15
#define DIVIDE_OP_IND 16

%}

%union{
int lnno;                       /* the current Sather line number  */
int ind;			/* the index into the string table */
ptr val;			/* a pointer to a syntactic object */
}

%type <val> opt_type_vars ident_list feature_list feature featdef type_spec typid_list
%type <val> type_spec_list var_dec single_var_dec mult_ident_list concrete_type_spec
%type <val> shared_attr_dec var_dec_list routine_dec routine_def routine_spec
%type <val> alias_dec alias_list statement_list statement 
%type <val> local_dec assignment conditional elsif_part else_part loop case 
%type <val> when_part protected assert call arg_vals exp_list expr /* kexpr kexp_list */
%type <val> cexpr nexpr aref const_attr_dec 
%type <val> old_dec old_dec_list require ensure
%type <lnno> getlnno

%token ABSTRACT AGAINST ASSERT ASSIGN ATTR BREAK CASE CLASS CONSTANT DEFINE 
%token ELSE ELSIF END ENSURE INVARIANT IF IS 

/*DPS - was commented out - changed 'inline' to 'include' */
%token INCLUDE

%token LOOP PRIVATE PROTECT RAISE READONLY REQUIRE RETURN SHARED THEN TYPECASE
%token UNDEFINE UNTIL WHEN WHILE CREF
%token <val> CHAR_CONST INT_CONST REAL_CONST
/* The old version returns a string associated with STR_CONST. */
%token <ind> STR_CONST
%token <ind> BOOL_CONST
%token <ind> IDENTIFIER
%token <ind> TYPID

%left OR
%left  AND
%left '=' NE
%nonassoc LE '<' '>' GE
%left '+' '-'
%left '*' '/'
/*
%right '^'
*/
%right NOT
%left '.'
%nonassoc NO_OP
%right UNARY

%%
class_list:			/* Empty */
  | class			/* One class */
  | class_list ';'		/* Extra semi-colon */
  | class_list ';' class	/* Several classes */
  | class_list ';' error ';'
                                /* YACC will print an error msg */
;

class:
   CLASS TYPID getlnno opt_type_vars IS feature_list END
    { main_process_classdef(0, classob_create(NULL,$2,$3,$4,$6,NULL)); }

   | CLASS TYPID getlnno error IS feature_list END
    { main_process_classdef(0, classob_create(NULL,$2,$3,NULL,$6,NULL)); }

   | ABSTRACT CLASS '$' TYPID getlnno opt_type_vars IS feature_list END
    { main_process_classdef(0, classob_create(NULL,$4,$5,$6,$8,1)); }

   | ABSTRACT CLASS '$' TYPID getlnno error IS feature_list END
    { main_process_classdef(0, classob_create(NULL,$4,$5,NULL,$8,1)); }

/* DPS - added two cases to allow ignored, optional type bounds */
   | CLASS TYPID getlnno opt_type_vars '<' type_spec_list IS feature_list END
    { main_process_classdef(0, classob_create(NULL,$2,$3,$4,$8,NULL)); }

   | ABSTRACT CLASS '$' TYPID getlnno opt_type_vars '<' type_spec_list IS feature_list END
    { main_process_classdef(0, classob_create(NULL,$4,$5,$6,$10,1)); }
;

opt_type_vars:			/* Might not be any */
    { $$ = (ptr)NULL; }
  | '{'  typid_list '}'
    { $$ = $2; }
  | '{' error '}'
                                /* YACC will print an error msg */
    { $$ = (ptr)NULL; }
;

typid_list: TYPID			         /* Comma separated type identifiers */
    { $$ = (ptr)lst_int_push(lst_int_create(NULL,5),$1); }
  | TYPID '<' type_spec			/* type bound */
    { $$ = (ptr)lst_int_push(lst_int_create(NULL,5),$1); }
  | typid_list ',' TYPID
    { $$ = (ptr)lst_int_push($1,$3); }
;

ident_list: IDENTIFIER		/* Comma separated identifiers */
    { $$ = (ptr)lst_int_push(lst_int_create(NULL,5),$1); }
  | ident_list ',' IDENTIFIER
    { $$ = (ptr)lst_int_push($1,$3); }
;

feature_list:
    feature			/* One feature */
    { $$ = (ptr)$1; }           /* NOTE: A list of features is returned by
				   "feature". */
  | feature_list ';'		/* Extra semi-colon */
    { $$ = (ptr)$1; }

  | feature_list ';' feature  	/* Several features */
    { $$ = (ptr)lst_featob_append($1,$3); }
  | feature_list ';' error ';'
                                /* YACC will print an error msg */
    { $$ = (ptr)$1; }
  |		                /* Empty */
    { $$ = (ptr)lst_featob_create(NULL,5); }	
                                /* If NULL would get pimped by feature */
;


feature: ATTR var_dec		        /* unimplemented feature */
    { $$ = (ptr)attr_decl_featob_create_lst(NULL,$2,NULL); }
 
  | featdef			/* implemented feature */
    { $$ = (ptr)$1; }

  | PRIVATE featdef		/* private features */
    { $$ = (ptr)$2;
      lst_featob_mark_private($$); }

  | PRIVATE var_dec		/* private features */
    { $$ = (ptr)attr_decl_featob_create_lst(NULL,$2,NULL); 
      lst_featob_mark_private($$); }

  | const_attr_dec		/* Constant attribute */
    { $$ = (ptr)$1; }

  | PRIVATE const_attr_dec		/* Constant attribute */
    { $$ = (ptr)$2;
      lst_featob_mark_private($$); }

  | INVARIANT IDENTIFIER ':' getlnno expr getlnno /* named invariant */
    { $$ = (ptr)lst_featob_push(
		   lst_featob_create(NULL,1),
                   rout_featob_create(NULL,$2,
				      rout_specob_create(NULL,NULL,NULL,NULL),
				      NULL,
				      simple_typeob_create(NULL,BOOL_IND), 
				      lst_stmtob_push(lst_stmtob_create(NULL,1),
                                       assign_stmtob_create(NULL,
                                              id_exprob_create(NULL,RES_IND),
							    $5,$4))),
                   $4,$6);
      lst_featob_mark_spec($$); }				      
				
  | READONLY featdef		/* private features */
    { $$ = (ptr)$2;
      lst_featob_mark_readonly($$); }

  | READONLY var_dec		/* private features */
    { $$ = (ptr)attr_decl_featob_create_lst(NULL,$2,NULL); 
      lst_featob_mark_readonly($$); }

  | routine_dec END
    { $$ = (ptr)lst_featob_push(lst_featob_create(NULL,1),
				$1);
      lst_featob_mark_abstract($$); 
    }  
/* DPS - duplicated to add optional include */
  | type_spec
    { $$ = (ptr)lst_featob_push(lst_featob_create(NULL,1),
				cinh_featob_create(NULL,$1)); }
  | INCLUDE type_spec
    { $$ = (ptr)lst_featob_push(lst_featob_create(NULL,1),
				cinh_featob_create(NULL,$2)); }
;

featdef: ATTR var_dec ASSIGN expr	/* Initialized attribute */
    { $$ = (ptr)attr_decl_featob_create_lst(NULL,$2,$4); }
    
  | ATTR IDENTIFIER ':' ASSIGN type_spec CREF IDENTIFIER arg_vals /* a: :=FOO::.. */
    { $$ = (ptr)attr_decl_featob_create_lst
	(NULL,
	 any_declob_create(NULL,lst_int_push(lst_int_create(NULL,1),$2),
			       $5),
	 typespec_args_exprob_create(NULL,
					 $5,
					 $7,
					 $8)); }
  | ATTR IDENTIFIER CREF '=' type_spec CREF IDENTIFIER arg_vals /* a::=FOO::.. */
    { $$ = (ptr)attr_decl_featob_create_lst
	(NULL,
	 any_declob_create(NULL,lst_int_push(lst_int_create(NULL,1),$2),
			       $5),
	 typespec_args_exprob_create(NULL,
					 $5,
					 $7,
					 $8)); }
  | ATTR IDENTIFIER ':' ASSIGN '#' concrete_type_spec arg_vals /* a: := #FOO... */
    { $$ = (ptr)attr_decl_featob_create_lst
	 (NULL,
	  any_declob_create(NULL,lst_int_push(lst_int_create(NULL,1),$2),
			       $6),
	  typespec_args_exprob_create(NULL,
					 $6,
					 NEW_IND,
					 $7)); }

  | ATTR IDENTIFIER CREF '=' '#' concrete_type_spec arg_vals     /* a::= #FOO... */
    { $$ = (ptr)attr_decl_featob_create_lst
	 (NULL,
	  any_declob_create(NULL,lst_int_push(lst_int_create(NULL,1),$2),
			       $6),
	  typespec_args_exprob_create(NULL,
					 $6,
					 NEW_IND,
					 $7)); }

  | DEFINE alias_list		/* Alias features */
    { $$ = (ptr)$2; }

  | UNDEFINE ident_list       /* undefine name, use old form foo: undefine */
    { $$ = (ptr)attr_decl_featob_create_lst
	          (NULL,
		   any_declob_create(NULL,$2,
		      (ptr)simple_typeob_create(NULL,UNDEF_TYPE_IND)
				  ),
		   NULL); }

  | shared_attr_dec		/* Shared attribute */
    { $$ = (ptr)$1; }		    

  | routine_def			/* Routine */
    { $$ = (ptr)lst_featob_push(lst_featob_create(NULL,1),
				$1); }		    
;

alias_list: alias_dec             /* Alias definition */
    { $$ = (ptr)lst_featob_push(lst_featob_create(NULL,1),$1); }
  | alias_list ',' alias_dec
    { $$ = (ptr)lst_featob_push($1,$3); }
;

alias_dec: IDENTIFIER getlnno IDENTIFIER       /* Alias feature: newname oldname */
    { $$ = (ptr)alias_featob_create(NULL,$1,$3,$2); }
;

type_spec: concrete_type_spec
    { $$ = (ptr) $1; }
  | '$' concrete_type_spec
    { $$ = (ptr)dispatch_typeob_create(NULL,$2); }
  | ABSTRACT concrete_type_spec
    { $$ = (ptr)dispatch_typeob_create(NULL,$2); }
;

concrete_type_spec: TYPID	/* Simple name */
    { $$ = (ptr)simple_typeob_create(NULL,$1); }
  | TYPID '{' type_spec_list '}'     /* Parameterized type */
    { $$ = (ptr)param_typeob_create(NULL,$1,$3); }
  | TYPID '{'  error '}'
                                          /* YACC will print an error msg */
    { $$ = (ptr)simple_typeob_create(NULL,$1); }
;

type_spec_list: type_spec	     /* Just one */
    { $$ = (ptr)lst_typeob_push(lst_typeob_create(NULL,5),$1); }
  | type_spec_list ',' type_spec     /* Comma separated list */
    { $$ = (ptr)lst_typeob_push($1,$3); }
;

var_dec: single_var_dec		/* Single variable declaration (so can 
                                   use in routine) */
    { $$ = (ptr)$1; }
  | mult_ident_list ':' type_spec 
                                /* Declaration of set of variables */
    { $$ = (ptr)any_declob_create(NULL,$1,$3); }
/* DPS - implied 'arg' for unnamed single arg */
  | type_spec
        { $$ = (ptr)any_declob_create(NULL,
		       lst_int_push(lst_int_create(NULL,1),index_of_arg),
		       $1); }
;

single_var_dec: IDENTIFIER ':' type_spec
    { $$ = (ptr)any_declob_create(NULL,
			       lst_int_push(lst_int_create(NULL,1),$1),
			       $3); }
  | TYPID ':' type_spec			/* C shared must allow uppercase */
    { $$ = (ptr)any_declob_create(NULL,
			       lst_int_push(lst_int_create(NULL,1),$1),
			       $3); }
;

mult_ident_list: ident_list ',' IDENTIFIER 
                                    /* More than one elements in list */
    { $$ = (ptr)lst_int_push($1,$3); }
;

shared_attr_dec: SHARED var_dec   /* Uninitialized shared attribute */
    { $$ = (ptr)shared_decl_featob_create_lst(NULL,$2,NULL); }
  | SHARED var_dec ASSIGN expr    /* Initialized shared attribute */
    { $$ = (ptr)shared_decl_featob_create_lst(NULL,$2,$4); }
;

var_dec_list: var_dec		/* One declaration */
    { $$ = (ptr)param_declob_create_lst(NULL,$1); }
  | var_dec_list ';'		/* Extra semi-colon */
    { $$ = (ptr)$1; }
  | var_dec_list ';' var_dec	/* Several declarations */
    { $$ = (ptr)lst_declob_append($1,param_declob_create_lst(NULL,$3)); }
/* DPS -- made it okay to use commas in arg lists */
  | var_dec_list ','		/* Extra semi-colon */
    { $$ = (ptr)$1; }
  | var_dec_list ',' var_dec	/* Several declarations */
    { $$ = (ptr)lst_declob_append($1,param_declob_create_lst(NULL,$3)); }
;

routine_def: IDENTIFIER routine_spec IS getlnno statement_list getlnno END
    { $$ = (ptr)rout_featob_create(NULL,$1,$2,NULL,NULL,$5,$4,$6); }

  | IDENTIFIER '(' var_dec_list ')' routine_spec IS getlnno statement_list getlnno END
    { $$ = (ptr)rout_featob_create(NULL,$1,$5,$3,NULL,$8,$7,$9); }

  | single_var_dec routine_spec IS getlnno statement_list getlnno END
    { $$ = (ptr)rout_featob_create(NULL,
				any_declob_ith_name($1,0),
				$2,
				NULL,
				any_declob_get_type_spec($1),
				$5,
				$4,$6); }

  | IDENTIFIER '(' var_dec_list ')' ':' type_spec routine_spec 
  IS getlnno statement_list getlnno END
    { $$ = (ptr)rout_featob_create(NULL,$1,$7,$3,$6,$10,$9,$11); }

;

routine_dec: IDENTIFIER routine_spec
    { $$ = (ptr)rout_featob_create(NULL,$1,$2,NULL,NULL,
				   lst_stmtob_create(NULL,1),NULL,NULL); }

  | IDENTIFIER '(' var_dec_list ')' routine_spec
    { $$ = (ptr)rout_featob_create(NULL,$1,$5,$3,NULL,
				   lst_stmtob_create(NULL,1),NULL,NULL); }

  | single_var_dec routine_spec
    { $$ = (ptr)rout_featob_create(NULL,
				any_declob_ith_name($1,0),
				$2,
				NULL,
				any_declob_get_type_spec($1),
				lst_stmtob_create(NULL,1),NULL,NULL); }

  | IDENTIFIER '(' var_dec_list ')' ':' type_spec routine_spec
    { $$ = (ptr)rout_featob_create(NULL,$1,$7,$3,$6,
				   lst_stmtob_create(NULL,1),NULL,NULL); }

;

require: REQUIRE assert
    { $$ = (ptr)$2; }

  | require ';'
    { $$ = (ptr)$1; }

  | require ';' error ';'
    { $$ = (ptr)$1; }

;

ensure: ENSURE assert
    { $$ = (ptr)$2; }

  | ENSURE CONSTANT
    { $$ = (ptr) (ptr)id_exprob_create(NULL, CONSTANT_IND); }
  | ensure ';'
    { $$ = (ptr)$1; }

  | ensure ';' error ';'
    { $$ = (ptr)$1; }

;

routine_spec:
    { $$ = (ptr)rout_specob_create(NULL,NULL,NULL,NULL); }

  | require
    { $$ = (ptr)rout_specob_create(NULL,$1,NULL,NULL); }

  | ensure 
    { $$ = (ptr)rout_specob_create(NULL,NULL,NULL,$1); }

  | require ensure
    { $$ = (ptr)rout_specob_create(NULL,$1,NULL,$2); }

  | require ensure WHEN old_dec_list
    { $$ = (ptr)rout_specob_create(NULL,$1,$4,$2); }

  | ensure WHEN old_dec_list
    { $$ = (ptr)rout_specob_create(NULL,NULL,$3,$1); }
   
;

const_attr_dec: CONSTANT var_dec ASSIGN expr
    { $$ = (ptr)const_decl_featob_create_lst(NULL,$2,$4);
      lst_featob_mark_shared($$); }

  | CONSTANT ATTR var_dec ASSIGN expr
    { $$ = (ptr)const_decl_featob_create_lst(NULL,$3,$5); }

;

protected: getlnno statement_list /* the protected portion */
    { $$ = (ptr)except_stmtob_create(NULL,$2,NULL,NULL,$1)	; }
  | protected AGAINST getlnno type_spec THEN statement_list /* with handler */
    { $$ = (ptr)except_stmtob_create(NULL,
				     lst_stmtob_push(lst_stmtob_create(NULL,1),$1),
				     $6,
		     (ptr)any_declob_create(NULL,
                           lst_int_push(lst_int_create(NULL,1),EXCEPTION_IND), 
                           $4),
		     $3); }
;

statement_list:
    statement		  	   /* One statement */
    { $$ = (ptr)lst_stmtob_push(lst_stmtob_create(NULL,5),$1); }
  | local_dec		  	   /* Local declaration */
    { $$ = (ptr)local_decl_stmtob_create_lst(NULL,$1); }
  | statement_list ';'		   /* Extra semi-colon */
    { $$ = (ptr)$1; }
  | statement_list ';' statement   /* Several statements  */
    { $$ = (ptr)lst_stmtob_push($1,$3); }
  | statement_list ';' local_dec   
    { $$ = (ptr)lst_stmtob_append($1,local_decl_stmtob_create_lst(NULL,$3)); }
  | statement_list ';' error ';'
                                   /* YACC will print an error msg */
    { $$ = $1; }
  |			           /* Empty */
    { $$ = (ptr)lst_stmtob_create(NULL,5); }
;

/* 
  Local declaration is not reduced to statement first, because we
  need to expand local declaration into a list of declaration nodes,
  unlike other kinds of statements which are directly added.
*/
statement: IDENTIFIER
    { $$ = (ptr)call_stmtob_create(NULL,
				id_exprob_create(NULL,
					      $1)); }
  | assignment			
    { $$ = (ptr)$1; }
  | conditional
    { $$ = (ptr)$1; }
  | UNTIL getlnno '(' expr ')'           /* == if expr then break! end */
   { $$ = (ptr) cond_stmtob_create(NULL,$4, /* then */
				   lst_stmtob_push(lst_stmtob_create(NULL,1),
				   		   break_stmtob_create(NULL,NULL) ), 
				   lst_stmtob_create(NULL,1), /* empty elsif */
				   NULL,           /* empty else */
				   $2              /* line */
				   );			   
   }

  | WHILE getlnno '(' expr ')'           /* == if not expr then break! end */
   { $$ = (ptr) cond_stmtob_create(NULL, 
				   op_exprob_create_unary(NULL,NOT_OP_IND,$4),
				   /* then */
				   lst_stmtob_push(lst_stmtob_create(NULL,1),
				   		   break_stmtob_create(NULL,NULL) ), 
				   lst_stmtob_create(NULL,1), /* empty elsif */
				   NULL,           /* empty else */
				   $2              /* line */
				   );			   
   }

  | PROTECT protected END
    { $$ = (ptr)$2; }
  | loop
    { $$ = (ptr)$1; }
  | case
    { $$ = (ptr)$1; }
/*  | typecase    
    { $$ = (ptr)$1; } */
  | BREAK		/* Exit current block unconditionally */
    { $$ = (ptr)break_stmtob_create(NULL,NULL); }
	     /* Exit named block */
  /*  | BREAK IDENTIFIER
    { $$ = (ptr)break_stmtob_create(NULL,NULL); 
      exprob_put_name($$,$2); } */
  | RAISE expr                  /* Exceptional exit from current block */
    { $$ = (ptr)break_stmtob_create(NULL,$2); }
  /* Exceptional exit from named block  */
  /* | BREAK IDENTIFIER       
    { $$ = (ptr)break_stmtob_create(NULL,NULL); 
      exprob_put_name($$,$2); } */
  | RETURN			/* Return statement */
    { $$ = (ptr)return_stmtob_create(NULL); }
  | call
    { $$ = (ptr)call_stmtob_create(NULL,$1); }
  | ASSERT assert
    { $$ = $2; }
;

/* 
  Currently a LOCAL_DECL_STMTOB contains a list of names.  The
    expansion is done when pushed into the statement list.
*/
local_dec: var_dec		/* Uninitialized */
    { $$ = (ptr)local_decl_stmtob_create(NULL,$1,NULL); }
  | var_dec ASSIGN expr	        /* Initialized */
    { $$ = (ptr)local_decl_stmtob_create(NULL,$1,$3); }
  | IDENTIFIER ':' ASSIGN type_spec CREF IDENTIFIER arg_vals
    { $$ = (ptr)local_decl_stmtob_create
	(NULL,
	 any_declob_create(NULL,lst_int_push(lst_int_create(NULL,1),$1),
			       $4),
	 typespec_args_exprob_create(NULL,
					 $4,
					 $6,
					 $7)); }
  | IDENTIFIER CREF '=' type_spec CREF IDENTIFIER arg_vals
    { $$ = (ptr)local_decl_stmtob_create
	(NULL,
	 any_declob_create(NULL,lst_int_push(lst_int_create(NULL,1),$1),
			       $4),
	 typespec_args_exprob_create(NULL,
					 $4,
					 $6,
					 $7)); }
  | IDENTIFIER ':' ASSIGN '#' concrete_type_spec arg_vals
    { $$ = (ptr)local_decl_stmtob_create
	 (NULL,
	  any_declob_create(NULL,lst_int_push(lst_int_create(NULL,1),$1),
			       $5),
	  typespec_args_exprob_create(NULL,
					 $5,
					 NEW_IND,
					 $6)); }
  | IDENTIFIER CREF '=' '#' concrete_type_spec arg_vals
    { $$ = (ptr)local_decl_stmtob_create
	 (NULL,
	  any_declob_create(NULL,lst_int_push(lst_int_create(NULL,1),$1),
			       $5),
	  typespec_args_exprob_create(NULL,
					 $5,
					 NEW_IND,
					 $6)); }

;

old_dec: var_dec ASSIGN expr	/* Initialized */
    { $$ = (ptr)local_decl_stmtob_create(NULL,$1,$3); }

;

old_dec_list: old_dec /* Local declaration */
    { $$ = (ptr)local_decl_stmtob_create_lst(NULL,$1); }
  | old_dec_list ';' old_dec   
    { $$ = (ptr)lst_stmtob_append($1,local_decl_stmtob_create_lst(NULL,$3)); }
  | old_dec_list ';'
    { $$ = (ptr)$1; }
  | old_dec_list ';' error ';'
    { $$ = $1; }
;

assignment: expr getlnno ASSIGN expr        /* Check L-value during semantics */
    { $$ = (ptr)assign_stmtob_create(NULL,$1,$4,$2); }
;

conditional: 
    IF getlnno expr THEN statement_list
    elsif_part else_part END
    { $$ = (ptr)cond_stmtob_create(NULL,$3,$5,$6,$7,$2); }
;

elsif_part:			/* Empty */
    { $$ = (ptr)lst_stmtob_create(NULL,5); }
  | elsif_part ELSIF getlnno expr THEN statement_list
    { $$ = (ptr)lst_stmtob_push($1,elsif_stmtob_create(NULL,$4,$6,$3)); }
;

else_part:
    ELSE statement_list
    { $$ = (ptr)$2; }
  |			/* Empty */
    { $$ = (ptr)NULL; };

loop: LOOP getlnno statement_list END
   { $$ = (ptr)loop_stmtob_create(NULL,NULL,$3,$2); }
;

case: CASE getlnno expr when_part else_part END
    { $$ = (ptr)switch_stmtob_create(NULL,$3,$4,$5,$2); }
;

when_part:
    when_part WHEN exp_list THEN statement_list
    { $$ = (ptr)lst_when_stmtob_push($1,when_stmtob_create(NULL,$3,$5)); }
  |			/* Empty */
    { $$ = (ptr)lst_when_stmtob_create(NULL, 5);}     
;

assert: getlnno expr
  { $$ = (ptr)assert_stmtob_create(NULL,$2,$1); }
;

call: 
    IDENTIFIER '(' exp_list ')'
    { $$ = (ptr)id_args_exprob_create(NULL,
				   $1,
				   $3); }
/*  |
    IDENTIFIER '(' exp_list ',' kexp_list ')'
    { $$ = (ptr)id_args_exprob_create(NULL,
				   $1,
				   $3); } */

  | cexpr '.' IDENTIFIER arg_vals 
                                  /* Routine call on computed objects */
    { $$ = (ptr)expr_args_exprob_create(NULL,
				     $1,
				     $3,
				     $4); }
  | type_spec CREF IDENTIFIER arg_vals 
                                  /* Routine call in another class */
    { $$ = (ptr)typespec_args_exprob_create(NULL,
					 $1,
					 $3,
					 $4); }
  | type_spec CREF TYPID arg_vals 
                                  /* Routine call in C class may require upcase */
    { $$ = (ptr)typespec_args_exprob_create(NULL,
					 $1,
					 $3,
					 $4); }
  | '#' concrete_type_spec arg_vals 
                                  /* Routine call in another class */
    { $$ = (ptr)typespec_args_exprob_create(NULL,
					 $2,
					 NEW_IND,
					 $3); }
;

arg_vals:
    '(' exp_list ')'		/* List of argument values */
    { $$ = (ptr)$2; }
  |			        /* Empty */
    { $$ = NULL; };

exp_list: expr		/* One expressions */
    { $$ = (ptr)lst_exprob_push(lst_exprob_create(NULL, -1),$1); }
  | exp_list ',' expr	/* Several expressions */
    { $$ = (ptr)lst_exprob_push($1,$3); }
  | exp_list ',' error ','
                        /* YACC will print an error msg */
    { $$ = $1; }
;

/* kexp_list: kexpr           */  /* One expression */ /*
     { $$ = (ptr)lst_exprob_push(lst_exprob_create(NULL, -1),$1); }
  | kexp_list ',' kexpr	*/ /* Several expressions */ /*
    { $$ = (ptr)lst_exprob_push($1,$3); }
  | kexp_list ',' error ','
                       */ /* YACC will print an error msg */ /*
    { $$ = $1; }
; 

kexpr: IDENTIFIER ':' expr       */ /* keyword parameter passing for new */ /* 
                                 */ /* can perhaps be extended more generally later */ /* 
        { $$=$3; 
	  exprob_put_name($$,$1); }
; */

expr: cexpr			/* Callable expr can apply . or [] to it */
    { $$=$1; }
  | nexpr			/* Noncallable expresssion */
    { $$=$1; }
;

cexpr: 
    IDENTIFIER		/* Callable expression */
    { $$ = (ptr)id_exprob_create(NULL, $1); }
  | CHAR_CONST
    { $$ = (ptr)char_const_exprob_create(NULL, $1); }
  | INT_CONST
    { $$ = (ptr)int_const_exprob_create(NULL, $1); }
  | REAL_CONST
    { $$ = (ptr)real_const_exprob_create(NULL, $1); }
  | BOOL_CONST
    { $$ = (ptr)bool_const_exprob_create(NULL, $1); }
  | STR_CONST
    { $$ = (ptr)str_const_exprob_create(NULL, $1); }
  | call		
    { $$ = (ptr)$1; }
  | aref		
    { $$ = (ptr)$1; }
  | '(' expr ')'
    { $$ = $2; }
  | '(' error ')'                 
                                /* YACC will print an error msg */
    { $$ = (ptr)int_const_exprob_create(NULL, "0"); }
;


nexpr: NOT expr
    { $$ = (ptr)op_exprob_create_unary(NULL,NOT_OP_IND,$2); }
  | expr '<' expr
    { $$ = (ptr)op_exprob_create_binary(NULL,LT_OP_IND,$1,$3); }
  | expr '>' expr
    { $$ = (ptr)op_exprob_create_binary(NULL,GT_OP_IND,$1,$3); }
  | expr LE expr
    { $$ = (ptr)op_exprob_create_binary(NULL,LE_OP_IND,$1,$3); }
  | expr GE expr
    { $$ = (ptr)op_exprob_create_binary(NULL,GE_OP_IND,$1,$3); }
  | expr '=' expr
    { $$ = (ptr)op_exprob_create_binary(NULL,EQ_OP_IND,$1,$3); }
  | expr NE expr
    { $$ = (ptr)op_exprob_create_binary(NULL,NE_OP_IND,$1,$3); }
  | expr AND expr
    { $$ = (ptr)op_exprob_create_binary(NULL,AND_OP_IND,$1,$3); }
  | expr OR expr
    { $$ = (ptr)op_exprob_create_binary(NULL,OR_OP_IND,$1,$3); }
  | '-' expr      %prec UNARY
    { $$ = (ptr)op_exprob_create_unary(NULL,UMINUS_OP_IND,$2); }
  | '+' expr      %prec UNARY
    { $$ = (ptr)op_exprob_create_unary(NULL,UPLUS_OP_IND,$2); }
/*
  | expr '^' expr
    { $$ = (ptr)op_exprob_create_binary(NULL,EXP_OP_IND,$1,$3); }
*/
  | expr '+' expr
    { $$ = (ptr)op_exprob_create_binary(NULL,PLUS_OP_IND,$1,$3); }
  | expr '-' expr
    { $$ = (ptr)op_exprob_create_binary(NULL,MINUS_OP_IND,$1,$3); }
  | expr '*' expr
    { $$ = (ptr)op_exprob_create_binary(NULL,MULT_OP_IND,$1,$3); }
  | expr '/' expr
    { $$ = (ptr)op_exprob_create_binary(NULL,DIVIDE_OP_IND,$1,$3); }
;

aref: cexpr '[' exp_list ']'	/* Array reference */
    { $$ = (ptr)aref_exprob_create(NULL,$1,$3); }
  | '[' exp_list ']'		/* Reference to self when of array type */
    { $$ = (ptr)aref_exprob_create(NULL,NULL,$2); }
;

/* 
  A Rule that simply provides the current line number.
  Other Rules use this so that they may store the line number before the
  Sather construct they represent (e.g. to store the line number of the beginning
  of an until statement rather than the end).
*/
getlnno:
   { $$ = (int)globals_curr_lineno; }
;

/* end of grammar */
%%
#include "lexer.h"


