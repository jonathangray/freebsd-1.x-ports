/*  -*- Mode: C;  -*-
 * File: lexer.h
 * Author: Stephen M. Omohundro (om@ICSI.Berkeley.EDU)
 * Copyright (C) International Computer Science Institute, 1991, 1992, 1993 
 *
 * COPYRIGHT NOTICE: This code is provided "AS IS" WITHOUT ANY WARRANTY
 * and is subject to the terms of the SATHER LIBRARY GENERAL PUBLIC
 * LICENSE contained in the file: "sather/doc/license.txt" of the Sather
 * distribution. The license is also available from ICSI, 1947 Center
 * St., Suite 600, Berkeley CA 94704, USA.
 * 
 * Changes: Heinz W. Schmidt (hws@csis.dit.csiro.au)
 * (c) Commonwealth Scientific and Industrial Research Organisation (CSIRO),
 * Australia, 1992, 1993.
 * The modifications are provided "AS IS" WITHOUT ANY WARRANTY and are subject
 * to the terms of the SATHER LIBRARY GENERAL PUBLIC LICENCE referred to above.
 *  **~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 ** FUNCTION: Lexical analyzer for Sather.
 **
 ** RELATED PACKAGES: sather.y
 **
 ** RCS: $Id: lexer.h,v 1.1 1994/02/12 03:21:46 hsu Exp $
 ** HISTORY:
 ** Last edited: Oct 24 23:18 1993 (hws)
 **  Oct 24 23:18 1993 (hws): add include keyword
 **  Oct  9 00:32 1993 (hws): remove unreached return stmt.
 **  Sep 27 10:46 1993 (hws): Sather 1 exception syntax
 **  Feb 21 17:15 1993 (hws): remove debug
 **  Feb 21 14:58 1993 (hws): alias kwd is "define" now, fix error msg's.
 **  Oct  9 01:49 1992 (hws): str_buffer_terminate must be reassigned to buf.
 **  Oct  8 23:31 1992 (hws): more uniform error messages
 **  Sep 19 17:24 1992 (hws): change error format so Emacs edit-errors can parse.
 **  Aug 28 03:48 1991 (hws): get_string_const fixed to not accept unescaped RET.
 **  Modified: Mon Jul 16 12:55:34 PDT 1990
 **  Chu-Cheow Lim
 **  -- Change the code to use / interface with Sather classes.
 **  -- Rewrite code for reading numeric constants.
 ** Created: Thu Feb 20 11:04:58 1992 (hws)
 **~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */
/*
Strips out comments and whitespace.

Recognizes the keywords and poduces tokens of the same name in caps:
against, and, assert, attr, break, case, class, constant, define, ensure, 
else, elsif, end, except, from, if, is, loop, not, or, protect, raise, 
readonly, require, shared, then, typecase, undefine, until, when, while.

Produces the single character symbols:
'+'  '-'  '*'  '/'  '='  '<'  '>'  '('  ')'  '['  ']'  ','  
';'  ':' '$' '.', '{', '}', '#' others are errors.

Produces the two-character tokens:
NE /=, LE <=, GE >=, ASSIGN :=, CREF ::

Recognizes constants: CHAR_CONST for characters, STR_CONST for strings, 
INT_CONST for integers, REAL_CONST for floats with value equal to the 
defining string.

Other alphanumeric with underscore is an IDENTIFIER with value equal
to a lowercase string.  */

#include <ctype.h>

/* Stored in Sather class */
extern int globals_curr_lineno;		/* Line number of current file */
extern ptr globals_curr_filename;	/* Input file name */
extern ptr globals_str_table;           /* Global string table implemented
					   in Sather */
#define str_ptr_(s) (int)((s)+8)

int lower[256];			/* to convert to lower case, filled in init*/
FILE *fin;			/* input file pointer */
int c;				/* global containing current char */
ptr buf;			/* the buffer for strings as they come in */
int omerrs=0;
char prev_token;
int stored_token=0;             /* flag that a previous token is found in 
				   previous scan */

static char prt_err_str[128];
/* Since "sprintf" fails for fields with > 128 characters, we simply use
   same limit. */

extern int yychar;			/* Current token number */

/* Function for printing current token when error is found. */
int print_err_tok()
{
  switch (yychar) {
  case 0: fprintf(stderr, "EOF"); break;
  case (ABSTRACT): fprintf(stderr, "ABSTRACT"); break;
  case (AGAINST): fprintf(stderr, "AGAINST"); break;
  case (ASSERT): fprintf(stderr, "ASSERT"); break;
  case (ASSIGN): fprintf(stderr, "ASSIGN"); break;
  case (ATTR): fprintf(stderr, "ATTR"); break;
  case (BREAK): fprintf(stderr, "BREAK!"); break;
  case (CASE): fprintf(stderr, "CASE"); break;
  case (CLASS): fprintf(stderr, "CLASS"); break;
  case (CONSTANT): fprintf(stderr, "CONST"); break;
  case (CREF): fprintf(stderr, "::"); break;
  case (DEFINE): fprintf(stderr, "DEFINE"); break;
  case (ELSE): fprintf(stderr, "ELSE"); break;
  case (ELSIF): fprintf(stderr, "ELSIF"); break;
  case (END): fprintf(stderr, "END"); break;
  case (ENSURE): fprintf(stderr, "POST"); break;
  case (IF): fprintf(stderr, "IF"); break;
  case (INCLUDE): fprintf(stderr, "INCLUDE"); break;
  case (INVARIANT): fprintf(stderr, "INVARIANT"); break;
  case (IS): fprintf(stderr, "IS"); break;
  case (LOOP): fprintf(stderr, "LOOP"); break;
  case (PRIVATE): fprintf(stderr, "PRIVATE"); break;
  case (PROTECT): fprintf(stderr, "PROTECT"); break;
  case (RAISE): fprintf(stderr, "RAISE"); break;
  case (READONLY): fprintf(stderr, "READONLY"); break;
  case (REQUIRE): fprintf(stderr, "PRE"); break;
  case (RETURN): fprintf(stderr, "RETURN"); break;
  case (SHARED): fprintf(stderr, "SHARED"); break;
  case (THEN): fprintf(stderr, "THEN"); break;
  case (TYPECASE): fprintf(stderr, "TYPECASE"); break;
  case (UNDEFINE): fprintf(stderr, "UNDEFINE"); break;
  case (UNTIL): fprintf(stderr, "UNTIL"); break;
  case (WHEN): fprintf(stderr, "WHEN"); break;
  case (WHILE): fprintf(stderr, "WHILE"); break;
  case (CHAR_CONST): 
    fprintf(stderr, "CHARACTER = \"%s\"", str_ptr_(yylval.val)); 
    break;
  case (INT_CONST): 
    fprintf(stderr, "INTEGER = %s", str_ptr_(yylval.val));
    break;
  case (REAL_CONST): 
    fprintf(stderr, "REAL = %s", str_ptr_(yylval.val));
    break;
  case (STR_CONST): 
    fprintf(stderr, "STRING = \"%s\"",
	    str_ptr_(str_table_at_index(globals_str_table, yylval.ind))); 
    break;
  case (BOOL_CONST): 
    fprintf(stderr, "BOOLEAN = %s", ((yylval.ind==0)?"false":"true")); 
    break;
  case (IDENTIFIER): 
    fprintf(stderr, "IDENTIFIER = %s", 
	    str_ptr_(str_table_at_index(globals_str_table, yylval.ind)));
    break;
  case (TYPID): 
    fprintf(stderr, "TYPE IDENTIFIER = %s", 
	    str_ptr_(str_table_at_index(globals_str_table, yylval.ind)));
    break;
  case (OR): fprintf(stderr, "OR"); break;
  case (AND): fprintf(stderr, "AND"); break;
  case (NE): fprintf(stderr, "/="); break;
  case (LE): fprintf(stderr, "<="); break;
  case (GE): fprintf(stderr, ">="); break;
  case (NOT): fprintf(stderr, "NOT"); break;
  default: fprintf(stderr, "token = %c", yychar); break;
  }
}

yyerror(s)
char *s;
{
  int ch;

  fprintf(stderr, "\"%s\", line %d: syntax error at or near character %c / ",
	  str_ptr_(globals_curr_filename), globals_curr_lineno,c);
  print_err_tok();
  fprintf(stderr, "\n");
#ifdef ERR_PAUSE__
  fprintf(stderr, "Continue ? (y/n) ");
  ch = getc(stdin);
  if ((ch == 'n') || (ch == 'N')) {
    exit(1);
  }
  ch = getc(stdin);		/* Get rid of '\n' */
#endif
  omerrs++;
  if(omerrs>50) {fprintf(stderr, "More than 50 errors\n"); exit(1);};
  
}

error_msg(str)
char *str;
{
  int ch;

  fprintf(stderr, str);
#ifdef ERR_PAUSE__
  fprintf(stderr, "Continue ? (y/n) ");
  ch = getc(stdin);
  if ((ch == 'n') || (ch == 'N')) {
    exit(1);
  }
  ch = getc(stdin);		/* Get rid of '\n' */
#endif
  omerrs++;
  if(omerrs>50) {fprintf(stderr, "More than 50 errors\n"); exit(1);}
}

/* Function for easier debugging of scanner */
int print_tok(tok)
int tok;
{
  switch (tok) {
  case 0: fprintf(stdout, "EOF\n"); break;
  case (ABSTRACT): fprintf(stdout, "ABSTRACT\n"); break;
  case (AGAINST): fprintf(stdout, "AGAINST\n"); break;
  case (ASSERT): fprintf(stdout, "ASSERT\n"); break;
  case (ASSIGN): fprintf(stdout, "ASSIGN\n"); break;
  case (ATTR): fprintf(stdout, "ATTR\n"); break;
  case (BREAK): fprintf(stdout, "BREAK\n"); break;
  case (CASE): fprintf(stdout, "CASE\n"); break;
  case (CLASS): fprintf(stdout, "CLASS\n"); break;
  case (CONSTANT): fprintf(stdout, "CONSTANT\n"); break;
  case (CREF): fprintf(stdout, "CREF\n"); break;
  case (DEFINE): fprintf(stdout, "DEFINE\n"); break;
  case (ELSE): fprintf(stdout, "ELSE\n"); break;
  case (ELSIF): fprintf(stdout, "ELSIF\n"); break;
  case (END): fprintf(stdout, "END\n"); break;
  case (ENSURE): fprintf(stdout, "POST\n"); break;
  case (IF): fprintf(stdout, "IF\n"); break;
  case (INCLUDE): fprintf(stdout, "INCLUDE\n"); break;
  case (INVARIANT): fprintf(stdout, "INVARIANT\n"); break;
  case (IS): fprintf(stdout, "IS\n"); break;
  case (LOOP): fprintf(stdout, "LOOP\n"); break;
  case (PRIVATE): fprintf(stdout, "PRIVATE\n"); break;
  case (PROTECT): fprintf(stdout, "PROTECT\n"); break;
  case (RAISE): fprintf(stdout, "RAISE\n"); break;
  case (READONLY): fprintf(stdout, "READONLY\n"); break;
  case (REQUIRE): fprintf(stdout, "PRE\n"); break;
  case (RETURN): fprintf(stdout, "RETURN\n"); break;
  case (SHARED): fprintf(stdout, "SHARED\n"); break;
  case (THEN): fprintf(stdout, "THEN\n"); break;
  case (UNDEFINE): fprintf(stdout, "UNDEFINE\n"); break;
  case (UNTIL): fprintf(stdout, "UNTIL\n"); break;
  case (WHEN): fprintf(stdout, "WHEN\n"); break;
  case (WHILE): fprintf(stdout, "WHILE\n"); break;
  case (CHAR_CONST): 
    fprintf(stdout, "CHAR_CONST = %s\n", str_ptr_(yylval.val)); 
    break;
  case (INT_CONST): 
    fprintf(stdout, "INT_CONST = %s\n", str_ptr_(yylval.val));
    break;
  case (REAL_CONST): 
    fprintf(stdout, "REAL_CONST = %s\n", str_ptr_(yylval.val));
    break;
  case (STR_CONST): 
    fprintf(stdout, "STR_CONST = \"%s\"\n", 
	    str_ptr_(str_table_at_index(globals_str_table, yylval.ind))); 
    break;
  case (BOOL_CONST): 
    fprintf(stdout, "BOOL_CONST = %d\n", yylval.ind); 
    break;
  case (IDENTIFIER): 
    fprintf(stdout, "IDENTIFIER = %s\n", 
	    str_ptr_(str_table_at_index(globals_str_table, yylval.ind)));
    break;
  case (TYPID): 
    fprintf(stdout, "TYPE IDENTIFIER = %s\n", 
	    str_ptr_(str_table_at_index(globals_str_table, yylval.ind)));
    break;
  case (OR): fprintf(stdout, "OR\n"); break;
  case (AND): fprintf(stdout, "AND\n"); break;
  case (NE): fprintf(stdout, "NE\n"); break;
  case (LE): fprintf(stdout, "LE\n"); break;
  case (GE): fprintf(stdout, "GE\n"); break;
  case (NOT): fprintf(stdout, "NOT\n"); break;
  default: fprintf(stdout, "TOK = %c\n", tok); break;
  }
  return tok;  
}

/* Do it as one big switch statement for optimal efficiency */
char is_typeid = 0;

int yylex()
{
  int c1;

  if (stored_token)
    {
      stored_token = 0; return prev_token;
    }
  str_buffer_init(buf);
  c = getc(fin);
  switch(c)
    {
    case EOF: return 0;		/* done */
      
    case '\n': 
      globals_curr_lineno++;	
      /* falls through to other whitespace */
    case ' ': case '\t': case '\b': case '\f': case '\r': case '\v':
      while (1)			/* loop till something breaks out */
	{
	  switch(c = getc(fin))
	    {
	    case '\n': 
	      globals_curr_lineno++; /* falls through to other whitespace */
	    case ' ': case '\t': case '\b': case '\f': case '\r': case '\v':
	      continue;		/* the loop keeps going */
	    default: break;	/* leaves the loop */
	    }
	  /* only get here if it didn't continue */
	  ungetc(c,fin); return yylex(); /* return what comes after space */
	}
      break;

    case '0': case '1': case '2': case '3': case '4': case '5': case '6':
    case '7': case '8': case '9': /* a number */
      /* Check for hexadecimal number. */
      if (c=='0') {
	c1=c; c=getc(fin);
	if ((c=='x')||(c=='X')) {
	  get_hex(); ungetc(c,fin);
	  buf=str_buffer_terminate(buf);
	  copy_to_yylval();
	  return INT_CONST;
	}
	else {
	  ungetc(c,fin); c=c1;
	}
      }
      get_digits();
      if (c=='.')
	/* we have to look ahead to distinguish between "3.op(1)" and "3.e2" or
	   "3.2" etc */
	{
	  c=getc(fin); 
	  buf=str_buffer_push(buf,'.');
	  if (isdigit(c))
	    {
	      get_digits();		/* optional mantissa */
	      get_opt_exponent();	/* optional exponent */
	      ungetc(c,fin);
	      buf=str_buffer_terminate(buf); 
	      copy_to_yylval();
	      return REAL_CONST;
	    }
	  if (isalpha(c))
	    {
	      ungetc(c,fin); c1=str_buffer_pop(buf);
	      buf=str_buffer_terminate(buf);
	      copy_to_yylval();
	      prev_token = '.'; stored_token = 1;
	      return INT_CONST;
	    }
	  get_opt_exponent();
	  ungetc(c,fin); buf=str_buffer_terminate(buf);
	  copy_to_yylval(); return REAL_CONST;
	}
      else
	{
	  if (get_opt_exponent())
	    {ungetc(c,fin); buf=str_buffer_terminate(buf); 
	     copy_to_yylval(); return REAL_CONST;}
	  else
	    {ungetc(c,fin); buf=str_buffer_terminate(buf); 
	     copy_to_yylval(); return INT_CONST;}
	}
      break;
      
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
    case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
    case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
    case 'v': case 'w': case 'x': case 'y': case 'z': 
      is_typeid = (char)0;
      return(get_identifier());
      break;
    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
    case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':
    case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
    case 'V': case 'W': case 'X': case 'Y': case 'Z': /* an alphabetic */
      is_typeid = (char)1;
      return(get_identifier());
      break;
    case '\'':			/* string constant */
      return get_char_const(); break;
    case '"':			/* char constant */
      return get_string_const(); break;
    case '.': c = getc(fin);
      if (isdigit(c))		/* float */
	{
	  buf=str_buffer_push(buf,'.'); /* put in decimal point 
					 (can't overflow here since at front) */
	  get_digits();		/*  mantissa */
	  get_opt_exponent();	/* optional exponent */
	  ungetc(c,fin);
	  buf=str_buffer_terminate(buf); 
	  copy_to_yylval(); return REAL_CONST;
	}
      ungetc(c,fin); return '.'; break;	/* just a period */
    case '/': c = getc(fin);
      if (c == '=') return NE;
      ungetc(c,fin); return '/'; break;
    case '<': c = getc(fin);
      if (c == '=') return LE;
      ungetc(c,fin); return '<'; break;
    case '>': c = getc(fin);
      if (c == '=') return GE;
      ungetc(c,fin); return '>'; break;
    case ':': c = getc(fin);
      if (c == '=') return ASSIGN;
      if (c == ':') return CREF;
      ungetc(c,fin); return ':'; break;
    case '-': c = getc(fin);
      if (c == '-')		/* comment */
	{			
	  while((c=getc(fin)) != '\n' && c != EOF); /* go to end of line */
	  if (c == EOF) return 0;
	  globals_curr_lineno++; 
	  return yylex(); /* return what comes after comment */
	}
      ungetc(c,fin); return('-'); break;
    case '+': case '*': case '=': case '(': case ')': case '^':
    case '[': case ']': case ',': case ';': case '$': case '{': case '}':
    case '#': 
      return c; break;
    default: 
      error_msg(sprintf(prt_err_str, 
		       "(SCAN_ERROR) %s, line %d: Illegal character '%c'\n",
		       str_ptr_(globals_curr_filename),  globals_curr_lineno, c));
      exit(1);
    }
}

#define ishexdigit(c) (isdigit(c) || ((c>='a')&&(c<='f')) || ((c>='A')&&(c<='F')))

/* We have found a "0x" or "0X", so read in a hexadecimal number. */
int 
get_hex()
{
  buf=str_buffer_push(buf,'0');
  buf=str_buffer_push(buf,c);	/* "c" holds an 'x' or 'X'. */
  c=getc(fin);
  while (ishexdigit(c))
    {buf=str_buffer_push(buf,c); c=getc(fin);}
}

/* if c is a digit, use buf to append it to string, followed by all 
   contiguous digits, leave c on first non-digit. */
int 
get_digits()
{
  while(isdigit(c))
    {buf=str_buffer_push(buf,c); c=getc(fin);}
}

/* if c is pointing to exponent, add it to buf, otherwise leave alone.
   Returns 1 for exponent, 0 otherwise.*/
int 
get_opt_exponent()
{
  int c1;
  int sign;

  if (c=='e' || c=='E')
    {
      c1=c; 
      c=getc(fin);		/* check next character */
      if (c==EOF)
	{error_msg(sprintf(prt_err_str,
			   "(SCAN_ERROR) %s, line %d: Unexpected end of file\n",
			   str_ptr_(globals_curr_filename),  globals_curr_lineno));
	 buf=str_buffer_push(buf,'0'); return 1;}
      if (c=='+' || c=='-' || isdigit(c))
	{
	  if (c=='+' || c== '-') 
	    {
	      buf=str_buffer_push(str_buffer_push(buf,c1),c);
	      sign=c; c=getc(fin);
	      if (!isdigit(c)) 
		{
		  error_msg(sprintf(prt_err_str,
				    "(SCAN_ERROR) %s, line %d: At least 1 digit after %c\n",
				    str_ptr_(globals_curr_filename),
				    globals_curr_lineno, 
				    sign));
		  ungetc(c,fin); c='0';
		}
	    }
	  else {		/* must be a digit */
	    buf=str_buffer_push(buf,c1);
	  }
	  get_digits();
	  return 1;
	}
      /* Otherwise, put back current character */
      ungetc(c,fin);
      c=c1;
      return 0;
    }
  return 0;
}

/* c is first character of an identifer, put it and the rest in buf and 
   return the correct key.  */
int
  get_identifier()
{
  buf=str_buffer_push(buf,c);
  while(isalnum(c=getc(fin)) || c=='_' || c=='!' )
    {buf=str_buffer_push(buf,c);
     if (isalpha(c) && (! isupper(c))) { is_typeid = (char)0; };
     if (c=='!') {c=getc(fin); break;}; /* allow last character only */
   };
  ungetc(c,fin); buf=str_buffer_terminate(buf);
  return identifier_key();
}

/* Checks if buf contains a reserved word, and if it does, returns its
   key, otherwise returns the IDENTIFIER key.  */
int identifier_key()
{
  int ind;			/* index of string */

  ind = str_table_index_of_str(globals_str_table,buf);	
  /* install in table if not there */

  switch(ind)
    {
    case ABSTRACT_IND: return(ABSTRACT);
    case AGAINST_IND: return(AGAINST);
    case AND_IND: return(AND); 
    case ASSERT_IND: return(ASSERT);
    case ATTR_IND: return(ATTR);
    case BREAK_IND: return(BREAK);
    case CASE_IND: return(CASE);
    case CLASS_IND: return(CLASS);
    case CONSTANT_IND: return(CONSTANT);
    case DEFINE_IND: return(DEFINE);
    case ELSE_IND: return(ELSE);
    case ELSIF_IND: return(ELSIF);
    case END_IND: return(END);
    case ENSURE_IND: return(ENSURE);
    case FALSE_IND: yylval.ind=0; return(BOOL_CONST); 
    case IF_IND: return(IF);
    case INCLUDE_IND: return(INCLUDE);
    case INVARIANT_IND: return(INVARIANT);
    case IS_IND: return(IS);
    case LOOP_IND: return(LOOP);
    case NOT_IND: return(NOT);
    case OR_IND: return(OR);
    case PRIVATE_IND: return(PRIVATE);
    case PROTECT_IND: return(PROTECT);
    case RAISE_IND: return(RAISE);
    case READONLY_IND: return(READONLY);
    case REQUIRE_IND: return(REQUIRE);
    case RETURN_IND: return(RETURN); 
    case SHARED_IND: return(SHARED);
    case THEN_IND: return(THEN);
    case TRUE_IND: yylval.ind=1; return(BOOL_CONST);
    case UNDEFINE_IND: return(UNDEFINE);
    case UNTIL_IND: return(UNTIL);
    case WHEN_IND: return(WHEN);
    case WHILE_IND: return(WHILE);
    default: yylval.ind=ind; 
      if (is_typeid==(char)1) { return(TYPID);
	} else { return(IDENTIFIER); };
    }
}

void string_key();

/* c is a double quote, put it and string into buf and return STR. */
int get_string_const()
{
  /* The double quote is not put into string because constant strings 
     in C are printed different from constant strings in Sather. */
  while(1)
    {
      c=getc(fin);
      if (c=='"')		/* done */
	{buf=str_buffer_terminate(buf); 
	 /* copy_to_yylval(); */
	 /* The old scanner returns string constants as array of characters. */
         string_key(); return STR_CONST;}

      switch (c) {

      case '\\': 
	buf=str_buffer_push(buf,c); c=getc(fin); /* eat escaped chars */
	if (c=='\n') globals_curr_lineno++; 
	break;
	
      case '\n': 
	error_msg(sprintf(prt_err_str,
			  "(SCAN_ERROR) %s, line %d: Unescaped RET in a string constant\n",
			  str_ptr_(globals_curr_filename), globals_curr_lineno));
	globals_curr_lineno++; /* C compiler may give up on generated code */
	
      case EOF: 
	error_msg(sprintf(prt_err_str,
			  "(SCAN_ERROR) %s, line %d: EOF in a string constant.\n",
			  str_ptr_(globals_curr_filename), globals_curr_lineno));
	c=EOF; buf=str_buffer_terminate(buf);
	/* copy_to_yylval(); */
	/* The old scanner returns string constants as array of characters. */
	string_key(); return STR_CONST;

      };

      buf=str_buffer_push(buf,c);
    }
}

/* c is a single quote, put it and char const into buf and return CHAR. */
int get_char_const()
{
  int v;			/* stores integer following \ */
  char num[5];                  
  /* contains octal string representation of integer */
  int i;

  buf=str_buffer_push(buf,c);
  /* Unlike constant strings, character constants have the same 
     representation in C and Sather, hence we need to put in the quote. */

  c=getc(fin);
  if (c=='\'')		/* done */
    {error_msg(sprintf(prt_err_str,
		       "(SCAN_ERROR) %s, line %d: No character found\n",
		       str_ptr_(globals_curr_filename), globals_curr_lineno));
     buf=str_buffer_push(buf,'0');
     buf=str_buffer_push(buf,c); buf=str_buffer_terminate(buf); 
     copy_to_yylval(); return CHAR_CONST;}
  if (c=='\\')		
    {
      buf=str_buffer_push(buf,c); c=getc(fin);
      if (isdigit(c))
	{
	  ungetc(c,fin);
	  fscanf(fin,"%o",&v);	/* Octal encoding of character */
	  if ((v > 0xFF) || (v < 0)) 
	    {v=0;}
	  sprintf(num,"%o\0",v);
	  for (i=0; num[i] != '\0'; i++) /* store integer */
	    {buf=str_buffer_push(buf,num[i]);}
	}
      else 
	{
	  if (c==EOF) 
	    {error_msg(sprintf(prt_err_str,
			       "(SCAN_ERROR) %s, line %d: Unexpected EOF\n",
			       str_ptr_(globals_curr_filename), 
			      globals_curr_lineno));
	     buf=str_buffer_push(str_buffer_push(buf,'0'),'\'');}
	  else
	    {buf=str_buffer_push(buf,c); c=getc(fin);
	     if (c != '\'')
	       {error_msg(sprintf(prt_err_str,
				  "(SCAN_ERROR) %s, line %d: Missing \'\n",
				  str_ptr_(globals_curr_filename), 
				  globals_curr_lineno));
		ungetc(c,fin);}
	     buf=str_buffer_push(buf,'\''); copy_to_yylval(); return CHAR_CONST;
	   }
	}
    }
  if (c==EOF) 
    {error_msg(sprintf(prt_err_str,
		       "(SCAN_ERROR) %s, line %d: EOF in a character constant\n", 
		       str_ptr_(globals_curr_filename), 
		       globals_curr_lineno));
     buf=str_buffer_push(buf,'0'); /* random character */
     buf=str_buffer_push(buf,'\''); buf=str_buffer_terminate(buf);
     copy_to_yylval(); return CHAR_CONST;}

  buf=str_buffer_push(buf,c);
  c=getc(fin);
  if (c != '\'')
    {error_msg(sprintf(prt_err_str, 
		       "(SCAN_ERROR) %s, line %d: Missing \' at end of character\n",
		       str_ptr_(globals_curr_filename), 
		       globals_curr_lineno));
     ungetc(c,fin);}
  buf=str_buffer_push(buf,'\''); copy_to_yylval(); return CHAR_CONST;
}

#ifdef YYDEBUG
extern int yydebug;
#endif

/* To be called before anything else. Fills in the lower array, 
initializes the string buffer. */
lex_init()
{
  int i;

#ifdef YYDEBUG 
#ifdef PARSER_DEBUG
  yydebug = 1;
#endif
#endif
  for(i=0; i<256; i++)		/* put in lowercase conversions */
    {
      if (isupper(i)) lower[i]=tolower(i);
      else lower[i]=i;
    }
  buf=str_buffer_create(0, 50);
}

/* Copies the current contents of buf to yylval.val. */
copy_to_yylval()
{
  char *s;

  yylval.val=str_buffer_strval(buf);
}

/* Install string constants. */
void string_key()
{
  yylval.ind=str_table_index_of_str(globals_str_table,buf);
}
