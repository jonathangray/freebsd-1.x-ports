%{
#include <stdio.h>
#include <ctype.h>
#include "SeParse.h"

void (*callbackProc)();
%}

%token <sval> WORD

%union {
  char *sval;
}

%start Script

%%

Script		: Script FunCall
		    | Empty
;
FunCall		: FunName '(' ArgList ')' ';'
			{ (*callbackProc)(3, NULL); }
;
FunName		: WORD { (*callbackProc)(1, $1); }
;
ArgList		: Args
  		    | Empty
;
Args	    : Args ',' Arg
		    | Arg
;
Arg		    : WORD { (*callbackProc)(2, $1); }
;
Empty		:
;
%%

void
ParseThis(line, callback)
	 char     *line;
	 void     (*callback)();
{
  callbackProc = callback;
  scSetInputBuffer(line);
  yyparse();
}

void yyerror(char *msg)
{
  (*callbackProc)(4, msg);
}

#ifdef TEST
void SignalBeginFunction(char *name)
{
  printf("** Function call: %s(", name);
}

void SignalArg(char *arg)
{
  char *p = arg;
  printf("\n++Arg: (");
  while (*p) {
    if (isprint(*p))
      putchar(*p);
    else 
      printf("\\0%o", (int)*p);
    p++;
  }
  putchar(')');
}

void SignalEndFunction()
{
  printf("\n)\n");
}

void
main(int argc, char *argv[])
{
  char long_line[1000];

  char input_str[] = "This(is, a, real, funky); script();
            Scripts(); Can(be); Multi(Line, \"Can't they?\");
            Commas(are, no, longer, optional, inside, arglists);
	    Scripts(); Can(); contain(\"tabs \\t and backspaces \\b\");
	    As(\"Well\\ as Quoted Strings\", and, '\"Quoted Strings inside
	    quoted strings\"');
	esc(can, appear, outside, strings, ^z, \\012\\015\\n);
	    But(parenthesis, should, match);
  We(\"have a funny way of specifying \\012 chars and even)\"); 
	backslashes( \" \\\\ \");
  new(\"in this version are ^m and ^A ctr-escapes, as in ^S^Q\");
 The(next, line, will, give, a, syntax, error, because, it, has, two, adj, functions,
	without, a, separating, semicolon);
 End() script()";

  printf("------ String to parse: \n%s\n\n---- Parsing begins:\n", input_str);
  strcpy(long_line, input_str);
  ParseThis(long_line);
  strcpy(long_line, input_str);
  ParseThis(long_line);
}
#endif TEST
