/* prcc.c: This file contains the parser for the calls command. */

/* This program is a modification of Steve Kirkendall's ctags(1) which
   is distributed as part of his vi-clone elvis.  It is contributed
   to the public domain by Andrew Moore of Talke Studio. */

#include <stdio.h>
#include <ctype.h>

#ifdef __STDC__
# include <string.h>
# include <stdlib.h>
#endif

#ifndef FALSE
# define FALSE	0
# define TRUE	1
#endif

#ifndef PATH_MAX
# define PATH_MAX 1024
#endif

#ifndef __P
# ifndef __STDC__
#  define __P(proto) ()
# else
#  define __P(proto) proto
# endif
#endif

typedef struct ns		/* name structure */
{
	struct ns *next;
	char *name;
} name_t;

#define NAME_MAX 32		/* maximun identifier name length */
#define TAG_MAX (NAME_MAX * 2 + PATH_MAX + 20)	/* maximum tag length */
#define TAG_STACKSIZE 20	/* tag stack size */

extern void complain __P((int));
extern int cpp_getc __P((void));
extern int cpp_open __P((char *));
extern int cpp_ungetc __P((int));
extern void ctags __P((void));
extern int file_getc __P((void));
extern int file_open __P((char *));
extern int file_ungetc __P((int));
extern void free_list __P((name_t **));
extern int lex_gettoken __P((void));
extern void main __P((int, char **));
extern void maketag __P((int, int));
extern int name_in_list __P((name_t *));
extern int name_from_list __P((name_t **));
extern int name_redefined __P((int));
extern int name_to_list __P((name_t **));
extern void per_file_cleanup __P((void));
extern void per_file_init __P((void));
extern int tag_pop __P((char *));
extern int tag_push __P((char *));

/* -------------------------------------------------------------------------- */
/* Some global variables */

/* The following boolean variables are set according to command line flags */
int incl_vars;		/* -v include variables */
int gnu_keywords;	/* -g includes GNU keywords */

/* -------------------------------------------------------------------------- */
/* These are used for reading a source file.  It keeps track of line numbers */

char file_name[PATH_MAX];	/* name of the current file */
FILE *file_fp;		/* stream used for reading the file */
long file_lnum;		/* line number in the current file */
int file_afternl;	/* boolean: was previous character a newline? */
int file_prevch;	/* a single character that was ungotten */

/* -------------------------------------------------------------------------- */

char *usage = "usage: %s [-gv] file ...\n";
char *pgm;

void
main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	extern char *optarg;

	int c;

	pgm = argv[0];

	/* parse the option flags */
	while ((c = getopt(argc, argv, "gv")) != EOF)
		switch (c)
		{
		  case 'g':
			gnu_keywords = TRUE;
			break;
		  case 'v':
			incl_vars = TRUE;
			break;
		  default:
			fprintf(stderr, usage, pgm);
			exit(2);
		}
	argv += optind;
	do
	{
		/* can't open file */
		if (!cpp_open(*argv ? *argv++ : "-"))
		{
			break;		/* least the error goes unnoticed */
		}

		/* initialize name lists */
		per_file_init();

		/* process file */
		ctags();

		/* free name lists */
		per_file_cleanup();
	} while (*argv);

	/* flush tag stack */
	maketag(0, 0);
	exit(0);
	/*NOTREACHED*/
}


/* -------------------------------------------------------------------------- */
/* This is the parser.  It locates tag candidates, and then decides
   whether to generate a tag for them. */

/* These are C tokens after which a parenthesis is valid
   which would otherwise be tagged as function names. The
   reserved words which are not listed are break, continue,
   default and goto.  */
char *reserved[] =
{
	"EXTERN", "FORWARD", "PRIVATE",
	"auto", "case", "char", "const", "do", "double", "else",
	"entry", "enum", "extern", "float", "for", "fortran",
	"if", "int", "long", "private", "register", "return", "short",
	"signed", "sizeof", "static", "struct", "switch", "typedef",
	"union", "unsigned", "void", "volatile", "while",
	0
};

char *gnu_reserved[] =
{
	"asm", "__asm__",
	"__const__",
	"__extension__",
	"inline", "__inline__",
	"__signed__",
	"typeof", "__typeof__",
	"__volatile__",
};

/* list of reserved words */
name_t *keyword;

enum lex_tokens			/* token types */
{
	DELETED,
	BODY,
	BODYEND,
	LBRACKET,
	RBRACKET,
	ARGS,
	ARGSEND,
	COMMA,
	SEMICOLON,
	COLON,
	KSTATIC,
	KEXTERN,
	KSTRUCT,
	KTYPEDEF,
	TYPESPEC,
	NAME,
	STRUREF,
	ASSIGN,
	OPERATOR
};


/* Basic types for initializing type specifier list */
char *type[] =
{
	"char", "double", "enum", "float", "int", "long", "short",
	"signed", "struct", "union", "unsigned", "void",
	0
};

/* A type specifer list prevents declarations such as: int (*func())();
   from being parsed as calls */
name_t *type_specifier;

/* A functions-previously-defined list assures that functions are defined
   only once. */
name_t *func_defined;

/* A functions/variables-previously-called list assures that calls are
   printed only once per function definition. */
name_t *per_func_ref;

/* Three variables lists to track global/extern/local variable
   declarations. */
name_t *variable;
name_t *extern_var;
name_t *per_func_var;

int gotname;		/* boolean: does lex_name contain a tag candidate? */
int blockno;		/* marks the extent of a scope */
int listno;		/* marks the extent of a parameter list */
int func_seen;		/* boolean: true if function is redefined */

/* This function parses a source file and prints function calls. */
void
ctags()
{
	int initializer = FALSE;	/* initialization list */
	int prev;		/* the previous token from the source file */
	int token = SEMICOLON;	/* the current token from the source file */
	int scope = 0;		/* normally 0, but could be KTYPEDEF or KSTATIC */
	int scopeno = 0;	/* block number upon entering a scope */
	int structdeclr = FALSE;	/* struct/union/enum declaration */
	int structno = 0;	/* block number upon entering struct */
	int typedeclr = FALSE;	/* type declaration */

	/* reset */
	gotname = FALSE;

	/* parse until the end of the file */
	while (prev = token, (token = lex_gettoken()) != EOF)
	{
		/* scope keyword? */
		if (token == KTYPEDEF || token == KSTATIC || token == KEXTERN)
		{
			/* set scope */
			scope = token;
			scopeno = blockno;
			gotname = FALSE;
			continue;
		}

		/* type declaration */
		if (token == KSTRUCT || token == TYPESPEC)
		{
			typedeclr = TRUE;
			gotname = FALSE;
			continue;
		}

		/* (not STRUREF) NAME: NAME is tag? */
		if (token == NAME && prev != STRUREF && prev != KSTRUCT)
		{
			gotname = TRUE;
			continue;
		}

		/* ASSIGN BODY: initilizer */
		if (token == BODY && prev == ASSIGN)
		{
			gotname = FALSE;

			/* global */
			if (blockno == 1)
			{
				initializer = TRUE;
			}
			continue;
		}

		/* [NAME] BODY (no ARGS): struct declr [NAME is a struct tag] */
		if (token == BODY && prev != ARGS)
		{
			gotname = FALSE;

			/* not already in struct/union/enum */
			if (!structdeclr)
			{
				structno = blockno;
			}
			structdeclr = TRUE;
			continue;
		}

		/* NAME ARGS BODY: NAME is function */
		if (gotname && prev == ARGS && token == BODY)
		{
			gotname = FALSE;
			typedeclr = FALSE;
			structdeclr = FALSE;

			/* no name clash */
			if (!(func_seen = name_redefined(0)))
			{
				/* KSTATIC okay -- make a tag */
				maketag(scope, 0);
			}
			scope = 0;
			continue;
		}

		/* NAME ARGS in BODY (not global initializer):
		   NAME is function call? */
		if (gotname && token == ARGS && blockno && !initializer)
		{
			gotname = FALSE;

			/* not function declaration */
 			if (!typedeclr)
 			{
				/* make a tag */
 				maketag(scope, 1);
			}
			continue;
		}

		/* TYPE ARGSEND: type cast */
		if (typedeclr && !gotname && token == ARGSEND)
		{
			typedeclr = FALSE;
			continue;
		}


		/* KTYPEDEF NAME (not struct tag): new type specifier */
		if (gotname && scope == KTYPEDEF && !structdeclr)
		{
			gotname = FALSE;

			/* global */
			if (!blockno)
			{
				/* no name clash */
				if (!name_redefined(0))
				{
					/* add specifier to type list */
					name_to_list(&type_specifier);
				}
			}
		}


		/* TYPE NAME (no ARGS) [SEMICOLON,COMMA,ARGSEND,ASSIGN]:
		   NAME is type declr */
		if (typedeclr && gotname && prev != ARGS && (token == SEMICOLON
		 || token == COMMA || token == ARGSEND || token == ASSIGN
		 || token == LBRACKET || token == OPERATOR))
		{
			gotname = FALSE;

			/* global */
			if (!blockno)
			{
				/* -v flag and not extern and (no name clash ||
				   name only extern) [name on extern variable
				   list is removed] */
				if (incl_vars && scope != KEXTERN
				 && (!name_redefined(1) || name_in_list(extern_var)
				 && name_from_list(&extern_var)))
				{
					/* make a tag */
					maketag(ASSIGN, 0);
				}
				/* (not -v flag || extern) && no name clash */
				else if ((!incl_vars || scope == KEXTERN)
				      && !name_redefined(1))
				{
					/* add name to global variables list */
					name_to_list(&extern_var);
				}
			}
			/* not in struct/union/enum and no name clash */
			else if (!structdeclr && !name_in_list(per_func_var))
			{
				/* add name to per-function variables list */
				name_to_list(&per_func_var);
			}
		}

		/* (no TYPE) NAME in BODY (not global initializer):
		   var/label reference */
		if (!typedeclr && gotname  && blockno && !initializer)
		{
			gotname = FALSE;

			/* -v flag set and global variable reference */
			if (incl_vars && (name_in_list(variable) || name_in_list(extern_var)))
			{
				maketag(NAME, 0);
			}
		}

		/* delimiter */
		if (token == OPERATOR || token == ASSIGN || token == BODYEND
		 || token == ARGSEND || token == COLON || token == COMMA
		 || token == LBRACKET || token == RBRACKET)
		{
			gotname = FALSE;

			/* assignment */
			if (token == ASSIGN)
			{
				/* reset */
				typedeclr = FALSE;

			}

			/* body end and end of struct definition */
			if (token == BODYEND && structno == blockno + 1)
			{
				/* reset */
				structdeclr = FALSE;
			}

			/* end of global compound */
			if (token == BODYEND && !blockno)
			{
				/* reset */
				initializer = FALSE;
				func_seen = 0;
				free_list(&per_func_var);
				free_list(&per_func_ref);
			}
			continue;
		}

		/* semicolon */
		if (token == SEMICOLON)
		{
			/* reset */
			gotname = FALSE;
			typedeclr = FALSE;

			/* end of scope */
			if (scopeno == blockno)
			{
				scope = 0;
			}
		}

	/* The source file will be automatically closed */
	}
}

char lex_name[NAME_MAX];	/* the name of a "NAME" token */

/* This function generates a tag for the object in lex_name */
void
maketag(scope, iscall)
	int scope;	/* 0 if global, or KSTATIC if static */
	int iscall;	/* function call? */
{
	static char  caller_name[NAME_MAX];	/* name of caller function */
	char tag[TAG_MAX], buf[TAG_MAX];	/* tag buffers */

	/* external declaration or lex_name is a keyword */
	if (func_seen || scope == KEXTERN || name_in_list(per_func_ref))
	{
		/* whoa!  never output tag for redefined function or
		   an extern declr or  keyword, and only output a tag
		   for variable/call reference once per function */
		return;
	}

	/* global variable declaration */
	if (scope == ASSIGN)
	{
		/* add name to global variables list */
		name_to_list(&variable);
	}

	/* variable reference */
	else if (scope == NAME)
	{
		/* add lex_name to calls made in this function */
		name_to_list(&per_func_ref);
	}

	/* new function definition */
	else if (!iscall)
	{
		/* update caller function name */
		strcpy(caller_name, lex_name);

		/* add name to function defined list */
		name_to_list(&func_defined);
	}

	/* function call */
	else
	{
		/* add lex_name to references made in this function */
		name_to_list(&per_func_ref);
	}

	/* make tag */
	sprintf(tag, iscall ? "%s\t%s\t{}"
		: (scope == NAME) ? "%s\t%s\t{v}"
		: (scope == ASSIGN) ? "%s\t%s\t{v %s %ld}"
		: "%s\t%s\t{%s %ld}",
		(scope == ASSIGN) ? lex_name
		: caller_name, lex_name, file_name, file_lnum);

	/* nested call */
	if (listno)
	{
		/* push new tag onto tag stack */
		tag_push(tag);
	}

	/* function definition or first call in current expression */
	else
	{
		/* first call in expression */
		if (iscall)
		{
			listno = 1;
		}

		/* print tags from previous expression */
		while (tag_pop(buf))
		{
			printf("%s\n", buf);
		}

		/* push new tag onto tag stack */
		tag_push(tag);
	}
}

char *tag_stack[TAG_STACKSIZE];
int tag_sp = 0;

/* pushes buffer onto tag stack */
tag_push(b)
	char *b;
{
	if (tag_sp < TAG_STACKSIZE
	 && (tag_stack[tag_sp] = (char *) malloc(strlen(b) + 1)) != NULL)
	{
		strcpy(tag_stack[tag_sp++], b);
		return 1;
	}
	return 0;
}

/* pops value off tag stack to buffer */
tag_pop(b)
	char *b;
{
	if (tag_sp)
	{
		strcpy(b, tag_stack[--tag_sp]);
		free(tag_stack[tag_sp]);
		return 1;
	}
	return 0;
}



/* -------------------------------------------------------------------------- */
/* This is the lexical analyser.  It gets characters from the
   preprocessor, and gives tokens to the parser.  Some special codes
   are...

   (deleted)  / *...* / (comments)
   (deleted) //...\n	(comments)
   (deleted) [...]	(array subscript, when ... contains no ])
   BODY	{		('{' can occur anywhere)
   BODYEND }		(end of a body)
   LBRACKET [		()
   LBRACKET ]		()
   ARGS (		(in function block, args of a function call)
   ARGSEND )		(end of args)
   ARGS	(...{		(args of a function defintion --- ANSI or K&R)
   ARGS	(...)...;	(args of an extern/forward function declaration)
   ARGS	(...)...,	(args of an extern/forward function declaration)
   ARGS	(...]...,	(args of an extern/forward function declaration)
   COMMA ,		(separate declarations that have same scope)
   SEMICOLON ;		(separate declarations that have different scope)
   KSTRUCT struct	()
   KSTRUCT union	()
   KSTRUCT enum		()
   KTYPEDEF typedef	(the "typedef" keyword)
   KSTATIC static	(the "static" keyword)
   KSTATIC private	(the "static" keyword)
   KSTATIC PRIVATE	(the "static" keyword)
   KEXTERN extern	(the "extern" keyword)
   KEXTERN extern	(the "extern" keyword)
   STRUREF .		(direct structure reference operator)
   STRUREF ->		(indirect structure reference operator)
   ASSIGN =		(assignment operator, esp. initializer)
   OPERATOR 		(any constant or operator or part of an operator)
   TYPESPEC [a-z]+  	(type specifier, including typedefs)
   NAME	[a-z]+		(really any valid name that isn't reserved word) */

/* returns token of next item in input */
lex_gettoken()
{
	static int expanded = 0;	/* boolean: ARGSEND expanded? */

	int ch;			/* a character from the preprocessor */
	int oc;			/* previous character from the preprocessor */
	int token;		/* the token that we'll return */
	int lp = 0;		/* lex_name index */

	/* loop until we get a token that isn't "DELETED" */
	do
	{
		/* process the character */
		switch (ch = cpp_nonwhite())
		{
		  case ',':
			token = COMMA;
			break;

		  case ';':
			token = SEMICOLON;
			break;

		  case ':':
		  	token = COLON;
		  	break;

		  case '\'':
			/* skip to matching '\'' */
			while ((ch = cpp_getc()) != '\'' && ch != EOF)
			{
				if (ch == '\\') ch = cpp_getc();
			}
			token = DELETED;
			break;

		  case '\"':
			/* skip to matching '\"' */
			while ((ch = cpp_getc()) != '\"' && ch != EOF)
			{
				if (ch == '\\') ch = cpp_getc();
			}
			token = DELETED;
			break;

		  case '(':
			/* entering list */
			if (listno) listno++;

			/* in block -- function call? */
			if (blockno)
			{
				token = ARGS;
				break;
			}

			/* indirect declarator or parenthized expression */
			else if ((ch = cpp_nonwhite()) == '*' || !gotname)
			{
				cpp_ungetc(ch);
				token = DELETED;
				break;
			}

			/* function declarator with parameter list */
			else if (ch != ')')
			{

				/* read past parameter list */
				for (lp = 1; lp && (ch = cpp_nonwhite()) != EOF;)
				{
					ch == '(' && lp++ || ch == ')' && lp--;
				}
			}

			/* read to end of declarator, i.e., past )...[](... */
			while ((ch = cpp_nonwhite()) == ')' || ch == '[' || ch == '(')
				switch (ch)
				{
				case '(':
					for (lp = 1; lp && (ch = cpp_nonwhite()) != EOF;)
					{
						ch == '(' && lp++ || ch == ')' && lp--;
					}
					break;
				case '[':
					for (lp = 1; lp && (ch = cpp_nonwhite()) != EOF;)
					{
						ch == '[' && lp++ || ch == ']' && lp--;
					}
					break;
				}

			/* type declarations following declarator */
			if (ch != ',' && ch != ';' && ch != '{')
			{
				/* read until ... ;{ */			/*}}}*/
				for (oc = ch; ((ch = cpp_nonwhite()) != '{'
				  || oc != ';') && ch != EOF; oc = ch)
					;
			}
			cpp_ungetc(ch);
			token = ARGS;
			break;

		  case ')':
			/* expand ARGSEND to COMMA ARGSEND - allows
			   proper handling of variable references at end
			   of parameter lists */
			if (expanded)
			{
				/* leaving parameter list */
				expanded = 0;
				if (listno) listno--;
				token = ARGSEND;
			}
			else
			{
				expanded = 1;
				cpp_ungetc(ch);
				token = COMMA;
			}
			break;

		  case '[':
			token = LBRACKET;
			break;

		  case ']':
			token = RBRACKET;
			break;

		  case '{':
			/* entering block */
			blockno++;
			token = BODY;
			break;

		  case '}':
			/* leaving block */
			blockno--;
			token = BODYEND;
			break;

		  case EOF:
			func_seen = 0;
			lex_name[0] = '\0';
			token = EOF;
			break;

		  default:
			/* is this the start of a name/keyword? */
			if (isalpha(ch) || ch == '_')
			{
				/* collect the whole word */
				lex_name[0] = ch;
				for (lp = 1; (isalnum(ch = cpp_getc())
				  || ch == '_') && lp < NAME_MAX - 1;)
				{
					lex_name[lp++] = ch;
				}
				lex_name[lp] = '\0';

				/* junk remainder of word */
				while (isalnum(ch) || ch == '_')
				{
					ch = cpp_getc();
				}
				cpp_ungetc(ch);

				/* is it a reserved word? */
				if (!strcmp(lex_name, "typedef"))
				{
					token = KTYPEDEF;
				}
				else if (!strcmp(lex_name, "static")
				      || !strcmp(lex_name, "private")
				      || !strcmp(lex_name, "PRIVATE"))
				{
					token = KSTATIC;
				}
				else if (!strcmp(lex_name, "extern")
				      || !strcmp(lex_name, "EXTERN")
				      || !strcmp(lex_name, "FORWARD"))
				{
					token = KEXTERN;
				}
				else if (!strcmp(lex_name, "struct")
				      || !strcmp(lex_name, "union")
				      || !strcmp(lex_name, "enum"))
				{
					token = KSTRUCT;
				}
				else if (name_in_list(type_specifier))
				{
					token = TYPESPEC;
				}
				else if (!name_in_list(keyword))
				{
					token = NAME;
				}
				else
				{
					token = DELETED;
				}
			}

			/* structure reference operator */
			else if (ch == '.')
			{
				token = STRUREF;
			}

			/* indirect reference or assignment operator? */
			else
			{
				token = OPERATOR;
				lp = cpp_getc();

				/* assignment (esp., initializer) */
				if (ch == '=' && lp != '=')
				{
					token = ASSIGN;
					cpp_ungetc(lp);
				}

				/* indirect reference */
				else if (ch == '-' && lp == '>')
				{
					token = STRUREF;
				}

				/* any other other operator or constant */
				else
				{
					cpp_ungetc(lp);
				}
			}

		} /* end switch(ch) */

	} while (token == DELETED);

	return token;
}

/* -------------------------------------------------------------------------- */
/* This section handles preprocessor directives.  It strips out all of the
   directives, and may emit a tag for #define directives.  */

int cpp_afternl;	/* boolean: look for '#' character? */
int cpp_prevch;		/* an ungotten character, if any */
int cpp_refsok;		/* boolean: can we echo characters out to "refs"? */

/* This function opens the file & resets variables */
cpp_open(name)
	char *name;	/* name of source file to be opened */
{
	/* use the lower-level file_open function to open the file */
	if (file_open(name))
	{
		/* reset variables */
		cpp_afternl = TRUE;
		cpp_refsok = TRUE;
		return 1;
	}
	return 0;
}

/* returns next nonwhite-space character */
cpp_nonwhite()
{
	int ch;
	int next;

	while ((ch = cpp_getc()) != EOF && (isspace(ch) || ch == '/'))
		if (ch == '/')
			switch (ch = cpp_getc())
			{
			case '*':
				ch = cpp_getc();
				next = cpp_getc();
				while (next != EOF && (ch != '*' || next != '/'))
				{
					ch = next;
					next = cpp_getc();
				}
				break;
			case '/':
				while ((ch = cpp_getc()) != '\n' && ch != EOF)
					;
				break;
			default:
				cpp_ungetc(ch);
				return '/';
			}
	return ch;
}


/* This function returns the next character which isn't part of a directive */
cpp_getc()
{
	int ch;			/* the next input character */
	int i = 0;
	char buf[PATH_MAX];	/* path name of source file */
	char *scan;

	/* if we have an ungotten character, then return it */
	if (ch = cpp_prevch)
	{
		cpp_prevch = 0;
		return ch;
	}

	/* Get a character from the file.  Return it if not special '#' */
	if ((ch = file_getc()) == '\n')
	{
		cpp_afternl = TRUE;
		return ch;
	}
	else if (ch != '#' || !cpp_afternl)
	{
		/* normal character.  Any non-whitespace should turn off
		   afternl */
		if (ch != ' ' && ch != '\t')
		{
			cpp_afternl = FALSE;
		}
		return ch;
	}

	/* Yikes!  We found a directive */

	/* skip whitespace */
	while ((ch = file_getc()) == ' ' || ch == '\t')
	{
		;		/* do nothing */

	}

	/* # directive followed by a digit */
	if (isdigit(ch))
	{
		/* assert: directive of the form: # nn "filename" */

		/* update line number */
		file_lnum = ch - '0';
		while (isdigit(ch = file_getc()))
		{
			file_lnum = file_lnum * 10 + ch - '0';
		}

		/* adjust line number for newline of directive */
		file_lnum--;

		/* skip to path name */
		while ((ch = file_getc()) != '\"' && ch != EOF)
		{
			;		/* do nothing */
		}

		/* collect whole path */
		while ((ch = file_getc()) != '\"' && ch != EOF && i < PATH_MAX-1)
		{
			file_name[i++] = ch;
		}
		file_name[i] = '\0';
	}

	/* skip to the end of the directive -- a newline that isn't
	   preceded by a '\' character.  */
	while (ch != '\n' && ch != EOF)
	{
		if (ch == '\\')
		{
			ch = file_getc();
		}
		ch = file_getc();
	}

	/* return the newline that we found at the end of the directive */
	return ch;
}


/* This puts a character back into the input queue for the source file */
cpp_ungetc(ch)
	int ch;		/* a character to be ungotten */
{
	return cpp_prevch = ch;
}

/* -------------------------------------------------------------------------- */

/* This function opens a file, and resets the line counter.  If it fails, it
   it will display an error message and leave the file_fp set to NULL.  */
file_open(name)
	char *name;		/* name of file to be opened */
{
	/* if another file was already open, then close it */
	if (file_fp)
	{
		fclose(file_fp);
	}

	/* cannot open file */
	if (!(file_fp = !strcmp(name, "-") ? stdin : fopen(name, "r")))
	{
		perror(name);
		return 0;
	}

	/* reset the name & line number */
	strcpy(file_name, name);
	file_lnum = 0L;
	file_afternl = TRUE;
	return 1;
}

/* This function reads a single character from the stream.  If the
   *previous* character was a newline, then it also increments
   file_lnum and sets file_offset.  */
file_getc()
{
	int ch;

	/* if there is an ungotten character, then return it. */
	if (file_prevch)
	{
		ch = file_prevch;
		file_prevch = 0;
		return ch;
	}

	/* if previous character was a newline, then we're starting a line */
	if (file_afternl)
	{
		file_afternl = FALSE;
		file_lnum++;
	}

	/* Get a character.  If no file is open, then return EOF */
	ch = (file_fp ? getc(file_fp) : EOF);

	/* if it is a newline, then remember that fact */
	if (ch == '\n')
	{
		file_afternl = TRUE;
	}

	/* return the character */
	return ch;
}

/* This function ungets a character from the current source file */
file_ungetc(ch)
	int ch;		/* character to be ungotten */
{
	return file_prevch = ch;
}

/* -------------------------------------------------------------------------- */

/* initialize per-file name lists */
void
per_file_init()
{
	char **p;

	for (p = type; *p; p++)
	{
		/* add type to type specifier list */
		strcpy(lex_name, *p);
		name_to_list(&type_specifier);
	}

	for (p = reserved; *p; p++)
	{
		/* add reserved word to keyword list */
		strcpy(lex_name, *p);
		name_to_list(&keyword);
	}

	if (gnu_keywords)
	{
		for (p = gnu_reserved; *p; p++)
		{
			strcpy(lex_name, *p);
			name_to_list(&keyword);
		}
	}
}

/* free per-file name lists */
void
per_file_cleanup()
{
	free_list(&type_specifier);
	free_list(&keyword);
	free_list(&variable);
	free_list(&extern_var);
}

/* check lex_name for (potential) name space conflict */
name_redefined(isvar)
	int isvar;
{
	/* name redefined */
	if (name_in_list(func_defined) || name_in_list(type_specifier)
	 || name_in_list(extern_var) || name_in_list(variable))
	{
		complain(isvar);
		return 1;
	}
	return 0;
}


/* add lex_name to a list; return 0 on error */
name_to_list(lp)
	name_t **lp;		/* list pointer */
{
	name_t *p;

	/* name structure and name buffer alloc'd */
	if ((p = (name_t *) malloc(sizeof(name_t))) != NULL
	 && (p->name = (char *) malloc(strlen(lex_name) + 1)) != NULL)
	{
		/* initialize name structure */
		strcpy(p->name, lex_name);

		/* add structure to head of list */
		p->next = *lp;
		*lp = p;
		return 1;
	}
	return 0;
}

/* remove lex_name from a list; lex_name must be on list */
name_from_list(lp)
	name_t **lp;		/* list pointer */
{
	name_t *q, *p = *lp;

	if (strcmp((*lp)->name, lex_name) == 0)
	{
		*lp = (*lp)->next;
		free(p->name);
		free(p);
	}
	else
	{
		for (; strcmp(p->next->name, lex_name); p = p->next)
			;
		q = p->next;
		p->next = p->next->next;
		free(q->name);
		free(q);
	}
	return 1;
}


/* return 1 if lex_name in list rl, otherwise 0 */
name_in_list(l)
	name_t *l;
{
	for (; l; l = l->next)
	{
		if (!strcmp(lex_name, l->name))
			return 1;
	}
	return 0;
}


/* free a list's memory */
void
free_list(lp)
	name_t **lp;
{
	name_t *l = *lp;
	name_t *t;

	for (; l; l = t)
	{
		t = l->next;
		free(l->name);
		free(l);
	}
	*lp = NULL;
}

void
complain(isvar)
	int isvar;
{
	fprintf(stderr, isvar ? "%s: cannot redefine: %s\t{v %s %ld}\n"
		: "%s: cannot redefine: %s\t{%s %ld}\n", pgm, lex_name,
		file_name, file_lnum);
}
