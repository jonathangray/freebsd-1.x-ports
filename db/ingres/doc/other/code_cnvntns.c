# include	"/usr/sys/param.h"
# include	<sccs.h>

/*
**  CODE_CNVNTNS.C -- A Program to Display the INGRES Coding Style
**
**	This hunk of code does virtually nothing of use.  Its main
**	purpose is to demonstrate the "official" ingres coding style.
**
**	This demonstrates comments.  There should be a block comment
**	at the beginning of every file and/or procedure to explain
**	the function of the code.  Important information to put here
**	includes the parameters of the routines, any options that the
**	user may specify, etc.
**
**	The first line of the comment should be a one-line description
**	of what's going on.  The remainder should be more detailed.
**	Blank lines should separate major points in the comments.  In
**	general, ask yourself the question, "If I didn't know what this
**	code was, what it was for, how it fit in, etc., and if I didn't
**	even have the documentation for it, would these comments be
**	enough for me?"
**
**	Some general guidelines for the code:
**
**	*****  GENERAL SYNTAX  *****
**
**	- Commas and semicolons should always be followed by a space.
**		Binary operators should be surrounded on both sides by
**		spaces.  Unary operators should be in direct contact
**		with the object that they act on, except for "sizeof",
**		which should be separated by one space.
**
**	- Two statements should never go on the same line.  This includes
**		such things as an if and the associated conditionally
**		executed statement.
**		In cases such as this, the second half of the if
**		should be indented one tab stop more than the if.  For
**		example, use:
**			if (cond)
**				statement;
**		never:
**			if (cond) statement;
**		or:
**			if (cond)
**			statement;
**
**	- Braces ({}) should (almost) always be on a line by them-
**		selves.  Exceptions are closing a do, and terminating
**		a struct definition or variable initialization.  Braces
**		should start at the same indent as the statement with
**		which they bind, and code inside the braces should be
**		indented one stop further.  For example, use:
**			while (cond)
**			{
**				code
**			}
**		and never:
**			while (cond)
**				{
**				code
**				}
**		or:
**			while (cond) {
**				code
**			}
**		or:
**			while (cond)
**			{
**			code
**			}
**		or anything else in that line.  Braces which match
**		should always be at the same tab stop.
**
**	- Do statements must always be of the form:
**			do
**			{
**				code;
**			} while (cond);
**		even if "code" is only one line.  This is done so that
**		it is clear that the "while" is with a do, rather than
**		a standalone "while" which is used for the side effects of
**		evaluation of the condition.
**
**	- There should always be a space following a keyword (i.e.,
**		for, if, do, while, switch, and return), but never 
**		between a function and the paren preceeding its
**		arguments.  For example, use:
**			if (i == 0)
**				exit();
**		never:
**			if(i == 0)
**				exit ();
**
**	- Every case in a switch statement (including default) should
**		be preceeded by a blank line.  The actual word "case" or
**		"default" should have the indent of the switch statement plus
**		two spaces.  It should be followed by a space (not a
**		tab) and the case constant.  Multiple case labels on
**		a single block of code should be on separate lines, but
**		they should not be separated by blank lines.  The
**		switch statement should in general be used in place of
**		such constructs as:
**			if (i == 1)
**				code1;
**			else if (i == 34)
**				code2;
**			else if (i == -1643)
**				code3;
**		which can be more succinctly stated as:
**			switch (i)
**			{
**			  case 1:
**				code1;
**				break;
**
**			  case 34:
**				code2;
**				break;
**
**			  case -1643:
**				code3;
**				break;
**			}
**		In point of fact the equivalent switch will compile
**		extremely efficiently.  (Note that if you had some
**		instance where you could not use a case, e.g., checking
**		for i < 5, else check for j > 3, else whatever, then
**		the above ("if") code is in the correct style.  However,
**			if (i < 5)
**				code1;
**			else
**				if (j > 3)
**					code2;
**				else
**					code3;
**		is acceptable.
**
**	- A blank line should separate the declarations and the code
**		in a procedure.  Blank lines should also be used freely
**		between major subsections of your code.  The major
**		subsections should also have a block comment giving
**		some idea of what is about to occur.
**
**	*****  PREPROCESSOR USAGE  *****
**
**	- Fields of #defines and #includes should line up.  Use:
**			# define	ARPA		25
**			# define	MAXFIELDS	18
**		and not:
**			#define ARPA 25
**			#define MAXFIELDS 18
**		Conditional compilation (#ifdef/#endif) should be used
**		around all trace information, timing code, and code
**		which may vary from version to version of UNIX.  See
**		the code below for an example of conditional compila-
**		tion use.
**
**	*****  VARIABLES AND DECLARATIONS  *****
**
**	- Defined constants (defined with the # define feature) must
**		be entirely upper case.  The exceptions to this are
**		compilation flags, which begin with a lower case "x",
**		and some sub-types for parser symbols.  In any case,
**		the majority of the symbol is upper case.
**
**	- Global variables should begin with an upper case letter and
**		be otherwise all lower case.  Local symbols should be
**		entirely lower case.  Procedure names are all lower
**		case.  The only exception to this is the trace routine
**		"tTf".  You should avoid user non-local symbols (globals
**		or # define'd symbols) which are one character only;
**		it is impossible to distinguish them.  Capitalization
**		may be used freely inside global names so long as they
**		are primarily lower case; for example, "ProcName" is
**		an acceptable name (and preferred over either Proc_name
**		or Procname).
**
**	- Use descriptive variable names, particularly for global var-
**		iables.  "IEH3462" tells me nothing; nor does "R".  On
**		the other hand, "Resultid" tells me quite a lot,
**		including what it might be, where I might go to see
**		how it is initialized, etc.  Try not to use variables
**		for multiple purposes.  Variable names like "i" are
**		acceptable for loop indices & temporary storage
**		provided that the value does not have long-term
**		semantic value.
**
**	- When the storage structure or type of a variable is
**		important, always state it explicitly.  In particular,
**		include "auto" if you are going to take the address
**		of something using the ampersand operator (so that
**		some wayward programmer doesn't change it to register),
**		and declare int parameters as int instead of letting
**		them default.
**
**	*****  GENERAL COMMENTS  *****
**
**	- It is quite possible to name a file "printr.c" and then
**		put the code for "destroydb" in it.  Try to arrange
**		the names of your files so that given the name of a
**		routine, it is fairly easy to figure out which file
**		it is in.
**
**	- Sometimes it is really pretty much impossible to avoid doing
**		something tricky.  In these cases, put in a comment
**		telling what you are doing and why you are doing it.
**
**	- Try to write things that are clear and safe, rather than
**		things which you think are easier to compile.  For
**		example, always declare temporary buffers as local,
**		rather than as global.  This way you can another
**		routine accidently when it still had useful info
**		in it.
**
**	*****  COMMENTS  *****
**
**	- The importance of comments cannot be overemphasised.
**		INGRES is primarily a research vehicle rather than
**		a program product.  This means that there will be
**		many people pouring over your code, trying to
**		understand what you have done & modify it to do
**		other things.  Try to make life easy for them &
**		maybe they will be nice to you someday.
**	
**	- Try to keep an idea of the flow of your program.  Put
**		block comments at the beginning of major segments,
**		and use one-line comments at less major junctures.
**		A person viewing your code at ten paces should be
**		able to tell where the major segments lay.
**
**	- The preferred format for block comments is to begin with
**		a line containing slash-star alone, followed by a
**		number of lines all beginning star-star containing
**		the comment, and terminating with a line containing
**		star-slash alone.  Comments without the double-star
**		at the beginning of each line should be avoided,
**		since it makes the comments seemingly disappear into
**		the body of the code.
**
**	- The beginning of each routine should have a comment block
**		in parametric form as demonstrated below.  The fields
**		"Parameters", "Returns", and "Side Effects" should
**		be considered mandatory.  Mark parameters as being
**		(IN), (IN/OUT), or (OUT) parameters, depending on
**		whether the parameter is used only to transmit infor-
**		mation into the routine, in and out of the routine,
**		or only to return information; the default is (IN).
**
**	Remember, it is easy to write totally incomprehensible code in
**	C, but we don't go in for that sort of thing.  It isn't too
**	much harder to write brilliantly clear code, and the code is
**	worth a lot more later.
**
**	For efficiency reasons, you should always use register variables
**	when possible.  A simple and extremely effective tip is to define
**	a register variable, and assign an oft-used parameter to it,
**	since it is particularly inefficient to reference a parameter.
**	Another particularly inefficient operation is referencing arrays
**	of structures.  When possible, define a register pointer to the
**	structure, and then say:
**		struct xyz structure[MAX];
**		register struct xyz *p;
**		...
**		for (i = 0; i < MAX; i++)
**		{
**			p = &structure[i];
**			p->x = p->y + p->z;
**			(diddle with p->???)
**		}
**	and not:
**		struct xyz structure[MAX];
**		...
**		for (i = 0; i < MAX; i++)
**		{
**			Structure[i].x = Structure[i].y + Structure[i].z;
**			(diddle with Structure[i].???)
**		}
**	Remember, the nice things about register variables is that they
**	make your code smaller and they run faster.  It is hard to
**	lose with registers.  There are three restrictions which you
**	should be aware of on register variables, however.  First,
**	The only types which may be registers are int's, char's,
**	and pointers.  Second, there may only be three register
**	variables per subroutine.  Third, you may not take the address
**	of a register variable (i.e., you may not say "&i" if i is
**	typed as a register variable).
**
**	Usage:
**		example [flags] argument
**
**	Positional Parameters:
**		argument -- this gets echoed to the standard
**			output.
**
**	Flags:
**		-n -- don't put a newline at the end.
**		-x -- don't do anything.
**		-b -- echo it with a bell character.
**
**	Return Codes:
**		0 -- successful
**		else -- failure
**
**	Defined Constants:
**		XEQ1 -- maximum number of simultaneous equijoins.
**
**	Compilation Flags:
**		xTRACE -- enable trace information
**
**	Trace Flags:
**		5 -- general debug
**		6 -- reserved for future use
**
**	Compilation Instructions:
**		cc -n example.c
**		mv a.out example
**		chmod 755 example
**
**	Notes:
**		These comments don't apply to the code at all,
**			since this is just an example program.
**		Also, it is wise to avoid this many comments
**			except at the head of main programs and
**			at the head of major modules.  For example,
**			this sort of commenting is appropriate at
**			the top of ingres.c (system startup) and
**			view.c (virtual view subsystem), but not
**			in small utility routines, e.g., length.c.
**			This sort of routine should be limited to
**			"Parameters:", "Returns:", "Side Effects:",
**			and anything else that seems relevant in
**			that context.
**		A fairly large comment block should exist at the
**			top of modules [files] which contain many
**			procedures; this block should clarify why
**			the procedures are grouped together, that
**			is, their common purpose in life.  A small
**			block should occur at the top of each
**			procedure explaining details of that proce-
**			dure.
**		Procedures should be on separate pages (use the
**			form feed character, control-L).
**		A program will help you generate this comment block.
**			In ex, go to the line where you want to insert
**			a block and say "so /mnt/ingres/comment".  It
**			will ask you for a block type, e.g., "main"
**			for main program, "modfn" for a file which
**			contains only one function, "function" or
**			"procedure" for a procedure within a module,
**			"module" for a module header (e.g., as a
**			separate comment block for a major module
**			[check .../qrymod/view.c for an example] or
**			in header files.
**		SCCS should be considered an essential tool, if only
**			to maintain history fields.
**
**	Deficiencies:
**		It should handle pseudo tty's.
*/

/* the following macro is defined by <sccs.h> */
SCCSID(%W%);	/* %W% is replaced by a version number by SCCS */






# define	XEQ1		5

struct magic
{
	char	*name;		/* name of symbol */
	int	type;		/* type of symbol, defined in symbol.h */
	int	value;		/* optional value.  This is actually
				 * the value if it is type "integer",
				 * a pointer to the value if it is a
				 * string. */
};

struct magic	Stuff;

main(argc, argv)
	int argc;
	char *argv[];
{
	register struct magic *r;
	register int i;
	register int j;
	int timebuf[2];
	auto int status;

	/*
	** Note that in the declarations of argc and argv above, all
	** parameters to any function should be declared, even if they
	** are of type int (which is the default).
	*/

	r = &Stuff;
	/* initialize random # generator */
	time(timebuf);
	srand(timebuf[1]);

	/* scan Stuff structure */
	for (i = 0; i < XEQ1; i++)
	{
#		ifdef xTRACE
		if (tTf(5, 13))
			printf("switch on type %d\n", r->reltype);
#		endif
		switch (r->type)
		{

		  case 0:
		  case 1:
		  case 3:
			/* initialize */
			printf("hi\n");
			break;

		  case 2:
			/* end of query */
			printf("bye\n");
			break;

		  default:
			/*
			** be sure to print plenty of info on an error;
			** "syserr("bad reltype");" would not have been
			** sufficient.  However, don't make syserr's
			** overly verbose; they take much space in the
			** object module, and it will probably be
			** necessary to look at the code anyway.
			*/
			syserr("main: bad type %d", r->type);

		}
	}

	/* resist the temptation to say "} else {" */
	if (i == 5)
	{
		i++;
		j = 4;
	}
	else
		i--;

	/* plot the results */
	do
	{
		i = rand() & 017;
		plot(i);
	} while (j--);

	/* wait for child processes to complete */
	wait(&status);
	/* end of run, print termination message and exit */
	for (i = 0; i < 2; i++)
		printf("bye ");
	printf("\n");
}
/*
**  PLOT -- Plot a Bar-Graph
**
**	Does a simple plot on a terminal -- one line's worth.
**
**	Parameters:
**		n (IN) -- number of asterisks to plot
**
**	Returns:
**		none
**
**	Side Effects:
**		none
**
**	Deficiencies:
**		Should allow scaling.
*/

plot(n)
	int n;
{
	register int i;

	for (i = n; i-- > 0; )
	{
		printf("*");
	}
	printf("\n");
}
