/* 
 *
 *	fences.c
 *
 * Match up various fenceposts, like (), [], {}, */ /*, #if, #el, #en
 *
 * Most code probably by Dan Lawrence or Dave Conroy for MicroEMACS
 * Extensions for vile by Paul Fox
 *
 *	$Log: fences.c,v $
 *	Revision 1.1  1994/02/01 03:29:21  jkh
 *	Initial revision
 *
 * Revision 1.15  1993/09/16  11:06:43  pgf
 * make parentheses act like braces in c-mode -- for indentation purposes, in
 * languages like scheme (and lisp?)
 *
 * Revision 1.14  1993/06/28  14:27:25  pgf
 * new arg to catnap()
 *
 * Revision 1.13  1993/06/02  14:28:47  pgf
 * see tom's 3.48 CHANGES
 *
 * Revision 1.12  1993/05/24  15:21:37  pgf
 * tom's 3.47 changes, part a
 *
 * Revision 1.11  1993/04/01  13:06:31  pgf
 * turbo C support (mostly prototypes for static)
 *
 * Revision 1.10  1993/03/31  19:28:50  pgf
 * fixed bug introduced in 3.38 -- getfence is sometimes called internally
 * on empty lines, with ch preset, to find indents etc.
 *
 * Revision 1.9  1993/03/25  19:50:58  pgf
 * see 3.39 section of CHANGES
 *
 * Revision 1.8  1993/03/24  17:30:30  pgf
 * cleaned gcc warnings
 *
 * Revision 1.7  1993/03/18  17:42:20  pgf
 * see 3.38 section of CHANGES
 *
 * Revision 1.6  1993/01/23  14:26:54  foxharp
 * protect against trying to fence on empty lines
 *
 * Revision 1.5  1992/12/23  09:18:34  foxharp
 * allow match of fence that is first char. in buffer
 *
 * Revision 1.4  1992/08/20  23:40:48  foxharp
 * typo fixes -- thanks, eric
 *
 * Revision 1.3  1992/06/08  08:56:05  foxharp
 * fixed infinite loop if simple fence not found, and
 * suppressed beeping in input mode if fence not found
 * ,.
 * 
 *
 * Revision 1.2  1992/06/03  08:37:23  foxharp
 * removed nested comment
 *
 * Revision 1.1  1992/05/29  09:38:33  foxharp
 * Initial revision
 *
 *
 *
 */

#include	"estruct.h"
#include	"edef.h"

#if CFENCE

#define	CPP_UNKNOWN -1
#define	CPP_IF       0
#define	CPP_ELIF     1
#define	CPP_ELSE     2
#define	CPP_ENDIF    3

static	int	cpp_keyword P(( LINE *, int ));
static	int	cpp_fence P(( int, int ));

static int
cpp_keyword(lp,off)
LINE	*lp;
int	off;
{
	char	temp[NSTRING];
	register char *d = temp;
	register int  n;

	static	struct	{
		char	*name;
		int	code;
	} keyword_table[] = {
		{ "if",     CPP_IF },
		{ "ifdef",  CPP_IF },
		{ "ifndef", CPP_IF },
		{ "elif",   CPP_ELIF },
		{ "else",   CPP_ELSE },
		{ "endif",  CPP_ENDIF }
	};

	while (off < llength(lp)) {
		n = lgetc(lp,off++);
		if ((d - temp < sizeof(temp)-2) && isident(n))
			*d++ = n;
		else
			break;
	}
	*d = EOS;

	for (n = 0; n < SIZEOF(keyword_table); n++)
		if (!strcmp(temp, keyword_table[n].name))
			return keyword_table[n].code;
	return CPP_UNKNOWN;
}

static int
cpp_fence(sdir,key)
int sdir;
int key;
{
	int count = 1;
	int i, j, this;

	/* patch: this should come from arguments */
	if (key == CPP_ENDIF)
		sdir = REVERSE;
	else
		sdir = FORWARD;

	/* set up for scan */
	if (sdir == REVERSE)
		DOT.l = lBACK(DOT.l);
	else
		DOT.l = lFORW(DOT.l);

	while (count > 0 && !is_header_line(DOT, curbp)) {
		if ((i = firstchar(l_ref(DOT.l))) >= 0
		 && lGetc(DOT.l,i) == '#'
		 && (j = nextchar(l_ref(DOT.l),i+1)) >= 0
		 && ((this = cpp_keyword(l_ref(DOT.l),j)) != CPP_UNKNOWN)) {
			int	done = FALSE;

			switch (this) {
			case CPP_IF:
				if (sdir == FORWARD) {
					count++;
				} else {
					done = ((count-- == 1) && 
						(key != this));
					if (done)
						count = 0;
				}
				break;

			case CPP_ELIF:
			case CPP_ELSE:
				done = ((sdir == FORWARD) && (count == 1));
				if (done)
					count = 0;
				break;

			case CPP_ENDIF:
				if (sdir == FORWARD) {
					done = (--count == 0);
				} else {
					count++;
				}
			}

			if ((count <= 0) || done) {
				DOT.o = i;
				break;
			}
		}

		if (sdir == REVERSE)
			DOT.l = lBACK(DOT.l);
		else
			DOT.l = lFORW(DOT.l);

		if (is_header_line(DOT,curbp) || interrupted)
			return FALSE;
	}
	if (count == 0) {
		curwp->w_flag |= WFMOVE;
		if (doingopcmd)
			fulllineregions = TRUE;
		return TRUE;
	}
	return FALSE;
}

/*	the cursor is moved to a matching fence */
int
matchfence(f,n)
int f,n;
{
	int s = getfence(0, (!f || n > 0) ? FORWARD:REVERSE);
	if (s == FALSE)
		TTbeep();
	return s;
}

int
matchfenceback(f,n)
int f,n;
{
	int s = getfence(0, (!f || n > 0) ? REVERSE:FORWARD);
	if (s == FALSE)
		TTbeep();
	return s;
}

int
getfence(ch,sdir)
int ch; /* fence type to match against */
int sdir; /* direction to scan if we're not on a fence to begin with */
{
	MARK	oldpos; 	/* original pointer */
	register int ofence = 0;	/* open fence */
	int s, i;
	int key = CPP_UNKNOWN;

	/* save the original cursor position */
	oldpos = DOT;

	/* ch may have been passed, if being used internally */
	if (!ch) {
		if ((i = firstchar(l_ref(DOT.l))) < 0)	/* offset of first nonblank */
			return FALSE;		/* line is entirely blank */

		if (DOT.o <= i && (ch = lGetc(DOT.l,i)) == '#') {
			if (lLength(DOT.l) < i+3)
				return FALSE;
		} else if ((ch = char_at(DOT)) == '/' || ch == '*') {
			/* EMPTY */;
		} else if (sdir == FORWARD) {
			/* get the current character */
			if (oldpos.o < lLength(oldpos.l)) {
				do {
					ch = char_at(oldpos);
				} while(!isfence(ch) &&
					++oldpos.o < lLength(oldpos.l));
			}
			if (is_at_end_of_line(oldpos)) {
				return FALSE;
			}
		} else {
			/* get the current character */
			if (oldpos.o >= 0) {
				do {
					ch = char_at(oldpos);
				} while(!isfence(ch) && --oldpos.o >= 0);
			}

			if (oldpos.o < 0) {
				return FALSE;
			}
		}

		/* we've at least found a fence -- move us that far */
		DOT.o = oldpos.o;
	}

	/* setup proper matching fence */
	switch (ch) {
		case '(':
			ofence = ')';
			sdir = FORWARD;
			break;
		case ')':
			ofence = '(';
			sdir = REVERSE;
			break;
		case LBRACE:
			ofence = RBRACE;
			sdir = FORWARD;
			break;
		case RBRACE:
			ofence = LBRACE;
			sdir = REVERSE;
			break;
		case '[':
			ofence = ']';
			sdir = FORWARD;
			break;
		case ']':
			ofence = '[';
			sdir = REVERSE;
			break;
		case '#':
			if ((i = firstchar(l_ref(DOT.l))) < 0)
				return FALSE;	/* line is entirely blank */
			if ((i = nextchar(l_ref(DOT.l),i+1)) >= 0
			 && ((key = cpp_keyword(l_ref(DOT.l),i)) != CPP_UNKNOWN))
			 	break;
			return FALSE;
		case '*':
			ch = '/';
			if (DOT.o+1 < lLength(DOT.l) &&
				(lGetc(DOT.l, DOT.o+1) == '/')) {
				sdir = REVERSE;
				forwchar(TRUE,1);
				break;
			} else if (DOT.o > 0 &&
				(lGetc(DOT.l, DOT.o-1) == '/')) {
				sdir = FORWARD;
				backchar(TRUE,1);
				if (doingopcmd)
					pre_op_dot = DOT;
				break;
			}
			return FALSE;
		case '/':
			if (DOT.o+1 < lLength(DOT.l) &&
				(lGetc(DOT.l, DOT.o+1) == '*')) {
				sdir = FORWARD;
				break;
			} else if (DOT.o > 0 &&
				(lGetc(DOT.l, DOT.o-1) == '*')) {
				sdir = REVERSE;
				break;
			}
			/* FALL THROUGH */
		default: 
			return(FALSE);
	}

	/* ops are inclusive of the endpoint */
	if (doingopcmd && sdir == REVERSE) {
		forwchar(TRUE,1);
		pre_op_dot = DOT;
		backchar(TRUE,1);
	}

	if (key != CPP_UNKNOWN) {  /* we're searching for a cpp keyword */
		s = cpp_fence(sdir, key);
	} else if (ch == '/') {
		s = comment_fence(sdir);
	} else {
		s = simple_fence(sdir, ch, ofence);
	}

	if (s == TRUE)
		return TRUE;

	/* restore the current position */
	DOT = oldpos;
	return(FALSE);
}

int
simple_fence(sdir, ch, ofence)
int sdir;
int ch;
int ofence;
{
	int count = 1;
	int c, s = FALSE;

	/* set up for scan */
	if (sdir == REVERSE)
		backchar(FALSE, 1);
	else
		forwchar(FALSE, 1);

	while (count > 0) {
		if (is_at_end_of_line(DOT))
			c = '\n';
		else
			c = char_at(DOT);

		if (c == ch)
			++count;
		else if (c == ofence)
			--count;

		if (sdir == FORWARD)
			s = forwchar(FALSE, 1);
		else if (count == 0) {
			s = FALSE;
			break;		/* prevent backup past buffer top */
		} else
			s = backchar(FALSE, 1);

		if (s == FALSE || interrupted)
			return FALSE;
	}

	/* if count is zero, we have a match, move the sucker */
	if (count == 0) {
		if (s == TRUE) {
			if (sdir == FORWARD) {
				if (!doingopcmd)
					backchar(FALSE, 1);
			} else {
				forwchar(FALSE, 1);
			}
		}
		curwp->w_flag |= WFMOVE;
		return TRUE;
	}
	return FALSE;
}

int
comment_fence(sdir)
int sdir;
{
	MARK comstartpos;
	int count = 1;
	int c, s = FALSE;

	/* set up for scan */
	if (sdir == REVERSE)
		backchar(FALSE, 1);
	else
		forwchar(FALSE, 1);

	comstartpos.l = null_ptr;

	while (count > 0) {
		if (is_at_end_of_line(DOT))
			c = '\n';
		else
			c = char_at(DOT);

		if (c == '/') {
			/* is it a comment-end? */
			if ( DOT.o > 0 && lGetc(DOT.l, DOT.o-1) == '*') {
				if ( sdir == FORWARD) {
					count = 0;
				} else {
					if (l_ref(comstartpos.l)) {
						DOT = comstartpos;
						count = 0;
					} else {
						return FALSE;
					}
				}
			}
			/* is it a comment start? */
			if ( sdir == REVERSE &&
				DOT.o < lLength(DOT.l)-1 &&
				lGetc(DOT.l, DOT.o+1) == '*') {
				/* remember where we are */
				comstartpos = DOT;
			}

		}

		if (sdir == FORWARD)
			s = forwchar(FALSE, 1);
		else
			s = backchar(FALSE, 1);

		if (s == FALSE) {
			if (l_ref(comstartpos.l)) {
				DOT = comstartpos;
				count = 0;
				break;
			}
			return FALSE;
		}

		if (interrupted)
			return FALSE;
	}

	/* if count is zero, we have a match, move the sucker */
	if (count == 0) {
		if (s == TRUE) {
			if (sdir == FORWARD) {
				if (!doingopcmd && s)
					backchar(FALSE, 1);
			} else {
				forwchar(FALSE, 1);
			}
		}
		curwp->w_flag |= WFMOVE;
		return(TRUE);
	}
	return FALSE;
}

/* get the indent of the line containing the matching brace. */
int
fmatchindent()
{
	int ind;
	    
	MK = DOT;
	    
	if ((getfence(RPAREN,FORWARD) == FALSE) &&
	    (getfence(RBRACE,FORWARD) == FALSE)) {
		gomark(FALSE,1);
		return previndent((int *)0);
	}

	ind = indentlen(l_ref(DOT.l));

	gomark(FALSE,1);
	    
	return ind;
}



/*	Close fences are matched against their partners, and if
	on screen the cursor briefly lights there		*/
int
fmatch(ch)
int ch;	/* fence type to match against */
{
	MARK	oldpos; 		/* original position */
	register LINE *toplp;	/* top line in current window */
	register int count; /* current fence level count */
	register char opench;	/* open fence */
	register char c;	/* current character in scan */

	/* first get the display update out there */
	(void)update(FALSE);

	/* save the original cursor position */
	oldpos = DOT;

	/* setup proper open fence for passed close fence */
	if (ch == ')')
		opench = '(';
	else if (ch == RBRACE)
		opench = LBRACE;
	else
		opench = '[';

	/* find the top line and set up for scan */
	toplp = lBack(curwp->w_line.l);
	count = 1;
	backchar(TRUE, 2);

	/* scan back until we find it, or reach past the top of the window */
	while (count > 0 && l_ref(DOT.l) != toplp) {
		if (is_at_end_of_line(DOT))
			c = '\n';
		else
			c = char_at(DOT);
		if (c == ch)
			++count;
		if (c == opench)
			--count;
		if (backchar(FALSE, 1) != TRUE)
			break;
	}

	/* if count is zero, we have a match, display the sucker */
	/* there is a real machine dependent timing problem here we have
	   yet to solve......... */
	if (count == 0) {
		forwchar(FALSE, 1);
		if (update(FALSE) == TRUE)
		/* the idea is to leave the cursor there for about a
			quarter of a second */
			catnap(250, FALSE);
	}

	/* restore the current position */
	DOT = oldpos;

	return(TRUE);
}

#endif /* CFENCE */
