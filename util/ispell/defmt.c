#ifndef lint
static char Rcs_Id[] =
    "$Id: defmt.c,v 1.2 1994/05/25 01:27:35 asami Exp $";
#endif

/*
 * defmt.c - Handle formatter constructs, mostly by scanning over them.
 *
 * This code originally resided in ispell.c, but was moved here to keep
 * file sizes smaller.
 *
 * Copyright (c), 1983, by Pace Willisson
 *
 * Copyright 1992, 1993, Geoff Kuenning, Granada Hills, CA
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All modifications to the source code must be clearly marked as
 *    such.  Binary redistributions based on modified source code
 *    must be clearly marked as modified versions in the documentation
 *    and/or other materials provided with the distribution.
 * 4. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgment:
 *      This product includes software developed by Geoff Kuenning and
 *      other unpaid contributors.
 * 5. The name of Geoff Kuenning may not be used to endorse or promote
 *    products derived from this software without specific prior
 *    written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY GEOFF KUENNING AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL GEOFF KUENNING OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * The TeX code is originally by Greg Schaffer, with many improvements from
 * Ken Stevens.  The nroff code is primarily from Pace Willisson, although
 * other people have improved it.
 */

/*
 * $Log: defmt.c,v $
 * Revision 1.2  1994/05/25 01:27:35  asami
 * This is the FreeBSD port by Piero Serini (piero@strider.st.dsi.unimi.it).
 * Fixes include specifying correct dictionary (/usr/share/dict/words),
 * changing Makefiles, adding package target, etc.
 *
 * Note that this package requires an enormous amount of free disk space in
 * /usr/tmp (30MB according to Piero).  There is no /usr/tmp on freefall so
 * I couldn't compile it here but it worked at my home machine.
 *
 * Revision 1.1  1994/05/08  12:56:24  piero
 * Initial revision
 *
 * Revision 1.27  1994/02/14  00:34:53  geoff
 * Pass length arguments to correct().
 *
 * Revision 1.26  1994/01/25  07:11:25  geoff
 * Get rid of all old RCS log lines in preparation for the 3.1 release.
 *
 */

#include <ctype.h>
#include "config.h"
#include "ispell.h"
#include "proto.h"
#include "msgs.h"

static char *	skiptoword P ((char * bufp));
char *		skipoverword P ((char * bufp));
void		checkline P ((FILE * ofile));
static int	TeX_math_end P ((char ** bufp));
static int	TeX_math_begin P ((char ** bufp));
static int	TeX_LR_begin P ((char ** bufp));
static int	TeX_LR_check P ((int begin_p, char ** bufp));
static void	TeX_skip_args P ((char ** bufp));
static int	TeX_math_check P ((int cont_char, char ** bufp));
static void	TeX_skip_parens P ((char ** bufp));
static void	TeX_open_paren P ((char ** bufp));
static void	TeX_skip_check P ((char ** bufp));

#define ISTEXTERM(c)   (((c) == TEXLEFTCURLY) || \
			((c) == TEXRIGHTCURLY) || \
			((c) == TEXLEFTSQUARE) || \
			((c) == TEXRIGHTSQUARE))
#define ISMATHCH(c)    (((c) == TEXBACKSLASH) || \
			((c) == TEXDOLLAR) || \
			((c) == TEXPERCENT))

static char * skiptoword (bufp)		/* Skip to beginning of a word */
    char *	bufp;
    {

    while (*bufp
      &&  !isstringch(bufp, 0)
      &&  (!iswordch(chartoichar (*bufp))
	||  isboundarych(chartoichar (*bufp))
	||  (tflag  &&  (math_mode & 1)  &&  !TeX_comment))
      )
	{
	/* check paren necessity... */
	if (tflag) /* TeX or LaTeX stuff */
	    {
	    /* Odd numbers mean we are in "math mode" */
	    /* Even numbers mean we are in LR or */
	    /* paragraph mode */
	    if (TeX_comment)
		;			/* Don't check comments */
	    else if (*bufp == TEXPERCENT)
		TeX_comment = 1;
	    else if (math_mode & 1)
		{
		if ((LaTeX_Mode == 'e'  &&  TeX_math_check('e', &bufp))
		  || (LaTeX_Mode == 'm'  &&  TeX_LR_check(1, &bufp)))
		    math_mode--;    /* end math mode */
		else
		    {
		    while (*bufp  && !ISMATHCH(*bufp))
			bufp++;
		    if (*bufp == 0)
			break;
		    if (TeX_math_end(&bufp))
			math_mode--;
		    }
		if (math_mode < 0)
		    {
		    (void) fprintf (stderr,
		     DEFMT_C_TEX_MATH_ERROR);
		    math_mode = 0;
		    }
		}
	    else
		{
		if (math_mode > 1
		  &&  *bufp == TEXRIGHTCURLY
		  &&  (math_mode < (math_mode & 127) * 128))
		    math_mode--;    /* re-enter math */
		else if (LaTeX_Mode == 'm'
		    || (math_mode && (math_mode >= (math_mode & 127) * 128)
		  &&  (strncmp(bufp, "\\end", 4)
		    == 0)))
		    {
		    if (TeX_LR_check(0, &bufp))
			math_mode--;
		    }
		else if (LaTeX_Mode == 'b'  &&  TeX_math_check('b', &bufp))
		    {
		    /* continued begin */
		    math_mode++;
		    }
		else if (LaTeX_Mode == 'r')
		    {
		    /* continued "reference" */
		    TeX_skip_parens(&bufp);
		    LaTeX_Mode = 'P';
		    }
		else if (TeX_math_begin(&bufp))
		    /* checks references and */
		    /* skips \ commands */
		    math_mode++;
		}
	    if (*bufp == 0)
		break;
	    }
	else			/* formatting escape sequences */
	    {
	    if (*bufp == NRBACKSLASH)
		{
		switch ( bufp[1] )
		    {
		    case 'f':
			if(bufp[2] == NRLEFTPAREN)
			    {
			    /* font change: \f(XY */
			    bufp += 5;
			    }
			else
			    {
			    /* ) */
			    /* font change: \fX */
			    bufp += 3;
			    }
			continue;
		    case 's':
			/* size change */
			bufp += 2;
			if (*bufp == '+'  ||  *bufp == '-')
			    bufp++;
			/* This looks wierd 'cause we
			** assume *bufp is now a digit.
			*/
			bufp++;
			if (isdigit (*bufp))
			    bufp++;
			continue;
		    default:
			if (bufp[1] == NRLEFTPAREN)
			    {
			    /* extended char set */
			    /* escape:  \(XX */
			    /* ) */
			    bufp += 4;
			    continue;
			    }
			else if (bufp[1] == NRSTAR)
			    {
			    if (bufp[2] == NRLEFTPAREN)
				bufp += 5;
			    else
				bufp += 3;
			    continue;
			    }
			break;
		    }
		}
	    }
	bufp++;
	}
    if (*bufp == '\0')
	TeX_comment = 0;
    return bufp;
    }

char * skipoverword (bufp)	/* Return pointer to end of a word */
    register char *	bufp;	/* Start of word -- MUST BE A REAL START */
    {
    register char *	lastboundary;
    register int	scharlen; /* Length of a string character */

    lastboundary = NULL;
    for (  ;  ;  )
	{
	if (*bufp == '\0')
	    {
	    TeX_comment = 0;
	    break;
	    }
	else if (l_isstringch(bufp, scharlen, 0))
	    {
	    bufp += scharlen;
	    lastboundary = NULL;
	    }
	/*
	** Note that we get here if a character satisfies
	** isstringstart() but isn't in the string table;  this
	** allows string characters to start with word characters.
	*/
	else if (iswordch (chartoichar (*bufp)))
	    {
	    bufp++;
	    lastboundary = NULL;
	    }
	else if (isboundarych (chartoichar (*bufp)))
	    {
	    if (lastboundary == NULL)
		lastboundary = bufp;
	    bufp++;
	    }
	else
	    break;			/* End of the word */
	}
    /*
    ** If the word ended in one or more boundary characters, 
    ** the address of the first of these is in lastboundary, and it
    ** is the end of the word.  Otherwise, bufp is the end.
    */
    return (lastboundary != NULL) ? lastboundary : bufp;
    }

void checkline (ofile)
    FILE *		ofile;
    {
    register char *	p;
    register char *	endp;
    int			hadlf;
    register int	len;
    register int	i;
    int			ilen;

    currentchar = contextbufs[0];
    len = strlen (contextbufs[0]) - 1;
    hadlf = contextbufs[0][len] == '\n';
    if (hadlf)
	contextbufs[0][len] = 0;

    if (!tflag)
	{
	/* skip over .if */
	if (*currentchar == NRDOT
	  &&  (strncmp (currentchar + 1, "if t", 4) == 0
	    ||  strncmp (currentchar + 1, "if n", 4) == 0))
	    {
	    copyout (&currentchar,5);
	    while (*currentchar
	      &&  myspace (chartoichar (*currentchar)))
		copyout (&currentchar, 1);
	    }

	/* skip over .ds XX or .nr XX */
	if (*currentchar == NRDOT
	  &&  (strncmp (currentchar + 1, "ds ", 3) == 0 
	    ||  strncmp (currentchar + 1, "de ", 3) == 0
	    ||  strncmp (currentchar + 1, "nr ", 3) == 0))
	    {
	    copyout (&currentchar, 4);
	    while (*currentchar
	      &&  myspace (chartoichar (*currentchar)))
		copyout(&currentchar, 1);
	    while (*currentchar
	      &&  !myspace (chartoichar (*currentchar)))
		copyout(&currentchar, 1);
	    if (*currentchar == 0)
		{
		if (!lflag  &&  (aflag  ||  hadlf))
		    (void) putc ('\n', ofile);
		return;
		}
	    }
	}


    /* if this is a formatter command, skip over it */
    if (!tflag && *currentchar == NRDOT)
	{
	while (*currentchar  &&  !myspace (chartoichar (*currentchar)))
	    {
	    if (!aflag && !lflag)
		(void) putc (*currentchar, ofile);
	    currentchar++;
	    }
	if (*currentchar == 0)
	    {
	    if (!lflag  &&  (aflag  ||  hadlf))
		(void) putc ('\n', ofile);
	    return;
	    }
	}

    for (  ;  ;  )
	{
	p = skiptoword (currentchar);
	if (p != currentchar)
	    copyout (&currentchar, p - currentchar);

	if (*currentchar == 0)
	    break;

	p = ctoken;
	endp = skipoverword (currentchar);
	while (currentchar < endp  &&  p < ctoken + sizeof ctoken - 1)
	    *p++ = *currentchar++;
	*p = 0;
	if (strtoichar (itoken, ctoken, INPUTWORDLEN * sizeof (ichar_t), 0))
	    (void) fprintf (stderr, WORD_TOO_LONG (ctoken));
	ilen = icharlen (itoken);

	if (lflag)
	    {
	    if (ilen > minword
	      &&  !good (itoken, 0, 0)  &&  !cflag  &&  !compoundgood (itoken))
		(void) fprintf (ofile, "%s\n", ctoken);
	    }
	else
	    {
	    if (aflag)
		{
		if (ilen <= minword)
		    {
		    /* matched because of minword */
		    if (!terse)
			(void) fprintf (ofile, "*\n");
		    continue;
		    }
		if (good (itoken, 0, 0))
		    {
		    if (hits[0].prefix == NULL
		      &&  hits[0].suffix == NULL)
			{
			/* perfect match */
			if (!terse)
			    (void) fprintf (ofile, "*\n");
			}
		    else if (!terse)
			{
			/* matched because of root */
			(void) fprintf (ofile, "+ %s\n",
			  hits[0].dictent->word);
			}
		    }
		else if (compoundgood (itoken))
		    {
		    /* compound-word match */
		    if (!terse)
			(void) fprintf (ofile, "-\n");
		    }
		else
		    {
		    makepossibilities (itoken);
		    if (pcount)
			{
			/*
			** print &  or ?, ctoken, then
			** character offset, possibility
			** count, and the possibilities.
			*/
			(void) fprintf (ofile, "%c %s %d %d",
			  easypossibilities ? '&' : '?',
			  ctoken,
			  easypossibilities,
			  (int) ((currentchar - contextbufs[0])
			    - strlen (ctoken)));
			for (i = 0;  i < MAXPOSSIBLE;  i++)
			    {
			    if (possibilities[i][0] == 0)
				break;
			    (void) fprintf (ofile, "%c %s",
			      i ? ',' : ':', possibilities[i]);
			    }
			(void) fprintf (ofile, "\n");
			}
		    else
			{
			/*
			** No possibilities found for word TOKEN
			*/
			(void) fprintf (ofile, "# %s %d\n",
			  ctoken,
			  (int) ((currentchar - contextbufs[0])
			    - strlen (ctoken)));
			}
		    }
		}
	    else
		{
		if (!quit)
		   correct (ctoken, sizeof ctoken, itoken, sizeof itoken,
		     &currentchar);
		}
	    }
	if (!aflag  &&  !lflag)
	   (void) fprintf (ofile, "%s", ctoken);
	}

    if (!lflag  &&  (aflag  ||  hadlf))
       (void) putc ('\n', ofile);
   }

/* must check for \begin{mbox} or whatever makes new text region. */
static int TeX_math_end (bufp)
    char **	bufp;
    {

    if (TeX_comment)
	return 0;
    else if (**bufp == TEXDOLLAR)
	{
	if ((*bufp)[1] == TEXDOLLAR)
	    (*bufp)++;
	return 1;
	}
    else if (**bufp == TEXPERCENT)
	{
	TeX_comment = 1;
	return 0;
	}
    /* processing extended TeX command */
    (*bufp)++;
    if (**bufp == TEXRIGHTPAREN  ||  **bufp == TEXRIGHTSQUARE)
	return 1;
    if (TeX_LR_begin (bufp))	/* check for switch back to LR mode */
	return 1;
    if (strncmp (*bufp, "end", 3) == 0)
	/* find environment that is ending */
	return TeX_math_check ('e', bufp);
    else
	return 0;
    }

static int TeX_math_begin (bufp)
    char **	bufp;
    {

    if (**bufp == TEXDOLLAR)
	{
	if ((*bufp)[1] == TEXDOLLAR)
	    (*bufp)++;
	return 1;
	}
    while (**bufp == TEXBACKSLASH)
	{
	(*bufp)++; /* check for null char here? */
	if (**bufp == TEXLEFTPAREN  ||  **bufp == TEXLEFTSQUARE)
	    return 1;
	if (strncmp (*bufp, "begin", 5) == 0)
	    {
	    if (TeX_math_check ('b', bufp))
		return 1;
	    else
		(*bufp)--;
	    }
	else
	    {
	    TeX_skip_check (bufp);
	    return 0;
	    }
	}
      /*
       * Ignore references for the tib (1) bibliography system, that
       * is, text between a ``[.'' or ``<.'' and ``.]'' or ``.>''.
       * We don't care whether they match, tib doesn't care either.
       *
       * A limitation is that the entire tib reference must be on one
       * line, or we break down and check the remainder anyway.
       */ 
    if ((**bufp == TEXLEFTSQUARE  ||  **bufp == TEXLEFTANGLE)
      &&  (*bufp)[1] == TEXDOT)
	{
	(*bufp)++;
	while (**bufp)
	    {
	    if (*(*bufp)++ == TEXDOT
	      &&  (**bufp == TEXRIGHTSQUARE  ||  **bufp == TEXRIGHTANGLE))
		return TeX_math_begin (bufp);
	    }
	return 0;
	}
    else
	return 0;
    }

static int TeX_LR_begin (bufp)
    char **	bufp;
    {

    if ((strncmp (*bufp, "mbox", 4) == 0)
      ||  (strncmp (*bufp, "makebox", 7) == 0)
      ||  (strncmp (*bufp, "fbox", 4) == 0)
      || (strncmp (*bufp, "framebox", 8) == 0))
	math_mode += 2;
    else if ((strncmp(*bufp, "parbox", 6) == 0)
      || (strncmp(*bufp, "raisebox", 8) == 0))
	{
	math_mode += 2;
	TeX_open_paren (bufp);
	if (**bufp)
	    (*bufp)++;
	else
	    LaTeX_Mode = 'r'; /* same as reference -- skip {} */
	}
    else if (strncmp(*bufp, "begin", 5) == 0)
	return TeX_LR_check (1, bufp);	/* minipage */
    else
	return 0;

    /* skip tex command name and optional or width arguments. */
    TeX_open_paren (bufp);
    return 1;
    }

static int TeX_LR_check (begin_p, bufp)
    int		begin_p;
    char **	bufp;
    {

    TeX_open_paren (bufp);
    if (**bufp == 0)	/* { */
	{
	LaTeX_Mode = 'm';
	return 0;	/* remain in math mode until '}' encountered. */
	}
    else
	LaTeX_Mode = 'P';
    if (strncmp (++(*bufp), "minipage", 8) == 0)
	{
	TeX_skip_parens (bufp);
	if (**bufp)
	    (*bufp)++;
	if (begin_p)
	    {
	    TeX_skip_parens (bufp); /* now skip opt. args if on this line. */
	    math_mode += 2;
	    /* indicate minipage mode. */
	    math_mode += ((math_mode & 127) - 1) * 128;
	    }
	else
	    {
	    math_mode -= (math_mode & 127) * 128;
	    if (math_mode < 0)
		{
		(void) fprintf (stderr, DEFMT_C_LR_MATH_ERROR);
		math_mode = 1;
		}
	    }
	return 1;
	}
    (*bufp)--;
    return 0;
    }

/* Skips the begin{ARG}, and optionally up to two {PARAM}{PARAM}'s to
 *  the begin if they are required.  However, Only skips if on this line.
 */
static void TeX_skip_args (bufp)
    char **	bufp;
    {
    register int skip_cnt = 0; /* Max of 2. */

    if (strncmp(*bufp, "tabular", 7) == 0
      ||  strncmp(*bufp, "minipage", 8) == 0)
	skip_cnt++;
    if (strncmp(*bufp, "tabular*", 8) == 0)
	skip_cnt++;
    TeX_skip_parens (bufp);	/* Skip to the end of the \begin{} parens */
    if (**bufp)
	(*bufp)++;
    else
	return;
    if (skip_cnt--)
	TeX_skip_parens (bufp);	/* skip 1st {PARAM}. */
    else
	return;
    if (**bufp)
	(*bufp)++;
    else
	return;
    if (skip_cnt)
	TeX_skip_parens (bufp);	/* skip to end of 2nd {PARAM}. */
    }

static int TeX_math_check (cont_char, bufp)
    int		cont_char;
    char **	bufp;
    {

    TeX_open_paren (bufp);
    /* Check for end of line, continue later. */
    if (**bufp == 0)
	{
	LaTeX_Mode = (char) cont_char;
	return 0;
	}
    else
	LaTeX_Mode = 'P';

    if (strncmp (++(*bufp), "equation", 8) == 0
      ||  strncmp (*bufp, "eqnarray", 8) == 0
      ||  strncmp (*bufp, "displaymath", 11) == 0
      ||  strncmp (*bufp, "array", 5) == 0
      ||  strncmp (*bufp, "picture", 7) == 0
#ifdef IGNOREBIB
      ||  strncmp (*bufp, "thebibliography", 15) == 0
#endif
      ||  strncmp (*bufp, "math", 4) == 0)
	{
	(*bufp)--;
	TeX_skip_parens (bufp);
	return 1;
	}
    if (cont_char == 'b')
	TeX_skip_args (bufp);
    else
	TeX_skip_parens (bufp);
    return 0;
    }

static void TeX_skip_parens (bufp)
    char **	bufp;
    {

    while (**bufp  &&  **bufp != TEXRIGHTCURLY)
	(*bufp)++;
    }

static void TeX_open_paren (bufp)
    char **	bufp;
    {
    while (**bufp  &&  **bufp != TEXLEFTCURLY)
	(*bufp)++;
    }

static void TeX_skip_check (bufp)
    char **	bufp;
    {
    int		charlen;

    /* ADDITIONALLY, MAY WANT TO ADD:
     * input, include, includeonly,
     * documentstyle, pagestyle, pagenumbering
     * WITH TWO {} {}'S TO SKIP:
     * setcounter, addtocounter,
     * setlength, addtolength, settowidth
     */

    if (strncmp(*bufp, "end", 3) == 0
      ||  strncmp(*bufp, "vspace", 6) == 0
      ||  strncmp(*bufp, "hspace", 6) == 0
      ||  strncmp(*bufp, "cite", 4) == 0
      ||  strncmp(*bufp, "ref", 3) == 0
      ||  strncmp(*bufp, "parbox", 6) == 0
      ||  strncmp(*bufp, "label", 5) == 0
      ||  strncmp(*bufp, "input", 5) == 0
      ||  strncmp(*bufp, "nocite", 6) == 0
      ||  strncmp(*bufp, "include", 7) == 0
      ||  strncmp(*bufp, "includeonly", 11) == 0
      ||  strncmp(*bufp, "documentstyle", 13) == 0
#ifndef IGNOREBIB
      ||  strncmp(*bufp, "bibliography", 12) == 0
      ||  strncmp(*bufp, "bibitem", 7) == 0
#endif
      ||  strncmp(*bufp, "hyphenation", 11) == 0
      ||  strncmp(*bufp, "pageref", 7) == 0)
	{
	TeX_skip_parens (bufp);
	if (**bufp == 0)
	    LaTeX_Mode = 'r';
	}
    else if (strncmp(*bufp, "rule", 4) == 0)	/* skip two args. */
	{
	TeX_skip_parens (bufp);
	if (**bufp == 0)	/* Only skips one {} if not on same line. */
	    LaTeX_Mode = 'r';
	else			/* Skip second arg. */
	    {
	    (*bufp)++;
	    TeX_skip_parens (bufp);
	    if (**bufp == 0)
		LaTeX_Mode = 'r';
	    }
	}
    else
	{
	/* Optional tex arguments sometimes should and
	** sometimes shouldn't be checked
	** (eg \section [C Programming] {foo} vs
	**     \rule [3em] {0.015in} {5em})
	** SO -- we'll just igore it rather than make a
	** full LaTeX parser.
	*/

	/* Must look at the space after the command. */
	while (**bufp
	  && (l1_isstringch (*bufp, charlen, 0)
	    ||  iswordch (chartoichar (**bufp))))
	    {
	    if (!isstringch (*bufp + charlen, 0)
	      &&  !iswordch (chartoichar ((*bufp)[charlen])))
		break;
	    *bufp += charlen;
	    }
	}
    }
