#ifndef lint
static char Rcs_Id[] =
    "$Id: good.c,v 1.2 1994/05/25 01:27:41 asami Exp $";
#endif

/*
 * good.c - see if a word or its root word
 * is in the dictionary.
 *
 * Pace Willisson, 1983
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
 */

/*
 * $Log: good.c,v $
 * Revision 1.2  1994/05/25 01:27:41  asami
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
 * Revision 1.39  1994/01/25  07:11:31  geoff
 * Get rid of all old RCS log lines in preparation for the 3.1 release.
 *
 */

#include <ctype.h>
#include "config.h"
#include "ispell.h"
#include "proto.h"

int		good P ((ichar_t * word, int ignoreflagbits, int allhits));
#ifndef NO_CAPITALIZATION_SUPPORT
static int	cap_ok P ((ichar_t * word, struct success * hit, int len));
static int	entryhasaffixes P ((struct dent * dent, struct success * hit));
#endif /* NO_CAPITALIZATION_SUPPORT */
void		flagpr P ((ichar_t * word, int preflag, int prestrip,
		  int preadd, int sufflag, int sufadd));

static ichar_t *	orig_word;

#ifndef NO_CAPITALIZATION_SUPPORT
int good (w, ignoreflagbits, allhits)
    ichar_t *		w;		/* Word to look up */
    int			ignoreflagbits;	/* NZ to ignore affix flags in dict */
    int			allhits;	/* NZ to ignore case, get every hit */
#else
/* ARGSUSED */
int good (w, ignoreflagbits, dummy)
    ichar_t *		w;		/* Word to look up */
    int			ignoreflagbits;	/* NZ to ignore affix flags in dict */
    int			dummy;
#define allhits	0	/* Never actually need more than one hit */
#endif
    {
    ichar_t		nword[INPUTWORDLEN + MAXAFFIXLEN];
    register ichar_t *	p;
    register ichar_t *	q;
    register		n;
    register struct dent * dp;

    /*
    ** Make an uppercase copy of the word we are checking.
    */
    for (p = w, q = nword;  *p;  )
	*q++ = mytoupper (*p++);
    *q = 0;
    n = q - nword;

    numhits = 0;

    if (cflag)
	{
	(void) printf ("%s", ichartosstr (w, 0));
	orig_word = w;
	}
    else if ((dp = lookup (nword, 1)) != NULL)
	{
	hits[0].dictent = dp;
	hits[0].prefix = NULL;
	hits[0].suffix = NULL;
#ifndef NO_CAPITALIZATION_SUPPORT
	if (allhits  ||  cap_ok (w, &hits[0], n))
	    numhits = 1;
#else
	numhits = 1;
#endif
	}
    if (numhits  &&  !allhits)
	return 1;

    /* try stripping off affixes */

#if 0
    numchars = icharlen (nword);
    if (numchars < 4)
	{
	if (cflag)
	    (void) putchar ('\n');
	return numhits  ||  (numchars == 1);
	}
#endif

    chk_aff (w, nword, n, ignoreflagbits, allhits);

    if (cflag)
	(void) putchar ('\n');

#ifndef NO_CAPITALIZATION_SUPPORT
    if (numhits)
	return allhits  ||  cap_ok (w, &hits[0], n);
    else
	return 0;
#else
    return numhits;
#endif
    }

#ifndef NO_CAPITALIZATION_SUPPORT
static int cap_ok (word, hit, len)
    register ichar_t *		word;
    register struct success *	hit;
    int				len;
    {
    register ichar_t *		dword;
    register ichar_t *		w;
    register struct dent *	dent;
    ichar_t			dentword[INPUTWORDLEN + MAXAFFIXLEN];
    int				preadd;
    int				prestrip;
    int				sufadd;
    ichar_t *			limit;
    long			thiscap;
    long			dentcap;

    thiscap = whatcap (word);
    /*
    ** All caps is always legal, regardless of affixes.
    */
    preadd = prestrip = sufadd = 0;
    if (thiscap == ALLCAPS)
	return 1;
    else if (thiscap == FOLLOWCASE)
	{
	/* Set up some constants for the while(1) loop below */
	if (hit->prefix)
	    {
	    preadd = hit->prefix->affl;
	    prestrip = hit->prefix->stripl;
	    }
	else
	    preadd = prestrip = 0;
	sufadd = hit->suffix ? hit->suffix->affl : 0;
	}
    /*
    ** Search the variants for one that matches what we have.  Note
    ** that thiscap can't be ALLCAPS, since we already returned
    ** for that case.
    */
    dent = hit->dictent;
    for (  ;  ;  )
	{
	dentcap = captype (dent->flagfield);
	if (dentcap != thiscap)
	    {
	    if (dentcap == ANYCASE  &&  thiscap == CAPITALIZED
	     &&  entryhasaffixes (dent, hit))
		return 1;
	    }
	else				/* captypes match */
	    {
	    if (thiscap != FOLLOWCASE)
		{
		if (entryhasaffixes (dent, hit))
		    return 1;
		}
	    else
		{
		/*
		** Make sure followcase matches exactly.
		** Life is made more difficult by the
		** possibility of affixes.  Start with
		** the prefix.
		*/
		(void) strtoichar (dentword, dent->word, INPUTWORDLEN, 1);
		dword = dentword;
		limit = word + preadd;
		if (myupper (dword[prestrip]))
		    {
		    for (w = word;  w < limit;  w++)
			{
			if (mylower (*w))
			    goto doublecontinue;
			}
		    }
		else
		    {
		    for (w = word;  w < limit;  w++)
			{
			if (myupper (*w))
			    goto doublecontinue;
			}
		    }
		dword += prestrip;
		/* Do root part of word */
		limit = dword + len - preadd - sufadd;
		while (dword < limit)
		    {
		    if (*dword++ != *w++)
		      goto doublecontinue;
		    }
		/* Do suffix */
		dword = limit - 1;
		if (myupper (*dword))
		    {
		    for (  ;  *w;  w++)
			{
			if (mylower (*w))
			    goto doublecontinue;
			}
		    }
		else
		    {
		    for (  ;  *w;  w++)
			{
			if (myupper (*w))
			    goto doublecontinue;
			}
		    }
		/*
		** All failure paths go to "doublecontinue,"
		** so if we get here it must match.
		*/
		if (entryhasaffixes (dent, hit))
		    return 1;
doublecontinue:	;
		}
	    }
	if ((dent->flagfield & MOREVARIANTS) == 0)
	    break;
	dent = dent->next;
	}

    /* No matches found */
    return 0;
    }

/*
** See if this particular capitalization (dent) is legal with these
** particular affixes.
*/
static int entryhasaffixes (dent, hit)
    register struct dent *	dent;
    register struct success *	hit;
    {

    if (hit->prefix  &&  !TSTMASKBIT (dent->mask, hit->prefix->flagbit))
	return 0;
    if (hit->suffix  &&  !TSTMASKBIT (dent->mask, hit->suffix->flagbit))
	return 0;
    return 1;			/* Yes, these affixes are legal */
    }
#endif

/*
 * Print a word and its flag, making sure the case of the output matches
 * the case of the original found in "orig_word".
 */
void flagpr (word, preflag, prestrip, preadd, sufflag, sufadd)
    register ichar_t *	word;		/* (Modified) word to print */
    int			preflag;	/* Prefix flag (if any) */
    int			prestrip;	/* Lth of pfx stripped off orig_word */
    int			preadd;		/* Length of prefix added to w */
    int			sufflag;	/* Suffix flag (if any) */
    int			sufadd;		/* Length of suffix added to w */
    {
    register ichar_t *	origp;		/* Pointer into orig_word */
    int			orig_len;	/* Length of orig_word */

    orig_len = icharlen (orig_word);
    /*
     * We refuse to print if the cases outside the modification
     * points don't match those just inside.  This prevents things
     * like "OEM's" from being turned into "OEM/S" which expands
     * only to "OEM'S".
     */
    if (preflag > 0)
	{
	origp = orig_word + preadd;
	if (myupper (*origp))
	    {
	    for (origp = orig_word;  origp < orig_word + preadd;  origp++)
		{
		if (mylower (*origp))
		    return;
		}
	    }
	else
	    {
	    for (origp = orig_word;  origp < orig_word + preadd;  origp++)
		{
		if (myupper (*origp))
		    return;
		}
	    }
	}
    if (sufflag > 0)
	{
	origp = orig_word + orig_len - sufadd;
	if (myupper (origp[-1]))
	    {
	    for (  ;  *origp != 0;  origp++)
		{
		if (mylower (*origp))
		    return;
		}
	    }
	else
	    {
	    origp = orig_word + orig_len - sufadd;
	    for (  ;  *origp != 0;  origp++)
		{
		if (myupper (*origp))
		    return;
		}
	    }
	}
    /*
     * The cases are ok.  Put out the word, being careful that the
     * prefix/suffix cases match those in the original, and that the
     * unchanged characters from the original actually match it.
     */
    (void) putchar (' ');
    origp = orig_word + preadd;
    if (myupper (*origp))
	{
	while (--prestrip >= 0)
	    (void) fputs (printichar ((int) *word++), stdout);
	}
    else
	{
	while (--prestrip >= 0)
	    (void) fputs (printichar ((int) mytolower (*word++)), stdout);
	}
    for (prestrip = orig_len - preadd - sufadd;  --prestrip >= 0;  word++)
	(void) fputs (printichar ((int) *origp++), stdout);
    if (origp > orig_word)
	origp--;
    if (myupper (*origp))
	(void) fputs (ichartosstr (word, 0), stdout);
    else
	{
	while (*word)
	    {
	    (void) fputs (printichar ((int) mytolower (*word++)), stdout);
	    }
	}
    /*
     * Now put out the flags
     */
    (void) putchar (hashheader.flagmarker);
    if (preflag > 0)
	(void) putchar (preflag);
    if (sufflag > 0)
	(void) putchar (sufflag);
    }
