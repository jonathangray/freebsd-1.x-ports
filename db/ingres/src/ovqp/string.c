#include <stdio.h>

#include <ingres.h>
#include <aux.h>
#include <symbol.h>
#include <tree.h>
#include "../decomp/globs.h"
#include "sccs.h"
#include <errors.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#define INGRES_GUTIL
#define INGRES_DECOMP
#define INGRES_SUPPORT
#include "protos.h"

SCCSID(@(#)string.c	8.5	5/30/88)

/*
**	This file contains the string
**	manipulation routines
*/


/*
**	Concat takes the two character strings in
**	s1 and s2 and concatenates them together
**	into a new location.
**
**	trailing blanks are removed from the first symbol.
**	The size of the concatenation equals the sum of
**	the two original strings.
*/
void
concatsym(register sym_t *s1, register sym_t *s2)
{
	register char	*p;
	int		size1, size2, i;
	char		*px;

/*	size1 = size(s1);*/	/* compute size w/o trailing blanks */
	size1 = s1->len & I1MASK;
	if (size1 == 0 && s1->len != 0)
		size1++;	/* don't allow string to be trunc to zero length */
	size2 = s2->len & I1MASK;	/* size of second string remains the same */
	i = (s1->len & I1MASK) + size2;	/* i equals sum of original sizes */
	if (i > MAX_FIELD_SIZE)
		i = MAX_FIELD_SIZE;	/* a string can't exceed this size */
	if (size2 + size1 > MAX_FIELD_SIZE)
		size2 = MAX_FIELD_SIZE - size1;	/* adjust size2 to not exceed MAX_FIELD_SIZE */

	px = p = need(De.ov_ovqpbuf, i);	/* request the needed space */
	bmove(s1->value.sym_data.cptype, p, size1);	/* copy first string */
	p = &p[size1];
	bmove(s2->value.sym_data.cptype, p, size2);
	p = &p[size2];
	s1->value.sym_data.cptype = px;
	s1->len = i;
	/* pad with blanks if necessary */
	i -= size1 - size2;
	while (i--)
		*p++ = ' ';

#ifdef xOTR1
	if (tTf(82, 3)) {
		printf("Concat:");
		prstack(s1);
	}
#endif
}

/*
**	Size determines the size of a character symbol
**	without trailing blanks.
*/
int
size(register sym_t *s)
{
	register char		*c;
	register int		i;

	c = s->value.sym_data.cptype;
	i = s->len & I1MASK;

	for (c += i; i; i--)
		if(*--c != ' ')
			break;

	return (i);
}

/*
**	Converts the numeric symbol to
**	ascii. Formats to be used are determined
**	by Out_arg.
*/
void
ascii(register sym_t *s)
{
	register int		i;
	register char		*p;
	char			temp[MAX_FIELD_SIZE];
	double			dtemp;
	extern struct out_arg	Out_arg;	/* used for float conversions */

	i = 0;
	p = temp;
	switch(s->type) {

	  case INT_CONST:
		if (s->len == 4) {
			i = Out_arg.i4width;
			p = locv(s->value.sym_data.i4type);
		} else {
			if (s->len == 2) {
				itoa(s->value.sym_data.i2type, p);
				i = Out_arg.i2width;
			} else {
				itoa(s->value.sym_data.i1type, p);
				i = Out_arg.i1width;
			}
		}
		break;

	  case CHAR_CONST:
		return;

	  case FLOAT_CONST:
		if (s->len == 4) {
			i = Out_arg.f4width;
			dtemp = s->value.sym_data.f4type;
			ftoa(dtemp, p, i, Out_arg.f4prec, Out_arg.f4style);
		} else {
			i = Out_arg.f8width;
			ftoa(s->value.sym_data.f8type, p, i, Out_arg.f8prec, Out_arg.f8style);
		}
	}
	s->value.sym_data.cptype = need(De.ov_ovqpbuf, i);
	pmove(p, s->value.sym_data.cptype, i, ' ');	/* blank pad to fixed length i */
	s->type = CHAR_CONST;
	s->len = i;
}

/*
**	PAT_INSERT 
**	
**	Moves str and its corresponding length into Pats[index] 
**	where index refers to the PAT_SPEC index.
**
**	May be called even though the Pats[index] string is not the one 
**	which will eventually be used for the replace.  For instance, 
**	if the pattern matching coincides but the line number doesn't.
**
**	Side Effects:
**		Patnum is incremented indicating an insertion was done.
**		Pats[index] record gets a new string and length.
**
**	Returns:  none
**
**	Calls:	bmove
**
**	Called By:
**		pmatch, lexcomp
*/
void
pat_insert(char *str, int slen, char where, int no_blanks)
/* str - the string being moved to Pats[where] */
/* slen - length of str */
/* where - index into Pats */
{
	int	index;		/* integer value of Pats' index */
	int 	i;

	index = where - '0';
	if (no_blanks)		/* get rid of blanks */
	    while (*(str + slen - 1) == ' ')  
		slen--;

	if (Pats[index].string) 		/* for overwriting string */ {
		xfree(Pats[index].string);
		Pats[index].string = NULL;	/* Not really necessary, but helps with debugging */
	}
		Patnum++;

	Pats[index].string = xalloc(slen, 0, 1);
	bmove(str,Pats[index].string,slen);	/* move str to Pats[].string */
	Pats[index].len = slen;
#ifdef xOTR1
	if (tTf(82,9)) {
		for (i = 0; i < PATNUM; i++)
			printf("Pats[%d] = %s, len = %d\n", i,Pats[i].string, Pats[i].len);
	}
#endif
}

/*
**	PMATCH
**
**	Checks if a pattern containing a pattern matching character 
**	(either PAT_ANY or PAT_SPEC) is found in a string.
**
**	Returns:
**		 0 -- if pattern found in string
**		-1 -- if no match
**
**	Called By:
**		lexcomp
**
**	Calls:
**		pat_insert, lexcomp
*/
int
pmatch(char patarg, char *pat, int plen,  char *str, int slength)
/* patarg - index for pattern matching--FALSE when no indices used */
/* pat - the string holding the pattern matching char */
/* str - the string to be checked */
{
	register char	d, *s;
	register int	slen,count;
	char		c;

#ifdef xOTR1
		if (tTf(82,5)) {
			printf("PMATCH: starting\n");
			printf("patarg = %c\n",patarg);
			printf("string with pattern char = %s\n", pat);
			printf("string len = %d \n", plen);
			printf("string to check = %s\n", str);
			printf("string len = %d\n", slength);
		}
#endif
	s = str;
	slen = slength;

	if (plen == 0) {
		if ( patarg ) {
			pat_insert(str,slength,patarg,1);
		}
		return(0);	/* a match if no more chars in p */
	}

	/*
	** If the next character in "pat" is not another
	** pattern matching character, then scan until
	** first matching char and continue comparison.
	*/
	if ((c = *pat) != PAT_ANY && c != PAT_SPEC &&
	    c != PAT_LBRAC && c != PAT_ONE) {
		count = 0;
		while (slen--) {
			if (((d = *s) == c ||
			     d == PAT_ANY ||
			     d == PAT_SPEC ||
			     d == PAT_LBRAC) &&
			    d != PAT_ONE) {
				if ( patarg ) {
					pat_insert(str,count,patarg,0);
				}
				if (lexcomp(s, slen + 1,pat, plen, 1) == 0) {
					return (0);
				}
			}
			s++;
			count++;
		}
	} else {
		while (slen) {
			if (lexcomp(s++, slen--,pat, plen, 1) == 0) {
				return (0);	/* match */
			}
		}
	}
	return (-1);	/* no match */
}

/*
**	LEXCOMP performs character comparisons between the two
**	strings ss1 and ss2. All blanks and null are ignored in
**	both strings. In addition pattern matching is performed
**	using the "shell syntax". Pattern matching characters
**	are converted to the pattern matching symbols PAT_ANY etc.
**	by the scanner.
**
**	Pattern matching characters can appear in either or
**	both strings. Since they cannot be stored in relations,
**	pattern matching chars in both strings can only happen
**	if the user types in such an expression.
**
**	examples:
**
**	"Smith, Homer" = "Smith,Homer"
**
**	"abcd" < "abcdd"
**
**	"abcd" = "aPAT_ANYd"
**
**	returns	<0 if s1 < s2
**		 0 if s1 = s2
**		>0 if s1 > s2
*/

char	*S1,*S2;
int	L1,L2;

int
lexcomp(register char *s1, register int l1, register char *s2, register int l2, int x)
{
	char		c1, c2;
	int		howmany = Patnum;	/* howmany PAT_SPEC char matchings so far */
	int 		retval;
	int		i;
	char		*t1, *t2;

#ifdef xOTR1
	if (tTf(82, 4)) {
		printf("LEXCOMP: starting...\n");
		t1 = s1;
		t2 = s2;
		printf("howmany = %d\n", howmany);
		printf("first string= '");
		for (i = 0; i < l1; i++)
			printf("%c", *t1++);
		printf("'\n");
		printf("length = %d\n", l1);
		printf("second string= '");
		for (i = 0; i < l2; i++)
			printf("%c", *t2++);
		printf("'\n");
		printf("length = %d\n", l2);
	}
#endif
	/* save initial information in case a PAT_GLOB is found */
	if (x==0) {
	    S1 = s1; 
	    S2 = s2;
	    L1 = l1; 
	    L2 = l2;
	}
loop:
	while (l1--) {
		switch (c1 = *s1++) {

		  /* case ' ': */
		  case '\0':
			break;

		  case PAT_GLOB:
		  {
			return(gmatch(S1,L1,S2,L2));
	       	  }

		  case PAT_ANY:
			return (pmatch(FALSE, s1, l1, s2, l2));

		  case PAT_SPEC:
			retval = pmatch(*(s1-1),++s1, --l1, s2, l2);

			/*
			** If there was no match in pmatch, 
			** reset Patnum to previous value
			*/

#ifdef xOTR1
		if (tTf(82,4))
			printf("lexcomp: return %d\n", retval);

#endif
			if (retval) 
			   Patnum = howmany;
			return (retval);

		  case PAT_LBRAC:
			return (lmatch(s1, l1, s2, l2));

		  default:
			while (l2--) {
				switch (c2 = *s2++) {

				  /* case ' ': */
				  case '\0':
					continue;

				  case PAT_GLOB:
				  {
					return(gmatch(S2,L2,S1,L1));
				  }

				  case PAT_ANY:
					return( pmatch(FALSE,s2,l2,--s1,++l1));

				  case PAT_SPEC:
					retval = pmatch(*(s2-1),++s2, --l2, --s1, ++l1);

#ifdef xOTR1
					if (tTf(82,4))
						printf("lexcomp: retval = %d\n", retval);
#endif

					if (retval) 
					   Patnum = howmany;
					return (retval);

				  case PAT_LBRAC:
					return (lmatch(s2, l2, --s1, ++l1));

				  default:
					if (c1 == c2)
						goto loop;
					if (c1 == PAT_ONE || c2 == PAT_ONE)
						goto loop;
#ifdef xOTR1
					if (tTf(82,4))
						printf("lexcomp: 2.return %d\n",c1 - c2);
#endif
					return (c1 - c2);
				}
			}
#ifdef xOTR1
			if (tTf(82,4))
				printf("lexcomp: returning 1\n");
#endif
			return (1);	/* s1 > s2 */
		}
	}

	/* examine remainder of s2 for any characters */
	while (l2) {
		l2--;
		if ((c1 = *s2++) == PAT_SPEC) {
		    pat_insert("",0,*s2,0);  /* insert empty string */
		    s2++,l2--;	        /* get rid of index */
		}
		/* if (((c1 = *s2) != ' ') && (c1 != '\0') */
		if ((c1 != ' ') && (c1 != '\0') 
			&& (c1 != PAT_ANY) && (c1 != PAT_SPEC)) {
#ifdef xOTR1
			if (tTf(82,4))
				printf("lexcomp: returning -1\n");
#endif
			Patnum = howmany;
			return (-1);	/* s1 < s2 */
		}
	}
#ifdef xOTR1
		if (tTf(82,4))
			printf("lexcomp: returning 0\n");
#endif
	return (0);
}

int
lmatch(char *pat, int plen, char *str, int slen)
/* pat - the string holding the pattern matching char */
/* str - the other string */
{
	register char	*p, *s;
	register int	cc;
	int		oldc, c, found;

#ifdef xOTR1
		if (tTf(82,6)) {
			printf("LMATCH: starting...\n");
			printf("Pat = %s, length = %d\n", pat, plen);
			printf("Str = %s, length = %d\n", str, slen);
		}
#endif
	p = pat;
	s = str;

	/* find a non-blank, non-null char in s */
	while (slen--) {
		if ((c = *s++) != ' ' && c != '\0') {
			/* search for a match on 'c' */
			found = 0;	/* assume failure */
			oldc = 0777;	/* make previous char large */

			while (plen--) {

				switch(cc = *p++) {

				  case PAT_RBRAC:
					if (found) {
						return (lexcomp(s, slen,p, plen, 1));
					}
					return (-1);

				  case '-':
					if (plen-- == 0)
						return (-1);	/* not found */
					if (oldc <= c && c <= (cc = *p++))
						found++;
					break;

				  default:
					if (c == (oldc = cc))
						found++;
				}
			}
			return (-1);	/* no match */
		}
	}
	return (1);
}

int flink[MAX_FIELD_SIZE];  		/* array for storing failure points in string */

/*
**	SCANSTR:  Scan a string for a pattern.  
**
**	Returns:
**		-1 -- couldn't find pattern in string
**		index in string to start of pattern -- if getstart is true
**		index in string following pattern   -- if getstart is false
*/
int
scanstr(char *str, int slen, char *pat, int plen, int getstart, char num)
/* str - string being scanned 			     */
/* pat - pattern being searched for 			     */
/* slen - str length					     */
/* plen - pat length					     */
/* getstart - if true, include pat in the string to be returned */
/* num - number of occurance to look for		     */
{
    int i,	/* index into str */
	j,	/* index into pat */
	k,
	found;	/* true when pattern found in string */

#ifdef xOTR1
	if (tTf(82,11)) {
		printf("SCANSTR: \n");
		printf("str = %s, len = %d\n", str, slen);
		printf("pat = %s, len = %d\n", pat, plen);
	}
#endif

    createlink(pat,plen);
    i = -1;

    /* for each occurance of pattern in string */
    for (k = 0; k < (num & I1MASK); k++) {
	i += 1;
	j = 0;
	found = 0;
	while (i < slen) {

	    /* keep searching str until a potential match for pat is found */
	    while ( j != -1 && pat[j] != str[i])
		j = flink[j];

	    if (j == plen-1) 	/* found pat in str */
	    {
		found = 1;
		break;
	    }
	    else {		/* else check that rest of pat matches */
		i += 1;
		j += 1;
	    }
	}
	if (!found || i == slen) return(-1);	/* didn't find pat in str */
    }

    /** at this point, found pattern in string **/
    if (getstart)
    {
	return(i-plen+1);
    }
    else 
    {
	return(i+1);
    }
} /* scanstr */


/*
**	GMATCH: checks for string matches while grabbing all instances
**		of the string delimited by PAT_GLOB.
**
*/
int
gmatch(register char *s1, int l1,register char *s2, int l2)
{
	char	*start,*end,*pat,*c,*temps2;
	int	slen=0,elen=0,plen=0;
	int	index,stindex,endex;
	int	retval,templ2,smlen,first;

	smlen = 0;
#ifdef xOTR1
		if (tTf(82,7)) {
			printf("GMATCH: s1 = %s\n", s1);
			printf("GMATCH: l1 = %d\n", l1);
			printf("GMATCH: s2= %s\n", s2);
			printf("GMATCH: l2 = %d\n", l2);
		}
#endif
	c = s2;
	for (c += l2; l2; l2--)
		if(*--c != ' ')
			break;
	c = s1;
	for (c += l1; l1; l1--)
		if(*--c != ' ')
			break;

	if (*s1 == PAT_SPEC) {
	    s1 += 2;
	    l1 -= 2;
	} else if (*s1 == PAT_ANY) {
	    s1++;
	    l1--;
	}
	c = (start = need(De.ov_ovqpbuf, l1));
	while (l1-- && PAT_GLOB != *s1++) {
	    *c++ = *(s1-1);
	    slen++;
	}
	c = (pat = need(De.ov_ovqpbuf, l1));
	while ( l1-- && *s1++ != PAT_GLOB) {
	    *c++ = *(s1-1);
	    plen++;
	}
	end = s1;
	elen = l1;

	if (slen != elen && (!slen || !elen)) {
	    return(-1);
	}

	Globs = NULL;
	if (!slen) {
	    index = scanstr(s2,l2,pat,plen,1,1);
	    if (index == -1) 
	    {
		return(-1);
	    }
	    add_glob(s2,index);
	    for (;;) {			/* this loop ends when index is -1 */
		s2 += index + plen;
		l2 -= index + plen;
		index = scanstr(s2, l2,pat,plen,1,1);
		if (index == -1) {	/* since string is finite, guaranteed to happen */
		    add_glob(s2,l2);
		    Globfoot->next = NULL;
		    return(0);
		}
		add_glob(s2,index);
	    }
	}
	else {
	    retval = 1;
	    first = 0;
	    temps2 = s2;
	    templ2 = 0;
	    for(;;) {
		if (first) {
		    s2 += smlen + elen;
		    l2 -= smlen + elen;
		    templ2 += smlen + elen;
		}
		else
		    first = 1;
		if ((stindex=scanstr(s2,l2,start,slen,1,1)) == -1 ||
		    (endex = scanstr(s2+stindex+slen,l2-stindex-slen,end,elen,1,1)) == -1)
		    {
			if (!retval) {
			    templ2 += l2;
			    add_glob(temps2,templ2);
			}
			return(retval);
		    }
		s2 += stindex + slen;
		l2 -= stindex + slen;
		templ2 += stindex + slen;
		smlen = endex;
		for (;(index = scanstr(s2,smlen,pat,plen,1,1)) != -1;) {
		    retval = 0;
		    templ2 += index;
		    add_glob(temps2,templ2);
		    temps2 += templ2 + plen;
		    templ2 = 0;
		    s2 += index + plen;
		    l2 -= index + plen;
		    smlen -= index + plen;
		}
	    }
	}
		    
}

void
add_glob(char *str, int slen)
{
#ifdef xOTR1
		if (tTf(82,8))
			printf("ADD_GLOB: str = %s, slen = %d\n", str, slen);
#endif
	    if (Globs == NULL) {
		Globs = Globfoot = xalloc(sizeof(GLIST), 0, 1);
		Globs->string = xalloc(slen, 0, 1);
		bmove(str,Globs->string,slen);
		Globlen = Globs->len = slen;
		Globnum = 1;
	    } else {
		Globfoot->next = xalloc(sizeof(GLIST), 0, 1);
		Globfoot = Globfoot->next;
		Globfoot->string = xalloc(slen, 0, 1);
		bmove(str,Globfoot->string,slen);
		Globlen += (Globfoot->len = slen);
		Globnum++;
	    }

}

/*
**
**	INSERT_CHARS replaces all [PAT_SPEC, index] pairs with strings from 
**	the Pats[] array.  The PAT_SPEC index corresponds to the index into
**	the Pats[] array.
**
**	Calls:	bmove
**
**	Called by:  interpret
**
**	Returns:  none	
*/
void
insert_chars(sym_t *op)
{
    char 	*st, *s,	/* pointers to sym_t string */
		*new;		/* pointer to new string being formed */
    int 	l,		/* length of sym_t string */
		size = 0;	/* size of new string being formed */
    int		tot,		/* total size of new string being formed */
		index,		/* PAT_SPEC index */
		flag=0;

#ifdef xOTR1
		if (tTf(82,10))
			printf("INSERT_CHARS: starting...\n");
#endif
    l = op->len & I1MASK;
    st = s = op->value.sym_data.cptype; 
    while (*(s+l-1) == ' ')
	l--;				/* don't worry about blanks */
    tot = l;
    while (l--) {
	if (*st == PAT_GLOB) {
		insert_glob(&s,++st,&tot,l);
		break;
	}
	if (*st++ == PAT_SPEC) {
		index = *st++ - '0';
		l--;

		/* subtract 2 for PAT_SPEC and corresponding index */
		tot += Pats[index].len - 2;

		new = need(De.ov_ovqpbuf, tot);
		if (size)
		    bmove(s,new,size);

		/* append the Pats[] string to the currently forming string */
		bmove(Pats[index].string,new+size,Pats[index].len);

		if (!flag)
		    flag = 1;
		else
		    xfree(s);
		s = new;  
		size += Pats[index].len;
		if (l) {
		    bmove(st,new+size,l);
		    st = new + size;
		}	
	}
	else 
	    size++;
    } /* while */

    /*
    **  replace sym_t string with
    **  new string and length
    */
    op->value.sym_data.cptype = s;
    op->len = tot;
}

void
insert_glob(char **start, char *rest, int *slen, int rlen)
{ 
	char	*pat = rest,*new;
	int	plen = 0,newlen,i;
	GLIST	*g;	

	while (rlen-- && *rest++ != PAT_GLOB)
	    plen++;
	/* put in error checking about 2nd PAT_GLOB */
	*slen -= plen + 2 + rlen;
	newlen = *slen + rlen + Globlen + (Globnum-1)*plen;
	new = need(De.ov_ovqpbuf, newlen);
	bmove(*start,new,*slen);
	*start = new;
	new += *slen;
	for (i = Globnum,g=Globs;i>1;i--,g=g->next) {
		bmove(g->string,new,g->len);
		new += g->len;
		bmove(pat,new,plen);
		new += plen;
	}
	bmove(g->string,new,g->len);
	new += g->len;
	bmove(rest,new,rlen);
	*slen = newlen;
}

void
newstring(register sym_t *op1, register sym_t *op2)
{
	int	stsize,psize,index,index2;

	psize = op2->len & I1MASK;	/* ignore trailing blanks */
	stsize = op1->len & I1MASK;
	if (op2->start != -1) {
		index = op2->start;
	}
	else
		index = scanstr(op1->value.sym_data.cptype,stsize,
				op2->value.sym_data.cptype,psize,
				CLOSED,(char) 1);	/* get start of string */	
	if (index != -1) {
		index2 = index + psize;
		bmove(op1->value.sym_data.cptype + index2,
		      op1->value.sym_data.cptype + index, stsize - index2);
		for (index += stsize - index2; index < stsize; index++)
		      *(op1->value.sym_data.cptype + index) = ' ';
	}
}

	    
void
createlink(char *pat, int plen)
{
    int i,j;

    flink[0] = -1;
    i = 1;
    while (i < plen) 
    {
	j = flink[i-1];
	while (j != -1 && pat[j] != pat[i-1])
	    j = flink[j];
	flink[i] = j + 1;
	i += 1;
    }
}

void
backlink(char *pat, int plen)
{
    int i,j;

    flink[plen - 1] = plen;
    i = plen - 2;
    while (i >= 0) 
    {
	j = flink[i+1];
	while (j != plen && pat[j] != pat[i+1])
	    j = flink[j];
	flink[i] = j - 1;
	i -= 1;
    }
}


/*
**	BACKSCAN
**
**	Searches backwards through string for pattern.
**
**	Returns:
**		-1 -- if pattern not found
**		index in string where pattern starts -- if getstart is true
**		index in string right after pattern ends -- if getstart is false
*/
int
backscan(char *str, int slen, char *pat, int plen, int getstart, char num)
/* str - string being scanned 			*/
/* pat - pattern being searched for 			*/
/* slen - length of string				*/
/* plen - length of pattern				*/
/* getstart - if true, return pointer which includes pat	*/
		/* if false, return pointer following pat	*/
/* num - which occurance of pat in string		*/
{
    int i,		/* index into string		   */
	j,		/* index into pat and flink	   */
	k,		/* number of occurance found	   */
	found;		/* true if pattern found in string */

#ifdef xOTR1
	if (tTf(82,12)) {
		printf("BACKSCAN: \n");
		printf("str = %s, len = %d\n", str, slen);
		printf("pat = %s, len = %d\n", pat, plen);
	}
#endif
    backlink(pat,plen);		/* set up flink for backwards scanning */
    i = slen ;

    /* for each occurance of pat in string */
    for (k = 0; k < (num & I1MASK); k++) {
	i -= 1;
	j = plen - 1;
	found = 0;

	/* search for pat from end of string until whole string is examined */
	while (i >= 0) {
	    while ( j != plen && pat[j] != str[i])
		j = flink[j];
	    if (j == 0)  {
		found = 1;
		break;
	    }	
	    else {
		i -= 1;
		j -= 1;
	    }
	}
	if (!found || i < 0) return(-1);
    }
    /* return pointers to pattern in string */
    if (getstart)
    {
	return(i);
    }
    else 
    {
	return(i+plen);
    }
} /* backscan */

int
getend(int len, int dropend, int howmany)
{
	int i;

	for (i = 0;i < (howmany & I1MASK); i++)
	    len--;
	if (dropend)
	    len--;
	return(len);
}

/*
**	SPECDELIM -- scan a string for a pattern specified by a special
**		delimiter 
**
**		Parameters:
**			str - string to be scanned
**			slen - length of string
**			dname - name of delimitor
**			getstart - type of interval
**			num - occurrence of pattern to look for
**
**		Returns:
**			index into string of pattern
**			-1 if pattern not found
**			-2 if delimitor was never defined
**
**		Called by:
**			grabstring
**
*/
int
specdelim(char *str, int slen, char *dname, int getstart, char num, int *plen)
{
	extern DELIMLIST	*Delimhead;	/* ptr to queue of delims */
	DELIMLIST		*d;
	DMAP			*map;		/* ptr to bitmap */
	char			patch;
	int			start = -1;	/* index to start of pattern */
	int			match;		/* true while a pattern is matching */
	char			*savestr;
	int			savelen;
	int			k;
	int			i;

	map = (DMAP *) NULL;
#ifdef xOTR1
	if (tTf(82,2)) {
		printf("SPECDELIM: starting...\n");
		printf("str = %s\n",str);
		printf("slen = %d\n",slen);
		printf("delim = %s\n",dname);
	}
#endif

	savestr = str;
	savelen = slen;
	*plen = 0;
	/* find correct delimiter in the queue */
	for (d = Delimhead; d != NULL && strcmp(d->delim,dname);d = d->back)
		continue;

	if (d == NULL) {
		ov_err(BADDELIM);
	}

	for (k = 0; k < (num & I1MASK); k++) {
		if (k) {
			start = start - 1 + *plen;
		/* 	savestr = &savestr[start]; */

			for ( i = 0; i < *plen - 1; i++) {
				savestr++;
				savelen--;
			}

		}
		while (savelen > 0) {
			map = d->maptr;
			start++;
			*plen = 0;
			str = savestr;
			slen = savelen;
			savestr++;
			savelen--;
			patch = *str++;
			match = TRUE;
	
			while ((map != NULL) && (slen >= 0) && (match)) {
				switch (map->type) {
				case RE_ONE:
					if (test(map->bits, patch)) {
						map = map->next;
						patch = *str++;
						slen--;
						(*plen)++;
					} else {
						match = FALSE;
					}
					break;
	
				case RE_ZEROMORE:
					while((slen >= 0) && (test(map->bits,patch))) {
						patch = *str++;
						slen--;
						(*plen)++;
					}
					map = map->next;
					break;
				}
			}
	
			if ((map == NULL)) {
				/* pattern was found */
				break;
			}
		}
		if ((slen <= 1) && (map != NULL))
			return(-1);
	}
	return(start);
}

/*
**	GRABSTRING grabs a string described by a pattern matching 
**	interval in a query.
**
**	Called by:  getsymbol
**
**	Calls:	scanstr, backscan, getend, specdelim
**
**	Returns:
**		NULL -- if pattern was not found in string
**		ptr to pattern which matches interval -- otherwise
*/
char *
grabstring(STRKEEPER *strinfo, char *str, int *len, int *startptr)
/* strinfo - info about delimitors */
/* str - string to search */
/* len - length of string */
{
	int 	start=0,end=0;		/* start and end of substring */
	char	*s;
	char 	leftint, rightint;	/* type of interval */
	char 	leftnum, rightnum;	/* number of occurrence to find */
	char 	leftspec, rightspec;	/* special chars 1= special delim */
					/* 2 = search backwards */
	char	*leftpat, *rightpat;	/* left and right patterns */
	int	stsearch;		/* where to start searching 2nd time */
	int	leftlen, rightlen;	/* lengths of patterns returned from specdelim*/

	/* initialization */
	leftint = strinfo->type[0];
	rightint = strinfo->type[1];
	leftnum = strinfo->number[0];
	rightnum = strinfo->number[1];
	leftspec = strinfo->flag[0];
	rightspec = strinfo->flag[1];
	leftpat = strinfo->string[0];
	rightpat = strinfo->string[1];

	*len &= I1MASK;			/* only look at lower byte */

	while (*(str+*len-1) == ' ')	/* find last nonblank char of string */
	    *len -= 1;

#ifdef xOTR1
	if (tTf(82,1)) {
		printf("GRABSTRING:\n");
		printf("str = %s, len = %d\n", str, *len);
		printf("leftint = %d, leftnum = %d, leftspec = %d\n", leftint, leftnum, leftspec);
		printf("left pattern = %s, len = %d\n", leftpat, strlen(leftpat));
		if (rightpat) {
		  printf("rightint = %d, rightnum = %d, rightspec = %d\n", rightint, rightnum, rightspec);
		  printf("right pattern = %s, len = %d\n", rightpat, strlen(rightpat));
		}
	}
#endif

	/* search for left endpoint */

	/* CASE 1: special chars */
	if (leftspec & 1) {
	    	start = specdelim(str,*len,leftpat,leftint,leftnum,&leftlen);
		if (leftint == CLOSED)
			stsearch = start + leftlen;
		else {
			start += leftlen;
			stsearch = start;
		}
	}
	/* CASE 2: backwards searching */
	else if (leftspec & 2) {
	    	if (leftpat == NULL)
			start = 1 + getend(*len,leftint,leftnum);
	    	else
			start = backscan(str , *len, leftpat, 
					 strlen(leftpat),leftint,leftnum);
		if (leftint == CLOSED)
			stsearch = start + strlen(leftpat);
		else
			stsearch = start;
	} else {
	/* CASE 3: forwards searching */
		    start = scanstr(str + start, *len, leftpat, 
				    strlen(leftpat),leftint,leftnum);
		if (leftint == CLOSED)
			stsearch = start + strlen(leftpat);
		else
			stsearch = start;
	}



	if (start == -1)		/* if pattern was not found in str */ {
		return(NULL);
	}

	/* search for right endpoint */

	/* CASE 1: special chars */
	else if (rightspec & 1) {
		if ((end = specdelim(str + stsearch,*len - stsearch,
					  rightpat, 1 - rightint, rightnum,
					  &rightlen)) == -1)
			return(NULL);
		else {
			if (rightint == CLOSED)
				end += stsearch + rightlen;
			else
				end += stsearch;
		}
	}
	/* Backwards searching */
	else if (rightspec & 2) {
	  if (rightpat == NULL)
	    end = *len;
	  else
	    end = backscan(str, *len, rightpat, strlen(rightpat),1 - rightint,
			   rightnum);
	}
	/* Forwards searching */
	else {
	  if (rightpat == NULL)
	    end = stsearch;
	  else
	    if ((end = scanstr(str + stsearch, *len,rightpat, 
			       strlen(rightpat),1 - rightint,
			       rightnum)) == -1)
	      return(NULL);
	    else
	      {
		end += stsearch;
	      }
	}

	
	if (end == -1 || end - start <= 0)	/* if end of interval couldn't 
						** be found or end did not come 
						** after start */ {
		return(NULL);
	} else {	
	    *len = end - start;
	    s = need(De.ov_ovqpbuf, *len);
	    bmove (str + start, s, *len);
	    *startptr = start;
	}


	return(s);
} /* grabstring */
