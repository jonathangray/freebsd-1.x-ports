#include <stdio.h>

#include <ingres.h>
#include <aux.h>
#include <symbol.h>
#include <tree.h>
#include <catalog.h>
#include "../decomp/globs.h"
#include "strategy.h"
#include <btree.h>
#include "sccs.h"

#define INGRES_IUTIL
#include "protos.h"

SCCSID(@(#)key.c	8.4	3/20/85)



/*
**	Exactkey checks to see if the relation described
**	by "ap" can be used in a hashed scan.
**	All the key domains of the relation must
**	have simple clauses of equality associated
**	with them in the qualification.
**
**	Returns 0 if the relation can't be used.
**
**	Returns > 0 if it can.
*/
int
exactkey(acc_param_t *ap, key_t *key)
{
	register acc_param_t	*a;
	register key_t		*k;
	register simp_t		*s;
	int				d, i, j;

#ifdef xOTR1
	if (tTf(85, -1))
		printf("Exactkey\n");
#endif

	a = ap;
	k = key;
	i = 0;
	if (a->mode == EXACTKEY) {

		for (i = 0; (d = a->keydno[i]) != 0 ; i++) {
	
			s = De.ov_simp;
			for (j = 0; j < De.ov_nsimp; j++) {
				if (s->relop == opEQ && s->att == d) {
					k->keysym = s->constv;
					k->dnumber = (a->sec_index == TRUE) ? i+1 : d;
					k++;
#ifdef xOTR1
					if (tTf(85, 1)) {
						printf("exact key on dom %d\tvalue=", d);
						prsym(s->constv);
					}
#endif
					break;
				}
				s++;
			}
			if (j == De.ov_nsimp) {
				i = 0;	/* failure. at lease one key isn't used */
				break;
			}
		}
		k->dnumber = 0;	/* mark end of list */
	}
#ifdef xOTR1
	if (tTf(85, 9))
		printf("exactkey returning %d\n", i);
#endif
	return (i);
}

/*
**	Attempts to use the B-Tree for retrieval.
**	There are two types of searches possible, exact and range searches.
**	In order for an exact search to be possible, there must be a simple
**	equality clause using the lid field.
**	For a range search, either or both lid ranges must be provided.
**
**	Returns		1  exact BTREEKEY search possible
**		       -1  low lid key provided
**		       -2  high lid key provided
**		       -3  both lids provided
*/
int
btreekey(key_t *lkey, key_t *hkey)
{
	register key_t	*l, *h;
	register simp_t	*s;
	int			i, j, k;
	int			provided[MAXLID];
	sym_t			*(save[MAXLID]);

#ifdef xOTR1
	if (tTf(85, -1))
		printf("Btreekey\n");
#endif

	l = lkey;
	h = hkey;
	i = 0;
	for (j = 0; j < MAXLID; ++j)
		provided[j] = 0;
	if (De.ov_scanr->d_r.r_dim > 0 && De.ov_scanr->d_r.r_tupc > 0) {
		s = De.ov_simp;
		for (j = 0; j < De.ov_nsimp; ++j) {
			if (s->att >= De.ov_scanr->d_r.r_attrc - De.ov_scanr->d_r.r_dim + 1) {
				if (s->relop == opEQ || s->relop == opGTGE) {
					l->keysym = s->constv;
					l->dnumber = s->att;
					if (s->relop == opEQ) {
						provided[De.ov_scanr->d_r.r_attrc - s->att] = 1;
						save[De.ov_scanr->d_r.r_attrc - s->att] = l->keysym;

					}
					else if (i == -2)
						i = -3;
					else if (!i)
						i = -1;
					++l;
				}
				if (s->relop == opLTLE) {
					h->keysym = s->constv;
					h->dnumber = s->att;
					h++;
					if (i == -1)
						i = -3;
					else if (!i)
						i = -2;
				}
			}
			s++;
			for (k = 0; k < De.ov_scanr->d_r.r_dim; ++k)
				if (!provided[k])
					break;
			if (k >= De.ov_scanr->d_r.r_dim) {
				i = 1;
				break;
			}
		}
		if (i != 1)
			for (k = 0; k < De.ov_scanr->d_r.r_dim; ++k)
				if (provided[k]) {
					h->keysym = save[k];
					h->dnumber = De.ov_scanr->d_r.r_attrc - k;
					h++;
					i = -3;
				}
		/* mark ends of lists */
		l->dnumber = 0;
		h->dnumber = 0;
	}
#ifdef xOTR1
	if (tTf(85, 9))
		printf("btreekey returning %d\n", i);
#endif
	return(i);
}

/*
**	Range key checks if the relation described by
**	"ap" is ISAM and there are simple clauses
**	on the first key and any additional keys.
**
**	Rangekey accumulates both high and low keys,
**	which are not necessary the same. If it
**	every finds a high or a low key on the first
**	domain of the relation then success=TRUE.
**
**	Returns  1 if Rangekey ok
**		 0 if Rangekey is not ok
**		-1 if Rangekey ok and all clauses are equality clauses
*/
int
rangekey(acc_param_t *ap, key_t *l, key_t *h)
{
	register key_t	*low, *high;
	register simp_t	*s;
	acc_param_t	*a;
	int			sec_indx, d, i;
	int			rel, success, ns, lowkey, allexact;

#ifdef xOTR1
	if (tTf(85, 5))
		printf("Rangekey\n");
#endif

	a = ap;
	sec_indx  = a->sec_index == TRUE;
	low = l;
	high = h;
	allexact = -1;	/* assume all clauses equality clauses */
	s = De.ov_simp;
	success = FALSE;
	if (a->mode == LRANGEKEY) {

		for (ns = 0; ns < De.ov_nsimp; ns++) {
			rel = s->relop;
			for (i = 0; (d = a->keydno[i]) != 0 ; i++) {
				if (d == s->att) {
					/* this is either a high range value or low range value */
					lowkey = (rel == opGTGE);
					if (lowkey || rel == opEQ) {
						/* low range key */
#ifdef xOTR1
						if (tTf(85, 6))
							printf("low key on dom %d\t", d);
#endif
						low->keysym = s->constv;
						low->dnumber = sec_indx ? i+1 : d;
						low++;
					}
					if (!lowkey || rel == opEQ) {
						/* high range key */
#ifdef xOTR1
						if  (tTf(85, 6))
							printf("high key on dom %d\t", d);
#endif
						high->keysym = s->constv;
						high->dnumber = sec_indx ? i+1 : d;
						high++;
					}
#ifdef xOTR1
					if (tTf(85, 6))
						prsym(s->constv);
#endif
					if (i == 0)
						success = TRUE;
					if (rel != opEQ)
						allexact = 1;	/* at least one inequality */
					break;
				}
			}
			s++;	/* try next simple clause */
		}
	}

	high->dnumber = 0;	/* mark end of list */
	low->dnumber = 0;	/* mask end of list */

	/* if success then return whether all clauses were equality */
	if (success)
		success = allexact;

#ifdef xOTR1
	if (tTf(85, 5))
		printf("rangekey returning %d\n", success);
#endif
	return (success);
}

/*
**	Setallkey takes a key struct, decodes it and
**	calls setkey with each value.
**
**	Called from strategy().
**
**	returns 0 if ok.
**	returns -1 in the special case of a deblanked hashkey
**	being bigger than the corresponding domain.
*/
int
setallkey(key_t *relkey, char *keytuple)
{
	register key_t	*k;
	register sym_t		*sk;
	register int		dnum;
	sym_t		**s;
	char			*p, temp[256];
	int			l;

	clearkeys(De.ov_scanr);
	k = relkey;
	while ((dnum = k->dnumber) != 0) {
		s = &k->keysym;
		sk = (sym_t *) De.ov_stack;
		getsymbol(sk, &s);	/* copy symbol to stack. caution:getsym changes the value of s. */
		rcvt(sk, De.ov_scanr->d_fmt[dnum], De.ov_scanr->d_len[dnum]);	/* convert key to correct type */
		p = (char *)&sk->value;

		if (sk->type == CHAR_CONST) {
			/*
			** The length of a character key must
			** be made equal to the domain length.
			** The key is copied to a temp place
			** and a null byte is inserted at the
			** end. In addition, if the key without
			** blanks is longer than the domain and
			** this is an exactkey, then the query
			** is false.
			*/
			p = temp;
			l = cmove(sk, p);	/* copy symbol to temp removing blanks & nulls */
#ifdef xOTR1
			if (tTf(86, 9))
				printf("length is %d\n", l);
#endif
			if (De.ov_fmode == EXACTKEY && l > (De.ov_scanr->d_len[dnum] & I1MASK))
				/* key too large. qualification is false */
				return (-1);
		}
		ingres_setkey(De.ov_scanr, keytuple, p, dnum);	/* set the key */
		k++;
	}
#ifdef xOTR1
	if (tTf(86, 8))
		printup(De.ov_scanr, keytuple);
#endif
	return (0);
}

/*
**	Cmove copies a char symbol into "dest".
**	It stops when the length is reached or
**	when a null byte is found.
**
**	returns the number of non-blank chars
**	in the string.
*/
int
cmove(sym_t *sym, char *dest)
{
	register char	*d, *s;
	register int	l;
	int		blank;

	s = sym->value.sym_data.cptype;	/* s points to the char string */
	d = dest;
	blank = 0;

	for (l = (sym->len & I1MASK); l--; s++) {
		*d++ = *s;
		if (*s == ' ')
			blank++;
		if (*s == '\0') {
			d--;
			break;
		}
	}

	*d = '\0';
	return ((d - dest) - blank);	/* return length of string */
}

/*
**	Indexcheck is called by scan() to check whether
**	a secondary index tuple satisfies the simple
**	clauses under which it was scanned.
**
**	Returns 1 if the tuple is ok,
**		0 otherwise.
*/
int
indexcheck(void)
{
	register int	i;

	if (De.ov_fmode == EXACTKEY)
		i = keycheck(De.ov_lkey_struct, De.ov_keyl, 0);	/* check for equality */
	else {
		i = keycheck(De.ov_lkey_struct, De.ov_keyl, 1);	/* check for >= */
		/* If the lowkey passed, check the highkey also */
		if (i)
			i = keycheck(De.ov_hkey_struct, De.ov_keyh, -1);	/* check for <= */
	}
#ifdef xOTR1
	if (tTf(86, 10))
		printf("indexcheck ret %d\n", i);
#endif
	return (i);
}

/*
**	Keycheck compares De.ov_intup with keytuple
**	according to the domains specified in the
**	"keys" struct.
**
**	mode is either >0, =0, <0 depending on
**	whether check is for De.ov_intup >= keytuple,
**	De.ov_intup == keytuple, De.ov_intup <= keytuple respectively
**
**	returns TRUE or FALSE accordingly.
*/
int
keycheck(key_t *keys, char *keytuple, int mode)
{
	register key_t	*k;
	register char		*kp;
	register int		dnum;
	int			offset, i, success;

	kp = keytuple;
	success = TRUE;

	for (k = keys; (dnum = k->dnumber) != 0 ; k++) {

		offset = De.ov_scanr->d_off[dnum];
		if ((i = icompare(&De.ov_intup[offset], &kp[offset], De.ov_scanr->d_fmt[dnum], De.ov_scanr->d_len[dnum] & I1MASK)) != 0) {
			if ((i < 0 && mode < 0) || (i > 0 && mode > 0))
				continue;
			success = FALSE;
			break;
		}
	}
	return (success);
}
