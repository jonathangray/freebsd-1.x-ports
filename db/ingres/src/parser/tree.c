#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <ingres.h>
#include <aux.h>
#include <tree.h>
#include <pv.h>
#include "parser.h"
#include <symbol.h>
#include "sccs.h"
#include <errors.h>

#define INGRES_IUTIL
#define INGRES_GUTIL
#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)tree.c	8.4	5/30/88)
/*
** GETCOP
**	routine to lookup 'string' in constant operators table
**	constant table is declared in tables.y
**	structure is defined in ../parser.h
*/
int
getcop(char *string)
{
	register struct constop	*cpt;
	register char		*sptr;
	extern struct constop	Coptab[];

	sptr = string;
	for (cpt = Coptab; cpt->copname; cpt++)
		if (strcmp(sptr, cpt->copname) == 0)
			return (cpt->copnum);
	return (BADCOP);
}



/*
** TREE
**  	FUNCTION TO ADD NODE TO QUERY TREE
**	RETURN VALUE IS POINTER TO NODE JUST CREATED		
*/
qtree_t *
par_tree(qtree_t *lptr, qtree_t *rptr, char typ, int len, register int valu, register att_ent_t *attnum)
{
	register qtree_t	*tptr;
	extern char	Trfrmt;
	extern char	Trfrml;
	extern int	Err_current;

#ifdef	xPTR3
	tTfp(55, 0, "tree type(%d), len(%d), value(%d).\n", typ, len, valu);
#endif

	if (Err_current)
		return (NULL);

	/* XXX - Following is a hack.  Sorry about that John.  */
	if (typ == AND)
		len = sizeof(rootnode_t) - sizeof(short);
	
	tptr = (qtree_t *) need(Qbuf, QT_HDR_SIZ + len);
	tptr->left = lptr;
	tptr->right = rptr;
	tptr->sym.type = typ;
	tptr->sym.len = len;

	switch (typ) {
	  case VAR:
		tptr->sym.value.sym_var.varno = valu & I1MASK;
		tptr->sym.value.sym_var.attno = attnum->atbid;
		tptr->sym.value.sym_var.varfrmt = attnum->atbfrmt;
		tptr->sym.value.sym_var.varfrml = attnum->atbfrml;
		tptr->sym.value.sym_var.valptr = NULL;
		tptr->sym.value.sym_var.varstr = NULL;
		break;

	  case ROOT:
	  case AGHEAD:
		tptr->sym.value.sym_root.rootuser = valu;
		break;

	  case TREE:
	  case BYHEAD:
	  case AND:
	  case OR:
	  case QLEND:
		break;

	  case UOP:
	  case BOP:
		tptr->sym.value.sym_op.opno = valu;
		format(tptr);
		break;

	  case COP:
		if ((tptr->sym.value.sym_op.opno = getcop(valu)) == BADCOP) {
			/* bad const operator */
			par_error(BADCONSTOP, WARN, valu, 0, 0);
			return(NULL);
		}
		break;

	  case AOP:
		format(tptr->right);
		tptr->sym.value.sym_op.agfrmt = Trfrmt;
		tptr->sym.value.sym_op.agfrml = Trfrml;

	  case RESDOM:
		tptr->sym.value.sym_resdom.resno = valu;
		format(tptr);
		tptr->sym.value.sym_resdom.resfrmt = Trfrmt;
		tptr->sym.value.sym_resdom.resfrml = Trfrml;
		break;

	  default:
		/* INT_CONST, FLOAT_CONST, CHAR_CONST */
		bmove(valu, &tptr->sym.value, len & I1MASK);
		break;
	}
	return (tptr);
}

/*
** WINDUP
**	assign resno's to resdoms of an agg fcn
*/
void
windup(qtree_t *ptr)
{
	register qtree_t	*t;
	register int		tot;
	register int		kk;

	/* COUNT THE RESDOM'S OF THIS TARGET LIST */
	kk = 1;
	for (t = ptr; t; t = t->left) {
		kk++;
	}
	tot = 1;
	for (t=ptr; t;t = t->left) {
		t->sym.value.sym_resdom.resno = kk - tot++;
	}
}

/*
** ADDRESDOM - makes a new entry for the target list
**
**	Trname must contain the name of the resdom to
**	use for the header, create and Rsdmno for append, replace
**
**	the parameters are pointers to the subtrees to be
**	suspended from the node
*/
qtree_t *
addresdom(qtree_t *lptr, qtree_t *rptr)
{
	register qtree_t	*rtval;
	register att_ent_t		*aptr;
	char				buf[10];
					/* buffer type and len for dbu */
	extern int			Opflag;
	extern int			Rsdmno;
	extern int			Equel;
	extern int			Resrng;
	extern char			Trfrmt;
	extern char			Trfrml;
	extern char			*Trname;
	extern PARRNG			Parrng[];

	switch (Opflag) {
	  case mdSTOP:
		rtval = NULL;
		break;
	  case mdRETR:
	  case mdRET_UNI:
	  case mdVIEW:
		Rsdmno++;
		if (Rsdmno >= MAX_DOMAINS) {
			/* too many resdoms */
			par_error(RESXTRA, FATAL, 0, 0, 0);
		}
		rtval = par_tree(lptr, rptr, RESDOM, sizeof(resdomnode_t), Rsdmno, (att_ent_t *) NULL);
		if (!Equel || Resrng) {
			/* buffer info for header or CREATE */
			setp(PV_STR, Trname, 0);

			buf[0] = Trfrmt & I1MASK;
			smove(iocv(Trfrml & I1MASK), &buf[1]);

			setp(PV_STR, buf, 0);
		}
		break;

	  default:
		/*
		** for append and replace, the result domain
		** number is determined by the location of
		** the attribute in the result relation
		*/
		if (strcmp(Trname, "tid") == 0) {
			/* attrib not found */
			par_error(NOATTRIN, WARN, Trname,
			    trim_relname(Parrng[Resrng].vardesc.d_r.r_id), 0);
		}
#ifdef	DISTRIB
		if (strcmp(Trname, "sid") == 0) {
			/* attrib not found */
			par_error(NOATTRIN, WARN, Trname,
			    trim_relname(Parrng[Resrng].vardesc.d_r.r_id), 0);
		}
#endif
		aptr = attlookup(Resrng, Trname);
		Rsdmno = aptr->atbid;
		rtval = par_tree(lptr, rptr, RESDOM, sizeof(resdomnode_t), Rsdmno, aptr);
		if (Opflag != mdPROT) {
			/* INTEGRITY not possible here */
			attcheck(aptr);
		}
		break;
	}
	return (rtval);
}

/*
** SUBSTRING
**	creates structure to save delimiters of a substring
**	structure is defined in ../h/tree.h
*/
STRKEEPER *
substring(char *str,int isname)
{
	STRKEEPER	*s;

	s = (STRKEEPER *) need(Qbuf,sizeof(STRKEEPER));
	s->number[1] = s->number[0] = 1;
	s->string[1] = NULL;
	s->flag[0] = s->flag[1] = 0;
	s->string[0] = str;
	if (isname)
	    s->flag[0] = 1;
	if (str == NULL)
	    s->flag[0] |= 2;
	return(s);
}

STRKEEPER *
endvals(STRKEEPER *interval, int left, int right)
{
	if (left == '(')
		interval->type[0] = OPEN;
	else
		interval->type[0] = CLOSED;
	if (right == ')')
		interval->type[1] = OPEN;
	else
		interval->type[1] = CLOSED;
	return(interval);
}

void
setnumber(STRKEEPER *interval, short *num)
{
	interval->number[0] = *num;
}

void
groupstrings(STRKEEPER *left, STRKEEPER *right)
{
	left->string[1] = right->string[0];
	left->flag[1] = right->flag[0];
	left->number[1] = right->number[0];
}

/*
**	CHECK_BNF -- check the legality of a simplified BNF defnition
**
**		Parameters: 
**			str-- the string to be checked
**	
**		Returns:
**			0 - the string is legal
**			<0 - the string is not legal
**				-1 : bracket,brace not matched
**				-2 : hyphen misused
**
**		Called by:
**			make_tuples
**
**		Comments: 
**			the string may not contain nested braces or brackets
**			these chars have special meaning and must be 
**			backslashed: { } [ ] - \
**
*/
int
check_bnf(char *str)
{
	char	*temp;		/* temp ptr to string */
	int	len;		/* length of string */
	char	ch;		/* ptr to one char of string */
	char 	nextch;
	int	inbrace=0;	/* keeps track of braces */
	int 	inbrak=0;	/* keeps track of brackets */


	len = strlen(str);
	temp = str;

	while (len > 0) {
		len--;
		ch = *temp++;

		switch (ch) {
		    case RE_LBRACKET:
			if (!inbrace)
				inbrak++;
			else
				return(-1);
			break;
		    case RE_RBRACKET:
			inbrak--;
			if (inbrak != 0)
				return(-1);
			break;
		    case RE_LBRACE:
			if (!inbrak)
				inbrace++;
			else
				return(-1);
			break;
		    case RE_RBRACE:
			inbrace--;
			if (inbrace != 0)
				return(-1);
			break;
		    case '-':
			return(-2);
			break;
		    case '\\':
			temp++;
			break;
		    default:
			nextch = *temp;
			if (nextch == '-') {
				temp++;
				len--;
				if (!len)
					return(-2);
				ch = *temp;
				switch(ch) {
				    case RE_LBRACKET:
				    case RE_RBRACKET:
				    case RE_LBRACE:
				    case RE_RBRACE:
				    case '-':
					return(-2);
					break;
				    case '\\':
					temp++;
					break;
				    default:
					break;
				}
			}
		}
	}
	if ((inbrace) || (inbrak))
		return(-1);
	return(0);
}

/*
**	MAKE_TUPLES -- create the tuples for the 'rdelim' relation
**			as specified by a user-defined delimitor
**
**		Paramaters:
**			desc--descriptor for the relation
**			group--group name for the delimitor
**			delim--name of the delimitor
**			str-bnf string specifying the delimitor
**
**		Returns:
**			0 if successful
**			<0 if not successful
**			-1,-2: BNF expression not legal
**
*/
int
make_tuples(desc_t *desc, char *group, char *delim, char *str)
{
	int	err;		/* error status of bnf string */
	char	*map;		/* pointer to next string to make into bitmap */
	int	len;		/* len of str */
	int 	mlen;		/* len of substring to make into bitmap */
	int 	order;		/* order of bitmap */
	int 	type;		/* type of interval RE_ONE or RE_ZEROMORE */
	char	ch;		/* pointer to current char */

	err = check_bnf(str);
	if (err < 0)
		return(err);

	len = strlen(str);
	order = 0;

	while (len > 0) {
		order++;
		map = str;
		mlen = 0;

		ch = *str++;
		len--;

		switch (ch) {
		case RE_LBRACKET:
			    type = RE_ONE;
			    map = str;
			    while ((ch = *str++) != RE_RBRACKET) {
				mlen++;
				len--;
				if (ch == '\\') {
					ch = *str++;
					mlen++;
					len--;
				}
			    }
			    len--;
			    break;
			
		case RE_LBRACE:
			    type = RE_ZEROMORE;
			    map = str;
			    while ((ch = *str++) != RE_RBRACE)
			    {
				mlen++;
				len--;
				if (ch == '\\') {
					ch = *str++;
					mlen++;
					len--;
				}
			    }
			    len--;
			    break;

		default:
			    type = RE_ONE;
			    if (ch == '\\')
			    {
				map = str;
				ch = *str++;
				len--;
				mlen = 1;
			    }
			    if (*str == '-')
			    {
				str++;
				len--;
				mlen++;
				str++;
				len--;
				mlen++;
			    }
			    else
				mlen = 1;
			    break;
		}

		create_tup(desc,order,group,delim,type,map,mlen);
	}
	return(0);
}


/*
**	CREATE_TUP-- create a tuple in the 'rdelim' relation
**
**		Parameters:
**			desc - descriptor for the relation
**			order - order field for tuple
**			group - group field for tuple
**			delim - delim field for tuple
**			type - type field for tuple
**			str - string to be converted into bitmap
**			strlen - length of str
**
**		Called by:
**			make_tuples
*/
void
create_tup(desc_t *desc, int order, char *group, char *delim, int type, char *str, int strlen)
{
	DELIM_TUP	*tuple;
	tid_t		*tid;
	char		b[BITMAPLEN];
	int		i;


	tuple = xalloc(sizeof(DELIM_TUP), 0, 1);
	tuple->order = order;
	strcpy(tuple->group,group);
	strcpy(tuple->delim,delim);
	tuple->type = type;

	make_dmap(str,strlen,b);
	for ( i= 0; i< BITMAPLEN; i++)
		tuple->bitmap[i] = b[i];

	insert(desc,&tid,tuple,1);
}

/*
**	MAKE_DMAP -- given a BNF string, make the corresponding bitmap
**
**		Parameters:
**			str - BNF string
**		 	len - length of string
**		
**		Called by:
**			create_tup
**
**		Returns:
**			pointer to the bitmap of 16 chars
**
**		Comments:
**			The bitmap is formed of 16 chars. The total bits
**		(128) represents the characters of the ASCII set.
**		If the BNF string indicates a character, the bit
**		corresponding to that char is set in the bitmap.
**		All other bits are reset.
*/
char *
make_dmap(char *str, int len, char *b)
{
	char	ch;
	char	nextch;
	int	i;

#ifdef xPTR3
	tTfp(42,0,"DMAP: str = %s, len = %d\n",str,len);
#endif
	for (i = 0; i < ACHARS; i++)
		reset(b,i);

	while (len > 0) {
		ch = *str++;
		len--;
		if (ch == '\\') {
			ch = *str++;
			len--;
		}
		if ( (len > 0) && (*str == '-')) {
			str++;
			len--;
			nextch = *str++;
			len--;
			for (i = ch; i <= nextch; i++) {
				set(b,i);
			}
		} else {	
			set(b,ch);
		}
	}
	return(b);
}

/*
**	SET,RESET -- bitmap setting routines
**
**		Parameters:
**			map: the array of chars which forms the bitmap
**			n: the bit to set or reset
**	
**		Called by:
**			make_bitmap
**
*/
void
set(char *map, int n)
{
	map[n/BITS] |= (1<<(n%BITS));
}

void
reset(char *map, int n)
{
	map[n/BITS] &= ((1<<(n%BITS)) ^ MAX_FIELD_SIZE); 
}

int
test(char *map, int n)
{
	return ((map[n/BITS] & (1<<(n%BITS))) != 0);
}

/*
**	MAKE_LIST -- puts the delimitors to be used in the delim queue
**
**		Parameters:
**			desc - descriptor for the relation
**			group - group of delims to use
**
**		Returns:
**			0 if ok
**			-1 if no delims could be found in the specified group
**
**		Comments:
**			given a group name, adds all delimitors in that
**			group to the head of the delim queue, which is
**			pointed to by Delimhead.
**			if the queue is empty, the predefined delimitors
**			'w' and 'c' will be added to the list
*/
int
make_list(desc_t *desc, char *group)
{
	DELIM_TUP		tuple;
	tid_t			lotid, hitid;
	extern DELIMLIST	*Delimhead;
	DELIMLIST		*d;
	DMAP			*map, *m;
	char			delim[12];
	int			start = 1;
	int			i;
	int			notfound = 1;

	m = (DMAP *) NULL;
#ifdef xPTR3
	tTfp(42,0,"Make_list: group = %s\n", group);
#endif
	if (!strcmp (group,"system")) {
		predef_delims();
		return(0);
	}
	if (find(desc,LRANGEKEY, &lotid, &hitid, group) < 0)
		return(-1);
	find(desc,HRANGEKEY, &lotid, &hitid, group);
	while (!get(desc, &lotid, &hitid, &tuple, 1)) {
		if (strcmp(tuple.group, group)) {
			continue;
		}
		notfound = FALSE;
		/* check if it is a new delimitor */
		if (strcmp(tuple.delim, delim))
			start = 1;

		/* start a new delimitor node */
		if (start) {
			d = xalloc(sizeof(DELIMLIST), 0, 1); 
			strcpy(delim, tuple.delim);
			strcpy(d->group,tuple.group);
			strcpy(d->delim,delim);
			d->back = Delimhead;
			Delimhead = d;

			map = xalloc(sizeof(DMAP), 0, 1);
			map->order = tuple.order;
			map->type = tuple.type;
			for ( i = 0; i < BITMAPLEN; i++)
				map->bits[i] = tuple.bitmap[i];
			map->next = NULL;
			d->maptr = map;
			m = map;
			start = 0;
		} else {
			/* add another bitmap to the delimitor node */
			map = xalloc(sizeof(DMAP), 0, 1);
			map->order = tuple.order;
			map->type = tuple.type;
			for ( i = 0; i < BITMAPLEN; i++)
				map->bits[i] = tuple.bitmap[i];
			map->next = NULL;
			m->next = map;
			m = m->next;

		}

	}
	/*prlist(Delimhead); */
	if (notfound)
		return(-1);
	return(0);
}

/*
**	PREDEF_DELIMS - add the predefined delims to the queue
**
**		Called by:
**			make_list
**
**		Side Effects:
**			the delim queue pointed to by Delimhead
**			is initialized with the delims 'w' and 'c'.
**
*/
void
predef_delims(void)
{
	extern DELIMLIST	*Delimhead;
	DELIMLIST	*d;
	DMAP		*m, *m2;
	int		i;

	d = xalloc(sizeof(DELIMLIST), 0, 1);
	strcpy(d->group, "system");
	strcpy(d->delim, "c");
	d->back = NULL;

	m = xalloc(sizeof(DMAP), 0, 1);
	m->order = 1;
	m->type = RE_ONE;
	bzero(m->bits, sizeof(m->bits));
	for (i = ' '; i <= '~'; i++)
		set(m->bits, i);
	m->next = NULL;
	d->maptr = m;
	Delimhead = d;


	d = xalloc(sizeof(DELIMLIST), 0, 1);
	strcpy(d->group, "system");
	strcpy(d->delim, "w");
	d->back = NULL;
	m = xalloc(sizeof(DMAP), 0, 1);
	m->order = 1;
	m->type = RE_ONE;
	bzero(m->bits, sizeof(m->bits));
	for (i = 'A'; i <= 'Z'; i++)
		set(m->bits, i);
	for (i = 'a'; i <= 'z'; i++)
		set(m->bits, i);
	d->maptr = m;

	m2 = xalloc(sizeof(DMAP), 0, 1);
	m2->order = 2;
	m2->type = RE_ZEROMORE;
	bzero(m2->bits, sizeof(m->bits));
	for (i = 'A'; i <= 'Z'; i++)
		set(m2->bits, i);
	for (i = 'a'; i <= 'z'; i++)
		set(m2->bits, i);
	m->next = m2;
	m2->next = NULL;

	d->back = Delimhead;
	Delimhead = d;
}

/*
**	PRLIST -- print contents of delimiter queue
*/
int
prlist(struct delimlist	*d)
{
	struct delimlist	*q;
	DMAP			*m;
	int			i;

	printf("DELIM QUEUE:\n");
	q = d;
	while (q != NULL) {
		printf("-------------------------------------------------------\n");
		printf("NODE:   group= %s,  delim = %s \n", q->group, q->delim);
		m = q->maptr;
		while (m != NULL) {
			printf("maps:\n");
			printf("order = %d, type = %d \n", m->order, m->type);
			for (i = 0; i < ACHARS; i++)
				printf("%d ", test(m->bits,i));
			printf("\n");
			m = m->next;
		}
		q = q->back;
		printf("-------------------------------------------------------\n");
	}

		return(0);

}

/*
**	SHRINK_LIST -- remove the delims in specified group from list
**
**		Parameters:
**			group - name of the group to remove
**
*/
int
shrink_list(char *group)
{
	struct delimlist		*p, *q;
	extern struct delimlist		*Delimhead;

	q = (struct delimlist *) NULL;
	/* may not delete sytem delims */
	if (!strcmp(group, "system"))
		return(-1);

	p = Delimhead;

	while ((p != NULL) && (strcmp(p->group, group))) {
		q = p;
		p = p->back;
	}
	if (p == NULL) {
		return(-1);	/* error group not found */
	}

	while(!strcmp(p->group, group)) {
		if (p == Delimhead)
			Delimhead = p->back;
		else
			q->back = p->back;
		
		if (p->back ==  NULL) {
			return(0);
		}

		p = p-> back;
	}

	/* prlist(Delimhead); */
	return(0);
}

/*
**	DESTROY_DELIM -- remove the group of delims from the relation 'rdelim'
**
**		Parameters:
**			group - the group of delims to remove
**
**		Called By:
**			grammar.y
**	
**		Returns:
**			0 if delims were successfully removed
**			-1 if delims were not found in relation
*/
int
destroy_delim(desc_t *desc, char *group)
{
	DELIM_TUP	tuple;
	tid_t		lotid,hitid;
	int		notfound = 1;

	if (find(desc,LRANGEKEY, &lotid, &hitid, group) < 0)
		return(-1);
	find(desc,HRANGEKEY, &lotid, &hitid, group);

	while (!get(desc, &lotid, &hitid, &tuple, 1)) {
		if (!strcmp(tuple.group, group)) {
			notfound = 0;
			delete(desc,&lotid);
		}
	}
	if (notfound) 
		return(-1);
	return(0);
}
