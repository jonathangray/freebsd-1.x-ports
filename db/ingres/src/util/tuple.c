#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <ingres.h>
#include <symbol.h>
#include <access.h>
#include <aux.h>
#include <catalog.h>
#include <btree.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)tuple.c	8.1	12/31/84)


/*
**	Clr_tuple initializes all character domains
**	to blank and all numeric domains to zero.
*/
void
clr_tuple(desc_t *desc, void *tuplearg)
{
	register desc_t	*d;
	register char	*tup;
	register int	i;
	int		j, pad;
	char		*tuple;

	tuple = (char *) tuplearg;
	d = desc;

	for (i = 1; i <= d->d_r.r_attrc; i++) {
		if (d->d_fmt[i] == CHAR_CONST) {
			pad = ' ';
		} else {
			pad = 0;
		}

		tup = &tuple[d->d_off[i]];
		j = d->d_len[i] & I1MASK;

		while (j--) {
			*tup++ = pad;
		}
	}
}

/*
**	Delete the specified tuple from the
**	current page.
**
**	The space occupied by the tuple is
**	compacted and the effected remaining
**	tuple line offsets are adjusted.
*/
void
del_tuple(tid_t *tid, int width)
{
	register char	*startpt, *midpt;
	register int	i;
	char		*endpt;
	int		cnt, offset, nextline;
	int		linenum;

	linenum = tid->line_id & I1MASK;
	offset = Acc_head->am_linev[-linenum];
	nextline = Acc_head->am_nextline;

	startpt = get_addr(tid);
	midpt = startpt + width;
	endpt = (char *)Acc_head + Acc_head->am_linev[-nextline];

	cnt = endpt - midpt;

	/* delete tuple */
	Acc_head->am_linev[-linenum] = 0;

	/* update affected line numbers */
	for (i = 0; i <= nextline; i++) {
		if (Acc_head->am_linev[-i] > offset)
			Acc_head->am_linev[-i] -= width;
	}

	/* compact the space */
	while (cnt--)
		*startpt++ = *midpt++;
	Acc_head->am_flags |= BUF_DIRTY;
}

/*
**  GET_RELTUP -- get appropriate tuple from relation catalog
**
**	Get the tuple for the relation specified by 'name'
**	and put it in the descriptor 'dx'.
**
**	First a relation named 'name' owned
**	by the current user is searched for. If that fails,
**	then a relation owned by the dba is searched for.
*/
int
get_reltup(desc_t *d, char *name)
{
	relation_t	rel;
	register int	i;

	clearkeys(&Admin.ad_rel);

	/* make believe relation relation is read only for concurrency */
	Admin.ad_rel.d_opened = abs(Admin.ad_rel.d_opened);

	/* relation relation is open. Search for relation 'name' */
	ingres_setkey(&Admin.ad_rel, (char *) &rel, name, RELID);
	ingres_setkey(&Admin.ad_rel, (char *) &rel, Usercode, RELOWNER);

	if ((i = getequal(&Admin.ad_rel, (char *) &rel, d, &d->d_tid)) == 1) {
		/* not a user relation. try relation owner by dba */
		ingres_setkey(&Admin.ad_rel, (char *) &rel,
					Admin.ad_h.adm_owner, RELOWNER);
		i = getequal(&Admin.ad_rel, (char *) &rel, d, &d->d_tid);
	}

	flush_rel(&Admin.ad_rel, TRUE);

#ifdef xATR1
	if (tTf(21, 1))
		printf("get_reltup: %d\n", i);
#endif

	/* restore relation relation to read/write mode */
	Admin.ad_rel.d_opened = -Admin.ad_rel.d_opened;
	return (i);
}

/*
**	Routine associated with getting a tuple out of
**	the current buffer.
*/

/*
**	Uncompress - decompress the tuple at address cp
**	according to descriptor.
*/
void
uncomp_tup(desc_t *d, char *cp, char *tuple)
{
	register char	*src, *dst, *start;
	int		i, j, numatts;

	dst = tuple;
	src = cp;
	start = dst - d->d_off[1];

	/* for each domain, copy and blank pad if char */
	/* ignore lid field */
	numatts = d->d_r.r_attrc - d->d_r.r_dim;
	for (i = 1; i <= numatts; i++) {
		j = d->d_len[i] & I1MASK;
		dst = start + d->d_off[i];
		if (d->d_fmt[i] == CHAR_CONST) {
			while (j--) {
				if ((*dst++ = *src++) == 0) {
					/* back up one */
					dst--;
					j++;
					break;
				}
			}

			/* blank pad if necessary */
			while (j-- > 0)
				*dst++ = ' ';
		} else {
			while (j--)
				*dst++ = *src++;
		}
	}
}

/*
**	Get_tuple - take the tuple specified
**	by tid and move it to "tuple"
*/
void
get_tuple(desc_t *d, tid_t *tid, char *tuple)
{
	register char	*cp;
	tid_t		tidloc;
	char		lid[MAXLID * LIDSIZE];
	extern desc_t	Btreesec;

	cp = get_addr(tid);

	if (M_COMPRESSED(d->d_r.r_spec)) {
		uncomp_tup(d, cp, tuple);	/* compressed tuple */
	} else {
		if (d->d_r.r_dim == 0)
			bmove(cp, tuple, d->d_r.r_width);	/* uncompressed tuple */
		else
			/* ignore lid field for now */
			bmove(cp, tuple, d->d_r.r_width - d->d_r.r_dim * LIDSIZE);
	}

	if (d->d_r.r_dim > 0) {
		/* find corresponding lid value given main relation tid */
		bmove(d->d_btree, &Btreesec, sizeof(Btreesec));
		setglobalint(BTREE_FD_NAME, d->d_btreefd);
		search_btree(*tid, &tidloc);
		get_lid(&tidloc, lid);
		/* attach lid value to end of tuple */
		cp = tuple + d->d_r.r_width - d->d_r.r_dim * LIDSIZE;
		bmove(lid, cp, d->d_r.r_dim * LIDSIZE);
	}
}

/*
**	Getint_tuple - get the tuple specified by
**	tid. If possible avoid moving the tuple.
**	Return value is address of tuple.
*/
char *
getint_tuple(desc_t *d, tid_t *tid, char *tuple)
{
	register char	*cp, *ret;
	tid_t		tidloc;
	char		lid[MAXLID * LIDSIZE];
	extern desc_t	Btreesec;

	cp = get_addr(tid);

	if (M_COMPRESSED(d->d_r.r_spec)) {
		ret = tuple;
		uncomp_tup(d, cp, ret);	/* compressed tuple */
	} else if (d->d_r.r_dim == 0) {
		ret = cp;			/* uncompressed tuple */
	} else {
		/* ignore lid field for now */
		ret = tuple;
		bmove(cp, tuple, d->d_r.r_width - d->d_r.r_dim * LIDSIZE);
	}
	if (d->d_r.r_dim > 0) {
		bmove(d->d_btree, &Btreesec, sizeof(Btreesec));
		setglobalint(BTREE_FD_NAME, d->d_btreefd);
		/* find corresponding lid value */
		search_btree(*tid, &tidloc);
		get_lid(&tidloc, lid);
		/* attach lid value to end of tuple */
		cp = ret + d->d_r.r_width - d->d_r.r_dim * LIDSIZE;
		bmove(lid, cp, d->d_r.r_dim * LIDSIZE);
	}
	return (ret);
}

/*
**	Routine to compute the address of a tuple
**	within the current buffer.
**	Syserr if specified tuple deleted.
*/
char *
get_addr(tid_t *tid)
{
	register int	offset;
	long		lvar[2];

	offset = Acc_head->am_linev[-(tid->line_id & I1MASK)];
	if (offset == 0) {
		(void) memcpy(&lvar[0], &Acc_head->am_rel, sizeof(lvar[0]));
		(void) memcpy(&lvar[1], &tid, sizeof(lvar[1]));
		syserr("get_addr rel=%ld tid=%ld", lvar[0], lvar[1]);
	}
#ifdef xATR3
	if (offset < 0 || offset > PGSIZE)
		syserr("get_addr: offset=%d\n", offset);
#endif
	return (((char *) Acc_head) + offset);
}

/*
**	Check if a line number is valid.
**	If linenumber is illegal return AMINVL_ERR
**	if Line has been deleted return DELTUP
**	else return 0
*/
int
invalid(tid_t *tid)
{
	register int	i;

	i = tid->line_id & I1MASK;

	if (i >= Acc_head->am_nextline)
		return (acc_err(AMINVL_ERR));

	if (Acc_head->am_linev[-i] == 0)
		return (DELTUP);

	return (0);
}

/*
**  PRINTUP -- print tuple
**
**	Parameters:
**		d -- a descriptor describing the tuple.
**		tuple -- the tuple to print.
**
**	Returns:
**		??
**
**	Side Effects:
**		None.
*/

#define	PAGELGTH	56

int	Printlgth;
extern struct out_arg	Out_arg;
int	Hdr = 0;
HDRINFO	*Fieldwidth, *Hdrptr;


/*
**  FORMATTED ATTRIBUTE PRINT
**
**	Attribute 'value' is printed.  It is type 'type' and has a
**	field width of 'width'.
*/
void
printfatt(int width, char *value)
{
	register char	*p;
	register int	w;
	register int	v;

	w = width;
	p = value;
	v = strlen(p);

	if (v > w) {
		/* field overflow */
		while (w--) {
			putc('*', stdout);
		}
	} else {
		/* output the field */
		for (w -= v; w > 0; w--) {
			putc(' ', stdout);
		}
		(void) fwrite(p, 1, v, stdout);
	}
}

int
printatt(char type, int length, void *val)
{
	char		buf[MAX_FIELD_SIZE];
	ANYTYPE		*value;
	double		dbl;
	extern HDRINFO  *Hdrptr;
	extern HDRINFO	*Fieldwidth;
	extern int	Hdr;
	register char	*p;

	value = (ANYTYPE *) val;
	putc(Out_arg.coldelim, stdout);
	switch (type) {
	case INT_CONST:
		switch (length) {
		case 1:
			printfatt(Out_arg.i1width, iocv(value->i1type));
			break;

		case 2:
			printfatt(Out_arg.i2width, iocv(value->i2type));
			break;

		case 4:
			printfatt(Out_arg.i4width, locv(value->i4type));
			break;

		default:
			syserr("printatt: i%d", length);
		}
		return (0);

	case FLOAT_CONST:
		switch (length) {
		case 4:
		        dbl = (double) value->f4type;

			ftoa(dbl, buf, Out_arg.f4width, Out_arg.f4prec,
							Out_arg.f4style);
			printfatt(Out_arg.f4width, buf);
			break;

		case 8:
			dbl = value->f8type;
			ftoa(dbl, (char *)buf, (int)Out_arg.f8width,
				(int) Out_arg.f8prec, (char)Out_arg.f8style);
			printfatt(Out_arg.f8width, buf);
			break;

		default:
			syserr("printatt: f%d", length);
		}
		return (0);

	case CHAR_CONST:
		length &= I1MASK;
		if (Hdr) {
			if (length > Fieldwidth->len) {
				length = Fieldwidth->len;
			}
		}
		(void) fwrite(value, 1, length, stdout);
		if (Hdr) {
			if ((length = Fieldwidth->len - length) > 0) {
				while (length--) {
					putc(' ', stdout);
				}
			}
			if ((length = Out_arg.c0width - Fieldwidth->len) > 0) {
				while (length--) {
					putc(' ', stdout);
				}
			}
			Fieldwidth = Fieldwidth->next;
			if (Fieldwidth == NULL) {
				Fieldwidth = Hdrptr;
			}
		} else {
			if ((length = Out_arg.c0width - length) > 0) {
				while (length--) {
					putc(' ', stdout);
				}
			}
		}
		return(0);

	  case BINARY:
		length &= I1MASK;
		p = (char *) value;
		while (length--) {
			xputchar(*p++);
		}
		return(0);

	  default:
		syserr("printatt type %d", type);
	}
	return(-1);
}

void
printeol(void)
{
	putc(Out_arg.coldelim, stdout);
	putc('\n', stdout);
}

void
printup(desc_t *d, void *tuple_arg)
{
	register short	*foff;
	register char	*ftype;
	register char	*flen;
	static double	dbl;
	long		lng;
	char		*tuple;
	int		i;
	int		type;

	tuple = (char *) tuple_arg;
	ftype = &d->d_fmt[1];
	flen = &d->d_len[1];
	foff = &d->d_off[1];
	i = d->d_r.r_attrc;

	/* If relation is S_BINARY then print char fields escaped */
	if (d->d_r.r_status & S_BINARY) {
		while (i--) {
			printatt((type = *ftype++) == CHAR_CONST ? BINARY : type,
					*flen++, (ANYTYPE *) &tuple[*foff++]);
		}
	} else {
		while (i--) {
			switch ( *ftype ) {
			case FLOAT_CONST:
				bmove(&tuple[*foff++],&dbl,*flen);
				printatt(*ftype++, *flen++, (ANYTYPE *)&dbl);
				break;
			
			case CHAR_CONST:
				printatt(*ftype++, *flen++,
						(ANYTYPE *)&tuple[*foff++]);
				break;

			case INT_CONST:
				if ( *flen != 4 ) {
					printatt(*ftype++, *flen++,
						(ANYTYPE *)&tuple[*foff++]);
				} else {
					bmove(&tuple[*foff++],&lng, sizeof(lng));
					printatt(*ftype++, *flen++,
							(ANYTYPE *)&lng);
				}
				break;
			}
		}
	}
	printeol();
}

void
printeh(void)
{
	register int		i;

	putc(Out_arg.coldelim, stdout);
	for (i = 1; i < Printlgth; i++) {
		putc('-', stdout);
	}
	printeol();
}

void
printhdr(char type, int length, char *value)
{
	register int	i;
	char		c;

	switch (type) {
	case INT_CONST:
		switch (length) {
		case 1:
			length = Out_arg.i1width;
			break;

		case 2:
			length = Out_arg.i2width;
			break;

		case 4:
			length = Out_arg.i4width;
			break;

		default:
			syserr("printhdr: i%d", length);
		}
		break;

	case FLOAT_CONST:
		switch (length) {
		case 4:
			length = Out_arg.f4width;
			break;

		case 8:
			length = Out_arg.f8width;
			break;

		default:
			syserr("printhdr: f%d", length);
		}
		break;

	case CHAR_CONST:
		length &= I1MASK;
		if (length < Out_arg.c0width) {
			length = Out_arg.c0width;
		}
		break;

	default:
		syserr("printhdr: type 0%o", type);
	}

	putc(Out_arg.coldelim, stdout);
	for (i = 0; i < length && i < MAX_NAME_SIZE; i++) {
		if ((c = *value++) != 0) {
			putc(c, stdout);
		} else {
			break;
		}
	}

	for ( ; i < length; i++) {
		putc(' ', stdout);
	}

	Printlgth += length + 1;
}

void
beginhdr(void)
{
	Printlgth = 0;
	putchar('\n');
}

char	*Acctuple;
char	Accanon[MAX_TUP_SIZE];


/*
**  PUT_TUPLE
**
**	Put the canonical tuple in the position
**	on the current page specified by tid
**
**	Trace Flags:
**		27.3-5
*/
void
put_tuple(tid_t *tid, char *tuple, int length)
{
	register char	*tp, *ttp;
	register	i;

#ifdef xATR2
	if (tTf(27, 3)) {
		printf("put_tuple:len=%d,", length);
		ttp = tuple;
		for (i = 0; i < length; i++) {
			printf("%3d,", *ttp);
			ttp++;
		}
		dumptid(tid);
	}
#endif

	/* get address in buffer */
	tp = get_addr(tid);

	/* move the tuple */
	bmove(tuple, tp, length);

	/* mark page as dirty */
	Acc_head->am_flags |= BUF_DIRTY;
}

/*
**  COMP_TUP
**
**	Compress the tuple into Accanon. Compression is
**	done by copying INT_CONST and FLOAT_CONST as is.
**	For CHAR_CONST fields, the tuple is copied until a null
**	byte or until the end of the field. Then trailing
**	blanks are removed and a null byte is inserted at
**	the end if any trailing blanks were present.
*/
int
comp_tup(desc_t *d, char *src)
{
	register char	*dst;
	char		*save;
	char		*domlen;
	char		*domtype;
	int		numatts;
	int		len;
	int		i;
	int		j;

	dst = Accanon;

	domlen = &d->d_len[1];
	domtype = &d->d_fmt[1];
	save = src - d->d_off[1];

	/* ignore lid field */
	numatts = d->d_r.r_attrc - d->d_r.r_dim;
	for (i = 1; i <= numatts; i++) {
		len = *domlen++ & I1MASK;
		src = save + d->d_off[i];
		if (*domtype++ == CHAR_CONST) {
			for (j = 1; j <= len; j++) {
				if ((*dst++ = *src++) == 0) {
					dst--;
					break;
				}
			}

			while (j--) {
				if (*--dst != ' ') {
					break;
				}
			}

			if (j != len) {
				*++dst = 0;
			}

			dst++;
		} else {
			while (len--)
				*dst++ = *src++;

		}
	}
	return (dst - Accanon);
}

/*
**  CANONICAL
**
**	Make the tuple canonical and return the length
**	of the tuple.
**
**	If the relation is compressed then the tuple in
**	compressed into the global area Accanon.
**
**	As a side effect, the address of the tuple to be
**	inserted is placed in Acctuple.
**
**	returns: length of canonical tuple
**
**	Trace Flags:
**		27.6, 7
*/
int
canonical(desc_t *d, char *tuple)
{
	register int	i;

	if (M_COMPRESSED(d->d_r.r_spec)) {
		/* compress tuple */
		i = comp_tup(d, tuple);
		Acctuple = Accanon;
	} else {
		Acctuple = tuple;
		/* ignore lid field */
		i = d->d_r.r_width - 4 * d->d_r.r_dim;
	}
#ifdef xATR3
	if (tTf(27, 6))
		printf("canonical: %d\n", i);
#endif
	return (i);
}

/*
**  SPACE_LEFT
**
**	Determine how much space remains on the page in
**	the current buffer. Included in computation
**	is the room for an additional line number
*/
int
space_left(accbuf_t *bp)
{
	register int	nextoff;
	register int	i;
	register short	*pp;

	nextoff = bp->am_nextline;
#ifdef xATR3
	if (nextoff < 0 || nextoff > PGSIZE)
		syserr("space_left: nextoff=%d", nextoff);
#endif

	/* compute space left on page */
	pp = &bp->am_linev[-nextoff];
	i = PGSIZE - *pp - (nextoff + 2) * 2;

	/* see if there is also a free line number */
	if (nextoff < MAX_TUPS_PER_PG) {
		return (i);
	}
	while (++pp <= &bp->am_linev[0])
		if (*pp == 0)
			return (i);
	return (0);
}

/*
**	Tup_len finds the amount of space occupied by the
**	tuple specified by "tid"
*/
int
tup_len(tid_t *tid)
{
	register short	*lp;
	register int	nextoff;
	register int	off;
	int		lineoff;
	int		i;

	/* point to line number table */
	lp = (short *) Acc_head->am_linev;

	/* find offset for tid */
	lineoff = lp[-(tid->line_id & I1MASK)];

	/* assume next line number follows lineoff */
	nextoff = lp[-Acc_head->am_nextline];

	/* look for the line offset following lineoff */
	for (i = 0; i < Acc_head->am_nextline; i++) {
		off = *lp--;
		if (off <= lineoff) {
			continue;
		}
		if (off < nextoff) {
			nextoff = off;
		}
	}
#ifdef xATR3
	if (tTf(27, 8)) {
		printf("tup_len ret %d\n", nextoff - lineoff);
	}
#endif
	return(nextoff - lineoff);
}
