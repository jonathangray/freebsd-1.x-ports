#include <stdio.h>

#include <errors.h>
#include <ingres.h>
#include <aux.h>
#include <access.h>
#include <tree.h>
#include <symbol.h>
#include "globs.h"
#include "sccs.h"

#define INGRES_IUTIL
#define INGRES_GUTIL
#define INGRES_CTLMOD
#define INGRES_OVQP
#include "protos.h"

SCCSID(@(#)call_ovqp.c	8.6	12/18/85)


/*
** CALL_OVQP -- Routines which interface to the One Variable Query Processor.
**
**	This file contains the routines associated with sending queries
**	and receiving results from OVQP. The interface to these routines is
**	still messy. Call_ovqp is given the query, mode, and result relation
**	as parameters and gets the source relation, and two flags
**	(De.de_newq, De.de_newr) as globals. The routines include:
**
**	Call_ovqp -- Sends a One-var query to ovqp and flushes the pipe.
**
**	Readresult -- Reads the result from a one-var query.
**
**	Endovqp    -- Informs ovqp that the query is over. Helps to synchronize
**			the batch file (if any).
**
**	Trace Flags:
**		61
*/

void
ov_err(int code)
{
	derror(code);
}


/*
**	Add node q to ovqp's list
*/
void
ovqpnod(register qtree_t *q)
{
	register sym_t	*s;
	register int	i;

	s = &q->sym;

	/* VAR nodes must be specially processed */
	if (s->type == VAR) {
		/* locate currently active VAR */
		q = ckvar(q);

		/* Allocate an ovqp var node for the VAR */
		s = (sym_t *) need(De.ov_ovqpbuf, SYM_HDR_SIZ + sizeof(s->value.sym_var));
		s->len = sizeof(s->value.sym_var);
		s->value.sym_var.attno = q->sym.value.sym_var.attno;
		s->value.sym_var.varfrmt = q->sym.value.sym_var.varfrmt;
		s->value.sym_var.varfrml = q->sym.value.sym_var.varfrml;
		s->value.sym_var.varstr = q->sym.value.sym_var.varstr;

		/* If VAR has been substituted for, get value */
		if (q->sym.value.sym_var.valptr) {
			/* This is a substituted variable */
			if (q->sym.value.sym_var.varno == De.de_sourcevar) {
				syserr("ovqpnod:bd sub %d,%d",
						q->sym.value.sym_var.varno,
						De.de_sourcevar);
			}
			s->type = S_VAR;
			s->value.sym_var.valptr = q->sym.value.sym_var.valptr;
		} else {
			/* Var for one variable query */
			if (q->sym.value.sym_var.varno != De.de_sourcevar) {
				syserr("ovqpnod:src var %d,%d",
						q->sym.value.sym_var.varno,
						De.de_sourcevar);
			}
			s->type = VAR;
			i = q->sym.value.sym_var.attno;
			s->value.sym_var.valptr = (i == 0) ?
				(ANYTYPE *) &De.ov_intid :
			(ANYTYPE *) (De.ov_intup + De.ov_source->d_off[i]);
		}
	}
	if (s->type == AOP)
		De.ov_agcount++;

	/* add symbol to list */
	if (De.de_qvptr > MAXNODES - 1)
		ov_err(NODOVFLOW);
	De.de_qvect[De.de_qvptr++] = s;
}

/*
** Call_ovqp -- send query down pipe to ovqp and flush pipe.
**	Inputs are:
**		mode		retrieve, append, etc.
**		resultnum	result relation id
**		tree		the query
**		De.de_sourcevar	(global) if >= 0 then source var
**		De.de_newq		send NEWQ symbol
**		De.de_newr		send NEWR symbol
*/
int
call_ovqp(register qtree_t *tree, int mode, int resultnum)
{
	register int	i;
	extern bool	Batchupd;
	extern desc_t	Inddes;
	int		ovqpbuf[1+LBUFSIZE/sizeof(int)];


#ifdef xDTR1
	if (tTf(61, -1)) {
		if (tTf(61, 0))
			printf("CALL_OVQP-\n");
		if (tTf(61, 1)) {
			if (De.de_newq) {
				printf("new query to ovqp\n");
				treepr(tree);
			} else
				printf("query same as previous\n");
		}
		if (tTf(61, 2)) {
			printf("De.de_sourcevar=%d\t", De.de_sourcevar);
			if (De.de_sourcevar >= 0)
				printf("relid=%s\t", rangename(De.de_sourcevar));
			if (resultnum >= 0)
				printf("De.ov_resultname=%s", rnum_convert(resultnum));
			if (tree->sym.value.sym_root.rootuser)
				printf(", userqry");
			printf("\n");
		}
	}
#endif



	/* assign mode of this query */
	De.de_qmode = mode;

	if (De.de_newr) {
		De.de_newr = FALSE;
	}

	if (resultnum >= 0) {
		De.ov_result = specopen(resultnum);
	} else {
		De.ov_result = NULL;
	}

	if (De.de_sourcevar >= 0) {
		De.ov_source = readopen(De.de_sourcevar);
	} else {
		De.ov_source = NULL;
	}

	/* assume this will be direct update */
	De.ov_userqry = De.de_buflag = FALSE;

	if (tree->sym.value.sym_root.rootuser) {
		De.ov_userqry = TRUE;
		/* handle batch file */
		if (De.ov_result && De.de_qmode != mdRETR) {
			if (Batchupd || De.ov_result->d_r.r_indexed > 0) {
				if (De.ov_bopen == 0) {
					if (De.ov_result->d_r.r_indexed > 0)
						opencatalog("indices", OR_READ);
					if ((i = openbatch(De.ov_result, &Inddes, De.de_qmode)) != 0)
						syserr("call_ovqp:opn batch %d", i);
					De.ov_bopen = TRUE;
				}
				De.de_buflag = TRUE;
			}
		}
	}

	/*  now write the query list itself  */
	if (De.de_newq) {
		De.ov_ovqpbuf = (char *)ovqpbuf;
		initbuf(De.ov_ovqpbuf, LBUFSIZE, LISTFULL, derror);
		De.de_qvptr = 0;
		De.ov_alist = De.ov_bylist = De.ov_qlist = De.ov_tlist = NULL;
		De.ov_targvc = tree->sym.value.sym_root.lvarc;
		De.ov_qualvc = bitcnt(tree->sym.value.sym_root.rvarm);
		De.ov_agcount = 0;

		if (tree->sym.type == AGHEAD) {
			De.ov_alist = &De.de_qvect[0];
			if (tree->left->sym.type == BYHEAD) {
				mklist(tree->left->right);
				ovqpnod(tree->left);	/* BYHEAD node */
				De.ov_bylist = &De.de_qvect[De.de_qvptr];
				mklist(tree->left->left);
			} else
				mklist(tree->left);
		} else {
			if (tree->left->sym.type != TREE) {
				De.ov_tlist = &De.de_qvect[0];
				mklist(tree->left);
			}
		}

		/* now for the qualification */
		ovqpnod(tree);	/* ROOT node */

		if (tree->right->sym.type != QLEND) {
			De.ov_qlist = &De.de_qvect[De.de_qvptr];
			mklist(tree->right);
		}
		ovqpnod(De.de_qle);	/* QLEND node */
	}

	/* Now call ovqp */
	i = (strategy()) ? scan() : EMPTY;

	/* return result of query */
	return (i == NONEMPTY);	/* TRUE if tuple satisfied */
}

/*
** Endovqp -- Inform ovqp that processing is complete. "Ack" indicates
**	whether to wait for an acknowledgement from ovqp. The overall
**	mode of the query is sent followed by an EXIT command.
**
**	Ovqp decides whether to use batch update or not. If ack == ACK
**	then endovqp will read a RETVAL symbol from ovqp and return
**	a token which specifies whether to call the update processor or not.
*/
int
endovqp(int ack)
{
	register int	i;

	if (ack != RUBACK) {
		if (Equel && De.de_qry_mode == mdRETTERM) {
			/* signal end of retrieve to equel process */
			equeleol(EXIT);
		}
	}

	i = NOUPDATE;

	if (ack == ACK) {
		if (De.ov_bopen) {
			closebatch();
			De.ov_bopen = FALSE;
			i = UPDATE;
		}
	} else {
		if (De.ov_bopen) {
			rmbatch();
			De.ov_bopen = FALSE;
		}
	}

	closecatalog(FALSE);

	return (i);
}

/*
**  READAGG_RESULT
*/
void
readagg_result(qtree_t **result)
{
	register qtree_t	**r;
	register qtree_t	*aop;
	register int		i;

	r = result;
	De.ov_tend = De.ov_outtup;
	for (r = result ; (aop = *r++) != 0 ; ) {
		i = aop->sym.len & I1MASK;
		if (aop->sym.type == CHAR_CONST) {
			pad(De.ov_tend, i);
		}
		bmove(De.ov_tend, (char *)&aop->sym.value, i);
		De.ov_tend += i;
#ifdef xDTR1
		if (tTf(61, 3))
			nodepr(aop);
#endif
	}
}

desc_t *
openindex(char *name)
{
	register desc_t	*d;
	register int	varno;

	varno = SECINDVAR;
	De.de_rangev[varno].relnum = rnum_findadd(name);
	d = readopen(varno);
	return (d);
}

/*
**	Use "closer()" for closing relations. See
**	desc_close in openrs.c for details.
*/
int	(*Des_closefunc)()	= closer;

void
init_decomp(void)
{
	static accbuf_t	xtrabufs[12];

	set_so_buf();
	acc_addbuf(xtrabufs, 12);
}

void
startdecomp(void)
{
	/* called at the start of each user query */
	initrange();
	rnum_init();
	startovqp();
}
