#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <ingres.h>
#include <aux.h>
#include <symbol.h>
#include <tree.h>
#include "parser.h"
#include <pv.h>
#include "scanner.h"
#include "sccs.h"
#include <../ovqp/ovqp.h>
#include <errors.h>

#define INGRES_IUTIL
#define INGRES_GUTIL
#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)control.c	8.4	1/22/88)

/*
**  CONTROL.C -- -- collection of control functions for the parser
**
**	These routines administrate the operation of the parser for internal
**	sequencing.  There are 2 pairs of routines, one pair for each
**	quel statement, and one for each go-block, and there is one
**	routine to finish retrieve statements.
**
**	Defines:
**		startgo		-- initialize for a go-block
**		init_quelst	-- initialize for a quel statement
**		endquelst	-- clean up after a quel statement
**		endretrieve	-- clean up after a retrieve
**		endgo		-- clean up after a go-block
**
**	Trace Flags:
**		control.c ~~ 48, 49
**
**	History:
**		6 Jun 80 (jiw)		modified and redocumented for 6.3
**		15 Jan 79 (rick)	collected and documented more
**		ancient history
*/

/*
** INIT_QUELST -- set vbles for default mode before each quel statement
**
**	Parameters:
**		none
**
**	Returns:
**		nothing
**
**	Trace Flags:
**		init_quelst ~~ 48.0
*/

int
init_quelst(void)
{
	extern int	Err_current;
	extern int	Pars;
	extern int	Lcase;
	extern int	Dcase;
	extern int	Agflag;
	extern int	Opflag;
	extern int	Resrng;
	extern int	Qlflag;

#ifdef	xPTR3
	tTfp(48, 0, "Init_quelst\n");
#endif

	Err_current = 0;			/* no error yet */
	Pars = 1;				/* set scanner into "parser" mode */
	Lcase = Dcase;				/* set case mapping to default */
	Agflag = 0;				/* reset aggregate flag */
	Opflag = 0;				/* reset qmode flag */
	Resrng = -1;				/* reset result relation slot */
	Qlflag = 0;				/* reset qualification flag */

	initp();				/* initialize parameter vector */
	init_qt();				/* assume we have qrytree */

	freesym();				/* free symbol table space */
	rngreset();				/* reset used bits in range tbl */

	return (1);
}

/*
** ENDQUELST -- finish command checking and processing for each quel statement
**
**	Parameters:
**		op -- the type of query to finish up
**
**	Returns:
**		nothing
**
**	Trace Flags:
**		endquelst ~~ 48.4, 48.5
*/

int
endquelst(register int op)
{
	register int			i;
	char				ibuf[2];	/* two char buffer for index keys */
	char				*cp;

	extern char			*Indexspec;
	extern char			*Indexname;
	extern int			Equel;
	extern int			Agflag;

	extern short			yyerrflag;
	extern int			Err_current;
	extern int			Ingerr;
	extern int			Err_fnd;

	extern desc_t			Attdes;
	extern desc_t			Reldesc;
	extern int			Rsdmno;
	extern PARRNG			Parrng[];
	extern int			Resrng;

#ifdef	xPTR3
	if (tTf(48, 4))
		prvect(0, getp());
#endif

	/* check next token for GOVAL if the next token has been read */
	if (!Err_current && !yyerrflag)
		switch (op) {
		   case mdSAVE:
		   case mdCOPY:
		   case mdCREATE:

#ifdef	DISTRIB
		  case mdDCREATE:
#endif

		  case mdINDEX:
		  case mdRANGE:
		  case mdSTOP:
			break;

		  default:
			/* has vble ending and therefore must detect valid end of command */
#ifdef	xPTR3
			tTfp(48, 5, "before NXTCMDERR\n");
#endif
			if (Lastok.tokop != GOVAL)
				/* next token not start of command */
				par_error(NXTCMDERR, WARN, 0, 0, 0);
			break;
		}

	if (Agflag >= MAX_QRY_AGGS)
		/* too many aggregates */
		par_error(AGGXTRA, WARN, 0, 0, 0);

	/* command ok so far, finish up */
	if (!Err_fnd) {
		switch (op) {
		  case mdINDEX:
			if (tTf(48, 5))
				printf("mdINDEX\n");
			if (call(op, NULL) < 0)
				ack_err();
			if (tTf(48, 5))
				printf("after call to call\n");

			if (Ingerr) {
				if (tTf(48, 5))
					printf("Ingerr = %d\n", Ingerr);

				endgo();

				return (-1);
			}

			if (Indexspec) {
				initp();
				setp(PV_STR, Indexname, 0);	/* type */
				setp(PV_STR, Indexspec, 0);	/* specs */
				setp(PV_STR, "num", 0);
				for (i = 1; i <= Rsdmno; i++) {
					ibuf[0] = i & I1MASK;
					ibuf[1] = '\0';
					setp(PV_STR, ibuf, 0);
				}
				if (call(mdMODIFY, NULL) < 0)
					ack_err();
			}
			break;

		  case mdRETR:
		  case mdRET_UNI:
		  case mdVIEW:
			if (Resrng >= 0)		/* implies result reln */ {
				if (calln(mdCREATE, NULL) < 0)
					ack_err();

				cleanrel(&Attdes);

				if ((i = openr(&Reldesc, OR_RELTID, trim_relname(Parrng[Resrng].vardesc.d_r.r_id))) < 0)
					syserr("result reln: error in openr '%d'", i);
				
				rngent(R_INTERNAL, "", &Reldesc);
			}
			else if (!Equel)
				/* need to print header */
				header(getp());

			if (Ingerr) {
				/*
				** might be nice to back out the create already done
				** by this point so that the user doesn't need to
				*/
				resetp();

				endgo();	/* abort rest of go-block */

				return (-1);
			}
			initp();
			/* fall through */

		  case mdAPP:
		  case mdDEL:
		  case mdREPL:
			if (op != mdVIEW) {
				call_tree(op, mdQRY, ack_err);

				if (op == mdRETR || op == mdRET_UNI)
					endretrieve(ack_err);

				Patnum = 0;
				for (i = 0; i < PATNUM; i++)
					if (Pats[i].string) {
						xfree(Pats[i].string);
						Pats[i].string = NULL;
						Pats[i].len = 0;
					}
				break;
			}

#ifdef DISTRIB
		  case mdDISTRIB:
			op = mdVIEW;
#endif
			/* else, do VIEW */
			cp = trim_relname(Parrng[Resrng].vardesc.d_r.r_id);
			setp(PV_STR, cp, 0);

		  case mdINTEG:
		  case mdPROT:
			call_tree(op, op, ack_err);
			break;

		  case mdCREATE:

#ifdef	DISTRIB
		  case mdDCREATE:
#endif

		  case mdDESTROY:
		  case mdMODIFY:
#ifdef	V6POINT3COMPAT
			/* in this case, if an error in the dbu's will not */
			/* cause other processing to halt */
			call(op, NULL);
#else
			if (call(op, NULL) < 0)
				ack_err();
#endif
			cleanrel(&Attdes);
			break;

		  case mdCOPY:
		  case mdHELP:
		  case mdPRINT:
		  case mdSAVE:
		  case mdDISPLAY:
		  case mdREMQM:
#ifdef	V6POINT3COMPAT
			call(op, NULL);
#else
			if (call(op, NULL) < 0)
				ack_err();
#endif
			break;

		  case mdSTOP:
		  case mdRANGE:
			break;

		  default:
			syserr("Endquelst: bad op %d", op);
		}
	}
    
	/* refresh r_status bits if necessary */
	rngfresh(op);
	if (init_quelst() < 0)
		return (-1);

	return (1);
}

/*
** STARTGO -- do whatever needs doing to set up a go-block
**
**	Parameters:
**		none
**
**	Returns:
**		nothing
**
**	Trace Flags:
**		startgo ~~ 48.8
*/
int
startgo(void)
{
	extern int	Err_fnd;
	extern int	Ingerr;
	extern int	yyline;

#ifdef	xPTR3
	tTfp(48, 8, "startgo\n");
#endif

	/* initialize for go-block */
	get_scan(PRIME);		/* prime the scanner input */
	Err_fnd = 0;		/* no errors have been found yet */
	Ingerr = 0;

	if (init_quelst() < 0)	/* most other init's are done for each statement */
		return (-1);

	yyline = 1;		/* reset line counter */

	return (1);
}

/*
**  ENDGO -- do whatever needs doing to clean up after a go block
**
**	Parameters:
**		none
**
**	Returns:
**		nothing
**
**	Trace Flags:
**		endgo ~~ 48.12
*/
void
endgo(void)
{
	extern int	Equel;
	extern int	Err_fnd;
#ifdef	xPTR3
	tTfp(48, 12, "endgo\n");
#endif

	if (!Equel && Err_fnd > 1)
		error(SUMMARY, iocv(Err_fnd), 0);

	get_scan(SYNC);

	resetp();
}

/*
**  ENDRETRIEVE -- finishes any sort of retrieve
**
**	Endretrieve either creates a result relation or prints a trailer
**
**	Parameters:
**		err_fcn -- function to pass to call
**
**	Returns:
**		nothing
**
**	Trace Flags:
**		endretrieve ~~ 48.14	
**
**	History:
**		June '80 -- (jiw) broken off from call_tree
*/	
void
endretrieve(int (*err_fcn)(void))
{
	char			*cp;
	extern int		Resrng;
	extern char		*Relspec;
	extern PARRNG		Parrng[];
	extern int		Equel;
	extern int		Hdr;

	if (Resrng >= 0) {
		if (Relspec) {
			initp();

			cp = trim_relname(Parrng[Resrng].vardesc.d_r.r_id);
			setp(PV_STR, cp, 0);
			setp(PV_STR, Relspec, 0);
			if (call(mdMODIFY, err_fcn) < 0)
				(*err_fcn)();
		}
	} else if (!Equel) {
		printeh();
		Hdr = FALSE;
	}
}

int
printtrail(void)
{
	extern int	Equel;

	if (!Equel)
		printeh();

	return (-1);
}
