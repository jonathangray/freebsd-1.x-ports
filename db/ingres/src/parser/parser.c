#include <stdio.h>
#include <ingres.h>
#include <range.h>
#include <tree.h>
#include <func.h>
#include <pv.h>
#include "parser.h"
#include "../equel/constants.h"
#include "../equel/globals.h"
#include "sccs.h"
#include <errors.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#define INGRES_IUTIL
#define INGRES_GUTIL
#define INGRES_EQUEL
#include "protos.h"

SCCSID(@(#)parser.c	8.3	2/8/85)

short			tTparser[100];

func_t		ParserFn = {
	"PARSER",
	parser,
	par_init,
	NULL,
	NULL,
	0,
	tTparser,
	100,
	'P',
	0
};

desc_t	Reldesc;
att_ent_t		Attable[MAXATT];/* attrib stash space, turned into a list later */
att_ent_t		*Freeatt;	/* free list of attrib stash */
qtree_t	*Tidnode;	/* pointer to tid node of targ list
					   for REPLACE, DELETE */
qtree_t	*Lastree;	/* pointer to root node of tree */
DELIMLIST 	*Delimhead;	/* pointer to head of Delim queue */
static int	FirstCall = 1;  /* true for the very first call of the parser */
extern att_ent_t	Faketid;	/* atstash structure for tid_t node */
#ifdef	DISTRIB
extern att_ent_t	Fakesid;	/* atstash structure for SID node */
#endif

int			Rsdmno;		/* result domain number */
int			Opflag;		/* operator flag contains query mode */
char			*Relspec;	/* ptr to storage structure of result relation */
char			*Indexspec;	/* ptr to stor strctr of index */
char			*Indexname;	/* ptr to name of index */
char			Trfrmt;		/* format for type checking */
char			Trfrml;		/* format length for type checking */
char			*Trname;	/* pointer to attribute name */
int			Agflag;		/* how many aggs in this qry */
int			Equel;		/* indicates EQUEL preprocessor on */
int			Ingerr;		/* set to error num if a query returns
					   an error from processes below */
int			Qlflag;		/* set when processing a qual */
int			Noupdt;		/* INGRES user override of no update restriction */
int			Err_fnd;	/* no actions done if 1 */
int			Err_current;	/* 1 if error found in current statement */
int			yyline;		/* line counter */
int			Dcase;		/* default case mapping */
int			Permcomd;
int			Qrymod;		/* qrymod on in database flag */
tid_t			tid;


/*
**  PARSER -- the actual main routine
**
**	Trace Flags:
**		Parser ~~ 64
*/
int
parser(void)
{

	int	i;


#ifdef	xPTR1
	tTfp(64, 0, "Parser %d\n", getpid());
#endif

	if (FirstCall) {
		FirstCall = 0;
		if ((i = openr(&Reldesc, OR_WRITE, "rdelim")) < 0)
			syserr("relname: error in openr '%d'", i);
		if (i > 0) {
			/* invalid relation name */
			par_error(RNGEXIST, WARN, "rdelim", 0, 0);
		} else {
			if ((i = make_list(&Reldesc, "system")) < 0)
				par_error(DELEXIST, WARN, 0, 0, 0);
		}
		closer(&Reldesc);
	}
	if (startgo() < 0) {
		endgo();
		return (-1);
	}

	/* yyparse returns 1 in case of error */
	if (yyparse()) {
		endgo();
		return (-2);
	}

	endgo();

	return(0);
}
