#include <stdio.h>

#include <ingres.h>
#include <aux.h>
#include <access.h>
#include <func.h>
#include "sccs.h"

#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)univingres.c	8.2	12/18/85)

/*
**  Configuration table for INGRES backend
*/

char	Qbuf[16000];
int	QbufSize = sizeof(Qbuf);

int	Noupdt;


desc_t	Reldes;
desc_t	Attdes;
desc_t	Inddes;
desc_t	Treedes;
desc_t	Prodes;
desc_t	Intdes;
desc_t	Btreesec;

struct desxx	Desxx[] = {
	{ "relation",		&Reldes,	&Admin.ad_rel },
	{ "attribute",		&Attdes,	&Admin.ad_attr },
	{ "indices",		&Inddes,	NULL },
	{ "tree",		&Treedes,	NULL },
	{ "protect",		&Prodes,	NULL },
	{ "integrities",	&Intdes,	NULL },
	{ NULL }
};


short	tTdbu[100];

extern func_t	ParserFn;
extern func_t	QryModFn;
extern func_t	DefProFn;
extern func_t	DefIntFn;
extern func_t	DefViewFn;
extern func_t	DeOvqpFn;
extern func_t	CopyFn;
extern func_t	CreateFn;
extern func_t	DstroyFn;
extern func_t	HelpFn;
extern func_t	DsplayFn;
extern func_t	KsortFn;
extern func_t	ModifyFn;
extern func_t	PrintFn;
extern func_t	ResetrFn;
extern func_t	RmqmFn;
extern func_t	RupdatFn;
extern func_t	SaveFn;
extern func_t	IndexFn;
extern func_t	SysDmpFn;

func_t	*FuncVect[] = {
	&ParserFn,	/* 0 -- parser */
	&QryModFn,	/* 1 -- qrymod (normal query) */
	&DefViewFn,	/* 2 -- define view */
	&DefIntFn,	/* 3 -- define integrity */
	&DefProFn,	/* 4 -- define permit */
	&DeOvqpFn,	/* 5 -- decomp/ovqp */
	&CreateFn,	/* 6 */
	&DstroyFn,	/* 7 */
	&RupdatFn,	/* 8 */
	&PrintFn,	/* 9 */
	&HelpFn,	/* 10 */
	&ResetrFn,	/* 11 */
	&CopyFn,	/* 12 */
	&SaveFn,	/* 13 */
	&ModifyFn,	/* 14 */
	&IndexFn,	/* 15 */
	&DsplayFn,	/* 16 */
	&SysDmpFn,	/* 17 -- unused */
	&RmqmFn,	/* 18 */
	&KsortFn,	/* 19 */
};

int	NumFunc = sizeof(FuncVect) / sizeof(FuncVect[0]);

void
rubproc(void)
{
	ctlmod_rubproc();
}

void
main(int argc, char **argv)
{
#ifdef DEBUG
	printf("%d: univingres: calling ctlmod_main\n", getpid());
#endif
	exit(ctlmod_main(argc, argv, 0));
}
