#include <stdio.h>

#include <ingres.h>
#include <aux.h>
#include <access.h>
#include "sccs.h"

SCCSID(@(#)cat_desc.c	8.1	12/31/84)

/*
**	SYSTEM RELATION DESCRIPTOR CACHE DEFINITION
**
*/

desc_t	Reldes;
desc_t	Attdes;
desc_t	Inddes;
desc_t	Treedes;
desc_t	Prodes;
desc_t	Intdes;


struct desxx	Desxx[] = {
	{"relation",	&Reldes,	&Admin.ad_rel},
	{"attribute",	&Attdes,	&Admin.ad_attr},
	{"indices",	&Inddes,	NULL},
	{"tree",	&Treedes,	NULL},
	{"protect",	&Prodes,	NULL},
	{"integrities",	&Intdes,	NULL},
	{ NULL }
};
