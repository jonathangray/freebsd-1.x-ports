#include "ingres.h"
#include "func.h"

#include "protos.h"

/*	@(#)externs.c	8.1	12/31/84	*/

int
test(char *map, int n)
{
	return ((map[n/BITS] & (1<<(n%BITS))) != 0);
}

char	Qbuf[1000];
int     QbufSize = sizeof(Qbuf);

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
