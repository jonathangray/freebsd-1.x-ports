#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <ingres.h>
#include "globs.h"
#include "sccs.h"

#define INGRES_IUTIL
#define INGRES_GUTIL
#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)rnum.c	8.2	5/1/85)


/*
**	Internal numbers are used in decomp to
**	represent relation names. The numbers
**	from 0 to FIRSTNUM-1 refer to the names
**	stored in De.de_name_table[].
**
**	The number from FIRSTNUM to LASTNUM have
**	names which are computed from aa, ab, etc.
*/




/*
**	Assign an internal number rnum to name.
*/
int
rnum_assign(char *name)
{
	register int	i;

	for (i = 0; i < FIRSTNUM; i++)
		if (De.de_num_used[i] == 0) {
			bmove(name, De.de_name_table[i], MAX_NAME_SIZE);
			De.de_num_used[i]++;
			return (i);
		}
	syserr("rnum_assign:no room");
	return (-1);
}

/*
**	Find an existing rnum on a relation or add one.
*/
int
rnum_findadd(char *name)
{
	register int	i;

	for (i = 0; i < FIRSTNUM; i++)
		if (De.de_num_used[i]
		  && !bcmp(name, De.de_name_table[i], MAX_NAME_SIZE))
			return(i);
	return(rnum_assign(name));
}

/*
**	Allocate the next available name
*/
int
rnum_alloc(void)
{
	register int	i;
	register char	*cp;

	cp = &De.de_num_used[FIRSTNUM];
	for (i = FIRSTNUM; i < LASTNUM; i++)
		if (*cp++ == 0) {
			--cp;
			(*cp)++;
			return (i);
		}
	syserr("no free names");
	return (-1);
}

/*
**	Convert internal relation number
**	to its real name. Guarantee '\0' at end.
*/

char *
rnum_convert(int num)
{
	register int	i;
	register char	*ret;
	static char	temp[MAX_NAME_SIZE+1];
	extern char	*Fileset;

	i = num;
	if (i > LASTNUM || De.de_num_used[i] == 0)
		syserr("no name for %d", i);

	ret = temp;

	if (i < FIRSTNUM) {
		bmove(De.de_name_table[i], ret, MAX_NAME_SIZE);
	} else {
		/* compute temp name */
		i -= FIRSTNUM;
		strcpy(temp, "_SYS");
		temp[4] = i%26 + 'a';
		temp[5] = i/26 + 'a';
		temp[6] = '\0';
		concat(temp, Fileset, temp);
		pad(temp, MAX_NAME_SIZE);
	}
	return (ret);
}

/*
**	Remove a num from the used list
*/
void
rnum_remove(int num)
{
	register char	*cp;

	cp = &De.de_num_used[num];

	if (*cp == 0)
		syserr("cant remove %d", num);
	*cp = 0;
}

/*
**	returns number of largest assigned temp number.
**	zero if none
*/
int
rnum_last(void)
{
	register int	i;

	for (i = LASTNUM - 1; i >= FIRSTNUM; i--) {
		if (De.de_num_used[i]) {
			return (i);
		}
	}

	return (0);
}

/*
**	Predicate to check whether rnum is a temporary relation or not
*/
int
rnum_temp(int rnum)
{
	register int	i;

	i = rnum;

	return (i >= FIRSTNUM || bequal("_SYS", rnum_convert(i), 4));
}

/*
**	Clear tag fields from previous query
*/
void
rnum_init(void)
{
	register char	*cp;
	register int	i;

	cp = De.de_num_used;
	i = FIRSTNUM;
	while (--i)
		*cp++ = 0;
}
