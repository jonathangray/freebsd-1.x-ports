#include <sys/types.h>
#include <sys/stat.h>

#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <useful.h>
#include "sccs.h"

#define INGRES_GUTIL
#define INGRES_LIBQ
#include "protos.h"

SCCSID(@(#)copydb.q	8.2	1/17/85)


#define	MAX_NAME_SIZE	13	/* one more for the null */
#define	MAX_2ND_KEYS	6	/* changing this is insuficient */
#define	MAXINDEX	10	/* change as needed */
#define	MAXREL	1000	/* can be increased as needed */
#define	MAX_DOMAINS 	50

##  char	Usercode[100];
char	**Wanted;
/*
int	Status;
*/

extern	void	(*ExitFn)();
FILE	*Ifile, *Ofile;

typedef struct relation {
	char	name[MAX_NAME_SIZE];
	char	indx;
	int	spec;
	int	atts;
} relation_t;

typedef struct attribute {
	char	aname[MAX_NAME_SIZE];
	char	format;
	int	length;
} attr_t;

typedef struct index {
	char	iname[MAX_NAME_SIZE];
	char	ispec;
	char	idom[MAX_2ND_KEYS];
} index_t;

int retatts(char *name, attr_t *attributes, int *attxtra);

/*
** sysexit
**	The function to call for an exit to occur, this
**	is all for the benefit of syserr, so it doesn't core dump.
*/
void
sysexit(int value)
{
	exit(value);
}/* sysexit */

void
putboth(char ch)
{
	putc(ch, Ofile);
	putc(ch, Ifile);
}

/*
** Check to see if relation is wanted.
*/

int
notwanted(char *relname)
{

	register char	**wantlist;
	register char	*nptr;

	if (*(wantlist = Wanted) == (char *) 0)
		return (0);

	nptr = relname;
	do {
		if (!scompare(nptr, 0, *wantlist++, 0))
			return (0);
	} while (*wantlist != (char *) 0);
	return (1);
}

void
exitfn()
{
	fflush(Ifile);
	fflush(Ofile);
	exit(-1);
}

void
writein(char *string)
{
	register char	*sp;

	sp = string;
	while (*sp)
		putc(*sp++,Ifile);
}

void
modify(char *name, attr_t *attributes, int *attxtra, int spec, int xcount)
{
	register	i,j;
	writein("modify ");
	writein(name);
	writein(" to ");
	switch(spec) {

	  case -5:
		writein("cheap");
		goto nokeys;

	  case -10:
	  case -11:
		writein("c");
	  case 10:
	  case 11:
		writein("isam on ");
		break;

	  case -20:
	  case -21:
		writein("c");
	  case 21:
	  case 20:
		writein("hash on ");
		break;

	  default:
		syserr("Unimplimented spec: %d on %s",
			spec,name);

	}
	j = 0;
	for (i = 1; i <= xcount; i++) {
		if(j++)
			writein(",\n\t");
		writein(attributes[attxtra[i]].aname);
	}
	if (!j) {
		syserr("No keys found for %s!!!",name);
		noise(2);
	}
nokeys:
	writein("\n\\p\\g\n");
}

void
writeboth(char *string)
{
	register char	*sp;

	sp = string;

	while(*sp) {
		putc(*sp, Ofile);
		putc(*sp++, Ifile);
	}
}

void
writeout(char *string)
{
	register char 	*sp;

	sp = string;
	while (*sp)
		putc(*sp++,Ofile);
}

void
main(int argc, char **argv)
{

##	char	*db;
	char	*path;
##	char	name[MAX_NAME_SIZE];
##	int	indx;
##	int	atts;
##	int	spec;
##	char	iname[MAX_NAME_SIZE];
##	int	idomv1, idomv2, idomv3, idomv4, idomv5, idomv6;

##	int	n;

	register relation_t	*relptr;
	register attr_t	*attptr;
	register int			i;
	relation_t			relations[MAXREL];
	attr_t		attributes[MAX_DOMAINS], indatts[MAX_2ND_KEYS];
	index_t			indices[MAXINDEX];
	index_t			*indxptr;
	int				attxtra[MAX_DOMAINS], indattx[MAX_2ND_KEYS];
	int				xcount;
##	char				*uover;

	umask(022);
	ExitFn = sysexit;		/* set up exit routine for syserr */

	if (argc < 3)
		syserr(0,"usage: copydb database targetdir [relation] ...");

	uover = 0;
	if (bequal(*++argv, "-u", 2))
		uover = *argv++;
	db = *argv++;
	path = *argv++;
	Wanted = argv++;

	if(path[0] != '/') {
		noise(2);
		printf("Warning %s is not a full path name\n",path);
		noise(2);
	}


	if ((Ifile = fopen((char *)ztack(path,"/copy.in"), "w")) == NULL)
		syserr(0,"Cannot create %s!",ztack(path,"/copy.in"));
	if ((Ofile = fopen((char *)ztack(path,"/copy.out"), "w")) == NULL)
		syserr(0,"Cannot create %s!",ztack(path,"/copy.out"));


##	ingres	db uover

##	retrieve (Usercode = usercode)

##	range of r is relation
	i = 0;

	/*
	**	Bring the relations into core
	*/
	relptr = relations;
##	retrieve (name = r.r_id, spec = r.r_spec, indx = r.r_indexed,
##		atts = r.r_attrc)
##		where r.r_owner = Usercode
##		and mod(r.r_status,2) != 1 /* sys catalog */
##		and r.r_indexed >= 0
##		and r.r_id != "_SYS*" /* system temporaries */
##	{
		if (notwanted(name))
			continue;
		if(i++>=MAXREL) {
			syserr("Too many relations!!!");
			noise(3);
		}
		smove(name,(relptr->name));
		relptr->spec = spec;
		relptr->indx = indx;
		relptr++->atts = atts;
##	}


	/*
	**	For each relation bring its attributes into core
	**	in domain order
	*/
##	range of a is attribute
##	range of i is indices
	n = i;
	printf("%d relations found\n",n);
	for (relptr = relations; relptr < &relations[n]; relptr++) {
		writein("create ");
		writeout("copy ");
		smove(relptr->name,name);
		writeboth(name);
		writeboth("(\n\t");

		xcount = retatts(name,attributes,attxtra);
		for(i = 0; i < relptr->atts; i++) {
			attptr = &attributes[i];
			if(i)
				writeboth(",\n\t");
			writeboth(attptr->aname);
			putboth('=');
			switch(attptr->format) {

			  case 'i':
				putboth('i');
				break;

			  case 'f':
				putboth('f');
				break;

			  case 'c':
				putboth('c');
				break;

			  default:
				syserr("Bad type: %d, in %c of %s!!",
					attptr->format,attptr->aname,
					name);
				noise(3);

			}
				attptr->length &= I1MASK;
				writeboth(iocv(attptr->length));
		}

		writeboth("\n)");
		writeout(" into \"");
		writein("\n\\p\\g\ncopy ");
		writein(name);
		writein("() from \"");
		writeboth(path);
		putboth('/');
		writeboth(name);
		writeboth(Usercode);
		writeboth("\"\n\\p\\g\n");


		if (relptr->spec != 5 && relptr->spec != 0)
			modify(name,attributes,attxtra,relptr->spec,xcount);
		if (relptr->indx) {
			indxptr = indices;
			i = 0;

##			retrieve (iname = i.i_index, spec = i.i_indrelspec,
##				idomv1 = i.idom1, idomv2 = i.idom2, idomv3 = i.idom3,
##				idomv4 = i.idom4, idomv5 = i.idom5, idomv6 = i.idom6)
##			where i.i_relname = name and i.i_owner = Usercode
##			{

				if (i++ >= MAXINDEX) {
					syserr("Too many indices on %s!!",name);
					noise(2);
				}
				smove(iname,indxptr->iname);
				indxptr->ispec = spec;
				indxptr->idom[0] = idomv1;
				indxptr->idom[1] = idomv2;
				indxptr->idom[2] = idomv3;
				indxptr->idom[3] = idomv4;
				indxptr->idom[4] = idomv5;
				indxptr->idom[5] = idomv6;
				indxptr++;
##			}

			while (--indxptr >= indices) {
	
				writein("index on ");
				writein(name);
				writein(" is ");
				writein(indxptr->iname);
				writein("(\n\t");
	
				for (i=0; indxptr->idom[i] && i < MAX_2ND_KEYS; i++) {
					if(i)
						writein(",\n\t");
					writein(attributes[indxptr->idom[i]-1].aname);
				}
				writein(")\n\\p\\g\n");
				if(indxptr->ispec != 10 && indxptr->ispec != 11) {
					xcount = retatts(indxptr->iname,indatts,indattx);
					modify(indxptr->iname,indatts,indattx,indxptr->ispec,xcount);
				}
			}
		}
	}
	fflush(Ifile);
	fflush(Ofile);
	noise(1);
	printf("All done!\n\n");
	noise(1);
}

int
retatts(char *name, attr_t *attributes, int *attxtra)
{
	register	xcount, i;
	register attr_t	*attptr;

##	char	aname[MAX_NAME_SIZE];
##	int	domain, length, extra;
##	char	format[1];

	i = xcount = 0;
##	retrieve (aname = a.a_name,domain = a.a_id, format = a.a_fmt,
##		 length = a.a_len, extra = a.a_iskey)
##
##	where a.a_rel = name and a.a_owner = Usercode
##	{
		if (i++ >= MAX_DOMAINS) {
			syserr("Too many attributes!!!");
			noise(3);
		}
		attptr = &attributes[domain-1];
		smove(aname,attptr->aname);
		attptr->format = format[0];
		attptr->length = length;
		if(extra)
		{
			attxtra[extra] = domain - 1;
			xcount++;
		}
##	}
	return(xcount);
}
