#include <stdio.h>

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <time.h>

#include <pv.h>
#include <ingres.h>
#include <aux.h>
#include <catalog.h>
#include <access.h>
#include <func.h>
#include <signal.h>
#include "sccs.h"
#include <errors.h>

#define INGRES_IUTIL
#define INGRES_GUTIL
#define INGRES_CTLMOD
#define INGRES_SUPPORT
#include "protos.h"

SCCSID(@(#)help.c	8.5	2/8/85)

extern short 	tTdbu[100];

func_t HelpFn = {
	"HELP",
	help,
	null_fn,		/* initialization function */
	null_fn,
	NULL,
	0,
	tTdbu,
	100,
	'Z',
	0
};



/*
**  Nroff Manual Section
**
**	The manual section given by 'name' is nroff'ed.  Returns one
**	on success, zero if the manual section is not found.
**
**	Uses trace flag 11
*/

int
man(char *name)
{
	char		manual[100];
	register int	i;
	char		name_nr[18];
	register char	*naa;

	if (strlen(name) > MAXFILENAMESIZ) {
		return (0);
	}

	/* a null manual name gives table of contents */
	if (name[0] == 0)
		smove("../toc.nr", name_nr);
	else
		concat(name, ".nr", name_nr);

	concat(ztack(Pathname, "/doc/quel/"), name_nr, manual);
	if ((i = open(manual, O_RDONLY)) < 0) {
		/* try a unix command instead */
		concat(ztack(Pathname, "/doc/unix/"), name_nr, manual);
		if ((i = open(manual, O_RDONLY)) < 0)
			return (0);
	}
	if (close(i))
		syserr("cannot close %s", manual);
	ruboff(0);	/* wait for child's death if rubout occures */
	i = fork();
	if (i == 0) {
		signal(SIGINT, SIG_DFL);	/* die on rubout */
		setuid(getuid());
		setgid(getgid());
		naa = ztack(Pathname, "/doc/naa");
		execl("/bin/nroff", "nroff", naa, manual, 0);
		execl("/usr/bin/nroff", "nroff", naa, manual, 0);
		syserr("help: exec: nroff");
	}
	/* wait for nroff if fork succeeded */
	if (i > 0)
		fullwait(i, "help: nroff");
	rubon();
	return (1);
}

/*
**  Print Relation Information
**
**	Prints detailed information regarding the relation.
**
**	Uses trace flag 13
*/
int
rel_fmt(register relation_t *r)
{
	struct tup_id		limtid, tid;
	char			buf[MAX_LINE_SIZE + 1];
	attr_t			att;
	index_t			indkey;
	index_t			ind;
	register int		i;
	int			j;
	extern desc_t		Attdes, Inddes;

	printf("\nRelation:\t\t%s\n", trim_relname(r->r_id));
	i = getuser(r->r_owner, buf);
	if (i) {
		smove("(xx)", buf);
		bmove(r->r_owner, &buf[1], sizeof(r->r_owner));
	} else {
		for (i = 0; buf[i] != ':'; i++)
			continue;
		buf[i] = 0;
	}
	printf("Owner:\t\t\t%s\n", buf);
	printf("Tuple width:\t\t%d\n", r->r_width);
	if (r->r_savetime != 0) {
		printf("Saved until:\t\t%s", ctime(&r->r_savetime));
	}
	if ((r->r_status & S_VIEW) == 0) {
		printf("Number of tuples:\t%ld\n", r->r_tupc);
		printf("Storage structure:\t");
		if (M_COMPRESSED(r->r_spec)) {
			printf("compressed ");
		}
		switch (M_TYPEOF(r->r_spec)) {
	
		  case M_HEAP:
			printf("paged heap\n");
			break;
	
		  case M_ISAM:
			printf("ISAM file\n");
			break;
	
		  case M_HASH:
			printf("random hash\n");
			break;
	
		  default:
			printf("unknown structure %d\n", i);
			break;
	
		}
	}

	printf("Relation type:\t\t");
	if (r->r_status & S_CATALOG)
		printf("system catalog\n");
	else if (r->r_status & S_VIEW)
		printf("view\n");
	else
		if (r->r_dim > 0)
			printf("ordered relation\n");
		else if (r->r_indexed < 0) {
			printf("secondary index on ");
			opencatalog("indices", OR_READ);
			ingres_setkey(&Inddes, &indkey, r->r_owner, IOWNERP);
			ingres_setkey(&Inddes, &indkey, r->r_id, IRELIDI);
			if (!getequal(&Inddes, &indkey, &ind, &tid))
				printf("%s\n", trim_relname(ind.i_relname));
			else
				printf("unknown relation\n");
		} else {
			if (r->r_status & S_DISTRIBUTED)
				printf("distributed ");
			printf("user relation\n");
		}
	if (r->r_indexed > 0) {
		printf("Secondary Indices:\t");
		opencatalog("indices", OR_READ);
		ingres_setkey(&Inddes, &indkey, r->r_id, IRELIDP);
		ingres_setkey(&Inddes, &indkey, r->r_owner, IOWNERP);
		if ((i = find(&Inddes, EXACTKEY, &tid, &limtid, &indkey)) != 0)
			syserr("help: find %d indices", i);
		j = FALSE;
		while ((i = get(&Inddes, &tid, &limtid, &ind, 1)) == 0) {
			if (!bequal(&indkey, &ind, MAX_NAME_SIZE + 2))
				continue;
			if (j)
				printf(", ");
			j =TRUE;
			printf("%s", trim_relname(ind.i_index));
		}
		if (i < 0)
			syserr("help:get indices %d", i);
		if (!j)
			printf("unknown");
	}
	printf("\n");

	opencatalog("attribute", OR_READ);
	printf("\n attribute name    type  length  keyno.\n\n");
	seq_init(&Attdes, (desc_t *) r);
	while (seq_attributes(&Attdes, (desc_t *) r, &att)) {
		printf(" %.12s	    %c%8d",
			att.a_name, att.a_fmt, att.a_len & I1MASK);
		if (att.a_iskey)
			printf("%7d", att.a_iskey);
		printf("\n");
	}

	printf("\n");
	return (0);
}

/*
**  PRINT DATABASE INFORMATION
**
**	Prints a list of all the relations in the database, together
**	with their owner.
**
**	Uses trace flag 12
*/
int
relpr(int mode)
{
	extern desc_t	Reldes;
	register desc_t	*d;
	register int			i;
	register char			*cp;
	struct tup_id			limtid, tid;
	char				buf[MAX_LINE_SIZE + 1];
	char				lastuser[USERCODE_SIZE];
	relation_t			rel;

	opencatalog("relation", OR_READ);
	d = &Reldes;
	if ((i = find(d, NOKEY, &tid, &limtid, (void *) NULL)) != 0)
		syserr("help: relpr: find %d", i);

	lastuser[0] = '\0';

	if (mode == HELP_RELLIST)
		printf("\n relation name     relation owner\n\n");

	while ((i = get(d, &tid, &limtid, &rel, 1)) == 0) {
		if (mode == HELP_RELLIST) {
			if (!bequal(lastuser, rel.r_owner, sizeof(rel.r_owner))) {
				if (getuser(rel.r_owner, buf)) {
					/* cant find user code */
					bmove("  ", buf, sizeof(rel.r_owner));
					cp = &buf[sizeof(rel.r_owner)];
					bmove(rel.r_owner, cp, sizeof(rel.r_owner));
					cp += sizeof(rel.r_owner);
					*cp = '\0';
				} else {
					for (cp = buf; *cp != ':'; cp++)
						;
					*cp = '\0';
				}
				bmove(rel.r_owner, lastuser, sizeof(rel.r_owner));
			}
			printf(" %.12s      %s\n", rel.r_id, buf);
		} else {
			if ((rel.r_status & S_CATALOG) ||
			     bequal("_SYS", rel.r_id, 4)) {
				continue;
			}
			if (bequal(Usercode, rel.r_owner, USERCODE_SIZE) ||
			    bequal(Admin.ad_h.adm_owner, rel.r_owner, USERCODE_SIZE)) {
				rel_fmt(&rel);
			}
		}
	}

	if (i < 0)
		syserr("help: relpr: get %d", i);
	if (mode == HELP_RELLIST)
		printf("\n");
	return (0);
}


/*
**	PRINT_DELIM - print one delim of the rdelim relation
**
**		Parameters:
**
*/
void
print_delim(char *delim)
{
	printf("%s\n", delim);
}

/*
**	CONVERT_BITMAP - convert a bitmap back to a BNF expression
**
**		Parameters:
**			dstring - string to stuff the BNF expression info
**			tuple - tuple containing the bitmap
**
*/
void
convert_bitmap(char *dstring, DELIM_TUP *tuple)
{
	int	i,j;
	char	*pntr;

	pntr = dstring + strlen(dstring);
	*pntr++ = (tuple->type == RE_ONE ? RE_LBRACKET : RE_LBRACE);
	i = 0;
	/* XXX */	
	while (i < 128 ) {
		if (test(tuple->bitmap, i)) {
			*pntr++ = i;
			j = ++i;
			while ((j < 128) && test(tuple->bitmap, j))
				j++;
			if ( (j - i) >= 5) {
				j--;
				*pntr++ = '-';
				*pntr++ = j;
				i = j + 1;
			}
		}
		else
			i++;
	}
	*pntr++ = (tuple->type == RE_ONE) ? RE_RBRACKET : RE_RBRACE;
	*pntr = 0;
}

/*
**	HELPDELIM - print all delims presently defined.
**
**		Parameters:
**			group - the group of delims to print
**
**		Returns:
**			0 - if successful
**			-1 - if relation not found
*/
int
helpdelim(char *group, desc_t *des)
{
	DELIM_TUP	tuple;
	tid_t		lotid,hitid;
	char		delim[12];
	int		start = 0;
	int		begin = 1;
	char		dstring[1024];
	int		found=0;

	if (find(des,LRANGEKEY, &lotid, &hitid, group) < 0)
		return(-1);
	find(des,HRANGEKEY, &lotid, &hitid, group);
	printf("\n \t>>>>  %s  <<<<\n", group);

	while (!get(des, &lotid, &hitid, &tuple, 1)) {

		if (strcmp(tuple.group, group))
			continue;

		if (strcmp(tuple.delim, delim))
			start = 1;

		if (start) {
			found = 1;
			if (begin) {
				begin = 0;
			} else {
				print_delim(dstring);
			}

			/*start a new string*/
			(void) strcpy(delim, tuple.delim);
			start = 0;
			dstring[0] = 0;
			(void) strcat(dstring, tuple.delim);
			strcat(dstring, ":		");
			convert_bitmap(dstring, &tuple);
		} else {
			/*add to old string*/
			convert_bitmap(dstring, &tuple);
		}
	}
	if (!found) {
		printf("group %s does not exist\n", group);
	} else {
		print_delim(dstring);
	}
	return(0);
}

/*
**	ALLDELIMS - print all the delims currently defined
**
*/
int
alldelims(desc_t *des)
{
	DELIM_TUP	tuple;
	int		start=1;
	char		group[12];
	tid_t		lotid;
	tid_t		hitid;

	printf("Delimitor groups:\n");
	if (find(des,LRANGEKEY, &lotid, &hitid, group) < 0)
		return(-1);
	find(des,HRANGEKEY, &lotid, &hitid, group);

	while (!get(des, &lotid, &hitid, &tuple, 1)) {

		if (strcmp(tuple.group, group))
			start = 1;

		if (start) {
			strcpy(group, tuple.group);
			printf("\t\t %s\n", group);
			/*helpdelim(group, des); */
			start = 0;
		}
	}
	return(0);
}

/*
**  HELP - Provide Information to User
**
**	Arguments:
**		pv[i] - code
**			HELP_RELINFO - print relation information
**			HELP_MANSEC - print manual section
**			DELIMLIST - print delim information
**			HELP_RELLIST - print relation list
**			HELP_ALLRELINFO - print relation info for all accessible 
**						relations
**
**		pv[i+1] - name of entity for modes 0 or 1
**
**	Trace Flags:
**		44
*/
int
help(int parmc, paramv_t *parmv)
{
	desc_t		des;
	int		mode;
	register paramv_t	*pv;
	register int	ret;
	int		i;

#ifdef	xZTR1
	if (tTf(44, -1)) {
		printf(">>help\n");
		if (tTf(44, 0))
			prvect(parmc, parmv);
	}
#endif

	ret = 0;
	pv = parmv;
	/* init getuser for modes HELP_RELINFO & HELP_MANSEC */
	getuser((char *) -1, (char *) NULL);
	while (pv->pv_type != PV_EOF) {
		mode = (pv++)->pv_val.pv_int;
		if (mode < HELP_RELLIST && pv->pv_type == PV_EOF)
			syserr("help: mode %d no val", mode);

#ifdef xZTR1
		if (tTf(44, -1)) {
			printf("help %d", mode);
			if (mode != HELP_RELLIST)
				printf(" %s", pv->pv_val.pv_str);
			putchar('\n');
		}
#endif
		switch (mode) {

		  case HELP_RELINFO:	/* help relation */
			if (!openr(&des, OR_RELTID, pv->pv_val.pv_str)) {
				rel_fmt((relation_t *) &des);
				pv->pv_val.pv_str = NULL;
			}
			pv++;
			break;

		  case HELP_MANSEC:	/* help manual section */
			if (man(pv->pv_val.pv_str))
				pv->pv_val.pv_str = NULL;
			pv++;
			break;

		  case HELP_DELLIST:	/* help delim */
			if (!openr(&des, OR_READ, "rdelim")) {
				if ((i = helpdelim(pv->pv_val.pv_str,&des)) == -1)
					return(error(RDELIMERR, pv->pv_val.pv_str, 0));
				closer(&des);
				pv->pv_val.pv_str = NULL;
			}
			pv++;
			break;

		  case HELP_RELLIST:
		  case HELP_ALLRELINFO:
			relpr(mode);
			break;

		  case HELP_ALLDELLIST:
			if (!openr(&des, OR_READ, "rdelim")) {
				if ((i = alldelims(&des)) == -1)
					return(error(RDELIMERR, pv->pv_val.pv_str, 0));
				closer(&des);
				pv->pv_val.pv_str = NULL;
			}
			pv++;
			break;

		  default:
			syserr("HELP: mode %d", mode);
		}
	}
	getuser(0, (char *) NULL);	/* close getuser in case mode HELP_RELINFO or HELP_MANSEC */

	/* now rescan for error messages */
	pv = parmv;
	while (pv->pv_type != PV_EOF) {
		mode = (pv++)->pv_val.pv_int;

		if (mode < HELP_RELLIST) {
			if (pv->pv_val.pv_str != NULL)
				ret = nferror(NORELEXIST + mode, pv->pv_val.pv_str, 0);
			pv++;
		}
	}
	return (ret);
}
