#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif

/* unistd.h defines _POSIX_VERSION on POSIX.1 systems.  */
#if defined(DIRENT) || defined(_POSIX_VERSION)
#include <dirent.h>
#define NLENGTH(dirent) (strlen((dirent)->d_name))
#else /* not (DIRENT or _POSIX_VERSION) */
#define dirent direct
#define NLENGTH(dirent) ((dirent)->d_namlen)
#ifdef SYSNDIR
#include <sys/ndir.h>
#endif /* SYSNDIR */
#ifdef SYSDIR
#include <sys/dir.h>
#endif /* SYSDIR */
#ifdef NDIR
#include <ndir.h>
#endif /* NDIR */
#endif /* not (DIRENT or _POSIX_VERSION) */

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#include <sys/stat.h>

#include <stdio.h>
#include <errno.h>
#include <ctype.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#include <ingres.h>
#include <aux.h>
#include <version.h>
#include <access.h>
#include <symbol.h>
#include <pv.h>
#include "sccs.h"
#include <ctlmod.h>

#define INGRES_IUTIL
#define INGRES_GUTIL
#define INGRES_DBU
#include "protos.h"

SCCSID(@(#)creatdb.c	8.7	10/7/86)

/*
**  CREATDB -- create database (or modify database status)
**
**	This program creates a new database.  It takes the name of
**	the new database (syntax defined below) and a series of
**	flags (also defined below).
**
**	In order to perform this command, you must be enabled by
**	having the U_CREATDB bit set in the user status word
**	of the users file.
**
**	The -m flag specifies that the directory for the database
**	already exists.  It stands for "mounted-file-system",
**	because this is presumably when you might use this feature.
**	The directory must be empty.
**
**	The -e flag specifies that the database already exists.
**	It must be in all ways a valid database.  This mode allows
**	you to turn flags on and off, as controlled by the other
**	flags.
**
**	Usage:
**		creatdb [flags] databasename
**
**	Positional Parameters:
**		databasename -- the name of the database to create.
**			It must conform to all the usual rules
**			about names.  Notice that this is more
**			restrictive than UNIX usually is:  names
**			must begin with an alpha, and must be
**			composed of alphanumerics.  It may be
**			at most MAXFILENAMESIZ characters long.  Underscore
**			counts as an alphabetic.
**
**	Flags:
**		-m
**			This is a mounted filesystem.  Actually,
**			this just means that the directory in which
**			the database is to reside already exists.
**			It must be empty.
**		-e
**			This database exists.  When the -e flag is
**			specified, the database is brought up to date,
**			rather than created.  Things which may be
**			changed with the -e flag is anything that
**			affects the database status or the relation
**			status bits.
**		-uXX
**			Run as user XX (usercode or login name).  This
**			flag may only be used by the INGRES superuser.
**			Normally, the database administrator for the
**			new database is the user who performs the
**			command, but the -u flag allows INGRES to
**			give the database to someone else.  Thus, it
**			is possible for someone to be a DBA without
**			having the U_CREATDB bit set.
**		-Fpathname
**			Use the named file as the database template.
**			This is, of course, for debugging use only.
**		+-c
**			Turn concurrency control on/off.  The default
**			for a new database depends on the dbtmplt file,
**			but as of this writing it defaults on.
**		+-q
**			Turn query modification on/off.
**		+-l
**			Turn protection violation logging on/off.
**
**	Files:
**		.../files/dbtmplt<VERSION>
**			This file drives the entire program.  The
**			syntax of this file is defined below in
**			readdbtemp().  Briefly, it tells the database
**			status, the relations in an 'empty' database,
**			and the status and format of those relations.
**		.../data/base
**			This directory is the directory in which all
**			databases eventually reside.  Each database is
**			a subdirectory of this directory.
**
**	Return Codes:
**		zero -- success
**		else -- failure.
**
**	Defined Constants:
**		MAXRELNS
**			This defines the maximum number of relations
**			which may be declared in the dbtmplt file.
**		MAXDBTEMP
**			The maximum size of the dbtmplt file.  This
**			determines the maximum number of characters
**			which may be in the file.
**
**	Compilation Flags:
**		xB_UNIX -- if defined, says that we have a "Berkeley
**			UNIX" system, with no group id's.  If
**			undefined, specifies either a version seven
**			UNIX (with 16-bit group id's) or version six
**			UNIX (with 8-bit group id's); either way,
**			"setgid(getgid())" will work.
**
**	Trace Flags:
**		-Tn, as below:
**
**		1 -- makereln()
**		2 -- create()
**		10 -- makeadmin()
**		12 -- makefile()
**		20 -- makedb()
**
**	Compilation Instructions:
**		% setup creatdb
**
**		- which translates to -
**
**		% cc -n -O creatdb.c error.c ../../lib/libdbu.a \
**			../../lib/access ../../lib/utility
**		% chmod 4711 a.out
*/


struct _cm_t	Cm;		/* the system topography map */
struct _ctx_t	Ctx;		/* the current context */
int		Syncs[CM_MAXPROC];/* expected SYNC's from each proc */

/* General System Information */
  jmp_buf		CmReset;	/* restart addr on interrupt */

#ifdef xMONITOR
monitor_t	CmMonBuf;	/* monitor buffer for CM overhead */
#endif /* xMONITOR */

#define	MAXRELNS	20
#define	MAXDBTEMP	2000

/* relation & attribute reln descriptors used in access methods */
/*  extern desc_t	Reldes;        */
desc_t Reldes;
/*  extern desc_t	Attdes;        */
desc_t Attdes;
/*   extern admin_t	Admin;      */

desc_t		Btreesec;	/* desc for btree sec. structure */
char		*Fileset;
int		Dbstat;		/* database status */
int		Dbson, Dbsoff;	/* same: bits turned on, bits turned off */

typedef struct reldes {
	int	bitson;
	int	bitsoff;
	paramv_t	parmv[PV_MAXPC];
} RELDES;

RELDES	Rellist[MAXRELNS];
char		Delim;
extern char	*Dbpath;
short		tTdbu[100];

/*
**  FLAGLKUP -- look up user flag
**
**	This routine helps support a variety of user flags.  The
**	routine takes a given user flag and looks it up (via a
**	very crude linear search) in the 'Flags' vector, and
**	returns a pointer to the value.
**
**	The 'flag' struct defines the flags.  The 'flagname' field
**	is the character which is the flag id, for example, 'c'
**	in the flag '-c'.  The 'flagtype' field defines how the
**	flag may appear; if negative, only '-c' may appear, if
**	positive, only '+c' may appear; if zero, either form may
**	appear.  Finally, the 'flagval' field is the value of the
**	flag -- it may default any way the user wishes.
**
**	Parameters:
**		flagname -- the name (as defined above) of the
**			flag to be looked up.
**		plusminus -- a character, '+' means the '+x' form
**			was issued, '-' means the '-x' form was
**			issued, something else means *don't care*.
**			If an illegal form was issued (that is,
**			that does not match the 'flagtype' field
**			in the structure), the "not found" return
**			is taken.
**
**	Returns:
**		NULL -- flag not found, or was incorrect type,
**			as when the '+x' form is specified in the
**			parameters, but the 'Flags' struct says
**			that only a '-x' form may appear.
**		else -- pointer to the 'flagval' field of the correct
**			field in the 'Flags' vector.
**
**	Side Effects:
**		none
**
**	Called By:
**		main
**		flagval
**
**	Trace Flags:
**		none
*/

struct flag {
	char	flagname;	/* the name of the flag */
	char	flagtype;	/* -1: -x form; +1: +x form; 0: both */
	int	flagval;	/* user-defined value of the flag */
};

struct flag	Flags[] = {
	{ 'q',	0,	0 },
	{ 'l',	0,	0 },
	{ 'c',	0,	0 },
	{ 'e',	-1,	0 },
	{ 'm',	-1,	0 },
	{ 0 }
};

int *
flaglkup(char flagname, char plusminus)
{
	register char		f;
	register struct flag	*p;
	register char		pm;

	f = flagname;
	pm = plusminus;

	/* look up flag in vector */
	for (p = Flags; p->flagname != f; p++) {
		if (p->flagname == 0)
			return (NULL);
	}

	/* found in list -- check type */
	if ((pm == '+' && p->flagtype < 0) ||
	    (pm == '-' && p->flagtype > 0))
		return (NULL);
	
	/* type is OK -- return pointer to value */
	return (&p->flagval);
}

/*
**  FLAGVAL -- return value of a flag
**
**	Similar to 'flaglkup', except that the value is returned
**	instead of the address, and no error return can occur.
**
**	Parameters:
**		f -- the flag to look up (see flaglkup).
**
**	Returns:
**		The value of flag 'f'.
**
**	Side Effects:
**		none
**
**	Called By:
**		readdbtemp()
**		main()
**
**	Trace Flags:
**		none
*/
int
flagval(register char f)
{
	register char	*p;

	/* get value of flag */
	p = (char *)flaglkup(f, 0);

	/* test for error return, syserr if so */
	if (p == NULL)
		syserr("flagval: flag %c", f);

	/* return value */
	return (*p);
}

/*
**  CHECK -- check database name syntax
**
**	The name of a database is checked for validity.  A valid
**	database name is not more than MAXFILENAMESIZ characters long, begins
**	with an alphabetic character, and contains only alpha-
**	numerics.  Underscore is considered numeric.
**
**	Parameters:
**		p -- the string to check.
**
**	Returns:
**		TRUE -- ok.
**		FALSE -- failure.
**
**	Side Effects:
**		none
**
**	Called By:
**		main
**
**	Trace Flags:
**		none
*/
int
check(char *p)
{
	register char	c;

	/* check string length */
	if (strlen(p) > MAXFILENAMESIZ)
		return (FALSE);

	/* check the first character of the string for alphabetic */
	c = *p++;
	if (!islower(c)) {
		return (FALSE);
	}

	/* check the rest for alphanumeric */
	while ((c = *p++) != 0) {
		if (c == '_')
			continue;
		if (isdigit(c))
			continue;
		if (islower(c))
			continue;
		return (FALSE);
	}
	return (TRUE);
}

/*
**  MAKEFILE -- make an 'empty' file for a relation
**
**	This routine creates a file with a single (empty) page
**	on it -- it is part of the 'create' code, essentially.
**
**	Parameters:
**		rr -- a pointer to the 'reldes' structure for this
**			relation (file).
**
**	Returns:
**		none
**
**	Side Effects:
**		A file with one page is created.
**
**	Called By:
**		makedb
**		changedb
**
**	Trace Flags:
**		12
v*/
void
makefile(register RELDES *r)
{
	desc_t		d;
	register int	i;

	ingresname(r->parmv[1].pv_val.pv_str, Usercode, d.d_r.r_id);
#ifdef xSTR1
	if (tTf(12, 0))
		printf("creat %s\n", d.d_r.r_id);
#endif
	if ((d.d_fd = open(d.d_r.r_id, O_CREAT | O_TRUNC | O_WRONLY, FILEMODE)) < 0)
		syserr("creat %s", d.d_r.r_id);
	if ((i = formatpg(&d, (long) 1)) != 0)
		syserr("makefile: formatpg %d", i);
	close(d.d_fd);
}

/*
**  MAKERELN -- make a relation
**
**	This is the second half of the create, started by 'makefile'.
**
**	This routine just sets up argument vectors and calls create,
**	which does the real work.
**
**	Parameters:
**		rr -- a pointer to the Rellist entry for the relation
**			to be created.
**
**	Returns:
**		none
**
**	Side Effects:
**		Information will be inserted into the 'relation' and
**			'attribute' relations.
**
**	Called By:
**		makedb
**		changedb
**
**	Trace Flags:
**		1
*/
void
makereln(register RELDES *r)
{
	register int	pc;
	register paramv_t	*pv;
	int		i;

	pc = 0;
	for (pv = r->parmv; pv->pv_type != PV_EOF; pv++)
		pc++;
	pv = r->parmv;
	i = create(pc, pv);
	if (i != 0)
		syserr("create %d", i);
}

/*
**  CHANGEDB -- change status bits for database/relations
**
**	In this function we change the status bits for use with the
**	-e flag.
**
**	This module always uses the differential status
**	change information, so that existing bits are not touched.
**
**	We check to see that invalid updates, such as turning off
**	query modification when it is already on, can not occur.
**	This is because of potential syserr's when the system is
**	later run, e.g., because of views without instantiations.
**
**	In the second step, the database status is updated.  This is
**	done strictly in-core, and will be updated in the database
**	after we return.
**
**	The list of valid relations are then scanned.  For each
**	relation listed, a series of steps occurs:
**
**	(1) The relation is checked for existance.  If it does not
**	exist, it is created, and we return to the beginning of the
**	loop.  Notice that we don't have to change modes in this
**	case, since it already has been done.
**
**	(2) If the relation does exist, we check to see that it
**	is a system catalog.  If it is not, we have an error, since
**	this is a user relation which just happenned to have the
**	same name.  We inform the user and give up.
**
**	(3) If the relation exists, is a system catalog, and all
**	that, then we check the changes we need to make in the
**	bits.  If no change need be made, we continue the loop;
**	otherwise, we change the bits and replace the tuple in
**	the relation relation.
**
**	(4) If the relation being updated was the "relation" or
**	"attribute" relation, we change the Admin struct accordingly.
**
**	Notice that the result of all this is that all relations
**	which might ever be used exist and have the correct status.
**
**	Notice that it is fatal for either the attribute or relation
**	relations to not exist, since the file is created at the
**	same time that relation descriptors are filled in.  This
**	should not be a problem, since this is only called on an
**	existing database.
**
**	As a final note, we open the attribute relation cache not
**	because we use it, but because we want to do a closer
**	in main() to insure that the tupcount is updated in all
**	cases.
**
**	Parameters:
**		none
**
**	Returns:
**		none
**
**	Side Effects:
**		The database is brought up to date, as described
**			above.
**		Tuples may be added or changed in system catalogs.
**		Files may be created.
**
**	Called By:
**		main
**
**	Trace Flags:
**		40
*/
void
changedb(void)
{
	register RELDES	*r;
	relation_t	relk, relt;
	tid_t		tid;
	register int	i;

#ifdef xSTR1
	if (tTf(40, 0))
		printf(">>> changedb: Dbson=%o, Dbsoff=%o\n", Dbson, Dbsoff);
#endif

	/* check to see we aren't doing anything illegal */
	if (flagval('q') < 0) {
		syserr(0, "I'm sorry, it is not possible to turn query modification off");
	}

	/* update the database status field */
	Admin.ad_h.adm_flags = (Admin.ad_h.adm_flags | Dbson) & ~Dbsoff;

	/* open the system catalog caches */
	opencatalog("relation", OR_WRITE);
	opencatalog("attribute", OR_READ);

	/* scan the relation list:- Rellist */
	for (r = Rellist; r->parmv[1].pv_type != PV_EOF; r++) {
		/* see if this relation exists */
		clearkeys(&Reldes);
		ingres_setkey(&Reldes, &relk, r->parmv[1].pv_val.pv_str, RELID);
		i = getequal(&Reldes, &relk, &relt, &tid);

		if (i < 0)
			syserr("changedb: getequal");

		if (i > 0) {
			/* doesn't exist, create it */
			printf("Creating relation %s\n", r->parmv[1].pv_val.pv_str);
			makefile(r);
			makereln(r);
		} else {
			/* exists -- check to make sure it is the right one */
			if ((relt.r_status & S_CATALOG) == 0) {
				/* exists as a user reln -- tough luck buster */
				printf("Relation %s already exists -- I cannot bring this database\n", r->parmv[1].pv_val.pv_str);
				printf("  up to date.  Sorry.\n");
				exit(3);
			}

			/* it exists and is the right one -- update status */
			if (r->bitson == 0 && r->bitsoff == 0)
				continue;

			/* actual work need be done */
			relt.r_status = (relt.r_status | r->bitson) & ~r->bitsoff;

			/* replace tuple in relation relation */
			i = replace(&Reldes, &tid, &relt, FALSE);
			if (i != 0)
				syserr("changedb: replace %d", i);

			/* update Admin struct if "relation" or "attribute" */
			if (strcmp(r->parmv[1].pv_val.pv_str, "relation") == 0)
				Admin.ad_rel.d_r.r_status = relt.r_status;
			else if (strcmp(r->parmv[1].pv_val.pv_str, "attribute") == 0)
				Admin.ad_attr.d_r.r_status = relt.r_status;
		}
	}
}

/*
**  Rubout processing.
*/
void
rubproc(void)
{
	exit(-2);
}

/*
**  ROCTAL -- Read an octal number from standard input.
**
**	This routine just reads a single octal number from the standard
**	input and returns its value.  It will only read up to a non-
**	octal digit.  It will also skip initial and trailing blanks.
**	'Delim' is set to the next character in the input stream.
**
**	Parameters:
**		none
**
**	Returns:
**		value of octal number in the input stream.
**
**	Side Effects:
**		'Delim' is set to the delimiter which terminated the
**			number.
**		File activity on stdin.
**
**	Called By:
**		getstat()
**
**	Trace Flags:
**		none
*/
int
roctal(void)
{
	register int	c;
	register int	val;

	val = 0;

	/* skip initial blanks */
	while ((c = getchar()) == ' ')
		continue;

	/* get numeric value */
	while (c >= '0' && c <= '7') {
		val = (val << 3) | (c - '0');
		c = getchar();
	}

	/* skip trailing blanks */
	while (c == ' ')
		c = getchar();

	/* set Delim and return numeric value */
	Delim = c;
	return (val);
}
/*
**  GETSTAT -- Get status word
**
**	A status word is read from the standard input (presumably
**	'dbtmplt').  The string read is interpreted as an octal
**	number.  The syntax is:
**		N{+c+N[~N]}
**	where N is a number, + is a plus or a minus sign, and c is
**	a flag.  '+c+N1[~N2]' groups are interpreted as follows:
**	If flag 'c' is set (assuming the preceeding character is a +,
**	clear if it is a -), then set (clear) bits N1.  If tilde N2
**	is present, then if flag 'c' is unset (called as '-c' ('+c'))
**	clear (set) bits N2; if ~N2 is not present, clear (set)
**	bits N1.
**
**	For example, an entry might be (but probably isn't):
**		1-c-1+q+6~2
**	having the following meaning:
**
**	1. Default to the 1 bit set.
**
**	2. If the -c flag is specified, clear the '1' bit.  If the
**	+c flag is specified, set the '1' bit.  If it is unspecified,
**	leave the '1' bit alone.
**
**	3. If the +q flag is specified, set the '2' bit and the '4'
**	bit.  If the -q flag is specified, clear the '2' bit (but leave
**	the '4' bit alone).  If the +-q flag is unspecified, leave
**	those bits alone.
**
**	Thus, a database with this entry is initially created with
**	the 1 bit on.  The '4' bit is a history, which says if the
**	'q' flag has ever been set, while the '2' bit tells if it is
**	currently set.
**
**	Parameters:
**		def -- the default to return if there is no number
**			there at all.
**		bitson -- a pointer to a word to contain all the
**			bits to be turned on -- used for the -e flag.
**		bitsoff -- same, for bits turned off.
**
**	Returns:
**		The value of the status word.
**		There are no error returns.
**
**	Side Effects:
**		File activity.
**
**	Called By:
**		readdbtemp
**
**	Trace Flags:
**		none
*/
int
getstat(int def, int *bitson, int *bitsoff)
{
	register int	c;
	register int	stat;
	register int	i;
	int		setbits;
	int		clrbits;
	char		ctlch;

	/* reset bits being turned on and off */
	*bitson = *bitsoff = 0;

	/* check to see if a base status wolushs defined */
	if (Delim == '\n' || (Delim = c = getchar()) < '0' || c > '7') {
		/* no, use default */
		stat = def;
	} else {
		/* read base status field */
		ungetc(c, stdin);
		stat = roctal();
	}

	/* scan '+c+N' entries */
	for (;;) {
		/* check for a flag present */
		c = Delim;
		if (c == '\n' || c == ':')
			return (stat);
		if (c != '+' && c != '-') {
		badfmt:
			syserr("getstat: bad fmt %c", c);
		}
		
		/* we have some flag -- get it's value */
		i = flagval(getchar());

		/* save sign char on flag */
		ctlch = c;

		/* get sign on associated number and the number */
		c = getchar();
		if (c != '+' && c != '-')
			goto badfmt;
		setbits = roctal();

		/* test whether action on -X same as on +X */
		if (Delim == '~') {
			/* they have different bits (see above) */
			clrbits = roctal();
		} else {
			/* on 'creatdb -e -X', use opposite bits of +X */
			clrbits = setbits;
		}

		/* test for any effect at all */
		if (i == 0)
			continue;

		/* test whether we should process the '+N' */
		if ((ctlch == '-') ? (i < 0) : (i > 0))
			i = setbits;
		else {
			i = clrbits;

			/* switch sense of bit action */
			if (c == '+')
				c = '-';
			else
				c = '+';
		}

		if (c == '+') {
			stat |= i;
			*bitson |= i;
		} else {
			stat &= ~i;
			*bitsoff |= i;
		}
	}
}

/*
**  GETNAME -- get name from standard input
**
**	This function reads a name from the standard input.  A
**	name is defined as a string of letters and digits.
**
**	The character which caused the scan to terminate is stored
**	into 'Delim'.
**
**	Parameters:
**		ptr -- a pointer to the buffer in which to dump the
**			name.
**
**	Returns:
**		The length of the string.
**
**	Side Effects:
**		File activity on standard input.
**
**	Called By:
**		readdbtemp
**
**	Trace Flags:
**		none
*/
int
getname(char *ptr)
{
	register int	len;
	register int	c;
	register char	*p;

	len = 0;

	for (p = ptr; (c = getchar()) != EOF; len++) {
		/* check for end of name */
		if (!isalnum(c) && c != '_') {
			break;
		}

		/* store character into buffer */
		*p++ = c;
	}

	/* null-terminate the string */
	*p = '\0';

	/* store the delimiting character and return length of string */
	Delim = c;
	return (len);
}
/*
**  READDBTEMP -- read the dbtmplt file and build internal form
**
**	This routine reads the dbtmplt file (presumably openned as
**	the standard input) and initializes the Dbstat (global database
**	status word) and Rellist variables.
**
**	Rellist is an array of argument vectors, exactly as passed to
**	'create'.
**
**	The syntax of the dbtmplt file is as follows:
**
**	The first line is a single status word (syntax defined below)
**	which is the database status.
**
**	The rest of the file is sets of lines separated by blank lines.
**	Each set of lines define one relation.  Two blank lines in a
**	row or an end-of-file define the end of the file.  Each set
**	of lines is broken down:
**
**	The first line is in the following format:
**		relname:status
**	which defines the relation name and the relation status for
**	this relation (syntax defined in 'getstat' below).  Status
**	may be omitted, in which case a default status is assumed.
**
**	Second through n'th lines of each set define the attributes.
**	They are of the form:
**		attname		format
**	separated by a single tab.  'Format' is the same as on a
**	'create' statement in QUEL.
**
**	Notice that we force the S_CATALOG bit to be on in any
**	case.  This is because the changedb() routine will fail
**	if the -e flag is ever used on this database if any
**	relation appears to be a user relation.
**
**	Parameters:
**		none
**
**	Returns:
**		none
**
**	Side Effects:
**		Dbstat gets the database status.
**		Rellist is created with a list of the relations,
**			(as parameter vectors -01:2st as passed to
**			create).  The entry after the last entry
**			has its pv[0] == NULL.
**
**	Called By:
**		main
**
**	Trace Flags:
**		none
*/
void
readdbtemp(void)
{
	static char	buf[MAXDBTEMP];
	register RELDES	*r;
	register paramv_t	*q;
	register int	i;
	int		j;
	char		*p;
	int		defrstat;
	int	bitson, bitsoff;

	/* read database status */
	defrstat = S_CATALOG | S_NOUPDT | S_CONCUR | S_PROTALL;
	bitson = bitsoff = 0;
	Dbstat = getstat(A_DBCONCUR, &Dbson, &Dbsoff);
	if (Delim == ':')
		defrstat = getstat(defrstat, &bitson, &bitsoff);
	if (Delim != '\n')
		syserr("readdbtemp: bad Dbstat %c", Delim);

	/* compute default relation status */

	/* start reading relation info */
	p = buf;
	for (r = Rellist; ; r++) {
		r->bitson = bitson;
		r->bitsoff = bitsoff;

		q = r->parmv;

		/* get relation name */
		q[1].pv_type = PV_STR;
		q[1].pv_val.pv_str = p;
		p += getname(p) + 1;

		/* test for end of dbtmplt file */
		if (q[1].pv_val.pv_str[0] == 0)
			break;
		
		/* get relation status */
		i = getstat(defrstat, &r->bitson, &r->bitsoff);
		i |= S_CATALOG;		/* guarantee system catalog */
		q->pv_type = PV_STR;
		q++->pv_val.pv_str = p;
		*p++ = ((i >> 15) & 1) + '0';
		for (j = 12; j >= 0; j -= 3) {
			*p++ = ((i >> j) & 07) + '0';
		}
		*p++ = 0;
		q++;
		if (Delim != '\n')
			syserr("readdbtemp: bad rel %c", Delim);
		
		/* read attribute names and formats */
		for (;;) {
			/* get attribute name */
			q->pv_type = PV_STR;
			q++->pv_val.pv_str = p;
			p += getname(p) + 1;
			if (q[-1].pv_val.pv_str[0] == 0) {
				break;
			}
			if (Delim != '\t') {
				syserr("readdbtemp: bad att %s, d='%c'",
				    q[-1].pv_val.pv_str, Delim);
			}
			
			/* get attribute type */
			q->pv_type = PV_STR;
			q++->pv_val.pv_str = p;
			p += getname(p) + 1;
			if (Delim != '\n')
				syserr("readdbtemp: bad type %c", Delim);
		}

		/* insert end of argv signal */
		(--q)->pv_type = PV_EOF;

		/* ad-hoc overflow test */
		if (p >= &buf[MAXDBTEMP])
			syserr("readdbtemp: overflow");
	}
	/* mark the end of list */
	q[1].pv_type = PV_EOF;
}

/*
**  MAKEADMIN -- manually initialize descriptor for admin file
**
**	The relation descriptor pointed to by 'pv' is turned into
**	a descriptor, returned in 'd'.  Presumably, this descriptor
**	is later written out to the admin file.
**
**	Notice that the 'd_tid' field is filled in sequentially.
**	This means that the relations put into the admin file
**	must be created in the same order that they are 'made'
**	(by this routine), that the format of tid's must not
**	change, and that there can not be over one page worth of
**	relations in the admin file.  Our current system currently
**	handles this easily.
**
**	Parameters:
**		d -- the descriptor to get the result.
**		pv -- a parm vector in 'create' format, which drives
**			this routine.
**
**	Returns:
**		none
**
**	Side Effects:
**		none
**
**	Called By:
**		main
**
**	Trace Flags:
**		10
*/
void
makeadmin(desc_t *d, paramv_t *pv)
{
	register desc_t	*des;
	register paramv_t	*p;
	register int	i;
	int	len;
	static int	lineno;
	char		fname[MAX_NAME_SIZE + 3];

	des = d;
	p = pv;

#ifdef xSTR2
	if (tTf(10, -1)) {
		printf("creating %s in admin\n", p[1].pv_val.pv_str);
	}
#endif
	i = oatoi(p++->pv_val.pv_str);
	ingresname(p++->pv_val.pv_str, Usercode, fname);
	bmove(fname, des->d_r.r_id, MAX_NAME_SIZE + 2);
	des->d_r.r_status = i;
	des->d_r.r_attrc = 0;
	des->d_r.r_width = 0;
	des->d_r.r_spec = M_HEAP;
	(void) memset(&des->d_tid, 0, sizeof(des->d_tid));
	des->d_tid.line_id = lineno++;
	des->d_fd = open(fname, O_RDWR);
	if (des->d_fd < 0)
		syserr("makeadmin: open %s", fname);
	des->d_opened = (des->d_fd + 1) * -5;

	/* initialize domain info */
	for (; p++->pv_type != PV_EOF; p++) {
		register char	c;

		c = p[0].pv_val.pv_str[0];
		if (c != INT_CONST && c != CHAR_CONST && c != FLOAT_CONST) {
			syserr("dbtmplt: type err on %s", p[0].pv_val.pv_str);
		}
		des->d_fmt[++(des->d_r.r_attrc)] = c;
		len = atoi(&p[0].pv_val.pv_str[1]); 
		des->d_len[des->d_r.r_attrc] = len;
#ifdef ADDR_ROUNDUP
		if (c != CHAR_CONST)
			des->d_r.r_width=((des->d_r.r_width-1)|(len-1))+1;
#endif
		des->d_off[des->d_r.r_attrc] = des->d_r.r_width;
		des->d_r.r_width += len;
	}
}

/*
**  MAKEDB -- make a database from scratch
**
**	This is the code to make a database if the -e flag is off.
**
**	The first step is to make a copy of the admin file
**	in the internal 'Admin' struct.  This is the code which
**	subsequently gets used by openr and opencatalog.  Notice
**	that the admin file is not written out; this happens after
**	makedb returns.
**
**	Next, the physical files are created with one initial (empty)
**	page.  This has to happen before the 'create' call so
**	that it will be possible to flush 'relation' and 'attribute'
**	relation pages during the creates of the 'relation' and
**	'attribute' relations.  Other relations don't need this,
**	but it is more convenient to be symmetric.
**
**	The next step is to create the relations.  Of course, all
**	this really is is inserting stuff into the system catalogs.
**
**	When we are all done we open the relation relation for the
**	admin cache (which of course should exist now).  Thus,
**	the closer's in main (which must be around to update the
**	tuple count) will work right.
**
**	Parameters:
**		none
**
**	Returns:
**		none
**
**	Side Effects:
**		A database is created!!
**		Several files will be created in the current directory,
**			one for each relation mentioned in the
**			'dbtmplt' file.
**		The 'Admin' struct will be filled in.
**
**	Called By:
**		main
**
**	Trace Flags:
**		20
*/
void
makedb(void)
{
	register RELDES	*r;

#ifdef xSTR3
	if (tTf(51, 0))
		printf(">>makedb, Usercode = %s (%p)\n", Usercode, Usercode);
#endif

	/* create the physical files */
	for (r = Rellist; r->parmv[1].pv_type != PV_EOF; r++) {
		makefile(r);
	}

	/* initialize the admin file internal cache */
	bmove(Usercode, Admin.ad_h.adm_owner, USERCODE_SIZE);
	Admin.ad_h.adm_flags = Dbstat;
	makeadmin(&Admin.ad_rel, Rellist[0].parmv);
	makeadmin(&Admin.ad_attr, Rellist[1].parmv);

	/* done with admin initialization */

	/* initialize relations */
	for (r = Rellist; r->parmv[1].pv_type != PV_EOF; r++) {
		makereln(r);
	}
}

#if 1
/*
**  READADMIN -- read the admin file into the 'Admin' cache
**
**	This routine opens and reads the 'Admin' cache from the
**	'admin' file in the current directory.
**
**	This version of the routine is modified for creatdb --
**	the '-e' flag is checked, and nothing is performed
**	unless it is set.
**
**	If not set, the 'relation' and 'attribute' relations
**	are opened, and the descriptors for them in the Admin
**	struct are filled in with their file descriptors.
**
**	Parameters:
**		none
**
**	Returns:
**		none
**
**	Side Effects:
**		The 'Admin' struct is filled in.
**		The 'relation...xx' and 'attribute...xx' files are
**			opened.
**
**	Called By:
**		acc_init (accbuf.c)
**		changedb
**
**	Trace Flags:
**		none
*/
void
readadmin(int fake)
{
	register int	i;
	char		relname[MAX_NAME_SIZE + 4];

	/* read the stuff from the admin file */
	if (flagval('e')) {
		i = open("admin", O_RDONLY);
		if (i < 0) {
			syserr("readadmin: open admin %d", i);
		}
		checkadmin(i);
		close(i);

		/* open the physical files for 'relation' and 'attribute' */
		ingresname("relation", Admin.ad_h.adm_owner, relname);
		if ((Admin.ad_rel.d_fd = open(relname, O_RDWR)) < 0) {
			syserr("readadmin: open `%.14s'", relname);
		}
		ingresname("attribute", Admin.ad_h.adm_owner, relname);
		if ((Admin.ad_attr.d_fd = open(relname, O_RDWR)) < 0) {
			syserr("readadmin: open `%.14s'", relname);
		}
		Admin.ad_rel.d_opened = (Admin.ad_rel.d_fd + 1) * -5;
		Admin.ad_attr.d_opened = (Admin.ad_attr.d_fd + 1) * 5;
	}
}
#endif

void
main(int argc, char **argv)
{
	register int	i;
	int		j;
	int		admin;
	int		code;
	char		*database;
	register char	*p;
	int		faterr;
	register int	*flagv;
	char		*dbtmpfile;
	int		exists;

	DIR			*dirp;		/* pointer to '.' */
	struct dirent		*dp;			/* directory entry */

	argv[argc] = NULL;

#ifdef xSTR1
	tTrace(argv, 'T', tTdbu, 100);
#endif

	/*
	**  Do a lot of magic initialization.
	**
	**	'exists' get set to -1 if the database does not exist
	**		at all, 1 if it exists, and 0 if it does not
	**		exist but there is an indirect pointer to it.
	*/

	exists = 0;
	i = initucode(argc, argv, TRUE, NULL, -1);
	switch (i) {
	  case 0:
	        break;

	  case 5:
		exists = 1;
		break;

	  case 6:
		exists = 0;

	  case 1:
		break;

	  case 2:
		printf("You are not authorized to create this database\n");
		exit(-1);

	  case 3:
		printf("You are not a valid INGRES user\n");
		exit(-1);

	  case 4:
		printf("No database name specified\n");
	usage:
		printf("Usage: creatdb [-uname] [-e] [-m] [+-c] [+-q] dbname\n");
		exit(-1);

	  default:
		syserr("initucode %d", i);
	}

	faterr = 0;
	dbtmpfile = 0;
	for (j = 0 ; (p = getflagvect(j)) != (char *) NULL ; j++) {
		if (p[0] != '-' && p[0] != '+')
			syserr("flag %s", p);
		switch (p[1]) {
		  case 'F':		/* dbtmplt file */
			if (p[2] == 0)
				goto badflag;
			dbtmpfile = &p[2];
			break;

		  case 'T':		/* trace flag */
			break;
		
		  default:
			if ((flagv = flaglkup(p[1], p[0])) != 0) {
				if (p[0] == '+')
					*flagv = 1;
				else
					*flagv = -1;
				continue;
			}
		badflag:
			printf("bad flag %s\n", p);
			faterr++;
			continue;

		}
		if (p[0] == '+')
			goto badflag;
	}

	/* check for legality of database name */
	database = getparmvect(0);
	if (getparmvect(1) != NULL) {
		printf("Too many parameters to creatdb");
		goto usage;
	}
	if (!check(database)) {
		printf("Illegal database name %s\n", database);
		exit(-1);
	}

	if ((getglobalint(STATUS_NAME) & U_CREATDB) == 0) {
		printf("You are not allowed this command\n");
		exit(-1);
	}

	/* end of input checking */
	if (faterr != 0)
		exit(2);

	/* now see if it should have been there */
	if (flagval('m') || flagval('e')) {
		if (exists <= 0) {
			printf("Database %s does not exist\n", database);
			exit(-1);
		}

#ifdef xSTR3
		if (tTf(1, 14))
			printf("Dbpath = '%s'\n", Dbpath);
#endif
		if (chdir(Dbpath) < 0)
			syserr("chdir %s", Dbpath);
		if (!flagval('e')) {
			/* make certain that it is empty */
			if ( (dirp = opendir(".")) == NULL )
				syserr(0,"Can't open '.'");
			while ((dp = readdir(dirp)) != (struct dirent *) NULL) {
				if ( strcmp(".",dp->d_name) && strcmp("..",dp->d_name) )
					syserr(0, "%s is not empty", database);
			}
			closedir(dirp);
		} else {
			/* check for correct owner */
			acc_init(0, !flagval('e'));
			if (!bequal(Usercode, Admin.ad_h.adm_owner, USERCODE_SIZE)) {
				syserr(0, "You are not the DBA for this database");
			}
		}
	} else {
		if (exists > 0) {
			printf("Database %s already exists\n", database);
			exit(-1);
		}

		/* create it */
		i = fork();
		if (i < 0)
			syserr("fork err");
		if (i == 0) {
			/* split off directory */
			*(p = strrchr(Dbpath, '/')) = '\0';
			chdir(Dbpath);
			if (setuid(getuid()))
				syserr("setuid");
			if (setgid(getgid()))
				syserr("setgid");
			umask(00);
			*p++ = '/';
			execl("/bin/mkdir", "/bin/mkdir", p, 0);
			execl("/usr/bin/mkdir", "/usr/bin/mkdir", p, 0);
			syserr("exec /bin/mkdir");
		}
		while (wait(&code) != -1)
			continue;

		/* move into data/base directory */
		if (chdir(Dbpath) < 0)
			syserr("chdir %s: probably bad default mode in mkdir",
			    Dbpath);

		/* change the mode of the database */
		i = fork();
		if (i < 0)
			syserr("fork 2");
		if (i == 0) {
			setuid(getuid());
			if (chmod(".", 0777))
				syserr("chmod");
			exit(0);
		}

		while (wait(&code) != -1)
			continue;
		if ((code & I1MASK) != 0)
			exit(code);
	}

	/* reset 'errno', set from possibly bad chdir */
	errno = 0;

	/* determine name of dbtmplt file and open */
	if (dbtmpfile == NULL) {
		/* create dbtmplt file name */
		dbtmpfile = ztack(ztack(Pathname, "/files/dbtmplt"), MAJOR_VERSION);
	}
	if (freopen(dbtmpfile, "r", stdin) == NULL)
		syserr("dbtmplt open %s", dbtmpfile);
	
	readdbtemp();

	/* check for type -- update database status versus create */
	if (flagval('e'))
		changedb();
	else
		makedb();

	/* close the cache descriptors */
#ifdef xSTR3
	if (tTf(50, 0)) {
		printf("Attdes.d_tid = ");
		dumptid(&Attdes.d_tid);
		printf("Reldes.d_tid = ");
		dumptid(&Reldes.d_tid);
	}
#endif
	if ((i = closer(&Attdes)) != 0)
		syserr("creatdb: closer(att) %d", i);
	if ((i = closer(&Reldes)) != 0)
		syserr("creatdb: closer(rel) %d", i);

	/* bring tupcount in 'admin' file to date */
	Admin.ad_rel.d_r.r_tupc = Reldes.d_r.r_tupc;
	Admin.ad_attr.d_r.r_tupc = Attdes.d_r.r_tupc;

	/* set other fields as appropriate */
	Admin.ad_rel.d_fd = Admin.ad_attr.d_fd = -1;
	Admin.ad_rel.d_opened = Admin.ad_attr.d_opened = 0;
	Admin.ad_h.adm_len = sizeof(Admin.ad_h);
	Admin.ad_h.adm_rellen = Admin.ad_h.adm_attrlen = sizeof(Admin.ad_rel);
	Admin.ad_h.adm_version = DBVERCODE;

	if ((admin = open("admin", O_CREAT | O_TRUNC | O_WRONLY, FILEMODE)) < 0)
		syserr("main: creat admin");
	if (write(admin, &Admin, sizeof(Admin)) != sizeof(Admin))
		syserr("main: write Admin");
	close(admin);

	execl((ztack(Pathname, "/bin/sysmod")), "sysmod", database, 0);
	/* exit successfully */
	exit(0);
}
