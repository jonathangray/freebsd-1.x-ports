#include <sys/types.h>

#ifdef HAVE_SYS_FILE_H
#include <sys/file.h>
#endif

#include <stdio.h>
#include <errno.h>
#include <signal.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <ingres.h>
#include <access.h>
#include <pwd.h>
#include "sccs.h"

#define INGRES_GUTIL
#define INGRES_LIBQ
#include "protos.h"

SCCSID(@(#)ingconv.q	8.5 1/16/85)

/*
**  INGRES.H -- basic header file for ingres.
**
**	See also aux.h for definitions used by some but not all.
**
**	Version:
**		@(#)ingres.h	7.1	2/5/81
*/


/*
**	RELATION relation struct
**
**	The RELATION relation contains one tuple for each relation
**	in the database.  This relation contains information which
**	describes how each relation is actually stored in the
**	database, who the owner is, information about its size,
**	assorted operation information, etc.
*/

struct orelation {
	char	r_id[MAX_NAME_SIZE];	/* relation name	*/
	char	r_owner[2];	/* code of relation owner */
	i_1	r_spec;	/* storage mode of relation	*/
				/* M_HEAP  unsorted paged heap	*/
				/* -M_HEAP compressed heap	*/
				/* M_ISAM  isam			*/
				/* -M_ISAM compressed isam	*/
				/* M_HASH  hashed		*/
				/* -M_HASH compressed hash	*/
	i_1	r_indexed;	/* -1 rel is an index, 0 not indexed, 1 indexed */
	i_2	r_status2;	/* more status bits */
	i_2	r_status;	/* relation status bits */
	i_4	r_savetime;	/*unix time until which relation is saved*/
	i_4	r_tupc;	/*number of tuples in relation	*/
	i_2	r_attrc;	/*number of attributes in relation	*/
	i_2	r_width;		/*width (in bytes) of relation	*/
	i_4	r_primc;	/*no. of primary pages in relation*/
	i_4	r_free;	/* head of freelist (b-trees only) */
	i_4	r_modtime;	/*time of last mod*/
};


/*
**  Definitions for the range table.
**
**	Version:
**		@(#)range.h	7.1	2/5/81
*/



/*
**	DESCRIPTOR struct
**
**	The DESCRIPTOR struct is initialized by OPENR to describe any
**	open relation.  The first part of the descriptor is the tuple
**	from the RELATION relation.  The remainder contains some magic
**	numbers and a template initialized from the ATTRIBUTE relation.
**
**	This structure also defines the range table.
*/

struct odescriptor {
	struct orelation	d_r;
		/*the above part of the descriptor struct is identical
		  to the relation struct and the inormation in this
		  part of the struct is read directly from the
		  relation tuple by openr.  the rest of the descriptor
		  struct is calculated by openr.  */
	char	d_rangevar[MAX_NAME_SIZE];	/* range variable name */
	i_2	d_fd;		/*filep for relation , if open	*/
	i_2	d_opened;		/*indicates if relation is really open*/
	tid_t	d_tid;	/*when relation is open, this indicates
				  the tid in the relation relation for
				  this relation */
	i_4	d_addc;	/*no. of additions of tuples during this open*/
	i_2	d_off[MAX_DOMAINS];	/*d_off[i] is offset to domain i 	*/
	char	d_fmt[MAX_DOMAINS]; /* format of domain i
				 ** INT_CONST, FLOAT_CONST, or CHAR_CONST  */
	char	d_len[MAX_DOMAINS]; /* d_len[i] is an unsigned integer
				  which indicates length
				  in bytes of domain */
	char	d_iskey[MAX_DOMAINS]; /*d_iskey[i] is non-zero if domain i is
				 ** a key domain for the relation */
	char	d_given[MAX_DOMAINS]; /*cleared by openr and set before
				  call to find to indicate value of this
				  domain has been supplied in the key*/
};

#
/*
**  ACCESS.H -- definitions relating to the access methods.
**
**	Version:
**		@(#)access.h	7.1	2/5/81
*/


/*
**  ADMIN file struct
**
**	The ADMIN struct describes the initial part of the ADMIN file
**	which exists in each database.  This file is used to initially
**	create the database, to maintain some information about the
**	database, and to access the RELATION and ATTRIBUTE relations
**	on OPENR calls.
*/


struct oadmin {
	adminhdr_t		ad_h;
	struct odescriptor	ad_rel;
	struct odescriptor	ad_attr;
};

#if 0
/*
**  VERSION.H -- system version definition file.
**
**	NOTICE:
**		Version numbers stored in files are SCCS id's
**		and may not correspond to the external distribution
**		version number.  The distribution number applies to
**		the entire system and not to any particular file.
**		This file defines a "release" number, used for
**		creating file names.  The entire system version
**		number (including mod number) is defined by
**		conf/version.c.
**
**	Version:
**		@(#)version.h	8.1	12/31/84
*/


/*
**	VERSION is the version number of this incarnation of INGRES
**		for purposes of creating file names.
**	DBVERCODE is the code for this database version stored in
**		the admin file.
**	PATHEXT is an extension for the path as derived from the
**		"ingres" entry in the password file to determine
**		the "Pathname" variable.  If it is not defined,
**		no extension is made.
*/

#define	VERSION		"8"		/* version number */
#define	DBVERCODE	1		/* database version code */
#else
#include "version.h"
#endif

extern	struct	oadmin	OAdmin;
admin_t		Admin;
char			Ingcode[3];	/* Ingres code of ingres */
char			User[3];	/* Ingres code for DBA of this database */

void
changerels(database)
char*database;
{
char*user;int sysmod;
char*delname;

	char	data[3];
	extern	char	Ingcode[];
	extern	char	User[];

	user = data;
	user[0] = user[1] = user[2] = '\0';
	sysmod = -1;
	delname = "delim";


{IIingres(database,"-rheap",0);}
{IIwrite("range of r=relation");IIsync(0);}
{IIwrite("range of a=attribute");IIsync(0);}

{IIwrite("retrieve(sysmod=r.r_spec)where r.r_id=\"_rtempv8\"");IIsetup();while(IIn_get(0)){IIn_ret(&sysmod,6);if(IIerrtest())continue;
}IIflushtup(0);}
	if ( sysmod != -1 )
		syserr("The data base '%s' has a the relation '_rtempv8'. Please remove, or change this relation before using this program");

{IIwrite("retrieve(sysmod=r.r_spec)where r.r_id=\"_rtempv8\"");IIsetup();while(IIn_get(0)){IIn_ret(&sysmod,6);if(IIerrtest())continue;
}IIflushtup(0);}
	if ( sysmod != -1 )
		syserr("The data base '%s' has a the relation '_atempv8'. Please remove, or change this name before using this program");

{IIwrite("create _rtempv8(r_id=c12,r_owner=c2,r_spec=i1,r_indexed=i1,r_status2=i2,r_status=i2,r_savetime=i4,r_tupc");
IIwrite("=i4,r_attrc=i2,r_width=i2,r_primc=i4,r_free=i4,r_modtime=i4,r_dim=i2)");IIsync(0);}




{IIwrite("retrieve(user=r.r_owner)where r.r_id=\"relation\"");IIsetup();while(IIn_get(0)){IIn_ret(user,3);if(IIerrtest())continue;
}IIflushtup(0);}	User[0] = user[0];
	User[1] = user[1];
	User[2] = '\0';

{IIwrite("create rdelim(order=i4,group=c12,");IIwrite(delname);IIwrite("=c12,type=i4,bitmap=c16)");IIsync(0);}{IIwrite("");
IIwrite("modify rdelim to isam on order,group,");IIwrite(delname);IIwrite("");IIsync(0);}

{IIwrite("retrieve _rtempv81(r.all,r_dim=0)");IIsync(0);}
{IIwrite("range of t=_rtempv81");IIsync(0);}
{IIwrite("append _rtempv8(t.all)");IIsync(0);}
{IIwrite("destroy _rtempv81");IIsync(0);}
{IIwrite("range of t=_rtempv8");IIsync(0);}
{IIwrite("delete t where t.r_id=\"_rtempv8\"or t.r_id=\"_rtempv81\"");IIsync(0);}
{IIwrite("replace t(r_savetime=0,r_status=275,r_spec=11)where t.r_id=\"rdelim\"");IIsync(0);}
{IIwrite("replace t(r_tupc=t.r_tupc-1,r_attrc=t.r_attrc+1,r_width=t.r_width+2)where t.r_id=\"relation\"");IIsync(
0);}


{IIwrite("retrieve _atempv8(a.all)");IIsync(0);}
{IIwrite("append _atempv8(a_rel=\"relation\",a_owner=");IIcvar(user,3,0);IIwrite(",a_id=14,a_name=\"r_dim\",a_off");
IIwrite("=44,a_fmt=\"i\",a_len=2,a_iskey=0)");IIsync(0);}
{IIwrite("range of t=_atempv8");IIsync(0);}
{IIwrite("delete t where t.a_rel=\"_rtempv8\"or t.a_rel=\"_rtempv81\"or t.a_rel=\"_atempv8\"");IIsync(0);}

	sysmod = 0;

{IIwrite("retrieve(sysmod=r.r_spec)where r.relid=\"relation\"and r.r_owner=");IIcvar(user,3,0);IIwrite("");IIsetup();
while(IIn_get(0)){IIn_ret(&sysmod,6);if(IIerrtest())continue;}IIflushtup(0);}
	if ( sysmod != 5 ) {
{IIwrite("modify _rtempv8 to hash on r_id");IIsync(0);}
{IIwrite("modify _atempv8 to hash on a_rel,a_owner");IIsync(0);}
	}

	user[0] = user[1] = '\0';
{IIwrite("retrieve(user=r.r_owner)where r.r_id=\"_atempv8\"");IIsetup();while(IIn_get(0)){IIn_ret(user,3);if(IIerrtest())continue;
}IIflushtup(0);}
	if ( (Ingcode[0] = user[0]) == '\0' )
		syserr("can't find my own name");
	Ingcode[1] = user[1];
	Ingcode[2] = '\0';
{IIexit();}
	sync();
}

struct	oadmin	OAdmin;

/*
**  STARTADMIN -- starts admin file version, etc.
**
**	The checks for database version code and whatnot are
**	factored out into this routine.  When this routine returns,
**	the admin file should be legible to this program.
**	If the admin file is not legible, it will syserr.
**
**	Parameters:
**		fd -- open file descriptor for admin file.  Only
**			read access is required.
**
**	Returns:
**		nothing if ok.
**		not at all (or via syserr) if not ok.
**
**	Side Effects:
**		The OAdmin.ad_h struct will be filled in.
*/
void
startadmin(register int fd)
{
	register int	i;
	register int	k;

	i = ((char *) &OAdmin.ad_h.adm_version) - ((char *) &OAdmin.ad_h);
	if (read(fd, (char *) &OAdmin.ad_h, i) != i) {
		syserr("readadmin: admin read err 1");
	}
	if (!BITISSET(A_NEWFMT, OAdmin.ad_h.adm_flags)) {
		syserr("readadmin: cannot use old databases");
	}

	/* read in remainder of admin header */
	i = sizeof(OAdmin.ad_h);
	if (OAdmin.ad_h.adm_len < i) {
		i = OAdmin.ad_h.adm_len;
	}
	i -= ((char *) &OAdmin.ad_h.adm_version) - ((char *) &OAdmin.ad_h);
	if (i <= 0) {
		syserr("readadmin: adlen=%d, hdrsz=%d, ct=%d", OAdmin.ad_h.adm_len, sizeof(OAdmin.ad_h), i);
	}
	if ((k = read(fd, (char *) &OAdmin.ad_h.adm_version, i)) != i) {
		syserr("readadmin: admin read err 2, i=%d k=%d", i, k);
	}

	/* check versions here */
	if (OAdmin.ad_h.adm_version != DBVERCODE) {
		syserr("cannot handle code %d databases (current code is %d)",
			OAdmin.ad_h.adm_version, DBVERCODE);
	}
	if (OAdmin.ad_h.adm_rellen != sizeof(OAdmin.ad_rel)) {
		syserr("checkadmin: descriptor size mismatch, dec=%d, actual=%d",
			OAdmin.ad_h.adm_rellen, sizeof(OAdmin.ad_rel));
	}

	/* get to beginning of descriptors */
	if (lseek(fd, (off_t) OAdmin.ad_h.adm_len, 0) < 0) {
		syserr("checkadmin: seek");
	}

	/* read the descriptors */
	if (read(fd, (char *) &OAdmin.ad_rel, OAdmin.ad_h.adm_rellen) != OAdmin.ad_h.adm_rellen) {
		syserr("checkadmin: reld read sz=%d", OAdmin.ad_h.adm_rellen);
	}
	if (read(fd, (char *) &OAdmin.ad_attr, OAdmin.ad_h.adm_attrlen) != OAdmin.ad_h.adm_attrlen) {
		syserr("checkadmin: attd read sz=%d", OAdmin.ad_h.adm_attrlen);
	}
}

/*
** Assign an old descriptor to a new descriptor.
*/
void
descasign(struct odescriptor *a, struct descriptor *b)
{
	struct	orelation	*orel;
	relation_t	*rel;

	strcpy(b->d_rangevar,a->d_rangevar);
	b->d_fd = a->d_fd;
	b->d_opened = a->d_opened;
	b->d_tid = a->d_tid;
	b->d_addc = a->d_addc;
	bcopy(a->d_off,b->d_off,(sizeof(i_2)) * MAX_DOMAINS);
	bcopy(a->d_fmt,b->d_fmt,(sizeof(char)) * MAX_DOMAINS);
	bcopy(a->d_len,b->d_len,(sizeof(char)) * MAX_DOMAINS);
	bcopy(a->d_iskey,b->d_iskey,(sizeof(char)) * MAX_DOMAINS);
	bcopy(a->d_given,b->d_given,(sizeof(char)) * MAX_DOMAINS);

	orel = &a->d_r;
	rel = &b->d_r;

	bcopy(orel->r_id,rel->r_id,MAX_NAME_SIZE);
	bcopy(orel->r_owner,rel->r_owner,2);
	rel->r_spec = orel->r_spec;
	rel->r_indexed = orel->r_indexed;
	rel->r_status2 = orel->r_status2;
	rel->r_status = orel->r_status;
	rel->r_savetime = orel->r_savetime;
	rel->r_tupc = orel->r_tupc;
	rel->r_attrc = orel->r_attrc;
	rel->r_width = orel->r_width;
	rel->r_primc = orel->r_primc;
	rel->r_free = orel->r_free;
	rel->r_modtime = orel->r_modtime;
}

void
goodbye(void)
{
	exit(1);
}

/*
** Ingconv
**	Convert version 7 databases into version 8 databases.
**	We assume that there is a $P/bin7 to fake everything with.
*/
void
main(int argc, char **argv)
{

	struct	passwd	*pwd;
	int		fd;
	int		i;
	RETSIGTYPE		(*oldsigint)(int);
	RETSIGTYPE		(*oldsigquit)(int);
	char		relname[15];
	char		attname[15];
	char		orelname[15];
	char		oattname[15];
	char		buffer[1000];
	char		*path;
	int		sflag = 0;
	extern	void	(*ExitFn)();

	argc--;
	argv++;

	ExitFn = goodbye;

	if ( argc != 1 ) {
		if ( argc == 2 && strcmp(*argv,"-s") == 0 ) {
				sflag = 1;
				argc--;
				argv++;
		}
		else
			syserr("ingconv: wrong number of arguments, useage 'ingconv [-s] database'");
	}

	if ( (pwd = getpwnam("ingres")) == 0 )
		syserr("ingconv: can't find ingres in password file");

	/*
	** Wander into ~ingres/bin7, and then make sure
	** that the Ingres we get is the one here. This will
	** be a version 7 ingres.
	*/
	if ( chdir(pwd->pw_dir) == -1 )
		syserr("can't chdir to %s",pwd->pw_dir);
	if ( chdir("bin7") == -1 )
		syserr("can't chdir to bin7");
	if ( (path = getenv("PATH")) == 0 )
		syserr("No PATH environment");
	strcpy(path,":.:");

	printf("converting database '%s'\n",*argv);
	/*
	** Make new versions of the relation relation, and
	** attribute relations. Store these new things as
	** "_rtempv8" and "_atempv8".
	*/
	changerels(*argv);


	/*
	** Now we go into the data/base/database, and do some magic
	** disgusting stuff.
	*/
	if ( chdir(pwd->pw_dir) == -1 )
		syserr("can't chdir to %s",pwd->pw_dir);
	if ( chdir("data/base") == -1 )
		syserr("can't chdir to data/base");

	errno = 0;
	if ( chdir(*argv) == -1 ) {
		if ( errno == ENOTDIR )
			syserr("can't handle indirect files, change it into a symbolic link first");
		syserr("no database '%s'",*argv);
	}

	bzero(&Admin, sizeof(Admin));
	bzero(&OAdmin, sizeof(OAdmin));

	if ( (fd = open("admin",O_RDONLY)) == -1 )
		syserr("can't open admin file");

	/*
	** Read in the old admin relation, then write out a new
	** version into _admin.
	*/
	startadmin(fd);
	close(fd);
	Admin.ad_h = OAdmin.ad_h;
	descasign(&OAdmin.ad_rel, &Admin.ad_rel);
	descasign(&OAdmin.ad_attr, &Admin.ad_attr);
        Admin.ad_h.adm_rellen = Admin.ad_h.adm_attrlen = sizeof(Admin.ad_rel);

	if ( (fd = open("_admin",O_WRONLY|O_CREAT,0600)) == -1 )
		syserr("can't create '_admin' temp file");
	if ( write(fd,&Admin, sizeof(Admin)) != sizeof(Admin) )
		syserr("can't write _admin file");
	close(fd);



	strcpy(orelname,"relation    ");
	strcat(orelname,User);
	strcpy(relname,"_rtempv8    ");
	strcat(relname,Ingcode);

	strcpy(oattname,"attribute   ");
	strcat(oattname,User);
	strcpy(attname,"_atempv8    ");
	strcat(attname,Ingcode);
	/*
	** Up to now, we can be interrupted, now we come to the
	** magic stuff that might leave us undefended...
	*/
	oldsigint = signal(SIGINT,SIG_IGN);
	oldsigquit = signal(SIGQUIT,SIG_IGN);
	for ( i = 1 ; i < NSIG ; i++ )
		signal(i,SIG_IGN);

	if ( rename(attname,oattname) == -1 )
		syserr("Could not rename '%s' to '%s'",attname, oattname);
	if ( rename(relname,orelname) == -1 )
		syserr("Could not rename '%s' to '%s', database is now corrupted!!!!, restore the attribute relation from tape.",relname, orelname);
	if ( rename("_admin","admin") == -1 )
		syserr("Could not rename '_admin' to 'admin'. Database is now corrupted, read the attribute and relation relations in from tape.");

	/*
	** Turn the interrupts back on, now that the
	** critical section is over.
	*/
	for ( i = 1 ; i < NSIG ; i++ )
		signal(i,SIG_DFL);
	signal(SIGINT,oldsigint);
	signal(SIGQUIT,oldsigquit);
	if ( sflag )
		sprintf(buffer,"%s/bin/sysmod -s %s",pwd->pw_dir,*argv);
	else
		sprintf(buffer,"%s/bin/sysmod %s",pwd->pw_dir,*argv);
	printf("%s\n",buffer);
	system(buffer);
	printf("Database '%s' is now a version 8 database.\n",*argv);
}
