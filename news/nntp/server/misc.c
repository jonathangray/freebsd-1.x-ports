#ifndef lint
static char	*sccsid = "@(#)$Header: /a/cvs/386BSD/ports/news/nntp/server/misc.c,v 1.2 1994/04/25 23:58:24 adam Exp $";
#endif

#include "common.h"

/*
 * open_valid_art -- determine if a given article name is valid;
 *		if it is, return a file pointer to the open article,
 *		along with a unique id of the article.
 *
 *	Parameters:	"artname" is a string containing the
 *			name of the article.
 *			"id" is space for us to put the article
 *			id in.
 *
 *	Returns:	File pointer to the open article if the
 *			article is valid; NULL otherwise
 *
 *	Side effects:	None.
 */

FILE *
open_valid_art(artname, id)
	char		*artname;
	char		*id;
{
	static int	crnt_art_num;
	static char	crnt_art_id[MAXBUFLEN];
	int		fd;
	struct stat	statbuf;

	if (art_fp != NULL) {
		if (crnt_art_num == atoi(artname)) {
			if (fseek(art_fp, (long) 0, 0) < 0)
				close_crnt();
			else {
				(void) strcpy(id, crnt_art_id);
				return (art_fp);
			}
		} else 
			close_crnt();
	}

	art_fp = fopen(artname, "r");

	if (art_fp == NULL)
		return (NULL);

	fd = fileno(art_fp);

	if (fstat(fd, &statbuf) < 0) {
		close_crnt();
		return (NULL);
	}

	if ((statbuf.st_mode & S_IFMT) != S_IFREG) {
		close_crnt();
		return (NULL);
	}

	get_id(art_fp, id);
	(void) strcpy(crnt_art_id, id);
	crnt_art_num = atoi(artname);
	return (art_fp);
}


/*
 * gethistent -- return the path name of an article if it's
 * in the history file.
 *
 *	Parameters:	"msg_id" is the message ID of the
 *			article, enclosed in <>'s.
 *			"lookup", only check if article exists
 *
 *	Returns:	A char pointer to a static data area
 *			containing the full pathname of the
 *			article, or NULL if the message-id is not
 *			in thef history file.
 *
 *	Side effects:	opens dbm database
 *			(only once, keeps it open after that).
 *			If running Bnews, converts "msg_id" to lower case.
 *			If running Cnews, converts "msg_id" per rfc822.
 *			
 */

#ifndef NDBM
# ifndef DBM
#  ifndef USGHIST
#   define USGHIST
#  endif not USGHIST
# endif not DBM
#endif not DBM

char *
gethistent(msg_id, lookup)
	char		*msg_id;
	int		lookup;
{
	char		line[MAXBUFLEN];
	char		*tmp;
	register char	*cp;
	long		ltmp;
	static char	path[MAXPATHLEN];
#ifdef USGHIST
	char		*histfile();
	register int	len;
#else not USGHIST
#ifdef DBM
	static int	dbopen = 0;
	datum		fetch();
#else not DBM
	static DBM	*db = NULL;	/* History file, dbm version */
#endif DBM
	datum		 key, content;
#endif USGHIST
	static FILE	*hfp = NULL;	/* history file, text version */

#ifdef CNEWS
	cp = rindex(msg_id,'@');	/* look for @ in message id */
	if( cp != NULL)
	{	
	    for(;*cp != '\0';++cp)
#else
	{
	    for (cp = msg_id; *cp != '\0'; ++cp)
#endif
		if (isupper(*cp))
			*cp = tolower(*cp);
/* Make ctags happy */
#ifdef CNEWS
	}
#else
	}
#endif
#ifdef USGHIST
	hfp = fopen(histfile(msg_id), "r");
	if (hfp == NULL) {
#ifdef SYSLOG
		syslog(LOG_ERR, "gethistent: histfile: %m");
#endif SYSLOG
		return (NULL);
	}

	len = strlen(msg_id);
	while (fgets(line, sizeof (line), hfp))
		if (!strncasecmp(msg_id, line, len))
			break;

	if (feof(hfp)) {
		(void) fclose(hfp);
		return (NULL);
	}
#else not USGHIST
#ifdef DBM
	if (!dbopen) {
		if (dbminit(historyfile) < 0) {
#ifdef SYSLOG
			syslog(LOG_ERR, "openartbyid: dbminit %s: %m",
				historyfile);
#endif SYSLOG
			return (NULL);
		} else
			dbopen = 1;
	}
#else	/* ndbm */
	if (db == NULL) {
		db = dbm_open(historyfile, O_RDONLY, 0);
		if (db == NULL) {
#ifdef SYSLOG
			syslog(LOG_ERR, "openartbyid: dbm_open %s: %m",
				historyfile);
#endif SYSLOG
			return (NULL);
		}
	}
#endif DBM

	key.dptr = msg_id;
	key.dsize = strlen(msg_id) + 1;

#ifdef DBM
	content = fetch(key);
#else	/* ndbm */
	content = dbm_fetch(db, key);
#endif DBM
	if (content.dptr == NULL)
		return (NULL);

	/*
	 * If we are just checking to see if it exists return a non-NULL
	 * result
	 */
	if (lookup)
		return ((char *)1);

	if (hfp == NULL) {
		hfp = fopen(historyfile, "r");
		if (hfp == NULL) {
#ifdef SYSLOG
			syslog(LOG_ERR, "message: fopen %s: %m",
				historyfile);
#endif SYSLOG
			return (NULL);
		}
	} else {
/* Why do this if we are going to do an absolute fseek below? XXX */
		rewind(hfp);
	}

	bcopy(content.dptr, (char *)&ltmp, sizeof (long));
	if (fseek(hfp, ltmp, 0) < 0) {
#ifdef SYSLOG
		syslog(LOG_ERR, "message: %s: fseek to %ld on %d: %m", 
		       historyfile, ltmp, hfp);
#endif SYSLOG
		return (NULL);
	}

	(void) fgets(line, sizeof(line), hfp);
#endif USGHIST

	if ((cp = index(line, '\n')) != NULL)
		*cp = '\0';
	cp = index(line, '\t');
	if (cp != NULL)
		cp = index(cp+1, '\t');
#ifdef SYSLOG
	else
	    syslog(LOG_ERR,
		"message: malformed line in history file at %ld bytes, id %s",
			ltmp, msg_id);
#endif SYSLOG
	if (cp == NULL) return(NULL); /* this article has expired */
	tmp = cp+1;

	if ((cp = index(tmp, ' ')) != NULL)
		*cp = '\0';
	
	while ((cp = index(tmp, '.')) != NULL)
		*cp = '/';

	(void) strcpy(path, spooldir);
	(void) strcat(path, "/");
	(void) strcat(path, tmp);
#ifdef USGHIST
	(void) fclose(hfp);
#endif
	return (path);
}

/*
 * openartbyid -- open an article by message-id.
 *
 *	Arguments:	"msg_id" is the message-id of the article
 *			to open.
 *
 *	Returns:	File pointer to opened article, or NULL if
 *			the article was not in the history file or
 *			could not be opened.
 *
 *	Side effects:	Opens article.
 */

FILE *
openartbyid(msg_id)
	char	*msg_id;
{
	char	*path;

	path = gethistent(msg_id, 0);
	if (path != NULL)
		return (fopen(path, "r"));
	else
		return (NULL);
}


/*
 * check_ngperm -- check to see if they're allowed to see this
 * article by matching Newsgroups: and Distribution: line.
 *
 *	Parameters:	"fp" is the file pointer of this article.
 *
 *	Returns:	0 if they're not allowed to see it.
 *			1 if they are.
 *
 *	Side effects:	None.
 */

check_ngperm(fp)
	register FILE	*fp;
{
	char		buf[MAXBUFLEN];
	register char	*cp;
	static char	**ngarray;
	int		ngcount = 0;

	if (ngpermcount == 0) {
		if (ALLBUT == 0)
			return 0;
		return (1);
	}

	while (fgets(buf, sizeof (buf), fp) != NULL) {
		if (buf[0] == '\n')		/* End of header */
			break;
		if (buf[0] != 'N' && buf[0] != 'n')
			continue;
		cp = index(buf, '\n');
		if (cp)
			*cp = '\0';
		cp = index(buf, ':');
		if (cp == NULL)
			continue;
		*cp = '\0';
		if (!strcasecmp(buf, "newsgroups")) {
			ngcount = get_nglist(&ngarray, cp+2);
			break;
		}
	}

#ifndef USG
	(void) rewind(fp);
#else
	rewind(fp);
#endif
	if (ngcount == 0)	/* Either no newgroups or null entry */
		return (1);

	return (ngmatch(s1strneql, ALLBUT,
		ngpermlist, ngpermcount, ngarray, ngcount));
}


/*
 * spew -- spew out the contents of a file to stdout, doing
 * the necessary cr-lf additions at the end.  Finish with
 * a "." on a line by itself, and an fflush(stdout).
 *
 *	Parameters:	"how" tells what part of the file we
 *			want spewed:
 *				ARTICLE   The entire thing.
 *				HEAD	  Just the first part.
 *				BODY	  Just the second part.
 *			"fp" is the open file to spew from.
 *
 *	Returns:	Nothing.
 *
 *	Side effects:	Changes current position in file.
 */

spew(fp, how)
	FILE		*fp;
	int		how;
{
	char		line[NNTP_STRLEN];
	register char	*cp;

#ifdef LOG
	++arts_acsd;
#endif

	if (how == STAT) {
		(void) fflush(stdout);
		return;
	}

	while (fgets(line, sizeof(line)-6, fp) != NULL && *line != '\n') {
		if (how == BODY)	/* We need to skip this anyway */
			continue;
		cp = index(line, '\n');
		if (cp != NULL)
			*cp = '\0';
		if (*line == '.')
			putchar('.');
		putline(line);
		if (cp == NULL) {
			for (;;) {
				if ((fgets(line, sizeof(line)-6, fp) == NULL)
				    || (index(line, '\n') != NULL))
					break;
			}
		}
	}

	if (how == HEAD) {
		putchar('.');
		putchar('\r');
		putchar('\n');
		(void) fflush(stdout);
		return;
	} else if (how == ARTICLE) {
		putchar('\r');
		putchar('\n');
	}

	while (fgets(line, sizeof(line)-6, fp) != NULL) {
		cp = index(line, '\n');
		if (cp != NULL)
			*cp = '\0';
		if (*line == '.')
			putchar('.');
		putline(line);

		if (cp == NULL) {
			for (;;) {
				if ((fgets(line, sizeof(line)-6, fp) == NULL)
				    || (index(line, '\n') != NULL))
					break;
			}
		}
	}
	putchar('.');
	putchar('\r');
	putchar('\n');
	(void) fflush(stdout);
}


/*
 * get_id -- get the message id of the current article.
 *
 *	Parameters:	"art_fp" is a pointer to the open file.
 *			"id" is space for the message ID.
 *
 *	Returns:	Nothing.
 *
 *	Side effects:	Seeks and rewinds on "art_fp".
 *			Changes space pointed to by "id".
 */

get_id(art_fp, id)
	register FILE	*art_fp;
	char		*id;
{
	char		line[MAXBUFLEN];
	register char	*cp;

	while (fgets(line, sizeof(line), art_fp) != NULL) {
		if (*line == '\n')
			break;
		if (*line == 'M' || *line == 'm') {	/* "Message-ID" */
			if ((cp = index(line, ' ')) != NULL) {
				*cp = '\0';
				if (!strcasecmp(line, "Message-ID:")) {
					(void) strcpy(id, cp + 1);
					if ((cp = index(id, '\n')) != NULL)
						*cp = '\0';
#ifndef USG
					(void) rewind(art_fp);
#else
					rewind(art_fp);
#endif
					return;
				}
			}
		}
	}
#ifndef USG
	(void) rewind(art_fp);
#else
	rewind(art_fp);
#endif
	(void) strcpy(id, "<0>");
}
		

/*
 * close_crnt -- close the current article file pointer, if it's
 *	open.
 *
 *	Parameters:	None.
 *
 *	Returns:	Nothing.
 *
 *	Side effects:	Closes "art_fp" if it's open; sets "art_fp" to NULL.
 */

close_crnt()
{
	if (art_fp != NULL)
		(void) fclose(art_fp);
	art_fp = NULL;
}


/*
 * findart -- find an article number in the article array.
 *
 *	Parameters:	"artname" is a string containing
 *			the name of the article.
 *
 *	Returns:	An index into "art_array",
 *			or -1 if "artname" isn't in "art_array".
 *			
 *	Side effects:	None.
 *
 *	Improvement:	Replace this linear search with a binary one.
 */

findart(artname)
	char		*artname;
{
	register int	i, artnum;

	artnum = atoi(artname);

	for (i = 0; i < num_arts; ++i)
		if (art_array[i] == artnum)
			return(i);

	return (-1);
}


/*
 * get_distlist -- return a nicely set up array of distribution groups
 * along with a count, when given an NNTP-spec distribution list
 * in the form <dist1,dist2,...,distn>.
 *
 *	Parameters:		"array" is storage for our array,
 *				set to point at some static data.
 *				"list" is the NNTP distribution list.
 *
 *	Returns:		Number of distributions found.
 *				-1 on error.
 *
 *	Side effects:		Changes static data area.
 */

get_distlist(array, list)
	char		***array;
	char		*list;
{
	char		*cp;
	int		distcount;
	static char	**dist_list = (char **) NULL;

	if (list[0] != '<')
		return (-1);

	cp = index(list + 1, '>');
	if (cp != NULL)
		*cp = '\0';
	else
		return (-1);

	for (cp = list + 1; *cp != '\0'; ++cp)
		if (*cp == ',')
			*cp = ' ';
	distcount = parsit(list + 1, &dist_list);
	*array = dist_list;
	return (distcount);
}


/*
 * lower -- convert a character to lower case, if it's upper case.
 *
 *	Parameters:	"c" is the character to be
 *			converted.
 *
 *	Returns:	"c" if the character is not
 *			upper case, otherwise the lower
 *			case equivalent of "c".
 *
 *	Side effects:	None.
 */

char
lower(c)
	register char	c;
{
	if (isascii(c) && isupper(c))
		c = c - 'A' + 'a';
	return (c);
}


/* the following is from news 2.11 */

#ifdef USGHIST
/*
** Generate the appropriate history subfile name
*/
char *
histfile(hline)
char *hline;
{
	char chr;	/* least significant digit of article number */
	static char subfile[BUFSIZ];

	chr = findhfdigit(hline);
	sprintf(subfile, "%s.d/%c", HISTORY_FILE, chr);
	return subfile;
}

findhfdigit(fn)
char *fn;
{
	register char *p;
	register int chr;

	p = index(fn, '@');
	if (p != NULL && p > fn)
		chr = *(p - 1);
	else
		chr = '0';
	if (!isdigit(chr))
		chr = '0';
	return chr;
}
#endif USGHIST
#ifdef USG
#ifndef GAZETTE
bcopy(s, d, l)
	register char *s, *d;
	register int l;
{
	while (l-- > 0)
		*d++ = *s++;
}

bcmp(s1, s2, l)
	register char *s1, *s2;
	register int l;
{
	if (l == 0)
		return (0);

	do
		if (*s1++ != *s2++)
			break;
	while (--l);

	return (l);
}

bzero(p, l)
	register char *p;
	register int l;
{
	while (l-- > 0)
		*p++ = 0;
}
#endif

dup2(x,y)
int x,y;
{ 
	close(y); 
	return(fcntl(x, F_DUPFD,y ));
}

#endif

/*
 * The following is a mish-mosh of code submitted to the net
 * by Stan Barber <sob@bcm.tmc.edu>, Tad Guy <tadguy@cs.odu.edu>,
 * Chris Jepeway <jepeway@utkcs2.cs.utk.edu>, and Tom Lane <tgl@cs.cmu.edu>.
 */

/*
 * returns 1 if there are lots of free blocks for the nntp server to use;
 * a zero value is the small number of blocks remaining (more or less).
 */
#define DFREE_OK	0
#define DFREE_INODES	1
#define DFREE_BLOCKS	2
#define DFREE_ERR	3

int
space(min_free)
int min_free;
{
    int result, dfree();

    result = dfree(SPOOLDIR,min_free);
    if (result == DFREE_OK) return(1);
#ifdef SYSLOG
    switch (result) {
	case DFREE_ERR:
		syslog(LOG_ERR,"dfree failed due to syscall error");
		break;
#ifdef LOG
	case DFREE_INODES:
		syslog(LOG_INFO,"no inodes on %s",SPOOLDIR);
		break;
	case DFREE_BLOCKS:
		syslog(LOG_INFO,"no space on %s",SPOOLDIR);
		break;
#endif
	    }    
#endif
    return(0);
}

/*
 * Now we define the dfree() routine, which returns the free space
 * on the file system containing the specified directory.
 * Space is measured in kilobytes.
 * A negative value is returned on error.
 */
#ifndef READ_SUPER
#if defined(sun) || defined(hpux) || defined(pyr) || defined(hp300) || defined(NeXT) || defined(__FreeBSD__)
#ifdef __FreeBSD__
#include <sys/mount.h>
#else
#include <sys/vfs.h>
#endif
#define statfilesys	statfs		/* routine to call when trying to  */
					/* stat a file system to get the # */
					/* of free blocks available	   */
typedef struct statfs statfs_type;	/* the data type into which statfs() */
					/* wants to return useful information*/
#define bombed(call)    ((call) == -1)	/* boolean expression returning 1 if */
					/* a call to statfs() fails	     */
#define blkfree(fs)	((fs).f_bfree)	/* given a statfs_type, return total */
					/* # of free blocks		     */
#define blkavail(fs)	((fs).f_bavail)	/* given a statfs_type called fs,  */
					/* return # of blocks available to */
					/* a non-privileged user	   */
#define filfree(fs)	((fs).f_ffree)	/* given a statfs_type called fs,  */
 					/* return number of free inodes	   */
#endif 

#if defined(apollo)
#include <sys/types.h>
#include <sys/statfs.h>
#define statfilesys(a,b)	statfs(a,b, sizeof(struct statfs), 0)		/* routine to call when trying to  */
					/* stat a file system to get the # */
					/* of free blocks available	   */
typedef struct statfs statfs_type;	/* the data type into which statfs() */
					/* wants to return useful information*/
#define bombed(call)    ((call) == -1)	/* boolean expression returning 1 if */
					/* a call to statfs() fails	     */
#define blkfree(fs)	((fs).f_bfree)	/* given a statfs_type, return total */
					/* # of free blocks		     */
#define blkavail(fs)	((fs).f_bfree)	/* given a statfs_type called fs,  */
					/* return # of blocks available to */
					/* a non-privileged user	   */
#define filfree(fs)	((fs).f_ffree)	/* given a statfs_type called fs,  */
 					/* return number of free inodes	   */
#endif /* apollo */

#ifdef ultrix
#include <sys/mount.h>
typedef struct fs_data statfs_type;
#define statfilesys	statfs
#define bombed(call)	((call) <= 0)
#define blkfree(fs)	((int)((fs).fd_req.bfree))
#define blkavail(fs)	((int)((fs).fd_req.bfreen))
#define filfree(fs)	((int)((fs).fd_req.gfree))
#endif 

#if defined(USG) && !defined(hpux)
#include <ustat.h>
typedef struct ustat statfs_type;
/*
 * You've got to make calls to 2 functions to get
 * free blocks on a USG system, so statfilesys can't just be a macro.
 * written by Stan Barber <sob@watson.bcm.tmc.edu>
 */
int
statfilesys(dir, fs)
char *dir;
statfs_type *fs;
{
    struct stat file;
    if (stat(dir,&file)) return(-1);
    if (ustat(file.st_dev, fs)) return(-2);
    return(0);
}
#define bombed(call)	(call != 0)
#define blkfree(fs)	((fs).f_tfree)
#define blkavail(fs)	((fs).f_tfree)
				/* USG doesn't reserve blocks for root */
#define filfree(fs)	((fs).f_tinode)	
#endif USG

#ifdef CMU_MACH
/* This code supplied by Tom Lane <tgl@cs.cmu.edu> */
#include <sys/ioctl.h>
typedef struct fsparam statfs_type;
int
statfilesys(dir, fs)
char *dir;
statfs_type *fs;
{
    int fd;
    fd = open(dir, O_RDONLY);
    if (fd < 0) return(-1);
    if (ioctl(fd, FIOCFSPARAM, fs) < 0) {
	close(fd);
	return(-2);
    }
    close(fd);
    return(0);
}
#define bombed(call)	((call) < 0)
#define blkfree(fs)	((fs).fsp_free-((fs).fsp_size*(fs).fsp_minfree+99)/100)
#define blkavail(fs)	(-1)
#endif MACH

dfree(spool,free_space)
char *spool;
int free_space;
{
    statfs_type fsys;
    int err;

    if (bombed(err = statfilesys(SPOOLDIR, &fsys)))
	return(DFREE_ERR);		/* can't get file system info */
# if defined(filfree) && defined(MINFILES)
     if (filfree(fsys) < MINFILES )
 	return( DFREE_INODES );
# endif
    if (blkavail(fsys) < 0L) {
	/* the bavail field doesn't apply to this file system */
	if(blkfree(fsys) < free_space)
	    return( DFREE_BLOCKS );
     } else {
	if (blkavail(fsys) < free_space )
	    return( DFREE_BLOCKS );
     }
    return( DFREE_OK );
}

#else READ_SUPER
/*
 * This code is used if you've got to directly read the superblock
 * to determine how much space you've got left.  It's copied from
 * patches posted by Tad Guy <tadguy@cs.odu.edu>
 */

#include <sys/fs.h>
#include <fstab.h>

/*
 * return the number of free kilobytes remaining on the filesystem where
 * the named file resides.  returns -1 on error.
 */

off_t lseek();

dfree(name, free_space)
char *name;
int free_space;
{
    struct stat namest, fsst;
    struct fstab *fsp;
    char lname[MAXPATHLEN];
    int fd;
    union {
	struct fs u_fs;
	char dummy[SBSIZE];
    } sb;
#define sblock sb.u_fs

    strcpy(lname,name);
    do {
	if (stat(lname,&namest))		/* if stat fails, die */
	{
#ifdef SYSLOG
	  syslog(LOG_ERR,"dfree stat(%s) failed: %m", lname);
#endif
	  return  DFREE_ERR;			
	}
	if ((namest.st_mode & S_IFMT) == S_IFLNK) { /* if symlink */
	    if ((fd = readlink(lname,lname,sizeof(lname))) < 0) 
	    {
#ifdef SYSLOG
	      syslog(LOG_ERR,"dfree readlink() failed: %m");
#endif
	      return DFREE_ERR;
	    }
	    lname[fd] = '\0';
	}
    } while ((namest.st_mode & S_IFMT) == S_IFLNK);

    (void) setfsent();

    while (fsp = getfsent()) {
	if (stat(fsp->fs_spec,&fsst))
	  continue;
	if (fsst.st_rdev == namest.st_dev)
	  break;
    }

    if (!fsp ||	(fd = open(fsp->fs_spec,O_RDONLY)) < 0) {
	(void) endfsent();
#ifdef SYSLOG
	syslog(LOG_ERR,"dfree open(%s,O_RDONLY) failed: %m", fsp->fs_spec);
#endif
	return DFREE_ERR;
    }
    (void) endfsent();

    (void) lseek(fd,SBLOCK*DEV_BSIZE,L_SET);
    if (read(fd,(char *)&sblock,SBSIZE) != SBSIZE ||
	(sblock.fs_magic != FS_MAGIC))
    {
#ifdef SYSLOG
      syslog(LOG_ERR,"dfree read() failed: %m");
#endif
      return DFREE_ERR;
    }
    (void) close(fd);

# if defined(filfree) && defined(MINFILES)
    if (filfree(fsys) < MINFILES )
	return( DFREE_INODES );
# endif
    if( ((((sblock.fs_dsize) * ( 100 - sblock.fs_minfree) / 100)
	  - ((sblock.fs_dsize) 
	     - (sblock.fs_cstotal.cs_nbfree 
		* sblock.fs_frag + sblock.fs_cstotal.cs_nffree))) 
	 * sblock.fs_fsize / 1024) < free_space )
	return( DFREE_BLOCKS );
   return( DFREE_OK );
}

#endif READ_SUPER

#ifdef LOAD
/*
**  GETLA -- get the current load average
**
**	This code stolen from la.c. (And subsequently stolen from sendmail,
**		conf.c for nntpd.)
**
**	Parameters:
**		none.
**
**	Returns:
**		The current load average as an integer.
**
**	Side Effects:
**		none.
*/

#ifdef USG
getla()
{
	return(0);
}
#else
#include <nlist.h>
#include <sys/ioctl.h>

struct	nlist Nl[] =
{
	{ "_avenrun" },
#define	X_AVENRUN	0
	{ 0 },
};

getla()
{
	static int kmem = -1;
# ifdef FSCALE
	long avenrun[3];
# else
	double avenrun[3];
# endif
	extern off_t lseek();

	if (kmem < 0)
	{
		kmem = open("/dev/kmem", 0, 0);
		if (kmem < 0)
			return (-1);
		(void) ioctl(kmem, (int) FIOCLEX, (char *) 0);
		nlist("/vmunix", Nl);
		if (Nl[0].n_type == 0)
			return (-1);
	}
	if (lseek(kmem, (off_t) Nl[X_AVENRUN].n_value, 0) == -1 ||
	    read(kmem, (char *) avenrun, sizeof(avenrun)) < sizeof(avenrun))
	{
		/* thank you Ian */
		return (-1);
	}
# ifdef FSCALE
	return ((int) (avenrun[0] + FSCALE/2) >> FSHIFT);
# else
	return ((int) (avenrun[0] + 0.5));
# endif
}
#endif
#endif LOAD
