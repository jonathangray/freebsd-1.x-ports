/*	PATH.C	
 *		The routines in this file handle the conversion of pathname
 *		strings.
 *
 * $Log: path.c,v $
 * Revision 1.1  1994/02/01 03:29:36  jkh
 * Initial revision
 *
 * Revision 1.17  1993/09/06  16:31:57  pgf
 * suppress doubled path separators in pathcat()
 *
 * Revision 1.16  1993/09/03  09:11:54  pgf
 * tom's 3.60 changes
 *
 * Revision 1.15  1993/07/09  13:59:37  pgf
 * comment on djgcc ifdef
 *
 * Revision 1.14  1993/07/06  16:39:04  pgf
 * integrated Tuan DANG's changes for the djgpp compiler under DOS
 *
 * Revision 1.13  1993/06/25  11:25:55  pgf
 * patches for Watcom C/386, from Tuan DANG
 *
 * Revision 1.12  1993/06/02  14:28:47  pgf
 * see tom's 3.48 CHANGES
 *
 * Revision 1.11  1993/05/24  15:21:37  pgf
 * tom's 3.47 changes, part a
 *
 * Revision 1.10  1993/05/11  16:22:22  pgf
 * see tom's CHANGES, 3.46
 *
 * Revision 1.9  1993/05/06  11:59:58  pgf
 * added ifdefs for USE_D_NAMLEN, for systems that don't have or don't
 * need it (d_name[] is null-terminated on most systems)
 *
 * Revision 1.8  1993/04/20  12:18:32  pgf
 * see tom's 3.43 CHANGES
 *
 * Revision 1.7  1993/04/09  13:42:32  pgf
 * ifdefed out extern decls for getpw{nam,uid} -- to many type/prototyple
 * clashes
 *
 * Revision 1.6  1993/04/02  11:02:15  pgf
 * ls-based directory routines, and support for "old-style" directories
 *
 * Revision 1.5  1993/04/01  15:55:02  pgf
 * added extern decls for getpw{nam,uid}()
 *
 * Revision 1.4  1993/04/01  13:07:50  pgf
 * see tom's 3.40 CHANGES
 *
 * Revision 1.3  1993/03/25  19:50:58  pgf
 * see 3.39 section of CHANGES
 *
 * Revision 1.2  1993/03/16  16:04:01  pgf
 * fix 'parentheses suggested' warnings
 *
 * Revision 1.1  1993/03/16  10:53:21  pgf
 * see 3.36 section of CHANGES file
 *
 */

#include	"estruct.h"
#include        "edef.h"

#if UNIX
#include <sys/types.h>
#include <pwd.h>
#endif

#if VMS
#include <file.h>
#endif

#include <sys/stat.h>

#include "dirstuff.h"

/*
 * Fake directory-routines for system where we cannot otherwise figure out how
 * to read the directory-file.
 */
#if USE_LS_FOR_DIRS
DIR *
opendir (path)
char	*path;
{
	static	char	fmt[] = "/bin/ls %s";
	char	lscmd[NFILEN+sizeof(fmt)];

	(void)lsprintf(lscmd, fmt, path);
	return npopen(lscmd, "r");
}

DIRENT *
readdir (dp)
DIR	*dp;
{
	static	DIRENT	dummy;

	if ((fgets(dummy.d_name, NFILEN, dp)) != NULL) {
		/* zap the newline */
		dummy.d_name[strlen(dummy.d_name)-1] = EOS;
		return &dummy;
	}
	return 0;
}

int
closedir (dp)
DIR	*dp;
{
	(void)npclose(dp);
	return 0;
}
#endif

/*
 * Use this routine to fake compatibility with unix directory routines.
 */
#if OLD_STYLE_DIRS
DIRENT *
readdir(dp)
DIR	*dp;
{
	static	DIRENT	dbfr;
	return (fread(&dbfr, sizeof(dbfr), 1, dp)\
				? &dbfr\
				: (DIRENT *)0);
}
#endif

#if MSDOS
/*
 * If the pathname begins with an MSDOS-drive, return the pointer past it.
 * Otherwise, return null.
 */
char *
is_msdos_drive(path)
char	*path;
{
	if (isalpha(*path) && path[1] == ':') {
		(void)mklower(path);	/* MS-DOS isn't case-sensitive */
		return path+2;
	}
	return NULL;
}
#endif

#if VMS
#define VMSPATH_END_NODE   1
#define VMSPATH_END_DEV    2
#define VMSPATH_BEGIN_DIR  3
#define VMSPATH_NEXT_DIR   4
#define VMSPATH_END_DIR    5
#define	VMSPATH_BEGIN_FILE 6
#define VMSPATH_BEGIN_TYP  7
#define VMSPATH_BEGIN_VER  8

/*
 * Returns true if the string is delimited in a manner compatible with VMS
 * pathnames.  To be consistent with the use of 'is_pathname()', insist that
 * at least the "[]" characters be given.
 *
 * Complete syntax:
 *	node::device:[dir1.dir2]filename.type;version
 *	    ^1     ^2^3   ^4  ^5^6      ^7   ^8
 */
int
is_vms_pathname(path, option)
char	*path;
int	option;		/* true:directory, false:file, -true:don't care */
{
	char	*base	= path;
	int	this	= 0,
		next	= -1;

	while (ispath(*path)) {
		switch (*path) {
		case '[':
			next = VMSPATH_BEGIN_DIR;
			break;
		case ']':
			if (this < VMSPATH_BEGIN_DIR)
				return FALSE;
			next = VMSPATH_END_DIR;
			break;
		case '.':
			next = (this >= VMSPATH_END_DIR)
				? VMSPATH_BEGIN_TYP
				: (this >= VMSPATH_BEGIN_DIR
					? VMSPATH_NEXT_DIR
					: VMSPATH_BEGIN_TYP);
			break;
		case ';':
			next = VMSPATH_BEGIN_VER;
			break;
		case ':':
			if (path[1] == ':') {
				path++;	/* eat "::" */
				if (this >= VMSPATH_END_NODE)
					return FALSE;
				next = VMSPATH_END_NODE;
			} else
				next = VMSPATH_END_DEV;
			break;
		case '!':
		case '/':
			return FALSE;	/* a DEC-shell name */
		default:
			if (!ispath(*path))
				return FALSE;
			next = this;
			break;
		}
		if (next < this)
			break;
		this = next;
		path++;
	}

	if ((*path != EOS)
	 || (this  <  next))
		return FALSE;

	if (this == 0)
		this = VMSPATH_BEGIN_FILE;

	return (option == TRUE  && (this == VMSPATH_END_DIR))	/* dir? */
	  ||   (option == FALSE && (this >= VMSPATH_BEGIN_FILE))/* file? */
	  ||   (option == -TRUE && (this >= VMSPATH_END_DIR	/* anything? */
				 || this <  VMSPATH_BEGIN_DIR));
}
#endif

#if VMS
/*
 * Returns a pointer to the argument's last path-leaf (i.e., filename).
 */
char *
vms_pathleaf(path)
char	*path;
{
	register char	*s;
	for (s = path + strlen(path);
		s > path && !strchr(":]", s[-1]);
			s--)
		;
	return s;
}
#endif

/*
 * Returns a pointer to the argument's last path-leaf (i.e., filename).
 */

#ifdef __WATCOMC__

char *
pathleaf(path)
char	*path;
{
	register char	*s = last_slash(path);
	if (s == 0) {
#if MSDOS
		if (!(s = is_msdos_drive(path)))
#endif
		s = path;
	} else
		s++;
	return s;
}

#else

#if !VMS
#define	unix_pathleaf	pathleaf
#endif

char *
unix_pathleaf(path)
char	*path;
{
	register char	*s = last_slash(path);
	if (s == 0) {
#if MSDOS
		if (!(s = is_msdos_drive(path)))
#endif
		s = path;
	} else
		s++;
	return s;
}
#endif /* __WATCOMC__ */


#if VMS
char *pathleaf(path)
char	*path;
{
	if (is_vms_pathname(path, -TRUE))
		return vms_pathleaf(path);
	return unix_pathleaf(path);
}
#endif

/*
 * Concatenates a directory and leaf name to form a full pathname
 */
char *
pathcat (dst, path, leaf)
char	*dst;
char	*path;
char	*leaf;
{
	char	temp[NFILEN];
	register char	*s = dst;

	if (path == 0)
		return strcpy(dst, leaf);

	leaf = strcpy(temp, leaf);		/* in case leaf is in dst */

	if (s != path)
		(void)strcpy(s, path);
	s += strlen(s) - 1;

#if VMS
	if (!is_vms_pathname(dst, TRUE))	/* could be DecShell */
#endif
	 if (!slashc(*s++)) {
		*s++ = slash;
	 }

	(void)strcpy(s, leaf);
	return dst;
}

/*
 * Tests to see if the string contains a slash-delimiter.  If so, return the
 * last one (so we can locate the path-leak).
 */
char *
last_slash(fn)
char *fn;
{
	register char	*s;

	for (s = fn + strlen(fn) - 1; s >= fn; s--)
		if (slashc(*s))
			return s;
	return 0;
}

/*
 * If a pathname begins with "~", lookup the name in the password-file.  Cache
 * the names that we lookup, because searching the password-file can be slow,
 * and users really don't move that often.
 */
#if UNIX
typedef	struct	_upath {
	struct	_upath *next;
	char	*name;
	char	*path;
	} UPATH;

static	UPATH	*user_paths;

static	char *	save_user P(( char *, char * ));
static char *
save_user(name, path)
char	*name;
char	*path;
{
	register UPATH *q;

	if (name != NULL
	 && path != NULL
	 && (q = typealloc(UPATH)) != NULL) {
		if ((q->name = strmalloc(name)) != NULL
		 && (q->path = strmalloc(path)) != NULL) {
			q->next = user_paths;
			user_paths = q;
			return q->path;
		} else {
			FreeIfNeeded(q->name);
			FreeIfNeeded(q->path);
			free((char *)q);
		}
	}
	return NULL;
}

static	char *	find_user P(( char * ));
static char *
find_user(name)
char	*name;
{
	register struct	passwd *p;
	register UPATH	*q;

	if (name != NULL) {
		for (q = user_paths; q != NULL; q = q->next) {
			if (!strcmp(q->name, name)) {
				return q->path;
			}
		}

		/* not-found, do a lookup */
		if (*name != EOS)
			p = getpwnam(name);
		else
			p = getpwuid((int)getuid());

		if (p != NULL)
			return save_user(name, p->pw_dir);
#if NEEDED
	} else {	/* lookup all users (for globbing) */
		(void)setpwent();
		while ((p = getpwent()) != NULL)
			(void)save_user(p->pw_name, p->pw_dir);
		(void)endpwent();
#endif
	}
	return NULL;
}

char *
home_path(path)
char	*path;
{
	if (*path == '~') {
		char	temp[NFILEN];
		char	*s, *d;

		/* parse out the user-name portion */
		for (s = path+1, d = temp; (*d = *s) != EOS; d++, s++) {
			if (slashc(*d)) {
				*d = EOS;
				break;
			}
		}

		if ((d = find_user(temp)) != NULL)
			(void)pathcat(path, d, s);
	}
	return path;
}
#endif

/* canonicalize a pathname, to eliminate extraneous /./, /../, and ////
	sequences.  only guaranteed to work for absolute pathnames */
char *
canonpath(ss)
char *ss;
{
	char *p, *pp;
	char *s;

	if ((s = is_appendname(ss)) != 0)
		return (canonpath(s) != 0) ? ss : 0;

	s = ss;

	if (!*s)
		return s;

#if MSDOS
	(void)mklower(ss);	/* MS-DOS is case-independent */
	/* pretend the drive designator isn't there */
	if ((s = is_msdos_drive(ss)) == 0)
		s = ss;
#endif

#if UNIX
	(void)home_path(s);
#endif

#if VMS
	/*
	 * If the code in 'lengthen_path()', as well as the scattered calls on
	 * 'fgetname()' are correct, the path given to this procedure should
	 * be a fully-resolved VMS pathname.
	 */
	if (!is_vms_pathname(s, -TRUE)) {
		mlforce("BUG: canonpath '%s'", s);
		return ss;
	}
#endif

#if UNIX || MSDOS
	p = pp = s;
	if (!slashc(*s)) {
		mlforce("BUG: canonpath '%s'", s);
		return ss;
	}

#if APOLLO
	if (!slashc(p[1])) {	/* could be something like "/usr" */
		char	*cwd = current_directory(FALSE);
		char	temp[NFILEN];
		if (!strncmp(cwd, "//", 2)
		 && strlen(cwd) > 2
		 && (p = strchr(cwd+2, '/')) != 0) {
			(void)strcpy(strcpy(temp, cwd) + (p+1-cwd), s);
			(void)strcpy(s, temp);
		}
	}
	p = s + 1;	/* allow for leading "//" */
#endif

	p++; pp++;	/* leave the leading slash */
	while (*pp) {
		switch (*pp) {
		case '/':
#if MSDOS
		case '\\':
#endif
			pp++;
			continue;
		case '.':
			if (slashc(*(pp+1))) {
				pp += 2;
				continue;
			}
		default:
			break;
		}
		break;
	}
	while (*pp) {
#if DEBUG
		if (pp != p)
			*p = EOS;
		printf(" s is %s\n",s);
		printf("pp is %*s%s\n",pp-s,"",pp);
#endif
		if (slashc(*pp)) {
			while (slashc(*(pp+1)))
				pp++;
			if (p > s && !slashc(*(p-1)))
				*p++ = slash;
			if (*(pp+1) == '.') {
				if (*(pp+2) == EOS) {
					/* change "/." at end to "" */
					*(p-1) = EOS;	/* and we're done */
					break;
				}
				if (slashc(*(pp+2))) {
					pp += 2;
					continue;
				} else if (*(pp+2) == '.' && (slashc(*(pp+3))
							|| *(pp+3) == EOS)) {
					while (p-1 > s && slashc(*(p-1)))
						p--;
					while (p > s && !slashc(*(p-1)))
						p--;
					if (p == s)
						*p++ = slash;
					pp += 3;
					continue;
				}
			}
			pp++;
			continue;
		} else {
			*p++ = *pp++;
		}
	}
	if (p > s && slashc(*(p-1)))
		p--;
	if (p == s)
		*p++ = slash;
	*p = EOS;
#endif	/* UNIX || MSDOS */

	return ss;
}

char *
shorten_path(path, keep_cwd)
char *path;
int keep_cwd;
{
	char	temp[NFILEN];
	char *cwd;
	char *ff;
	char *slp;
	char *f;
#if VMS
	char *dot;
#endif

	if (!path || *path == EOS)
		return NULL;

	if (isInternalName(path))
		return path;

	if ((f = is_appendname(path)) != 0)
		return (shorten_path(f, keep_cwd) != 0) ? path : 0;

#if VMS
	/*
	 * This assumes that 'path' is in canonical form.
	 */
	cwd = current_directory(FALSE);
	slp = ff = path;
	dot = 0;

	(void)strcpy(temp, "[");	/* hoping for relative-path */
	while (*cwd && *ff) {
		if (*cwd != *ff) {
			if (*cwd == ']' && *ff == '.') {
				;	/* "[.DIRNAME]FILENAME.TYP;1" */
			} else if (*cwd == '.' && *ff == ']') {
				/* "[-]FILENAME.TYP;1" */
				while (*cwd != EOS) {
					if (*cwd++ == '.')
						(void)strcat(temp, "-");
				}
			} else if (dot != 0) {
				/* "[-.DIRNAME]FILENAME.TYP;1" */
				while (*dot != EOS) {
					if (*dot == ']')
						break;
					if (*dot++ == '.') {
						(void)strcat(temp, "-");
						if (dot == ff) {
							(void)strcat(temp, ".");
							break;
						}
					}
				}
				ff = dot;
			} else
				break;
			return strcpy(path, strcat(temp, ff));
		}
		switch (*ff) {
		case ']':	slp = ff;	break;
		case '.':	dot = ff;
		}
		cwd++;
		ff++;
	}

	if (*cwd == EOS) {	/* "[]FILENAME.TYP;1" */
		if (keep_cwd)
			(void)strcat(temp, "]");
		else
			*temp = EOS;
		return strcpy(path, strcat(temp, ff));
	}
#endif

#if UNIX || MSDOS
	cwd = current_directory(FALSE);
	slp = ff = path;
	while (*cwd && *ff && *cwd == *ff) {
		if (slashc(*ff))
			slp = ff;
		cwd++;
		ff++;
	}

	/* if we reached the end of cwd, and we're at a path boundary,
		then the file must be under '.' */
	if (*cwd == EOS) {
		if (keep_cwd) {
			temp[0] = '.';
			temp[1] = slash;
			temp[2] = EOS;
		} else
			*temp = EOS;
		if (slashc(*ff))
			return strcpy(path, strcat(temp, ff+1));
		if (slp == ff - 1)
			return strcpy(path, strcat(temp, ff));
	}
	
	/* if we mismatched during the first path component, we're done */
	if (slp == path)
		return path;

	/* if we mismatched in the last component of cwd, then the file
		is under '..' */
	if (last_slash(cwd) == 0)
		return strcpy(path, strcat(strcpy(temp, ".."), slp));

	/* we're off by more than just '..', so use absolute path */
#endif	/* UNIX || MSDOS */

	return path;
}

/*
 * Undo nominal effect of 'shorten_path()'
 */
char *
lengthen_path(path)
char *path;
{
#if VMS
	struct	FAB	my_fab;
	struct	NAM	my_nam;
	char		my_esa[NAM$C_MAXRSS];	/* expanded: SYS$PARSE */
	char		my_rsa[NAM$C_MAXRSS];	/* result: SYS$SEARCH */
#endif
	register int len;
	char	*cwd;
	char	*f;
	char	temp[NFILEN];
#if MSDOS
	char	drive;
#endif

	if ((f = is_appendname(path)) != 0)
		return (lengthen_path(f) != 0) ? path : 0;

	if ((f = path) == 0)
		return path;

	if (*path != EOS && isInternalName(path))
		return path;

#if UNIX
	(void)home_path(f);
#endif

#if VMS
	/*
	 * If the file exists, we can ask VMS to tell the full pathname.
	 */
	if ((*path != EOS) && maybe_pathname(path)) {
		int	fd;
		long	status;
		char	temp[NFILEN],
			leaf[NFILEN];
		register char	*s;

		if (!strchr(path, '*') && !strchr(path, '?')) {
			if ((fd = open(path, O_RDONLY, 0)) >= 0) {
				getname(fd, temp);
				(void)close(fd);
				return strcpy(path, temp);
			}
		}

		/*
		 * Path either contains a wildcard, or the file does
		 * not already exist.  Use the system parser to expand
		 * the pathname components.
		 */
		my_fab = cc$rms_fab;
		my_fab.fab$l_fop = FAB$M_NAM;
		my_fab.fab$l_nam = &my_nam;	/* FAB => NAM block	*/
		my_fab.fab$l_dna = "";		/* Default-selection	*/
		my_fab.fab$b_dns = strlen(my_fab.fab$l_dna);

		my_fab.fab$l_fna = path;
		my_fab.fab$b_fns = strlen(path);

		my_nam = cc$rms_nam;
		my_nam.nam$b_ess = NAM$C_MAXRSS;
		my_nam.nam$l_esa = my_esa;
		my_nam.nam$b_rss = NAM$C_MAXRSS;
		my_nam.nam$l_rsa = my_rsa;

		if ((status = sys$parse(&my_fab)) == RMS$_NORMAL) {
			my_esa[my_nam.nam$b_esl] = EOS;
			return strcpy(path, my_esa);
		} else {
			/* later: try to expand partial directory specs, etc. */
		}
	}
#endif

#if UNIX || MSDOS
#if MSDOS
	if ((f = is_msdos_drive(path)) != 0)
		drive = *path;
	else {
		drive = EOS;
		f = path;
	}
#endif
	if (!slashc(f[0])) {
#if MSDOS
		cwd = curr_dir_on_drive(drive!=EOS?drive:curdrive());
#else
		cwd = current_directory(FALSE);
#endif
		len = strlen(strcpy(temp, cwd));
		temp[len++] = slash;
#if GO32
		temp[0] = slash;  /* DJGCC returns '/', we may want '\' */
#endif
		(void)strcpy(temp + len, f);
		(void)strcpy(path, temp);
	}
#if MSDOS
	if (is_msdos_drive(path) == 0) { /* ensure that we have drive too */
		temp[0] = curdrive();
		temp[1] = ':';
		(void)strcpy(temp+2, path);
		(void)strcpy(path, temp);
	}
#endif
#endif	/* UNIX || MSDOS */

	return canonpath(path);
}

/*
 * Returns true if the argument looks more like a pathname than anything else.
 *
 * Notes:
 *	This makes a syntax-only test (e.g., at the beginning of the string).
 *	VMS can accept UNIX-style /-delimited pathnames.
 */
int
is_pathname(path)
char *path;
{
	char	*f;

	if ((f = is_appendname(path)) != 0)
		return is_pathname(f);

#if VMS
	if (is_vms_pathname(path, -TRUE))
		return TRUE;
#endif

#if UNIX || MSDOS || VMS
	if ((f = path) != 0) {
#if UNIX
		if (f[0] == '~')
			return TRUE;
#endif
		if (slashc(f[0]))
			return TRUE;
		else if (*f++ == '.') {
			if (*f == '.')
				f++;
			if (slashc(f[0]))
				return TRUE;
		}
	}
#endif	/* UNIX || MSDOS */

	return FALSE;
}

/*
 * A bit weaker than 'is_pathname()', checks to see if the string contains
 * path delimiters.
 */
int
maybe_pathname(fn)
char *fn;
{
	if (is_pathname(fn))	/* test the obvious stuff */
		return TRUE;
#if MSDOS
	if (is_msdos_drive(fn))
		return TRUE;
#endif
	if (last_slash(fn) != 0)
		return TRUE;
#if VMS
	while (*fn != EOS) {
		if (ispath(*fn) && !isident(*fn))
			return TRUE;
		fn++;
	}
#endif
	return FALSE;
}

/*
 * Returns the filename portion if the argument is an append-name (and not an
 * internal name!), otherwise null.
 */
char *
is_appendname(fn)
char *fn;
{
	if (fn != 0) {
		if (isAppendToName(fn)) {
			fn += 2;	/* skip the ">>" prefix */
			while (isspace(*fn))
				fn++;
			if (!isInternalName(fn))
				return fn;
		}
	}
	return 0;
}

/*
 * Returns the special string consisting of program name + version, used to
 * fill in the filename-field for scratch buffers that are not associated with
 * an external file.
 */
char *
non_filename()
{
	static	TBUFF	*ptr;
	if (!ptr) {
		char	buf[80];
		(void)lsprintf(buf, "       %s   %s",prognam,version);
		(void)tb_scopy(&ptr, buf);
	}
	return tb_values(ptr);
}

/*
 * Returns true if the filename is either a scratch-name, or is the string that
 * we generate for the filename-field of [Help] and [Buffer List].  Use this
 * function rather than simple tests of '[' to make tests for VMS filenames
 * unambiguous.
 */
int
is_internalname(fn)
char *fn;
{
#if VMS
	if (is_vms_pathname(fn, FALSE))
		return FALSE;
#endif
	if (!strcmp(fn, non_filename()))
		return TRUE;
	return (*fn == EOS) || (*fn == SCRTCH_LEFT[0]);
}

/*
 * Test if the given path is a directory
 */
int
is_directory(path)
char *	path;
{
	struct	stat	sb;

#if VMS
	if (is_vms_pathname(path, TRUE))
		return TRUE;
#endif
	return ((*path != EOS)
	  &&	(stat(path, &sb) >= 0)
	  &&	((sb.st_mode & S_IFMT) == S_IFDIR));
}

#if NO_LEAKS
void
path_leaks()
{
#if UNIX
	while (user_paths != NULL) {
		register UPATH *paths = user_paths;
		user_paths = paths->next;
		free(paths->name);
		free(paths->path);
		free((char *)paths);
	}
#endif
}
#endif	/* NO_LEAKS */
