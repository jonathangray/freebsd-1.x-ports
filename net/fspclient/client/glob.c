/*
 * Copyright (c) 1990 Rene' Seindal
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

/*
 * Generalised to allow multiple types of directory structures
 * by Philip G. Richards 1992; all these modifications are copyright
 * Philip G. Richards, 1992.  All rights reserved.
 *
 * Both redistribution, use, and warranty are as above.
 */

#include "client.h"
#include <pwd.h>

#ifdef TEST
#define ffprintf (void)fprintf
static FILE *STDDBG = STDERR;
#endif

#ifndef ANSI_PROTOTYPES
static char *real_getname();
static VOIDDIR *real_opendir();
static void real_closedir();
static VOIDDIRENT *real_readdir();
static int real_stat();

static VOIDDIR *(*OPENDIR)()    = real_opendir;
static void (*CLOSEDIR)()       = real_closedir;
static VOIDDIRENT *(*READDIR)() = real_readdir;
static char *(*GETNAME)()       = real_getname;
static int (*GLOBSTAT)()        = real_stat;

static void matchdir();
static void do_glob();
static void add_name();
static int split_pat();

#else /* ANSI_PROTOTYPES */

static char *real_getname(VOIDDIRENT *dp);
static VOIDDIR *real_opendir(char *dirname);
static void real_closedir(VOIDDIR *dirp);
static VOIDDIRENT *real_readdir(VOIDDIR *dirp);
static int real_stat(const char *buf, struct stat *s);

static VOIDDIR *(*OPENDIR)(char *)                  = real_opendir;
static void (*CLOSEDIR)(VOIDDIR *)                  = real_closedir;
static VOIDDIRENT *(*READDIR)(VOIDDIR *)            = real_readdir;
static char *(*GETNAME)(VOIDDIRENT *)               = real_getname;
static int (*GLOBSTAT)(const char *, struct stat *) = real_stat;

static void matchdir(char *path_end, char **gpat); /* non-simple globbing */
static void do_glob(char *path_end, char **gpat);
				/* do simple globbing, on segment at a time */
static void add_name(void); /* add a name to namelist */
static int split_pat(char *pattern, char **table); /* split pattern into segments */

#endif /* ANSI_PROTOTYPES */

/* glob_match matches pattern against string according to the normal
 * rules for shell globbing.  It returns SUCCES if string matches
 * pattern, FAIL if it doesn't, and ERROR if pattern is illformed.
 *
 * To be more specific, a pattern can be:
 *	?	which matches any single character
 *	*	which matches zero or more character
 *	[...]	which matches any single character in ...
 *	[^...]	which matches any single character not in ...
 *	\x	where x is any character, matches that character literally
 *	x	where x is any character except ? * [ and \, matches x
 * 
 * Character within [...] can include single characters and ranges of
 * the form x-y, which matches any characters in the ranges x - y
 * inclusive.  It is considered an error is y < x.  An ] can be included
 * as the very first character in ..., and a - as the first (after a
 * possibly [) or last character in ...
 */

#define PSUCCES	2
#define SUCCES	1
#define FAIL	0
#define ERROR	0

#define NEXTP {if ( (p = *++pattern) == '\0') return ERROR;}

int
#ifndef ANSI_PROTOTYPES
glob_match( pattern, string )
    char *pattern;
    char *string;
#else /* ANSI_PROTOTYPES */
glob_match(char *pattern, char *string)
#endif /* ANSI_PROTOTYPES */
{
    char p;
    char seenstar = '\0';

    for ( ; (p = *pattern); pattern++ ) {

	if ( p == '\\' ) {
	    NEXTP;		/* matches next char literally (but not \0) */
	    if ( p != *string++ )
		return FAIL;	/* string too short */
	    continue;
	} else if ( p == '?' ) {
	    if ( *string++ )	/* matches any character */
		continue;
	    else
		return FAIL;	/* string too short */
	} else if ( p == '*' ) {
	    seenstar = '\1';
	    continue;
	} else {

	    if ( seenstar ) {
		int tmp;
		while ( *string ) {
		    tmp = glob_match( pattern, string );
		    if ( tmp != FAIL )
			return tmp;
		    string++;
		}
		return FAIL;
		/* NOTREACHED */
	    }

	    if ( p == '\0' )
		return FAIL;

	    if ( p == '[' ) {
		char s = *string++;
		char reverse = '\0';
		char first, last;
		char gotcha = '\0';

		NEXTP;
		if ( p == '^' ) {
		    reverse = '\1';
		    NEXTP;
		}
		if ( p == ']' ) { /* special case */
		    gotcha = (s==p);
		    NEXTP;
		}

		while (  p != ']' && !gotcha ) {
		    first = p;
		    NEXTP;
		    if ( p == '-' && pattern[1] != ']' ) {
			NEXTP;
			last = p;
			NEXTP;
		    } else
			last = first;
		    if ( first > last )
			return ERROR;
		    gotcha = (first <= s && s <= last );
		}
		while ( p != ']' )
		    NEXTP;

		if ( reverse ? gotcha : !gotcha )
		    return FAIL;
	    } else if ( p != *string )
		return FAIL;
	    else
		string++;
	}
    }
    if ( seenstar )
	return SUCCES;

    if ( *string )
	return FAIL;
    return SUCCES;
}

static char *main_path;		/* ptr to scratchpad */
static int offset;		/* no of leading char in main_path to ignore */
static char **namelist;		/* name list buildup */
static int nnames;		/* no of names found */
static int left;		/* no of slots allocated but not used yet */

#define GLOBMAXSEG	50	/* max segments in pattern */
#define GLOBCHUNK	20	/* no of slots to allocate at a time */

#ifndef MAXNAMLEN
#define MAXNAMLEN 256
#endif

int
#ifndef ANSI_PROTOTYPES
glob_path( pattern, names )
    char *pattern;
    char ***names;
#else /* ANSI_PROTOTYPES */
glob_path(char *pattern, char ***names)
#endif /* ANSI_PROTOTYPES */
{
    char mpath[ MAXPATHLEN + MAXNAMLEN + 1 ];
    char *gpat[GLOBMAXSEG];
    char *pat;

    if (pattern == 0)
	return -1;

    if ((pat = strdup(pattern)) == NULL)
	return -1;

    if (split_pat(pat, gpat) < 0)
    {
	(void)free(pat);
	return -1;
    }
    
    main_path = mpath;		/* initalisation of static storage */
    namelist = 0;
    nnames = left = 0;

    if ( *gpat && **gpat == '/' )
    {
	main_path[0] = '/';
	main_path[1] = '\0';
	offset = 0;
	do_glob(main_path, gpat + 1);
    }
    else
    {
	main_path[0] = '.';
	main_path[1] = '\0';
	offset = 2;
	do_glob(main_path + 1, gpat);
    }

    (void)free(pat);

    if (namelist == 0)
	*names = (char **)malloc(sizeof(char *));
    else
	*names = (char **)realloc(namelist, (nnames+1) * sizeof(char *));

    if (*names == 0)
	return -1;
    (*names)[nnames] = 0;
    return nnames;
}

static int
#ifndef ANSI_PROTOTYPES
split_pat(pattern, table)
    char *pattern;
    char **table;
#else /* ANSI_PROTOTYPES */
split_pat(char *pattern, char **table)
#endif /* ANSI_PROTOTYPES */
{
    char *pp = pattern;
    int size = GLOBMAXSEG;

    if (*pattern == '/')
    {
	*table++ = "/";
	--size;
    }

    do
    {
	while (*pp == '/')
	    *pp++ = '\0';
	if (*pp == '\0')
	    break;
	if (--size < 0)
	    return -1;
	*table++ = pp;
	while (*pp && *pp != '/')
	    pp++;
    }
    while (*pp);

    *table = 0;
    return 0;
}


#define ISGLOB(x) ((x)=='*' || (x)=='?' || (x)=='[')

static int
#ifndef ANSI_PROTOTYPES
no_glob(pat)
    char *pat;
#else /* ANSI_PROTOTYPES */
no_glob(char *pat)
#endif /* ANSI_PROTOTYPES */
{
    while (*pat && !ISGLOB(*pat))
	pat++;

    return (*pat == '\0');
}
	
static void
#ifndef ANSI_PROTOTYPES
do_glob(path_end, gpat)
    char *path_end;		/* ptr to the end of main_path */
    char **gpat;		/* the rest of the pattern segments */
#else /* ANSI_PROTOTYPES */
do_glob(char *path_end, char **gpat)
                   		/* ptr to the end of main_path */
                		/* the rest of the pattern segments */
#endif /* ANSI_PROTOTYPES */
{
    char *saved_end = path_end;	/* saved to be resored */
    char *pat;			/* current pattern segment */
    struct stat st;		/* to check if file exists */

#ifdef GLOBDEBUG
    ffprintf(STDDBG,"do_glob: path = '%s', pat = '%s'\n", main_path, *gpat );
#endif

    for ( ; (pat = *gpat) != 0 && no_glob(pat); gpat++ )
    {
#ifdef GLOBDEBUG
	ffprintf(STDDBG,"no_glob: path = '%s', pat = '%s'\n", main_path, pat );
#endif
	*path_end = '/';
	(void)strcpy(path_end+1, pat);
	path_end += strlen(pat) + 1;

	if (GLOBSTAT(main_path, &st) != 0 )
	{
	    *saved_end = '\0';
	    return;
	}
    }
    if (pat)
	matchdir(path_end, gpat);
    else
	add_name();

    *saved_end = '\0';
    return;
}

static void
#ifndef ANSI_PROTOTYPES
matchdir(path_end, gpat)
    char *path_end;		/* ptr to end of main_path */
    char **gpat;		/* the rest of the pattern segments */
#else /* ANSI_PROTOTYPES */
matchdir(char *path_end, char **gpat)
                   		/* ptr to end of main_path */
                		/* the rest of the pattern segments */
#endif /* ANSI_PROTOTYPES */
{
    char *x;			/* scratch */
    VOIDDIR *dirp;		/* for directory reading */
    VOIDDIRENT *dp;		/* directory entry */
    struct stat st;		/* to determine files type */

#ifdef GLOBDEBUG
    ffprintf(STDDBG,"matchdir: path = '%s', pat = '%s'\n", main_path, *gpat );
#endif
    if ((dirp = OPENDIR(main_path)) == NULL)
	return;

    *path_end = '/';

    while ((dp = READDIR(dirp)) != NULL)
    {
	char *dirname;
	x = dirname = GETNAME(dp);	/* was dp->d_name */
	if (*x == '.' && (*++x == '\0' || (*x == '.' && *++x == '\0')))
	    continue;
	if (*dirname == '.' && **gpat != '.')
	    continue;

	(void)strcpy(path_end + 1, dirname);

	if (glob_match(*gpat, dirname))
	{   /* this is a match */
	    if ( *(gpat+1) == 0 )
	    {	/* and it is the last */
		add_name();	/* so eat it */
		continue;
	    }
	    if (GLOBSTAT(main_path, &st) == 0 /* else not the last */
		&& (st.st_mode & S_IFMT) == S_IFDIR)
		do_glob(path_end + strlen(dirname) + 1, gpat + 1);
	} 
    }

    (void)CLOSEDIR(dirp);

    *path_end = '\0';
}

static void
#ifndef ANSI_PROTOTYPES
add_name()
#else /* ANSI_PROTOTYPES */
add_name(void)
#endif /* ANSI_PROTOTYPES */
{
    char *np;
#ifdef GLOBDEBUG
    ffprintf(STDDBG,"Globbed: %s\n", main_path+offset);
#endif

    if (--left <= 0)
    {
	if (namelist == 0)
	    namelist = (char**)malloc(GLOBCHUNK * sizeof(char*));
	else
	    namelist = (char**)realloc(namelist,
			       (nnames+GLOBCHUNK)*sizeof(char*));

	if (namelist == NULL)
	    return;
	
	left = GLOBCHUNK;
    }

    if ((np = strdup(main_path + offset)) == 0)
	return;

    namelist[nnames++] = np;
}

/* the following is not general purpose code. */
const char *globerr;
char globerr_buf[1024];
char *home = 0;

/* extern int qsort(); */

static int
#ifndef ANSI_PROTOTYPES
name_cmp(s1, s2)
    char **s1;
    char **s2;
#else /* ANSI_PROTOTYPES */
name_cmp(char **s1, char **s2)
#endif /* ANSI_PROTOTYPES */
{
    return strcmp(*s1, *s2);
}


char *
#ifndef ANSI_PROTOTYPES
expand_tilde(pat)
    char *pat;
#else /* ANSI_PROTOTYPES */
expand_tilde(char *pat)
#endif /* ANSI_PROTOTYPES */
{
    static char buf[BUFSIZ];

    struct passwd *pw;
    char *tmp;
    char *bp;

    if (*pat != '~')
	return 0;

    pat++;
    if (*pat && *pat != '/')
    {
	bp = buf;
	for (tmp = pat; *tmp && *tmp != '/'; )
	    *bp++ = *tmp++;
	*bp = '\0';

	pw = getpwnam(buf);
	if (pw == 0)
	{
	    (void)sprintf(globerr_buf, "%s: Unknown user.", pat);
	    globerr = globerr_buf;
	    return 0;
	}
	pat = tmp ? tmp : "";
	tmp = pw->pw_dir;
    }
    else
    {
	if (*pat)
	    pat++;
	tmp = (char *)getenv("HOME");
     }

    for (bp = buf; *tmp; )
	*bp++ = *tmp++;
    *bp++ = '/';

    while (*pat)
	*bp++ = *pat++;
    *bp = '\0';

    return buf;
}

char **
#ifndef ANSI_PROTOTYPES
glob(pat)
    char *pat;
#else /* ANSI_PROTOTYPES */
glob(char *pat)
#endif /* ANSI_PROTOTYPES */
{
    char **names;
    int nnames;

    if (*pat == '~')
    {
	pat = expand_tilde(pat);
	if (pat == 0)
	    return 0;
    }

    nnames = glob_path(pat, &names);

    switch (nnames)
    {
      case -1:
	globerr = sys_errlist[errno];
	return 0;
      case 0:
	globerr = "No match.";
	return 0;
      default:
	qsort(names, nnames, sizeof(char *), name_cmp);
	return names;
    }
}

void
#ifndef ANSI_PROTOTYPES
free_glob(argv)
    char **argv;
#else /* ANSI_PROTOTYPES */
free_glob(char **argv)
#endif /* ANSI_PROTOTYPES */
{
    char **a;

    if (argv == (char**)0)
	return;

    for (a = argv; *a; a++)
	(void)free(*a);

    (void)free((char*)argv);
}

void
#ifndef ANSI_PROTOTYPES
set_glob_routines(dopen, dclose, dread, dgetname, dstat)
    VOIDDIR	*(*dopen)();
    void	(*dclose)();
    VOIDDIRENT	*(*dread)();
    char	*(*dgetname)();
    int		(*dstat)();
#else /* ANSI_PROTOTYPES */
set_glob_routines(VOIDDIR *(*dopen)(char *),
		  void (*dclose)(VOIDDIR *),
		  VOIDDIRENT *(*dread)(VOIDDIR *),
		  char *(*dgetname)(VOIDDIRENT *),
		  int (*dstat)(const char *, struct stat *))
#endif /* ANSI_PROTOTYPES */
{
    OPENDIR	= dopen;
    CLOSEDIR	= dclose;
    GETNAME	= dgetname;
    READDIR	= dread;
    GLOBSTAT	= dstat;
}

static char
#ifndef ANSI_PROTOTYPES
*real_getname(dp)
    VOIDDIRENT *dp;
#else /* ANSI_PROTOTYPES */
*real_getname(VOIDDIRENT *dp)
#endif /* ANSI_PROTOTYPES */
{
    return ((struct dirent *)dp)->d_name;
}

/*****************************************************************************
* wrappers for the real functions
*****************************************************************************/
static VOIDDIR *
#ifndef ANSI_PROTOTYPES
real_opendir(dirname)
    char *dirname;
#else /* ANSI_PROTOTYPES */
real_opendir(char *dirname)
#endif /* ANSI_PROTOTYPES */
{
    return (VOIDDIR*)opendir(dirname);
}

static void
#ifndef ANSI_PROTOTYPES
real_closedir(dirp)
    VOIDDIR *dirp;
#else /* ANSI_PROTOTYPES */
real_closedir(VOIDDIR *dirp)
#endif /* ANSI_PROTOTYPES */
{
    (void)closedir((DIR*)dirp);
}

static VOIDDIRENT *
#ifndef ANSI_PROTOTYPES
real_readdir(dirp)
    VOIDDIR *dirp;
#else /* ANSI_PROTOTYPES */
real_readdir(VOIDDIR *dirp)
#endif /* ANSI_PROTOTYPES */
{
    return (VOIDDIRENT*)readdir((DIR*)dirp);
}

static int
#ifndef ANSI_PROTOTYPES
real_stat(buf, s)
    const char *buf;
    struct stat *s;
#else /* ANSI_PROTOTYPES */
real_stat(const char *buf, struct stat *s)
#endif /* ANSI_PROTOTYPES */
{
    return stat(buf, s);
}

void
#ifndef ANSI_PROTOTYPES
local_glob_routines()
#else /* ANSI_PROTOTYPES */
local_glob_routines(void)
#endif /* ANSI_PROTOTYPES */
{
    set_glob_routines(real_opendir, real_closedir,
		      real_readdir, real_getname, real_stat);
}

#ifndef TEST
/*****************************************************************************
* wrappers for the FSP remote functions
*****************************************************************************/
static char *
#ifndef ANSI_PROTOTYPES
my_getname(dp)
    VOIDDIRENT *dp;
#else /* ANSI_PROTOTYPES */
my_getname(VOIDDIRENT *dp)
#endif /* ANSI_PROTOTYPES */
{
    return ((struct rdirent *)dp)->rd_name;
}

static VOIDDIR *
#ifndef ANSI_PROTOTYPES
my_opendir(dirname)
    char *dirname;
#else /* ANSI_PROTOTYPES */
my_opendir(char *dirname)
#endif /* ANSI_PROTOTYPES */
{
    return (VOIDDIR*)util_opendir(dirname);
}

static void
#ifndef ANSI_PROTOTYPES
my_closedir(dirp)
    VOIDDIR *dirp;
#else /* ANSI_PROTOTYPES */
my_closedir(VOIDDIR *dirp)
#endif /* ANSI_PROTOTYPES */
{
    (void)util_closedir((RDIR*)dirp);
}

static VOIDDIRENT *
#ifndef ANSI_PROTOTYPES
my_readdir(dirp)
    VOIDDIR *dirp;
#else /* ANSI_PROTOTYPES */
my_readdir(VOIDDIR *dirp)
#endif /* ANSI_PROTOTYPES */
{
    return (VOIDDIRENT*)util_readdir((RDIR*)dirp);
}

void
#ifndef ANSI_PROTOTYPES
remote_glob_routines()
#else /* ANSI_PROTOTYPES */
remote_glob_routines(void)
#endif /* ANSI_PROTOTYPES */
{
    set_glob_routines(my_opendir,my_closedir,my_readdir,my_getname,util_stat);
}
#endif

#ifdef TEST

int
#ifndef ANSI_PROTOTYPES
main(argc, argv)
    int argc;
    char *argv[];
#else /* ANSI_PROTOTYPES */
main(int argc, char *argv[])
#endif /* ANSI_PROTOTYPES */
{
    char **names;

    globerr = 0;
    home = getenv("HOME");

    names = glob(argv[1]);

    if (names == 0) {
	ffprintf(STDERR, "glob error: %s\n", globerr);
	return 1;
    }

    while (*names)
	ffprintf(STDOUT,"%s\n", *names++);

    return 0;
}
#endif 
