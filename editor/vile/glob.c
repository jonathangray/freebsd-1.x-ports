/*
 *	glob.c
 *
 * Performs wildcard-expansion for UNIX, VMS and MS-DOS systems.
 * Written by T.E.Dickey for vile (april 1993).
 *
 * (MS-DOS code was originally taken from the winc app example of the
 * zortech compiler - pjr)
 *
 * To do:
 *	allow '^' treatment in range-expressions.
 *
 *	make the wildcard expansion know about escaped characters (e.g.,
 *	with backslash a la UNIX.
 *
 *	modify (ifdef-style) 'expand_leaf()' to allow ellipsis.
 *
 * $Log: glob.c,v $
 * Revision 1.1  1994/02/01 03:29:23  jkh
 * Initial revision
 *
 * Revision 1.14  1993/09/06  16:36:47  pgf
 * changed glob() to doglob() to avoid symbol conflicts
 *
 * Revision 1.13  1993/09/03  09:11:54  pgf
 * tom's 3.60 changes
 *
 * Revision 1.12  1993/08/05  14:29:12  pgf
 * tom's 3.57 changes
 *
 * Revision 1.11  1993/06/25  11:25:55  pgf
 * patches for Watcom C/386, from Tuan DANG
 *
 * Revision 1.10  1993/06/18  15:57:06  pgf
 * tom's 3.49 changes
 *
 * Revision 1.9  1993/06/02  14:28:47  pgf
 * see tom's 3.48 CHANGES
 *
 * Revision 1.8  1993/05/11  16:22:22  pgf
 * see tom's CHANGES, 3.46
 *
 * Revision 1.7  1993/05/06  11:59:58  pgf
 * added ifdefs for USE_D_NAMLEN, for systems that don't have or don't
 * need it (d_name[] is null-terminated on most systems)
 *
 * Revision 1.6  1993/04/28  14:34:11  pgf
 * see CHANGES, 3.44 (tom)
 *
 * Revision 1.5  1993/04/22  11:08:03  pgf
 * suppress some "may be used before set" warnings
 *
 * Revision 1.4  1993/04/21  16:10:08  pgf
 * typo
 *
 * Revision 1.3  1993/04/21  15:56:49  pgf
 * allow nulls to separate filenames from the pipe
 *
 * Revision 1.2  1993/04/21  15:45:04  pgf
 * added iswild() check to catch more possible wild characters (UNIX_GLOBBING
 * only)
 *
 * Revision 1.1  1993/04/21  14:39:07  pgf
 * glob mode support, either string or boolean
 *
 * Revision 1.0  1993/04/20  12:14:31  pgf
 * Initial revision
 *
 */

#include "estruct.h"	/* global structures and defines */
#include "edef.h"	/* defines 'slash' */
#include "dirstuff.h"	/* directory-scanning interface & definitions */
#include "glob.h"	/* interface & definitions for this module */

#define	isname(c)	(isalnum(c) || ((c) == '_'))
#define	isdelim(c)	((c) == '(' || ((c) == '{'))

#if MSDOS
# define UNIX_GLOBBING !SMALLER
# if UNIX_GLOBBING
#  define DirEntryStr(p)		p->d_name
# else
#  ifdef __ZTC__
#   define DeclareFind(p)		struct FIND *p
#   define DirEntryStr(p)		p->name
#   define DirFindFirst(path,p)		(p = findfirst(path, 0))
#   define DirFindNext(p)		(p = findnext())
#  else
#   define DeclareFind(p)		struct find_t p
#   define DirEntryStr(p)		p.name
#   define DirFindFirst(path,p)		(!_dos_findfirst(path, 0, &p))
#   define DirFindNext(p)		(!_dos_findnext(&p))
#  endif
# endif
# define DirEntryLen(p)			strlen(DirEntryStr(p))
#endif	/* MSDOS */

/*
 * Make the default unix globbing code use 'echo' rather than our internal
 * globber if we do not configure the 'glob' string-mode.
 */
#if UNIX && defined(GVAL_GLOB)
# define UNIX_GLOBBING 1
#endif

#ifndef UNIX_GLOBBING
# define UNIX_GLOBBING 0
#endif

/*
 * Verify that we don't have both boolean- and string-valued 'glob' modes.
 */
#if defined(GMDGLOB) && defined(GVAL_GLOB)
	huh??
#else
# ifdef GMDGLOB		/* boolean */
#  define globbing_active() global_g_val(GVAL_GLOB)
# endif
# ifdef GVAL_GLOB	/* string */
#  define globbing_active() strcmp("off", global_g_val_ptr(GVAL_GLOB))
# endif
# ifndef globbing_active
#  define globbing_active() TRUE
# endif
#endif

/*--------------------------------------------------------------------------*/

static	int	string_has_wildcards P((char *));
static	int	record_a_match P((char *));
static	int	expand_pattern P((char *));
#if OPT_GLOB_ENVIRON
static	void	expand_environ P(( char * ));
#endif

/* the expanded list is defined outside of the functions because if we
 * handle ellipsis, the generating function must be recursive.
 */
static	SIZE_T	myMax, myLen;	/* length and index of the expanded list */
static	char **	myVec;		/* the expanded list */

/*--------------------------------------------------------------------------*/
static int
string_has_wildcards (item)
char	*item;
{
#if VMS || UNIX || MSDOS
	while (*item != EOS) {
#if UNIX_GLOBBING
		if (iswild(*item))
			return TRUE;
#endif
		if (*item == GLOB_SINGLE || *item == GLOB_MULTI)
			return TRUE;
#if OPT_GLOB_ELLIPSIS || VMS
		if (!strncmp(item, GLOB_ELLIPSIS, sizeof(GLOB_ELLIPSIS)-1))
			return TRUE;
#endif
#if OPT_GLOB_RANGE && !VMS
		if (*item == GLOB_RANGE[0])
			return TRUE;
#endif
#if OPT_GLOB_ENVIRON && !VMS
		if (*item == '$' && (isname(item[1]) || isdelim(item[1])))
			return TRUE;
#endif
		item++;
	}
#endif
	return FALSE;
}

/*
 * Record a pattern-match in 'myVec[]', returning false if an error occurs
 */
static int
record_a_match(item)
char	*item;
{
	if (item != 0 && *item != EOS) {
		if (!(item = strmalloc(item)))
			return no_memory("glob-match");

		if (myLen + 2 >= myMax) {
			myMax = myLen + 2;
			if (myVec == 0)
				myVec = typeallocn(char *, myMax);
			else
				myVec = typereallocn(char *, myVec, myMax);
		}
		if (myVec == 0)
			return no_memory("glob-pointers");

		myVec[myLen++] = item;
		myVec[myLen] = 0;
	}
	return TRUE;
}

#if UNIX_GLOBBING
static	char *	next_leaf P(( char * ));
static	char *	wild_leaf P(( char * ));
static	int	match_leaf P(( char *, char * ));
static	int	expand_leaf P(( char *, char * ));

/*
 * Point to the leaf following the given string (i.e., skip a slash), returns
 * null if none is found.
 */
static char *
next_leaf (path)
char	*path;
{
	if (path != 0) {
		while (*path != EOS) {
			if (slashc(*path))
				return path+1;
			path++;
		}
	}
	return 0;
}

/*
 * Point to the beginning (after slash, if present) of the first leaf in
 * the given pattern argument that contains a wildcard.
 */
static char *
wild_leaf (pattern)
char	*pattern;
{
	register int	j, k, c, ok;

	/* skip leading slashes */
	for (j = 0; pattern[j] != EOS && slashc(pattern[j]); j++)
		;

	/* skip to the leaf with wildcards */
	while (pattern[j] != EOS) {
		int	skip = FALSE;
		for (k = j+1; (c = pattern[k]) != EOS; k++) {
			if (slashc(c)) {
				pattern[k] = EOS;
				ok = string_has_wildcards(pattern+j);
				pattern[k] = c;
				if (ok)
					return pattern+j;
				skip = TRUE;	/* skip this leaf */
				break;
			}
		}
		if (skip)
			j = k+1;	/* point past slash */
		else if (c == EOS)
			break;
		else
			j++;		/* leaf is empty */
	}
	return string_has_wildcards(pattern+j) ? pattern+j : 0;
}

/*
 * This is the heart of the wildcard matching.  We are given a directory
 * leaf and a pointer to the leaf that contains wildcards that we must
 * match against the leaf.
 */
static int
match_leaf(leaf, pattern)
char	*leaf;
char	*pattern;
{
	while (*leaf != EOS && *pattern != EOS) {
		if (*pattern == GLOB_SINGLE) {
			leaf++;
			pattern++;
		} else if (*pattern == GLOB_MULTI) {
			int	multi = FALSE;
			pattern++;
			while (*leaf != EOS) {
				if (match_leaf(leaf, pattern)) {
					multi = TRUE;
					break;
				}
				leaf++;
			}
			if (!multi && *leaf != EOS)
				return FALSE;
#if OPT_GLOB_RANGE
		} else if (*pattern == GLOB_RANGE[0]) {
			int	found	= FALSE;
			char *	first	= ++pattern;

			while (*pattern != EOS) {
				if (*pattern == GLOB_RANGE[1]) {
					pattern++;
					break;
				}
				if (*pattern == '-' && pattern != first) {
					int	lo = pattern[-1];
					int	hi = pattern[1];
					if (hi == GLOB_RANGE[1])
						hi = '~';
					if ((lo <= *leaf) && (*leaf <= hi))
						found = TRUE;
					if (pattern[1] != GLOB_RANGE[1])
						pattern++;
				} else if (*pattern++ == *leaf)
					found = TRUE;
			}
			if (!found)
				return FALSE;
#endif
		} else if (*pattern++ != *leaf++)
			return FALSE;
	}
	return (*leaf == EOS && *pattern == EOS);
}

/*
 * Recursive procedure that allows any leaf (or all!) leaves in a path to
 * have wildcards.  Except for an ellipsis, each wildcard is completed
 * within a single leaf.
 *
 * Returns false if we ran out of memory (a problem on ms-dos), and true
 * if everything went well (e.g., matches).
 */
static int
expand_leaf (path, pattern)
char	*path;		/* built-up pathname, top-level */
char	*pattern;
{
	DIR	*dp;
	DIRENT	*de;
	int	result	= TRUE;
	int	save = 0; /* warning suppression */
	SIZE_T	len;
	char	*leaf;
	char	*wild	= wild_leaf(pattern);
	char	*next	= next_leaf(wild);
	register char	*s;

	/* Fill-in 'path[]' with the non-wild leaves that we skipped to get
	 * to 'wild'.
	 */
	if (wild == pattern) {	/* top-level, first leaf is wild */
		if (*path == EOS)
			(void)strcpy(path, ".");
	} else {
		len = wild - pattern - 1;
		if (*(s = path) != EOS) {
			s += strlen(s);
			*s++ = slash;
		}
		if (len != 0)
			strncpy(s, pattern, len)[len] = EOS;
	}
	leaf = path + strlen(path) + 1;

	if (next != 0) {
		save = next[-1];
		next[-1] = EOS;		/* restrict 'wild[]' to one leaf */
	}

	/* Scan the directory, looking for leaves that match the pattern.
	 */
	if ((dp = opendir(path)) != 0) {
		leaf[-1] = slash;
		while ((de = readdir(dp)) != 0) {
#if MSDOS
			(void)mklower(strcpy(leaf, de->d_name));
			if (strchr(pattern, '.') && !strchr(leaf, '.'))
				(void)strcat(leaf, ".");
#else
#if USE_D_NAMLEN
			len = de->d_namlen;
			strncpy(leaf, de->d_name, len)[len] = EOS;
#else
			(void)strcpy(leaf, de->d_name);
#endif
#endif
			if (!strcmp(leaf, ".")
			 || !strcmp(leaf, ".."))
				continue;
			if (!match_leaf(leaf, wild))
				continue;
			if (next != 0) {	/* there are more leaves */
				if (!string_has_wildcards(next)) {
					s = leaf + strlen(leaf);
					*s++ = slash;
					(void)strcpy(s, next);
					if (!record_a_match(path)) {
						result = FALSE;
						break;
					}
				} else if (is_directory(path)) {
#if MSDOS
					s = strrchr(path, '.');
					if (s[1] == EOS)
						s[0] = EOS;
#endif
					if (!expand_leaf(path, next)) {
						result = FALSE;
						break;
					}
				}
			} else if (!record_a_match(path)) {
				result = FALSE;
				break;
			}
		}
		(void)closedir(dp);
	} else
		result = SORTOFTRUE;	/* at least we didn't run out of memory */

	if (next != 0)
		next[-1] = save;

	return result;
}

/*
 * Comparison-function for 'qsort()'
 */
#if __STDC__ || defined(__TURBOC__) || WATCOM
#if defined(apollo) && !defined(__STDCPP__)
#define	ANSI_QSORT 0	/* cc 6.7 */
#else
#define	ANSI_QSORT 1
#endif
#else
#define	ANSI_QSORT 0
#endif

#if !ANSI_QSORT
static	int	compar P(( char **, char ** ));
#endif

static int
#if ANSI_QSORT
compar (const void *a, const void *b)
#else
compar (a, b)
char	**a;
char	**b;
#endif
{
	return strcmp(*(char **)a, *(char **)b);
}
#endif

#if OPT_GLOB_PIPE
static	int	glob_from_pipe P(( char *));

static int
glob_from_pipe(pattern)
char	*pattern;
{
	FILE	*cf;
	char	tmp[NFILEN];
	int	result = FALSE;
	register SIZE_T len;
	register char *s, *d;
#ifdef GVAL_GLOB
	char	*cmd = global_g_val_ptr(GVAL_GLOB);
	int	single,
		save	= EOS;

	/*
	 * For now, assume that only 'echo' will supply the result all on
	 * one line.  Other programs (e.g., 'ls' and 'find' do the sensible
	 * thing and break up the output with newlines.
	 */
	for (d = cmd+1; *d != EOS && isspace(*d); d++)
		;
	for (s = d; *s != EOS; s++)
		if (isspace(*s)) {
			save = *s;
			*s = EOS;
			break;
		}
	single = !strcmp(pathleaf(d), "echo");
	if (save != EOS)
		*s = save;
#else
	static	char	*cmd = "!echo %s";
	static	int	single = TRUE;
	d = cmd+1;
#endif

	(void)lsprintf(tmp, d, pattern);
	if ((cf = npopen(tmp, "r")) != 0) {
		char	old[NFILEN+1];

		*(d = old) = EOS;

		while ((len = fread(tmp, sizeof(*tmp), sizeof(tmp), cf)) > 0) {
			/*
			 * Split the buffer up.  If 'single', split on all
			 * whitespace, otherwise only on newlines.
			 */

			for (s = tmp; s-tmp < len; s++) {
				if ((single && isspace(*s))
				 || (!single && (*s == '\n' || *s == '\0'))) {
					*d = EOS;
					result = record_a_match(d = old);
					*d = EOS;
					if (!result)
						break;
				 } else {
				 	*d++ = *s;
				 }
			}
		}
		if (*old != EOS)
			result = record_a_match(old);
		npclose(cf);
	} else
		result = FALSE;

	return result;
}
#endif

#if OPT_GLOB_ENVIRON
/*
 * Expand environment variables in 'pattern[]'
 * It allows names of the form
 *
 *	$NAME
 *	$(NAME)
 *	${NAME}
 */
static void
expand_environ(pattern)
char	*pattern;
{
	register int	j, k;
	int	delim,
		left,
		right;
	char	*s;
	char	save[NFILEN];

	for (j = 0; pattern[j] != EOS; j++) {
		if (pattern[j] == '$') {

			k = j+1;
			if (pattern[k] == '(')		delim = ')';
			else if (pattern[k] == '{')	delim = '}';
			else				delim = EOS;

			if (delim != EOS)
				k++;
			left	=
			right	= k;

			while (pattern[k] != EOS) {
				right = k;
				if (delim != EOS) {
					if (pattern[k++] == delim)
						break;
				} else if (isname(pattern[k])) {
					k++;
				} else {
					break;
				}
			}

			(void)strcpy(save, pattern+k);
			if (right != left) {
				pattern[right] = EOS;
				if ((s = getenv(pattern+left)) == 0)
					s = "";
			} else
				s = "";

			(void)strcpy(pattern+j, s);
			(void)strcat(pattern, save);
			j += strlen(s) - 1;
		}
	}
}
#else
#define	expand_environ(pattern)
#endif

/*
 * Notes:
 *	VMS's sys$search function (embedded in our fake 'readdir()') handles
 *	all of the VMS wildcards.
 *
 *	MS-DOS has non-UNIX functions that scan a directory and recognize DOS-style
 *	wildcards.  Use these to get the smallest executable.  However, DOS-
 *	style wildcards are crude and clumsy compared to UNIX, so we provide them as
 *	an option.  (For example, the DOS-wildcards won't match "..\*\*.bak").
 */
static int
expand_pattern (item)
char	*item;
{
	int	result;
#if VMS
	DIR	*dp;
	DIRENT	*de;

	result = TRUE;
	if (dp = opendir(item)) {
		while (de = readdir(dp)) {
#if USE_D_NAMLEN
			char	temp[NFILEN];
			int	len = de->d_namlen;
			strncpy(temp, de->d_name, len)[len] = EOS;
			if (!record_a_match(temp)) {
				result = FALSE;
				break;
			}
#else
			if (!record_a_match(strcpy(temp, de->d_name))) {
				result = FALSE;
				break;
			}
#endif
		}
		(void)closedir(dp);
	} else
		result = FALSE;
#endif
#if OPT_GLOB_PIPE
# ifdef GVAL_GLOB
	if (isShellOrPipe(global_g_val_ptr(GVAL_GLOB))) {
		result = glob_from_pipe(item);
	} else
# else
	result = glob_from_pipe(item);
#  if UNIX_GLOBBING
	huh ??		/* thought I turned that off ... */
#  endif
# endif
#endif
#if UNIX_GLOBBING
	{
	char	builtup[NFILEN];
	char	pattern[NFILEN];
	SIZE_T	first	= myLen;

	(void)strcpy(pattern, item);
	*builtup = EOS;
#if MSDOS
	(void)mklower(pattern);
#endif
	expand_environ(pattern);
	if (string_has_wildcards(pattern)) {
		if ((result = expand_leaf(builtup, pattern)) != FALSE
		 && (myLen-first > 1)) {
			qsort((char *)&myVec[first], myLen-first, sizeof(*myVec), compar);
		}
	} else
		result = record_a_match(pattern);
	}
#endif				/* UNIX-style globbing */
#if MSDOS && !UNIX_GLOBBING
	/* native DOS-wildcards */
	DeclareFind(p);
	char	temp[FILENAME_MAX + 1];
	char    path[FILENAME_MAX + 1];
	char *cp = pathleaf(
			strcpy(temp,
				strcpy(path, item)));

	result = TRUE;
	if (DirFindFirst(path,p)) {
		do {
			(void)strcpy(cp, DirEntryStr(p));
			if (!record_a_match(temp)) {
				result = FALSE;
				break;
			}
		} while (DirFindNext(p));
	}
#endif				/* native MS-DOS globbing */
	return result;		/* true iff no errors noticed */
}

/*--------------------------------------------------------------------------*/

/*
 * Tests a list of items to see if at least one of them needs to be globbed.
 */
#if !UNIX
int
glob_needed (list_of_items)
char	**list_of_items;
{
	register int 	n;

	for (n = 0; list_of_items[n] != 0; n++)
		if (string_has_wildcards(list_of_items[n]))
			return TRUE;
	return FALSE;
}
#endif

/*
 * Expands the items in a list, returning an entirely new list (so it can
 * be freed without knowing where it came from originally).
 */
char **
glob_expand (list_of_items)
char	**list_of_items;
{
	int	len = glob_length(list_of_items);
	int	i;

	myMax = 0;
	myLen = 0;
	myVec = 0;

	for (i = 0; i < len; ++i) {
		char	*item = list_of_items[i];
		/*
		 * For UNIX, expand '~' expressions in case we've got a pattern
		 * like "~/test*.log".
		 */
#if UNIX
		char	temp[NFILEN];
		item = home_path(strcpy(temp, item));
#endif
		if (!isInternalName(item)
		 && globbing_active()
		 && string_has_wildcards(item)) {
			if (!expand_pattern(item))
				return 0;
		} else if (!record_a_match(item)) {
			return 0;
		}
	}
	return myVec;
}

/*
 * A special case of 'glob_expand()', expands a single string into a list.
 */
char **
glob_string (item)
char	*item;
{
	char	*vec[2];

	vec[0] = item;
	vec[1] = 0;

	return glob_expand(vec);
}

/*
 * Counts the number of items in a list of strings.  This is simpler (and
 * more useful) than returning the length and the list as arguments from
 * a procedure.  Note that since the standard argc/argv convention puts a
 * null pointer on the end, this function is applicable to the 'argv[]'
 * parameter of the main program as well.
 */
int
glob_length (list_of_items)
char	**list_of_items;
{
	register int	len;
	if (list_of_items != 0) {
		for (len = 0; list_of_items[len] != 0; len++)
			;
	} else
		len = 0;
	return len;
}

/*
 * Frees the strings in a list, and the list itself.  Note that this should
 * not be used for the main program's original argv, because on some systems
 * it is a part of a larger data area, as are the command strings.
 */
char **
glob_free (list_of_items)
char	**list_of_items;
{
	register int	len;
	if (list_of_items != 0) {
		for (len = 0; list_of_items[len] != 0; len++)
			free(list_of_items[len]);
		free ((char *)list_of_items);
	}
	return 0;
}


#if !UNIX
/*
 * Expand wildcards for the main program a la UNIX shell.
 */
void
expand_wild_args(argcp, argvp)
int *argcp;
char ***argvp;
{
	if (glob_needed(*argvp)) {
		char	**newargs = glob_expand(*argvp);
		if (newargs != 0) {
			*argvp = newargs;
			*argcp = glob_length(newargs);
		}
	}
}
#endif

/*
 * Expand a string, permitting only one match.
 */
int
doglob (path)
char	*path;
{
	char	**expand = glob_string(path);

	if (glob_length(expand) > 1) {
		if (mlyesno("Too many filenames.  Use first") != TRUE) {
			(void)glob_free(expand);
			return FALSE;
		}
	}
	(void)strcpy(path, expand[0]);
	(void)glob_free(expand);
	return TRUE;
}
