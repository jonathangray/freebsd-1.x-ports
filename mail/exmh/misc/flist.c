/*
 * flist.c --
 * List MH folders with unseen messages
 * David Nichols, Xerox-PARC, November, 1992
 *
 * Copyright (c) 1994 Xerox Corporation.
 * Use and copying of this software and preparation of derivative works based
 * upon this software are permitted. Any distribution of this software or
 * derivative works must comply with all applicable United States export
 * control laws. This software is made available AS IS, and Xerox Corporation
 * makes no warranty about the software, its performance or its conformity to
 * any specification.
 */

#include <stdio.h>
#include "../h/mh.h"
#include "../h/local.h"
#ifdef notdef
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#endif

#define FALSE   0
#define TRUE    1

static struct swit switches[] = {
#define ALLSW           0
    "all", 0,
#define UNSEENSW        1
    "unseen", 0,
#define HELPSW          2
    "help", 4,
#define ALPHASW         3
    "alpha", 0,
#define NOALPHASW       4
    "noalpha", 0,
#define TOTAL           5
    "total", 0,
#define NOTOTAL         6
    "nototal", 0,
#define RECURSE         7
    "recurse", 0,
#define NORECURSE       8
    "norecurse", 0,
    NULL, NULL
};

struct Folder {
    char *name;
    int priority;
    int nMsgs;
    int nUnseen;
};
struct Folder *orders = NULL;
int nOrders = 0;
int nOrdersAlloced = 0;
struct Folder *folders = NULL;
int nFolders = 0;
int nFoldersAlloced = 0;

char **foldersToDo;
int nFoldersToDo;

int allFolders = FALSE;		/* show all folders or just with unseen msgs? */
int alphaOrder = FALSE;		/* want alphabetical order only */
int noTotal = FALSE;		/* display the folder name only? */
int recurse = FALSE;		/* show nested folders? */
char *unseenSequence;

/* Forwards */
int CompareFolders();

main(argc, argv)
    int argc;
    char *argv[];
{
    char *cp;
    char **ap;
    char **argp;
    char **lastArg;
    char *arguments[MAXARGS];
    char buf[100];

    invo_name = r1bindex(argv[0], '/');
    if ((cp = m_find(invo_name)) != NULL) {
	ap = brkstring(cp = getcpy(cp), " ", "\n");
	ap = copyip(ap, arguments);
    } else
	ap = arguments;
    lastArg = copyip(argv + 1, ap);
    argp = arguments;
    argc = lastArg - argp;
    foldersToDo = (char **) malloc(argc * sizeof(char *));
    nFoldersToDo = 0;

    while (cp = *argp++) {
	if (*cp == '-') {
	    switch (smatch(++cp, switches)) {
	    case AMBIGSW:
		ambigsw(cp, switches);
		done(1);
	    case UNKWNSW:
		adios(NULLCP, "-%s unknown", cp);
	    case ALLSW:
		allFolders = TRUE;
		break;
	    case UNSEENSW:
		allFolders = FALSE;
		break;
	    case ALPHASW:
		alphaOrder = TRUE;
		break;
	    case NOALPHASW:
		alphaOrder = FALSE;
		break;
	    case TOTAL:
		noTotal = FALSE;
		break;
	    case NOTOTAL:
		noTotal = TRUE;
		break;
	    case RECURSE:
		recurse = TRUE;
		break;
	    case NORECURSE:
		recurse = FALSE;
		break;
	    case HELPSW:
		(void) sprintf(buf, "%s [switches]", invo_name);
		help(buf, switches);
		done(1);
	    }
	} else {
	    if (*cp == '+')
		++cp;
	    foldersToDo[nFoldersToDo++] = cp;
	}
    }

    unseenSequence = m_find("Unseen-Sequence");

    GetFolderOrder();
    BuildFolderList();
    if (nFoldersToDo == 0)
	qsort(folders, nFolders, sizeof(struct Folder), CompareFolders);
    PrintFolders();
}

/*
 * Read the folder-order profile entry to determine how to sort folders for
 * output.
 */
GetFolderOrder()
{
    char *p = m_find("Folder-order");
    char *s;
    int priority = 1;
    struct Folder *o;

    if (p == NULL)
	return;
    for (;;) {
	while (isspace(*p))
	    ++p;
	s = p;
	while (*p && !isspace(*p))
	    ++p;
	if (p != s) {
	    /* Found one. */
	    AllocFolders(&orders, &nOrdersAlloced, nOrders + 1);
	    o = &orders[nOrders++];
	    o->priority = priority++;
	    o->name = (char *) malloc(p - s + 1);
	    strncpy(o->name, s, p - s);
	    o->name[p - s] = 0;
	} else
	    break;
    }
}

/* Build our list of folders. */
BuildFolderList()
{
    char *home = getenv("HOME");
    char *path = m_find("path");
    int i;

    if (home != NULL && chdir(home) < 0)
	adios(NULL, "Can't chdir to home dir.");
    if (path != NULL && chdir(path) < 0)
	adios(NULL, "Can't chdir to mail dir (%s).", path);
    if (nFoldersToDo > 0) {
	for (i = 0; i < nFoldersToDo; ++i)
	    AddFolder(foldersToDo[i], TRUE);
    } else {
	BuildFolderListR("");
    }
}

BuildFolderListR(dirName)
    char *dirName;
{
    DIR *dir;
    struct direct *dp;
    struct stat s;
    char name[MAXPATHLEN];

    dir = opendir(dirName);
    if (dir == NULL)
	adios(NULL, "Can't scan mail dir.");
    while ((dp = readdir(dir)) != NULL) {
	if (dp->d_name[0] == '.')
	    continue;
	strcpy(name, dirName);
	if (*dirName != 0)
	    strcat(name, "/");
	strcat(name, dp->d_name);
	if (stat(name, &s) != -1 && (s.st_mode & S_IFMT) == S_IFDIR) {
	    AddFolder(name, allFolders);
	    if (recurse && s.st_nlink > 2)
		BuildFolderListR(name);
	}
    }
    closedir(dir);
}

/* Add this folder to our list, counting the messages (seen and unseen). */
AddFolder(name, force)
    char *name;
    int force;
{
    struct msgs *mp;
    int nUnseen;
    int i;
    struct Folder *f;
    int flags;
    char buf[1000];
    struct stat s;

    if (!force) {
	/*
	 * Attempt to avoid reading this directory if we're only looking for
	 * unseen messages.
	 */
	sprintf(buf, "%s/%s", name, mh_seq);
	if (stat(buf, &s) < 0)
	    return;
    }
    mp = m_gmsg(name);
    nUnseen = 0;
    if (mp == NULL) {
	advise(NULL, "Can't read folder %s.", name);
	return;
    }
    if (unseenSequence != NULL)
	flags = m_seqflag(mp, unseenSequence);
    else
	flags = 0;
    if (flags != 0) {
	for (i = mp->lowmsg; i <= mp->hghmsg; ++i) {
	    if (mp->msgstats[i] & flags)
		++nUnseen;
	}
    }
    /* Save it. */
    if (nUnseen > 0 || force) {
	AllocFolders(&folders, &nFoldersAlloced, nFolders + 1);
	f = &folders[nFolders++];
	f->name = (char *) malloc(strlen(name) + 1);
	strcpy(f->name, name);
	f->nMsgs = mp->nummsg;
	f->nUnseen = nUnseen;
	f->priority = AssignPriority(f->name);
    }
}

PrintFolders()
{
    int i;

    for (i = 0; i < nFolders; ++i) {
	if (noTotal) {
	    printf("%s\n", folders[i].name);
	} else {
	    printf("  +%-20s %4d new out of %4d\n", folders[i].name,
		   folders[i].nUnseen, folders[i].nMsgs);
	}
    }
}

/* Put them in priority order. */
CompareFolders(f1, f2)
    struct Folder *f1, *f2;
{
    if (!alphaOrder && f1->priority != f2->priority)
	return f1->priority - f2->priority;
    else
	return strcmp(f1->name, f2->name);
}

/* Make sure we have at least n folders allocated. */
AllocFolders(f, nfa, n)
    struct Folder **f;
    int *nfa;
    int n;
{
    if (n <= *nfa)
	return;
    if (*f == NULL) {
	*nfa = 10;
	*f = (struct Folder *) malloc(*nfa * (sizeof(struct Folder)));
    } else {
	*nfa *= 2;
	*f = (struct Folder *) realloc(*f, *nfa * (sizeof(struct Folder)));
    }
}

/*
 * Return the priority for a name.  The highest comes from an exact match.
 * After that, the longest match (then first) assigns the priority.
 */
int
AssignPriority(name)
    char *name;
{
    int i;
    struct Folder *o;
    int nl = strlen(name);
    int ol;
    int best = nOrders;
    int bestLen = 0;

    for (i = 0; i < nOrders; ++i) {
	o = &orders[i];
	if (strcmp(name, o->name) == 0)
	    return o->priority;
	ol = strlen(o->name);
	if (nl < ol - 1)
	    continue;
	if (ol < bestLen)
	    continue;
	if (o->name[0] == '*'
		&& strcmp(o->name + 1, name + (nl - ol + 1)) == 0) {
	    best = o->priority;
	    bestLen = ol;
	} else if (o->name[ol - 1] == '*' && strncmp(o->name, name, ol - 1) == 0) {
	    best = o->priority;
	    bestLen = ol;
	}
    }
    return best;
}
