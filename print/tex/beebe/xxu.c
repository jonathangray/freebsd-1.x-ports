/* /usr.MC68020/csc-sun/u/local/src/C/xxu.c, Sat Jun 27 13:53:01 1987, */
/* Add special[] processing, strip trailing dots from names */

/*  X X U  --  20-to-Unix filename converter  */

/*
 Change DEC-20 or VAX/VMS style filenames into normal Unix names.
 Handy for use after ftp MGETs, when you find your directory full of
 files with names like LIB:<KERMIT>CKUFIO.C.2 or FRED::[ETHEL]A.B;37
 when all you really wanted was ckufio.c and a.b.

 Usage: xxu file(s)

 Action: Renames argument files as follows:
   strips Unix path name from front (up to rightmost '/') if present
   strips DEC device:, node:: names from front (up to rightmost ':') if present
   strips DEC-20 <directory> or VMS [directory] name if present
   strips DEC-20 version number from end (everything after 2nd dot) if present
   strips VMS generation number from end (everything after ';') if present
   lowercases any uppercase letters
   honors DEC-20 CTRL-V quote for special characters
   discards unquoted unprintable characters
   if result is null, file is renamed to xxfile-n, where n is a number.
   if result would write over an existing file, file also renamed to xxfile-n.

 Recommended procedure: make a new directory, cd to it, then FTP files
 from DEC-20 or VMS system, then do "xxu *".

 Author:  F. da Cruz, CUCCA, July 85

 Added directory handling: Brad Davis, UofU VCIS Group, Aug 85
*/
#include <stdio.h>
#include <ctype.h>
#include <sys/file.h>		/* For access() */
#ifdef DODIRS
#include <sys/types.h>
#include <sys/dir.h>
#endif
#include <errno.h>

/* A few names will be preserved in upper case or mixed case */
char* special[] =
  { /* "old name", "new name" */
  "bugs",   "BUGS",
  "install",	"INSTALL",
  "makefile",	"Makefile",
  "note",   "NOTE",
  "readme", "README",
  "todo",   "TODO",
  (char*)NULL,  (char*)NULL
  };

char name[500];		    /* File name buffer */
char *pp, *cp, *xp;	    /* Character pointers */
char delim;		/* Directory Delimiter */
int dc = 0, n = 0;	    /* Counters */
int quote = 0, indir = 0; done = 0; /* Flags */
#ifdef DODIRS
DIR *dir;
struct direct *entry;
#endif

main(argc, argv)
int argc;
char **argv;
{
    if (argc < 2) {	    /* Give message if no args */
#ifdef DODIRS
	(void)fprintf(stderr,"Usage: xxu directory(s)\n");
#else
	(void)fprintf(stderr,"Usage: xxu file(s)\n");
#endif
	exit(1);
    }
    n = 0;		/* Unfixable filename counter */
    while (--argc > 0) {	/* For all files on command line... */
	argv++;
#ifdef DODIRS
	dir = opendir(*argv);
	while ((entry = readdir(dir)) != (struct direct *)NULL) {
	    xp = entry->d_name; /* Copy pointer for simplicity */
	if (*xp == '.') continue;   /* leave . and .. alone */
#else
	    xp = *argv; /* Copy pointer for simplicity */
#endif
	    (void)printf("%s ",xp);	/* Echo name of this file */
	    pp = name;  /* Point to translation buffer */
	    *name = '\0';   /* Initialize buffer */
	    dc = 0;	/* Filename dot counter */
	    done = 0;	/* Flag for early completion */
	    for (cp = xp; (*cp != '\0') && !done; cp++)
	    {   /* Loop thru chars... */
		if (quote) {	/* If this char quoted, */
		    *pp++ = *cp;    /*  include it literally. */
		    quote = 0;
		    continue;
		}   /*  */
		if (indir) {	    /* If in directory name, */
		    if (*cp == delim) indir = 0; /* look for end delimiter. */
		    continue;
		}
		switch (*cp) {
		case '<':	/* Discard DEC-20 directory name */
		    indir = 1;
		    delim = '>';
		    break;
		case '[':	/* Discard VMS directory name */
		    indir = 1;
		    delim = ']';
		    break;
		case '/':	/* Discard Unix path name */
		case ':':	    /*  or DEC dev: or node:: name */
		    pp = name;
		    break;
		case '.':	/* DEC -20 generation number */
		    if (++dc == 1)  /* Keep first dot */
			*pp++ = *cp;
		    else	/* Discard everything starting */
			done = 1;   /* with second dot. */
		    break;
		case ';':	/* VMS generation or DEC-20 attrib */
		    done = 1;	/* Discard everything starting with */
		    break;	/* semicolon */
		case '\026':	    /* Control-V quote for special chars */
		    quote = 1;  /* Set flag for next time. */
		    break;
		default:
		    if (isupper(*cp))	/* Uppercase letter to lowercase */
			    *pp++ = tolower(*cp);
		    else if (isprint(*cp)) /* Other printable, just keep */
			*pp++ = *cp;
		}
	    }
	    *pp = '\0';		/* Done with name, terminate it */
	    if (strcmp(name,xp) == 0) { /* If no renaming necessary, */
		(void)printf("(ok)\n");   /*  just give message. */
		continue;
	    }
	    /* Strip trailing dot (so makefile. becomes makefile)
	       and then check for special Unix files with
	       mixed case */
	    {
		 char **p;
		 if (*(pp-1) == '.')
		   *(pp-1) = '\0';
		 for (p = special; (*p != (char*)NULL); p += 2)
		   {
		 if (strcmp(name,*p) == 0)
		     {
		       strcpy(name,*(p+1));
		       break;
		     }
		   }
	    }
	    while (*name == '\0' ||
		   (access(name,0) == 0) && (errno != ENOENT))
	    {	/* Find unique name */
		(void)sprintf(name,"xxfile-%d",n++);
	    }
	    (void)printf("=> %s ",name); /* Tell what new name will be */
	    if ((strcmp(xp,name) == 0) ||
	        (rename(xp,name) == 0))   /* Try to rename it */
		(void)printf("(ok)\n");   /* Say what happened */
	    else
		perror("failed");
	}
#ifdef DODIRS
    }
#endif
    exit(0);		    /* Done. */
}
