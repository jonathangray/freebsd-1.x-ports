/*                  Tiblook -- looks up references                       */
/* as supplied, tiblook uses unix routine getopt;
   if unavailable, program can be modified by removing definition
   of GETOPT ---
   resulting program works in interactive mode only                      */

#define GETOPT
#include "stdio.h"
#include "tib.h"

char *locate();

int     max_klen =   6;     /*  max length of keys                      */
char    *common =           /*  name of file of common words            */
            COMFILE;
char    INDEXF[MAXSTR] =    /*  name of index file                      */
            INDXFILE;
char    optch2[]=OPTCH;     /*  option character on call                */

int     argc;
char    **argv;

#ifdef GETOPT
main(argc,argv)
int argc;
char **argv;
{   char *refs;
    char keys[MAXSTR];
    char *p,*q, *pcom, *getenv();
    char one_index[MAXSTR];
    char *pindx, *getenv();

    int getopt();
    int opt;
    extern char *optarg;
    extern int optind;

   while ((opt = getopt(argc, argv, "l:c:p:")) !=EOF)
    {   switch (opt)
        {   case 'l':   max_klen= atoi(optarg);
                        break;
            case 'c':   common=  optarg;
                        break;
            case 'p':   strcpy(INDEXF,optarg);
                        break;
	    case '?':
            default:    fprintf(stderr, "invalid switch '%c'\n", opt);
        }
    }
#endif
#ifndef GETOPT

main(argcount,arglist)
int argcount;
char **arglist;
{   char *refs;
    char keys[MAXSTR];
    char *p,*q, *pcom, *getenv();
    char one_index[MAXSTR];
    char *pindx, *getenv();

    argc= argcount-1;
    argv= arglist+1;
    flags();
#endif   

    /*  add SYSINDEX to search path.  all names are comma terminated */
	strcat(INDEXF, ",");
        pindx = getenv("SYSINDEX");
        if (pindx == NULL)
           strcat(INDEXF, SYSINDEX);
        else
           strcat(INDEXF, pindx);
	strcat(INDEXF, ",");

/* set common words file name */

    pcom = getenv("COMFILE");
    if (pcom != NULL)
       strcpy (common,pcom);

#ifdef GETOPT

    if (optind != argc) {
       for ( ; optind < argc; optind++) {
	   strcat(keys, argv[optind]);
           strcat(keys, " ");
       }
       for (p = one_index, q = INDEXF; *q != NULL; q++)
       if (*q == ',' )
   	{   *p = 0;
   	    refs = locate(keys, one_index, max_klen, common);
   	    if( refs==NULL ) {
		strcpy(q-strlen(one_index),q+1);
		q = q-strlen(one_index)-1;
	    }
	    if (refs!=NULL && *refs!=NULL) break;
	    p = one_index;
	}
	else *p++ = *q;

    if (refs==NULL || *refs==NULL)  printf("No references found.\n");
    else                            printf("%s", refs);
    if (refs!=NULL) free(refs);

    exit(0);
    }
#endif

  /* header */
     fprintf (stderr, "Tiblook -- version %s, released %s.\n", VERSION, RDATE);

    printf("Enter keys separated by spaces for search, <return> to exit ...\n\n*");

    while (fgets(keys,MAXSTR,stdin)!=NULL)
    {   if (keys[0] == '\n') exit(0);
        for (p = one_index, q = INDEXF; *q != 0 ; q++)
	    if (*q == ',' )
	    {   *p = 0;
	        refs = locate(keys, one_index, max_klen, common);
		if( refs==NULL )
		{   fprintf(stderr,
			"%s removed from index list.\n", one_index);
		    /* delete this file name (shift remainder on top) */
			strcpy(q-strlen(one_index),q+1);
			q = q-strlen(one_index)-1;
		}
                if (refs!=NULL && *refs!=NULL) break;
	        p = one_index;
	    }
	    else *p++ = *q;

        if (refs==NULL || *refs==NULL)  printf("No references found.\n");
        else                            printf("%s", refs);
        if (refs!=NULL) free(refs);
        printf("*");
    }
    exit(0);
}

   clnup()
{
   return;
}

#define    operand     (strlen(*argv+2)==0 ? (argv++,argc--,*argv) : *argv+2)

flags()
{   for (; argc>0 && *argv[0]==optch2[0];  argc--,argv++)
    {   switch ((*argv)[1])
        {   case 'l':   max_klen= atoi(operand);
                        break;
            case 'c':   common=  operand;
                        break;
            case 'p':   strcpy(INDEXF,operand);
                        break;
            default:    fprintf(stderr, "invalid switch '%s'\n", *argv);
        }
    }
}
