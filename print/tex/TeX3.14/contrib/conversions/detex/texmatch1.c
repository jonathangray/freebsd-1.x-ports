/* COPYRIGHT (C) 1987 Kamal Al-Yahya */
/*
 * texmatch: checks matching parantheses, braces, brackets, and dollar signs
 * in TeX documents.
 */

char *documentation[] = {
" SYNTAX",
"        texmatch [-i] file1 [file2 .....]",
"     or texmatch [-i]  < file1 [file2 ....]",
"",
"See the manual page for more details.",
"",
};

/* Author: Kamal Al-Yahya, Stanford University,		11/1/83 */
/* Last modified:					1/25/87 */

int	doclength = { sizeof documentation/sizeof documentation[0] };

#include    "setups.h"

#ifdef tops20
#define TEMPFILE "texXXXXXX"
#else
#define TEMPFILE "/tmp/texXXXXXX"
#endif

#ifdef MSC
#else
struct sgttyb ttystat;
#endif

extern char *mktemp();
char scratch_file[MAXWORD];

int wflag=0;		/* for consistency with other programs */
int xargc;
char **xargv;

main(argc,argv)
int argc; 
char *argv[];
{
char *buf;
FILE *temp,*scr;
register char *cptr;
int piped_in;
int i,iflag;

if (((buf = (char *)malloc(MAXLEN*sizeof(char))) == (char *)NULL))
	{
    	fprintf(stderr,"texmatch: Cannot malloc() internal buffer space\n\
Need two arrays of %d characters\n",MAXLEN);
	exit(-1);
	}

/* If no arguments, and not in a pipeline, self document */
#ifdef MSC	/* MS-DOS cannot distinguish piped input from no input */
piped_in = (argc == 1);
#else
piped_in = ioctl ((fileno (stdin)), TIOCGETP, &ttystat);
#endif
if (argc == 1 && !piped_in)
	{
	for( i=0; i<doclength; i++)
		printf("%s\n",documentation[i]);
	exit (0);
	}

/* process option flags */
xargc = argc;
xargv = argv;
for (xargc--,xargv++; xargc; xargc--,xargv++)
	{
	cptr = *xargv; 
	if( *cptr=='-' )
		{
		while( *(++cptr))
			{
			switch( *cptr )
				{
				case 'i':
					iflag=1;
					break;
				default:
			     		fprintf(stderr,
					"texmatch: unknown flag -%c\n",*cptr);
					break;
				}
			}
		}
	}

/* first process pipe input */
if(piped_in)
	{
	if (iflag != 1)
		{
/* need to buffer; can't seek in pipes */
/* make a temporary and volatile file in /tmp */
		strcpy(scratch_file,TEMPFILE);
		mktemp(scratch_file);
		if ((scr=fopen(scratch_file,"w")) == (FILE *)NULL)
			{
			fprintf(stderr,
			"texmatch: Cannot open scratch file [%s]\n",scratch_file);
			exit(-1);
			}
		scrbuf(stdin,scr);
		fclose(scr);
		scr=fopen(scratch_file,"r");
		unlink(scratch_file);
		Match(scr);
		fseek(scr,0,0);
		Expand(scr,buf);
		fclose(scr);
		}
	else
		Match(stdin);
	}

/* then process input line for arguments and assume they are input files */
xargc = argc;
xargv = argv;
for (xargc--,xargv++; xargc; xargc--,xargv++)
	{
	cptr = *xargv; 
	if( *cptr=='-' ) continue;		/* this is a flag */
	if((temp=fopen(cptr,"r")) != (FILE *)NULL)
		{
		fprintf(stderr,"%s:\n",cptr);
		Match(temp);
		fprintf(stderr,"\n");
		if (iflag != 1)
			{
			fseek(temp,0,0);
			Expand(temp,buf);
			}
		fclose(temp);
		}
	else
		fprintf(stderr,"texmatch: Cannot open %s\n",cptr);
	}

}
