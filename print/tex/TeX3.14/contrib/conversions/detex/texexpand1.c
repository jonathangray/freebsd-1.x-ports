/* COPYRIGHT (C) 1987 Kamal Al-Yahya */
/* texexpand: to expand TeX and LaTeX \input and include files */

char *documentation[] = {
" SYNTAX",
"        texexpand [-w] file1 [file2 .....]",
"     or texexpand [-w] < file1 [file2 ....]",
"",
"        Flags:",
"              -w    maching is not checked",
"",
"See the manual page for more details.",
"",
};

/* Author: Kamal Al-Yahya, Stanford University,		11/1/83 */
/* Last modified:					1/25/87 */

int	doclength = { sizeof documentation/sizeof documentation[0] };

#include     "setups.h"

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

int wflag;
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
int i;

if (((buf = (char *)malloc(MAXLEN*sizeof(char))) == (char *)NULL))
	{
    	fprintf(stderr,"texpand: Cannot malloc() internal buffer space\n\
Need an array of %d characters\n",MAXLEN);
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
				case 'w':
					wflag=1;
					break;
				default:
			     		fprintf(stderr,
					"texexpand: unknown flag -%c\n",*cptr);
					break;
				}
			}
		}
	}

/* first process pipe input */
if(piped_in)
	{
	if (wflag != 1)
		{
/* need to buffer; can't seek in pipes */
/* make a temporary and volatile file in /tmp */
		strcpy(scratch_file,TEMPFILE);
		mktemp(scratch_file);
		if ((scr=fopen(scratch_file,"w")) == (FILE *)NULL)
			{
			fprintf(stderr,
			"texexpand: Cannot open scratch file [%s]\n",scratch_file);
			exit(-1);
			}
		scrbuf(stdin,scr);
		fclose(scr);
		scr=fopen(scratch_file,"r");
		unlink(scratch_file);
		fprintf(stderr,"Checking matching...\n");
		Match(scr);
		fseek(scr,0,0);
		Expand(scr,buf);
		fprintf(stderr,"Checking matching done\n\n");
		fclose(scr);
		}
	else
		Expand(stdin,buf);
	fputs(buf,stdout);
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
		if (wflag != 1)
			{
			fprintf(stderr,"Checking matching...\n");
			fprintf(stderr,"%s:\n",cptr);
			Match(temp);
			fprintf(stderr,"\n");
			fseek(temp,0,0);
			}
		Expand(temp,buf);
		if (wflag != 1)
			fprintf(stderr,"Checking matching done\n\n");
		fputs(buf,stdout);
		fclose(temp);
		}
	else
		fprintf(stderr,"texexpand: Cannot open %s\n",cptr);
	}

}
