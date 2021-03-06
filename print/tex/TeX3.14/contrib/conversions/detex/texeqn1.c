/* COPYRIGHT (C) 1987 Kamal Al-Yahya */
/* texeqn: TeX equation stripping */

char *documentation[] = {
" SYNTAX",
"        texeqn [-iw] file1 [file2 .....]",
"     or texeqn [-iw]  < file1 [file2 ....]",
"",
"        Flags:",
"              -i     ignores TeX's and LaTeX's \input files",
"              -w     matching is not checked",
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
int iflag,i;

if (((buf = (char *)malloc(MAXLEN*sizeof(char))) == (char *)NULL))
	{
    	fprintf(stderr,"texmatch: Cannot malloc() internal buffer space\n\
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
				case 'i':
					iflag=1;
					break;
				case 'w':
					wflag=1;
					break;
				default:
			     		fprintf(stderr,
						"texeqn: unknown flag -%c\n",*cptr);
					break;
				}
			}
		}
	}

/* first process pipe input */
if(piped_in)
	{
/* need to buffer; can't seek in pipes */
/* make a temporary and volatile file in /tmp */
	strcpy(scratch_file,TEMPFILE);
	mktemp(scratch_file);
	if ((scr=fopen(scratch_file,"w")) == (FILE *)NULL)
		{
		fprintf(stderr,
		"texeqn: Cannot open scratch file [%s]\n",scratch_file);
		exit(-1);
		}
	scrbuf(stdin,scr);
	fclose(scr);
	scr=fopen(scratch_file,"r");
	unlink(scratch_file);
	if (wflag != 1)
		{
		fprintf(stderr,"Checking matching...\n");
		Match(scr);
		fseek(scr,0,0);
		}
/* either expand or buffer */
	if (iflag != 1)
		{ Expand(scr,buf);	fclose(scr); }
	else
		{ tmpbuf(scr,buf);	fclose(scr); }
	if (wflag != 1)
		fprintf(stderr,"Checking matching done\n\n");
	Eqn(buf,stdout);
	fclose(scr);
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
/* either expand or buffer */
		if (iflag != 1)
			{ Expand(temp,buf);	fclose(temp); }
		else
			{ tmpbuf(temp,buf);	fclose(temp); }
		if (wflag != 1)
			fprintf(stderr,"Checking matching done\n\n");
		Eqn(buf,stdout);
		fclose(temp);
		}
	else
		fprintf(stderr,"texeqn: Cannot open %s\n",cptr);
	}

}
