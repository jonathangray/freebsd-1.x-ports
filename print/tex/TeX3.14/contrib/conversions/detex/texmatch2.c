/* COPYRIGHT (C) 1987 Kamal Al-Yahya */
/*
 * texmatch: checks matching parantheses, braces, brackets, and dollar signs
 * in TeX documents.
 */

char *documentation[] = {
" SYNTAX",
"        texmatch [-i] [parameters] [inputfiles]",
"",
"        flags:",
"              -i   ignores TeX's and LaTeX's \input and \include commands",
"",
"        parameters:",
"              in=filename       filename is the input file",
"                                (Default: in=stdin)",
""
};

/* Author: Kamal Al-Yahya, Stanford University,		11/1/83 */
/* Last modified:					1/25/87 */

int	doclength = { sizeof documentation/sizeof documentation[0] };

#include    "setups.h"

char string[MAXWORD], filename[MAXWORD];
struct sgttyb ttystat;
extern char *strcpy(), *mktemp();
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
Need two arrays of %d characters each\n",MAXLEN);
	exit(-1);
	}

/* If no arguments, and not in a pipeline, self document */
piped_in = ioctl ((fileno (stdin)), TIOCGETP, &ttystat);
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
xargc = argc;
xargv = argv;
if(piped_in)
	{
	if (iflag != 1)
		{
/* need to buffer; can't seek in pipes */
/* make a temporary and volatile file in /tmp */
		strcpy(scratch_file,"/tmp/texXXXXXX");
		mktemp(scratch_file);
		scr=fopen(scratch_file,"w");
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

/* next process in=inputfiles */
if(getpar_("in","s",string))
	{
	sscanf(string,"%s",filename);
	if((temp=fopen(filename,"r")) != NULL)
		{
		fprintf(stderr,"%s:\n",filename);
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
		fprintf(stderr,"texmatch: Cannot open %s\n",filename);
	}

/* then process input line for arguments and assume they are input files */
for (xargc--,xargv++; xargc; xargc--,xargv++)
	{
	cptr = *xargv; 
	if( *cptr=='-' ) continue;		/* this is a flag */
	while (*cptr)
		{
		if (*cptr == '=')  break; /* this is for getpar */
		cptr++;
		}       
	if (*cptr)  continue;
	cptr = *xargv;
	if((temp=fopen(cptr,"r")) != NULL)
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
