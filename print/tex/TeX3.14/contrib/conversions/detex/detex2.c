/* COPYRIGHT (C) 1987 Kamal Al-Yahya */
/* detex: strips TeX's and LaTeX's commands */


char *documentation[] = {
" SYNTAX",
"        detex [-iw] [parameters] [inputfiles]",
"",
"        flags:",
"              -i   ignores TeX's and LaTeX's \input and \include commands",
"              -w   does not check matching",
"",
"        parameters:",
"              in=filename       filename is the input file",
"                                (Default: in=stdin)",
"",
"              out=filename      filename is the output file",
"                                (Default: out=stdout)",
""
};

/* Author: Kamal Al-Yahya, Stanford University,		11/1/83 */
/* Last modified:					1/25/87 */

int	doclength = { sizeof documentation/sizeof documentation[0] };

#include        "setups.h"

#ifdef tops20
#define TEMPFILE "texXXXXXX"
#else
#define TEMPFILE "/tmp/texXXXXXX"
#endif

char string[MAXWORD], filename[MAXWORD], scratch_file[MAXWORD];
FILE *out_file;
extern char *mktemp();

#ifdef MSC
#else
struct sgttyb ttystat;
#endif

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
    	fprintf(stderr,"detex: Cannot malloc() internal buffer space\n\
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

out_file = stdout;		/* default output */

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
						"detex: unknown flag -%c\n",*cptr);
					break;
				}
			}
		}
	}

/* process getpar parameters */
xargc = argc;
xargv = argv;

if(getpar_("out","s",string))
	{
	sscanf(string,"%s",filename);
	if((temp=fopen(filename,"w")) == NULL)
		fprintf(stderr,"detex: Cannot open output file %s\n",filename);
	else
		out_file = temp;
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
	        "detex: Cannot open scratch file [%s]\n",scratch_file);
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
	DeTeX(buf,out_file);
	fclose(scr);
	}

/* next process in=inputfiles */
if(getpar_("in","s",string))
	{
	sscanf(string,"%s",filename);
	if((temp=fopen(filename,"r")) != NULL)
		{
		if (wflag != 1)
			{
			fprintf(stderr,"Checking matching...\n");
			fprintf(stderr,"%s:\n",filename);
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
		DeTeX(buf,out_file);
		fclose(temp);
		}
	else
		fprintf(stderr,"detex: Cannot open %s\n",filename);
	}

/* then process input line for arguments and assume they are input files */
xargc = argc;
xargv = argv;

for (xargc--,xargv++; xargc; xargc--,xargv++)
	{
	cptr = *xargv; 
	if( *cptr=='-' ) continue; /* this is a flag */
	while (*cptr)
		{
		if (*cptr == '=')  break; /* this is for getpar */
		cptr++;
		}       
	if (*cptr)  continue;
	cptr = *xargv;
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
		DeTeX(buf,out_file);
		fclose(temp);
		}
	else
		fprintf(stderr,"detex: Cannot open %s\n",cptr);
	}

}
