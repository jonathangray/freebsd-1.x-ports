/*
Take an error message log on stdin with lines of the form

file(linenumber) : error message

and display on stdout the error message followed by the offending
source lines.

Microsoft C uses this format for its messages.

Usage:
	errshow <error_log_file >merged_source_and_error_file

[28-Aug-86]
*/

/* This version works with Microsoft C Version 3.0 or later */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAXLINE 255
#define MAXFNAME 63	/* PC DOS has short names */

#define SUCCESS 0
#define FAILURE 1

char errmsg[MAXLINE];
char line[MAXLINE];
char fname[MAXFNAME] = "";
FILE *fp = (FILE *)NULL;
int lno = 0;
int errlno = 0;

void cleanup();
void display();
int getfile();

void
main()
{
	while (fgets(errmsg,MAXLINE,stdin) != (char *)NULL)
		(void)display();
	(void)cleanup();
}

void
cleanup()	/* clean up and exit */
{
	if (fp != (FILE *)NULL)
		(void)fclose(fp);
	exit(0);
}

void
display()	/* display source line matching errmsg[] */
{
	char *p;

	if ((p = strchr(errmsg,'(')) == (char *)NULL)
		return;	/* not an error message line */
	*p++ = '\0';	/* clobber '('; p points now to line number */
	if ((errlno = atoi(p)) <= 0)	/* error line number in source file */
		return;			/* not an error message line */
	if (getfile() != SUCCESS)
		return;
	if (lno > errlno)	/* cannot happen */
	{
		(void)printf("\n?Line number backed up: %s(%s\n",errmsg,p);
		rewind(fp);
		lno = 0;
	}
	while (lno < errlno)
	{
		if (fgets(line,MAXLINE,fp) == (char *)NULL)
		{
			(void)printf(
			     "\n?Unexpected EOF before finding line: %s(%s\n",
			     errmsg,p);
			return;
		}
		lno++;
	}
	(void)printf("\n%s(%s%7d:%s",errmsg,p,lno,line);
}

int
getfile()	/* return SUCCESS on success, FAILURE on error */
{
	if (strcmp(fname,errmsg) != 0)
	{
		if (fp != (FILE *)NULL)
			(void)fclose(fp);
		(void)strncpy(fname,errmsg,MAXFNAME);
		fname[MAXFNAME-1] = '\0';
		if ((fp = fopen(fname,"r")) == (FILE *)NULL)
		{
			(void)printf("\n?Cannot open file [%s]\n",fname);
			return(FAILURE);
		}
		lno = 0;
	}
	return(SUCCESS);
}
