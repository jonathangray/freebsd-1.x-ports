/* COPYRIGHT (C) 1987 Kamal Al-Yahya */

#include    "setups.h"
unsigned int len=0;		/* length of document */

Expand(fp,buf)	/* expand TeX and LaTeX's \input and \include */

FILE *fp;
char *buf;
{
char *buf2;
FILE *fpp;
int c;
int c1=' ';				/* previous character */
char w[MAXWORD];
int i,j;
extern wflag;

if (((buf2 = (char *)malloc(MAXLEN*sizeof(char))) == (char *)NULL))
	{
    	fprintf(stderr,"Expand: Cannot malloc() internal buffer space\n\
Need an arrays of %d characters\n",MAXLEN);
	exit(-1);
	}

while ((c = getc(fp)) != EOF)
	{
	if (++len >= MAXLEN)
		{
		fprintf(stderr,"Document is too large\n");
		exit(-1);
		}
	if (c == '%' || c1 == '%')
		{
		*buf++ = c;
		while ((c =getc(fp)) != EOF)
			{
			if (++len >= MAXLEN)
				{
				fprintf(stderr,"Sorry: document is too large\n");
				exit(-1);
				}
			*buf++=c;
			if (c == '\n')		break;
			}
		c1=c;
		continue;
		}
	if (c != '\\')
		*buf++ = c;
	else			/* detect TeX commands (backslash) */
		{
		/* see if \input or \include is the control sequence */
		i=0;
		c1=c;		/* update last character */
		while ((c = getc(fp)) != EOF && i < MAXWORD)
			{
			if (++len >= MAXLEN)
				{
				fprintf(stderr,"Document is too large\n");
				exit(-1);
				}
			if (c == ' ' || c=='\n' || c=='$' || c=='#' || c=='%'
			    || c=='{' || c=='(' || c==')' || c == '\\')
				break;
			w[i++] = (char)c;
			}
		if (strncmp(w,"input",5) == 0 || (strncmp(w,"include",7) == 0
		    && strcmp(w,"includeonly") !=0))
			{
/* if it is \input or \include , get the file name */
			i=0;
			while ((c=getc(fp)) != EOF && i < MAXWORD)
				{
				if (c == ' ' || c == '\n'
				    || c == '\t' || c == '}' || c == '%')
					break;
				w[i++] = (char)c;
				}
			w[i] = NULL;
			fpp=fopen(w, "r"); /* open the new file */
			if( fpp == NULL )
				{
/* if file is not found, try file.tex  */
				strcat(w,".tex");
				fpp=fopen(w, "r");
				if( fpp == NULL )
					{
					fprintf(stderr,
					"TeXExpand: Cannot open %s\n",w);
					buf2[0] = NULL;
					}
				else
					{
					if (wflag != 1)
						{
						fprintf(stderr,"%s:\n",w);
						Match(fpp);
						fprintf(stderr,"\n");
						fseek(fpp,0,0);
						}
					Expand(fpp,buf2);
					fclose(fpp);
					}
				}
			else
				{
				if (wflag != 1)
					{
					fprintf(stderr,"%s:\n",w);
					Match(fpp);
					fprintf(stderr,"\n");
					fseek(fpp,0,0);
					}
				Expand(fpp,buf2);
				fclose(fpp);
				}
			strcat(buf,buf2);
			buf += strlen(buf2);
			w[0] = NULL;
			}
		else
/* if the control sequence is not \input or \include write it out */
			{
/* if it is \def, \newcommand, or \newenvironment, write the full command */
			if (strncmp(w,"def",3) == 0)
				{
				i = def_file(fp,&j,0);
				fseek(fp,-i,1);
				strcat(buf,"\\def\\");
				buf += 5;
				for (j=0; j < i; j++)
					*buf++=getc(fp);
				}
			else if (strncmp(w,"newcommand",10) == 0)
				{
				i = comm_file(fp,&j,0);
				fseek(fp,-i,1);
				strcat(buf,"\\newcommand{");
				buf += 12;
				for (j=0; j < i; j++)
					*buf++=getc(fp);
				}
			else if (strncmp(w,"newenvironment",14)==0)
				{
				i = getenv_file(fp,&j,0);
				fseek(fp,-i,1);
				strcat(buf,"\\newenvironment{");
				buf += 16;
				for (j=0; j < i; j++)
					*buf++=getc(fp);
				}
			else
				{
				*buf++='\\';
				for (j=0; j < i; j++)
					*buf++ = w[j];
				*buf++ = c;
				}
			}
		}
	c1 = c;				/* update last character */
	}
*buf = NULL;				/* terminate it with a null */
}
