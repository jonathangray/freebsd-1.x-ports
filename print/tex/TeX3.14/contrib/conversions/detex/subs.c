/* COPYRIGHT (C) 1987 Kamal Al-Yahya */

/* subroutines used programs in the package */
#include    "setups.h"

int
begin_to_end(buffer,environment)
char *buffer;
char *environment;
{
int c,len,i;
char w[MAXWORD];

buffer++;	len = 1;
while ((c = *buffer++) != NULL)
	{
	len++;
	if (c == '\\')
		{
		i = get_buf_word(buffer,w);
		buffer += i;	len += i;
		if (strcmp(w,"end") == 0)
			{
			buffer++;	len++;
			i = get_buf_word(buffer,w);
			buffer += i+1;	len += i+1;
			if (strcmp(w,environment) == 0)
				break;
			}
		}
	}
return(len);
}

int
command(inbuf,w)
char *inbuf;
char *w;
{
int c,i,j;
int left_br=0, right_br=0;

for (i=0; (c = *inbuf++) != NULL && i < MAXLINE; i++)
	{
	w[i] = (char)c;
	if (c == '{')	left_br++;
	if (c == '}')	right_br++;
	if (left_br == right_br && left_br != 0)	break;
	}
left_br = 0;	right_br = 0;
for (j=i+1; (c = *inbuf++) != NULL && j < MAXLINE; j++)
	{
	w[j] = (char)c;
	if (c == '{')	left_br++;
	if (c == '}')	right_br++;
	if (left_br == right_br && left_br != 0)	break;
	}
w[++j] = NULL;
return(j);
}

int
comm_file(fp,line,match)
FILE *fp;
int *line,match;
{
int i=0,lbrl=0;
int c;
int left_br=0, right_br=0;

while ((c = getc(fp)) != EOF)
	{
	i++;
	if (c == '\n')	(*line)++;
	if (c == '{')
		{
		left_br++;
		if (lbrl == 0)	lbrl = *line;
		}
	if (c == '}')	right_br++;
	if (right_br > left_br)		break;
	}
if (match == 1 && c == EOF && left_br > right_br)
	{
	fprintf(stderr,"file ends: %d unclosed left braces while reading \\newcommand, first opened at line %d\n",left_br-right_br,lbrl,lbrl);
	return(i);
	}
left_br = 0;	right_br = 0;	lbrl = 0;
while ((c = getc(fp)) != EOF)
	{
	i++;
	if (c == '\n')	(*line)++;
	if (c == '{')
		{
		left_br++;
		if (lbrl == 0)	lbrl = *line;
		}
	if (c == '}')	right_br++;
	if (left_br == right_br && left_br != 0)	break;
	}
if (match == 1 && c == EOF && left_br > right_br)
	fprintf(stderr,"file ends: %d unclosed left braces while reading \\newcommand, first opened at line %d\n",left_br-right_br,lbrl,lbrl);
return(i);
}

int
comment(buffer)			/* commented text is ignored */
char *buffer;
{
int c,len=0;

while((c = *buffer++) != NULL)
	{
	if (c == '\n')		break;
	len++;
	}
return(len);
}

int
def(inbuf,w)
char *inbuf, *w;
{
int c,i;
int left_br=0, right_br=0;

for (i=0; (c = *inbuf++) != NULL && i < MAXLINE; i++)
	{
	w[i] = (char)c;
	if (c == '{')	left_br++;
	if (c == '}')	right_br++;
	if (left_br == right_br && left_br != 0)	break;
	}
w[++i] = NULL;
return(i);
}

int
def_file(fp,line,match)
FILE *fp;
int *line,match;
{
int i=0, lbrl=0;
int c;
int left_br=0, right_br=0;

while ((c = getc(fp)) != EOF)
	{
	i++;
	if (c == '\n')	(*line)++;
	if (c == '{')
		{
		left_br++;
		if (lbrl == 0)	lbrl = *line;
		}
	if (c == '}')	right_br++;
	if (left_br == right_br && left_br != 0)	break;
	}
if (match == 1 && c == EOF && left_br > right_br)
	fprintf(stderr,"file ends: %d unclosed left braces while reading \\def, first opened at line %d\n",left_br-right_br,lbrl);
return(i);
}

int
display(buffer)
char *buffer;
{
int c,len=1;

while((c = *buffer++) != NULL)
	{
	len++;
	if(c == '\\')
		{
		len++;
		if ((c = *buffer++) == ']')
			break;
		}
	}
return(len);			
}

int
dollar(buffer,out_file)
char *buffer;
FILE *out_file;
{
int c,len;

c=' ' ;		/* "erase" the dollar sign */
putc (c,out_file);
c = *buffer++;
if(c == '$')		len=two_dollars(buffer,out_file)+1;
else			len=one_dollar(buffer);
return(len);
}

int
formula(buffer)
char *buffer;
{
int c,len=1;

while((c = *buffer++) != NULL)
	{
	len++;
	if (c == '\\')
		{
		if ((c = *buffer++) == ')')
			break;
		}
	}
return(len);			
}

int
get_buf_word(inbuf,w)
char *inbuf;
char *w;
{
int c,i;

for (i=0; (c = *inbuf++) != NULL && c != ' ' && c != '\n' && c != '\t'
	&& c != '$' && c != '{' && c != '}' && c != '%'
	&& c != '\\' && c != '#' && c != '(' && c != ')' && c != '['
	&& c != ']' && i < MAXWORD; i++)
		w[i] = (char)c;
if ((c == ' ' || c == '\n' || c == '\t' || c == '$' || c == '{' || c == '}' ||
	c == '%' || c == '\\' && c != '#' || c == '(' || c == ')' || c =='['
	|| c == ']') && i == 0)
		w[i++] = (char)c;
w[i] = NULL;
return(i);
}

int
getenv_file(fp,line,match)
FILE *fp;
int *line,match;
{
int i=0,lbrl=0;
int c;
int left_br=0, right_br=0;

while ((c = getc(fp)) != EOF)
	{
	i++;
	if (c == '\n')	(*line)++;
	if (c == '{')
		{
		left_br++;
		if (lbrl == 0)	lbrl = *line;
		}
	if (c == '}')	right_br++;
	if (right_br > left_br)		break;
	}
if (match == 1 && c == EOF && left_br > right_br)
	{
	fprintf(stderr,"file ends: %d unclosed left braces while reading \\newenvironment, first opened at line %d\n",left_br-right_br,lbrl);
	return(i);
	}
left_br = 0;	right_br = 0;	lbrl = 0;
while ((c = getc(fp)) != EOF)
	{
	i++;
	if (c == '\n')	(*line)++;
	if (c == '{')
		{
		left_br++;
		if (lbrl == 0)	lbrl = *line;
		}
	if (c == '}')	right_br++;
	if (left_br == right_br && left_br != 0)	break;
	}
if (match == 1 && c == EOF && left_br > right_br)
	{
	fprintf(stderr,"file ends: %d unclosed left braces while reading \\newenvironment, first opened at line %d\n",left_br-right_br,lbrl);
	return(i);
	}
left_br = 0;	right_br = 0;	lbrl = 0;
while ((c = getc(fp)) != EOF)
	{
	i++;
	if (c == '\n')	(*line)++;
	if (c == '{')
		{
		left_br++;
		if (lbrl == 0)	lbrl = *line;
		}
	if (c == '}')	right_br++;
	if (left_br == right_br && left_br != 0)	break;
	}
if (match == 1 && c == EOF && left_br > right_br)
	fprintf(stderr,"file ends: %d unclosed left braces while reading \\newenvironment, first opened at line %d\n",left_br-right_br,lbrl);
return(i);
}

int
get_file_word(fp,w,line,c)
FILE *fp;
char *w;
int *line, *c;
{
int i;

for (i=0; (*c = getc(fp)) != NULL && *c != ' ' && *c != '\n' && *c != '\t'
	&& *c != '$' && *c != '{' && *c != '}' && *c != '\\' && *c != '#'
	&& *c != '(' && *c != ')' && *c != '[' && *c != ']' && *c != '%'
	&& i < MAXWORD; i++)
		w[i] = (char)*c;
if (i == 0 && *c == '\n') (*line)++;
w[i] = NULL;
return(i);
}

int
is_new_env(w,env_count)
char *w;
int env_count;
{
int i;

for (i=0; i < env_count; i++)
	{
	if (strcmp(env[i].env_name,w) == 0)
		return(i);
	}
return(-1);
}

int
one_dollar(buffer)
char *buffer;
/* an in-line equation with one dollar signs as delimeters */
{

int c,len=0;

while((c = *buffer++) != NULL)
	{
	len++;
	if(c == '$')	break;
	}
return(len+1);
}

void
scrbuf(in,out)			/* copy input to output */
FILE *in,*out;
{
int c;
while ((c =getc(in)) != EOF)	putc(c,out);
}

void
tmpbuf(in,buffer)
/* copy input to buffer, buffer holds only MAXLEN characters */
FILE *in;
char *buffer;
{
int c;
unsigned int l=0;

while (l++ < MAXLEN && (c = getc(in)) != EOF)
	*buffer++ = (char)c;
if (l >= MAXLEN)
	{
	fprintf(stderr,"Sorry: document is too large\n");
	exit(-1);
	}
*buffer = NULL;
}

int
two_dollars(buffer,out_file)
char *buffer;
FILE *out_file;
/* displayed equation with two-dollar sign delimeters */
{
int c,len=0;

while((c = *buffer++) != NULL)
	{
	len++;
	if(c == '$')
		{
		c = *buffer++;		len++;
		if (c != '$')	putc(c,out_file);
		break;
		}
	}
return(len);
}
