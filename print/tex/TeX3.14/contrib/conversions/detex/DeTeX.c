/* COPYRIGHT (C) 1987  Kamal Al-Yahya */
#include   "setups.h"
DeTeX(buffer,out_file)			/* stripping TEX commands */

char *buffer;
FILE *out_file;
{
int c,cc;
char w[MAXWORD];

while ((c = *buffer++) != NULL)
	{
	switch (c)
		{
/* detect TeX commands (backslash) */
		case '\\':
			c=' ' ;		/* "erase" the backslash */
			putc(c,out_file);
			cc = *buffer++;
			if (cc == '\n')			putc(cc,out_file);
			else if (cc == '[')		buffer += display(buffer);
			else if (cc == '(')		buffer += formula(buffer);
			else if (cc == '$' || cc == '%')
				break;
/* check for LaTeX \begin{equation}, \begin{eqnarray}, and \begin{displaymath} */
			else
				{
				buffer--;
				buffer += get_buf_word(buffer,w);
				if (strcmp(w,"begin") == 0)
					{
					buffer++;
					buffer += get_buf_word(buffer,w);
					if (strcmp(w,"equation") == 0 ||
						strcmp(w,"eqnarray") == 0 ||
						strcmp(w,"displaymath") == 0)
						buffer += begin_to_end(buffer,w);
					}
				}
			break;

		case '$':
			buffer += dollar(buffer,out_file);
			break;
		case '%':
			buffer += comment(buffer);
			break;
/* erase these character */
		case '{':
			c=' ';
		case '}':
			c=' ';
		case '_':
			c=' ';
		case '^':
			c=' ';
		case '&':
			c=' ';
		case '#':
			c=' ';
/* default is doing nothing: pass the character to the output */
		default:
			putc(c,out_file);
			break;
		}
	}
}
