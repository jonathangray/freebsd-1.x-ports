/* COPYRIGHT (C) 1987  Kamal Al-Yahya */
#include    "setups.h"
Eqn(buffer,out_file)			/* srips TEX equations */

FILE *out_file;
char *buffer;
{
int c,d;
int i;
char w[MAXLINE], ww[MAXWORD];
while ((c = *buffer++) != NULL)
	{
	if(c == '%')
		{
		while ((c = *buffer++) != NULL)
			if (c == '\n') break;
		}
	else if(c == '$')
		{
		if ((d = *buffer++) == '$')
			{
			putc(c,out_file);	putc(d,out_file);
			while ((c = *buffer++) != NULL)
				{
				if(c != '$')   putc(c,out_file);
				else
					{
					buffer++;
					fprintf(out_file,"$$ \n");
					break;
					}
				}
			}
		}
/* check for LaTeX \begin{equation}, \begin{eqnarray}, and \begin{displaymath} */
	else if(c == '\\')
		{
		c = *buffer++;
		if (c == '[')
			{
			putc('\\',out_file); putc(c,out_file);
			while((c = *buffer++) != NULL)
				{
				if(c == '\\')
					{
					c = *buffer++;
					fprintf(out_file,"\\%c",c);
					if (c == ']')
						{
						putc('\n',out_file);
						break;
						}
					}
				else
					putc(c,out_file);
				}
			continue;
			}
		buffer--;
		buffer += get_buf_word(buffer,w);
		if (strcmp(w,"begin") == 0)
			{
			buffer++;
			i = get_buf_word(buffer,w);
			buffer += i;
			if (strcmp(w,"equation") == 0 || strcmp(w,"eqnarray")
				== 0 || strcmp(w,"displaymath") == 0)
				{
				fprintf(out_file,"\\begin{%s}",w);
				buffer++;
				while ((c = *buffer++) != NULL)
					{
					putc(c,out_file);
					if (c == '\\')
						{
						i = get_buf_word(buffer,ww);
						buffer += i;
						fprintf(out_file,"%s",ww);
						if (strcmp(ww,"end") == 0)
							{
							buffer++;
							i = get_buf_word(buffer,ww);
							buffer += i;
							fprintf(out_file,
								"{%s}\n",ww);
							buffer++;
							if (strcmp(ww,"equation")
							    == 0 ||
							    strcmp(ww,"eqnarray")
							    == 0 ||
							    strcmp(ww,"displaymath")
							    == 0)
								break;
							}
						}
					}
				}
			}
		else if (strcmp(w,"def") == 0)
			{
			i = def(buffer,w);
			buffer += i;
			fprintf(out_file,"\\def%s\n",w);
			}
		else if (strcmp(w,"newcommand") == 0)
			{
			i = command(buffer,w);
			buffer += i;
			fprintf(out_file,"\\newcommand%s\n",w);
			}
		}
	}
}
