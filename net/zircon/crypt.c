#include <stdio.h>
#include <string.h>

/*
 * Yes, this is a horrible piece of code isn't it.
 */

void decrypt (key, klen, line, len)
char *key;
int klen;
char *line;
int len;
{
    char *cp = line, *lp = line, vern = '\0';
    int cycle = 0;;
    
    while (*lp) 
    {
	if (*lp == '\\')
	{
	    len -= 1;
	    switch (*++lp)
	    {
	    case 'a' :
		*cp++ = '\001';
		break;
	    case '\\' :
		*cp++ = '\\';
		break;
	    case '0' :
		*cp++ = '\0';
		break;
	    case 'n' :
		*cp++ = '\n';
		break;
	    case 'r' :
		*cp++ = '\r';
		break;
	    }
	    lp++;
	}
	else
	{
	    *cp++ = *lp++;
	}
    }
    for (lp = line ; len--; lp++)
    {
	vern ^= *lp = vern ^ *lp ^ key[cycle];
	cycle = (cycle + 1) % klen;
    }
    *lp++='\n';
    *lp='\0';
    fputs(line, stdout);
}

void ncrypt (key, klen, line, len)
char *key;
int klen;
char *line;
int len;
{
    char *lp, vern = '\0', gash;
    int cycle = 0, len2 = len;

    for (lp = line ; len--; lp++)
    {
	gash = *lp;
	*lp = vern ^ *lp ^ key[cycle];
	vern ^= gash;
	cycle = (cycle + 1) % klen;
    }
    lp = line;
    while (len2--)
    {
	switch (*lp)
	{
	case '\001' :
	    fputs("\\a", stdout);
	    break;
	case '\\' :
	    fputs("\\\\", stdout);
	    break;
	case '\0' :
	    fputs("\\0", stdout);
	    break;
	case '\n' :
	    fputs("\\n", stdout);
	    break;
	case '\r' :
	    fputs("\\r", stdout);
	    break;
	    default :
	    putc(*lp, stdout);
	    break;
	}
	lp++;
    }
    putc('\n', stdout);
}

int main(argc, argv)
int argc;
char **argv;
{
    char *key = argv[3], line[1024];
    int klen = strlen(key), len;

    strcpy(line, argv[2]);
    len = strlen(line);
    if (*argv[1] == 'D')
    {
	decrypt(key, klen, line, len);
    }	
    else
    {
	ncrypt(key, klen, line, len);
    }	
    exit(0);
    
}

