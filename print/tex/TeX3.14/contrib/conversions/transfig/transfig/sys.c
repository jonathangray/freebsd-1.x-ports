#include <stdio.h>
#include "transfig.h"

#define MAXSYS 1000
static char sysbuf[MAXSYS];

char *sysls()
{
  FILE *ls;
  int i;
  char c;

  ls = popen("/bin/ls *.fig", "r");
  i = 0;
  c = fgetc(ls);
  while (c != EOF & i < MAXSYS-1)
  {
	sysbuf[i] = c;
	i += 1;
	c = fgetc(ls);
  }
  sysbuf[i] = NULL;
  return sysbuf;
}

sysmv(f)
char *f;
{
  sprintf(sysbuf, "%s~", f);
  unlink(sysbuf);
  if (!link(f, sysbuf)) unlink(f);
}

char *strip(str, suf)
char *str, *suf;
{
  char *p1, *p2;

  for (p1 = &str[strlen(str)-1], p2 = &suf[strlen(suf)-1];
	(p1 >= str && p2 >= suf) && (*p1 == *p2);
	--p1, --p2);

  if (p2 < suf)
  {
	*(p1+1) = NULL;
	return str;
  } else
	return NULL;
}

char *mksuff(name, suff)
char *name, *suff;
{
  char *temp;

  temp = (char *)malloc(strlen(name)+strlen(suff)+1);
  strcpy(temp, name);
  strcat(temp, suff);
  return temp;
}
