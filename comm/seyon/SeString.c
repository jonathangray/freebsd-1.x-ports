
/*
 * This file is part of the Seyon, Copyright (c) 1992-1993 by Muhammad M.
 * Saggaf. All rights reserved.
 *
 * See the file COPYING (1-COPYING) or the manual page seyon(1) for a full
 * statement of rights and permissions for this program.
 */

#include <ctype.h>

#include "config.h"
#include "SeDecl.h"

char           *str_strip_lead_end_space(),
               *str_strip_lead_space(),
               *str_strip_end_space();

/*
 * string routines
 */

char
itoa(num)
     int             num;
{
  char            buf[TIN_BUF];

  sprintf(buf, "%d", num);
  return buf[0];
}

/*
 * replace unwanted characters with something else
 */

char           *
str_filter(str, unwanted, replacement)
     char           *str,
                    *unwanted;
     char            replacement;
{
  int             i,
                  j;

  for (i = 0; str[i]; i++)
    for (j = 0; unwanted[j]; j++)
      if (str[i] == unwanted[j])
	str[i] = replacement;

  return str;
}

/*
 * strip spaces from both ends of a string
 */

char           *
str_strip_lead_end_space(str)
     char           *str;
{
  return str_strip_lead_space(str_strip_end_space(str));
}

/*
 * strip spaces from the beginning a string
 */

char           *
str_strip_lead_space(str)
     char           *str;
{
  int             i;

  for (i = 0; isspace(str[i]); i++);
  return str + i;
}

/*
 * strip spaces from the end a string
 */

char           *
str_strip_end_space(str)
     char           *str;
{
  int             i;

  for (i = strlen(str) - 1; isspace(str[i]) && i; i--);
  str[++i] = '\0';

  return str;
}

char           *
str_stripspc_copy(dest, source)
     char           *dest,
                    *source;
{
  char            buffer[REG_BUF],
                 *bufptr;

  strcpy(buffer, source);
  bufptr = str_strip_lead_end_space(buffer);
  return strcpy(dest, bufptr);
}


char           *
str_strip_end_char(str)
     char           *str;
{
  int             length;

  length = strlen(str);
  str[length - 1] = '\0';

  return str;
}

char*
StripSpace(str)
	 char *str;
{
  static char strBuf[LRG_BUF];
  
  str_stripspc_copy(strBuf, str);
  return strBuf;
}

char*
FmtString(fmt, a, b, c)
	 char *fmt, *a, *b, *c;
{
  static char strBuf[LRG_BUF];

  sprintf(strBuf, fmt, a, b, c);
  return strBuf;
}
 
/*
 * find first occurance of str2 in str1 and return a pointer to it
 */

#if !HAVE_STRSTR
char           *
strstr(str1, str2)
     char           *str1,
                    *str2;
{
  char           *Sptr,
                 *Tptr;
  int             len = strlen(str1) - strlen(str2) + 1;

  if (*str2)
    for (; len > 0; len--, str1++) {
      if (*str1 != *str2)
		continue;
	  
      for (Sptr = str1, Tptr = str2; *Tptr != '\0'; Sptr++, Tptr++)
		if (*Sptr != *Tptr)
		  break;
	  
      if (*Tptr == '\0')
		return str1;
    }

  return NULL;
}

#endif

char           *
strsqtok(str)
     char           *str;
{
  char           *line,
                 *wrd;
  static char    *saved_ptr;

  if (str == NULL)
    line = saved_ptr;
  else
    line = str;

  while (isspace(*line) && *line)
    line++;

  if (*line == '\0')
    return NULL;
  else if (*line == '\"')
    for (wrd = ++line; *line != '\"' && *line; line++);
  else
    for (wrd = line; !isspace(*line) && *line; line++);

  if (*line == '\0')
    saved_ptr = line;
  else
    saved_ptr = line + 1;

  *line = '\0';
  return wrd;
}

/*
 * return the first word of a string and set the start of the string
 * to point to the next one
 */

/*
 * WARNING: words returned by this routine are not null-termnated
 */

char           *
str_parse(buf)
     char          **buf;
{
  char           *line,
                 *wrd;

  /*
   * Remember: *buf is a pointer to the string, **buf is a character
   * in the string
   */

  line = *buf;

  while (isspace(*line) && *line)
    line++;

  if (!(*line))
    return NULL;
  else if (*line == '\"') {
    for (wrd = ++line; *line != '\"' && *line; line++);
    *line = '\0';
    line++;
  }
  else
    for (wrd = line; !isspace(*line) && *line; line++);

  *buf = line;
  return wrd;
}

/*
 * this routine is not currently used, and I'm not if it works
 */

char           *
get_word(str, word)
     char           *str,
                    *word;
{
  char           *wrd,
                  c;

  while (isspace(*str) && *str)
    str++;

  if (!(*str))
    word[0] = '\0';

  else if (*str == '\"') {
    for (wrd = ++str; *str != '\"' && *str; str++);
    *str = '\0';
    strcpy(word, wrd);
    *str = '\"';
    str++;
  }

  else {
    for (wrd = str; !isspace(*str) && *str; str++);
    c = *str;
    *str = '\0';
    strcpy(word, wrd);
    *str = c;
  }

  return str;
}

#if !HAVE_STRERROR

char *
strerror(err)
     int err;
{
  extern char *sys_errlist[];

  return sys_errlist[err];
}

#endif
