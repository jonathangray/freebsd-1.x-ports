/*
 * This file is part of the Seyon, Copyright (c) 1992-1993 by Muhammad M.
 * Saggaf. All rights reserved.
 *
 * See the file COPYING (1-COPYING) or the manual page seyon(1) for a full
 * statement of rights and permissions for this program.
*/

/*
    Revisions:

    2.2		lg	Now uses /dev/tty instead of stdin/stdout
    2.2		lg	Added command parser
*/

#include <signal.h>

#include <X11/Intrinsic.h>

#include "seyon.h"
#include "SeDecl.h"

extern FILE    *tfp;		/* Local terminal */
extern int      tfd;		/* Local terminal */

char            line[WBSIZE];	/* Input line */
char            word[WBSIZE];	/* Parsed word */
char           *wptr,
               *lptr;		/* Word and line pointers */
int             eof_flag = 0;	/* Indicates EOF during getline() processing */

void
sendstr(p)			/* send a string to the port */
     register char  *p;
{
  while (*p)
    sendbyte(*p++);
}

/* Convert uppercase characters to lowercase, (without
 * mangling non-uppercase characters), in a portable manner.
 */
int
mklow(c)
     int             c;
{
  if (isupper(c))
    return (tolower(c));
  return (c);
}

/*
 * parse the "line" array for a word
 */

void
getword()
{
  char           *ptr,
                  quote;
  int             bflag = 0,
                  qflag = 0;
  int             nflag = 0;

  ptr = word;

  *ptr = '\0';
  if (eof_flag || *lptr == '\0')
    return;

  while (isspace(*lptr))
    lptr++;

  wptr = lptr;

  if (*lptr == '\0')
    return;

  if (*lptr == '\'' || *lptr == '\"')
    quote = *lptr++;
  else
    quote = '\0';

  for (; *lptr != '\0'; lptr++) {
    if (quote) {
      if (*lptr == '\0') {
		word[0] = '\0';
		fprintf(tfp, "Unmatched quote: %s\r\n", line);
		eof_flag = 1;
		return;
      }
      if (*lptr == quote)
		break;
    }
    else if (!qflag && isspace(*lptr))
      break;

    if (bflag)
      *ptr++ = *lptr & 0x1f;
    else if (qflag)
      *ptr++ = *lptr;
    else if (*lptr == '^')
      bflag = 1;
    else if (*lptr == '\\')
      qflag = 1;
    else
      *ptr++ = *lptr;

    if (nflag == 1) {
      nflag = 0;
      bflag = 0;
      qflag = 0;
    }

    if (bflag == 1 || qflag == 1)
      nflag = 1;
  }

  if (*lptr)
	lptr++;
  *ptr = '\0';
}

void
GetWord(lin, wrd)
     char           *lin,
                    *wrd;
{
  char           *ptr;
  int             cc = 0xff;
  int             quote = 0;

  ptr = wrd;
  lptr = lin;

  *ptr = '\0';
  if (*lptr == '\0')
    return;

  while (isspace(*lptr))
    lptr++;

  wptr = lptr;

  if (*lptr == '\0')
    return;

  if (*lptr == '\"') {
    lptr++;
    quote = 1;
  }

  while (*lptr && (quote || !isspace(*lptr)) && (!quote || *lptr != '\"')) {
	
    if (*lptr == '^' && *(lptr + 1) && (!quote || *(lptr + 1) != '\"')) {
      lptr++;
      if (*lptr != '^')
		cc = 0x1f;
    }
    *ptr++ = *lptr & cc;
    lptr++;
    cc = 0xff;
  }

  if (*lptr)
    lptr++;;
  *ptr = '\0';
}

char*
NextWord(newLinePtr)
	 char *newLinePtr;
{
  static char nextWord[LRG_BUF], *linePtr;

  if (newLinePtr) linePtr = newLinePtr;
  GetWord(linePtr, nextWord);
  linePtr = lptr;
  return nextWord;
}

/*
 * make the specified word all lower case
 */

void
lc_word(ptr)
     char           *ptr;
{
  while (*ptr) {
    *ptr = mklow(*ptr);
    ptr++;
  }
}

/*
 * input a line from the specified file
 */

void
getline(fp)
     FILE           *fp;
{
  int             l;

  memset(line, 0, WBSIZE);

  if ((fgets((lptr = line), WBSIZE, fp)) == NULL) {
    eof_flag = 1;
    line[0] = '\0';
  }

  l = strlen(line);	       /* Purge newline if found */
  if (l--) {
    if (line[l] == '\n')
      line[l] = '\0';
  }
}

void
set_tty_mode()
{
  io_set_attr(tfd, &newmode);
}

void
restore_orig_mode()
{
  io_set_attr(tfd, &oldmode);
}

