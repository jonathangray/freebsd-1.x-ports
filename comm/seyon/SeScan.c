/*---------------------------------------------------------------------------+
| Free form input scanner
+---------------------------------------------------------------------------*/
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "SeParse.h"
#include "y.tab.h"

/*---------------------------------------------------------------------------+
| parse buffer - stores the string to be parsed (logical line)
+---------------------------------------------------------------------------*/
static unsigned parseBufLen;
static unsigned parseBufNextCh;
static char* 	parseBuf;

void scSetInputBuffer(char *line) 
{
  assert(line != 0);
  parseBufNextCh 	= 0;
  parseBufLen      	= strlen(line);
  parseBuf		= line;
}

int scGetChar() {
  if (parseBufNextCh < parseBufLen) {
#ifdef TEST
    printf("Reading char %c\n", parseBuf[parseBufNextCh]);
#endif
    return parseBuf[parseBufNextCh++];
  }
  else
    return EOF;
}

void scUngetChar(int c) {
  if (parseBufNextCh > 0)
    parseBuf[--parseBufNextCh] = c;
}

int scLook() {
  if (parseBuf != 0)
    return parseBuf[parseBufNextCh];
  else
    return EOF;
}

/*---------------------------------------------------------------------------+
| strBuf - stores word to be returned
+---------------------------------------------------------------------------*/
#define GROW_SIZE 100
static char *strBuf = 0;
static unsigned strBufSize = 0;
static unsigned strBufNextCh = 0;

static void _growBuf(unsigned size) {
  if (strBuf == 0)
    strBuf = (char *)malloc(strBufSize = size + GROW_SIZE);
  else if (strBufSize < (strBufNextCh + size + 1)) {
    strBufSize += (size + 1) + GROW_SIZE;
    strBuf = (char *)realloc(strBuf, strBufSize);
  }
}

static void _startWord() {
  strBufNextCh = 0;
}

static void _addCh(int ch) {
  _growBuf(1);
  strBuf[strBufNextCh++] = ch;
}

static char* _getWd() {
  if (strBuf != 0) strBuf[strBufNextCh] = 0;
  strBufNextCh = 0;
  return strBuf;
}

/*---------------------------------------------------------------------------+
| scanner state
+---------------------------------------------------------------------------*/
enum SC_STATE {
  SC_IN_WORD,
  SC_IN_STR,
  SC_OUTSIDE,
};

static ScanState = SC_OUTSIDE;
static ScanDelim = 0;	/* Current string delimiter */

void NEW_STATE(int st) {

#ifdef TEST
  static char* state_names[] = { "InWord", "InStr", "Out", "NONE" };
  printf("**Entering state %s\n", state_names[st]);
#endif

  ScanState = st;
}

#define C_ESC BACK_CHAR
#define C_SPACE ' '
#define C_SEP ';'
#define C_STR '"'

int cType(int c) {
  if (isspace(c)) return C_SPACE;
  switch (c) {
  case CTRL_CHAR: case BACK_CHAR: return C_ESC;
  case ';' : case ',' : case '(' : case ')' : return C_SEP;
  case '"' : case '\'' : return C_STR;
  default : return c;
  }
}

int doEsc(int c);

/*---------------------------------------------------------------------------+
| scNextWord breaks lines into sequences of words
|
| Args: recognize_seps - 1 == recognize and return "();,"
|			 0 == ignore those and put them into words
|	wd - pointer to last recognized word, = 0 if at end of line
|
| Returns: input token type (usually a representative char)
+---------------------------------------------------------------------------*/
int scNextWord(int recognize_seps, char **wd)
{
  int c;

  while ((c = scGetChar()) != EOF) {
    switch (ScanState) {
    case SC_OUTSIDE:
      switch (cType(c)) {
      case C_ESC: NEW_STATE(SC_IN_WORD); _startWord(); _addCh(doEsc(c)); break;
      case C_SPACE: 							break;
      case C_SEP: if (recognize_seps) return c; 
		  else {
		    NEW_STATE(SC_IN_WORD); _startWord(); _addCh(c);
		  }							break;
      case C_STR: NEW_STATE(SC_IN_STR);  _startWord(); ScanDelim = c; 	break;
      default:    NEW_STATE(SC_IN_WORD); _startWord(); _addCh(c);	break;
      }
      break;

    case SC_IN_STR:
      switch (cType(c)) {
      case C_STR: 
	if (ScanDelim == c) { NEW_STATE(SC_OUTSIDE); *wd = _getWd(); 
			      return WORD;
			    }
	else  _addCh(c);						break;
      case C_ESC: _addCh(doEsc(c)); 					break;
      default:	  _addCh(c); 						break;
      }
      break;

    case SC_IN_WORD:
      switch (cType(c)) {
      case C_ESC: _addCh(doEsc(c)); 					break;
      case C_STR: NEW_STATE(SC_IN_STR); ScanDelim = c; 			break;
      case C_SEP: 
	if (recognize_seps) {
	  ScanState = SC_OUTSIDE; *wd = _getWd(); scUngetChar(c); 
	  return WORD;
	}
	else	  _addCh(c);						break;
      case C_SPACE: NEW_STATE(SC_OUTSIDE); *wd = _getWd(); return WORD;break;
      default: _addCh(c);						break;
      }
      break;

    default: perror("Unknown state"); abort();
      break;
    }
  }

  switch(ScanState) {
  case SC_IN_WORD: *wd = _getWd(); NEW_STATE(SC_OUTSIDE); return WORD; 
  case SC_IN_STR:  *wd = _getWd(); NEW_STATE(SC_OUTSIDE); return WORD; 
  case SC_OUTSIDE: return 0;
  default:  perror("Unknown state"); abort();
  }
}

/*---------------------------------------------------------------------------+
| getOct - read an octal number with up to 3 digits
+---------------------------------------------------------------------------*/
int getOct()
{
  char intstr[4]; int next = 0;
  int ch;

  while ((ch = scGetChar()) != EOF && next <= 3) 
    if (isdigit(ch))
      intstr[next++] = ch;
    else if (ch != EOF) { 
      scUngetChar(ch); 
      break;
    }
  
  intstr[next]=0;
  return strtol(intstr, NULL,  8);
}
    
/*---------------------------------------------------------------------------+
| Mapping table for the \[a-z] sequences: we let the C compiler initialize
| them for us... Of course we can change the value of a particular char if
| we feel like it.
+---------------------------------------------------------------------------*/
static char special[26] = 
{ '\a', '\b', 'c', 'd', 'e', '\f', 'g', 'h', 'i', 'j', 'k', 'l', 
  'm', '\n', 'o', 'p', 'q', '\r', 's', '\t', 'u', '\v', 'w', 'x',
  'y', 'z'
};

/*---------------------------------------------------------------------------+
| doEsc - process esc (\) and control (^) sequences
+---------------------------------------------------------------------------*/
int doEsc(int escChar) {

  int     c = scGetChar();

  switch (escChar) {
  case BACK_CHAR:
    	 if (c == EOF) return escChar;
    else if (isdigit(c)) { scUngetChar(c); return getOct(); }
    else if (islower(c)) return special[c - 'a'];
    else return c;
    break;

  case CTRL_CHAR: 
    if	    (islower(c)) return c - 'a' + 1;
    else if (isupper(c)) return c - 'A' + 1;
    else   { scUngetChar(c);  return escChar;  }
    break;

  default:
    perror("Unknown escape sequence"); abort();
  }
}

/*---------------------------------------------------------------------------+
| yylex - interface with yacc parser generator
+---------------------------------------------------------------------------*/
int yylex() 
{
  int state = scNextWord(1, &yylval.sval);
  return state;
}

/*---------------------------------------------------------------------------+
| GetWord - return successive words in input line, 0 when finished
+---------------------------------------------------------------------------*/
char*
GetNextWord() 
{
  char *word;

  if (scNextWord(0, &word) == 0) return "";
  return word;
}

char*
GetFirstWord(line) 
	 char *line;
{
  scSetInputBuffer(line);
  return GetNextWord();
}

#ifdef TEST
main()
{
  scSetInputBuf("Just to see if we'\\'re \\n\\033 able to distinguish' words and strings
\"Also 'quotes' inside strings\" and 'strs \"inside quotes\"'
Not to forget ^S and ^q control ^ chars");

  while (lGetWord() != 0);
}
#endif
