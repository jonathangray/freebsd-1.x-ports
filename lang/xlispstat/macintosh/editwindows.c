/* edit - Macintosh editing functions                                  */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#ifdef MPWC
# include <TextEdit.h>
# include <Files.h>
# include <Packages.h>
# include <Windows.h>
# include <Strings.h>
# define MIN(x,y) (((x) < (y)) ? (x) : (y))
# define RETURNCHAR '\n'
#else
# include <TextEdit.h>
# include "FileMgr.h"
# include "StdFilePkg.h"
# include <WindowMgr.h>
# include <strings.h>
# define RETURNCHAR '\r'
#endif MPWC
# include "xlisp.h"
# include "TransEdit.h"
# include "windows.h"

# define BUFMAX STRMAX
# define MAXCOUNT 10000L
# define KEEPCOUNT 5000L

extern LVAL slot_value(), xmsend(), get_window_object();
extern char *get_edit_window_address();
extern mac_update_action(), mac_activate_action(), mac_close_action();

extern WindowPtr ttyWind;
extern LVAL s_true, sk_remove, s_input_stream;
extern char buf[];
static char ttybuf[BUFMAX];
static int ttybufcount = 0, ttyfixcount = 0;

# define enter 3
# ifndef shiftKey
# define shiftKey 0x0200
# endif

static LVAL get_output_stream(w)
	WindowPtr w;
{
  LVAL object = get_window_object(w), s_output_stream = xlenter("OUTPUT-STREAM");
  LVAL stream = NIL;

  if (mobject_p(object)) lex_slot_value(object, s_output_stream, &stream);
  return(ustreamp(stream) ? stream :NIL);
}
  
static LVAL get_input_stream()
{
  LVAL s_input_stream = xlenter("*INPUT-STREAM*");

  if (ttyWind == nil) return(NIL);
  else return(getvalue(s_input_stream));
}
  
static flush_output_stream(w)
	WindowPtr w;
{
  LVAL stream = get_output_stream(w);
  int ch;
  TEHandle te = GetEWindowTE(w);
  
  if (w == ttyWind) TtyFlush();
  if (stream != NIL && (ch = xlgetc(stream)) != EOF) {
    xlungetc(stream, ch);
    paste_stream(w, stream, FALSE);
    if ((w == ttyWind || ttyWind == nil) && (*te)->teLength > MAXCOUNT)
      flush_window(w, (*te)->teLength - KEEPCOUNT);
    adjust_fixed_count(w);
  }
}

static flush_window(w, byteCount)
	WindowPtr w;
	long byteCount;
{
  TEHandle te = GetEWindowTE(w);
  
  TESetSelect (0L, byteCount, te);          /* select text */
  TEDelete (te);                            /* clobber it */
  TESetSelect((*te)->teLength, (*te)->teLength, te);
  adjust_fixed_count(w);
  EWindowAdjustDisplay (w);
}

static InsertText(buf, count, te)
	char *buf;
	long count;
	TEHandle te;
{
  if ((*te)->teLength + count > 32000) xlfail("Buffer is too big");
  else TEInsert(buf, count, te);
}

static get_fixed_count(w, count)
	WindowPtr w;
	int *count;
{
  if (w == ttyWind || ttyWind == nil) {
    if (count != nil) *count = ttyfixcount;
    return(TRUE);
  }
  else return(FALSE);
}

static set_fixed_count(w, count)
	WindowPtr w;
	int count;
{
  if (w == ttyWind || ttyWind == nil) ttyfixcount = count;
}

adjust_insert(w)
	WindowPtr w;
{
  int count;
  TEHandle te = GetEWindowTE(w);
  
  flush_output_stream(w);
  if (get_fixed_count(w, &count) && (*te)->selStart < count) {
    TESetSelect((*te)->teLength, (*te)->teLength, te);
  }
}

static adjust_fixed_count(w) 
	WindowPtr w;
{
  int count;
  TEHandle te = GetEWindowTE(w);

  if (get_fixed_count(w, &count)) {
    set_fixed_count(w, (*te)->teLength);
  }
  if (get_fixed_count(w, &count) && (*te)->selStart < count) {
    TESetSelect((*te)->teLength, (*te)->teLength, te);
  }
}

static paste_stream(w, stream, input)
	WindowPtr w;
	LVAL stream;
	int input;
{
  int buffpos = 0, ch;

  if (! IsEWindow(w) || stream == NIL) return(FALSE);
  
  while((ch = xlgetc(stream)) != EOF) {
    if (ch == '\n') ch = RETURNCHAR;
    buf[buffpos++] = ch;
    if (buffpos > BUFMAX) {
      InsertText(buf, (long) buffpos, GetEWindowTE(w));
      buffpos = 0;
    }
  }
  InsertText(buf, (long) buffpos, GetEWindowTE(w));
  EWindowAdjustDisplay (w);
  if (input && w == ttyWind) return_action(GetEWindowTE(w));
  if (! input_enabled(w) || w == ttyWind) SetEWindowDirty(w, FALSE);
  return(TRUE);
}

static paste_string(w, str, input)
	WindowPtr w;
	char *str;
	int input;
{
  int buffpos = 0, ch;
  long len;
  
  if (! IsEWindow(w) || str == nil) return(FALSE);

  len = strlen(str);
  while(len -- > 0) {
    ch = *str++;
    if (ch == '\n') ch = RETURNCHAR;
    buf[buffpos++] = ch;
    if (buffpos > BUFMAX) {
      InsertText(buf, (long) buffpos, GetEWindowTE(w));
      buffpos = 0;
    }
  }
  InsertText(buf, (long) buffpos, GetEWindowTE(w));
  EWindowAdjustDisplay (w);
  if (input && w == ttyWind) return_action(GetEWindowTE(w));
  return(TRUE);
  EWindowAdjustDisplay (w);
  if (input && w == ttyWind) return_action(GetEWindowTE(w));
  if (! input_enabled(w) || w == ttyWind) SetEWindowDirty(w, FALSE);
  return(TRUE);
}

static input_enabled(w)
	WindowPtr w;
{
  LVAL enabled = s_true, object = get_window_object(w);
  LVAL s_input_enabled = xlenter("INPUT-ENABLED");
  
  if (w == ttyWind) return(TRUE);
  if (mobject_p(object)) lex_slot_value(object, s_input_enabled, &enabled);
  return((enabled != NIL));
}

static tty_enter()
{
  TEHandle te = GetEWindowTE(ttyWind);

  if (te == nil) return;
  adjust_insert(ttyWind);
  if ((*te)->selStart < (*te)->teLength)
    TESetSelect((*te)->teLength, (*te)->teLength, te);
  else tty_return();
}

static tty_return()
{
  TEHandle te = GetEWindowTE(ttyWind);

  if (te == nil) return;
  adjust_insert(ttyWind);
  TEKey (RETURNCHAR, te);
  return_action(te);
}

TtyPutc(c)
     char c;
{
  ttybuf[ttybufcount++] = c;
  if ((c == RETURNCHAR) || ttybufcount >= (BUFMAX - 1)) TtyFlush();
}

TtyPrint(s)
	char *s;
{
  while (strlen(s) > 0) TtyPutc(*s++);
  TtyFlush();
}

TtyFlush()
{
  TEHandle te = GetEWindowTE(ttyWind);
  int count;
  
  if (ttybufcount > 0) {
    if (get_fixed_count(ttyWind, &count) && (*te)->selStart < count) {
      TESetSelect((*te)->teLength, (*te)->teLength, te);
    }
    adjust_fixed_count(ttyWind);
    InsertText(ttybuf, (long) ttybufcount, GetEWindowTE(ttyWind));
    ttybufcount = 0;
    if ((*te)->teLength > MAXCOUNT) {
      flush_window(ttyWind, (*te)->teLength - KEEPCOUNT);
    }
    TESetSelect((*te)->teLength, (*te)->teLength, te);
    adjust_fixed_count(ttyWind);
    EWindowAdjustDisplay (ttyWind);
  }
}

static GetInputLine()
{
  int i, has_fixed, count;
  char **mytext;
  TEHandle te = GetEWindowTE(ttyWind);
  LVAL stream = get_input_stream();
  
  mytext = (char **)TEGetText(te);
  has_fixed = get_fixed_count(ttyWind, &count);
  if (! has_fixed) count = 0;
  if (ustreamp(stream) && check_parens(*mytext + count, (*te)->teLength - count)) {
    for (i = count; i < (*te)->teLength; i++)
      xlputc(stream, (*mytext)[i]);
    if (has_fixed) adjust_fixed_count(ttyWind);
    return(TRUE);
  }
  else return(FALSE);
}

static check_parens(s, n)
	char *s;
	int n;
{
  int parcount = 0, inquotes = FALSE, incomment = FALSE;
  char ch;
   
  while (n-- > 0) {
    ch = *s++;
    switch (ch) {
    case  '"': inquotes = ! inquotes; break;
    case  ';': if (! inquotes) incomment = TRUE; break;
    case '\r':
    case '\n': incomment = FALSE; break;
    case  '(': if (! inquotes && ! incomment) parcount++; break;
    case  ')': if (! inquotes && ! incomment) parcount--; break;
    }
  }
  return (parcount <= 0);
}

LVAL xsedit_window_paste_stream()
{
  LVAL stream;
  WindowPtr w;

  w = (WindowPtr) get_edit_window_address(xlgetarg());
  stream = xlgetfile();
  xllastarg();

  adjust_insert(w);
  return((paste_stream(w, stream, TRUE)) ? s_true : NIL);
}

LVAL xsedit_window_paste_string()
{
  LVAL string;
  WindowPtr w;

  w = (WindowPtr) get_edit_window_address(xlgetarg());
  string = xlgastring();
  xllastarg();

  adjust_insert(w);
  return((paste_string(w, getstring(string), TRUE)) ? s_true : NIL);
}

LVAL xsedit_window_flush_window()
{
  WindowPtr w;
  long count;
  
  w = (WindowPtr) get_edit_window_address(xlgetarg());
  count = (moreargs()) ? getfixnum(xlgafixnum()) : 32767;
  xllastarg();

  flush_window(w, count);
  return(NIL);
}

static flash_matching_paren(teEdit, start)
	TEHandle teEdit;
	int start;
{
  int parcount = 0, inquotes = FALSE, sel, par;
  char ch, *s;
  
  sel = (*teEdit)->selStart;
  s = *((char **)TEGetText(teEdit)) + sel - 1;
  par = sel;
  do {
    par--;
    ch = *s--;
    switch (ch) {
    case  '"': inquotes = ! inquotes; break;
    case  '(': if (! inquotes) parcount--; break;
    case  ')': if (! inquotes) parcount++; break;
    }
  } while (par >= start && parcount > 0);
  if (ch == '(') {
    TESetSelect(par, par + 1, teEdit);
	pardelay();
    TESetSelect(sel, sel, teEdit);
  }
  return (parcount <= 0);
}

static pardelay()
{
  long t = 5, f;
  Delay(t, &f);
}

static do_tab(teEdit, start)
	TEHandle teEdit;
	int start;
{
  int sel, curline, lastline, pos, nblanks, inquote, parcount;
  char *s, ch;
  
  /* for an edit window get rid of the inserted tab 
  if (teEdit != TTY.teEdit) TEKey('\b', teEdit);*/
      
  sel = (*teEdit)->selStart;
  s = *((char **)TEGetText(teEdit));

  /* find beginning of the line */
  curline = sel;
  while (curline > start && s[curline - 1] != RETURNCHAR) curline--;
  if (curline == start) return;

  /* find unmatched paren */
  parcount = 0;
  inquote = FALSE;
  pos = curline;
  while (parcount >= 0 && --pos >= start) {
    ch = s[pos];
    switch (ch) {
    case ')': if (! inquote) parcount++; break;
    case '(': if (! inquote) parcount--; break;
    case '"': inquote = ! inquote; break;
    }
  }
  if (parcount == 0) return;
  
  /* find beginning of the previous line */
  lastline = pos;
  while (lastline > 0 && s[lastline - 1] != RETURNCHAR) lastline--;

  /* skip forward an s-expression or to first non blank */
  pos += num_to_skip(s + pos, curline - pos);

  if (pos > curline) pos = curline;
  nblanks = pos - lastline;

  /* adjust for the number of blanks already present, replace tabs by blanks */
  for (pos = curline; 
       pos < (*teEdit)->teLength && (s[pos] == ' ' || s[pos] == '\t');
       nblanks--, pos++)
    if (s[pos] == '\t') s[pos] = ' ';
  
  /* insert or delete the appropriate number of blanks */
  if (nblanks == 0) return;
  
  sel += nblanks;
  if (pos > (*teEdit)->teLength) pos = (*teEdit)->teLength;
  TESetSelect(pos, pos, teEdit);
  fix_blanks(teEdit, nblanks, curline);
  if (sel > (*teEdit)->teLength) sel = (*teEdit)->teLength;
  TESetSelect(sel, sel, teEdit);
}

static fix_blanks(teEdit, nblanks, curline)
	TEHandle teEdit;
	int nblanks, curline;
{
  int i;
  
  if (nblanks > 0) {
    for (i = 0; i < nblanks; i++) buf[i] = ' ';
    TESetSelect(curline, curline, teEdit);
    TEInsert(buf, (long) nblanks, teEdit);
  }
  else {
    TESetSelect(curline, curline - nblanks, teEdit);
    TECut(teEdit);
  }
}

static at_text_end(teEdit)
	TEHandle teEdit;
{
  int i, result = TRUE;
  char *s = *((char **)TEGetText(teEdit));
  
  for (i = (*teEdit)->selStart; result && i < (*teEdit)->teLength; i++)
    if (! isspace(s[i])) result = FALSE;
  return(result);
}

static last_is_return(teEdit)
	TEHandle teEdit;
{
  int i;
  char *s = *((char **)TEGetText(teEdit));
  
  for (i = (*teEdit)->selStart - 1; i >= 0 && isspace(s[i]); i--)
    ;
  i = MIN((*teEdit)->selStart - 1, i + 1);
  return(s[i] == RETURNCHAR);
}

return_action(te)
	TEHandle te;
{	
  if (at_text_end(te) && last_is_return(te) && GetInputLine()) {
	flush_output_stream(ttyWind);
    getttyline(get_input_stream());
  }
}

static num_to_skip(s, n)
	char *s;
	int n;
{
  char str[4];
  int i, pos, oldpos;
  
  pos = 0;
  
  if (n <= 0) pos = 0;
  else if (*s == '(') {
  
    s++; n--; pos = 1;
    
    /* skip blanks */
    while (n > 0 && (*s == ' ' || *s == '\t')) { s++; n--; pos++; }
    
    /* check for end of line or list or lisp comment*/
    if (n > 0 && *s != RETURNCHAR && *s != ';' && *s != '(') {
    
      /* check for special symbols */
      for (i = 0; i < 3 && i < n; i++)
        str[i] = toupper(s[i]);
      str[i] = '\0';
      if (is_special(s, n) /* strcmp(str, "DEF") == 0 || strcmp(str, "LET") == 0 
          || strcmp(str, "FLE") == 0 */ )
        pos = 2;
      else {
        
        /* skip over the s-expression */
        oldpos = pos;
        while (n > 0 && *s != ' ' && *s != '\t' && *s != RETURNCHAR)
          { s++; n--; pos++; }
          
        /* check for another s expression */
        for (i = 0; n > 0 && (*s == ' ' || *s == '\t'); s++, n--, i++) ;
        if (n == 0 || *s == RETURNCHAR)
		  pos = (oldpos == pos) ? oldpos + 1 : oldpos;
        else pos += i;
      }
    }
  }
  else {
    
    /* skip over any blanks */
    for (i = 0; n > 0 && (*s == ' ' || *s == '\t'); s++, n--, i++) ;
    if (n > 0 && *s != RETURNCHAR) pos += i;
  }
  return(pos);
}

static is_special(s, n)
	char *s;
	int n;
{
  char str[10];
  int i;
  
  for (i = 0; i < n && i < 9; i++) str[i] = toupper(s[i]);  
  str[i] = '\0';

  if (n >= 5 && strncmp(str, "DEFUN", 5) == 0) return(TRUE);
  if (n >= 8 && strncmp(str, "DEFMACRO", 8) == 0) return(TRUE);
  if (n >= 7 && strncmp(str, "DEFMETH", 7) == 0) return(TRUE);
  if (n >= 8 && strncmp(str, "DEFPROTO", 8) == 0) return(TRUE);
  if (n >= 3 && strncmp(str, "LET", 3) == 0) return(TRUE);
  if (n >= 4 && strncmp(str, "FLET", 4) == 0) return(TRUE);
  if (n >= 4 && strncmp(str, "COND", 4) == 0) return(TRUE);
  if (n >= 4 && strncmp(str, "CASE", 4) == 0) return(TRUE);
  if (n >= 6 && strncmp(str, "LABELS", 6) == 0) return(TRUE);
  if (n >= 6 && strncmp(str, "LAMBDA", 6) == 0) return(TRUE);
  return(FALSE);
}

static close_listener() 
{
  HideWindow(ttyWind);
}

static key_filter(c)
	char c;
{
  WindowPtr editWind;
  TEHandle editTE;
  int count, has_fixed;
    
  GetPort(&editWind);
  editTE = GetEWindowTE(editWind);
  if (editWind == nil) return('\0');
  has_fixed = get_fixed_count(editWind, &count);
  if (! has_fixed) count = 0;
  
  if (! input_enabled(editWind)) return ('\0');
  switch (c) {
  case '\034':
  case '\035':
  case '\036':
  case '\037': return(c);  /* arrow keys */
  case '\t':   adjust_insert(editWind); do_tab(editTE, count); return('\0');
  case '\b':
    adjust_insert(editWind); 
    if ((*editTE)->selStart > count
        || ((*editTE)->selStart == count 
        && (*editTE)->selStart < (*editTE)->selEnd)) {
       return(c);
    }
    else return('\0');
  case enter: if (editWind == ttyWind && ttyWind != nil) tty_enter(); return('\0');
  case RETURNCHAR:
    if (editWind == ttyWind) {
      tty_return();
      return('\0');
    }
    else {
      adjust_insert(editWind);
      return(RETURNCHAR);
    }
  case ')':
  	adjust_insert(editWind);
    TEKey(c, editTE);
    flash_matching_paren(editTE, count);
    return('\0');
  default:
      adjust_insert(editWind);
      return(c);
  }
}

static tty_key()
{
  SetEWindowDirty (ttyWind, FALSE);
}

static edit_idle ()
{
  GrafPtr port;
  TEHandle editTE;
  
  GetPort(&port);
  flush_output_stream(port);
  if (! input_enabled(port)) {
    editTE = GetEWindowTE(port);
    if (editTE != nil) TEDeactivate (editTE);
  }
}

static close_edit()
{
  GrafPtr w;
  LVAL object;
  
  GetPort(&w);
  object = get_window_object(w);
  if (mobject_p(object)) send_message(object, sk_remove);
}

make_listener_window(r)
	Rect r;
{
  ttyWind = NewEWindow (&r, "\pXLISP-STAT", true, -1L, TRUE, 0L, FALSE);
  SetEWindowProcs(ttyWind, tty_key, nil, close_listener);
  SetEWindowKeyFilter(ttyWind, key_filter);
  SetEWindowIdle(ttyWind, edit_idle);
}

set_edit_window_procs(w)
	WindowPtr w;
{
  if (IsEWindow(w)) {
    SetEWindowProcs(w, nil, nil, close_edit);
    SetEWindowKeyFilter(w, key_filter);
    SetEWindowIdle(w, edit_idle);
  }
}
    
LVAL xsedit_window_update()   { return(NIL); }
LVAL xsedit_window_activate() { return(NIL); }