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
# include <Desk.h>
# include <Scrap.h>
#else
# include <TextEdit.h>
# include "FileMgr.h"
# include "StdFilePkg.h"
# include <WindowMgr.h>
# include <strings.h>
#endif MPWC
# include "xlisp.h"
# include "TransEdit.h"
# include "windows.h"

/* static global variables */
static short default_vref;

/* external variables */
extern LVAL s_true, s_hardware_address, sk_dispose, s_size, s_location,
  s_title, s_bind_to_file, sk_allocate, sk_go_away, s_go_away,
  s_input_stream, sk_show;
extern WindowPtr ttyWind;
extern char buf[];

/* external functions */
extern LVAL make_hardware_address(), get_hardware_object_by_address(),
  slot_value();
extern char *get_hardware_address(), *get_edit_window_address();
extern char *StCalloc();

#define dfltLeft 10
#define dfltTop 40
#define dfltWidth 490
#define dfltHeight 280

# define check_edit_window_address(d) valid_edit_window_address(slot_value(d, s_hardware_address))

/***********************************************************************/
/**                                                                   **/
/**                          Utilitiy Functions                       **/
/**                                                                   **/
/***********************************************************************/

get_default_volume() { GetVol(buf, &default_vref); }

set_volume(new_vref)
	int new_vref;
{
  short vref;
  
  GetVol(buf, &vref);
  SetVol(nil, new_vref);
  return((int) vref);
}

LVAL xssetvolume()
{
  int new_vref;
  
  new_vref = (moreargs()) ? getfixnum(xlgafixnum()) : default_vref;
  return(cvfixnum((FIXTYPE) set_volume(new_vref)));
}

Rect ListToRect(list)
	LVAL list;
{
  LVAL x;
  Rect r;
  int rvals[4], i;
  
  if (! listp(list) || llength(list) != 4)
    xlerror("not a rectangle", list);
  for (i = 0; i < 4; i++, list = cdr(list)) {
  	x = car(list);
  	if (! fixp(x)) xlerror("not an integer", x);
  	rvals[i] = getfixnum(x);
  }
  SetRect(&r, rvals[0], rvals[1], rvals[2], rvals[3]);
  return(r);
}

LVAL xsopenfiledialog()
{
  SFReply reply;
  SFTypeList	myTypes;
  Point p;
  LVAL result;
  Boolean setvol = TRUE;
  
  if (moreargs())
    setvol = (xlgetarg() != NULL) ? TRUE : FALSE;
  xllastarg();
	
  myTypes[0]='TEXT';
  SetPt(&p, 82, 90);
  SFGetFile( p, "\p", 0L, 1, myTypes, 0L, &reply );
  if (reply.good) {
    PtoCstr((char *) reply.fName);
    result = newstring(strlen(reply.fName) + 1);
    strcpy(getstring(result), reply.fName);
    if (setvol) SetVol(nil, reply.vRefNum);
  }
  else result = NULL;
  return(result);
}

LVAL xssetfiledialog()
{
  SFReply reply;
  Point p;
  LVAL result;
  char *prompt, *dflt = "";
  Boolean setvol = TRUE;
  
  prompt = (char *) getstring(xlgastring());
  if (moreargs())
    dflt = (char *) getstring(xlgastring());
  if (moreargs())
    setvol = (xlgetarg() != NULL) ? TRUE : FALSE;
  xllastarg();
  
  SetPt(&p, 82, 90);
  CtoPstr(prompt);
  CtoPstr(dflt);
  SFPutFile(p, prompt, dflt, nil, &reply);
  PtoCstr(prompt);
  PtoCstr(dflt);
  if (reply.good) {
    PtoCstr((char *) reply.fName);
    result = newstring(strlen(reply.fName) + 1);
    strcpy(getstring(result), reply.fName);
    if (setvol) SetVol(nil, reply.vRefNum);
  }
  else result = NULL;
	
  return(result);
}

LVAL xsfrontwindow() { return(get_hardware_object_by_address(FrontWindow())); }

LVAL xshidefrontwindow()
{
	WindowPeek wPeek;
	
	xllastarg();
	
	if ((wPeek = (WindowPeek) FrontWindow()) != NIL) {
		if (wPeek->windowKind < 0)
	    	CloseDeskAcc(wPeek->windowKind);
	    else
	      	HideWindow(FrontWindow());
	}
	return(NIL);
}

LVAL xssystem_edit()
{
  int item;
  
  item = getfixnum(xlgafixnum());
  xllastarg();
  
  return((SystemEdit(item)) ? s_true : NIL);
}

/***********************************************************************/
/**                                                                   **/
/**                          Listener Methods                         **/
/**                                                                   **/
/***********************************************************************/

LVAL xslistener_isnew()
{
  LVAL object = xlgaobject();
  LVAL s_input_stream = xlenter("INPUT-STREAM");
  LVAL s_output_stream = xlenter("OUTPUT-STREAM");
  LVAL s_input_enabled = xlenter("INPUT-ENABLED");
  
  if (xsboolkey(sk_show, TRUE)) send_message(object, sk_allocate);
  if (slot_value(object, s_input_stream) == NIL)
    set_slot_value(object, s_input_stream, getvalue(xlenter("*INPUT-STREAM*")));
  if (slot_value(object, s_output_stream) == NIL)
    set_slot_value(object, s_output_stream, newustream());
  set_slot_value(object, s_input_enabled, s_true);
  return(object);
}

LVAL xslistener_allocate()
{
  LVAL object = xlgaobject();
  WindowData data;
  
  if (get_window_data(ttyWind) == nil) {
    data = (WindowData) StCalloc(sizeof(struct window_data), 1);
    set_window_data(ttyWind, data);
  }
  if (slot_value(object, s_hardware_address) != NIL)
    standard_hardware_clobber(object);
  set_edit_window_address(ttyWind, object);
  set_window_object(ttyWind, object);
  return(object);
}

static LVAL clip_stream()
{
  LVAL stream;
  int numChar, count;
  char **scrapH, ch;

  TEFromScrap ();
  numChar = TEGetScrapLen();
  scrapH = TEScrapHandle();

  xlsave1(stream);

  stream = newustream();

  for (count = 0; count < numChar; count++) {
    ch = (*scrapH)[count];
    if (ch == '\r') ch = '\n';
    xlputc(stream, ch);
  }
  
  xlpop();	
  return(stream);
}

/***********************************************************************/
/**                                                                   **/
/**                         Edit Window Methods                       **/
/**                                                                   **/
/***********************************************************************/

LVAL xsedit_window_isnew()
{
  LVAL s_output_stream = xlenter("OUTPUT-STREAM");
  LVAL s_input_enabled = xlenter("INPUT-ENABLED");
  LVAL object, value;
  
  object = xlgaobject();
  object_isnew(object);
    
  if (slot_value(object, s_output_stream) == NIL)
    set_slot_value(object, s_output_stream, newustream());
  set_slot_value(object, s_input_enabled, s_true);
  if (! xlgetkeyarg(sk_go_away, &value)) value = s_true;
  set_slot_value(object, s_go_away, value);
  
  if (xsboolkey(sk_show, TRUE)) send_message(object, sk_allocate);
  return(object);
}

LVAL xsedit_window_allocate()
{
  LVAL object, value;
  Rect bounds;
  char *title;
  int visible, goAway, bindToFile;
  WindowPtr behind;
  long refNum;
  WindowPtr w;
  WindowData data;
  int left, top, width, height;
  
  object = xlgaobject();
  if (check_edit_window_address(object))
    send_message(object, sk_dispose);

  left = dfltLeft;
  top = dfltTop;
  width = dfltWidth;
  height = dfltHeight;
  get_window_bounds(object, &left, &top, &width, &height);
  SetRect(&bounds, left, top, left + width, top + height);

  value = slot_value(object, s_title);
  title = stringp(value) ? (char *) getstring(value) : "";
  if (strlen(title) == 0) title = nil;
  goAway = slot_value(object, s_go_away) != NIL;
  visible = TRUE;
  behind = (WindowPtr) -1;
  refNum = nil;
  bindToFile = (slot_value(object, s_bind_to_file) != nil) ? TRUE : FALSE;
  
  CtoPstr(title);
  w = NewEWindow (&bounds, title, visible, behind, 
                  goAway, refNum, bindToFile);
  PtoCstr(title);
  if (w == NULL) return(NIL);
  else {
    set_edit_window_procs(w);
    set_edit_window_address(w, object);
    data = (WindowData) StCalloc(sizeof(struct window_data), 1);
    set_window_data(w, data);
    set_window_object(w, object);
    return(object);
  }
}

static LVAL edit_window_edit(which)
	int which;
{
  WindowPtr w;

  w = (WindowPtr) get_edit_window_address(xlgetarg());
  xllastarg();
   
  if (IsEWindow(w) && w == FrontWindow()) {
    if (which == 3 || which == 5) adjust_insert(w);
    EWindowEditOp(which);
    if (w == ttyWind && which == 5) return_action(GetEWindowTE(w));
    if (w == ttyWind) SetEWindowDirty(w, FALSE);
  }
  return(NIL);
}

LVAL xsedit_window_cut()   { return(edit_window_edit(3)); }
LVAL xsedit_window_copy()  { return(edit_window_edit(4)); }
LVAL xsedit_window_paste() { return(edit_window_edit(5)); }

static LVAL ewindow_action(action)
	int action;
{
  WindowPtr w;
  int result;

  w = (WindowPtr) get_edit_window_address(xlgetarg());
  xllastarg();
  if (w == ttyWind) return(NIL);
  
  switch (action) {
  case 'R': result = EWindowRevert(w); break;
  case 'S': result = EWindowSave(w); break;
  case 'A': result = EWindowSaveAs(w); break;
  case 'C': result = EWindowSaveCopy(w); break;
  }
  return((result) ? s_true : NIL);
}

LVAL xsedit_window_revert()    { return(ewindow_action('R')); }
LVAL xsedit_window_save()      { return(ewindow_action('S')); }
LVAL xsedit_window_save_as()   { return(ewindow_action('A')); }
LVAL xsedit_window_save_copy() { return(ewindow_action('C')); }

LVAL xsedit_window_selection_stream()
{
  WindowPtr w;
  TEHandle te;
	
  w = (moreargs()) ? (WindowPtr) get_edit_window_address(xlgetarg()) : nil;
  xllastarg();

  if (w != nil) {
    te = GetEWindowTE(w);
    if (te == nil) xlfail("not an edit window");
    TECopy (te);
    ZeroScrap ();
    TEToScrap ();
  }
  return(clip_stream());
}

LVAL xsedit_window_remove()
{
  WindowPtr w;
  LVAL object;
  int result;
  WindowData data;
  
  object = xlgaobject();
  if (check_edit_window_address(object)) {
    w = (WindowPtr) get_edit_window_address(object);
    data = (WindowData) get_window_data(w);
    if (w == ttyWind) result = FALSE;
    else result = (! IsEWindow(w) || EWindowClose(w));
    if (result) StFree(data);
  }
  else result = TRUE;
  if (result) standard_hardware_clobber(object);
  return((result) ? s_true : NIL);
}

LVAL xsedit_window_find()
{
  WindowPtr w;
  int result = FALSE;
  char **ptext, *text, *s;
  long i, n, len, start;
  TEHandle te;
  
  w = (WindowPtr) get_edit_window_address(xlgaobject());
  s = (char *) getstring(xlgastring());

  if (IsEWindow(w) && (te = GetEWindowTE(w)) != nil) {
    n = (*te)->teLength;
    start = (*te)->selEnd;
    ptext = (char **) TEGetText(te);
    len = strlen(s);
    for (i = start; i < n - len; i++) {
      text = *ptext + i;
      if (match(s, text, len)) {
        TESetSelect (i, i + len, te);
        result = TRUE;
        break;
      }
    }
  }
  if (result) EWindowOverhaul(w, TRUE, FALSE, FALSE);
  return(result ? s_true : NIL);
}

static match(s1, s2, n)
	char *s1, *s2;
	long n;
{
  while (n-- > 0)
    if (toupper(*s1++) != toupper(*s2++)) return(FALSE);
  return(TRUE);
}