

/*
 * This file is part of the Seyon, Copyright (c) 1992-1993 by Muhammad M.
 * Saggaf. All rights reserved.
 *
 * See the file COPYING (1-COPYING) or the manual page seyon(1) for a full
 * statement of rights and permissions for this program.
*/

#include <math.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Dialog.h>

#include "seyon.h"
#include "SeDecl.h"

extern int      param_pipe[2];

int             DetNewlineTrMode(),
                ToggleNewlineMode(),
                SetIStrip(),
                ToggleDelMode(),
                ToggleMetaKeyTr(),
                ToggleXOffMode(),
                ToggleRtsctsMode(),
                ToggleZmodemAutoDownload(),
                ToggleIdleGuard(),
                MenuSetGetBaud(),
                MenuSetGetCSize(),
                MenuSetGetParity(),
                MenuSetGetStopBits(),
                MenuSetGetParamSCut(),
                MenuSetGetNewlineTrMode();
void            UpdateToggleSettings(),
                UpdateStates(),
                SetGetRadioVal(),
                SetDoGetRadioVal(),
                SetGetValue(),
                SetDoGetValue();
int             SetBaud(),
                SetPort();

int             newlineTrMode;

struct _setToggle {
  Widget          widget;
  char           *name;
  int             state;
  int            (*call_back)();
};

struct _setRadio {
  String          name;
  String          button[10];
  int             activeIndex;
  int             (*call_back)();
};

struct _setValue {
  char           *name;
  char            value[SM_BUF];
                  int(*call_back)();
};

static struct _setToggle set_toggle[] =
{
  {NULL, "stripHighBit", 0, SetIStrip},
  {NULL, "del", 0, ToggleDelMode},
  {NULL, "meta_tr", 0, ToggleMetaKeyTr},
  {NULL, "xoff", 0, ToggleXOffMode},
  {NULL, "rtscts", 0, ToggleRtsctsMode},
  {NULL, "autozm", 0, ToggleZmodemAutoDownload},
  {NULL, "idle", 0, ToggleIdleGuard},
  {NULL, NULL, 0, NULL}
};

static struct _setRadio setRadio[] =
{
  {"baud", {"300", "1200", "2400", "4800", "9600", "19200", "38400",
    "57600", "115200",
    NULL}, 1, MenuSetGetBaud},
  {"bits", {"5", "6", "7", "8", NULL}, 1, MenuSetGetCSize},
  {"parity", {"none", "odd", "even", NULL}, 1, MenuSetGetParity},
  {"stopBits", {"1", "2", NULL}, 1, MenuSetGetStopBits},
  {"cut", {"8n1", "7e1", "other", NULL}, 1, MenuSetGetParamSCut},
  {"nl_tr", {"nl", "cr", "cr_lf", NULL}, 1, MenuSetGetNewlineTrMode},
  {NULL, {NULL}, 0, NULL}
};

static struct _setValue set_value[] =
{
  {"port", "", SetPort},
  {NULL, "", NULL}
};

struct _setRadio *curRadObjPtr;
struct _setValue *curValObjPtr;

void
TopSet(w)
     Widget          w;
{
  Widget          popup,
                  mBox,
                  uBox,
                  lBox;
  struct _setToggle *tptr;
  struct _setRadio *rPtr;
  struct _setValue *vptr;

  ErrorIfBusy()

  tptr = set_toggle;
  tptr->state = qres.stripHighBit; tptr++;
  tptr->state = qres.backspaceTranslation; tptr++;
  tptr->state = qres.metaKeyTranslation; tptr++;
  tptr->state = qres.xonxoffFlowControl; tptr++;
  tptr->state = qres.rtsctsFlowControl; tptr++;
  tptr->state = qres.autoZmodem; tptr++;
  tptr->state = qres.idleGuard; tptr++;

  popup = SeAddPopup("set", w);
  mBox = AddPaned("mBox", popup);
  uBox = AddBox("uBox", mBox);
  lBox = AddBox("lBox", mBox);

  for (tptr = set_toggle; tptr->name != NULL; tptr++) {
    tptr->widget = SeAddToggleWCD(tptr->name, uBox, UpdateToggleSettings,
								  (XtPointer)(tptr->call_back));
    SeSetUnsetToggle(tptr->widget, tptr->state);
  }

  for (rPtr = setRadio; rPtr->name != NULL; rPtr++)
    SeAddButtonWCD(rPtr->name, uBox, SetGetRadioVal, (XtPointer) rPtr);

  for (vptr = set_value; vptr->name != NULL; vptr++)
    SeAddButtonWCD(vptr->name, uBox, SetGetValue, (XtPointer) vptr);

  SeAddButtonWCD("dismiss", lBox, DestroyShellCallBack, (XtPointer) mBox);

  XtPopup(popup, XtGrabExclusive);
}

void
SetGetRadioVal(widget, client_data)
     Widget          widget;
     XtPointer       client_data;
{
  struct _setRadio *rPtr = (struct _setRadio*)client_data;

  rPtr->activeIndex = (*rPtr->call_back)(-1);
  SePopupRadio(rPtr->name, widget, rPtr->button, rPtr->activeIndex,
			   SetDoGetRadioVal, (XtPointer)rPtr);
}

void
SetDoGetRadioVal(widget, client_data)
     Widget          widget;
     XtPointer       client_data;
{
  int   TerminalRefreshParameters();

  struct _setRadio *rPtr = (struct _setRadio *)client_data;
  Boolean         state;

  XtVaGetValues(widget, XtNradioData, &(rPtr->activeIndex), XtNstate,
		&state, NULL);

  /* The callback routine is called both when the widget is set or unset,
	 so we make sure we do nothing if the widget is unset */
  if (state == False)
    return;

  (void)(*rPtr->call_back) (rPtr->activeIndex);
  TerminalRefreshParameters();
  SeyonMessage("Parameter Change Performed");
}

void
SetGetValue(widget, client_data)
     Widget          widget;
     XtPointer       client_data;
{
  struct _setValue *vptr;

  vptr = set_value;
  strcpy(vptr->value, modem_port);
  vptr++;

  curValObjPtr = (vptr = (struct _setValue *)client_data);
  SePopupDialogGetStringE("set_value", widget, SetDoGetValue,
			  client_data, vptr->value, True);
}

void
SetDoGetValue(widget, client_data)
     Widget          widget;
     XtPointer       client_data;
{
  int   TerminalRefreshParameters();

  Widget          dialog = XtParent(widget);
  struct _setValue *vptr = (struct _setValue *)client_data;

  str_stripspc_copy(vptr->value, XawDialogGetValueString(dialog));
  DestroyShell(dialog);
  (void)(*vptr->call_back) (vptr->value);
  RestartTerminal();
  SeyonMessage("Parameter Change Performed");
}

void
setVal_action_ok(widget)
     Widget          widget;
{
  SetDoGetValue(widget, (XtPointer) curValObjPtr);
}

void
UpdateToggleSettings(widget, clientData)
     Widget          widget;
     XtPointer       clientData;
{
  int   TerminalRefreshParameters();

  /* We have to complicate things a bit to avoid the danger of the case
     SeGetTog.. = True = -1. One should not assume True = 1*/
  ((void (*)())clientData)(SeGetToggleState(widget) ? 1 : 0);
  TerminalRefreshParameters();
  SeyonMessage("Parameter Change Performed");
}

int
SetIStrip(state)
	 int state;
{
  qres.stripHighBit = (Boolean)state;
  return MdmSetGetIStrip(state);
}

int
ToggleDelMode()
{
  toggle_flag(&(qres.backspaceTranslation));
  return 0;
}

int
ToggleMetaKeyTr()
{
  toggle_flag(&(qres.metaKeyTranslation));
  return 0;
}

int
ToggleXOffMode()
{
  toggle_flag(&(qres.xonxoffFlowControl));
  xc_setflow();
  return 0;
}

int
ToggleRtsctsMode()
{
  toggle_flag(&(qres.rtsctsFlowControl));
  set_rtscts();
  return 0;
}

int
ToggleZmodemAutoDownload()
{
  toggle_flag(&(qres.autoZmodem));
  return 0;
}

int
ToggleIdleGuard()
{
  toggle_flag(&(qres.idleGuard));
  IdleGuard();
  return 0;
}

int
DetNewlineTrMode(keyword)
     String          keyword;
{
  char            kw[SM_BUF];

  if ((keyword == NULL) || (*keyword == '\0')) {
    SeError("missing newlineTranslation keyword");
    return -1;
  }

  str_stripspc_copy(kw, keyword);
  lc_word(kw);
  if (strcmp(kw, "nl") == 0)
    return 1;
  else if (strcmp(kw, "cr") == 0)
    return 2;
  else if (strcmp(kw, "cr/lf") == 0)
    return 3;
  else {
    SeErrorF("illigal newlineTranslation keyword: %s", keyword, "", "");
    return -1;
  }
}

void
SetNewlineTrMode(keyword)
     String          keyword;
{
  if ((newlineTrMode = DetNewlineTrMode(keyword)) < 0)
    newlineTrMode = 2;
}

int
MenuSetGetNewlineTrMode(trMode)
     int             trMode;
{
  if (trMode != -1)
    newlineTrMode = trMode;
  return newlineTrMode;
}

void
SetScrNewlineTrMode()
{
  getword();
  if (word[0] == '\0') {
    SeError("'set newlineTranslation' must specify translation mode");
    eof_flag++;
    return;
  }

  SetNewlineTrMode(word);
}

int
MenuSetGetBaud(baudIndex)
     int             baudIndex;
{
  return MdmSetGetBaud(baudIndex);
}

int
MenuSetGetCSize(bitsIndex)
     int             bitsIndex;
{
  return MdmSetGetCSize(bitsIndex == -1 ? bitsIndex :
			bitsIndex + 4) - 4;
}

int
MenuSetGetParity(parityIndex)
     int             parityIndex;
{
  return MdmSetGetParity(parityIndex == -1 ? parityIndex :
			 parityIndex - 1) + 1;
}

int
MenuSetGetStopBits(bitsIndex)
     int             bitsIndex;
{
  return MdmSetGetStopBits(bitsIndex);
}

int
MenuSetGetParamSCut(paramIndex)
     int             paramIndex;
{
  int             bits,
                  parity,
                  stopBits;

  if (paramIndex == -1) {

    bits = MdmSetGetCSize(-1);
    parity = MdmSetGetParity(-1);
    stopBits = MdmSetGetStopBits(-1);

    if (bits == 8 && parity == 0 && stopBits == 1)
      return 1;
    else if (bits == 7 && parity == 2 && stopBits == 1)
      return 2;
    else
      return 3;
  }

  if (paramIndex == 1) {
    MdmSetGetCSize(8);
    MdmSetGetParity(0);
    MdmSetGetStopBits(1);
  }
  else if (paramIndex == 2) {
    MdmSetGetCSize(7);
    MdmSetGetParity(2);
    MdmSetGetStopBits(1);
  }

  return paramIndex;
}

int
SetBaud(baud)
     String          baud;
{
  if (mbaud(baud) < 0) {
    SeErrorF("unsupported baud rate %s", baud, "", "");
    return False;
  }
  return True;
}

int
SetPort(port)
     String          port;
{
  int             retStatus,
	              reopRetStatus;
  String          oldPort = XtNewString(mport(NULL));

  unlock_tty();
  CloseModem();

  if ((retStatus = OpenModem(port)) < 0) {
	ShowOpenModemErrMsg(port, retStatus);

	if ((reopRetStatus = OpenModem(oldPort)) < 0)
	  {ShowOpenModemErrMsg(oldPort, reopRetStatus);
	  SeError(FmtString("Could not re-open old modem device %s", oldPort, "", 
						""));}
  }

  XtFree(oldPort);
  return retStatus;
}

void            s_set_xoff(),
                s_set_baud(),
                set_port(),
                s_set_cr(),
                ScrSetIStrip(),
                s_set_del(),
                set_meta_tr(),
                s_set_rtscts(),
                s_set_autozm(),
                SetScrIdleGuardMode(),
                ScrSetCSize(),
                ScrSetParity(),
                ScrSetStopBits();

struct kw       setlist[] =
{
  {"stripHighBit", ScrSetIStrip},
  {"bits", ScrSetCSize},
  {"parity", ScrSetParity},
  {"stopBits", ScrSetStopBits},
  {"newlineTranslation", SetScrNewlineTrMode},
  {"del", s_set_del},
  {"meta_tr", set_meta_tr},
  {"xoff", s_set_xoff},
  {"rtscts", s_set_rtscts},
  {"autozm", s_set_autozm},
  {"idleGuard", SetScrIdleGuardMode},
  {"baud", s_set_baud},
  {"port", set_port},
  {NULL, NULL}};

void
s_set()
{
  struct kw      *ptr;

  GETTEST_ARG("set");

/*  lc_word(word);*/

  for (ptr = setlist; ptr->keyword != NULL; ptr++)
    if (strcmp(ptr->keyword, word) == 0)
      {(*ptr->rtn)(); return;}

  SeErrorF("Invalid set keyword `%s'", word, "", "");
  eof_flag++;
}

void
s_set_cr()
{
  set_onoff(&(qres.newlineTranslation));
}

void
ScrSetIStrip()
{
  Boolean tmpSetFlag;

  set_onoff(&tmpSetFlag);
  SetIStrip(tmpSetFlag ? 1 : 0);
}

void
s_set_del()
{
  set_onoff(&(qres.backspaceTranslation));
}

void
set_meta_tr()
{
  set_onoff(&(qres.metaKeyTranslation));
}

void
s_set_xoff()
{
  set_onoff(&(qres.xonxoffFlowControl));
  xc_setflow();
}

void
s_set_rtscts()
{
  set_onoff(&(qres.rtsctsFlowControl));
  set_rtscts();
}

void
s_set_autozm()
{
  set_onoff(&(qres.autoZmodem));
}

void
SetScrIdleGuardMode()
{
  set_onoff(&(qres.idleGuard));
  IdleGuard();
}

void
set_onoff(flag)
     Boolean        *flag;
{
  String          keyWord = XtNewString(word);

  getword();
  lc_word(word);

  if (strcmp(word, "on") == 0) *flag = True;
  else if (strcmp(word, "off") == 0) *flag = False;
  else {
    SeErrorF("Argument `%s' to keyword `%s' is neither on nor off", 
			 word, keyWord, "");
    eof_flag++;
  }

  if (keyWord) XtFree(keyWord);
}

#define ScrGetArg(msg) \
{ \
  getword(); \
  if (word[0] == '\0') { \
    SeError(msg); \
    eof_flag++; \
    return; \
  } \
}

void
ScrSetCSize()
{
  ScrGetArg("set bits: missing argument");
  MdmSetGetCSize(atoi(word));
}

void
ScrSetParity()
{
  ScrGetArg("set parity: missing argument");
  MdmSetGetParity(atoi(word));
}

void
ScrSetStopBits()
{
  ScrGetArg("set stopBits: missing argument");
  MdmSetGetStopBits(atoi(word));
}

void
s_set_baud()
{
  getword();
  if (word[0] == '\0') {
    SeError("'set baud' must specify baud rate");
    eof_flag++;
    return;
  }

  if (SetBaud(word) == False)
    eof_flag++;
}

void
set_port()
{
  SeNotice("/sp/ Sorry, \"set port\" is not supported in this release");
  return;

  getword();
  if (word[0] == '\0') {
    SeError("'set port' must specify modem device");
    eof_flag++;
    return;
  }

  if (SetPort(word) == False)
    eof_flag++;
}

struct param {
  Boolean         autoZmodem;
  Boolean         idleGuard;
  Boolean         stripHighBit;
  Boolean         backspaceTranslation;
  Boolean         metaKeyTranslation;
  Boolean         xonxoffFlowControl;
  Boolean         rtsctsFlowControl;
  int             newlineTrMode;

};

int
PutParameters(destination)
	 int *destination;
{
  struct param    param;
  int             writeRetStatus;

  param.idleGuard = qres.idleGuard;
  param.stripHighBit = qres.stripHighBit;
  param.backspaceTranslation = qres.backspaceTranslation;
  param.metaKeyTranslation = qres.metaKeyTranslation;
  param.xonxoffFlowControl = qres.xonxoffFlowControl;
  param.rtsctsFlowControl = qres.rtsctsFlowControl;
  param.autoZmodem = qres.autoZmodem;
  param.newlineTrMode = newlineTrMode;

  if ((writeRetStatus = write(destination[1], (char*)&param, sizeof(param)))
	  == -1)
    SePError("Could not write to pipe");
  
  return writeRetStatus;
}

void
GetParameters(clientData, source)
	 XtPointer  clientData;
	 int       *source;
{
  struct param    param;

  if (read(source[0], (char*)&param, sizeof(param)) == -1)
	SePError("Could not read from pipe");

  qres.idleGuard = param.idleGuard;
  qres.stripHighBit = param.stripHighBit;
  qres.backspaceTranslation = param.backspaceTranslation;
  qres.metaKeyTranslation = param.metaKeyTranslation;
  qres.xonxoffFlowControl = param.xonxoffFlowControl;
  qres.rtsctsFlowControl = param.rtsctsFlowControl;
  qres.autoZmodem = param.autoZmodem;
  newlineTrMode = param.newlineTrMode;

  get_modem_attr();
}
