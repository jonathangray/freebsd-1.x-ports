
/*
 * This file is part of the Seyon, Copyright (c) 1992-1993 by Muhammad M.
 * Saggaf. All rights reserved.
 *
 * See the file COPYING (1-COPYING) or the manual page seyon(1) for a full
 * statement of rights and permissions for this program.
 */

#include <time.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include "seyon.h"
#include "SeDecl.h"

#ifdef SUNOS_3
#include <setjmp.h>
#endif

#define	MAX_PATH	256
#define	MAX_LINE	128

extern int      MdmTimedReadChar();

extern FILE    *tfp;
extern char     scriptDirectory[REG_BUF];

static FILE    *cf;
int             waitfor_time = 0,
                if_flag = 0,
                waitflag = 0;
Boolean         tty_flag = True,
                echo_flag = False,
                captflag = True;
jmp_buf         here;

void            k_debug(),
                k_echo(),
                k_flush(),
                k_hangup(),
                k_purge(),
                k_send_break(),
                k_shell(),
                k_waitfor(),
                k_when(),
                k_transmit(),
                k_pause(),
                k_exit(),
                k_quit(),
                k_if(),
                k_goto(),
                k_else(),
                k_endif(),
                k_redial(),
                k_dial(),
                s_set(),
                k_tty(),
                k_capture();

/* globals */

int             linkflag = 0;

static struct kw kw[] =
{
  {"debug", k_debug},
  {"echo", k_echo},
  {"flush", k_flush},
  {"hangup", k_hangup},
  {"purge", k_purge},
  {"send_break", k_send_break},
  {"shell", k_shell},
  {"waitfor", k_waitfor},
  {"when", k_when},
  {"transmit", k_transmit},
  {"pause", k_pause},
  {"exit", k_exit},
  {"if", k_if},
  {"else", k_else},
  {"endif", k_endif},
  {"goto", k_goto},
  {"dial", k_dial},
  {"redial", k_redial},
  {"quit", k_quit},
  {"set", s_set},
  {"capture", k_capture},
  {"tty", k_tty},
  {NULL, NULL}};

Boolean
do_script(scriptFileName)
     String         scriptFileName;
{
  char           *scriptDir,
                  buf[REG_BUF];
  FILE           *scriptFP;

  if (qres.scriptDirectory) scriptDir = qres.scriptDirectory;
  else scriptDir = qres.defaultDirectory;
  
  strcpy(buf, scriptFileName);
  if ((scriptFP = open_file(buf, scriptDir)) == NULL)
    return False;
  
  exec_close_script(scriptFP);
  return True;
}

void
exec_close_script(script_fp)
     FILE           *script_fp;
{
  if_flag = 0;
  echo_flag = False;
  captflag = False;
  tty_flag = True;
  eof_flag = 0;

  if (linkflag == 2)
    linkflag = 0;

  while (!eof_flag)
    get_line(script_fp);

  fclose(script_fp);
  if (captflag)
    fclose(cf);

  eof_flag = 0;
  lptr = strcpy(line, "");
  k_when();

  linkflag = 0;

  return;
}

static char     wf[MAX_LINE];

void
get_line(script_fp)
     FILE           *script_fp;
{
  int             i;

  getline(script_fp);

  if (eof_flag)
    return;

  getword();

  if (echo_flag) {
    if (strcmp(word, "transmit"))
      show(line);
    else
      show("TRANSMIT...");
  }

  if (word[0] == '\0')	       /* Ignore blank lines */
    return;
  if (word[0] == '#')	       /* Ignore comments */
    return;

  if (word[strlen(word) - 1] == ':')	/* Ignore labels */
    return;

  if (if_flag == -1) {
    if (strcmp(word, "else") && strcmp(word, "endif"))
      return;
  }

  for (i = 0; kw[i].keyword != NULL; i++)
    if (strcmp(kw[i].keyword, word) == 0) {
      (*kw[i].rtn) (script_fp);
      return;
    }

  fprintf(tfp, "UNDEFINED COMMAND: \"%s\"\r\n", word);
  eof_flag = 1;
  return;
}

struct _when {
  String          expect;
  String          send;
  String          ptr;
};

struct _when    when[MAX_ENT] =
{
  {NULL, NULL, NULL}};

void
k_waitfor()
{
  long            t;
  char           *ptr = wf, c;
  struct _when   *whenPtr;


  GETTEST_ARG("waitfor");
  strcpy(wf, word);

  GET_ARG();

  for (whenPtr = when; whenPtr->expect; whenPtr++)
    whenPtr->ptr = whenPtr->expect;

  if (word[0])
    waitfor_time = atoi(word);
  else
    waitfor_time = 30;

  t = time(NULL) + waitfor_time;

  while (t != time(NULL) && !eof_flag) {
    if (MdmTimedReadChar(&c, 1) < 0)
      continue;

    if (tty_flag)
      fputc(c, tfp);

    if (captflag)
      fputc(c, cf);

    if ((char)c != *ptr)
      ptr = wf;
    else if (*++ptr == '\0') {
      waitflag = 1;
      return;
    }

    for (whenPtr = when; whenPtr->expect; whenPtr++) {
      if ((char)c != *(whenPtr->ptr))
		whenPtr->ptr = whenPtr->expect;
      else if (*++(whenPtr->ptr) == '\0')
		MdmPutString(whenPtr->send);
    }

  }

  waitflag = 0;
}

void
k_when()
{
  struct _when   *whenPtr;

  GET_ARG();
  if (word[0] == '\0') {
    for (whenPtr = when; whenPtr->expect; whenPtr++) {
      XtFree(whenPtr->expect);
      XtFree(whenPtr->send);
    }
    when[0].expect = NULL;
    return;
  }

  for (whenPtr = when; whenPtr->expect; whenPtr++);
  whenPtr->expect = XtNewString(word);
  (whenPtr + 1)->expect = NULL;

  GETTEST_ARG("when");
  whenPtr->send = XtNewString(word);
}

void
k_transmit()
{

  getword();
  if (eof_flag)
    return;

  if (word[0] == '\0') {
    fprintf(tfp, "No argument to TRANSMIT command\r\n");
    eof_flag = 1;
    return;
  }

  MdmPutString(word);
}

void
k_pause()
{
  int             pause_time;

  getword();
  if (eof_flag)
    return;

  if (word[0] == '\0')
    pause_time = 5;
  else
    pause_time = atoi(word);

  sleep(pause_time);
}

void
k_quit()
{
  write_child_info(child_pipe, EXIT_PROGRAM, "");
  k_exit();
}

void
k_exit()
{
  eof_flag = 1;
}

static char     label[WBSIZE];

void
k_goto(script_fp)
     FILE           *script_fp;
{
  int             found = 0,
                  i;

  getword();
  if (word[0] == '\0') {
    fprintf(tfp, "No argument for GOTO: %s\r\n", line);
    eof_flag++;
    return;
  }

  strcpy(label, word);

  rewind(script_fp);
  while (!found) {
    getline(script_fp);
    if (eof_flag)
      break;

    getword();
    if (word[0] == '\0' || word[0] == '#')
      continue;

    if (word[i = (strlen(word) - 1)] != ':')
      continue;

    word[i] = '\0';

    found = (strcmp(word, label) == 0);
  }

  if (eof_flag) {
    fprintf(tfp, "Label %s not found\r\n", label);
    eof_flag++;
    return;
  }

  if_flag = 0;		       /* reset IF flag */
}

static          if_negate = 0;

static int
if_test(cond)
     int             cond;
{
  if (if_negate)
    cond = !cond;

  if (cond)
    return 1;
  else
    return -1;
}

void
k_if()
{
  char           *ptr;

  if (if_flag) {
    fprintf(tfp, "Nested IF statements not allowed\r\n");
    eof_flag++;
    return;
  }

  if_negate = 0;
  getword();
  if (word[0] == '\0') {
    fprintf(tfp, "No condition on IF statement\r\n");
    eof_flag++;
    return;
  }

  if (strcmp(word, "not") == 0) {
    if_negate = 1;
    getword();
    if (word[0] == '\0') {
      fprintf(tfp, "No condition on IF statement\r\n");
      eof_flag++;
      return;
    }
  }

  if (word[0] == '!') {
    if_negate = 1;
    ptr = word + 1;
  }
  else
    ptr = word;

  if (strcmp(ptr, "waitfor") == 0) {
    if_flag = if_test(waitflag);
    return;
  }

  if (strcmp(ptr, "linked") == 0) {
    if_flag = if_test(linkflag);
    return;
  }

  fprintf(tfp, "Undefined IF condition %s\r\n", ptr);
  eof_flag++;
  return;
}

void
k_else()
{
  if (!if_flag) {
    fprintf(tfp, "ELSE not within IF\r\n");
    eof_flag++;
    return;
  }

  if_flag = -if_flag;
}

void
k_endif()
{
  if (!if_flag) {
    fprintf(tfp, "ENDIF not wihtin IF\r\n");
    eof_flag++;
    return;
  }

  if_flag = 0;
}

void
k_dial()
{
  getword();

  if (word[0] == '\0') {
    fprintf(tfp, "DIAL command must have an argument\r\n");
    eof_flag++;
    return;
  }

  dial(word);
}

void
k_redial()
{
  if (redial(NULL)) {
    eof_flag++;
    return;
  }
}

void
k_debug()
{
  set_onoff(&echo_flag);
  return;
}

void
k_echo()
{
  GET_ARG();
  show(word);
}

void
k_flush()
{
  MdmIFlush();
}

void
k_hangup()
{
  MdmHangup();
}

void
k_purge()
{
  MdmPurge();
}

void
k_send_break()
{
  send_break();
}

void
k_shell()
{
  GETTEST_ARG("shell");
  ExecShellCommand(word, 0);
}

void
k_capture()
{
  Boolean         val = captflag;

  set_onoff(&captflag);
  if (eof_flag)
    return;

  if (val == captflag)
    return;

  if (captflag == False)
    fclose(cf);
  else {
    if ((cf = fopen(captureFile, "a")) == NULL) {
      fprintf(tfp, "Cannot open capture file %s\r\n", captureFile);
      eof_flag++;
      return;
    }
  }
}

void
k_tty()
{
  set_onoff(&tty_flag);
  return;
}

/*
 * Dial a phone number, using proper format and delay.
 */

static char    *last_nbr = NULL;

void
dial(s)
     char           *s;
{
  if (last_nbr)
    XtFree(last_nbr);

  last_nbr = XtNewString(s);

  mprintf("\r%s %s%s", qres.dialPrefix, s, qres.dialSuffix);
}

int
redial(last_nbr)
     char           *last_nbr;
{
  char           *s;

  if (last_nbr == NULL) {
    show("REDIAL FAILURE");
    return 1;
  }

  s = XtNewString(last_nbr);
  dial(s);
  XtFree(s);
  return 0;
}
