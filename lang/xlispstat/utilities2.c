/* utilities2 - basic utility functions                                */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"

/* external functions */
extern LVAL list2(), list3(), xmsend(), xsapplysubr(), xscallsubrvec();

/* external variables */
extern char buf[];
  
/**************************************************************************/
/**                                                                      **/
/**                          Utility Functions                           **/
/**                                                                      **/
/**************************************************************************/

LVAL integer_list_2(a, b)
	int a, b;
{
  LVAL list, temp;
  
  xlstkcheck(2);
  xlsave(temp);
  xlsave(list);
  temp = cvfixnum((FIXTYPE) b); list = consa(temp);
  temp = cvfixnum((FIXTYPE) a); list = cons(temp, list);
  xlpopn(2);
  return(list);
}

LVAL integer_list_3(a, b, c)
	int a, b, c;
{
  LVAL list, temp;
  
  xlstkcheck(2);
  xlsave(temp);
  xlsave(list);
  temp = cvfixnum((FIXTYPE) c); list = consa(temp);
  temp = cvfixnum((FIXTYPE) b); list = cons(temp, list);
  temp = cvfixnum((FIXTYPE) a); list = cons(temp, list);
  xlpopn(2);
  return(list);
}

LVAL integer_list_4(a, b, c, d)
	int a, b, c, d;
{
  LVAL list, temp;
  
  xlstkcheck(2);
  xlsave(temp);
  xlsave(list);
  temp = cvfixnum((FIXTYPE) d); list = consa(temp);
  temp = cvfixnum((FIXTYPE) c); list = cons(temp, list);
  temp = cvfixnum((FIXTYPE) b); list = cons(temp, list);
  temp = cvfixnum((FIXTYPE) a); list = cons(temp, list);
  xlpopn(2);
  return(list);
}

LVAL send_message(object, msg)
     LVAL object, msg;
{
  LVAL argv[2];
  
  argv[0] = object;
  argv[1] = msg;
  return(xscallsubrvec(xmsend, 2, argv));
}

LVAL send_message1(object, msg, a)
	LVAL object, msg;
	int a;
{
  LVAL La, result, argv[3];
  
  xlsave(La);
  La = cvfixnum((FIXTYPE) a);
  argv[0] = object;
  argv[1] = msg;
  argv[2] = La;
  result = xscallsubrvec(xmsend, 3, argv);
  xlpop();
  return(result);
}

LVAL send_message_1L(object, symbol, value)
     LVAL object, symbol, value;
{
  LVAL argv[3];
  
  argv[0] = object;
  argv[1] = symbol;
  argv[2] = value;
  return(xscallsubrvec(xmsend, 3, argv));
}

LVAL apply_send(object, symbol, args)
     LVAL object, symbol, args;
{
  LVAL result;

  xlprot1(args);
  args = cons(symbol, args);
  args = cons(object, args);
  result = xsapplysubr(xmsend, args);
  xlpop();
  return(result);
}

LVAL double_list_2(a, b)
	double a, b;
{
  LVAL list, temp;
  
  xlstkcheck(2);
  xlsave(temp);
  xlsave(list);
  temp = cvflonum((FLOTYPE) b); list = consa(temp);
  temp = cvflonum((FLOTYPE) a); list = cons(temp, list);
  xlpopn(2);
  return(list);
}

/* make a LISP string from a C string */
LVAL make_string(s)
	char *s;
{
  LVAL result = newstring(strlen(s) + 1);
  strcpy(getstring(result), s);
  return(result);
}

LVAL xsnumtostring()
{
  LVAL x;
  
  x = xlgetarg();
  xllastarg();
  
  if (fixp(x)) sprintf(buf, "%ld", (long) getfixnum(x));
  else if (floatp(x)) sprintf(buf, "%g", (double) getflonum(x));
  else xlerror("not a number", x);
  
  return(make_string(buf));
}

LVAL xssysbeep()
{
  int count = 10;
  if (moreargs()) count = getfixnum(xlgafixnum());
  xllastarg();
  
  SysBeep(count);
  return(NIL);
}

