/* xlpp.c - xlisp pretty printer */
/* Copyright (c) 1989, by David Michael Betz.                            */
/* You may give out copies of this software; for conditions see the file */
/* COPYING included with this distribution.                              */

#include "xlisp.h"

/* external variables */
extern LVAL s_stdout;
extern int xlfsize;

/* local variables */
static int pplevel,ppmargin,ppmaxlen;
static LVAL ppfile;

/* xpp - pretty-print an expression */
LVAL xpp()
{
    LVAL expr;

    /* get expression to print and file pointer */
    expr = xlgetarg();
    ppfile = (moreargs() ? xlgetfile() : getvalue(s_stdout));
    xllastarg();

    /* pretty print the expression */
    pplevel = ppmargin = 0; ppmaxlen = 40;
    pp(expr); ppterpri(); /* argument removed - L. Tierney */

    /* return nil */
    return (NIL);
}

/* pp - pretty print an expression */
LOCAL pp(expr)
  LVAL expr;
{
#ifndef XLISP_ONLY
/*************************************************************************/
/*         Lines below added to allow for common lisp arrays             */
/*         Luke Tierney, March 1, 1988                                   */
/*************************************************************************/

  if (displacedarrayp(expr)) {
    extern LVAL array_to_nested_list();
    LVAL value;

    /* protect a pointer */
    xlsave1(value);
  
    ppputc('#');
    value = cvfixnum((FIXTYPE) arrayrank(expr));
    ppexpr(value);
    ppputc('A');
    value = array_to_nested_list(expr);
    if (value ==NULL) {
      ppputc('(');
      ppputc(')');
  }
  else
    pplist(value);
  
  /* restore the stack frame */
  xlpop();
    return;
  }
	
/*************************************************************************/
/*        Lines above added to allow for common lisp arrays              */
/*        Luke Tierney, March 1, 1988                                    */
/*************************************************************************/
#endif /* XLISP_ONLY */
    if (consp(expr))
	pplist(expr);
    else
	ppexpr(expr);
}

/* pplist - pretty print a list */
LOCAL pplist(expr)
  LVAL expr;
{
    int n;

    /* if the expression will fit on one line, print it on one */
    if ((n = flatsize(expr)) < ppmaxlen) {
	xlprint(ppfile,expr,TRUE);
	pplevel += n;
    }

    /* otherwise print it on several lines */
    else {
	n = ppmargin;
	ppputc('(');
	if (atom(car(expr))) {
	    ppexpr(car(expr));
	    ppputc(' ');
	    ppmargin = pplevel;
	    expr = cdr(expr);
	}
	else
	    ppmargin = pplevel;
	for (; consp(expr); expr = cdr(expr)) {
	    pp(car(expr));
	    if (consp(cdr(expr)))
		ppterpri();
	}
	if (expr != NIL) {
	    ppputc(' '); ppputc('.'); ppputc(' ');
	    ppexpr(expr);
	}
	ppputc(')');
	ppmargin = n;
    }
}

/* ppexpr - print an expression and update the indent level */
LOCAL ppexpr(expr)
  LVAL expr;
{
    xlprint(ppfile,expr,TRUE);
    pplevel += flatsize(expr);
}

/* ppputc - output a character and update the indent level */
LOCAL ppputc(ch)
  char ch;  /* changed from int to char - L. Tierney */
{
    xlputc(ppfile,ch);
    pplevel++;
}

/* ppterpri - terminate the print line and indent */
LOCAL ppterpri()
{
    xlterpri(ppfile);
    for (pplevel = 0; pplevel < ppmargin; pplevel++)
	xlputc(ppfile,' ');
}

/* flatsize - compute the flat size of an expression */
LOCAL int flatsize(expr)
  LVAL expr;
{
    xlfsize = 0;
    xlprint(NIL,expr,TRUE);
    return (xlfsize);
}
