/* macdynload - Dynamic loading and C function calling routines.       */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

/* Calling conventions are based on the conventions given in the New S */
/* book.                                                               */

#ifdef MPWC
#include <Memory.h>
#include <Resources.h>
#else
#include <MenuMgr.h>
#include <ResourceMgr.h>
#endif MPWC

#include "xlisp.h"
#include "xlsx.h"

#define nil 0L

#define seqlen(x) ((vectorp(x)) ? getsize(x) : llength(x))

extern char buf[];
extern LVAL s_true;

extern LVAL newvector(), make_string(), mklist(), getnextelement();
extern double makedouble();

typedef void  (*pfv_t)();    /* pointer to function returning void. */

/************************************************************************/
/**                                                                    **/
/**                  Resource File Handling Functions                  **/
/**                                                                    **/
/************************************************************************/

LVAL xsopen_resfile()
{ 
  char *name;
  int fn;
  
  name = (char *) getstring(xlgastring());
  xllastarg();
  
  CtoPstr(name);
  fn = OpenResFile(name);
  PtoCstr(name);
  return((fn >= 0) ? cvfixnum((FIXTYPE) fn) : NIL);
}

LVAL xsclose_resfile()
{
  int fn;
  
  fn = getfixnum(xlgafixnum());
  xllastarg();
  
  CloseResFile(fn);
  return(NIL);
}

/************************************************************************/
/**                                                                    **/
/**               Allocation and Error Signalling Functions            **/
/**                                                                    **/
/************************************************************************/

static LVAL current_allocs;

/* allocate space that will be garbage collected after return */
static char *xscall_alloc(n, m)
     int n, m;
{
  LVAL adata;
  char *p;

  adata = newadata(n, m, FALSE);
  if (adata == NIL || (p = getadaddr(adata)) == nil)
    xlfail("allocation failed");
  current_allocs = cons(adata, current_allocs);
  return(p);
}

/* error routine for use within C functions */
static xscall_fail(s) char *s; { xlfail(s); }

/************************************************************************/
/**                                                                    **/
/**                Lisp to C/FORTRAN Data Conversion                   **/
/**                                                                    **/
/************************************************************************/

#define IN 0
#define RE 1

typedef struct {
  int type, size;
  char *addr;
} call_arg;

/* convert lisp argument to allocated pointer */
static call_arg lisp2arg(x)
     LVAL x;
{
  call_arg a;
  LVAL elem, data;
  int i;

  xlprot1(x);

  /* make sure x is a sequence and find its length */
  if (! sequencep(x)) x = consa(x);
  a.size = seqlen(x);

  /* determine the mode of the data */
  for (i = 0, a.type = IN, data = x; i < a.size; i++) {
    elem = getnextelement(&data, i);
    if (floatp(elem)) a.type = RE;
    else if (! fixp(elem)) xlerror("not a real number", elem);
  }

  /* allocate space for the data */
  a.addr = xscall_alloc(a.size, (a.type == IN) ? sizeof(int) : sizeof(double));

  /* fill the space */
  for (i = 0, data = x; i < a.size; i++) {
    elem = getnextelement(&data, i);
    if (a.type == IN) ((int *) a.addr)[i] = getfixnum(elem);
    else ((double *) a.addr)[i] = makedouble(elem);
  }
  
  xlpop();
  return(a);
}

/* copy allocated pointer back to new lisp list */
static LVAL arg2lisp(a)
     call_arg a;
{
  LVAL x, next;
  int i;

  xlsave1(x);
  x = mklist(a.size, NIL);
  for (i = 0, next = x; i < a.size; i++, next = cdr(next)) {
    if (a.type == IN) rplaca(next, cvfixnum((FIXTYPE) ((int *) a.addr)[i]));
    else rplaca(next, cvflonum((FLOTYPE) ((double *) a.addr)[i]));
  }
  xlpop();
  return(x);
}

/************************************************************************/
/**                                                                    **/
/**                 Foreign Function Call Function                     **/
/**                                                                    **/
/************************************************************************/

LVAL xscall_cfun()
{
  LVAL result, Lname, old_allocs, next;
  call_arg *args, *pargs;
  int nargs, i;
  Handle rhandle;
  void (*routine)();
  char *name;
  XLSXblock params;

  xlstkcheck(3);
  xlsave(old_allocs);
  xlprotect(current_allocs);
  xlsave(result);
  old_allocs = current_allocs;
  current_allocs = NIL;

  /* get the routine pointer */
  Lname = xlgastring();
  name = (char *) getstring(Lname);
  CtoPstr(name);
  rhandle = GetNamedResource('XLSX', name);
  PtoCstr(name);
  if (! rhandle) xlerror("can't load XLSX resource", Lname);

  /* convert the arguments to allocated pointers */
  nargs = xlargc;
  if (nargs == 0) xlfail("too few arguments");
  args = (call_arg *) xscall_alloc(nargs, sizeof(call_arg));
  params.argc = nargs;
  params.argv = (char **) xscall_alloc(nargs, sizeof(char *));
  for (i = 0; i < nargs; i++) {
    args[i] = lisp2arg(xlgetarg());
	params.argv[i] = args[i].addr;
  }

  /* make the call */
  HLock(rhandle);
  routine = (pfv_t) *rhandle;
  (*routine)(&params);
  HUnlock(rhandle);
  
  /* convert the pointers back to lists, grouped in a list */
  result = (nargs > 0) ? mklist(nargs, NIL) : NIL;
  for (next = result, pargs = args; consp(next); next = cdr(next), pargs++)
    rplaca(next, arg2lisp(*pargs));
  
  current_allocs = old_allocs;
  xlpopn(3);

  return(result);
}
