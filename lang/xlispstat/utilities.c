/* utilities - basic utility functions                                 */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"

# define numberp(x) (floatp(x) || fixp(x))
# define seqlen(x) ((vectorp(x)) ? getsize(x) : llength(x))

/* external variables */
extern LVAL s_list;

/* external function */
extern LVAL array_to_nested_list(), arraydata(), newarray();
extern LVAL xmaparray(), mklist(), xlapply(), xappend(), concatenate();

extern char *calloc();

/************************************************************************/
/**                           Basic Utilities                          **/
/************************************************************************/

/* find length of a list */
llength(x)
     LVAL x;
{
  int n;
  
  for (n = 0; consp(x); n++, x = cdr(x));

  return(n);
}

/* return list of two elements */
LVAL list2(x1, x2)
     LVAL x1, x2;
{
  LVAL list, y1, y2;
  
  /* protect some pointers */
  xlstkcheck(3);
  xlsave(list);
  xlsave(y1);
  xlsave(y2);
  
  y1 = x1;
  y2 = x2;
  list = consa(y2);
  list = cons(y1, list);
  
  /* restore the stack frame */
  xlpopn(3);
  
  return(list);
}

/* return list of three elements */
LVAL list3(x1, x2, x3)
     LVAL x1, x2, x3;
{
  LVAL list, y1, y2, y3;
  
  /* protect some pointers */
  xlstkcheck(4);
  xlsave(list);
  xlsave(y1);
  xlsave(y2);
  xlsave(y3);

  y1 = x1;
  y2 = x2;
  y3 = x3;
  list = consa(y3);
  list = cons(y2, list);
  list = cons(y1, list);
  
  /* restore the stack frame */
  xlpopn(4);
  
  return(list);
}

/* return the i-th argument, without popping it; signal an error if needed. */
LVAL peekarg(i)
     int i;
{
  if (xlargc <= i) xltoofew();
  else return(xlargv[i]);
}

/* Get the next argument from the list or the stack; cdr the list */
LVAL getnextarg(plist, from_stack)
     LVAL *plist;
     int from_stack;
{
  LVAL arg;
  if (from_stack) arg = xlgetarg();
  else if (consp(*plist)) {
    arg = car(*plist);
    *plist = cdr(*plist);
  }
  else
    xlfail("no arguments left");
  return(arg);
}

/* Get the next element in the sequence; cdr the pointer if it is a list */
LVAL getnextelement(pseq, i)
     LVAL *pseq;
     int i;
{
  LVAL value;

  if (vectorp(*pseq)) value = getelement(*pseq, i);
  else {
    if (! consp(*pseq)) xlerror("not a list", *pseq);
    value = car(*pseq);
    *pseq = cdr(*pseq);
  }
  return(value);
}

/* get and check a sequence argument */
LVAL xsgetsequence()
{
  LVAL arg;
  
  arg = xlgetarg();
  if (! sequencep(arg)) xlerror("not a sequence", arg);
  return(arg);
}

/* set a fixnum node */
setfixnum(node, val)
     LVAL node;
     FIXTYPE val;
{
  node->n_fixnum = val;
  node->n_type = FIXNUM;
}

/* Set the next element in the sequence; cdr the pointer if it is a list */
setnextelement(pseq, i, value)
     LVAL *pseq, value;
     int i;
{
  if (vectorp(*pseq)) setelement(*pseq, i, value);
  else {
    rplaca(*pseq, value);
    *pseq = cdr(*pseq);
  }
}

/* Check for a nonnegative integer */
LVAL checknonnegint(x)
     LVAL x;
{
  if (! fixp(x) || getfixnum(x) < 0) xlerror("Not a nonnegative integer", x);
  return(x);
}

/* return value of a number coerced to a double */
double makedouble(x)
     LVAL x;
{
  if (! numberp(x)) xlerror("not a number", x);
  return((fixp(x)) ? (double) getfixnum(x) : getflonum(x));
}

/************************************************************************/
/**                  Function Applicaiton Utilities                    **/
/************************************************************************/

pushargvec(fun, argc, argv)
     LVAL fun, *argv;
     int argc;
{
  LVAL *newfp;
  int i;

  /* build a new argument stack frame */
  newfp = xlsp;
  pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
  pusharg(fun);
  pusharg(cvfixnum((FIXTYPE)argc));

  /* push the arguments */
  for (i = 0; i < argc; i++)
    pusharg(argv[i]);

  /* establish the new stack frame */
  xlfp = newfp;
}
    
LVAL xsapplysubr(f, args)
     LVAL (*f)(), args;
{
  LVAL *oldargv, val;
  int argc, oldargc;
   
  xlprot1(args); /* protect arguments while pushing */
  argc = pushargs(NIL, args);
  xlpop();       /* now they are protected since they are on the stack */

  oldargc = xlargc;
  oldargv = xlargv;
  xlargc = argc;
  xlargv = xlfp + 3;
  val = (*f)();
  xlargc = oldargc;
  xlargv = oldargv;

  /* remove the call frame */
  xlsp = xlfp;
  xlfp = xlfp - (int)getfixnum(*xlfp);
  return(val);
}

LVAL xscallsubrvec(f, argc, argv)
     LVAL (*f)(), *argv;
     int argc;
{
  LVAL *oldargv, val;
  int oldargc;
   
  pushargvec(NIL, argc, argv);
  oldargc = xlargc;
  oldargv = xlargv;
  xlargc = argc;
  xlargv = xlfp + 3;
  val = (*f)();
  xlargc = oldargc;
  xlargv = oldargv;

  /* remove the call frame */
  xlsp = xlfp;
  xlfp = xlfp - (int)getfixnum(*xlfp);
  return(val);
}

LVAL xscallsubr1(f, x)
     LVAL (*f)(), x;
{
  return(xscallsubrvec(f, 1, &x));
}

LVAL xscallsubr2(f, x, y)
     LVAL (*f)(), x, y;
{
  LVAL args[2];

  args[0] = x;
  args[1] = y;
  return(xscallsubrvec(f, 2, args));
}

LVAL xsfuncall1(fun, x)
     LVAL fun, x;
{
  pushargvec(fun, 1, &x);
  return(xlapply(1));
}

LVAL xsfuncall2(fun, x, y)
     LVAL fun, x, y;
{
  LVAL args[2];
  
  args[0] = x;
  args[1] = y;
  pushargvec(fun, 2, args);
  return(xlapply(2));
}

#ifdef DODO
/************************************************************************/
/**                                                                    **/
/**               Temporary Storage Allocation Routines                **/
/**                                                                    **/
/************************************************************************/

char *xstcalloc(n, size) 
     int n, size;
{
  char *result;

  if ((result = calloc((unsigned) n, (unsigned) size)) == NULL) 
    xlfail("memory allocation failed");
  return(result);
}

xstfree(ptr) 
char *ptr;
{
  free(ptr);
}

/************************************************************************/
/**                                                                    **/
/**         Lisp to/from C/Fortran Data Conversion Routines            **/
/**                                                                    **/
/************************************************************************/
double *data_to_double(x)
     LVAL x;
{
  LVAL data, val;
  double *result;
  int n, rows, cols, i, j;

  if (matrixp(x)) n = getsize(arraydata(x));
  else if (sequencep(x)) n = seqlen(x);
  else xlerror("Bad data type", x);

  result = (double *) xstcalloc(n, sizeof(double));

  data = (sequencep(x)) ? x : arraydata(x);

  if (matrixp(x)) {
    rows = numrows(x);
    cols = numcols(x);
    for (i = 0; i < rows; i++)
      for (j = 0; j < cols; j++) {
	val = getelement(data, cols * i + j);
	if (! numberp(val)) {
	  free(result);
	  xlerror("element not a number", val);
	}
	result[i + rows * j] = (fixp(val)) ? getfixnum(val) : getflonum(val);
      }
  }
  else {
    for (i = 0; i < n; i++) {
      val = getnextelement(&x, i);
      if (! numberp(val)) {
	free(result);
	xlerror("element not a number", val);
      }
      result[i] = (fixp(val)) ? getfixnum(val) : getflonum(val);
    }
  }
  return(result);
}

LVAL double_to_matrix(x, n, k)
     double *x;
     int n, k;
{
  LVAL dim, nn, kk, val, result, result_data;
  int i, j;

  /* protect some pointers */
  xlstkcheck(5);
  xlsave(dim);
  xlsave(nn);
  xlsave(kk);
  xlsave(val);
  xlsave(result);
  
  nn = cvfixnum((FIXTYPE) n);
  kk = cvfixnum((FIXTYPE) k);
  dim = list2(nn, kk);
  result = newarray(dim, NIL, NIL);
  result_data = arraydata(result);

  for (i = 0; i < n; i++)
    for (j = 0; j < k; j++) {
      val = cvflonum((FLOTYPE) x[i + n * j]);
      setelement(result_data, k * i + j, val);
    }
  
  /* restore the stack frame */
  xlpopn(5);
  
  return(result);
}
  
LVAL double_to_sequence(x, n, list)
     double *x;
     int n, list;
{
  LVAL val, result, next;
  int i;

  /* protect some pointers */
  xlstkcheck(2);
  xlsave(val);
  xlsave(result);
  
  result = (list) ? mklist(n, NIL) : newvector(n);
  
  for (i = 0, next = result; i < n; i++) {
    val = cvflonum((FLOTYPE) x[i]);
    setnextelement(&next, i, val);
  }
  
  /* restore the stack frame */
  xlpopn(2);
  
  return(result);
}
#endif DODO
/***********************************************************************/
/**                     Sequence Coercion Functions                   **/
/***********************************************************************/
 
LVAL coerce_to_list(x)
     LVAL x;
{
  LVAL next, result;
  int n, i;
  
  /* save the result pointer */
  xlsave1(result);
  
  if (displacedarrayp(x))
    result = array_to_nested_list(x);
  else if (vectorp(x)) {
    n = getsize(x);
    result = mklist(n, NIL);
    for (i = 0, next = result; i < n; i++, next = cdr(next))
      rplaca(next, getelement(x, i));
  }
  else if (objectp(x))
    return(NIL); /* include standard coercion message later */
  else if (listp(x))
    result = x;
  else if (atom(x)) {
    result = consa(x);
  }
  else result = NIL;
  
  /* restore the stack frame */
  xlpop();
  
  return(result);
}

LVAL coerce_to_vector(x)
     LVAL x;
{
  LVAL next, result;
  int n, i;
  
  /* save the result pointer */
  xlsave1(result);
  
  if (displacedarrayp(x)) result = arraydata(x);
  else if (vectorp(x)) result = x;
  else if (objectp(x))
    return(NIL); /* include standard coercion message later */
  else if (listp(x)) {
    n = llength(x);
    result = newvector(n);
    for (i = 0, next = x; i < n; i++, next = cdr(next))
      setelement(result, i, car(next));
  }
  else if (atom(x)) {
    result = newvector(1);
    setelement(result, 0, x);
  }
  else result = NIL;
  
  /* restore the previous stack frame */
  xlpop();
  
  return(result);
}

/*************************************************************************/
/**                          Copying Functions                          **/
/*************************************************************************/

LVAL copylist(list)
     LVAL list;
{
  LVAL result, nextl, nextr;
  
  if (! listp(list)) xlerror("not a list", list);
  
  /* protect the result pointer */
  xlsave1(result);
  
  result = mklist(llength(list), NIL);
  for (nextl = list, nextr = result; consp(nextl);
       nextl = cdr(nextl), nextr = cdr(nextr)) {
    rplaca(nextr, car(nextl));
  }
  
  /* restore the stack frame */
  xlpop();
  
  return(result);
}

LVAL copyvector(v)
     LVAL v;
{
  LVAL result;
  int n, i;
  
  if (! vectorp(v)) xlerror("not a vector", v);
  
  /* protect the result pointer */
  xlsave1(result);
  
  n = getsize(v);
  result = newvector(n);
  for (i = 0; i < n; i++) {
    setelement(result, i, getelement(v, i));
  }
  
  /* restore the stack frame */
  xlpop();
  
  return(result);
}

/***************************************************************************/
/**                    Statistical Functions (sort of)                    **/
/***************************************************************************/

LVAL splitlist(list, len)
     LVAL list;
     int len;
{
  LVAL result, sublist, next_r, next_s, next;
  int numlists, n;
  
  if (len < 1) xlfail("invalid length for sublists");
  
  /* protect some pointers */
  xlsave1(result);
  
  n = llength(list);
  if ((n % len) != 0)
    xlfail("list not divisible by this length");
  else 
    numlists = n / len;
  
  result = mklist(numlists, NIL);
  for (next = list, next_r = result; consp(next_r); next_r = cdr(next_r)) {
    sublist = mklist(len, NIL);
    rplaca(next_r, sublist);
    for (next_s = sublist; consp(next_s); 
	 next_s = cdr(next_s), next = cdr(next))
      rplaca(next_s, car(next));
  }

  /* restore the stack frame */
  xlpop();
  
  return(result);
}

/* replicates a list n times */ 
LVAL lrepeat(arg, n)
     LVAL arg;
     int n;
{
  LVAL data, nextd, nextr, result;
  
  /* protect some pointers */
  xlstkcheck(2);
  xlsave(data);
  xlsave(result);
  
  data = coerce_to_list(arg);
  
  /* make new data list */
  result = mklist(n * llength(data), NIL);
  
  /* insert values from data into list */
  for (nextr = result, nextd = data; consp(nextr); 
       nextr = cdr(nextr), nextd = cdr(nextd)) {
    if (nextd == NIL) nextd = data; /* cycle through the data */
    rplaca(nextr, car(nextd));
  }

  /* restore the stack frame */
  xlpopn(2);
  
  return(result);
}

/* Flatten a nested list to depth rank */
LVAL nested_list_to_list(list, rank)
     LVAL list;
     int rank;
{
  LVAL result;
  int i;
  
  /* protect the result pointer */
  xlsave1(result);
  
  for (i = 1, result = list; i < rank; i++)
    result = concatenate(s_list, result);
  
  /* restore the previous stack frame */
  xlpop();
  
  return (result);
}

xsboolkey(key, dflt)
	LVAL key;
	int dflt;
{
  LVAL val;
  int result = dflt;
  
  if (xlgetkeyarg(key, &val)) result = ((val != NIL) ? TRUE : FALSE);
  return(result);
}
