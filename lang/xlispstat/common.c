/* common - Additional Common Lisp functions not yet included in       */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "xlisp.h"
#include "complex.h"

/* external variables */
extern char buf[];
extern LVAL s_stdout, obarray, s_unbound;
extern LVAL s_ielement, s_true, s_ivalue, s_list, s_vector, a_float, a_string;
extern LVAL a_cons, a_flonum, a_complex, s_features, s_and, s_or, s_not;
extern LVAL s_function_documentation, s_variable_documentation, s_ielement;
extern LVAL xlenv, xlfenv, xldenv;
extern int xlfsize;

/* external functions */
extern LVAL makearglist(), xerror(), xbreak();
extern LVAL xmember(), xapply(), xload(), xeval(), xreverse();
extern long ftell();
extern LVAL copylist(), copyvector(), coerce_to_list(), coerce_to_vector(),
  xscopyvector(), peekarg(), xeql(), xequal(), list3(), list2(), xsfuncall2();
extern LVAL recursive_subr_map_elements(), xsapplysubr(), xscallsubr2();
extern double makedouble();
extern unsigned long time(), ticks_per_second(), run_tick_count(), real_tick_count();

/* forward declarations */
extern LVAL xsconcatenate();

/****************************************************************************/
/****************************************************************************/
/**                                                                        **/
/**                         Common Lisp Functions                          **/
/**                                                                        **/
/****************************************************************************/
/****************************************************************************/

/****************************************************************************/
/**                       APROPOS and APROPOS-LIST                         **/
/****************************************************************************/

/* internal version of APROPOS */
static LVAL apropos(print)
     int print;
{
  LVAL str, array, list, element, result;
  unsigned char *s1, *s2;
  int i, n;
  
  str = xlgetarg();
  xllastarg();
  
  if (stringp(str)) s1 = getstring(str);
  else if (symbolp(str)) s1 = getstring(getpname(str));
  else xlbadtype(str);
  
  array = getvalue(obarray);
  n = getsize(array);
  
  /* protect some pointers */
  xlsave1(result);
  
  result = NIL;
  for (i = 0; i < n; i++) {
    list = getelement(array, i);
    if (listp(list)) {
      for (; consp(list); list = cdr(list)) {
        element = car(list);
        if (symbolp(element) && element != s_unbound) {
          s2 = getstring(getpname(element));
          if (is_sub_str(s1, s2) == 0) {
            if (print) stdprint(element);
            else result = cons(element, result);
          }
        }
      }
    }
  }
  /* restore the stack and return the result */
  xlpop();
  
  return(result);
}

/* Common Lisp APROPOS function */
LVAL xsapropos() { return(apropos(TRUE)); }

/* Common Lisp APROPOS-LIST function */
LVAL xsaproposlist() { return(apropos(FALSE)); }

/* check if s1 is a substring of s2; case insensitive */
static is_sub_str(s1, s2)
     char *s1, *s2;
{
  int n, m, i;

  m = strlen(s1);
  n = strlen(s2) - m;
  
  for (i = 0; i <= n; i++)
    if (strck(s1, &s2[i], m)) return (0);
  return(1);
}

/* check if s1 and s2 agree up to character m; case insensitive */
static strck(s1, s2, m)
     char *s1, *s2;
     int m;
{
  char ch1, ch2;

  while (m-- > 0) {
    ch1 = isupper(*s1) ? tolower(*s1) : *s1;
    ch2 = isupper(*s2) ? tolower(*s2) : *s2;
    if (ch1 != ch2) return(0);
    s1++;
    s2++;
  }
  return(1);
}

/*************************************************************************/
/**         IDENTITY, MAKE-LIST, ADJOIN and FILE-POSITION functions     **/
/*************************************************************************/

/* Common Lisp IDENTITY function */
LVAL xsidentity()
{
  LVAL x;
  
  x = xlgetarg();
  xllastarg();
  return(x);
}

/* Internal version of MAKE-LIST */
LVAL mklist(n, elem)
     int n;
     LVAL elem;
{
  LVAL result = NIL, next;
  
  xlsave1(result);
  while (n-- > 0) 
    result = cons(NIL, result);
  if (elem != NIL) 
    for (next = result; consp(next); next = cdr(next))
      rplaca(next, elem);
  xlpop();
  return(result);
}

/* Common Lisp MAKE-LIST function */
LVAL xsmklist()
{
  int n;
  LVAL elem = NIL;
  
  n = getfixnum(xlgafixnum());
  xlgetkeyarg(s_ielement, &elem);
  
  return(mklist(n, elem));
}

/* Common Lisp ADJOIN function */
LVAL xsadjoin()
{
  LVAL x, list; 
  if (xlargc < 2) xltoofew();
  x = xlargv[0];
  list = xlargv[1];
  if (xmember()) return(list);
  else return(cons(x, list));
}

/* Common Lisp FILE-POSITION function */
LVAL xsfileposition()
{
  LVAL fptr;
  long pos;
  
  /* get file pointer */
  fptr = xlgetfile();
  
  /* make sure the file exists */
  if (getfile(fptr) == NULL) xlfail("file not open");
  
  if (moreargs()) {
    pos = getfixnum(xlgafixnum());
    xllastarg();
    fseek(getfile(fptr), pos, 0);
    return(s_true);
  }
  else {
    return(cvfixnum((FIXTYPE) ftell(getfile(fptr))));
  }
}

/****************************************************************************/
/**                   Common Lisp FORMAT function                          **/
/**         (Includes integer and floating point formatting)               **/
/****************************************************************************/

/* getstroutput - get the output stream string (internal) */
static LVAL getstroutput(stream)
  LVAL stream;
{
  unsigned char *str;
  LVAL next,val;
  int len,ch;

  /* compute the length of the stream */
  for (len = 0, next = gethead(stream); next != NIL; next = cdr(next))
    ++len;
  if (len - 1 > STRMAX) xlfail("string is too long");

  /* create a new string */
  val = newstring(len + 1);
    
  /* copy the characters into the new string */
  str = getstring(val);
  while ((ch = xlgetc(stream)) != EOF)
    *str++ = ch;
  *str = '\0';

  /* return the string */
  return (val);
}

/* parser for format directives (internal) */
static get_format_data(fmt, dir, nargs, fargs)
     char **fmt, *dir;
     int *nargs, *fargs;
{
  (*fmt)++;
  *nargs = 0;
  while ((isdigit(**fmt) || **fmt == ',' || **fmt == 'v' || **fmt == 'V')
	 && *nargs < 2) {
#ifdef AMIGA
    *dir = **fmt; /* dummy line needed for bug in Lattice 5.05 JKL */
#endif AMIGA
    if (isdigit(**fmt)) {
      (*nargs)++;
      sscanf(*fmt, "%d", fargs++);
      while (isdigit(**fmt))
	(*fmt)++;
      if (**fmt == ',') (*fmt)++;
    }
    else if ( **fmt == ',') {
      (*fmt)++;
      (*nargs)++;
      *fargs++ = -1;
    }
    else {
      (*fmt)++;
      (*nargs)++;
      *fargs++ = getfixnum(xlgafixnum());
      if (**fmt == ',') (*fmt)++;
    }
  }
  *dir = **fmt;
}

LVAL xsformat()
{
  LVAL stream, val, arg;
  unsigned char *fmt, dir, contr[50];
  int nargs, fargs[2], i;
  
  xlsave1(stream);

  /* get the stream and format string */
  stream = xlgetarg();
  if (stream == NIL)
    val = stream = newustream();
  else {
    if (stream == s_true)
      stream = getvalue(s_stdout);
    else if (streamp(stream)) {
	  if (getfile(stream) == NULL)
	    xlfail("file not open");
	}
	else if (!ustreamp(stream))
      xlbadtype(stream);
    val = NIL;
  }
  fmt = getstring(xlgastring());
    
  for (; *fmt != '\0'; fmt++) {
    if (*fmt != '~') xlputc(stream, *fmt);
    else {
      get_format_data(&fmt, &dir, &nargs, fargs);
      switch (dir) {
      case 'A':
      case 'a':
	xlfsize = 0;
	xlprint(stream, xlgetarg(), FALSE);
	if (nargs > 0)
	  for (; xlfsize < fargs[0]; /*xlfsize++*/) xlputc(stream, ' ');
	break;
      case 'S':
      case 's':
	xlfsize = 0;
	xlprint(stream, xlgetarg(), TRUE);
	if (nargs > 0)
	  for (; xlfsize < fargs[0]; /*xlfsize++*/) xlputc(stream, ' ');
	break;
      case 'D':
      case 'd':
	arg = xlgetarg();
	if (fixp(arg)) {
	  if (nargs == 0) {
	    sprintf(buf, "%ld", (long) getfixnum(arg));
	    xlputstr(stream, buf);
	  }
	  else {
	    sprintf(contr, "%%%dld", fargs[0]);
	    sprintf(buf, contr, (long) getfixnum(arg));
	    xlputstr(stream, buf);
	  }
	}
	else
	  xlprint(stream, arg, FALSE);
	break;
      case 'E':
      case 'e':
      case 'F':
      case 'f':
      case 'G':
      case 'g':
	dir = (isupper(dir)) ? tolower(dir) : dir;
	arg = xlgetarg();
	if (floatp(arg)) {
	  switch (nargs) {
	  case 0:
	    sprintf(contr, "%%l%c", dir);
	    break;
	  case 1:
	    sprintf(contr, "%%%dl%c", fargs[0], dir);
	    break;
	  case 2:
	    if (fargs[0] > 0)
	      sprintf(contr, "%%%d.%dl%c", fargs[0], fargs[1], dir);
	    else
	      sprintf(contr, "%%.%dl%c", fargs[1], dir);
	    break;
	  default:
	    xlfail("too many arguments for format directive");
	  }
	  sprintf(buf, contr, (double) getflonum(arg));
	  xlputstr(stream, buf);
	}
	else 		    
	  xlprint(stream, arg, FALSE);
	break;
      case '~':
	if (nargs == 0) xlputc(stream, '~');
	else if (nargs == 1) {
	  if (fargs[0] < 0) fargs[0] = 0;
	  for (i = 0; i < fargs[0]; i++)
	    xlputc(stream, '~');
	}
	break;
      case '%':
	if (nargs == 0) xlterpri(stream);
	else if (nargs == 1) {
	  if (fargs[0] < 0) fargs[0] = 0;
	  for (i = 0; i < fargs[0]; i++)
	    xlterpri(stream);
	}
	break;
      case '\n':
        while (isspace(*fmt)) fmt++;
        fmt--; /* set back because fmt will be advanced in the outer loop */
        break;
      default:
	xlfail("unknown format directive");
      }
    }
  }
  if (val != NIL) val = getstroutput(val);
  xlpop();

  /* return the value */
  return (val);
}

LVAL xsforce_output()
{
#ifdef MACINTOSH
  TtyFlush();
#else
  fflush(stdout);
#endif MACINTOSH
  return(NIL);
}

/****************************************************************************/
/**                     Sequence Copying Functions                         **/
/****************************************************************************/

/* Common Lisp COPY-LIST function */
LVAL xscopylist()
{
  LVAL list;
  
  list = xlgalist();
  xllastarg();
  
  return(copylist(list));
}

/* Common Lisp COPY-SEQ function */
LVAL xscopyseq()
{
  if (! moreargs()) xltoofew();
  return((vectorp(xlargv[0])) ? xscopyvector() : xscopylist());
}

/***********************************************************************/
/**                     REDUCE and MAP functions                      **/
/***********************************************************************/

/* Common Lisp REDUCE function (internal version) */
LVAL reduce(fcn, sequence, has_init, initial_value)
     LVAL fcn, sequence, initial_value;
     int has_init;
{
  LVAL next, result;
  int i, n;
  
  /* protect some pointers */
  xlstkcheck(3);
  xlsave(next);
  xlsave(result);
  xlprotect(fcn);
  
  if (consp(sequence)) {
    next = sequence;
    if (has_init) result = initial_value;
    else {
      result = car(next);
      next = cdr(next);
    }
    for (; consp(next); next = cdr(next)) 
      result = xsfuncall2(fcn, result, car(next));
  }
  else if (vectorp(sequence)) {
    n = getsize(sequence);
    i = 0;
    if (has_init) result = initial_value;
    else {
      result = getelement(sequence, 0);
      i = 1;
    }
    for (; i < n; i++) 
      result = xsfuncall2(fcn, result, getelement(sequence, i));
  }
  else badseq(sequence);
  
  /* restore the stack frame */
  xlpopn(3);

  return(result);
}

/* Common Lisp REDUCE function */
LVAL xsreduce()
{
  LVAL fcn, sequence, initial_value;
  int has_init;
  
  fcn = xlgetarg();
  sequence = xlgetarg();
  has_init = xlgetkeyarg(s_ivalue, &initial_value);
  
  return(reduce(fcn, sequence, has_init, initial_value));
}

#define seqlen(x) ((vectorp(x)) ? getsize(x) : llength(x))
#define makeresult(type, n) ((type == s_vector) ? newvector(n) : mklist(n, NIL))

/* compute the length of the result sequence */
LOCAL findrlen(args)
     LVAL args;
{
  LVAL next;
  int len, rlen;

  for (rlen = -1, next = args; consp(next); next = cdr(next)) {
    if (! sequencep(car(next))) badseq(car(next));
    len = seqlen(car(next));
    if (rlen == -1) rlen = len;
    else rlen = (len < rlen) ? len : rlen;
  }
  return(rlen);
}

LOCAL pushnextargs(fcn, n, args, i)
     LVAL fcn, args;
     int n, i;
{
  LVAL *newfp, next, value;

  /* build a new argument stack frame */
  newfp = xlsp;
  pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
  pusharg(fcn);
  pusharg(cvfixnum((FIXTYPE)n));
  
  /* push the arguments and shift the list pointers */
  for (next = args; consp(next); next = cdr(next)) {
    if (vectorp(car(next))) value = getelement(car(next), i);
    else {
      value = car(car(next));
      rplaca(next, cdr(car(next)));
    }
    pusharg(value);
  }

  /* establish the new stack frame */
  xlfp = newfp;
}
  
/* Internal version of Common Lisp MAP function */
LVAL map(type, fcn, args, rlen)
     LVAL type, fcn, args;
     int rlen;
{
  LVAL nextr, result;
  int nargs, i;

  /* protect some pointers */
  xlstkcheck(2);
  xlsave(result);
  xlprotect(fcn);
 
  if (rlen < 0) rlen = findrlen(args); 
  result = makeresult(type, rlen);
  nargs = llength(args);

  for (i = 0, nextr = result; i < rlen; i++) {
    pushnextargs(fcn, nargs, args, i);
    setnextelement(&nextr, i, xlapply(nargs));
  }

  /* restore the stack frame */
  xlpopn(2);
  
  return(result);
}

/* Common Lisp MAP function */
LVAL xsmap()
{
  LVAL fcn, type, args, result;
  
  /* protect some pointers */
  xlstkcheck(2);
  xlsave(fcn);
  xlsave(args);

  type = xlgasymbol();
  fcn = xlgetarg();
  if (type != s_list && type != s_vector)
    xlerror("Not a valid sequence type", type);
  args = makearglist(xlargc, xlargv);

  result = map(type, fcn, args, -1);

  /* restore the stack frame */
  xlpopn(2);
  return(result);
}

/***********************************************************************/
/**                       ELT and COERCE                              **/
/***********************************************************************/

/* Common Lisp ELT function */
LVAL xselt()
{
  LVAL x, index;
  int i;
  
  x = xlgetarg();
  index = xlgafixnum();
  xllastarg();

  i = getfixnum(index);
  if (simplevectorp(x)) {
    if (i < 0 || i >= getsize(x)) badindex(index);
    else return(getelement(x, i));
  }
  else if (listp(x)) {
    if (i < 0) badindex(index);
    else
      for (; consp(x) && i > 0; x = cdr(x), i--)
	;
    if (! consp(x)) badindex(index);
    else return(car(x));
  }
  else if (stringp(x)) {
    if (i < 0 || i >= strlen(getstring(x))) badindex(index);
    else return(cvchar(getstring(x)[i]));
  }
  else badseq(x);
}

/* Setf method for Common Lisp ELT function */
set_elt(x, index, val)
	LVAL x, index, val;
{
  int i;
  
  i = getfixnum(index);
  if (simplevectorp(x)) {
    if (i < 0 || i >= getsize(x)) badindex(index);
    else setelement(x, i, val);
  }
  else if (listp(x)) {
    if (i < 0) badindex(index);
    else
      for (; consp(x) && i > 0; x = cdr(x), i--)
	;
    if (! consp(x)) badindex(index);
    else rplaca(x, val);
  }
  else if (stringp(x)) {
    if (! charp(val)) xlerror("not a character", val);
    if (i < 0 || i >= strlen(getstring(x))) badindex(index);
    getstring(x)[i] = getchcode(val);
  }
  else badseq(x);
}

/* Common Lisp COERCE function */
LVAL xscoerce()
{
  LVAL x, result, type;
  
  x = xlgetarg();
  type = xlgasymbol();
  xllastarg();
  
  /* protect the result pointer */
  xlsave1(result);
  
  if (stringp(x) || type == a_string) {
    if (stringp(x) && type == a_string) result = x;
    else result = xscallsubr2(xsconcatenate, type, x);
  }
  else if (type == a_flonum || type == a_float) {
    if (fixp(x)) result = cvflonum((FLOTYPE) getfixnum(x));
    else if (floatp(x)) result = x;
    else xlfail("can't coerce this object to this type");
  }
  else if (type == a_complex) {
    if (fixp(x)) result = newicomplex(getfixnum(x), (FIXTYPE) 0);
    else if (floatp(x)) result = newdcomplex(getflonum(x), (FLOTYPE) 0.0);
    else if (complexp(x)) result = x;
    else xlfail("can't coerce this object to this type");
  }    
  else if (type == a_cons || type == s_list) 
    result = coerce_to_list(x);
  else if (type == s_vector) 
    result = coerce_to_vector(x);
  else if (type == s_true)
    result = x;
  else
    xlfail("can't coerce this object to this type");
  
  /* restore the stack frame */
  xlpop();
  
  return(result);
}

/***********************************************************************/
/**                                                                   **/
/**                Modified APPLY and EVAL functions                  **/
/**                                                                   **/
/***********************************************************************/

/* Common Lisp APPLY function. Modified to cons arguments onto last one */
LVAL xsapply()
{
  LVAL *newfp, last;
  int argc;

  /* build a new argument stack frame */
  newfp = xlsp;
  pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
  pusharg(xlgetarg());
  pusharg(NIL); /* will be argc */
  
  /* push all but the last argument */
  last = xlgetarg();
  for (argc = 0; moreargs(); argc++) {
    pusharg(last);
    last = nextarg();
  }

  /* push all elements of the last argument */
  if (! listp(last)) xlerror("not a list", last);
  for (; consp(last); last = cdr(last), argc++) 
    pusharg(car(last));

  /* establish the new stack frame */
  newfp[2] = cvfixnum((FIXTYPE)argc);
  xlfp = newfp;

  /* apply the function to the arguments */
  return (xlapply(argc));
}

/* Common Lisp EVAL function. Modified to use null lexical environment */
LVAL xseval()
{
  LVAL expr, result, oldenv, oldfenv;

  /* set the lexical environment to null */
  xlstkcheck(3);
  xlsave(oldenv);
  xlsave(oldfenv);
  xlsave(expr);
  oldenv = xlenv; xlenv = NIL;
  oldfenv = xlfenv; xlfenv = NIL;
  
  /* get the expression to evaluate */
  expr = xlgetarg();
  xllastarg();

  /* evaluate the expression */
  result = xleval(expr);
  
  /* reset the environment */
  xlenv = oldenv;
  xlfenv = oldfenv;
  xlpopn(3);
  
  return(result);
}

/* Common Lisp LOAD function. Modified to use null lexical environment */

LVAL xsload()
{
  LVAL result, oldenv, oldfenv;

  /* set the lexical environment to null */
  xlstkcheck(2);
  xlsave(oldenv);
  xlsave(oldfenv);
  oldenv = xlenv; xlenv = NIL;
  oldfenv = xlfenv; xlfenv = NIL;
  
  /* evaluate the expression */
  result = xload();
  
  /* reset the environment */
  xlenv = oldenv;
  xlfenv = oldfenv;
  xlpopn(2);
  
  return(result);
}

/***********************************************************************/
/**                                                                   **/
/**                     Complex Number Functions                      **/
/**                                                                   **/
/***********************************************************************/

LVAL xsgetreal()
{
  LVAL arg = xlgetarg();
  if (! numberp(arg)) xlerror("not a real number", arg);
  return(arg);
}

LVAL newicomplex(real, imag)
	FIXTYPE real, imag;
{
  LVAL val;
  
  if (imag == 0) val = cvfixnum(real);
  else {
    xlsave1(val);
    val = newvector(2);
    val->n_type = COMPLEX;
    setelement(val, 0, cvfixnum(real));
    setelement(val, 1, cvfixnum(imag));
    xlpop();
  }
  return(val);
}

LVAL newdcomplex(real, imag)
	double real, imag;
{
  LVAL val;
  
  xlsave1(val);
  val = newvector(2);
  val->n_type = COMPLEX;
  setelement(val, 0, cvflonum((FLOTYPE) real));
  setelement(val, 1, cvflonum((FLOTYPE) imag));
  xlpop();
  return(val);
}

/* newcomplex - allocate and initialize a new object */
LVAL newcomplex(real,imag)
  LVAL real,imag;
{
  if (fixp(real) && fixp(imag))
    return(newicomplex(getfixnum(real), getfixnum(imag)));
  else
    return(newdcomplex(makedouble(real), makedouble(imag)));
}

LVAL xscomplexp()
{
  LVAL arg = xlgetarg();
  xllastarg();
  
  return((complexp(arg)) ? s_true : NIL);
}

LVAL realpart(x)
	LVAL x;
{
  if (! complexp(x)) badcomplex(x);
  return(getelement(x, 0));
}

LVAL imagpart(x)
	LVAL x;
{
  if (! complexp(x)) badcomplex(x);
  return(getelement(x, 1));
}

static LVAL xscomplex()
{
  LVAL real, imag;
  
  real = xsgetreal();
  if (! moreargs()) return(real);
  else {
    imag = xsgetreal();
    return(newcomplex(real, imag));
  }
}

static LVAL xsconjugate()
{
  LVAL arg = xlgetarg();
  
  xllastarg();
  if (numberp(arg)) return(arg);
  if (fixp(realpart(arg)) && fixp(imagpart(arg)))
    return(newicomplex(getfixnum(realpart(arg)), -getfixnum(imagpart(arg))));
  else
    return(newdcomplex(makedouble(realpart(arg)), -makedouble(imagpart(arg))));
}

static LVAL xsrealpart()
{
  LVAL arg = xlgetarg();
  
  xllastarg();
  if (fixp(arg) || floatp(arg)) return(arg);
  else return(realpart(arg));
}

static LVAL xsimagpart()
{
  LVAL arg = xlgetarg();
  
  xllastarg();
  if (fixp(arg)) return(cvfixnum((FIXTYPE) 0));
  else if (floatp(arg)) return(cvflonum((FLOTYPE) 0.0));
  else return(imagpart(arg));
}

LVAL xsrcomplex()   { return (recursive_subr_map_elements(xscomplex, xsrcomplex));     }
LVAL xsrconjugate() { return (recursive_subr_map_elements(xsconjugate, xsrconjugate)); }
LVAL xsrrealpart()  { return (recursive_subr_map_elements(xsrealpart, xsrrealpart));   }
LVAL xsrimagpart()  { return (recursive_subr_map_elements(xsimagpart, xsrimagpart));   }

static LVAL equaltest(use_eql)
  int use_eql;
{
  LVAL arg1, arg2, r1, r2, i1, i2;
  
  if (! complexp(peekarg(0))) return((use_eql) ? xeql() : xequal());
  else {
    arg1 = xlgetarg();
    arg2 = xlgetarg();
    xllastarg();
    
    if (! complexp(arg2)) return(NIL);
    
    r1 = realpart(arg1);
    i1 = imagpart(arg1);
    r2 = realpart(arg2);
    i2 = imagpart(arg2);
    
    if (fixp(r1) && fixp(i1) && fixp(r2) && fixp(i2)
        && getfixnum(r1) == getfixnum(r2)
        && getfixnum(i1) == getfixnum(i2))
      return(s_true);
    else if (floatp(r1) && floatp(i1) && floatp(r2) && floatp(i2)
        && getflonum(r1) == getflonum(r2)
        && getflonum(i1) == getflonum(i2))
      return(s_true);
    else return(NIL);
  }
}

LVAL xseql()   { return(equaltest(TRUE));  }
LVAL xsequal() { return(equaltest(FALSE)); }

/***********************************************************************/
/**                                                                   **/
/**            Global Variable Declaration Function                   **/
/**                                                                   **/
/***********************************************************************/

defconstant(sym, val)
	LVAL sym, val;
{
  setvalue(sym, val);
  setconstant(sym, TRUE);
}

LVAL defsym(which)
	int which;
{
  LVAL sym, val, doc;
  
  sym = xlgasymbol();
  if (which == 'C' && isconstant(sym)) xlfail("can't assign to a constant");
  switch (which) {
  case 'C':
  case 'P': val = xlgetarg(); break;
  case 'V': val = (moreargs()) ? xlgetarg() : NIL; break;
  }
  doc = (moreargs()) ? xlgastring() : NIL;
  xllastarg();
  
  switch (which) {
  case 'C': defconstant(sym, xleval(val)); break;
  case 'P': setvalue(sym, xleval(val));    break;
  case 'V': if (getvalue(sym) == s_unbound) setvalue(sym, xleval(val)); break;
  }
  if (doc != NIL)
    set_variable_docstring(sym, doc);
  return(sym);
}

LVAL xsdefconstant()  { return(defsym('C')); }
LVAL xsdefparameter() { return(defsym('P')); }
LVAL xsdefvar()       { return(defsym('V')); }

LVAL xsmakunbound()
{
  LVAL sym;
  
  sym = xlgasymbol();
  xllastarg();
  
  setvalue(sym, s_unbound);
  setconstant(sym, FALSE);
  return(sym);
}

LVAL xsfmakunbound()
{
  LVAL sym;
  
  sym = xlgasymbol();
  xllastarg();
  
  setfunction(sym,s_unbound);
  return(sym);
}

keep_doc_strings()
{
  return(getvalue(xlenter("*KEEP-DOCUMENTATION-STRINGS*")) != NIL);
}

set_function_docstring(sym, str)
	LVAL sym, str;
{
  if (keep_doc_strings())
    xlputprop(sym, str, s_function_documentation);
}

set_variable_docstring(sym, str)
	LVAL sym, str;
{
  if (keep_doc_strings())
    xlputprop(sym, str, s_variable_documentation);
}

/***********************************************************************/
/**                                                                   **/
/**                  Features Maintenance Functions                   **/
/**                                                                   **/
/***********************************************************************/

static is_member(x, list)
	LVAL x, list;
{
  int result = FALSE;
  
  for (; ! result && consp(list); list = cdr(list))
    if (equal(x, car(list))) result = TRUE;
  return(result);
}

checkfeatures(arg, which)
	LVAL arg;
	int which;
{
  int has_feature;
  LVAL features = getvalue(s_features);
  
  if (consp(arg)) {
    if (car(arg) == s_and)
      for (has_feature = TRUE, arg = cdr(arg);
           consp(arg) && has_feature;
           arg = cdr(arg)) {
        has_feature = has_feature && checkfeatures(car(arg), which);
      }
    else if (car(arg) == s_or)
      for (has_feature = FALSE, arg = cdr(arg);
           consp(arg) && ! has_feature;
           arg = cdr(arg)) {
        has_feature = has_feature || checkfeatures(car(arg), which);
      }
    else if (car(arg) == s_not && consp(cdr(arg)))
      has_feature = ! checkfeatures(car(cdr(arg)), which);
    else xlerror("bad feature", arg);
  }
  else has_feature = is_member(arg, features);
  
  if (which == '-') has_feature = ! has_feature;
  return(has_feature);
}

/***********************************************************************/
/**                                                                   **/
/**                  Time and Environment Functions                   **/
/**                                                                   **/
/***********************************************************************/

LVAL xstime()
{
  LVAL result;
  unsigned long tm;
  double dtm;
  
  tm = run_tick_count();
  result = xeval();
  tm = run_tick_count() - tm;
  dtm = (tm > 0) ? tm : -tm;
  
  sprintf(buf, "The evaluation took %.2f seconds\n", dtm / ticks_per_second());
  stdputstr(buf);
  return(result);
}

LVAL xs_get_internal_run_time() { return(cvfixnum((FIXTYPE) run_tick_count())); }

LVAL xs_get_internal_real_time() { return(cvfixnum((FIXTYPE) real_tick_count())); }

LVAL xsgetenv()
{
  xllastarg();
  return(list3(xlenv, xlfenv, xldenv));
}

/***********************************************************************/
/**                                                                   **/
/**                       Concatenate Function                        **/
/**                                                                   **/
/***********************************************************************/

static sequence_length(x)
	LVAL x;
{
  if (simplevectorp(x)) return(getsize(x));
  else if (listp(x)) return(llength(x));
  else if (stringp(x)) return(strlen(getstring(x)));
  else badseq(x);
}

static reslength(x)
	LVAL x;
{
  int n;
  
  for (n = 0; consp(x); x = cdr(x)) n += sequence_length(car(x));
  return(n);
}

static LVAL makeseq(type, n)
	LVAL type;
	int n;
{
  LVAL result;
  int i;
  
  if (type == s_list) result = mklist(n, NIL);
  else if (type == s_vector) result = newvector(n);
  else if (type == a_string) {
    result = newstring(n + 1);
    for (i = 0; i < n; i++) getstring(result)[i] = ' ';
    getstring(result)[n] = '\0';
  }
  else xlerror("bad sequence type", type);
  return(result);
}

static LVAL next_elt(x, i)
	LVAL *x;
	int i;
{
  LVAL result;
  
  if (consp(*x)) {
    result = car(*x);
    *x = cdr(*x);
  }
  else if (simplevectorp(*x)) result = getelement(*x, i);
  else if (stringp(*x)) result = cvchar(getstring(*x)[i]);
  else badseq(*x);
  
  return(result);
}

static set_next_elt(x, i, val)
	LVAL *x, val;
	int i;
{
  if (consp(*x)) {
    rplaca(*x, val);
    *x = cdr(*x);
  }
  else if (simplevectorp(*x)) setelement(*x, i, val);
  else if (stringp(*x)) {
    if (! charp(val)) xlerror("not a character", val);
    getstring(*x)[i] = getchcode(val);
  }
  else badseq(*x);
}

LVAL concatenate(type, sequences)
	LVAL type, sequences;
{
  LVAL result, next, seq, x;
  int i, j, n, m;
  
  xlstkcheck(3);
  xlprotect(sequences);
  xlsave(result);
  xlsave(x);
  
  n = reslength(sequences);
  result = makeseq(type, n);
  next = result;
  
  for (i = 0; consp(sequences); sequences = cdr(sequences)) {
    seq = car(sequences);
    m = sequence_length(seq);
    for (j = 0; j < m; i++, j++) {
      x = next_elt(&seq, j);
      set_next_elt(&next, i, x);
    }
  }

  xlpopn(3);
  return(result);
}

LVAL xsconcatenate()
{
  LVAL type, sequences, result;
  
  xlsave1(sequences);
  type = xlgasymbol();
  sequences = makearglist(xlargc, xlargv);
  result = concatenate(type, sequences);
  xlpop();
  return(result);
}

/***********************************************************************/
/**                                                                   **/
/**                      SOME and EVERY Functions                     **/
/**                                                                   **/
/***********************************************************************/

static LVAL every_some(which, negate)
	int which, negate;
{
  LVAL fcn, args, result;
  int nargs, i, rlen;

  /* protect some pointers */
  xlstkcheck(2);
  xlsave(args);
  xlsave(fcn);
 
  fcn = xlgetarg();
  args = makearglist(xlargc, xlargv);
  rlen = findrlen(args);
  nargs = llength(args);

  result = NIL;
  for (i = 0; i < rlen; i++) {
    pushnextargs(fcn, nargs, args, i);
    result = xlapply(nargs);
    if (which == 'A') {
      if (result != NIL) break;
    }
    else if (result == NIL) break;
  }
  if (negate) result = (result == NIL) ? s_true : NIL;
  
  /* restore the stack frame */
  xlpopn(2);
  
  return(result);
}

LVAL xssome()     { return(every_some('A', FALSE)); }
LVAL xsevery()    { return(every_some('E', FALSE)); }
LVAL xsnotany()   { return(every_some('A', TRUE));  }
LVAL xsnotevery() { return(every_some('E', TRUE));  }

/***********************************************************************/
/**                                                                   **/
/**                          Set Functions                            **/
/**                                                                   **/
/***********************************************************************/

/* apply MEMBER function, allowing for extra arguments. args should    */
/* contain two postitons for x and list arguments, and should be       */
/* protected from garbage collection before the call.                  */
static apply_member(x, list, args)
	LVAL x, list, args;
{
  LVAL result;
  
  if (consp(args) && consp(cdr(args))) {
    rplaca(args, x);
    rplaca(cdr(args), list);
  }
  result = xsapplysubr(xmember, args);
  return(result != NIL ? TRUE : FALSE);
}

static LVAL set_op(which)
	int which;
{
  LVAL x, list1, list2, rest, result;
  
  xlstkcheck(2);
  xlsave(rest);
  xlsave(result);
  
  list1 = xlgalist();
  list2 = xlgalist();
  rest = makearglist(xlargc, xlargv);
  rest = cons(NIL, rest);
  rest = cons(NIL, rest);
  
  switch(which) {
  case 'U':
    for (result = list1; consp(list2); list2 = cdr(list2)) {
      x = car(list2);
      if (! apply_member(x, list1, rest)) result = cons(x, result);
    }
    break;
  case 'I':
    for (result = NIL; consp(list2); list2 = cdr(list2)) {
      x = car(list2);
      if (apply_member(x, list1, rest)) result = cons(x, result);
    }
    break;
  case 'D':
    for (result = NIL; consp(list1); list1 = cdr(list1)) {
      x = car(list1);
      if (! apply_member(x, list2, rest)) result = cons(x, result);
    }
    break;
  case 'S':
    for (result = s_true; consp(list1); list1 = cdr(list1)) {
      x = car(list1);
      if (!apply_member(x, list2, rest)) {
        result = NIL;
        break;
      }
    }
    break;
  }
  
  xlpopn(2);
  return(result);
}

LVAL xsunion()          { return(set_op('U')); }
LVAL xsintersection()   { return(set_op('I')); }
LVAL xsset_difference() { return(set_op('D')); }
LVAL xssubsetp()        { return(set_op('S')); }

LVAL xsremove_duplicates()
{
  LVAL x, list, rest, result, tail;
  
  xlstkcheck(2);
  xlsave(rest);
  xlsave(result);
  
  list = xlgalist();
  rest = makearglist(xlargc, xlargv);
  rest = cons(NIL, rest);
  rest = cons(NIL, rest);
  
  for (result = NIL; consp(list); list = cdr(list)) {
    x = car(list);
    if (! apply_member(x, result, rest)) {
      if (result == NIL) {
        result = consa(x);
        tail = result;
      }
      else {
        rplacd(tail, consa(x));
        tail = cdr(tail);
      }
    }
  }
  xlpopn(2);
  return(result);
}
  
LVAL xsbutlast()
{
  LVAL x, next;
  int n, k;
  
  xlsave1(x);
  x = xlgalist();
  k = (moreargs()) ? getfixnum(xlgafixnum()) : 1;
  xllastarg();
  
  x = copylist(x);
  n = llength(x) - k;
  if (n <= 0) x = NIL;
  else if (k >= 1) {
    for (next = x; consp(next) && consp(cdr(next)) && n > 1; next = cdr(next), n--);
    rplacd(next, NIL);
  }
  
  xlpop();
  return(x);
}

static badseq(s)
	LVAL s;
{ 
  xlerror("not a sequence", s);
}

static badindex(i)
	LVAL i;
{
  xlerror("not a valid index", i);
}

static badcomplex(c)
	LVAL c;
{
  xlerror("not a valid complex number", c);
}

LVAL xsmake_string()
{
  int n, i;
  char c = ' ';
  LVAL arg, result;
  
  n = getfixnum(xlgafixnum());
  if (xlgetkeyarg(s_ielement, &arg)) {
    if (! charp(arg)) xlerror("not a character", arg);
    c = getchcode(arg);
  }
  result = newstring(n + 1);
  for (i = 0; i < n; i++)
    getstring(result)[i] = c;
  getstring(result)[n] = '\0';
  return(result);
}

/***********************************************************************/
/**                                                                   **/
/**                         FIND and POSITION                         **/
/**                                                                   **/
/***********************************************************************/

LVAL xsfind()
{
  LVAL result = xmember();
  return(consp(result) ? car(result) : NIL);
}

LVAL xsposition()
{
  int n;
  LVAL result;
  
  if (xlargc < 2 || ! listp(peekarg(1))) xlfail("bad arguments");
  n = llength(peekarg(1));
  result = xmember();
  n -= consp(result) ? llength(result) : 0;
  return(consp(result) ? cvfixnum((FIXTYPE) n) : NIL);
}

/***********************************************************************/
/**                                                                   **/
/**                         ERROR and BREAK                           **/
/**                                                                   **/
/***********************************************************************/

LVAL xserror()
{
  LVAL result;
  
  xlprot1(result);
  result = makearglist(xlargc, xlargv);
  result = cons(NIL, result);
  result = xsapplysubr(xsformat, result);
  result = consa(result);
  result = xsapplysubr(xerror, result);
  xlpop();
  
  return(result);
}

LVAL xsbreak()
{
  LVAL result;
  
  if (moreargs()) {
    xlprot1(result);
    result = makearglist(xlargc, xlargv);
    result = cons(NIL, result);
    result = xsapplysubr(xsformat, result);
    result = consa(result);
    result = xsapplysubr(xbreak, result);
    xlpop();
  }
  else result = xbreak();
  return(result);
}
