/* compound - Compound data implementation and Elementwise mapping     */
/* functions.                                                          */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "xlisp.h"

/* external variables */
extern LVAL s_true, s_vector, s_list, s_compound_data_proto;
extern LVAL sk_data_length, sk_data_seq, sk_make_data;

/* external functions */
extern LVAL coerce_to_list(), coerce_to_vector(), getarraydata(), map();
extern LVAL arraydata(), makedisplacedarray(), displacedarraydim();
extern LVAL makearglist(), xfuncall(), send_message(), list3(), xmsend();
extern LVAL send_message_1L();

/*************************************************************************/
/*************************************************************************/
/**                                                                     **/
/**                    Compound Data Implementation                     **/
/**                                                                     **/
/*************************************************************************/
/*************************************************************************/

/* Compound data items contain a data sequence and structural            */
/* information. The sequence can be extracted, the natural type of the   */
/* sequence can be determined, the length of the sequence can be         */
/* determined and a sequence of the appropriate length can be coerced to */
/* match the shape of an object.                                         */
/*                                                                       */
/* For the moment, x is compound if it is a cons or an array of positive */
/* size, or an object iheriting from COMPOUND-DATA-PROTO.                */
/*                                                                       */
/* If x is compound and y is a sequence then makecompound(x, seq) will   */
/* return a compound item of the same shape as x with data sequence seq. */
/* for sequences, same shape means same length. For arrays it means      */
/* equal dimensions. For objects it means whatever x thinks it means.    */

/* internal predicate */
compoundp(x) 
     LVAL x;
{
  if (consp(x)) return(TRUE);
  else if (arrayp(x) && getsize(arraydata(x)) > 0) return(TRUE);
  else if (objectp(x)) return(kind_of_p(x, getvalue(s_compound_data_proto)));
  else return(FALSE);
}

/* Built in COMPOUNDP */
LVAL xscompoundp()
{
  LVAL x;

  x = xlgetarg();
  xllastarg();
  return((compoundp(x)) ? s_true : NIL);
}

/* Check for a compound data item; pass it through or signal an error */
LVAL checkcompound(x)
     LVAL x;
{
  if (! compoundp(x)) xlerror("not a compound data item", x);
  return(x);
}

/* find length of a compound item's data sequence */
compounddatalen(x)
     LVAL x;
{
  if (objectp(x)) {
    LVAL n = send_message(x, sk_data_length);
    if (! fixp(n) || getfixnum(n) < 0) xlerror("bad length", n);
    return((int) getfixnum(n));
  }
  return((arrayp(x)) ? getsize(arraydata(x)) : llength(x));
}

/* Built in COMPOUND-DATA-LENGTH */
LVAL xscompound_length()
{
  LVAL x;
  
  x = checkcompound(xlgetarg());
  xllastarg();
  return(cvfixnum((FIXTYPE) compounddatalen(x)));
}

/* get compound item's data sequence */
LVAL compounddataseq(x) 
     LVAL x;
{
  if (objectp(x)) {
    LVAL seq = send_message(x, sk_data_seq);
    if (! sequencep(seq)) xlerror("not a sequence", seq);
    return(seq);
  }
  return((listp(x)) ? (x) : arraydata(x));
}

/* Built in COMPOUND-DATA-SEQ */
LVAL xscompound_seq()
{
  LVAL x;
  
  x = checkcompound(xlgetarg());
  xllastarg();
  return(compounddataseq(x));
}

/* get 'natural' type of of compound item's data sequence */
#define compoundseqtype(x) (vectorp(x)) ? s_vector : s_list;

/* Make sequence into a compound item of the same shape as form */
LVAL makecompound(form, seq)
     LVAL form, seq;
{
  LVAL result;

  xlsave1(result);
  if (listp(form))
    result = coerce_to_list(seq);
  else if (simplevectorp(form))
    result = coerce_to_vector(seq);
  else if (displacedarrayp(form)) {
    result = coerce_to_vector(seq);
    result = makedisplacedarray(displacedarraydim(form), result);
  }
  else if (objectp(form)) {
    result = send_message_1L(form, sk_make_data, seq);
  }
  else xlerror("not a compound data item", form);

  xlpop();
  return(result);
}

/*************************************************************************/
/*************************************************************************/
/**                                                                     **/
/**                 Element-Wise Mapping Functions                      **/
/**                                                                     **/
/*************************************************************************/
/*************************************************************************/

/* MAP-ELEMENTS acts like FUNCALL if all arguments are simple (i. e. not */
/* compound). If one is compound all should be of the same shape. In     */
/* this case simple arguments are treates as constant compound items of  */
/* the appropriate shape. The function is applied elementwise and the    */
/* result is returned as a compound item of the same shape as its        */
/* arguments (in particular its first compound argument). If the         */
/* arguments are sequences the result is a sequence of the same type as  */
/* the first sequence argument.                                          */

/* Check the stack for a compound data argument and return it or NIL     */
LOCAL LVAL findcompound(skip_one)
     int skip_one;
{
  LVAL *next;
  int n;
  
  n = xlargc;
  next = xlargv;
  
  if (skip_one) {
    n--;
    next++;
  }

  for (; n > 0; n--, next++) 
    if (compoundp(*next))
      return(*next);
  return(NIL);
}

/* find the length of the result sequence for map for the arguments in args */
LOCAL findrlen(args)
     LVAL args;
{
  LVAL next;
  int len, rlen;

  for (rlen = -1, next = args; consp(next); next = cdr(next))
    if (compoundp(car(next))) {
      len = compounddatalen(car(next));
      if (rlen < 0) rlen = len;
      else if (len != rlen) xlfail("arguments not all the same length");
    }
  return(rlen);
}

/* replace displaced array arguments by their data vectors and simple */
/* arguments by circular lists of one element.                        */
LOCAL fixuparglist(list)
     LVAL list;
{
  LVAL next;
  for (next = list; consp(next); next = cdr(next))
    if (! compoundp(car(next))) { 
      /* make circular list */
      rplaca(next, consa(car(next)));
      rplacd(car(next), car(next));
    }
    else
      rplaca(next, compounddataseq(car(next)));
}

/* MAP-ELEMENTS for internal subroutines */
LVAL subr_map_elements(f)
     LVAL (*f)();
{
  LVAL arglist, result, fcn, first_compound, type;
  int rlen;

  first_compound = findcompound(FALSE);

  if (first_compound == NIL) result = (*f)();
  else {
    xlstkcheck(3);
    xlsave(arglist);
    xlsave(fcn);
    xlsave(result);
    fcn = cvsubr(f, SUBR, 0);
    type = compoundseqtype(first_compound);
    arglist = makearglist(xlargc, xlargv);
    rlen = findrlen(arglist);
    fixuparglist(arglist);
    result = map(type, fcn, arglist, rlen);
    result = makecompound(first_compound, result);
    xlpopn(3);
  }
  return(result);
}

/* recursive MAP-ELEMENTS for internal subroutines */
LVAL recursive_subr_map_elements(bf, f)
     LVAL (*bf)(), (*f)();
{
  if (findcompound(FALSE) == NIL) return((*bf)());
  else return(subr_map_elements(f));
}

/* Built in MAP-ELEMENTS */
LVAL xsmap_elements()
{
  LVAL arglist, result, fcn, first_compound, type;
  int rlen;

  if (xlargc < 2) xltoofew();
  first_compound = findcompound(TRUE);

  if (first_compound == NIL) result = xfuncall();
  else {
    xlstkcheck(2)
    xlsave(arglist);
    xlsave(result);
    fcn = xlgetarg();
    type = compoundseqtype(first_compound);
    arglist = makearglist(xlargc, xlargv);
    rlen = findrlen(arglist);
    fixuparglist(arglist);
    result = map(type, fcn, arglist, rlen);
    result = makecompound(first_compound,result);
    xlpopn(2);
  }
  return(result);
}
