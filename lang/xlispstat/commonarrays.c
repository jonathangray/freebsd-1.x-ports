/* commonarrays - Implementation of Common Lisp multi-dimensional      */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "xmath.h"
#include "xlisp.h"

/* external variables */
extern LVAL s_ielement, s_icontents, s_displacedto, s_arrayident;
extern LVAL s_subarray, s_true, s_printcase, k_downcase;

/* external functions */
extern LVAL evmatch(), evarg();
extern LVAL coerce_to_list(), coerce_to_vector(), mklist(), splitlist(),
  getnextarg(), getnextelement(), checknonnegint(), nested_list_to_list();

/* Forward declarations */
FORWARD LVAL arraydata(), displacedarraydim(), getdim();

/***************************************************************************/
/**                                                                       **/
/**                          Utility Functions                            **/
/**                                                                       **/
/***************************************************************************/
/* Compute the rank of an array with dimensions given by list or vector dim */
LOCAL rankfordim(dim) 
     LVAL dim;
{
  if (listp(dim)) return(llength(dim));
  else if (vectorp(dim)) return(getsize(dim));
  else xlerror("bad dimension specifier", dim);
}

/* Compute the size of an array with dimensions given by list or vector dim */
LOCAL sizefordim(dim)
     LVAL dim;
{
  int rank, size, i;

  if (vectorp(dim)) rank = getsize(dim);
  if (dim == NIL || (vectorp(dim) && rank == 0)) size = 1;
  else 
    for (size = 1, i = 0; consp(dim) || (vectorp(dim) && i < rank); i++)
      size *= getfixnum(checknonnegint(getnextelement(&dim, i)));
  return(size);
}	

/* get an array from the argument list */
LVAL xsgetarray()
{
  LVAL arg;
  arg = xlgetarg();
  if (! checkarrayp(arg)) xlerror("not an array", arg);
  else return(arg);
}

/* get and check a displaced array argument */
LVAL xsgetdisplacedarray()
{
  LVAL arg;
    arg = xsgetarray();
  if (! displacedarrayp(arg)) xlerror("not a displaced array", arg);
  return(arg);
}

/***************************************************************************/
/***************************************************************************/
/****                                                                   ****/
/****                      Internal Representation                      ****/
/****                                                                   ****/
/***************************************************************************/
/***************************************************************************/

/* Multidimensional arrays are implemented as displaced arrays.        */
/* Internally they are represented as a vector of  three components.   */
/* The first component is an identifying symbol, s_arrayident. The     */
/* second is the dimension vector and the third is the data vector.    */

/***************************************************************************/
/**                                                                       **/
/**                            Basic Predicates                           **/
/**                                                                       **/
/***************************************************************************/

/* A displaced array is any vector of length 3 whose first component is    */
/* eq to the symbol s_arrayident. Does not check for consistence of dims.  */
displacedarrayp(x)
     LVAL x;
{
/*  return (vectorp(x) && getsize(x) == 3 && getelement(x,0) == s_arrayident);*/
  return((x) && ntype(x) == DISPLACED_ARRAY);
}

simplevectorp(x)
     LVAL x;
{
  return(vectorp(x) && ! displacedarrayp(x)); 
}

/* check for consistency of dims in a displaced array. Return TRUE for a  */
/* simple vector, false for a non array.                                  */
checkdims(x)
     LVAL x;
{
  if (displacedarrayp(x))
    return(sizefordim(displacedarraydim(x)) == getsize(arraydata(x)));
  else if vectorp(x) return(TRUE);
  else return(FALSE);
}

/* check for an array; do not check dimensions */
arrayp(x)
     LVAL x;
{
  return(displacedarrayp(x) || vectorp(x));
}

/* check for an array; check dimensions if displaced */
checkarrayp(x)
     LVAL x;
{
  return((displacedarrayp(x) && checkdims(x)) || vectorp(x));
}

/* check if a subscript sequence is in array bounds */
inboundsp(x, indices, from_stack)
     LVAL x, indices;
     int from_stack;
{
  LVAL index;
  int i, rank;
  
  if (simplevectorp(x)) {
    index = getnextarg(&indices, from_stack);
    xllastarg();
    return(fixp(index) && getfixnum(index) >= 0 && 
	   getfixnum(index) < getsize(x));
  }
  else if (displacedarrayp(x)) {
    rank = arrayrank(x);
    for (i = 0; i < rank; i++) {
      index = getnextarg(&indices, from_stack);
      if (! fixp(index) || getfixnum(index) < 0
	  || getfixnum(index) >= getfixnum(getdim(x, i)))
	return(FALSE);
    }
    xllastarg();
    return(TRUE);
  }
  else xlerror("not an array", x);
}

/***************************************************************************/
/**                                                                       **/
/**                            Basic Selectors                            **/
/**                                                                       **/
/***************************************************************************/

/* Return x if x is a simple vector or the vector x is displaced to if x   */
/* is a displaced array.                                                   */
LVAL arraydata(x)
     LVAL x;
{
  if (simplevectorp(x)) return(x);
  else if (displacedarrayp(x)) return(getelement(x,2));
  else xlerror("not an array", x);
}

/* Return the dimension vector of a displaced array. */
LVAL displacedarraydim(x)
     LVAL x;
{
  if (displacedarrayp(x)) return(getelement(x,1));
  else xlerror("not a displaced array", x);
}

/* Size of dimension d; no error checking */
static LVAL getdim(x, d)
	LVAL x;
	int d;
{
	return(getelement(getelement(x,1), d));
}

/* Rank of x; no error checking */
arrayrank(x)
	LVAL x;
{
	return((displacedarrayp(x)) ? getsize(getelement(x,1)) : 1);
}

/***************************************************************************/
/**                                                                       **/
/**                            Basic Constructor                          **/
/**                                                                       **/
/***************************************************************************/

/* Form an array representation from dim sequence and data vector */
/* Both arguments should be protected from garbage collection     */
LVAL makedisplacedarray(dim, data)
     LVAL dim, data;
{
  LVAL dimvector, result;
  int rank, size;

  rank = rankfordim(dim);

  /* Check dim and data for consistency */
  size = sizefordim(dim);
  if (! vectorp(data)) xlerror("bad data argument", data);
  if (size != getsize(data)) xlfail("dimensions do not match data length");

  if (rank == 1) {
    result = data;
  }
  else {
    /* protect some pointers */
    xlstkcheck(2);
    xlsave(dimvector);
    xlsave(result);

    dimvector = coerce_to_vector(dim);

    result = newvector(3);
    result->n_type = DISPLACED_ARRAY;
    setelement(result, 0, s_arrayident);
    setelement(result, 1, dimvector);
    setelement(result, 2, data);
    
    xlpopn(2);
  }
  return(result);
}

/***************************************************************************/
/***************************************************************************/
/****                                                                   ****/
/****                     Implementation Independent Part               ****/
/****                                                                   ****/
/***************************************************************************/
/***************************************************************************/

/***************************************************************************/
/**                                                                       **/
/**                              Predicates                               **/
/**                                                                       **/
/***************************************************************************/

/* Common Lisp ARRAYP function */
LVAL xsarrayp()
{
  LVAL x;
  
  x = xlgetarg();
  xllastarg();
  
  return((checkarrayp(x)) ? s_true : NIL);
}

/****************************************************************************/
/**                                                                        **/
/**                              Selectors                                 **/
/**                                                                        **/
/****************************************************************************/

/* Get array size */
static getarraysize(x)
	LVAL x;
{
	return(getsize(arraydata(x)));
}

/* Common Lisp ARRAY-DIMENSIONS function */
LVAL xsarraydimensions()
{
  LVAL x;
  LVAL result;
  
  x = xsgetarray();
  xllastarg();
  
  xlsave1(result);
  if (simplevectorp(x)) {
    result = cvfixnum((FIXTYPE) getsize(x));
    result = consa(result);
  }
  else
    result = coerce_to_list(displacedarraydim(x));
  xlpop();
  return(result);
}

/* Common Lisp ARRAY-RANK function */
LVAL xsarrayrank()
{
  LVAL x;
  
  x = xsgetarray();
  xllastarg();
  
  if (simplevectorp(x)) 
    return(cvfixnum((FIXTYPE) 1));
  else 
    return(cvfixnum((FIXTYPE) arrayrank(x)));
}

/* Common Lisp ARRAY-TOTAL-SIZE function */
LVAL xsarraytotalsize()
{
  LVAL x;
  
  x = xsgetarray();
  xllastarg();
  
  return(cvfixnum((FIXTYPE) getarraysize(x)));
}

/* Common Lisp ARRAY-DIMENSION function */
LVAL xsarraydimension()
{
  LVAL x, i;

  x = xsgetarray();
  i = checknonnegint(xlgafixnum());
  xllastarg();

  if (getfixnum(i) >= arrayrank(x)) xlerror("dimension exceeds rank", i);
  else if (simplevectorp(x)) return(cvfixnum((FIXTYPE) getsize(x)));
  else return(getdim(x, (int) getfixnum(i)));
}

/* Common Lisp ARRAY-IN-BOUNDS-P function */
LVAL xsarrayinboundsp()
{
  return((inboundsp(xsgetarray(), NIL, TRUE)) ? s_true : NIL);
}

/* Compute row major index from indices list or array or from stack args */
rowmajorindex(x, indices, from_stack)
     LVAL x, indices;
     int from_stack;
{
  LVAL dim, index;
  int rank, k, fsize, i;
  
  if (simplevectorp(x)) {
    index = checknonnegint(getnextarg(&indices, from_stack));
    if (getfixnum(index) >= getsize(x))
      xlerror("index out of range", index);
    return(getfixnum(index));
  }
  else if (displacedarrayp(x)) {
    
    dim = displacedarraydim(x);
    
    rank = arrayrank(x);
    for (i = 0, k = 0; i < rank; i++) {
      index = checknonnegint(getnextarg(&indices, from_stack));
      fsize = getfixnum(getelement(dim, i));
      if (getfixnum(index) < 0
	  || getfixnum(index) >= getfixnum(getdim(x, i)))
	xlerror("index out of range", index);
      k = fsize * k + getfixnum(index);
    }
    return(k);
  }
  else xlerror("not an array", x);
}

/* Common Lisp ARRAY-ROW-MAJOR-INDEX function */
LVAL xsarrayrowmajorindex()
{
  LVAL x;
  
  x = xlgetarg();
  
  return(cvfixnum((FIXTYPE) rowmajorindex(x, NIL, TRUE)));
}

/* Common Lisp AREF function */
LVAL xsaref()
{
  LVAL x;

  x = xsgetarray();

  return (getelement(arraydata(x), rowmajorindex(x, NIL, TRUE)));
}


/****************************************************************************/
/**                                                                        **/
/**                            Constructors                                **/
/**                                                                        **/
/****************************************************************************/

/* Make a new array of dimension dim with contents specified by the keyword */
/* argument.                                                                 */
LVAL newarray(dim, key, key_arg)
     LVAL dim, key, key_arg;
{
  LVAL data, contents, result;
  int rank, size, i;
    
  /* protect some pointers */
  xlstkcheck(3);
  xlsave(data);
  xlsave(contents);
  xlsave(result);
  
  /* make the array data vector */
  if (key == NIL) 
    data = newvector(sizefordim(dim));
  else if (key == s_ielement) {
    size = sizefordim(dim);
    data = newvector(size);
    for (i = 0; i < size; i++)
      setelement(data, i, key_arg);
  }
  else if (key == s_icontents) {
    rank = rankfordim(dim);
    size = sizefordim(dim);
    contents = nested_list_to_list(key_arg, rank);
    if (llength(contents) != size)
      xlerror("initial contents does not match dimensions", key_arg);
    data = newvector(size);
    for (i = 0; consp(contents); i++, contents = cdr(contents))
      setelement(data, i, car(contents));
  }
  else if (key == s_displacedto)
    data = arraydata(key_arg);
  else
    xlerror("bad keyword", key);

  result = makedisplacedarray(dim, data);
  
  /* restore the stack frame */
  xlpopn(3);
  
  return (result);
}

/* convert nested list to array - used by read macro. Determines dimension */
/* from first list element, without checking others, then calls newarray.  */
LVAL nested_list_to_array(list, rank)
     LVAL list;
     int rank;
{
  LVAL next, dim, data, result;
  int i;
  
  /* protect some pointers */
  xlstkcheck(2);
  xlsave(dim);
  xlsave(result);
  
  dim = mklist(rank, NIL);
  for (i = 0, data = list, next = dim; i < rank; i++, next = cdr(next)) {
    rplaca(next, cvfixnum((FIXTYPE) llength(data)));
    if ((i < rank) && (! consp(data)))
      xlerror("data does not match rank", list);
    data = car(data);
  }
  
  result = newarray(dim, s_icontents, list);
  
  /* restore the stack frame */
  xlpopn(2);
  
  return (result);
}

/* Common Lisp MAKE-ARRAY function. Allows one of the keywords */
/* :INITIAL-ELEMENT, :INITIAL-CONTENTS, or :DISPLACED-TO       */
LVAL xsmakearray()
{
  LVAL dim, key = NIL, key_arg = NIL, result;
  
  /* protect some pointes */
  xlstkcheck(2);
  xlsave(dim);
  xlsave(result);
  
  dim = xlgetarg();
  if (xlgetkeyarg(s_ielement, &key_arg)) key = s_ielement;
  else if (xlgetkeyarg(s_icontents, &key_arg)) key = s_icontents;
  else if (xlgetkeyarg(s_displacedto, &key_arg)) key = s_displacedto;
  
  if (fixp(dim)) dim = consa(dim);
  if (! listp(dim)) xlerror("bad dimension argument", dim);
  
  result = newarray(dim, key, key_arg);
  
  /* restore the stack frame */
  xlpopn(2);
  
  return (result);
}

/*************************************************************************/
/**                                                                     **/
/**                            Mutators                                 **/
/**                                                                     **/
/*************************************************************************/

/* setf function for aref */
evsetarrayelement(place, value)
     LVAL place, value;
{
  LVAL args, next, x, rest;
  
  /* protect args pointer */
  xlsave1(args);

  args = mklist(llength(place), NIL);
/*  rplaca(args, evmatch(VECTOR,&place));*/
  rplaca(args, evarg(&place));
  if (! arrayp(car(args))) xlerror("not an array", car(args));
  for (next = cdr(args); consp(next); next = cdr(next)) {
    rplaca(next, evmatch(FIXNUM,&place));
  }

  x = car(args);
  rest = cdr(args);
  if (checkarrayp(x)) {
    setelement(arraydata(x), rowmajorindex(x, rest, FALSE), value);
  }
  else
    xlerror("not an array", x);

  xlpop();
}

/*************************************************************************/
/**                                                                     **/
/**                             Print Array                             **/
/**                                                                     **/
/*************************************************************************/

/* Convert to a nested list for printing */
LVAL array_to_nested_list(array)
     LVAL array;
{
  int i;
  LVAL alist;
  
  if (! displacedarrayp(array)) xlerror("not a displaced array", array);

  /* protect the result pointer */
  xlsave1(alist);
  
  alist = coerce_to_list(arraydata(array));
  if (alist != NIL)
    for (i = arrayrank(array) - 1; i > 0; i--)
      alist = splitlist(alist, (int) getfixnum(getdim(array, i)));
  
  /* restore the stack frame */
  xlpop();
  
  return(alist);
}

/* print an array */
putarray(fptr, array, flag)
     LVAL fptr, array;
     int flag;
{
  LVAL value;
  
  if (! displacedarrayp(array)) xlerror("not an array", array);
  
  /* protect a pointer */
  xlsave1(value);
  
  xlputc(fptr,'#');
  value = cvfixnum((FIXTYPE) arrayrank(array));
  xlprint(fptr, value, flag);
  xlputc(fptr, (getvalue(s_printcase) == k_downcase) ? 'a' : 'A');
  value = array_to_nested_list(array);
  if (value == NIL) {
    xlputc(fptr,'(');
    xlputc(fptr,')');
  }
  else
    xlprint(fptr, value, flag);
  
  /* restore the stack frame */
  xlpop();
}
