/* basics - Basic functions for manipulating compound data             */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "xlisp.h"

/* external variables */
extern LVAL s_true, s_ielement, s_hardware_objects, sk_dispose, sk_allocate,
  s_listener;

/* external functions */
extern LVAL makearglist();
extern LVAL copyvector(), copylist(), arraydata(), displacedarraydim(), 
  makedisplacedarray(), xsgetarray(), splitlist(), compounddataseq(),
  getnextelement(), mklist(), subr_map_elements(), lrepeat(), peekarg(),
  checknonnegint(), coerce_to_list(), coerce_to_vector(), xlgetarray(),
  nested_list_to_list(), xsaref(), xselt(), newarray(), xsgetarray(),
  xsgetsequence(), xsgetdisplacedarray();

/**************************************************************************/
/**                                                                      **/
/**                         Sequence Predicate                           **/
/**                                                                      **/
/**************************************************************************/

/* internal sequencep */
sequencep(x)
     LVAL x;
{
  return(listp(x) || simplevectorp(x));
}

/* Built in SEQUENCEP */
LVAL xssequencep()
{
  LVAL x;

  x = xlgetarg();
  xllastarg();
  return((sequencep(x)) ? s_true : NIL);
}

/**************************************************************************/
/**                                                                      **/
/**                           Copying Functions                          **/
/**                                                                      **/
/**************************************************************************/

/* Built in COPY-VECTOR function */
LVAL xscopyvector()
{
  LVAL v;
  
  v = xlgavector();
  xllastarg();
  
  return(copyvector(v));
}

#define copyseq(x) ((vectorp(x)) ? copyvector(x) : copylist(x))

/* internal array copying function */
LVAL copyarray(array)
     LVAL array;
{
  LVAL dim, data, result;
  
  if (simplevectorp(array)) result = copyvector(array);
  else if (displacedarrayp(array)) {
  
    /* protext some pointers */
    xlstkcheck(3);
    xlsave(result);
    xlsave(dim);
    xlsave(data);
  
    dim = copyseq(displacedarraydim(array));
    data = copyvector(arraydata(array));
    result = makedisplacedarray(dim, data);
  
    /* restore the stack frame */
    xlpopn(3);
  }
  else xlerror("not an array", array);
  
  return(result);
}

LVAL xscopyarray()
{
  LVAL array;
  
  array = xsgetarray();
  xllastarg();
  
  return(copyarray(array));
}

/**************************************************************************/
/**                                                                      **/
/**                  Compound Data Decomposition Functions               **/
/**                                                                      **/
/**************************************************************************/

/* Built in SPLIT-LIST function */
LVAL xssplitlist()
{
  LVAL data;
  int n;
  
  data = xlgalist();
  n = getfixnum(xlgafixnum());
  xllastarg();
  
  return(splitlist(data, n));
}

/**************************************************************************/
/**                                                                      **/
/**                         WHICH Function                               **/
/**                                                                      **/
/**************************************************************************/

/* Built in WHICH function. Generates indices in the data sequence of     */
/* a compound data item where argument elements are not nil. Should do    */
/* something more reasonable for non sequence compound data.              */
LVAL xswhich()
{
  LVAL x, result, data, index, tail;
  int i, n;
  
  /* protect the result pointer */
  xlstkcheck(3);
  xlsave(result);
  xlsave(index);
  xlsave(data);
  
  x = xlgetarg();
  xllastarg();
  
  if (compoundp(x)) {
    data = compounddataseq(x);
    n = compounddatalen(x);
    for (i = 0; i < n; i++)
      if (getnextelement(&x, i) != NIL) {
	index = cvfixnum((FIXTYPE) i);
	if (result == NIL) {
	  result = consa(index);
	  tail = result;
	}
	else {
	  rplacd(tail, consa(index));
	  tail = cdr(tail);
	}
      }
  }
  else xlbadtype(x);

  /* restore the stack frame */
  xlpopn(3);
  
  return(result);
}

/**************************************************************************/
/**                                                                      **/
/**                       List Construction Functions                    **/
/**                                                                      **/
/**************************************************************************/

/* internal version of ISEQ function */
LVAL iseq(m, n) 
  int m, n;
{
  int length, i;
  LVAL result, next;

  /* protect the result pointer */
  xlsave1(result);
  
  length = abs(n - m) + 1;
  result = mklist(length, NIL);
  
  for (next = result, i = m; consp(next); next = cdr(next), 
       (m <= n) ? i++ : i--) 
    rplaca(next, cvfixnum((FIXTYPE) i));
  
  /* restore the stack frame */
  xlpop();
  
  return(result);
}

/* Built in ISEQ function. Generates a list of consecutive integers */
LVAL xsiseq()
{
  int n, m;
  
  m = getfixnum(xlgafixnum());
  if (moreargs()) n = getfixnum(xlgafixnum());
  else if (m > 0) {
    n = m - 1;
    m = 0;
  }
  else if (m < 0) {
    m = m + 1;
    n = 0;
  }
  else return(NIL);
  xllastarg();

  return(iseq(m, n));
}

/* Built in REPEAT function */
LVAL xsrepeat()
{
  LVAL data, result;
  int reps;
 
  if (xlargc != 2) xlfail("wrong number of arguments");
  else if (compoundp(xlargv[1])) {
    xlsave1(result);
    result = subr_map_elements(xsrepeat);
    result = coerce_to_list(result);
    result = nested_list_to_list(result, 2);
    xlpop();
  }
  else {
    data = xlgetarg();
    reps = getfixnum(checknonnegint(xlgetarg()));
    xllastarg();
    result = lrepeat(data, reps);
  }
  return(result);
}

/**************************************************************************/
/**                                                                      **/
/**               Subset Selection and Mutation Functions                **/
/**                                                                      **/
/**************************************************************************/

/* is x an ordered list of nonnegative positive integers? */
LOCAL ordered_nneg_seq(x)
     LVAL x;
{
  LVAL elem;
  int n, i, length;

  length = (simplevectorp(x)) ? getsize(x) : 0;

  if (sequencep(x)) {
    for (n = 0, i = 0; consp(x) || i < length; i++) {
      elem = checknonnegint(getnextelement(&x, i));
      if (n > getfixnum(elem)) return(FALSE);
      else n = getfixnum(elem);
    }
    return(TRUE);
  }
  else return(FALSE);
}
      
/* select or set the subsequence corresponding to the specified indices */
LVAL subsequence(x, indices, set_values, values)
     LVAL x, indices, values;
     int set_values;
{
  int rlen, dlen, vlen, i, j;
  LVAL data, result, nextx, nextr, index, elem;

  /* Check the input data */
  if (! sequencep(x)) xlerror("not a sequence", x);
  if (set_values && ! sequencep(values))
    xlerror(" bad value sequence", values);

  /* protect some pointers */
  xlstkcheck(2)
  xlsave(result);
  xlsave(data);

  /* Find the data sizes */
  data =  (ordered_nneg_seq(indices)) ? x : coerce_to_vector(x);
  dlen = (vectorp(data)) ? getsize(data) : llength(data);
  rlen = (vectorp(indices)) ? getsize(indices) : llength(indices);
  if (set_values) {
    vlen = (vectorp(values)) ? getsize(values) : llength(values);
    if (vlen != rlen && indices != s_true) 
      xlfail("value and index sequences do not match");
  }

  /* set up the result/value sequence */
  if (set_values)     result = values;
  else result = (listp(x)) ? mklist(rlen, NIL) : newvector(rlen);

  /* get or set the sequence elements */
  if (indices == s_true) /* do all indices */
    if (set_values)
      for (i = 0; i < dlen; i++)
	setnextelement(&x, i, getnextelement(&values, i));
    else
      result = x;
  else if (sequencep(indices)) { 
    if (set_values) {
      for (nextx = x, nextr = result, i = 0, j = 0; i < rlen; i++) {
	index = getnextelement(&indices, i);
	if (dlen <= getfixnum(index)) xlerror("index out of range", index);
	elem = getnextelement(&result, i);
	if (listp(x)) {
	  if (j > getfixnum(index)) {
	    j = 0;
	    nextx = x;
	  }
	  for (; j < getfixnum(index) && consp(nextx);
	       j++, nextx = cdr(nextx))
	    ;
	  rplaca(nextx, elem);
	}
	else 
	  setelement(x, getfixnum(index), elem);
      }
    }
    else 
      for (nextx = data, nextr = result, i = 0, j = 0; i < rlen; i++) {
	index = getnextelement(&indices, i);
	if (dlen <= getfixnum(index)) xlerror("index out of range", index);
	if (listp(data)) { /* indices must be ordered */
	  for (; j < getfixnum(index) && consp(nextx); j++, nextx = cdr(nextx))
	    ;
	  elem = car(nextx);
	}
	else 
	  elem = getelement(data, getfixnum(index));
	setnextelement(&nextr, i, elem);
      }
  }
  else xlerror("bad indices", indices);
  
  /* restore the stack frame */
  xlpopn(2);
  
  return(result);
}

/* translate row major index in resulting submatrix to row major index in */
/* the original matrix                                                    */
old_rowmajor_index(index, indices, dim, olddim)
     int index;
     LVAL indices, dim, olddim;
{
  int face, oldface, rank, i, oldindex;
  
  rank = getsize(dim);
  
  for (face = 1, oldface = 1, i = 0; i < rank; i++) {
    face *= getfixnum(getelement(dim, i));
    oldface *= getfixnum(getelement(olddim, i));
  }
  
  for (oldindex = 0, i = 0; i < rank; i++) {
    face /= getfixnum(getelement(dim, i));
    oldface /= getfixnum(getelement(olddim, i));
    oldindex +=
      oldface *
	getfixnum(getelement(getelement(indices, i), index / face));
    index = index % face;
  }
  return(oldindex);
}

/* extract or set subarray for the indices from a displaced array */
LVAL subarray(a, indexlist, set_values, values)
     LVAL a, indexlist, values;
     int set_values;
{
  LVAL indices, index, dim, vdim, data, result_data, olddim, result;
  int rank, n, i, j, k;
  
  /* protect some pointers */
  xlstkcheck(4);
  xlsave(indices);
  xlsave(dim);
  xlsave(olddim);
  xlsave(result);

  if (! displacedarrayp(a)) xlerror("not a displaced array", a);
  if (! listp(indexlist)) xlerror("bad index list", indices);
  if (llength(indexlist) != arrayrank(a)) xlfail("wrong number of indices");

  indices = coerce_to_vector(indexlist);
  
  olddim = displacedarraydim(a);
  olddim = coerce_to_vector(olddim);

  /* compute the result dimension vector and fix up the indices */
  rank = arrayrank(a);
  dim = newvector(rank);
  for (i = 0; i < rank; i++) {
    index = getelement(indices, i);
    n = getfixnum(getelement(olddim, i));
    if (index == s_true) {
      index = newvector(n);
      setelement(indices, i, index);
      for (j = 0; j < n; j++)
	setelement(index, j, cvfixnum((FIXTYPE) j));
    }
    else {
      index = coerce_to_vector(index);
      k = getsize(index);
      for (j = 0; j < k; j++) 
	if (n <= getfixnum(checknonnegint(getelement(index, j))))
	  xlerror("index out of bounds", getelement(index, j));
      setelement(indices, i, index);
    }
    setelement(dim, i, cvfixnum((FIXTYPE) getsize(index)));
  }
    
  /* set up the result or check the values*/
  if (set_values) {
    if (! compoundp(values))
      result = newarray(dim, s_ielement, values);
    else {
      if (! displacedarrayp(values) || rank != arrayrank(values))
	xlerror("bad values array", values);
      vdim = displacedarraydim(values);
      for (i = 0; i < rank; i++) 
	if (getfixnum(getnextelement(&vdim, i)) 
	    != getfixnum(getelement(dim, i)))
	  xlerror("bad value array dimensions", values);
      result = values;
    }
  }
  else
    result = newarray(dim, NIL, NIL);

  /* compute the result or set the values */
  data = arraydata(a);
  result_data = arraydata(result);
  n = getsize(result_data);
  for (i = 0; i < n; i++) {
    k = old_rowmajor_index(i, indices, dim, olddim);
    if (0 > k || k >= getsize(data)) xlfail("index out of range");
    if (set_values)
      setelement(data, k, getelement(result_data, i));
    else
      setelement(result_data, i, getelement(data, k));
  }
  
  /* restore the stack frame */
  xlpopn(4);
  
  return(result);
}

/* are all arguments beyond the first fixnums? */
LOCAL allfixargs()
{
  int i;
  
  for (i = 1; i < xlargc; i++) 
    if (! fixp(xlargv[i])) return(FALSE);
  return(TRUE);
}

/* Built in SELECT function */
LVAL xsselect()
{
  LVAL x, indices, result;
  
  if (allfixargs()) {
    if (displacedarrayp(peekarg(0))) result = xsaref();
    else result = xselt();
  }
  else if (sequencep(peekarg(0))) {
    x = xlgetarg();
    indices = xlgetarg();
    result = subsequence(x, indices, FALSE, NIL);
  }
  else if (displacedarrayp(peekarg(0))) {
    xlsave1(indices);
    x = xlgetarg();
    indices = makearglist(xlargc, xlargv);
    result = subarray(x, indices, FALSE, NIL);
    xlpop();
  }
  else xlbadtype(xlgetarg());

  return(result);
}

static setcons(x, first, rest)
	LVAL x, first, rest;
{
  x->n_type = CONS;
  rplaca(x, first);
  rplacd(x, rest);
}

/* Built in SET-SELECT (SETF method for SELECT) */
/* This function uses node data to avoid creating garbage nodes. */
/* This use of nodes *should* be safe, since there *should* be   */
/* no chance of a garbage collection during this operation.      */
LVAL xssetselect()
{
  LVAL x, indices, values, next;
  struct node index_node, value_node;
  LVAL i_list = &index_node, v_list = &value_node;
  
  xlsave1(indices);
  xlsave1(values);
  
  x = xlgetarg();
  indices = makearglist(xlargc, xlargv);
  if (! consp(cdr(indices))) xltoofew();
  else {
    for (next = indices; consp(cdr(cdr(next))); next = cdr(next))
      ;
    values = car(cdr(next));
    rplacd(next, NIL);
  }

  if (sequencep(x)) {
    if (! consp(indices)) xlerror("bad indices", indices);
    indices = car(indices);
    if (fixp(indices)) {
	  setcons(i_list, indices, NIL);
	  setcons(v_list, values, NIL);
      subsequence(x, i_list, TRUE, v_list);
    }
    else
      subsequence(x, indices, TRUE, values);
  }
  else if (displacedarrayp(x))
    subarray(x, indices, TRUE, values);
  else xlbadtype(x);

  xlpopn(2);

  return(values);
}

/**************************************************************************/
/**                                                                      **/
/**                     Array Permutation Function                       **/
/**                                                                      **/
/**************************************************************************/


/* permute x into y using perm; all should be vectors; If check is TRUE */
/* the routine will check to make sure no indices are reused, but x     */
/* will be destroyed.                                                   */
static permute_indices(x, y, perm, check) 
     LVAL x, y, perm;
     int check;
{
  LVAL index;
  int rank, i, k;

  rank = getsize(x);
  for (i = 0; i < rank; i++) {
    index = getelement(perm, i);
    if (! fixp(index)) xlerror("bad permutation sequence", perm);
    k = getfixnum(index);
    if (k < 0 || k >= rank) xlerror("bad permutation sequence", perm);
    setelement(y, i, getelement(x, k));
    if (check)
      setelement(x, k, NIL); /* to insure dimensions are not re-used */
  }
}

/* compute indices in a from rowmajor index k, put in vector result */
/* The indices are stored in cons cells, which are treated locally  */
/* fixnums. This SEEMS to be safe since it is entirely local, but   */
/* it may be dangerous......                                        */
static indices_from_rowmajor(a, k, result)
     LVAL a, result;
     int k;
{
  LVAL next, dim;
  int face, i, rank;
  
  if (! displacedarrayp(a)) xlerror("not a displaced array", a);
  if (0 > k || k >= getsize(arraydata(a))) xlfail("index out of range");
  
  dim = displacedarraydim(a);
  rank = arrayrank(a);
  
  for (i = 0, face = 1, next = dim; i < rank; i++)
    face *= getfixnum(getnextelement(&next, i));

  for (i = 0, next = dim; i < rank; i++) {
    face /= getfixnum(getnextelement(&next, i));
    setfixnum(getelement(result, i),(FIXTYPE) k / face);
    k = k % face;
  }
}

/* Translate row major index in original array to row major index in new */
/* array. Use indices vectors and ilist for temporary storage.           */
static translate_index(i, result, x, perm, indices, oldindices, ilist)
     LVAL result, x, perm, indices, oldindices, ilist;
     int i;
{
  LVAL next;
  int rank, k;

  rank = arrayrank(x);

  indices_from_rowmajor(x, i, oldindices); 
  permute_indices(oldindices, indices, perm, FALSE);

  for (next = ilist, k = 0; k < rank && consp(next); k++, next = cdr(next))
    rplaca(next, getelement(indices, k));

  return(rowmajorindex(result, ilist, FALSE));
}

/* Built in PERMUTE-ARRAY function */
LVAL xspermutearray()
{
  LVAL x, perm, result, data, result_data, dim, olddim, indices;
  LVAL oldindices, ilist;
  int rank, i, k, n;

  /* protect some pointers */
  xlstkcheck(7);
  xlsave(result);
  xlsave(dim);
  xlsave(olddim);
  xlsave(indices);
  xlsave(oldindices);
  xlsave(perm);
  xlsave(ilist);

  /* Get and ckeck the arguments */
  x = xsgetdisplacedarray();
  perm = xsgetsequence();
  perm = coerce_to_vector(perm);
  if (getsize(perm) != arrayrank(x)) xlerror("bad permutation sequence", perm);
  xllastarg();

  /* get old dimension vector */
  olddim = coerce_to_vector(displacedarraydim(x));
  rank = getsize(perm);

  /* construct new dimension vector */
  dim = newvector(rank);
  olddim = copyvector(olddim); /* since permute_indices will destroy this */
  permute_indices(olddim, dim, perm, TRUE);

  /* make result array and the index vectors and lists */
  result = newarray(dim, NIL, NIL);
  indices = newvector(rank);
  oldindices = newvector(rank);
  for (i = 0; i < rank; i++)
    setelement(oldindices, i, consa(NIL));
  ilist = mklist(rank, NIL);

  /* fill in the result */
  data = arraydata(x);
  result_data = arraydata(result);
  if (getsize(data) != getsize(result_data)) xlfail("bad data");
  n = getsize(data);
  for (i = 0; i < n; i++) {
    k = translate_index(i, result, x, perm, indices, oldindices, ilist);
    setelement(result_data, k, getelement(data, i));
  }

  /* restore stack */
  xlpopn(7);

  return(result);
}

#ifdef SAVERESTORE
/* xrestore - restore a saved memory image */
LVAL xsrestore()
{
  extern jmp_buf top_level;
  unsigned char *name;
  LVAL hlist;

  /* get the file name, verbose flag and print flag */
  name = getstring(xlgetfname());
  xllastarg();

  /* dispose of all hardware objects */
  if (consp(getvalue(s_hardware_objects))) {
    xlsave1(hlist);
    hlist = copylist(getvalue(s_hardware_objects));
    for (; consp(hlist); hlist = cdr(hlist))
      send_message(car(cdr(cdr(car(hlist)))), sk_dispose);
    xlpop();
  }
  
  /* restore the saved memory image */
  if (!xlirestore(name))
  return (NIL);

  /* restore hardware items (this may be dangerous) */
  if (symbolp(s_listener) && objectp(getvalue(s_listener)))
    send_message(getvalue(s_listener), sk_allocate);
  if (consp(getvalue(s_hardware_objects))) {
    xlsave1(hlist);
    hlist = copylist(getvalue(s_hardware_objects));
    setvalue(s_hardware_objects, NIL);
    for (; consp(hlist); hlist = cdr(hlist))
      send_message(car(cdr(cdr(car(hlist)))), sk_allocate);
    xlpop();
  }
  
  /* return directly to the top level */
  stdputstr("[ returning to the top level ]\n");
  longjmp(top_level,1);
}
#endif
