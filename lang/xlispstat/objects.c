/* objects - Additional object functions                               */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
# include "xlisp.h"

/* external variables */
extern LVAL xlenv, xlfenv, xlvalue;
extern LVAL s_true, sk_own, s_lambda, sk_print, s_stdout, s_tracelist,
  s_self, s_documentation, s_instance_slots, s_proto_name, 
  sk_dispose, s_unbound, s_set_slot_hook, s_message_hook;
extern LVAL s_lambda, xlenv, xlfenv, xldenv;
extern char buf[];

/* external functions */
extern LVAL makearglist(), copylist(), xdelete(), xsfuncall2(),
  xscallsubr2();

/* macros to handle tracing */
#define trenter(sym,argc,argv) {if (sym) doenter(sym,argc,argv);}
#define trexit(sym,val) {if (sym) doexit(sym,val);}

/***********************************************************************/
/**                                                                   **/
/**                          CLASS Definitions                        **/
/**                                                                   **/
/***********************************************************************/

/* instance variable numbers for the class 'CLASS' */
# define CVARS		  2 /* list of class variable names */
# define CVALS		  3 /* list of class variable values */
# define SUPERCLASS   4
# define IVARTOTAL	  6

/* time stamp for determining validity of allocated objects */
long time_stamp;

/***********************************************************************/
/**                                                                   **/
/**                          Utility Functions                        **/
/**                                                                   **/
/***********************************************************************/

/* Built in KIND-OF-P function */
LVAL xskind_of_p()
{
  LVAL x, y;
  x = xlgetarg();
  y = xlgetarg();
  xllastarg();
  
  return((kind_of_p(x, y)) ? s_true : NIL);
}

LVAL xsobject_null_method() { return(NIL); }

/***********************************************************************/
/***********************************************************************/
/***                                                                 ***/
/***                         New Object System                       ***/
/***                                                                 ***/
/***********************************************************************/
/***********************************************************************/
#define OBJECT_SIZE 4
#define getslots(x) getelement(x, 1)
#define getmethods(x) getelement(x, 2)
#define getparents(x) getelement(x, 3)
#define getpreclist(x) getelement(x, 4)
#define setslots(x, v) setelement(x, 1, v)
#define setmethods(w, v) setelement(x, 2, v)
#define setparents(x, v) setelement(x, 3, v)
#define setpreclist(x, v) setelement(x, 4, v)

static LVAL object_class, root_object;
int in_send = FALSE;

/***********************************************************************/
/**                                                                   **/
/**                          Utility Functions                        **/
/**                                                                   **/
/***********************************************************************/

/* get SELF for the current message; signal an error if not in a message */
static LVAL get_self()
{
  LVAL p = xlxgetvalue(s_self);
  
  if (! mobject_p(p)) xlerror("bad object", p);
  return(p);
}

/* simple form of EQUAL test */
static equal(x, y)
	LVAL x, y;
{
  if (x == y) return(TRUE);
  else if (consp(x) && consp(y)
           && equal(car(x), car(y)) && equal(cdr(x), cdr(y)))
    return(TRUE);
  else return(FALSE);
}

/* check if x is a member of list; use simple equal test */
static is_member(x, list)
	LVAL x, list;
{
  int result = FALSE;
  
  for (; ! result && consp(list); list = cdr(list))
    if (equal(x, car(list))) result = TRUE;
  return(result);
}

/* check if list contains any duplicates */
static has_duplicates(list)
	LVAL list;
{
  int result = FALSE;
  
  for (; ! result && consp(list); list = cdr(list))
    if (is_member(car(list), cdr(list))) result = TRUE;
  return(result);
}

/* destructively delete duplicates from list x */
static LVAL delete_duplicates(x)
     LVAL x;
{
  LVAL last, result;

  if (x == NIL) return(NIL);
  else if (consp(x)) {
    for (; consp(x) && is_member(car(x), cdr(x)); x = cdr(x)) ;

    result = x;

    for (last = x, x = cdr(x); consp(x); x = cdr(x))
      if (is_member(car(x), cdr(x))) rplacd(last, cdr(x));
      else last = x;
  }
  else xlerror("not a list", x);
  return(result);
}

/* destructively append y to x */
static LVAL append_list(x, y)
	LVAL x, y;
{
  LVAL result;
  
  if (x == NIL) result = y;
  else if (consp(x)) {
    result = x;
    for (; consp(cdr(x)); x = cdr(x)) ;
    rplacd(x, y);
  }
  else xlerror("not a list", x);
  return(result);
}

/* destructively delete x from list */
static LVAL delete(x, list)
	LVAL x, list;
{
  return(xscallsubr2(xdelete, x, list));
}

/***********************************************************************/
/**                                                                   **/
/**                Predicate and Stack Access Functions               **/
/**                                                                   **/
/***********************************************************************/

mobject_p(x)
	LVAL x;
{
  return(objectp(x) && getclass(x) == object_class && getsize(x) == 5);
}

static LVAL check_object(object)
	LVAL object;
{
  if (! mobject_p(object)) xlerror("bad object", object);
  else return(object);
}

kind_of_p(x, y)
	LVAL x, y;
{
  if (! mobject_p(x) || ! mobject_p(y)) return(FALSE);
  return(is_member(y, getpreclist(x)));
}

LVAL xsgetmobject() { return(check_object(xlgetarg())); }

/***********************************************************************/
/**                                                                   **/
/**                    Precedence List Functions                      **/
/**                                                                   **/
/***********************************************************************/

/* find set of object and ancestors */
static LVAL find_SC(object)
	LVAL object;
{
  return(copylist(getpreclist(check_object(object))));
}

/* find set of object and ancestors */
static LVAL find_S(object)
	LVAL object;
{
  LVAL result, parents;
  
  xlstkcheck(2);
  xlprotect(object);
  xlsave(result);
  parents = getparents(object);
  for (result = NIL; consp(parents); parents = cdr(parents))
    result = append_list(find_SC(car(parents)), result);
  result = cons(object, result);
  result = delete_duplicates(result);
  xlpopn(2);
  return(result);
}

/* find local precedence ordering */
static LVAL find_RC(object)
	LVAL object;
{
  LVAL list, next;
  
  xlstkcheck(2);
  xlprotect(object);
  xlsave(list);
  list = copylist(getparents(check_object(object)));
  for (next = list; consp(next); next = cdr(next)) {
    rplaca(next, cons(object, car(next)));
    object = cdr(car(next));
  }
  xlpopn(2);
  return(list);
}

/* find partial precedence ordering */
static LVAL find_R(S)
	LVAL S;
{
  LVAL result;
  
  xlstkcheck(2);
  xlprotect(S);
  xlsave(result);
  for (result = NIL; consp(S); S = cdr(S))
    result = append_list(result, find_RC(car(S)));
  result = delete_duplicates(result);
  xlpopn(2);
  return(result);
}

/* check if x has a predecessor according to R */
static has_predecessor(x, R)
	LVAL x, R;
{
  int result = FALSE;
  
  for (; ! result && consp(R); R = cdr(R))
    if (consp(car(R)) && x == cdr(car(R))) result = TRUE;
  return(result);
}

/* find list of objects in S without predecessors, by R */
static LVAL find_no_predecessor_list(S, R)
	LVAL S, R;
{
  LVAL result;
  
  xlstkcheck(3);
  xlprotect(S);
  xlprotect(R);
  xlsave(result);
  for (result = NIL; consp(S); S = cdr(S))
    if (! has_predecessor(car(S), R))
      result = cons(car(S), result);
  xlpopn(3);
  return(result);
}

/* find the position of child, if any, of x in P, the list found so far */
static child_position(x, P)
	LVAL x, P;
{
  int count;
  
  for (count = 0; consp(P); P = cdr(P), count++)
    if (is_member(x, getparents(car(P)))) return(count);
  return(-1);
}

/* find the next object in the precedence list from objects with no */
/* predecessor and current list.                                    */
static LVAL next_object(no_preds, P)
	LVAL no_preds, P;
{
  LVAL result;
  int count, tcount;

  if (! consp(no_preds)) result = NIL;
  else if (! consp(cdr(no_preds))) result = car(no_preds);
  else {
    for (count = -1, result = NIL; consp(no_preds); no_preds = cdr(no_preds)) {
      tcount = child_position(car(no_preds), P);
      if (tcount > count) {
        result = car(no_preds);
        count = tcount;
      }
    }
  }
  return(result);
}

/* remove object x from S */
static LVAL trim_S(x, S)
	LVAL x, S;
{
  LVAL next;
  
  while (consp(S) && x == car(S)) S = cdr(S);
  for (next = S; consp(S) && consp(cdr(next));)
    if (x == car(cdr(next))) rplacd(next, cdr(cdr(next)));
    else next = cdr(next);
  return(S);
}

/* remove all pairs containing x from R. x is assumed to have no */
/* predecessors, so only the first position is checked.          */
static LVAL trim_R(x, R)
	LVAL x, R;
{
  LVAL next;
  
  while (consp(R) && consp(car(R)) && x == car(car(R))) R = cdr(R);
  for (next = R; consp(R) && consp(cdr(next));)
    if (consp(car(next)) && x == car(car(cdr(next))))
      rplacd(next, cdr(cdr(next)));
    else next = cdr(next);
  return(R);
}

/* calculat the object's precedence list */
static LVAL precedence_list(object)
	LVAL object;
{
  LVAL R, S, P, no_preds, next;
  
  check_object(object);
  xlstkcheck(5);
  xlprotect(object);
  xlsave(R);
  xlsave(S);
  xlsave(P);
  xlsave(no_preds);
  S = find_S(object);
  R = find_R(S);
  P = NIL;
  while (consp(S)) {
    no_preds = find_no_predecessor_list(S, R);
    next = next_object(no_preds, P);
    if (next == NIL) xlfail("inconsistent precedence order");
    else {
      P = append_list(P, consa(next));
      S = trim_S(next, S);
      R = trim_R(next, R);
    }
  }
  xlpopn(5);
  return(P);
}

/***********************************************************************/
/**                                                                   **/
/**                  Object Construction Functions                    **/
/**                                                                   **/
/***********************************************************************/

static LVAL calculate_preclist(object)
	LVAL object;
{
  LVAL result, parent, parents;
  
  parents = getparents(check_object(object));
  if (consp(parents)) {
    xlstkcheck(2);
    xlprotect(object);
    xlsave(result);
    if (! consp(cdr(parents))) {
      parent = check_object(car(parents));
      result = getpreclist(parent);
      result = cons(object, result);
    }
    else result = precedence_list(object);
    xlpopn(2);
  }
  else xlerror("bad parent list", parents);
  return(result);
}

static check_parents(parents)
	LVAL parents;
{
  if (parents == NIL) return;
  else if (mobject_p(parents)) return;
  else if (consp(parents)) {
    for (; consp(parents); parents = cdr(parents))
      check_object(car(parents));
  }
  else xlerror("bad parents", parents);
  if (consp(parents) && has_duplicates(parents))
    xlfail("parents may not contain duplicates");
}

static LVAL make_object(parents, object)
	LVAL parents, object;
{
  check_parents(parents);
  
  xlstkcheck(2);
  xlprotect(parents);
  xlprotect(object);
  
  if (! mobject_p(object))
    object = newobject(object_class, OBJECT_SIZE);

  setpreclist(object, getpreclist(root_object));
  if (parents == NIL) setparents(object, consa(root_object));
  else if (mobject_p(parents)) setparents(object, consa(parents));
  else setparents(object, parents);
  
  setpreclist(object, calculate_preclist(object));
  xlpopn(2);
  return(object);
}

LVAL xsmake_object()
{
  LVAL parents, object;
  
  xlsave1(parents);
  parents = makearglist(xlargc, xlargv);
  object = make_object(parents, NIL);
  xlpop();
  return(object);
}

LVAL xsreparent_object()
{
  LVAL parents, object;
  LVAL s_hardware_object = xlenter("HARDWARE-OBJECT-PROTO");
  object = xsgetmobject();

  xlsave1(parents);
  if (kind_of_p(object, getvalue(s_hardware_object)))
    send_message(object, sk_dispose);
  parents = makearglist(xlargc, xlargv);
  object = make_object(parents, object);
  xlpop();
  return(object);
}

/***********************************************************************/
/**                                                                   **/
/**                      Slot Access Functions                        **/
/**                                                                   **/
/***********************************************************************/

#define make_slot_entry(x, y) cons((x), (y))
#define slot_entry_p(x) consp((x))
#define slot_entry_key(x) car((x))
#define slot_entry_value(x) cdr((x))
#define set_slot_entry_value(x, v) rplacd((x), (v))

static LVAL find_own_slot(x, slot)
	LVAL x, slot;
{
  LVAL slots;
  
  if (! mobject_p(x)) return(NIL);
  for (slots = getslots(x); consp(slots); slots = cdr(slots))
    if (slot_entry_p(car(slots)) && slot_entry_key(car(slots)) == slot) 
      return(car(slots));
  return(NIL);
}

static LVAL find_slot(x, slot)
	LVAL x, slot;
{
  LVAL slot_entry, preclist;
  
  if (! mobject_p(x)) slot_entry = NIL;
  else {
    for (slot_entry = NIL, preclist = getpreclist(x);
         slot_entry == NIL && consp(preclist);
         preclist = cdr(preclist))
      slot_entry = find_own_slot(car(preclist), slot);
  }    
  return(slot_entry);
}

static add_slot(x, slot, value)
	LVAL x, slot, value;
{
  LVAL slot_entry;
  
  xlstkcheck(3);
  xlprotect(x);
  xlprotect(slot);
  xlprotect(value);
  check_object(x);
  
  if (! symbolp(slot)) xlerror("not a symbol", slot);
  slot_entry = find_own_slot(x, slot);
  if (slot_entry != NIL) set_slot_entry_value(slot_entry, value);
  else {
    xlsave1(slot_entry);
    slot_entry = make_slot_entry(slot, value);
    setslots(x, cons(slot_entry, getslots(x)));
    xlpop();
  }
  xlpopn(3);
}

static LVAL delete_slot(x, slot)
	LVAL x, slot;
{
  LVAL entry, slots;
  
  if (! mobject_p(x)) return(NIL);
  else {
    entry = find_own_slot(x, slot);
    if (entry == NIL) return(NIL);
    else {
      slots = getslots(x);
      setslots(x, delete(entry, slots));
      return(s_true);
    }
  }
}

LVAL slot_value(x, slot)
	LVAL x, slot;
{
  LVAL slot_entry;
  
  check_object(x);
  slot_entry = find_slot(x, slot);
  if (slot_entry_p(slot_entry)) return(slot_entry_value(slot_entry));
  else xlerror("no slot by this name", slot);
}

#define CONSTRAINTHOOKS

check_hooks(object, sym, slot)
	LVAL object, sym;
	int slot;
{
#ifdef CONSTRAINTHOOKS
  LVAL hook, hooksym, olddenv;
  
  hooksym = (slot) ? s_set_slot_hook : s_message_hook;
  hook = getvalue(hooksym);
  if (hook != s_unbound && hook != NIL) {
    /* rebind the hook function to nil */
    olddenv = xldenv;
    xldbind(hooksym,NIL);

    xsfuncall2(hook, object, sym);

    /* unbind the hook symbol */
    xlunbind(olddenv);
  }
#endif CONSTRAINTHOOKS
}

LVAL set_slot_value(x, slot, value)
	LVAL x, slot, value;
{
  LVAL slot_entry;
  
  check_object(x);
  slot_entry = find_own_slot(x, slot);
  if (slot_entry_p(slot_entry)) {
    set_slot_entry_value(slot_entry, value);
    check_hooks(x, slot_entry_key(slot_entry), TRUE);
  }
  else {
    if (find_slot(x, slot) != NIL)
      xlerror("object does not own slot", slot);
    else xlerror("no slot by this name", slot);
  }
  return(value);
}

LVAL xshas_slot()
{
  LVAL x, slot, own, slot_entry;
  
  x = xsgetmobject();
  slot = xlgasymbol();
  if (! xlgetkeyarg(sk_own, &own)) own = NIL;
  
  slot_entry = (own == NIL) ? find_slot(x, slot) : find_own_slot(x, slot);
  return((slot_entry != NIL) ? s_true : NIL);
}

LVAL xsadd_slot()
{
  LVAL x, slot, value;
  
  x = xsgetmobject();
  slot = xlgasymbol();
  value = (moreargs()) ? xlgetarg() : NIL;
  xllastarg();
  
  add_slot(x, slot, value);
  return(value);
}

LVAL xsdelete_slot()
{
  LVAL x, slot;
  
  x = xsgetmobject();
  slot = xlgasymbol();
  xllastarg();
  
  return(delete_slot(x, slot));
}

LVAL xsslot_value()
{
  LVAL x, slot, value;
  int set = FALSE;
  
  x = get_self(); /*xsgetmobject();*/
  slot = xlgasymbol();
  if (moreargs()) {
    set = TRUE;
    value = xlgetarg();
  }
  xllastarg();
  
  if (set) return(set_slot_value(x, slot, value));
  else return(slot_value(x, slot));
}

/***********************************************************************/
/**                                                                   **/
/**                    Method Access Functions                        **/
/**                                                                   **/
/***********************************************************************/

#define make_method_entry(x, y) cons((x), (y))
#define method_entry_p(x) consp((x))
#define method_entry_key(x) car((x))
#define method_entry_method(x) cdr((x))
#define set_method_entry_method(x, v) rplacd((x), (v))

static LVAL find_own_method(x, selector)
	LVAL x, selector;
{
  LVAL methods;
  
  if (! mobject_p(x)) return(NIL);
  for (methods = getmethods(x); consp(methods); methods = cdr(methods))
    if (method_entry_p(car(methods)) 
        && method_entry_key(car(methods)) == selector)
      return(car(methods));
  return(NIL);
}

static LVAL find_method(x, selector)
	LVAL x, selector;
{
  LVAL method_entry, preclist;
  
  if (! mobject_p(x)) method_entry = NIL;
  else {
    for (method_entry = NIL, preclist = getpreclist(x);
         method_entry == NIL && consp(preclist);
         preclist = cdr(preclist))
      method_entry = find_own_method(car(preclist), selector);
  }    
  return(method_entry);
}

static add_method(x, selector, method)
	LVAL x, selector, method;
{
  LVAL method_entry;
  
  xlstkcheck(3);
  xlprotect(x);
  xlprotect(selector);
  xlprotect(method);
  
  check_object(x);
  if (! symbolp(selector)) xlerror("not a symbol", selector);
  method_entry = find_own_method(x, selector);
  if (method_entry != NIL)
    set_method_entry_method(method_entry, method);
  else {
    xlsave1(method_entry);
    method_entry = make_method_entry(selector, method);
    setmethods(x, cons(method_entry, getmethods(x)));
    xlpop();
  }
  xlpopn(3);
}

static LVAL delete_method(x, selector)
	LVAL x, selector;
{
  LVAL entry, methods;
  
  if (! mobject_p(x)) return(NIL);
  else {
    entry = find_own_method(x, selector);
    if (entry == NIL) return(NIL);
    else {
      methods = getmethods(x);
      setmethods(x, delete(entry, methods));
      return(s_true);
    }
  }
}

static LVAL message_method(x, selector)
	LVAL x, selector;
{
  LVAL method_entry;
  
  check_object(x);
  method_entry = find_method(x, selector);
  if (method_entry_p(method_entry)) 
    return(method_entry_method(method_entry));
  else xlfail("no method for this selector");
}

#ifdef DODO
static LVAL set_message_method(x, selector, method)
	LVAL x, selector, method;
{
  LVAL method_entry;
  
  check_object(x);
  method_entry = find_method(x, selector);
  if (method_entry_p(method_entry))
    set_method_entry_method(method_entry, method);
  else xlfail("no method for this selector");
  return(method);
}
#endif DODO
  
LVAL xshas_method()
{
  LVAL x, selector, own, method_entry;
  
  x = xsgetmobject();
  selector = xlgasymbol();
  if (! xlgetkeyarg(sk_own, &own)) own = NIL;
  
  method_entry = (own == NIL)
               ? find_method(x, selector) : find_own_method(x, selector);
  return((method_entry != NIL) ? s_true : NIL);
}

LVAL xsadd_method()
{
  LVAL x, selector, method;
  
  x = xsgetmobject();
  selector = xlgasymbol();
  method = (moreargs()) ? xlgetarg() : NIL;
  xllastarg();
  
  add_method(x, selector, method);
  return(method);
}

LVAL xsdelete_method()
{
  LVAL x, selector;
  
  x = xsgetmobject();
  selector = xlgasymbol();
  xllastarg();
  
  return(delete_method(x, selector));
}

LVAL xsmessage_method()
{
  LVAL x, selector;
  
  x = xsgetmobject();
  selector = xlgasymbol();
  xllastarg();
  
  return(message_method(x, selector));
}

/***********************************************************************/
/**                                                                   **/
/**                    Message Sending Functions                      **/
/**                                                                   **/
/***********************************************************************/

static LVAL current_preclist = NIL;
static LVAL current_selector = NIL;

/*#define SAFEMESS*/
#ifndef SAFEMESS
static LVAL callmethod(method, object, argc, argv)
	LVAL method, object, *argv;
    int argc;
{
  LVAL *newfp;
  int i;
    
  /* build a new argument stack frame */
  newfp = xlsp;
  pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
  pusharg(method);
  pusharg(cvfixnum((FIXTYPE) (argc + 1)));

  /* push each argument */
  pusharg(object);
  for (i = 0; i < argc; i++) pusharg(argv[i]);

  /* establish the new stack frame */
  xlfp = newfp;

  return(xlapply(argc + 1));
}
#endif /* SAFEMESS */

static LVAL sendmsg(object, selector)
	LVAL object, selector;
{
  LVAL method_entry, method, old_preclist, preclist, val, old_selector;
  LVAL tracing = NIL;
#ifdef SAFEMESS
  LVAL args;
#endif

  old_selector = current_selector;
  current_selector = selector;

  /* look for the message in the precedence list */
  old_preclist = current_preclist;
  for (method_entry = NIL, preclist = current_preclist;
       method_entry == NIL && consp(preclist);
       preclist = cdr(preclist)) {
    method_entry = find_own_method(car(preclist), selector);
    current_preclist = preclist;
  }
  if (method_entry == NIL)
    xlerror("no method for this message", selector);
  else if (! method_entry_p(method_entry)) xlfail("bad method entry");
  else method = method_entry_method(method_entry);
   
  /* invoke the method */
  if (getvalue(s_tracelist) && is_member(selector,getvalue(s_tracelist)))
    tracing = selector;
  trenter(tracing,xlargc,xlargv);
#ifdef SAFEMESS
  xlsave1(args);
  args = makearglist(xlargc, xlargv);
  args = cons(object, args);
  val = xlapply(pushargs(method, args));
  xlpop();
#else
  val = callmethod(method, object, xlargc, xlargv);
#endif /* SAFEMESS */
  trexit(tracing,val);
  
  current_preclist = old_preclist;
  current_selector = old_selector;
  check_hooks(object, method_entry_key(method_entry), FALSE);
  return(val);
}

/* send message with arguments on the stack */
LVAL send_message_stk(object, selector)
	LVAL object, selector;
{
  LVAL old_preclist, result;
  int old_in_send = in_send;
  
  old_preclist = current_preclist;
  current_preclist = getpreclist(object);
  in_send = TRUE;
  result = sendmsg(object, selector);
  current_preclist = old_preclist;
  in_send = old_in_send;
  return(result);
}


/* xmsendsuper - send a message to the superobject of an object */
LVAL xmsendsuper()
{
  LVAL old_preclist, object, result;
  int old_in_send = in_send;
  
  object = get_self();
  old_preclist = current_preclist;
  if (! consp(current_preclist))
    xlfail("no more objects in precedence list");
  current_preclist = cdr(current_preclist);
  in_send = TRUE;
  result = sendmsg(object, xlgasymbol());
  current_preclist = old_preclist;
  in_send = old_in_send;
  return(result);
}

/* xscall_next - call inherited version of current method */
LVAL xscall_next()
{
  LVAL old_preclist, object, result;
  int old_in_send = in_send;
  
  object = get_self();
  old_preclist = current_preclist;
  if (! consp(current_preclist))
    xlfail("no more objects in precedence list");
  current_preclist = cdr(current_preclist);
  in_send = TRUE;
  result = sendmsg(object, current_selector);
  current_preclist = old_preclist;
  in_send = old_in_send;
  return(result);
}

LVAL xmsend()
{
  LVAL object, old_preclist, result;
  int old_in_send = in_send;
  
  object = xlgaobject();
  if (! mobject_p(object)) return(NIL);

  old_preclist = current_preclist;
  current_preclist = getpreclist(object);
  in_send = TRUE;
  result = sendmsg(object, xlgasymbol());
  current_preclist = old_preclist;
  in_send = old_in_send;
  return(result);
}
  
LVAL xscall_method() 
{
  LVAL object, self, old_preclist, result;
  int old_in_send = in_send;
  
  object = xlgaobject();
  self = get_self();
  old_preclist = current_preclist;
  current_preclist = getpreclist(object);
  in_send = TRUE;
  result = sendmsg(self, xlgasymbol());
  current_preclist = old_preclist;
  in_send = old_in_send;
  return(result);
}
  
print_mobject(object, stream)
	LVAL object, stream;
{
  send_message_1L(object, sk_print, stream);
}

LVAL xsshow_object()
{
  LVAL x, fptr;
  
  x = xsgetmobject();
  fptr = (moreargs() ? xlgetfile() : getvalue(s_stdout));
  xllastarg();
  
  xlputstr(fptr, "Slots = "); xlprint(fptr, getslots(x), TRUE); xlterpri(fptr);
  xlputstr(fptr, "Methods = "); xlprint(fptr, getmethods(x), TRUE); xlterpri(fptr);
  xlputstr(fptr, "Parents = "); xlprint(fptr, getparents(x), TRUE); xlterpri(fptr);
  xlputstr(fptr, "Precedence List = "); xlprint(fptr, getpreclist(x), TRUE); xlterpri(fptr);
  return(NIL);
}

LVAL xsparents()
{
  LVAL x;
  
  x = xsgetmobject();
  xllastarg();
  
  return(copylist(getparents(x)));
}

LVAL xsprecedence_list()
{
  LVAL x;
  
  x = xsgetmobject();
  xllastarg();
  
  return(copylist(getpreclist(x)));
}

static LVAL get_cars(x)
	LVAL x;
{
  LVAL next;
  
  for (next = x; consp(next); next = cdr(next))
  	if (consp(car(next)))
  	  rplaca(next, car(car(next)));
  return(x);
}

LVAL xsobject_methods()
{
  LVAL x;
  
  x = xsgetmobject();
  xllastarg();
  
  return(get_cars(copylist(getmethods(x))));
}

LVAL xsobject_slots()
{
  LVAL x;
  
  x = xsgetmobject();
  xllastarg();
  
  return(get_cars(copylist(getslots(x))));
}

statobsymbols()
{
  object_class = getvalue(xlenter("OBJECT"));
  root_object = getvalue(xlenter("*OBJECT*"));
}

lex_slot_value(object, sym, pval)
	LVAL object, sym, *pval;
{
  int has = (find_slot(object, sym) != NIL);
  if (has) *pval = slot_value(object, sym);
  return(has);
}

object_isnew(object)
	LVAL object;
{
  LVAL slots, sym, ksym, value;

  for (slots = getslots(object); consp(slots); slots = cdr(slots)) {
    sym = car(car(slots));
    if (! symbolp(sym)) xlerror("bad slot entry", car(slots));
    sprintf(buf, ":%s", getstring(getpname(sym)));
    ksym = xlenter(buf);
    if (xlgetkeyarg(ksym, &value)) set_slot_value(object, sym, value);
  }
}

LVAL xsobject_isnew()
{
  LVAL object;

  object = xsgetmobject();
  object_isnew(object);
  return(object);
}
  
#define FIRST_METHOD_OFFSET 300

/* xsaddmsg - add a message to an object */
xsaddmsg(object, str)
	LVAL object;
	char *str;
{
  LVAL fcn;
  extern FUNDEF funtab[];
  static offset = FIRST_METHOD_OFFSET; 

  xlsave1(fcn);
  fcn = cvsubr(funtab[offset].fd_subr,funtab[offset].fd_type,offset);
  add_method(object, xlenter(str), fcn);
  xlpop();
  
  offset++;
}

xsaddslot(object, str)
	LVAL object;
	char *str;
{
  add_slot(object, xlenter(str), NIL);
}

LVAL xsnewproto(str, parents)
	char *str;
	LVAL parents;
{
  LVAL sym = xlenter(str), object;

  xlsave1(object);
  object = make_object(parents, NIL);
  make_prototype(object, sym, NIL, NIL, NIL, TRUE);
  xlpop();
  
  return(object);
}

LVAL init_root_object()
{
  LVAL s__object_ = xlenter("*OBJECT*");
  
  object_class = getvalue(xlenter("OBJECT"));
  root_object = newobject(object_class, OBJECT_SIZE);
  setvalue(s__object_, root_object);
  setpreclist(root_object, consa(root_object));
  
  add_slot(root_object, s_instance_slots, NIL);
  add_slot(root_object, s_proto_name, s__object_);
  return(root_object);
}

static LVAL find_documentation(x, sym, add)
	LVAL x, sym;
	int add;
{
  LVAL doc;
  
  if (! mobject_p(x)) return(NIL);
  doc = find_own_slot(x, s_documentation);
  if (doc == NIL && add) add_slot(x, s_documentation, NIL);
  if (consp(doc)) doc = cdr(doc);
  for (; consp(doc); doc = cdr(doc))
    if (consp(car(doc)) && car(car(doc)) == sym) return(car(doc));
  return(NIL);
}

/* x should be protected from gc before calling add_slot */
static add_documentation(x, sym, value)
	LVAL x, sym, value;
{
  LVAL doc_entry;
  
  xlstkcheck(3);
  xlprotect(x);
  xlprotect(sym);
  xlprotect(value);
  check_object(x);
  if (! symbolp(sym)) xlerror("not a symbol", sym);
  doc_entry = find_documentation(x, sym, TRUE);
  if (doc_entry != NIL) rplacd(doc_entry, value);
  else {
    xlsave1(doc_entry);
    doc_entry = cons(sym, value);
    set_slot_value(x,
                   s_documentation,
                   cons(doc_entry, slot_value(x, s_documentation)));
    xlpop();
  }
  xlpopn(3);
}

static LVAL get_documentation(x, sym)
	LVAL x, sym;
{
  LVAL doc_entry, list;
  
  check_object(x);
#ifdef DODO /* this olny looks in the object itself! */
  for (list = getpreclist(x); consp(list); list = cdr(list)) {
    doc_entry = find_documentation(x, sym, FALSE);
    if (doc_entry != NIL) break;
  }
#endif DODO
  doc_entry = find_documentation(x, sym, FALSE);
  return (consp(doc_entry) ? cdr(doc_entry) : NIL);
}

LVAL xsobject_documentation()
{
  LVAL x, sym, val;
  
  x = xsgetmobject();
  sym = xlgasymbol();
  if (moreargs()) {
    val = xlgetarg();
    add_documentation(x, sym, val);
  }
  return(get_documentation(x, sym));
}
  

LVAL xsdefmeth()
{
  LVAL object, sym, fargs, arglist, fcn;
  
  xlstkcheck(3);
  xlsave(fargs);
  xlsave(arglist);
  xlsave(fcn);
  object = xleval(xlgetarg());
  sym = xlgasymbol();
  fargs = xlgalist();
  arglist = makearglist(xlargc,xlargv);

  if (! mobject_p(object)) xlerror("bad object", object);

  /* install documentation string */
  if (consp(arglist) && stringp(car(arglist)) && consp(cdr(arglist))) {
    add_documentation(object, sym, car(arglist));
    arglist = cdr(arglist);
  }

  /* create a new function definition */
  fargs = cons(s_self, fargs);
  fcn = xlclose(sym, s_lambda, fargs, arglist, xlenv, xlfenv);

  /* add the method to the object */
  add_method(object, sym, fcn);
  
  /* restore the stack and return the symbol */
  xlpopn(3);
  return (sym);
}

/***********************************************************************/
/**                                                                   **/
/**                  Prototype Construction Functions                 **/
/**                                                                   **/
/***********************************************************************/

static LVAL instance_slots(x, slots)
	LVAL x, slots;
{
  LVAL parents = getparents(x), result, sym, temp, tail;
  
  xlsave1(result);
  result = copylist(slots);
  result = delete_duplicates(result);
  for (tail = result; consp(tail) && consp(cdr(tail)); tail = cdr(tail));

  for (; consp(parents); parents = cdr(parents)) {
    for (temp = slot_value(car(parents), s_instance_slots);
         consp(temp);
         temp = cdr(temp)) {
      sym = car(temp);
      if (! is_member(sym, result)) {
        if (result == NIL) {
          result = consa(sym);
          tail = result;
        }
        else {
          rplacd(tail, consa(sym));
          tail = cdr(tail);
        }
      }
    }
  }
  xlpop();
  
  return(result);
}

static LVAL get_initial_slot_value(object, slot)
	LVAL object, slot;
{
  LVAL entry = find_slot(object, slot);
  return((entry != NIL) ? cdr(entry) : NIL);
}

static make_prototype(object, name, ivars, cvars, doc, set)
	LVAL object, name, ivars, cvars, doc;
	int set;
{
  LVAL slot;
  
  xlprot1(ivars);
  
  ivars = instance_slots(object, ivars);
  add_slot(object, s_instance_slots, ivars);
  add_slot(object, s_proto_name, name);
  
  for (; consp(ivars); ivars = cdr(ivars)) {
    slot = car(ivars);
    add_slot(object, slot, get_initial_slot_value(object, slot));
  }
  
  for (; consp(cvars); cvars = cdr(cvars)) 
    add_slot(object, car(cvars), NIL);
    
  if (doc != NIL && stringp(doc))
    add_documentation(object, xlenter("PROTO"), doc);
    
  if (set) setvalue(name, object);

  xlpop();
}

xsaddinstanceslot(x, s)
	LVAL x;
	char *s;
{
  LVAL sym = xlenter(s), ivars = slot_value(x, s_instance_slots);
  
  if (! is_member(sym, ivars)) {
    add_slot(x, sym, get_initial_slot_value(x, sym));
    set_slot_value(x, s_instance_slots, cons(sym, ivars));
  }
}

xssetslotval(x, s, val)
	LVAL x, val;
	char *s;
{
  set_slot_value(x, xlenter(s), val);
}

LVAL xsdefproto()
{
  LVAL object, name, ivars, cvars, parents, doc;
  
  xlstkcheck(5);
  xlsave(object);
  xlsave(ivars);
  xlsave(cvars);
  xlsave(parents);
  xlsave(doc);
  
  name = xlgasymbol();
  ivars = (moreargs()) ? xleval(ivars = xlgetarg()) : NIL;
  cvars = (moreargs()) ? xleval(cvars = xlgetarg()) : NIL;
  parents = (moreargs()) ? xleval(parents = xlgetarg()) : NIL;
  doc = (moreargs()) ? xleval(doc = xlgetarg()) : NIL;
  
  if (! listp(parents)) parents = consa(parents);
  object = make_object(parents, NIL);
  make_prototype(object, name, ivars, cvars, doc, TRUE);
  
  xlpopn(5);
  return(name);
}

LVAL xsmakeproto()
{
  LVAL object, name, ivars;
  
  object = xsgetmobject();
  name = xlgasymbol();
  ivars = (moreargs()) ? xlgetarg() : NIL;
  
  make_prototype(object, name, ivars, NIL, NIL, FALSE);
  
  return(object);
}

LVAL clanswer () { return(NIL); }
LVAL clisnew () { return(NIL); }
LVAL clnew () { return(NIL); }
obsymbols () {}
LVAL obclass () { return(NIL); }
LVAL obshow () { return(NIL); }
LVAL obisnew () { return(NIL); }
LVAL xsend () { return(NIL); }
xlobgetvalue (a, b, c) LVAL a,b,*c; { return(FALSE); }
xlobsetvalue (a, b, c) LVAL a,b,c;  { return(FALSE); }
LVAL xsendsuper () { return(NIL); }
xloinit () {}
