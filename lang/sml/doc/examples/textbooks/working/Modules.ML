(**** ML Programs from the book

  ML for the Working Programmer
  by Lawrence C. Paulson, Computer Laboratory, University of Cambridge.
  (Cambridge University Press, 1991)

Copyright (C) 1991 by Cambridge University Press.
Permission to copy without fee is granted provided that this copyright
notice and the DISCLAIMER OF WARRANTY are included in any copy.

DISCLAIMER OF WARRANTY.  These programs are provided `as is' without
warranty of any kind.  We make no warranties, express or implied, that the
programs are free of error, or are consistent with any particular standard
of merchantability, or that they will meet your requirements for any
particular application.  They should not be relied upon for solving a
problem whose incorrect solution could result in injury to a person or loss
of property.  If you do use the programs or functions in such a manner, it
is at your own risk.  The author and publisher disclaim all liability for
direct, incidental or consequential damages resulting from your use of
these programs or functions.
****)


(**** Chapter 7.  MODULES ****)

(*** Queues represented by (heads, reversed tails).  See Burton (1982). ***)

signature QUEUE = 
  sig
  type 'a T			(*type of queues*)
  exception E			(*for errors in hd, deq*)
  val empty: 'a T		(*the empty queue*)
  val enq: 'a T * 'a -> 'a T    (*add to end*)
  val null: 'a T -> bool	(*test for empty queue*)
  val hd: 'a T -> 'a		(*return front element*)
  val deq: 'a T -> 'a T		(*remove element from front*)
  end;


structure Queue : QUEUE = 
  struct
  datatype 'a T = Queue of ('a list * 'a list);
  exception E;

  val empty = Queue([],[]);

  fun norm (Queue([],tails)) = Queue(rev tails, [])
    | norm q = q;

  fun enq(Queue(heads,tails), x) = norm(Queue(heads, x::tails));

  fun null(Queue([],[])) = true
    | null _ = false;

  fun hd(Queue(x::_,_)) = x
    | hd(Queue([],_)) = raise E;

  fun deq(Queue(x::heads,tails)) = norm(Queue(heads,tails))
    | deq(Queue([],_)) = raise E;
  end;


(**** Binary trees as a general-purpose type ***)

signature TREE = 
  sig
  datatype 'a T = Lf  |  Br of 'a * 'a T * 'a T
  val count: 'a T -> int
  val depth: 'a T -> int
  val reflect: 'a T -> 'a T
  end;


functor TreeFUN () : TREE = 
  struct
  datatype 'a T = Lf
		| Br of 'a * 'a T * 'a T;

  fun count Lf = 0
    | count (Br(v,t1,t2)) = 1 + count t1 + count t2;

  (*book version refers to this as a free identifier*)
  fun maxl[m] : int = m
    | maxl(m::n::ns) = if m>n  then  maxl(m::ns)  else  maxl(n::ns);

  fun depth Lf = 0
    | depth (Br(v,t1,t2)) = 1 + maxl[depth t1, depth t2];

  fun reflect Lf = Lf
    | reflect (Br(v,t1,t2)) = Br(v, reflect t2, reflect t1);
  end;


(*** Concrete array operations ***)

nonfix sub;  (*required for Standard ML of New Jersey*)

signature ARRAYOPS = 
  sig
  structure Tree: TREE
  exception E				(*errors in operations*)
  val sub:    'a Tree.T * int -> 'a
  val update: 'a Tree.T * int * 'a -> 'a Tree.T
  val hirem:  'a Tree.T * int -> 'a Tree.T
  end;


functor ArrayOpsFUN (structure Tree: TREE) : ARRAYOPS = 
  struct
  structure Tree = Tree;
  exception E;

  local open Tree  
  in
    fun sub (Lf, _) = raise E
      | sub (Br(v,t1,t2), k) =
	    if k=1 then v
	  else if k mod 2 = 0 
		 then sub (t1, k div 2)
		 else sub (t2, k div 2);

    fun update (Lf, k, w) = 
	  if k = 1 then Br (w, Lf, Lf)
	  else  raise E
      | update (Br(v,t1,t2), k, w) =
	  if k = 1 then Br (w, t1, t2)
	  else if k mod 2 = 0 
	       then Br (v,  update(t1, k div 2, w),  t2)
	       else Br (v,  t1,  update(t2, k div 2, w));

    fun hirem (Lf, n) = raise E
      | hirem (Br(v,t1,t2), n) =
	  if n = 1 then Lf
	  else if n mod 2 = 0 
	       then Br (v,  hirem(t1, n div 2),  t2)
	       else Br (v,  t1,  hirem(t2, n div 2));
  end
  end;


(*** Functional arrays as abstract type ***)

signature ARRAY = 
  sig
  type 'a T
  exception Sub and Update and Hirem
  val empty:  'a T
  val sub:    'a T * int -> 'a
  val update: 'a T * int * 'a -> 'a T
  val hiext:  'a T * 'a -> 'a T
  val hirem:  'a T -> 'a T
  end;


functor ArrayFUN (structure ArrayOps: ARRAYOPS) : ARRAY = 
  struct
  datatype 'a T = Array of 'a ArrayOps.Tree.T * int;
  exception Sub and Update and Hirem;

  val empty = Array(ArrayOps.Tree.Lf, 0);

  fun sub (Array(t,n), k) = 
      if 1<=k andalso k<=n 
      then ArrayOps.sub(t,k)
      else raise Sub;

  fun update (Array(t,n), k, w) = 
      if 1<=k andalso k<=n 
      then Array(ArrayOps.update(t,k,w), n)
      else raise Update;

  fun hiext (Array(t,n), w) = Array(ArrayOps.update(t,n+1,w), n+1);

  fun hirem(Array(t,n)) = 
      if n>0 then Array(ArrayOps.hirem(t,n) , n-1)
      else raise Hirem;
  end;


(**** FUNCTORS ****)

(*** Linearly ordered types ***)

signature ORDER = 
  sig
  type T
  val less: T*T -> bool
  end;


(*** Tables as Binary search trees ***)

signature TABLE = 
  sig
  type key				(*type of keys*)
  type 'a T				(*type of tables*)
  exception Lookup			(*errors in lookup*)
  val empty: 'a T			(*the empty table*)
  val lookup: 'a T * key -> 'a
  val update: 'a T * key * 'a -> 'a T
  end;

functor TableFUN (structure Order: ORDER and Tree: TREE) : TABLE = 
  struct
  type key = Order.T;
  type 'a T = (key * 'a) Tree.T;
  exception Lookup;

  local open Tree
  in
    val empty = Lf;

    fun lookup (Br ((a,x),t1,t2), b) =
	  if      Order.less(b,a) then  lookup(t1, b)
	  else if Order.less(a,b) then  lookup(t2, b)
	  else x
      | lookup (Lf, b) = raise Lookup;

    fun update (Lf, b, y) = Br((b,y), Lf, Lf)
      | update (Br((a,x),t1,t2), b, y) =
	  if Order.less(b,a) 
	  then  Br ((a,x),  update(t1,b,y),  t2)
	  else if Order.less(a,b) 
	  then  Br ((a,x),  t1,  update(t2,b,y))
	  else (*a=b*) Br ((a,y),t1,t2);
  end
  end;


(*** Priority queues ***)

signature PQUEUE = 
  sig
  structure Order: ORDER	(*ordering for elements*)
  type T			(*type of priority queues*)
  exception E			(*for errors in hd, deq*)
  val empty: T			(*the empty priority queue*)
  val enq: T * Order.T -> T    	(*insert into priority queue*)
  val null: T -> bool		(*test for empty*)
  val hd: T -> Order.T		(*return front element*)
  val deq: T -> T		(*remove element from front*)
  end;


functor PQueueFUN (structure Order: ORDER 
		   and       ArrayOps: ARRAYOPS) : PQUEUE = 
  struct
  structure Order = Order;
  datatype T = PQueue of Order.T ArrayOps.Tree.T * int;
  exception E;

  local open ArrayOps.Tree
        infix <<
        fun v<<w = Order.less(v,w)
  in
  fun upheap (Lf, n, w) = Br (w, Lf, Lf)
    | upheap (Br(v,t1,t2), n, w) =  (* assume n>1 *)
	if w<<v then
	    if n mod 2 = 0 
	    then Br (v,  upheap(t1, n div 2, w),  t2)
	    else Br (v,  t1,  upheap(t2, n div 2, w))
	else if n mod 2 = 0 
	     then Br (w, upheap(t1, n div 2, v), t2)
	     else Br (w, t1, upheap(t2, n div 2, v));

  fun downheap (Br(_,t1,t2), w) =
    case t1 of  Lf => Br(w,Lf,Lf)
      | Br(v1,_,_) =>
	 (case t2 of
	     Lf => if v1<<w then Br(w, t1, Lf)
		   else Br(v1, downheap(t1,w), Lf)
	   | Br(v2,_,_) =>
	       if v1<<v2
	       then if v2<<w then Br(w, t1, t2)
			     else Br(v2, t1, downheap(t2,w))
	       else if v1<<w then Br(w, t1, t2)
			     else Br(v1, downheap(t1,w), t2) );

  val empty = PQueue(Lf,0);

  fun enq(PQueue(t,n), w) = PQueue(upheap(t,n+1,w), n+1);

  fun null (PQueue(Br _, _)) = false
    | null (PQueue(Lf,   _)) = true;

  fun hd (PQueue(Br(w,_,_), n)) = w
    | hd (PQueue(Lf,        _)) = raise E;

  (*Remove position n from heap, for n>0. *)
  fun deq (PQueue(t,n)) = 
	if n>1 then PQueue(downheap(ArrayOps.hirem(t,n), 
				    ArrayOps.sub(t,n)), 
			   n-1)
	else if n=1 then empty
	else raise E;
  end
  end;


(** Alternative implementation of Tables -- Illustrates eqtype **)
functor AlistFUN (eqtype key) : TABLE = 
  struct
  type key = key;
  type 'a T = (key * 'a) list;
  exception Lookup;

  val empty = [];

  fun lookup ([], a) = raise Lookup
    | lookup ((x,y)::pairs, a) =
	if a=x then  y  else  lookup(pairs, a);

  fun update (pairs, b, y) = (b,y)::pairs;
  end;


(******** SHORT DEMONSTRATIONS ********)

(** Application of the functors  **)
structure Tree = TreeFUN();
structure ArrayOps = ArrayOpsFUN (structure Tree=Tree);
structure FArray = ArrayFUN (structure ArrayOps=ArrayOps);

open FArray;
hiext(empty, "A");
hiext(it,"B");
hiext(it,"C");
val tletters = hiext(it,"D");
val tdag = update(tletters, 4, "dagger");
sub(tletters,4);
sub(tdag,4);
hirem tletters;
hirem it;
empty=empty;    (*STILL admits equality!*)

structure StringIntAlist = AlistFUN(type key=string*int);
local open StringIntAlist in 
val atab = update(update(empty, ("Henry", 5), "Good"),
		  ("Henry", 8), "Bad")
end;


