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


(**** Chapter 8.  IMPERATIVE PROGRAMMING IN ML ****)

(*** SEQUENCES USING REFERENCES ***)

signature SEQUENCE = 
  sig
  type 'a T
  exception E
  val empty: 'a T
  val cons: '_a * (unit -> '_a T) -> '_a T
  val null: 'a T -> bool
  val hd: 'a T -> 'a
  val tl: 'a T -> 'a T
  val take: int * 'a T -> 'a list
  val append: '_a T * '_a T -> '_a T
  val map: ('a -> '_b) -> 'a T -> '_b T
  val filter: ('_a -> bool) -> '_a T -> '_a T
  val cycle: ((unit -> '_a T) -> '_a T) -> '_a T
  end;


functor ImpSeqFUN () : SEQUENCE =
  struct
  datatype 'a T  = Nil
		 | Cons of 'a * ('a T) ref
		 | Delayed of unit -> 'a T;

  exception E;

  fun delay xf = ref(Delayed xf);

  (*sequence "constructors" for export*)
  val empty = Nil;
  fun cons(x,xf) = Cons(x, delay xf);

  (*gets tail value, perhaps with the side effect of storing it*)
  fun pull xp = 
	case !xp of
	    Delayed f => let val s = f()
			 in  xp := s;  s  end
	  | s => s;

  (** these functions do not expect Delayed -- it is only permissible
      in the tail of a Cons, where it is enclosed in a reference **)

  fun null Nil = true
    | null (Cons _) = false;

  fun hd Nil = raise E
    | hd (Cons(x,_)) = x;

  fun tl Nil = raise E
    | tl (Cons(_,xp)) = pull xp;

  fun take (0, xq) = []
    | take (n, Nil) = []
    | take (n, Cons(x,xp)) = x :: take (n-1, pull xp);

  fun append (Nil,    yq) = yq
    | append (Cons(x,xp), yq) =
	  Cons(x, delay(fn()=> append(pull xp, yq)));

  fun map f Nil  = Nil
    | map f (Cons(x,xp)) = 
          Cons(f x, delay(fn()=> map f (pull xp)));

  fun filter pred Nil = Nil
    | filter pred (Cons(x,xp)) =
	  if pred x 
          then Cons(x, delay(fn()=> filter pred (pull xp)))
	  else filter pred (pull xp);

  (*idea thanks to C. Reade, see Appendix 3 of his book *)
  fun cycle seqfn =
      let val knot = ref Nil
      in  knot := seqfn (fn()=> !knot);  !knot  end;
  end;


(*** Ring Buffers, or Doubly Linked Lists ***)

signature RINGBUF = 
  sig
  type 'a T
  exception E
  val empty: unit -> '_a T
  val null: 'a T -> bool
  val label: 'a T -> 'a
  val moveleft: 'a T -> unit
  val moveright: 'a T -> unit
  val insert: '_a T * '_a -> unit
  val delete: 'a T -> 'a
  end;


structure Ringbuf : RINGBUF =
  struct
  datatype 'a buf = Empty
		  | Node of 'a buf ref * 'a * 'a buf ref;
  datatype 'a T = Ptr of 'a buf ref;

  exception E;

  fun left (Node(lp,_,_)) = lp
    | left Empty = raise E;

  fun right (Node(_,_,rp)) = rp
    | right Empty = raise E;

  fun empty() = Ptr(ref Empty);

  fun null (Ptr p) = case !p of Empty => true
			      | Node(_,x,_) => false;

  fun label (Ptr p) = case !p of Empty => raise E
			       | Node(_,x,_) => x;

  fun moveleft (Ptr p) = (p := !(left(!p)));

  fun moveright (Ptr p) = (p := !(right(!p)));

  (*Insert to left of the window, which is unchanged unless empty. *)
  fun insert (Ptr p, x) =
      case !p of
	  Empty => 
	      let val lp = ref Empty
		  and rp = ref Empty
		  val new = Node(lp,x,rp)
	      in  lp := new;  rp := new;  p := new  end
	| Node(lp,_,_) =>
	      let val new = Node(ref(!lp), x, ref(!p))
	      in  right(!lp) := new;  lp := new  end;

  (*Delete the current node, raising E if there is none. *)
  fun delete (Ptr p) =
      case !p of
	  Empty => raise E
	| Node(lp,x,rp) =>
	     (if left(!lp) = lp then p := Empty
	      else (right(!lp) := !rp;  left (!rp) := !lp;
		    p := !rp);
              x)
  end;


(**** V-arrays; see A. Aasa, S. Holmstr\"om, C. Nilsson (1988) ***)

nonfix sub;  (*required for Standard ML of New Jersey*)

signature VARRAY = 
  sig
  type 'a T
  exception E
  val array: int * '_a -> '_a T
  val reroot: 'a T -> 'a T
  val sub: 'a T * int -> 'a
  val just_update: '_a T * int * '_a -> '_a T
  val update: '_a T * int * '_a -> '_a T
  end;


structure Varray : VARRAY =
  struct
  datatype 'a T = Modif of {limit: int, 
			    index: int ref,
			    elem: 'a ref, 
			    next: 'a T ref}
		| Vector of 'a Array.array;

  exception E;

  (*create a new array containing x in locations 0 to n-1. *)
  fun array (n,x) = 
	if n <= 0  then  raise E
	else  Modif{limit=n, index=ref 0, elem=ref x, 
		    next=ref(Vector(Array.array(n,x)))};

  fun reroot (va as Modif{index, elem, next,...}) =
      case !next of
	 Vector _ => va  (*have reached root*)
       | Modif _ => 
	   let val Modif{index=bindex,elem=belem,next=bnext,...} =
		     reroot (!next)
	       val Vector vec = !bnext
	   in  bindex := !index;  
	       belem := Array.sub(vec, !index);
	       Array.update(vec, !index, !elem);
	       next := !bnext;
	       bnext := va;
	       va
	   end;

  fun sub (Modif{index,elem,next,...}, i) =
       case !next of
	  Vector vec => Array.sub(vec,i)
	| Modif _ =>    if  !index=i  then  !elem  
				      else  sub(!next,i);

  (*plain update, no rerooting*)
  fun just_update(va as Modif{limit,...}, i, x) = 
	if  0<=i andalso i<limit  
	then Modif{limit=limit, index= ref i, 
                   elem=ref x, next=ref va}
	else raise E;

  (*update and reroot*)
  fun update(va,i,x) = reroot(just_update(va,i,x));

  end;


(******** SHORT DEMONSTRATIONS ********)

(** Sequences ***)
structure Seq = ImpSeqFUN();

fun pairs (xq,yq) = 
      Seq.cons((Seq.hd xq, Seq.hd yq), 
                fn()=>pairs(Seq.tl xq, Seq.tl yq));

fun add (xq,yq): int Seq.T = Seq.map op+ (pairs(xq,yq));

val fib = Seq.cycle(fn fibf =>
    Seq.cons(1, fn()=> 
         Seq.cons(1, fn()=> 
              add(fibf(), Seq.tl(fibf())))));

(*if it returns quickly, sharing works;
  if not shared, computation time is EXPONENTIAL in length of sequence *)
Seq.take(40,fib);


