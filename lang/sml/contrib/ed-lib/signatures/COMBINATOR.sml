(*$COMBINATOR *)

signature COMBINATOR =
sig

(* COMBINATOR FUNCTIONS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		15 Nov 1989

   Copying heavily from similar signatures by Gloria Quintinilla, Rod Burstall
   and Mick Francis.

Maintenance:	Author


DESCRIPTION

   Combinators.


NOTES

   The single letter combinators have upper case names, by convention.

   I'm not sure that  can,  fby  and  over  really belong here.

   C', B' and S' are taken from Field and Harrison, "Functional Programming",
   Addison-Wesley, p287.

RCS LOG

$Log: COMBINATOR.sml,v $
Revision 1.1  1994/02/08 00:23:24  jkh
Initial revision

Revision 1.4  91/02/11  18:18:47  18:18:47  db (Dave Berry)
Added comments for curry and uncurry.

Revision 1.3  91/01/25  16:54:56  16:54:56  db (Dave Berry)
Changed signature name to all upper case, added make tag.

Revision 1.2  91/01/24  17:06:10  17:06:10  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/17  16:47:27  16:47:27  db (Dave Berry)
Initial revision


*)


(* PERVASIVES *)

  val o: ('b -> 'c) * ('a -> 'b) -> ('a -> 'c)


(* GENERAL *)

  val curry: ('a * 'b -> 'c) -> 'a -> 'b -> 'c
   (* curry f x y = f (x, y). *)

  val uncurry: ('a -> 'b -> 'c) -> 'a * 'b -> 'c
   (* uncurry f (x, y) = f x y. *)


(* NEW COMBINATORS *)

  val I: 'a -> 'a
   (* I x; the identity function -- returns x.  I x = x. *)

  val K: 'a -> 'b -> 'a
   (* K x; the constant function -- when applied to y, returns x.
      K x y = x. *)

  val C: ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
   (* C f; swaps the arguments of a curried function.  (C f) x y = f y x. *)

  val C': ('a -> 'b -> 'c) -> ('d -> 'a) -> 'b -> 'd -> 'c
   (* C' k f; like C, except that an extra argument before f is unaltered.
      C' k x y z = k (x z) y.  (C (B A E1) E2 = C' A E1 E2). *)

  val CK: 'a -> 'b -> 'b
   (* CK; returns the second of two arguments.  (CK) x y = K y x = y. *)

  val W: ('a -> 'a -> 'b) -> 'a -> 'b
   (* W f; returns a function of one argument that passes that argument to
      both arguments of f.  W f x = f x x.  *)

  val B: ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
   (* B f g; curried function composition.  B f g x = f (g x). *)

  val B': ('a -> 'b -> 'c) -> 'a -> ('d -> 'b) -> 'd -> 'c
   (* B' k f g; like B, except that an extra argument before f is unaltered.
      B' k f g x = k f (g x).  (B (A x) y = B' A x y). *)

  val S: ('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c
   (* S f g x = f x (g x). *)

  val S': ('a -> 'b -> 'c) -> ('d -> 'a) -> ('d -> 'b) -> 'd -> 'c
   (* S' k f g; like S, except that an extra argument before f is unaltered.
      S' k f g x = k (f x) (g x).  (S (B A x) y = S' A x y). *)

  val Y: (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
   (* Y f; the fixed point combinator. If F is an operation on functions 
      that returns a function of the same type, then Y F is the (least) 
      fixed point of F satisfying F(Y F) = (Y F). *)

  val cond: bool -> 'a -> 'a -> 'a
   (* cond b x y; returns x if b is true and y if b is false. *)

  (* infix 3 oo *)
  val oo: ('c -> 'd) * ('a -> 'b -> 'c) -> 'a -> 'b -> 'd
   (* f oo g; composition of a unary and curried binary function.
      (f oo g) x y = f (g x y). *)

  (* infix 3 co *)
  val co: ('b -> 'c -> 'd) * ('a -> 'b) -> 'c -> 'a -> 'd
   (* f co g; composition of a curried binary and a unary function.
      (f co g) x y = f (g y) x. *)

  val can: ('a -> 'b) -> 'a -> bool
   (* can f x; returns true if f x doesn't raise an exception, false
      otherwise. *)

  (* infix 3 fby *)
  val fby: ('a -> 'b) * ('a -> 'c) -> 'a -> 'c
   (* f fby g; an application of f to x followed by (fby for short) an
      application of g to x.  Any exception raised by f is caught and ignored.
      This function is often given the name "then", but that clashes with the
      SML keyword. *)

  (* infix 3 over *)
  val over: ('a -> 'b) * ('a -> 'b) -> 'a -> 'b
   (* (f over g) x; returns f x unless that raises an exception in which case
     it returns g x. *)

end;
