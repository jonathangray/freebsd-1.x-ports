(*$General: GENERAL *)

loadSig "GENERAL";

structure General: GENERAL =

(* GENERAL DEFINITIONS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:	        21 Sep 89

Maintenance:	Author

DESCRIPTION

   SML/NJ provides the Overflow function.


RCS LOG

$Log: General.sml,v $
Revision 1.1  1994/02/08 00:23:14  jkh
Initial revision

Revision 1.1  91/09/13  14:18:17  14:18:17  db (Dave Berry)
Initial revision



*)

struct


(* PERVASIVES *)

  exception Bind = Bind
  and Match = Match
  and Interrupt = Interrupt

  type unit = unit
  and  exn = exn

  val op o = op o
  val op <> = op <>

  exception Overflow = Overflow
  and OldDiv = Div


(* TYPES *)

  datatype 'a Option = None | Some of 'a

  datatype ('a, 'b) Result = OK of 'a | Fail of 'b

  type Nat = int


  exception Nat of string * int

  exception NotImplemented of string


(* FUNCTIONS *)

  fun id x = x

  fun curry f x y = f (x, y)

  fun uncurry f (x, y) = f x y

  infix 3 oo;
  fun op oo (f, g) x y = f (g x y);

  infix 0 before
  fun x before _ = x;

  local
    fun iterate' 0 _ x = x
    |   iterate' n f x = iterate' (n-1) f (f x)
  in
    fun iterate n f x =
	  if n < 0 then raise Nat ("iterate", n)
	  else iterate' n f x
  end

  local
    fun repeat' 0 _ _ = ()
    |   repeat' n f x = (f x; repeat' (n-1) f x)
  in
    fun repeat n f x =
	  if n < 0 then raise Nat ("repeat", n)
	  else repeat' n f x
  end

  local
    fun primRec' _ x 0 = x
    |   primRec' f x n = primRec' f (f x n) (n-1)
  in
    fun primRec f x n =
	  if n < 0 then raise Nat ("primRec", n)
	  else primRec' f x n
  end

  fun until p f x =
    if p x then x
    else until p f (f x);
end
