signature MAKE_GLOBAL =

(* GLOBAL DEFINITIONS

Created by:     Nick Rothwell, LFCS, University of Edinburgh
                nick@lfcs.ed.ac.uk
Date:           30 Oct 1990

                Modified to fit the library structure by Dave Berry,
                24 Jan 1991.

Maintenance:    Author


DESCRIPTION

   This is part of the portable make system.


RCS LOG

$Log: GLOBAL.sml,v $
Revision 1.1  1994/02/08 00:23:21  jkh
Initial revision

Revision 1.3  91/01/25  20:13:22  20:13:22  db (Dave Berry)
Prefixed local signature and functor names with MAKE_ or Make respectively.

Revision 1.2  91/01/25  15:48:17  db
Moved option, substring and fold here from CoreUtils.
Deleted disj_sum.

Revision 1.1  91/01/25  11:41:02  11:41:02  db (Dave Berry)
Initial revision


*)

sig
  structure CoreUtils: CORE_UTILS
  type 'a pair
  type 'a triple
  type 'a predicate
  type 'a procedure
  type ('a, 'b) currying
  type 'a mapping
  type 'a generator
  type 'a relation

  val never: 'a predicate

  val fst: 'a * 'b -> 'a
  val snd: 'a * 'b -> 'b

  datatype 'a option = NONE | SOME of 'a

  val substring: string * int * int -> string

  val fold: ('b * 'a -> 'a) -> 'b list -> 'a -> 'a

  val inputLine: instream -> string

end;

