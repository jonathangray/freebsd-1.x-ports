signature CORE_ARRAY =

(* CORE ARRAY FUNCTIONS

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           24 Jan 1991

Maintenance:    Author


DESCRIPTION

   These is the implementation of arrays agreed between the implementors
   of SML/NJ, Poly/ML and Poplog ML in Autumn 1990.  The main library
   adds more functionality.


RCS LOG

$Log: ARRAY.sml,v $
Revision 1.1  1994/02/08 00:23:22  jkh
Initial revision

Revision 1.1  1991/01/25  11:28:51  db
Initial revision


*)

sig

  eqtype 'a array

  exception Size

  exception Subscript

  val array: int * '_a -> '_a array

  val arrayoflist: '_a list -> '_a array

  val tabulate: int * (int -> '_a) -> '_a array

  val sub: 'a array * int -> 'a

  val update: 'a array * int * 'a -> unit

  val length: 'a array -> int
end
