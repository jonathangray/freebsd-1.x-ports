(*$Combinator : COMBINATOR General *)

loadSig "COMBINATOR";

structure Combinator: COMBINATOR =

(* COMBINATOR FUNCTIONS

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        15 Nov 1989

Maintenance: Author

RCS LOG

$Log: Combinator.sml,v $
Revision 1.1  1994/02/08 00:23:17  jkh
Initial revision

Revision 1.4  91/09/13  16:46:49  16:46:49  db (Dave Berry)
Combinator.oo now equals General.oo.

Revision 1.3  91/01/25  20:16:59  20:16:59  db (Dave Berry)
Changed signature names to all upper case.
Amended tag declarations to match above change.

Revision 1.2  91/01/24  17:21:02  17:21:02  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/20  14:51:57  14:51:57  db (Dave Berry)
Initial revision


*)

struct


(* PERVASIVES *)

  val op o = op o


(* NEW COMBINATORS *)

   fun S f g x = f x (g x)
   and K x y   = x
   and I x     = x
   and B f g x = f(g x)
   and C f x y = f y x
   and CK x y  = y
   and W f x   = f x x
   and Y f     = f (Y f)
   and B' k f g x = k f (g x)
   and C' k f g x = k (f x) g
   and S' k f g x = k (f x) (g x)
   and cond true x y = x |
       cond false x y = y;

   infix 3 oo;
   val op oo = General.oo

   infix 3 co;
   fun op co (f, g) x y = f (g y) x;

   fun can f x = ((f x) ; true) handle _ => false;

   infix 3 fby;
   fun op fby (f, g) x =  ((can f x);(g x));

   infix 3 over;
   fun op over (g, f) x =  (g x) handle _ => f x;

   val curry = General.curry

   val uncurry = General.uncurry
end;     
