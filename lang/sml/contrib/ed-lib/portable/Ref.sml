(*$Ref: REF *)

loadSig "REF";

structure Ref: REF =
struct

(* REFERENCES

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:	        3 Oct 1989

Maintenance:	Author


RCS LOG

$Log: Ref.sml,v $
Revision 1.1  1994/02/08 00:23:20  jkh
Initial revision

Revision 1.5  91/02/11  20:43:50  20:43:50  db (Dave Berry)
Added type synonym 'a T as part of the major reorganisation of the library.

Revision 1.4  91/01/31  17:47:58  17:47:58  db (Dave Berry)
Added type.

Revision 1.3  91/01/25  20:19:28  20:19:28  db (Dave Berry)
Changed signature names to all upper case.
Amended tag declarations to match above change.

Revision 1.2  91/01/24  17:25:29  17:25:29  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/20  15:03:19  15:03:19  db (Dave Berry)
Initial revision


*)


(* PERVASIVES *)

  type 'a ref = 'a ref

  val ! = !
  val (op :=) = (op :=)


(* TYPES *)

  type 'a T = 'a ref


(* OBSERVERS *)

  fun eq x y = (x = y)
  fun ne x y = (x <> y)


(* MANIPULATORS *)

  fun inc r = (r := !r + 1)

  fun dec r = (r := !r - 1)

  (* Given a seed, mkRandom returns a psuedo-random number generator
     which takes an integer argument of one more than the maximum
     return value required. ( Linear Congruential, after Sedgewick,
     "Algorithms", Addison-Wesley, 1983, Chapter 3 pp 37-38.) *)

  fun mkRandom seed = 
        let infix r_mod r_div
	    fun A r_div B = real (floor (A/B))
	    fun A r_mod B = A - ((A r_div B) * B)
	    val r = ref (real seed)
	    val a = 31415821.0
	    val m = 100000000.0

	    fun f n =
	      let val rand = (r := ((a * !r + 1.0) r_mod m);
			      (!r * real n) r_div m)
	      in floor (if n < 0 then rand + 1.0 else rand)
	      end
         in f
        end;

end
