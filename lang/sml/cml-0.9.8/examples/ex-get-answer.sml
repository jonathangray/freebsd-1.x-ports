(* ex-get-answer.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *)

(* BEGIN EXAMPLE *)
fun getAnswer (question, t) = let open CML CIO
      in
	print question; flush_out std_out;
	select [
	    wrap (inputLineEvt std_in, SOME),
	    wrap (timeout t, fn () => NONE)
	  ]
      end
(* END EXAMPLE *)
