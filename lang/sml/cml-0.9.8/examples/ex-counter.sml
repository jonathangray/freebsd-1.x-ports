(* ex-counter.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * A simple stream-style program to generate primes.
 *)

(* BEGIN EXAMPLE *)
fun counter start = let
      val ch = channel()
      fun count i = (send(ch, i); count(i+1))
      in
        spawn (fn () => count start);
	ch
      end
(* END EXAMPLE *)
