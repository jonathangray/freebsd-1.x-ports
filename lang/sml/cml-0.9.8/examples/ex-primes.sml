(* ex-primes.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * A simple stream-style program to generate primes.  This uses
 * ex-counter.sml and ex-primes.sml.
 *)

(* BEGIN EXAMPLE *)
fun primes n = let
      val ch = sieve ()
      fun loop 0 = ()
	| loop i = (CIO.print(makestring(accept ch)^"\n"); loop(i-1))
      in
	loop n
      end
(* END EXAMPLE *)
