(* ex-sieve.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * A simple stream-style program to generate primes.  This uses ex-counter.sml.
 *)

(* BEGIN EXAMPLE *)
fun sieve () = let
      val primes = channel ()
      fun filter (p, inCh) = let
	val outCh = channel()
	fun loop () = let val i = accept inCh
	      in
		if ((i mod p) <> 0) then send (outCh, i) else ();
		loop ()
	      end
	in
	  spawn loop;
	  outCh
	end
      fun head ch = let val p = accept ch
	    in
	      send (primes, p);
	      head (filter (p, ch))
	    end
      in
	spawn (fn () => head (counter 2));
	primes
      end
(* END EXAMPLE *)
