(* ex-future.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * Multi-lisp style futures.
 *)

(* BEGIN EXAMPLE *)
fun future f x = let
      datatype 'a msg_t = RESULT of 'a | EXN of exn
      val resCh = channel()
      fun repeater x = (send(resCh, x); repeater x)
      in
        spawn (fn () => repeater(RESULT(f x) handle ex => EXN ex));
        wrap (
          receive resCh,
          fn (RESULT x) => x | (EXN ex) => raise ex)
      end
(* END EXAMPLE *)
