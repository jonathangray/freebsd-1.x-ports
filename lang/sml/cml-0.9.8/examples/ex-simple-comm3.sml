(* ex-simple-comm3.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * A simple example of process comunication with guards and abort functions.
 *)

(* BEGIN EXAMPLE *)
fun simple_comm3 () = let
      val ch1 = channel() and ch2 = channel()
      val pr = CIO.print
      in
	pr "hi-0\n";
	spawn (fn () => (pr "hi-1\n";  send(ch1, 17);  pr "bye-1\n"));
	spawn (fn () => (pr "hi-2\n";  send(ch2, 37);  pr "bye-2\n"));
	select [
          guard (fn () => (
            pr "guard-0.1\n";
            wrapAbort (wrap (receive ch1, fn _ => pr "bye-0.1\n"),
              fn () => pr "abort-0.1\n"))),
          guard (fn () => (
            pr "guard-0.2\n";
            wrapAbort (wrap (receive ch2, fn _ => pr "bye-0.2\n"),
              fn () => pr "abort-0.2\n")))
        ]
      end
(* END EXAMPLE *)
