(* ex-simple-comm.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * A very simple example of process comunication.
 *)

(* BEGIN EXAMPLE *)
fun simple_comm () = let
      val ch = channel()
      val pr = CIO.print
      in
	pr "hi-0\n";
	spawn (fn () => (pr "hi-1\n";  send(ch, 17);  pr "bye-1\n"));
	spawn (fn () => (pr "hi-2\n";  accept ch;     pr "bye-2\n"));
	pr "bye-0\n"
      end
(* END EXAMPLE *)
