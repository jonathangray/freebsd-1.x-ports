(* main.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * This is a test diriver for the calculator.
 *)

local
  open Widget Calc Shell

  fun tester server = let
	val root = mkRoot server
	fun quit () = (delRoot root; RunCML.shutdown())
	val cal = mkCalc root
	val shell = mkShell
	      (cal, NONE, {win_name = SOME "calc", icon_name = SOME "calc"})
	in
	  init shell
	end
in

fun doit' (debugFlags, server) = (
      XDebug.init debugFlags;
      RunCML.doit (fn () => tester server, SOME 10))

fun doit s = doit'([],s)

fun main (prog::server::_,_) = doit server
  | main _ = doit ""

end (* local *)
