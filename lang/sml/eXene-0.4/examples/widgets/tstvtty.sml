(* test the vtty widget *)

local
  open CML EXeneBase Widget Vtty
  open CIO

  fun tester server = let
        val root = mkRoot server
        fun quit () = (delRoot root; RunCML.shutdown())

        val vtty = mkVtty root {rows = 24, cols = 80}
        val (ins, outs) = openVtty vtty
        val shell = Shell.mkShell (widgetOf vtty, NONE, {
		win_name = SOME "test", icon_name = SOME "test"
	      })

	fun catFile fname = let
	      val inf = open_in fname
              fun outF () = (case (can_input inf)
		     of 0 => ()
		      | n => (output(outs,input_line(inf)); outF ())
		      (* | n => (output(outs,input(inf,n)); outF ()) *)
		      (* | n => (let val s = input(inf,n) in *)
			  (* output(std_out,s); output(outs,s) end; outF ()) *)
		    (* end case *))
	      in
		outF ();
		close_in inf
	      end
		handle (Io msg) => output (outs, (msg^"\n"))
	fun cat [] = output (outs,"cat: missing file name\n")
	  | cat files = app catFile files
	fun loop () = let
	      val _ = (output (outs,"> "); flush_out outs)
	      val line = input_line ins
	      fun doCmd ("cat"::t) = cat t
		| doCmd ("quit"::_) = quit ()
		| doCmd ("help"::_) = 
		    output (outs,"Commands: cat <files>,quit,help\n")
		| doCmd (s::_) = 
		    output (outs,"Unknown command: " ^ s ^ "\n")
		| doCmd [] = ()
	      in
		doCmd (StringUtil.tokenize " \t\n" (line,0));
		loop ()
	      end
	in
	  Shell.init shell;
	  loop ()
	end

in

fun doit' (debugFlags, server) = (
      XDebug.init debugFlags;
      RunCML.doit (fn () => tester server, SOME 20))

fun doit s = doit'([],s)

fun main (prog::server::_,_) = doit server
  | main _ = doit ""

end (* local *)
