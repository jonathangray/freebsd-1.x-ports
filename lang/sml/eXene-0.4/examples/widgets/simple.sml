(* simple.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *)

local
  open Widget Box Button Shell
  fun goodbye server = let
        val root = mkRoot server
        fun quit () = (delRoot root; RunCML.shutdown())
        val layout = mkLayout root (VtCenter [
                Glue {nat=30, min=0, max=NONE},
                WBox (FramedButton.widgetOf (FramedButton.mkFrTextCmd root {
		   action = quit,
                   foregrnd = SOME(EXB.blackOfScr(screenOf root)),
		   backgrnd = NONE,
                   label="Goodbye, Cruel World!", 
                   border_width = 1
                })),
                Glue {nat=30, min=0, max=NONE}
              ])
        val shell = mkShell (Box.widgetOf layout, NONE, {win_name = NONE, icon_name = NONE})
        fun loop () =
              if (CIO.input_line CIO.std_in) = "quit\n"
                then quit ()
                else loop ()
        in
          init shell;
          loop ()
        end

in
fun doit' (debugFlags, server) = (
      XDebug.init debugFlags;
      RunCML.doit (fn () => goodbye server, SOME 20))

fun doit s = doit'([],s)

fun main (prog::server::_,_) = doit server
  | main _ = doit ""

end

