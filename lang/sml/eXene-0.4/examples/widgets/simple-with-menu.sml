(* simple-with-menu.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Test the simple menu package.
 *)

local
  open Widget Box Button Shell SimpleMenu

  val menu1 = MENU[
	  MenuItem("item-1", 1),
	  MenuItem("item-2", 2),
	  MenuItem("item-3", 3),
	  Submenu("submenu1", MENU[
	      MenuItem("item-4", 4),
	      MenuItem("item-5", 5),
	      MenuItem("item-6", 6)		    
	    ]),
	  MenuItem("item-7", 7)
	]
		  
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
	val (widget, evt) = attachMenu (Box.widgetOf layout, [Interact.MButton 3], menu1)
	fun monitor () = let
	      val n = CML.sync evt
	      in
		CIO.print("menu choice "^ makestring n ^ "\n");
		monitor ()
	      end
        val shell = mkShell (widget, NONE, {win_name = NONE, icon_name = NONE})
        fun loop () =
              if (CIO.input_line CIO.std_in) = "quit\n"
                then quit ()
                else loop ()
        in
	  CML.spawn monitor;
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

