(* label-slider.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *)

local
  open Widget CML Box Label Shell
  structure Sl = Slider
in
  fun mkLabelSlide root (wid, color) = let
        val label = mkLabel root {
          label="  0",
          foregrnd=color,
          backgrnd=NONE,
          font=NONE,
          align=HRight
        }
        val slider = Sl.mkHSlider root {
          foregrnd=color,
          wid=wid,
          init=0,
          scale=100
        }
        val set = setLabel label
        val evt = Sl.evtOf slider
        fun loop () = loop (set (makestring (sync evt)))
        in
          spawn loop;
          Box.widgetOf(mkLayout root (HzCenter [
              (* Glue {nat=20, min=0, max=NONE}, *)
              WBox (widgetOf label), 
              Glue {nat=20, min=20, max=SOME 20},
              WBox (Sl.widgetOf slider)
            ]))
        end

  fun tester server = let
        val root = mkRoot server
        fun quit () = (delRoot root; RunCML.shutdown())
        val lslider = mkLabelSlide root (20, NONE)
	val layout = mkLayout root (VtCenter [
		WBox lslider,
		HzCenter [Glue{nat=300, min=0, max=NONE}]
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
end

fun doit' (debugFlags, server) = (
      XDebug.init debugFlags;
      RunCML.doit (fn () => tester server, SOME 20))

fun doit s = doit'([],s)

fun main (prog::server::_,_) = doit server
  | main _ = doit ""


