(* calc.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * The calculator interface.
 *)

signature CALC =
  sig
    structure W : WIDGET
    val mkCalc : W.root -> W.widget
  end (* CALC *)

structure Calc : CALC =
  struct

    structure W = Widget

    open CML Geometry Widget Box Button Toggle Frame Label Shape
    open Acc

    val blackc = EXeneBase.RGB{red=0,green=0,blue=0}

    fun mkDpyLine root w = let
          val scr = screenOf root
	  val dpy = mkFrame {
		  widget = mkRigid w,
		  color = SOME (EXeneBase.blackOfScr scr),
		  width = 2
		}
	  in
	    HzCenter [
	        Glue{nat=5, min=0, max=NONE},
	        WBox (Frame.widgetOf dpy),
	        Glue{nat=5, min=0, max=NONE}
	      ]
	  end

    fun mkSwitchLine sw = HzCenter [
	    Glue{nat=5, min=0, max=NONE},
	    WBox sw,
	    Glue{nat=5, min=5, max=SOME 5}
	  ]

    fun mkLine root itemlist = let
          val blackc = EXB.blackOfScr (screenOf root)
	  val hglue = Glue {nat=5, min=5, max=SOME 5}
	  fun addBox ((name,act), l) = let
		val fw = FramedButton.mkFrTextCmd root {
			   action = act,
			   foregrnd = SOME blackc,
			   backgrnd = NONE,
			   label = name,
                           border_width = 1
                         }
		in
		  hglue::(WBox (FramedButton.widgetOf fw))::l
		end
	  val boxlist = fold addBox itemlist [hglue]
	  in
	    (HzCenter boxlist)
	  end

    fun mkCalc root = let
	  val display = mkLabel root {
		  label = "          0",
		  foregrnd = NONE,
		  backgrnd = NONE,
		  font = NONE,
		  align = HRight
		}
	  val display_line = mkDpyLine root (Label.widgetOf display)
	  val pause = TIME{sec=0,usec=500000}
	  fun quit () = spawn (fn () => (
		    sync(timeout pause); delRoot root; RunCML.shutdown()))
	  val sw = Toggle.widgetOf (mkToggleSwitch root {
		  action = fn _ => (quit ();()),
		  state = Widget.Active false,
		  backgrnd = NONE,
		  foregrnd = NONE
		})
	  val switch_line = mkSwitchLine sw
	  val acc = mkAcc ()
	  val send_acc = sendAcc acc
	  fun printer () = let
		val acc_evt = Acc.evtOf acc
		val set = setLabel display
		fun loop () = loop (case (sync acc_evt)
		       of OVal v => set (makestring v)
			| OInfinity => set "Infinity"
			| OOverflow => set "Overflow"
		      (* end case *))
		in
		  loop ()
		end (* printer *)
	  fun opfn msg () = send_acc msg
	  val line1 = mkLine root [
		  ("7", opfn (Val 7)), ("8", opfn (Val 8)),
		  ("9", opfn (Val 9)), ("+", opfn (Op Plus))]
	  val line2 = mkLine root [
		  ("4", opfn (Val 4)), ("5", opfn (Val 5)),
		  ("6", opfn (Val 6)), ("-", opfn (Op Minus))]
	  val line3 = mkLine root [
		  ("1", opfn (Val 1)), ("2", opfn (Val 2)),
		  ("3", opfn (Val 3)), ("*", opfn (Op Times))]
	  val line4 = mkLine root [
		  ("C", opfn (Clear)), ("0", opfn (Val 0)),
		  ("=", opfn (Equal)), ("/", opfn (Op Divide))]
	  val vglue = Glue {nat=5, min=1, max=NONE}
	  in
	    spawn printer;
	    Box.widgetOf (mkLayout root (VtCenter [
		vglue,
		display_line,
		vglue,
		switch_line,
		vglue,
		line1,
		vglue,
		line2,
		vglue,
		line3,
		vglue,
		line4,
		vglue
	      ]))
	  end (* end mkCalc *)

  end (* Calc *)
