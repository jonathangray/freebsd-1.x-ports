(* scrollport.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * ScrollPort widget, for panning over a child widget
 * using scrollbars.
 *
 * TODO:
 *   granularity
 *)

structure ScrollPort : SCROLL_PORT =
  struct

    structure W = Widget
    structure CML = CML

    open CML Geometry EXeneBase EXeneWin Interact Widget Viewport

    datatype scroll_port = SP of {widget : widget}

    val scrollWid = 12

    fun mkVScroll (_, _, NONE) = (NONE, NONE)
      | mkVScroll (root, color, SOME{left}) = let
	  open Scrollbar
	  val sb = mkVScrollbar root {color=color,sz=scrollWid} 
	  val line = Divider.mkVertDivider root {color=color,width=1}
	  val s = if left
		then Box.mkLayout root (Box.HzCenter[
		    Box.Glue{nat=1, min=1, max=SOME 1},
		    Box.WBox (widgetOf sb),
		    Box.Glue{nat=1, min=1, max=SOME 1},
		    Box.WBox line
		  ])
		else Box.mkLayout root (Box.HzCenter[
		    Box.WBox line,
		    Box.Glue{nat=1, min=1, max=SOME 1},
		    Box.WBox (widgetOf sb),
		    Box.Glue{nat=1, min=1, max=SOME 1}
		  ])
	  in
	    (SOME sb, SOME{sb=Box.widgetOf s,pad=1,left=left})
	  end

  fun mkHScroll (_, _, NONE) = (NONE, NONE)
    | mkHScroll (root, color, SOME{top}) = let
	  open Scrollbar
	  val sb = mkHScrollbar root {color=color,sz=scrollWid} 
	  val line = Divider.mkHorzDivider root {color=color,width=1}
	  val s = if top
		then Box.mkLayout root (Box.VtCenter[
		    Box.Glue{nat=1, min=1, max=SOME 1},
		    Box.WBox (widgetOf sb),
		    Box.Glue{nat=1, min=1, max=SOME 1},
		    Box.WBox line
		  ])
		else Box.mkLayout root (Box.VtCenter[
		    Box.WBox line,
		    Box.Glue{nat=1, min=1, max=SOME 1},
		    Box.WBox (widgetOf sb),
		    Box.Glue{nat=1, min=1, max=SOME 1}
		  ])
	  in
	    (SOME sb, SOME{sb=Box.widgetOf s, pad=1, top=top})
	  end

    fun monitor (continuous, scrollb, setview, geomEvt) = let
	  open Scrollbar

	  val set = setVals scrollb
	  val scrollEvt = evtOf scrollb

	  fun init (origin, sz, total) = let
		val r_total = real total
		val r_sz = real sz
		val maxo = total-sz

	        fun shiftUp (r, y) = let
		      val y' = y+min(maxo-y, truncate((1.0-r)*r_sz))
		      in
		  	if (y = y')
			  then y
			  else (
			    setview y';
			    set{top = SOME((real y')/r_total), sz = NONE};
			    y')
			      handle _ => y
		      end

		fun shiftDown (r, y) = let
		      val y' = max(0,y-truncate(r*r_sz))
		      in
			if (y = y')
			  then y
			  else (
			    setview y';
			    set{top = SOME((real y')/r_total), sz = NONE};
			    y') 
			      handle _ => y
		    end

		fun adjust (r,y) = let
		      val y' = truncate(r*r_total)
		      in
			if (y = y') then y else ((setview y'; y') handle _ => y)
		      end

		fun handle_sb adjustfn arg = (case arg
		       of (ScrStart r,y) => adjustfn(r,y)
			| (ScrUp r,y) => shiftUp(r,y)
			| (ScrDown r,y) => shiftDown(r,y)
			| (ScrMove r,y) => adjustfn(r,y)
			| (ScrEnd r,y) => adjust(r,y)
		    (* end of case *))

		val handleSB = if continuous
		      then (handle_sb adjust)
		      else (handle_sb (fn (_, y) => y))

		fun loop origin = select [
		      wrap (scrollEvt, fn evt => loop (handleSB (evt, origin))),
		      wrap (geomEvt, init)
		    ]
		in
		  set {sz = SOME(r_sz/r_total), top = SOME((real origin)/r_total)};
		  loop origin
		end (* init *)
	  in
	    init (0,1,1)
	  end (* monitor *)

    fun main (vp,vchan,hchan) = let
	  val vpEvt = evtOf vp
	  fun loop () = let
		val {rect=RECT{x,y,wid,ht},childSz=SIZE sz} = sync vpEvt
		in
		  send (vchan, (y,ht, #ht sz));
		  send (hchan, (x,wid, #wid sz));
		  loop ()
		end
	  in
	    loop ()
	  end (* main *)

    fun mkScrollPort {widget, continuous, color, hsb, vsb} = let
	  val vp = mkViewport widget
          val root = rootOf widget
	  val hsbval = mkHScroll (root, color, hsb)
	  val vsbval = mkVScroll (root, color, vsb)
	  val widget = Box.widgetOf(ScrollLayout.mkSBLayout root {
		  widget = widgetOf vp,
		  hsb = #2 hsbval,
		  vsb = #2 vsbval
		})
	  val scr = screenOf root
	  fun realizeScrollPort arg = let
		val vchan = channel () and hchan = channel ()
		fun dummy ch = (accept ch; dummy ch)
		fun spawnMonitor (NONE, sv, ch) = spawn (fn () => dummy ch)
		  | spawnMonitor (SOME sb, sv, ch) = 
		      spawn (fn () => monitor(continuous,sb,sv,receive ch))
		in
		  spawn (fn () => main (vp,vchan,hchan));
		  spawnMonitor (#1 vsbval, setVert vp, vchan);
		  spawnMonitor (#1 hsbval, setHorz vp, hchan);
		  realizeFn widget arg
		end (* realizeScrollPort *)
	  in
	    SP {
		widget = mkWidget{
		    root = root, 
		    boundsOf = boundsFn widget,
		    realize = realizeScrollPort
		  }
	    }
	  end (* mkScrollPort *)

    fun widgetOf (SP{widget,...}) = widget

  end (* ScrollPort *)

