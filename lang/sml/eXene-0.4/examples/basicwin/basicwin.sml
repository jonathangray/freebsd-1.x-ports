(* basicwin.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * This code was transcribed from a C program that is under the following copyright:
 *
 * Copyright 1989 O'Reilly and Associates, Inc.
 *)

local
  open Geometry EXeneBase

  val minWid = 300 and minHt = 200
  val minSz = SIZE{wid = minWid, ht = minHt}

(* a trace module for debugging output (see CML manual) *)
  val bounceTM = TraceCML.traceModule(XDebug.eXeneTM, "bounce")
  fun trace f = TraceCML.trace (bounceTM, f)

  fun init dpyName = let
val _ = trace(fn () => ["open display ", dpyName, "\n"])
	val dpy = openDisplay dpyName
	val scr = defaultScreenOf dpy
	val winSz = let val SIZE{wid, ht} = sizeOfScr scr
	      in
		SIZE{wid = wid quot 3, ht = ht quot 4}
	      end
	val (win, inEnv) =
	      EXeneWin.createSimpleTopWin scr {
		  geom = WGEOM{pos=PT{x=0, y=0}, sz=winSz, border=1},
		  border = blackOfScr scr,
		  backgrnd = whiteOfScr scr
		}
(** The real basicwin gets the list of icon sizes for the display here **)
	val iconTile = createTileFromImage scr IconBitmap.icon_bitmap
	in
trace(fn () => ["set props\n"]);
	  EXeneWin.setWMProperties win {
	      argv = System.argv(),
	      win_name = SOME "Basic Window Program",
	      icon_name = SOME "basicwin",
	      size_hints = [
		  ICCC.HINT_PPosition(PT{x = 0, y = 0}),
		  ICCC.HINT_PSize winSz,
		  ICCC.HINT_PMinSize minSz
		],
	      wm_hints = [ICCC.HINT_IconTile iconTile],
	      class_hints = SOME{res_name="basicwin", res_class="Basicwin"}
	    };
	  EXeneWin.mapWin win;
	  (dpy, scr, inEnv, win)
	end

  fun mkPen scr = Drawing.newPen [
	  Drawing.PV_Foreground(blackOfScr scr),
	  Drawing.PV_LineWidth 6,
	  Drawing.PV_LineStyle_OnOffDash,
	  Drawing.PV_CapStyle_Round,
	  Drawing.PV_JoinStyle_Round,
	  Drawing.PV_DashOffset 0,
	  Drawing.PV_Dash_List [12, 24]
	]

  fun loadFont dpy = Font.openFont dpy "9x15"

  fun placeText (win, pen, font, SIZE{wid, ht}) = let
val _ = trace(fn () => ["placeText:\n"])
	val drawString = Drawing.drawString (Drawing.drawableOfWin win) pen font
	val textWidth = Font.textWidth font
	val (fontHt, fontDescent) = let val {ascent, descent} = Font.fontHt font
	      in
		(ascent + descent, descent)
	      end
	fun draw (yPos, s) = let
	      val w = textWidth s
	      in
		drawString(PT{x = ((wid - w) quot 2), y = yPos}, s)
	      end
	val yOffset = (ht quot 2) - fontHt - fontDescent
	val SIZE{wid=scrWid, ht=scrHt} = sizeOfScr(EXeneWin.screenOfWin win)
	val depth = depthOfScr(EXeneWin.screenOfWin win)
	in
trace(fn () => ["placeText: draw text\n"]);
	  app draw [
	      (fontHt,			"Hi! I'm a window, who are you?"),
	      (ht - (2*fontHt),		"To terminate program: press any key"),
	      (yOffset,			"Screen Dimensions:"),
	      (yOffset + fontHt,	" Height - "^(makestring scrHt)^" pixels"),
	      (yOffset + (2*fontHt),	" Width  - "^(makestring scrWid)^" pixels"),
	      (yOffset + (3*fontHt),	" Depth  - "^(makestring depth)^" plane(s)"),
	      (ht - fontHt,		"or button while in this window")
	    ]
	end

  fun placeGraphics (win, pen, SIZE{wid=winWid, ht=winHt}) = let
val _ = trace(fn () => ["placeGraphics:\n"])
	val wid = (3 * winWid) quot 4
	val ht = winHt quot 2
	in
	  Drawing.drawRect (Drawing.drawableOfWin win) pen
	    (RECT{
		x = (winWid quot 2) - (wid quot 2),
		y = (winHt quot 2) - (ht quot 2),
		wid = wid, ht = ht
	      })
	end

  fun tooSmall (win, pen, font) = let
	val {ascent, ...} = Font.fontHt font
	in
	  Drawing.drawString (Drawing.drawableOfWin win) pen font
	    (PT{x=2, y=ascent+2}, "Too Small")
	end

  fun basicwin dpy = let
	open CML Interact
val _ = trace(fn () => ["init\n"]);
	val (dpy, scr, InEnv{m, k, ci, ...}, win) = init dpy
	val m = wrap(m, msgBodyOf)
	val k = wrap(k, msgBodyOf)
	val ci = wrap(ci, msgBodyOf)
val _ = trace(fn () => ["mkPen\n"]);
	val pen = mkPen scr
val _ = trace(fn () => ["load\n"]);
	val font = loadFont dpy
	fun quit _ = (closeDisplay dpy; RunCML.shutdown())
	fun sizeTooSmall (SIZE{wid, ht}) = (wid < minWid) orelse (ht < minHt)
	fun loop (sz) = let
	      fun handleM (MOUSE_FirstDown _) = quit()
		| handleM (MOUSE_LastUp _) = quit()
		| handleM _ = loop (sz)
	      fun handleCI (CI_Resize(RECT{wid, ht, ...})) =
		    loop (SIZE{wid=wid, ht=ht})
		| handleCI (CI_Redraw _) = (
		    if (sizeTooSmall sz)
		      then tooSmall(win, pen, font)
		      else (
			placeText(win, pen, font, sz);
			placeGraphics (win, pen, sz));
		    loop sz)
		| handleCI (CI_Die) = quit()
	      in
		sync (choose [
		    wrap(m, handleM),
		    wrap(k, quit),
		    wrap(ci, handleCI)
		  ])
	      end
	in
trace(fn () => ["go\n"]);
	  loop(minSz)
	end

in

fun doit' (flgs, dpy) = (
      XDebug.init flgs;
      RunCML.doit (
	fn () => (XDebug.xspawn("basicwin", fn () => basicwin dpy); ()),
	SOME 20))

fun doit s = doit' ([], s)

fun main (prog::server::_,_) = doit server
  | main _ = doit ""

end (* end local *)
