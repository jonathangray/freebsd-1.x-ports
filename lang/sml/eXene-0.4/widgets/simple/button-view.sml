(* button-view.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Views for various buttons.
 *)

signature BUTTON_VIEW = 
  sig

    structure EXB : EXENE_BASE
    structure W : WIDGET

    type button_view

    type button_state

    val flip : button_state -> button_state
    val getState : button_state -> bool
    val setState : (bool * button_state) -> button_state
    val getActive : button_state -> bool
    val setActive : (bool * button_state) -> button_state

    datatype arrow_dir = 
      AD_Up
    | AD_Down
    | AD_Left
    | AD_Right

    val mkArrow : W.root -> {
      backgrnd : W.EXB.color option,
      dir : arrow_dir,
      foregrnd : W.EXB.color option,
      sz : int
    } -> button_view

    val mkCheck : W.root -> {color : W.EXB.color option,sz : int}
      -> button_view

    val mkIcon : W.root -> {
      backgrnd : W.EXB.color option,
      foregrnd : W.EXB.color option,
      icon : EXB.tile
    } -> button_view

    val mkText : W.root -> {
      align : W.halign,
      backgrnd : W.EXB.color option,
      foregrnd : W.EXB.color option,
      label : string
    } -> button_view

    val mkRoundedText : W.root -> {
      backgrnd : W.EXB.color option,
      foregrnd : W.EXB.color option,
      label : string
    } -> button_view

    val mkSwitch : W.root -> {
      backgrnd : W.EXB.color option,
      foregrnd : W.EXB.color option
    } -> button_view

    val mkCircle : W.root -> {
      radius : int,
      backgrnd : W.EXB.color option,
      foregrnd : W.EXB.color option
    } -> button_view

  end (* BUTTON_VIEW *)

structure ButtonView : BUTTON_VIEW = struct

  structure EXB = EXeneBase
  structure W = Widget

  open CML Geometry EXeneBase EXeneWin Interact Drawing Widget Font

  type button_state = wstate

  fun flip (Active b) = Active (not b)
    | flip (Inactive b) = Inactive (not b)
    
  fun getState (Active b) = b
    | getState (Inactive b) = b
  
  fun setState (true, Inactive _) = Inactive true
    | setState (true, Active _) = Active true
    | setState (false, Inactive _) = Inactive false
    | setState (false, Active _) = Active false

  fun getActive (Active _) = true
    | getActive _ = false

  fun setActive (true, Inactive v) = Active v
    | setActive (false, Active v) = Inactive v
    | setActive (_,s) = s

  type button_view = {
    bounds_of : unit -> bounds,
    config : window -> size -> button_state -> unit
  }

  fun inset (RECT{x,y,wid,ht},v) = RECT{x=v,y=v,wid=wid-2*v,ht=ht-2*v}

  val on_switch_data = imageFromAscii (32, [[
    "0x0C000000", "0x1B000000", "0x28C00000", "0x5A300000",
    "0xA88C0000", "0xDA230000", "0xA888FFFE", "0xDA228002",
    "0xA888AAAA", "0xDA228002", "0xA888AAAA", "0xDA228002",
    "0xAC88AAAA", "0xDF228002", "0xBFC8AAAA", "0xFFF28002",
    "0xFFFCAAAA", "0xFFFF8002", "0xFFFFFFFE", "0xFFFFFFFE",
    "0x7FFFFFFC"
  ]])

  val on_switch_mask = imageFromAscii (32, [[
    "0x0c000000", "0x1f000000", "0x3fc00000", "0x7ff00000",
    "0xfffc0000", "0xffff0000", "0xfffffffe", "0xfffffffe",
    "0xfffffffe", "0xfffffffe", "0xfffffffe", "0xfffffffe",
    "0xfffffffe", "0xfffffffe", "0xfffffffe", "0xfffffffe",
    "0xfffffffe", "0xfffffffe", "0xfffffffe", "0xfffffffe",
    "0x7ffffffc"
  ]])

  val off_switch_data = imageFromAscii (32, [[
    "0x00000060", "0x000001B0", "0x00000628", "0x000018B4",
    "0x0000622A", "0x000188B6", "0xFFFE222A", "0x800288B6",
    "0xAAAA222A", "0x800288B6", "0xAAAA222A", "0x800288B6",
    "0xAAAA226A", "0x800289F6", "0xAAAA27FA", "0x80029FFE",
    "0xAAAA7FFE", "0x8003FFFE", "0xFFFFFFFE", "0xFFFFFFFE",
    "0x7FFFFFFC"
  ]])

  val off_switch_mask = imageFromAscii (32, [[
    "0x00000060", "0x000001f0", "0x000007f8", "0x00001ffc",
    "0x00007ffe", "0x0001fffe", "0xfffffffe", "0xfffffffe",
    "0xfffffffe", "0xfffffffe", "0xfffffffe", "0xfffffffe",
    "0xfffffffe", "0xfffffffe", "0xfffffffe", "0xfffffffe",
    "0xfffffffe", "0xfffffffe", "0xfffffffe", "0xfffffffe",
    "0x7ffffffc"
  ]])

  val stippleData = (16, [[
    "0x8888", "0x2222", "0x1111", "0x4444",
    "0x8888", "0x2222", "0x1111", "0x4444",
    "0x8888", "0x2222", "0x1111", "0x4444",
    "0x8888", "0x2222", "0x1111", "0x4444"
  ]])

  val stippleData' = (16, [[
    "0x5555", "0xaaaa", "0x5555", "0xaaaa",
    "0x5555", "0xaaaa", "0x5555", "0xaaaa",
    "0x5555", "0xaaaa", "0x5555", "0xaaaa",
    "0x5555", "0xaaaa", "0x5555", "0xaaaa"
  ]])

  datatype arrow_dir = 
    AD_Up
  | AD_Down
  | AD_Left
  | AD_Right

  fun setColor root (SOME c, _) = c
    | setColor root (NONE, pix) = pix

  fun mkArrow root {dir,sz,foregrnd,backgrnd} = let
    val _ = if sz < 4 
              then LibBase.badArg{module="ButtonView",func="mkArrow",msg="sz < 4"} 
              else ()
    val scr = screenOf root
    val pad = 1
    val offset = pad

    val forec = setColor root (foregrnd, blackOfScr scr)
    val backc = setColor root (backgrnd, whiteOfScr scr)
    val stipple = createTileFromAsciiData (screenOf root) stippleData

    val forePen = newPen [PV_Foreground forec]
    val backPen = newPen [PV_Foreground backc]
    val stippleForePen = newPen [PV_Foreground forec, 
      PV_FillStyle_Stippled, PV_Stipple stipple]
    val stippleBackPen = newPen [PV_Foreground backc,
      PV_FillStyle_Stippled, PV_Stipple stipple]

    fun config win (sz as SIZE{wid,ht}) = let
      val drawable = drawableOfWin win

      val verts =
        case dir of
          AD_Up => [
            PT{x=wid div 2,y=offset},
            PT{x=offset,y=ht-offset},
            PT{x=wid-offset,y=ht-offset}
          ]
        | AD_Down => [
            PT{x=wid div 2,y=ht-offset},
            PT{x=offset,y=offset},
            PT{x=wid-offset,y=offset}
          ]
        | AD_Left => [
            PT{x=offset,y=ht div 2},
            PT{x=wid-offset,y=ht-offset},
            PT{x=wid-offset,y=offset}
          ]
        | AD_Right => [
            PT{x=wid-offset,y=ht div 2},
            PT{x=offset,y=ht-offset},
            PT{x=offset,y=offset}
          ]
      val r = RECT{x=0,y=0,wid=wid,ht=ht}

      fun draw (pen,pen') = (
        fillRect drawable pen r;
        fillPolygon drawable pen' {verts=verts,shape=ConvexShape}
      )

      fun setf (Active true) = draw (forePen, backPen)
        | setf (Active false) = draw (backPen, forePen)
        | setf (Inactive true) = draw (forePen, stippleBackPen)
        | setf (Inactive false) = draw (backPen, stippleForePen)
    in
      setf
    end

    val size = fixBounds (sz,sz)
  in
    {
      bounds_of = fn () => size, 
      config=config
    }
  end

  fun mkCheck root { 
    sz : int, 
    color : color option
  } = 
  let
    val _ = if sz < 14 
              then LibBase.badArg{module="ButtonView",func="mkCheck",msg="sz < 14"} 
              else ()
    val scr = screenOf root

    val forec = setColor root (color, blackOfScr scr)
    val stipple = createTileFromAsciiData (screenOf root) stippleData

    val boxPen = newPen [
      PV_Foreground forec, 
      PV_LineWidth 2,
      PV_JoinStyle_Miter
    ]
    val chkPen = newPen [
      PV_Foreground forec, 
      PV_LineWidth 3,
      PV_JoinStyle_Miter
    ]
    val inactiveBoxPen = updatePen(boxPen,
        [PV_FillStyle_Stippled,PV_Stipple stipple])
    val inactiveChkPen = updatePen(chkPen,
        [PV_FillStyle_Stippled,PV_Stipple stipple])

    fun config win (sz as SIZE{wid,ht}) = let
      val drawable = drawableOfWin win
      val xdelta = wid div 4
      val ydelta = ht div 4

      val boxR = RECT{x=xdelta,y=ydelta,wid=2*xdelta,ht=2*ydelta}
      val chkPts = [
        PT{x=xdelta+4,y=2*ydelta},
        PT{x=wid div 2,y=ht-(ydelta+4)},
        PT{x=(3*xdelta)+4,y=ht div 6}
      ]

      fun drawBox pen = drawRect drawable pen boxR
      fun clear () = clearDrawable drawable
      fun drawCheck pen = drawLines drawable pen chkPts

      fun setf (Active true) = (drawBox boxPen;drawCheck chkPen)
        | setf (Active false) = (clear();drawBox boxPen)
        | setf (Inactive true) = (clear();drawBox inactiveBoxPen;drawCheck inactiveChkPen)
        | setf (Inactive false) = (clear();drawBox inactiveBoxPen)
    in
      setf
    end

    val size = fixBounds (sz,sz)
  in
    {
      bounds_of = fn () => size, 
      config=config
    }
  end

  fun mkSwitch root { 
    foregrnd : color option,
    backgrnd : color option
  } = 
  let
    val scr = screenOf root

    val forec = setColor root (foregrnd, blackOfScr scr)
    val backc = setColor root (backgrnd, whiteOfScr scr)
    val stipple = createTileFromAsciiData (screenOf root) stippleData'

    val on_icon = createTileFromImage scr on_switch_data
    val off_icon = createTileFromImage scr off_switch_data
    val on_pattern = createPixmapFromImage scr on_switch_mask
    val off_pattern = createPixmapFromImage scr off_switch_mask
    val on_mask = createTileFromPixmap on_pattern
    val off_mask = createTileFromPixmap off_pattern

      (* create stippled masks *)
    val sz = sizeOfPixmap on_pattern
    val spen = newPen [PV_Stipple stipple, PV_FillStyle_OpaqueStippled,
          PV_Foreground color1, PV_Background color0 , PV_Function OP_And] 
    val _ = fillRect (drawableOfPM off_pattern) spen (mkRect(originPt,sz))
    val _ = fillRect (drawableOfPM on_pattern) spen (mkRect(originPt,sz))
    val inactive_on_mask = createTileFromPixmap on_pattern
    val inactive_off_mask = createTileFromPixmap off_pattern
(*
    val _ = destroyPixmap on_pattern
    val _ = destroyPixmap off_pattern
*)

    val {sz=SIZE{wid=twid,ht=tht},...} = geomOfTile on_icon
    val on_src = on_icon
    val off_src = off_icon
    (* val src_rect = RECT{x=0,y=0,wid=twid,ht=tht} *)

    fun config win (sz as SIZE{wid,ht}) = let
      val drawable = drawableOfWin win

          (* Compute point at which to blt centered icon *)
      val pt = PT{x=(wid-twid) div 2,y=(ht-tht) div 2}

      val onPen = newPen [PV_Foreground forec, PV_Background backc,
        PV_ClipMask on_mask, PV_ClipOrigin pt]
      val offPen = updatePen(onPen, [PV_ClipMask off_mask])
      val inactiveOnPen = updatePen(onPen, [PV_ClipMask inactive_on_mask])
      val inactiveOffPen = updatePen(offPen, [PV_ClipMask inactive_off_mask])

      fun draw (src, pen) = (
        clearDrawable drawable;
        tileBlt drawable pen {src=src,dst_pos=pt}
      )

      fun setf (Active true) = draw (on_src, onPen)
        | setf (Active false) = draw (off_src, offPen)
        | setf (Inactive true) = draw (on_src, inactiveOnPen)
        | setf (Inactive false) = draw (off_src, inactiveOffPen)
    in
      setf
    end

    val size = fixBounds(twid,tht) 
  in
    {
      bounds_of = fn () => size, 
      config=config
    }
  end

  fun mkIcon root { icon, foregrnd, backgrnd} = let
    val scr = screenOf root
    val {sz=sz as SIZE{wid=twid,ht=tht},...} = geomOfTile icon

    val forec = setColor root (foregrnd, blackOfScr scr)
    val backc = setColor root (backgrnd, whiteOfScr scr)

    val stipple = createTileFromAsciiData (screenOf root) stippleData'
    val mask = createPixmap scr (sz,1)
    val spen = newPen [PV_Stipple stipple, PV_FillStyle_OpaqueStippled,
          PV_Foreground color1, PV_Background color0]
    val _ = fillRect (drawableOfPM mask) spen (mkRect(originPt,sz))
    val mask = createTileFromPixmap mask

    val forePen = newPen [PV_Foreground forec, PV_Background backc]
    val backPen = newPen [PV_Foreground backc, PV_Background forec]
    val inactiveForePen = updatePen (forePen, [PV_ClipMask mask])
    val inactiveBackPen = updatePen (backPen, [PV_ClipMask mask])

    val src = icon
    (* val src_rect = RECT{x=0,y=0,wid=twid,ht=tht} *)

    fun config win (sz as SIZE{wid,ht}) = let
      val drawable = drawableOfWin win

          (* Compute point at which to blt centered icon *)
      val pt = PT{x=(wid-twid) div 2,y=(ht-tht) div 2}

      fun draw pen = tileBlt drawable pen {src=src,dst_pos=pt}

      fun setf (Active true) = draw backPen
        | setf (Active false) = draw forePen
        | setf (Inactive true) = (clearDrawable drawable;draw inactiveBackPen)
        | setf (Inactive false) = (clearDrawable drawable;draw inactiveForePen)
    in
      setf
    end

    val size = fixBounds (twid,tht) 
  in
    {
      bounds_of = fn () => size, 
      config=config
    }
  end

  fun mkText root {label,foregrnd,backgrnd,align} = let 
    val fontName = "8x13"
    val hpad = 1
    val (SIZE{wid=lwid,ht=lht}, configLabel) = 
      LabelView.mkLabelView (root, fontName) {label=label,align=align}
    val scr = screenOf root

    val forec = setColor root (foregrnd, blackOfScr scr)
    val backc = setColor root (backgrnd, whiteOfScr scr)
    val stipple = createTileFromAsciiData (screenOf root) stippleData'

    val forePen = newPen [PV_Foreground forec, PV_Background backc]
    val backPen = newPen [PV_Foreground backc, PV_Background forec]
    val inactiveForePen = updatePen(forePen, [PV_FillStyle_OpaqueStippled,
      PV_Stipple stipple])
    val inactiveBackPen = updatePen(backPen, [PV_FillStyle_OpaqueStippled,
      PV_Stipple stipple])

    fun config win (sz as SIZE{wid,ht}) = let
      val drawable = drawableOfWin win
      val r = RECT{x=0,y=0,wid=wid,ht=ht}
      val drawTxt = configLabel (drawable,PT{x=hpad,y=0},SIZE{wid=wid-2*hpad,ht=ht})

      fun draw (pen,pen') = (
        fillRect drawable pen r;
        drawTxt pen'
      )

      fun setf (Active true) = draw (forePen, backPen)
        | setf (Active false) = draw (backPen, forePen)
        | setf (Inactive true) = draw (forePen, inactiveBackPen)
        | setf (Inactive false) = draw (backPen, inactiveForePen)
    in
      setf
    end

    val size = { 
      x_dim = DIM{base=lwid+2*hpad,incr=1,min=0,nat=1,max=NONE},
      y_dim = DIM{base=lht,incr=1,min=0,nat=1,max=NONE}
    }
  in
    {
      bounds_of = fn () => size, 
      config=config
    }
  end

  fun mkRoundedText root { label, foregrnd, backgrnd} = let
    val bordw = 1
    val pad = 4
    val maxrad = 10
    val fontName = "8x13"
    val (SIZE{wid=lwid,ht=lht}, configLabel) = 
      LabelView.mkLabelView (root, fontName) {label=label,align=HCenter}
    val scr = screenOf root

    val forec = setColor root (foregrnd, blackOfScr scr)
    val backc = setColor root (backgrnd, whiteOfScr scr)
    val stipple = createTileFromAsciiData (screenOf root) stippleData'

    val forePen = newPen [PV_Foreground forec, PV_Background backc]
    val backPen = newPen [PV_Foreground backc, PV_Background forec]
    val inactiveForePen = updatePen(forePen, [PV_FillStyle_OpaqueStippled,
      PV_Stipple stipple])
    val inactiveBackPen = updatePen(backPen, [PV_FillStyle_OpaqueStippled,
      PV_Stipple stipple])

    fun config win (sz as SIZE{wid,ht}) = let

          (* Compute point at which to draw centered text *)
      val dst = drawableOfWin win
      val xmax = wid-1
      val ymax = ht-1
      val rad = min(10, min(wid, ht) div 6)
      val pts = [
          PT{x = 0, y = rad},
          PT{x = 0, y = rad + rad},
          PT{x = 0, y = ymax - rad - rad},
          PT{x = 0, y = ymax - rad},
          PT{x = rad, y = ymax},
          PT{x = rad + rad, y = ymax},
          PT{x = xmax - rad - rad, y = ymax},
          PT{x = xmax - rad, y = ymax},
          PT{x = xmax, y = ymax - rad},
          PT{x = xmax, y = ymax - rad - rad},
          PT{x = xmax, y = rad + rad},
          PT{x = xmax, y = rad},
          PT{x = xmax - rad, y = 0},
          PT{x = xmax - rad - rad, y = 0},
          PT{x = rad + rad, y = 0},
          PT{x = rad, y = 0}
        ]
      val splinePts = Spline.closedBSpline pts
      val drawTxt = configLabel (dst,originPt,sz)

      fun draw_on txtpen = (
         fillPolygon dst forePen {verts=splinePts,shape=ConvexShape};
         drawLines dst forePen splinePts;
         drawTxt txtpen
      )

      fun draw_off txtpen = (
         fillPolygon dst backPen {verts=splinePts,shape=ConvexShape};
         drawLines dst forePen splinePts;
         drawTxt txtpen
      )

      fun setf (Active true) = draw_on backPen
        | setf (Active false) = draw_off forePen
        | setf (Inactive true) = draw_on inactiveBackPen
        | setf (Inactive false) = draw_off inactiveForePen
    in
      setf
    end

    val size = 
      let
        val natht = lht+2*(pad+bordw)
        val rad = natht div 2
      in
        {
          x_dim=DIM{base=lwid+2*(pad+maxrad),incr=1,min=0,nat=0,max=NONE},
          y_dim=DIM{base=lht+2*(pad+bordw),incr=1,min=0,nat=0,max=NONE}
        }
      end
  in
    {
      bounds_of = fn () => size, 
      config=config
    }
  end

  fun mkCircle root { 
    radius : int, 
    foregrnd : color option,
    backgrnd : color option
  } = 
  let
    val _ = 
      if radius < 4 
        then LibBase.badArg{module="ButtonView",func="mkCircle",msg="radius < 4"} 
        else ()
    val scr = screenOf root

    val forec = setColor root (foregrnd, blackOfScr scr)
    val backc = setColor root (backgrnd, whiteOfScr scr)
    val stipple = createTileFromAsciiData (screenOf root) stippleData

    val forePen = newPen [PV_Foreground forec, PV_LineWidth 2]
    val inactiveForePen = updatePen(forePen,
        [PV_FillStyle_Stippled,PV_Stipple stipple])
    val backPen = newPen [PV_Foreground backc]
    val inactiveBackPen = updatePen(backPen,
        [PV_FillStyle_Stippled,PV_Stipple stipple])

    fun config win (sz as SIZE{wid,ht}) = let
      val drawable = drawableOfWin win

          (* Center point *)
      val pt = PT{x=(wid div 2),y=(ht div 2)}

      val circle = {center=pt,rad=radius}

      fun clear () = clearDrawable drawable

      fun draw (fillpen,drawpen) = (
        fillCircle drawable fillpen circle;
        drawCircle drawable drawpen circle
      )

      fun setf (Active true) = draw (forePen,forePen)
        | setf (Active false) = draw (backPen,forePen)
        | setf (Inactive true) = 
            (clear ();draw (inactiveForePen,inactiveForePen))
        | setf (Inactive false) = 
            (clear ();draw (inactiveBackPen,inactiveForePen))
    in
      setf
    end

    val size = fixBounds (2+(2*radius),2+(2*radius))
  in
    {
      bounds_of = fn () => size, 
      config=config
    }
  end

end (* ButtonView *)
