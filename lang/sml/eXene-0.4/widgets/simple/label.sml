(* label.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Label widget.
 *)

signature LABEL = 
  sig

    structure W : WIDGET

    type label

    val mkLabel : W.root -> {
        label : string, 
        font : string option,
        foregrnd : W.EXB.color option, 
        backgrnd : W.EXB.color option, 
        align : W.halign
      } -> label

    val widgetOf : label -> W.widget
    val setLabel : label -> string -> unit
    val setBackground : label -> W.EXB.color option -> unit
    val setForeground : label -> W.EXB.color option -> unit

  end (* LABEL *)

structure Label : LABEL = struct

  structure W = Widget

  open CML Geometry EXeneBase EXeneWin Interact Drawing Widget

  datatype rqst = 
      GetSize of bounds chan
    | SetLabel of string
    | SetBC of color option
    | SetFC of color option

  type label_state = {
    size : bounds,
    realize : (window * size) -> pen -> unit
  }

  val dfltFontName = "8x13"

  datatype label = Label of (widget * rqst chan)

  fun mkLabel root {
    label : string, 
    font : string option,
    foregrnd : color option, 
    backgrnd : color option, 
    align : halign
  } = 
  let
    val fontName = case font of NONE => dfltFontName | SOME f => f
    val initLabel = LabelView.mkLabelView (root,fontName)
    val rqstChan = channel ()
    val realizeVar : {env : in_env, win : window, sz : size} cond_var =
       condVar ()

    fun setLabel label = let
      val (SIZE{wid,ht}, rfn) = initLabel {label=label,align=align}
      val x_dim = DIM {base = wid, incr = 1, min = 0, nat = 0, max = NONE}
      val y_dim = DIM {base = ht, incr = 1, min = 0, nat = 0, max = NONE}
    in
      {size={x_dim=x_dim,y_dim=y_dim}, realize=rfn}
    end

    fun initMe (label,bc,fc) = let
      val {size,realize} = setLabel label
      in 
        {size=size,realize=realize,bc=bc,fc=fc} 
      end

    fun realizeLabel {env, win, sz} {size,realize,bc,fc} = let

      val InEnv{ci,co,...} = ignoreInput env
      val drawable = drawableOfWin win

      fun doFC c = let
        val color = 
          case c of 
            NONE => blackOfScr (screenOf root)
          | SOME c => c
        in
          newPen [PV_Foreground color]
        end handle _ => doFC NONE

      fun doBC NONE = (clearArea drawable)
        | doBC (SOME c) =
            (fillRect drawable (newPen [PV_Foreground c]))
              handle _ => doBC NONE

      fun chkSize ({x_dim,...} : bounds,RECT{wid,...}) =
          if natDim x_dim >= wid then sync (co CO_ResizeReq) else ()

      fun resetWinSz (size,realize,winsz,clearfn,txtpen) =
        init(size,realize,mkRect(originPt,winsz),realize(drawable,originPt,winsz),
          clearfn,txtpen,false)

      and resetLabel(label,winrect,clearfn,txtpen) = let
            val {size,realize} = setLabel label
          in
            chkSize(size,winrect);
            init(size,realize,winrect,realize(drawable,originPt,sizeOfRect winrect),
              clearfn,txtpen,true)
          end

      and init (size,realize,winrect,drawfn,clearfn,txtpen,update) = let

        fun draw () = (clearfn winrect; drawfn txtpen)

        fun handleCIEvt (CI_Redraw _) = draw ()
          | handleCIEvt (CI_Resize (RECT{wid,ht,...})) = 
              resetWinSz (size,realize,SIZE{wid=wid,ht=ht},clearfn,txtpen)
          | handleCIEvt (_) = ()
  
        fun handleRqst (GetSize repc) = send (repc, size)
          | handleRqst (SetLabel label) = resetLabel(label,winrect,clearfn,txtpen)
          | handleRqst (SetBC color) = 
              init(size,realize,winrect,drawfn,doBC color,txtpen,true)
          | handleRqst (SetFC color) =
              init(size,realize,winrect,drawfn,clearfn, doFC color,true)
  
        fun cmdP () = 
          cmdP (sync(choose [
            wrap (receive rqstChan, fn req => handleRqst req),
            wrap (ci, fn evt => handleCIEvt (msgBodyOf evt))
          ]))
      in
        if update then draw () else(); cmdP ()
      end
    in
      init(size,realize,mkRect(originPt,sz),realize(drawable,originPt,sz),
          doBC bc,doFC fc,false)
    end

    fun handleRqst (GetSize repc,me) = (send (repc, #size me);me)
      | handleRqst (SetLabel label,{bc,fc,...}) = initMe (label,bc,fc)
      | handleRqst (SetBC color,{size,realize,fc,...}) = 
          {size=size,realize=realize,bc=color,fc=fc}
      | handleRqst (SetFC color,{size,realize,bc,...}) =
          {size=size,realize=realize,bc=bc,fc=color}
  
    fun initLoop me =
      select [
        wrap(receive rqstChan, fn arg => initLoop(handleRqst(arg,me))),
        wrap(readVarEvt realizeVar, fn arg => realizeLabel arg me)
      ]

    fun bounds_of () = let
      val retChan = channel ()
      in
        send(rqstChan, GetSize retChan);
        accept retChan
      end

  in
    spawn (fn () => initLoop (initMe (label,backgrnd,foregrnd)));
    Label(
      mkWidget{
        root=root,
        boundsOf = bounds_of,
        realize = fn arg => writeVar(realizeVar,arg)
      },
      rqstChan
    )
  end

  fun widgetOf (Label(widget,_)) = widget
  fun setLabel (Label(_,rqstChan)) arg = (send (rqstChan, SetLabel arg))
  fun setBackground (Label(_,rqstChan)) arg = (send (rqstChan, SetBC arg))
  fun setForeground (Label(_,rqstChan)) arg = (send (rqstChan, SetFC arg))

end (* Label *)
