(* button.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Common buttons.
 *)

signature BUTTON = 
  sig

    structure CML : CONCUR_ML
    structure W : WIDGET
    structure Interact : INTERACT

    datatype arrow_dir = 
      AD_Up
    | AD_Down
    | AD_Left
    | AD_Right

    datatype button_act = 
      BtnDown of Interact.mbutton 
    | BtnUp of Interact.mbutton
    | BtnExit

    type button

    val evtOf : button -> button_act CML.event
    val widgetOf : button -> W.widget
    val setActive : (button * bool) -> unit
    val getActive : button -> bool

    val mkArrowBtn : W.root -> {
      backgrnd : W.EXB.color option,
      dir : arrow_dir,
      foregrnd : W.EXB.color option,
      sz : int
    } -> button

    val mkArrowCmd : W.root -> {
      action : unit -> unit,
      backgrnd : W.EXB.color option,
      dir : arrow_dir,
      foregrnd : W.EXB.color option,
      sz : int
    } -> button

    val mkTextBtn : W.root -> {
      rounded : bool,
      backgrnd : W.EXB.color option,
      foregrnd : W.EXB.color option,
      label : string
    } -> button

    val mkTextCmd : W.root -> {
      rounded : bool,
      action : unit -> unit,
      backgrnd : W.EXB.color option,
      foregrnd : W.EXB.color option,
      label : string
    } -> button

  end (* BUTTON *)

structure Button : BUTTON = struct

  structure CML = CML
  structure W = Widget
  structure Interact = Interact

  open CML Geometry EXeneBase EXeneWin Interact Drawing Widget
  open Font ButtonView ButtonCtrl

  fun textView (root,true, label, forec, backc) =
        mkRoundedText root {backgrnd=backc,foregrnd=forec, label = label}
    | textView (root,false, label, forec, backc) =
        mkText root {align=HCenter,backgrnd=backc,foregrnd=forec, label = label}

  fun mkTextBtn root {
        rounded : bool,
        label : string, 
        foregrnd : color option,
        backgrnd : color option
      } = mkButton root (true,textView(root,rounded,label,foregrnd,backgrnd))

  fun mkTextCmd root {
        rounded : bool,
        label : string, 
        action : unit -> unit,
        foregrnd : color option,
        backgrnd : color option
      } = mkCommandBtn root (fn _ => action (),
            textView(root,rounded,label,foregrnd,backgrnd))

  fun mkArrowCmd root {
        dir : arrow_dir, 
        sz : int,
        action : unit -> unit,
        foregrnd : color option,
        backgrnd : color option
      } = let
        val view = mkArrow root 
              {backgrnd=backgrnd,foregrnd=foregrnd,sz=sz,dir=dir}
        in
          mkCommandBtn root (fn _ => action (),view)
        end

  fun mkArrowBtn root {
        dir : arrow_dir, 
        sz : int,
        foregrnd : color option,
        backgrnd : color option
      } = let
        val view = mkArrow root 
              {backgrnd=backgrnd,foregrnd=foregrnd,sz=sz,dir=dir}
        in
          mkButton root (true,view)
        end

end (* Button *)
