(* framed-button.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.
 *
 * Text buttons wrapped in a frame.
 *)

signature FRAMED_BUTTON = 
  sig

    structure W : WIDGET
    structure B : BUTTON
  
    type fr_button
  
    val mkFrTextBtn : W.root -> {
      border_width : int, 
      backgrnd : W.EXB.color option,
      foregrnd : W.EXB.color option,
      label : string
    } -> fr_button
  
    val mkFrTextCmd : W.root -> {
      border_width : int, 
      action : unit -> unit,
      backgrnd : W.EXB.color option,
      foregrnd : W.EXB.color option,
      label : string
    } -> fr_button
  
    val evtOf : fr_button -> B.button_act CML.event
    val widgetOf : fr_button -> W.widget
    val setActive : (fr_button * bool) -> unit
    val getActive : fr_button -> bool
  
  end (* FRAMED_BUTTON *)

structure FramedButton : FRAMED_BUTTON = 
  struct

    structure W = Widget
    structure B = Button

    datatype fr_button = FB of {
        frame : Frame.frame,
        button : Button.button
      }

    fun mkFrTextBtn root {border_width, backgrnd, foregrnd, label} = let
          val button = Button.mkTextBtn root {
                         label = label,
                         rounded = false,
                         foregrnd = foregrnd,
                         backgrnd = backgrnd
                       }
          val frame = Frame.mkFrame {
                    color = foregrnd,
                    width = border_width,
                    widget = Button.widgetOf button
                  }
        in
          FB{frame=frame,button=button}
        end

    fun mkFrTextCmd root {action, border_width, backgrnd, foregrnd, label} = let
          val button = Button.mkTextCmd root {
                         action = action,
                         label = label,
                         rounded = false,
                         foregrnd = foregrnd,
                         backgrnd = backgrnd
                       }
          val frame = Frame.mkFrame {
                    color = foregrnd,
                    width = border_width,
                    widget = Button.widgetOf button
                  }
        in
          FB{frame=frame,button=button}
        end

    fun widgetOf (FB{frame,...}) = Frame.widgetOf frame
    fun evtOf (FB{button,...}) = Button.evtOf button
    fun setActive (FB{button,...},v) = Button.setActive (button,v)
    fun getActive (FB{button,...}) = Button.getActive button

  end (* FramedButton *)
