(* framed-toggle.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.
 *
 * Text toggle wrapped in a frame.
 *)

signature FRAMED_TOGGLE = 
  sig

    structure W : WIDGET
  
    type fr_toggle
  
    val mkFrToggleText : W.root -> {
      state : W.wstate,
      border_width : int, 
      action : bool -> unit,
      backgrnd : W.EXB.color option,
      foregrnd : W.EXB.color option,
      label : string
    } -> fr_toggle
  
    val widgetOf : fr_toggle -> W.widget
    val getState : fr_toggle -> bool
    val setState : fr_toggle * bool -> unit
    val setActive : fr_toggle * bool -> unit
    val getActive : fr_toggle -> bool
  
  end (* FRAMED_TOGGLE *)

structure FramedToggle : FRAMED_TOGGLE = 
  struct

    structure W = Widget
    structure B = Button

    datatype fr_toggle = FT of {
        frame : Frame.frame,
        toggle : Toggle.toggle
      }

    fun mkFrToggleText root 
          {state, border_width, action, backgrnd, foregrnd, label} = let
        val toggle = Toggle.mkToggleText root {
                       label = label,
                       action = action,
                       rounded = false,
                       state = state,
                       foregrnd = foregrnd,
                       backgrnd = backgrnd
                     }
        val frame = Frame.mkFrame {
                      color = foregrnd,
                      width = border_width,
                      widget = Toggle.widgetOf toggle
                    }
        in
          FT{frame=frame,toggle=toggle}
        end

    fun widgetOf (FT{frame,...}) = Frame.widgetOf frame
    fun setActive (FT{toggle,...},v) = Toggle.setActive (toggle,v)
    fun getActive (FT{toggle,...}) = Toggle.getActive toggle
    fun setState (FT{toggle,...},v) = Toggle.setActive (toggle,v)
    fun getState (FT{toggle,...}) = Toggle.getActive toggle

  end (* FramedToggle *)
