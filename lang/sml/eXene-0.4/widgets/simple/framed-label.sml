(* framed-label.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.
 *
 * Labels wrapped in a frame.
 *)

signature FRAMED_LABEL = 
  sig

    structure W : WIDGET

    type fr_label

    val mkFrLabel : W.root -> {
        label : string, 
        font : string option,
        border_width : int, 
        foregrnd : W.EXB.color option, 
        backgrnd : W.EXB.color option, 
        align : W.halign
      } -> fr_label

    val widgetOf : fr_label -> W.widget
    val setLabel : fr_label -> string -> unit
    val setBackground : fr_label -> W.EXB.color option -> unit
    val setForeground : fr_label -> W.EXB.color option -> unit

  end (* FRAMED_LABEL *)

structure FramedLabel : FRAMED_LABEL = 
  struct

    structure W = Widget

    datatype fr_label = FL of {
        frame : Frame.frame,
        label : Label.label
      }

    fun mkFrLabel root {label, font, border_width, foregrnd, backgrnd, align} = let
        val label = Label.mkLabel root {
                      label = label,
                      font = font,
                      foregrnd = foregrnd,
                      backgrnd = backgrnd,
                      align = align
                    }
        val frame = Frame.mkFrame {
                      color = foregrnd,
                      width = border_width,
                      widget = Label.widgetOf label
                    }
        in
          FL{frame=frame,label=label}
        end

    fun widgetOf (FL{frame,...}) = Frame.widgetOf frame
    fun setLabel (FL{label,...}) s = Label.setLabel label s
    fun setBackground (FL{label,...}) copt = Label.setBackground label copt
    fun setForeground (FL{label,frame}) copt = (
          Label.setForeground label copt;
          Frame.setColor frame copt
        )

  end (* FramedLabel *)
