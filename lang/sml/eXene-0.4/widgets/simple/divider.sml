(* divider.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Divider widget, for drawing horizontal or vertical line.
 *)

signature DIVIDER = 
  sig

    structure W : WIDGET

    val mkHorzDivider : W.root -> {
      color : W.EXB.color option,
      width : int 
    } -> W.widget

    val mkVertDivider : W.root -> {
      color : W.EXB.color option,
      width : int 
    } -> W.widget

  end (* DIVIDER *)

structure Divider : DIVIDER = 
  struct

    structure W = Widget
  
    fun mkDivider isHorz root {color, width} = let
          val _ = if width < 0 
                    then LibBase.badArg{module="Divider",func="mkDivider",msg="width < 0"}
                    else ()
          val size = let
                val fixD = W.fixDim width
                val stretchD = W.DIM{base=1,incr=1,min=0,nat=0,max=NONE}
                in
                  case isHorz of
                    true => {x_dim=stretchD, y_dim=fixD}
                  | false => {y_dim=stretchD, x_dim=fixD}
                end
          in
            ColorRect.mkColorRect root (color, fn () => size)
          end

    val mkHorzDivider = mkDivider true
    val mkVertDivider = mkDivider false

  end (* Divider *)

