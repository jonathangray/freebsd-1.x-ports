(* color-rect.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Widget that fills rectangle with a color.
 *)

signature COLOR_RECT = 
  sig

    structure W : WIDGET

    val mkColorRect : W.root -> 
          (W.EXB.color option * (unit -> W.bounds)) -> W.widget

  end (* COLOR_RECT *)

structure ColorRect : COLOR_RECT = 
  struct

    structure W = Widget

    local open Geometry EXeneBase Interact Drawing in

    fun mkColorRect root (colorOpt,boundsOf) = let
          val scr = W.screenOf root
          val backColor = case colorOpt of
                            SOME color => color
                          | NONE => blackOfScr scr
          val pen = newPen [PV_Foreground backColor]

          fun realize {env, win, sz} = let
                val fill = fillRect (drawableOfWin win) pen
                val InEnv{ci,...} = ignoreInput env
                fun loop () = (
                      case msgBodyOf(CML.sync ci) of
                        CI_Redraw rects => app fill rects
                      | _ => ();
                      loop ()
                    )
                in
                  CML.spawn loop; ()
                end
          in
            W.mkWidget{
              root = root, 
              boundsOf = boundsOf,
              realize = realize
            }
          end

    end (* local *)

  end (* ColorRect *)

