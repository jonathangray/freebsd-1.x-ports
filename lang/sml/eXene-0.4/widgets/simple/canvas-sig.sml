(* canvas-sig.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *)

signature CANVAS =
  sig

    structure W : WIDGET
    structure D : DRAWING

    type canvas

    val mkCanvas : W.root -> W.bounds
          -> (canvas * W.G.size * W.Interact.in_env)

    val widgetOf : canvas -> W.widget
    val sizeOf : canvas -> W.G.size
    val drawableOfCanvas : canvas -> D.drawable

  end (* CANVAS *)
