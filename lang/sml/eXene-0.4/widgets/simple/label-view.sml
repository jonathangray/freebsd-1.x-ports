(* label-view.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * View for label widget.
 *)

signature LABEL_VIEW = 
  sig

    structure EXB : EXENE_BASE
    structure D : DRAWING
    structure W : WIDGET

    val mkLabelView : (W.root * string) -> {
        label : string, 
        align : W.halign
      } -> (EXB.G.size * ((D.drawable * EXB.G.point * EXB.G.size) -> D.pen -> unit))

  end (* LABEL_VIEW *)

structure LabelView : LABEL_VIEW = struct

  structure EXB = EXeneBase
  structure D = Drawing
  structure W = Widget

  open CML Geometry EXeneBase Drawing Widget

  val pad = 1

  fun mkLabelView (root, fontName) = let
    val scr = screenOf root
    val font = Font.openFont (displayOf root) fontName
    val {ascent=fonta,descent=fontd} = Font.fontHt font
    val fonth = fonta + fontd

    val emptySize = SIZE{wid=2*pad, ht=fonth+(2*pad)}
    fun setGeom {label="", ...} = (emptySize, fn _ => fn _ => ())
      | setGeom {label, align} = let
(*
      val Font.CharInfo {left_bearing=l,right_bearing=r,...}
          = #overall_info (Font.textExtents font label)
      val natx = r - l
      val offx = l
*)
      val natx = Font.textWidth font label
      val size = SIZE{wid=natx+(2*pad),ht=fonth+(2*pad)}
      val offx = 0

      fun drawfn (drawable,PT{x,y},SIZE{wid,ht}) = let
        val basey = fonta + ((ht - fonth) div 2)
        val pt =
          case align of
            HLeft => PT{x=x+pad-offx,y=y+basey}
          | HCenter => PT{x=x+((wid - natx) div 2) - offx, y=y+basey}
          | HRight => PT{x=x+(wid - (natx + pad + offx)),y=y+basey}
      in
        fn pen => (
          drawString drawable pen font (pt, label)
        )
      end
    in
      (size, drawfn)
    end
  in
    setGeom
  end

end (* LabelView *)
