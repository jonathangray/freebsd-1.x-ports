(* spot.sml
 *)

signature SPOT = sig

  structure W : WIDGET
  structure ColorState : COLORSTATE

  type spot

  val mkSpot : W.root -> {
      init_rgb : ColorState.rgb,
      ht : int, 
      wid : int 
    } -> spot

  val widgetOf : spot -> W.widget
  val setSpot : spot -> ColorState.rgb -> unit

end; (* SPOT *)

structure Spot : SPOT = struct

  structure W = Widget
  structure ColorState = ColorState

  open Geometry W

  datatype spot = Spot of (widget * (ColorState.rgb -> unit))

  fun mkSpot root {init_rgb,ht,wid} = let
    val colorOf = ColorState.colorOf root
    
    val label = Label.mkLabel root {
                  label="", 
                  font=NONE, 
                  foregrnd=NONE, 
                  backgrnd = SOME(colorOf init_rgb),
                  align = HCenter
                }
    val widget = Shape.fixSize (Label.widgetOf label, SIZE{wid=wid,ht=ht})
    in
      Spot(widget, fn rgb => Label.setBackground label (SOME(colorOf rgb)))
    end

  fun widgetOf (Spot(w,_)) = w
  fun setSpot (Spot(_,f)) = f

end; (* Spot *)
