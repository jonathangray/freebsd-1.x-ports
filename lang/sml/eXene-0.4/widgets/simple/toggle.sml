(* toggle.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Common toggles.
 *)

signature TOGGLE = 
  sig

    structure W : WIDGET
  
    type toggle
  
    val widgetOf : toggle -> W.widget
    val getState : toggle -> bool
    val setState : (toggle * bool) -> unit
    val setActive : (toggle * bool) -> unit
    val getActive : toggle -> bool
  
    val mkToggleCheck : W.root -> {
      state : W.wstate,
      action : bool -> unit,
      color : W.EXB.color option,
      sz : int
    } -> toggle
  
    val mkToggleText : W.root -> {
      state : W.wstate,
      rounded : bool,
      action : bool -> unit,
      backgrnd : W.EXB.color option,
      foregrnd : W.EXB.color option,
      label : string
    } -> toggle
  
    val mkToggleSwitch : W.root -> {
      state : W.wstate,
      action : bool -> unit,
      backgrnd : W.EXB.color option,
      foregrnd : W.EXB.color option
    } -> toggle
  
    val mkToggleCircle : W.root -> {
      state : W.wstate,
      action : bool -> unit,
      backgrnd : W.EXB.color option,
      foregrnd : W.EXB.color option,
      radius : int
    } -> toggle
  
    val mkToggleIcon : W.root -> {
      state : W.wstate,
      action : bool -> unit,
      backgrnd : W.EXB.color option,
      foregrnd : W.EXB.color option,
      icon : W.EXB.tile
    } -> toggle
  
  end (* TOGGLE *)

structure Toggle : TOGGLE = 
  struct

    structure W = Widget

    local open ButtonView in

    open ToggleCtrl

    fun mkToggleSwitch root {action, state, foregrnd, backgrnd} = 
          mkToggle root (action, state,
              mkSwitch root {backgrnd=backgrnd,foregrnd=foregrnd})

    fun mkToggleCheck root {state, sz, action, color} =
          mkToggle root (action, state, mkCheck root {color=color,sz=sz})

    fun mkToggleText root {state, rounded,label,action,foregrnd=fc,backgrnd=bc} = let 
          val view = if rounded then 
                       mkRoundedText root {backgrnd=bc,foregrnd=fc,label=label}
                     else
                       mkText root 
                         {align=W.HCenter,backgrnd=bc,foregrnd=fc,label= label}
          in
            mkToggle root (action,state, view)
          end

    fun mkToggleCircle root {state, radius, action, foregrnd=forec, backgrnd=backc} = 
          mkToggle root (action,state,  
              mkCircle root {backgrnd=backc,foregrnd=forec, radius = radius})

    fun mkToggleIcon root {state, icon,action,foregrnd,backgrnd} = 
          mkToggle root (action, state,
              mkIcon root {backgrnd=backgrnd,foregrnd=foregrnd, icon = icon})

    end (* local *)
  end (* Toggle *)
