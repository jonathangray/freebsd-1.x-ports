(* text_list.sml
 *
 * COPYRIGHT (c) 1991,1992 by AT&T Bell Laboratories
 *
 * List widget, for text lists.
 *)

signature TEXT_LIST =
  sig

    structure W : WIDGET

    exception BadIndex
    exception MultipleChoices

    type 'a text_list
    datatype 'a list_evt = Set of 'a | Unset of 'a
    (* type 'a list_item = (string * 'a * W.wstate) 
     *)
    type 'a list_item
    val mkItem : (string * 'a * W.wstate) -> 'a list_item

    datatype list_mode = OneSet | MultiSet

    val mkHList : W.root -> {
	    mode : list_mode,
	    backgrnd : W.EXB.color option,
	    foregrnd : W.EXB.color option,
	    items : '2a list_item list
	  } -> '2a text_list
    val mkVList : W.root -> {
	    mode : list_mode,
	    backgrnd : W.EXB.color option,
	    foregrnd : W.EXB.color option,
	    items : '2a list_item list
	  } -> '2a text_list

    val evtOf : 'a text_list -> 'a list_evt CML.event
    val widgetOf : 'a text_list -> W.widget

    val setChosen : 'a text_list -> (int * bool) list -> unit
    val setActive : 'a text_list -> (int * bool) list -> unit
    val getChosen : 'a text_list -> int list
    val getState : 'a text_list -> W.wstate list

  end (* TEXT_LIST *)

structure TextList : TEXT_LIST = 
  struct

    structure W = Widget

    open Geometry WidgetSet

    type 'a list_item = (string * 'a * W.wstate)
    fun mkItem x = x
    datatype 'a list_evt = Set of 'a | Unset of 'a
    datatype list_mode = OneSet | MultiSet
    datatype 'a text_list = TL of {
        widget : W.widget,
        wset : widget_set,
        evt : 'a list_evt CML.event
    }

    fun dummy _ = ()
    fun mkListItem (root,backgrnd,foregrnd,ch) (label, v, state) = let
          open Toggle
          val w = mkToggleText root {
                    state=state,
                    rounded=false,
                    action = dummy,
                    backgrnd = backgrnd,
                    foregrnd = foregrnd,
                    label = label
                  }
          fun pick_fn b = (
                if b then CML.send(ch,Set v) else CML.send(ch,Unset v);
                setState (w,b))
          fun active_fn b = setActive (w,b)
          in 
            {
              widget=W.ignoreMouse(widgetOf w),
              state=state,
              pick_fn=pick_fn,
              active_fn=active_fn
            } 
          end

    fun maxSize ({widget,...} : WidgetSet.set_item, SIZE{wid,ht}) = let
          val SIZE{wid=w,ht=h} = Widget.natSize widget
          in SIZE{wid=max(wid,w),ht=max(ht,h)} end

    fun setSize sz {widget,state,pick_fn,active_fn} = {
          widget=Shape.freeSize (widget,sz), 
          state=state,
          pick_fn=pick_fn,
          active_fn=active_fn
        }

    fun mkList layout root {mode, backgrnd, foregrnd, items} = let
          val ch = CML.channel()
          val items = map (mkListItem (root,backgrnd, foregrnd, ch)) items
          val sz = revfold maxSize items (SIZE{wid=0,ht=0})
          val items' = map (setSize sz) items
          val (wset,wl) = case mode of
                            OneSet => WidgetSet.mkSingleSet root items'
                          | MultiSet => WidgetSet.mkMultiSet root items'
          val box = Box.mkLayout root (layout(map Box.WBox wl))
          in
            TL {
              widget = Box.widgetOf box,
              wset = wset,
              evt = CML.receive ch
            }
          end

    val mkHList = mkList Box.HzCenter
    val mkVList = mkList Box.VtCenter

    fun evtOf (TL{evt,...}) = evt
    fun widgetOf (TL{widget,...}) = widget
    fun setChosen (TL{wset,...}) arg = WidgetSet.setChosen wset arg
    fun setActive (TL{wset,...}) arg = WidgetSet.setActive wset arg
    fun getChosen (TL{wset,...}) = WidgetSet.getChosen wset
    fun getState (TL{wset,...}) = WidgetSet.getState wset

end (* TextList *)
