(* xevttypes.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *)

structure XEventTypes =
  struct

    local
      open Geometry XProtTypes

      val & = Bits.andb
      val ++ = Bits.orb
      val << = Bits.lshift
      infix ++ << &

    in

  (* X event names *)
    datatype xevent_name
      = XEVT_KeyPress
      | XEVT_KeyRelease
      | XEVT_ButtonPress
      | XEVT_ButtonRelease
      | XEVT_EnterWindow
      | XEVT_LeaveWindow
      | XEVT_PointerMotion
      | XEVT_PointerMotionHint
      | XEVT_Button1Motion
      | XEVT_Button2Motion
      | XEVT_Button3Motion
      | XEVT_Button4Motion
      | XEVT_Button5Motion
      | XEVT_ButtonMotion
      | XEVT_KeymapState
      | XEVT_Exposure
      | XEVT_VisibilityChange
      | XEVT_StructureNotify
      | XEVT_ResizeRedirect
      | XEVT_SubstructureNotify
      | XEVT_SubstructureRedirect
      | XEVT_FocusChange
      | XEVT_PropertyChange
      | XEVT_ColormapChange
      | XEVT_OwnerGrabButton

  (* The types of the information carried by some XEvents *)
    type key_xevtinfo = {	    (* KeyPress and KeyRelease *)
	    root : win_id,		(* the root of the source window *)
	    event : win_id,		(* the window in which this was generated *)
	    child : win_id option,	(* the child of the event window that is the *)
					(* ancestor of the source window *)
	    same_screen : bool,		(* *)
	    root_pt : point,		(* event coords in the root window *)
	    event_pt : point,		(* event coords in the event window *)
	    keycode : keycode,		(* the keycode of the depressed key *)
	    mod_state : modkey_state,	(* the modifier-key state *)
	    mbut_state : mbutton_state, (* the mouse button state *)
	    time : CML.time
	    }
    type button_xevtinfo = {	    (* ButtonPress and ButtonRelease *)
	    root : win_id,		(* the root of the source window *)
	    event : win_id,		(* the window in which this was generated *)
	    child : win_id option,	(* the child of the event window that is the *)
					(* ancestor of the source window *)
	    same_screen : bool,		(* *)
	    root_pt : point,		(* event coords in the root window *)
	    event_pt : point,		(* event coords in the event window *)
	    button : mbutton,		(* the button that was pressed *)
	    mod_state : modkey_state,	(* the modifier-key state *)
	    mbut_state : mbutton_state, (* the mouse button state *)
	    time : CML.time
	  }
    type inout_xevtinfo = {	    (* EnterNotify and LeaveNotify *)
	    root : win_id,		(* the root window for the pointer position *)
	    event : win_id,		(* the event window *)
	    child : win_id option,	(* the child of event containing the pointer *)
	    same_screen : bool,		(* *)
	    root_pt : point,		(* final pointer position in root coords *)
	    event_pt : point,		(* final pointer position in event coords *)
	    mode : focus_mode,		(* *)
	    detail : focus_detail,	(* *)
	    mod_state : modkey_state,	(* the modifier-key state *)
	    mbut_state : mbutton_state, (* the mouse button state *)
	    focus : bool,		(* true, if event is the focus *)
	    time : CML.time
	  }
    type focus_xevtinfo = {	    (* FocusIn and FocusOut *)
	    event : win_id,		(* the window that gained the focus *)
	    mode : focus_mode,
	    detail : focus_detail
	  }


  (* X event messages *)
    datatype xevent
      = KeyPressXEvt of key_xevtinfo
      | KeyReleaseXEvt of key_xevtinfo
      | ButtonPressXEvt of button_xevtinfo
      | ButtonReleaseXEvt of button_xevtinfo
      | MotionNotifyXEvt of {
	    root : win_id,		(* the root of the source window *)
	    event : win_id,		(* the window in which this was generated *)
	    child : win_id option,	(* the child of the event window that is the *)
					(* ancestor of the source window *)
	    same_screen : bool,		(* *)
	    root_pt : point,		(* event coords in the root window *)
	    event_pt : point,		(* event coords in the event window *)
	    hint : bool,		(* true, if PointerMotionHint is selected *)
	    mod_state : modkey_state,	(* the modifier-key state *)
	    mbut_state : mbutton_state, (* the mouse button state *)
	    time : CML.time
	  }
      | EnterNotifyXEvt of inout_xevtinfo
      | LeaveNotifyXEvt of inout_xevtinfo
      | FocusInXEvt of focus_xevtinfo
      | FocusOutXEvt of focus_xevtinfo
      | KeymapNotifyXEvt of {}
      | ExposeXEvt of {
	    window : win_id,		(* the exposed window *)
	    rects : rect list,		(* the exposed rectangle.  This is a list, so *)
					(* that multiple events can be packed *)
	    count : int			(* number of subsequent expose events *)
	  }
      | GraphicsExposeXEvt of {
	    drawable : drawable_id,
	    rect : rect,		(* the obscured rectangle. *)
	    count : int,		(* the # of additional GraphicsExpose events *)
	    major_opcode : int,		(* the graphics operation code *)
	    minor_opcode : int		(* always 0 for core protocol *)
	  }
      | NoExposeXEvt of {
	    drawable : drawable_id,
	    major_opcode : int,		(* the graphics operation code *)
	    minor_opcode : int		(* always 0 for core protocol *)
	  }
      | VisibilityNotifyXEvt of {
	    window : win_id,		(* the window with changed visibility state *)
	    state : visibility		(* the new visibility state *)
	  }
      | CreateNotifyXEvt of {
	    parent : win_id,		(* the created window's parent *)
	    window : win_id,		(* the created window *)
	    rect : rect,		(* the window's rectangle *)
	    border_wid : int,		(* the width of the border *)
	    override_redirect : bool	(* *)
	  }
      | DestroyNotifyXEvt of {
	    event : win_id,		(* the window on which this was generated *)
	    window : win_id		(* the destroyed window *)
	  }
      | UnmapNotifyXEvt of {
	    event : win_id,		(* the window on which this was generated *)
	    window : win_id,		(* the window being unmapped *)
	    from_config : bool		(* true, if parent was resized *)
	  }
      | MapNotifyXEvt of {
	    event : win_id,		(* the window on which this was generated *)
	    window : win_id,		(* the window being mapped *)
	    override_redirect : bool	(* *)
	  }
      | MapRequestXEvt of {
	    parent : win_id,		(* the parent *)
	    window : win_id		(* the mapped window *)
	  }
      | ReparentNotifyXEvt of {
	    event : win_id,		(* the window on which this was generated *)
	    parent : win_id,		(* the new parent *)
	    window : win_id,		(* the re-rooted window *)
	    corner : point,		(* the upper-left corner *)
	    override_redirect : bool	(* *)
	  }
      | ConfigureNotifyXEvt of {
	    event : win_id,		(* the window on which this was generated *)
	    window : win_id,		(* the reconfigured window *)
	    sibling : win_id option,	(* the sibling that window is above (if any) *)
	    rect : rect,		(* the window's rectangle *)
	    border_wid : int,		(* the width of the border *)
	    override_redirect : bool	(* *)
	  }
      | ConfigureRequestXEvt of {
	    parent : win_id,		(* the parent *)
	    window : win_id,		(* the window to reconfigure *)
	    sibling : win_id option,	(* the new sibling (if any) *)
	    x : int option,		(* the window's rectangle *)
	    y : int option,
	    wid : int option,
	    ht : int option,
	    border_wid : int option,	(* the width of the border *)
	    stack_mode : stack_mode option (* the mode for stacking windows *)
	  }
      | GravityNotifyXEvt of {
	    event : win_id,		(* the window on which this was generated *)
	    window : win_id,		(* the window being moved *)
	    corner : point		(* upper-left corner of window *)
	  }
      | ResizeRequestXEvt of {
	    window : win_id,		(* the window to resize *)
	    req_sz : size		(* the requested new size *)
	  }
      | CirculateNotifyXEvt of {
	    event : win_id,		(* the window on which this was generated *)
	    window : win_id,		(* the window being circulated *)
	    parent : win_id,		(* the parent *)
	    place : stack_pos		(* the new place *)
	  }
      | CirculateRequestXEvt of {
	    parent : win_id,		(* the parent *)
	    window : win_id,		(* the window to circulate *)
	    place : stack_pos		(* the place to circulate the window to *)
	  }
      | PropertyNotifyXEvt of {
	    window : win_id,		(* the window with the changed property *)
	    atom : atom,		(* the affected property *)
	    time : CML.time,		(* when the property was changed*)
	    deleted : bool		(* true, if the property was deleted *)
	  }
      | SelectionClearXEvt of {
	    owner : win_id,		(* the current owner of the selection *)
	    selection : atom,		(* the selection *)
	    time : CML.time		(* the last-change time *)
	  }
      | SelectionRequestXEvt of {
	    owner : win_id,		(* the owner of the selection *)
	    selection : atom,		(* the selection *)
	    target : atom,		(* the requested type for the selection *)
	    requestor : win_id,		(* the requesting window *)
	    property : atom option,	(* the property to store the selection in *)
	    time : CML.time		(* *)
	  }
      | SelectionNotifyXEvt of {
	    requestor : win_id,		(* the requestor of the selection *)
	    selection : atom,		(* the selection *)
	    target : atom,		(* the requested type of the selection *)
	    property : atom option,	(* the property to store the selection in *)
	    time : CML.time		(* *)
	  }
      | ColormapNotifyXEvt of {
	    window : win_id,		(* the affected window *)
	    cmap : colormap_id option,	(* the colormap *)
	    new : bool,			(* true, if the colormap attribute is changed *)
	    installed : bool		(* true, if the colormap is installed *)
	  }
      | ClientMessageXEvt of {
	    window : win_id,		(* *)
	    typ : atom,			(* the type of the message *)
	    value : raw_data		(* the message value *)
	  }
      | ModifierMappingNotifyXEvt	(* really a MappingNotify event *)
      | KeyboardMappingNotifyXEvt of {	(* really a MappingNotify event *)
	    first_keycode : keycode,
	    count : int
	  }
      | PointerMappingNotifyXEvt	(* really a MappingNotify event *)

    fun maskOfXEvt XEVT_KeyPress		= XEVTMASK(1 << 0)
      | maskOfXEvt XEVT_KeyRelease		= XEVTMASK(1 << 1)
      | maskOfXEvt XEVT_ButtonPress		= XEVTMASK(1 << 2)
      | maskOfXEvt XEVT_ButtonRelease		= XEVTMASK(1 << 3)
      | maskOfXEvt XEVT_EnterWindow		= XEVTMASK(1 << 4)
      | maskOfXEvt XEVT_LeaveWindow		= XEVTMASK(1 << 5)
      | maskOfXEvt XEVT_PointerMotion		= XEVTMASK(1 << 6)
      | maskOfXEvt XEVT_PointerMotionHint	= XEVTMASK(1 << 7)
      | maskOfXEvt XEVT_Button1Motion		= XEVTMASK(1 << 8)
      | maskOfXEvt XEVT_Button2Motion		= XEVTMASK(1 << 9)
      | maskOfXEvt XEVT_Button3Motion		= XEVTMASK(1 << 10)
      | maskOfXEvt XEVT_Button4Motion		= XEVTMASK(1 << 11)
      | maskOfXEvt XEVT_Button5Motion		= XEVTMASK(1 << 12)
      | maskOfXEvt XEVT_ButtonMotion		= XEVTMASK(1 << 13)
      | maskOfXEvt XEVT_KeymapState		= XEVTMASK(1 << 14)
      | maskOfXEvt XEVT_Exposure		= XEVTMASK(1 << 15)
      | maskOfXEvt XEVT_VisibilityChange	= XEVTMASK(1 << 16)
      | maskOfXEvt XEVT_StructureNotify		= XEVTMASK(1 << 17)
      | maskOfXEvt XEVT_ResizeRedirect		= XEVTMASK(1 << 18)
      | maskOfXEvt XEVT_SubstructureNotify	= XEVTMASK(1 << 19)
      | maskOfXEvt XEVT_SubstructureRedirect	= XEVTMASK(1 << 20)
      | maskOfXEvt XEVT_FocusChange		= XEVTMASK(1 << 21)
      | maskOfXEvt XEVT_PropertyChange		= XEVTMASK(1 << 22)
      | maskOfXEvt XEVT_ColormapChange		= XEVTMASK(1 << 23)
      | maskOfXEvt XEVT_OwnerGrabButton		= XEVTMASK(1 << 24)

    fun maskOfXEvtList l = let
	  fun f ([], m) = XEVTMASK m
	    | f (xevt::r, m) = let
		  val (XEVTMASK m') = maskOfXEvt xevt
		  in
		    f (r, m ++ m')
		  end
	  in
	    f (l, 0)
	  end

    fun unionXEvtMasks (XEVTMASK m1, XEVTMASK m2) = XEVTMASK(m1 ++ m2)
    fun interXEvtMasks (XEVTMASK m1, XEVTMASK m2) = XEVTMASK(m1 & m2)

  end (* local *)
  end (* XEventTypes *)
