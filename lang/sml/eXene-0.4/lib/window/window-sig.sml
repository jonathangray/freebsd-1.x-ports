(* window-sig.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *)

signature WINDOW =
  sig

  (* eXene windows *)
    type window
      sharing type window = DrawTypes.window

  (* user-level window attributes *)
    datatype window_attr
      = WA_Background_None
      | WA_Background_ParentRelative
      | WA_Background_Pixmap of DrawTypes.pixmap
      | WA_Background_Tile of DrawTypes.tile
      | WA_Background_Color of ColorServer.color
      | WA_Border_CopyFromParent
      | WA_Border_Pixmap of DrawTypes.pixmap
      | WA_Border_Tile of DrawTypes.tile
      | WA_Border_Color of ColorServer.color
      | WA_BitGravity of XProtTypes.gravity
      | WA_WinGravity of XProtTypes.gravity
      | WA_Cursor_None
      | WA_Cursor of Cursor.cursor

  (* window configuration values *)
    datatype window_config
      = WC_Origin of Geometry.point
      | WC_Size of Geometry.size
      | WC_BorderWid of int
      | WC_StackMode of XProtTypes.stack_mode
      | WC_RelStackMode of (window * XProtTypes.stack_mode)

    exception BadWindowGeometry

(**** OBSOLETE ****
    val allocSimpleTopWin : Display.screen
	  -> (window * WindowEnv.in_env * ({
	      geom : Geometry.win_geom,
	      border : ColorServer.color,
	      backgrnd : ColorServer.color
	    } -> window))
    val allocSimpleSubwin : window
	  -> (window * ({
	      geom : Geometry.win_geom,
	      border : ColorServer.color option,
	      backgrnd : ColorServer.color option
	    } -> window))
**** OBSOLETE ****)

    val createSimpleTopWin : Display.screen -> {
	      geom : Geometry.win_geom,
	      border : ColorServer.color,
	      backgrnd : ColorServer.color
	    } -> (window * WindowEnv.in_env)
    val createSimpleSubwin : window -> {
	      geom : Geometry.win_geom,
	      border : ColorServer.color option,
	      backgrnd : ColorServer.color option
	    } -> window
    val createTransientWin : window -> {
	      geom : Geometry.win_geom,
	      border : ColorServer.color,
	      backgrnd : ColorServer.color
	    } -> (window * WindowEnv.in_env)
    val createSimplePopupWin : Display.screen -> {
	      geom : Geometry.win_geom,
	      border : ColorServer.color,
	      backgrnd : ColorServer.color
	    } -> (window * WindowEnv.in_env)
    val createInputOnlyWin : window -> Geometry.rect -> window

  (* raise this exception on operations, such as drawing, that are illegal for
   * InputOnly windows. *)
    exception InputOnly

  (* set the properties of a top-level window *)
    val setWMProperties : window -> {
	      win_name : string option,
	      icon_name : string option,
	      argv : string list,
	      size_hints : XProps.size_hints list,
	      wm_hints : XProps.wm_hints list,
	      class_hints : {res_class : string, res_name : string} option
	    } -> unit

  (* set the window-manager protocols for a window *)
    val setWMProtocols : window -> XProtTypes.atom list -> bool

  (* Various routines to reconfigure window layout *)
    val configureWin : window -> window_config list -> unit
    val moveWin : window -> Geometry.point -> unit
    val resizeWin : window -> Geometry.size -> unit
    val moveAndResizeWin : window -> Geometry.rect -> unit

  (* map a point in the window's coordinate system to the screen's
   * coordinate system *)
    val winPtToScrPt : window -> Geometry.point -> Geometry.point

    val setCursor : window -> Cursor.cursor option -> unit

    val mapWin : window -> unit
    val unmapWin : window -> unit
    val destroyWin : window -> unit

    val screenOfWin : window -> Display.screen
    val displayOfWin : window -> Display.display

  end (* WINDOW *)
