(* iccc.sml
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * Types and operations to support the X Inter-Client Communications Conventions.
 *)

signature ICCC =
  sig

    structure G : GEOMETRY
    structure EXB : EXENE_BASE

    type atom

  (* Standard X atoms *)
    val atom_PRIMARY : atom
    val atom_SECONDARY : atom
    val atom_ARC : atom
    val atom_ATOM : atom
    val atom_BITMAP : atom
    val atom_CARDINAL : atom
    val atom_COLORMAP : atom
    val atom_CURSOR : atom
    val atom_CUT_BUFFER0 : atom
    val atom_CUT_BUFFER1 : atom
    val atom_CUT_BUFFER2 : atom
    val atom_CUT_BUFFER3 : atom
    val atom_CUT_BUFFER4 : atom
    val atom_CUT_BUFFER5 : atom
    val atom_CUT_BUFFER6 : atom
    val atom_CUT_BUFFER7 : atom
    val atom_DRAWABLE : atom
    val atom_FONT : atom
    val atom_INTEGER : atom
    val atom_PIXMAP : atom
    val atom_POINT : atom
    val atom_RECTANGLE : atom
    val atom_RESOURCE_MANAGER : atom
    val atom_RGB_COLOR_MAP : atom
    val atom_RGB_BEST_MAP : atom
    val atom_RGB_BLUE_MAP : atom
    val atom_RGB_DEFAULT_MAP : atom
    val atom_RGB_GRAY_MAP : atom
    val atom_RGB_GREEN_MAP : atom
    val atom_RGB_RED_MAP : atom
    val atom_STRING : atom
    val atom_VISUALID : atom
    val atom_WINDOW : atom
    val atom_WM_COMMAND : atom
    val atom_WM_HINTS : atom
    val atom_WM_CLIENT_MACHINE : atom
    val atom_WM_ICON_NAME : atom
    val atom_WM_ICON_SIZE : atom
    val atom_WM_NAME : atom
    val atom_WM_NORMAL_HINTS : atom
    val atom_WM_SIZE_HINTS : atom
    val atom_WM_ZOOM_HINTS : atom
    val atom_MIN_SPACE : atom
    val atom_NORM_SPACE : atom
    val atom_MAX_SPACE : atom
    val atom_END_SPACE : atom
    val atom_SUPERSCRIPT_X : atom
    val atom_SUPERSCRIPT_Y : atom
    val atom_SUBSCRIPT_X : atom
    val atom_SUBSCRIPT_Y : atom
    val atom_UNDERLINE_POSITION : atom
    val atom_UNDERLINE_THICKNESS : atom
    val atom_STRIKEOUT_ASCENT : atom
    val atom_STRIKEOUT_DESCENT : atom
    val atom_ITALIC_ANGLE : atom
    val atom_X_HEIGHT : atom
    val atom_QUAD_WIDTH : atom
    val atom_WEIGHT : atom
    val atom_POINT_SIZE : atom
    val atom_RESOLUTION : atom
    val atom_COPYRIGHT : atom
    val atom_NOTICE : atom
    val atom_FONT_NAME : atom
    val atom_FAMILY_NAME : atom
    val atom_FULL_NAME : atom
    val atom_CAP_HEIGHT : atom
    val atom_WM_CLASS : atom
    val atom_WM_TRANSIENT_FOR : atom

  (* Hints about the window size *)
    datatype size_hints
      = HINT_USPosition
      | HINT_PPosition of G.point	(* obsolete in X11R4 *)
      | HINT_USSize
      | HINT_PSize of G.size		(* obsolete in X11R4 *)
      | HINT_PMinSize of G.size
      | HINT_PMaxSize of G.size
      | HINT_PResizeInc of G.size
      | HINT_PAspect of { min : (int * int), max : (int * int) }
      | HINT_PBaseSize of G.size
      | HINT_PWinGravity of EXB.gravity

  (* Window manager hints *)
    datatype wm_hints
      = HINT_Input of bool		  (* does this application rely on the window *)
					  (* manager to get keyboard input? *)
					  (* Initial window state (choose one) *)
      | HINT_WithdrawnState		    (* for windows that are not mapped *)
      | HINT_NormalState		    (* most want to start this way *)
      | HINT_IconicState		    (* application wants to start as an icon *)
      | HINT_IconTile of EXB.tile	  (* tile to be used as icon *)
      | HINT_IconPixmap of EXB.pixmap	  (* pixmap to be used as icon *)
      | HINT_IconWindow of EXB.window	  (* window to be used as icon *)
      | HINT_IconMask of EXB.pixmap	  (* icon mask bitmap *)
      | HINT_IconPosition of G.point      (* initial position of icon *)
      | HINT_WindowGroup of EXB.window    (* the group leader *)

  (** atom operations **)
    val internAtom : EXB.display -> string -> atom
    val lookupAtom : EXB.display -> string -> atom option
    val nameOfAtom : EXB.display -> atom -> string

  end (* signature ICCC *)
