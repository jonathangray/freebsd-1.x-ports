(* window.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *)

structure Window : WINDOW =
  struct

    open Geometry XProtTypes XWin Display DrawTypes

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
      = WC_Origin of point
      | WC_Size of size
      | WC_BorderWid of int
      | WC_StackMode of XProtTypes.stack_mode
      | WC_RelStackMode of (window * XProtTypes.stack_mode)

  (* extract the pixel from a color *)
    fun pixelOf (ColorServer.COLOR{pixel, ...}) = pixel

  (* map user-level window attributes to internal x-window attributes *)
    fun winAttrToXWinAttr (WA_Background_None) =
	  XWV_BackgroundPixmap_None
      | winAttrToXWinAttr (WA_Background_ParentRelative) =
	  XWV_BackgroundPixmap_ParentRelative
      | winAttrToXWinAttr (WA_Background_Pixmap(PM{id, ...})) =
	  XWV_BackgroundPixmap id
      | winAttrToXWinAttr (WA_Background_Tile(TILE(PM{id, ...}))) = 
	  XWV_BackgroundPixmap id
      | winAttrToXWinAttr (WA_Background_Color color) =
	  XWV_BackgroundPixel(pixelOf color)
      | winAttrToXWinAttr (WA_Border_CopyFromParent) =
	  XWV_BorderPixmap_CopyFromParent
      | winAttrToXWinAttr (WA_Border_Pixmap(PM{id, ...})) =
	  XWV_BorderPixmap id
      | winAttrToXWinAttr (WA_Border_Tile(TILE(PM{id, ...}))) =
	  XWV_BorderPixmap id
      | winAttrToXWinAttr (WA_Border_Color color) =
	  XWV_BorderPixel(pixelOf color)
      | winAttrToXWinAttr (WA_BitGravity g) =
	  XWV_BitGravity g
      | winAttrToXWinAttr (WA_WinGravity g) =
	  XWV_WinGravity g
      | winAttrToXWinAttr (WA_Cursor_None) =
	  XWV_Cursor_None
      | winAttrToXWinAttr (WA_Cursor(Cursor.CURSOR{id, ...})) =
	  XWV_Cursor id

    val mapAttrs = List.map winAttrToXWinAttr

    val stdXEventMask = XEventTypes.maskOfXEvtList [
	    XEventTypes.XEVT_KeyPress,
	    XEventTypes.XEVT_KeyRelease,
	    XEventTypes.XEVT_ButtonPress,
	    XEventTypes.XEVT_ButtonRelease,
	    XEventTypes.XEVT_PointerMotion,
            XEventTypes.XEVT_EnterWindow,
            XEventTypes.XEVT_LeaveWindow,
	    XEventTypes.XEVT_Exposure,
	    XEventTypes.XEVT_StructureNotify,
	    XEventTypes.XEVT_SubstructureNotify
	  ]

    val popupXEventMask = XEventTypes.maskOfXEvtList [
	    XEventTypes.XEVT_Exposure,
	    XEventTypes.XEVT_StructureNotify,
	    XEventTypes.XEVT_SubstructureNotify
	  ]

    exception BadWindowGeometry

    fun chkGeom g = if Geometry.validGeom g then g else raise BadWindowGeometry

(**** OBSOLETE ****
  (* Allocate a simple top-level window, returning the window, its input environment
   * and its creation function.
   *)
    fun allocSimpleTopWin (scr as SCREEN{scr=scrinfo, dpy}) = let
	  val SCR{xscr=XDisplay.XSCR{root, ...}, root_servers, ...} = scrinfo
	  val SCRDEPTH{depth, ...} = root_servers
	  val DPY{xdpy = XDisplay.XDPY{conn, nextXId, ...}, ...} = dpy
	  val winId = nextXId()
	  val (inEnv, win) = TopLevelWin.mkTopLevelWinEnv (scr, root_servers, winId)
	  fun createFn {geom, border, backgrnd} = (
		XWin.newXWin conn {
		    id = winId,
		    parent = root,
		    in_only = SOME false,
		    depth = depth,
		    visual = NONE,
		    geom = chkGeom geom,
		    attrs = [
			XWin.XWV_BorderPixel(pixelOf border),
			XWin.XWV_BackgroundPixel(pixelOf backgrnd),
			XWin.XWV_EventMask stdXEventMask
		      ]
		  };
		win)
	  in
	    (win, inEnv, createFn)
	  end

  (* allocate a simple subwindow, returning the window and its creation function *)
    fun allocSimpleSubwin (WIN{id=parentId, scr, draw_cmd, scr_depth, ...}) = let
	  val SCREEN{dpy=DPY{xdpy=XDisplay.XDPY{conn, nextXId, ...}, ...}, ...} = scr
	  val winId = nextXId()
	  val win = WIN{
		  id = winId,
		  scr = scr,
		  draw_cmd = draw_cmd,
		  scr_depth = scr_depth
		}
	  val SCRDEPTH{depth, ...} = scr_depth
	  fun createFn {geom, border, backgrnd} = let
		val borderPixel = (case border
		       of NONE => XWin.XWV_BorderPixmap_CopyFromParent
			| (SOME c) => XWin.XWV_BorderPixel(pixelOf c)
		      (* end case *))
		val backgroundPixel = (case backgrnd
		       of NONE => XWin.XWV_BackgroundPixmap_ParentRelative
			| (SOME c) => XWin.XWV_BackgroundPixel(pixelOf c)
		      (* end case *))
		in
		  XWin.newXWin conn {
		      id = winId,
		      parent = parentId,
		      in_only = SOME false,
		      depth = depth,
		      visual = NONE,
		      geom = chkGeom geom,
		      attrs = [
			  borderPixel,
			  backgroundPixel,
			  XWin.XWV_EventMask stdXEventMask
			]
		    };
		  win
		end
	  in
	    (win, createFn)
	  end
**** OBSOLETE ****)

    fun createSimpleTopWin (scr as SCREEN{scr=scrinfo, dpy}) = let
	  val SCR{xscr=XDisplay.XSCR{root, ...}, root_servers, ...} = scrinfo
	  val SCRDEPTH{depth, ...} = root_servers
	  val DPY{xdpy = XDisplay.XDPY{conn, nextXId, ...}, ...} = dpy
	  val winId = nextXId()
	  val (inEnv, win) = TopLevelWin.mkTopLevelWinEnv (scr, root_servers, winId)
	  fun createFn {geom, border, backgrnd} = (
		XWin.newXWin conn {
		    id = winId,
		    parent = root,
		    in_only = SOME false,
		    depth = depth,
		    visual = NONE,
		    geom = chkGeom geom,
		    attrs = [
			XWin.XWV_BorderPixel(pixelOf border),
			XWin.XWV_BackgroundPixel(pixelOf backgrnd),
			XWin.XWV_EventMask stdXEventMask
		      ]
		  };
		(win, inEnv))
	  in
	    createFn
	  end (* createSimpleTopWin *)

    fun createSimpleSubwin (WIN{id=parentId, scr, draw_cmd, scr_depth, ...}) = let
	  val SCREEN{dpy=DPY{xdpy=XDisplay.XDPY{conn, nextXId, ...}, ...}, ...} = scr
	  val winId = nextXId()
	  val win = WIN{
		  id = winId,
		  scr = scr,
		  draw_cmd = draw_cmd,
		  scr_depth = scr_depth
		}
	  val SCRDEPTH{depth, ...} = scr_depth
	  fun createFn {geom, border, backgrnd} = let
		val borderPixel = (case border
		       of NONE => XWin.XWV_BorderPixmap_CopyFromParent
			| (SOME c) => XWin.XWV_BorderPixel(pixelOf c)
		      (* end case *))
		val backgroundPixel = (case backgrnd
		       of NONE => XWin.XWV_BackgroundPixmap_ParentRelative
			| (SOME c) => XWin.XWV_BackgroundPixel(pixelOf c)
		      (* end case *))
		in
		  XWin.newXWin conn {
		      id = winId,
		      parent = parentId,
		      in_only = SOME false,
		      depth = depth,
		      visual = NONE,
		      geom = chkGeom geom,
		      attrs = [
			  borderPixel,
			  backgroundPixel,
			  XWin.XWV_EventMask stdXEventMask
			]
		    };
		  win
		end
	  in
	    createFn
	  end


  (* create a simple popup window  *)
    fun createSimplePopupWin (scrn as SCREEN{scr, dpy}) {geom, border, backgrnd} = let
	  val SCR{xscr=XDisplay.XSCR{root, ...}, root_servers, ...} = scr
	  val SCRDEPTH{depth, ...} = root_servers
	  val DPY{xdpy = XDisplay.XDPY{conn, nextXId, ...}, ...} = dpy
	  val winId = nextXId()
	  val (inEnv, win) = TopLevelWin.mkTopLevelWinEnv(scrn, root_servers, winId)
	  in
	    XWin.newXWin conn {
		id = winId,
		parent = root,
		in_only = SOME false,
		depth = depth,
		visual = NONE,
		geom = chkGeom geom,
		attrs = [
		    XWin.XWV_OverrideRedirect true,
		    XWin.XWV_SaveUnder true,
		    XWin.XWV_BorderPixel(pixelOf border),
		    XWin.XWV_BackgroundPixel(pixelOf backgrnd),
		    XWin.XWV_EventMask popupXEventMask
		  ]
	      };
	    (win, inEnv)
	  end

  (* create a simple transient window *)
    fun createTransientWin propWin {geom, border, backgrnd} = let
	  open XProps
	  (* open XProtTypes XAtoms XProps *)
          val WIN{id, scr=scrn as SCREEN{scr, dpy},...} = propWin
	  val SCR{xscr=XDisplay.XSCR{root, ...}, root_servers, ...} = scr
	  val SCRDEPTH{depth, ...} = root_servers
	  val DPY{xdpy = XDisplay.XDPY{conn, nextXId, ...}, ...} = dpy
	  val winId = nextXId()
	  val (inEnv, win) = TopLevelWin.mkTopLevelWinEnv(scrn, root_servers, winId)
	  in
	    XWin.newXWin conn {
		id = winId,
		parent = root,
		in_only = SOME false,
		depth = depth,
		visual = NONE,
		geom = chkGeom geom,
		attrs = [
		    XWin.XWV_BorderPixel(pixelOf border),
		    XWin.XWV_BackgroundPixel(pixelOf backgrnd),
		    XWin.XWV_EventMask stdXEventMask
		  ]
	      };
	    dpyRequest dpy (XRequest.encodeChangeProperty {
                win = winId, prop = makeTransientHint propWin, mode = ReplaceProp
              });
	    (win, inEnv)
	  end

    exception InputOnly

    fun createInputOnlyWin win (RECT{x, y, wid, ht}) = let
	  val WIN{id=parentId, scr, scr_depth, draw_cmd, ...} = win
	  val SCREEN{dpy=DPY{xdpy=XDisplay.XDPY{conn, nextXId, ...}, ...}, ...} = scr
	  val winId = nextXId()
	  fun drawCmd (arg as (DrawMaster.DMSG_Destroy _)) = draw_cmd arg
	    | drawCmd _ = raise InputOnly
	  val win = WIN{
		  id = winId,
		  scr = scr,
		  draw_cmd = drawCmd,
		  scr_depth = scr_depth
		}
	  in
	    XWin.newXWin conn {
		id = winId,
		parent = parentId,
		in_only = SOME true,
		depth = 0,
		visual = NONE,
		geom = chkGeom(WGEOM{pos=PT{x=x, y=y}, sz=SIZE{wid=wid, ht=ht}, border=0}),
		attrs = [XWin.XWV_EventMask stdXEventMask]
	      };
	    win
	  end

  (* Set the standard window-manager properties of a top-level window *)
    fun setWMProperties win {
	  win_name, icon_name, argv, size_hints, wm_hints, class_hints
	} = let
	  open XProtTypes XAtoms XProps
	  val WIN{id, scr=SCREEN{dpy, ...}, ...} = win
	  val request = dpyRequest dpy
	  fun putProp p = request (XRequest.encodeChangeProperty {
		 win = id, prop = p, mode = ReplaceProp
		})
	  fun putStrProp (_, NONE) = ()
	    | putStrProp (atom, SOME s) = putProp (makeStringProp (atom, s))
	  in
	    putStrProp (atom_WM_NAME, win_name);
	    putStrProp (atom_WM_ICON_NAME, icon_name);
	    putProp (makeWMSizeHints(atom_WM_NORMAL_HINTS, size_hints));
	    putProp (makeWMHints wm_hints);
	    case class_hints
	     of SOME{res_name, res_class} =>
		  putProp (makeStringProp (atom_WM_CLASS, res_name^"\000"^res_class))
	      | NONE => ();
	    case argv of [] => () | _ => putProp (makeCommandHints(argv))
	  end

  (* Set the window-manager protocols for a window *)
    fun setWMProtocols win atoml = let
	  open XProtTypes XAtoms XProps
	  val WIN{id, scr=SCREEN{dpy, ...}, ...} = win
	  val request = dpyRequest dpy
	  fun putProp n a = request (XRequest.encodeChangeProperty {
		 win = id, prop = makeAtomProp(n,a), mode = ReplaceProp
		})
          in
            case lookupAtom dpy "WM_PROTOCOLS" of
              NONE => false
            | SOME protocols_atom => let
                in
	          case atoml of [] => () | _ => app (putProp protocols_atom) atoml;
                  true
                end
          end

  (* Map window configuration values to a value list *)
    fun doConfigVal arr = let
	  fun upd (i, v) = Array.update(arr, i, SOME v)
	  in
	    fn (WC_Origin(PT{x, y})) => (upd(0, x); upd(1, y))
	     | (WC_Size(SIZE{wid, ht})) => (upd(2, wid); upd(3, ht))
	     | (WC_BorderWid wid) => upd(4, wid)
	     | (WC_StackMode mode) => (
		  Array.update(arr, 5, NONE);
		  upd(6, XCvtFuns.stackModeToWire mode))
	     | (WC_RelStackMode(WIN{id=(XID x), ...}, mode)) => (
		  upd(5, x); upd(6, XCvtFuns.stackModeToWire mode))
	  end
    val doConfigVals = XCvtFuns.doValList 7 doConfigVal

    fun configureWin (WIN{id, scr=SCREEN{dpy, ...}, ...}) vals =
	  dpyRequest dpy (XRequest.encodeConfigureWindow{
	      win = id, vals = doConfigVals vals
	    })

    fun moveWin win pt = configureWin win [WC_Origin pt]

    fun resizeWin win sz = configureWin win [WC_Size sz]

    fun moveAndResizeWin win (RECT{x, y, wid, ht}) = configureWin win [
	    WC_Origin(PT{x=x, y=y}), WC_Size(SIZE{wid=wid, ht=ht})
	  ]

  (* Map a window *)
    fun mapWin (WIN{id, scr=SCREEN{dpy, ...}, ...}) = (
	  dpyRequest dpy (XRequest.encodeMapWindow{win=id});
	  dpyFlushOut dpy)

  (* Unmap a window *)
    fun unmapWin (WIN{id, scr=SCREEN{dpy, ...}, ...}) = (
	  dpyRequest dpy (XRequest.encodeUnmapWindow{win=id});
	  dpyFlushOut dpy)

  (* Destroy a window.  We do this via the draw-master, to avoid a race with any
   * pending draw requests on the window.
   *)
    fun destroyWin (WIN{id, draw_cmd, ...}) = 
      draw_cmd(DrawMaster.DMSG_Destroy(DrawMaster.DSTRY_Win id))

  (* map a point in the window's coordinate system to the screen's
   * coordinate system *)
    fun winPtToScrPt (WIN{id, scr, ...}) pt = let
	  val SCREEN{dpy, scr=SCR{xscr=XDisplay.XSCR{root, ...}, ...}, ...} = scr
	  val {dst_pt, ...} = XReply.decodeTranslateCoordsReply (
		CML.sync (dpyRequestReply dpy
		  (XRequest.encodeTranslateCoords{
		    src_win=id, dst_win=root, src_pt=pt
		  })))
	  in
	    dst_pt
	  end

  (* set the cursor of the window *)
    fun setCursor (WIN{id, scr, ...}) c = let
	  val SCREEN{dpy=DPY{xdpy=XDisplay.XDPY{conn, ...}, ...}, ...} = scr
	  val cur = winAttrToXWinAttr (case c
	       of NONE => WA_Cursor_None
		| (SOME c) => WA_Cursor c)
	  in
	    XWin.changeXWinAttrs conn (id, [cur])
	  end

    fun screenOfWin (WIN{scr, ...}) = scr
    fun displayOfWin (WIN{scr=SCREEN{dpy, ...}, ...}) = dpy

  end (* Window *)
