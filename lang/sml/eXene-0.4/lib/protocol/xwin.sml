(* xwin.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *)

structure XWin =
  struct

  (* window configuration values *)
    datatype xwin_val
      = XWV_BackgroundPixmap_None
      | XWV_BackgroundPixmap_ParentRelative
      | XWV_BackgroundPixmap of XProtTypes.pixmap_id
      | XWV_BackgroundPixel of XProtTypes.pixel
      | XWV_BorderPixmap_CopyFromParent
      | XWV_BorderPixmap of XProtTypes.pixmap_id
      | XWV_BorderPixel of XProtTypes.pixel
      | XWV_BitGravity of XProtTypes.gravity
      | XWV_WinGravity of XProtTypes.gravity
      | XWV_BackingStore of XProtTypes.backing_store
      | XWV_BackingPlanes of XProtTypes.plane_mask
      | XWV_BackingPixel of XProtTypes.pixel
      | XWV_SaveUnder of bool
      | XWV_EventMask of XProtTypes.event_mask
      | XWV_DoNotPropagateMask of XProtTypes.event_mask
      | XWV_OverrideRedirect of bool
      | XWV_ColorMap_CopyFromParent
      | XWV_ColorMap of XProtTypes.colormap_id
      | XWV_Cursor_None
      | XWV_Cursor of XProtTypes.cursor_id

    local
      open Geometry XProtTypes XDisplay

      fun doWinVal arr = let
	    fun update (i, x) = Array.update (arr, i, SOME x)
	    in
	      fn (XWV_BackgroundPixmap_None) => update (0, 0)
	       | (XWV_BackgroundPixmap_ParentRelative) => update (0, 1)
	       | (XWV_BackgroundPixmap(XID p)) => update (0, p)
	       | (XWV_BackgroundPixel(PIXEL p)) => update (1, p)
	       | (XWV_BorderPixmap_CopyFromParent) => update (2, 0)
	       | (XWV_BorderPixmap(XID p)) => update (2, p)
	       | (XWV_BorderPixel(PIXEL p)) => update (3, p)
	       | (XWV_BitGravity g) => update (4, XCvtFuns.gravityToWire g)
	       | (XWV_WinGravity g) => update (5, XCvtFuns.gravityToWire g)
	       | (XWV_BackingStore BS_NotUseful) => update (6, 0)
	       | (XWV_BackingStore BS_WhenMapped) => update (6, 1)
	       | (XWV_BackingStore BS_Always) => update (6, 2)
	       | (XWV_BackingPlanes(PLANEMASK m)) => update (7, m)
	       | (XWV_BackingPixel(PIXEL p)) => update (8, p)
	       | (XWV_OverrideRedirect b) => update (9, XCvtFuns.boolToWire b)
	       | (XWV_SaveUnder b) => update (10, XCvtFuns.boolToWire b)
	       | (XWV_EventMask(XEVTMASK m)) => update (11, m)
	       | (XWV_DoNotPropagateMask(XEVTMASK m)) => update (12, m)
	       | (XWV_ColorMap_CopyFromParent) => update (13, 0)
	       | (XWV_ColorMap(XID x)) => update (13, x)
	       | (XWV_Cursor_None) => update (14, 0)
	       | (XWV_Cursor(XID x)) => update (14, x)
	    end
      val doWinValList = XCvtFuns.doValList 15 doWinVal

    in

  (* Create a new X-window with the given xid *)
    fun newXWin conn {id, parent, in_only, depth, visual, geom, attrs} = let
	  val msg = XRequest.encodeCreateWindow {
		  win = id,
		  parent = parent,
		  input_only = in_only,
		  depth = depth,
		  visual = visual,
		  geom = geom,
		  vals = doWinValList attrs
		}
	  in
	    XIo.request conn msg
	  end

  (* Map a window *)
    fun mapXWin conn w = (XIo.request conn (XRequest.encodeMapWindow{win=w}))

  (* change window attributes *)
    fun changeXWinAttrs conn (win, attrs) = (
	  XIo.request conn (XRequest.encodeChangeWindowAttributes{
	      win = win, vals = doWinValList attrs
	    });
	  XIo.flushOut conn)

    end (* local *)
  end (* XWin *)
