(* draw-types.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * Types of objects that can be drawn on (or are pixel sources).
 *)

structure DrawTypes =
  struct
    local
      open Geometry XProtTypes Display DrawMaster
    in

  (* An on-screen bitmap *)
    datatype window = WIN of {
	id : win_id,
	scr : screen,
	scr_depth : scr_depth,
(****
	backgrnd_color : color option,
	border_color : color option,
****)
	draw_cmd : draw_msg -> unit
      }

  (* An off-screen bitmap *)
    datatype pixmap = PM of {  
	id : pixmap_id,
	scr : screen,
	sz : size,
	scr_depth : scr_depth
      }

  (* immutable pixmaps *)
    datatype tile = TILE of pixmap

  (* identity tests *)
    fun sameWindow (WIN{id=id1, scr=s1, ...}, WIN{id=id2, scr=s2, ...}) =
	  ((id1 = id2) andalso sameScreen(s1, s2))
    fun samePixmap (PM{id=id1, scr=s1, ...}, PM{id=id2, scr=s2, ...}) =
	  ((id1 = id2) andalso sameScreen(s1, s2))
    fun sameTile (TILE p1, TILE p2) = samePixmap(p1, p2)

  (* Sources for bitblt operations *)
    datatype draw_src
      = WSRC of window
      | PMSRC of pixmap
      | TSRC of tile

    fun depthOfWin (WIN{scr_depth=SCRDEPTH{depth, ...}, ...}) = depth
    fun depthOfPixmap (PM{scr_depth=SCRDEPTH{depth, ...}, ...}) = depth
    fun depthOfTile (TILE(PM{scr_depth=SCRDEPTH{depth, ...}, ...})) = depth

    fun depthOfDrawSrc (WSRC win) = depthOfWin win
      | depthOfDrawSrc (PMSRC pm) = depthOfPixmap pm
      | depthOfDrawSrc (TSRC tile) = depthOfTile tile

    fun geomOfWin (WIN{id, scr=SCREEN{dpy, ...}, ...}) = let
	  open XRequest XReply
	  val reply = CML.sync (dpyRequestReply dpy (encodeGetGeometry {drawable=id}))
	  val {depth, geom=WGEOM{pos, sz, border}, ...} = decodeGetGeometryReply reply
	  in
	    {pos = pos, sz = sz, depth = depth, border = border}
	  end
    fun geomOfPixmap (PM{sz, scr_depth=SCRDEPTH{depth, ...}, ...}) = {
	    pos = originPt, sz = sz, depth = depth, border = 0
	  }
    fun geomOfTile (TILE pm) = geomOfPixmap pm

    fun geomOfDrawSrc (WSRC w) = geomOfWin w
      | geomOfDrawSrc (PMSRC pm) = geomOfPixmap pm
      | geomOfDrawSrc (TSRC(TILE pm)) = geomOfPixmap pm

    fun sizeOfWin win = let
	  val {sz, ...} = geomOfWin win
	  in
	    sz
	  end
    fun sizeOfPixmap (PM{sz, ...}) = sz
    fun sizeOfTile (TILE pm) = sizeOfPixmap pm


  (** drawables **
   *
   * these are abstract views of drawable objects (e.g., windows or pixmaps).
   *)
    datatype draw_root = DWIN of window | DPM of pixmap
    datatype drawable = DRAWABLE of {
	root : draw_root,
	draw_cmd : draw_msg -> unit
      }

  (* make a drawable from a window *)
    fun drawableOfWin (w as WIN{draw_cmd, ...}) =
	  DRAWABLE{root = DWIN w, draw_cmd = draw_cmd}

  (* make a drawable from a pixmap *)
    fun drawableOfPM (pm as PM{sz, scr_depth=SCRDEPTH{draw_cmd, ...}, ...}) = let
	  fun drawCmd (DMSG{dst, pen, oper = DOP_ClearArea(RECT{x, y, wid, ht})}) = let
		fun clip (z, 0, max) = max - z
		  | clip (z, w, max) = if ((z + w) > max) then (max - z) else w
		val SIZE{wid = pmWid, ht = pmHt} = sz
		val dstRect = RECT{
			x = x, y = y,
			wid = clip(x, wid, pmWid), ht = clip(y, ht, pmHt)
		      }
		in
		  draw_cmd (DMSG{
		      dst = dst,
		      pen = PenRep.defaultPen,
		      oper = DOP_PolyFillRect[dstRect]
		    });
                (* the following is needed to avoid race between updating the
		 * pixmap and using it as the source of a blt.
		 *)
		  draw_cmd DMSG_Flush
		end
	    | drawCmd dmsg = draw_cmd dmsg
	  in
	    DRAWABLE{root = DPM pm, draw_cmd=drawCmd}
	  end

    fun depthOfDrawable (DRAWABLE{root = DWIN w, ...}) = depthOfWin w
      | depthOfDrawable (DRAWABLE{root = DPM pm, ...}) = depthOfPixmap pm

  (* Create an unbuffered drawable for real-time feedback.  This basically
   * works by adding flush messages after each draw command.  There is
   * probably a better way.
   *)
    fun feedback (DRAWABLE{root as DWIN w, draw_cmd}) = DRAWABLE{
	    root = root,
	    draw_cmd = fn msg => (draw_cmd msg; draw_cmd DMSG_Flush)
	  }
      | feedback d = d

  (* the following exception is raised if an attempt is made to use a stale
   * overlay drawable (i.e., one that has been released).
   *)
    exception StaleOverlay

  (* Create an overlay drawable for the given window.  This provides concurrency
   * control on the window and its descendents during rubber-banding (using OP_Xor).
   * The first result is the overlay drawable, the second is the release operation
   * for the drawable.
   *)
    fun createOverlay (w as WIN{draw_cmd, ...}) = let
	  open CML
	  val releaseCV = condVar()
	  val newDrawCh = channel()
	(* the draw command for the overlay.  It raises StaleOverlay if called
	 * after the overlay is released. *)
	  val errorEvt = wrap (readVarEvt releaseCV, fn () => raise StaleOverlay)
	  fun drawFn msg = select [
		  transmit(newDrawCh, msg),
		  errorEvt
		]
	  fun drawAndFlush msg = (drawFn msg; drawFn DMSG_Flush)
	(* the function used to release the overlay.  Multiple calls are allowed,
	 * so we must handle WriteTwice.
	 *)
	  fun releaseFn () = (writeVar(releaseCV, ()) handle _ => ())
	  in
	    draw_cmd (DMSG_CreateOverlay{
		cmd_strm = newDrawCh,
		release_evt = readVarEvt releaseCV
	      });
	    {
	      drawable = DRAWABLE{root = DWIN w, draw_cmd = drawAndFlush},
	      release = releaseFn
	    }
	  end

    end (* local *)
  end (* DrawTypes *)
