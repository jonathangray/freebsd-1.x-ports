(* xdisplay.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *)

structure XDisplay =
  struct

    exception BadAddr = XServerAddr.BadAddr

    local open Geometry XProtTypes in

    datatype xdisplay = XDPY of {
	conn : XIo.connection,		(* the connection to the server *)
	name : string,			(* "host:display.scr" *)
	vendor : string,		(* the name of the server's vendor *)
	default_scr : int,		(* the number of the default screen *)
	screens : xscreen list,		(* the screens attached to this display. *)
	pixmap_formats : pixmap_format list,
	max_req_len : int,
	image_byte_order : order,
	bitmap_bit_order : order,
	bitmap_scanline_unit : raw_format,
	bitmap_scanline_pad : raw_format,
	min_keycode : keycode,
	max_keycode : keycode,
	nextXId : unit -> xid		(* resource id allocator *)
      }

    and xscreen = XSCR of {
	id : int,			(* the number of this screen *)
	root : win_id,			(* the root window id of this screen *)
	cmap : colormap_id,		(* the default colormap *)
	white : pixel,			(* White and Black pixel values *)
	black : pixel,
	root_input_mask : event_mask,	(* initial root input mask *)
	sz_in_pixels : size,		(* the width and height in pixels *)
	sz_in_mm : size,		(* the width and height in millimeters *)
	min_installed_cmaps : int,
	max_installed_cmaps : int,
	root_visual : visual_depth,
	backing_store : backing_store,
	save_unders : bool,
	visualdepths : visual_depth list
      }

    end (* local open XProtTypes *)

    local
      open Geometry XProtTypes

      val << = Bits.lshift
      val >> = Bits.rshift
      val ++ = Bits.orb
      val & = Bits.andb
      infix << >> ++ &

    (* return index of first bit set (starting at 1), return 0 if n = 0, and
     * assume that n > 0. *)
      fun ffs 0 = 0
	| ffs n = let
	    fun lp (i, j) = if ((i & 1) = 0) then lp(i >> 1, j+1) else j
	    in
	      lp (n, 1)
	    end
    in

    local
      open ByteArray XServerAddr
      structure C = System.Unsafe.CInterface and SysIO = System.Unsafe.SysIO

      fun read (fd, buf, i, n) = (
	    CML.sync(CML.syncOnInput fd);
	    SysIO.readi (fd, buf, i, n))

      val write = System.Unsafe.SysIO.write

    (* Parse the address and open the appropriate kind of connection *)
      fun connect s = let
	    val {addr, dpy_name, screen} = (getServerAddr s)
	    val fd = (case addr
		   of (UNIX path) => (SysIO.connect_unix path)
		    | (INET addr) => let
			fun connect 0 = (SysIO.connect_inet addr)
			  | connect i =
			      ((SysIO.connect_inet addr) handle _ => connect(i-1))
			in
			  connect 4  (* try upto five times *)
			end
		  (* end case *))
		    handle (System.Unsafe.CInterface.SystemCall s) => raise (BadAddr s)
	    in
	      (fd, dpy_name, screen)
	    end

      fun initConnection fd = let
	    val strToBA : string -> bytearray = System.Unsafe.cast
	    val connect_msg = "B\000\000\011\000\000\000\000\000\000\000\000"
	    val _ = write(fd, strToBA connect_msg, size connect_msg)
	    fun readData nbytes = let
		  val buf = System.Unsafe.Assembly.A.create_s nbytes
		  fun fillBuf i = if (i < nbytes)
			then fillBuf(i + read(fd, strToBA buf, i, nbytes-i))
			else ()
		  in
		    fillBuf 0;
		    buf
		  end
	    val hdr = readData 8
	    val len = (((ordof(hdr, 6) << 8) ++ ordof(hdr, 7)) << 2)
	    val reply = readData len
	    in
	      if (ordof(hdr, 0) <> 1)
		then MLXError.xerror
		  ("connection refused: "^substring(reply, 0, ordof(hdr, 1)))
		else XReply.decodeConnectReqReply (hdr, reply)
	    end

    (* build a resource-id allocation function *)
      fun mkResourceFn (base, mask) = let
	    val resCh = CML.channel()
	    val incr = ffs(mask)
	    fun loop i = (CML.send(resCh, XID i); loop(i+incr))
	    in
	      (* CML.spawn (fn () => (loop base)); *)
	      XDebug.xspawn ("ResourceIdAlloc", fn () => (loop base));
	      fn () => (CML.accept resCh)
	    end

      fun mkScreen (scr_num) {
	      root_win, cmap, white, black, input_masks, pixel_wid, pixel_ht,
	      mm_wid, mm_ht, installed_maps = {min, max}, root_visualid,
	      backing_store, save_unders, root_depth, visualdepths
	    } = let
	    fun getRootVisual [] = (MLXError.xerror "cannot find root visual")
	      | getRootVisual ((Depth _) :: r) = getRootVisual r
	      | getRootVisual ((v as VisualDepth{id, depth, ...}) :: r) =
		  if ((id = root_visualid) andalso (depth = root_depth))
		    then v
		    else (getRootVisual r)
	    in
	      XSCR {
		  id = scr_num,
		  root = root_win,
		  cmap = cmap,
		  white = white,
		  black = black,
		  root_input_mask = input_masks,
		  sz_in_pixels = SIZE{wid = pixel_wid, ht = pixel_ht},
		  sz_in_mm = SIZE{wid = mm_wid, ht = mm_ht},
		  min_installed_cmaps = min,
		  max_installed_cmaps = max,
		  root_visual = getRootVisual visualdepths,
		  backing_store = backing_store,
		  save_unders = save_unders,
		  visualdepths = visualdepths
		}
	    end (* mkScreen *)
    fun mkScreens info_list = let
	  fun mkS (i, []) = []
	    | mkS (i, info::r) = (mkScreen i info) :: mkS(i+1, r)
	  in
	    mkS (0, info_list)
	  end
    in
    fun openXDisplay s = let
	  val (fd, name, scrNum) = connect s
	  val info = initConnection fd
	  val conn = XIo.openConn fd
	  val _ = XShutdown.logConnection conn
	  val screens = mkScreens (#roots info)
	  val (dpy as (XDPY dpyrec)) = XDPY {
		  conn = conn,
		  name = name,
		  vendor = #vendor info,
		  screens = screens,
		  default_scr = scrNum,
		  pixmap_formats = #formats info,
		  max_req_len = #max_req_len info,
		  image_byte_order = #im_byte_order info,
		  bitmap_bit_order = #bitmap_order info,
		  bitmap_scanline_unit = #bitmap_scanline_unit info,
		  bitmap_scanline_pad = #bitmap_scanline_pad info,
		  min_keycode = #min_keycode info,
		  max_keycode = #max_keycode info,
		  nextXId = mkResourceFn (#rsrc_id_base info, #rsrc_id_mask info)
		}
	  fun errHandler () = let
		val (seqn, errMsg) = XIo.readXError conn
		in
		  CIO.output(CIO.std_err, implode [
		      "Error on request #", makestring seqn, ": ",
		      XPrint.xerrorToString(XReply.decodeError errMsg),
		      "\n"]);
		  errHandler ()
		end
	  in
	    XDebug.xspawn("errHandler", errHandler);
	    dpy
	  end
    end (* local *)

  (* closeDisplay : xdisplay -> unit *)
    fun closeDisplay (XDPY{conn, ...}) = (
	  XIo.closeConn conn;
	  XShutdown.unlogConnection conn)

    fun depthOfVisual (Depth d) = d
      | depthOfVisual (VisualDepth{depth, ...}) = depth

    fun displayClassOfVisual (Depth _) = NONE
      | displayClassOfVisual (VisualDepth{class, ...}) = SOME class

    end (* local open *)

  end (* XDisplay *)
