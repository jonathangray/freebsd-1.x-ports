(* xreply.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * Routines to decode reply, error and event messages received from the server.
 *
 * TODO
 *   events
 *     decodeKeymapNotify
 *   replies
 *     decodeAllocColorCellsReply
 *     decodeAllocColorPlanesReply
 *     decodeGetPointerMappingReply
 *     decodeListExtensionsReply
 *     decodeQueryExtensionReply
 *     decodeQueryKeymapReply
 *)

structure XReply =
  struct
    local
      open Geometry XProtTypes XEventTypes

      val << = Bits.lshift
      val >> = Bits.rshift
      val & = Bits.andb
      val ++ = Bits.orb
      infix << >> & ++

      fun isSet(x, i) = (((x >> i) & 1) = 1)

      fun pad n = (case (n & 3) of 0 => n | r => (n+(4-r)))

      (* val get8 = System.Unsafe.ordof *)
      val get8 = String.ordof
      val getString = String.substring

      fun getSigned8 arg = let val x = get8 arg
	    in
	      if ((x & 128) = 0) then x else (x - 256)
	    end

      fun get16 (s, i) = let
	    val b1 = get8(s, i) and b2 = get8(s, i+1)
	    in
	      (b1 << 8) ++ b2
	    end

      fun getSigned16 arg = let
	    val x = get16 arg
	    in
	      if ((x & 32768) = 0) then x else (x - 65536)
	    end

      fun get32 (s, i) = let
	    val b1 = get8(s, i) and b2 = get8(s, i+1)
	    and b3 = get8(s, i+2) and b4 = get8(s, i+3)
	    in
	      if (b1 > 63)
	        then raise (MLXError.xerror "CARD32 too big")
	        else (b1 << 24) ++ (b2 << 16) ++ (b3 << 8) ++ b4
	    end

      fun getList (f, sz : int) (buf, i, n) = let
	    fun get (_, 0, l) = List.rev l
	      | get (i, n, l) = get (i+sz, n-1, f(buf, i)::l)
	    in
	      get (i, n, [])
	    end

    (* get a list of strings, where each string is preceded by a one-byte length
     * field.
     *)
      fun getStringList (buf, i, n) = let
	    fun get (_, 0, l) = List.rev l
	      | get (i, n, l) = let
		  val len = get8(buf, i) and j = i+1
		  in
		    get (j+len, n-1, getString(buf, j, len) :: l)
		  end
	    in
	      get (i, n, [])
	    end

      val getXAtom = (XAtom o get32)
      fun getXAtomOption arg =  (case get32 arg of 0 => NONE | x => SOME(XAtom x))
      val getXId = (XID o get32)
      fun getXIdOption arg = (case get32 arg of 0 => NONE | x => SOME(XID x))

      val getEventMask = (XEVTMASK o get32)
      val getVisualId = (VISUALID o get32)
      fun getVisualIdOption arg = (case get32 arg of 0 => NONE | x => SOME(VISUALID x))
      val getPixel = PIXEL o get32

      local
	open System.Timer
	val addin01 = TIME{sec = 1073741, usec = 824000} (* (2^30) ms. *)
	val addin10 = TIME{sec = 2147483, usec = 648000} (* (2^31) ms. *)
	val addin11 = TIME{sec = 3221225, usec = 472000} (* (2^31+2^30) ms. *)
      in
      fun getTS (s, i) = let
	    val hi8 = get8(s, i)
	    val lo24 = (get8(s, i+1) << 16) ++ get16(s, i+2)
	    fun cvt t = TIME{sec = t div 1000, usec = (t mod 1000)*1000}
	    in
	      if ((lo24 = 0) andalso (hi8 = 0))
		then MLXError.impossible "[getTS: server generated CurrentTime]"
	      else let
		val base = cvt (((hi8 & 63) << 24) ++ lo24)
		in
		  case (hi8 >> 6)
		   of 0 => base
		    | 1 => add_time (base, addin01)
		    | 2 => add_time (base, addin10)
		    | _ => add_time (base, addin11)
		end
	    end
      end (* local *)

      fun getBool arg = (case (get8 arg) of 0 => false | _ => true)
      fun getPt (s, i) = PT { x = getSigned16(s, i), y = getSigned16(s, i+2) }
      fun getSize (s, i) = SIZE { wid = get16(s, i), ht = get16(s, i+2) }
      fun getRect (s, i) = RECT {
	      x = getSigned16(s, i), y = getSigned16(s, i+2),
	      wid = get16(s, i+4), ht = get16(s, i+6)
	    }
      fun getWGeom (s, i) = WGEOM {
	      pos = getPt(s, i), sz = getSize(s, i+4), border = get16(s, i+8)
	    }

      val getKeyCode = (KEYCODE o get8)

      fun getStkPos arg = (case (get8 arg) of 0 => PlaceOnTop | _ => PlaceOnBottom)

      fun getFocusMode (s, i) = (case get8(s, i)
	   of 0 => FocusNormal | 1 => FocusGrab
	    | 2 => FocusUngrab | 3 => FocusWhileGrabbed
	    | _ => MLXError.impossible "bad focus mode")
      fun getFocusDetail (s, i) = (case get8(s, i)
	   of 0 => FocusAncestor | 1 =>FocusVirtual | 2 => FocusInferior
	    | 3 => FocusNonlinear | 4 => FocusNonlinearVirtual
	    | 5 => FocusPointer | 6 => FocusPointerRoot | 7 => FocusNone
	    | _ => MLXError.impossible "bad focus detail")

      fun getKeyButSet (s, i) = let val m = get16(s, i)
	    in
	      (MKState(m & 0xFF), MBState(m & 0x1F00))
	    end

      fun getRGB (buf, i) =
	    RGB{red = get16(buf, i), green = get16(buf, i+2), blue = get16(buf, i+4) }

      fun getBS (buf, i) = (case get8(buf, i)
	    of 0 => BS_NotUseful | 1 => BS_WhenMapped | _ => BS_Always)

      fun getFontDir (buf, i) = (case get8(buf, i)
	    of 0 => FontLeftToRight | 1 => FontRightToLeft
	     | _ => MLXError.impossible "bad font direction")

      val getXIdList = getList (getXId, 4)
      val getXAtomList = getList (getXAtom, 4)

      local
        fun intToGravity 1 = SOME NorthWestGravity
	  | intToGravity 2 = SOME NorthGravity
	  | intToGravity 3 = SOME NorthEastGravity
	  | intToGravity 4 = SOME WestGravity
	  | intToGravity 5 = SOME CenterGravity
	  | intToGravity 6 = SOME EastGravity
	  | intToGravity 7 = SOME SouthWestGravity
	  | intToGravity 8 = SOME SouthGravity
	  | intToGravity 9 = SOME SouthEastGravity
	  | intToGravity 10 = SOME StaticGravity
	  | intToGravity _ = NONE
      in
      fun getBitGravity arg = (
	    case intToGravity(get8 arg) of NONE => ForgetGravity | SOME g => g)
      fun getWinGravity arg = (
	    case intToGravity(get8 arg) of NONE => UnmapGravity | SOME g => g)
      end (* local *)

      fun getRawFormat arg = (case (get8 arg)
	   of 8 => Raw8 | 16 => Raw16 | 32 => Raw32
	    | _ => MLXError.impossible "[getRawFormat: bad ClientMessage]")

    in

  (** Get the reply from a connection request **)
    local
      val prefix_sz = 8
      fun getOrder(buf, i) = (case get8(buf, i) of 0 => LSBFirst | _ => MSBFirst)
      fun getFormat (buf, i) = FORMAT {
	      depth = get8(buf, i), 
	      bits_per_pixel = get8(buf, i+1), 
	      scanline_pad = getRawFormat(buf, i+2)
	    }
      fun getVisualDepth (buf, i, depth) = VisualDepth {
	      id = getVisualId(buf, i),
	      depth = depth,
	      class = (case get8(buf, i+4)
		 of 0 => StaticGray | 1 => GrayScale | 2 => StaticColor
		  | 3 => PseudoColor | 4 => TrueColor | 5 => DirectColor
		  | _ => MLXError.impossible "bad visual depth"),
	      bits_per_rgb = get8(buf, i+5),
	      cmap_entries = get16(buf, i+6),
	      red_mask = get32(buf, i+8),
	      green_mask = get32(buf, i+12),
	      blue_mask = get32(buf, i+16)
	    }
      fun getVisualDepthList (buf, i, ndepths) = let
	    fun getDepths (0, i, l) = (List.rev l, i)
	      | getDepths (ndepths, i, l) = let
		  val depth = get8(buf, i)
		  in
		    case (get16(buf, i+2))
		     of 0 => getDepths (ndepths-1, i+8, (Depth depth)::l)
		      | nVisuals => getVisuals (ndepths-1, depth, nVisuals, i+8, l)
		  end
	    and getVisuals (ndepths, _, 0, i, l) = getDepths (ndepths, i, l)
	      | getVisuals (ndepths, depth, k, i, l) =
		  getVisuals (ndepths, depth, k-1, i+24,
		    getVisualDepth(buf, i, depth)::l)
	    in
	      getDepths (ndepths, i, [])
	    end
      fun getScreen (buf, i) = let
	    val (vdepths, next) = getVisualDepthList(buf, i+40, get8(buf, i+39))
	    in (
	      {
		root_win = getXId(buf, i),
		cmap = getXId(buf, i+4),
		white = getPixel(buf, i+8),
		black = getPixel(buf, i+12),
		input_masks = getEventMask(buf, i+16),
		pixel_wid = get16(buf, i+20),
		pixel_ht = get16(buf, i+22),
		mm_wid = get16(buf, i+24),
		mm_ht = get16(buf, i+26),
		installed_maps = {min = get16(buf, i+28), max = get16(buf, i+30)},
		root_visualid = getVisualId(buf, i+32),
		backing_store = getBS(buf, i+36),
		save_unders = getBool(buf, i+37),
		root_depth = get8(buf, i+38),
		visualdepths = vdepths
	      }, next)
	    end
      val getFormats = getList (getFormat, 8)
      fun getScreens (buf, i, nscreens) = let
	    fun get (0, _, l) = List.rev l
	      | get (n, i, l) = let
		  val (scr, next) = getScreen(buf, i)
		  in
		    get(n-1, next, scr::l)
		  end
	    in
	      get (nscreens, i, [])
	    end
    in
    fun decodeConnectReqReply (prefix, msg) = let
	  val vendor_len = get16(msg, 16)
	  val nscreens = get8(msg, 20)
	  val nformats = get8(msg, 21)
	  val format_offset = pad (32 + vendor_len)
	  val screen_offset = format_offset + 8*nformats
	  in {
	    protocol_version = {
		major = get16(prefix, 2),
		minor = get16(prefix, 4)
	      },
	    release_num = get32(msg, 0),
	    rsrc_id_base = get32(msg, 4),
	    rsrc_id_mask = get32(msg, 8),
	    motion_buf_sz = get32(msg, 12),
	    max_req_len = get16(msg, 18),
	    im_byte_order = getOrder(msg, 22),
	    bitmap_order = getOrder(msg, 23),
	    bitmap_scanline_unit = getRawFormat(msg, 24),
	    bitmap_scanline_pad = getRawFormat(msg, 25),
	    min_keycode = getKeyCode(msg, 26),
	    max_keycode = getKeyCode(msg, 27),
	    vendor = getString(msg, 32, vendor_len),
	    formats = getFormats(msg, format_offset, nformats),
	    roots = getScreens(msg, screen_offset, nscreens)
	  } end
    end (* local *)


  (** decode event messages **)

    local
      fun getKeyXEvt buf = let val (mks, mbs) = getKeyButSet(buf, 28)
	    in {
	      keycode = getKeyCode(buf, 1),
	      time = getTS(buf, 4),
	      root = getXId(buf, 8),
	      event = getXId(buf, 12),
	      child = getXIdOption(buf, 16),
	      root_pt = getPt(buf, 20),
	      event_pt = getPt(buf, 24),
	      mod_state = mks,
	      mbut_state = mbs,
	      same_screen = getBool(buf, 30)
	    } end
      fun getButtonXEvt buf = let val (mks, mbs) = getKeyButSet(buf, 28)
	    in {
	      button = MButton(get8(buf, 1)),
	      time = getTS(buf, 4),
	      root = getXId(buf, 8),
	      event = getXId(buf, 12),
	      child = getXIdOption(buf, 16),
	      root_pt = getPt(buf, 20),
	      event_pt = getPt(buf, 24),
	      mod_state = mks,
	      mbut_state = mbs,
	      same_screen = getBool(buf, 30)
	    } end
      fun decodeMotionNotify buf = let
	    val (mks, mbs) = getKeyButSet(buf, 28)
	    in
	      MotionNotifyXEvt {
		  hint = getBool(buf, 1),
		  time = getTS(buf, 4),
		  root = getXId(buf, 8),
		  event = getXId(buf, 12),
		  child = getXIdOption(buf, 16),
		  root_pt = getPt(buf, 20),
		  event_pt = getPt(buf, 24),
		  mod_state = mks,
		  mbut_state = mbs,
		  same_screen = getBool(buf, 30)
		}
	    end
      fun getEnterLeaveXEvt buf = let
	    val (mks, mbs) = getKeyButSet(buf, 28)
	    val flags = get8(buf, 31)
	    in {
	      detail = getFocusDetail(buf, 1),
	      time = getTS(buf, 4),
	      root = getXId(buf, 8),
	      event = getXId(buf, 12),
	      child = getXIdOption(buf, 16),
	      root_pt = getPt(buf, 20),
	      event_pt = getPt(buf, 24),
	      mod_state = mks,
	      mbut_state = mbs,
	      mode = getFocusMode(buf, 30),
	      focus = isSet(flags, 0),
	      same_screen = isSet(flags, 1)
	    } end
      fun getFocusXEvt buf = {
	      detail = getFocusDetail(buf, 1),
	      event = getXId(buf, 4),
	      mode = getFocusMode(buf, 8)
	    }
      fun decodeKeymapNotify buf = KeymapNotifyXEvt {}(** NOTE: no seqn # **) (** FIX **)
      fun decodeExpose buf = ExposeXEvt {
	      window = getXId(buf, 4),
	      rects = [getRect(buf, 8)],
	      count = get16(buf, 16)
	    }
      fun decodeGraphicsExpose buf = GraphicsExposeXEvt {
	      drawable = getXId(buf, 4),
	      rect = getRect(buf, 8),
	      minor_opcode = get16(buf, 16),
	      count = get16(buf, 18),
	      major_opcode = get16(buf, 20)
	    }
      fun decodeNoExpose buf = NoExposeXEvt {
	      drawable = getXId(buf, 4),
	      minor_opcode = get16(buf, 8),
	      major_opcode = get16(buf, 10)
	    }
      fun decodeVisibilityNotify buf = VisibilityNotifyXEvt {
	      window = getXId(buf, 4),
	      state = case get8(buf, 8)
		 of 0 => VisibilityUnobscured
		  | 1 => VisibilityPartiallyObscured
		  | 2 => VisibilityFullyObscured
		  | _ => MLXError.impossible "bad VisibilityNotify"
	    }
      fun decodeCreateNotify buf = CreateNotifyXEvt {
	      parent = getXId(buf, 4),
	      window = getXId(buf, 8),
	      rect = getRect(buf, 12),
	      border_wid = get16(buf, 20),
	      override_redirect = getBool(buf, 21)
	    }
      fun decodeDestroyNotify buf = DestroyNotifyXEvt {
	      event = getXId(buf, 4),
	      window = getXId(buf, 8)
	    }
      fun decodeUnmapNotify buf = UnmapNotifyXEvt {
	      event = getXId(buf, 4),
	      window = getXId(buf, 8),
	      from_config = getBool(buf, 12)
	    }
      fun decodeMapNotify buf = MapNotifyXEvt {
	      event = getXId(buf, 4),
	      window = getXId(buf, 8),
	      override_redirect = getBool(buf, 12)
	    }
      fun decodeMapRequest buf = MapRequestXEvt {
	      parent = getXId(buf, 4),
	      window = getXId(buf, 8)
	    }
      fun decodeReparentNotify buf = ReparentNotifyXEvt {
	      event = getXId(buf, 4),
	      parent = getXId(buf, 8),
	      window = getXId(buf, 12),
	      corner = getPt(buf, 16),
	      override_redirect = getBool(buf, 20)
	    }
      fun decodeConfigureNotify buf = ConfigureNotifyXEvt {
	      event = getXId(buf, 4),
	      window = getXId(buf, 8),
	      sibling = getXIdOption(buf, 12),
	      rect = getRect(buf, 16),
	      border_wid = get16(buf, 20),
	      override_redirect = getBool(buf, 22)
	    }
      fun decodeConfigureRequest buf = let
	    val mask = get16(buf, 26)
	    fun getOpt16 (i, j) = if isSet(mask, i) then SOME(get16(buf, j)) else NONE
	    in
	      ConfigureRequestXEvt {
		  stack_mode = if isSet(mask, 6)
		    then (case get8(buf, 1)
		     of 0 => SOME Above | 1 => SOME Below | 2 => SOME TopIf
		      | 3 => SOME BottomIf | 4 => SOME Opposite
		      | _ => MLXError.impossible "bad ConfigureRequest")
		    else NONE,
		parent = getXId(buf, 4),
		window = getXId(buf, 8),
		sibling = getXIdOption(buf, 12),
		x = getOpt16(0, 16),
		y = getOpt16(1, 18),
		wid = getOpt16(2, 20),
		ht = getOpt16(3, 22),
		border_wid = getOpt16(3, 24)
	      }
	    end
      fun decodeGravityNotify buf = GravityNotifyXEvt {
	      event = getXId(buf, 4),
	      window = getXId(buf, 8),
	      corner = getPt(buf, 12)
	    }
      fun decodeResizeRequest buf = ResizeRequestXEvt {
	      window = getXId(buf, 4),
	      req_sz = getSize(buf, 8)
	    }
      fun decodeCirculateNotify buf = CirculateNotifyXEvt {
	      event = getXId(buf, 4),
	      window = getXId(buf, 8),
	      parent = getXId(buf, 12),
	      place = getStkPos(buf, 16)
	    }
      fun decodeCirculateRequest buf = CirculateRequestXEvt {
	      parent = getXId(buf, 4),
	      window = getXId(buf, 8),
	      place = getStkPos(buf, 12)
	    }
      fun decodePropertyNotify buf = PropertyNotifyXEvt {
	      window = getXId(buf, 4),
	      atom = getXAtom(buf, 8),
	      time = getTS(buf, 12),
	      deleted = getBool(buf, 16)		
	    }
      fun decodeSelectionClear buf = SelectionClearXEvt {
	      time = getTS(buf, 4),
	      owner = getXId(buf, 8),
	      selection = getXAtom(buf, 12)
	    }
      fun decodeSelectionRequest buf = SelectionRequestXEvt {
	      time = getTS(buf, 4),
	      owner = getXId(buf, 8),
	      requestor = getXId(buf, 12),
	      selection = getXAtom(buf, 16),
	      target = getXAtom(buf, 20),
	      property = getXAtomOption(buf, 24)
	    }
      fun decodeSelectionNotify buf = SelectionNotifyXEvt {
	      time = getTS(buf, 4),
	      requestor = getXId(buf, 8),
	      selection = getXAtom(buf, 12),
	      target = getXAtom(buf, 16),
	      property = getXAtomOption(buf, 20)
	    }
      fun decodeColormapNotify buf = ColormapNotifyXEvt {
	      window = getXId(buf, 4),
	      cmap = getXIdOption(buf, 8),
	      new = getBool(buf, 12),
	      installed = getBool(buf, 13)
	    }
      fun decodeClientMessage buf = ClientMessageXEvt {
	      window = getXId(buf, 4),
	      typ = getXAtom(buf, 8),
	      value = RAW_DATA {
		  format = getRawFormat(buf, 1),
		  data = getString(buf, 12, 20)
		}
	    }
      fun decodeMappingNotify buf = (case get8(buf, 4)
	   of 0 => ModifierMappingNotifyXEvt
	    | 1 => KeyboardMappingNotifyXEvt {
		      first_keycode = getKeyCode(buf, 5),
		      count = get8(buf, 6)
		    }
	    | 2 => PointerMappingNotifyXEvt
	    | _ => MLXError.impossible "bad MappingNotify")
    in
    fun decodeXEvent (code, buf) = let
	  val n = (code & 0x7f)
	  val xevt = case n
	       of 2 => KeyPressXEvt (getKeyXEvt buf)
		| 3 => KeyReleaseXEvt (getKeyXEvt buf)
		| 4 => ButtonPressXEvt (getButtonXEvt buf)
		| 5 => ButtonReleaseXEvt (getButtonXEvt buf)
		| 6 => decodeMotionNotify buf
		| 7 => EnterNotifyXEvt (getEnterLeaveXEvt buf)
		| 8 => LeaveNotifyXEvt (getEnterLeaveXEvt buf)
		| 9 => FocusInXEvt (getFocusXEvt buf)
		| 10 => FocusOutXEvt (getFocusXEvt buf)
		| 11 => decodeKeymapNotify buf
		| 12 => decodeExpose buf
		| 13 => decodeGraphicsExpose buf
		| 14 => decodeNoExpose buf
		| 15 => decodeVisibilityNotify buf
		| 16 => decodeCreateNotify buf
		| 17 => decodeDestroyNotify buf
		| 18 => decodeUnmapNotify buf
		| 19 => decodeMapNotify buf
		| 20 => decodeMapRequest buf
		| 21 => decodeReparentNotify buf
		| 22 => decodeConfigureNotify buf
		| 23 => decodeConfigureRequest buf
		| 24 => decodeGravityNotify buf
		| 25 => decodeResizeRequest buf
		| 26 => decodeCirculateNotify buf
		| 27 => decodeCirculateRequest buf
		| 28 => decodePropertyNotify buf
 		| 29 => decodeSelectionClear buf
		| 30 => decodeSelectionRequest buf
		| 31 => decodeSelectionNotify buf
		| 32 => decodeColormapNotify buf
		| 33 => decodeClientMessage buf
		| 34 => decodeMappingNotify buf
		| _ => MLXError.impossible "bad event code"
	  in
	    (code = n, xevt)
	  end
  (* we export the decode functions for reporting graphics exposures *)
    val decodeGraphicsExpose = decodeGraphicsExpose
    val decodeNoExpose = decodeNoExpose
    end (* local *)


  (** decode error messages **)
      local
	open XErrors
	fun get_err (kind, buf) = XErr{
		kind = kind,
		minor_op = get16(buf, 8),
		major_op = get8(buf, 10)
	      }
      in
      fun decodeError buf = (case (get8 (buf, 1))
	   of 1 => get_err (BadRequest, buf)
	    | 2 => get_err (BadValue(getString(buf, 4, 4)), buf)
	    | 3 => get_err (BadWindow(getXId(buf, 4)), buf)
	    | 4 => get_err (BadPixmap(getXId(buf, 4)), buf)
	    | 5 => get_err (BadAtom(getXId(buf, 4)), buf)
	    | 6 => get_err (BadCursor(getXId(buf, 4)), buf)
	    | 7 => get_err (BadFont(getXId(buf, 4)), buf)
	    | 8 => get_err (BadMatch, buf)
	    | 9 => get_err (BadDrawable(getXId(buf, 4)), buf)
	    | 10 => get_err (BadAccess, buf)
	    | 11 => get_err (BadAlloc, buf)
	    | 12 => get_err (BadColor(getXId(buf, 4)), buf)
	    | 13 => get_err (BadGC(getXId(buf, 4)), buf)
	    | 14 => get_err (BadIDChoice(getXId(buf, 4)), buf)
	    | 15 => get_err (BadName, buf)
	    | 16 => get_err (BadLength, buf)
	    | 17 => get_err (BadImplementation, buf)
	    | _ => MLXError.impossible "bad error number")
      end (* local *)


  (** decode reply messages **)

    fun decodeGetWindowAttributesReply msg = {
	    backing_store = getBS(msg, 1),
	    visual = getVisualId(msg, 8),
	    input_only = case get16(msg, 12)
	       of 1 => false | 2 => true
		| _ => MLXError.impossible "bad GetWindowAttributes reply",
	    bit_gravity = getBitGravity(msg, 14),
	    win_gravity = getWinGravity(msg, 15),
	    backing_planes = PLANEMASK(get32(msg, 16)),
	    backing_pixel = getPixel(msg,20),
	    save_under = getBool(msg, 24),
	    map_is_installed = getBool(msg, 25),
	    map_state = case get8(msg, 26)
	       of 0 => WinIsUnmapped | 1 => WinIsUnviewable | 2 => WinIsViewable
		| _ => MLXError.impossible "bad GetWindowAttributes reply",
	    override_redirect = getBool(msg, 27),
	    colormap = getXIdOption(msg, 28),
	    all_event_mask = getEventMask(msg, 32),
	    event_mask = getEventMask(msg, 36),
	    do_not_propagate = getEventMask(msg, 40)
	  }

    fun decodeAllocColorCellsReply msg = {
	    err = MLXError.impossible "unimplemented" (*** FIX ***)
	  }
    fun decodeAllocColorPlanesReply msg = {
	    err = MLXError.impossible "unimplemented" (*** FIX ***)
	  }

    fun decodeAllocColorReply msg = {
	    visual_rgb = getRGB(msg, 8),
	    pixel = getPixel(msg, 16)
	  }

    fun decodeAllocNamedColorReply msg = {
	    pixel = getPixel(msg, 8),
	    exact_rgb = getRGB(msg, 12),
	    visual_rgb = getRGB(msg, 18)
	  }

    fun decodeGetAtomNameReply msg = getString(msg, 32, get16(msg, 8))

    fun decodeGetFontPathReply msg = getStringList (msg, 32, get16(msg, 8))

    fun decodeGetGeometryReply msg = {
	    depth = get8(msg, 1),
	    root = getXId(msg, 8),
	    geom = getWGeom(msg, 12)
	  }

    fun decodeGetImageReply msg = {
	    depth = get8(msg, 1),
	    visualid = getVisualIdOption(msg, 8),
	    data = getString(msg, 32, (get32(msg, 4)) << 2)
	  }

    fun decodeGetInputFocusReply msg = {
	    revert_to = case get8(msg, 1)
	       of 0 => RevertToNone | 1 => RevertToPointerRoot | _ => RevertToParent,
	    focus = case get32(msg, 8)
	       of 0 => InputFocus_None
		| 1 => InputFocus_PointerRoot
		| w => (InputFocus_Window(XID w))
	  }

    fun decodeGetKeyboardControlReply msg = {
	    glob_auto_repeat = getBool(msg, 1),
	    led_mask = get32(msg, 8),
	    key_click_pct = get8(msg, 12),
	    bell_pct = get8(msg, 13),
	    bell_pitch = get16(msg, 14),
	    bell_duration = get16(msg, 16),
	    auto_repeats = getString(msg, 20, 32)
	  }

    fun decodeGetKeyboardMappingReply msg = let
	  val symsPerCode = get8(msg, 1)
	  val nKeyCodes = get32(msg, 4) quot symsPerCode
	(* get the keysyms bound to a given keycode;  Discard trailing NoSymbols,
	 * but include intermediate ones. *)
	  fun cleanTl (NoSymbol :: r) = cleanTl r
	    | cleanTl l = rev l
	  fun getSyms (i, 0, l) = cleanTl l
	    | getSyms (i, j, l) = (case get32(msg, i)
	       of 0 => getSyms(i+4, j-1, NoSymbol :: l)
		| k => getSyms(i+4, j-1, (KEYSYM k) :: l))
	  in
	    getList (fn (_, i) => getSyms(i, symsPerCode, []), symsPerCode*4)
	      (msg, 32, nKeyCodes)
          end

    fun decodeGetModifierMappingReply msg = let
	  val codesPerMod = get8(msg, 1)
	  fun getSyms k = let
		fun get (i, 0) = []
		  | get (i, j) = (case get8(msg, i)
		     of 0 => get(i+1, j-1)    (* 0 == unused *)
		      | k => (KEYCODE k) :: get(i+1, j-1))
		in
		  get (32 + codesPerMod*k, codesPerMod)
		end
	  in {
	    shift_keycodes = getSyms 0,
	    lock_keycodes = getSyms 1,
	    cntl_keycodes = getSyms 2,
	    mod1_keycodes = getSyms 3,
	    mod2_keycodes = getSyms 4,
	    mod3_keycodes = getSyms 5,
	    mod4_keycodes = getSyms 6,
	    mod5_keycodes = getSyms 7
          } end

    local
      val getEvts = getList
	    ((fn (buf, i) => { time = getTS(buf, i), coord = getPt(buf, i+4) }), 8)
    in
    fun decodeGetMotionEventsReply msg = getEvts (msg, 32, get16(msg, 8))
    end (* local *)

    fun decodeGetPointerControlReply msg = {
	    acceleration_numerator = get16(msg, 8),
	    acceleration_denominator = get16(msg, 10),
	    threshold = get16(msg, 12)
	  }

    fun decodeGetPointerMappingReply msg = {
	    err = MLXError.impossible "unimplemented" (*** FIX ***)
	  }

    fun decodeGetPropertyReply msg = (case get32(msg, 8)
	   of 0 => NONE
	    | t => let
		val nitems = get32(msg, 16)
		val (fmt, nbytes) = case get8(msg, 1)
		     of 8 => (Raw8, nitems)
		      | 16 => (Raw16, nitems << 1)
		      | 32 => (Raw32, nitems << 2)
		      | _ => MLXError.impossible "bad GetProperty reply"
		in
		  SOME {
		      typ = getXAtomOption(msg, 8),
		      bytes_after = get32(msg, 12),
		      value = RAW_DATA {
			  format = fmt,
			  data = getString(msg, 32, nbytes)
			}
		    }
		end)

    fun decodeGetScreenSaverReply msg = {
	    timeout = get16(msg, 8),
	    interval = get16(msg, 10),
	    prefer_blanking = getBool(msg, 12),
	    allow_exposures = getBool(msg, 13)
	  }

    fun decodeGetSelectionOwnerReply msg = getXIdOption(msg, 8)

    local
      fun decodeGrabReply msg = (
	    case get8(msg, 1)
	     of 0 => GrabSuccess | 1 => AlreadyGrabbed
	      | 2 => GrabInvalidTime | 3 => GrabNotViewable
	      | _ => GrabFrozen)
    in
    val decodeGrabKeyboardReply = decodeGrabReply
    val decodeGrabPointerReply = decodeGrabReply
    end (* local *)

    fun decodeInternAtomReply msg = getXAtom(msg, 8)

    fun decodeListExtensionsReply msg = {
	    err = MLXError.impossible "unimplemented" (*** FIX ***)
	  }

    fun decodeListFontsReply msg = getStringList (msg, 32, get16(msg, 8))

    local
      fun getHostList (buf, n) = let
	    fun get (_, 0, l) = l
	      | get (i, n, l) = let
		  val addr_len = get16(buf, i+2)
		  val addr = getString(buf, i+4, addr_len)
		  val host = case get8(buf, i)
			 of 0 => InternetHost addr
			  | 1 => DECnetHost addr
			  | 2 => ChaosHost addr
			  | _ => raise (MLXError.xerror "unknown host family")
		  in
		    get (i+(pad addr_len)+4, n-1, host::l)
		  end
	    in
	      get (32, n, [])
	    end
    in
    fun decodeListHostsReply msg = {
	    enabled = getBool(msg, 1),
	    hosts = getHostList(msg, get16(msg, 8))
	  }
    end (* local *)

    fun decodeListInstalledColormapsReply msg = getXIdList(msg, 32, get16(msg, 8))

    fun decodeListPropertiesReply msg = getXAtomList(msg, 32, get16(msg, 8))

    fun decodeLookupColorReply msg = {
	    exact_rgb = getRGB(msg, 8),
	    visual_rgb = getRGB(msg, 14)
	  }

    fun decodeQueryBestSizeReply msg = {
	    wid = get16(msg, 8),
	    ht = get16(msg, 10)
	  }

    local
      val getRGBList = getList (getRGB, 8)
    in
    fun decodeQueryColorsReply msg = getRGBList(msg, 32, get16(msg, 8))
    end (* local *)

    fun decodeQueryExtensionReply msg = {
	    err = MLXError.impossible "unimplemented" (*** FIX ***)
	  }

    local
      val getProps = getList ((fn (buf, i) => FontProp {
		name = getXAtom(buf, i),
		value = getString(buf, i+4, 4)
	      }), 8)
      fun getCharInfo (buf, i) = CharInfo {
		left_bearing = getSigned16(buf, i),
		right_bearing = getSigned16(buf, i+2),
		char_wid = getSigned16(buf, i+4),
		ascent = getSigned16(buf, i+6),
		descent = getSigned16(buf, i+8),
		attributes = get16(buf, i+10)
	      }
      val getCharInfoList = getList (getCharInfo, 12)
      fun getInfo buf = let
	    val n_props = get16(buf, 46)
	    in {
	      min_bounds = getCharInfo(buf, 8),
	      max_bounds = getCharInfo(buf, 24),
	      min_char = get16(buf, 40),
	      max_char = get16(buf, 42),
	      default_char = get16(buf, 44),
	      draw_dir = getFontDir(buf, 48),
	      min_byte1 = get8(buf, 49),
	      max_byte1 = get8(buf, 50),
	      all_chars_exist = getBool(buf, 51),
	      font_ascent = get16(buf, 52),
	      font_descent = get16(buf, 54),
	      n_props = n_props,
	      properties = getProps(buf, 60, n_props)
	    } end
    in
(****** THIS GENERATES MULTIPLE REPLYS ****
  (* this gets a list of font name/info replies *)
    fun decodeListFontsWithInfoReply msg = let
	  fun getList l = let
	    val (msg, extra) = getReply (conn, sizeOfListFontsWithInfoReply)
	    val name_len = get8(msg, 1)
	    in
	      if (name_len = 0)
		then (* this is the last in a series of replies *)
		  (rev l)
		else let
		  val info = getInfo(msg, extra)
		  val reply = {
			  min_bounds = #min_bounds info,
			  max_bounds = #max_bounds info,
			  min_char = #min_char info,
			  max_char = #max_char info,
			  default_char = #default_char info,
			  draw_dir = #draw_dir info,
			  min_byte1 = #min_byte1 info,
			  max_byte1 = #max_byte1 info,
			  all_chars_exist = #all_chars_exist info,
			  font_ascent = #font_ascent info,
			  font_descent = #font_descent info,
			  replies_hint = get32(msg, 56),
			  properties = #properties info,
			  name = getString(extra, 8*(#n_props info), name_len)
			}
		  in
		    getList (reply :: l)
		  end
	    end (* getList *)
	  in
	    getList []
	  end (* getListFontsWithInfoReply *)
*********)

    fun decodeQueryFontReply msg = let
	  val info = getInfo msg
	  in {
	    min_bounds = #min_bounds info,
	    max_bounds = #max_bounds info,
	    min_char = #min_char info,
	    max_char = #max_char info,
	    default_char = #default_char info,
	    draw_dir = #draw_dir info,
	    min_byte1 = #min_byte1 info,
	    max_byte1 = #max_byte1 info,
	    all_chars_exist = #all_chars_exist info,
	    font_ascent = #font_ascent info,
	    font_descent = #font_descent info,
	    properties = #properties info,
	    char_infos = getCharInfoList(msg, 60+8*(#n_props info), get32(msg, 56))
	  } end
    end (* local *)

    fun decodeQueryKeymapReply msg = {
	    err = MLXError.impossible "unimplemented" (*** FIX ***)
	  }

    fun decodeQueryPointerReply msg = let val (mks, mbs) = getKeyButSet(msg, 24)
	  in {
	    same_scr = getBool(msg, 1),
	    root = getXId(msg, 8),
	    child = getXIdOption(msg, 12),
	    root_pt = getPt(msg, 16),
	    win_pt = getPt(msg, 20),
	    mod_state = mks,
	    mbut_state = mbs
	  } end

    fun decodeQueryTextExtentsReply msg = {
	    draw_dir = getFontDir(msg, 1),
	    font_ascent = get16(msg, 8),
	    font_descent = get16(msg, 10),
	    overall_ascent = get16(msg, 12),
	    overall_descent = get16(msg, 14),
	    overall_wid = get16(msg, 16),
	    overall_left = get16(msg, 18),
	    overall_right = get16(msg, 20)
	  }

    fun decodeQueryTreeReply msg = {
	    root = getXId(msg, 8),
	    parent = getXIdOption(msg, 12),
	    children = getXIdList (msg, 32, get16(msg, 16)) 
	  }

    local
    fun getSetMappingReply msg = (case get8(msg, 1)
	   of 0 => MappingSuccess | 1 => MappingBusy | _ => MappingFailed)
    in
    val decodeSetModifierMappingReply = getSetMappingReply
    val decodeSetPointerMappingReply = getSetMappingReply
    end (* local *)

    fun decodeTranslateCoordsReply msg = {
	    child = getXIdOption(msg, 8),
	    dst_pt = getPt(msg, 12)
	  }

    end (* local open XTypes *)

  end (* XReply *)
