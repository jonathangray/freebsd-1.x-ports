(* xrequest.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * Functions for encoding X11 protocol request messages.
 *
 * TODO
 *   - encodeAllocColorCells
 *   - encodeAllocColorPlanes
 *   - encodeChangeKeyboardMapping
 *   - encodeSetPointerMapping
 *   - encodeGetPointerMapping
 *   - encodeSetModifierMapping
 *)

structure XRequest =
  struct
    local
      open Geometry XProtTypes

      val << = Bits.lshift
      val >> = Bits.rshift
      val & = Bits.andb
      val ++ = Bits.orb
      infix << >> & ++

      fun pad n = if ((n & 3) <> 0) then pad(n+1) else n

    (* We use uninitialized strings and destructively update them *)
      val mkReqBuf = System.Unsafe.Assembly.A.create_s
      val (put8 : (string * int * int) -> unit) = System.Unsafe.cast ByteArray.update

      fun putSigned8 (buf, i, x) = if (x < 0)
	    then put8(buf, i, x + 256)
	    else put8(buf, i, x)
      fun put16 (buf, i, x) = let val a = (x >> 8) and b = (x & 255)
	    in
	      put8(buf, i, a); put8(buf, i+1, b)
	    end
      fun putSigned16 (buf, i, x) = if (x < 0)
	    then put16(buf, i, x + 65536)
	    else put16(buf, i, x)
      fun put32 (buf, i, x) = let
	    val a = ((x >> 24) & 255) and b = ((x >> 16) & 255)
	    val c = ((x >> 8) & 255) and d = (x & 255)
	    in
	      put8(buf, i, a); put8(buf, i+1, b);
	      put8(buf, i+2, c); put8(buf, i+3, d)
	    end

      fun putString (buf, i, s) = let
	    fun copy (i, j) = (put8(buf, i, ordof(s, j)); copy(i+1, j+1))
	    in
	      (copy (i, 0)) handle Ord => ()
	    end

      fun putBool (buf, i, false) = put8 (buf, i, 0)
	| putBool (buf, i, true) = put8 (buf, i, 1)

      fun putXId (buf, i, XID n) = put32 (buf, i, n)
      fun putXIdOption (buf, i, NONE) = put32 (buf, i, 0)
	| putXIdOption (buf, i, SOME(XID n)) =  put32 (buf, i, n)

      fun putAtom (buf, i, XAtom n) = put32 (buf, i, n)
      fun putAtomOption (buf, i, NONE) = put32 (buf, i, 0)
	| putAtomOption (buf, i, SOME(XAtom n)) = put32 (buf, i, n)

      fun putPixel (buf, i, PIXEL n) = put32(buf, i, n)
      fun putPlaneMask (buf, i, PLANEMASK n) = put32(buf, i, n)
      fun putEventMask (buf, i, XEVTMASK m) = put32(buf, i, m)
      fun putPtrEventMask (buf, i, XEVTMASK m) = put16(buf, i, m)

      fun putPt (buf, i, PT{x, y}) = (
	    putSigned16(buf, i, x); putSigned16(buf, i+2, y))
      fun putSize (buf, i, SIZE{wid, ht}) = (
	    put16(buf, i, wid); put16(buf, i+2, ht))
      fun putRect (buf, i, RECT{x, y, wid, ht}) = (
	    putSigned16(buf, i, x); putSigned16(buf, i+2, y);
	    put16(buf, i+4, wid); put16(buf, i+6, ht))
      fun putArc (buf, i, ARC{x, y, wid, ht, angle1, angle2}) = (
	    putSigned16(buf, i, x); putSigned16(buf, i+2, y);
	    put16(buf, i+4, wid); put16(buf, i+6, ht);
	    putSigned16(buf, i+8, angle1); putSigned16(buf, i+10, angle2))
      fun putWGeom (buf, i, WGEOM{pos, sz, border}) = (
	    putPt(buf, i, pos); putSize(buf, i+4, sz); put16(buf, i+8, border))

      local
	open System.Timer
	val t2_32 = TIME{sec=4294967, usec=296000}  (* 2^32 msec *)
	val t2_31 = TIME{sec=2147483, usec=648000}  (* 2^31 msec *)
	val t2_30 = TIME{sec=1073741, usec=824000}  (* 2^30 msec *)
	fun adjustTime t =
	      if earlier(t, t2_32) then t else adjustTime(sub_time(t, t2_32))
	fun highBits t = let
	      val t = adjustTime t
	      val (t, b31) = if (earlier(t, t2_31))
		      then (t, 0)
		      else (sub_time(t, t2_31), 32768)
	      in
		if (earlier(t, t2_30))
		  then (t, b31)
		  else (sub_time(t, t2_30), b31++16384)
	      end
      in
      fun putTS (buf, i, CurrentTime) = put32(buf, i, 0)
	| putTS (buf, i, TimeStamp(t as TIME{sec, usec})) = 
	    put32(buf, i, sec*1000 + usec quot 1000)
		handle Overflow => let
		  val (TIME{sec, usec}, bits31_30) = highBits t
		  val remainder = sec*1000 + usec quot 1000
		  in
		    put16(buf, i, bits31_30++(remainder >> 16));
		    put16(buf, i+2, remainder & 65535)
		  end
      end (* local *)

      fun putRGB (buf, i, RGB{red, green, blue}) = (
	    put16(buf, i, red), put16(buf, i+2, green), put16(buf, i+4, blue))

      fun putGrabMode(buf, i, SynchronousGrab) = put8(buf, i, 0)
	| putGrabMode(buf, i, AsynchronousGrab) = put8(buf, i, 1)

      fun putList (f, sz : int) (buf, base, list) = let
	    fun put (_, []) = ()
	      | put (i, x::r) = (f(buf, i, x); put(i+sz, r))
	    in
	      put (base, list)
	    end

      val putPts = putList (putPt, 4)
      val putRects = putList (putRect, 8)
      val putPixels = putList (putPixel, 4)

    (* build a value list and mask from a value option array *)
      fun mkValList (VALS arr) = let
	    fun f (~1, n, m, l) = (n, VALMASK m, l)
	      | f (i, n, m, l) = case Array.sub(arr, i)
		   of (SOME x) => f(i-1, n+1, (m ++ (1 << i)), x::l)
		    | NONE => f(i-1, n, m, l)
	    in
	      f ((Array.length arr)-1, 0, 0, [])
	    end

    (* Put value masks and lists *)
      local
	val putVals = putList (put32, 4)
      in
      fun putValList (buf, i, VALMASK m, vals) = (
	    put32(buf, i, m);
	    putVals(buf, i+4, vals))
      fun putValList16 (buf, i, VALMASK m, vals) = (
	    put16(buf, i, m);
	    putVals(buf, i+4, vals))
      end (* local *)

    (** X11 protocol request codes and sizes (from "Xproto.h") **)
      val reqCreateWindow		= {code = 1, size = 8}
      val reqChangeWindowAttributes	= {code = 2, size = 3}
      val reqGetWindowAttributes	= {code = 3, size = 2}
      val reqDestroyWindow		= {code = 4, size = 2}
      val reqDestroySubwindows		= {code = 5, size = 2}
      val reqChangeSaveSet		= {code = 6, size = 2}
      val reqReparentWindow		= {code = 7, size = 4}
      val reqMapWindow			= {code = 8, size = 2}
      val reqMapSubwindows		= {code = 9, size = 2}
      val reqUnmapWindow		= {code = 10, size = 2}
      val reqUnmapSubwindows		= {code = 11, size = 2}
      val reqConfigureWindow		= {code = 12, size = 3}
      val reqCirculateWindow		= {code = 13, size = 2}
      val reqGetGeometry		= {code = 14, size = 2}
      val reqQueryTree			= {code = 15, size = 2}
      val reqInternAtom			= {code = 16, size = 2}
      val reqGetAtomName		= {code = 17, size = 2}
      val reqChangeProperty		= {code = 18, size = 6}
      val reqDeleteProperty		= {code = 19, size = 3}
      val reqGetProperty		= {code = 20, size = 6}
      val reqListProperties		= {code = 21, size = 2}
      val reqSetSelectionOwner		= {code = 22, size = 4}
      val reqGetSelectionOwner		= {code = 23, size = 2}
      val reqConvertSelection		= {code = 24, size = 6}
      val reqSendEvent			= {code = 25, size = 11}
      val reqGrabPointer		= {code = 26, size = 6}
      val reqUngrabPointer		= {code = 27, size = 2}
      val reqGrabButton			= {code = 28, size = 6}
      val reqUngrabButton		= {code = 29, size = 3}
      val reqChangeActivePointerGrab	= {code = 30, size = 4}
      val reqGrabKeyboard		= {code = 31, size = 4}
      val reqUngrabKeyboard		= {code = 32, size = 2}
      val reqGrabKey			= {code = 33, size = 4}
      val reqUngrabKey			= {code = 34, size = 3}
      val reqAllowEvents		= {code = 35, size = 2}
      val reqGrabServer			= {code = 36, size = 1}
      val reqUngrabServer		= {code = 37, size = 1}
      val reqQueryPointer		= {code = 38, size = 2}
      val reqGetMotionEvents		= {code = 39, size = 4}
      val reqTranslateCoords		= {code = 40, size = 4}
      val reqWarpPointer		= {code = 41, size = 6}
      val reqSetInputFocus		= {code = 42, size = 3}
      val reqGetInputFocus		= {code = 43, size = 1}
      val reqQueryKeymap		= {code = 44, size = 1}
      val reqOpenFont			= {code = 45, size = 3}
      val reqCloseFont			= {code = 46, size = 2}
      val reqQueryFont			= {code = 47, size = 2}
      val reqQueryTextExtents		= {code = 48, size = 2}
      val reqListFonts			= {code = 49, size = 2}
      val reqListFontsWithInfo		= {code = 50, size = 2}
      val reqSetFontPath		= {code = 51, size = 2}
      val reqGetFontPath		= {code = 52, size = 1}
      val reqCreatePixmap		= {code = 53, size = 4}
      val reqFreePixmap			= {code = 54, size = 2}
      val reqCreateGC			= {code = 55, size = 4}
      val reqChangeGC			= {code = 56, size = 3}
      val reqCopyGC			= {code = 57, size = 4}
      val reqSetDashes			= {code = 58, size = 3}
      val reqSetClipRectangles		= {code = 59, size = 3}
      val reqFreeGC			= {code = 60, size = 2}
      val reqClearArea			= {code = 61, size = 4}
      val reqCopyArea			= {code = 62, size = 7}
      val reqCopyPlane			= {code = 63, size = 8}
      val reqPolyPoint			= {code = 64, size = 3}
      val reqPolyLine			= {code = 65, size = 3}
      val reqPolySegment		= {code = 66, size = 3}
      val reqPolyRectangle		= {code = 67, size = 3}
      val reqPolyArc			= {code = 68, size = 3}
      val reqFillPoly			= {code = 69, size = 4}
      val reqPolyFillRectangle		= {code = 70, size = 3}
      val reqPolyFillArc		= {code = 71, size = 3}
      val reqPutImage			= {code = 72, size = 6}
      val reqGetImage			= {code = 73, size = 5}
      val reqPolyText8			= {code = 74, size = 4}
      val reqPolyText16			= {code = 75, size = 4}
      val reqImageText8			= {code = 76, size = 4}
      val reqImageText16		= {code = 77, size = 4}
      val reqCreateColormap		= {code = 78, size = 4}
      val reqFreeColormap		= {code = 79, size = 2}
      val reqCopyColormapAndFree	= {code = 80, size = 3}
      val reqInstallColormap		= {code = 81, size = 2}
      val reqUninstallColormap		= {code = 82, size = 2}
      val reqListInstalledColormaps	= {code = 83, size = 2}
      val reqAllocColor			= {code = 84, size = 4}
      val reqAllocNamedColor		= {code = 85, size = 3}
      val reqAllocColorCells		= {code = 86, size = 3}
      val reqAllocColorPlanes		= {code = 87, size = 4}
      val reqFreeColors			= {code = 88, size = 3}
      val reqStoreColors		= {code = 89, size = 2}
      val reqStoreNamedColor		= {code = 90, size = 4}
      val reqQueryColors		= {code = 91, size = 2}
      val reqLookupColor		= {code = 92, size = 3}
      val reqCreateCursor		= {code = 93, size = 8}
      val reqCreateGlyphCursor		= {code = 94, size = 8}
      val reqFreeCursor			= {code = 95, size = 2}
      val reqRecolorCursor		= {code = 96, size = 5}
      val reqQueryBestSize		= {code = 97, size = 3}
      val reqQueryExtension		= {code = 98, size = 2}
      val reqListExtensions		= {code = 99, size = 1}
      val reqChangeKeyboardMapping	= {code = 100, size = 2}
      val reqGetKeyboardMapping		= {code = 101, size = 2}
      val reqChangeKeyboardControl	= {code = 102, size = 2}
      val reqGetKeyboardControl		= {code = 103, size = 1}
      val reqBell			= {code = 104, size = 1}
      val reqChangePointerControl	= {code = 105, size = 3}
      val reqGetPointerControl		= {code = 106, size = 1}
      val reqSetScreenSaver		= {code = 107, size = 3}
      val reqGetScreenSaver		= {code = 108, size = 1}
      val reqChangeHosts		= {code = 109, size = 2}
      val reqListHosts			= {code = 110, size = 1}
      val reqSetAccessControl		= {code = 111, size = 1}
      val reqSetCloseDownMode		= {code = 112, size = 1}
      val reqKillClient			= {code = 113, size = 2}
      val reqRotateProperties		= {code = 114, size = 3}
      val reqForceScreenSaver		= {code = 115, size = 1}
      val reqSetPointerMapping		= {code = 116, size = 1}
      val reqGetPointerMapping		= {code = 117, size = 1}
      val reqSetModifierMapping		= {code = 118, size = 1}
      val reqGetModifierMapping		= {code = 119, size = 1}
      val reqNoOperation		= {code = 127, size = 1}

    (* Allocate a buffer for a fixed-sized message and initialize the
      * code and size fields.  Return the buffer.
     *)
      fun mkReq {code, size} = let
	    val buf = mkReqBuf(size<<2)
	    in
	      put8 (buf, 0, code);		(* request opcode *)
	      put16 (buf, 2, size);		(* request size (in words) *)
	      buf
	    end

    (* Allocate a buffer for a fixed-sized message that contains an xid
     * in its first field, and initialize the code and size fields.  Return
     * the buffer.
     *)
      fun mkResourceReq (info, xid) = let
	    val buf = mkReq info
	    in
	      putXId (buf, 4, xid);  (* resource id *)
	      buf
	    end

    (* Allocate and initialize a buffer for a variable-sized request.
     * Return the new buffer.
     *)
      fun mkExtraReq ({code, size}, extra) = let
	    val sz = size+extra
	    val buf = mkReqBuf (sz << 2)
	    in
	      put8 (buf, 0, code);	(* request opcode *)
	      put16 (buf, 2, sz);	(* request size (in words) *)
	      buf
	    end

    (* Allocate and initialize a buffer for a variable-sized request.  Only allocate
     * space for the header.  Return the new buffer.
     *)
      fun mkVarReq ({code, size}, extra) = let
	    val sz = size+extra
	    val buf = mkReqBuf (size << 2)
	    in
	      put8 (buf, 0, code);		(* request opcode *)
	      put16 (buf, 2, size+extra);	(* request size (in words) *)
	      buf
	    end

    in

    fun encodeCreateWindow { win, parent, input_only, depth, visual, geom, vals } = let
	  val (nvals, mask, vals) = mkValList vals
	  val msg = mkExtraReq (reqCreateWindow, nvals)
	  in
	    put8(msg, 1, depth);
	    putXId(msg, 4, win);
	    putXId(msg, 8, parent);
	    putWGeom(msg, 12, geom);
	    put16(msg, 22, case input_only
	       of NONE => 0
		| (SOME false) => 1
		| (SOME true) => 2);
	    putXIdOption(msg, 24, visual);
	    putValList (msg, 28, mask, vals);
	    msg
	  end

    fun encodeChangeWindowAttributes { win, vals } = let
	  val (nvals, mask, vals) = mkValList vals
	  val msg = mkExtraReq (reqChangeWindowAttributes, nvals)
	  in
	    putXId(msg, 4, win);
	    putValList (msg, 8, mask, vals);
	    msg
	  end

    fun encodeGetWindowAttributes { win } = mkResourceReq (reqGetWindowAttributes, win)

    fun encodeDestroyWindow { win } = mkResourceReq (reqDestroyWindow, win)
    fun encodeDestroySubwindows { win } = mkResourceReq (reqDestroySubwindows, win)

    fun encodeChangeSaveSet { insert, win } = let
	  val msg = mkReq (reqChangeSaveSet)
	  in
	    putBool(msg, 1, insert);
	    putXId(msg, 4, win);
	    msg
	  end

    fun encodeReparentWindow { win, parent, pos } = let
	  val msg = mkResourceReq (reqReparentWindow, win)
	  in
	    putXId (msg, 8, parent);
	    putPt (msg, 12, pos);
	    msg
	  end

    fun encodeMapWindow { win } = mkResourceReq (reqMapWindow, win)
    fun encodeMapSubwindows { win } = mkResourceReq (reqMapSubwindows, win)
    fun encodeUnmapWindow { win } = mkResourceReq (reqUnmapWindow, win)
    fun encodeUnmapSubwindows { win } = mkResourceReq (reqUnmapSubwindows, win)

    fun encodeConfigureWindow { win, vals } = let
	  val (nvals, mask, vals) = mkValList vals
	  val msg = mkExtraReq (reqConfigureWindow, nvals)
	  in
	    putXId(msg, 4, win);
	    putValList16 (msg, 8, mask, vals);
	    msg
	  end

    fun encodeCirculateWindow { parent, win, place } = let
	  val msg = mkReq (reqCirculateWindow)
	  in
	    putXId(msg, 4, parent);
	    putXId(msg, 8, win);
	    put8(msg, 12, case place of PlaceOnTop => 0 | PlaceOnBottom => 1);
	    msg
	  end

    fun encodeGetGeometry { drawable } = mkResourceReq (reqGetGeometry, drawable)

    fun encodeQueryTree { win } = mkResourceReq (reqQueryTree, win)

    fun encodeInternAtom { name, only_if_exists } = let
	  val n = String.length name
	  val msg = mkExtraReq (reqInternAtom, (pad n) >> 2)
	  in
	    putBool (msg, 1, only_if_exists);
	    put16 (msg, 4, n);
	    putString (msg, 8, name);
	    msg
	  end

    fun encodeGetAtomName { atom = (XAtom id) } =
	  mkResourceReq (reqGetAtomName, XID id)

    fun encodeChangeProperty
	{ win, prop = PROP_VAL{name, typ, value = RAW_DATA{format, data}}, mode } = let
	  val nbytes = String.length data
	  val msg = mkExtraReq (reqChangeProperty, (pad nbytes) >> 2)
	  val (nitems, fmt) = case format
	       of Raw8 => (nbytes, 8)
		| Raw16 => (nbytes >> 1, 16)
		| Raw32 => (nbytes >> 2, 32)
	  in
	    put8(msg, 1,
	      case mode of ReplaceProp => 0 | PrependProp => 1 | AppendProp => 2);
	    putXId(msg, 4, win);
	    putAtom(msg, 8, name);
	    putAtom(msg, 12, typ);
	    put8(msg, 16, fmt);
	    put32(msg, 20, nitems);
	    putString(msg, 24, data);
	    msg
	  end

    fun encodeDeleteProperty { win, prop } = let
	  val msg = mkReq reqDeleteProperty
	  in
	    putXId(msg, 4, win);
	    putAtom(msg, 8, prop);
	    msg
	  end

    fun encodeGetProperty { win, prop, typ, offset, len, delete } = let
	  val msg = mkReq (reqGetProperty)
	  in
	    putBool(msg, 1, delete);
	    putXId(msg, 4, win);
	    putAtom(msg, 8, prop);
	    putAtomOption(msg, 12, typ);
	    put32(msg, 16, offset);
	    put32(msg, 20, len);
	    msg
	  end

    fun encodeListProperties { win } = mkResourceReq (reqListProperties, win)

    fun encodeSetSelectionOwner { win, selection, timestamp } = let
	  val msg = mkReq reqSetSelectionOwner
	  in
	    putXIdOption(msg, 4, win);
	    putAtom(msg, 8, selection);
	    putTS(msg, 12, timestamp);
	    msg
	  end

    fun encodeGetSelectionOwner { selection = (XAtom x) } =
	  mkResourceReq (reqGetSelectionOwner, XID x)

    fun encodeConvertSelection
	{ selection, target, property, requestor, timestamp } = let
	  val msg = mkReq reqConvertSelection
	  in
	    putXId(msg, 4, requestor);
	    putAtom(msg, 8, selection);
	    putAtom(msg, 12, target);
	    putAtomOption(msg, 16, property);
	    putTS(msg, 20, timestamp);
	    msg
	  end

  (* NOTE: this just encodes the header info; the encoding of the event
   * message is handled by the routines in XSendEvent.
   *)
    fun encodeSendEvent { dst, propagate, evt_mask } = let
	  val msg = mkReq (reqSendEvent)
	  in
	    putBool (msg, 1, propagate);
	    case dst
	     of SendEvtTo_PointerWindow => put32(msg, 4, 0)
	      | SendEvtTo_InputFocus => put32(msg, 4, 1)
	      | (SendEvtTo_Window wid) => putXId(msg, 4, wid)
	    (* end case *);
	    putEventMask (msg, 8, evt_mask);
	    msg
	  end

    fun encodeGrabPointer
	{ win, owner_evts, evt_mask, ptr_mode, kbd_mode, confine_to, cursor, time } = let
	  val msg = mkReq (reqGrabPointer)
	  in
	    putBool (msg, 1, owner_evts);
	    putXId (msg, 4, win);
	    putPtrEventMask (msg, 8, evt_mask);
	    putGrabMode (msg, 10, ptr_mode);
	    putGrabMode (msg, 11, kbd_mode);
	    putXIdOption (msg, 12, confine_to);
	    putXIdOption (msg, 16, cursor);
	    putTS (msg, 20, time);
	    msg
	  end

    fun encodeGrabKeyboard
	{ win, owner_evts, ptr_mode, kbd_mode, time } = let
	  val msg = mkReq (reqGrabKeyboard)
	  in
	    putBool(msg, 1, owner_evts);
	    putXId(msg, 4, win);
	    putTS(msg, 8, time);
	    putGrabMode(msg, 12, ptr_mode);
	    putGrabMode(msg, 13, kbd_mode);
	    msg
	  end

    local
      fun ungrab info { time } = let
	    val msg = mkReq (info)
	    in
	      putTS(msg, 4, time);
	      msg
	    end
    in
    val encodeUngrabPointer = ungrab reqUngrabPointer
    val encodeUngrabKeyboard = ungrab reqUngrabKeyboard
    end

    fun encodeChangeActivePointerGrab { evt_mask, cursor, time } = let
	  val msg = mkReq (reqChangeActivePointerGrab)
	  in
	    putXIdOption(msg, 4, cursor);
	    putTS(msg, 8, time);
	    putPtrEventMask(msg, 12, evt_mask);
	    msg
	  end

    local
      fun putModifiers(buf, i, mset) = let
	    val m = case (KeyBut.mkModState mset)
		 of AnyModKey => 0x8000
		  | (MKState m) => m
	    in
	      put16(buf, i, m)
	    end
      fun putButton(buf, i, SOME(MButton b)) = put8(buf, i, b)
	| putButton(buf, i, NONE) = put8(buf, i, 0)
      fun putKeyCode(buf, i, KEYCODE k) = put8(buf, i, k)
    in
    fun encodeGrabButton
	{ button, modifiers, win, owner_evts, evt_mask, ptr_mode, kbd_mode,
	  confine_to, cursor } = let
	  val msg = mkReq reqGrabButton
	  in
	    putBool(msg, 1, owner_evts);
	    putXId(msg, 4, win);
	    putPtrEventMask(msg, 8, evt_mask);
	    putGrabMode(msg, 10, ptr_mode);
	    putGrabMode(msg, 11, kbd_mode);
	    putXIdOption(msg, 12, confine_to);
	    putXIdOption(msg, 16, cursor);
	    putButton(msg, 18, button);
	    putModifiers(msg, 20, modifiers);
	    msg
	  end

    fun encodeGrabKey { key, modifiers, win, owner_evts, ptr_mode, kbd_mode } = let
	  val msg = mkReq reqGrabKey
	  in
	    putBool(msg, 1, owner_evts);
	    putXId(msg, 4, win);
	    putModifiers(msg, 8, modifiers);
	    putKeyCode(msg, 10, key);
	    putGrabMode(msg, 11, ptr_mode);
	    putGrabMode(msg, 12, kbd_mode);
	    msg
	  end

    fun encodeUngrabButton { button, modifiers, win } = let
	  val msg = mkReq (reqUngrabButton)
	  in
	    putButton(msg, 1, button);
	    putXId(msg, 4, win);
	    putModifiers(msg, 8, modifiers);
	    msg
	  end

    fun encodeUngrabKey { key, modifiers, win } = let
	  val msg = mkReq (reqUngrabKey)
	  in
	    putKeyCode(msg, 1, key);
	    putXId(msg, 4, win);
	    putModifiers(msg, 8, modifiers);
	    msg
	  end
    end (* local *)

    fun encodeAllowEvents { mode, time } = let
	  val msg = mkReq (reqAllowEvents)
	  in
	    put8(msg, 1, case mode
	       of AsyncPointer => 0 | SyncPointer => 1 | ReplayPointer => 2
		| AsyncKeyboard => 3 | SyncKeyboard => 4 | ReplayKeyboard => 5
		| AsyncBoth => 6 | SyncBoth => 7);
	    putTS(msg, 4, time);
	    msg
	  end

    fun encodeQueryPointer { win } = mkResourceReq (reqQueryPointer, win)

    fun encodeGetMotionEvents { win, start, stop } = let
	  val msg = mkReq (reqGetMotionEvents)
	  in
	    putXId(msg, 4, win);
	    putTS(msg, 8, start);
	    putTS(msg, 12, stop);
	    msg
	  end

    fun encodeTranslateCoords { src_win, dst_win, src_pt } = let
	  val msg = mkResourceReq (reqTranslateCoords, src_win)
	  in
	    putXId (msg, 8, dst_win);
	    putPt (msg, 12, src_pt);
	    msg
	  end

    fun encodeWarpPointer { src, dst, src_rect, dst_pt } = let
	  val msg = mkReq reqWarpPointer
	  in
	    putXIdOption(msg, 4, src);
	    putXIdOption(msg, 8, dst);
	    putRect(msg, 12, src_rect);
	    putPt(msg, 20, dst_pt);
	    msg
	  end

    fun encodeSetInputFocus { focus, revert_to, timestamp } = let
	  val msg = mkReq reqSetInputFocus
	  in
	    put8(msg, 1, (case revert_to
	       of RevertToNone => 0 | RevertToPointerRoot => 1 | RevertToParent =>  2));
	    putXId(msg, 4, (case focus
	       of InputFocus_None => (XID 0)
		| InputFocus_PointerRoot => (XID 1)
		| (InputFocus_Window w) => w));
	    putTS (msg, 8, timestamp);
	    msg
	  end

    fun encodeOpenFont { font, name } = let
	  val n = String.length name
	  val msg = mkExtraReq (reqOpenFont, (pad n) >> 2)
	  in
	    putXId (msg, 4, font);
	    put16 (msg, 8, n);
	    putString (msg, 12, name);
	    msg
	  end

    fun encodeCloseFont { font } = mkResourceReq (reqCloseFont, font)

    fun encodeQueryFont { font } = mkResourceReq (reqQueryFont, font)

    fun encodeQueryTextExtents { font, str } = let
	  val len = String.length str
	  val p = pad len
	  val msg = mkExtraReq(reqQueryTextExtents, p >> 2)
	  in
	    putBool(msg, 1, (p = 2));
	    putXId(msg, 4, font);
	    putString(msg, 8, str);
	    msg
	  end

    local
      fun encode info { pattern, max } = let
	    val len = String.length pattern
	    val msg = mkExtraReq (info, (pad len) >> 2)
	    in
	      put16(msg, 4, max);
	      put16(msg, 6, len);
	      putString(msg, 8, pattern);
	      msg
	    end
    in
    val encodeListFonts = encode reqListFonts
    val encodeListFontsWithInfo = encode reqListFontsWithInfo
    end (* local *)

    fun encodeSetFontPath { path } = let
	  fun f ([], n, l) = (n, implode(List.rev l))
	    | f (s::r, n, l) = f(r, n+1, s :: (chr(String.length s)) :: l)
	  val (nstrs, data) = f(path, 0, [])
	  val len = String.length data
	  val msg = mkExtraReq (reqSetFontPath, (pad len) >> 2)
	  in
	    put16(msg, 4, nstrs);
	    putString(msg, 8, data);
	    msg
	  end

    fun encodeCreatePixmap { pixmap, drawable, depth, size } = let
	  val msg = mkResourceReq (reqCreatePixmap, pixmap)
	  in
	    put8 (msg, 1, depth);
	    putXId (msg, 8, drawable);
	    putSize (msg, 12, size);
	    msg
	  end

    fun encodeFreePixmap { pixmap } = mkResourceReq (reqFreePixmap, pixmap)

    fun encodeCreateGC { gc, drawable, vals } = let
	  val (nvals, mask, vals) = mkValList vals
	  val msg = mkExtraReq (reqCreateGC, nvals)
	  in
	    putXId(msg, 4, gc);
	    putXId(msg, 8, drawable);
	    putValList (msg, 12, mask, vals);
	    msg
	  end

    fun encodeChangeGC { gc, vals } = let
	  val (nvals, mask, vals) = mkValList vals
	  val msg = mkExtraReq (reqChangeGC, nvals)
	  in
	    putXId(msg, 4, gc);
	    putValList (msg, 8, mask, vals);
	    msg
	  end

    fun encodeCopyGC { src, dst, mask = VALMASK m } = let
	  val msg = mkReq (reqCopyGC)
	  in
	    putXId(msg, 4, src);
	    putXId(msg, 8, dst);
	    put32(msg, 12, m);
	    msg
	  end

    fun encodeSetDashes { gc, dash_offset, dashes } = let
	  val n = List.length dashes
	  val msg = mkExtraReq (reqSetDashes, (pad n) >> 2)
	  in
	    putXId(msg, 4, gc);
	    put16(msg, 8, dash_offset);
	    put16(msg, 10, n);
	    putList (put8, 1) (msg, 12, dashes);
	    msg
	  end

    fun encodeSetClipRectangles { gc, clip_origin, ordering, rects } = let
	  val msg = mkExtraReq (reqSetClipRectangles, (List.length rects) << 1)
	  in
	    put8(msg, 1, case ordering
	       of UnsortedOrder => 0 | YSortedOrder => 1
		| YXSortedOrder => 2 | YXBandedOrder => 3);
	    putXId(msg, 4, gc);
	    putPt(msg, 8, clip_origin);
	    putRects(msg, 12, rects);
	    msg
	  end

    fun encodeFreeGC { gc } = mkResourceReq (reqFreeGC, gc)

    fun encodeClearArea { win, rect, exposures } = let
	  val msg = mkResourceReq (reqClearArea, win)
	  in
	    putBool (msg, 1, exposures);
	    putRect (msg, 8, rect);
	    msg
	  end

    fun encodeCopyArea { gc, src, dst, src_pt, size, dst_pt } = let
	  val msg = mkResourceReq (reqCopyArea, src)
	  in
	    putXId (msg, 8, dst);
	    putXId (msg, 12, gc);
	    putPt (msg, 16, src_pt);
	    putPt (msg, 20, dst_pt);
	    putSize (msg, 24, size);
	    msg
	  end

    fun encodeCopyPlane { gc, src, dst, src_pt, size, dst_pt, plane } = let
	  val msg = mkResourceReq (reqCopyPlane, src)
	  in
	    putXId (msg, 8, dst);
	    putXId (msg, 12, gc);
	    putPt (msg, 16, src_pt);
	    putPt (msg, 20, dst_pt);
	    putSize (msg, 24, size);
	    put32 (msg, 28, (1 << plane));
	    msg
	  end


    local
      fun encodePoly req_info { drawable, gc, relative, items } = let
	    val msg = mkExtraReq (req_info, List.length items)
	    in
	      putBool(msg, 1, relative);
	      putXId(msg, 4, drawable);
	      putXId(msg, 8, gc);
	      putPts (msg, 12, items);
	      msg
	    end
    in
    val encodePolyPoint = encodePoly reqPolyPoint
    val encodePolyLine = encodePoly reqPolyLine
    end


    local
      fun encode (info, putItems, sz) { drawable, gc, items } = let
	    val msg = mkExtraReq (info, sz*(List.length items))
	    in
	      putXId(msg, 4, drawable);
	      putXId(msg, 8, gc);
	      putItems (msg, 12, items);
	      msg
	    end
      val putSegs = putList
	      (fn (buf, i, LINE(p1, p2)) => (putPt(buf, i, p1); putPt(buf, i+4, p2)), 8)
      val putArcs = putList (putArc, 12)
    in
    val encodePolySegment = encode (reqPolySegment, putSegs, 2)
    val encodePolyRectangle = encode (reqPolyRectangle, putRects, 2)
    val encodePolyFillRectangle = encode (reqPolyFillRectangle, putRects, 2)
    val encodePolyArc = encode (reqPolyArc, putArcs, 3)
    val encodePolyFillArc = encode (reqPolyFillArc, putArcs, 3)
    end (* local *)

    fun encodeFillPoly { drawable, gc, shape, relative, pts } = let
	  val msg = mkExtraReq (reqFillPoly, List.length pts)
	  in
	    putXId(msg, 4, drawable);
	    putXId(msg, 8, gc);
	    put8(msg, 12, case shape
	       of ComplexShape => 0 | NonconvexShape => 1 | ConvexShape => 2);
	    putBool(msg, 13, relative);
	    putPts (msg, 16, pts);
	    msg
	  end

    local
      fun putImageFormat (buf, i, XYBitmap) = put8(buf, i, 0)
	| putImageFormat (buf, i, XYPixmap) = put8(buf, i, 1)
	| putImageFormat (buf, i, ZPixmap) = put8(buf, i, 2)
    in
    fun encodePutImage { drawable, gc, depth, size, dst, lpad, format, data } = let
	  val n = String.length data
	  val msg = mkExtraReq (reqPutImage, (pad n)>>2)
	  in
	    putImageFormat(msg, 1, format);
	    putXId(msg, 4, drawable);
	    putXId(msg, 8, gc);
	    putSize(msg, 12, size);
	    putPt(msg, 16, dst);
	    put8(msg, 20, lpad);
	    put8(msg, 21, depth);
	    putString(msg, 24, data);
	    msg
	  end
    fun encodeGetImage { drawable, rect, plane_mask, format } = let
	  val msg = mkResourceReq (reqGetImage, drawable)
	  in
	    putImageFormat(msg, 1, format);
	    putRect(msg, 8, rect);
	    putPlaneMask(msg, 16, plane_mask);
	    msg
	  end
    end (* local *)

    local
      fun textlen (nil, n) = n
	| textlen ((FontItem _)::r, n) = textlen(r, n+5)
	| textlen ((TextItem(_, s))::r, n) = textlen(r, n+2+(String.length s))
      fun encode (itemlen, req_info) { drawable, gc, pt, items } = let
	    fun put (msg, i, []) = ()
	      | put (msg, i, (FontItem fid) :: r) = (
		  put8(msg, i, 255);
		  putXId(msg, i+1, fid);
		  put (msg, i+5, r))
	      | put (msg, i, (TextItem(delta, s)) :: r) = let
		  val n = itemlen s
		  in
		    if (n > 254)
		      then MLXError.impossible "excessive string in PolyText"
		      else ();
		    put8(msg, i, n);
		    putSigned8(msg, i+1, delta);
		    putString(msg, i+2, s);
		    put (msg, i+2+(String.length s), r)
		  end
	    val l = textlen (items, 0)
	    val p = pad l
	    val msg = mkExtraReq (req_info, p >> 2)
	    in
	      if (p = l) then () else put8(msg, 16+l, 0);  (* Xlib does this *) 
	      putXId(msg, 4, drawable);
	      putXId(msg, 8, gc);
	      putPt(msg, 12, pt);
	      put(msg, 16, items);
	      msg
	    end
    in
    val encodePolyText8 = encode (String.length, reqPolyText8)
    val encodePolyText16 = encode (fn s => ((String.length s) >> 1), reqPolyText16)
    end (* local *)

    local
      fun encode (textlen, req_info) { drawable, gc, pt, str } = let
	    val len = String.length str
	    val msg = mkExtraReq (req_info, (pad len) >> 2)
	    in
	      put8(msg, 1, textlen str);
	      putXId(msg, 4, drawable);
	      putXId(msg, 8, gc);
	      putPt(msg, 12, pt);
	      putString(msg, 16, str);
	    msg
	    end
    in
    val encodeImageText8 = encode (String.length, reqImageText8)
    val encodeImageText16 = encode (fn s => ((String.length s) >> 1), reqImageText16)
    end (* local *)

    fun encodeCreateColormap { cmap, win, visual, all_writable } = let
	  val msg = mkReq reqCreateColormap
	  in
	    putBool(msg, 1, all_writable);
	    putXId(msg, 4, cmap);
	    putXId(msg, 8, win);
	    putXId(msg, 12, visual);
	    msg
	  end

    fun encodeFreeColormap { cmap } = mkResourceReq (reqFreeColormap, cmap)

    fun encodeCopyColormapAndFree { src, dst } = let
	  val msg = mkReq reqCopyColormapAndFree
	  in
	    putXId(msg, 4, dst);
	    putXId(msg, 8, src);
	    msg
	  end

    fun encodeInstallColormap { cmap } = mkResourceReq (reqInstallColormap, cmap)
    fun encodeUninstallColormap { cmap } = mkResourceReq (reqUninstallColormap, cmap)

    fun encodeListInstalledColormaps { win } =
	  mkResourceReq (reqListInstalledColormaps, win)

    fun encodeAllocColor { cmap, color } = let
	  val msg = mkReq (reqAllocColor)
	  in
	    putXId(msg, 4, cmap);
	    putRGB(msg, 8, color);
	    msg
	  end

    fun encodeAllocNamedColor { cmap, name } = let
	  val n = String.length name
	  val msg = mkExtraReq (reqAllocNamedColor, (pad n) >> 2)
	  in
	    putXId(msg, 4, cmap);
	    put16(msg, 8, n);
	    putString (msg, 12, name);
	    msg
	  end

(**************************************************************************************
    fun encodeAllocColorCells = let
	  val msg = mkReq (reqAllocColorCells)
	  in
	    raise XERROR "unimplemented" (*** FIX ***)
	  end
    fun encodeAllocColorPlanes = let
	  val msg = mkReq (reqAllocColorPlanes)
	  in
	    raise XERROR "unimplemented" (*** FIX ***)
	  end
**************************************************************************************)

    fun encodeFreeColors { cmap, plane_mask, pixels } = let
	  val msg = mkExtraReq (reqFreeColors, List.length pixels)
	  in
	    putXId(msg, 4, cmap);
	    putPlaneMask(msg, 8, plane_mask);
	    putPixels (msg, 12, pixels);
	    msg
	  end

    local
      fun putColorItem (buf, i, COLORITEM{pixel, red, green, blue}) = let
	    val rmask = case red of NONE => 0 | (SOME x) => (put16(buf, i+4, x); 1)
	    val gmask = case green of NONE => 0 | (SOME x) => (put16(buf, i+6, x); 2)
	    val bmask = case blue of NONE => 0 | (SOME x) => (put16(buf, i+8, x); 4)
	    in
	      putPixel(buf, i, pixel);
	      put8(buf, i+10, rmask ++ gmask ++ bmask)
	    end
      val putColorItemList = putList (putColorItem, 12)
    in
    fun encodeStoreColors { cmap, items } = let
	  val msg = mkExtraReq (reqStoreColors, 3*(List.length items))
	  in
	    putXId(msg, 4, cmap);
	    putColorItemList(msg, 8, items);
	    msg
	  end
    end (* local *)

    fun encodeStoreNamedColor
	{ cmap, name, pixel, do_red, do_green, do_blue } = let
	  val n = String.length name
	  val msg = mkExtraReq (reqStoreNamedColor, (pad n) >> 2)
	  val mask = (if do_red then 1 else 0)
		  ++ (if do_green then 2 else 0)
		  ++ (if do_blue then 4 else 0)
	  in
	    put8(msg, 1, mask);
	    putXId(msg, 4, cmap);
	    putPixel(msg, 8, pixel);
	    putString (msg, 12, name);
	    msg
	  end

    fun encodeQueryColors { cmap, pixels } = let
	  val msg = mkExtraReq (reqQueryColors, List.length pixels)
	  in
	    putXId(msg, 4, cmap);
	    putPixels (msg, 8, pixels);
	    msg
	  end

    fun encodeLookupColor { cmap, name } = let
	  val n = String.length name
	  val msg = mkExtraReq (reqLookupColor, (pad n) >> 2)
	  in
	    putXId(msg, 4, cmap);
	    put16(msg, 8, n);
	    putString(msg, 12, name);
	    msg
	  end

    fun encodeCreateCursor { cursor, src, mask, fore_rgb, back_rgb, hot_spot} = let
	  val msg = mkReq (reqCreateCursor)
	  in
	    putXId(msg, 4, cursor);
	    putXId(msg, 8, src);
	    putXIdOption(msg, 12, mask);
	    putRGB(msg, 16, fore_rgb);
	    putRGB(msg, 22, back_rgb);
	    putPt(msg, 24, hot_spot);
	    msg
	  end

    fun encodeCreateGlyphCursor
	{ cursor, src_font, mask_font, src_chr, mask_chr, fore_rgb, back_rgb } = let
	  val msg = mkReq (reqCreateGlyphCursor)
	  in
	    putXId(msg, 4, cursor);
	    putXId(msg, 8, src_font);
	    putXIdOption(msg, 12, mask_font);
	    put16(msg, 16, src_chr);
	    put16(msg, 18, mask_chr);
	    putRGB(msg, 20, fore_rgb);
	    putRGB(msg, 26, back_rgb);
	    msg
	  end

    fun encodeFreeCursor { cursor } = mkResourceReq (reqFreeCursor, cursor)

    fun encodeRecolorCursor { cursor, fore_color, back_color } = let
	  val msg = mkReq reqRecolorCursor
	  in
	    putXId(msg, 4, cursor);
	    putRGB(msg, 8, fore_color);
	    putRGB(msg, 14, back_color);
	    msg
	  end

    fun encodeQueryBestSize { class, drawable, size } = let
	  val msg = mkReq reqQueryBestSize
	  in
	    put8(msg, 1, case class
	       of CursorShape => 0 | TileShape => 1 | StippleShape => 2);
	    putXId(msg, 4, drawable);
	    putSize(msg, 8, size);
	    msg
	  end

    fun encodeQueryExtension name = let
	  val n = String.length name
	  val msg = mkExtraReq (reqQueryExtension, (pad n) >> 2)
	  in
	    put16(msg, 4, n);
	    putString(msg, 8, name);
	    msg
	  end

(**************************************************************************************
    fun encodeChangeKeyboardMapping = let
	  val msg = mkReq (reqChangeKeyboardMapping)
	  in
	    raise XERROR "unimplemented" (*** FIX ***)
	  end
**************************************************************************************)

    fun encodeGetKeyboardMapping {first=(KEYCODE k), count} = let
	  val msg = mkReq reqGetKeyboardMapping
	  in
	    put8(msg, 4, k);
	    put8(msg, 5, count);
	    msg
	  end

    fun encodeChangeKeyboardControl { vals } = let
	  val (nvals, mask, vals) = mkValList vals
	  val msg = mkExtraReq (reqChangeKeyboardControl, nvals)
	  in
	    putValList(msg, 4, mask, vals);
	    msg
	  end

    fun encodeBell { percent } = let
	  val msg = mkReq reqBell
	  in
	    putSigned8(msg, 1, percent);
	    msg
	  end

    fun encodeChangePointerControl { acceleration, threshold } = let
	  val msg = mkReq reqChangePointerControl
	  in
	    case acceleration
	     of NONE => putBool(msg, 10, false)
	      | (SOME{numerator, denominator}) => (
		  putBool(msg, 10, true);
		  putSigned16(msg, 4, numerator);
		  putSigned16(msg, 6, denominator));
	    case threshold
	     of NONE => putBool(msg, 11, false)
	      | (SOME threshold) => (
		  putBool(msg, 11, false);
		  putSigned16(msg, 8, threshold));
	    msg
	  end

    fun encodeSetScreenSaver
	{ timeout, interval, prefer_blanking, allow_exposures } = let
	  val msg = mkReq reqSetScreenSaver
	  fun put (i, NONE) = put8(msg, i, 2)
	    | put (i, SOME b) = putBool(msg, i, b)
	  in
	    putSigned16(msg, 4, timeout);
	    putSigned16(msg, 6, interval);
	    put(8, prefer_blanking);
	    put(9, allow_exposures);
	    msg
	  end

    fun encodeChangeHosts { host, remove } = let
	  val (family, addr) = case host
		 of (InternetHost s) => (0, s)
		  | (DECnetHost s) => (1, s)
		  | (ChaosHost s) => (2, s)
	  val len = String.length addr
	  val msg = mkExtraReq (reqChangeHosts, (pad len) >> 2)
	  in
	    putBool(msg, 1, remove);
	    put8(msg, 4, family);
	    put16(msg, 6, len);
	    putString(msg, 8, addr);
	    msg
	  end

    fun encodeSetAccessControl { enable } = let
	  val msg = mkReq (reqSetAccessControl)
	  in
	    putBool(msg, 1, enable);
	    msg
	  end

    fun encodeSetCloseDownMode { mode } = let
	  val msg = mkReq (reqSetCloseDownMode)
	  in
	    put8(msg, 1, case mode
	       of DestroyAll => 0 | RetainPermanent => 1 | RetainTemporary => 2);
	    msg
	  end

    fun encodeKillClient { resource } = let
	  val rid = case resource of NONE => (XID 0) | (SOME x) => x
	  in
	    mkResourceReq (reqKillClient, rid)
	  end

    fun encodeRotateProperties { win, delta, properties } = let
	  val n = List.length properties
	  val msg = mkExtraReq (reqRotateProperties, n)
	  in
	    putXId(msg, 4, win);
	    put16(msg, 8, n);
	    put16(msg, 10, n);
	    putList (putAtom, 4) (msg, 12, properties);
	    msg
	  end

    fun encodeForceScreenSaver { activate } = let
	  val msg = mkReq (reqForceScreenSaver)
	  in
	    putBool(msg, 1, activate);
	    msg
	  end

(**************************************************************************************
    fun encodeSetPointerMapping = let
	  val msg = mkReq (reqSetPointerMapping)
	  in
	    raise XERROR "unimplemented" (*** FIX ***)
	  end
    fun encodeGetPointerMapping = let
	  val msg = mkReq (reqGetPointerMapping)
	  in
	    raise XERROR "unimplemented" (*** FIX ***)
	  end
    fun encodeSetModifierMapping = let
	  val msg = mkExtraReq (reqSetModifierMapping, ?)
	  in
	    raise XERROR "unimplemented" (*** FIX ***)
	  end
**************************************************************************************)

  (* Fixed requests *)
    val requestNoOperation = mkReq reqNoOperation
    val requestGetInputFocus = mkReq reqGetInputFocus
    val requestQueryKeymap = mkReq reqQueryKeymap
    val requestGrabServer = mkReq reqGrabServer
    val requestUngrabServer = mkReq reqUngrabServer
    val requestGetFontPath = mkReq reqGetFontPath
    val requestListExtensions = mkReq reqListExtensions
    val requestGetKeyboardControl = mkReq reqGetKeyboardControl
    val requestGetPointerControl = mkReq reqGetPointerControl
    val requestGetScreenSaver = mkReq reqGetScreenSaver
    val requestListHosts = mkReq reqListHosts
    val requestGetModifierMapping = mkReq reqGetModifierMapping

    end (* local open ... *)

  end (* XRequests *)
