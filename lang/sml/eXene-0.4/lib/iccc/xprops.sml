(* xprops.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * Support for the standard X properties and types as defined in
 * version 1.0 of the ICCCM.  These routines can be used to build
 * various property values (including the standard ones).
 *)

structure XProps =
  struct
    local
      open Geometry XProtTypes XAtoms DrawTypes

      val << = Bits.lshift
      val >> = Bits.rshift
      val ++ = Bits.orb
      val & = Bits.andb
      infix << >> ++ &

      fun intToString x = let
	    val b0 = chr(x & 255) and x = (x >> 8)
	    val b1 = chr(x & 255) and x = (x >> 8)
	    val b2 = chr(x & 255) and x = (x >> 8)
	    val b3 = chr(x & 255)
	    in
	      implode [b3,b2,b1,b0]
	    end

      fun arrToString arr = let
	    fun f (0, l) = implode l
	      | f (i, l) = let
		  val i = i-1
		  val x = Array.sub(arr, i)
		  val b0 = chr(x & 255) and x = (x >> 8)
		  val b1 = chr(x & 255) and x = (x >> 8)
		  val b2 = chr(x & 255) and x = (x >> 8)
		  val b3 = chr(x & 255)
		  in
		    f (i, b3::b2::b1::b0::l)
		  end
	    in
	      f (Array.length arr, [])
	    end

    (* map a list of hints to an int array, with position 0 containing
     * the field mask, and the other positions containing the field values.
     *)
      fun mkHintData (sz, putHint) lst = let
	    val data = Array.array(sz, 0)
	    val put1 = putHint (fn (i, x) => Array.update(data, i, x))
	    fun put ([], m) = m
	      | put (x::r, m) = put(r, put1(x, m))
	    val mask = put (lst, 0)
	    in
	      Array.update(data, 0, mask);
	      arrToString data
	    end
    in

  (* Hints about the window size *)
    datatype size_hints
      = HINT_USPosition
      | HINT_PPosition of point		(* obsolete in X11R4 *)
      | HINT_USSize
      | HINT_PSize of size		(* obsolete in X11R4 *)
      | HINT_PMinSize of size
      | HINT_PMaxSize of size
      | HINT_PResizeInc of size
      | HINT_PAspect of { min : (int * int), max : (int * int) }
      | HINT_PBaseSize of size
      | HINT_PWinGravity of gravity

  (* Window manager hints *)
    datatype wm_hints
      = HINT_Input of bool		(* does this application rely on the window *)
					(* manager to get keyboard input? *)
					(* Initial window state (choose one) *)
      | HINT_WithdrawnState		  (* for windows that are not mapped *)
      | HINT_NormalState		  (* most applications want to start this way *)
      | HINT_IconicState		  (* application wants to start as an icon *)
      | HINT_IconTile of tile		(* tile to be used as icon *)
      | HINT_IconPixmap of pixmap	(* pixmap to be used as icon *)
      | HINT_IconWindow of window	(* window to be used as icon *)
      | HINT_IconMask of pixmap		(* icon mask bitmap *)
      | HINT_IconPosition of point	(* initial position of icon *)
      | HINT_WindowGroup of window	(* the group leader *)

  (* Build a property value of type STRING *)
    fun makeStringProp (prop, data) = PROP_VAL {
	    name = prop,
	    typ = atom_STRING,
	    value = RAW_DATA{format = Raw8, data = data}
	  }

  (* Build a property value of type ATOM *)
    fun makeAtomProp (prop, XAtom v) = PROP_VAL {
	    name = prop,
	    typ = atom_ATOM,
	    value = RAW_DATA{format = Raw32, data = intToString v}
	  }

    local
      val sizeHintsData = let
	    fun putHint upd = let
		  fun putSz (i, SIZE{wid, ht}) = (
			upd(i, wid); upd(i+1, ht))
		  fun put1 (HINT_USPosition, m) = (m ++ 1)
		    | put1 (HINT_PPosition(PT{x, y}), m) = (
			upd(1, x); upd(2, y); m ++ 2)
		    | put1 (HINT_USSize, m) = (m ++ 4)
		    | put1 (HINT_PSize sz, m) = (putSz(3, sz); m ++ 8)
		    | put1 (HINT_PMinSize sz, m) = (putSz(5, sz); m ++ 16)
		    | put1 (HINT_PMaxSize sz, m) = (putSz(7, sz); m ++ 32)
		    | put1 (HINT_PResizeInc sz, m) = (putSz(9, sz); m ++ 64)
		    | put1 (HINT_PAspect{min=(x1, y1), max=(x2, y2)}, m) = (
			upd(11, x1); upd(12, y1); upd(13, x2); upd(14, y2);
			m ++ 128)
	 	    | put1 (HINT_PBaseSize sz, m) = (putSz(15, sz); m ++ 256)
	 	    | put1 (HINT_PWinGravity g, m) = (
			upd(17, XCvtFuns.gravityToWire g); m ++ 512)
		  in
		    put1
		  end
	    in
	      mkHintData (18, putHint)
	    end
    in
    fun makeWMSizeHints (prop, lst) = PROP_VAL {
	    name = prop,
	    typ = atom_WM_SIZE_HINTS,
	    value = RAW_DATA{format = Raw32, data = sizeHintsData lst}
	  }
    end (* local *)

    local
      val wmHintsData = let
	    fun putHint upd (hint, m) = (case hint
		 of (HINT_Input true) => (upd(1, 1); m ++ 1)
		  | HINT_WithdrawnState => (upd(2, 0); m ++ 2)
		  | HINT_NormalState => (upd(2, 1); m ++ 2)
		  | HINT_IconicState => (upd(2, 3); m ++ 2)
		  | (HINT_IconTile(TILE(PM{id=XID pix, ...}))) => (upd(3, pix); m ++ 4)
		  | (HINT_IconPixmap(PM{id=XID pix, ...})) => (upd(3, pix); m ++ 4)
		  | (HINT_IconWindow(WIN{id=XID win, ...})) => (upd(4, win); m ++ 8)
		  | (HINT_IconPosition(PT{x, y})) => (
			upd(5, x); upd(6, y); m ++ 16)
		  | (HINT_IconMask(PM{id=XID pix, ...})) => (upd(7, pix); m ++ 32)
		  | (HINT_WindowGroup(WIN{id=XID win, ...})) => (
			upd(8, win); m ++ 64)
		  | _ => raise (MLXError.XERROR "Bad WM Hint"))
	    in
	      mkHintData (9, putHint)
	    end
    in
    fun makeWMHints lst = PROP_VAL {
	    name = atom_WM_HINTS,
	    typ = atom_WM_HINTS,
	    value = RAW_DATA{format = Raw32, data = wmHintsData lst}
	  }
    end (* local *)

  (* Build a command-line argument property *)
    fun makeCommandHints args = PROP_VAL {
	    name = atom_WM_COMMAND,
	    typ = atom_STRING,
	    value = RAW_DATA{
		format = Raw8,
		data = implode (map (fn s => s^"\000") args)
	      }
	  }

    fun makeTransientHint (WIN{id=XID win, ...}) = PROP_VAL {
	    name = atom_WM_TRANSIENT_FOR,
	    typ = atom_WINDOW,
	    value = RAW_DATA{format = Raw32, data = intToString win}
	  }
    end (* local *)
  end (* XProps *)
