(* xatoms.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * These are the pre-defined X11 atoms (extracted from X11/Xatom.h)
 *)

structure XAtoms =
  struct
    local open XProtTypes in

      val atom_PRIMARY = XAtom 1
      val atom_SECONDARY = XAtom 2
      val atom_ARC = XAtom 3
      val atom_ATOM = XAtom 4
      val atom_BITMAP = XAtom 5
      val atom_CARDINAL = XAtom 6
      val atom_COLORMAP = XAtom 7
      val atom_CURSOR = XAtom 8
      val atom_CUT_BUFFER0 = XAtom 9
      val atom_CUT_BUFFER1 = XAtom 10
      val atom_CUT_BUFFER2 = XAtom 11
      val atom_CUT_BUFFER3 = XAtom 12
      val atom_CUT_BUFFER4 = XAtom 13
      val atom_CUT_BUFFER5 = XAtom 14
      val atom_CUT_BUFFER6 = XAtom 15
      val atom_CUT_BUFFER7 = XAtom 16
      val atom_DRAWABLE = XAtom 17
      val atom_FONT = XAtom 18
      val atom_INTEGER = XAtom 19
      val atom_PIXMAP = XAtom 20
      val atom_POINT = XAtom 21
      val atom_RECTANGLE = XAtom 22
      val atom_RESOURCE_MANAGER = XAtom 23
      val atom_RGB_COLOR_MAP = XAtom 24
      val atom_RGB_BEST_MAP = XAtom 25
      val atom_RGB_BLUE_MAP = XAtom 26
      val atom_RGB_DEFAULT_MAP = XAtom 27
      val atom_RGB_GRAY_MAP = XAtom 28
      val atom_RGB_GREEN_MAP = XAtom 29
      val atom_RGB_RED_MAP = XAtom 30
      val atom_STRING = XAtom 31
      val atom_VISUALID = XAtom 32
      val atom_WINDOW = XAtom 33
      val atom_WM_COMMAND = XAtom 34
      val atom_WM_HINTS = XAtom 35
      val atom_WM_CLIENT_MACHINE = XAtom 36
      val atom_WM_ICON_NAME = XAtom 37
      val atom_WM_ICON_SIZE = XAtom 38
      val atom_WM_NAME = XAtom 39
      val atom_WM_NORMAL_HINTS = XAtom 40
      val atom_WM_SIZE_HINTS = XAtom 41
      val atom_WM_ZOOM_HINTS = XAtom 42
      val atom_MIN_SPACE = XAtom 43
      val atom_NORM_SPACE = XAtom 44
      val atom_MAX_SPACE = XAtom 45
      val atom_END_SPACE = XAtom 46
      val atom_SUPERSCRIPT_X = XAtom 47
      val atom_SUPERSCRIPT_Y = XAtom 48
      val atom_SUBSCRIPT_X = XAtom 49
      val atom_SUBSCRIPT_Y = XAtom 50
      val atom_UNDERLINE_POSITION = XAtom 51
      val atom_UNDERLINE_THICKNESS = XAtom 52
      val atom_STRIKEOUT_ASCENT = XAtom 53
      val atom_STRIKEOUT_DESCENT = XAtom 54
      val atom_ITALIC_ANGLE = XAtom 55
      val atom_X_HEIGHT = XAtom 56
      val atom_QUAD_WIDTH = XAtom 57
      val atom_WEIGHT = XAtom 58
      val atom_POINT_SIZE = XAtom 59
      val atom_RESOLUTION = XAtom 60
      val atom_COPYRIGHT = XAtom 61
      val atom_NOTICE = XAtom 62
      val atom_FONT_NAME = XAtom 63
      val atom_FAMILY_NAME = XAtom 64
      val atom_FULL_NAME = XAtom 65
      val atom_CAP_HEIGHT = XAtom 66
      val atom_WM_CLASS = XAtom 67
      val atom_WM_TRANSIENT_FOR = XAtom 68

  (* Atom operations *)
    local
      open Display
      fun intern dpy arg = XReply.decodeInternAtomReply (
	    CML.sync (dpyRequestReply dpy (XRequest.encodeInternAtom arg)))
    in
    fun internAtom dpy name = intern dpy {name = name, only_if_exists = false}
    fun lookupAtom dpy name = (case (intern dpy {name = name, only_if_exists = true})
	 of (XAtom 0) => NONE
	  | xa => SOME xa)
    fun nameOfAtom dpy atom = XReply.decodeGetAtomNameReply (
	  CML.sync (dpyRequestReply dpy (XRequest.encodeGetAtomName{atom = atom})))
    end

    end
  end (* XAtoms *)
