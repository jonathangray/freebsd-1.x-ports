(* abstract.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * make the eXene interface abstract.
 *)

structure EXene : sig
(* abstraction EXene : sig *)

    structure Geometry : GEOMETRY
    structure EXeneBase : EXENE_BASE
    structure Font : FONT
    structure Drawing : DRAWING
    structure ICCC : ICCC
    structure Interact : INTERACT
    structure EXeneWin : EXENE_WIN
    structure StdCursor : STD_CURSOR

    sharing CML = Interact.CML
    sharing Geometry = EXeneBase.G = Drawing.G = ICCC.G = Interact.G = EXeneWin.G
    sharing EXeneBase = Font.EXB = Drawing.EXB = ICCC.EXB = Interact.EXB
		= EXeneWin.EXB = StdCursor.EXB
    sharing ICCC = EXeneWin.ICCC
    sharing Interact = EXeneWin.Interact

    sharing type Font.font = EXeneBase.font
    sharing type EXeneWin.window = Drawing.window = EXeneBase.window
    sharing type Drawing.pixmap = EXeneBase.pixmap
    sharing type Drawing.tile = EXeneBase.tile
    sharing type Drawing.font = EXeneBase.font
    sharing type Drawing.color = EXeneBase.color
    sharing type ICCC.atom = EXeneBase.atom
    sharing type Interact.time = CML.time

  end = struct

    structure Geometry : GEOMETRY = Geometry

    structure EXeneBase : EXENE_BASE =
      struct
	structure G = Geometry

	open EXeneVersion

	exception BadAddr = XDisplay.BadAddr

	open XProtTypes DrawTypes FontBase Display Cursor ColorServer Pixmap Image Tile
	open HashWindow

        local fun copyRGB (RGB rgb) = CMS_RGB rgb in
	val white = copyRGB whiteRGB
	val black = copyRGB blackRGB
	end

      end

    structure Font : FONT =
      struct
	structure EXB = EXeneBase
	open XProtTypes FontBase FontServer Display
      end

    structure Drawing : DRAWING =
      struct
	structure G = Geometry
	structure EXB = EXeneBase
	open XProtTypes FontBase ColorServer DrawTypes PenRep Pen Draw
      end

    structure ICCC :ICCC =
      struct
	structure G = Geometry
	structure EXB = EXeneBase
	open XProtTypes XAtoms XProps
      end

    structure Interact : INTERACT =
      struct
	structure CML = CML
	structure G = Geometry
	structure EXB = EXeneBase
	open CML KeyBut XProtTypes WindowEnv KeysymTranslation
      end

    structure EXeneWin : EXENE_WIN =
      struct
	structure G = Geometry
	structure EXB = EXeneBase
	structure ICCC = ICCC
	structure Interact = Interact
	open XProtTypes XProps Window HashWindow
      end

    structure StdCursor : STD_CURSOR =
      struct
	structure EXB = EXeneBase
	open Cursor
      end

  end; (* abstraction EXene *)


(* hide internal eXene structures *)
structure XProtTypes = struct end;
structure KeyBut = struct end;
structure XEventTypes = struct end;
structure XErrors = struct end;
structure HashXId = struct end;
structure XPrint = struct end;
structure XCvtFuns = struct end;
structure XReply = struct end;
structure XRequest = struct end;
structure XIo = struct end;
structure XShutdown = struct end;
structure XDisplay = struct end;
structure XWin = struct end;
structure Keysym = struct end;
structure Keymap = struct end;
structure PenRep = struct end;
structure WinRegistry = struct end;
structure FontBase = struct end;
structure FontServer = struct end;
structure GCServer = struct end;
structure DrawMaster = struct end;
structure Display = struct end;
structure DrawTypes = struct end;
structure HashWindow = struct end;
structure Pen = struct end;
structure Draw = struct end;
structure ColorServer = struct end;
structure Pixmap = struct end;
structure Image = struct end;
structure Tile = struct end;
structure Cursor = struct end;
structure XAtoms = struct end;
structure XProps = struct end;
structure WindowEnv = struct end;
structure TopLevelWin = struct end
structure Window = struct end;
structure KeysymTranslation = struct end;

(* open EXene *)
structure EXeneBase = EXene.EXeneBase;
structure Font = EXene.Font;
structure Drawing = EXene.Drawing;
structure ICCC = EXene.ICCC;
structure Interact = EXene.Interact;
structure EXeneWin = EXene.EXeneWin;
structure StdCursor = EXene.StdCursor;

