(* keysym.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
 * and the Massachusetts Institute of Technology, Cambridge, Massachusetts.
 *
 * Symbolic names for the common keysyms in the X11 standard.  This is a situation
 * where SML doesn't really have the necessary features (e.g., symbolic constants),
 * so it is pretty ugly.
 *)

structure Keysym =
  struct

    local
      structure S : sig
	  datatype keysym = NoSymbol | KEYSYM of int
	end = XProtTypes
    in
    open S
    end

    val voidSymbol = KEYSYM 0xFFFFFF

    datatype char_set
      = CS_Latin1 | CS_Latin2 | CS_Latin3 | CS_Latin4
      | CS_Kana | CS_Arabic | CS_Cyrillic | CS_Greek
      | CS_Technical | CS_Special | CS_Publishing | CS_Apl
      | CS_Hebrew | CS_Keyboard

    fun charSetOf (KEYSYM ks) = (case Bits.andb(ks, 0xff00)
	 of 0 => CS_Latin1 | 1 => CS_Latin2
	  | 2 => CS_Latin3 | 3 => CS_Latin4
	  | 4 => CS_Kana | 5 => CS_Arabic
	  | 6 => CS_Cyrillic | 7 => CS_Greek
	  | 8 => CS_Technical | 9 => CS_Special
	  | 10 => CS_Publishing | 11 => CS_Apl
	  | 12 => CS_Hebrew | 255 => CS_Keyboard
	  | _ => MLXError.impossible "[Keysym.charSetOf: unknown character set]")

  end (* KeySym *)
