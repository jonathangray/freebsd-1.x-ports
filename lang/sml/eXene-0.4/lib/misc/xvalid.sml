(* xvalid.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories. See COPYRIGHT file for details.
 *)

structure XValid =
  struct

    local
      val not8 = Bits.notb 0xFF
      val not16 = Bits.notb 0xFFFF
    in
      fun valid8 i = (Bits.andb(i, not8) = 0) 
      fun validSigned8 i = (i < 128) andalso (i >= ~128)
      fun valid16 i = (Bits.andb(i, not16) = 0) 
      fun validSigned16 i = (i < 32768) andalso (i >= ~32768)
    end (* local *)

  end (* XValid *)
