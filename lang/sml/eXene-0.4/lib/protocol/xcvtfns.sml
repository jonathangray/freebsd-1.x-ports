(* xcvtfns.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * Conversion routines between X types and their representation in messages.
 *)

structure XCvtFuns =
  struct
    local open XProtTypes in

  (* Process a configuration value list, producing an value_list. *)
    fun doValList n f lst = let
	  val arr = Array.array(n, NONE)
	  in
	    List.app (f arr) lst; VALS arr
	  end

    fun graphOpToWire OP_Clr		= 0
      | graphOpToWire OP_And		= 1
      | graphOpToWire OP_AndNot		= 2
      | graphOpToWire OP_Copy		= 3
      | graphOpToWire OP_AndInverted	= 4
      | graphOpToWire OP_Nop		= 5
      | graphOpToWire OP_Xor		= 6
      | graphOpToWire OP_Or		= 7
      | graphOpToWire OP_Nor		= 8
      | graphOpToWire OP_Equiv		= 9
      | graphOpToWire OP_Not		= 10
      | graphOpToWire OP_OrNot		= 11
      | graphOpToWire OP_CopyNot	= 12
      | graphOpToWire OP_OrInverted	= 13
      | graphOpToWire OP_Nand		= 14
      | graphOpToWire OP_Set		= 15

    fun gravityToWire ForgetGravity	= 0	(* bit gravity only *)
      | gravityToWire UnmapGravity	= 0	(* window gravity only *)
      | gravityToWire NorthWestGravity	= 1
      | gravityToWire NorthGravity	= 2
      | gravityToWire NorthEastGravity	= 3
      | gravityToWire WestGravity	= 4
      | gravityToWire CenterGravity	= 5
      | gravityToWire EastGravity	= 6
      | gravityToWire SouthWestGravity	= 7
      | gravityToWire SouthGravity	= 8
      | gravityToWire SouthEastGravity	= 9
      | gravityToWire StaticGravity	= 10

    fun boolToWire false = 0
      | boolToWire true = 1

    fun stackModeToWire Above	    = 0
      | stackModeToWire Below	    = 1
      | stackModeToWire TopIf	    = 2
      | stackModeToWire BottomIf    = 3
      | stackModeToWire Opposite    = 4

    end (* local *)
  end (* XCvtFuns *)
