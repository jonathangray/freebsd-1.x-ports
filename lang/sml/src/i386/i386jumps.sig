(* i386jumps.sig
 * by Yngvi Guttesen (ysg@id.dth.dk) and Mark Leone (mleone@cs.cmu.edu)
 *
 * Copyright 1989 by	  Department of Computer Science, 
 *			  The Technical University of Denmak
 *			  DK-2800 Lyngby 
 *)

signature I386JUMPS = 
sig
   datatype JumpKind = JMP | Jcc of int | LEA of int | LABPTR of int
   datatype Size = Byte | Word | Long

   val sizeint	: int			-> Size
   val ebyte	: int			-> string
   val elong	: int			-> string
   val sizejump : JumpKind*int*int*int	-> int
   val emitjump : JumpKind*int*int*int	-> string
   val emitlong : int			-> string
end
