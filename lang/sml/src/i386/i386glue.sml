(* i386glue.sml
 * by Yngvi Guttesen (ysg@id.dth.dk) and Mark Leone (mleone@cs.cmu.edu)
 *
 * Copyright 1989 by	  Department of Computer Science, 
 *			  The Technical University of Denmak
 *			  DK-2800 Lyngby 
 *)

structure I386MC : CODEGENERATOR = 
struct

  structure MachineCoder = I386MCode(I386Jumps)
  structure CMachine	 = I386CM(MachineCoder)
  structure MachineGen	 = CPScomp(CMachine)

  fun generate lexp = (MachineGen.compile lexp; MachineCoder.finish())

end (* structure I386MC *)

structure I386AC : ASSEMBLER =
struct

  structure AssemCoder = I386AsCode()
  structure CMachine   = I386CM(AssemCoder)
  structure AssemGen   = CPScomp(CMachine)

  fun generate(lexp,stream) = (I386Assem.outfile := stream;
			       AssemGen.compile lexp)
end (* structure I386AC *)

structure IntI386 = IntShare(structure Machm = I386MC
			     val fileExtension = ".i386"
			     functor Debugger = BogusDebugger
			    );


structure IntI386D = IntShare(structure Machm = I386MC
			      val fileExtension = ".i386"
			      functor Debugger =  RealDebugger
			     );

structure CompI386 = Batch(structure M=I386MC and A=I386AC)
