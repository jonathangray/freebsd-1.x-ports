(* Copyright 1989 by AT&T Bell Laboratories *)
structure VaxMC : CODEGENERATOR = 
struct
  structure CM = VaxCM(VaxMCode.Coder)
  structure G = CPScomp(CM)
  fun generate lexp_err = (G.compile lexp_err; VaxMCode.finish())
end

structure VaxAC : ASSEMBLER =
struct
  structure CM = VaxCM(VaxAsCode)
  structure AssemGen = CPScomp(CM)
  fun generate(lexp_err,stream) = (VaxAssem.outfile := stream;
			       AssemGen.compile lexp_err)
end

structure IntVax = IntShare(structure Machm = VaxMC
			    val fileExtension = ".vax"
			    functor Debugger = BogusDebugger
			   )
structure IntVaxD = IntShare(structure Machm = VaxMC
			    val fileExtension = ".vax"
			    functor Debugger =  RealDebugger
			   )
structure CompVax = Batch(structure M=VaxMC and A=VaxAC)
