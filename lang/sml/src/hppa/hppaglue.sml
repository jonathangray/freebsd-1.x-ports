(* Copyright 1992 Scott Draves *)

structure HppaMC : CODEGENERATOR = 
struct
    structure HppaC = Coder(structure M = HppaInstr and E = HppaMCEmit)
    structure MachGen = CPScomp(HppaCM(structure C = HppaC))
    fun generate lexp = (
	  MachGen.compile lexp;
	  HppaC.finish();
	  HppaMCode.getCodeString())
end

structure HppaAC : ASSEMBLER =
struct
    structure HppaC = Coder(structure M = HppaInstr and E = HppaAsEmit)
    structure AssemGen = CPScomp(HppaCM(structure C = HppaC))
    fun generate (lexp, stream) = (
	  HppaAsCode.outfile := stream;
	  AssemGen.compile lexp;
	  HppaC.finish())
end

structure IntHppa = IntShare(structure Machm = HppaMC
		  val fileExtension = ".hppa"
		  functor Debugger = BogusDebugger
		 )

structure IntHppaD = IntShare(structure Machm = HppaMC
		  val fileExtension = ".hppa"
		  functor Debugger = RealDebugger
		 )

structure CompHppa = Batch(structure M=HppaMC and A=HppaAC)
