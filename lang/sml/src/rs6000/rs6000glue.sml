(** IBM RS6000 glue **)
structure RS6000Assembler : ASSEMBLER = 
  struct

    structure RS6000Coder = Coder (structure M=RS6000Depend 
				         and E=RS6000AssemblyEmitter)
    
    structure CMachineAssembler = RS6000CM (structure C=RS6000Coder)

    structure MLAssembler = CPScomp(CMachineAssembler)

    fun generate(lexp,stream) =	(RS6000AsmStream.asmStream := stream;
				 MLAssembler.compile lexp;
				 RS6000Coder.finish())
  end

structure RS6000CodeGen : CODEGENERATOR = 
  struct

    structure RS6000Coder = Coder (structure M=RS6000Depend
				         and E=RS6000MCodeEmitter)
    structure CMachineCoder = RS6000CM (structure C=RS6000Coder)

    structure RS6000CodeGenerator = CPScomp (CMachineCoder)
    fun generate lexp = (RS6000CodeGenerator.compile lexp;
			 RS6000Coder.finish();
			 KeepRS6000MCode.getCodeString())
  end


structure CompRS6000 = Batch (structure M=RS6000CodeGen
			            and A=RS6000Assembler)

structure IntRS6000  = IntShare (structure Machm   = RS6000CodeGen
			 	 functor Debugger  = BogusDebugger
			         val fileExtension = ".rs6000")

structure IntRS6000D = IntShare (structure Machm   = RS6000CodeGen
			 	 functor Debugger  = RealDebugger
				 val fileExtension = ".rs6000")



