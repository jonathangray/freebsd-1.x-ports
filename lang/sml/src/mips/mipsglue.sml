functor MipsAssembler(structure Endian : ENDIAN) : ASSEMBLER = 
  struct
    structure MipsAssemblyEmitter = MipsAsCode()
    structure CoderInstr = MipsInstr(structure E=Endian)
    structure MipsCoder = Coder(structure M=CoderInstr 
				      and E=MipsAssemblyEmitter)
    structure CMachineAssembler = MipsCM(structure C=MipsCoder
					       and E=Endian)
    structure MLAssembler = CPScomp(CMachineAssembler)

    fun generate(lexp,stream) =	(MipsAsmStream.asmStream := stream;
				 MLAssembler.compile lexp;
				 MipsCoder.finish())
  end

functor MipsCodeGen(structure Endian : ENDIAN ) : CODEGENERATOR =  
  struct
    structure MipsMCodeEmitter = MipsMCode(structure E=Endian)
    structure CoderInstr = MipsInstr(structure E=Endian)
    structure MipsCoder = Coder(structure M=CoderInstr
				      and E=MipsMCodeEmitter)
    structure CMachineCoder = MipsCM(structure C=MipsCoder
					   and E=Endian)
    structure MipsCodeGenerator = CPScomp(CMachineCoder)
    fun generate lexp = (MipsCodeGenerator.compile lexp;
			 MipsCoder.finish();
			 KeepMipsMCode.getCodeString())
  end


structure MipsAssemblerBig = MipsAssembler(structure Endian=BigEndian)
structure MipsMCodeBig     = MipsCodeGen  (structure Endian=BigEndian)
structure CompMipsBig      = Batch        (structure M=MipsMCodeBig
			                         and A=MipsAssemblerBig)
structure IntMipsBig       = IntShare     (structure Machm=MipsMCodeBig
			 	                 val fileExtension=".mipseb"
			 	           functor Debugger=BogusDebugger)
structure IntMipsBigD      = IntShare     (structure Machm=MipsMCodeBig
				                 val fileExtension=".mipseb"
			 	           functor Debugger=RealDebugger)

structure MipsAssemblerLittle = MipsAssembler(structure Endian=LittleEndian)
structure MipsMCodeLittle     = MipsCodeGen  (structure Endian=LittleEndian)
structure CompMipsLittle      = Batch        (structure M=MipsMCodeLittle
			                            and A=MipsAssemblerLittle)
structure IntMipsLittle       = IntShare     (structure Machm=MipsMCodeLittle
				                    val fileExtension=".mipsel"
			 	              functor Debugger=BogusDebugger)
structure IntMipsLittleD      = IntShare     (structure Machm=MipsMCodeLittle
				                    val fileExtension=".mipsel"
			 	              functor Debugger=RealDebugger)




