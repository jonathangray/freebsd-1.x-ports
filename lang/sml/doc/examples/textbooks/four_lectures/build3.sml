(* build3.sml *)

               (* building the system *)

structure Expression= Expression();

structure Parser= Parser(Expression);

structure Value = Value();

structure Evaluator= 
   Evaluator(structure Expression= Expression
             structure Value = Value);


structure AppTy = AppType(); 

structure AppUnify = Unify(AppTy);

structure AppTyCh= 
  TypeChecker(structure Ex = Expression
              structure Ty = AppTy
              structure Unify= AppUnify);

structure AppInter=
  Interpreter(structure Ty= AppTy
              structure Value = Value
              structure Parser = Parser
              structure TyCh = AppTyCh
              structure Evaluator = Evaluator);

val (appint,appeval,apptc)=(AppInter.interpret,AppInter.eval,AppInter.tc);

structure ImpTy = ImpType(); 

structure ImpUnify = Unify(ImpTy);

structure ImpTyCh= 
  TypeChecker(structure Ex = Expression
              structure Ty = ImpTy
              structure Unify= ImpUnify);

structure ImpInter=
  Interpreter(structure Ty= ImpTy
              structure Value = Value
              structure Parser = Parser
              structure TyCh = ImpTyCh
              structure Evaluator = Evaluator);

val (impint,impeval,imptc)=
    (ImpInter.interpret,ImpInter.eval,ImpInter.tc);


