(* build1.sml *)

               (* building the system *)

structure Expression= Expression();

structure Parser= Parser(Expression);

structure Value = Value();

structure Evaluator= 
   Evaluator(structure Expression= Expression
             structure Value = Value);


structure Ty = Type(); 


structure TyCh= 
  TypeChecker(structure Ex = Expression
              structure Ty = Ty);

structure Interpreter=
  Interpreter(structure Ty= Ty
              structure Value = Value
              structure Parser = Parser
              structure TyCh = TyCh
              structure Evaluator = Evaluator);

open Interpreter;
