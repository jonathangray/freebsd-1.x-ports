(* build4.sml *)

               (* building the system *)

structure Expression= Expression();

structure Parser= Parser(Expression);

structure Value = Value();

structure Evaluator= 
   Evaluator(structure Expression= Expression
             structure Value = Value);


structure Ty = Type(); 

structure Unify = Unify(Ty);

structure TE = Environment();

structure TyCh= 
  TypeChecker(structure Ex = Expression
              structure Ty = Ty
              structure Unify= Unify
              structure TE = TE);
structure Interpreter=
  Interpreter(structure Ty= Ty
              structure Value = Value
              structure Parser = Parser
              structure TyCh = TyCh
              structure Evaluator = Evaluator);

open Interpreter;
