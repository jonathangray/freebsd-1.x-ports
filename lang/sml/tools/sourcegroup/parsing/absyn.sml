(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins
   School of Computer Science
   Carnegie-Mellon University
   Pittsburgh, PA 15213
   rollins@cs.cmu.edu *)

functor AbSynFun (structure Operator :OPERATOR) = struct

type operator = Operator.operator
datatype ast = AST of operator * (ast list)
exception AST_ERROR

fun ast (opr:operator) (subterms:ast list) = (AST (opr,subterms))
fun terminal (opr:operator) = (AST (opr,[]))
fun oper (AST (opr, subterms) :ast) = opr
fun subterms (AST (opr, subterms) :ast) = subterms
fun arity (AST (opr, subterms) :ast) = length subterms

fun subterm (AST (opr,args) :ast) (i :int) =
  let fun nth [] n = raise AST_ERROR
        | nth (head::tail) n = if n=1 then head else nth tail (n-1)
  in nth args i end

fun pr (strm:outstream) (indent:string) (AST (opr, subterms) :ast) :unit =
  (outputc strm indent;
   outputc strm (Operator.opstring opr);
   outputc strm "\n";
   map (pr strm (indent ^ "  ")) subterms;
   ())

fun printast (strm:outstream) (ast:ast) = pr strm "" ast

end
