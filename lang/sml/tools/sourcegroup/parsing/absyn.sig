(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins
   School of Computer Science
   Carnegie-Mellon University
   Pittsburgh, PA 15213
   rollins@cs.cmu.edu *)

signature ABSYN =
  sig
    type operator
    datatype ast = AST of operator * (ast list)
    exception AST_ERROR
    val arity : ast -> int
    val ast : operator -> ast list -> ast
    val oper : ast -> operator
    val subterm : ast -> int -> ast
    val subterms : ast -> ast list
    val terminal : operator -> ast
    val printast : outstream -> ast -> unit
  end
