(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins
   School of Computer Science
   Carnegie-Mellon University
   Pittsburgh, PA 15213
   rollins@cs.cmu.edu *)

signature CONN_OPERATORS = sig

datatype operator =
   ID_ of string | STRING_ of string | IDLIST_ of string list
 | IMPORT_ | EXPORT_ | SOURCE_ | CONN_ | NAMESPACE_

  val opstring :operator -> string
end
