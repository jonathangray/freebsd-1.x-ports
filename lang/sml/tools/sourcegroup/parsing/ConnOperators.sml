(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins
   School of Computer Science
   Carnegie-Mellon University
   Pittsburgh, PA 15213
   rollins@cs.cmu.edu *)

functor ConnOperatorsFun () = struct

datatype operator =
   ID_ of string | STRING_ of string | IDLIST_ of string list
 | IMPORT_ | EXPORT_ | SOURCE_ | CONN_ | NAMESPACE_

fun concat ([] :string list) = ""
  | concat (head::[]) = head
  | concat (head::next::tail) = head ^ " " ^ (concat (next::tail))

fun opstring (opr :operator) =
  case opr of
     IMPORT_ => "IMPORT_"
   | EXPORT_ => "EXPORT_"
   | SOURCE_ => "SOURCE_"
   | CONN_   => "CONN_"
   | NAMESPACE_ => "NAMESPACE_"
   | (ID_ value) => ("(ID_ " ^ value ^ ")")
   | (STRING_ value) => ("(STRING_ " ^ value ^ ")")
   | (IDLIST_ value) => ("(IDLIST_ " ^ (concat value) ^ ")")

end
