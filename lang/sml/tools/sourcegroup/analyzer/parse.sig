(* Copyright (c) 1992 by Carnegie Mellon University *)

signature PARSE = sig
  val parseSource :string -> System.Compile.Ast.dec
end
