(* Copyright (c) 1992 by Carnegie Mellon University *)

structure StringModuleNames = struct

exception ModuleNamesError0 and ModuleNamesError1 and ModuleNamesError2

type moduleKind = int
type token = string
datatype moduleName = M of moduleKind * token list

val structMod = 0
val sigMod = 1
val functorMod = 2
val fsigMod = 3

val modKindString :string vector = Vector.vector ["s%","s$","f%","f$"]

val total'refs :int ref = ref 0
val total'size :int ref = ref 0

val token'table = 0
val name'table = 0

fun tokenOf (name:string) :token =
  (total'refs := (!total'refs) + 1; total'size := (!total'size) + (size name);
   name)

fun nameOf (token:token) :string = token

datatype moduleName = M of moduleKind * token list

val symname = System.Symbol.name

fun symkind (s:symbol) =
  case System.Symbol.kind s of
     "structure" => structMod
   | "signature" => sigMod
   | "functor" => functorMod
   | "functor signature" => fsigMod
   | _ => raise ModuleNamesError1

fun lastSymbol lst =
  case lst of
     [] => raise ModuleNamesError2
   | [element] => element
   | (head::(tail as (_::_))) => lastSymbol tail

fun modulePath (path:symbol list) =
  (M (symkind (lastSymbol path), map (tokenOf o symname) path))

fun structPath (path:symbol list) =
  (M (structMod, map (tokenOf o symname) path))
fun sigPath (path:symbol list) =
  (M (sigMod, map (tokenOf o symname) path))
fun functorPath (path:symbol list) =
  (M (functorMod, map (tokenOf o symname) path))
fun fsigPath (path:symbol list) =
  (M (fsigMod, map (tokenOf o symname) path))

fun qualNameString (qualName:string list) :string =
  case qualName of
     [] => ""
   | [name] => name
   | (head::tail) => head^"."^(qualNameString tail)

fun modNameString (M (modKind, tokens)) =
  (Vector.sub (modKindString, modKind)) ^ (qualNameString (map nameOf tokens))

fun report () = (print ("strings = "^(makestring (!total'refs))^"\n");
	         print ("total'size = "^(makestring (!total'size))^"\n"))

end
