(* Copyright (c) 1992 by Carnegie Mellon University *)

structure ModuleNames :MODULE_NAMES = struct

exception ModuleNamesError0 and ModuleNamesError1 and ModuleNamesError2 and
          ModuleNamesError3

type moduleKind = int
type token = int

datatype moduleKind = structMod | sigMod | functorMod | fsigMod

type moduleName = moduleKind * token list

fun ordOf structMod = 0
  | ordOf sigMod = 1
  | ordOf functorMod = 2
  | ordOf fsigMod = 3

fun modKindString structMod = "s%"
  | modKindString sigMod = "s$"
  | modKindString functorMod = "f%"
  | modKindString fsigMod = "f$"

fun modKind (module_kind,_) = module_kind

val next'token :token ref = ref 1
val total'refs :int ref = ref 0

val token'table :(string,token) Hash.table = Hash.createDefault ([]:token list)
val name'table'size = ref 32
val name'table :string array ref = ref (Array.array (!name'table'size, ""))

fun copy'name'table (pos:int) =
  if pos < (!name'table'size) then Array.sub (!name'table, pos) else ""

fun enter'in'name'table (token, name) =
 (if token < (!name'table'size) then () else 
    let val new'size = (!name'table'size) * 2 in
      name'table := Array.tabulate (new'size, copy'name'table);
      name'table'size := new'size
    end;
  Array.update (!name'table, token, name))

fun tokenOf (name:string) :token =
  let val nameH = Hasher.hasher name in
    total'refs := (!total'refs) + 1;
    case Hash.lookup token'table nameH of
       NONE =>
         let val t = !next'token in
           Hash.enter token'table nameH t;
           enter'in'name'table (t, name);
           next'token := !next'token + 1;
           t
         end
     | (SOME t) => t
  end

fun nameOf (token:token) :string =
  if token >= (!next'token) then raise ModuleNamesError0
    else Array.sub (!name'table, token)

val symname = Pervasives.symbol_name

fun symkind (s:symbol) =
  case Pervasives.symbol_kind s of
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

fun createModuleName (symbolKind, path) =
  let val tokens = map (tokenOf o symname) path in
    (symbolKind, tokens)
  end

fun genericModuleName () = (structMod, [tokenOf "<GENERIC>"])

fun modulePath  (path:symbol list) =
  createModuleName (symkind (lastSymbol path), path)
fun structPath  (path:symbol list) = createModuleName (structMod, path)
fun sigPath     (path:symbol list) = createModuleName (sigMod, path)
fun functorPath (path:symbol list) = createModuleName (functorMod, path)
fun fsigPath    (path:symbol list) = createModuleName (fsigMod, path)

fun qualNameString (qualName:string list) :string =
  case qualName of
     [] => ""
   | [name] => name
   | (head::tail) => head^"."^(qualNameString tail)

fun modNameQualId (_, tokens) = qualNameString (map nameOf tokens)

fun modNameString (modname as (modKind, _)) =
  (modKindString modKind) ^ (modNameQualId modname)

fun headModule (kind, tokens) =
  case tokens of
     [] => raise ModuleNamesError3
   | (first::[]) => (kind, nameOf first)
   | (first::second::rest) => (structMod, nameOf first)

fun moduleKind (sym:symbol) :moduleKind option =
  case Pervasives.symbol_kind sym of
     "structure" => (SOME structMod)
   | "signature" => (SOME sigMod)
   | "functor" => (SOME functorMod)
   | "functor signature" => (SOME fsigMod)
   | _ => NONE

end
