(* Copyright (c) 1992 by Carnegie Mellon University *)

functor NamespaceTableFun (structure Hash :HASH) :NAMESPACETABLE = struct

type nameToFileMap = (string, string*int) Hash.table

type t = (string, nameToFileMap) Hash.table

fun create (size:int) =
  Hash.create Hash.defaultEqual size ([]:(nameToFileMap list))

fun lookup (namespaces:t) spaceH nameH =
  case Hash.lookup namespaces spaceH of
     NONE => NONE
   | (SOME table) => Hash.lookup table nameH

fun enter (namespaces:t) spaceH nameH filenameH =
  let val nameToFileMap =
            case Hash.lookup namespaces spaceH of
               NONE =>
                 let val table = Hash.createDefault ([]:(string*int) list) in
                   Hash.enter namespaces spaceH table; table
                 end
             | (SOME table) => table
  in
    Hash.enter nameToFileMap nameH filenameH
  end

fun remove (namespaces:t) spaceH nameH =
  case Hash.lookup namespaces spaceH of
     NONE => ()
   | (SOME table) => Hash.remove table nameH

fun scan (namespaces:t) namespaceOper nameOper =
  let fun opr spaceH namespace =
        (namespaceOper spaceH; Hash.scan namespace nameOper)
  in
    Hash.scan namespaces opr
  end

fun eliminate (namespaces:t) predicate =
  let fun opr spaceH namespace =
        (Hash.eliminate namespace (predicate spaceH))
  in
    Hash.scan namespaces opr
  end

end