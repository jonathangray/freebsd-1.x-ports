(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins
   School of Computer Science
   Carnegie-Mellon University
   Pittsburgh, PA 15213
   rollins@cs.cmu.edu *)

functor NameRefTableFun (structure Hash :HASH) :NAMEREFTABLE = struct

type t = (string, string list) Hash.table

fun create (size:int) =
  Hash.create Hash.defaultEqual size ([]:(string list) list)

fun enter (table:t) spaceH value = Hash.enter table spaceH value

fun lookup (table:t) spaceH = Hash.lookup table spaceH

fun append (table:t) spaceH value =
  case Hash.lookup table spaceH of
     NONE => Hash.enter table spaceH [value]
  | (SOME name'list) => Hash.enter table spaceH (value::name'list)

fun getNameList table spaceH =
  case Hash.lookup table spaceH of
     NONE => []
   | (SOME name'list) => name'list

fun nestedFold table opr init =
  let fun process'name'list spaceH (name'list :string list) acc =
        fold (opr spaceH) name'list acc
  in
    Hash.fold table process'name'list init
  end

fun nestedScan table opr =
  let fun process'name'list spaceH (name'list :string list) =
        (map (opr spaceH) name'list; ())
  in
    Hash.scan table process'name'list
  end

fun scan table opr = Hash.scan table opr

fun fold table opr init = Hash.fold table opr init

end
