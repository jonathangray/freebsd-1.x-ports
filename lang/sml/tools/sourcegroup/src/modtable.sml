(* Copyright (c) 1992 by Carnegie Mellon University *)

structure ModTable = struct

type t = (string, unit) Hash.table

val tableSize = 37

fun indexOf space =
  case space of
     "signature" => 1
   | "structure" => 2
   | "functor" => 3
   | "functor signature" => 4
   | _ => 0

fun enter (modtable:t) (spaceIndex:int, (name,hsh):string*int) =
  Hash.enter modtable (name,hsh+spaceIndex) ()

fun enterModule (modtable:t) (symbol:symbol) =
  let val index = indexOf (System.Symbol.kind symbol) in
    if index = 0 then ()
      else enter modtable (index, Hasher.hasher (System.Symbol.name symbol))
  end
      
fun create (env:environment) :t =
  let val modtable = Hash.create (op =) tableSize ([]:unit list)
      val symbols = System.Env.catalogEnv (System.Env.staticPart env)
  in
    app (enterModule modtable) symbols; modtable
  end

fun lookup (modtable:t) (space:string, (name,hsh):string*int) :bool =
  let val index = indexOf space in
    if index = 0 then false
      else case Hash.lookup modtable (name,hsh+index) of
              NONE => false
            | (SOME _) => true
  end

val pervasives = create (!System.Env.pervasiveEnvRef)

end
