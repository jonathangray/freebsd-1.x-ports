(* Copyright (c) 1992 by Carnegie Mellon University *)

functor ScopesFun (structure ModuleDecls :MODULE_DECLS) = struct

structure MD = ModuleDecls
structure MN = MD.MN
structure Env = System.Env

exception ScopesInternalError

type modtable = (MN.moduleKind*string, MD.moduleInfo) Hash.table
datatype scopeKind = LocalScope | BindScope | PlainScope
datatype scope = Scope of {defTable:modtable, parent:parent, kind:scopeKind}
    and parent = Imports of modtable | Defs of scope

fun emptyTable() =
  Hash.create (fn(x,y:MN.moduleKind*string)=>x=y) 31 ([]:MD.moduleInfo list)
fun sourceScope () =
  (Scope{defTable=emptyTable(),parent=(Imports(emptyTable())),kind=PlainScope})

fun topScope (scope as Scope{kind,parent,defTable}:scope) =
 (case parent of (Defs sc) => topScope sc | (Imports _) => scope)
fun importTable (scope as Scope{kind,parent,defTable}:scope) =
 (case parent of (Defs sc) => importTable sc | (Imports tbl) => tbl)

fun sharedTable () = emptyTable ()

fun err (x:int) =
  (print "Scopes Error: ";print x;print "\n"; raise ScopesInternalError)

fun moduleHasher (arg as (modkind, name)) :(MN.moduleKind*string)*int =
 let val (_,h) = Hasher.hasher name in (arg, h + MN.ordOf modkind) end

val pervasivesTable =
  let val tbl = emptyTable()
      val symbols = Env.catalogEnv (Env.staticPart (!Env.pervasiveEnvRef))
      fun enterModule sym =
        case MN.moduleKind sym of NONE => ()
         | (SOME kind) =>
             (Hash.enter tbl (moduleHasher (kind,System.Symbol.name sym))
                         MD.emptyInfo)
  in
    app enterModule symbols; tbl
  end

val debug = ref false

fun plainScope parentScope =
  (if (!debug) then (print ">plainScope\n") else ();
   Scope {defTable=emptyTable(), parent=(Defs parentScope), kind=PlainScope})

fun bindScope parentScope = 
  (if (!debug) then (print ">bindScope\n") else ();
   Scope {defTable=emptyTable(), parent=(Defs parentScope), kind=BindScope})

fun localScope parentScope = 
  (if (!debug) then (print ">localScope\n") else ();
   Scope {defTable=emptyTable(), parent=(Defs parentScope), kind=LocalScope})

fun popScope (Scope {parent,...}) =
 (if (!debug) then (print "<popScope\n") else ();
  case parent of (Defs sc) => sc | (Imports _) => err 0)

fun lookup'name (scope as Scope{defTable,parent,...}:scope) nameH =
 case Hash.lookup defTable nameH of
    (SOME value) => (SOME value)
  | NONE =>
      (case parent of
          (Defs sc) => lookup'name sc nameH
        | (Imports importTable) => Hash.lookup importTable nameH)

fun lookupName (scope:scope) nameH :MD.moduleInfo option =
 case lookup'name scope nameH of
    (SOME value) => (SOME value)
  | NONE => Hash.lookup pervasivesTable nameH 

fun lookupShared (shared'table:modtable) (nameH:(MN.moduleKind*string)*int) =
  Hash.lookup shared'table nameH

fun enterShared (shared'table:modtable) (nameH:(MN.moduleKind*string)*int)
                (info :MD.moduleInfo) =
  Hash.enter shared'table nameH info

fun enterDefinition (scope as Scope{kind,parent,defTable}:scope, nameH, info) =
 (case kind of
     PlainScope => (Hash.enter defTable nameH info; ())
   | BindScope =>  (Hash.enter defTable nameH info; ())
   | LocalScope =>
       (case parent of
           (Imports _) => err 1
         | (Defs (Scope{kind,parent,...})) =>
             (case kind of
                 PlainScope => err 2 | LocalScope => err 3
               | BindScope =>
                   (case parent of
                       (Imports _) => err 4
                     | (Defs sc) => enterDefinition (sc, nameH, info)))))

fun enterRef (shared'table:modtable) (scope:scope)
             (moduleName:MN.moduleName) :MD.moduleInfo =
  let val nameH = moduleHasher (MN.headModule moduleName) in
    case lookupName scope nameH of
       NONE =>
         let val info = case lookupShared shared'table nameH of
                          NONE => MD.emptyInfo | (SOME value) => value
         in
           Hash.enter (importTable scope) nameH info; info
         end
     | (SOME value) => value
  end

fun enterDef (scope:scope) (moduleName:MN.moduleName, info:MD.moduleInfo) =
  enterDefinition (scope, moduleHasher (MN.headModule moduleName), info)

fun extract ((kind,modname),_) _ (a_str, a_sig, a_fun, a_fsig) =
  case kind of
     MN.structMod  => (modname::a_str, a_sig, a_fun, a_fsig)
   | MN.sigMod     => (a_str, modname::a_sig, a_fun, a_fsig)
   | MN.functorMod => (a_str, a_sig, modname::a_fun, a_fsig)
   | MN.fsigMod    => (a_str, a_sig, a_fun, modname::a_fsig)

fun importList (scope:scope) =
  Hash.fold (importTable scope) extract ([],[],[],[])

fun exportList (scope:scope) =
  let val (Scope{defTable=exportTable,...}) = topScope scope in
    Hash.fold exportTable extract ([],[],[],[])
  end

fun connections scope = (importList scope, exportList scope)

val sort = ListSort.sort (fn(x,y:string)=>(x<y))

fun printAll (out:outstream) (scope as Scope{kind,parent,defTable}:scope) =
  let val pr = outputc out
      val width = ref 0
      fun namePrinter name =
        let val len = size name in
          if (!width) + len < 70 then ()
            else (pr "\n    "; width := 4);
          pr name; pr " "; width := (!width) + len + 1
        end
      fun dump (_, []) = ()
        | dump (label, lst) =
            (pr ("\n  "^label^" "); width := 19; app namePrinter lst)
      val (i_str, i_sig, i_fun, i_fsig) = importList scope
      val (e_str, e_sig, e_fun, e_fsig) = exportList scope
  in
    dump ("import structure", sort i_str);
    dump ("import signature", sort i_sig);
    dump ("import functor", sort i_fun);
    dump ("import funsig", sort i_fsig);
    dump ("export structure", sort e_str);
    dump ("export signature", sort e_sig);
    dump ("export functor", sort e_fun);
    dump ("export funsig", sort e_fsig);
    pr ";\n\n"
  end

end
