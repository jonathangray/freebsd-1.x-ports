(* Copyright (c) 1992 by Carnegie Mellon University *)

structure Pretty :PRETTY = struct

structure MD = ModuleDecls
structure MN = ModuleDecls.MN

open ModuleDecls MN

val indent = ref ""
val delta = ". "
val size'delta = size delta
fun beginBlk () = (indent := (!indent) ^ delta)
fun endBlk () = (indent := substring(!indent,0,(size (!indent))-size'delta))

fun flagChr (flagOpt :bool option) :string =
  (case flagOpt of NONE=>" " | (SOME true)=>"T" | (SOME false)=>"F")

fun operator_common (text:string) = print (!indent^text^"\n")
fun operator (text:string) = (print " "; operator_common text)
fun operator' (flagOpt:bool option) (text:string) =
  (print (flagChr flagOpt); operator_common text)

fun pp_moduleName (ast:moduleName) = operator (modNameString ast)
fun pp_moduleName' flagOpt ast = operator' flagOpt (modNameString ast)

fun ppSequence ppFn lst =
 (beginBlk(); app ppFn lst; endBlk ())

fun ppList ppFn lst =
 (beginBlk(); app ppFn lst; endBlk ())

fun ppOption label ppFn opt =
  case opt of
     NONE => ()
   | (SOME x) =>
       (if label = "" then () else operator label;
        beginBlk(); ppFn x; endBlk())

fun printNameLine (width:int) (names:string list) =
  case names of
     [] => (print "\n"; [])
   | (head::tail) => 
       if width <= (size head) then (print "\n"; names)
         else (print (" "^head); printNameLine (width - ((size head)+1)) tail)

fun exhaust opr [] = ()
  | exhaust opr (lst as (_::_)) = exhaust opr (print (" "^(!indent)); opr lst)

fun ppNameList (operator:string)([]:moduleName list) = ()
  | ppNameList (operator:string)((args as (_::_)):moduleName list) =
 let val indent'size = (size (!indent)) + (size operator)
     val names = map modNameString args
     val _ = print (" "^(!indent)^operator)
     val rest = printNameLine (78 - indent'size) names
 in
   beginBlk ();
   exhaust (printNameLine (78 - (indent'size + size'delta))) rest;
   endBlk()
 end

fun pp_decl (ast:decl) =
 (case ast of
     (ModDecl arg) =>
       (operator "ModDecl"; beginBlk(); app pp_binder arg; endBlk ())
   | (LocalDecl (bindings, body)) =>
       (operator "LocalDecl";
        beginBlk();
        operator "bindings"; beginBlk(); app pp_decl bindings; endBlk ();
        operator "body"; beginBlk(); app pp_decl body; endBlk ();
        endBlk ())
   | (IncludeDecl name) => (operator ("Include "^(modNameString name)))
   | (SeqDecl arg) => (operator "SeqDecl"; ppSequence pp_decl arg)
   | (OpenDecl arg) => (ppNameList "Open" arg)
   | (DeclRef arg) => (ppNameList "Ref" arg)
 )
and pp_modExp (flag:bool option, ast:modExp) =
 (case ast of
     (VarModExp arg) => (pp_moduleName' flag arg)
   | (StructModExp arg) =>
       (operator' flag "StructModExp"; beginBlk(); pp_decl arg; endBlk())
   | (AppModExp (name,argList)) => 
       (operator' flag ("AppModExp "^(modNameString name));
        ppSequence (fn(modExp,flg)=>pp_modExp (SOME flg, modExp)) argList)
   | (LetModExp (decl,modExp)) =>
       (operator' flag "LetModExp";
        beginBlk();
        operator "bindings"; beginBlk(); pp_decl decl; endBlk();
        operator "body"; beginBlk(); pp_modExp (NONE, modExp); endBlk();
        endBlk())
   | (FctModExp {params, body}) =>
       (operator' flag "FctModExp";
        beginBlk();
        operator "params"; beginBlk(); app pp_fctArg params; endBlk();
        operator "body"; beginBlk(); pp_modExp (NONE, body); endBlk();
        endBlk())
 )
and pp_binder {name:MN.moduleName,def:modExp,constraint:modExp option} =
  (pp_moduleName name; 
   beginBlk();
   operator "def"; beginBlk(); pp_modExp (NONE, def); endBlk();
   operator "constraint"; beginBlk(); pp_modExpOpt constraint; endBlk();
   endBlk())
and pp_fctArg (nameOpt:moduleName option, modExp:modExp) =
 (case nameOpt of NONE => operator "Anonymous Functor Parameter"
      | (SOME modname) => pp_moduleName modname;
  beginBlk(); pp_modExp (NONE, modExp); endBlk())
and pp_modExpOpt (arg :modExp option) =
  (case arg of NONE => () | (SOME modExp) => pp_modExp (NONE, modExp))

fun prettyPrint filename =
  pp_decl (Traverse.traverse (Parse.parseSource filename))

fun show'off strm () =
  if end_of_stream strm then ()
    else (print (input_line strm); show'off strm ())

fun showDir directory =
  let fun scanner d f t =
        let val e = Pathname.extension f
            val pathname = Pathname.mergePathnames[d,f]
        in
          if not ((e="sml") orelse (e="sig")) then ()
            else (print ("--------------- "^ pathname ^" ---------------\n");
                  IO_Stream.withInStream (open_in pathname) show'off ();
                  print "\n---------------------------------------------\n";
                  prettyPrint pathname handle _ => ();
                  print "=============================================\n")
        end
  in
    DirFile.scan scanner [DirFile.ALPHA] directory
  end

end
