(* Copyright 1992 by AT&T Bell Laboratories *)
(* build/inithooks.sml *)

functor InitHooks(structure Interact: INTERACT
		  structure Compile: COMPILEUNIT) : sig end =
struct

  open System.Hooks
  val cast = System.Unsafe.cast

  fun makeSource(srcName,lineNum,sourceStream,interactive,ppconsumer)
      : Source.inputSource =
      Source.newSource(srcName,lineNum,sourceStream,interactive,ppconsumer,
		       Index.openIndexFile srcName)

  val _ =

  (
  (* System.Symbol *)
     valSymbol_ref    := cast Symbol.varSymbol;
     tycSymbol_ref    := cast Symbol.tycSymbol;
     sigSymbol_ref    := cast Symbol.sigSymbol;
     strSymbol_ref    := cast Symbol.strSymbol;
     fctSymbol_ref    := cast Symbol.fctSymbol;
     fixSymbol_ref    := cast Symbol.fixSymbol;
     labSymbol_ref    := cast Symbol.labSymbol;
     tyvSymbol_ref    := cast Symbol.tyvSymbol;
     fsigSymbol_ref   := cast Symbol.fsigSymbol;
     name_ref         := cast Symbol.name;
     makestring_ref   := cast Symbol.symbolToString;
     kind_ref         := cast (Symbol.nameSpaceToString o Symbol.nameSpace);
     nameSpace_ref    := cast Symbol.nameSpace;

  (* System.Env *)
     emptyEnv_ref     := cast (fn () => Environment.emptyEnv);
     concatEnv_ref    := cast Environment.concatEnv;
     layerEnv_ref     := cast Environment.layerEnv;
     staticPart_ref   := cast Environment.staticPart;
     layerStatic_ref  := cast Environment.layerStatic;
     filterEnv_ref    := cast Environment.filterEnv;
     filterStaticEnv_ref    := cast Environment.filterStaticEnv;
     catalogEnv_ref   := cast Environment.catalogEnv;
     describe_ref     := cast Environment.describe;

  (* System.Compile *)
     makeSource_ref  := cast makeSource;
     closeSource_ref := cast Source.closeSource;
     changeLvars_ref := cast Compile.changeLvars;
     elaborate_ref   := cast Compile.elaborateUnit;
     parse_ref       := cast Compile.parseUnit;
     compile_ref     := cast Compile.compileUnit;
     compileAst_ref  := cast Compile.compileAst;
     execute_ref     := cast Compile.executeUnit;
     eval_stream_ref := cast Interact.eval_stream;
     use_file_ref    := cast Interact.use_file;
     use_stream_ref  := cast Interact.use_stream;

  (* System.Control *)
     allocProfReset_ref := cast AllocProf.reset;
     allocProfPrint_ref := cast AllocProf.print_profile_info;

  (* System.PrettyPrint *)
     mk_ppstream_ref := cast PrettyPrint.mk_ppstream;
     dest_ppstream_ref := cast PrettyPrint.dest_ppstream;
     begin_block_ref := cast PrettyPrint.begin_block;
     end_block_ref := cast PrettyPrint.end_block;
     add_break_ref := cast PrettyPrint.add_break;
     add_string_ref := cast PrettyPrint.add_string;
     add_newline_ref := cast PrettyPrint.add_newline;
     clear_ppstream_ref := cast PrettyPrint.clear_ppstream;
     flush_ppstream_ref := cast PrettyPrint.flush_ppstream;
     with_pp_ref := cast PrettyPrint.with_pp;
     install_pp_ref := cast PPTable.install_pp
    )

end (* functor InitHooks *)
