(* Copyright (c) 1992 by Carnegie Mellon University *)

signature PERVASIVES = sig
  val makeSource  :string * int * IO.instream * bool *
                     System.Compile.PP.ppconsumer -> source
  val closeSource :source -> unit
  val parse :source * staticEnv -> System.Compile.Ast.dec * staticEnv
  val staticPart :environment -> staticEnv
  val symbol_name :symbol -> string
  val symbol_kind :symbol -> string 
end


structure Pervasives :PERVASIVES = struct

val cast = System.Unsafe.cast

val makeSource_ref = ref (!System.Hooks.makeSource_ref)
val makeSource  :string * int * IO.instream * bool *
                   System.Compile.PP.ppconsumer -> source =
      cast (fn x => !makeSource_ref x)

val closeSource_ref = ref (!System.Hooks.closeSource_ref)
val closeSource :source -> unit = cast (fn x => !closeSource_ref x)

val parse_ref = ref (!System.Hooks.parse_ref)
val parse :source * staticEnv -> System.Compile.Ast.dec * staticEnv =
      cast(fn x => !parse_ref x)

val staticPart_ref = ref (!System.Hooks.staticPart_ref)
val staticPart :environment -> staticEnv = cast(fn x => !staticPart_ref x)

val symbol_name_ref = ref (!System.Hooks.name_ref)
val symbol_name :symbol -> string = cast(fn x => !symbol_name_ref x)

val symbol_kind_ref = ref (!System.Hooks.kind_ref)
val symbol_kind :symbol -> string = cast(fn x => !symbol_kind_ref x)

end
