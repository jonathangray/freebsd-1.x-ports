(* Copyright 1989 by AT&T Bell Laboratories *)
(* basictypes.sig *)
 
signature BASICTYPES =
sig

  val arrowTycon : Types.tycon 
  val --> : Types.ty * Types.ty -> Types.ty
  val isArrowType : Types.ty -> bool
  val domain : Types.ty -> Types.ty
  val range : Types.ty -> Types.ty

  val intTycon : Types.tycon 
  val intTy : Types.ty

  val realTycon  : Types.tycon 
  val realTy : Types.ty

  val stringTycon  : Types.tycon 
  val stringTy : Types.ty

  val exnTycon : Types.tycon 
  val exnTy : Types.ty

  val contTycon : Types.tycon 

  val arrayTycon : Types.tycon 

  val vectorTycon : Types.tycon

  val unitTycon : Types.tycon 
  val unitTy : Types.ty

  val recordTy : (Types.label * Types.ty) list -> Types.ty
  val tupleTy : Types.ty list -> Types.ty

  val boolTycon : Types.tycon 
  val boolTy : Types.ty
  val boolsign : Access.conrep list
  val falseDcon : Types.datacon
  val trueDcon : Types.datacon

  val optionTycon : Types.tycon 
  val NONEDcon : Types.datacon
  val SOMEDcon : Types.datacon

  val refTycon : Types.tycon 
  val refPatType : Types.ty
  val refDcon : Types.datacon

  val listTycon : Types.tycon 
  val nilDcon : Types.datacon
  val consDcon : Types.datacon

  val symbolTycon : Types.tycon
  val environmentTycon : Types.tycon
  val staticEnvTycon : Types.tycon

  val sourceTycon : Types.tycon
  val codeUnitTycon : Types.tycon

  val fragTycon : Types.tycon
  val ANTIQUOTEDcon : Types.datacon
  val QUOTEDcon : Types.datacon

end (* signature BASICTYPES *)
