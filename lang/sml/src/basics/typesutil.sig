(* Copyright 1989 by AT&T Bell Laboratories *)
(* typesutil.sig *)

(* types.sig *)

signature TYPESUTIL = sig

  structure Types : TYPES

 (* operations to build tyvars, VARtys *)
  val mkMETA : int -> Types.tvinfo
  val mkFLEX : ((Symbol.symbol * Types.ty) list) * int -> Types.tvinfo
  val mkUBOUND : Symbol.symbol * ErrorMsg.complainer -> Types.tvinfo
  val mkRefMETAty : int -> Types.ty
  val mkMETAty : unit -> Types.ty

  (* primitive operations on tycons *)
  val tycName : Types.tycon -> Symbol.symbol
  val tycPath : Types.tycon -> Symbol.symbol list
  val tyconArity : Types.tycon -> int
  val setTycPath : Types.tycon * Symbol.symbol list -> Types.tycon
  val eqTycon : Types.tycon * Types.tycon -> bool
  val mkCONty : Types.tycon * Types.ty list -> Types.ty

  val prune : Types.ty -> Types.ty

  val eqTyvar : Types.tyvar * Types.tyvar -> bool
  val bindTyvars : Types.tyvar list -> unit
  val bindTyvars1 : Types.tyvar list -> {weakness:int, eq:bool} list
    
  exception ReduceType
  val applyTyfun : Types.tyfun * Types.ty list -> Types.ty
  val reduceType : Types.ty -> Types.ty
  val headReduceType : Types.ty -> Types.ty
  val equalType  : Types.ty * Types.ty -> bool
  val equalTycon : Types.tycon * Types.tycon -> bool

  (* making a "generic" copy of a type *)
  val typeArgs : int -> Types.ty list
  val mkPolySign : int -> {weakness:int, eq:bool} list
  
  (* matching a scheme against a target type -- used declaring overloadings *)
  val matchScheme : Types.tyfun * Types.ty -> Types.ty

  (* get rid of INSTANTIATED indirections in a type *)
  val compressTy : Types.ty -> unit  

  type occ
  val Rand : occ -> occ
  val Abstr : occ -> occ
  val Rator : occ -> occ
  val LetDef: occ -> occ
  val Root : occ
  val lamdepth : occ -> int
  val abscount : occ -> int
  val generalize_point : occ -> int
  val toplevel : occ -> bool

  val applyPoly : Types.ty * occ -> Types.ty
  val instantiateType: Types.ty * occ -> Types.ty

  val compareTypes : {spec:Types.ty,actual:Types.ty} -> bool
  (* returns true if actual type > spec type *)

  (* see if a bound tyvar has occurred in some datatypes, e.g. 'a list. 
   * this is useful for representation analysis. 
   *)
  val getRecTyvarMap : int * Types.ty -> (int -> bool)

end  (* signature TYPESUTIL *)
