(* Copyright 1989 by AT&T Bell Laboratories *)
(* types.sml *)

structure Types : TYPES =
struct

  (* tycpos: type constructor or structure position in an INSTANCE structure.
     The integer list gives a path through the structure instantiation
     arrays and the offset gives the position in the type array of the
     last structure *)
     
  type tycpos = int list * int

  type polysign = {weakness: int, eq: bool} list
  type label = Symbol.symbol

  datatype absfbpos = PARAM of int list (* position in parameter *)
                    | SEQ of int (* position in body (direct) *)
                    | SEQind of int * int list
                      (* inside a structure defined by functor application *)

  datatype eqprop = YES | NO | IND | OBJ | DATA | UNDEF

  val infinity = 10000000

  datatype tvinfo
    = INSTANTIATED of ty
    | OPEN of {depth: int,
	       weakness: int,
	       eq: bool,
	       kind: tvkind}

  and tvkind
	(* metavariables:
		depth = infinity for metaargs
		depth < infinity for lambda bound
	*)
    = META
	(* user bound type variables *)
    | UBOUND of Symbol.symbol		(* user name of variable *)
	(* flex record variables *)
    | FLEX of (label * ty) list		(* field name, field type pairs *)

  and datacon  (* exceptions are a special case with rep=VARIABLE() *)
    = DATACON of
	{name  : Symbol.symbol,
	 const : bool,
	 typ   : ty,
	 rep   : Access.conrep,
	 sign  : Access.conrep list}

  and tyckind
    = PRIMtyc  	        (* primitive type constructors like int *)
    | ABStyc of tycon   (* abstract type constructors formed by abstype *)
    | DATAtyc of datacon list (* datatype constructors *)
    | FORMtyck		(* dummy type constructors used in functor body *)

  and tycon
    = GENtyc of {stamp : Stamps.stamp, 
		 arity : int, 
		 eq    : eqprop ref,
		 path  : Symbol.symbol list,
		 kind  : tyckind ref}
    | DEFtyc of {path  : Symbol.symbol list,
		 strict: bool list,
		 tyfun : tyfun}
    | RECORDtyc of label list
    | FORMtyc of {pos : int, spec : tycon, name: Symbol.symbol}
    | OPENFORMtyc of {pos: tycpos, spec : tycon, name : Symbol.symbol list}
    | RELtyc of  {pos : tycpos, name : Symbol.symbol list}
    | ABSFBtyc of absfbpos
    | ERRORtyc
    | FULLtyc (* added to reserve ERRORtyc for true errors *)

  and ty 
    = VARty of tyvar
    | CONty of tycon * ty list
	(* inferred bound type variables -- indexed *)
    | IBOUND of int
    | WILDCARDty
    | POLYty of {sign: {weakness:int, eq:bool} list, tyfun: tyfun, abs: int}
    | UNDEFty

  and tyfun
    = TYFUN of
	{arity : int,
	 body : ty}

  withtype tyvar = tvinfo ref

  fun mkTyvar(kind: tvinfo) : tyvar = ref kind

  val bogusCON = DATACON{name=Symbol.varSymbol "bogus",
			 const=true,typ=WILDCARDty,
			 rep=Access.CONSTANT 0,sign=[]}

  val bogusEXN = DATACON{name=Symbol.varSymbol "bogus",
			 const=true,typ=WILDCARDty,
			 rep=Access.CONSTANT 0,sign=[]}

end (* structure Types *)
