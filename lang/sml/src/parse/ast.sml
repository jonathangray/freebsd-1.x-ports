(* Copyright 1992 by AT&T Bell Laboratories *)
(* Abstract syntax of bare ML *)

(****************************************************************************
 *            PLEASE PROPAGATE ANY MODIFICATIONS TO THIS FILE               *
 *                    INTO PERV.SML AND SYSTEM.SIG                          *
 ****************************************************************************)

structure Ast = struct

local
  open Symbol Fixity
in

(* to mark positions in files *)
type linenum = int
type range = linenum * linenum

(* symbolic path (Modules.spath) *)
type path = symbol list

(* EXPRESSIONS *)

datatype exp
  = VarExp of path		(* variable *)
  | FnExp of rule list		(* abstraction *)
  | AppExp of {function:exp,argument:exp}
				(* application *)
  | CaseExp of{expr:exp,rules:rule list}
				(* case expression *)
  | LetExp of {dec:dec,expr:exp} (* let expression *)
  | SeqExp of exp list		(* sequence of expressions *)
  | IntExp of int		(* integer *)
  | RealExp of string		(* floating point coded by its string *)
  | StringExp of string		(* string *)
  | RecordExp of (symbol * exp) list	(* record *)
  | TupleExp of exp list	(* tuple (derived form) *)
  | SelectorExp of symbol	(* selector of a record field *)
  | ConstraintExp of {expr:exp,constraint:ty}
				(* type constraint *)
  | HandleExp of {expr:exp, rules:rule list}
				(* exception handler *)
  | RaiseExp of exp		(* raise an exception *)
  | IfExp of {test:exp, thenCase:exp, elseCase:exp}
				(* if expression (derived form) *)
  | AndalsoExp of exp * exp	(* andalso (derived form) *)
  | OrelseExp of exp * exp	(* orelse (derived form) *)
  | WhileExp of {test:exp,expr:exp}
				(* while (derived form) *)
  | MarkExp of exp * linenum * linenum	(* mark an expression *)
  | VectorExp of exp list       (* vector *)

(* RULE for case functions and exception handler *)
and rule = Rule of {pat:pat,exp:exp}

(* PATTERN *)
and pat = WildPat				(* empty pattern *)
	| VarPat of path			(* variable pattern *)
	| IntPat of int				(* integer *)
	| RealPat of string			(* floating point number *)
	| StringPat of string			(* string *)
	| RecordPat of {def:(symbol * pat) list, flexibility:bool}
						(* record *)
	| TuplePat of pat list			(* tuple *)
	| AppPat of {constr:path,argument:pat}	(* application *)
	| ConstraintPat of {pattern:pat,constraint:ty}
						(* constraint *)
	| LayeredPat of {varPat:pat,expPat:pat}	(* as expressions *)
	| MarkPat of pat * linenum * linenum	(* mark a pattern *)
        | VectorPat of pat list                 (* vector pattern *)

(* STRUCTURE EXPRESSION *)
and strexp = VarStr of path			(* variable structure *)
	   | StructStr of dec			(* defined structure *)
	   | AppStr of path * (strexp * bool) list
						(* application *)
	   | LetStr of dec * strexp		(* let in structure *)
	   | MarkStr of strexp * linenum * linenum (* mark *)

(* FUNCTOR EXPRESSION *)
and fctexp = VarFct of path * fsigexp option	(* functor variable *)
	   | FctFct of {			(* definition of a functor *)
		params	   : (symbol option * sigexp) list,
		body	   : strexp,
		constraint : sigexp option}
	   | LetFct of dec * fctexp
	   | AppFct of path * (strexp * bool) list * fsigexp option
						(* application *)
	   | MarkFct of fctexp * linenum * linenum (* mark *)

(* SIGNATURE EXPRESSION *)
and sigexp = VarSig of symbol			(* signature variable *)
	   | SigSig of spec list		(* defined signature *)
	   | MarkSig of sigexp * linenum * linenum	(* mark *)

(* FUNCTOR SIGNATURE EXPRESSION *)
and fsigexp = VarFsig of symbol			(* funsig variable *)
	    | FsigFsig of {param: (symbol option * sigexp) list, def:sigexp}
						(* defined funsig *)
	    | MarkFsig of fsigexp * linenum * linenum	(* mark a funsig *)

(* SPECIFICATION FOR SIGNATURE DEFINITIONS *)
and spec = StrSpec of (symbol * sigexp) list			(* structure *)
	 | TycSpec of ((symbol * tyvar list) list * bool)	(* type *)
	 | FctSpec of (symbol * fsigexp) list			(* functor *)
	 | ValSpec of (symbol * ty) list			(* value *)
	 | DataSpec of db list					(* datatype *)
	 | ExceSpec of (symbol * ty option) list		(* exception *)
	 | FixSpec of  {fixity: fixity, ops: symbol list} 	(* fixity *)
	 | ShareSpec of path list			(* structure sharing *)
	 | ShatycSpec of path list			(* type sharing *)
	 | LocalSpec of spec list * spec list		(* local specif *)
	 | IncludeSpec of symbol			(* include specif *)
	 | OpenSpec of path list			(* open structures *)
	 | MarkSpec of spec * linenum * linenum		(* mark a spec *)

(* DECLARATIONS (let and structure) *)
and dec	= ValDec of vb list				(* values *)
	| ValrecDec of rvb list				(* recursive values *)
	| FunDec of fb list				(* recurs functions *)
	| TypeDec of tb list				(* type dec *)
	| DatatypeDec of {datatycs: db list, withtycs: tb list}
							(* datatype dec *)
	| AbstypeDec of {abstycs: db list, withtycs: tb list, body: dec}
							(* abstract type *)
	| ExceptionDec of eb list			(* exception *)
	| StrDec of strb list				(* structure *)
	| AbsDec of strb list				(* abstract struct *)
	| FctDec of fctb list				(* functor *)
	| SigDec of sigb list				(* signature *)
	| FsigDec of fsigb list				(* funsig *)
	| LocalDec of dec * dec				(* local dec *)
	| SeqDec of dec list				(* sequence of dec *)
	| OpenDec of path list				(* open structures *)
	| OvldDec of symbol * ty * exp list	(* overloading (internal) *)
        | FixDec of {fixity: fixity, ops: symbol list}  (* fixity *)
        | ImportDec of string list		(* import (unused) *)
        | MarkDec of dec * linenum * linenum		(* mark a dec *)

(* VALUE BINDINGS *)
and vb = Vb of {pat:pat, exp:exp}
       | MarkVb of vb * linenum * linenum

(* RECURSIVE VALUE BINDINGS *)
and rvb = Rvb of {var:symbol, exp:exp, resultty: ty option}
        | MarkRvb of rvb * linenum * linenum

(* RECURSIVE FUNCTIONS BINDINGS *)
and fb = Fb of {var:symbol, clauses:clause list}
       | MarkFb of fb * linenum * linenum

(* CLAUSE: a definition for a single pattern in a function binding *)
and clause = Clause of {pats: pat list, resultty: ty option, exp:exp}

(* TYPE BINDING *)
and tb = Tb of {tyc : symbol, def : ty, tyvars : tyvar list}
       | MarkTb of tb * linenum * linenum

(* DATATYPE BINDING *)
and db = Db of {tyc : symbol, tyvars : tyvar list,
		def : (symbol * ty option) list}
       | MarkDb of db * linenum * linenum

(* EXCEPTION BINDING *)
and eb = EbGen of {exn: symbol, etype: ty option} (* Exception definition *)
       | EbDef of {exn: symbol, edef: path}	  (* defined by equality *)
       | MarkEb of eb * linenum * linenum

(* STRUCTURE BINDING *)
and strb = Strb of {name: symbol,def: strexp,constraint: sigexp option}
	 | MarkStrb of strb * linenum * linenum

(* FUNCTOR BINDING *)
and fctb = Fctb of {name: symbol,def: fctexp}
	 | MarkFctb of fctb * linenum * linenum

(* SIGNATURE BINDING *)
and sigb = Sigb of {name: symbol,def: sigexp}
	 | MarkSigb of sigb * linenum * linenum

(* FUNSIG BINDING *)
and fsigb = Fsigb of {name: symbol,def: fsigexp}
	  | MarkFsigb of fsigb * linenum * linenum

(* TYPE VARIABLE *)
and tyvar = Tyv of symbol
	  | MarkTyv of tyvar * linenum * linenum

(* TYPES *)
and ty 
    = VarTy of tyvar			(* type variable *)
    | ConTy of symbol list * ty list	(* type constructor *)
    | RecordTy of (symbol * ty) list 	(* record *)
    | TupleTy of ty list		(* tuple *)
    | MarkTy of ty * linenum * linenum	(* mark type *)
 
end

end (* structure Ast *)
