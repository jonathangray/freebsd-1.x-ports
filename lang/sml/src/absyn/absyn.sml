(* Copyright 1989 by AT&T Bell Laboratories *)

(* Abstract syntax of bare ML (was called bareabsyn) *)

structure Absyn = struct

local
  open Variables Types Modules Access Fixity Stamps
in

type linenum = int
datatype numberedLabel = LABEL of {name: Symbol.symbol, number: int}

datatype exp
  = VARexp of var ref * ty option
  | CONexp of datacon * ty option 
  | INTexp of int
  | REALexp of string
  | STRINGexp of string
  | RECORDexp of (numberedLabel * exp) list
  | VECTORexp of exp list
  | SEQexp of exp list
  | APPexp of exp * exp
  | CONSTRAINTexp of exp * ty
  | HANDLEexp of exp * handler
  | RAISEexp of exp * ty 
  | LETexp of dec * exp
  | CASEexp of exp * rule list
  | FNexp of rule list * ty         
  | MARKexp of exp * linenum * linenum

and rule = RULE of pat * exp

and handler = HANDLER of exp

and pat = WILDpat
	| VARpat of var
	| INTpat of int
	| REALpat of string
	| STRINGpat of string
	| CONpat of datacon * ty option 
	| RECORDpat of {fields : (label * pat) list, flex : bool,
		        typ : ty ref, pats : pat list ref}
	| APPpat of datacon * ty option * pat
	| CONSTRAINTpat of pat * ty
	| LAYEREDpat of pat * pat
	| ORpat of pat * pat
        | NOpat
        | VECTORpat of pat list * ty       

and strexp = VARstr of structureVar
	   | STRUCTstr of {body: dec list, str: Structure,
			   locations: trans list}
	   | APPstr of {oper: functorVar, argexp: strexp, argthin: thinning,
			str: Structure}
	   | LETstr of dec * strexp
	   | MARKstr of strexp * linenum * linenum

and dec	= VALdec of vb list
	| VALRECdec of rvb list
	| TYPEdec of tb list
	| DATATYPEdec of {datatycs: tycon list, withtycs: tb list}
	| ABSTYPEdec of {abstycs: tycon list, withtycs: tb list, body: dec}
	| EXCEPTIONdec of eb list
	| STRdec of strb list
	| ABSdec of strb list
	| FCTdec of fctb list
	| SIGdec of signatureVar list
	| FSIGdec of funsigVar list
	| LOCALdec of dec * dec
	| SEQdec of dec list
	| OPENdec of structureVar list
	| OVLDdec of var
        | FIXdec of {fixity: fixity, ops: Symbol.symbol list} 
        | MARKdec of dec * linenum * linenum

and vb = VB of {pat:pat, exp:exp, tyvars: tyvar list ref}

and rvb = RVB of {var:var, exp:exp, resultty:ty option, tyvars: tyvar list ref}

and fb = FB of {var:var, clauses: clause list, tyvars: tyvar list ref}

and clause = CLAUSE of {pats: pat list, resultty: ty option, exp:exp}

and tb = TB of {tyc : tycon, def : ty}

and eb = EBgen of {exn: datacon, etype: ty option, ident: exp}
       | EBdef of {exn: datacon, edef: datacon}

and strb = STRB of {strvar: structureVar, def: strexp,
		    thin: thinning, constraint: Signature option}

and fctexp = VARfct of {thinIn: thinning,thinOut:thinning, def: functorVar,
		        constraint: FctSignature option}
	   | FCTfct of {param: structureVar, def: strexp, thin: thinning, 
			constraint: Signature option}
	   | LETfct of dec * fctexp

and fctb = FCTB of {fctvar: functorVar, def:fctexp}



end

end (* structure Absyn *)
