(***************************************************************************

 REPRESENTATION.SML This files gives a description of the main types used
 to represent structures. 

 - Most of the types are transfered directly from the module description.
 - There is a new "fctSignature" with all the related types.
 - Note FCT_FORMAL, FCT_ABSFB and FCT_INSTANCE in Functor.
   Their meaning is close to   the meaning in Structure of STR_FORMAL,
   STR_ABSFB and INSTANCE.(note: should be rewriten in STR_INSTANCE).
 - APPLY describes an application of a functor to an argument. It is simplified
   into the result alone if we the functor is not a parameter of an
   enclosing functor.
 - FULL_SIG is used when we don't want any coercion of a structure in an
   instance
 - IRRELEVANT means that there is no reason for giving a kind to the signature
   because it has been elaborated with the structure it describes. The contents
   of kind describes how to build the skeleton of a structure instance of the
   signature.


Constraints: 

- In INSTANCEs, arrays must contain objects in the order of their definition
  (at least for origin structures). It is used to abstract them.

- In true signatures, the symbol field must contain the list of bound names
  in the order of their definition.

- In a functor body, if the argument is necesary, it is stored at index 0.

- SIMPLEs are only used for toplevel structures. Arguments of functors (after
  coercion by the functor signature) and functor bodies must be instances.

 ***************************************************************************)

structure Modules = struct

local
  open Types Symbol Access
in

(* symbolic paths *)
type spath = symbol list

(* position in files *)
type strpos = int;

(* BINDINGS *)

(* The following datatypes are used to describe objects stored in static
   environments.
   - Name is the symbol used to access them but it is not necessarily the key
   stored in the environment.
   - Binding is the value itself.
   - Access describes the path to follow to get the value of the component in
   the dynamic representation of the object. In a structure it is the slot
   where the object is stored. In the global environment, it is a PATH with
   the global lvar to look for *)


datatype fixityVar
    = FIXvar of {name: Symbol.symbol, binding: Fixity.fixity}

datatype signatureVar
    = SIGvar of {name: Symbol.symbol, binding: Signature}

and funsigVar 
    = FSIGvar of {name: Symbol.symbol, binding: FctSignature}

and structureVar
    = STRvar of {name: Symbol.symbol,
		 access: Access.access,	   
		 binding: Structure (* was strb *)}

and functorVar
    = FCTvar of {name: Symbol.symbol,
		 access: Access.access,
		 binding: Functor}

(* all those types are grouped by binding *)

and binding
    = VARbind of Variables.var
    | CONbind of Types.datacon
    | TYCbind of Types.tycon
    | SIGbind of signatureVar
    | STRbind of structureVar
    | FSIGbind of funsigVar
    | FCTbind of functorVar
    | FIXbind of fixityVar

(* SIGNATURES *)

and Signature
    = SIG of { (* the ordered list of symbols defined (in sig only) *)
	      symbols : Symbol.symbol list ref,
              env : env ref, (* specification of objects *)
              stamp : Stamps.stamp, (* for fast equality test *)
              path : symbol option, (* used for printing only *)
              kind : sigkind ref}
    (* the signature is the full actual structure *)
    | FULL_SIG 
    (* bogus signature *)
    | ERROR_SIG

(* a structure obtained by coercion by a signature is of type instance.
   signatures contain enough information to build the skeleton of the
   instance *)

and sigkind 
  (* This is a toplevel signature. All sharing constraints have been pushed
     to that level and are expressed by reference to the root of the signature
     strcount, fctcount and typecount are the size of the arrays needed to
     build an instance of the signature. Slotcount gives the size of the
     record denoting it. *)
  = TOP of {
	(* specifies the shape of the skeleton of an instance *)
	strcount : int,
	fctcount : int, 
	typecount : int,
	slotcount : int,

	(* specifies the constraints of sharing *)
	sConstraints : {internal: spath list,
			external: Structure option} list,
	tConstraints : {internal: spath list,
			external: Types.tycon option} list}
  (* The signature is embedded in a top signature. The instance structure will
     share the instance arrays with its embedding structure *)
  | EMBEDDED 
  (* the kind field is irrelevant in a signature of an instance. It is true
     when the signature is obtained while building the structure. But even
     then it is important to maintain the invariant that signature contains
     enough information to rebuild the structure because it is used by
     instantiate. *)
  | IRRELEVANT 

(* FUNCTOR SIGNATURE: note the parallel between this and fct and sig *)
and FctSignature
    (* a complete specification of a functor signature *)
    = FSIG of {path : symbol option, 	(* its name (toplevel object) *)
	       paramName : symbol, 	(* name of the parameter (printing) *)
               argument : Signature,	(* signature of the argument *)
               body : Signature}	(* signature of the body *)
    (* the signature is irrelevant: look at the actual functor to get it *)
    | FULL_FSIG
    (* the signature is bogus *)
    | ERROR_FSIG

(* STRUCTURES *)
and Structure
    (* a simple raw structure defined at toplevel *)
    = SIMPLE of {stamp: Stamps.stamp,	(* stamp *)
                 env: env,		(* environment defining the bindings *)
                 path:spath		(* path to the structure *)
      }
    (* result of coercing the origin structure by a signature. *)
    | INSTANCE of
         {sign : Signature,		(* signature describing the instance *)
          subStrs : Structure array,	(* structure array *)
          subFcts : Functor array,	(* functor array *)
          types : Types.tycon array,	(* types array *)
          origin : Structure,		(* structure coerced by sign *)
          path : spath			(* path to the structure *)
      }
    (* representation of a binding coming from an opened structure *)
    | STR_OPEN of {pos : int list, spec : Signature, name : spath}
    (* representation of a formal binding in a signature *)
    | STR_FORMAL of {pos : strpos, spec : Signature}
    (* abstracted binding in a functor body *)
    | STR_ABSFB of absfbpos
    (* used to give a stamp to instance without origin *)
    | SELF of Stamps.stamp
    (* describe the result res of the application of functor fct to arg *)
    | APPLY of {fct: Functor, arg: Structure, res: Structure} 
    (* an error has been found *)
    | ERROR_STR

(* FUNCTORS *)

(* Invariants maintained in functors:
   - The parent structure must be an instance or ERROR_STR (which means that
   the functor is a toplevel one).
   - The body describes an INSTANCE structure. It is a recepee to build it.
	- str describes the root of the result
	- sequences describe basic operation. Objects resulting from these
	  are identified by the position of the basic operation is the sequence
	- strseq describe how to build a one level structure from other 
	  structures types or functors obtained by
	    - looking up in the parameter ABSFB(PARAM path)
	    - looking up in the result of another basic operation
		ABSFB(SEQ number) or ABSFB(SEQ(number,path))
	    - applying a functor to a structure (structure of the APPLY form)
	    - taking the object as is if it is not in any of the previous
	      categories
   - The result structures verifies certain properties depending on how it
     has been obtained.
	- if the specification is just a variable (a component of the parameter
	  or of the outside world). Then the body is just that structure
	- if the specification is a complete definition of a structure, then
	  this structure is an INSTANCE whose substructure stored in 0 is
	  the argument. The argument is a pair made of the Parent structure
	  and of the actual parameter. It is an instance. The way it is done
	  now leads that the parent is in slot 0 and the parameter in slot 1
	- if it is an application of another functor to an argument, then the
	  argument verifies the constraints as defined previously but it is
	  the result of the second application that is given as body
*)

and Functor
    (* raw functor *)
    = FCT of {stamp: Stamps.stamp, 	(* stamp for identification and age *)
	      parent: Structure,	(* parent structure *)
              paramName: symbol,	(* formal name of the parameter *)
              argument: Signature,	(* signature coercing the argument *)
              body: {strseq: Structure list,	(* sequence of orders to fol *)
                     fctseq: Functor list,	(* low to build a new instan *)
                     tyseq: tycon list,		(* ce of the body	     *)
                     str: Structure,            (* a list of actions        *) 
                     fullstr: Structure}}       (* the full str, only used for
                                                   representation analysis *)
    (* as STR_FORMAL *)
    | FCT_FORMAL of {pos: strpos, spec: FctSignature}
    (* as STR_OPEN *)
    | FCT_OPEN of {pos : int list, spec : FctSignature, name : spath}
    (* as INSTANCE a new parent may be given and used in FctSignature *)
    | FCT_INSTANCE of {fsig:FctSignature,fct:Functor,parent: Structure}
    (* as STR_ABSFB *)
    | FCT_ABSFB of absfbpos
    (* as ERROR_STR *)
    | ERROR_FCT

withtype env = binding Env.env


(* THINNING *)

datatype trans 
     (* old position, used for val, exn, or unthinned str *)
   = VALtrans of access * ty * ty option
     (* old str position, substr thinning *)
   | THINtrans of access * lvar * trans list
     (* functor body thinning, same as previous for the fields 
        idea: THINtrans translates into fn str => {strthined}
              FCTtrans translates into 
		fn f => fn a => case (f a) of str => {strthined}
	where strthined is describe by the trans list
      *)
   | FCTtrans of access * Signature * thinning * thinning
     (* constructor as value component *)
   | CONtrans of datacon * ty option

withtype thinning = (lvar * trans list) option

type fctThinning = (thinning * thinning) option
end

end (* structure Moduless *)
