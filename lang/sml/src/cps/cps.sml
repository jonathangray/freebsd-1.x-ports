(* cps.sml
 *
 * Copyright 1989 by AT&T Bell Laboratories
 *)

signature CPS =
  sig

    type lvar
    exception Ctable

    structure P : sig
      (* These are two-way branches dependent on pure inputs *)
	datatype branch
	  = boxed | unboxed | < | <= | > | >= | ieql | ineq
	  | lessu | gequ | feql | fge | fgt | fle | flt | fneq
      (* These all update the store *)
	datatype setter
	  = store | unboxedupdate | boxedupdate | update
          | updatef | sethdlr | setvar | uselvar | setspecial
      (* These fetch from the store.  They can never have 
         functions as arguments. *)
	datatype looker
	  = ! | gethdlr | subscript | subscriptf | getvar | deflvar | ordof
	  | getspecial
      (* These might raise exceptions.  They can never have 
         functions as arguments.*)
	datatype arith
	  = * | + | - | div | fadd | fdiv | fmul | fsub | ~ | floor | round
      (* These don't raise exceptions and don't access the store. *)
	datatype pure
	  = length | objlength | makeref | rshift | lshift | orb
	  | andb | xorb | notb | fnegd | fabsd | real | subscriptv
	  | gettag | mkspecial
      end                 

    type lty 

    datatype value
      = VAR of lvar
      | LABEL of lvar
      | INT of int 
      | REAL of string 
      | STRING of string
      | OBJECT of System.Unsafe.object

    datatype accesspath = OFFp of int | SELp of int * accesspath

  (* The rebind list of a branch never mentions a function *)
    datatype cexp
      = RECORD of (Access.record_kind * (value * accesspath) list 
                  * lvar * cexp)
      | SELECT of int * value * lvar * cexp
      | OFFSET of int * value * lvar * cexp
      | APP of value * value list
      | FIX of (lvar * lvar list * cexp) list * cexp
      | SWITCH of value * lvar * cexp list
      | BRANCH of P.branch * value list * lvar * cexp * cexp
      | SETTER of P.setter * value list * cexp
      | LOOKER of P.looker * value list * lvar * cexp
      | ARITH of P.arith * value list * lvar * cexp
      | PURE of P.pure * value list * lvar * cexp

    type function
    val combinepaths : accesspath * accesspath -> accesspath
    val lenp : accesspath -> int

  end (* signature CPS *)

structure CPS : CPS = struct

type lvar = int
exception Ctable

structure P =  struct
  (* These are two-way branches dependent on pure inputs *)
    datatype branch
      = boxed | unboxed | < | <= | > | >= | ieql | ineq
      | lessu | gequ | feql | fge | fgt | fle | flt | fneq
  (* These all update the store *)
    datatype setter
      = store | unboxedupdate | boxedupdate | update
      | updatef | sethdlr | setvar | uselvar | setspecial
  (* These fetch from the store.  They can never have functions as arguments. *)
    datatype looker
      = ! | gethdlr | subscript | subscriptf | getvar | deflvar | ordof
      | getspecial
  (* These might raise exceptions.  They can never have functions as arguments.*)
    datatype arith
      = * | + | - | div | fadd | fdiv | fmul | fsub | ~ | floor | round
  (* These don't raise exceptions and don't access the store. *)
    datatype pure
      = length | objlength | makeref | rshift | lshift | orb
      | andb | xorb | notb | fnegd | fabsd | real | subscriptv
      | gettag | mkspecial
end (* P *)

type lty = Lambda.lty  (* this is temporary, should be redefined *)

datatype value = VAR of lvar
	       | LABEL of lvar
	       | INT of int 
	       | REAL of string 
	       | STRING of string
	       | OBJECT of System.Unsafe.object

datatype accesspath = OFFp of int | SELp of int * accesspath

datatype cexp
  = RECORD of (Access.record_kind * (value * accesspath) list * lvar * cexp)
  | SELECT of int * value * lvar * cexp
  | OFFSET of int * value * lvar * cexp
  | APP of value * value list
  | FIX of (lvar * lvar list * cexp) list * cexp
  | SWITCH of value * lvar * cexp list
  | BRANCH of P.branch * value list * lvar * cexp * cexp
  | SETTER of P.setter * value list * cexp
  | LOOKER of P.looker * value list * lvar * cexp
  | ARITH of P.arith * value list * lvar * cexp
  | PURE of P.pure * value list * lvar * cexp
  withtype function = lvar * lvar list * cexp

fun combinepaths(p,OFFp 0) = p
  | combinepaths(p,q) = 
    let val rec comb =
	fn (OFFp 0) => q
	 | (OFFp i) => (case q of
		          (OFFp j) => OFFp(i+j)
		        | (SELp(j,p)) => SELp(i+j,p))
	 | (SELp(i,p)) => SELp(i,comb p)
    in comb p
    end

fun lenp(OFFp _) = 0
  | lenp(SELp(_,p)) = 1 + lenp p

end
