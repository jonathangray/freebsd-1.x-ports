(* Copyright 1989 by AT&T Bell Laboratories *)
(* lambda.sml *)

(* We add a very simple set of type information to the lambda calculus 
 * to do the representation analysis.   (zsh)
 *
 * We make the following assumptions in our typed lambda language LEXP:
 *   (1) Each lvar is uniquely named, i.e., no two bound lvar use same names 
 *       (integers). Note the only places where new variables get introduced 
 *       are at the lambda abstraction FN or the mutually recursive function 
 *       definition FIX.  
 *   (2) All data constructors are marked with a type. How to interpret these
 *       constructors depends on their conrep and lty. The current version is 
 *       doing recursive wrap-unwrap on all non-zero arity datatypes. The lty
 *       field for same constructors always gives the same type, e.g. cons 
 *       will have the type 'a * 'a list -> 'a. In the future, this lty field
 *       may contain the instantiated type at the different application site,
 *       e.g. cons : (int * int) * (int * int) list -> (int * int) list in the
 *       expression cons((1,1),nil). 
 *   (3) Type variables in a polymorphic functions are now treated as BOXEDty.
 *       Most of sum types (concrete datatypes except those transparent) are 
 *       treated as BOXEDty. 
 *   (4) All primops are also marked with a type, depending on how they are 
 *       used in the program.
 *   (5) BOGUSty is a type used to be filled in some place such as the * in 
 *       APP(FN(_,*,_),_) in the lambda expression, but its content should be 
 *       never used. 
 *   (6) RBOXEDty is used to denote those type variables that should be 
 *       recursively wrapped or unwrapped when matched by some ML types.
 *       (e.g., when 'a list -> 'a is instantiated into "(int * int) list 
 *       -> int * int", the type variable 'a is treated as an RBOXEDty). 
 *)

signature LAMBDA = sig

type primop sharing type primop = Access.primop

datatype lty 
  = INTty 
  | BOOLty
  | REALty
  | BOXEDty
  | RECORDty of lty list
  | ARROWty of lty * lty
  | CONTty of lty     
  | RBOXEDty                    (* recursively boxed type variables *) 

datatype con
  = DATAcon of Symbol.symbol * Access.conrep * lty
  | INTcon of int
  | REALcon of string
  | STRINGcon of string
  | VLENcon of int

datatype lexp
  = VAR of Access.lvar
  | FN of Access.lvar * lty * lexp
  | FIX of Access.lvar list * lty list * lexp list * lexp
  | APP of lexp * lexp
  | INT of int
  | REAL of string
  | STRING of string
  | SWITCH of lexp * Access.conrep list *
               (con * lexp) list * lexp option
  | CON of (Symbol.symbol * Access.conrep * lty) * lexp
  | DECON of (Symbol.symbol * Access.conrep * lty) * lexp
  | RECORD of lexp list
  | VECTOR of lexp list 
  | SELECT of int * lexp
  | RAISE of lexp * lty
  | HANDLE of lexp * lexp
  | PRIM of primop * lty
  | WRAP of lty * lexp
  | UNWRAP of lty * lexp

val BOGUSty : lty
val CON' : (Symbol.symbol * Access.conrep * lty) * lexp -> lexp
val DECON' : (Symbol.symbol * Access.conrep * lty) * lexp -> lexp

end

structure Lambda : LAMBDA = struct 

 open Access

 type primop = Access.primop

 datatype lty 
   = INTty 
   | BOOLty
   | REALty
   | BOXEDty
   | RECORDty of lty list
   | ARROWty of lty * lty
   | CONTty of lty  
   | RBOXEDty                    (* recursively boxed type variables *)

 type dataconstr = Symbol.symbol * Access.conrep * lty

 datatype con
   = DATAcon of dataconstr
   | INTcon of int
   | REALcon of string
   | STRINGcon of string
   | VLENcon of int	(* VLENcon is a kludge, it should go away
			   soon.  Don't assume it exists.  WEA 8/13/92 *)

 datatype lexp
   = VAR of lvar
   | FN of lvar * lty * lexp
   | FIX of lvar list * lty list * lexp list * lexp
   | APP of lexp * lexp
   | INT of int
   | REAL of string
   | STRING of string
   | SWITCH of lexp * conrep list * (con*lexp) list * lexp option
   | CON of dataconstr * lexp
   | DECON of dataconstr * lexp
   | RECORD of lexp list
   | VECTOR of lexp list
   | SELECT of int * lexp
   | RAISE of lexp * lty 
   | HANDLE of lexp * lexp
   | PRIM of primop * lty
   | WRAP of lty * lexp
   | UNWRAP of lty * lexp

 val BOGUSty = BOXEDty    

 fun CON' ((_,REF,lt),e) = APP(PRIM(Access.P.MAKEREF,lt),e)
   | CON' x = CON x

 fun DECON' ((_,REF,lt),e) = APP(PRIM(Access.P.DEREF,lt),e)
   | DECON' x = DECON x

end
