(* Copyright 1989 by AT&T Bell Laboratories *)
(* conrep.sml *)

structure ConRep : CONREP =
struct
open Access Types

fun count predicate l
  = let fun test (a::rest,acc) = test (rest,if predicate a then 1+acc else acc)
	  | test (nil,acc) = acc
     in test (l,0)
    end

fun reduce ty =
  case TypesUtil.headReduceType ty
   of POLYty{tyfun=TYFUN{body,...},...} => reduce body
    | ty => ty

fun notconst(_,true,_) = false
  | notconst(_,_,CONty(_,[t,_])) = 
        (case (reduce t) of CONty(RECORDtyc nil,_) => false
                          | _ => true)
  | notconst _ = true

(*fun show((sym,_,_)::syms, r::rs) = 
  (print(Symbol.name sym); print ":   "; PPBasics.ppRep r; 
   print "\n"; show(syms,rs))
  | show _ = (print "\n")
*)

fun boxed ([(_,false,_)]: (Symbol.symbol*bool*ty) list): conrep list = [TRANSPARENT]
  | boxed cons =
      let val multiple = count notconst cons > 1
	  fun decide (const_tags,nonconst_tags,(_,true,_)::rest) = 
			CONSTANT const_tags :: 
		            decide(const_tags+1,nonconst_tags,rest)
	    | decide (const_tags,nonconst_tags,(_,false,CONty(_,[ty,_]))::rest) =
		  (case (reduce ty, multiple)
		    of (CONty(RECORDtyc nil,_),_) => 
			       CONSTANT const_tags :: 
				    decide(const_tags+1,nonconst_tags,rest)
		     | (CONty(RECORDtyc l, _), true) => 
			       (if !System.Control.CG.newconreps
				then TAGGEDREC(nonconst_tags,length l) 
				else TAGGED(nonconst_tags))
			       :: decide(const_tags,nonconst_tags+1,rest)
		     | (CONty(RECORDtyc l, _), false) => 
			       UNTAGGEDREC(length l) :: 
				     decide(const_tags,nonconst_tags,rest)
		     | (_, true) =>  TAGGED nonconst_tags :: 
				       decide(const_tags,nonconst_tags+1,rest)
		     | (_, false) => UNTAGGED :: 
					decide(const_tags,nonconst_tags,rest))
	    | decide (_,_,nil) = []
       in decide(0,0, cons)
      end

(*val boxed = fn l => let val l' = boxed l in show(l,l'); l' end*)

end (* structure ConRep *)
