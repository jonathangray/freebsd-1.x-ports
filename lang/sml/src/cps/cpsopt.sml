(* Copyright 1989 by AT&T Bell Laboratories *)
functor CPSopt(val maxfree : int) : sig
    structure Eta : sig
	val eta : {function: CPS.function, afterClosure: bool} -> CPS.function
      end
    val reduce : (CPS.function * System.Unsafe.object option * bool) -> CPS.function
  end = struct

 
structure CG = System.Control.CG
structure Eta = Eta(val maxfree=maxfree);
structure Contract = Contract(val maxfree=maxfree);

fun reduce (function, _, afterClosure) =
let
 val debug = !CG.misc1 (* false *)
 fun debugprint s = if debug then System.Print.say(s) else ()
 fun debugflush() = if debug then System.Print.flush() else ()
 val clicked = ref 0
 fun click (s:string) = (debugprint s; inc clicked)

 fun hoist(fvar,fargs,x) =
    if !CG.hoistup orelse !CG.hoistdown then (fvar,fargs, Hoist.hoist click x)
	else (fvar,fargs,x)

 
  fun contract last func = Contract.contract{function=func,click=click,
	 				     last=last,arg=NONE}

  fun expand(f,n,unroll) = Expand.expand{function=f,click=click,bodysize=n,
				       afterClosure=afterClosure,
				       unroll=unroll,do_headers=true}

  fun lambdaprop x = (*if !CG.lambdaprop
		       then (debugprint "\nLambdaprop:"; CfUse.hoist x)
		       else*) x

  val bodysize = !CG.bodysize
  val rounds = !CG.rounds
  val reducemore = !CG.reducemore

  fun contracter last function =
	 let val function = (clicked := 0; contract false function)
	  in if !clicked <= reducemore
	     then if last
		  then contract last function
		  else function
	     else contracter last function
	 end


  (* 
   * Note the parameter k starts at rounds..0 
   *)
  fun linear_decrease k = (bodysize * k) quot rounds
  fun double_linear k = (bodysize*2*k quot rounds) - bodysize
  fun cosine_decrease k = 
      truncate(real bodysize * (cos(1.5708*(1.0 - real k / real rounds))))

  fun cycle(0,true,function) = function
    | cycle(0,false,function) = unroll function
    | cycle(k,unrolled,function) = 
	let val _ = debugprint "\nHoist: "
	    val function = hoist function
	    val function = lambdaprop function
	    val _ = clicked := 0
	    val function = if !CG.betaexpand
		           then expand(function,linear_decrease k,false)
			   else function
	    val cl = !clicked before clicked := 0
        in if cl <= reducemore
	   then if unrolled then contract true function
                            else unroll function
	   else cycle(k-1, unrolled, contracter (k=0) function)
	end

   and unroll function = cycle(rounds,true,expand(function,bodysize,true))


in (if rounds < 0 
      then function
      else cycle (rounds, not(!CG.unroll),
	      contracter false
		(Eta.eta{function=(contract false function), afterClosure=false})))
   before (debugprint "\n"; debugflush())
end
end
