(* Copyright 1989 by AT&T Bell Laboratories *)
functor CPScomp(CM : CMACHINE) : 
	sig val compile : CPS.function * System.Unsafe.object option * ErrorMsg.complainer -> unit 
	end =
struct

val maxfree = case CM.arithtemps of [] => 3+length(CM.miscregs)-1
                                  | _ => 3+length(CM.miscregs)

structure CPSg = CPSgen(CM)
structure CPSopt = CPSopt(val maxfree = maxfree)
structure Eta = CPSopt.Eta;
structure Closure = Closure(val maxfree = maxfree)
(*
structure ClosureCallee = ClosureCallee(val maxfree = maxfree)
*)
structure Spill = Spill(val maxfree = maxfree)
val pr = System.Print.say

fun time (f,m,s,p) x =
  let val _ = CompUtil.debugmsg m
      val t = System.Timer.start_timer()
      val r = f x
      val t' = System.Timer.check_timer t
  in  System.Stats.update(s,t');
      if (!System.Control.CG.printit orelse !System.Control.CG.printsize)
	  then (pr "\nAfter "; pr m; pr ":\n")
      else ();
      CompUtil.timemsg m t';
      p r;
      System.Print.flush();
      r
  end

fun fprint (function as (f,vl,cps)) =
  (if !System.Control.CG.printsize
       then CPSsize.printsize cps
   else ();
   if !System.Control.CG.printit
       then CPSprint.showfun pr function
   else ())
	   
fun flprint functions = 
  if !System.Control.CG.printit
      then app (CPSprint.showfun pr) functions
  else ()

fun nullprint _ = ()

fun compile(function,argument,err) =
 let

  val _ = if !System.Control.CG.printit orelse !System.Control.CG.printsize
	     then (pr "\nAfter convert:\n";
		   fprint function; System.Print.flush())

             else ()

  val reduce = CPSopt.reduce

  val cpsopt = if !System.Control.CG.cpsopt
		then time(reduce,"cpsopt",System.Stats.cpsopt,fprint)
		else fn (cps,_,_) => cps
  val function = cpsopt(function,argument,false)

  val fiddle = if !System.Control.CG.knownfiddle
                then time(KnownFiddle.fiddle,"knownfiddle",
			  System.Stats.closure, fprint)
		else fn cps => cps

  val function = fiddle function

  val closure = time(Closure.closeCPS,"closure",
		     System.Stats.closure,fprint)
  val function = closure function

  val unfiddle = if !System.Control.CG.knownfiddle
                then time(fn f => Eta.eta{function=f,afterClosure=true},
			  "unfiddle", System.Stats.closure, fprint)
		else fn cps => cps
      
  val function = unfiddle function


  val globalfix = time(GlobalFix.globalfix,"globalfix",
		       System.Stats.globalfix,flprint)
  val carg = globalfix function

  fun reoptimize((f,vl,body)::carg') = 
           globalfix(cpsopt((f,vl,CPS.FIX(carg',body)),argument,true))
    | reoptimize _ = ErrorMsg.impossible "reoptimize"

  val carg = if !System.Control.CG.optafterclosure
    then let open System.Control.CG
	     val u = !hoistup and d = !hoistdown
	 in hoistup := false; hoistdown := false;
            reoptimize carg            
            before (hoistup := u; hoistdown := d)
         end
    else carg

  val spill     = time(Spill.spill,"spill",System.Stats.spill,flprint)
  val carg = spill carg

(**
  val branch = time(Branch.branch, "branch", System.Stats.spill)
  val carg = if !System.Control.CG.misc3>0 then branch carg else carg
  val _ = (flprint carg; write "\n")
**)

  val limit = time(Limit.nolimit,"limit",System.Stats.globalfix,nullprint)
  val limits = limit carg

  val codegen   = time(CPSg.codegen,"generic",System.Stats.codegen,nullprint)
  val _ = codegen(carg,limits,err)
  val _ = CompUtil.debugmsg "\ndone\n"
 in  ()
 end
(* end of compile *)


end (* functor CPScomp *)
