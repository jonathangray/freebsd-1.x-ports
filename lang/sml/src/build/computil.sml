(* Copyright 1992 by AT&T Bell Laboratories *)

signature COMPUTIL =
sig
  exception Abort
  exception Eof

  val timemsg : string -> System.Timer.time -> bool
  val infomsg : string -> System.Timer.time -> bool
  val debugmsg : string -> bool

  val translate : Modules.env * Absyn.dec * Source.inputSource
                  -> Access.lvar list * Lambda.lexp
  val transStrb : Modules.env * Absyn.strb * Source.inputSource
                  -> Lambda.lexp
  val transFctb : Modules.env * Absyn.fctb * Source.inputSource
                  -> Lambda.lexp

  val isolate : ('a -> 'b) -> 'a -> 'b

  val convert : Lambda.lexp -> CPS.function

  val gengetty : InverseEnv.invenv -> (Access.lvar -> Lambda.lty)
end


structure CompUtil : COMPUTIL =
struct

  open Access Modules Absyn System.Timer

  val update = System.Stats.update

  (* conditional message functions *)
  fun printmsg (flag: bool ref) (msg: string) : bool =
      if !flag
      then (app System.Print.say[msg, "\n"]; System.Print.flush(); true)
      else false

  fun timemsg s t = printmsg System.Control.timings 
			(implode[s,", ",makestring t,"s"])
  val debugmsg = printmsg System.Control.debugging	
  fun infomsg s t = timemsg s t orelse debugmsg s

  exception Abort 
  exception Eof

  fun translate(env,absyn,source) =
      let val timer = start_timer()
	  val newlvars = Linkage.getvars absyn
	  val absyn' = SProf.instrumDec source (Prof.instrumDec(absyn))
          val lambda = Translate.transDec env (ErrorMsg.error source)
			  (ErrorMsg.matchErrorString source) absyn'
                          (Lambda.RECORD (map Lambda.VAR newlvars))
	  val time = check_timer timer
       in update(System.Stats.translate,time);
	  infomsg "translate" time;
	  (newlvars, lambda)
      end

  fun transStrb(env,sb,source) =
      let val timer = start_timer()
	  val sb = Prof.instrumStrb sb
	  val STRB{strvar=STRvar{access=PATH[v],...},...} = sb
	  val lam = Translate.transDec env (ErrorMsg.error source)
			      (ErrorMsg.matchErrorString source)
			      (Absyn.STRdec[sb]) (Lambda.VAR v)
	  val time = check_timer timer
       in update(System.Stats.translate,time);
	  infomsg "translate" time;
	  lam
      end

  fun transFctb(env,fb,source) =
      let val timer = start_timer()
	  val fb = Prof.instrumFctb fb
	  val FCTB{fctvar=FCTvar{access=PATH[v],...},...} = fb
	  val lam = Translate.transDec env (ErrorMsg.error source)
			(ErrorMsg.matchErrorString source)
		        (Absyn.FCTdec[fb]) (Lambda.VAR v)
	  val time = check_timer timer
       in update(System.Stats.translate,time);
	  infomsg "translate" time;
	  lam
      end

  exception Top_level_callcc

  local val cont_stack = ref (nil : unit ref list)

  in fun isolate f x = (* Just like f x, except that it catches
		       top-level callcc's  *)
   let val r = ref()
       val _ = cont_stack := r :: !cont_stack;
       fun pop_stack() =
	   case !cont_stack
	    of r' :: rest => (cont_stack := rest;
			      if r<>r' then raise Top_level_callcc else ())
	     | _ => raise Top_level_callcc (* can this ever happen? *)
       val a = f x 
	       handle e => (pop_stack(); raise e)
    in pop_stack (); 
       a
   end
  end

  fun convert lambda =
      let val timer = start_timer()
	  val _ = if !System.Control.CG.printLambda
		      then MCprint.printLexp lambda
		  else ()
	  val lambda = LambdaOpt.lambdaopt lambda
	  val _ = if !System.Control.CG.printLambda
		      then MCprint.printLexp lambda
		  else ()
	  val lambda = Reorder.reorder lambda
	  val _ = if !System.Control.CG.printLambda
		      then MCprint.printLexp lambda
		  else ()
	  val time0 = check_timer timer
	  val _ = infomsg "codeopt" time0
	  val (function,_) =  Convert.convert lambda
	  val time = sub_time(check_timer timer, time0)
      in  update(System.Stats.codeopt,time0);
	  update(System.Stats.convert,time);
	  infomsg "convert" time;
	  function
      end

   fun gengetty ienv =
        (fn v => ((#ty(InverseEnv.look ienv v))
                   handle InverseEnv.Unbound => ((!CoreInfo.coreLty) v)))


end (* structure CompUtil *)
