(* Copyright 1989 by AT&T Bell Laboratories *)
signature OPT =
sig
  val freevars : Lambda.lexp -> Access.lvar list
  val closestr : (Access.lvar -> string) * (Access.lvar -> Lambda.lty)
                  * Lambda.lexp * Access.lvar list -> Lambda.lexp
                  
  val closetop : Lambda.lexp * Access.lvar list 
                  * (Access.lvar -> Lambda.lty) -> Lambda.lexp
end

structure Opt : OPT =
struct

open Access Types Variables Lambda

fun root [v] = v | root (_::p) = root p
  | root _ = ErrorMsg.impossible "root [] in codegen/opt";

fun freevars e =
    let val t = Intset.new()
	val set = Intset.add t
	val unset = Intset.rmv t
	val done = Intset.mem t
	val free : int list ref = ref []
	val rec mak =
	 fn VAR w => if done w then () else (set w; free := w :: !free)
	  | FN (w,_,b) => (set w; mak b; unset w)
	  | FIX (vl,_,el,b) => (app set vl; app mak (b::el); app unset vl)
	  | APP (f,a) => (mak f; mak a)
	  | SWITCH(e,_,l,d) => 
	      (mak e;
	       app (fn (DATAcon(_,VARIABLE(PATH p),_),e) =>
			 (mak(VAR(root p)); mak e)
		     | (DATAcon(_,VARIABLEc(PATH p),_),e) =>
			 (mak(VAR(root p)); mak e)
		     | (c,e) => mak e)
		   l;
	       case d of NONE => () | SOME a => mak a)
	  | RECORD l => app mak l
          | VECTOR l => app mak l
          | CON((_,VARIABLE(PATH p),_),e) => (mak(VAR(root p)); mak e)
          | CON((_,VARIABLEc(PATH p),_),e) => (mak(VAR(root p)); mak e)
          | CON(_,e) => mak e
          | DECON(_,e) => mak e
	  | SELECT (i,e) => mak e
	  | HANDLE (a,h) => (mak a; mak h)
	  | RAISE(e,_) => mak e
	  | INT _ => ()
	  | REAL _ => ()
	  | STRING _ => ()
	  | PRIM _ => ()
          | WRAP(_,e) => mak e
          | UNWRAP(_,e) => mak e
    in  mak e; !free
    end

val boot_zeroSym = Symbol.varSymbol "boot_zero" (* receives unit *)
val boot_oneSym = Symbol.varSymbol "boot_one"   (* traverses free list *)
val boot_twoSym = Symbol.varSymbol "boot_two"   (* final bogus arg *)

fun closestr(lookup: int->string, getty : int -> lty, 
             e:lexp, extras : int list) : lexp =
    let val fv = SortedList.uniq(extras @ freevars e)
	val names = map lookup fv
        fun g(v,(f,t)) = 
           let val w = namedLvar boot_oneSym
               val t' = RECORDty[getty v,t]
	    in (FN(w,t',APP(FN(v,BOGUSty,APP(f,SELECT(1,(VAR w)))),
		 	    SELECT(0,(VAR w)))),
                t')
	   end
        val (body,t) = fold g fv (FN(namedLvar boot_twoSym,BOGUSty,e),BOGUSty)
        val sl = fold (fn (s,f) => RECORD[STRING s, f]) names (RECORD[])
        val _ = if !System.Control.debugging
	        then app (fn s => app System.Print.say[s," "]) names
	        else ()
     in FN(namedLvar boot_zeroSym,INTty,RECORD[body,sl])
    end

val lookupSym = Symbol.varSymbol "lookup"

fun closetop(le: lexp, extras: int list, getty : lvar -> lty): lexp =
  let val fv = SortedList.uniq(extras @ freevars le)
      val looker = namedLvar lookupSym
      fun g(v,f) = APP(FN(v,BOGUSty,f),
                       UNWRAP(getty(v),APP(VAR looker,INT v)))
   in FN(looker,ARROWty(INTty,BOXEDty),fold g fv le)
  end

end (* structure Opt *)
