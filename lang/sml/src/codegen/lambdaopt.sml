(* Copyright 1989 by AT&T Bell Laboratories *)

structure LambdaOpt : sig val lambdaopt: Lambda.lexp -> Lambda.lexp end =
struct

open Access Lambda
fun click a = if !System.Control.CG.misc1 then System.Print.say a else ()
    		
fun last [x] = x | last(a::r) = last r

fun lambdaopt lexp = if not(!System.Control.CG.lambdaopt )
			 then lexp else
let exception LambdaOpt
    val m : (int * lexp) Intmap.intmap 
	     = Intmap.new(32,LambdaOpt)
    val enter = Intmap.add m
    val get0 = Intmap.map m
    fun get v = SOME(get0 v) handle LambdaOpt => NONE
    val kill = Intmap.rmv m

 fun all p (a::r) = p a andalso all p r | all p nil = true

 val rec reduce_ok =
  fn VAR v => true
   | APP(FN(v,_,a),b) => (reduce_ok a andalso reduce_ok b)
   | FN(v,_,e) => true
   | PRIM _ => true
   | INT _ => true
   | REAL _ => true
   | STRING _ => true
   | _ => false
	 (* It's dangerous, for reasons of space complexity, 
	  to hoist the operators SELECT, DECON, SWITCH,
	  FIX, and HANDLE downwards.  The operators 
	  APP, RAISE might have a sideeffect. 
	  And, even though it's safe, there's not much point in
	  optimizing "let v = RECORD ... " because cps-conversion
	  will just re-introduce the "let" expression. 
	*)

 val rec pass1 =
  fn VAR v => (case get v
	        of SOME(0,e) => enter(v,(1,e))
		 | SOME _ => kill v
		 | NONE => ())
   | APP(FN(v,_,a),b) => (if reduce_ok b
		 	    then enter(v, (0,b)) else ();
		        pass1 a; pass1 b)
   | FN(v,_,e) => pass1 e
   | FIX(fl,_,el,b) => (app pass1 el; pass1 b)
   | APP(a,b) => (pass1 a; pass1 b)
   | SWITCH(e,_,l,NONE) => (pass1 e; app conpass1 l)
   | SWITCH(e,_,l,SOME d) => (pass1 e; app conpass1 l; pass1 d)
   | CON(c,e) => conpass1(DATAcon c, e)
   | DECON(c,e) => conpass1(DATAcon c, e)
   | RECORD el => app pass1 el
   | VECTOR el => app pass1 el
   | SELECT(_,e) => pass1 e
   | RAISE(e,_) => pass1 e
   | HANDLE(a,b) => (pass1 a; pass1 b)
   | PRIM _ => ()
   | INT _ => ()
   | REAL _ => ()
   | STRING _ => ()
   | WRAP(_,e) => pass1 e
   | UNWRAP(_,e) => pass1 e

 and conpass1 =
   fn (DATAcon(_,VARIABLE(PATH p),_), e) =>(kill(last p); pass1 e)
    | (DATAcon(_,VARIABLEc(PATH  p),_), e) =>(kill(last p); pass1 e)
    | (_,e) => pass1 e

 val rec g =
  fn a as VAR v => (case get v
		     of SOME(_,e) => (kill v; g e)
		      | NONE => a)
   | FN(v,t,b) => FN(v, t, g b)
   | APP(a as FN(v,_,e), b) =>
	  (case get v
            of SOME(1,_) => (click "$"; g e)
             | SOME _ => (kill v; click "#"; g e)
	     | NONE => APP(g a, g b))
   | FIX(fl,t,el,b) => FIX(fl, t, map g el, g b)
   | APP(a as VAR f, b) => 
	      (case get f
		of SOME(_,e) => (kill f; click "%"; g(APP(e,b)))
		 | NONE => APP(a, g b))
   | APP(a,b) => APP(g a, g b)
   | SWITCH(e,cl,el,d) => 
	  SWITCH(g e, cl, 
		 map (fn (c,e) => (c, g e)) el,
		 case d of SOME d' => SOME(g d') | NONE => NONE)
   | CON(c,e) => CON(c, g e)
   | DECON(c,e) => DECON(c, g e)
   | RECORD el => RECORD (map g el)
   | VECTOR el => VECTOR (map g el)
   | SELECT(i,e) => SELECT(i, g e)
   | RAISE(e,t) => RAISE(g e,t)
   | HANDLE(a,b) => HANDLE(g a, g b)
   | WRAP(t,e) => WRAP(t,g e)
   | UNWRAP(t,e) => UNWRAP(t,g e)
   | e => e
 in click "Lambda Opt: \n"; pass1 lexp;
     g lexp before click "\n"
end

end

