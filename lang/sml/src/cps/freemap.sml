signature FREEMAP =
  sig
    val freevars: CPS.cexp -> CPS.lvar list
    val freemap : (CPS.lvar * CPS.lvar list -> unit)
			-> (CPS.cexp -> CPS.lvar list)
    val cexp_freevars: (CPS.lvar->CPS.lvar list) -> CPS.cexp -> CPS.lvar list
    val freemapClose : CPS.cexp
			-> ((CPS.lvar -> CPS.lvar list) *
			    (CPS.lvar -> bool) *
			    (CPS.lvar -> bool))
  end

structure FreeMap : FREEMAP =
struct
open CPS SortedList

local fun vars(l, VAR x :: rest) = vars(x::l, rest)
	| vars(l, _::rest) = vars(l,rest)
	| vars(l, nil) = uniq l
in fun clean l = vars(nil, l)
end
val enter = fn (VAR x,y) => enter(x,y) | (_,y) => y

val rec freevars =
  fn APP(v,args) => enter(v,clean args)
   | SWITCH(v,c,l) => enter(v,foldmerge (map freevars l))
   | RECORD(_,l,w,ce) => merge(clean (map #1 l), rmv(w, freevars ce))
   | SELECT(_,v,w,ce) => enter(v, rmv(w, freevars ce))
   | OFFSET(_,v,w,ce) => enter(v, rmv(w, freevars ce))
   | SETTER(_,vl,e) => merge(clean vl, freevars e)
   | LOOKER(_,vl,w,e) => merge(clean vl, rmv(w, freevars e))
   | ARITH(_,vl,w,e) => merge(clean vl, rmv(w, freevars e))
   | PURE(_,vl,w,e) => merge(clean vl, rmv(w, freevars e))
   | BRANCH(_,vl,c,e1,e2) => merge(clean vl, merge(freevars e1, freevars e2))
   | FIX(fl,e) =>
	let fun g(f,vl,ce) = difference(freevars ce, uniq vl)
	 in difference(foldmerge (freevars e :: map g fl), uniq(map #1 fl))
	end



fun freemap add =
let 
    (* Doesn't apply "add" to the rebound variables of a branch *)

    fun setvars (w,free) = let val g = rmv(w,free)
			   in add(w,g); g
			   end
    val rec freevars =
	 fn APP(v,args) => enter(v,clean args)
	  | SWITCH(v,c,l) => enter(v,foldmerge (map freevars l))
	  | RECORD(_,l,w,ce) => merge(clean (map #1 l), setvars(w, freevars ce))
	  | SELECT(_,v,w,ce) => enter(v, setvars(w, freevars ce))
	  | OFFSET(_,v,w,ce) => enter(v, setvars(w, freevars ce))
	  | SETTER(_,vl,e) => merge(clean vl, freevars e)
	  | LOOKER(_,vl,w,e) => merge(clean vl, setvars(w, freevars e))
	  | ARITH(_,vl,w,e) => merge(clean vl, setvars(w, freevars e))
	  | PURE(_,vl,w,e) => merge(clean vl, setvars(w, freevars e))
	  | BRANCH(_,vl,c,e1,e2) => 
		merge(clean vl,merge(freevars e1, freevars e2))
	  | FIX _ => ErrorMsg.impossible "FIX in Freemap.freemap"
in freevars
end



(* 
 * cexp_freevars
 *	- To be used in conjunction with FreeMap.freemap.
 *	Consequently, raises an exception for FIX.
 *)
fun cexp_freevars lookup cexp =
    let val rec f = 
	fn RECORD(_,vl,w,_) => merge(clean(map #1 vl), lookup w)
	 | SELECT(_,v,w,_) => enter(v, lookup w)
	 | OFFSET(_,v,w,_) => enter(v, lookup w)
	 | APP(f,vl) =>  clean (f::vl)
	 | FIX _ => ErrorMsg.impossible "FIX in Freemap.cexp_freevars"
	 | SWITCH(v,c,cl) => 
	       enter(v, foldmerge (map f cl))
         | SETTER(_,vl,e) => merge(clean vl, f e)
	 | LOOKER(_,vl,w,e) => merge(clean vl, lookup w)
	 | ARITH(_,vl,w,e) => merge(clean vl, lookup w)
	 | PURE(_,vl,w,e) => merge(clean vl, lookup w)
	 | BRANCH(_,vl,c,e1,e2) => merge(clean vl,merge(f e1, f e2))
    in f cexp
    end


(* Produces a free variable mapping at each function binding.
   The mapping includes the functions bound at the FIX, but
   not the arguments of the function. *)
fun freemapClose ce =
let exception Freemap
    val vars : lvar list Intmap.intmap = Intmap.new(32, Freemap)
    val escapes = Intset.new()
    val escapesP = Intset.mem escapes
    fun escapesM(VAR v) = Intset.add escapes v
      | escapesM _ = ()
    val known = Intset.new()
    val knownM = Intset.add known
    val rec freevars =
	 fn FIX(l,ce) =>
		let val functions = uniq(map #1 l)
		    (* MUST be done in this order due to side-effects *)
		    val freeb = freevars ce
		    val freel =
			fold (fn ((v,args,body),freel) =>
			       (let val l = remove(uniq args,freevars body)
				in  Intmap.add vars (v,l);
				    l::freel
				end))
			      l nil
		in  app (fn v => if escapesP v then () else knownM v)
		        functions;
		    remove(functions,foldmerge(freeb::freel))
		end
	  | APP(v,args) => (app escapesM args;
			    enter(v, clean args))
	  | SWITCH(v,c,l) => foldmerge (clean[v]::(map freevars l))
	  | RECORD(_,l,w,ce) => (app (escapesM o #1) l;
			       merge(clean (map #1 l), rmv(w,freevars ce)))
	  | SELECT(_,v,w,ce) => enter(v,rmv(w,freevars ce))
	  | OFFSET(_,v,w,ce) => enter(v,rmv(w,freevars ce))
	  | LOOKER(_,vl,w,ce) => (app escapesM vl; 
				 merge(clean vl, rmv(w,freevars ce)))
	  | ARITH(_,vl,w,ce) => (app escapesM vl;
				merge(clean vl, rmv(w,freevars ce)))
	  | PURE(_,vl,w,ce) => (app escapesM vl;
			       merge(clean vl, rmv(w,freevars ce)))
	  | SETTER(_,vl,ce) => (app escapesM vl; merge(clean vl, freevars ce))
	  | BRANCH(_,vl,c,e1,e2) =>
	          (app escapesM vl; 
		   merge(clean vl,merge(freevars e1, freevars e2)))
in  freevars ce;
    (Intmap.map vars, Intset.mem escapes, Intset.mem known)
end

(* temporary, for debugging *)
fun timeit f a =
  let val t = System.Timer.start_timer()
      val r = f a
  in  System.Stats.update(System.Stats.freemap,System.Timer.check_timer t);
      r
  end
val freemap = timeit freemap
val freemapClose = timeit freemapClose
val freevars = timeit freevars

end (* structure FreeMap *)

