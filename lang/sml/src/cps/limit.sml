structure Limit : sig datatype kind = ESCAPES | KNOWN_CHECK | KNOWN
		      val nolimit : CPS.function list -> CPS.lvar -> 
			             {known: kind, words_alloc: int}
                       (* indicate which functions don't need limit checks,
			  and for each function how many words are allocated
			  before the next limit check *)
		  end =
struct

open Access CPS
val say = System.Print.say

val MAX_ALLOC = 1023  (* maximum number of words to allocate per check *)

datatype kind = ESCAPES | KNOWN_CHECK | KNOWN

fun findescapes(fl as (firstfun,_,_)::rest) =
 let exception Limit
     val m : kind Intmap.intmap = Intmap.new(32,Limit)
     val add0 = Intmap.add m
     fun add(LABEL v) = add0(v,ESCAPES) | add _ = ()
     fun f(RECORD(_,l,_,e)) = (app (add o #1) l; f e)
       | f(SELECT(_,_,_,e)) = f e
       | f(OFFSET(_,_,_,e)) = f e
       | f(SWITCH(_,_,el)) = app f el
       | f(SETTER(_,vl,e)) = (app add vl; f e)
       | f(LOOKER(_,vl,_,e)) = (app add vl; f e)
       | f(ARITH(_,vl,_,e)) = (app add vl; f e)
       | f(PURE(_,vl,_,e)) = (app add vl; f e)
       | f(BRANCH(_,vl,c,e1,e2)) = (app add vl; f e1; f e2)
       | f(APP(_,vl)) = app add vl
       | f (FIX _) = ErrorMsg.impossible "8931 in limit"
     val escapes = Intmap.map m
 in  add0(firstfun,ESCAPES);
     app (fn (h,_,_) => add0(h,KNOWN)) rest;
     app (f o #3) fl; 
     {escapes = escapes,
      check = fn f => case escapes f of KNOWN => add0(f, KNOWN_CHECK)
                                      | _ => ()}
 end

fun path escapes functions = 
  let exception Limit'
      val b : cexp Intmap.intmap = Intmap.new(32,Limit')
      val _ = app (Intmap.add b o (fn (f,_,body) => (f,body))) functions
      val body = Intmap.map b

      val m : {known:kind, words_alloc: int} Intmap.intmap = 
	                                     Intmap.new(32,Limit')
      val look = Intmap.map m
      val storeListSz = 4  (* size of store list record *)
      fun g(d, RECORD(_,vl,_,e)) = g(d+length(vl)+1, e)
        | g(d, SELECT(_,_,_,e)) = g(d, e)
        | g(d, OFFSET(_,_,_,e)) = g(d, e)
        | g(d, SWITCH(_,_,el)) = fold max (map (fn e => g(d,e)) el) 0
        | g(d, SETTER(P.update,_,e)) = g(d+storeListSz, e)
        | g(d, SETTER(P.boxedupdate,_,e)) = g(d+storeListSz, e)
        | g(d, ARITH(P.fadd,_,_,e)) = g(d+3, e)
        | g(d, ARITH(P.fsub,_,_,e)) = g(d+3, e)
        | g(d, ARITH(P.fmul,_,_,e)) = g(d+3, e)
        | g(d, ARITH(P.fdiv,_,_,e)) = g(d+3, e)
        | g(d, PURE(P.fnegd,_,_,e)) = g(d+3, e)
        | g(d, PURE(P.fabsd,_,_,e)) = g(d+3, e)
        | g(d, PURE(P.real,_,_,e)) = g(d+3, e)
        | g(d, LOOKER(P.subscriptf,_,_,e)) = g(d+3, e)
        | g(d, SETTER(_,_,e)) = g(d,e)
        | g(d, LOOKER(_,_,_,e)) = g(d,e)
        | g(d, ARITH(_,_,_,e)) = g(d,e)
        | g(d, PURE(_,_,_,e)) = g(d,e)
        | g(d, BRANCH(_,_,_,a,b)) = max(g(d,a), g(d,b))
        | g(d, APP(LABEL w, _)) = 
	         (case maxpath w
		   of {known=KNOWN, words_alloc=n} => 
		                if d+n > MAX_ALLOC
		                   then (Intmap.add m (w,{known=KNOWN_CHECK,
							  words_alloc=n});
					 d)
				   else d+n
		    | _ => d)
        | g(d, APP(_, _)) = d
        | g(d, FIX _) = ErrorMsg.impossible "8932 in limit"
      and maxpath w =
	   look w
	   handle Limit' =>
 	   (case escapes w
            of KNOWN => let val n = g(0, body w)
			    val z = if n>MAX_ALLOC
				      then {known=KNOWN_CHECK,words_alloc=n}
				      else {known=KNOWN,words_alloc=n}
		         in Intmap.add m (w,z);
			      z
                        end
	     | kind => let val z = (Intmap.add m (w,{known=kind,words_alloc=0});
				    {known=kind,words_alloc=g(0, body w)})
		        in Intmap.add m (w,z); z
		       end)
   in app (maxpath o #1) functions;
      Intmap.map m
  end
		         
fun nolimit functions =
  let val {escapes, check} = findescapes functions
      fun makenode (f,vl,body) =
	  let fun edges (RECORD(_,_,_,e)) = edges e
		| edges (SELECT(_,_,_,e)) = edges e
		| edges (OFFSET(_,_,_,e)) = edges e
		| edges (SWITCH(_,_,el)) = fold (op @) (map edges el) nil
		| edges (SETTER(_,_,e)) = edges e
		| edges (LOOKER(_,_,_,e)) = edges e
		| edges (ARITH(_,_,_,e)) = edges e
		| edges (PURE(_,_,_,e)) = edges e
		| edges (BRANCH(_,_,_,a,b)) = edges a @ edges b
                | edges (APP(LABEL w, _)) = (case escapes w of KNOWN => [w] 
		                                             | _ => nil)
                | edges (APP _) = nil
		| edges (FIX _) = ErrorMsg.impossible "8933 in limit"
	   in (f, edges body)
	  end
   in if !System.Control.CG.printit
         then (say "Starting feedback..."; System.Print.flush()) else ();
      app check (Feedback.feedback (map makenode functions));
      if !System.Control.CG.printit
         then (say "Finished\n"; System.Print.flush()) else ();
      path escapes functions
  end

val nolimit = fn functions =>
    if !System.Control.CG.printit
         then 
	     let val info = nolimit functions
		 fun showinfo (f,_,_) = 
		     (print(Access.lvarName f); say "\t";
		      case info f
			of {known=KNOWN,words_alloc=n} => 
			     (say "K  "; print(makestring n))
			 | {known=KNOWN_CHECK, words_alloc=n} =>
			     (say "C  "; print(makestring n))
			 | {known=ESCAPES, words_alloc=n} =>
			     (say "E  "; print(makestring n));
		      say "\n")
	      in app showinfo functions;
		 info
	     end
	 else nolimit functions

end





