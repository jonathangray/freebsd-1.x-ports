(* Copyright 1989 by AT&T Bell Laboratories *)
signature SPILL =
  sig val spill : CPS.function list -> CPS.function list
  end

functor Spill(val maxfree : int) : SPILL =
struct

structure SortedList =
(* this structure is duplicated here for efficiency *)
struct

fun enter(new:int,l) =
  let fun f [] = [new]
	| f (l as h::t) = if new<h then new::l else if new>h then h::f t else l
  in  f l
  end

fun uniq l =
    let fun loop([],acc) = acc
	  | loop(a::r,acc) = loop(r,enter(a,acc))
    in loop(l,[])
    end

fun merge(a,[]) = a
  | merge([],a) = a
  | merge(l as (i:int)::a, m as j::b) = 
      if j<i then j::merge(l,b) else i::merge(a,if i<j then m else b)

local fun loop (a::b::rest) = loop(merge(a,b)::loop rest)
        | loop l = l
in fun foldmerge l = hd(loop l) handle Hd => []
end

fun remove(x as (xl:int)::xr, y as yl::yr) =
    if xl>yl then yl::remove(x,yr) else remove(xr,if xl<yl then y else yr)
  | remove(_,y) = y

fun rmv (x : int,l) =
    let fun loop nil = nil
	  | loop (a::b) = if x=a then b else a::loop b
    in loop l
    end

fun member l (e:int) =
  let fun f [] = false
	| f (h::t) = if h<e then f t else e=h
  in  f l
  end

fun intersect(nil,_) = nil
  | intersect(_,nil) = nil
  | intersect(l as (a:int)::b,r as c::d) =
	if a=c then a::intersect(b,d)
	else if a<c then intersect(b,r)
	else intersect(l,d)

fun difference(nil,_) = nil
  | difference(l,nil) = l
  | difference(l as (a:int)::b,r as c::d) =
	if a=c then difference(b,d)
	else if a<c then a::difference(b,r)
	else difference(l,d)	
end


open Access SortedList CPS
val error = ErrorMsg.impossible
fun sublist test =
  let fun subl(a::r) = if test a then a::(subl r) else subl r
        | subl [] = []
  in  subl
  end

local val spillname = Symbol.varSymbol "spillrec"
in    fun spillLvar() = namedLvar spillname
end


val pr = System.Print.say
val ilist = PrintUtil.prIntPath

fun sayv(VAR v) = pr(Access.lvarName v)
  | sayv(LABEL v) = pr("(L)" ^ Access.lvarName v)
  | sayv(INT i) = pr(makestring i)
  | sayv(REAL r) = pr r
  | sayv(STRING s) = (pr "\""; pr s; pr "\"")
val vallist = PrintUtil.printClosedSequence("[",",","]") sayv

fun cut(0,_) = []
  | cut(i,a::x) = a::cut(i-1,x)
  | cut(_,[]) = []

fun nextuse x =
 let val infinity = 1000000
     fun xin[] = false | xin(VAR y::r) = x=y orelse xin r | xin(_::r) = xin r
     fun g(level,a) =
     let val rec f =
      fn ([],[]) => infinity
       | ([],next) => g(level+1,next)
       | (SWITCH(v,c,l)::r,next) => if xin[v] then level else f(r,l@next)
       | (RECORD(_,l,w,c)::r,next) =>
	 if xin(map #1 l) then level else f(r,c::next)
       | (SELECT(i,v,w,c)::r,next) => if xin[v] then level else f(r,c::next)
       | (OFFSET(i,v,w,c)::r,next) => if xin[v] then level else f(r,c::next)
       | (SETTER(i,a,c)::r,next) => if xin a then level else f(r,c::next)
       | (LOOKER(i,a,w,c)::r,next) => if xin a then level else f(r,c::next)
       | (ARITH(i,a,w,c)::r,next) => if xin a then level else f(r,c::next)
       | (PURE(i,a,w,c)::r,next) => if xin a then level else f(r,c::next)
       | (BRANCH(i,a,c,e1,e2)::r,next) => 
			   if xin a then level else f(r,e1::e2::next)
       | (APP(v,vl)::r,next) => if xin(v::vl) then level else f(r,next)
     in f(a,[])
     end
     fun h y = g(0,[y])
 in h
 end

local val sort = Sort.sort (fn ((i:int,_),(j,_)) => i>j)
in fun sortdups(cexp,dups) =
       map #2 (sort (map (fn dup as (v,w) => (nextuse v cexp, dup)) dups))
end

(* should do the first n and then only go 
   deep enough to prove that it is not needed *)

fun next_n_dups(0,cexp,dups) = []
  | next_n_dups(n,cexp,dups) =
    if n >= length dups
    then dups
    else cut(n,sortdups(cexp,dups))

fun show (SWITCH(v,c,l)) = (pr "SWITCH ["; (pr o makestring) c; pr "]\n")
  | show (RECORD(_,_,w,_)) = (pr "RECORD "; (pr o makestring) w; pr "\n")
  | show (SELECT(_,_,w,_)) = (pr "SELECT "; (pr o makestring) w; pr "\n")
  | show (OFFSET(_,_,w,_)) = (pr "OFFSET "; (pr o makestring) w; pr "\n")
  | show (LOOKER(_,_,w,_)) = (pr "LOOKER "; (pr o makestring) w; pr "\n")
  | show (SETTER(_,_,_)) = (pr "SETTER "; pr "\n")
  | show (ARITH(_,_,w,_)) = (pr "LOOKER "; (pr o makestring) w; pr "\n")
  | show (PURE(_,_,w,_)) = (pr "LOOKER "; (pr o makestring) w; pr "\n")
  | show (BRANCH(_,_,c,_,_)) = (pr "BRANCH "; (pr o makestring) c; pr "\n")
  | show (APP(f,vl)) = (pr "APP "; sayv f; vallist vl; pr "\n")

    local fun vars(l, VAR x :: rest) = vars(x::l, rest)
            | vars(l, _::rest) = vars(l,rest)
            | vars(l, nil) = uniq l
       in fun clean l = vars(nil, l)
      end

nonfix before
val \/ = merge and /\ = intersect
infix 6 \/   infix 7 /\

local exception TooMany
      fun checklen(w,l) = if length l >= maxfree then raise TooMany else ()
in fun check body = (FreeMap.freemap checklen body; true)
                    handle TooMany => false
end

fun spillit (func,vl,body) =
let 
    val freevars = 
	let exception SpillFreemap
	    val m = Intmap.new(32, SpillFreemap) : lvar list Intmap.intmap
	    val _ = FreeMap.freemap (Intmap.add m) body
         in fn x => ((Intmap.map m x) handle SpillFreemap => 
                      (pr "&&&&&&&&  "; (pr o makestring) x; pr "  \n";
                       raise SpillFreemap))
        end

  fun f(results : lvar list,
	  uniques : lvar list,
          dups : (lvar*lvar) list,
 	  spill : (lvar list * value) option,
	  cexp : cexp) =
    let val (before,after) =  (* variables free in this operation, and after
	  			  not including the newly-bound variables *)
	 let val rec free =
	      fn SWITCH(v,_,l) => foldmerge(clean[v] :: map free l)
	       | RECORD(_,l,w,c) =>  clean (map #1 l) \/ freevars w
	       | SELECT(i,v,w,c) => clean[v] \/ freevars w
	       | OFFSET(i,v,w,c) => clean[v] \/ freevars w
	       | SETTER(i,vl,c) => clean vl \/ free c
	       | LOOKER(i,vl,w,c) => clean vl \/ freevars w
	       | ARITH(i,vl,w,c) => clean vl \/ freevars w
	       | PURE(i,vl,w,c) => clean vl \/ freevars w
	       | BRANCH(i,vl,c,c1,c2) => clean vl \/ free c1 \/ free c2
	       | APP(f,vl) => clean(f::vl)
	   in case cexp
	      of SWITCH(v,_,l) => (clean[v], foldmerge(map free l))
	       | RECORD(_,l,w,c) =>  (clean(map #1 l), freevars w)
	       | SELECT(i,v,w,c) => (clean[v], freevars w)
	       | OFFSET(i,v,w,c) => (clean[v], freevars w)
	       | SETTER(i,vl,c) => (clean vl, free c)
	       | LOOKER(i,vl,w,c) => (clean vl, freevars w)
	       | ARITH(i,vl,w,c) => (clean vl, freevars w)
	       | PURE(i,vl,w,c) => (clean vl, freevars w)
	       | BRANCH(i,vl,c,c1,c2) => (clean vl, free c1 \/ free c2)
	       | APP(f,vl) => (clean(f::vl), [])
	 end

        val uniques = uniques \/ results (* is this line necessary? *)
	val uniques_after = uniques /\ after
        val uniques_before = (uniques /\ before) \/ uniques_after
        val spill_after = 
	    case spill of
	      NONE => NONE
	    | SOME(contents,_) =>
	      case uniq contents /\ after of
	        [] => NONE
	      | _ => spill
	val maxfree' = case spill of NONE => maxfree | SOME _ => maxfree-1
	val maxfreeafter = case spill_after of
			     NONE => maxfree | SOME _ => maxfree-1
	val avail = maxfree' - length(uniques_before \/ results)
	val dups = next_n_dups(avail,cexp,dups)

        fun getpath p (VAR v) =
	  if member uniques_before v
	  then (VAR v, OFFp 0)
	  else let fun try((w,x)::l) = if v=w then (VAR x, OFFp 0) else try l
		     | try [] = let val SOME (l,sv) = spill
			            fun find(i,w::l) = 
				        if v=w
				        then (sv, SELp(i,OFFp 0))
				        else find(i+1,l)
				      | find(_,[]) = error "not found in spill"
			        in find(0,l)
			        end handle Bind => error "2332 in spill"
	       in try dups
	       end
	  | getpath _ x = (x, OFFp 0)

	fun makeSpillRec args =
	    let val contents = clean args \/ after
	        val spillrec = map (getpath true o VAR) contents
		val sv = spillLvar()
(*		val cexp = if not(!System.Control.CG.allocprof) then cexp
			   else AllocProf.profSpill(length contents) cexp *)
		val dups' = map (fn x => (x,x)) uniques_before @ dups
 	     in inc System.Control.CG.spillGen;
		RECORD(RK_SPILL,spillrec,sv,
                  let val ce = f([],[],dups',SOME(contents, VAR sv),cexp)
                   in if not(!System.Control.CG.allocprof) then ce
                      else AllocProf.profSpill (length contents) ce
                  end)
	    end

        fun g(args,res,conts,gen) = 
	if length(clean args \/ uniques_after) > maxfreeafter orelse
	   length res + length uniques_after > maxfreeafter
	then makeSpillRec args
	else let val paths = map (fn x => (x, getpath false (VAR x))) (clean args)
		 fun fetchit (_,(_,OFFp 0)) = false | fetchit _ = true
	     in case sublist fetchit paths of
		  [(v,(w,SELp(i,OFFp 0)))] =>
		  let val x = dupLvar v
 		  in (* pr "Fetching ";
		     (pr o makestring) v;
		     pr "\n"; *)
		     SELECT(i,w,x,f([],uniques_before,(v,x)::dups,
		     	            spill_after,cexp))
		  end
		| (v,(w,SELp(i,OFFp 0)))::_ =>
		  let val x = dupLvar v
 		  in (* pr "fetching ";
		     (pr o makestring) v;
		     pr "\n"; *)
		     SELECT(i,w,x,f([],uniques_before,(v,x)::dups,spill,cexp))
		  end
	        | [] => let fun f' cexp = f(uniq res,uniques_after,
					    dups,spill_after,cexp)
		        in gen(map (#1 o (getpath false)) args,res,map f' conts)
			end
	     end

     in case ((*show cexp;*) cexp)
         of SWITCH(v,c,l) => g([v],[],l,fn([v],[],l)=>SWITCH(v,c,l))
          | RECORD(k,l,w,c) =>
	    if 1+length uniques_after > maxfreeafter
	    then makeSpillRec (map #1 l)
	    else let val paths = map (fn (v,p) =>
					 let val (v',p') = getpath true v 
					 in (v', combinepaths(p',p))
					 end)
	                             l
	         in RECORD(k,paths,w,f([w],uniques_after,dups,spill_after,c))
		 end
          | SELECT(i,v,w,c) => g([v],[w],[c], fn([v],[w],[c])=>SELECT(i,v,w,c))
	  | OFFSET(i,v,w,c) => g([v],[w],[c], fn([v],[w],[c])=>OFFSET(i,v,w,c))
	  | SETTER(i,vl,c) => g(vl,[],[c], fn(vl,_,[c])=>SETTER(i,vl,c))
	  | LOOKER(i,vl,w,c) => g(vl,[w],[c], fn(vl,[w],[c])=>LOOKER(i,vl,w,c))
	  | ARITH(i,vl,w,c) => g(vl,[w],[c], fn(vl,[w],[c])=>ARITH(i,vl,w,c))
	  | PURE(i,vl,w,c) => g(vl,[w],[c], fn(vl,[w],[c])=>PURE(i,vl,w,c))
	  | BRANCH(i,vl,c,c1,c2) => 
		 g(vl,[],[c1,c2], fn(vl,_,[c1,c2])=>BRANCH(i,vl,c,c1,c2))
	  | APP(f,vl) => g(f::vl,[],[],fn(f::vl,[],[])=>APP(f,vl))
   end

in (func,vl,f([],uniq vl, [],NONE,body))
end

fun improve cexp = 
   let exception Spillmap
(*       val _ = if !System.Control.CG.misc1 then CPSprint.show System.Print.say cexp else () *)
       val m : (int ref*int*value) Intmap.intmap = Intmap.new(32,Spillmap)
       val enter = Intmap.add m
       fun get(VAR x) = (SOME(Intmap.map m x) handle Spillmap => NONE)
	 | get _ = NONE
       fun kill(VAR v) = Intmap.rmv m v
	 | kill _ = ()
       fun use v =  case get v
		     of SOME(r as ref 0,i,w) => r := 1
		      | SOME _ => kill v
		      | NONE => ()
       val rec pass1 =
	fn SELECT(i,v,w,e) => (kill v; enter(w,(ref 0,i,v)); pass1 e)
	 | OFFSET(i,v,w,e) => (kill v; pass1 e)
	 | RECORD(_,vl,w,e) => (app (use o #1) vl; pass1 e)
	 | APP(v,vl) => (kill v; app kill vl)
	 | FIX(l,e) => error "33832 in spill"
	 | SWITCH(v,_,el) => (kill v; app pass1 el)
	 | BRANCH(i,vl,c,e1,e2) => (app kill vl; pass1 e1; pass1 e2)
	 | SETTER(i,vl,e) => (app kill vl; pass1 e)
	 | LOOKER(i,vl,w,e) => (app kill vl; pass1 e)
	 | ARITH(i,vl,w,e) => (app kill vl; pass1 e)
	 | PURE(i,vl,w,e) => (app kill vl; pass1 e)
       fun ren(v,p) = case get v
		       of SOME(_,i,w) => (w,SELp(i,p))
			| NONE => (v,p)
       val rec g =
	fn SELECT(i,v,w,e) => (case get(VAR w) 
				of SOME _ => g e
				 | NONE => SELECT(i,v,w, g e))
	 | OFFSET(i,v,w,e) => OFFSET(i,v,w, g e)
	 | RECORD(k,vl,w,e) => RECORD(k,map ren vl, w, g e)
	 | e as APP(v,vl) => e
	 | FIX(l,e) => error "33832 in spill"
	 | SWITCH(v,c,el) => SWITCH(v,c,map g el)
	 | BRANCH(i,vl,c,e1,e2) => BRANCH(i,vl,c, g e1, g e2)
	 | SETTER(i,vl,e) => SETTER(i,vl, g e)
	 | LOOKER(i,vl,w,e) => LOOKER(i,vl,w, g e)
	 | ARITH(i,vl,w,e) => ARITH(i,vl,w, g e)
	 | PURE(i,vl,w,e) => PURE(i,vl,w, g e)
      val count = (pass1 cexp; length(Intmap.intMapToList m))
   in if !System.Control.CG.misc1 then (pr "count="; (pr o makestring) count;
					pr "\n") else ();
      if count>0 then SOME(g cexp) else NONE
  end		

fun spill1 (arg as (func,vl,body)) = 
	if check body then arg
	else case improve body
	      of SOME body' => spill1(func,vl,body')
	       | NONE => spillit(func,vl,body)

val spill = map spill1

end (* structure Spill *)
