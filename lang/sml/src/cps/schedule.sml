(* schedule.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories
 *)

structure Schedule : sig
    val schedule: CPS.function -> CPS.function
  end = struct

open CPS Access
val uniq = SortedList.uniq

val say = System.Print.say

fun sublist test =
  let fun subl(a::r) = if test a then a::(subl r) else subl r
        | subl [] = []
  in  subl
  end

fun member l = let fun f(i,j::rest) = i=j orelse f(i,rest) | f(_,nil) = false
                in fn i => f(i,l)
	       end

fun sum f (i::r) = f i + sum f r | sum f nil = 0

fun all pred = 
      let fun f nil = true | f(i::rest) = pred i andalso f rest in f end

type child = {path: int list,
	      branches: int,
	      uses: lvar list,
	      defs: lvar list,
	      make: cexp list -> cexp}

fun show nil = say "."
  | show (v::vl) = (say(makestring(v:int)); say ","; show vl)

fun showl nil = say "]"
  | showl [x] = (say(makestring(x:int)); say "]")
  | showl (x::rest) = (say(makestring(x:int)); say "."; showl rest)
		      

fun showc (({defs=nil,path,...}:child)::vl) =
      (say "["; showl path; say "branch,"; showc vl)
  | showc ({defs=w::_,path,...}::vl) =
      (say "["; showl path; say(makestring w); say ","; showc vl)
  | showc nil = say "."

fun schedule(f,vl,cexp) =
let 
    val _ = if !System.Control.CG.printit
	      then(say "\nBefore:\n\n"; CPSprint.show say cexp)
	      else ()
	    
    exception Schedmap
    val m : child list Intmap.intmap = Intmap.new(32,Schedmap)
    val addm = Intmap.add m
    val getm = Intmap.map m

    fun def w = addm(w, nil)

    val mem0 = mkLvar()
    val done0 = mem0::vl
    val _ = app def done0

    val biglist : child list ref = ref nil

    fun enter(path,mem,e) = 
	let 
	    fun vars(VAR v :: rest) = v::(vars rest)
	      | vars(_ :: rest) = vars rest
	      | vars nil = nil

            fun note(child as {path,branches,uses,defs,make}) =
		    (app (fn v => addm(v, child :: getm v)) uses;
		     app def defs;
		     biglist := child :: !biglist)

	    fun usedef(uses,defs,c,make) = 
		(note{path=path,branches=1,uses= uniq(vars uses),
		      defs=defs,make=make};
		 enter(path,mem,c))
   
	    fun storeop (p,args,res,[c]) =
		 let val m = mkLvar()
		 in note{path=path,branches=1,
                         uses= uniq(vars(VAR mem :: args)),
			 defs=res@[m], make=fn cl=>PRIMOP(p,args,res,cl)};
		    enter(path,m,c)
		 end

	    fun fetchop(p,args,res,[c]) = 
		  usedef(VAR(mem)::args, res, c, fn cl=>PRIMOP(p,args,res,cl))

	    fun branch(vl,cl,make) =
		let fun f(i,c::cl) = (enter(path@[i],mem,c); f(i+1, cl))
	                | f _ = ()
		in note{path=path,branches=length cl,uses= uniq(vars vl),
			defs=[], make=make};
		   f(0,cl)
                end

	 in case e 
	     of SELECT(i,v,w,c) => usedef([v],[w],c, fn[c]=>SELECT(i,v,w,c))
	      | OFFSET(i,v,w,c) => usedef([v],[w],c, fn[c]=>OFFSET(i,v,w,c))
	      | RECORD(k,l,w,c) => usedef(map #1 l, [w], c, fn[c]=>RECORD(k,l,w,c))
	      | APP(v,vl) => branch(VAR(mem)::v::vl, nil, fn _ => e)
              | PRIMOP(args as (P.store,_,_,_)) => storeop args
              | PRIMOP(args as (P.update,_,_,_)) => storeop args
              | PRIMOP(args as (P.boxedupdate,_,_,_)) => storeop args
              | PRIMOP(args as (P.unboxedupdate,_,_,_)) => storeop args
              | PRIMOP(args as (P.sethdlr,_,_,_)) => storeop args
		         (* the following are storeops because they
			    may raise exceptions.  How annoying. *)
              | PRIMOP(args as (P.*,_,_,_)) => storeop args
              | PRIMOP(args as (P.+,_,_,_)) => storeop args
              | PRIMOP(args as (P.-,_,_,_)) => storeop args
              | PRIMOP(args as (P.div,_,_,_)) => storeop args
              | PRIMOP(args as (P.fadd,_,_,_)) => storeop args

(* still to implement:  
      floor | round | real | subscriptf | updatef | unboxed lessu gequ *)
              | PRIMOP(args as (P.fdiv,_,_,_)) => storeop args
              | PRIMOP(args as (P.fmul,_,_,_)) => storeop args
              | PRIMOP(args as (P.fsub,_,_,_)) => storeop args
              | PRIMOP(args as (P.~,_,_,_)) => storeop args

	      | PRIMOP(args as (P.!, _,_,_)) => fetchop args
	      | PRIMOP(args as (P.ordof, _,_,_)) => fetchop args
	      | PRIMOP(args as (P.subscript, _,_,_)) => fetchop args
	      | PRIMOP(args as (P.gethdlr, _,_,_)) => fetchop args
	      | PRIMOP(p,vl,res,[c]) => usedef(vl,res,c, 
					       fn cl=>PRIMOP(p,vl,res,cl))
	      | SWITCH(v,cl) => branch([v],cl, fn cl=>SWITCH(v,cl))
	      | PRIMOP(p,vl,[],cl) => branch(vl,cl, fn cl=>PRIMOP(p,vl,[],cl))
	      | PRIMOP(_,vl,_,_) => ErrorMsg.impossible "8223 in schedule"
	      | _ => ErrorMsg.impossible "8224 in schedule"
	 end

     val _ = enter(nil,mem0,cexp)

   fun prefix(nil,_) = true
     | prefix((i:int)::i', j::j') = i=j andalso prefix(i',j')
     | prefix _ = false

   fun proper_prefix(nil,_::_) = true
     | proper_prefix((i:int)::i', j::j') = i=j andalso proper_prefix(i',j')
     | proper_prefix _ = false

     fun isready done ({uses,...}:child) = all (member done) uses
     fun newly_ready(done,nil) = nil
       | newly_ready(done,w::rest) =
	          sublist (isready(w::done)) (getm w) @ 
		  newly_ready(w::done, rest)

     fun sched(path, done, ready) =
      let 
	  val _ = if !System.Control.CG.printit
	           then (say "\nDone= "; show done;
		   say "\nReady = "; showc ready) else ()
		    
	  fun howgood {path=p0,branches,uses,defs,make} =
	      let fun f v = let fun count(({defs=nil,...}:child)::rest) = 
		                        count rest
				  | count({path=p,defs as w::_,...}::rest) = 
				    if prefix(path,p)
					     andalso not(member done w) 
					then 1 + count rest
				        else count rest
				  | count nil = 0
			    in 100 quot count(getm v) handle Div => 100
			    end
	      in if prefix(p0,path)
		   then sum f uses - 100*length defs
		       - (if branches=1 then 0 else 1000)
		   else ~10000
	      end

          fun bestl(~10000, choice, nil,others) = 
	          ErrorMsg.impossible "765 in schedule"
	    | bestl(goodness, choice, nil,others) = (choice,others)
	    | bestl(g, ch, r::rest, others) = 
	      let val g' = howgood r
	       in if g'>g then bestl(g',r,rest,ch::others)
		          else bestl(g,ch,rest,r::others)
	      end
	  fun best(a::rest) = bestl(howgood a, a, rest, nil)
	    | best nil = ErrorMsg.impossible "1234 in schedule"

       in case best ready
	   of ({defs, branches=1, make,...}, rest) =>
	          make[sched(path, defs@done, newly_ready(done,defs) @ rest)]
	    | ({defs=nil, branches, make,...}, rest) =>
		  let fun f i = if i<branches 
				    then sched(path@[i],done,rest)::f(i+1)
				    else nil
		   in make(f 0)
		  end
	    | _ => ErrorMsg.impossible "1133 in schedule"
      end
	        
    val cexp = sched(nil, done0, sublist (isready done0) (!biglist))
    val _ = if !System.Control.CG.printit
	      then(say "\nAfter:\n\n"; CPSprint.show say cexp)
	      else ()
  in (f,vl,cexp)
 end

end
