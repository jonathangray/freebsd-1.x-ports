(* Copyright 1989 by AT&T Bell Laboratories *)
structure Expand :
	sig val expand : {function: CPS.function,
			  bodysize: int,
			  unroll: bool,
			  afterClosure: bool, do_headers: bool,
			  click: string->unit} -> CPS.function
        end =
struct

 open Access CPS
 structure CG = System.Control.CG

 fun map1 f (a,b) = (f a, b)

 fun sum f = let fun h [] = 0 
		   | h (a::r) = f a + h r
	     in h
	     end

 fun last0[x]=x | last0(a::b)=last0 b | last0 _ = 0
			  
 fun sameName(x,VAR y) = Access.sameName(x,y) 
   | sameName(x,LABEL y) = Access.sameName(x,y) 
   | sameName _ = ()

 datatype mode = ALL | NO_UNROLL | UNROLL of int | HEADERS

fun expand{function=(fvar,fargs,cexp),unroll,bodysize,click,afterClosure,
	   do_headers} =
 let
   val clicked_any = ref false
   val click = fn z => (click z; clicked_any := true)
   val debug = !CG.misc1 (* false *)
   val debugprint = if debug then System.Print.say else fn _ => ()
   val debugflush = if debug then System.Print.flush else fn _ => ()
   fun label v = if afterClosure then LABEL v else VAR v
   datatype info = Fun of {escape: int ref, call: int ref, size: int ref,
			   args: lvar list, body: cexp,
			   invariant: bool list ref, (* one for each arg *)
			   unroll_call: int ref, level: int,
			   within: bool ref}
		 | Arg of {escape: int ref, savings: int ref,
			   record: (int * lvar) list ref}
		 | Sel of {savings: int ref}
		 | Rec of {escape: int ref, size: int,
			   vars: (value * accesspath) list}
		 | Real
		 | Const
		 | Other

   exception Expand
   val m : info Intmap.intmap = Intmap.new(128,Expand)
   val note = Intmap.add m
   val get = Intmap.map m
   fun getval(VAR v) = get v
     | getval(LABEL v) = get v
     | getval(REAL _) = Other (*Real*)
     | getval(INT _) = Const
     | getval _ = Other
   fun call(v, args) = case getval v
			of Fun{call,within=ref false,...} => inc call
			 | Fun{call,within=ref true,unroll_call,
			       args=vl,invariant,...} => 
			     let fun g(VAR x :: args, x' :: vl, i::inv) =
				       (i andalso x=x') :: g(args,vl,inv)
				   | g( _ :: args, _ :: vl, i::inv) =
				       false :: g(args,vl,inv)
				   | g _ = nil
			      in inc call; inc unroll_call;
				 invariant := g(args,vl,!invariant)
			     end
			 | Arg{savings,...} => savings := !savings+1
			 | Sel{savings} => savings := !savings+1
			 | _ => ()
   fun escape v = case getval v
		   of Fun{escape,...} => inc escape
		    | Arg{escape,...} => inc escape
		    | Rec{escape,...} => inc escape
		    | _ => ()
   fun escapeargs v = case getval v
		       of Fun{escape,...} => inc escape
			| Arg{escape,savings, ...} =>
				   (inc escape; savings := !savings + 1)
			| Sel{savings} => savings := !savings + 1
			| Rec{escape,...} => inc escape
			| _ => ()
   fun unescapeargs v = case getval v
			 of Fun{escape,...} => dec escape
			  | Arg{escape,savings, ...} =>
				      (dec escape; savings := !savings - 1)
			  | Sel{savings} => savings := !savings - 1
			  | Rec{escape,...} => dec escape
			  | _ => ()
   fun setsize(f,n) = case get f of Fun{size,...} => (size := n; n)
   fun notearg v = (note (v,Arg{escape=ref 0,savings=ref 0, record=ref []}))
   fun notereal v = note (v,Other(*Real*))
   fun noteother v = note (v,Other)
   fun enter level (f,vl,e) = 
              (note(f,Fun{escape=ref 0, call=ref 0, size=ref 0,
			  args=vl, body=e, within=ref false,
			  unroll_call = ref 0, 
			  invariant = ref(map (fn _ => !CG.invariant) vl),
			  level=level});
	       app notearg vl)
   fun noterec(w, vl, size) = note (w,Rec{size=size,escape=ref 0,vars=vl})
   fun notesel(i,v,w) = (note (w, Sel{savings=ref 0});
			 case getval v
			  of Arg{savings,record,...} => (inc savings;
						  record := (i,w)::(!record))
			   | _ => ())

   fun save(v,k) = case getval v
		    of Arg{savings,...} => savings := !savings + k
		     | Sel{savings} => savings := !savings + k
		     | _ => ()
   fun nsave(v,k) = case getval v
		     of Arg{savings,...} => savings := k
		      | Sel{savings} => savings := k
		      | _ => ()
   fun savesofar v = case getval v 
		      of Arg{savings,...} => !savings
		       | Sel{savings} => !savings
		       | _ => 0

   fun within f func arg =
        case get f of Fun{within=w,...} => 
	    (w := true; func arg before (w := false))

   val rec prim = fn (level,vl,e) =>
       let fun vbl(VAR v) = (case get v of Rec _ => 0 | _ => 1)
	     | vbl _ = 0
	   val nonconst = sum vbl vl
	   val sl = map savesofar vl
	   val afterwards = pass1 level e
	   val zl = map savesofar vl
	   val overhead = length vl + 1
	   val potential = overhead
	   val savings = case nonconst of
			   1 => potential
			 | 2 => potential div 4
			 | _ => 0
	   fun app3 f = let fun loop (a::b,c::d,e::r) = (f(a,c,e); loop(b,d,r))
			      | loop _ = ()
			in loop
			end
       in app3(fn (v,s,z)=> nsave(v,s + savings + (z-s))) (vl,sl,zl);
	  overhead+afterwards
       end

   and primreal = fn (level,(_,vl,w,e)) =>
       (notereal w;
	app (fn v => save(v,1)) vl;
	2*(length vl + 1) + pass1 level e)

   and pass1 : int -> cexp -> int= fn level =>
    fn RECORD(_,vl,w,e) =>
	(app (escape o #1) vl;
	 noterec(w,vl,length vl);
	 2 + length vl + pass1 level e)
     | SELECT (i,v,w,e) => (notesel(i,v,w); 1 + pass1 level e)
     | OFFSET (i,v,w,e) => (noteother w; 1 + pass1 level e)
     | APP(f,vl) => (call(f,vl); 
		     app escapeargs vl; 
		     1 + ((length vl + 1) quot 2))
     | FIX(l, e) => 
	  (app (enter level) l; 
	   sum (fn (f,_,e) => setsize(f, within f (pass1 (level+1)) e)) l + length l + pass1 level e)
     | SWITCH(v,_,el) => let val len = length el
			     val jumps = 4 + len
			     val branches = sum (pass1 level) el
			  in save(v, (branches*(len-1)) quot len + jumps);
			     jumps+branches
			 end
     | BRANCH(_,vl,c,e1,e2) =>
       let fun vbl(VAR v) = (case get v of Rec _ => 0 | _ => 1)
	     | vbl _ = 0
	   val nonconst = sum vbl vl
	   val sl = map savesofar vl
	   val branches = pass1 level e1 + pass1 level e2
	   val zl = map savesofar vl
	   val overhead = length vl
	   val potential = overhead + branches quot 2
	   val savings = case nonconst of
			   1 => potential
			 | 2 => potential div 4
			 | _ => 0
	   fun app3 f = let fun loop (a::b,c::d,e::r) = (f(a,c,e); loop(b,d,r))
			      | loop _ = ()
			in loop
			end
       in app3(fn (v,s,z)=> nsave(v,s + savings + (z-s) quot 2)) (vl,sl,zl);
	  overhead+branches
       end
     | LOOKER(_,vl,w,e) => (noteother w; prim(level,vl,e))
     | SETTER(_,vl,e) => prim(level,vl,e)
     | ARITH(args as (P.floor,_,_,_)) => primreal (level,args)
     | ARITH(args as (P.round,_,_,_)) => primreal (level,args)
     | ARITH(args as (P.fadd,_,_,_)) => primreal (level,args)
     | ARITH(args as (P.fdiv,_,_,_)) => primreal (level,args)
     | ARITH(args as (P.fmul,_,_,_)) => primreal (level,args)
     | ARITH(args as (P.fsub,_,_,_)) => primreal (level,args)
     | ARITH(_,vl,w,e) => (noteother w; prim(level,vl,e))
     | PURE(P.fnegd,[v],w,e) => (notereal w; save(v,1); 4+(pass1 level e))
     | PURE(P.fabsd,[v],w,e) => (notereal w; save(v,1); 4+(pass1 level e))
     | PURE(P.real,vl,w,e) => (notereal w; prim(level,vl,e))
     | PURE(_,vl,w,e) => (noteother w; prim(level,vl,e))

   fun substitute(args,wl,e,level,alpha) =
    let exception Alpha
	val vm : value Intmap.intmap = Intmap.new(16, Alpha)
	fun use(v0 as VAR v) = (Intmap.map vm v handle Alpha => v0)
	  | use(v0 as LABEL v) = (Intmap.map vm v handle Alpha => v0)
	  | use x = x
	fun def v = if alpha
	             then let val w = dupLvar v 
			   in Intmap.add vm (v, VAR w); w
			  end
		     else v 
	fun defl v = if alpha
	             then let val w = dupLvar v 
			   in Intmap.add vm (v, label w);
			       w
			  end
		     else v
	fun bind(a::args,w::wl) = 
	       (sameName(w,a); Intmap.add vm (w,a); bind(args,wl))
	  | bind _ = ()
	val rec g =
       fn RECORD(k,vl,w,ce) => RECORD(k,map (map1 use) vl, def w, g ce)
	| SELECT(i,v,w,ce) => SELECT(i, use v, def w, g ce)
	| OFFSET(i,v,w,ce) => OFFSET(i, use v, def w, g ce)
	| APP(v,vl) => APP(use v, map use vl)
	| FIX(l,ce) => 
	  let fun h1(f,vl,e) = (f,defl f, vl, e)
	      fun h2(f,f',vl,e) =
		  let val vl' = map def vl
		      val e'= g e
		  in (f', vl', e')
		  end
	   in FIX(map h2(map h1 l), g ce)
	  end
	| SWITCH(v,c,l) => SWITCH(use v, def c, map g l)
	| LOOKER(i,vl,w,e) => LOOKER(i, map use vl, def w, g e)
	| ARITH(i,vl,w,e) => ARITH(i, map use vl, def w, g e)
	| PURE(i,vl,w,e) => PURE(i, map use vl, def w, g e)
	| SETTER(i,vl,e) => SETTER(i, map use vl, g e)
	| BRANCH(i,vl,c,e1,e2) => BRANCH(i, map use vl, def c, g e1, g e2)
    val cexp = (bind(args,wl); g e)
    in  (*debugprint("\nSize=" ^ makestring(pass1 level cexp)); debugprint " "; *)
	if alpha then pass1 level cexp else 0;
	cexp
    end

   fun whatsave(acc, size, (v:value)::vl, a::al) =
       if acc>=size
       then acc
       else
       (case get a of
	  Arg{escape=ref esc,savings=ref save,record=ref rl} =>
	  let val (this, nvl: value list, nal) =
	       case getval v
		of Fun{escape=ref 1,...} =>
			(if esc>0 then save else 6+save,vl,al)
		 | Fun _ => (save,vl,al)
		 | Rec{escape=ref ex,vars,size} =>
		      let exception Chase
			  fun chasepath(v,OFFp 0) = v
			    | chasepath(v, SELp(i,p)) =
			       (case getval v
				 of Rec{vars,...} =>
					chasepath(chasepath(nth(vars,i)),p)
				  | _ => raise Chase)
			    | chasepath _ = raise Chase
			  fun loop([],nvl,nal) = 
			      (if ex>1 orelse esc>0
			       then save
			       else save+size+2,nvl,nal)
			    | loop((i,w)::rl,nvl,nal) =
			       loop(rl,
				  chasepath(nth(vars,i))::nvl,
				    w::nal)
		       in loop(rl,vl,al)
			  handle Chase => (0,vl,al)
			       | Nth => (0,vl,al)
		      end 
		(* | Real => (save,vl,al)*)
		 | Const => (save,vl,al)
		 | _ => (0,vl,al)
	  in whatsave(acc+this - (acc*this) quot size, size, nvl,nal)
	  end
	| Sel{savings=ref save} =>
	  let val this =
	      case v
	       of VAR v' => (case get v' of
			      Fun _ => save
			    | Rec _ => save
			    | _ => 0)
		| _ => save
	  in whatsave(acc + this - (acc*this) quot size,size, vl,al)
	  end)
     | whatsave(acc,size,_,_) = acc

   fun beta(n, (* how many expansions we are within *)
	    d, (* path length from start of current function *)
	    u,  (* unroll-info *)
	    e (* expression to traverse *)
	    ) = case e
    of RECORD(k,vl,w,ce) => RECORD(k,vl, w, beta(n,d+2+length vl, u, ce))
     | SELECT(i,v,w,ce) => SELECT(i, v, w, beta(n,d+1, u, ce))
     | OFFSET(i,v,w,ce) => OFFSET(i, v, w, beta(n,d+1, u, ce))
     | APP(v,vl) => 
	 (case getval v
	   of info as Fun{args,body,...} =>
	       if should_expand(n,d,u,e,info)
		   then let val new = beta(n+1, d+1, u, 
				      substitute(vl,args,body,
						 case u of UNROLL lev => lev
						         | _ => 0,
						 true))
			in click "^";
			   case v of VAR vv => debugprint(makestring vv) | _ => ();
			   app unescapeargs vl;
			   new
			end
		    else e
	    | _ => e)
     | FIX(l,ce) => FIX(if n<1 then map (fundef(n,d,u)) l else l, 
			beta(n,d+length l, u,ce))
     | SWITCH(v,c,l) => SWITCH(v, c, map (fn e => beta(n,d+2,u,e)) l)
     | LOOKER(i,vl,w,e) => LOOKER(i, vl, w, beta(n,d+2,u,e))
     | ARITH(i,vl,w,e) => ARITH(i, vl, w, beta(n,d+2,u,e))
     | PURE(i,vl,w,e) => PURE(i, vl, w, beta(n,d+2,u,e))
     | SETTER(i,vl,e) => SETTER(i, vl, beta(n,d+2,u,e))
     | BRANCH(i,vl,c,e1,e2) => BRANCH(i, vl, c,beta(n,d+2,u,e1), 
				      beta(n,d+2,u,e2))

    and should_expand(n,d,HEADERS,e,_) = false
      | should_expand(n,d,u,e as APP(v,vl), 
		      Fun{escape,call,unroll_call,size=ref size,args,body,
			  level,within=ref within,...}) =
      let val stupidloop =  (* prevent infinite loops  at compile time *)
	    case (v,body) 
	     of (VAR vv, APP(VAR v',_)) => vv=v' 
	      | (LABEL vv, APP(LABEL v',_)) => vv=v' 
	      | _ => false
	val calls = case u of UNROLL _ => !unroll_call | _ => !call
	val small_fun_size = case u of UNROLL _ => 0 | _ => 50
	val savings = whatsave(0,size,vl,args)
	val predicted = 
	    let val real_increase = size-savings-(1+length vl)
	    in  real_increase * calls - 
		(* don't subtract off the original body if
		   the original body is huge (because we might
		   have guessed wrong and the consequences are
		   too nasty for big functions); or if we're
		   in unroll mode *)
		(if size < small_fun_size then size else 0)
	    end
	val depth = 2 and max = 2
	val increase = (bodysize*(depth - n)) quot depth

    in if false andalso debug
	  then (CPSprint.show System.Print.say e;
		debugprint(makestring predicted); debugprint "   "; 
		debugprint(makestring increase);
		debugprint"   "; debugprint (makestring n); debugprint "\n")
	 else ();

       not stupidloop
       andalso case u
	    of UNROLL lev => 
		 (* Unroll if: the loop body doesn't make function
		    calls orelse "unroll_recur" is turned on; andalso 
		    we are within the definition of the function; 
		    andalso it looks like things won't grow too much.
		  *)
		   (!CG.unroll_recur orelse level >= lev)
		   andalso n <= max
		   andalso within andalso predicted <= increase
	     | NO_UNROLL =>
		   !unroll_call = 0 andalso
		   not within andalso n <= max andalso
		   (predicted <= increase  
		     orelse (!escape=0 andalso calls = 1))
	     | HEADERS => false
	     | ALL => n <= max andalso
		   (predicted <= increase  
		     orelse (!escape=0 andalso calls = 1))
  end

   and fundef (n,d,HEADERS) (f,vl,e) = 
    let val Fun{within,escape=ref escape,call,unroll_call,
		invariant=ref inv,...} = get f

     in within := true;
	(if  escape = 0 andalso !unroll_call > 0
	     andalso (!call - !unroll_call > 1 orelse exists (fn t=>t) inv)
	 then let val f'::vl' = map dupLvar (f::vl)
		  val within' = ref true
		  fun drop(false::r,a::s) = a::drop(r,s)
		    | drop(true::r,_::s) = drop(r,s)
		    | drop _ = nil
		  val e' =substitute(label f' :: map VAR (drop(inv,vl')),
				     f :: drop(inv,vl),
				     beta(n,0,HEADERS,e),
				     0, false) 
	       in click "!"; debugprint(makestring f);
		   
		  enter 0 (f',vl',e');
		  (f,vl,FIX([(f',vl',e')], APP(label f', map VAR vl)))
	     end
	else (f,vl,beta(n,0,HEADERS,e)))

        before within := false
   end

    | fundef (n,d,u) (f,vl,e) = 
    let val Fun{level,within,escape=ref escape,...} = get f

	val u' = case u of UNROLL _ => UNROLL level | _ => u

    in if (case e
	    of FIX([(g,[b,k],APP _)], APP(VAR c,[VAR g'])) =>
	           c=last0 vl andalso g=g'
	     | APP _ => escape > 0
	     | _ => false)
        then (f,vl,e) (* Don't contract eta-splits *) 
	else (within := true; (f,vl,beta(n,0,u',e)) before within := false)
   end

  in notearg fvar; app notearg fargs;
(*     if !CG.printit then CPSprint.show System.Print.say cexp
	 else ();
*)     debugprint("\nExpand   "); debugprint(makestring(pass1 0 cexp));
     if unroll
	 then let val _ = (debugprint("  (unroll)\n"); debugflush());
		  val e' = beta(0,0,UNROLL 0,cexp)
	      in if !clicked_any 
		     then expand{function=(fvar, fargs, e'),
				 bodysize=bodysize,click=click,unroll=unroll,
				 afterClosure=afterClosure,
				 do_headers=do_headers}
		 else ((*debugprint("\nExpand\n"); 
		         debugflush();
		       (fvar, fargs, beta(0,0,ALL,cexp)) *)
		       (fvar, fargs, e'))
	      end
     else if !CG.unroll
	 then let val _ = (debugprint(" (headers)\n"); debugflush())
		  val e' = if do_headers then beta(0,0,HEADERS,cexp) else cexp
	       in if !clicked_any
		  then expand{function=(fvar,fargs,e'),
			      bodysize=bodysize,click=click,unroll=unroll,
			      afterClosure=afterClosure, do_headers=false}
		  else (debugprint("\n  (non-unroll)\n"); debugflush();
			(fvar, fargs, beta(0,0,NO_UNROLL,e')))
	      end
     else (debugprint("\n"); debugflush();
	       (fvar, fargs, beta(0,0,ALL,cexp)))
     
 end

end
