(****************************************************************************
 *                                                                          *
 * NOTE: debugging code has been commented out as follows:                  *
 *           (***> ...code... <***)                                         *
 *                                                                          *
 ****************************************************************************)

signature CLOSURE =
  sig
    val closeCPS : CPS.function -> CPS.function
  end

functor Closure(val maxfree : int) : CLOSURE =
struct

open CPS Access AllocProf SortedList
structure CGoptions = System.Control.CG

val OFFp0 = OFFp 0

(**********************************************************************
 * Miscellaneous utility functions.                                   *
 *                                                                    *
 * NOTE: An important property of partition and sublist is that they  *
 *  take sorted lists to sorted lists.                                *
 **********************************************************************)
fun partition f l = fold (fn (e,(a,b)) => if f e then (e::a,b) else (a,e::b))
			 l (nil,nil)

fun sublist test =
  let fun subl(a::r) = if test a then a::(subl r) else subl r
        | subl nil = nil
  in  subl
  end

fun formap f =
  let fun iter(nil,_) = nil
	| iter(hd::tl,i) = f(hd,i)::iter(tl,i+1)
  in  iter o (fn l => (l,0))
  end

val error = ErrorMsg.impossible

local val save = (!saveLvarNames before saveLvarNames := true)
      val closure = namedLvar(Symbol.varSymbol "closure")
in    val closureLvar = (saveLvarNames := save; fn () => dupLvar closure)
end


(**********************************************************************
 * Static environments.                                               *
 **********************************************************************)
datatype closureRep = CR of int * {functions : (lvar * lvar) list,
				   values : lvar list,
				   closures : (lvar * closureRep) list,
				   stamp : lvar}
datatype object = Value
		| Function of {label:lvar,free:lvar list}
		| Closure of closureRep
                | Callee of value * value list
datatype access = Direct
		| Path of (lvar * accesspath)

abstype env = Env of lvar list *                    (* values *)
	             (lvar * closureRep) list *     (* closures *)
                     object Intmap.intmap           (* what map *)
with
exception NotBound
fun emptyEnv() = Env(nil,nil,Intmap.new(32,NotBound))

(* Update an environment *)
fun augment(m as (v,obj),e as Env(valueL,closureL,whatMap)) =
      (Intmap.add whatMap m;
       case obj
         of Value => Env(v::valueL,closureL,whatMap)
          | Closure cr => Env(valueL,(v,cr)::closureL,whatMap)
          | _ => e)
fun augmentV(v,Env(valueL,closureL,whatMap)) =
      (Intmap.add whatMap(v,Value);
       Env(v::valueL,closureL,whatMap))
fun addCallee(Env(_,_,whatMap),(v,c)) = Intmap.add whatMap (v,Callee c)

(* Return the immediately enclosing closure, if any.  This is a hack. *)
fun getClosure (Env(_,closureL,_)) =
  let fun getc ([z]) = SOME z
	| getc (_::tl) = getc tl
	| getc nil = NONE
  in  getc closureL
  end

(* Return all the closures currently in the environment. *)
fun getClosures (Env(_,closureL,_)) = closureL



(**********************************************************************
 * Environment printing, for debugging.                               *
 **********************************************************************)
val pr = System.Print.say
val vp = pr o Access.lvarName
fun plist p l = (app (fn v => (pr " "; p v)) l; pr "\n")
val ilist = plist vp
fun sayv(VAR v) = pr(Access.lvarName v)
  | sayv(LABEL v) = (pr "(L)"; pr(Access.lvarName v))
  | sayv(INT i) = (pr "(I)"; pr(makestring i))
  | sayv(REAL r) = pr r
  | sayv(STRING s) = (pr "\""; pr s; pr "\"")
  | sayv(OBJECT _) = pr "**OBJECT**"
val vallist = plist sayv

fun printEnv(Env(valueL,closureL,whatMap)) =
  let fun ip i = pr(Integer.makestring i)
      val tlist = plist (fn (a,b) => (vp a; pr "/"; sayv(LABEL b)))
      fun fp(v,Function{label,free}) =
	(vp v; pr "/known "; sayv(LABEL label); pr " -"; ilist free)
        | fp _ = ()
      fun cp (v,Callee(v',vl)) =
	(vp v; pr "/callee "; sayv v'; pr " -"; vallist vl)
        | cp _ = ()
      fun p(indent,l,seen) =
	let fun c(v,CR(offset,{functions,values,closures,stamp})) =
	      (indent(); pr "Closure "; vp v; pr "/"; ip stamp;
	       pr " @"; ip offset;
	       if member seen stamp
	       then pr "(seen)\n"
	       else (pr ":\n";
		     case functions
		       of nil => ()
		        | _ => (indent(); pr "  Funs:"; tlist functions);
		     case values
		       of nil => ()
		        | _ => (indent(); pr "  Vals:"; ilist values);
		     p(fn() => (indent();pr "  "),closures,enter(stamp,seen))))
	in  app c l
	end
  in  pr "Values:"; ilist valueL;
      pr "Closures:\n"; p(fn () => (),closureL,nil);
      pr "Known function mapping:\n"; Intmap.app fp whatMap;
      pr "Callee-save continuation mapping:\n";
      Intmap.app cp whatMap
  end


exception Lookup of lvar * env
(************************************************************************
 * whatIs: return type of object bound to an lvar in an environment.    *
 ************************************************************************)
fun whatIs(env as Env(_,_,whatMap),v) =
  Intmap.map whatMap v
    handle NotBound => raise Lookup(v,env)

(************************************************************************
 * whereIs: find the access path to a value in an environment.          *
 ************************************************************************)
fun whereIs(env as Env(valueL,closureL,whatMap),target) =
  let fun bfs(nil,nil) = raise Lookup(target,env)
	| bfs(nil,next) = bfs(next,nil)
	| bfs((h,CR(offset,{functions,values,closures,stamp}))::m,next) =
            let fun cls(nil,i,next) = bfs(m,next)
		  | cls((v,cr)::t,i,next) =
		    if target=v
		    then h(SELp(i-offset,OFFp 0))
		    else cls(t,i+1,
			     (fn p => h(SELp(i-offset,p)),cr)::next)
		fun vls(nil,i) = cls(closures,i,next)
		  | vls(v::t,i) =
		    if target=v
		    then h(SELp(i-offset,OFFp 0))
		    else vls(t,i+1)
		fun fns(nil,i) = vls(values,i)
		  | fns((v,l)::t,i) =
		    if target=v
		    then h(OFFp(i-offset)) (* possible OFFp 0 *)
		    else fns(t,i+1)
	    in  if target=stamp
		then h(OFFp(~offset)) (* possible OFFp 0 *)
		else fns(functions,0)
	    end
      fun search closures =
	let val (v,p) = bfs(map (fn (v,cr) => ((fn p => (v,p)),
					       cr)) closures,
			    nil)
	in  Path(v,p)
	end
      fun lookC ((v,_)::tl) =
	    if target=v then Direct else lookC tl
	| lookC nil = search closureL
      fun lookV (v::tl) =
	    if target=v then Direct else lookV tl
	| lookV nil = search closureL
  in  case whatIs(env,target)
	of Function _ => Direct
	 | Callee _ => Direct
	 | Closure _ => lookC closureL
	 | Value => lookV valueL
  end

end (* abstype env *)


(* return the ith object of a closure *)
fun select(i,CR(offset,{functions,values,closures,...})) =
  let val index = offset + i - length functions
      val vlen = length values
  in  if index >= vlen
      then (Closure o #2 o nth)(closures,index - vlen)
	  handle Nth => error "bad select in cps/closure"
      else if index < 0 then error "bad select' in cps/closure"
      else Value
  end 

(* Bind the offset of a closure to a variable in an environment *)
fun offset(i,CR(offset,x),v,env) =
  augment((v,Closure(CR(offset+i,x))),env)

(* Perhaps we should change the representation of closures
   to make this faster. *)
fun inClosure (c,CR(_,{functions,values,closures,stamp})) v =
  (v=c
   orelse v=stamp
   orelse exists (fn (w,_) => v=w) functions
   orelse exists (fn w => v=w) values
   orelse exists (fn z => inClosure z v) closures)



(****************************************************************************
 * "Alpha conversion": the closure converter introduces duplicate bindings  *
 * at function arguments (the free variables of known functions) and at     *
 * SELECT's and OFFSET's from closures.  This function restores unique      *
 * bindings, and also eliminates OFFSET's of 0 (which are introduced as     *
 * a side effect of trying to improve lazy display).  It assumes that a     *
 * FIX has no free variables.                                               *
 ****************************************************************************)
fun unrebind ce =
let fun rename rebind(VAR v) =
	      let fun f nil = VAR v
		    | f ((w:int,v')::t) = if v=w then v' else f t
	      in  f rebind
	      end
      | rename _ x = x
    fun f (l,args,b) =
      let val (args',rebind') = fold (fn(v,(args',rebind')) =>
					let val v' = dupLvar v
					in  (v'::args',(v, VAR v')::rebind')
					end)
				     args (nil,nil)
      in  (l,args',g rebind' b)
      end
    and g (rebind: (lvar * value) list) =
      let val rename = rename rebind
	  val rec h =
	       fn RECORD(kind,vl,w,e) =>
		    RECORD(kind,map (fn(v,p) => (rename v,p)) vl,w,h e)
		| OFFSET(0,v,w,e) => g ((w,rename v)::rebind) e
		| OFFSET(i,v,w,e) =>
			let val w' = dupLvar w
			in  OFFSET(i,rename v,w',g ((w, VAR w')::rebind) e)
			end
(*		| SELECT(i,v,w,e as APP(VAR x, args)) =>
			let val w' = dupLvar w
			in  if w=x
			    then SELECT(i,rename v,w',APP(VAR w',map rename args))
			    else SELECT(i,rename v,w',g((w,VAR w')::rebind) e)
			end
*)		| SELECT(i,v,w,e) =>
			let val w' = dupLvar w
			in  SELECT(i,rename v,w',g((w, VAR w')::rebind) e)
			end
		| APP(f,vl) => APP(rename f,map rename vl)
		| FIX(l,e) => FIX(map f l,h e)
		| SWITCH(v,c,el) => SWITCH(rename v,c,map h el)
		| BRANCH(i,vl,c,e1,e2) => BRANCH(i,map rename vl,c, h e1, h e2)
		| SETTER(i,vl,e) => SETTER(i,map rename vl,h e)
		| LOOKER(i,vl,w,e) => LOOKER(i,map rename vl,w,h e)
		| ARITH(i,vl,w,e) => ARITH(i,map rename vl,w,h e)
		| PURE(i,vl,w,e) => PURE(i,map rename vl,w,h e)
      in  h 
      end
in  g nil ce
end


(****************************************************************************
 * closeCallGraph: compute the transitive closure of the call graph of a    *
 * set of (possibly recursive) functions.                                   *
 ****************************************************************************)
type info = {v:lvar,args:lvar list,body:cexp,other:lvar list}
fun closeCallGraph g =
  let fun getNeighbors l =
        fold (fn (({v,...}:info,_,nbrs),n) =>
	      if member l v then merge(nbrs,n)
		  else n) g l
      fun f ((x,len,nbrs),(l,change)) =
	  let val nbrs' = getNeighbors nbrs
	      val len' = length nbrs'
	  in  ((x,len',nbrs')::l,change orelse len<>len')
	  end
      val (g',change) = fold f g (nil,false)
  in  if change then closeCallGraph g' else g'
  end


(**********************************************************************
 * Simple closure strategies.                                         *
 **********************************************************************)
local
fun flat(env,free) =
 fold (fn (v,(vls,cls)) =>
        let val obj = whatIs(env,v)
	in  case obj
              of Value => (v::vls,cls)
               | Closure cr => (vls,(v,cr)::cls)
	       | _ => error "unexpected vc in cps/closure.sml"
	end) free (nil,nil)
fun link(env,free) =
  case getClosure(env)
    of NONE => flat(env,free)
     | SOME z =>
         let val notIn = sublist (not o (inClosure z)) free
	     val (values,closures) = flat(env,notIn)
	 in  if length(notIn) = length(free) (* Does adding the
                                                closure help? *)
	     then (values,closures)          (* NO *)
	     else (values,z::closures)       (* YES *)
	 end
in
fun closureStrategy(functions,free,env) =
  let val (values,closures) =
        case !CGoptions.closureStrategy
	  of 3 => link(env,free)
	   | 2 => link(env,free)
	   | _ => flat(env,free)
      val cname = closureLvar()
  in  (cname,{cname=cname,
   	      cr=CR(0,{functions=functions,
		       values=values,closures=closures,stamp=cname}),
	      contents=(map (LABEL o #2) functions)
	               @ (map VAR values)
	               @ (map (VAR o #1) closures)})
  end
end (* local *)


(**********************************************************************
 * sameClosureOpt: if two free variables are functions from the same  *
 * closure, just one of them is sufficient to access both.            *
 **********************************************************************) 
fun sameClosureOpt(free,env) =
case !CGoptions.closureStrategy
  of 0 => free (* flat without aliasing *)
   | 2 => free (* linked without aliasing *)
   | _ => (* all others have aliasing *)
  let val mapping = map (fn v => let val obj = whatIs(env,v)
				 in  (v,obj)
				 end) free
      fun uniq ((hd as (v,Closure(CR(_,{stamp,...}))))::tl) =
	let val m' = uniq tl
	in  if exists (fn (_,Closure(CR(_,{stamp=stamp',...}))) => stamp=stamp'
			| _ => false) m'
	    then m' else hd::m'
	end
	| uniq (hd::tl) = hd::uniq tl
	| uniq nil = nil
  in  map #1 (uniq mapping)
  end


local
fun follow rootvar =
  let val rec follow0 =
	fn (v,OFFp off,h) =>
		h o (fn ce => OFFSET(off,VAR v,rootvar,ce))
	 | (v,SELp(i,OFFp 0),h) =>
		h o (fn ce => SELECT(i,VAR v,rootvar,ce))
	 | (v,SELp(i,p),h) =>
		let val w = mkLvar()
		in  follow0(w,p,h o (fn ce => SELECT(i,VAR v, w, ce)))
		end handle Bind => error "follow in closure"
  in  follow0
  end
in
(****************************************************************************
 * fixAccess: find the access path to a variable.  A header to select the   *
 * variable from the environment is returned, along with a new environment  *
 * that reflects the actions of the header (this last implements a "lazy    *
 * display").  fixAccess actually causes rebindings -- the variable         *
 * requested is rebound if it is not immediately available in the           *
 * environment.                                                             *
 ****************************************************************************)
fun fixAccess(args,env) =
let
fun access(VAR rootvar,(env,header)) =
  let val what = whatIs(env,rootvar)
  in  case what
	of Callee _ => error "Callee in fixAccess"
	 | Function _ => error "Function in fixAccess"
	 | _ => ();
      case whereIs(env,rootvar)
	of Direct => (env,header)
	 | Path(start,path) =>
	    let val header = follow rootvar (start,path,header)
		val env = augment((rootvar,what),env)
	    in  if not(!CGoptions.allocprof) then (env,header)
		else (env,header o profLinks(lenp path))
	    end
  end
  | access(_, y) = y
in  fold access args (env,fn x => x)
end

fun fixArgs(args,env) =
  let fun fixArgs0 (nil,env,h) = (nil,env,h)
	| fixArgs0 (hd::tl,env,h) =
            (case hd
	       of VAR rootvar =>
		  let val what = whatIs(env,rootvar)
		  in  case what
			of Callee(label,extra) =>
			     let val args' = (label::extra)@tl
				 val (env,h') = fixAccess(args',env)
			     in  (args',env,h o h')
			     end
			 | Function _ => error "Function in fixArgs"
			 | _ =>
			  (case whereIs(env,rootvar)
			     of Direct =>
				 let val (args',env,h) = fixArgs0(tl,env,h)
				 in  (hd::args',env,h)
				 end
			    | Path(start,path) =>
				  let val h = follow rootvar
					             (start,path,h)
				      val env = augment((rootvar,what),env)
				      val (args',env,h) = fixArgs0(tl,env,h)
				  in  if not(!CGoptions.allocprof)
				      then (hd::args',env,h)
				      else (hd::args',env,
					    h o profLinks(lenp path))
				  end)
		  end
                | _ => let val (args',env,h) = fixArgs0(tl,env,h)
		       in  (hd::args',env,h)
		       end)
   in  fixArgs0(args,env,fn x => x)
   end (* fixArgs *)
end (* local *)



(****************************************************************************
 * recordEl: Find the complete access paths for elements of a record.       *
 * Return a header for profiling purposes if needed.                        *
 ****************************************************************************)
fun recordEl(l,env) =
if not(!CGoptions.allocprof)
then (map (fn (VAR v,p) => 
       	    (case whereIs(env,v)
	       of Direct => (VAR v,p)
	        | Path(start,path) => (VAR start, combinepaths(path,p)))
            | vp => vp)
          l,
      fn x => x)
else
let val (rl,cl) = 
      fold (fn ((VAR v,p),(l,cl)) =>
	         let val (m,cost) =
                       case whereIs(env,v)
			 of Direct => ((VAR v,p),0)
			  | Path(start,path) =>
			      ((VAR start, combinepaths(path,p)),
			       lenp path)
		 in  (m::l,cost::cl)
		 end
	      | (m,(l,cl)) => (m::l,0::cl))
	    l (nil,nil)
in  (rl,profRecLinks cl)
end



(****************************************************************************
 * freeAnalysis: Take a free variable list and:                             *
 * (1) replace knownfuncs by their free variables;                          *
 * (2) replace callee-save continuations by their extra arguments.          *
 ****************************************************************************)
local
fun clean l = 
let fun vars(l, VAR x :: rest) = vars(x::l, rest)
      | vars(l, _::rest) = vars(l,rest)
      | vars(l, nil) = l
 in vars(nil,l)
end
in
fun freeAnalysis(free,env) =
  fold (fn (v,l) =>
	  case whatIs(env,v)
	      of Callee(VAR v',extra) => (** outside case **)
                                   (** the clean is unnecessary **)
		     merge(l,enter(v',uniq(clean extra)))
	       | Callee(LABEL _,extra) => (** inside case **)
		     merge(l,uniq(clean extra))
	       | Function{free,...} => merge(free,l)
	       | _ => enter(v,l))
	free nil
end



(**********************************************************************
 * closeCPS: MAIN FUNCTION                                            *
 **********************************************************************)
fun closeCPS(f,vl,ce) =
let

val baseEnv = emptyEnv()

val numCSregs = !System.Control.CG.calleesaves
(*
val numCSregs = if (numCSregs = 1 orelse numCSregs < 0)
		then 2 else numCSregs
*)

local
val ((f,vl,ce),iscont0,ebinfo0) =
 (case numCSregs
    of 0 => ((f,vl,ce),fn _ => false, 
              fn _ => error "check EBinfo when callee=0 in closure.sml")
     | _ => ContMap.contmap(f,vl,ce))

val (freevars,isEscape,isKnown) = FreeMap.freemapClose ce
in    
(** A continuation will be known if the function that it gets
    passed to is inlined.  If so, we just treat it as a regular
    known function. **)
fun isCallee v = (iscont0 v) andalso (not(isKnown v))
val ebinfo = ebinfo0
val (f,vl,ce,freevars,isEscape) = (f,vl,ce,freevars,isEscape)
end (* local *)


val extraConst =
  let fun ec(0) = nil
	| ec(k) = (INT 0)::(ec(k-1))
  in  ec(numCSregs)
  end

(***********************************************************************
 * addExtra: look at the formal arguments of a function, and replace   *
 * parameters to be treated as callee-save continuations by new        *
 * parameters for the continuation and its extra arguments.            *
 ***********************************************************************)
local
fun extraLvar(0) = nil
  | extraLvar(k) =  mkLvar()::extraLvar(k-1)
in
fun addExtra(args) =
  fold (fn (a,(al,el)) =>
	 if isCallee a
	 then let val a' = dupLvar a
		  val el' = extraLvar(numCSregs)
	      in  if not(null el)
			 (* A function can have 1 or 0 continuation
			    arguments -- 0 if it is a continuation. *)
		  then error "closure/addExtra: >1 continuation"
		  else (addCallee(baseEnv,(a,(VAR a',map VAR el')));
			(a'::(el'@al),el'))
	      end
	 else  (a::al,el)) args (nil,nil)
end (* local *)

fun addinc(c,label,extra) = addCallee(baseEnv,(c,(LABEL label,extra)))

(***>
val alphac = System.Control.CG.alphac
val unrebind = fn x => if !alphac then unrebind x else x
<***)



(**********************************************************************
 * makenv: Create the environments for functions in a FIX.            *
 **********************************************************************)
fun makenv(initEnv,bindings: (lvar * lvar list * cexp) list,extraArgs) =
let

(***>
    fun COMMENT f = if !System.Control.CG.comment then (f(); ()) else ()
    val _ = COMMENT(fn() => (pr "BEGINNING MAKENV.\nFunctions: ";
			     ilist (map #1 bindings);
			     pr "Initial environment:\n";
			     printEnv initEnv; pr "\n"))

    val freevars =
      (fn v => let val free = freevars v
	       in  COMMENT(fn() => (pr "Free in "; vp v; pr ":"; ilist free));
		   free
	       end)
<***)

(* Separate function bindings into those that escape, those which are
   known functions, and callee-save continuations. *)

val (escapeB,knownB) = partition (isEscape o #1) bindings
val (calleeB,escapeB) = partition (isCallee o #1) escapeB
val escapeV = uniq(map #1 escapeB)
val calleeV = uniq(map #1 calleeB)

local val knownV = uniq(map #1 knownB)
   in val knownlvar = member knownV
  end

(***>
val _ = COMMENT(fn() => (pr "Known functions:"; ilist (map #1 knownB)))
<***)

(* Initial processing of the known function bindings. *)
val knownB =
 map (fn(v,args,body) =>
        let val free = freevars v

            (* Separate the free variable list into known functions 
             * defined in this FIX and other free variables. 
             *)
            val (fns,other) = partition knownlvar free
         in ({v=v,args=args,body=body,other=other},length fns,fns)
        end) knownB

(* Compute the closure of the call graph of the known functions. *)
val knownB = closeCallGraph knownB

(* See which known functions require a closure, pass 1. *)
local fun gatherNbrs l init =
        fold (fn (({v,other,...}:info,_,_),free) =>
	      if member l v then merge(other,free)
		  else free) knownB init
in
val (knownB,fullClosure) = fold
 (fn ((x as {args,other,...}:info,_,fns),(k,fullClosure)) =>
  let
      (* Get the combined list of free variables of all the functions
         reachable in the call graph. *)
      val free = gatherNbrs fns other
      val len = length free

      (* Remove any escaping functions of the FIX from the free variable
         list and mark that the function requires the closure. *)
      local val free' = difference(free,escapeV)
	    val len' = length free'
      in
      val callc = (len<>len')
      val free = free'
      val len = len'
      end

      (* Remove any callee-save continuations of the FIX from the free
         variable list and mark that the function requires the continuation
	 extra arguments *)
      local val free' = difference(free,calleeV)
	    val len' = length free'
      in
      val extrac = (len<>len')
      val free = free'
      val len = len'
      end

      (* Replace known functions defined in other FIX'es by their free
         variables, and callee-save continuation variables defined in
	 other FIX'es by their extra arguments. *)
      val free = freeAnalysis(free,initEnv)

      (* If the free list contains two functions from the same
         closure, we only need one pointer to the closure. *)
      val free = sameClosureOpt(free,initEnv)

      (* If the function has too many extra arguments to fit into
         registers, then we must put them in the closure. *)
      val len = length free
      val callc = callc orelse
	          ((length args + numCSregs + len >= maxfree)
		   andalso len > 1)

      (* for calleesave only *)
      val callc = callc orelse extrac
   in ((x,free,callc,fns)::k,fullClosure orelse extrac)
  end) knownB (nil,false)
end (* local *)

(* See which known functions require a closure, pass 2. *)
local fun checkNbrs l init =
        fold (fn (({v,...}:info,_,callc,_),c) =>
	        c orelse (callc andalso (member l v)))
              knownB init
in
val (knownB,collected) = fold
 (fn (({v,args,body,...}:info,free,callc,fns),(l,c)) =>
   let val callc = checkNbrs fns callc
       val (free,collected) = if callc then (nil,merge(free,c))
			      else (free,c)
       val label = Access.dupLvar v
   in  ({v=v,args=args,body=body,free=free,label=label,
	 callc=callc}::l,collected)
   end) knownB (nil,nil)
end (* local *)



(***>
val _ = COMMENT(fn() => (pr "Escaping functions:"; ilist (map #1 escapeB)))
<***)

(* Get the combined list of the free variables of all the escaping functions
   of the FIX. *)
val escapeFree =
 let val f = fold (fn (v,f') =>
		         let val f'' = freevars v
			 in  merge(f'',f')
			 end)
                      escapeV nil
 in  remove(escapeV, f)
 end

(* Decide on labels for the escaping functions. *)
val escapeB = map (fn (v,a,b) => (v,Access.dupLvar v,a,b)) escapeB

(* Replace knownfuncs defined in this FIX with their free variables. *)
local val (fns,other) = partition knownlvar escapeFree
in
val escapeFree : lvar list =
  fold (fn ({v,free,...},b) =>
	if member fns v
	    then merge(free,b)
	else b)
  knownB other
end (* local *)

(* Remove callee-save continuations defined in this FIX, and mark that
 * the closure should contain all free variables of the continuations. 
 *)
local val contlvar = member calleeV
in
val fullClosure = fullClosure orelse (exists contlvar escapeFree)
val escapeFree = difference(escapeFree,calleeV)
end (* local *)

(* Add the free variables of knownfuncs in this FIX which were spilled into
 * the closure. 
 *)
val escapeFree = merge(collected,escapeFree)

(* Replace knownfuncs defined elsewhere with their free variables, 
 * and escaping functions defined elsewhere with their closures, and 
 * callee-save continuations with their extra arguments. 
 *)
val escapeFree : lvar list =
  let val escapeFree = freeAnalysis(escapeFree,initEnv)
      val escapeFree = sameClosureOpt(escapeFree,initEnv)
  in  escapeFree
  end


(***>
val _ = COMMENT(fn() => (pr "Callee-save continuations:";
                         ilist (map #1 calleeB)))
<***)

(* Get the combined list of the free variables of all the callee-save
 * continuations of the FIX. 
 *)
val calleeFree : lvar list =
 let val f = fold (fn (v,f') =>
		   let val f'' = freevars v
		   in  merge(f'',f')
		   end)
                  calleeV nil
 in  remove(escapeV,remove(calleeV,f))
 end

(* Decide on labels for the callee-save continuations. *)
val calleeB = map (fn (v,a,b) => (v,Access.dupLvar v,a,b)) calleeB

(* Replace knownfuncs defined in this FIX with their free variables. *)
val calleeFree : lvar list =
  let val (fns,other) = partition knownlvar calleeFree
  in  fold (fn ({v,free,...},b) =>
	    if member fns v
		then merge(free,b)
	    else b) knownB other
  end

val calleeFree : lvar list =
  let val calleeFree = freeAnalysis(calleeFree,initEnv)
      val calleeFree = sameClosureOpt(calleeFree,initEnv)
  in  calleeFree
  end


(**********************************************************************
 * CALLEE-SAVE REGISTER TARGETING                                     *
 **********************************************************************)
local
(** TJ comments are delimited (** ... **). **)

(** There is a problem with closures and aliasing: each function in the
    closure is a name for the closure.  This has some effect on all the
    following functions. **)

fun in'(v,nil) = false
  | in'(v:int,hd::tl) = if v = hd then true else in'(v,tl)

(* Look through a list of enclosing closures to see if we can use them.
 *    retract : lvar list * ((lvar * (lvar * object)) list) -> lvar list 
 *)
fun retract(fl,cl) = (* fl = free list, cl = closure list *)
  let (** How many of fl are reached from the closure? **)
      fun weight (x as (v,_)) =
        let val t = sublist (inClosure x) fl
        in  (** Return how many, which ones, and the closure **)
	    (length t,t,v)
        end

      (** The closures that reach at least one from fl **)
      val clinfo = sublist (fn (k,_,_) => (k > 0)) (map weight cl)

      (** The closures that reach at least 4 from fl,
          OR that are in fl themselves.
          Makes the previous filter bogus -- might as well eliminate it.
	  These closures might still reach values that are not in fl,
	  so in general it is NOT safe for space to use them. **)
      val clinfo = sublist (fn (k,_,v) => (k > 3) orelse (in'(v,fl))) clinfo

      (** Sort the closures by their usefulness.  Unfortunately, this
          sorts into REVERSE order, i.e. the least useful appear at the
          front of the returned list. **)
      val op btr = fn ((k1,_,_),(k2,_,_)) => (k1 > (k2 : int))
      val clinfo = (Sort.sort op btr) clinfo

      (** Consider each closure in turn.  If it looks like it will help
          us, subtract the reachable elements from fl and add the
	  closure itself. **)
      fun repclos (fl,nil) = fl
        | repclos (fl,(_,t,v)::tl) = 
             let val t' = sublist (fn x => in'(x,fl)) t
	     in if ((length(t')) > 3) orelse in'(v,fl)
                then repclos(enter(v,difference (fl,t')),tl)
                else repclos(fl,tl)
             end
   in repclos(fl,clinfo)
  end

val method = 0 (* or !System.Control.CG.misc3 *)

local
(** If a non-closure free variable is reachable from a closure free variable,
    don't bother to include it in the new closure. **)
fun thinner(fl,cl) =
  let val clinfo = sublist (fn (v,_) => in'(v,fl)) cl

      (** inClinfo(x) iff x is reachable from clinfo. **)
      fun inClinfo z =
         let fun inl nil = false
               | inl (hd::tl) = (inClosure hd z) orelse inl tl
         in  inl clinfo
         end
  in  (** clinfo plus elements of fl not reachable from clinfo. **)
      (merge(uniq(map #1 clinfo),sublist (not o inClinfo) fl))
  end
in
(** NOT safe for space unless all of the cl can be included safely
    (see filter0 in calleealloc). **)
fun preproc(fl,cl) = 
  let val m = length(fl)
      (** inImmedAll(v,e) iff all of fl is reachable from e **)
      fun inImmedAll x =
	      (m = length(sublist (inClosure x) fl))
      (** If all of the free variables are reachable from a single closure,
          just use that one.  Otherwise, do thinner or retract. **)
      fun h nil = if method <> 2 then thinner(fl,cl) else retract(fl,cl)
        | h ((x as (v,_))::tl) = if inImmedAll x then [v] else h(tl)
   in h cl
  end 
end (* local *)

(** NOT safe for space unless all of the cl can be included safely
    (see filter0 in calleealloc). **)
(** newfl = list of free variables,
    cl = list of closures,
    k = a threshold value **)
fun preproc2(newfl,cl,k) = 
  let (** how many of newfl are reachable from a closure? **)
      fun weight (x as (v,_)) =
        let val t = sublist (inClosure x) newfl
	in  (** Return how many, which ones, and the closure. **)
	    (length t,t,v)
        end
      val m = length(newfl)

      (** The closures that reach at least one from newfl. **)
      (** In fact we only consider cases where k > 1, might as well
         filter out k=1 here. **)
      val clinfo = sublist (fn (k,_,_) => (k>0))  (map weight cl)

      (** Sort the closures by their usefulness.  Unfortunately, this
          sorts into REVERSE order, i.e. the least useful appear at the
          front of the returned list.  Also, we only care about the
	  MOST useful one, ought to be doing an O(n) max operation
	  instead of a sort. **)
      val op btr = fn ((k1,_,_),(k2,_,_)) => (k1 > (k2 : int))
      val clinfo = (Sort.sort op btr) clinfo

   in case clinfo of 
        nil => (** None of the free variables appear in closures. **)
	    (newfl,NONE)
      | (j,t,v)::_ =>
	    (let (** Do we bring the number of free variables below
		     the threshold? **)
		 val doit = ((j > 1) andalso ((m - j) <= k))

		 (** Or do we save a lot?  Note that this is NOT safe
		     for space, as dead variables might be reachable
		     from the closure. **)
		 val doit = doit orelse (j > (m div 2) + 1)

		 (** And finally, were we also over the threshold to
		     begin with? **)
		 val doit = doit andalso (m > k+1)

	     in  (** Decide whether to use the closure or not. **)
		 if doit then (difference(newfl,t),SOME v)
		 else (newfl,NONE)
	     end )
  end


in (* body of local *)      
   
(* Try to see if there are any closures already made in the environment which
 * could dramatically decrease the size of current closures.
 * shorten : lvar list -> lvar list
 *)
fun shorten(fl) =
 (case fl
    of nil => nil 
     | _ => (if method <> 2 then fl
	     else if length(fl) < 4 then fl
	     else retract(fl,getClosures initEnv)))


(* Given a list of free variables, if k = 0 we use aggressive approach, if 
 * k = numCSregs -1, we use diligent (or lazy) approach. However when EB 
 * is false, we'll use conservative approach anyway.
 *) 
fun calleealloc(fl,k) =
  let

(***>
      val _ = COMMENT(fn () => (pr "Calleealloc:"; ilist fl))
<***)

      (* this piece of code is used to stop unsafe closure sharing used 
       * among embedied continuation functions. closlist is re-filtered
       * by the free variable list fl.
       *)
      (** Consider only closures which do not hold on to dead values.
          This version is overly restrictive: every single element
	  of the closure must be free.  If the closure has two functions,
	  both must be in the free list; in fact both will never be in
	  the free list if we have already passed the free list through
	  sameClosureOpt -- and this is always the case with the fl passed
	  to calleealloc.  A similar remark applies if one of the contents
	  is a closure. **)
      val closlist = getClosures initEnv
      fun filter0 (v,CR(_,{functions,values,closures,...})) =
	let val functions = map #1 functions
	    val closures = map #1 closures
	    val t1 = uniq(functions @ values @ closures)
        in  fold (fn (x,b) => (in'(x,fl) andalso b)) t1 true
        end
      val closlist = sublist filter0 closlist 

      val restm = sublist (fn x => in'(x,fl)) extraArgs
      val rest = if (length restm) = numCSregs then (tl restm)
                 else restm
      exception FAIL of lvar list * int
      fun first(nil,0,res) = res
        | first(nil,k,res) = raise FAIL (res,k)
        | first(_,0,res) = res
        | first(hd::tl,k,res) = first(tl,k-1,res@[hd])
      and first0(wl,vl,k) = first(wl,k,nil) handle FAIL (c,i) => 
                               (first(vl,i,c) handle FAIL (res,_) => res)

      (** Take two lists, and return a list of length equal to the longer
          of the two lists.  The i^th element of the list will be the
          i^th element of the first list, or the i^th element of the second
	  list if the first list isn't long enough. **)
      fun punch(nil,nil) = nil
        | punch(nil,tl) = tl 
        | punch(tl,nil) = tl
        | punch(hd1::tl1,hd2::tl2) = hd1::punch(tl1,tl2)

      (* Test if it's the innermost continuation function. *)
      fun bmerge((x,xl),(y,yl)) = ((x orelse y),merge(xl,yl))

    (** the getv function may be modified by doing more data flow analysis **)
    (* Choose k lvars from vl, default is rest *)
      fun getv(vl,k,rest) = 
         if length(vl) <= k then punch(vl,rest)
         else (let val el = (map (ebinfo o (fn (v,_,_,_) => v)) calleeB)
                   val (EB,ncand) = fold bmerge el (false,nil)
                in if (not EB) then  (* leaf cont nodes *)
                       punch (rest,first0(difference(vl,uniq rest),nil,k))
                   else (let val wl = difference(vl,ncand)
                             val ul = difference(vl,wl)
                          in punch (first0(wl,ul,k),rest)
                         end)
	       end)
   in if k=0                      
      then (preproc(fl,closlist),rest)
      else (let val newfl = preproc(fl,closlist) 
             in if length(newfl) <= 1 then (newfl,rest)
                else case preproc2(newfl,closlist,k) of 
                       (vl,NONE) => 
                         (let val t = length(vl)
                              val cand = if t <= (k+1) 
                                         then tl(punch(vl,restm))
                                         else getv(vl,k,rest)
                           in (difference(vl,uniq cand),cand)
                          end)
                     | (vl,SOME v) => 
                         (if length(vl) <= k then 
                              ([v],getv(vl,k,rest))
                          else 
                            (let val rest0 = if length(rest) = k 
                                             then (tl rest)
					     else rest
                                 val cand = getv(vl,k-1,rest0)
                                 val result0 = difference(vl,uniq cand)
                              in (result0,v::cand)
                             end))
            end)
  end (* calleealloc *)         
end (* local *)


(* Decide which variables go into the closure and which variables go
   into callee-save registers. *)
val (escapeFree,rest) = case (calleeB,escapeB,collected) 
    of (nil,_,_) =>  (shorten(escapeFree),nil)
     | (_,nil,nil) =>
	    if fullClosure
	    then calleealloc(calleeFree,0)
	    else calleealloc(calleeFree,numCSregs-1)
     | (_,nil,_) =>
	let val left = difference(calleeFree,escapeFree)
	    val (nfl,rest) = if fullClosure
			     then calleealloc(left,0)
			     else calleealloc(left,numCSregs-1)
	in (merge(escapeFree,nfl),rest)
	end
     | _ => (let val fl = merge(escapeFree,calleeFree)
              in calleealloc(fl,0)
             end) 

(***>
val _ = COMMENT(fn () =>
        (pr "Free variables to be accessible from the closure:";
	 ilist escapeFree;
	 pr "Free variables to be accessible from callee-save arguments:";
	 ilist rest))
<***)

(* Given the functions to be defined in the closure (from escapeB), the free
   variables which should be contained in the closure (escapeFree), and their
   current locations (initEnv), decide on a closure representation. *)
val (closureInfo,calleeReg1) = 
      case (escapeB,escapeFree)
       of (nil,nil) => (NONE,NONE)
        | (nil,[v]) => (NONE,SOME v)
        | _ => 
          (let val escapeB' = map (fn(v,l,_,_) => (v,l)) escapeB
               val (cname,clos) = closureStrategy(escapeB',escapeFree,initEnv)
            in (SOME clos, SOME cname)
           end)

val rest = case calleeReg1 of NONE => rest
                            | SOME cname => (cname::rest)

(* Add new known function information to the environment. *)
local fun addF(v,label,free) = augment((v,Function{label=label,free=free}),
				       baseEnv)
in
val _ = app
 (case calleeReg1
    of NONE =>
	(fn{v,free,callc,label,...} =>
	  if callc then error "29488 in closure"
	  else addF(v,label,free))
     | SOME cname =>
	(fn{v,free,callc,label,...} =>
	  if callc
	  then addF(v,label,enter(cname,free))
	  else addF(v,label,free)))
 knownB
end (* local *)


(* Final construction of the environment for each standard function. *)
val escapeFrags : (lvar * lvar list * cexp * env * lvar list) list =
 (case (escapeB,closureInfo)
    of (nil,_) => nil
     | (_,NONE) => error "unexpected 23422 in closure"
     | (_,SOME{cr,...}) => 
	  let val env1 = baseEnv
	      fun f ((v,l,args,body),i) =
		let val myCname = v
		    val env = offset(i,cr,myCname,env1)
		    val (args,kl) = addExtra(args)
		    val env = fold augmentV args env
(***>
		    val _ = COMMENT(fn () => (pr "\nEnvironment in escaping ";
					      vp v; pr ":\n";
					      printEnv env))
<***)
		in  inc System.Control.CG.escapeGen;
		    (l,mkLvar()::myCname::args,body,env,kl)
		end
	  in  formap f escapeB
	  end)


(* Final construction of the environment for each known function. *)
val knownFrags : (lvar * lvar list * cexp * env * lvar list) list =
 map (fn{v,args,body,free,label,callc} =>
  let fun addv(v,env) = case whatIs(initEnv,v)
             of (Function _) => error "cps/closure.223"
              | obj => augment((v,obj),env)

      val (free',env) =
        case (callc,calleeReg1)
	  of (false,_) =>
	        (inc System.Control.CG.knownGen;
		 (free,baseEnv))
	   | (true,NONE) => error "unexpected ~4 in closure"
	   | (true,SOME v) =>
               (let val env' = case closureInfo
                      of NONE => addv(v,baseEnv)
                       | SOME{cname,cr,...} => 
                            augment((cname,Closure cr),baseEnv)
                 in (inc CGoptions.knownClGen; 
                     (enter(v,free),env'))
                end) 

      val env = fold addv free env
      val (args,extra) = addExtra(args)
      val env = fold augmentV args env

(***>
      val _ = COMMENT(fn () => (pr "\nEnvironment in known ";
	                        vp v; pr ":\n";
				printEnv env))
<***)

      val args = args @ free'
  in  if extra = nil then (label,args,body,env,extraArgs)
      else (label,args,body,env,extra)
  end)
 knownB

(* Final construction of the environment for each callee-save
   continuation. *)
val calleeFrags : (lvar * lvar list * cexp * env * lvar list) list =
  case calleeB
    of nil => nil
     | _ => 
    (let val rest =
	   let (** The variables already in the calleesave arguments. **)
	       val wl = extraArgs
	       (** The new values to put in the calleesave arguments. **)
	       val wl' = uniq wl
	       val left = sublist (fn x => not (member wl' x)) rest
	       val vl = uniq rest
	       (** A first assigment of calleesave arguments -- if the
		   position is already occupied by something we need,
		   leave it there (SOME case) otherwise mark that it
		   is free to be filled (NONE case). **)
	       val base =
                 let fun g(x::tl,vl) = 
                            if member vl x then ((SOME x)::g(tl,rmv(x,vl)))
                            else (NONE::g(tl,vl))
                       | g(nil,vl) = nil
                  in g(wl,vl)
                 end
	       (** Fill in the holes in the calleesave argument template with
		   the new values.  **)
	       fun h(tl,nil) = tl
                 | h((SOME x)::tl,ul) = (SOME x)::h(tl,ul)
		 | h(NONE::tl,u::ul) = (SOME u)::h(tl,ul)
		 | h _ = error "errors in closure1.sml --- rest/h"
	   in  h(base,left)
	   end

	 (** Only used in extramap. **)
	 fun getenv (SOME x) = 
                ((case whatIs(initEnv,x) of 
		      (Function _) => error "cps/closure437"
		    | obj => (VAR x,obj))
                 handle Lookup(tt,ee) =>
                   (case closureInfo of 
                       NONE => (raise (Lookup(tt,ee)))
                     | SOME{cname,cr,...} =>
                        (if (cname=x) then (VAR cname, Closure cr)
                         else raise (Lookup(tt,ee)))))
	   | getenv NONE = (INT 0,Value)
 
         val extramap = if numCSregs = 0 then nil
                        else (map getenv rest)

	 val _ = app (fn (v,l,a,b) => addinc(v,l,map #1 extramap)) calleeB

	 val extraenv = map (fn (VAR x,c) => (x,c)
                              | (_,l) => (mkLvar(),Value))
	                    extramap
	 val env = baseEnv
	 val env = fold augment extraenv env

	 fun f (v,l,args,body) =
	   let val env = fold augmentV args env
	       val args' = mkLvar()::args@(map #1 extraenv)
(***>
	       val _ = COMMENT(fn () =>
			       (pr "\nEnvironment in callee-save continuation ";
				vp v; pr ":\n";
				printEnv env))
<***)
	   in  inc CGoptions.calleeGen;
	       (l,args',body,env,map #1 extraenv)
	   end
     in  map f calleeB
     end)
		    

fun mkrexp(contents,cname) =
  if not(!CGoptions.allocprof) then 
                     fn ce => RECORD(RK_RECORD,contents,cname,ce)
  else let val prof =
	     case (escapeB,knownB,calleeB)
               of (nil,_,nil) => profKClosure
	        | (nil,_,_) => profCClosure
		| _ => profClosure
       in  prof(length contents) o (fn ce => 
                                      RECORD(RK_RECORD,contents,cname,ce))
       end

val frags = escapeFrags@knownFrags@calleeFrags
val env = initEnv

val (rexp,env) =
 (case closureInfo
    of NONE => ((fn x => x),env)
     | SOME{cname,cr,contents} => 
	  let val (contents,header) =
                recordEl(map (fn v => (v, OFFp0)) contents, env)
	      val env = augment((cname,Closure cr),env)
	  in  (header o mkrexp(contents,cname),env)
	  end)

in  (* body of makenv *)
(***>
	      COMMENT(fn () => (pr "\nEnvironment after FIX:\n";
	                        printEnv env; pr "MAKENV DONE.\n\n"));
<***)
    (rexp,frags,env)
end (* makenv *)


(**********************************************************************
 * close0: MAIN LOOP of closeCPS                                      *
 **********************************************************************)
fun close0(ce,env,extraArgs) =
let
fun close1(f,vl,ce,env,eA') =
  ((f,vl,close0(ce,env,eA'))
   handle Lookup(v,env) => (pr "LOOKUP FAILS on "; vp v;
			    pr "\nin environment:\n";
			    printEnv env;
			    pr "\nin function:\n";
			    CPSprint.showfun pr (f,vl,ce);
			    error "Lookup failure in cps/closure.sml"))
fun close(ce,env) =
  case ce
    of FIX(bindings,b) =>
        let val (header,frags,env') = makenv(env,bindings,extraArgs)
	in  FIX(map close1 frags, header(close(b,env')))
	end
     | APP(f,args) =>
        let val obj = (case f of VAR v => whatIs(env,v) | _ => Value)
	in  case obj
	      of Closure(CR(offset,{functions,...})) =>
		   let val (env,h) = fixAccess([f],env)
		       val (args',_,h') = fixArgs(args,env)
		       val (_,label) = nth(functions,offset)
		       val call = APP(LABEL label,LABEL label::f::args')
		   in  if not(!CGoptions.allocprof)
		       then h(h'(call))
		       else h(h'(case args
				   of [_] => profCntkCall call
				    | _ => profStdkCall call))
		   end
	       | Function{label,free} =>
		   let (* NOTE: 0 or 1 arg will be a continuation --
			  0 if f is a known continuation. *)
		       val (args',_,header) = fixArgs(args@(map VAR free),env)
		       val call = APP(LABEL label,args')
		   in  if not(!CGoptions.allocprof)
		       then header call
		       else header(profKnownCall call)
		   end
	       | Callee(label,extra) =>
		   let val args' = args@extra
		       val (env,header) = fixAccess(label::args',env)
		       val call = APP(label,label::args')
		   in  if not(!CGoptions.allocprof)
		       then header call
		       else header(case label
			             of LABEL _ => profCSCntkCall call
	                              | _ => profCSCntCall call)
		   end
	       | Value =>
                   let (* Ugly hack to handle functions introduced by
			  contmap for strange continuation variables. *)
		       val args' = case args
				     of [_,INT 0] => args@extraConst
				      | [_,INT _] => error "29 closure"
				      | _ => args
		       val (env,h) = fixAccess([f],env)
		       val (args',_,h') = fixArgs(args',env)
		       val l = mkLvar()
		       val call = SELECT(0,f,l,(APP(VAR l,(VAR l)::f::args')))
		   in  if not(!CGoptions.allocprof)
		       then h(h'(call))
		       else h(h'(case args
				   of [_] => profCntCall call
				    | _ =>  profStdCall call))
		   end
	end
     | SWITCH(v,c,l) =>
	let val (env,header) = fixAccess([v],env)
	in  header (SWITCH(v,c,map (fn c => close(c,env)) l))
	end
     | RECORD(k,l,v,c) =>
	let val (l,header) = recordEl(l,env)
	    val record = RECORD(k,l,v,close(c,augmentV(v,env)))
	in  if not(!CGoptions.allocprof)
	    then header record
	    else header(profRecord (length l) record)
	end
     | SELECT(i,v,w,c) =>
        let val (env,header) = fixAccess([v],env)
	in  header(SELECT(i,v,w,close(c,augmentV(w,env))))
	end
     | OFFSET(i,v,w,c) => error "OFFSET in cps/closure.sml!"
     | BRANCH(i,args,c,e1,e2) =>
	let val (env,header) = fixAccess(args,env)
	in  header (BRANCH(i,args,c,close(e1,env),close(e2,env)))
	end
     | SETTER(i,args,e) =>
	let val (env,header) = fixAccess(args,env)
	in  header (SETTER(i,args,close(e,env)))
	end
     | LOOKER(i,args,w,e) =>
	let val (env,header) = fixAccess(args,env)
	in  header (LOOKER(i,args,w,close(e,augmentV(w,env))))
	end
     | ARITH(i,args,w,e) =>
	let val (env,header) = fixAccess(args,env)
	in  header (ARITH(i,args,w,close(e,augmentV(w,env))))
	end
     | PURE(i,args,w,e) =>
	let val (env,header) = fixAccess(args,env)
	in  header (PURE(i,args,w,close(e,augmentV(w,env))))
	end
in  (* body of close0 *)
    close(ce,env)
end

val (vl,e) = addExtra(vl)
val env1 = fold augmentV (f::vl) baseEnv

in  (* body of closeCPS *)
    (mkLvar(),mkLvar()::f::vl,unrebind(close0(ce,env1,e)))
end

end (* structure Closure *)

