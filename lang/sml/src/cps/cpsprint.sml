(* Copyright 1989 by AT&T Bell Laboratories *)

signature CPS_PRINT =
sig
  val showfun : (string -> unit) -> Access.lvar * Access.lvar list * CPS.cexp
                -> unit
  val show : (string -> unit) -> CPS.cexp -> unit
end

structure CPSprint : CPS_PRINT =
struct

open CPS

fun lookerName P.! = "!"
  | lookerName P.gethdlr = "gethdlr"
  | lookerName P.subscript = "subscript"
  | lookerName P.subscriptf = "subscriptf"
  | lookerName P.getvar = "getvar"
  | lookerName P.deflvar = "deflvar"
  | lookerName P.ordof = "ordof"
  | lookerName P.getspecial = "getspecial"

fun branchName P.boxed = "boxed"
  | branchName P.unboxed = "unboxed"
  | branchName P.< = "<"
  | branchName P.<= = "<="
  | branchName P.> = ">"
  | branchName P.>= = ">="
  | branchName P.ieql = "ieql"
  | branchName P.ineq = "ineq"
  | branchName P.lessu = "lessu"
  | branchName P.gequ = "gequ"
  | branchName P.feql = "feql"
  | branchName P.fge = "fge"
  | branchName P.fgt = "fgt"
  | branchName P.fle = "fle"
  | branchName P.flt = "flt"
  | branchName P.fneq = "fneq"

fun setterName P.store = "store"
  | setterName P.unboxedupdate = "unboxedupdate"
  | setterName P.boxedupdate = "boxedupdate"
  | setterName P.update = "update"
  | setterName P.updatef = "updatef"
  | setterName P.sethdlr = "sethdlr"
  | setterName P.setvar = "setvar"
  | setterName P.uselvar = "uselvar"
  | setterName P.setspecial = "setspecial"

fun arithName P.* = "*"
  | arithName P.+ = "+"
  | arithName P.- = "-"
  | arithName P.div = "div"
  | arithName P.fadd = "fadd"
  | arithName P.fdiv = "fdiv"
  | arithName P.fmul = "fmul"
  | arithName P.fsub = "fsub"
  | arithName P.~ = "~"
  | arithName P.floor = "floor"
  | arithName P.round = "round"

fun pureName P.length = "length"
  | pureName P.objlength = "objlength"
  | pureName P.makeref = "makeref"
  | pureName P.rshift = "rshift"
  | pureName P.lshift = "lshift"
  | pureName P.orb = "orb"
  | pureName P.andb = "andb"
  | pureName P.xorb = "xorb"
  | pureName P.notb = "notb"
  | pureName P.real = "real"
  | pureName P.subscriptv = "subscriptv"
  | pureName P.gettag = "gettag"
  | pureName P.mkspecial = "mkspecial"


fun show0 say =
  let fun sayv(VAR v) = say(Access.lvarName v)
        | sayv(LABEL v) = say("(L)" ^ Access.lvarName v)
	| sayv(INT i) = say("(I)" ^ makestring i)
	| sayv(REAL r) = say r
	| sayv(STRING s) = (say "\""; say s; say "\"")
      fun sayvlist [v] = sayv v
        | sayvlist nil = ()
	| sayvlist (v::vl) = (sayv v; say ","; sayvlist vl)
      fun saypath(OFFp 0) = ()
	| saypath(OFFp i) = (say "+"; say(makestring i))
	| saypath(SELp(j,p)) = (say "."; say(makestring j); saypath p)
      fun sayvp (v,path) = (sayv v; saypath path)
      fun saylist f [x] = f x | saylist f nil = () 
	| saylist f (x::r) = (f x; say ","; saylist f r)
      fun indent n =
	let fun space 0 = () | space k = (say " "; space(k-1))
	    fun nl() = say "\n"
    	    val rec f =
	     fn RECORD(k,vl,v,c) => (
		  space n;
		  case k of Access.RK_VECTOR => say "#{" | _ => say "{";
		  saylist sayvp vl; say "} -> ";
		  sayv(VAR v);
		  nl(); f c)
	      | SELECT(i,v,w,c) =>
		    (space n; sayv v; say "."; say(makestring i); say " -> ";
		     sayv(VAR w); nl(); f c)
	      | OFFSET(i,v,w,c) =>
		    (space n; sayv v; say "+"; say(makestring i); say " -> ";
		    sayv(VAR w); nl(); f c)
	      | APP(w,vl) => 
		    (space n; sayv w; say "("; sayvlist vl; say ")\n")
	      | FIX(bl,c) =>
		    let fun g(v,wl,d) = 
			    (space n; sayv(VAR v); say "("; 
			     sayvlist (map VAR wl);
			     say ") =\n"; indent (n+3) d)
		     in app g bl; f c
		    end
	      | SWITCH(v,c,cl) =>
		   let fun g(i,c::cl) =
			(space(n+1); say(makestring(i:int));
			 say " =>\n"; indent (n+3) c; g(i+1,cl))
			 | g(_,nil) = ()
		    in space n; say "case "; sayv v; say "  ["; 
		       say(makestring(c));
		       say "] of\n"; 
		       g(0,cl)
		   end
	      | LOOKER(i,vl,w,e) =>
		   (space n; say(lookerName i); say "("; sayvlist vl;
		    say ") -> "; sayv(VAR w); nl(); f e)
	      | ARITH(i,vl,w,e) =>
		   (space n; say(arithName i); say "("; sayvlist vl;
		    say ") -> "; sayv(VAR w); nl(); f e)
	      | PURE(i,vl,w,e) =>
		   (space n; say(pureName i); say "("; sayvlist vl;
		    say ") -> "; sayv(VAR w); nl(); f e)
	      | SETTER(i,vl,e) =>
		   (space n; say(setterName i); say "("; sayvlist vl;
		    say ")"; nl(); f e)
	      | BRANCH(i,vl,c,e1,e2) =>
	           (space n; say "if "; say(branchName i);
			 say "("; sayvlist vl; say ") ["; 
                         sayv(VAR c); say "] then\n";
		    indent (n+3) e1;
		    space n; say "else\n";
		    indent (n+3) e2)
         in f
        end
 in  indent
 end

fun showfun say (f,vl,e) =
let   
      fun sayvlist [v] = say(Access.lvarName v)
        | sayvlist nil = ()
	| sayvlist (v::vl) = (say(Access.lvarName v); say ","; sayvlist vl)
 in 
 (say(Access.lvarName f); say "("; sayvlist vl; say ") =\n";
  show0 say 3 e)
end

fun show say = show0 say 0

end  (* structure CPSprint *)
