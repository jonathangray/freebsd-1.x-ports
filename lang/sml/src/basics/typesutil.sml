(* Copyright 1989 by AT&T Bell Laboratories *)
(* typesutil.sml *)

structure TypesUtil : TYPESUTIL = struct

structure Types = Types

open Array List PrintUtil Types List2 ErrorMsg BasicTypes
infix 9 sub (* stupid! *)

(*************** misc. functions for use in parsing tyvars ***************)

(*
 * left(s,pos):
 *
 *    This function returns the left part of the string s up to but
 * NOT including position pos.  Positions off to the right
 * or left of the string do the obvious thing.
 *
 * Ex: left(s,0) = "", left("hello",1) = "h", left("hi",10) = "hi"
 *)
fun left(s,pos) =
    if pos < 0 then
	""
    else
	(substring(s,0,pos) handle Substring => s)

(*
 * right(s,pos):
 *
 *    This function returns the right part of the string s from
 * the position pos to the end.  Positions off to the right
 * or left of the string do the obvious thing.
 *
 * Ex: right(s,0) = s, right("hello",1) = "ello", right("hi",10) = ""
 *
 * Note: for all i, s = left(s,i) ^ right(s,i)
 *)
fun right(s,pos) =
    let val len = size s
    in
	if pos < 0 then
	    s
	else
	    (substring(s,pos,len - pos) handle Substring => "")
    end

fun split(s,pos) = (left(s,pos), right(s,pos))

(*
 * get_digits s:
 *
 *    This function splits s into 2 strings.  The split occurs just after
 * the regular expression [0-9]*.
 *
 * Ex: get_digits("123abc") = ("123", "abc"), get_digits("abc") = ("", "abc")
 *)
fun get_digits s =
    let fun loop i = if Ascii.isDigit(ordof(s,i)) then loop(i+1) else i
    in
	(split(s,loop 0)) handle Ord => (s, "")
    end

(*
 * string_to_int infinity s:
 *
 *    This function returns the integer value represented by the leading
 * digits of s.  If no digits are present, then 0 is returned.  If the
 * number in question is larger than infinity, then infinity is
 * returned.
 *
 * Ex: string_to_int 200 "123abc4" = 123, string_to_int 200 "9999e" = 200
 *)
fun string_to_int infinity s =
    let fun loop i n =
		(let val c = ordof(s,i)
		in
		    loop (i+1) (n*10 + c-Ascii.zero)
		end) handle Ord => n
			  | Overflow => infinity
    in
	min(loop 0 0, infinity)
    end

(*************** operations to build tyvars, VARtys ***************)

fun mkTvinfo(kind, depth) : tvinfo = 
	OPEN{kind=kind,
	     depth=depth,
	     eq=false,
	     weakness=infinity}

fun mkMETA depth = mkTvinfo(META, depth)

fun mkFLEX(fields, depth) = mkTvinfo(FLEX fields, depth)

val max_weakness = infinity div 2;

fun extract_varname_info name =
    let val name = right(name,1)		(* remove leading "'" *)
	val (eq, name) =
	    if left(name,1) = "'" then		(* a "'" at start now *) 
		(true,right(name,1))		(*   means equality   *)
	    else
		(false,name)
	val (digits, rest) = get_digits name
	val (weakness, name) =
	    if left(name,1) = "_" then		(* "_" at start means *)
		(1, right(name,1))		(*   weakness 1	      *)
	    else if digits = "" then
		(infinity, name)		(* no #/_ = strong    *)
	    else
		(string_to_int (max_weakness + 1) digits, rest)
    in
	(name, eq, weakness)
    end

fun mkUBOUND(id : Symbol.symbol,err: ErrorMsg.complainer) : tvinfo =
    let val (name, eq, weakness) = extract_varname_info (Symbol.name id)
    in
        OPEN{kind=UBOUND (Symbol.tyvSymbol name),
             depth=infinity,
             eq=eq,
             weakness=
		(if weakness = max_weakness + 1 then
		    (err COMPLAIN "weakness number too large in type variable"
		      nullErrorBody;
		    max_weakness)
		else
		    weakness)}
    end

(*
 * mkMETAty:
 *
 *   This function returns a type that represents a new meta variable
 * which does NOT appear in the "context" anywhere.  To do the same
 * thing for a meta variable which will appear in the context (because,
 * for example, we are going to assign the resulting type to a program
 * variable), use mkRefMETAty with the appropriate depth.
 *)
fun mkRefMETAty depth : ty = VARty(mkTyvar (mkMETA depth))

fun mkMETAty() = mkRefMETAty infinity


(*************** primitive operations on tycons ***************)

fun impossibleTyc s =
    fn (GENtyc _) => impossible (s ^ " GENtyc")
     | (DEFtyc _) => impossible (s ^ " DEFtyc")
     | (RECORDtyc _) => impossible (s ^ " RECORDtyc")
     | (RELtyc _) => impossible (s ^ " RELtyc [something]")
     | (ABSFBtyc _) => impossible (s ^ " ABSFBtyc [something]")
     | (FORMtyc l) => impossible (s ^ " FORMtyc [something]")
     | (ERRORtyc) => impossible (s ^ " ERRORtyc")
     | _ => impossible (s ^ " [some tyc]")

fun tycName(tycon) =	(* fix this some day to return entire path *)
    case tycon
      of GENtyc{path=name::_,...} => name
       | DEFtyc{path=name::_,...} => name
       | ERRORtyc => Symbol.tycSymbol "error"     
       | _ => impossibleTyc "tycName" tycon

fun tyconArity(GENtyc{arity,...}) = arity
  | tyconArity(RECORDtyc l) = length l
  | tyconArity(DEFtyc{tyfun=TYFUN{arity,...},...}) = arity
  | tyconArity(FORMtyc{spec,...}) = tyconArity spec
  | tyconArity(OPENFORMtyc{spec,...}) = tyconArity spec
  | tyconArity(ERRORtyc) = 0
  | tyconArity tycon = impossibleTyc "tyconArity" tycon

fun tycPath (GENtyc{path,...}) = path
  | tycPath (DEFtyc{path,...}) = path
  | tycPath ERRORtyc = [Symbol.tycSymbol "error"]
  | tycPath tycon  = impossibleTyc "tycPath" tycon

fun setTycPath(tycon,path) =
    case tycon
      of GENtyc{stamp,arity,eq,kind,...} =>
	   GENtyc{stamp=stamp,path=path,arity=arity,eq=eq,kind=kind}
       | DEFtyc{tyfun,strict,...} => DEFtyc{tyfun=tyfun,path=path,strict=strict}
       | _ => impossibleTyc "setTycName" tycon

fun eqTycon(GENtyc{stamp=s,...},GENtyc{stamp=s',...}) = s=s'
  | eqTycon(ERRORtyc,_) = true
  | eqTycon(_,ERRORtyc) = true
	(*
	 * This last case used for comparing DEFtyc's, RECORDtyc's.
	 * Also used in PPBasics to check data constructors of
	 * a datatype.  Used elsewhere?
	 *)
  | eqTycon(t1,t2) = (t1 = t2) 

	(* for now... *)
fun mkCONty(ERRORtyc, _) = WILDCARDty
  | mkCONty(tycon as DEFtyc{tyfun,strict,...}, args) =
	CONty(tycon, map2 (fn (ty,strict) => if strict then ty else WILDCARDty)
                          (args,strict) )
  | mkCONty(tycon, args) = CONty(tycon, args);

fun prune(VARty(tv as ref(INSTANTIATED ty))) : ty =
      let val pruned = prune ty
       in tv := INSTANTIATED pruned; pruned
      end
  | prune ty = ty
    
fun eqTyvar(tv1: tyvar, tv2: tyvar) = (tv1 = tv2)







fun bindTyvars(tyvars: tyvar list) : unit =
    let fun loop([],_) = ()
	  | loop(tv::rest,n) =
	      (tv := INSTANTIATED (IBOUND n);
	       loop(rest,n+1))
     in loop(tyvars,0)
    end

fun bindTyvars1(tyvars: tyvar list) : {weakness:int,eq:bool} list =
    let fun loop([],_) = []
	  | loop((tv as ref(OPEN{kind=UBOUND _,weakness,eq,...}))::rest,n) =
	       (tv := INSTANTIATED (IBOUND n);
	        {weakness=weakness,eq=eq} :: loop(rest,n+1))
     in loop(tyvars,0)
    end

exception SHARE

(* assume that f fails on identity, i.e. f x raises SHARE instead of 
   returning x *)
fun shareMap f nil = raise SHARE
  | shareMap f (x::l) =
      (f x) :: ((shareMap f l) handle SHARE => l)
      handle SHARE => x :: (shareMap f l)

fun applyTyfun(TYFUN{arity,body},args) =
    let fun subst(IBOUND n) = nth(args,n)
	  | subst(CONty(tyc,args)) = CONty(tyc, shareMap subst args)
	  | subst(VARty(ref(INSTANTIATED ty))) = subst ty
	  | subst _ = raise SHARE
     in if arity > 0
	then subst body
	     handle SHARE => body
		  | Nth => impossible "applyTyfun: not enough arguments"
	else body
    end

exception ReduceType

fun reduceType(CONty(DEFtyc{tyfun,...}, args)) = applyTyfun(tyfun,args)
  | reduceType(VARty(ref(INSTANTIATED ty))) = ty
  | reduceType _ = raise ReduceType

fun headReduceType ty = headReduceType(reduceType ty) handle ReduceType => ty

fun equalType(ty,ty') =
    let fun eq(IBOUND i1, IBOUND i2) = i1 = i2
	  | eq(VARty(tv),VARty(tv')) = eqTyvar(tv,tv')
	  | eq(ty as CONty(tycon, args), ty' as CONty(tycon', args')) =
	      if eqTycon(tycon, tycon') then List2.all2 equalType(args,args') 
	      else (eq(reduceType ty, ty')
		    handle ReduceType =>
		      (eq(ty,reduceType ty') handle ReduceType => false))
	  | eq(ty1 as VARty _, ty2 as CONty _) =
	      (eq(ty1,reduceType ty2)
	       handle ReduceType => false)
	  | eq(ty1 as CONty _, ty2 as VARty _) =
	      (eq(reduceType ty1, ty2)
	       handle ReduceType => false)
	  | eq(WILDCARDty,_) = true
	  | eq(_,WILDCARDty) = true
	  | eq _ = false
     in eq(prune ty, prune ty')
    end

local
    fun makeDummyType() =
	CONty(GENtyc{stamp = Stamps.newFree(),
		     path = [Symbol.tycSymbol "dummy"], arity = 0,
		     eq = ref YES, kind = ref(PRIMtyc)},
	      [])
    fun makeargs 0 = []
      | makeargs i = makeDummyType() :: makeargs(i-1)
    val args = makeargs 10
    fun dargs(0,_,d) = d
      | dargs(n,a::r,d) = dargs(n-1,r,a::d)
      | dargs(n,[],d) = dargs(n-1,[],makeDummyType()::d)
 in fun dummyargs n = dargs(n,args,[])
end

fun equalTycon(ERRORtyc,_) = true
  | equalTycon(_,ERRORtyc) = true
  | equalTycon(t1 as RELtyc _,t2) = t1=t2
  | equalTycon(t1,t2 as RELtyc _) = t1=t2
  | equalTycon(t1,t2) =
     let val a1 = tyconArity t1 and a2 = tyconArity t2
     in if a1<>a2 then false
        else
	  let val args = dummyargs a1
	  in equalType(mkCONty(t1,args),mkCONty(t2,args))
	  end
     end

(* instantiating polytypes *)

fun typeArgs n = 
    if n>0
    then mkMETAty() :: typeArgs(n-1)
    else []

val default_tvprop = {weakness=infinity,eq=false}

fun mkPolySign 0 = []
  | mkPolySign n = default_tvprop :: mkPolySign(n-1)

(* matching a scheme against a target type -- used declaring overloadings *)
fun matchScheme(TYFUN{arity,body}: tyfun, target: ty) : ty =
    let val tyenv = array(arity,UNDEFty)
	fun matchTyvar(i:int, ty: ty) : unit = 
	    case tyenv sub i
	      of UNDEFty => update(tyenv,i,ty)
	       | ty' => if equalType(ty,ty')
			then () 
			else impossible("this compiler was inadvertantly \
					\distributed to a user who insists on \
					\playing with 'overload' declarations.")
        fun match(scheme:ty, target:ty) =
	    case (prune scheme,prune(target))
	      of (WILDCARDty, _) => ()		(* Wildcards match any type *)
	       | (_, WILDCARDty) => ()		(* Wildcards match any type *)
	       | ((IBOUND i),ty) => matchTyvar(i,ty)
	       | (CONty(tycon1,args1), pt as CONty(tycon2,args2)) =>
		   if eqTycon(tycon1,tycon2)
		   then app2 match (args1, args2)
		   else (match(reduceType scheme, target)
			 handle ReduceType =>
			   (match(scheme, reduceType pt)
			    handle ReduceType =>
			      impossible "matchScheme: match -- tycons "))
	       | _ => impossible "matchScheme: match"
     in case prune target
	  of POLYty{sign,tyfun=TYFUN{arity=arity',body=body'},abs} =>
	       (match(body,body');
	        POLYty{sign = sign, abs=abs,
		       tyfun = TYFUN{arity = arity',
			             body = if arity>1
					    then
					tupleTy(ArrayExt.listofarray tyenv)
					    else tyenv sub 0}})
	   | ty => 
	       (match(body,ty);
	        if arity>1
		then tupleTy(ArrayExt.listofarray tyenv)
		else tyenv sub 0)
    end

val rec compressTy =
   fn t as VARty(x as ref(INSTANTIATED(VARty(ref v)))) =>
	(x := v; compressTy t)
    | VARty(ref(OPEN{kind=FLEX fields,...})) =>
	app (compressTy o #2) fields
    | CONty(tyc,tyl) => app compressTy tyl
    | POLYty{tyfun=TYFUN{body,...},...} => compressTy body
    | _ => ()

(*
 * 8/18/92: cleaned up occ "state machine" some and fixed bug #612.
 *
 * Note that Rator abbreviates Operator and Rand Operand.
 *
 * Known behaviour of the attributes about the context that are kept:
 *
 * lamd = # of Abstr's seen so far.  Starts at 0 with Root.
 *
 * top = true iff haven't seen a LetDef yet.
 *
 * rpending = # of Rator's pending.  Imagine Rator's as open parens and
 *            Abstr's as close parens.   Then this is (almost) the
 *            current nesting level.  Almost, because it is reset to 0
 *            on any operator other than a Rator or an Abstr and because
 *            it is never allowed to go negative.
 *
 * base = # of "non-canceled" Abstr's.  To determine which Abstr's are
 *        "canceled", take the original context and keep deleting
 *        Rator/Abstr pairs (Rator occurs first/highest) until can't
 *        do so any more.  Those Abstr's and Rator's that would be deleted
 *        by this process are said to be canceled.
 *
 * absd = To calculate absd, first delete Abstrs and Rators as in the
 *        case of base.  Additionally, delete Rators before Rands
 *        until no Rands are preceeded by Rators.  Then, absd is the
 *        # of Abstr's left - # of Rator's left.  Note that this can be
 *        negative unlike base.
 *
 * wmax = ?
 *)

abstype occ = OCC of {lamd: int, top: bool, rpending: int,
		      base: int, absd: int, wmax: int}
with

 val Root = OCC{lamd=0,top=true,rpending=0,base=0,absd=0,wmax=infinity}

 fun LetDef(OCC{lamd,base,absd,...}) =
	OCC{lamd=lamd,base=base,absd=absd,
	    top=false,rpending=0,wmax=infinity}

 fun Abstr(OCC{lamd,top,rpending,base,absd,wmax}) =
	OCC{top=top,wmax=wmax,
	    lamd=lamd+1,rpending=max(0,rpending-1),absd=absd+1,
		(* increment base if this Abstr is not canceled: *)
	    base=if rpending=0 then base+1 else base}

 fun Rator(OCC{lamd,top,rpending,base,absd,wmax}) =
	OCC{lamd=lamd,top=top,wmax=wmax,base=base,
	    rpending=rpending+1,absd=absd-1}

 fun Rand(OCC{lamd,top,rpending,base,absd,wmax}) =
	OCC{lamd=lamd,top=top,base=base,
	    rpending=0,absd=absd+rpending,wmax=min(wmax,absd+rpending)}

 fun lamdepth (OCC{lamd,...}) = lamd
 fun toplevel (OCC{top,...})  = top
 fun abscount (OCC{absd,...}) = absd 
 fun base     (OCC{base,...}) = base
 fun wmax     (OCC{wmax,...}) = wmax

fun calc_weakness(abs, (OCC{absd,base,wmax,...}):occ,
		weakness) =
        if weakness >= infinity then
	    infinity
	else
	    min(max(weakness+absd-abs,base),wmax)

(* return weakness such that any weakness larger than this
   can be safely generalized.  Equal and lower weakness levels
   can not be safely generalized.  *)

fun generalize_point(occ:occ) = calc_weakness(0,occ,0)

 (* can't be merged with typeInContext and used in varApplied, etc. *)
 fun applyPoly(POLYty{sign,tyfun,abs}, occ) : ty =
     let val args =
	     map (fn {weakness,eq} => 
		   VARty(ref(OPEN{kind=META,
				  weakness=calc_weakness(abs,occ,weakness),
				  depth = infinity,
				  eq = eq})))
		 sign
     in  applyTyfun(tyfun, args)
     end
   | applyPoly(ty,_) = ty

 (* instantiateType: ty * occ -> ty
    New function to instantiate type of applied occurrences of variables.
    Called in Typecheck.expType.
    This adjusts weakness of free META type variables in addition to 
    calculating the weakness of instantiations of the bound type variables.
 *)
 fun instantiateType(POLYty{sign,tyfun=TYFUN{body,...},abs}: ty,
		     occ) : ty =
     (* all variable types assumed to be polytypes.  This is so the
	abs value will be stored in the type for use here. *)
     let val args =
	     map (fn {weakness,eq} => 
		   VARty(ref(OPEN{kind=META,
				  weakness=calc_weakness(abs,occ,weakness),
				  depth = infinity,
				  eq = eq})))
		  sign
	 fun subst(IBOUND n) = nth(args,n)
	   | subst(ty as VARty(r as ref(OPEN{kind=META,weakness,depth,eq}))) =
	       let val newWeakness = calc_weakness(abs,occ,weakness)
	       in  (if newWeakness < weakness
		   then r := OPEN{kind=META,weakness=newWeakness,
				   depth=depth, eq=eq}
		   else ());
		   raise SHARE
	       end
	   | subst(CONty(tyc,args)) = CONty(tyc, shareMap subst args)
	   | subst(VARty(ref(INSTANTIATED ty))) = subst ty
	   | subst _ = raise SHARE
     in  subst body
	 handle SHARE => body
	      | Nth => impossible "instantiateType: bad POLYty (wrong arity)"
     end
   | instantiateType (WILDCARDty,_) = WILDCARDty
   | instantiateType (ty,_) = ty

end (* abstype occ *)

local 
  exception CHECKEQ
in
fun checkEqTySig(ty, sign: polysign) =
    let fun eqty(VARty(ref(INSTANTIATED ty))) = eqty ty
	  | eqty(CONty(DEFtyc{tyfun,...}, args)) =
	      eqty(applyTyfun(tyfun,args))
	  | eqty(CONty(GENtyc{eq,...}, args)) =
	     (case !eq
		of OBJ => ()
		 | YES => app eqty args
		 | NO => raise CHECKEQ
		 | IND => raise CHECKEQ
		 | _ => impossible "checkEqTySig")
	  | eqty(IBOUND n) = 
	      let val {eq,...} = nth(sign,n)
	       in if eq then () else raise CHECKEQ
	      end
	  | eqty _ = ()
     in eqty ty;
	true
    end
    handle CHECKEQ => false
end

exception CompareTypes
fun compType(specty, specsign:polysign, actty,
	     actsign:polysign, actarity): unit =
    let val env = array(actarity,UNDEFty)
	fun comp'(WILDCARDty, _) = ()
	  | comp'(_, WILDCARDty) = ()
	  | comp'(ty1, IBOUND i) =
	     (case env sub i
		of UNDEFty =>
		    (let val {weakness=aw,eq=ae} = nth(actsign,i)
		     in if aw < infinity
			then let fun checkweak(IBOUND n) =
					let val {weakness=sw,...} = nth(specsign,n)
					 in if sw > aw then raise CompareTypes
					    else ()
					end
				    | checkweak(CONty(_,args)) = app checkweak args
				    | checkweak (VARty(ref(INSTANTIATED ty))) =
					checkweak ty
				    | checkweak _ = impossible "compType/checkweak"
			      in checkweak ty1
			     end
			else ();
			if ae andalso not(checkEqTySig(ty1,specsign))
			then raise CompareTypes
			else ();
			update(env,i,ty1)
		    end handle Nth => ())
		 | ty => if equalType(ty1,ty)
			 then ()
			 else raise CompareTypes)
	  | comp'(ty1 as CONty(tycon, args), ty2 as CONty(tycon', args')) =
	      if eqTycon(tycon,tycon') then
		   app2 comp (args,args')
	      else
		   raise CompareTypes
	  | comp' _ = raise CompareTypes
        and comp(ty1,ty2) = comp'(headReduceType ty1, headReduceType ty2)
     in comp(specty,actty)
    end

fun compareTypes {spec : ty, actual: ty} : bool =
    let val actual = prune actual
     in case spec
	  of POLYty{sign,tyfun=TYFUN{body,...},...} =>
	      (case actual
		 of POLYty{sign=sign',tyfun=TYFUN{arity,body=body'},...} =>
		      (compType(body,sign,body',sign',arity); true)
		  | WILDCARDty => true
		  | _ => false)
	   | WILDCARDty => true
	   | _ =>
	      (case actual
		 of POLYty{sign,tyfun=TYFUN{arity,body},...} =>
		      (compType(spec,[],body,sign,arity); true)
		  | WILDCARDty => true
		  | _ => equalType(spec,actual))
    end handle CompareTypes => false

(* getRecTyvarMap : int * ty -> (int -> bool) 
 * see if a bound tyvar has occurred in some datatypes, e.g. 'a list. 
 * this is useful for representation analysis. 
 *)
fun getRecTyvarMap(n,ty) =
  let val s = Array.array(n,false)
      fun special(GENtyc{arity=0,...}) = false
        | special(tyc as (GENtyc _)) =
            if ((tyc = arrowTycon) orelse (tyc = contTycon)) then false 
            else true
        | special _ = false

      fun scan(b,(IBOUND n)) = if b then (update(s,n,true)) else ()
        | scan(b,CONty(tyc,args)) = 
           let val nb = (special tyc) orelse b
            in app (fn t => scan(nb,t)) args
           end
	| scan(b,VARty(ref(INSTANTIATED ty))) = scan(b,ty)
	| scan _ = ()
   in fn i => (Array.sub(s,i) handle Array.Subscript => 
                 impossible "Strange things in TypesUtil.getRecTyvarMap")
  end

end (* structure TypesUtil *)
