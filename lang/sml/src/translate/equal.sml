(* Copyright 1989 by AT&T Bell Laboratories *)
(* equal.sml *)
signature EQUAL = sig val equal : Modules.env -> Types.ty -> Lambda.lexp
                  end

structure Equal : EQUAL = 

struct

open ErrorMsg Types Lambda Access BasicTypes TypesUtil EqTypes Transtypes
open PrettyPrint

fun transDcon(DATACON{name,rep,typ,...}) = (name,rep,transTyLty typ)
val trueDcon' = transDcon trueDcon
val falseDcon' = transDcon falseDcon

fun translatepath [v] = VAR v
  | translatepath (x::p) = SELECT(x,translatepath p)
  | translatepath nil = ErrorMsg.impossible "equal.translatepath nil"

fun argType(POLYty{tyfun=TYFUN{arity,body=CONty(_,[domain,_])},...}, args) =
      applyTyfun(TYFUN{arity=arity,body=domain},args)
  | argType(CONty(_,[argty,_]), []) = argty
  | argType _ = impossible "Equal.argType"


exception Poly

val inteqty = ARROWty(RECORDty[INTty,INTty],BOOLty)
val booleqty = ARROWty(RECORDty[BOOLty,BOOLty],BOOLty)
val realeqty = ARROWty(RECORDty[REALty,REALty],BOOLty)
val boxedeqty = ARROWty(RECORDty[BOXEDty,BOXEDty],BOOLty)

fun atomeq tyc =
    if eqTycon(tyc,intTycon) 
    then PRIM(P.IEQL,inteqty)
    else if eqTycon(tyc,boolTycon) then PRIM(P.IEQL,booleqty) 
    else if eqTycon(tyc,refTycon) then PRIM(P.IEQL,boxedeqty) 
    else if eqTycon(tyc,arrayTycon) then PRIM(P.IEQL,boxedeqty)
    else if eqTycon(tyc,realTycon) then PRIM(P.FEQLd,realeqty)
    else if eqTycon(tyc,stringTycon) then 
      translatepath(!CoreInfo.stringequalPath)
    else raise Poly

exception Notfound

val trueLexp = CON(trueDcon',RECORD[])
val falseLexp = CON(falseDcon',RECORD[])

fun eqType(ty,ty') =
    let fun eq(ty as CONty(tycon, args), ty' as CONty(tycon', args')) =
	    (case tycon
	      of RELtyc _ => raise Poly
	       | _ =>
		 (case tycon'
		    of RELtyc _ => raise Poly
	             | _ =>
		       if eqTycon(tycon, tycon')
		       then List2.all2 eqType(args,args') 
		       else (eqType(reduceType ty, ty')
			     handle ReduceType =>
				 (eqType(ty,reduceType ty')
				  handle ReduceType => false))))
	  | eq(VARty _, _) = raise Poly
	  | eq(_, VARty _) = raise Poly
	  | eq(POLYty _, _) = raise Poly
	  | eq(_, POLYty _) = raise Poly
	  | eq _ = false
     in eq(prune ty, prune ty')
    end

fun equal (env:Modules.env) (concreteType : ty) : lexp =
    let val cache : (ty * lexp * lexp ref) list ref = ref nil
	fun enter ty =
	    let val v = VAR(mkLvar())
		val r = ref v
	     in if !System.Control.debugging 
		then with_pp (ErrorMsg.defaultConsumer())
		      (fn ppstrm =>
		       (add_string ppstrm "enter: ";
			PPType.resetPPType();
			PPType.ppType env ppstrm ty))
		else ();
		cache := (ty, v, r) :: !cache; (v,r)
	    end
	fun find ty =
	    let fun f ((t,v,e)::r) =
		      if eqType(ty,t)
		      then v
		      else f r
		  | f nil = (if !System.Control.debugging
			      then System.Print.say "find-notfound\n"
			      else ();
			     raise Notfound)
	     in if !System.Control.debugging 
		then with_pp (ErrorMsg.defaultConsumer())
		      (fn ppstrm =>
		       (add_string ppstrm "find: ";
			PPType.resetPPType();
			PPType.ppType env ppstrm ty))
		else ();
		f (!cache)
	    end

	fun test(ty) =
	(if !System.Control.debugging
	 then with_pp (ErrorMsg.defaultConsumer())
	       (fn ppstrm =>
		(add_string ppstrm "test: ";
		 PPType.resetPPType();
		 PPType.ppType env ppstrm ty))
	 else ();
	 case ty
	  of VARty(ref(INSTANTIATED t)) => test t
	   | CONty(tyc as GENtyc{kind=ref(PRIMtyc),eq=ref YES,...}, tyl) =>
	       atomeq tyc
	   | CONty(GENtyc{kind=ref(ABStyc tyc),eq=ref NO,...}, tyl) =>
	       test(mkCONty(tyc,tyl))
	       (* assume that an equality datatype has been converted
		  to an ABStyc in an abstype declaration *)
	   | CONty(DEFtyc _, _) => test(reduceType ty)
	   | CONty(tyc as
		    GENtyc{kind=ref(DATAtyc [DATACON{const=false,rep,typ,...}]),
		             ...},
		   tyl) =>
	       (case rep
		  of TRANSPARENT => 
		       (find ty handle Notfound =>
			 let val (eqv,patch) = enter ty
			     val v = mkLvar()
	                     val ty' = argType(typ,tyl)
                             val lt = transTyLty(ty')
			  in patch := FN(v,RECORDty[lt,lt],
                                         APP(test ty', VAR v));
			     eqv
		         end)
		   | REF => atomeq tyc
		   | _ => impossible "Equal #498")
	   | CONty(RECORDtyc _, tyl) =>
	      (find ty
	       handle Notfound =>
	       let val v = mkLvar() and x=mkLvar() and y=mkLvar()
		   val (eqv, patch) = enter ty
		   fun loop(n,[ty]) = APP(test(ty), RECORD[SELECT(n, VAR x),
					  	           SELECT(n, VAR y)])
	  	     | loop(n,ty::r) = SWITCH(loop(n,[ty]), boolsign,
				         [(DATAcon(trueDcon'), loop(n+1,r)),
					  (DATAcon(falseDcon'), falseLexp)],
				         NONE)
		     | loop(_,nil) = trueLexp
                   val lt = transTyLty(ty)
		in patch := FN(v,RECORDty[lt,lt],
                               APP(FN(x,BOGUSty,
                                        APP(FN(y,BOGUSty,loop(0,tyl)),
					    SELECT(1,VAR v))),
				   SELECT(0,VAR v)));
		   eqv
	       end)
	   | CONty(GENtyc{kind=ref(DATAtyc dcons),...}, tyl) =>
	      (find ty
	       handle Notfound =>
	       let val v = mkLvar() and x=mkLvar() and y=mkLvar()
		   val (eqv, patch) = enter ty
		   fun inside (DATACON{const=true,...}) = trueLexp
		     | inside (c as DATACON{typ, const=false,...}) =
			 APP(test(argType(typ,tyl)),
			     RECORD[DECON(transDcon c, VAR x),
				    DECON(transDcon c, VAR y)])
                   val lt = transTyLty ty
                   val argty = RECORDty[lt,lt]
                   val pty = ARROWty(argty,BOOLty)
		   val body = 
		       case dcons
			 of [dcon] => inside dcon	
			  | DATACON{sign,...}::_ =>
			      let fun concase dcon =
				     let val dcon' = DATAcon(transDcon dcon)
				      in (dcon',
				          SWITCH(VAR y, sign, 
						 [(dcon', inside dcon)],
						 SOME(falseLexp)))
				     end
			       in SWITCH(VAR x,sign,map concase dcons,NONE)
			      end
		   val body = SWITCH(APP(PRIM(P.IEQL,pty), 
                                         RECORD[VAR x, VAR y]),
				      boolsign,
                                      [(DATAcon(trueDcon'), trueLexp),
                                       (DATAcon(falseDcon'), body)],
                                      NONE)
		in patch := FN(v,argty,APP(FN(x,BOGUSty,
                                                APP(FN(y,BOGUSty,body),
					        SELECT(1,VAR v))),
				           SELECT(0,VAR v)));
		   eqv
	       end)
	   | _ => raise Poly)

	val body = test(concreteType)

     in FIX(map (fn (_,VAR v,_) => v | _ => impossible "Equal #324") (!cache),
            map (fn (ty,_,_) => let val lt = transTyLty(ty) 
                                 in ARROWty(RECORDty[lt,lt],BOOLty)
                                end) (!cache),
	    map (fn (_,_,e) => !e) (!cache),
	    body)
    end
    handle Poly => translatepath(!CoreInfo.polyequalPath)
		
end (* struct *)

