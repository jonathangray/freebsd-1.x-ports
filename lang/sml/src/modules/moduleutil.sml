structure ModuleUtil : MODULEUTIL = struct

local
  open Modules  ErrorMsg Variables Access Types TypesUtil PrintUtil Symbol;
  val DEBUG = true;
in

(* error given back by looking up function when they fail *)
exception UnboundComponent of spath
(* error given back by the same functions when an error structure is found *)
exception ErrorStructure

(* compiler bugs *)
fun error s = impossible ("ModuleUtil: "^s);

(* a symbol for an undefined structure variable *)
val bogusSTRvar =
  STRvar{name=Symbol.strSymbol "Bogus", access=SLOT 0, binding=ERROR_STR}

(* a symbol for an undefined functor variable *)
val bogusFCTvar = 
  FCTvar{name=Symbol.fctSymbol "Bogus", access=SLOT 0, binding=ERROR_FCT}

(* the last element of a list *)
fun last [x] = x
  | last nil = error "last"
  | last (h :: t) = last t

(* gives back the lvar (if any) declared by a binding *)
fun lvarOfBinding(VARbind(VALvar{access=PATH(p),...})) = SOME(last p)
  | lvarOfBinding(CONbind(DATACON{rep=VARIABLE(PATH(p)),...})) = SOME(last p)
  | lvarOfBinding(CONbind(DATACON{rep=VARIABLEc(PATH(p)),...})) = SOME(last p)
  | lvarOfBinding(STRbind(STRvar{name,access=PATH(p),...})) = SOME(last p)
  | lvarOfBinding(FCTbind(FCTvar{name,access=PATH(p),...})) = SOME(last p)
  | lvarOfBinding _ = NONE

val bogusCONbind = CONbind bogusCON

(* gets the stamp of a structure *)
fun getStrStamp (SIMPLE{stamp,...}) = stamp
  | getStrStamp (INSTANCE{origin=SELF stamp,...}) = stamp
  | getStrStamp (INSTANCE{origin=str,...})= getStrStamp str
  | getStrStamp (APPLY{res,...}) = getStrStamp res
  | getStrStamp _ = error "getStrStamp wrong origin"

(* gets the stamp of a signature *)
fun getSignStamp (SIG{stamp,...}) = stamp
  | getSignStamp _ = Stamps.null

(* gets the stamp of a functor *)
fun getFctStamp (FCT{stamp,...}) = stamp
  | getFctStamp (FCT_INSTANCE{fct,...}) = getFctStamp fct
  | getFctStamp ERROR_FCT = Stamps.null
  | getFctStamp _ = error "getFctStamp"


(* equality of signatures *)
fun eqSign (a,b) = getSignStamp a = getSignStamp b
(* equality of functor signatures (for printing only) *)
fun eqFsig (FSIG{argument=a1,body=b1,...},FSIG{argument=a2,body=b2,...}) =
      eqSign(a1,a2) andalso eqSign(b1,b2)
  | eqFsig _ = false

(* gets the origin of a structure *)
fun getOrigin (str as INSTANCE{origin = SELF _, ...}) = str
  | getOrigin (INSTANCE{origin, ...}) = origin
  | getOrigin str = str

(* equality of the origins of two structures (always true if one of the
   argument is unelaborated (ERROR_STR) *)
fun eqOrigin (x,y) =
  case (getOrigin x,getOrigin y)
  of (ERROR_STR,_) => true
   | (_,ERROR_STR) => true
   | (ox,oy) => getStrStamp ox = getStrStamp oy
 
(* basic function to make a new structure *)
fun mkStructure (env,path)=
  SIMPLE {stamp=Stamps.newFree(),env=env,path=path}

(* compose a thinning and a translation vector *)
fun compose (NONE,r) = r
  | compose (SOME(v,result),trans) =
      let val trans' : Access.access array =
	     Array.arrayoflist(map (fn VALtrans(access,_,_) => access
			       | _ => error "compose") trans)
	  fun replace (PATH [slot,lv]) = Array.sub(trans',slot)
	    | replace (access as INLINE _) = access
	    | replace _ = error "compose"
      in map (fn VALtrans(access,t,s) => VALtrans(replace access,t,s)
	       | THINtrans(access,t,l) => THINtrans(replace access,t,l)
	       | v => v) result
      end

(* appends two dynamic access path *)
fun appendAccess (SLOT s, l) = PATH (s :: l)
  | appendAccess (acc as INLINE _, l) = acc
  | appendAccess (acc,_) =
       (* hack so that we can print structures w/ bogus access paths *)
	    if !System.Control.internals then acc
	    else error "appendAccess"

(* translate a numeric path to the corresponding type in a given structure *)
fun transPosTycon str path =
    let fun f ([pos],INSTANCE{types,...}) = Array.sub(types,pos)
	  | f (h::t,INSTANCE{subStrs,...}) = f(t,Array.sub(subStrs,h))
	  | f (_,ERROR_STR) = ERRORtyc
	  | f (path,APPLY{res,...}) = f(path,res)
	  | f _ = error "transPosTycon 1"
     in f (path,str) handle Array.Subscript =>
           (System.Print.say "path: ";
	    PrintUtil.prIntPath path;
	    error "transPosTycon 2")
    end

(* translate a numeric path to the corresponding functor *)
fun transPosFct str path =
    let fun f ([pos],INSTANCE{subFcts,...}) = Array.sub(subFcts,pos)
	  | f (h::t,INSTANCE{subStrs,...}) =  f(t,Array.sub(subStrs,h))
	  | f (_,ERROR_STR) = ERROR_FCT
	  | f (path,APPLY{res,...}) = f(path,res)
	  | f _ = error "transPosFct 1"
     in f (path,str) handle Array.Subscript =>
	    (System.Print.say "path: ";
	     PrintUtil.prIntPath path;
	     error "transPosFct 2")
    end

(* translate a numeric acces path into the corresponding structure *)
fun transPosStr str path =
    let fun f ([],str) = str
	  | f (h::t,INSTANCE{subStrs,...}) = f(t,Array.sub(subStrs,h))
	  | f (p,APPLY{res,...}) = f(p,res)
	  | f (_,ERROR_STR) = ERROR_STR
	  | f _ = error "transPosStr 1"
     in f (path,str) handle Array.Subscript =>
	    (System.Print.say "path: ";
	     PrintUtil.prIntPath path;
	     error "transPosStr 2")
    end

(* translate a type in a given context *)
fun transType (str as INSTANCE {subStrs, types,...}) =
    let fun findFormTyc(path, tycIndex) = transPosTycon str (path@[tycIndex])

	fun transTycon (RELtyc {pos=tycAddress,...}) = findFormTyc tycAddress
	  | transTycon tyc = tyc

	fun transType0 ty =
	    case ty
	    of VARty _ => ty
	     | IBOUND _ => ty
	     | CONty (tc, tl) =>
		 mkCONty (transTycon tc, map transType0 tl)
	     | POLYty {sign, tyfun=TYFUN {arity, body}, abs} =>
		 POLYty{sign=sign,
			tyfun=TYFUN{arity=arity,body=transType0 body},
			abs=abs}
	     | UNDEFty => ty
	     | WILDCARDty => ty
     in transType0 end
  | transType (SIMPLE _) = (fn ty => ty)
  | transType (APPLY{res,...}) = transType res
  | transType _ = error "transtype"

(* transBindingINSTANCE: Structure array * tycon array * int list
			   -> Modules.binding -> Modules.binding

   The binding argument is assumed to be from the environment of
   signature of the structure (which must be an INSTANCE), so its access,
   if any, will be a SLOT, an OPEN, or INLINE.
   transBindingINSTANCE interprets types of value and constructor bindings,
   and adjusts access info to make it absolute (i.e. relative to the
   top-level dynamic environment).  The int list argument is the access
   path of the structure argument. *)

fun transBindingINSTANCE (str,subStrs,types,subFcts,apath) binding =
    let val transType = transType str
	(* invariant: Any structure binding in the sign of an
	   INSTANCE structure is a FORMAL *)
     in case binding
	of VARbind (VALvar {access, name, typ}) =>
		VARbind (VALvar {access=appendAccess (access, apath),
				 name=name,typ=ref(transType(!typ))})
	 | VARbind (OVLDvar _) => error "Modules.transBindingINSTANCE"
	 | VARbind ERRORvar => binding
	 | CONbind (DATACON {name, const, typ, rep, sign}) =>
	       CONbind (DATACON {name=name, const=const,
				 rep=
				 case rep
				 of VARIABLE access =>
				     VARIABLE (appendAccess(access,apath))
				  | VARIABLEc access =>
				     VARIABLEc (appendAccess (access,apath))
				  | _ => rep,
				 sign=sign,
				 typ=transType typ})
	 | TYCbind (FORMtyc {pos, ...}) => TYCbind (Array.sub(types,pos))
	 | TYCbind (OPENFORMtyc {pos=(path,pos), ...}) =>
	      TYCbind (transPosTycon str (path @ [pos]))
	 | SIGbind _ => binding
	 | STRbind (STRvar {name, access, binding=STR_FORMAL{pos, ...}}) =>
	       STRbind (STRvar {access=appendAccess (access, apath),
				name=name,binding=Array.sub(subStrs,pos)})
	 | STRbind (STRvar {name, access, binding=STR_OPEN{pos, ...}}) =>
	       STRbind (STRvar {access=appendAccess (access, apath),
				name=name,binding=transPosStr str pos})
	 | FCTbind (FCTvar {name, access, binding=FCT_FORMAL{pos, ...}}) =>
	       FCTbind (FCTvar {access=appendAccess (access, apath),
				name=name,binding=Array.sub(subFcts,pos)})
	 | FCTbind (FCTvar {name, access, binding=FCT_OPEN{pos, ...}}) =>
	       FCTbind (FCTvar {access=appendAccess (access, apath),
				name=name,binding=transPosFct str pos})
	 | _ => binding
   end

(* transBindingSIMPLE: int list -> Modules.binding -> Modules.binding
 * just adjusts access fields of bindings.  bindings assumed to come
 * from a SIMPLE structure, and the int list is its top-level access path 
 *)

fun transBindingSIMPLE apath binding =
    case binding
     of VARbind (VALvar {access, name, typ}) =>
	  VARbind (VALvar {access=appendAccess (access, apath),
			   name=name,typ=typ})
      | CONbind (DATACON {name, const, typ, rep, sign}) =>
	  CONbind (DATACON {name=name, const=const, sign=sign,
			    rep=
			    case rep
			    of VARIABLE access =>
				 VARIABLE(appendAccess (access, apath))
			     | VARIABLEc access =>
				 VARIABLEc(appendAccess(access,apath))
			     | rep => rep,
			    typ=typ})
      | STRbind (STRvar {name, access, binding}) =>
	  STRbind (STRvar {access=appendAccess (access, apath),
			   name=name,binding=binding})
      | FCTbind (FCTvar {name, access, binding}) =>
	  FCTbind (FCTvar {access=appendAccess (access, apath),
			   name=name,binding=binding})
      | binding => binding

(* convert formal bindings to opened, doesn't recompute types. *)

fun transBindingFORMAL (relpath,spath) binding = 
    case binding
    of TYCbind(FORMtyc{name,pos,spec}) =>
	     TYCbind(OPENFORMtyc{pos=(relpath,pos),spec=spec,
				 name=spath@[name]})
     | STRbind (STRvar{name,access,binding=STR_FORMAL{pos,spec}}) =>
	     STRbind(STRvar{name=name,access=access,
			    binding=STR_OPEN{pos=relpath@[pos],spec=spec,
					       name=spath@[name]}})
     | FCTbind (FCTvar{name,access,binding=FCT_FORMAL{pos,spec}}) =>
	     FCTbind(FCTvar{name=name,access=access,
			    binding=FCT_OPEN{pos=relpath@[pos],spec=spec,
					       name=spath@[name]}})
     | _ => binding

(* err: raise an exception when an unbound component in 
   the symbolic path is found.  It is passed the remainder
   of the symbolic path, including the unbound component.
   From this it computes the symbolic path to the unbound
   component. *)

fun err spath (r as (h::t)) =
      let fun g (0,_) = [h]
	    | g (i,h::t) = h :: g(i-1,t)
	    | g _ = error "err"
      in raise UnboundComponent(g(length spath-length r,spath)) end
  | err spath _ = error "spath"

(* find a binding, adjust its access paths and interpret its types.*)

fun lookBinding (topStr,spath,apath) : binding =
   let val err' = err spath
       fun get (str, [sym], apath) =
	((case str
	    of SIMPLE {env,...} =>
	       (transBindingSIMPLE apath (Env.look(env,sym)))
	     | INSTANCE {subStrs,types,subFcts,sign=SIG{env,...},...} =>
	       (transBindingINSTANCE(str,subStrs,types,subFcts,apath) 
					   (Env.look(!env,sym)))
	     | APPLY{res,...} => get (res,[sym],apath)
	     | ERROR_STR => raise ErrorStructure
	     | _ => (error ("lookBinding 1 "^Symbol.name sym)))
	  handle Env.Unbound => raise UnboundComponent spath)
	 | get (str, spath as (h::t), apath) =
	     let fun get_str str path =
		    (case str
		      of SIMPLE {env,...} =>
			  (case Env.look(env,h)
			   of STRbind(STRvar
				{access=SLOT slot,binding=str,...}) =>
				  (str,slot::path)
			    | _ => error "lookBinding 2 ")
		       | INSTANCE{sign=SIG{env,...},subStrs,...} =>
			  (case Env.look(!env,h)
			   of STRbind(STRvar{binding=STR_FORMAL{pos,...},
					     access=SLOT slot, ...}) =>
				(Array.sub(subStrs,pos),slot::path)
			    | STRbind(STRvar{binding=STR_OPEN{pos,...},
					     access=SLOT slot, ...}) =>
				(transPosStr str pos,slot::path)
			    | _ => (error "lookBinding 3"))
		       | APPLY{res,...} => get_str res path
		       | ERROR_STR => raise ErrorStructure
		       | _ => error "lookUnadjusted 2")
		     handle Env.Unbound => err' spath
		 val (str', apath') = get_str str apath
	      in get(str', t, apath')
	     end
	  | get _ = error "Modules.lookBinding DD95"
    in get (topStr,spath,apath)
    end

local
 fun lookBinding' (topStr,spath) : binding =
   let val err' = err spath
       fun get (str, [sym]) =
	((case str
	    of SIMPLE {env,...} =>
	         Env.look(env,sym)
	     | INSTANCE {subStrs,types,subFcts,sign=SIG{env,...},...} =>
	         transBindingINSTANCE(str,subStrs,types,subFcts,[]) 
					   (Env.look(!env,sym))
	     | APPLY{res,...} => get (res,[sym])
	     | ERROR_STR => raise ErrorStructure
	     | _ => (error ("lookBinding' 1 "^Symbol.name sym)))
	  handle Env.Unbound => raise UnboundComponent spath)
	 | get (str, spath as (h::t)) =
	     let fun get_str str =
		  (case str
		    of SIMPLE {env,...} =>
			(case Env.look(env,h)
			 of STRbind(STRvar{binding=str,...}) => str
			  | _ => error "lookBinding' 4")
		     | INSTANCE{sign=SIG{env,...},subStrs,...} =>
			(case Env.look(!env,h)
			 of STRbind(STRvar{binding=STR_FORMAL{pos,...},...}) =>
			      Array.sub(subStrs,pos)
			  | STRbind(STRvar{binding=STR_OPEN{pos,...}, ...}) =>
			      transPosStr str pos
			  | _ => error "lookBinding' 3")
		     | APPLY{res,...} => get_str res
		     | ERROR_STR => raise ErrorStructure
		     | _ => error "lookBinding' 2")
		   handle Env.Unbound => err' spath
		 val str' = get_str str
	      in get(str', t) end
	  | get _ = error "Modules.lookBinding DD95"
    in get (topStr,spath) end

in
(* lookBindingSTR: look up a structure binding, but don't adjust
   access paths.*)

fun lookBindingSTR (str,spath) =
    (case lookBinding' (str,spath)
     of STRbind str => str
      | _ => error "lookBindingSTR")
    handle ErrorStructure => bogusSTRvar

(* lookBindTYC: look up a type binding *)

fun lookBindingTYC (str,spath) =
    (case lookBinding' (str,spath)
     of TYCbind tyc => tyc
      | _ => error "lookBindingTYC")
    handle ErrorStructure => ERRORtyc
end

(* builds an environment from a structure *)
fun makeEnv (str as INSTANCE{sign=SIG{env,...},subStrs,types,subFcts, ...}, apath) =
      Env.open' (!env, transBindingINSTANCE(str,subStrs,types,subFcts,apath), Env.empty)
  | makeEnv (str as SIMPLE{env, ...}, apath) =
      Env.open' (env, transBindingSIMPLE(apath), Env.empty)
  | makeEnv (ERROR_STR, _) = Env.empty
  | makeEnv (INSTANCE{sign=FULL_SIG,...}, _) = 
      error "makeEnv 1"
  | makeEnv (APPLY{res,...},apath) = makeEnv (res,apath)
  | makeEnv _ = error "makeEnv 2"

(* should be in Symbol *)
val symbolToName = fn s => Symbol.nameSpaceToString(Symbol.nameSpace s)

(* look for a signature (necessaraly top) *)
fun lookSIG (env,id,err) = 
    (case Env.look(env,id) 
     of SIGbind(SIGvar{binding,...}) => binding
      | _ => error "lookSIG")
    handle Env.Unbound =>
	     (err COMPLAIN ("unbound signature: "^Symbol.name id) nullErrorBody;
	      ERROR_SIG)
	 | Bind =>
	      error 
		("lookSIG: bind exception looking up "
		 ^ Symbol.name id^" in name space "^symbolToName id)

(* look for a functor signature *)
fun lookFSIG (env,id,err) = 
    (case Env.look(env,id) 
     of  FSIGbind(FSIGvar{binding,...}) => binding
      | _ => error "lookFSIG")
    handle Env.Unbound =>
             (err COMPLAIN ("unbound funsig: "^Symbol.name id) nullErrorBody;
	      ERROR_FSIG)
	 | Bind =>
	     error
	       ("lookFSIG: bind exception looking up "
		^ Symbol.name id^" in name space "^symbolToName id)

(* fixity bindings *)

fun lookFIX (env,id) =
    let val binding = 
      case Env.look(env,id)
      of FIXbind(FIXvar{binding,...}) => binding
       | _ => error "lookFIX"
    in binding
    end handle Env.Unbound =>  Fixity.NONfix
	     | Bind => error ("lookFix: bind exception looking up "^Symbol.name id^" in name space "^symbolToName id)


(* lookFormalBinding: given a symbolic path, find a formal binding.
   Also return a relative path to it.*)

fun lookFormalBinding(env,spath) : binding * int list =
    let val err' = err spath
	fun get (env,[id],p) =
	      ((Env.look (env,id),rev p)
	       handle Env.Unbound => raise (UnboundComponent spath))
	  | get (env,spath as (first::rest),p) =
	     ((case Env.look (env,first)
	      of STRbind(STRvar{binding=STR_FORMAL{pos,
						   spec=SIG{env,kind,...}},
				...}) =>
		   get(!env,rest,
		       case !kind of EMBEDDED => p | _ => pos::p)
	       | STRbind(STRvar{binding=STR_FORMAL{spec=ERROR_SIG,...},...}) =>
		     raise ErrorStructure
	       | STRbind(STRvar{binding=ERROR_STR,...}) =>
		   raise ErrorStructure
	       | _ => error "lookFormalBinding 1")
	      handle Env.Unbound => err' spath)
	  | get _ = error "lookFormalBinding 2"
    in get (env,spath,[])
     end

(* lookGen: generic lookup function for identifiers which may occur 
   in:
       1. environments
       2. actual structure environments
       3. signature parsing environments *)

fun lookGen (extract,errorVal) (env,path,err) =
    (case path
     of [id] => extract ((Env.look(env,id),nil,path)
			handle Env.Unbound => raise UnboundComponent path)
      | first::rest =>
	  let val strvar = 
		(case Env.look(env,first)
		 of STRbind(STRvar strvar) => strvar
		  | obj =>  error "lookGen 3")
		handle Env.Unbound => raise UnboundComponent [first]
	  in case strvar
	     of {binding=str,access=PATH p,...} =>
		     extract (lookBinding (str,rest, p),nil,path)
	      | {binding=STR_FORMAL{pos,spec=SIG{env,kind,...}},...} =>
		let val (binding,relpath) = lookFormalBinding(!env,rest)
		in extract (binding,case !kind 
				    of EMBEDDED => relpath
				     | _ => pos :: relpath,path)
		end
	      | {binding=STR_OPEN{pos,spec=SIG{env,...},name},...} =>
		 let val (binding,relpath) = lookFormalBinding (!env,rest)
		 in extract (binding,pos @ relpath,name@rest)
		 end
	      | {binding=ERROR_STR,...} => raise ErrorStructure
	      | {binding=STR_FORMAL{spec=ERROR_SIG,...},...} =>
			       raise ErrorStructure
	      | _ => error "lookGen 1"
	   end
       | _ => error "lookGen 2")
    handle UnboundComponent spath => 
	  let val badsym = last spath
	  in err COMPLAIN ("unbound "^symbolToName badsym^": "^
			   Symbol.name badsym^
			   (case path
			      of _ :: _ :: _ => " in path "^formatQid path
			       | _ => ""))
	         nullErrorBody;
	     errorVal
	  end
	 | ErrorStructure => errorVal
	 | Bind => error ("bind exception: lookGen: looking up "^formatQid path^" as a "^symbolToName (last path))
	 | exn => raise exn

(* look for a variable or a constructor (simple path) *)
fun lookShortVARCON (arg as (env,name,err)) =
	  Env.look(env,name)
	  handle Env.Unbound => 
	       (err COMPLAIN ("unbound "^symbolToName name^" "^
			      Symbol.name name)
		    nullErrorBody;
		bogusCONbind)

(* look for a variable or a constructor (complete path) *)
val lookVARCON = lookGen (fn (x,_,_) => x,bogusCONbind)

(* look for a structure *)
val lookSTR = lookGen (fn (STRbind sv,_,_) => sv
			| _ => error "lookSTR",
		       bogusSTRvar)

(* look for a functor *)
val lookFCT = lookGen (fn (FCTbind sv,_,_) => sv
			| _ => error "lookSTR",
		       bogusFCTvar)

(* look for a type *)
val lookTYC =
    lookGen (fn (TYCbind (FORMtyc{pos,spec,...}),relpath,name) =>
		      RELtyc{name=name,pos=(relpath,pos)}
	       | (TYCbind tyc,_,_)=> tyc
	       | _ => error "lookTYC",
	     ERRORtyc)

(* tycon lookup with arity checking *)

fun checkArity(tycon, arity,err,result) =
    case tycon
    of ERRORtyc => result
     | _ =>
       if tyconArity(tycon) <> arity
       then (err COMPLAIN ("type constructor "^(Symbol.name(tycName(tycon)))^
		     " has the wrong number of arguments: "^makestring arity)
	       nullErrorBody;
	    ERRORtyc)
       else result

fun lookArTYC ((env,normEnv),qid: symbol list, arity: int, err) =
    let val normQid = Normalize.normalize(normEnv,qid)
    in
    lookGen (fn (TYCbind (FORMtyc {pos,spec,...}),relpos,name) =>
			checkArity(spec,arity,err,
				 RELtyc{name=name,pos=(relpos,pos)})
	      | (TYCbind (OPENFORMtyc {pos,spec,name,...}),[],_) =>
			checkArity(spec,arity,err,
				 RELtyc{name=name,pos=pos})
	      | (TYCbind (OPENFORMtyc _),_,_) =>
			 error "lookArTyc 1"
	      | (TYCbind tyc,_,_) => checkArity(tyc,arity,err,tyc)
	      | _ => error "lookArTyc 2",
	    ERRORtyc)  (env,normQid,err)
    end

(* looking for an exception *)
fun lookEXN (env,path,err) =
    let val binding =
	  case path
	  of [id] => (Env.look(env,id)
		      handle Env.Unbound => raise UnboundComponent path)
	   | first::rest =>
		 (let val binding = case Env.look(env,first)
		    of STRbind(STRvar binding) => binding
		     | _ => error "ModuleUtl.lookExn 3"
		  in case binding
		      of {binding=str,access=PATH p,...} =>
			       (lookBinding (str,rest, p))
		       | {binding=ERROR_STR,...} => raise ErrorStructure
		       | _ => error "lookExn 1"
		  end
		  handle Env.Unbound => raise UnboundComponent [first])
	   | _ => error "lookExn 2"
   in case binding
      of CONbind c =>
	   (case c
	    of DATACON {rep=VARIABLE _,...} => c
	     | DATACON {rep=VARIABLEc _,...} => c
	     | _ => (err COMPLAIN ("found data constructor \
				  \instead of exception")
			nullErrorBody;
		     bogusEXN))
       | VARbind _ =>
	      (err COMPLAIN ("found variable instead of exception")
	         nullErrorBody;
	       bogusEXN)
       | _ => error ("lookEXN: looking up " ^formatQid path
		   ^ " as a " ^ symbolToName (last path))
   end
    handle UnboundComponent spath => 
	    (err COMPLAIN ("unbound " ^
			  (if length path=length spath then "exception "
			   else "structure ") ^
			  Symbol.name (last spath)^
			  (case path
			   of _ :: _ :: _ => " in path "^formatQid path
			    | _ => ""))
	       nullErrorBody;
	     bogusEXN)
	 | ErrorStructure => bogusEXN
	 | exn => raise exn

fun openSigStructure (bindEnv,spath,baseEnv,complainer) =
  let fun makeEnv (str as INSTANCE{sign=SIG{env,...},subStrs,types,subFcts, ...},_,_) =
	   Env.open' (!env, transBindingINSTANCE(str,subStrs,types,subFcts,[]), baseEnv)
	| makeEnv (str as SIMPLE{env, ...},_,_) =
	     Env.open' (env, fn x =>x, baseEnv)
	| makeEnv (ERROR_STR, _,_) = Env.empty
	| makeEnv (STR_FORMAL{pos,spec=SIG {env,kind,...}},relpath,name) =
	     let val relpath' = case !kind
				of EMBEDDED => relpath
				 | _ => relpath @ [pos]
	     in Env.open'(!env,transBindingFORMAL (relpath',name),baseEnv)
	     end
	| makeEnv (STR_FORMAL{spec=ERROR_SIG,...},_,_) = baseEnv
	| makeEnv (STR_OPEN{pos,spec=SIG{env,...},name},nil,_) =
	      Env.open'(!env,transBindingFORMAL (pos,name),baseEnv)
	| makeEnv (STR_OPEN{spec=ERROR_SIG,...},nil,_) = baseEnv
	| makeEnv (STR_OPEN _,_,_) =
	      error "openSigStructure.makeEnv.OPENFORMAL"
	| makeEnv _ = error "openSigStructure.makeEnv"
  in lookGen(fn (STRbind (STRvar{binding,...}),relpath,name) =>
		   makeEnv(binding,relpath,name)
	      | _ => error "openSigStructure",
	      bindEnv) (bindEnv,spath,complainer)
  end

fun openStructureVar (env,STRvar{access=PATH p,binding=str,...}) : env =
      Env.atop (makeEnv (str, p), env)
     (* will generate spurious error messages unless we give up completely *)
  | openStructureVar (env,STRvar{binding=ERROR_STR,...}) = env 
  | openStructureVar _ = error "openStructureVar"

(* findPath:  convert symbolic path names to a printable string in the
  context of an environment.

  Its arguments are the path name in reverse order, a static semantic value,
  an equality function on static semantic values, and a lookup function
  mapping paths to their bindings (if any) in an environment.   The second
  argument of the lookup function is a function which is called if there
  is no binding for a path name in the environment.

  It looks up each suffix of the path name, going from shortest to longest
  suffix,in the current environment until it finds one whose lookup value
  equals the static semantic value argument.  It then converts that suffix
  to a string.  If it doesn't find any suffix, it returns "?" concatenated
  with the full path name.

  Example:
	 Given A.B.t as a path, and a lookup function for an
	 environment, this function tries:
		   t
		   B.t
		   A.B.t
	 If none of these work, it returns ?.A.B.t

  Note: the symbolic path is passed in reverse order because that is
  the way all symbolic path names are stored within static semantic objects.
 *)
    
fun findPath (p,elem0,eq,look) =
  let fun try(name::untried,tried) =
	    (let val elem = look (name :: tried,fn _ => raise Env.Unbound)
	     in if eq(elem0,elem)
		then formatQid(name::tried)
		else try(untried,name::tried)
	      end handle Env.Unbound => try(untried,name::tried))
	| try([],tried) = "?." ^ formatQid tried
   in try(p,[])
  end

(* sortEnvBindings: sort the bindings in an environment for printing
  purposes.  The bindings are sorted in the following order:
	     signatures
	     functors
	     structures
	     types
	     constructors
	     values
	     fixity declarations
 It is only correct to sort environments which have no duplicate bindings.
 All routines which build structure environments maintain this
 invariant, so it is ok to sort any structure environment using
 this function.
*)

local
  open Symbol
   fun binderGt(bind1: symbol * Modules.binding,
		bind2: symbol * Modules.binding) =
    case (bind1,bind2)
      of ((n1,FIXbind _),(n2,FIXbind _)) => symbolGt(n1,n2)
       | ((_,FIXbind _),_) => true
       | (_,(_,FIXbind _)) => false
       | ((n1,VARbind _),(n2,VARbind _)) => symbolGt(n1,n2)
       | ((_,VARbind _),_) => true
       | (_,(_,VARbind _)) => false
       | ((n1,CONbind _),(n2,CONbind _)) => symbolGt(n1,n2)
       | ((_,CONbind _),_) => true
       | (_,(_,CONbind _)) => false
       | ((n1,TYCbind _),(n2,TYCbind _)) => symbolGt(n1,n2)
       | ((_,TYCbind _),_) => true
       | (_,(_,TYCbind _)) => false
       | ((n1,STRbind _),(n2,STRbind _)) => symbolGt(n1,n2)
       | ((_,STRbind _),_) => true
       | (_,(_,STRbind _)) => false
       | ((n1,FCTbind _),(n2,FCTbind _)) => symbolGt(n1,n2)
       | ((_,FCTbind _),_) => true
       | (_,(_,FCTbind _)) => false
       | ((n1,SIGbind _),(n2,SIGbind _)) => symbolGt(n1,n2)
       | ((_,SIGbind _),_) => true
       | (_,(_,SIGbind _)) => false
       | ((n1,FSIGbind _),(n2,FSIGbind _)) => symbolGt(n1,n2)
in
  fun sortEnvBindings env =
       let val bl : (Symbol.symbol * Modules.binding) list ref = ref nil
       in Env.app(fn b => bl := b :: !bl) env;
	  Sort.sort binderGt (!bl)
       end
end

  (* notInitialLowerCase:  this function not currently used.  It could be
     used to detect anomalous noncapitalization of constructors. *)

  fun notInitialLowerCase string =
      (* string does NOT start with lower-case alpha *)
      let val firstchar = ordof(string,0)
       in firstchar < Ascii.lc_a orelse firstchar > Ascii.lc_z
       end

fun getStrPath (SIMPLE {path, ...}) = path
  | getStrPath (INSTANCE {path,...}) = path
  | getStrPath _ = []

fun getStrPos str (sym: Symbol.symbol) = 
    case lookBindingSTR(str,[sym]) of
	(STRvar {binding=(STR_FORMAL {pos,...}),...}) => pos
      | _ => error "Instantiate:getStrPos"

fun getStrTPos str sym =
    case lookBindingTYC (str,[sym]) of
	(FORMtyc {pos, ...}) => pos
      | _ => error "Instantiate:getStrTPos"

fun getSigPosGen (SIG {env, ...}) sym =
       (Env.look(!env,sym)
        handle Env.Unbound => error "instantiate:getSigPos 1")
  | getSigPosGen _ _ = error "instantiate:getSigPos 2"

fun getSigTPos (SIG {env, ...}) sym =
    (case Env.look(!env,sym) of
	 TYCbind (FORMtyc {pos, ...}) => pos
       | _ => error "instantiate:getSigTPos.1")
  | getSigTPos _ _ = error "instantiate:getSigPos.2"

fun getSigPos (sign as SIG {env, ...}) sym =
   ((case Env.look(!env,sym) of
	 (STRbind (STRvar {binding=STR_FORMAL {pos, ...},...})) => pos
       | _ => 
           error "Instantiate:getSigPos.1")
    handle Env.Unbound => 
      (error ("Instantiate:getSigPos.2"^(Symbol.name sym))))
  | getSigPos _ _ = error "Instantiate:getSigPos.2"

fun eqSignature (SIG {stamp=s1,...}, SIG {stamp=s2,...}) = s1=s2
  | eqSignature _ = false

end
end  (* structure ModuleUtil *)
