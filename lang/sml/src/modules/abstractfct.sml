(***************************************************************************

  ABSTRACTFCT.SML takes a structure representing the application of a
  functor to an argument (also provided) and gives back the recepee to
  perform this application.

 ***************************************************************************)

signature ABSTRACTFCT =
  sig
    val abstractBody : 
       Modules.Structure * Modules.Structure * 
       (Stamps.stamp -> bool) * (Stamps.stamp -> bool)
       -> {strseq : Modules.Structure list, fctseq : Modules.Functor list,
           tyseq : Types.tycon list,
	   str : Modules.Structure,
           fullstr : Modules.Structure}
  end

structure AbstractFct: ABSTRACTFCT = struct

open ErrorMsg Types Modules ModuleUtil Stamps Variables
val DEBUG = false
val say = System.Print.say

fun err s = impossible ("abstractfct:"^s);

exception Member;
exception NotFound;

fun next i = (!i before inc i);

fun appArray f = 
  let fun app i = (f i;app (i+1)) handle Array.Subscript => ()
  in app 0 end

(***** MAPS FOR MEMOIZING STRUCTURES AND FUNCTORS *****)

(* types for map cells. map are used to memoize the positions of structures
   and functors that have already been encountered *)

type memoizeCell = {instance:(stamp * Structure) list,raw:Structure option}
type memoizeMap = memoizeCell stampMap
 and memoizeFctMap = Functor stampMap
 and memoizeTycMap = tycon stampMap
;

(* add a new instance in the map of signature sign and position pos str 
   is the origin (should be a simple) *)

fun add_instance str sign pos (map:memoizeMap) =
  (let val {instance,raw} = applyMap (map,str)
   in updateMap map (str,{instance = (sign,pos)::instance, raw = raw}) end)
  handle Member =>
    updateMap map (str,{instance=[(sign,pos)],raw=NONE})
;

(* add a simple structure to the map *)

fun add_simple str pos (map:memoizeMap) =
  (let val {instance,raw} = applyMap (map,str)
   in case raw 
      of NONE => updateMap map (str,{instance = instance, raw = SOME pos}) 
      (* We want to keep the first path we have found to get a structure and
       * we don't want to redefine it. *)
       | _ => ()
   end)
  handle Member =>
    updateMap map (str,{instance=[],raw= SOME pos})
;

(* gets an instance from the map *)

fun get_instance str sign (map:memoizeMap) =
  (let val {instance,raw} = applyMap (map,str)
       fun find [] = raise NotFound
         | find ((s,pos)::l) = 
             if s=sign then pos else find l
   in find instance end)
  handle Member => raise NotFound
;

(* gets a simple structure from the map *)

fun get_simple str (map:memoizeMap) =
  (let val {instance,raw} = applyMap (map,str)
   in case raw 
      of NONE => raise NotFound
       | SOME str => str
   end)
  handle Member => raise NotFound
;

(***************************************************************************
   enrichMap: create mappings from stamps verifying ifCorrect to locations
   in the structure they are storecd using the function full_path that can
   be 
    - (fn x => PARAM x) when traversing the parameter
    - (fn x => SEQind(pos,x)) when traversing the result of a functor 
               application which will be stored in the pos slot

   Note that we do no asumption on the parameter: it can contains any kind
   of structures: No assumption can be made on Parents and result of functor
   application, we use it on more general structures than signature 
   instantiation (assumption made in the original version)
 ***************************************************************************)

fun enrichMap (isCorrect,strStampMap,(fctStampMap:memoizeFctMap),tycStampMap,
               (full_path:int list -> absfbpos)) =
  let fun scanTyc (typ,loc) =
        (case typ
         of GENtyc{stamp,...} =>
	      if isCorrect stamp
              then (Stamps.applyMap(tycStampMap,stamp); ())
		      handle Member =>
		        (Stamps.updateMap 
                           tycStampMap 
                           (stamp,ABSFBtyc (full_path (rev loc))))
              else ()
	  | _ => ())
      fun scanStr (ERROR_STR,_,strArray) = ()
        | scanStr(s as INSTANCE{sign,subStrs,subFcts,types,origin,...},
		  loc,strArray) =
	    let val strStamp = getStrStamp s
	        val signStamp = getSignStamp sign
		(* ugly but works: colapses paths if on the same array *)
		val newLoc = 
		  case (loc,strArray)
		  of (posStr::endloc,SOME subStrs') => 
                        if subStrs=subStrs' then endloc else loc
		   | _ => loc
 	    in
            if isCorrect strStamp
	    then (
	      (if DEBUG then
		 print("INSTANCE "^(Stamps.stampToString strStamp)^"/"^
		       (Stamps.stampToString signStamp)^"\n") 
	       else ();
	       get_instance strStamp signStamp strStampMap;())
	      handle NotFound => (
		add_instance strStamp signStamp 
			     (STR_ABSFB(full_path(rev loc))) strStampMap;
		case origin 
		of SELF _ => ()
		 | _ => scanStr (origin,loc,strArray);
		case sign 
		of SIG{kind=ref EMBEDDED,...} => ()
		 | _ => (
		   appArray (fn pos => scanStr(Array.sub(subStrs,pos),
					       pos::newLoc,SOME subStrs));
		   appArray (fn pos => scanFct(Array.sub(subFcts,pos),
					       pos::newLoc));
		   appArray (fn pos => scanTyc(Array.sub(types,pos),
					       pos::newLoc)))))
	    else ()
	    end
        | scanStr(s as SIMPLE{env,...},loc,strArray) =
            let val strStamp = getStrStamp s in
            if isCorrect strStamp then (
	      if DEBUG then
	        print("SIMPLE "^(Stamps.stampToString strStamp)^"\n")
              else ();
              add_simple strStamp (STR_ABSFB(full_path(rev loc))) strStampMap)
            else () end
        | scanStr (APPLY{res,...},loc,_) = scanStr (res,loc,NONE)
	| scanStr (STR_ABSFB _,_,_) = ()
	| scanStr (str,_,_) = 
           let
           fun C f x y = f y x
           in  PrettyPrint.with_pp (ErrorMsg.defaultConsumer())
                (C PPBasics.ppStructure (Env.empty,str,20));
	       err "scanStr"
           end
      and scanFct (FCT{stamp,...},loc) =
             if isCorrect stamp then
               updateMap fctStampMap (stamp,FCT_ABSFB(full_path(rev loc)))
             else ()
        | scanFct (FCT_INSTANCE{fct,fsig,parent},loc) = scanFct(fct,loc)
        | scanFct (ERROR_FCT,_) = ()
        | scanFct (FCT_FORMAL _,_) = err "scanFct: formal fct"
	| scanFct (FCT_OPEN _,_) = err "scanFct: open fct"
        | scanFct (FCT_ABSFB _,_) = ()
  in fn p => scanStr (p,[],NONE) end
;

val bogusTycSym = Symbol.tycSymbol "?.bogus"
val bogusName = [bogusTycSym]


(* the main function: it looks at environment in the order of their
   definition to ensure that maps are correctly defined while looking at
   them (problem of functor application). Memoization and processing is
   done in a single function (the previous version was rather weird
 *)

fun abstractBody (body,param,inBody: stamp -> bool,
		  inParam: stamp -> bool) =
let
   val tycCount = ref 0
   val strCount = ref 0
   val fctCount = ref 0
   val tycSeries : tycon list ref = ref nil
   val strSeries : Structure list ref = ref nil
   val fctSeries : Functor list ref = ref nil

   fun addTyc tyc = (tycSeries := tyc :: !tycSeries;
		     ABSFBtyc(SEQ (next tycCount)))
   fun addStr str = (strSeries := str :: !strSeries;
		     STR_ABSFB(SEQ (next strCount)))
   fun addFct fct = (fctSeries := fct :: !fctSeries;
		     FCT_ABSFB(SEQ (next fctCount)))

   val strMap = Stamps.newMap Member : memoizeMap
   val fctMap = Stamps.newMap Member : memoizeFctMap
   val paramTycMap = Stamps.newMap Member : memoizeTycMap
   val _ = enrichMap (inParam,strMap,fctMap,paramTycMap,fn s => PARAM s) param

   (* apply a function f to all DEFtycs and to all GENtycs with
      parameter or body stamps *)

   fun memoTyc (f : tycon * (tycon -> tycon) * (tycon -> unit) -> tycon) =
       let val tycMap = Stamps.newMap Member : tycon Stamps.stampMap
	   val defTycs : (tycon * tycon) list ref = ref nil
           fun findDefTyc deftyc =
	        let fun find ((tyc,elem) :: r) =
		          if tyc=deftyc then elem else find r
		      | find nil =
			  f(deftyc,rewriteTycon,
			    fn r => defTycs := (deftyc,r) :: !defTycs)
                in find(!defTycs) end
           and rewriteTycon tyc =
		  case tyc
		  of DEFtyc _ => findDefTyc tyc
		   | GENtyc{stamp,path=spath,...} =>
			if inParam stamp orelse inBody stamp then
			  (Stamps.applyMap(tycMap,stamp) handle Member =>
			    f(tyc,rewriteTycon,
			      fn r =>Stamps.updateMap tycMap (stamp,r)))
			else tyc
		    | tyc => tyc
	in (rewriteTycon,tycMap) end
   fun rewriteType (ty,memo) =
       let fun f ty =
           case ty
	   of VARty(ref(INSTANTIATED ty')) => f ty'
	    | POLYty{tyfun=TYFUN{body,arity},sign,abs} =>
	             POLYty{tyfun=TYFUN{body=f body,arity=arity},
			    sign=sign,abs=abs}
	    | VARty _ => ty
	    | IBOUND _ => ty
	    | CONty(tyc,args) => 
                  TypesUtil.mkCONty(memo tyc,map f args)
	    | WILDCARDty => ty
	    | _ => err "abstractBody.abstractType 2"
       in f ty
       end

    fun tyconSeries1 (tyc,memo,add) =
      case tyc
      of DEFtyc {path,strict,tyfun as (TYFUN{arity,body})} =>
	let val r =
	    let val body' = rewriteType(body,memo)
            in if body<>body' then 
                  addTyc(DEFtyc{path=path,
				strict=strict,
				tyfun=TYFUN{arity=arity,body=body'}})
                else tyc
            end
        in add r;
	   r
	end
      | GENtyc{stamp,kind,...} =>
          if inParam stamp then
	     Stamps.applyMap(paramTycMap,stamp)
	     handle Member =>
                err "tyconSeries1: param tycon not found"
          else if inBody stamp then
	    let val r = addTyc tyc
	    in add r;
	       case kind
	       of ref (DATAtyc dcons) =>
	         kind := DATAtyc
		        (map (fn DATACON {name,const,typ,rep,sign} =>
		               DATACON{name=name,const=const,rep=rep,
				       sign=sign,typ=rewriteType(typ,memo)})
			 dcons)
                | _ => ();
               r
            end
          else tyc
       | tyc => tyc

    val (tyconSeries,tycMap) = memoTyc tyconSeries1

    fun relativizeSimpleEnv env =
      let val extrastrs = ref (nil : Structure list)
          val extratycs = ref (nil : tycon list)
          val extrafcts = ref (nil : Functor list)
          val strcount = ref 0
          val tyccount = ref 0
          val fctcount = ref 0
          fun addstr str =
            (extrastrs := str :: !extrastrs;
	       STR_FORMAL{pos=next strcount,spec=FULL_SIG})
          fun addtyc(tyc,memo,add) =
            let val r = RELtyc {name=bogusName,pos=([],next tyccount)}
	    in extratycs := tyc :: !extratycs; add r; r end
          fun addfct fct =
            (extrafcts := fct :: !extrafcts;
             FCT_FORMAL{pos=next fctcount,spec=FULL_FSIG})
          val (tyconPosition,_) = memoTyc addtyc
          val relativeType = fn ty => rewriteType(ty,tyconPosition)
          fun abstractDcon(DATACON{name,const,typ,rep,sign}) =
	    DATACON
              {name=name,const=const,rep=rep,sign=sign,typ=relativeType typ}
          fun relativizeBinding binding =
	    case binding
            of VARbind(VALvar{name,access,typ}) =>
                 VARbind(VALvar{name=name,access=access,
	                        typ=ref(relativeType (!typ))})
             | CONbind dcon => CONbind(abstractDcon dcon)
	     | TYCbind tyc => (
	         case tyconPosition tyc
                 of RELtyc {pos=([],count),...} =>
		      TYCbind
                        (FORMtyc{pos=count,spec=ERRORtyc,name=bogusTycSym})
                  | tyc' => TYCbind tyc')
             | STRbind(STRvar{name,access,binding}) => 
                 STRbind(STRvar{name=name,access=access,
                                binding=(addstr binding)})
	     | FCTbind(FCTvar{name,access,binding}) =>
                 FCTbind(FCTvar{name=name,access=access,
                                binding=addfct binding})
             | _ => binding
          val env' = Env.map relativizeBinding env
          val newSubStrs = Array.arrayoflist (rev (!extrastrs))
          val newSubFcts = Array.arrayoflist (rev (!extrafcts))
          val newTypes = Array.arrayoflist (rev (!extratycs))
      in (env',newSubStrs,newSubFcts,newTypes) end

   fun relativizeSignEnv(env : Modules.env,types : tycon array) =
     let val extratycs = ref (nil : tycon list)
	 val tyccount = ref (Array.length types)
         fun addtyc(tyc,memo,add) =
	      let val r = RELtyc {name=bogusName,pos=([],next tyccount)}
	      in extratycs := tyc :: !extratycs;
		 add r;
		 r
	      end
         val (tyconPosition,_) = memoTyc addtyc
         val relativeType = fn ty => rewriteType(ty,tyconPosition)
         fun abstractDcon(DATACON{name,const,typ,rep,sign}) =
	                DATACON{name=name,const=const,rep=rep,sign=sign,
		                typ=relativeType typ}
         fun relativizeBinding binding =
	     case binding
	     of VARbind(VALvar{name,access,typ}) =>
		    VARbind(VALvar{name=name,access=access,
				   typ=ref(relativeType (!typ))})
	      | CONbind dcon => CONbind(abstractDcon dcon)
	      | _ => binding
         val env' = Env.map relativizeBinding env
	 val newTypes = 
	       case !extratycs
	       of nil => types
		| l => Array.arrayoflist(ArrayExt.listofarray types @ rev l)
    in (env',newTypes)
     end

    fun relativizeSign (arg as (SIG{symbols,env,kind,stamp,...},types)) =
      if inBody stamp then
        let val (env',types') = relativizeSignEnv(!env,types)
	in (SIG{symbols=symbols,kind=kind,env=ref env',stamp=Stamps.newFree(),
		path=NONE},  (* anonymous *)
	    types')
        end
      else arg
      | relativizeSign arg = arg

   fun abstract (str as (INSTANCE{sign = (sign as SIG{env,...}),
                                  origin,subStrs,subFcts,types,path})) = (
         let val strStamp = getStrStamp str
             val sigStamp = getSignStamp sign
         in 
         if DEBUG then
           (app say ["get INSTANCE ",(Stamps.stampToString strStamp),"/",
	         (Stamps.stampToString sigStamp),"\n"];
            if inBody strStamp orelse inParam strStamp 
            then say " FUNC\n" else say "\n")
         else ();
         (get_instance strStamp sigStamp strMap)
         handle NotFound =>
           if inBody strStamp orelse inParam strStamp 
	      orelse inParam sigStamp orelse inBody sigStamp then
             let val origin' =
                   case origin
                   of SELF s => SELF Stamps.null
                    | _ => abstract origin
                 val (sign',types') = relativizeSign(sign,types)
                 val new_str =
                   INSTANCE{sign=sign',origin=origin',
                            subStrs=subStrs,types=types',subFcts=subFcts,
                            path=path}
                 val result = addStr new_str
             in
             add_instance strStamp sigStamp result strMap;
		(* HACKING AROUND A BUG *)
	     case sign
	     of SIG{kind = ref EMBEDDED, ...} => ()
              | _ => abstract_env (subStrs,subFcts,types');
             result
             end
           else str
         end)
     | abstract (str as (SIMPLE{env,path,...})) = (
         let val strStamp = getStrStamp str
         in 
         get_simple strStamp strMap
         handle NotFound =>
           if inBody strStamp then impossible "abstract: SIMPLE in body"
           else str
         end)
     | abstract (str as (APPLY{fct,arg,res})) = (
         let val strStamp = getStrStamp str
         in 
         (get_simple strStamp strMap)
         handle NotFound =>
           if inBody strStamp then
             let val fct' = abstractFct fct
                 val arg' = abstract arg
                 (* we enrich our map with the structures produced by the
                    functor: we can only access them after the functor
                    has been applied. *)
                 val _ =
                   enrichMap 
                     (inBody,strMap,fctMap,tycMap,fn s => SEQind(!strCount,s))
                     res
                 val result = addStr(APPLY{fct=fct',arg=arg',res=ERROR_STR})
             in add_simple strStamp result strMap; result end
           else str
         end)
     | abstract str = str
   and abstractFct (FCT_INSTANCE{fsig,fct,parent}) = 
         let val fct' = abstractFct fct
             val parent' = abstract parent
         in FCT_INSTANCE{fsig=fsig,parent=parent',fct=fct'} end
     | abstractFct (fct as (FCT{stamp=fctStamp,parent,
                                paramName,argument,body})) = (
         if DEBUG then print("get FCT"^(Stamps.stampToString fctStamp))
         else ();
         applyMap(fctMap,fctStamp)
         handle Member =>
           let val parent' = abstract parent
               val fct' = 
                 addFct (FCT{stamp=fctStamp,parent=parent',
                             paramName=paramName,argument=argument,body=body})
           in updateMap fctMap (fctStamp,fct');fct' end)
     | abstractFct fct = fct
   and abstract_env (subStrs,subFcts,types) = (
     (* we must follow the order of the definition while abstracting the
        environment *)
     appArray (fn pos => Array.update(subStrs,pos,
					 abstract (Array.sub(subStrs,pos))));
     appArray (fn pos => Array.update (subFcts,pos,
				       abstractFct (Array.sub(subFcts,pos))));
     appArray (fn pos => Array.update (types,pos,
				       tyconSeries (Array.sub(types,pos)))))

   val main = abstract body      

in
{tyseq=rev(!tycSeries),strseq=rev(!strSeries),
 fctseq=rev(!fctSeries),str=main,
 fullstr = body}
end

end (* structure *)
