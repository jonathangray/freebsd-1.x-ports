(***************************************************************************

   APPLYFCT.SML applies a functor to an argument

 ***************************************************************************)


signature APPLYFUNCTOR =
    sig
       val applyFunctor : Modules.Functor 
                          * Modules.Structure * Stamps.scope
                          * Modules.spath
                          * ErrorMsg.complainer
                          * Modules.env
                          * ({abstract:bool,
                              err:ErrorMsg.complainer,
                              scope:Stamps.scope,
                              spath : Modules.spath,
                              printEnv: Modules.env,
                              self:bool,
                              sign:Modules.Signature,
			      arg_option: Modules.structureVar option,
                              str:Modules.Structure} 
                              -> Modules.Structure * Modules.thinning)
                           -> Modules.Structure
       val applyFunctorFull : Modules.Functor
                              * Modules.Structure * Stamps.scope
                              * Modules.spath
                              * ErrorMsg.complainer
                              * Modules.env
                              * ({abstract:bool,
                                  err:ErrorMsg.complainer,
                                  scope:Stamps.scope,
                                  spath : Modules.spath,
				  printEnv: Modules.env,
				  self:bool,
				  sign:Modules.Signature,
				  arg_option: Modules.structureVar option,
				  str:Modules.Structure} 
				  -> Modules.Structure * Modules.thinning)
			       -> Modules.Structure * Modules.thinning
    end


structure ApplyFunctor : APPLYFUNCTOR =
struct
  open Symbol Access Modules Types ModuleUtil Extern Stamps ErrorMsg
  val DEBUG = false
  val say = System.Print.say

(*****************************************************************************
   applyFunctor does not perform the argument signature matching because
   it is different if we apply the functor to the instantiation of its
   argument.
   This version is much more sequential than the original one. Structure
   must be processed in their definition order so that parent structure are
   sufficiently defined when they are provided to functors for functor 
   application.
   strinst or fctinst contain the result of instantiation and are initialized
   to ERROR_STR and ERROR_FCT
   strtemp and fcttemp contain the template for building the corresponding
   elements of the inst version.
   strarrays and fctarrays provide as usual a memo mechanism for the arrays
   used by structures with EMBEDDED signature kind. 
   As in the original version  inststr provides the element by accessing it
   or calls inststr' if it is not ready. instfct and instfct' are the 
   counter-part for functors.
 ****************************************************************************)

fun applyFunctor
     (FCT{paramName: symbol,parent=parentFct,
          stamp,
	  argument: Signature,
	  body={strseq,fctseq,tyseq,str,fullstr}},
      argstr: Structure,
      scope: scope,
      path: spath,
      err,
      parseEnv: Modules.env,
      sigmatch)
     : Structure =
    let	val makeStamp = Stamps.newStamp scope
        val strtemp = Array.arrayoflist strseq
        val strinst = Array.array(length strseq,ERROR_STR)
        val tyctemp = Array.arrayoflist tyseq
        val tycinst = Array.array(length tyseq,ERRORtyc)
	val fcttemp = Array.arrayoflist fctseq
        val fctinst = Array.array(length fctseq,ERROR_FCT)
        val strarrays : (Structure array * Structure array) list ref = ref nil
        val tycarrays : (tycon array * tycon array) list ref = ref nil
        val fctarrays : (Functor array * Functor array) list ref = ref nil
        fun inststr (STR_ABSFB (SEQ pos)) = (
	     (case Array.sub(strinst,pos)
              of ERROR_STR => 
                   inststr' (fn str => Array.update(strinst,pos,str))
                            (Array.sub(strtemp,pos))
               | str => str)
	      handle Array.Subscript => 
		impossible 
                  (implode ["Applyfct.inststr, pos = ", makestring pos]))
          | inststr (STR_ABSFB (SEQind (pos,path))) = (
	     (case Array.sub(strinst,pos)
              of ERROR_STR => 
                   let val str =
                     inststr' (fn str => Array.update(strinst,pos,str))
                              (Array.sub(strtemp,pos))
                   in transPosStr str path end
               | str => transPosStr str path)
	      handle Array.Subscript =>
		impossible
                 (implode ["Applyfct.inststr, pos = ", makestring pos]))
          | inststr (STR_ABSFB (PARAM pos)) = transPosStr argstr pos
	  | inststr str = str

        and instfct (FCT_ABSFB (SEQ pos)) = (
	     (case Array.sub(fctinst,pos)
              of ERROR_FCT => 
                   instfct' (fn fct => Array.update(fctinst,pos,fct))
                            (Array.sub(fcttemp,pos))
               | fct => fct)
	      handle Array.Subscript => 
		impossible
                  (implode ["Applyfct.instfct, pos = ", makestring pos]))
          | instfct (FCT_ABSFB (SEQind (pos,path))) = (
	     (case Array.sub(strinst,pos)
              of ERROR_STR => 
                   let val str =
                     inststr' (fn str => Array.update(strinst,pos,str))
                              (Array.sub(strtemp,pos))
                   in transPosFct str path end
               | str => transPosFct str path)
	      handle Array.Subscript =>
		impossible
                  (implode ["Applyfct.instfct, pos = ", makestring pos]))
          | instfct (FCT_ABSFB (PARAM pos)) = transPosFct argstr pos
	  | instfct (FCT_INSTANCE{fsig,parent,fct}) =
	      let val parent' = inststr parent
		  val fct' = instfct fct
	      in FCT_INSTANCE{fsig=fsig,fct=fct',parent=parent'} end
	  | instfct fct = fct

        and insttyc (ABSFBtyc (SEQ pos)) = (
	     (case Array.sub(tycinst,pos)
              of ERRORtyc => 
                   insttyc' (fn tyc => Array.update(tycinst,pos,tyc))
                            (Array.sub(tyctemp,pos))
               | tyc => tyc)
	      handle Array.Subscript => 
		impossible
                  (implode ["inststr, pos = ", makestring pos]))
          | insttyc (ABSFBtyc (SEQind (pos,path))) = (
	     (case Array.sub(strinst,pos)
              of ERROR_STR => 
                   let val str =
                     inststr' (fn str => Array.update(strinst,pos,str))
                              (Array.sub(strtemp,pos))
                   in transPosTycon str path end
               | str => transPosTycon str path)
	      handle Array.Subscript =>
		impossible
                  (implode ["inststr, pos = ", makestring pos]))
          | insttyc (ABSFBtyc (PARAM pos)) = transPosTycon argstr pos
	  | insttyc tyc = tyc

        and inststr' update 
              (INSTANCE{sign=sign as SIG{env,...},path=p,
                        subStrs,types,subFcts,origin}) =
	     let fun find (elem,a,elt) =
		    let fun f ((key,data) :: t) =
			     if elem=key then data else f t
		          | f nil =
			     let val r=Array.array(Array.length elem,elt)
			     in a := (elem,r) :: (!a); r end
		    in f (!a) end
		  val subStrs' = find(subStrs,strarrays,ERROR_STR)
		  val types' = find(types,tycarrays,ERRORtyc)
		  val subFcts' = find(subFcts,fctarrays,ERROR_FCT)
                  fun updtStr pos = (
                        case Array.sub(subStrs',pos)
                        of ERROR_STR =>
                             Array.update
                               (subStrs',pos,
                                inststr (Array.sub(subStrs,pos)))
                         | _ => ();
                        updtStr (pos+1))
                        handle Array.Subscript => ()
                  fun updtFct pos = (
                        case Array.sub(subFcts',pos)
                        of ERROR_FCT =>
                             Array.update
                               (subFcts',pos,
                                instfct (Array.sub(subFcts,pos)))
                         | _ => ();
                        updtFct (pos+1))
                        handle Array.Subscript => ()
                  fun updtTyc pos = (
                        case Array.sub(types',pos)
                        of ERRORtyc =>
                             Array.update
                               (types',pos,
                                insttyc (Array.sub(types,pos)))
                         | _ => ();
                        updtTyc (pos+1))
                        handle Array.Subscript => ()
	          val origin' =
		      case origin
		      of SELF _ => SELF (makeStamp())
                       | _ => getOrigin(inststr origin)
                  val new_str = INSTANCE{sign=sign, subStrs=subStrs',
                                         types=types', subFcts=subFcts',
                                         path=p@path, origin=origin'}
             in 
             update new_str;
             updtStr 0; updtFct 0; updtTyc 0;
	     new_str
             end
          | inststr' update (APPLY{fct,arg,...}) =
              let val fct' = instfct fct
                  val arg' = inststr arg
                  val (new_str,thinIn) = 
                    applyFunctorFull
                      (fct',arg',scope,path,err,parseEnv,sigmatch)
              in update new_str; new_str end

          | inststr' update _ = 
              impossible "ApplyFunctor.inststr'"
        and instfct' update 
                     (fct as FCT{parent,argument,body,stamp,paramName}) =
              let val new_fct = 
                FCT{parent=inststr parent, body=body, stamp=makeStamp(),
                    paramName=paramName, argument=argument}
              in (update new_fct; new_fct) end
          | instfct' update (FCT_INSTANCE{fsig,fct,parent}) =
              let val new_fct =
                    FCT_INSTANCE{fsig=fsig, fct = instfct fct,
                                 parent=inststr parent}
              in update new_fct; new_fct end
          | instfct' update _ =
              impossible "instfct'"
        and insttyc' update (GENtyc{stamp,arity,eq,path=path',kind}) =
              let val kind' = case !kind
		              of d as DATAtyc _ =>ref d
			       | _ => kind
	          val new_tyc =
                    GENtyc{stamp = makeStamp(), eq = eq, arity = arity,
                           path = path'@path, kind = kind'}
              in
              update new_tyc;
              case !kind'
              of (DATAtyc dcons) =>
                   kind' :=
                     DATAtyc(map (fn DATACON{name,const,typ,rep,sign} =>
	  	 	            DATACON{name=name,const=const,
					    rep=rep,sign=sign,
				            typ=insttype typ}) dcons)
              | _ => ();
              new_tyc
              end
	  | insttyc' update 
                    (DEFtyc{path=path',strict,tyfun=TYFUN{arity,body}}) =
	      let val new_tyc =
                DEFtyc{path=path'@path,strict=strict,
		       tyfun=TYFUN{arity=arity,body=insttype body}}
              in update new_tyc; new_tyc end
	  | insttyc' _ _ = impossible "applyFunctor.insttyc"
	and insttype ty =
	    case ty
	      of CONty(tycon,args) => 
                     TypesUtil.mkCONty(insttyc tycon,map insttype args)
	       | POLYty{sign,tyfun=TYFUN{arity,body},abs} =>
	           POLYty{sign=sign,abs=abs,
		  	 tyfun=TYFUN{arity=arity,body=insttype body}}
	       | _ => ty
    in inststr str end
  | applyFunctor _ =impossible "ApplyFunctor.strange functor"
and applyFunctorFull 
      (fct_def, argstr, scope, path, err, parseEnv, sigmatch) =
  case fct_def
  of FCT{argument=arg_sig,body,parent=fct_parent,...} =>
       let val _ = if DEBUG then say "I\n" else ()
	   val argument = make_argument{parent=fct_parent,parameter=argstr}
	   val _ = if DEBUG then say "II\n" else ()
           val (arg_pair,thinInTotal) = 
             sigmatch{abstract=false,err=err,scope=scope,spath=path,
                      printEnv=parseEnv,self=false, arg_option=NONE,
                      str=argument, sign=arg_sig}
           val _ = if DEBUG then say "III\n" else ()
	   val thinIn = 
	     case thinInTotal
             of SOME(_,[THINtrans(a,v,transl)]) => SOME (v,transl)
	      | _ => NONE
           val res = 
             applyFunctor
               (fct_def,arg_pair,scope,path,err,parseEnv,sigmatch)
           val _ = if DEBUG then say "IV\n" else ()
           val new_str = APPLY{fct=fct_def,arg=argstr,res=res}
       in (new_str,thinIn) end
   | FCT_INSTANCE{fsig = FSIG{argument=arg_sig,body,...},parent=fct_parent,
                  fct=fct'} =>
       let val argument = make_argument{parent=fct_parent,parameter=argstr}
           (* matching done for verification *)
           val (_,thinInTotal) = 
             sigmatch{abstract=false, err=err, scope=scope, spath=path,
		      arg_option=NONE, printEnv=parseEnv, self=false,
		      str=argument, sign=arg_sig}
           val thinIn =
             case thinInTotal
               of SOME(_,[THINtrans(a,v,transl)]) => SOME (v,transl)
                | _ => NONE
	   val argumentVar = 
	     STRvar{name=name_A, binding=argument, access=PATH []}
           val (res,_) =
             case applyFunctorFull
                    (fct',argstr,scope,path,err,parseEnv,sigmatch)
             of (APPLY{res,...},thinIn) => (res,thinIn)
              | _ => impossible "ApplyFunctor.strange result"
           (* matches the result against the body specification of the functor
            * signature *)
           val (res_match,thinRes) = 
             sigmatch {abstract=false, err=err, scope=scope, spath=path,
		       arg_option=SOME argumentVar, printEnv=parseEnv,
		       sign=body, str=res, self=false}
        in (APPLY{fct=fct_def,arg=argstr,res=res_match},thinIn) end
   | FCT_INSTANCE _ => (ERROR_STR,NONE) (* The signature is wrong *)
   | ERROR_FCT => (ERROR_STR,NONE)
   | _ => impossible "ApplyFunctorFull"

end (* structure ApplyFunctor *)
