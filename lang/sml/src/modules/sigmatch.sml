(* modules/sigmatch.sml *)

signature SIGMATCH = sig
  val match :
    {abstract:bool,
     arg_option: Modules.structureVar option,
     err:ErrorMsg.complainer,
     scope:Stamps.scope,
     spath : Modules.spath,
     printEnv: Modules.env,
     self:bool,
     sign:Modules.Signature,
     str:Modules.Structure} 
    -> Modules.Structure * Modules.thinning
  val matchFct :
    {abstract:bool,
     err:ErrorMsg.complainer,
     scope:Stamps.scope,
     spath : Modules.spath,
     printEnv: Modules.env,
     self:bool,
     fsig:Modules.FctSignature, fct:Modules.Functor} 
    -> Modules.Functor * Modules.fctThinning
end;

(* ASUMPTION: match is only used with TOP signature. 
 * This condition ensures a correct determination of the parent of functors
 * which must contain the structure resulting of the matching 
 *)

structure SigMatch : SIGMATCH =
struct
  open Modules ModuleUtil Types TypesUtil Variables PrettyPrint ErrorMsg Access
       Instantiate Extern

  val BogusTy = UNDEFty (*** this is just a place holder in VALtrans when 
                             the access is not INLINE and the 3rd componont
                             is NONE.  ***)

  (* checkSharing: check sharing constraints.  Takes a structure
     environment, a list of structure sharing constraints, a list
     of type sharing constraints, an error function, and a symbolic
     path as arguments.  The symbolic path is the path to this
     structure: it is prefixed to symbolic paths within this
     structure when error messages are printed.*)

  fun checkSharing(str : Structure, fullEnv : Modules.env,
		   sConstraints:{internal:spath list,
				 external: Structure option} list,
		   tConstraints:{internal:spath list,
				 external: Types.tycon option} list,
		    err) =
    let fun f (lookup : spath ->'a, printName : ppstream -> 'a -> unit,
	       eq : 'a * 'a -> bool, errName:string)
              ({internal:spath list,external : 'a option}) =
         let fun g first =
	     let fun check x =
		 if eq(lookup x,first)
		    then ()
		    else (err COMPLAIN (errName^" sharing violation")
		           (fn ppstrm =>
			    (add_newline ppstrm; printName ppstrm first;
			     add_string ppstrm " # "; PPUtil.ppSymPath ppstrm x)))
	     in app check internal
	     end
	  in case (internal,external)
	       of (l,SOME i) => g i
	        | (h::t,NONE) => g (lookup h)
                | (nil,NONE) => ()
         end
       val checkStructure = f (fn x => (fn STRvar{binding,...} => binding)
			               (lookBindingSTR(str,x)),
			       fn ppstrm => fn x =>
			         PPBasics.ppStructureName ppstrm (fullEnv,x),
			       eqOrigin,"structure")
       val checkType = f (fn x => lookBindingTYC(str,x),
			  fn ppstrm => fn tyc => PPType.ppTycon fullEnv ppstrm tyc,
			  equalTycon,"type")
    in app checkStructure sConstraints;
       app checkType tConstraints
    end

  fun compareTypes (env:Modules.env,err) (spec: ty, actual:ty,name) : unit =
    if TypesUtil.compareTypes{spec=spec,actual=actual} then ()
    else err COMPLAIN "value type in structure doesn't match signature spec"
	   (fn ppstrm =>
	    (PPType.resetPPType();
	     add_newline ppstrm;
	     app (add_string ppstrm) ["  name: ", Symbol.name name];
	     add_newline ppstrm;
	     add_string ppstrm "spec:   ";
	     PPType.ppType env ppstrm spec;
	     add_newline ppstrm;
	     add_string ppstrm "actual: ";
	     PPType.ppType env ppstrm actual))

      
  fun conforming(INSTANCE{sign=SIG{stamp,kind=ref(TOP _),...},...},
      		 SIG{stamp=stamp',kind=ref(TOP _),...}) = stamp=stamp'
    | conforming _ = false

  fun match1 _ {str=ERROR_STR,...} = (ERROR_STR,NONE)
    | match1 _ {str,sign=FULL_SIG,...} = (str,NONE)
    | match1 _ {str,sign=ERROR_SIG,...} = (str,NONE)
    | match1 (context: (Structure Array.array * Functor Array.array
                        * tycon Array.array) option) 
              {sign as SIG{symbols,env,kind,...},str,spath,
               scope,err,printEnv,abstract,self,parent_sig,arg_option}
	            : Structure * thinning =
      if conforming(str,sign)
      then ((if abstract then instantiate (sign, spath, scope,err)
                    else str(* Could copy with new spath. *)),
            NONE)
      else
      let val v = Access.mkLvar() (* local lvar for accessing str *)
	  val fullEnv = Env.atop(makeEnv(str,[v]),printEnv)
	       (* used for printing error msgs *)
	  val newContext as (subStrs,subFcts,types) = 
		case (context,!kind)
		  of (_, TOP{strcount,typecount,fctcount,...}) =>
		     (Array.array(strcount,ERROR_STR),
		      Array.array(fctcount,ERROR_FCT),
		      Array.array(typecount,ERRORtyc))
		   | (SOME st, EMBEDDED) => st
		   | _ => impossible "Sigmatch.match1"
	  val (self',oldEnv) = 
		if self then
	          case str
		  of INSTANCE{sign=SIG{env=str_env,...},subStrs,...} =>
			(false,SOME (str_env,!env,subStrs))
		   | INSTANCE _ => (false,NONE)
		   | _ => (true,NONE)
	        else (false,NONE)
	  val newstr = INSTANCE{sign=sign,
				subStrs=subStrs,
                                subFcts=subFcts,
				types=types,
				path=spath,
				origin =
				   if self' then SELF(Stamps.newStamp scope ())
				   else ModuleUtil.getOrigin str}

          val new_parent_sig = 
            case !kind
            of TOP _ => newstr
             | EMBEDDED => parent_sig
	     | IRRELEVANT => newstr
          val complain = fn s => err COMPLAIN s nullErrorBody
          val compare = compareTypes(fullEnv,err)
          val transType' = ModuleUtil.transType newstr

          (* findComponent: Given a binding specification, find the actual
             binding in a structure.  If the binding is not there, print an
             error message and raise the exception Error.

             We must handle exception bindings specially, since they are
             in the name space for constructors.  When we search for an
             actual exception binding, we may find a constructor binding
             instead.  For bindings in other namespaces, we will never
             accidentally find bindings in other name spaces. *)

          (* declare type *)

          val findComponent =
            let val complainMissing =
		  fn (UnboundComponent _,name,namespace) =>
                      (complain("unmatched "^namespace^" spec: "^
				Symbol.name name);
                       raise Error)
                   | (ErrorStructure,_,_)  => raise Error
                   | (exn,_,_) => raise exn
                val isExn = fn DATACON {rep,...} =>
		              case rep
			      of VARIABLE _ => true
			       | VARIABLEc _ => true
			       | _ => false
            in fn (CONbind(spec as (DATACON{rep,name,...}))) =>
                  ((case lookBinding(str,[name],[v])
		    of (binding as (CONbind actual)) =>
			  if isExn spec=isExn actual then binding
			  else raise UnboundComponent []
		     | _ => raise UnboundComponent [])
		   handle exn => 
		       complainMissing(exn,name,
				       if isExn spec
                                           then "exception"
                                           else "data constructor"))
	        (* we never check infix bindings; we can return anything
		    here *)
		| (spec as FIXbind _) => spec
		| (spec as STRbind(STRvar{name=id,...})) =>
                     if id=name_A then (
		       (* note that the access is not modified so that we
			  have a true LVAR and not a slot *)
		       case arg_option
                       of NONE => impossible "sigmatch: can't find the arg"
			| SOME arg => STRbind arg)
                     else 
		       (lookBinding (str,[id],[v])
			 handle exn => complainMissing(exn,id,"structure"))
                | spec =>
                  let val (name,namespace) =
                     case spec
                     of (FCTbind(FCTvar{name=id,...})) =>
			  (id,"structure")
                      | (TYCbind(FORMtyc{spec=sigTycon,...})) =>
                                (tycName sigTycon,"type")
                      | (CONbind(DATACON{name,...})) =>
                                (name,"data constructor")
                      | (VARbind(VALvar{name=[id],...})) => (id,"val")
		      | _ => impossible "SigMatch.findComponent"
                  in lookBinding (str,[name],[v])
		     handle exn => complainMissing(exn,name,namespace)
		  end
             end

	exception BadBinding;

        fun checkDataconSign(name,d1,d2) =
             let fun ck(DATACON{rep=r1,name=n1,...},DATACON{rep=r2,...}) =
                if r1=r2 then ()
                else (err COMPLAIN ("The constructor "^Symbol.name n1^
                                   " of datatype "^Symbol.name name^
                                   "\nhas different representations in \
                                   \the signature and the structure.  \n\
                                   \Change the definition of the types \
                                   \carried by the constructors in the\n\
                                   \functor formal parameter and the functor \
                                    \actual parameter so that\n\
                                   \they are both abstract, or so that \
                                   \neither is abstract.\n")
			   nullErrorBody;
		      raise BadBinding)
           in List2.app2 ck (d1,d2) end

	 (* compare datacon names of spec and actual datatype.  This
	    uses the fact that datacons have been sorted by name. *)
	 fun compareDcons(spec,actual) =
	     let fun comp(l1 as dc1::r1, l2 as dc2::r2, s_only, a_only) =
		     if Symbol.eq(dc1,dc2) then comp(r1,r2,s_only,a_only)
		     else if Symbol.symbolGt(dc1,dc2) then
		       comp(l1,r2,s_only,dc2::a_only)
		     else comp(r1,l2,dc1::s_only,a_only)
		   | comp([], [], s_only, a_only) = (rev s_only, rev a_only)
		   | comp([], r, s_only, a_only) = (rev s_only, rev a_only @ r)
		   | comp(r, [], s_only, a_only) = (rev s_only @ r, rev a_only)
	      in comp(spec,actual,[],[])
	     end

         fun checkTycBinding(specTycon,strTycon) =
	  let val name = fn () => Symbol.name(tycName specTycon)
	      fun complain' x = (complain x; raise BadBinding)
	      fun symbolsToString [] = ""
		| symbolsToString [n] = Symbol.name n
		| symbolsToString (n::r) =
		   implode(Symbol.name n ::
			   fold (fn(n,b) => (","::Symbol.name n::b)) r [])
	      fun dconName(DATACON{name,...}) = name
          in case specTycon
             of GENtyc {stamp=s,arity,kind,eq=ref eq,...} =>
		    (if arity <> tyconArity strTycon
		     then complain' ("tycon arity for "^name()
                                    ^ " does not match specified arity")
		     else case (!kind, strTycon)
			of (DATAtyc dcons,
			    GENtyc{arity=a',kind=ref (DATAtyc dc'),...})=>
			    (case compareDcons(map dconName dcons, map dconName dc')
			      of ([],[]) => checkDataconSign(tycName specTycon,
							     dcons,dc')
			       | (s_only, a_only) =>
			          complain'(implode["datatype ",name(),
				   " does not match specification\n",
				   case s_only
				    of [] => ""
				     | _  =>
				       implode["  constructors in spec only: ",
				               symbolsToString s_only, "\n"],
				   case a_only
				    of [] => ""
				     | _  =>
				       implode["  constructors in actual only: ",
				               symbolsToString a_only, "\n"]]))
			 | (DATAtyc _, _) => 
			      complain' ("type "^name()^" must be a datatype")
			 | (FORMtyck, _) =>
			     if eq=YES andalso not(EqTypes.isEqTycon strTycon)
				  then complain'
				      ("type "^name()^
				       " must be an equality type")
				  else ()
                         | _ => impossible "checkTycBinding 1")
              | ERRORtyc => raise BadBinding
              | _ => ErrorMsg.impossible "checkTycBinding 2"
           end

          (* checkSpec:  Check that a binding specification is matched by an
             actual binding in a structure.  Fill in the instantiation vectors
             for types and structures.*)

	  fun checkSpec spec =
            let val result = findComponent spec
            in
	      case (spec,result) 
		of (STRbind(STRvar{name=id,binding=STR_FORMAL{pos,spec,...},
                                   ...}),
		    STRbind(STRvar{access,binding=str',...})) =>
		      let val (str,thin) =
			if id = name_P then (str',NONE)
                        else
                          match1 (SOME newContext)
			    {sign=spec, str=str', spath = id :: spath,
			     scope=scope, err=err, parent_sig=new_parent_sig,
			     printEnv = printEnv, abstract=false,
			     self=false, arg_option = NONE}
		      in Array.update(subStrs,pos,str);
			 if (hidden id) then []
                         else
                           [case thin
			    of NONE => VALtrans(access,BogusTy,NONE)
			     | SOME(v,transl) => THINtrans(access,v,transl)]
		      end
                 | (FCTbind(FCTvar{name=id,binding=FCT_FORMAL{pos,spec,...},
                                   ...}),
		    FCTbind(FCTvar{access,binding=fct',...})) =>
                      let val (fct,thin) = matchFct1
			    {fsig=spec,
			     fct=fct',
			     spath = id :: spath,
			     scope = scope,
			     err=err,
                             parent_sig = new_parent_sig,
			     printEnv = printEnv,
			     abstract=false,
			     self=false}
		      in Array.update(subFcts,pos,fct);
			 case thin 
                          of NONE => [VALtrans(access,BogusTy,NONE)]
			   | SOME(thinI,thinO) => 
                               (case spec 
                                of FSIG{argument,...} => 
                                       [FCTtrans(access,argument,thinI,thinO)]
                                 | _ => impossible "sigmatch check-spec 324")
		      end
		 | (TYCbind(FORMtyc{pos,spec=sigTycon,...}),
		    TYCbind(strTycon)) =>
		        ( ((checkTycBinding(sigTycon,strTycon);
			    Array.update(types,pos,strTycon)
			   ) handle BadBinding =>
				 Array.update(types,pos,ERRORtyc));
			 nil)
		 | (CONbind(DATACON{name,typ,const,...}),
		    CONbind(DATACON{typ=typ',rep,...})) =>
	            (compare(transType' typ,typ',name);
                      case rep
                       of VARIABLE access => [VALtrans(access,typ',SOME typ)]
                        | VARIABLEc access => [VALtrans(access,typ',SOME typ)]
                        | _ => nil)
		 | (VARbind(VALvar{name=[name],typ,...}),a) =>
		    (case a
		       of VARbind(VALvar{access,typ=typ',...}) =>

			    (* no propagation of INLINE access!! *)

			   (compare(transType' (!typ),!typ',name);
			    [VALtrans(access,!typ',SOME(!typ))])
			| CONbind(dcon as DATACON{typ=typ',...}) =>
			    (compare(transType' (!typ),typ',name);
			     [CONtrans(dcon,SOME(!typ))])
			| _ => impossible "sigmatch.sml: 122")
		 | (FIXbind _,_) => nil (* nonchecked binding *)
                 | _ => impossible "sigmatch.sml: 124"
            end

	  fun checkList (a::rest) =
		(checkSpec (Env.look (!env,a))
		 handle Error => nil | Env.Unbound => impossible "checkList")
		 @ checkList rest
	    | checkList nil = nil

	  val trans = checkList (!symbols)
	  val _ = case !kind
	          of TOP{sConstraints,tConstraints,...} =>
		        checkSharing(newstr,fullEnv,sConstraints,tConstraints,err)
		   | EMBEDDED => ()
		   | IRRELEVANT => ()
	  val _ =
	    if self then (
	      case oldEnv
	      of SOME (env,sigenv,strs) => 
		   (* The meaning of this hack is the following:
		      If the structure is a self but belongs to a functor
		      then it may contain a useless and big INSTANCE used
		      as an intermediate structure for open.
		      This function destroys such instances and replace them
		      by a dummy structure. *)
		   let fun clrOpen n = (
			 case Array.sub(strs,n) 
		         of INSTANCE{path as _ :: _, ...} =>
			      if Symbol.eq(name_O,last path) then 
				(Array.update(strs,n,ERROR_STR);clrOpen (n+1))
			      else clrOpen (n+1)
			  | _ =>  clrOpen (n+1)) handle Array.Subscript => ()
		       val thinenv = ref Env.empty
		       (* Note: it would be nice if Env.map gave also the
			  symbol to the argument function *)
		       val _ = 
			 Env.app 
			   (fn (name,_) =>
			      thinenv := Env.bind(name,Env.look(!env,name),
						      !thinenv)
			      handle Env.Unbound => ())
			   sigenv
		   in env := Env.consolidate(!thinenv) ;  clrOpen 0 end
	       | NONE => ())
	    else ()
	  val str = if abstract then instantiate(sign,spath,scope,err)
                           else newstr
       in (str, SOME(v,trans))
      end

  and matchFct1 {abstract,err,scope,spath,printEnv,self,
                 parent_sig,fct,
                 fsig=fsig as FSIG{paramName,argument=sig_arg,
                                   body=sig_body,...}} =
      let 
         (* externalize the sharing constraints with the parent in the
          * argument signature so that we can use instantiate *)
          val arg_final = externalize_sharing name_P parent_sig sig_arg
         (* instance of the argument *)
          val inst_sig_arg = Instantiate.instantiate(arg_final,[],scope,err)
          val arg_var = STRvar{access=PATH [], name=name_A,
			       binding=inst_sig_arg}
          val _ = update_structure name_P parent_sig inst_sig_arg
         (* the corresponding version with the parent of the structure *)
          val STRvar{binding=val_X,...} = lookBindingSTR(inst_sig_arg,[name_X])
         (* we don't keep thinnings: thinin is a thinning against a dummy
            structure we usually never build and thinout is discarded (if
            we used one it would come with res_match *)
          val (res,thinIn) =
            ApplyFunctor.applyFunctorFull 
              (fct, val_X, scope,spath,err,printEnv, match) 
         (* matches the result against the body specification of the functor
          * signature. The specif of the functor body must be a TOP to get
          * a correct notion of parent_sig 
	  * we keep the thinning obtained *)
          val (res_match,thinning) = 
            match1 NONE 
                   {abstract=abstract, err=err, scope=scope, spath=spath,
                    printEnv=printEnv, self=false, parent_sig=ERROR_STR,
                    sign=sig_body, str=res, arg_option=SOME arg_var}
          val raw_fct = 
            case fct 
            of FCT_INSTANCE{fct,...} => fct
             | fct => fct
          val new_fct = 
            FCT_INSTANCE{fct=raw_fct,fsig=fsig,parent=parent_sig}
      in (new_fct,SOME(thinIn,thinning)) end
    | matchFct1 {fsig=ERROR_FSIG,fct,...} = (fct,NONE)

  and match {abstract:bool,
             err:ErrorMsg.complainer,
             arg_option: structureVar option,
             scope:Stamps.scope,
             spath : Modules.spath,
             printEnv: Modules.env,
             self:bool,
             sign:Modules.Signature,
             str:Modules.Structure} 
          = match1 NONE {abstract=abstract,err=err,scope=scope,spath=spath,
                         printEnv=printEnv,self=self,arg_option=arg_option,
                         parent_sig = ERROR_STR, sign=sign,str=str}
  and matchFct {abstract,err,scope,spath,printEnv,self,fsig,fct} =
    matchFct1 {abstract=abstract,err=err,scope=scope,printEnv=printEnv,
               self=self,spath=spath,fsig=fsig,fct=fct, parent_sig=ERROR_STR}
;

end  (* structure SigMatch *)
