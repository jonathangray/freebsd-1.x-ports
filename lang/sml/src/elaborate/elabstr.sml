(* Copyright 1992 by AT&T Bell Laboratories *)

(****************************************************************************

  ELABSTR.SML: elaborate the AST representing structures

 ****************************************************************************)

signature ELABSTR = sig
  val elaborateTop : Ast.dec * Modules.binding Env.env 
		   * (int * int -> ErrorMsg.complainer)
		   * (int * int -> string)
                   * (Absyn.dec -> Absyn.dec) 
		   -> Absyn.dec * Modules.binding Env.env
end
 
structure ElabStr :ELABSTR = struct
local
  open Access Symbol Types Modules ModuleUtil ErrorMsg Extern
       TyvarSet Absyn Variables ElabSig System.Control Ast PrettyPrint
  val DEBUG = false
in

val localStrName = Symbol.strSymbol "a funny structure"
    (* used in makeOpenDec to build redeclaration of components *)

val say = System.Print.say

val BogusTy = UNDEFty (*** BogusTy is just a place holder ***)

val functorId = Symbol.fctSymbol "<functor>"
val hiddenId = Symbol.strSymbol "<hidden>"

(*
   computes the number of elements defined in a structure
   it is used to preallocate the arrays of an INSTANCE structure
   before the real elaboration begins
*)

fun countStrFctTyp ldec: (int * int * int) =
  let val rec count =
    fn (TypeDec tb, (strN,fctN,typN)) => (strN,fctN,typN + length tb)
      | (DatatypeDec {datatycs,withtycs}, (strN,fctN,typN)) => 
	   (strN,fctN,typN + length datatycs + length withtycs)
      | (AbstypeDec {abstycs,withtycs,body,...}, t) => 
         let val (strN,fctN,typN) = count (body,t)
         in (strN,fctN,typN+length abstycs+length withtycs) end
      | (OpenDec l, (strN,fctN,typN)) =>
	  (strN + length l, fctN, typN)
      | (StrDec l, (strN,fctN,typN)) => (strN + length l, fctN, typN)
      | (AbsDec l, (strN,fctN,typN)) => (strN + length l, fctN, typN)
      | (FctDec l, (strN,fctN,typN)) => (strN, fctN + length l, typN)
      | (LocalDec (l1,l2),tN) =>
          (* the first count is probably (certainely) false *)
          let val t1 = count (l1,tN)
	      val t2 = count (l2,t1)
          in t2 end
      | (SeqDec ldec,t) => fold count ldec t
      | (MarkDec (d,_,_),t) => count (d,t)
      | (_, t) => t
  in count (ldec,(0,0,0)) end

(* TYPES FOR ELABORATING STRUCTURES *)

(* 
   This type describes the context in which an object is added 
   the FctBodyStr is only used by elaborate if its argument is actually
   a functor body.

   It must be seen as an abstraction of the structure built
   it forgets all the useless fields but contains a specification of the
   skeleton for InstanceStr. 
   the structure produced has IRRELEVANT instead because the skeleton is built
   directly and so its specification is not needed.)

   Top is used for the top level environment
*)

datatype strKind 
  = SimpleStr of Modules.env ref
  | InstanceStr of {
      myself: Structure,
      (* pointer to the environment (incremental updating) *)
      env: Modules.env ref,
      (* the parent structure *)
      parent: Structure,
      (* test if a stamp has been generated in the parent or 
       * in the argument *)
      inFunctor: Stamps.stamp -> bool,
      inArgument: Stamps.stamp -> bool,
      (* environment to translate paths used for sharing constraints in
       * the specification of functor arguments *)
      symtotal: Normalize.env ref,
      (* counters to allocate slots in arrays for objects *)
      strCount: int ref, 
      fctCount: int ref,
      typCount: int ref,
      (* arrays to store mutable objects *)
      strArray: Structure array,
      fctArray: Functor array,
      typArray: Types.tycon array}
  | FctBodyStr of {
      (* functions to test the age of stamps *)
      inFunctor: Stamps.stamp -> bool,
      inArgument: Stamps.stamp -> bool,
      (* argument of the functor to build *)
      argument: Structure,
      (* environment to shift and fix paths in signatures of functor args *)
      symtotal: Normalize.env ref} 
  | TopEnv of bool (* distinguishes local from true tops *)

fun isTop (TopEnv b) = b | isTop _ = false
fun next i = !i before inc i;

(* THE TYPE OF THE ELABORATION CONTEXT FOR STRUCTURES *)

type contextStr = {
  (* what kind of structure ? *)
  kind: strKind,
  (* the global environment *)
  total: Modules.env,
  (* path to the current structure in REVERSE order *)
  path: Symbol.symbol list,
  (* error function *)
  error: (linenum * linenum) -> ErrorMsg.complainer,
  errorMatch: (linenum * linenum) -> string,
  (* last positions recorded in the source file *)
  ln: (linenum * linenum),
  (* scope of stamps *)
  stampgen: Stamps.scope,
  (* transform of absyn prior to type checking *)
  transform: Absyn.dec -> Absyn.dec
}

(* Recompute paths after the elaboration of a structure *)

fun computePath (bind as VARbind(VALvar{access as INLINE _,typ,...}),
                 slotCount) = 
      (bind,[VALtrans(access,!typ,NONE)]) before (next slotCount)
  | computePath (VARbind(VALvar{name,typ,access}),slotCount) =
      let val access' = SLOT (next slotCount) in
      (VARbind(VALvar{access=access',name=name,typ=typ}),
       [VALtrans(access,!typ,NONE)])
      end
  | computePath (CONbind(DATACON{name,const,sign,typ,rep}),slotCount) =
      let val (rep',trans') = 
        case rep
        of VARIABLE access =>
             let val access' = SLOT(next slotCount)
             in (VARIABLE access',[VALtrans(access,typ,NONE)]) end
         | VARIABLEc access =>
             let val access' = SLOT(next slotCount)
             in (VARIABLEc access',[VALtrans(access,typ,NONE)]) end
         | _ => (rep,[])
      in 
      (CONbind(DATACON{name=name,const=const,sign=sign,typ=typ,rep=rep'}),
       trans')
      end
  | computePath (STRbind(STRvar{name,access,binding}),slotCount) =
      let val access' = 
            if hidden name then SLOT 0 else SLOT(next slotCount)
      in 
      (STRbind(STRvar{name=name,access=access',binding=binding}),
       if (hidden name) then [] else [VALtrans(access,BogusTy,NONE)])
      end
  | computePath (FCTbind(FCTvar{name,access,binding}),slotCount) =
      let val access' = SLOT(next slotCount)
      in 
      (FCTbind(FCTvar{name=name,access=access',binding=binding}),
       [VALtrans(access,BogusTy,NONE)])
      end
  | computePath (bind,_) = (bind,[])


(* Recompute all the paths in an environment, suppress doubles and
   allocate slots. Objects are ordered so that slot allocation is independant
   from the way elaboration is done *)

fun recomputePath origEnv =
  let val slotCount = ref 0
      val env = ref Env.empty
      val trans = ref ([]: trans list)
      val revEnv = sortEnvBindings (Env.consolidate origEnv) 
      val symbols = map #1 revEnv
  in
  app
    (fn (name,bind) => 
        let val (newbind,newtrans) = computePath (bind,slotCount)
	in trans := newtrans @ (!trans); env :=Env.bind(name,newbind,!env) end)
    revEnv;
  (Env.consolidate (!env),rev (!trans),symbols)
  end;

(*** ADDING OBJECTS TO THE DIFFERENT ELABORATION ENVIRONMENTS ***)
(* adds various simple objects to the environment *)

fun addObject (sym, binding, SimpleStr env,path) =
      (env := Env.bind(sym,binding,!env);binding)
  | addObject (sym,binding as TYCbind def,
               InstanceStr{env,typCount,typArray,symtotal,...},path) = 
      let val pos = next typCount
          val newbind = TYCbind(FORMtyc{pos=pos,name=sym,spec=def})
      in 
      Array.update(typArray,pos,def) 
        handle Subscript => impossible "addObject";
      symtotal := Normalize.bind(sym,sym::path,!symtotal);
      env := Env.bind(sym,newbind,!env);
      newbind
      end
  | addObject (sym, binding, InstanceStr{env,...},_) =
      (env := Env.bind(sym,binding,!env);binding)
  | addObject (sym, binding, _,_) = binding
;


(*
 * addStr1 is used to update the environment with the raw structure. If we
 * are not in a functor body, there is no reason to update the environment
 * so nothing is done. This means that the second call to addStr2 is
 * mandatory !
 *
 * Remark: We are building the full and true signature of the slot. It is 
 * necessary for elaborating functor arguments.
 *)

fun addStr1(sym,str,sgn,InstanceStr{env,strCount,strArray,symtotal,...},spath)=
      let val path = if hidden sym then PATH [] else PATH [namedLvar sym]
	  val pos = next strCount
          val newStr = STR_FORMAL{pos=pos,spec=sgn}
          val newVar = STRvar{name=sym,access=path,binding=newStr}
      in 
      Array.update(strArray,pos,str);
      symtotal := Normalize.bind(sym,spath,!symtotal);
      env := Env.bind(sym,STRbind newVar,!env)
      end
  | addStr1 _ = ()
;

(*
 * addStr2 adds the structure to the environment if it is a simple and
 * updates it if it is an instance (it finds the slot and modifies it
 * if it is a functor body, nothing is modified: the result of elaborate
 * will be used by the function building functors.
 *)

fun getEnvPos env (sym: Symbol.symbol) = 
    case Env.look(env,sym) of
	(STRbind(STRvar {binding=(STR_FORMAL {pos,...}),...})) => pos
      | _ => impossible "Instantiate:getStrPos"

fun addStr2 (sym,str,InstanceStr{env,strArray,...}) =
      let val (pos,access,symbbind)=
            case Env.look(!env,sym)
	    of (symbbind as STRbind(STRvar {binding=(STR_FORMAL {pos,...}),
					    access,...})) =>
		  (pos,access,symbbind)
	     | _ => impossible "Instantiate:getStrPos"
         val strvar = STRvar{name=sym,access=access,binding=str}
      in
      Array.update(strArray,pos,str) 
        handle Subscript => impossible "elaboration.2";
      (strvar,symbbind)
      end
  | addStr2 (sym,str,SimpleStr env) =
      let val path = if hidden sym then PATH [] else PATH [namedLvar sym]
          val newVar = STRvar{name=sym,access=path,binding=str}
      in env := Env.bind(sym,STRbind newVar, !env);(newVar,STRbind newVar) end
  | addStr2 (sym,str,_ ) = 
      let val path = if hidden sym then PATH [] else PATH [namedLvar sym]
          val newVar = STRvar{name=sym,access=path,binding=str}
      in (newVar,STRbind newVar) end

(* Add a functor in the environment *)

fun addFct (sym,fct,SimpleStr env) =
      let val slot = namedLvar(sym)
          val fctVar = FCTvar{name=sym,access=PATH [slot],binding=fct}
      in env := Env.bind(sym,FCTbind fctVar,!env);(fctVar,fctVar) end
  | addFct (sym,fct,InstanceStr{env,fctCount,fctArray,...}) =
      let val access = PATH[namedLvar(sym)]
	  val pos = next fctCount
          val newFct = FCT_FORMAL{pos=pos,spec=FULL_FSIG}
          val fctVar = FCTvar{name=sym,access=access,binding=newFct}
          val fctVar' = FCTvar{name=sym,access=access,binding=fct}
      in 
      Array.update(fctArray,pos,fct);
      env := Env.bind(sym,FCTbind fctVar,!env);(fctVar',fctVar)
      end
  | addFct (sym,fct,_) =
      let val slot = namedLvar(sym)
          val fctVar = FCTvar{name=sym,access=PATH [slot],binding=fct}
      in (fctVar,fctVar) end;

(* When we have the parent, first we nee its signature to elaborate
   the signature of the functor argument
 *)

fun signatureOfStructure (INSTANCE{sign,...}) =sign
  | signatureOfStructure ERROR_STR = ERROR_SIG
  | signatureOfStructure _ = impossible "signatureOfStructure"

(*** ELABORATE A STRUCTURE ***)

(* When we start, the context is the context of the embedding structure *)

fun elaborate_structure
      (context as {kind,total,path,error,errorMatch,ln,stampgen,transform}) abstract
      {name, constraint, def} =
  (* elaborate the raw structure: without the signature constraint *)
  let fun elab (context as {kind,total,path,error,errorMatch,ln,stampgen,transform})
	       (StructStr ldec) =
	    (* the list of the symbols defined: can't be known easily 
               because of patterns... *)
	    let val strPath = rev path  (* who needs this reversed path??? *)
		val makestamp = Stamps.newStamp stampgen
		(* builds the kind of the new structure *)
		val (newKind,newTotal) =
		  case kind 
		  of SimpleStr _ => (SimpleStr(ref Env.empty), total)
		   | InstanceStr {symtotal,inFunctor,inArgument,parent,...} =>
		       let (* preallocation and building of the skeleton *)
			   val (strs,fcts,typs) = countStrFctTyp ldec
			   val env = ref (Env.empty:env)
			   val strArray = Array.array(strs,ERROR_STR)
			   val fctArray = Array.array(fcts,ERROR_FCT)
			   val typArray = Array.array(typs,ERRORtyc)
			   val kindinst = 
			     TOP{strcount=strs, fctcount=fcts, typecount=typs,
				 slotcount=0, sConstraints=[], tConstraints=[]}
			   val sgn =
			     SIG{symbols=ref [], env=env, path=NONE,
				 stamp=makestamp (), kind=ref kindinst}
			   val str =
			     INSTANCE{
			       sign = sgn, path = path (* was strPath *),
			       subStrs=strArray,
			       subFcts=fctArray, types=typArray,
			       origin = SELF (makestamp ())}
		       in
		       addStr1 (name,str,sgn,kind,path);
		       (InstanceStr{
			  env=env, symtotal=symtotal,myself=str,
			  strCount=ref 0, fctCount=ref 0, typCount=ref 0,
			  parent=parent, inFunctor=inFunctor,strArray=strArray,
			  inArgument=inArgument, fctArray=fctArray,
			  typArray=typArray}, total)
		       end
		   | FctBodyStr {symtotal,argument,inFunctor,inArgument} =>
		       let (* same as previous + slot for argument *)
			   val (strs,fcts,typs) = countStrFctTyp ldec
			   val strArray = Array.array(strs+1,ERROR_STR)
			   val fctArray = Array.array(fcts,ERROR_FCT)
			   val typArray = Array.array(typs,ERRORtyc)
			   val argumentVar = 
			     STRvar{
			       binding =
				 STR_FORMAL{
				   pos=0,
				   spec=signatureOfStructure argument},
			       name = name_A, access = PATH []}
			   val argBind = STRbind argumentVar
			   val env = ref(Env.bind(name_A,STRbind argumentVar,
						  Env.empty))
			   val kindinst = 
			     TOP{strcount=strs+1,fctcount=fcts,typecount=typs,
				 slotcount=0, sConstraints=[], tConstraints=[]}
			   val sgn =
			     SIG{symbols=ref [], env=env, path=NONE,
				 stamp=makestamp (), kind=ref kindinst}
			   val str =
			     INSTANCE{
			       sign = sgn,  path = path,  (* was strPath, why?? *)
			       subStrs=strArray, subFcts=fctArray,
                               types=typArray,
			       origin = SELF(makestamp ())}
			   val _ = Array.update(strArray,0,argument)
			   val newTotal=
			     Env.bind
                               (name_P,
                                STRbind(STRvar
                                  {name=name_P,access=PATH[],binding=str}),
				total)
		       in
		       (InstanceStr{
			 env = env, symtotal = symtotal,myself=str,
			 strCount = ref 1, fctCount = ref 0, typCount = ref 0,
			 parent=str, inFunctor=inFunctor,inArgument=inArgument,
			 strArray = strArray,
			 fctArray = fctArray,
			 typArray = typArray}, newTotal)
		       end
                   | TopEnv _ => (SimpleStr (ref Env.empty), total)
	 	(* Builds the new context with the new kind *)
		val newContext = {
			kind = newKind, total = newTotal,
			path = path, error = error, errorMatch = errorMatch,
			ln = ln, stampgen = stampgen, transform = transform}
		(* elaborate the environment from the spec *)
		val (decStr,envStr) = elabDecl newContext ldec
		 (* fix paths and thinnings, fixes the set of symbols *)
		 val (resultat,locations) = 
		       case newKind 
		       of SimpleStr env =>
			    let val (nenv,loc,_)=recomputePath (!env)
			    in (SIMPLE{stamp=makestamp (),path=path,
				       env=nenv}, loc) end
			| InstanceStr{myself as INSTANCE{sign=SIG{symbols,... },
						      ...}, env, ... } =>
			    let val (nenv,loc,symb)=recomputePath (!env)
			    in env:=nenv;
			       symbols:= symb;
			       (myself,loc) end
			| _ => impossible "elaborate: resultat"
	     in 
	     (resultat,STRUCTstr{body=[decStr],str=resultat,locations=locations})
            end
	| elab (context as {kind,total,path,error,errorMatch,ln,stampgen,transform})
	       (AppStr (funcName ,[(arg,b)])) =
	    let val _ = if DEBUG then say "a\n" else ()
		val strPath = path
		(* a newKind to build the argument *)
		val (inArgument,newKind) =
		  case kind
		  of SimpleStr env => (fn _ => false,SimpleStr(ref (!env)))
		   | TopEnv _ => (fn _ => false,kind)
		   | FctBodyStr {inArgument,...} => (inArgument,kind)
		   | InstanceStr{env,symtotal,myself,strCount,fctCount,
				 typCount,parent,inFunctor,inArgument,strArray,
				 fctArray,typArray,...} =>
		       (inArgument,
			InstanceStr{
			  env = ref(!env), symtotal = ref(!symtotal),
			  myself=myself,strCount = ref(!strCount),
                          fctCount = ref 0, typCount = ref 0,parent=parent,
			  inFunctor=inFunctor, strArray = strArray,
			  fctArray = fctArray, typArray = typArray,
			  inArgument=inArgument})
		(* the context that goes with it *)
		val localContext = {
			kind = newKind, total = total, errorMatch=errorMatch,
			path = [name_X], error = error, ln = ln,
			stampgen = stampgen, transform = transform}
		val _ = if DEBUG then say "b\n" else ()
		(* call elaborate on the argument *)
		val (argDec as STRB{strvar=STRvar{binding=argStr,...},
				    def=argExp,...},_) = 
		  elaborate_structure 
		    localContext false
		    {def=arg,constraint=NONE,name=name_X}
		val _ = if DEBUG then say "c\n" else ()
		(* look for the functor definition *)
		val (fctVar as FCTvar{binding=funcFct,...}) = 
		  lookFCT (total,funcName,error ln) 
		val _ = if DEBUG then
		          with_pp (ErrorMsg.defaultConsumer())
			   (fn ppstrm =>
			     (PPBasics.ppFunctor ppstrm (total,funcFct,20);
			      add_newline ppstrm))
			else ()
	        (* apply the functor to its argument *)
		val (res,argThin) =
		  ApplyFunctor.applyFunctorFull
                    (funcFct,argStr,stampgen,strPath,error ln,
		     total,SigMatch.match)
		val _ = if DEBUG then say "e\n" else ()
		(* get rid of the info on the application if unnecessary *)
		val simpRes = 
		  if inArgument (getFctStamp funcFct) then res
		  else case res of APPLY{res,...} => res
				 | ERROR_STR => res
				 | _ => impossible "strange result of APPLY" 
		val resDec =
		  APPstr {oper=fctVar,argexp=argExp,argthin=argThin,
			  str=simpRes}
		val _ = if DEBUG then say "f\n" else ()
	    in addStr1(name,simpRes,FULL_SIG,kind,path);(simpRes,resDec) end
        | elab context (AppStr(qid,arg :: larg)) =
            elab context
		 (LetStr (StrDec[Strb{name=hiddenId,constraint=NONE,
                                            def=AppStr(qid,[arg])}],
                                AppStr([hiddenId,functorId],larg)))
        | elab context (AppStr(qid,[])) =
            impossible "elabStrRaw.AppStr"

        | elab (context as {kind,total,path,error,errorMatch,ln,stampgen,transform}) 
	      (VarStr name')=
            let val (var as STRvar{binding,...}) =
                  lookSTR (total,name',error ln)
            in addStr1(name,binding,FULL_SIG,kind,path);
	       (binding,VARstr var) end
	| elab (context as {kind,total,path,error,errorMatch,ln,stampgen,transform})
	      (LetStr (ldec,str)) =
            let (* context with new env reference *)
		 val after_local = 
		   case kind
		   of InstanceStr{env,...} => 
			let val old_env = !env in fn () => env := old_env end
		    | SimpleStr env =>
			let val old_env = !env in fn () => env := old_env end
		    | _ => (fn () => ())
		 val (ld1,env1) =  elabDecl context ldec
		 val _ = after_local ()
		 (* context with augmented env *)
		 val c2 = 
		   {kind=kind, total= Env.atop (env1,total), path=path,
		    error=error, ln=ln, stampgen=stampgen, transform=transform,
		    errorMatch=errorMatch}
		 val (binding,absstr) = elab c2 str
	     in (binding,LETstr(ld1,absstr)) end
        | elab {kind,total,path,error,errorMatch,ln,stampgen,transform} 
	      (MarkStr(str,l1,l2)) = 
	    let val (binding,str') =
		     elab {kind=kind,total=total,path=path,error=error,
			   errorMatch=errorMatch,
			   ln=(l1,l2),stampgen=stampgen,transform=transform} 
		          str
	    in (binding,MARKstr(str',l1,l2))
	    end

      (* Build the uncoerced structure from its specification *)
      val (strRaw,strDec) = elab context def
      (* Coerce it if necessary and get back the thinning *)
      val ((coercedStr,thin),sgnOption) = 
	case constraint
	of NONE => ((strRaw,NONE),NONE)
	 | SOME sgn =>
	     let (* translate the signature in the new environment *)
		 val sgnElab = 
		   elabTOPsig (error,ln)
			      (total,Stamps.newGenStamp stampgen,NONE)
			      sgn
 		 fun computeSelf (StructStr _) = true
 		   | computeSelf (MarkStr(def,_,_)) = computeSelf def
 		   | computeSelf _ = false
 		 val self = computeSelf def
	     in
	     (* translate the structure in a coerced one *)
	     (SigMatch.match
	        {abstract=abstract,arg_option=NONE,err=error ln,scope=stampgen,
                 spath=path,printEnv= total,self=self,
                 str=strRaw,sign=sgnElab}, 
              SOME sgnElab)
	     end
     (* Fix the version of the structure stored in the environments *)
     val (resVar,symbind) = addStr2(name,coercedStr,kind)
  in 
  (STRB{strvar=resVar,thin=thin,constraint=sgnOption,def=strDec},
   symbind)
  end

(*** ELABORATE A FUNCTOR ***)

and elaborate_functor (context as {kind,total,path,error,errorMatch,
				   ln,stampgen,transform})
                      (name, VarFct (spath,constraint)) =
      let (* looks for the functor *)
	  val (fctvar as FCTvar{binding=fct,...}) =
	    lookFCT(total,spath,error ln)
	  (* coerce it if necessary *)
	  val ((fctcoerced,thin),constraint') =
	    case constraint
	    of NONE => ((fct,NONE),NONE)
	     | SOME fsig =>
		let val (parent,symtotal) =
		      case kind
		      of InstanceStr{symtotal,parent,...} => 
			   (signatureOfStructure parent,ref (!symtotal))
		       | _ => (ERROR_SIG,ref Normalize.empty)
		    (* signature context to elaborate the functor signature *)
		    val context =
		      {strs=ref 0, fcts=ref 0, tycons=ref 0, slots=ref 0,
		       total=ref total, makeStamp=Stamps.newGenStamp stampgen,
		       inner=ref Env.empty, parent=parent,symtotal=symtotal,
		       names = [], t = ref [], s = ref []}
		    val fsig = 
		      ElabSig.make_fsigexp (error,ln) NONE context fsig
		in 
		(* verifies that the signature matches the functor *)
		(SigMatch.matchFct 
		   {abstract=false, err=error ln, spath=path, self=false,
		    scope=stampgen, printEnv=total, fsig=fsig, fct=fct},
		 SOME fsig)
		end
          val (thinIn,thinOut) = case thin of NONE => (NONE,NONE) | SOME p => p
	  (* add the definition obtained to the environments *)
	  val (realVar,formalVar) = addFct(name,fctcoerced,kind)
      in (FCTB{fctvar=realVar,
	       def=VARfct{thinIn=thinIn,thinOut=thinOut,
			  constraint=constraint',def=fctvar}},
	  FCTbind formalVar) 
      end
  | elaborate_functor 
	(context as {kind,total,path,error,errorMatch,ln,stampgen,transform})
	(name,LetFct (ldec,fct)) =
     let (* context with new env reference *)
	  val after_local = 
	    case kind
	    of InstanceStr{env,...} => 
		 let val old_env = !env in fn () => env := old_env end
	     | SimpleStr env =>
		 let val old_env = !env in fn () => env := old_env end
	     | _ => (fn () => ())
	  val (ld1,env1) =  elabDecl context ldec
	  val _ = after_local ()
	  (* context with augmented env *)
	  val c2 = 
	    {kind=kind, total= Env.atop (env1,total), path=path,
	     error=error, ln=ln, stampgen=stampgen, transform=transform,
	     errorMatch=errorMatch}
	  val (FCTB{fctvar, def}, binding) = elaborate_functor c2 (name,fct)
	  val absfct = FCTB{fctvar=fctvar,def=LETfct(ld1,def)}
      in (absfct,binding) end

  | elaborate_functor (context as {kind,total,path,error,errorMatch,
				   ln,stampgen,transform})
                      (name, MarkFct(fct,l1,l2)) =
      elaborate_functor 
        {kind=kind, total=total, path=path, error=error,errorMatch=errorMatch,
         stampgen=stampgen, transform=transform, ln=(l1,l2)}
        (name,fct)
  | elaborate_functor context (name,AppFct(qid,larg,constraint)) =
      elaborate_functor context
	   (name,LetFct (StrDec[Strb{name=hiddenId,constraint=NONE,
				def=AppStr(qid,larg)}],
		    VarFct([hiddenId,functorId],constraint)))

  | elaborate_functor context (_,FctFct{params=[],...}) =
      impossible "elaborate_functor.FctFct"
  | elaborate_functor (context as {kind,total,path,error,errorMatch,
				   ln,stampgen,transform})
                      (name, FctFct{params=[param],body=def,constraint}) =
      let (* to identify what has been elaborated during the
	     treatment of the functor *)
	  val argStampgen = Stamps.newBoundScope ()
	  val inParam = Stamps.isBound argStampgen
	  val bodyStampgen = Stamps.newBoundScope ()
	  val inBody = Stamps.isBound bodyStampgen
	  val makestamp = Stamps.newStamp stampgen
	  (* basic infos on the context of elaboration *)
	  val (inFunctor,inArgument,parent,symtotal) = 
	    case kind
	    of InstanceStr{inFunctor,inArgument,parent,symtotal,...} =>
		 (inFunctor,inArgument,parent,!symtotal)
	     | _ => 
		 (fn _ => false,fn _ => false,ERROR_STR,Normalize.empty)
	  (* get back the signature of the parent *)
	  val sigParent = signatureOfStructure parent
	  val (nameParam,specParam) =
	    case param of (NONE,s) => (name_X,s) | (SOME n,s) => (n,s)
	  val accParam = namedLvar (nameParam)
	  val symtotalParent = Normalize.liftEnv (name_P,symtotal)
	  val symtotalAll = ref symtotalParent
	  (* elaborate the signature of the argument (pair parent + parameter) *)
	  val sgnArg = 
		elabPARTIALsig 
		  (error,ln)
		  (name_P,sigParent,total,symtotalAll,
		   Stamps.newGenStamp argStampgen)
		  (SigSig[StrSpec[(name_X,specParam)]])
	  (* this signature is instantiated with the real parent to substitute
	     in place of the parent signature *)
	  val strArg = Instantiate.instantiate_argument nameParam
			 ([],argStampgen,error ln) parent sgnArg
	  val _ = if !internals then
		    with_pp (ErrorMsg.defaultConsumer())
		      (fn ppstrm =>
		        (add_string ppstrm "Parameter Structure";
			 add_newline ppstrm;
			 PPBasics.ppStructure ppstrm
			   (total,strArg,!Print.printDepth);
			 add_newline ppstrm))
		  else ()
	  (* take back the definition of the parameter *)
	  val STRvar{binding=bindParam,...} = lookBindingSTR (strArg,[name_X])
	  val varParam =
		STRvar{name=nameParam,access=PATH[accParam],binding=bindParam}
	  (* gives a def of the var for the argument (dummy path) *)
	  val varArg = STRvar{binding=strArg,access=PATH[],name=name_A}
	  (* elaboration of the Body begins here *)
	  (* so the elaboration of the argument is what has been until now 
	     elaboration of parent is included *)
	  fun inArg s = 
	    (inFunctor s) orelse (inParam s)
	  (* for the body, identify all thatt has been done while
	     parsing parameters (their signature and their instantiation) *)
	  fun inFunctorBody s =
	    (inFunctor s) orelse (inParam s) orelse (inBody s)
	  fun inArgumentBody s = 
	    (inArgument s) orelse (inParam s)
	  (* adjust total and symtotal with informations on the parameter *)
	  val (newTotal,newSymtotal) =
	    case param 
	    of (NONE,_) => (openStructureVar(total,varParam),
			       Normalize.openX(name_X,sgnArg,symtotalParent))
	     | (SOME _,_) => 
		 (Env.bind(nameParam, STRbind varParam, total), 
		  Normalize.bind(nameParam,[name_X],symtotalParent))
	  (* a new kind and a new context for the body *)
	  val newKind =
	     FctBodyStr {
	       inFunctor = inFunctorBody, inArgument = inArgumentBody,
	       argument = strArg,
	       symtotal = ref (Normalize.liftEnv (name_A,newSymtotal))}
	  val newContext:contextStr = 
	    {kind=newKind,total=newTotal,path=[],
	     error=error,errorMatch=errorMatch,
	     ln=ln,stampgen=bodyStampgen,transform=transform}
	  val _ = if DEBUG then say "7\n" else ()
	  (* the body is built *)
	  val (STRB{strvar,thin,constraint,def=defBody},bodyBind) =
	    elaborate_structure newContext false
				{name=name_B,def=def,constraint=constraint}
	  val STRvar{binding=strBody,...} = strvar
	  val _ = if !internals then
		    with_pp (ErrorMsg.defaultConsumer())
		      (fn ppstrm =>
		        (add_string ppstrm "Body Structure";
			 add_newline ppstrm;
			 PPBasics.ppStructure ppstrm
			   (newTotal,strBody,!Print.printDepth);
			 add_newline ppstrm))
		  else ()
	  (* Then it is abstracted *)
	  val fctBody =
		AbstractFct.abstractBody (strBody,strArg,inBody,inArg)
	  (* So that the  functor can be built *)
	  val fct =
		FCT{stamp = makestamp (), 
		    parent = parent,
		    paramName = nameParam,
		    argument = sgnArg,
		    body = fctBody}
	  (* and added to the environments *)
	  val (realVar,formalVar) = addFct(name,fct,kind)
      in (FCTB{fctvar = realVar,
	       def = FCTfct{param = varParam, def = defBody, thin = thin,
			    constraint = constraint}},
	  FCTbind formalVar)
      end
  | elaborate_functor context 
	(name,FctFct{params=param :: lparam,body,constraint}) =
      elaborate_functor context
	(name,FctFct{params=[param],
                     body=StructStr(FctDec[Fctb
                       {name=functorId,
                        def=FctFct{params=lparam,body=body,
                                   constraint=constraint}}]),
                        constraint=NONE})

(*** ELABORATE A DECLARATION (STRUCTURE LEVEL) ***)

and elabDecl (context as {kind,total,path,stampgen,error,
			  errorMatch,ln,transform}) =
  fn (OpenDec strs) =>
        let fun makeOpenDecls (str, strPath) =
		let (* get a list of component names (symbols) from a structure *)
		    fun getSymbols(SIMPLE{env,...}) =
			  let val r = ref([]: Symbol.symbol list)
			   in Env.app (fn (s,_) => r := s::(!r)) env;
			      !r
			  end
		      | getSymbols(INSTANCE{sign=SIG{symbols,...},...}) = !symbols
		      | getSymbols ERROR_STR = []
		      | getSymbols _ = ErrorMsg.impossible "getSymbols"
		    fun makeDecl(comp: Symbol.symbol, decls : Ast.dec list)
			  : Ast.dec list =
			case Symbol.nameSpace comp
			  of VALspace =>
			      (case lookBinding(str,[comp],[])
				 of VARbind(VALvar _) =>
				      (* ignore OVLDvar variables *)
				      ValDec([Vb{pat=VarPat[comp],
						 exp=VarExp([localStrName,comp])}])
				      :: decls
				  | CONbind(DATACON{rep=VARIABLE _,...}) =>
				      ExceptionDec([EbDef{exn=comp,
						       edef=([localStrName,comp])}])
				      :: decls
				  | CONbind(DATACON{rep=VARIABLEc _,...}) =>
				      ExceptionDec([EbDef{exn=comp,
						       edef=([localStrName,comp])}])
				      :: decls
		                  | _ => decls) (* ordinary datacon *)
			   | STRspace =>
			       if Symbol.eq(comp, Extern.name_A)
			       then decls (* ignore "<Argument>" *)
			       else StrDec[Strb{name=comp,
						def=VarStr([localStrName,comp]),
						constraint=NONE}]
			            :: decls
			   | FCTspace =>
			       FctDec[Fctb{name=comp,
					   def=VarFct([localStrName,comp],NONE)}]
			       :: decls
			   | _ => decls
		in  LocalDec(StrDec[Strb{name=localStrName,
					 def = VarStr(strPath),
					 constraint = NONE}],
			     SeqDec(fold makeDecl (getSymbols str) []))
		end
	    (* get the definition of structures *)
	    val lstr = map (fn id => lookSTR(total,id,error ln)) strs
	    (* open their environment *)
	    val openEnv =
              revfold 
		(fn (str,env) => openStructureVar(env,str))
		lstr Env.empty
	    (* if the kind is Instance there is a lot of work to do:
		- create a dummy that contains the defs obtained in openEnv
		- build the indirection in the current environment
		- add the dummy to the current structure
	       The reason for this: we must only use one slot per open *)
            val _ =
	      case kind
	      of InstanceStr{env,symtotal,strCount,strArray,...} =>
		   let val strC = ref 0 and fctC = ref 0 and typC = ref 0
		       val strA = ref [] and fctA = ref [] and typA = ref []
		       val auxEnv = ref Env.empty
		       val posOpen = next strCount val openEnv' = ref Env.empty
		       (* openStruct builds the new dummy and record the new
			  position of each element *)
		       fun openStruct 
			       (name,
			        orig as STRbind(STRvar{binding,access,...}))=(
			     let val pos = next strC
				 val auxBind =
				   STRbind(STRvar{
				     name=name,access=access,
				     binding=
				       STR_FORMAL{pos=pos,spec=FULL_SIG}})
				 val openBind =
				   STRbind(STRvar{
				     name=name,
				     binding=STR_OPEN{pos=[posOpen,pos],
						spec=FULL_SIG,name=[name]},
				     access=access})
			     in 
			     strA := binding :: (!strA);
			     auxEnv := Env.bind(name,auxBind,!auxEnv);
			     openBind end)
		         | openStruct 
			       (name,
			        orig as FCTbind(FCTvar{binding,access,...}))=(
			     let val pos = next fctC
				 val auxBind =
				   FCTbind(FCTvar{
				     name=name,access=access,
				     binding=
				       FCT_FORMAL{pos=pos,spec=FULL_FSIG}})
				 val openBind =
				   FCTbind(FCTvar{
				     name=name,
				     binding=FCT_OPEN{pos=[posOpen,pos],
						spec=FULL_FSIG,name=[name]},
				     access=access})
			     in 
			     fctA := binding :: (!fctA);
			     auxEnv := Env.bind(name,auxBind,!auxEnv);
			     openBind end)
			 | openStruct (name,orig as TYCbind binding) = (
			     let val pos = next typC
				 val auxBind =
				   TYCbind(FORMtyc{name=name,pos = pos,
					 	   spec=binding})
				 val openBind =
				   TYCbind(OPENFORMtyc{
					pos=([posOpen],pos),spec=binding,
					name=[name]})
			     in 
			     typA := binding :: (!typA);
			     auxEnv := Env.bind(name,auxBind,!auxEnv);
			     openBind end)
			 | openStruct (name,binding) = binding
			(* apply openStruct on the whole environment *)
		       val _ = 
			 Env.app
			   (fn binding as (name,_) => 
			     env := Env.bind(name,openStruct binding,!env))
			   openEnv;
			(* the kind of the intermediate with counts coming
			   from the application of openStruct *)
		       val kindinst = 
			 TOP{strcount= !strC,fctcount= !fctC,typecount= !typC,
			     slotcount=0, sConstraints=[], tConstraints=[]}
			(* the definition of the intermediate structure *)
		       val openStr =
			 INSTANCE{sign = SIG{symbols=ref [],env=auxEnv,
					     kind=ref kindinst,path = NONE,
					     stamp =
					       Stamps.newStamp stampgen ()},
				   subStrs = Array.arrayoflist(rev (!strA)),
				   subFcts = Array.arrayoflist(rev (!fctA)),
				   types = Array.arrayoflist(rev (!typA)),
				   origin = SELF (Stamps.newStamp stampgen ()),
				   path = rev (name_O :: path)}
		   in
		   Array.update(strArray,posOpen,openStr)
		   end
	       | SimpleStr env => env := Env.atop(openEnv,!env)
	       | TopEnv _ => ()
	       | FctBodyStr _  => impossible "elabDecl:open:FctBodyStr"
         in if isTop kind
	    then let val newDecs = 
		         fold (fn ((str,strPath),decs) =>
				    makeOpenDecls(str,strPath) :: decs)
			 (List2.zip2(map (fn STRvar{binding,...} => binding)
				         lstr,
				     strs)) []
		     val (SEQdec ld1,e1) = elabDecl context (SeqDec newDecs)
		 in  (SEQdec(OPENdec lstr :: ld1),  (* OPENdec is misplaced
						       but should be a no-op *)
		      Env.consolidate(Env.atop(e1,openEnv)))
		 end
	    else (OPENdec lstr,openEnv)
        end
   | (StrDec pstrl) =>
        let fun elabStrb ln (Strb (pstrb as {name,...}),(ldec,env)) =
		  let val (dec as STRB{strvar,...},str) =
			elaborate_structure 
			  {kind=kind,total=total,path=name::path,
			   stampgen=stampgen,error=error,ln=ln,
			   errorMatch=errorMatch,transform=transform}
			  false pstrb
		  in (dec::ldec, Env.bind(name,STRbind strvar,env)) end
	      | elabStrb _ (MarkStrb(s,l1,l2),ctx) = elabStrb (l1,l2) (s,ctx)
            val (strbl,env) = 
	      revfold (elabStrb ln) pstrl ([],Env.empty)
        in (STRdec (rev strbl),env) end
   | (AbsDec pstrl) =>
        let fun elabAbs ln (Strb(pstrb as {name,...}),(ldec,env)) =
		  let val (dec as STRB{strvar,...},_) =
			elaborate_structure 
			  {kind=kind,total=total,path=name::path,
			   stampgen=stampgen,error=error,ln=ln,
			   errorMatch=errorMatch,transform=transform}
			  true pstrb
		  in (dec::ldec, Env.bind(name,STRbind strvar,env)) end
	      | elabAbs _ (MarkStrb(s,l1,l2),ctx) = elabAbs (l1,l2) (s,ctx)
            val (strbl,env) =
	      revfold (elabAbs ln) pstrl ([],Env.empty)
        in (STRdec (rev strbl),env) end
   | (FctDec pfctl) =>
        let fun elabFctb ln (Fctb{name,def},(ldec,env)) =
		  let val (dec as FCTB{fctvar,...},_) =
			elaborate_functor context (name,def)
		  in (dec::ldec, Env.bind(name,FCTbind fctvar,env)) end
	      | elabFctb _ (MarkFctb(fctb,l1,l2),ctx) = 
		  elabFctb (l1,l2) (fctb,ctx)
            val (fctbl,env) = revfold (elabFctb ln) pfctl ([],Env.empty)
        in (FCTdec (rev fctbl),env) end
   | (SigDec psigbl) =>
       let fun elabSigb ln  (Sigb{name,def},(ldec,env)) =
		 let val sgndef = 
		       elabTOPsig 
			 (error,ln) 
			 (total,Stamps.newGenStamp stampgen,SOME name) def
		     val sgn =
		       SIGvar{name=name, binding= sgndef}
		     val sigbind = SIGbind sgn
		 in (sgn::ldec,Env.bind(name,sigbind,env)) end
	     | elabSigb _ (MarkSigb(s,l1,l2),ctx) = elabSigb (l1,l2) (s,ctx)
	   val (sigbl,env) = revfold (elabSigb ln) psigbl ([],Env.empty)
       in (SIGdec (rev sigbl),env) end
   | (FsigDec pfsigbl) =>
       let fun elabFsigb ln (Fsigb{name,def},(ldec,env)) =
                  let val (ldec',env') = 
		    elabFSIGB (error,ln) (#total context,stampgen) (name,def)
                  in (ldec'@ldec,Env.atop(env',env)) end
	     | elabFsigb  _ (MarkFsigb(s,l1,l2),ctx) = 
		 elabFsigb (l1,l2) (s,ctx)
           val (fsigbl,env) = 
             revfold (elabFsigb ln) pfsigbl ([],Env.empty)
       in (FSIGdec (rev fsigbl),env) end
   | (LocalDec (ldec1,ldec2)) =>
       let (* context with new env reference *)
           val local_kind =
              case kind
               of TopEnv _ => TopEnv false
                | _ => kind

	   val after_local = 
	     case kind
             of InstanceStr{env,...} => 
		  let val old_env = !env in fn () => env := old_env end
	      | SimpleStr env =>
		  let val old_env = !env in fn () => env := old_env end
	      | _ => (fn () => ())

           val c1 =
             {kind=local_kind, total=total, path=path,errorMatch=errorMatch,
              error=error, ln=ln, stampgen=stampgen, transform=transform}
           val (ld1,env1) =  elabDecl c1 ldec1

	   val _ = after_local ()
	   (* context with augmented env *)
	   val c2 = 
	     {kind=kind, total= Env.atop (env1,total), path=path,
	      errorMatch=errorMatch,
	      error=error, ln=ln, stampgen=stampgen, transform=transform}
	   val (ld2,env2) = elabDecl c2 ldec2
       in (LOCALdec(ld1,ld2),env2) end
   | (SeqDec ldec) => 
	let val (dec',env) = elaborateEnv context ldec
	in (SEQdec dec',env) end
   | (MarkDec (dec,l1,l2)) => 
	let val (dec',env) = 
	    elabDecl {total=total,kind=kind,path=path,error=error,ln=(l1,l2),
		      errorMatch=errorMatch,stampgen=stampgen,
		      transform=transform} dec
	in 
	(if !System.Control.markabsyn then MARKdec(dec',l1,l2) else dec',env)
        end
   | dec =>
        let val (decl',env',tyv,updt) =
              ElabCore.elabDec (error,errorMatch,ln) (total,path,stampgen) dec
	    val _ = updt tyv
	    val decl'' = transform decl'
            val _ = Env.app (fn (name,obj) => (addObject(name,obj,kind,path);
                                               ())) env'
	    val toplevel = 
		case kind
		  of TopEnv false => false (* local case *)
		   | _ => true  (* structure bodies considered toplevel
				   for type checking *)
            val ndecl = Typecheck.decType(Env.atop (env',total), decl'',
                                          toplevel, error,ln)
	 in (ndecl,env')
	end

and elaborateEnv {total,kind,path,error,errorMatch,ln,stampgen,transform}
		 ldec = 
  let val (ldec,env') = 
      revfold
	(fn (decl,(ld,env)) =>
	   let val (ld',env') =
		 elabDecl {total=Env.atop (env,total),kind=kind,path=path,
			   error=error,errorMatch=errorMatch,ln=ln,
			   stampgen=stampgen, transform=transform} 
			  decl
	   in (ld' :: ld,Env.atop(env',env)) end)
	ldec ([],Env.empty)
   in (rev ldec,env') 
  end


fun elaborateTop(lpdec,env,error,errorMatch,transform) = 
  let val strContext = 
	{kind=TopEnv true, total=env, error=error,ln=(0,0),
	 errorMatch=errorMatch, path=[], stampgen = Stamps.freeScope,
	 transform=transform}
      val (ldec,env) = elabDecl strContext lpdec
   in (ldec,env) 
  end

end (* local *)

end;
