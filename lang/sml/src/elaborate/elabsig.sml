(* Copyright 1992 by AT&T Bell Laboratories *)

structure ElabSig:ELABSIG =  struct

local
  open Extern
  open Symbol Absyn Ast PrintUtil AstUtil Types BasicTypes TyvarSet
  Modules EqTypes ModuleUtil TypesUtil Variables Misc ElabUtil  ErrorMsg
  Access ElabCore
  infix -->
in


(* type context:
       strs: the number of structures declared in the signature
       fcts: the number of functors declared in the signature
       tycons: the number of types declared in the signature,
       slots: the number of slots for values in the structure record
       inner: the current local environment
       total: the complete environment
       names: a stack of the names of the enclosing structures.
              There is no name for the outermost environment
	      enclosing the declaration of a top-level signature.
       s: a list of structure sharing constraints.  All sharing
          contraints for embedded signatures are moved to the
	  top-level signatures enclosing them.   When this is done,
	  symbolic constraints are normalized relative to the
	  enclosing top-level signature.
       t: a list of type sharing constraints, 

invariants: |names| = |enclosing|-1
*)

type context = {strs:int ref,tycons:int ref,slots:int ref,fcts:int ref,
		inner: env ref,	parent: Signature, symtotal: Normalize.env ref,
		names: symbol list,makeStamp:unit->Stamps.stamp,total:env ref,
		s:{internal:spath list,external:Structure option} list ref,
		t:{internal:spath list,external:Types.tycon option} list ref}

fun next r = (!r) before (inc r)
val functorId = Symbol.fctSymbol "<functor>"

fun bind (sym,binding,{inner,total,symtotal,names,...} : context, err) =
       (Env.look (!inner,sym);
        err COMPLAIN ("duplicate specifications for "
		      ^Symbol.nameSpaceToString(Symbol.nameSpace sym)
		      ^" "^Symbol.name sym^" in signature"))
            nullErrorBody
          handle Env.Unbound =>
	     (inner := Env.bind(sym,binding,!inner);
	      total := Env.bind(sym,binding,!total);
	      symtotal := Normalize.bind(sym,sym::names,!symtotal))

val addExistingConstraints = fn ({t,s,names,...}: context,
                                 {tConstraints=t',sConstraints=s'}) =>
    let val names' = rev names
        val prefix = fn {internal: spath list,external: 'a} =>
	      {internal = map (fn qid => names' @ qid) internal,
	       external=external}
    in t := ((map prefix t') @ !t);
       s := ((map prefix s') @ !s)
    end

local 

   (* adjustBinding: adjust all the slots and relative positions
      of bindings in an included SIGNATURE.*)

   fun adjustBinding (basetype,basestr,basefct,baseslot,makeStamp,redef) =
     let fun adjust baseslot binding =
	  case binding
	  of VARbind v =>
	         VARbind (case v
			 of VALvar {access=SLOT i,typ,name} =>
			      VALvar {access=SLOT (i+baseslot),
				      typ=ref(adjustType (!typ)),name=name}
		          | ERRORvar => ERRORvar
			  | _ => impossible "Sign.adjustbinding/VARbind")
          | CONbind (DATACON{name,const,typ,rep,sign}) =>
	      CONbind (DATACON{name=name,const=const,typ=adjustType typ,
			       rep=case rep
			           of VARIABLE(SLOT i) =>
				             VARIABLE(SLOT(i+baseslot))
				    | VARIABLEc(SLOT i) =>
					     VARIABLEc(SLOT(i+baseslot))
				    | _ => rep,sign=sign})
          | TYCbind tyc => TYCbind (adjustTycon tyc)
	  | STRbind(STRvar{name,access=SLOT k,binding=STR_FORMAL{pos,spec}}) =>
	       let val binding =
		     STR_FORMAL{pos=pos+basestr,spec=
			    case spec
			    of SIG {symbols,env,kind=ref EMBEDDED,...} =>
				   SIG{symbols=symbols,
				       path=NONE,
				       stamp=makeStamp(),
				       env=ref (Env.map (adjust 0) (! env)),
				       kind=ref EMBEDDED}
			     | _ => spec}
                in STRbind(STRvar{name=name,access=SLOT (k+baseslot),
				  binding=binding})
		end
          | STRbind(STRvar{binding=ERROR_STR,...}) => binding
	  | FCTbind(FCTvar{name,access=SLOT k,binding=FCT_FORMAL{pos,spec}}) =>
               FCTbind(FCTvar{name=name,access=SLOT (k+baseslot),
			      binding=FCT_FORMAL{pos=pos+basefct,spec=spec}})
          | FCTbind(FCTvar{binding=ERROR_FCT,...}) => binding
          | FIXbind _ => binding
          | _ => impossible "adustbinding"
        and relocate pos =
              let fun reloc [] = pos+basetype
                    | reloc ((porig,pdest)::l) =
                         if pos = porig then pdest else reloc l
              in reloc redef end
        and adjustType ty =
	    case ty
	    of (CONty (tycon,tylist)) =>
		    mkCONty(adjustTycon tycon,map adjustType tylist)
             | (POLYty{sign,tyfun=TYFUN{arity,body},abs}) =>
	           POLYty{sign=sign,
			  tyfun=TYFUN{arity=arity,body=adjustType body},
		          abs=abs}
             | ty => ty
       and adjustTycon tycon =
	   case tycon
	   of FORMtyc{pos,name,spec} =>
              FORMtyc{pos=relocate pos,
		      name=name,
		      spec=
		 case spec
		 of GENtyc{stamp,arity,eq,path,kind} =>
		      GENtyc{stamp=stamp,arity=arity,eq=eq,path=path,
			   kind=
			   case !kind
			   of FORMtyck => kind
			    | DATAtyc dl =>
			       ref (DATAtyc
			         (map (fn DATACON {name,const,typ,rep,sign} =>
				       (DATACON{name=name,const=const,
					      typ=adjustType typ,
					      rep=rep,sign=sign})) dl))
			    | _ => impossible "specTB.1"}
		  | _ => impossible "specTB.2"}
	    | RELtyc {name,pos=([],offset)} =>
		        RELtyc {name=name,pos=([],relocate offset)}
            | RELtyc {name,pos=(h::t,offset)} =>  
(* drt: fix: we need to adjust this by the structure offset *)
		       RELtyc {name=name,pos=((h+basestr)::t,offset)}
	    | tyc => tyc
   in adjust baseslot
   end
in
fun make_includespec (ID,err) 
                     (context as {strs,tycons,fcts,slots,total,makeStamp,...})=
  case lookSIG (!total,ID,err)
  of SIG{symbols,kind=ref(TOP {sConstraints=s,tConstraints=t,fctcount,
			       strcount,typecount,slotcount}),env,...} =>
    let val redef = 
	  let val accu = ref [] 
	  in 
	  Env.app
	    (fn (name,TYCbind(FORMtyc
		        {pos,spec=spec as GENtyc{kind=ref FORMtyck,
			 arity,eq=ref eq,...},name=name'})) => ((
		  case Env.look (!total,name)
	          of TYCbind(FORMtyc
		       {pos=pos2,spec=GENtyc{arity=arity2,
					     eq=ref eq2,...},...}) =>
			  if arity=arity2 
			  then accu := (pos,pos2) :: !accu
			  else bind(name,TYCbind(FORMtyc{pos=pos+ !tycons,
							 spec=spec, name=name'}
					         ),context,err)
		   | _ =>
		       bind(name,TYCbind(FORMtyc{pos=pos+ !tycons,spec=spec,
						 name=name'}),context,err))
		  handle Env.Unbound => 
		       bind(name,TYCbind(FORMtyc{pos=pos+ !tycons,spec=spec,
						 name=name'}),context,err))
	      | _ => ()) (!env);
	  !accu
	  end
	val adjust = adjustBinding (!tycons,!strs,!fcts,!slots,makeStamp,redef)
	fun mem_assoc name [] = false
	  | mem_assoc name ((name',_)::l) = 
	      (name=name') orelse (mem_assoc name l)
    in
    Env.app (fn (name,binding as TYCbind (FORMtyc
			{spec=GENtyc{kind=ref FORMtyck,...},...})) => ()
	      | (name,binding) => bind(name,adjust binding,context,err))
	    (!env);
    addExistingConstraints(context,{sConstraints=s,tConstraints=t});
    strs := strcount + !strs;
    fcts := fctcount + !fcts;
    tycons := typecount + !tycons;
    slots := slotcount + !slots;
    rev(!symbols)
    end
  | ERROR_SIG => nil
  | _ => impossible "make_includespec"
end
         
fun make_openspec (strpaths: spath list, err) 
                  ({total=total as ref env,
		    symtotal=symtotal as ref symenv,...}: context) =
     (app (fn p => total := openSigStructure (env,p,!total,err)) strpaths;
      app (fn p => (case lookSTR(env,p,fn _ => raise Normalize.Unbound)
		    of STRvar{binding=STR_FORMAL{spec,...},...} =>
			symtotal := Normalize.openStr(symenv,p,!symtotal,spec)
		      | STRvar{binding=STR_OPEN{spec,...},...} =>
			symtotal := Normalize.openStr(symenv,p,!symtotal,spec)
                     | _ => ())
	           handle Normalize.Unbound => ()) strpaths;
      nil) (* no bindings returned *)

fun make_dtyspec (error,ln) (context as {tycons,total,symtotal,...}:context) 
		 db =
    let fun predefine ln (Db{tyc=id,tyvars,def}) = 
	      let val r = ref(DATAtyc nil) 
		  val spec = GENtyc{path=[id],arity=length tyvars,
				    stamp=Stamps.null,
				    eq=ref DATA,kind=r}
		  val pos = next tycons
		  val tvs = elabTyvList error ln tyvars
		  val binding = TYCbind (FORMtyc{pos=pos,spec=spec,name=id})
	      in bind(id,binding,context,error ln); ((tvs,id,def,ln),r) end
	  | predefine ln (MarkDb(db,l1,l2)) = predefine (l1,l2) db

        (* add each constructor to the environment, checking whether env'
	   contains constructors whose names were used before in a val
	   spec or datatype spec *)
 
	fun redefine ((db as (tvs,name,parse_rhs,ln),r),(names,conlists)) = 
	      let val (r',env') = 
		    elabDB (Env.empty,(!total,SOME(!symtotal)),[],error) db
		  val conlist = map (fn (DATACON{name,...}) => name) r'
	      in r := DATAtyc(r');
                 Env.app (fn (name,binding) => (
			      bind(name,binding,context,error ln))) env';
		 (name :: names, conlist @ conlists)
	      end
 
	val pre = map (predefine ln) db
	val (tycbinds,alldconvals) = fold redefine pre ([],[])
     in tycbinds @ alldconvals end


fun make_tyspec(eq,tyvars,name,err) 
                   (context as {tycons,...}:context) =
    let val _ = checkbound(no_tyvars,tyvars,err)
	val pos = next tycons
	val eq = if eq then YES else UNDEF
	val spec = GENtyc{stamp = Stamps.null,
			  path = [name], arity = length tyvars,
			  eq = ref eq, kind = ref FORMtyck}
	val binding = TYCbind(FORMtyc{pos=pos,spec=spec,name=name})
    in bind(name,binding,context,err); [name] end

fun make_valspec(name,(ty,tv),err)
                (context as {slots,total,symtotal,...}:context) =
    let val typ = case get_tyvars tv
		   of [] => ty
		    | tvs => let val sign = TypesUtil.bindTyvars1 tvs
			      in POLYty{sign = sign, abs=0,
					tyfun = TYFUN{arity = length tvs, 
						      body = ty}}
			     end
	val _ = TypesUtil.compressTy typ
	val binding = 
	    VARbind(VALvar{name=[name],typ= ref typ,access=SLOT(next slots)})
     in bind(name,binding,context,err); [name] end

fun make_exnspec (name,err) (context as {slots,...}:context) =
  let val binding = CONbind(DATACON{name=name,const=true,typ=exnTy,sign=[],
				    rep=VARIABLEc(SLOT(next slots))})
  in bind(name,binding,context,err); [name] end

fun make_exnspecOF(name,(body,tv),err) 
                  (context as {slots,total,symtotal,...}:context) =
  let val typ = case get_tyvars tv
		 of nil => body --> exnTy
		  | _ => (err COMPLAIN ("type variable in exception spec: " ^
					Symbol.name name)
			    nullErrorBody;
			  WILDCARDty)
      val _ = TypesUtil.compressTy typ
      val binding = CONbind(DATACON{name=name, const=false, typ= typ,sign=[],
				    rep=VARIABLE(SLOT(next slots))})
  in bind(name,binding,context,err); [name] end

local
 fun normalizeConstraint 

      (look : Modules.env * Modules.spath * ErrorMsg.complainer -> 'a,
       eq : 'a * 'a -> bool,
       deferr : ErrorMsg.complainer -> Modules.spath * Modules.spath -> unit)

      (qids : Modules.spath list,
       totalenv : Modules.env,
       normenv : Normalize.env,
       err : ErrorMsg.complainer) =

    let 

    (* scan: scan a list of qualified identifiers, dividing them into local
       and definitional constraints.  Keep only one copy of the definitional
       constraints, since they've all got to be the same anyway.*)

    fun scan(nil,internal,definitional,_) =
	          {internal=internal,external=definitional}
      | scan (qid::r,internal,definitional,defqid) =

             (* look up the qualified identifer in the total environment
	        to make sure it exists and hasn't been hidden.*)
	  let val errflag = ref false in
          let val nQid =  Normalize.look(normenv,qid)
          in 
          look(totalenv,nQid,fn a => (errflag := true; err a));
          if !errflag then scan(r,internal,definitional,qid)
          else scan(r,nQid::internal,definitional,defqid)
          end
	  handle Normalize.Unbound =>
            (let val global = 
               look (totalenv,qid,fn a => (errflag := true; err a))
             in
             if !errflag then scan(r,internal,definitional,qid)
	     else 
               case definitional
	       of NONE => scan(r,internal,SOME global,qid)
		| SOME existing =>
		   (if eq(global,existing) then ()
		    else deferr err (defqid,qid);
		    scan(r,internal,definitional,defqid))
	     end)
	  end
   in scan(qids,[],NONE,[]) 
   end

   val normalizeStrConstraint =
      let val lookSTR' = fn arg =>
	   (fn STRvar{binding=b1,...} => b1) (lookSTR arg)
       in normalizeConstraint 
           (lookSTR',eqOrigin,
	    fn err => fn (qid1,qid2) =>
	 	(err COMPLAIN 
                   ("definitional sharing constraint " ^ formatQid qid1 ^ 
		    " = " ^ formatQid qid2 ^ " can never be satisfied")
		   nullErrorBody))
      end

   val normalizeTypeConstraint = 
       normalizeConstraint (lookTYC,equalTycon,
			fn err => fn (qid1,qid2) =>
			 (err COMPLAIN 
			      ("definitional type sharing constraint " ^
			       formatQid qid1 ^ " = " ^ formatQid qid2 ^
			       " can never be satisfied")
			      nullErrorBody))

    val addStrConstraint =
	fn ({total,symtotal,s,...} : context,qids,err) =>
          let val constraints=
		 normalizeStrConstraint (qids,!total,!symtotal,err)
	  in s := constraints :: !s
	  end

   val addTypeConstraint =
       fn ({total,symtotal,t,...} : context,qids,err) =>
          let val constraints =
		 normalizeTypeConstraint (qids,!total,!symtotal,err)
	  in t := (constraints :: !t)
	  end
in

   fun make_type_sharespec (patheqn,err) context =
      (addTypeConstraint(context,patheqn,err); nil)

   fun make_str_sharespec (patheqn,err) context =
      (addStrConstraint(context,patheqn,err); nil)

end (* local ... datatype 'a Constraint *)

fun elabSpec (error,ln) (context as {total,symtotal,inner,...}:context)(spec,names) =
  case spec
  of StrSpec lspec =>
       revfold 
         (fn ((name,sign),lnames) => 
	    make_strspec (name,sign,error,ln) context @ lnames)
	 lspec names
   | FctSpec lspec =>
       revfold
         (fn ((name,fsig),lnames) => 
	    make_fctspec (error,ln) context (name,fsig) @ lnames)
	 lspec names
   | TycSpec (lspec,eqprop) =>
       revfold 
         (fn ((name,tvs),lnames) => 
	    make_tyspec (eqprop,elabTyvList error ln tvs,name,error ln) context
	    @ lnames)
	 lspec names
   | ValSpec lspec =>
       revfold
         (fn ((name,ty),lnames) => 
	    let val ety = elabType error ln (!total,SOME(!symtotal)) ty
	    in make_valspec (name,ety,error ln) context @ lnames end)
	 lspec names
   | DataSpec dbs =>
	rev (make_dtyspec (error,ln) context dbs) @ names
   | ExceSpec (lspec) =>
       revfold 
         (fn ((name,topt),lnames) => 
	    case topt
	    of SOME ty =>
		 let val ety = elabType error ln (!total,SOME(!symtotal)) ty
		 in make_exnspecOF (name,ety,error ln) context @ lnames end
	     | NONE => make_exnspec (name,error ln) context @ lnames)
	 lspec names
   | MarkSpec (s,l1,l2) => elabSpec (error,(l1,l2)) context (s,names)
   | FixSpec {ops,fixity} => (
       error ln WARN "Fixity specification in signatures are not supported"
	     nullErrorBody;
       app (fn id => inner := Env.bind(id,FIXbind(FIXvar{name=id,
			        binding=fixity}),!inner)) ops;
       names)
   | ShareSpec pl =>  make_str_sharespec (pl,error ln) context @ names
   | ShatycSpec pl => make_type_sharespec (pl,error ln) context @ names
   | IncludeSpec s => make_includespec (s,error ln) context @ names
   | OpenSpec pl => make_openspec (pl,error ln) context @ names 
   | LocalSpec(sp1,sp2) => 
	(error ln WARN "LOCAL specs are only partially implemented"
	       nullErrorBody;
	 elabSpecList (error,ln) context sp1;
	 (elabSpecListRev (error,ln) context sp2) @ names)

and elabSpecListRev (error,ln) context specs =
      revfold (elabSpec (error,ln) context) specs []
and elabSpecList ctxt1 ctxt2 specs =
      rev (elabSpecListRev ctxt1 ctxt2 specs) 

and elabEMBEDsig (error,ln) 
		 (context as {strs,fcts,tycons,slots,makeStamp,inner,
			      parent,total,symtotal,names,t,s},name) dsc =
  case dsc
  of VarSig name' => 
       let val err = error ln
	   val sgn = lookSIG(!total,name',err)
           val strbind =
             STRbind (STRvar{name=name, 
			     access=if Extern.hidden name' then PATH []
				    else SLOT(next slots),
			     binding=STR_FORMAL{pos=next strs, spec=sgn}})
       in bind(name,strbind,context,err); sgn end
   | SigSig specs =>
       let val err = error ln
	   val inner' = ref Env.empty
	   val symbols = ref ([]:Symbol.symbol list)
	   val sgn = SIG {symbols=symbols, env=inner', kind=ref EMBEDDED,
			  stamp=makeStamp(), path=NONE}  
	   val binding = 
	     STRbind (STRvar{name=name,
			     access=if Extern.hidden name then PATH [] 
				    else SLOT (next slots),
			     binding=STR_FORMAL{pos=next strs,spec=sgn}})
	   val _ = bind(name,binding,context,err)
	   val context' = ({strs=strs, fcts=fcts, tycons=tycons,
			    slots=ref 0, parent=parent, makeStamp=makeStamp,
			    inner=inner', total=ref (!total),
			    symtotal=ref (!symtotal), names=name :: names,
			    t=t,s=s} : context)
	   val symbollist = elabSpecList (error,ln) context' specs
       in symbols := symbollist; sgn end
   | MarkSig(sign,l1,l2) =>
       elabEMBEDsig (error,(l1,l2)) (context,name) sign

and elabTOPsig (error,ln) (env,makeStamp,path) dsc =
  case dsc
  of VarSig name => lookSIG(env,name,error ln)
   | SigSig specs =>
       let val err = error ln
	   val inner = ref Env.empty
	   val symbols = ref []
	   val kind = ref IRRELEVANT
	   val sgn = SIG {symbols = symbols, kind=kind, env = inner, path=path,
			  stamp=makeStamp()}
	   val strs = ref 0 and fcts = ref 0 and tycons = ref 0 and 
	       slots = ref 0 and s = ref [] and t = ref []
	   val context = ({strs=strs, fcts=fcts, tycons=tycons, slots=slots,
			   parent = sgn, makeStamp = makeStamp, inner = inner,
			   total = ref env, symtotal = ref Normalize.empty,
			   names = [], t = t, s = s} : context)
	   val symbollist = elabSpecList (error,ln) context specs
       in 
       symbols := symbollist;
       kind := TOP{strcount = !strs, fctcount = !fcts, typecount = !tycons,
		   slotcount = !slots, sConstraints = !s, tConstraints = !t};
       if !System.Control.instSigs 
       then (Instantiate.instantiate(sgn,[],Stamps.newBoundScope(),err); ())
       else ();
       sgn
       end
   | MarkSig(sign,l1,l2) =>
       elabTOPsig (error,(l1,l2)) (env,makeStamp,path) sign

and elabPARTIALsig (error,ln) 
		   (ctx as (nameArg,sgnArg,total,symtotalArg,makeStamp)) dsc =
  case dsc
  of VarSig name => lookSIG(total,name,error ln)
   | SigSig specs =>
       let val symbols = ref ([]:Symbol.symbol list)
	   val bindArg = 
	     STRbind (STRvar{name=nameArg,access=SLOT 0,
			     binding=STR_FORMAL{pos=0, spec=sgnArg}})
	   val totalArg = Env.bind(nameArg,bindArg,total)
	   val inner = ref (Env.bind(nameArg,bindArg,Env.empty))
	   val kind = ref IRRELEVANT
	   val sgn = SIG {symbols = symbols, env = inner, path = NONE,
			  stamp = makeStamp (), kind = kind}
	   val context =
		 {strs = ref 1, fcts = ref 0,tycons = ref 0, slots = ref 0,
		  inner = inner, parent = sgn, total = ref totalArg,
		  symtotal = symtotalArg,
		  names = [], makeStamp = makeStamp, s = ref [], t = ref []}
	   val symbollist = elabSpecList (error,ln) context specs
       in
       symbols := nameArg :: symbollist;
       kind := TOP{strcount= !(#strs context), fctcount= !(#fcts context),
		   typecount= !(#tycons context), slotcount= !(#slots context),
		   sConstraints= !(#s context), tConstraints= !(#t context)};
       sgn
       end
   | MarkSig(sign,l1,l2) =>
       elabPARTIALsig (error,(l1,l2)) ctx sign

and make_strspec (name,sign,error,ln) (context as {inner,...} : context) =
    let val sgn = elabEMBEDsig (error,ln) (context, name) sign
        val bind = Env.look(!inner,name)
    in [name] end

and make_fsigexp (error,ln) path
                 (context as {total,symtotal,makeStamp,parent,...} : context)
                 (FsigFsig{param=[param],def}) =
      let val (nameParam,specParam) =
	     case param of (SOME n,s) => (n,s) | (NONE,s) => (name_X,s)
	  val symtotalParent = Normalize.liftEnv (name_P,!symtotal)
	  val sgnArg = 
            elabPARTIALsig (error,ln)
			   (name_P,parent,!total,ref symtotalParent,makeStamp)
			   (SigSig [StrSpec[(name_X,specParam)]])
	  val symtotalArg =
	    case param
	      of (NONE,_) => Normalize.openX(name_X,sgnArg,symtotalParent)
	       | (SOME _ ,_) => 
                   Normalize.bind(nameParam,[name_X],symtotalParent)
	  val sgnBody =
	    elabPARTIALsig 
	      (error,ln)
              (name_A,sgnArg,!total,
	       ref(Normalize.liftEnv (name_A,symtotalArg)),makeStamp)
	      def
	  val fsig =
	    FSIG{path = path,paramName = nameParam, argument = sgnArg,
		 body = sgnBody}
      in fsig end
  | make_fsigexp (error,ln) path ctxt (FsigFsig{param = a1 :: larg,def}) =
      make_fsigexp (error,ln) path ctxt
                  (FsigFsig{param=[a1],
                            def=SigSig[FctSpec[
                                 (functorId,FsigFsig{param=larg,def=def})]]})
  | make_fsigexp err path ctxt (FsigFsig{param = [],def}) =
       impossible "make_fsig"
  | make_fsigexp (error,ln) path ({total,...}: context) (VarFsig name) =
      lookFSIG(!total,name,error ln)
  | make_fsigexp (error,ln) path context (MarkFsig(fsig,l1,l2)) =
      make_fsigexp (error,(l1,l2)) path context fsig

and make_fctspec (error,ln) (context as {fcts,slots,...} : context)
		 (name,fsig) =
    let val fsgn = make_fsigexp (error,ln) NONE context fsig
	val binding = 
          FCTbind (FCTvar{name=name,access=SLOT (next slots),
			  binding=FCT_FORMAL{pos=next fcts,spec=fsgn}})
    in bind(name,binding,context,error ln); [name] end

and elabFSIGB (error,ln)  (arg as (env:env,scope)) (name,fsig) =
  let val context = 
	{strs=ref 0, fcts=ref 0, tycons=ref 0, slots=ref 0,  total = ref env,
         makeStamp = Stamps.newGenStamp scope, inner = ref Env.empty,
	 parent = ERROR_SIG,
	 symtotal = ref Normalize.empty, names = [], t = ref [], s = ref []}
      val binding = make_fsigexp (error,ln) (SOME name) context fsig
      val fsigvar = FSIGvar{name=name,binding=binding}
  in ([fsigvar],Env.bind(name,FSIGbind fsigvar,Env.empty):env) end

end

end
