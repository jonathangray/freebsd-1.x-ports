(* Copyright 1989 by AT&T Bell Laboratories *)
(* environ.sml *)

structure Environment : ENVIRONMENT =
struct

  type statenv = StaticEnv.statenv
  type dynenv  = DynamicEnv.dynenv
  type invenv  = InverseEnv.invenv

  type environment = {static: statenv, dynamic: dynenv, inverse: invenv}
  type staticEnv = {static: statenv, inverse: invenv}

  fun staticPart({static,inverse,...}: environment): staticEnv =
      {static=static, inverse=inverse}

  val emptyEnv = {static  = StaticEnv.empty,
		  dynamic = DynamicEnv.empty,
		  inverse = InverseEnv.empty}

  fun layerEnv({static,dynamic,inverse},
	       {static=static',dynamic=dynamic',inverse=inverse'}) =
      {static =  StaticEnv.atop(static,static'),
       dynamic = DynamicEnv.atop(dynamic,dynamic'),
       inverse = InverseEnv.atop(inverse,inverse')}
  
  fun layerStatic({static,inverse},
		  {static=static',inverse=inverse'}) =
      {static =  StaticEnv.atop(static,static'),
       inverse = InverseEnv.atop(inverse,inverse')}
  
  fun consolidateEnv({static,dynamic,inverse}) =
      {static = StaticEnv.consolidate static,
       dynamic = DynamicEnv.consolidate dynamic,
       inverse = InverseEnv.consolidate inverse}

  fun consolidateStatic({static,inverse}) =
      {static = StaticEnv.consolidate static,
       inverse = InverseEnv.consolidate inverse}

  val topLevelEnvRef : environment ref =
        System.Unsafe.cast System.Env.topLevelEnvRef
  val pervasiveEnvRef : environment ref =
        System.Unsafe.cast System.Env.pervasiveEnvRef


 (* functions to collect stale lvars for unbinding in concatEnv *)

  open ErrorMsg Access Variables Types Modules

  exception NotStale

  (* staleLvars: takes a new environment and a base environment to which
     it is to be added and returns a list of lvars that are unreachable 
     when the new environment is added to the base environment*)

  fun staleLvars(deltaEnv,baseEnv) : int list =
      let val lvarset = ref([] : int list)

	  (* collect: for each symbol bound in the new environment,
	     check if there was a binding in the base environment.
	     If there was a binding and it has an lvar associated
             with it, add it to the list of stale lvars. *)

	  fun collect (s,_) = 
	    let val v = case Env.look(baseEnv,s)
			 of VARbind(VALvar{access=PATH[v],...}) => v
			  | STRbind(STRvar{access=PATH[v],...}) => v
			  | FCTbind(FCTvar{access=PATH[v],...}) => v
			  | CONbind(DATACON{rep=VARIABLE(PATH[v]),...}) => v
			  | CONbind(DATACON{rep=VARIABLEc(PATH[v]),...}) => v
			  | _ => raise NotStale
	     in lvarset := v :: !lvarset
	    end handle NotStale => ()
		     | Env.Unbound => ()
       in Env.app collect deltaEnv;
	  !lvarset
      end

  fun bindLvars(newlvars,result,denv) =
      let fun f(i,v::r,denv) =
	        f(i+1,r,DynamicEnv.bind(v, Vector.sub(result,i),denv))
	    | f(_,nil,denv) = denv
       in f(0,newlvars,denv)
	  handle Subscript => impossible "Environment.bindLvars"
      end

  fun bindEnv(deltaStat: statenv,
	      deltaInv: invenv,
	      newlvars: Access.lvar list,
	      result: System.Unsafe.object Vector.vector,
	      {static,dynamic,inverse}: environment): environment =
      let val hidden_lvars = staleLvars(deltaStat,static)
       in {static=StaticEnv.consolidate(StaticEnv.atop(deltaStat,static)),
	   dynamic=bindLvars(newlvars,result,
			     DynamicEnv.remove(hidden_lvars,dynamic)),
	   inverse=InverseEnv.atop(deltaInv,
				   InverseEnv.remove(hidden_lvars,inverse))}
	  (* consolidation? *)
      end

  fun concatEnv({static=newstat,dynamic=newdyn,inverse=newinv},
		{static=oldstat,dynamic=olddyn,inverse=oldinv}) =
      let val hidden_lvars = staleLvars (newstat,oldstat)
       in {static=StaticEnv.consolidate(StaticEnv.atop(newstat,oldstat)),
	   dynamic=DynamicEnv.atop(newdyn,DynamicEnv.remove(hidden_lvars,olddyn)),
	   inverse=InverseEnv.atop(newinv,InverseEnv.remove(hidden_lvars,oldinv))}
	  (* consolidation? *)
      end

  fun getbindings(static: statenv, symbols: Symbol.symbol list) :
        (Symbol.symbol * Modules.binding) list =
      let fun loop([], bindings) = bindings
	    | loop(s::rest, bindings) =
	      let val bindings' = (s,Env.look(static,s)) :: bindings
				  handle Env.Unbound => bindings
	      in loop (rest, bindings') end
      in  loop (symbols,[])
      end

  fun copystat([],senv) = senv
    | copystat((s,b)::l,senv) = copystat(l,Env.bind(s,b,senv))

  fun filterStaticEnv({static,inverse}: staticEnv,
		      symbols: Symbol.symbol list) : staticEnv =
      {static = copystat(getbindings(static, symbols), Env.empty),
       inverse = inverse}

  fun makeStaticEnv(statenv: statenv) : staticEnv =
      let val invenv = ref(InverseEnv.empty)
	  fun scanBinding(_,FCTbind(FCTvar{name,access=PATH[lvar],binding})) = 
		invenv :=
		  InverseEnv.bind(lvar,
				  {name=name,
				   pid=ModuleUtil.getFctStamp binding,
				   ty=TransBinding.transFctLty(binding)},
				  !invenv)
	    | scanBinding(_,STRbind(STRvar{name,access=PATH[lvar], binding})) =
	        invenv := InverseEnv.bind(lvar,
			      {name=name,
			       pid=ModuleUtil.getStrStamp binding,
                               ty=TransBinding.transStrLty(binding)},
			  !invenv)
            | scanBinding(_,VARbind(VALvar{name=[n],access=PATH[lvar],typ})) =
                invenv := InverseEnv.bind(lvar,
                              {name=n,pid=Stamps.null,
                               ty=Transtypes.transTyLty(!typ)},
                          !invenv)
            | scanBinding(_,CONbind(DATACON{name=n,rep=VARIABLEc(PATH[lvar]),
                                            typ=typ,...})) =
                invenv := InverseEnv.bind(lvar,
                              {name=n,pid=Stamps.null,
                               ty=Transtypes.transTyLty(typ)},
                          !invenv)
            | scanBinding(_,CONbind(DATACON{name=n,rep=VARIABLE(PATH[lvar]),
                                            typ=typ,...})) =
                invenv := InverseEnv.bind(lvar,
                              {name=n,pid=Stamps.null,
                               ty=Transtypes.transTyLty(typ)},
                          !invenv)
            | scanBinding(_,VARbind _) = ()
            | scanBinding(_,CONbind _) = ()
            | scanBinding(_,TYCbind _) = ()
            | scanBinding(_,FSIGbind _) = ()
	    | scanBinding(_,SIGbind _) = ()
            | scanBinding(_,FCTbind _) = ()
            | scanBinding(_,STRbind _) = ()
            | scanBinding(_,FIXbind _) = ()
       in Env.app scanBinding statenv;
	  {static = statenv, inverse = !invenv}
      end

  fun filterEnv({static,dynamic,inverse}: environment,
		symbols: Symbol.symbol list) : environment =
      let val sbindings = getbindings(static,symbols)
	  fun copydyn([],denv) = denv
	    | copydyn((_,b)::l,denv) =
		(case ModuleUtil.lvarOfBinding b
		   of NONE => copydyn(l,denv)
	            | SOME lv =>
			copydyn(l,DynamicEnv.bind(lv,
						  DynamicEnv.look dynamic lv,
						  denv)))
          val senv = copystat(sbindings, Env.empty) 
          val ienv = #inverse(makeStaticEnv senv)
       in {static=senv,
	   dynamic=copydyn(sbindings, DynamicEnv.empty),
	   inverse=ienv}
      end

  fun catalogEnv({static,...}: staticEnv) : Symbol.symbol list =
      map #1 (ModuleUtil.sortEnvBindings static)

  fun describe ({static,...}: staticEnv) (s: Symbol.symbol) : unit =
      let open PrettyPrint in
	with_pp (ErrorMsg.defaultConsumer())
	 (fn ppstrm =>
	  (begin_block ppstrm CONSISTENT 0;
	   PPBasics.ppBinding ppstrm (static,StaticEnv.look(static,s),
				      !System.Print.printDepth);
	   add_newline ppstrm;
           end_block ppstrm))
      end
      handle Env.Unbound => print (Symbol.name s ^ " not found\n")

end (* structure Env *)
