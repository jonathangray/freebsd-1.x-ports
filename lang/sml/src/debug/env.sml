(* DebugEnv
 
   Support for synthetic environments when halted under debugger.
   Intended to be layered into appropriate spot in interactive debugger
   environment.

   The static environment special lookup
   function searches for a given symbol in the environment 
   current at last setEnvTime.  If the symbol is found,
   a synthetic binding for it is returned.  For variables and constructors,
   type reconstruction is performed and the resulting type is placed 
   in the synthetic binding. For type constructors and structures, 
   all component types are reconstructed.  For symbols with dynamic 
   representations, the synthetic binding contains the original lvar.

   The dynamic component of the special environment is a ref holding 
   a normal dynamic environment mapping lvars to appropriate value objects 
   for all identifiers looked up since setEnvTime was last called.  

   Similarly, the inverse environment is a ref holding a normal inverse
   environment mapping lvars to componentIds for all identifiers looked
   up since setEnvTime was last called.

   Underlying assumptions: 

   (1) Only lvars that appear in bindings returned by lookups in
   the static synthetic environment will ever be looked up in the dynamic
   synthetic environment.

   (2) Desired envTime never changes between parsing
   and execution of an expression involving variables from synthetic
   environment.
*)


signature DEBUG_ENV =
sig
  type time
  val useSpecial: bool ref
      (* enable special environment if true *)
  val setEnvTime: time -> unit
      (* set time to base special lookups on, clearing previous dynamic env *)
  val debugEnvironment: Environment.environment
      (* synthetic debugger environment *)
end

structure DebugEnv: DEBUG_ENV =
struct
  open DebugUtil DebugRun DebugBindings DebugMotions Types Variables
       Access Absyn Modules PrintUtil
  structure U = System.Unsafe

  exception Unbound = Env.Unbound

  val useSpecial = ref true

  val envTime:time ref = ref 0

  val dynEnvR = ref DynamicEnv.empty
  fun dynbind (lv,binding) = dynEnvR := DynamicEnv.bind(lv,binding,!dynEnvR)

  val invEnvR = ref InverseEnv.empty
  fun invbind (lv,compId) = invEnvR := InverseEnv.bind(lv,compId,!invEnvR)

  fun setEnvTime (t:time) =
    (envTime := t;
     dynEnvR := DynamicEnv.empty;
     invEnvR := InverseEnv.empty)

  (* look up symbol in a given namespace, and return new binding *)
  fun lookVARCON (symbol:Symbol.symbol) : binding  =
	let val n = Symbol.name symbol
	    val _ = dbgprint ("lookVARCON " ^ n ^ "\n")
	    val (t,c,(i,binding)) = findVARCONBind ([n],!envTime,0) 
	in case binding of
	     VARbind(v as VALvar{access=PATH[lv],typ=ref ty,...}) => 
	       let val (evn,args) = evnArgsAt t
		   val bv = nth (nthArgs(evn,c,args),i)
		   val ty = dynTypeAt (t,c) ty
		   val _ = dbgprint "gotVALVAR\n"
	       in dynbind(lv,bv);
                  invbind(lv,{name=symbol,
			      pid=Stamps.null,
			      ty=Transtypes.transTyLty ty});
		  VARbind(VALvar{access=PATH[lv],
				 name=[symbol],
				 typ=ref ty})
	       end
	   | VARbind(OVLDvar _) =>
	       (dbgprint "gotOVLDVAR\n";
		binding)
	   | CONbind(dc as (DATACON{const,rep,sign,typ,...}))  =>
	       let val typ = dynTypeAt (t,c) typ
		   val rep = case rep of
			       (v as VARIABLE(PATH[lv])) => 
				  let val (evn,args) = evnArgsAt t
				      val ev = nth(nthArgs(evn,c,args),i)
				  in dynbind(lv,ev);
				     v
				  end
			     | (v as VARIABLEc(PATH[lv])) => 
				  let val (evn,args) = evnArgsAt t
				      val ev = nth(nthArgs(evn,c,args),i)
				  in dynbind(lv,ev);
				     v
				  end
			     | _ => rep
		   val _ = dbgprint "gotCON\n"
	       in CONbind(DATACON{name=symbol,const=const,
				  typ=typ,rep=rep,sign=sign})
	       end
	end
  
  fun lookSTR  (symbol:Symbol.symbol) : binding =
	  let val n = Symbol.name symbol
	      val _ = dbgprint ("lookSTR " ^ n ^ "\n")
	      val (t,c,(i,STRbind(STRvar{binding=s,access=PATH[lv],...}))) = 
			     findSTRBind ([n],!envTime,0)
	      val s = dynStrAt t s
	      val sobj =
		  if i >= 0 then 
		    let val (evn,args) = evnArgsAt t
		    in nth(nthArgs(evn,c,args),i+1)
		    end
		  else let val args = argsAt (pred t)
		       in hd args
		       end
	      val _ = dbgprint "gotSTR\n"
	  in dynbind(lv,sobj);
	     invbind(lv,{name=symbol,
			 pid=ModuleUtil.getStrStamp s,
			 ty=TransBinding.transStrLty s});
	     STRbind(STRvar{name=symbol,access=PATH[lv],binding=s})
	  end
  
  fun lookFCT  (symbol:Symbol.symbol) : binding =
	  let val n = Symbol.name symbol
	      val _ = dbgprint ("lookFCT " ^ n ^ "\n")
	      val (t,c,(i,FCTbind(FCTvar{binding=f,access=PATH[lv],...}))) = 
		  findFCTBind ([n], !envTime,0)
	      val (evn,args) = evnArgsAt t
	      val fobj = nth(nthArgs(evn,c,args),i)
	      val _ = dbgprint "gotFCT\n"
	  in dynbind(lv,fobj);
	     invbind(lv,{name=symbol,
			 pid=ModuleUtil.getFctStamp f,
			 ty=TransBinding.transFctLty f});
	     FCTbind(FCTvar{name=symbol,access=PATH[lv],binding=f})
	  end
  
  fun lookSIG (symbol:Symbol.symbol) : binding =
	  let val n = Symbol.name symbol
	      val _ = dbgprint ("lookSIG " ^ n ^ "\n")
	      val (t,c,(i,SIGbind sv)) = findSIGBind ([n],!envTime,0)
	      val _ = dbgprint "gotSIG\n"
	  in SIGbind(sv)
	  end
  
  fun lookFSIG (symbol:Symbol.symbol) : binding =
	  let val n = Symbol.name symbol
	      val _ = dbgprint ("lookFSIG " ^ n ^ "\n")
	      val (t,c,(i,FSIGbind fsv)) = findFSIGBind ([n],!envTime,0)
	      val _ = dbgprint "gotFSIG\n"
	  in FSIGbind(fsv)
	  end
  
  fun lookTYC (symbol:Symbol.symbol) : binding = 
	  let val n = Symbol.name symbol
	      val _ = dbgprint ("lookTYC " ^ n ^ "\n")
	      val (t,c,(i,TYCbind tycon)) = findTYCBind ([n],!envTime,0)
	      val tycon = dynTyconAt t tycon
	      val _ = dbgprint "gotTYC\n"
	  in TYCbind(tycon)
	  end
  
  fun lookFIX (symbol:Symbol.symbol) : binding =
	  let val n = Symbol.name symbol
	      val _ = dbgprint ("lookFIX " ^ n ^ "\n")
	      val (t,c,(i,FIXbind fv)) = findFIXBind ([n],!envTime,0)
	      val _ = dbgprint "gotFIX\n"
	  in FIXbind(fv)
	  end
  

  fun looker' (symbol:Symbol.symbol) :binding =
	let val look = case Symbol.nameSpace symbol of
			 Symbol.VALspace => lookVARCON
		       | Symbol.TYCspace => lookTYC
		       | Symbol.SIGspace => lookSIG
		       | Symbol.FSIGspace => lookFSIG
		       | Symbol.STRspace => lookSTR
		       | Symbol.FCTspace => lookFCT
		       | Symbol.FIXspace => lookFIX
		       | _ => raise Unbound
	in case (withEstablishedTime (fn _ => 
		   SOME (look symbol)
		      handle Unbound => NONE
		           | QueryInterrupted => NONE)) of
	     NOTRUNNING => raise Unbound
           | COMPLETED(SOME binding) => binding
           | COMPLETED NONE => raise Unbound
	   | INTERRUPTED _ => raise Unbound  (* somewhat superfluously *)
	end

   fun statLooker (symbol:Symbol.symbol) : binding =
     if !useSpecial then
       looker' symbol
     else raise Unbound

   fun dynLooker (lvar:Access.lvar) : U.object = 
     if !useSpecial then
       DynamicEnv.look (!dynEnvR) lvar
     else raise DynamicEnv.Unbound

   fun invLooker (lvar:Access.lvar) : InverseEnv.componentId =
     if !useSpecial then
       InverseEnv.look (!invEnvR) lvar
     else raise InverseEnv.Unbound

   val debugEnvironment = 
       {static=StaticEnv.special (statLooker,StaticEnv.empty),
	dynamic=DynamicEnv.special (dynLooker,DynamicEnv.empty),
	inverse=InverseEnv.special (invLooker,InverseEnv.empty)}

end
