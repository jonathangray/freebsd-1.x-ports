(* 
   NORMALIZE: this structure defines environment associating the real path
   in the elaborated structure to a path used by the user.

   Basicly it is a finite function of type: spath -> spath.
   But as only the head can be moved and by consistency of paths, we can use
   symbol -> spaths.

   As it is necessary in functor signature to lift a whole environment (i.e.
   add a level of indirection at the head of each of its elements) we provide
   a way to express simply this lifting without redefining the whole
   environment.

   openStr is a primitive mechanism to handle include specification.   
   The other operations available are the basic operations on environment
*)

signature NORMALIZE =
  sig
     exception Unbound
     type env
     val empty : env
     val bind : Symbol.symbol * Modules.spath * env -> env
     val look : env * Modules.spath -> Modules.spath
     val openStr : env * Modules.spath * env * Modules.Signature -> env
     val openX : Symbol.symbol * Modules.Signature * env -> env
     val liftEnv : Symbol.symbol * env -> env
     val normalize : env option * Modules.spath -> Modules.spath
  end

structure Normalize : NORMALIZE = struct

open ErrorMsg Modules
exception Unbound

(* invariant: symbol list is in *reverse* order *)

abstype env = SymEnv of {
		(* the main part *)
                body : Symbol.symbol list Env.env,
		(* the optional lifted part: name is the prefix *)
		extension : {name : Symbol.symbol, env : env} option}
with

  (* the empty environment *)
  val empty = SymEnv{body=Env.empty,extension=NONE}

  (* look at a binding in an environment. *)

  fun look (SymEnv{body,extension},syms) =
    (* search the path to find the head, reverse it and add the tail *)
    let fun f nil= impossible "Normalize.look"
          | f [h] = rev(Env.look(body,h))
          | f (h :: t) = rev(Env.look(body,h)) @ t
    in
    (* if the search failed, looks recursively in the extensions adding the
     * prefix to the resulting path *)
    f syms handle Env.Unbound =>
      case extension
      of NONE => raise Unbound
       | SOME{name,env} => name :: look(env,syms)
    end

  (* same as look but the environment is optional and we get back the
   * symbol if it is not found *)
  fun normalize(env,sym) = 
    case env
    of NONE => sym
     | SOME env' => look (env',sym) handle Unbound => sym

  (* bind a symbol to a path in a normalization env *)
  fun bind (sym,fullname,SymEnv{body,extension}) = 
    SymEnv{body=Env.bind(sym,fullname,body), extension=extension}

  (* convert the list of defined symbols of a signature in an environment *)
  fun convertEnv (_,ERROR_SIG) = Env.empty
    | convertEnv (_,FULL_SIG) = impossible "Normalize.convertEnv"
    | convertEnv (names,SIG{env,...}) =
        let val normEnv = ref (Env.empty: Symbol.symbol list Env.env)
        in
	Env.app (fn (s,_) => normEnv := Env.bind(s,s::names,!normEnv)) (!env);
	!normEnv
        end

  (* includes a signature after a correct lifting in an environment *)
  fun openStr (bindEnv,spath,SymEnv{body=baseEnv,extension},sign) =
    SymEnv{
      body=Env.atop(convertEnv(rev(look(bindEnv,spath)),sign),baseEnv),
      extension=extension}

  fun openX (name,sgn,normEnv) =
    let val symbX = 
      (case sgn 
       of SIG{env,...} => (
            case Env.look(!env,name)
	    of STRbind(STRvar{binding=STR_FORMAL{spec=SIG{symbols, ...},...},
			      ...}) => !symbols
	     | STRbind(STRvar{binding=STR_FORMAL{spec=ERROR_SIG,...},...})=> []
             | _ => impossible "open_X 1")
	| ERROR_SIG => []
	| _ => impossible "open_X 2")
      handle Env.Unbound => impossible "open_X 3"
    in fold (fn (s,env) => bind(s,[s,name],env)) symbX normEnv
    end

  (* takes an environment and lift it. *)
  fun liftEnv (name,env) =
    SymEnv{body=Env.empty,extension=SOME{name=name,env=env}}

end (* abstype env *)

end (* structure Normalize *)


