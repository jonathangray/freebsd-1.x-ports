structure TemplateExpansion =
  struct
    open Absyn ErrorMsg MCCommon Access

    exception CANT_MATCH

    fun foo x = impossible "no templates yet"
(*
   	(case lookup (x, !constructor_env)
          of {rep = TEMPLrep (NOpat, _, _),...} => raise CANT_MATCH 
           | {rep = TEMPLrep x,...} => x 
           | _ => raise Internal 1)
	handle Lookup => raise (Internal 2) *)

    fun foo' x = impossible "no symbolic constants yet"
(*
   	(case lookup (x, !constructor_env)
          of {rep = CONSTrep (NOpat, _),...} => raise CANT_MATCH 
           | {rep = CONSTrep x,...} => x 
           | _ => raise Internal 3)
	handle Lookup => raise (Internal 4)
*)
    fun andPatterns(WILDpat, pat) = pat
      | andPatterns(pat, WILDpat) = pat
      | andPatterns(CONSTRAINTpat(pat, _), pat') = andPatterns(pat, pat')
      | andPatterns(pat, CONSTRAINTpat(pat', _))= andPatterns(pat, pat')
      | andPatterns(VARpat v, pat) = LAYEREDpat(VARpat v, pat)
      | andPatterns(pat, VARpat v) = LAYEREDpat(VARpat v, pat)
      | andPatterns(CONpat(k,t), CONpat(k',t')) = 
	  if conEq (k, k') then CONpat(k,t)
	  else if abstract k then LAYEREDpat(CONpat(k,t), CONpat(k',t'))
          else if abstract k' then LAYEREDpat(CONpat(k',t'), CONpat(k,t))
          else raise CANT_MATCH
      | andPatterns(CONpat(k,t), APPpat(k',t',pat)) =
	  if abstract k then LAYEREDpat(CONpat(k,t), APPpat(k',t',pat))
          else if abstract k' then LAYEREDpat(APPpat(k',t',pat), CONpat(k,t))
          else raise CANT_MATCH
      | andPatterns(APPpat(k',t',pat), CONpat(k,t)) =
	  if abstract k then LAYEREDpat(CONpat(k,t), APPpat(k',t',pat))
          else if abstract k' then LAYEREDpat(APPpat(k',t',pat), CONpat(k,t))
          else raise CANT_MATCH
      | andPatterns(APPpat(k,t,pat), APPpat(k',t',pat')) =
	  if conEq (k, k') then APPpat(k,t,andPatterns(pat,pat'))
	  else if abstract k then 
                    LAYEREDpat(APPpat(k,t,pat),APPpat(k',t',pat'))
          else if abstract k' then 
                    LAYEREDpat(APPpat(k',t',pat'), APPpat(k,t,pat))
          else raise CANT_MATCH
      | andPatterns(CONpat(k,t), pat) =
	  if abstract k then LAYEREDpat(CONpat(k,t), pat)
          else impossible "Non abstract CONpat & non constructor pat in andPat"
      | andPatterns(pat, CONpat(k,t)) =
	  if abstract k then LAYEREDpat(CONpat(k,t), pat)
          else impossible "non constructor pat & Non abstract CONpat in andPat"
      | andPatterns(APPpat(k,t,pat), pat') =
	  if abstract k then LAYEREDpat(APPpat(k,t,pat), pat')
          else impossible "Non abstract APPpat & non constructor pat in andPat"
      | andPatterns(pat, APPpat(k,t,pat')) = 
	  if abstract k then LAYEREDpat(APPpat(k,t,pat'), pat)
          else impossible "non constructor pat & Non abstract APPpat in andPat"
      | andPatterns(LAYEREDpat(CONSTRAINTpat(pat1, _), pat2), pat) =
          andPatterns(LAYEREDpat(pat1, pat2), pat) 
      | andPatterns(pat, LAYEREDpat(CONSTRAINTpat(pat1, _), pat2)) =
          andPatterns(pat, LAYEREDpat(pat1, pat2)) 
      | andPatterns(LAYEREDpat(pat1, pat2), pat) =
          LAYEREDpat(pat1, andPatterns(pat2, pat))
      | andPatterns(pat, LAYEREDpat(pat1, pat2)) =
          LAYEREDpat(pat1, andPatterns(pat2, pat))
      | andPatterns(INTpat n, INTpat n') =
	  if n = n' then INTpat n else raise CANT_MATCH
      | andPatterns(REALpat r, REALpat r') = 
	  if r = r' then REALpat r else raise CANT_MATCH
      | andPatterns(STRINGpat s, STRINGpat s') =
	  if s = s' then STRINGpat s else raise CANT_MATCH
      | andPatterns(pat1 as RECORDpat{pats=ref p,...}, 
                    pat2 as RECORDpat{pats=ref p',...}) =
          mkRECORDpat pat1 (multiAnd(p, p'))
(********************** how to and two types ? **************************)
      | andPatterns(VECTORpat(p,t), VECTORpat(p',t')) =
          if (length p) = (length p') then VECTORpat(multiAnd(p,p'),t) 
          else raise CANT_MATCH
      | andPatterns (p1, p2) = 
	  impossible "bas andPattern call"

    and multiAnd (nil, nil) = nil
      | multiAnd (pat::rest, pat'::rest') = 
          (andPatterns(pat,pat'))::(multiAnd(rest, rest'))
      | multiAnd _ = impossible "bad multiAnd call"

    fun instantiatePatexp (VARpat v, env) = lookup(v, env)
      | instantiatePatexp (LAYEREDpat(pat1, pat2),env) =
          andPatterns(instantiatePatexp(pat1,env),instantiatePatexp(pat2,env))
      | instantiatePatexp (CONSTRAINTpat(pat, _), env) =
          instantiatePatexp(pat, env)
      | instantiatePatexp (APPpat(k,t,pat), env) = 
	  APPpat(k,t,instantiatePatexp(pat, env))
      | instantiatePatexp (pat as RECORDpat{pats=ref pats,...}, env) =
          mkRECORDpat pat (multiInstantiatePatexp(pats, env))
      | instantiatePatexp (VECTORpat(pats,t), env) =
          VECTORpat (multiInstantiatePatexp(pats, env), t)
      | instantiatePatexp (pat, env) = pat
    and multiInstantiatePatexp(nil, env) = nil
      | multiInstantiatePatexp(pat::rest, env) = 
	  (instantiatePatexp(pat, env))::(multiInstantiatePatexp(rest, env))

    fun instance (VARpat (VALvar {name, typ, ...})) =
          VARsimp (VALvar{access=PATH[mkLvar()], name=name, typ=typ})
      | instance (VARpat _) = impossible "bad variabel in match"
      | instance (RECORDpat{pats=ref pats,...}) = RECORDsimp(map instance pats)
      | instance (CONSTRAINTpat(pat, _)) = instance pat
      | instance pat = impossible "bad instance call"

    fun simpToPat (VARsimp v) = VARpat v
      | simpToPat (RECORDsimp simps) = 
          RECORDpat {fields=nil, flex=false, 
                     typ = ref UNDEFty, pats=ref (map simpToPat simps)}

    fun trivpatTrivEnv (VARpat v, VARsimp x) = [(v, VARpat x)]
      | trivpatTrivEnv (CONSTRAINTpat(tpat, _), simp) = 
          trivpatTrivEnv (tpat, simp)
      | trivpatTrivEnv (RECORDpat{pats=ref tpats,...}, RECORDsimp simps) =
          multiTrivpatTrivEnv (tpats, simps)
      | trivpatTrivEnv _ = impossible "trivpatTrivEnv"
    and multiTrivpatTrivEnv (nil, nil) = nil
      | multiTrivpatTrivEnv (tpat::trest, simp::srest)=
          (trivpatTrivEnv(tpat, simp))@(multiTrivpatTrivEnv(trest, srest))
      | multiTrivpatTrivEnv _ = impossible "multiTrivpatTrivEnv"

    fun wildEnv (VARpat v) = [(v, WILDpat)]
      | wildEnv (CONSTRAINTpat(tpat, _)) = wildEnv tpat
      | wildEnv (RECORDpat{pats=ref pats,...}) =
	  fold op @ (map wildEnv pats) nil
      | wildEnv _ = impossible "wildEnv called on non-trivpat"
  
    fun matchTrivpat (VARpat v, pat)= ([(v, pat)], nil, nil)
      | matchTrivpat (CONSTRAINTpat(tpat, _), pat) = matchTrivpat(tpat, pat)
      | matchTrivpat (tpat, CONSTRAINTpat(pat, _)) = matchTrivpat(tpat, pat)
      | matchTrivpat (RECORDpat{pats=ref tps,...},RECORDpat{pats=ref ps,...}) =
 	  multiMatchTrivpat(tps, ps)
      | matchTrivpat (tpat, WILDpat) = 
          (wildEnv tpat, nil, nil)
      | matchTrivpat (tpat, VARpat v) =
          let val a = instance tpat
              val b = trivpatTrivEnv (tpat, a)
          in (b, [(v, a)], nil)
          end
      | matchTrivpat (tpat, CONpat(k,t)) =
          let val a = instance tpat
              val b = trivpatTrivEnv (tpat, a)
          in (b, nil, [(a, CONpat(k,t))])
          end
      | matchTrivpat (tpat, APPpat(k,t,pat)) =
          let val a = instance tpat
              val b = trivpatTrivEnv (tpat, a)
          in (b, nil, [(a, APPpat(k,t,pat))])
          end
      | matchTrivpat (tpat, LAYEREDpat(CONpat(k,t), pat)) =
          let val a = instance tpat
              val (pat', varEnv, constr) = 
                    matchTrivpat(tpat, andPatterns(simpToPat a, pat))
          in (pat', varEnv, (a, CONpat(k,t))::constr)
          end
      | matchTrivpat (tpat, LAYEREDpat(APPpat(k,t,spat), pat)) =
          let val a = instance tpat
              val (pat', varEnv, constr) = 
                    matchTrivpat(tpat, andPatterns(simpToPat a, pat))
          in (pat', varEnv, (a, APPpat(k,t,spat))::constr)
          end
      | matchTrivpat (tpat, LAYEREDpat(VARpat v, pat)) =
          let val a = instance tpat
              val (pat', varEnv, constr) = 
                 matchTrivpat(tpat, andPatterns(simpToPat a, pat))
          in (pat', (v,a)::varEnv, constr)
          end
      | matchTrivpat (tpat, LAYEREDpat(CONSTRAINTpat(pat1, _), pat2)) =
          matchTrivpat (tpat, LAYEREDpat(pat1, pat2))
      | matchTrivpat (tpat, pat) = impossible "bad matchTrivpat call"
    and multiMatchTrivpat (nil, nil) = (nil, nil, nil)
      | multiMatchTrivpat (tpat::trest, pat::prest) =
	  let val (patenv, varenv, constr) = multiMatchTrivpat(trest, prest)
              val (patenv', varenv', constr') = matchTrivpat(tpat, pat)
          in (patenv@patenv', varenv@varenv', constr@constr')
          end
      | multiMatchTrivpat _ = impossible "bad multiMatchTrivpat call"

    fun newVars (RECORDsimp simps, env) = 
	  multiNewVars(simps, env)
      | newVars (VARsimp (v as VALvar {name, typ, ...}), env) =
	  ((lookup(v, env); env) 
             handle Lookup => 
               ((v,VALvar{name=name, typ=typ,access=PATH[mkLvar()]})::env))
      | newVars (VARsimp _, _) = impossible "bad instance call to newVars"
    and multiNewVars(nil, env) = env
      | multiNewVars(simp::rest, env) = multiNewVars(rest, newVars(simp, env))

    fun instantiateLocalVars (nil, env) = env
      | instantiateLocalVars ((path,pat)::rest, env) =
          instantiateLocalVars(rest, newVars(path, env))

    fun instSimpexp(VARsimp v, env) = VARsimp (lookup(v, env))
      | instSimpexp(RECORDsimp simps, env) = 
          RECORDsimp (multiInstSimpexp (simps, env))
    and multiInstSimpexp(nil, env) = nil
      | multiInstSimpexp(simpexp::rest, env) = 
          (instSimpexp(simpexp, env))::(multiInstSimpexp(rest, env))

    fun instantiateConstrs(nil, locEnv, env) = nil
      | instantiateConstrs((simpexp, pat)::rest, locEnv, env) = 
          (instSimpexp(simpexp, locEnv), instantiatePatexp(pat, env))
            :: (instantiateConstrs(rest, locEnv, env))    

    fun liftenv nil = nil
      | liftenv ((v,x)::rest) = (v, VARpat x)::(liftenv rest)

    fun templExpand(k, pat) =
          let 
            val (patexp, trivpat, constrs) = foo k
            val (env, varnames, newconstrs) = matchTrivpat(trivpat, pat)
            val env' = instantiateLocalVars (constrs, nil)
            val newEnv = env@(liftenv env')
          in 
            (instantiatePatexp(patexp, newEnv),
             newconstrs@(instantiateConstrs(constrs, env', newEnv)),
             varnames)
          end              

    fun constExpand k =
          let 
            val (patexp, constrs) = foo' k
            val newEnv = instantiateLocalVars (constrs, nil)
	    val lNewEnv = liftenv newEnv
          in 
            (instantiatePatexp(patexp, lNewEnv),
             instantiateConstrs(constrs, newEnv, lNewEnv),
             nil)
          end              

    fun multiTemplateExpand nil = (nil, nil, nil)
      | multiTemplateExpand (pat::rest) =
          let 
            val (pats', constr1, varenv1) = multiTemplateExpand rest
            val (pat', constr2, varenv2) = templateExpandPattern pat
          in
            (pat'::pats', constr1@constr2, varenv1@varenv2)
          end
    
    and templateExpandPattern (APPpat(k,t,pat)) =
          let
            val (pat', patConstraints, patVarenv) = templateExpandPattern pat
          in
            if template k then
              let
                val (newPat, kConstraints, kVarenv) = templExpand(k, pat')
              in
                (newPat, patConstraints@kConstraints, patVarenv@kVarenv)
              end
            else
              (APPpat(k,t,pat'), patConstraints, patVarenv)
          end
      | templateExpandPattern (CONpat(k,t)) =
          if template k then
            let
              val (newPat, constraints, varenv) = constExpand k
            in
              (newPat, constraints, varenv)
            end
          else
            (CONpat(k,t), nil, nil)
      | templateExpandPattern (pat as RECORDpat{pats=ref pats,...}) =
          let 
            val (pats', constr, varenv) = multiTemplateExpand pats
          in
            (mkRECORDpat pat pats', constr, varenv)
          end
      | templateExpandPattern (VECTORpat(pats,t)) =
          let 
            val (pats', constr, varenv) = multiTemplateExpand pats
          in
            (VECTORpat(pats,t), constr, varenv)
          end
      | templateExpandPattern (LAYEREDpat(pat1, pat2)) =
          let 
            val (pat1', constr1, varenv1) = templateExpandPattern pat1
            val (pat2', constr2, varenv2) = templateExpandPattern pat2
          in
            (LAYEREDpat(pat1', pat2'), constr1@constr2, varenv1@varenv2)
          end
      | templateExpandPattern (CONSTRAINTpat(pat, _)) =
          templateExpandPattern pat
      | templateExpandPattern pat = (pat, nil, nil)

    fun fullyExpandBinding varenv (VARsimp v) =
          (fullyExpandBinding varenv (lookup(v, varenv))
             handle Lookup => VARsimp v)
      | fullyExpandBinding varenv (RECORDsimp simps) =
          RECORDsimp (map (fullyExpandBinding varenv) simps)

    fun fullyExpandBindingTrivpat varenv (VARpat v) =
          (fullyExpandBindingTrivpat varenv (simpToPat(lookup(v, varenv)))
             handle Lookup => VARpat v)
      | fullyExpandBindingTrivpat varenv (pat as RECORDpat{pats=ref pats,...})=
          mkRECORDpat pat (map (fullyExpandBindingTrivpat varenv) pats)
      | fullyExpandBindingTrivpat varenv (CONSTRAINTpat(pat, _)) =
          fullyExpandBindingTrivpat varenv pat
      | fullyExpandBindingTrivpat _ _ = 
          impossible "fullyExpandBindingTrivpat miscalled"
  
  end

