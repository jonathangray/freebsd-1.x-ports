signature MC =
  sig
    val bindCompile : 
      Modules.env
      -> (Absyn.pat * Lambda.lexp) list * Lambda.lty * ErrorMsg.complainer
      -> Lambda.lexp

    val matchCompile : 
      Modules.env
      -> (Absyn.pat * Lambda.lexp) list * Lambda.lty * ErrorMsg.complainer
      -> Lambda.lexp

    val matchCompileHandler : 
      Modules.env
      -> (Absyn.pat * Lambda.lexp) list * Lambda.lty * ErrorMsg.complainer
      -> Lambda.lexp
  end


structure MC : MC =

  struct
    open Absyn Lambda
    open Variables Types
    open Access ErrorMsg PrettyPrint
    open MCSet TemplateExpansion MCCommon

    val say = System.Print.say

    fun abstest0 _ = impossible "abstest0"
    fun abstest1 _ = impossible "abstest1"

    fun mkDATAcon (DATACON{name,rep,typ,...}) = 
          DATAcon(name, rep, Transtypes.transTyLty(typ))

    fun mkDATAcon1(DATACON{name,rep,typ,...},_) = 
          DATAcon(name, rep, Transtypes.transTyLty(typ))

    fun mkDELTAPATH((dcon,t),p) = DELTAPATH(mkDATAcon(dcon),t,p)

    fun vartolvar (VALvar{access=PATH[v],typ=ref ty,...}) = 
              (v, Transtypes.transTyLty(ty))
      | vartolvar _ = impossible "impossible variable in mc.sml"

    fun DECON''(k as DATAcon(d as (sym,rep,lt1)),SOME t,exp) = 
         let val lt2 = Transtypes.transTyLty(t)
          in (case (lt1,lt2) 
               of (ARROWty(_,t1),ARROWty(_,t2)) =>
                   (let val header = Transtypes.unwrapOp(t1,t2)
                     in header(DECON'(d, exp))
                    end)
                | _ =>   (* constant data constructors *)
                   (let val header = Transtypes.unwrapOp(lt1,lt2)
                     in header(DECON'(d,exp))
                    end))
         end
      | DECON''(k as (DATAcon d),NONE,exp) = DECON'(d,exp) 
      | DECON'' _ = impossible "DECON of non constructor"

    datatype andor
      = AND of {bindings : (int * var) list,
                subtrees : andor list,
                constraints : (dconinfo * int list * andor option) list}
      | CASE of {bindings : (int * var) list,
                 sign : Access.conrep list,
                 cases : (con * ty option * int list * andor list) list,
                 constraints : (dconinfo * int list * andor option) list}
      | LEAF of {bindings : (int * var) list,
                 constraints : (dconinfo * int list * andor option) list}
    
    datatype decision
      = CASEDEC of path * Access.conrep list * 
                  (con * ty option * int list * decision list) list * int list
      | ABSCONDEC of path * dconinfo * int list * decision list * int list 
      | BINDDEC of path * int list
    
    fun allConses(hds, tls) = 
        fold op @ (map (fn hd => (map (fn tl => hd::tl) tls)) hds) nil

    fun orExpand (ORpat(pat1,pat2)) = 
          [pat1, pat2]
      | orExpand (pat as RECORDpat{pats=ref pats,...}) =
          map (mkRECORDpat pat) (fold allConses (map orExpand pats) [nil])
      | orExpand (VECTORpat(pats,t)) =
          map (fn p => VECTORpat(p,t))
                  (fold allConses (map orExpand pats) [nil])
      | orExpand (APPpat(k,t,pat)) =
          map (fn pat => APPpat(k,t,pat)) (orExpand pat)
      | orExpand (CONSTRAINTpat(pat,_)) =
          orExpand pat
      | orExpand (LAYEREDpat(CONSTRAINTpat(lpat, _), bpat)) =
          orExpand (LAYEREDpat(lpat, bpat))
      | orExpand (LAYEREDpat(lpat, bpat)) =
          map (fn pat => LAYEREDpat(lpat,pat)) (orExpand bpat)
      | orExpand pat = 
          [pat]

    fun pathInstSimpexp varenv (VARsimp v) = 
          (lookup (v, varenv) handle Lookup => impossible "unbound 18")
      | pathInstSimpexp varenv (RECORDsimp simps) = 
          RECORDPATH (map (pathInstSimpexp varenv) simps)
  
    fun expandBindings (varenv, pathenv, nil) = nil
      | expandBindings (varenv, pathenv, v::rest) =
          (pathInstSimpexp pathenv (fullyExpandBinding varenv (VARsimp v)))
            :: (expandBindings(varenv, pathenv, rest))
  
    fun boundVariables (VARpat v) = 
          [v]
      | boundVariables (CONSTRAINTpat(pat,_)) = 
          boundVariables pat
      | boundVariables (LAYEREDpat(pat1, pat2)) = 
          (boundVariables(pat1))@(boundVariables(pat2))
      | boundVariables (APPpat(k,t,pat)) =
          boundVariables pat
      | boundVariables (RECORDpat{pats=ref pats,...}) =
          fold op @ (map boundVariables pats) nil
      | boundVariables (VECTORpat(pats,_)) =
          fold op @ (map boundVariables pats) nil
      | boundVariables (ORpat (pat1,_)) =
          boundVariables pat1
      | boundVariables _ = 
          nil
    
    fun patternBindings (VARpat v, path) = 
          [(v, path)]
      | patternBindings (CONSTRAINTpat(pat,_), path) = 
          patternBindings(pat, path)
      | patternBindings (LAYEREDpat(pat1, pat2), path) = 
          (patternBindings(pat1, path))@(patternBindings(pat2, path))
      | patternBindings (APPpat(k,t,pat), path) = 
          patternBindings(pat, DELTAPATH(mkDATAcon k, t, path))
      | patternBindings (RECORDpat{pats=ref pats,...}, path) = 
          let 
            fun doGen(n, nil) = nil
              | doGen(n, pat::rest) = 
                  (patternBindings(pat,PIPATH(n,path))) @ (doGen(n+1,rest))
          in 
            doGen(0, pats)
          end
      | patternBindings (VECTORpat(pats,t), path) = 
          let 
            fun doGen(n, nil) = nil
              | doGen(n, pat::rest) = 
                  (patternBindings(pat,VPIPATH(n,t,path))) @ (doGen(n+1,rest))
          in 
            doGen(0, pats)
          end
      | patternBindings (ORpat _, _) = 
          impossible "Unexpected or pattern"
      | patternBindings _ = 
          nil
     
    fun patPaths (pat, constrs) =
        let 
          val patEnv = patternBindings(pat, ROOTPATH)
          fun constrPaths (nil, env, acc) = 
                ((ROOTPATH, pat)::(rev acc), env)
            | constrPaths ((simpexp,cpat)::rest, env, acc) = 
                let
                  val guardPath = pathInstSimpexp env simpexp
                  val newEnv = patternBindings(cpat, guardPath)
                in
                  constrPaths(rest, env@newEnv, (guardPath, cpat)::acc)
                end
        in
          constrPaths(constrs, patEnv, nil)
        end

    fun genRHSFun ([], rhs) = FN(mkLvar(),INTty,rhs)
      | genRHSFun ([v], rhs) = 
          let val (argvar,argt) = vartolvar v
           in FN(argvar,argt,rhs)
          end
      | genRHSFun (vl, rhs) =
	  let val argvar = mkLvar()
              fun foo (nil, n) = (rhs,nil)
                | foo (v::vl, n) = 
                    let val (lv,lt) = vartolvar v
                        val (le,tt) = foo(vl,n+1)
                     in (APP(FN(lv,BOGUSty,le), 
                             SELECT(n,VAR argvar)),lt::tt)
                    end
              val (body,tt) = foo(vl,0)
           in FN(argvar,RECORDty tt,body)
          end
	   
    fun preProcessPat (pat, rhs) =
          let 
	    val bindings = boundVariables pat
            val fname = mkLvar()
            val rhsFun = genRHSFun (bindings, rhs)
            val pats = orExpand pat
            fun expand nil = nil
              | expand (pat::rest) =
                  let
                    val (newpat, constrs, varenv) = templateExpandPattern pat
                    val (newlist, pathenv) = patPaths (newpat, constrs)
                    val bindingPaths = expandBindings(varenv,pathenv,bindings)
                  in  
                    (newlist, bindingPaths, fname)::(expand rest)
                  end handle CANT_MATCH =>
                    ([(ROOTPATH, NOpat)], nil, fname)::(expand rest)
          in
            (expand pats, (fname, rhsFun))
          end

    fun addBinding (v, rule, AND{bindings, subtrees, constraints}) =
          AND{bindings=(rule,v)::bindings, subtrees=subtrees, 
              constraints=constraints}
      | addBinding (v, rule, CASE{bindings, sign, cases, constraints}) =
          CASE{bindings=(rule,v)::bindings, cases=cases, sign = sign,
               constraints=constraints}
      | addBinding (v, rule, LEAF{bindings, constraints}) =
          LEAF{bindings=(rule,v)::bindings, constraints=constraints}


    fun addAConstraint(k, NONE, rule, nil) =
          [(k, [rule], NONE)]
      | addAConstraint(k, SOME pat, rule, nil) =
          [(k, [rule], SOME(genAndor(pat, rule)))]
      | addAConstraint(k, patopt as SOME pat, rule, 
                       (constr as (k', rules, SOME subtree))::rest) =
          if conEq'(k, k') then
            (k, rule::rules, SOME(mergeAndor(pat, subtree, rule)))::rest
          else 
            constr::(addAConstraint(k, patopt, rule, rest))
      | addAConstraint(k, NONE, rule, (constr as (k', rules, NONE))::rest) =
          if conEq'(k, k') then (k, rule::rules, NONE)::rest
          else constr::(addAConstraint(k, NONE, rule, rest))
      | addAConstraint(k, patopt, rule, (constr as (k', rules, _))::rest) =
	  if conEq'(k, k') then impossible "arity conflict"
          else constr::(addAConstraint(k, patopt, rule, rest))

    and addConstraint(k, patopt, rule, AND{bindings, subtrees, constraints}) =
          AND{bindings=bindings, subtrees=subtrees, 
              constraints=addAConstraint(k, patopt, rule, constraints)}
      | addConstraint(k, patopt, rule, CASE{bindings, sign, cases, 
                                            constraints}) =
          CASE{bindings=bindings, cases=cases, sign = sign,
               constraints=addAConstraint(k, patopt, rule, constraints)}
      | addConstraint(k, patopt, rule, LEAF{bindings, constraints}) =
          LEAF{bindings=bindings, 
               constraints=addAConstraint(k, patopt, rule, constraints)}
          
    and genAndor (VARpat v, rule) =
          LEAF {bindings = [(rule, v)], constraints = nil}
      | genAndor (WILDpat, rule) =
          LEAF {bindings = nil, constraints = nil}
      | genAndor (CONSTRAINTpat(pat, _), rule) =
          genAndor(pat, rule)
      | genAndor (LAYEREDpat(CONSTRAINTpat(lpat,_), bpat), rule) =
          genAndor (LAYEREDpat(lpat, bpat), rule)
      | genAndor (LAYEREDpat(VARpat v, bpat), rule) =
          addBinding (v, rule, genAndor (bpat, rule))
      | genAndor (LAYEREDpat(CONpat(k,t), bpat), rule) =
          addConstraint ((k,t), NONE, rule, genAndor(bpat, rule))
      | genAndor (LAYEREDpat(APPpat(k,t,lpat), bpat), rule) =
          addConstraint ((k,t), SOME lpat, rule, genAndor(bpat, rule))
      | genAndor (INTpat n, rule) =
          CASE {bindings = nil, constraints = nil, sign = nil,
                cases = [(INTcon n, NONE, [rule], nil)]}
      | genAndor (REALpat r, rule) =
          CASE {bindings = nil, constraints = nil, sign = nil,
                cases = [(REALcon r, NONE, [rule], nil)]}
      | genAndor (STRINGpat s, rule) =
          CASE {bindings = nil, constraints = nil, sign = nil,
                cases = [(STRINGcon s, NONE, [rule], nil)]}
      | genAndor (RECORDpat{pats=ref pats,...}, rule) =
          AND{bindings = nil, constraints = nil, subtrees=multiGen(pats, rule)}
      | genAndor (VECTORpat(pats,t), rule) =
          CASE {bindings = nil, constraints = nil, sign = nil,
                cases = [(VLENcon (length pats), SOME t, [rule], 
                          multiGen(pats, rule))]}
      | genAndor (CONpat(k,t), rule) =
          if abstract k then
            LEAF {bindings = nil, constraints = [((k,t), [rule], NONE)]}
          else
            CASE {bindings = nil, constraints = nil, sign = signOfCon k,
                  cases = [(mkDATAcon k, t, [rule], nil)]}
      | genAndor (APPpat(k,t,pat), rule) =
          if abstract k then
            LEAF {bindings = nil, 
                  constraints = [((k,t), [rule], SOME(genAndor (pat, rule)))]}
          else
            CASE {bindings = nil, constraints = nil, sign = signOfCon k,
                  cases = [(mkDATAcon k, t, [rule], [genAndor(pat, rule)])]}
      | genAndor _ =
	  impossible "genandor applied to inapplicable pattern"

    and multiGen(nil, rule) = nil
      | multiGen(pat::rest, rule) = (genAndor(pat,rule))::multiGen((rest,rule))

    and mergeAndor (VARpat v, andor, rule) =
          addBinding (v, rule, andor)
      | mergeAndor (WILDpat, andor, rule) =
          andor
      | mergeAndor (CONSTRAINTpat(pat, _), andor, rule) =
          mergeAndor(pat, andor, rule)
      | mergeAndor (LAYEREDpat(CONSTRAINTpat(lpat,_), bpat), andor, rule) =
          mergeAndor (LAYEREDpat(lpat, bpat), andor, rule)
      | mergeAndor (LAYEREDpat(VARpat v, bpat), andor, rule) =
          addBinding (v, rule, mergeAndor (bpat, andor, rule))
      | mergeAndor (LAYEREDpat(CONpat(k,t), bpat), andor, rule) =
          addConstraint ((k,t), NONE, rule, mergeAndor(bpat, andor, rule))
      | mergeAndor (LAYEREDpat(APPpat(k,t,lpat), bpat), andor, rule) =
          addConstraint ((k,t), SOME lpat, rule, mergeAndor(bpat, andor, rule))
      | mergeAndor (CONpat(k,t), LEAF{bindings, constraints}, rule) =
          if abstract k then
            LEAF {bindings = nil, 
                  constraints = addAConstraint((k,t), NONE, rule, constraints)}
          else
            CASE {bindings = nil, constraints = nil, sign = signOfCon k,
                  cases = [(mkDATAcon k, t, [rule], nil)]}
      | mergeAndor (APPpat(k,t,pat), LEAF{bindings, constraints}, rule) =
          if abstract k then
            LEAF {bindings = bindings,
                  constraints = addAConstraint((k,t), SOME pat, 
                                               rule, constraints)}
          else
            CASE {bindings = bindings, constraints = constraints, 
                  sign = signOfCon k,
                  cases = [(mkDATAcon k, t, [rule], [genAndor(pat, rule)])]}
      | mergeAndor (pat, LEAF{bindings, constraints}, rule) =
          (case genAndor(pat, rule)
             of CASE{bindings=nil, constraints=nil, sign, cases} =>
                  CASE{bindings=bindings, sign=sign, 
                       constraints=constraints, cases=cases}
              | AND{bindings=nil, constraints=nil, subtrees} =>
                  AND{bindings=bindings, constraints=constraints, 
                      subtrees=subtrees}
              | _ => 
	          impossible "genAndor returned bogusly")
      | mergeAndor (INTpat n, CASE{bindings, cases, constraints, sign}, rule) =
          CASE {bindings = bindings, constraints = constraints, sign=sign,
                cases = addACase(INTcon n, NONE, nil, rule, cases)}
      | mergeAndor (REALpat r, CASE{bindings, cases, constraints,sign}, rule) =
          CASE {bindings = bindings, constraints = constraints, sign=sign,
                cases = addACase(REALcon r, NONE, nil, rule, cases)}
      | mergeAndor (STRINGpat s, CASE{bindings, cases, constraints,
                                      sign}, rule) =
          CASE {bindings = bindings, constraints = constraints, sign=sign,
                cases = addACase(STRINGcon s, NONE, nil, rule, cases)}
      | mergeAndor (RECORDpat{pats=ref pats,...}, 
                    AND{bindings, constraints, subtrees}, rule) =
          AND{bindings = bindings, constraints = constraints, 
              subtrees=multiMerge(pats, subtrees, rule)}
      | mergeAndor (VECTORpat(pats,t), CASE{bindings, cases, sign, 
                                         constraints}, rule) =
          CASE {bindings = bindings, constraints = constraints, sign = sign,
                cases = addACase(VLENcon(length pats),SOME t,pats,rule,cases)}
      | mergeAndor (CONpat(k,t), CASE{bindings, 
                                      cases, constraints, sign}, rule) =
          if abstract k then
            CASE {bindings=bindings, cases=cases, sign=sign,
                  constraints=addAConstraint((k,t), NONE, rule, constraints)}
          else
            CASE {bindings=bindings, constraints=constraints, sign=sign,
                  cases=addACase(mkDATAcon k, t, nil, rule, cases)}
      | mergeAndor (APPpat(k,t,pat), CASE{bindings, cases, 
                                          constraints, sign}, rule) =
          if abstract k then
            CASE {bindings=bindings, cases=cases,  sign=sign,
                  constraints=addAConstraint((k,t), SOME pat, 
                                             rule, constraints)}
          else
            CASE {bindings=bindings, constraints=constraints, sign=sign,
                  cases=addACase(mkDATAcon k, t, [pat], rule, cases)}
      | mergeAndor (CONpat(k,t), AND{bindings, constraints, subtrees}, rule) =
          if abstract k then
            AND {bindings=bindings, subtrees=subtrees,
                 constraints=addAConstraint((k,t), NONE, rule, constraints)}
          else
	    impossible "concrete constructor can't match record"
      | mergeAndor (APPpat(k,t,pat), AND{bindings,subtrees,constraints}, rule) =
          if abstract k then
            AND {bindings=bindings, subtrees=subtrees,
                 constraints=addAConstraint((k,t), SOME pat, 
                                            rule, constraints)}
          else
            impossible "concrete constructor application can't match record"
      | mergeAndor _ =
	  impossible "bad pattern merge"
    
   and addACase (con, t, pats, rule, nil) =
          [(con, t, [rule], multiGen(pats, rule))]
     | addACase (con, t, pats, rule, 
                 (aCase as (con',t',rules,subtrees))::rest) =
          if constantEq(con, con') then
            (con, t, rule::rules, multiMerge(pats, subtrees, rule))::rest
          else 
            aCase::(addACase(con, t, pats, rule, rest))

   and multiMerge (nil, nil, rule) = nil
     | multiMerge (pat::pats, subtree::subtrees, rule) =
         (mergeAndor(pat, subtree, rule))::(multiMerge(pats, subtrees, rule))
     | multiMerge _ = impossible "list length mismatch in multiMerge"


   fun addABinding (path, rule, nil) = [BINDDEC(path, [rule])]
     | addABinding (path, rule, (bind as BINDDEC(path', rules))::rest) =
         if pathEq(path, path') then BINDDEC(path, rule::rules)::rest
         else bind::(addABinding(path, rule, rest)) 
     | addABinding _ = impossible "non BINDDEC in binding list"

   fun flattenBindings (nil, path, active) = nil
     | flattenBindings (((rule, v)::rest), path, active) =
         if isthere(rule, active) then 
           addABinding(path, rule, flattenBindings(rest, path,active))
         else 
           flattenBindings(rest, path, active)

   fun flattenConstraints (nil, path, active) = nil
     | flattenConstraints ((con,rules,NONE)::rest, path, active) = 
	 let
           val yesActive = intersect(active, rules)
           val noActive = setDifference(active, rules)
           val rest' = flattenConstraints(rest, path, active)
         in 
           (ABSCONDEC(path, con, yesActive, nil, noActive))::rest'
	 end
     | flattenConstraints ((con,rules,SOME andor)::rest, path, active) = 
	 let
           val yesActive = intersect(active, rules)
           val noActive = setDifference(active, rules)
           val rest' = flattenConstraints(rest, path, active)
           val andor' = 
                flattenAndor(andor, mkDELTAPATH(con, path), active)
          in 
           (ABSCONDEC(path, con, yesActive, andor', noActive))::rest'
	 end     

    and flattenAndor (AND {bindings, subtrees, constraints}, path, active) =
          let val btests = flattenBindings(bindings, path, active)
              fun dotree (n, nil) =
                    flattenConstraints(constraints, path, active)
                | dotree (n, subtree::rest) =
                    let 
                      val othertests = dotree(n + 1, rest)
                    in 
                      (flattenAndor(subtree,PIPATH(n,path),active))@othertests
                    end
          in
            btests@(dotree(0, subtrees))
          end
      | flattenAndor (CASE {bindings, cases, constraints,sign}, path, active) =
          let val btests = flattenBindings(bindings, path, active)
              val ctests = flattenConstraints(constraints, path, active)
          in
            btests@((flattenCases(cases, path, active,sign))::ctests)
          end
      | flattenAndor (LEAF {bindings, constraints}, path, active) =       
          let 
            val btests = flattenBindings(bindings, path, active)
          in
            btests@(flattenConstraints(constraints, path, active))
          end

    and flattenACase((VLENcon n,SOME t,rules,subtrees),path,active,defaults) =
          let 
            val stillActive = intersect(union(rules, defaults), active)
            val ruleActive = intersect(rules, active)
            fun flattenVSubs (n, nil) = nil
              | flattenVSubs (n, subtree::rest) = 
                  (flattenAndor(subtree, VPIPATH(n,t,path), stillActive)) 
                    @ (flattenVSubs(n + 1, rest))
          in 
            (INTcon n, NONE, ruleActive, flattenVSubs(0, subtrees))
          end     
      | flattenACase((k as DATAcon _,t,rules,[subtree]),path,active,defaults) =
          let  
            val stillActive = intersect(union(rules, defaults), active)
            val ruleActive = intersect(rules, active)
            val newPath = DELTAPATH(k,t,path)                    
          in 
            (k,t,ruleActive,flattenAndor(subtree,newPath,stillActive))
          end 
      | flattenACase((constant,t,rules,nil),path,active,defaults) =
          (constant, t, intersect(rules, active), nil)
      | flattenACase _ =
          impossible "illegal subpattern in a case"
       
    and flattenCases(cases, path, active,sign) =
	let 
          fun calcDefaults (nil, active) = active
            | calcDefaults ((_,_,rules,_)::rest, active)  =
                calcDefaults(rest, setDifference(active, rules))
	  val defaults = calcDefaults(cases, active)
          fun doit nil = nil
            | doit (aCase::rest) = 
                ((flattenACase(aCase, path, active, defaults)) 
                 :: (doit(rest)))
        in 
	  case cases
            of (VLENcon _,_,_,_)::_ => 
                 CASEDEC(VLENPATH path, sign, doit cases, defaults)
             | cases => CASEDEC(path, sign, doit cases, defaults)
        end

    fun mergePatWithAndorList(path, pat, nil, n) =
          [(path, genAndor(pat, n))]
      | mergePatWithAndorList(path, pat, (path',andor)::rest, n) =
          if pathEq(path, path') then (path, mergeAndor(pat, andor, n))::rest
          else (path',andor)::(mergePatWithAndorList(path, pat, rest, n))

    fun genAndorList (nil, n) = impossible "no patterns (gen)"
      | genAndorList ([(path, pat)], n) = [(path, genAndor(pat, n))]
      | genAndorList ((path, pat)::rest, n) = 
	  mergePatWithAndorList(path, pat, genAndorList(rest, n), n)

    fun mergeAndorList (nil, aol, n) = impossible "no patterns (merge)"
      | mergeAndorList ([(path, pat)], aol, n) = 
          mergePatWithAndorList(path, pat, aol, n)
      | mergeAndorList ((path, pat)::rest, aol, n) = 
	  mergePatWithAndorList(path, pat, mergeAndorList(rest, aol, n), n)

    fun makeAndor' (nil, n) = impossible "no rules (makeAndor')"
      | makeAndor' ([(pats, _, _)], n) = 
          genAndorList (pats, n)
      | makeAndor' (([(_, NOpat)], env, bindings)::rest, n) =
          makeAndor'(rest, n+1)
      | makeAndor' ((pats, env, bindings)::rest, n) =
          mergeAndorList(pats, makeAndor'(rest, n+1), n)

    fun makeAndor x = makeAndor' x (* handle Foo => raise (Internal 99) *)

    fun bindings (n, l) = case (nth(l, n)) of (_,_,x) => x

    fun pathConstraints (RECORDPATH paths) =
          fold op @ (map pathConstraints paths) nil
      | pathConstraints path = [path]

    fun flattenAndors(nil, allrules) = nil
      | flattenAndors((path, andor)::rest, allrules) =
          (pathConstraints path, flattenAndor(andor, path, allrules))
            :: (flattenAndors(rest, allrules))

    fun removePath(path, path1::rest) =
          if pathEq(path, path1) then rest 
          else path1::(removePath(path, rest))
      | removePath (path, nil) = nil

    fun fireConstraint (path, (needPaths, decisions)::rest, ready, delayed) =
          (case removePath(path, needPaths) 
             of nil => fireConstraint(path, rest, decisions@ready, delayed)
              | x => fireConstraint(path, rest, ready, (x,decisions)::delayed))
      | fireConstraint (path, nil, ready, delayed) =
          (ready, delayed)

    fun mkAllRules (nil,_) = nil 
      | mkAllRules(([(ROOTPATH, NOpat)],_,_)::b, n) = (mkAllRules(b, n + 1))
      | mkAllRules(_::b, n) = n::(mkAllRules(b, n + 1))

    exception PickBest

    fun relevent (CASEDEC(_,_,_,defaults), rulenum) = 
          not (isthere(rulenum, defaults))
      | relevent (ABSCONDEC (_,_,_,_,defaults), rulenum) =
          not (isthere(rulenum, defaults))
      | relevent (BINDDEC _, _) = 
          impossible "BINDDEC not fired"

    fun metric (CASEDEC(_,_,cases, defaults)) = (length defaults, length cases)
      | metric (ABSCONDEC (_,_,_,_,defaults)) = (length defaults, 2)
      | metric (BINDDEC _) = impossible "BINDDEC not fired (metric)"

    fun metricBetter((a:int,b:int),(c,d)) = a < c orelse (a = c andalso b < d)

    fun doPickBest(nil, _, _, _, NONE) = raise PickBest
      | doPickBest(nil, _, _, _, SOME n) = n
      | doPickBest((BINDDEC _)::rest, _, n, _, _) = n
      | doPickBest((CASEDEC(_, [sign], _, _))::rest, _, n, _, _) = n
      | doPickBest(aCase::rest, active as act1::_, n, NONE, NONE) =
	  if relevent (aCase, act1) then
	    doPickBest(rest, active, n + 1, SOME(metric aCase), SOME n)
	  else 
	    doPickBest(rest, active, n + 1, NONE, NONE)
      | doPickBest(aCase::rest, active as act1::_, n, SOME m, SOME i) =
	  if relevent (aCase, act1) then
	    let val myMetric = metric aCase
	    in
	      if metricBetter(myMetric, m) then
                doPickBest(rest, active, n + 1, SOME(myMetric), SOME n)
	      else 
                doPickBest(rest, active, n + 1, SOME m, SOME i)
            end
	  else 
	    doPickBest(rest, active, n + 1, SOME m, SOME i)
      | doPickBest _ = impossible "impossible situation in doPickBest"
 
    fun pickBest (l, active) = doPickBest(l, active, 0, NONE, NONE)

    fun extractNth(0, a::b) = (a, b)
      | extractNth(n, a::b) = 
          let val (c,d) = extractNth(n - 1, b) in (c, a::d) end
      | extractNth _ = impossible "extractNth called with too big n"

    fun filter (f, nil) = nil | 
        filter (f, a::b) = if f a then a::(filter(f,b)) else filter(f,b)

    fun genDecisionTree((decisions, delayed), active as active1::_) =
          ((case extractNth(pickBest(decisions, active), decisions)
             of (BINDDEC(path, _), rest) =>
	          genDecisionTree(fireConstraint(path,delayed,rest,nil),active)
              | (CASEDEC(path, [sign], [(_,_,_,guarded)], defaults), rest) => 
                  genDecisionTree((rest@guarded, delayed), active)
              | (CASEDEC(path, sign, cases, defaults), rest) =>
	          let 
                    fun isActive(_,_,rules,_) = intersect(rules, active) <> []
                    val activeCases = filter(isActive, cases)
                    val caseTrees = 
                         gencases(activeCases, rest, delayed, defaults, active)
		    val defActive = intersect(active, defaults)
		    val defTree = 
                          if length activeCases = length sign then NONE 
                          else
                            SOME (genDecisionTree((rest, delayed), defActive))
 	          in 
                    CASETEST(path, sign, caseTrees, defTree)
                  end
              | (ABSCONDEC(path, con, yes, guarded, defaults), rest) =>
	          let 
                    val yesActive = intersect(active, union(yes, defaults))
                    val noActive = intersect(active,defaults)
                    val yesTree = 
                          genDecisionTree((rest@guarded, delayed), yesActive)
		    val defTree = genDecisionTree((rest, delayed), noActive)
 	          in 
	            if unary con then ABSTEST1(path, con, yesTree, defTree)
	            else ABSTEST0(path, con, yesTree, defTree)
                  end)
           handle PickBest => (RHS active1))
      | genDecisionTree (_,active) = impossible "nothing active"

    and gencases (nil, decs, delayed, defaults, active) = nil
      | gencases ((const,t,rules,guarded)::rest,decs,delayed,defaults,active)= 
	  let 
	      val rActive = intersect(union(defaults, rules), active)
          in 
            (const, t, genDecisionTree((decs@guarded, delayed),rActive))
              :: (gencases(rest,decs,delayed,defaults,active))
          end

    local 
      open PrintUtil
      val printDepth = System.Print.printDepth
    in
      fun matchPrint(env,rules,unused) =        
	   (fn ppstrm =>
	     let fun matchPrint' ([],_,_) = ()
		   | matchPrint' ([(pat,_)],_,_) = () (* never print last rule *)
		   | matchPrint' ((pat,_)::more,[],_) =
		       (add_string ppstrm "        "; 
			PPAbsyn.ppPat env ppstrm (pat,!printDepth);
			add_string ppstrm " => ...";
			add_newline ppstrm;
			matchPrint' (more,[],0))
		   | matchPrint' ((pat,_)::more,(taglist as (tag::tags)),i) =
		       if i = tag then
			 (add_string ppstrm "  -->   ";
			  PPAbsyn.ppPat env ppstrm (pat,!printDepth);
			  add_string ppstrm " => ..."; 
			  add_newline ppstrm;
			  matchPrint'(more,tags,i+1))
		       else 
			 (add_string ppstrm "        ";
			  PPAbsyn.ppPat env ppstrm (pat,!printDepth);
			  add_string ppstrm " => ...";
			  add_newline ppstrm;
			  matchPrint'(more,taglist,i+1))
	      in add_newline ppstrm;
		 begin_block ppstrm CONSISTENT 0;
		 matchPrint'(rules,unused,0);
		 end_block ppstrm
	     end)

      fun bindPrint(env,(pat,_)::_) =
	   (fn ppstrm =>
              (add_newline ppstrm;
	       add_string ppstrm "        "; 
               PPAbsyn.ppPat env ppstrm (pat,!printDepth);
	       add_string ppstrm " = ..."))
        | bindPrint _ = impossible "bindPrint in mc"
    end

    fun rulesUsed (RHS n) = [n]
      | rulesUsed (BIND(_, dt)) = rulesUsed dt
      | rulesUsed (CASETEST(_, _, cases, NONE)) =
	  fold (fn((_,_,a), b) => union(rulesUsed a, b)) cases nil
      | rulesUsed (CASETEST(_, _, cases, SOME dt)) =
	  fold (fn((_,_,a), b) => union(rulesUsed a, b)) cases (rulesUsed dt)
      | rulesUsed (ABSTEST0(_, _, yes, no)) = 
          union(rulesUsed yes, rulesUsed no)
      | rulesUsed (ABSTEST1(_, _, yes, no)) = 
          union(rulesUsed yes, rulesUsed no)

    fun fixupUnused(nil, _, _, _, out) = out
      | fixupUnused(unused, (nil, _)::rest, n, m, out) = 
          fixupUnused(unused, rest, n, m + 1, out)
      | fixupUnused(unused::urest, (rule::rules, x)::mrest, n, m, nil) =
          if unused = n then 
             fixupUnused(urest, (rules, x)::mrest, n + 1, m, [m])
	  else 
             fixupUnused(unused::urest, (rules, x)::mrest, n + 1, m, nil)
      | fixupUnused(unused::urest, (rule::rules, z)::mrest, n, m, x::y) =
          if unused = n then
             (if m <> x then
	        fixupUnused(urest, (rules, z)::mrest, n + 1, m, m::x::y)
              else 
                fixupUnused(urest, (rules, z)::mrest, n + 1, m, x::y))
          else 
             fixupUnused(unused::urest, (rules, z)::mrest, n + 1, m, x::y)
      | fixupUnused _ = impossible "bad fixup"

    fun redundant (nil, n) = false
      | redundant (a::b, n) = a <> n orelse redundant (b, n)
    
    fun complement(n, m, a::b) =
          if n < a then n::(complement(n + 1, m, a::b))
          else complement(n + 1, m, b)
      | complement(n, m, nil) = 
          if n < m then n::(complement(n + 1, m, nil)) else nil
  fun dividePathList(pred, nil, accyes, accno) = (accyes, accno)
      | dividePathList(pred, path::rest, accyes, accno) = 
          if pred path then dividePathList(pred, rest, path::accyes, accno)
          else dividePathList(pred, rest, accyes, path::accno)

    fun addPathToPathList (path, path1::rest) = 
          if pathEq(path, path1) then path1::rest
          else path1::(addPathToPathList(path, rest))
      | addPathToPathList (path, nil) = [path]

    fun unitePathLists(paths1, nil) = paths1
      | unitePathLists(nil, paths2) = paths2
      | unitePathLists(path1::rest1, paths2) = 
          addPathToPathList(path1, unitePathLists(rest1, paths2))

    fun onPathList (path1, nil) = false
      | onPathList (path1, path2::rest) = 
          pathEq(path1, path2) orelse onPathList(path1, rest)

    fun intersectPathLists(paths1, nil) = nil
      | intersectPathLists(nil, paths2) = nil
      | intersectPathLists(path1::rest1, paths2) = 
          if onPathList(path1,paths2) then 
            path1::(intersectPathLists(rest1, paths2))
          else
            intersectPathLists(rest1, paths2)

    fun differencePathLists(paths1, nil) = paths1
      | differencePathLists(nil, paths2) = nil
      | differencePathLists(path1::rest1, paths2) = 
          if onPathList(path1,paths2) then 
            differencePathLists(rest1, paths2)
          else
            path1::(differencePathLists(rest1, paths2))

    fun intersectPathsets(pathset1, nil) = nil
      | intersectPathsets(nil, pathset2) = nil
      | intersectPathsets(pathset1 as (n1:int, paths1)::rest1, 
                          pathset2 as (n2, paths2)::rest2) =
          if n1 = n2 then 
            case intersectPathLists(paths1, paths2)
              of nil => intersectPathsets(rest1, rest2)
               | pl => (n1, pl)::(intersectPathsets(rest1, rest2))
          else if n1 < n2 then 
            intersectPathsets(rest1, pathset2)
          else
            intersectPathsets(pathset1, rest2)
      
    fun unitePathsets(pathset1, nil) = pathset1
      | unitePathsets(nil, pathset2) = pathset2
      | unitePathsets(pathset1 as (n1:int, paths1)::rest1, 
                      pathset2 as (n2, paths2)::rest2) =
          if n1 = n2 then 
            (n1, unitePathLists(paths1, paths2))
              :: (unitePathsets(rest1, rest2))
          else if n1 < n2 then 
            (n1, paths1)::(unitePathsets(rest1, pathset2))
          else
            (n2, paths2)::(unitePathsets(pathset1, rest2))
      
    fun differencePathsets(pathset1, nil) = pathset1
      | differencePathsets(nil, pathset2) = nil
      | differencePathsets(pathset1 as (n1:int, paths1)::rest1, 
                           pathset2 as (n2, paths2)::rest2) =
          if n1 = n2 then 
            case differencePathLists(paths1, paths2)
              of nil => differencePathsets(rest1, rest2)
               | pl => (n1, pl)::(differencePathsets(rest1, rest2))
          else if n1 < n2 then 
            (n1, paths1)::(differencePathsets(rest1, pathset2))
          else
            differencePathsets(pathset1, rest2)

    fun doPathsetMember(path, metric, (n:int, paths)::rest) =
          (n < metric andalso doPathsetMember(path, metric, rest))
            orelse (n = metric andalso onPathList(path, paths))
      | doPathsetMember(path, metric, nil) = false    

    fun doAddElementToPathset(path, metric, nil) = [(metric, [path])]
      | doAddElementToPathset(path, metric, (n:int, paths)::rest) =
          if n = metric then (n, addPathToPathList(path, paths))::rest
          else if n < metric then
            (n,paths)::(doAddElementToPathset(path, metric, rest))
          else (metric, [path])::(n, paths)::rest

    fun dividePathset(pred, nil) = (nil, nil)
      | dividePathset(pred, (n, pathlist)::rest) =
          let val (yesSet, noSet) = dividePathset(pred, rest)
          in case dividePathList(pred, pathlist, nil, nil) 
               of (nil, nil) => impossible "paths dissappeared during divide"
                | (nil, no) => (yesSet, (n,no)::noSet)
                | (yes, nil) => ((n, yes)::yesSet, noSet)
                | (yes, no) => ((n, yes)::yesSet, (n,no)::noSet)
          end
    
    fun pathDepends path1 ROOTPATH = pathEq(path1, ROOTPATH)
      | pathDepends path1 (path2 as RECORDPATH paths) = 
          revfold (fn (a, b) => (pathDepends path1 a) orelse b) 
                  paths 
                  (pathEq(path1, path2))
      | pathDepends path1 (path2 as PIPATH(_, subpath)) =
          pathEq(path1, path2) orelse pathDepends path1 subpath      
      | pathDepends path1 (path2 as VPIPATH(_,_,subpath)) =
          pathEq(path1, path2) orelse pathDepends path1 subpath      
      | pathDepends path1 (path2 as DELTAPATH(_,_,subpath)) =
          pathEq(path1, path2) orelse pathDepends path1 subpath
      | pathDepends path1 (path2 as (VLENPATH subpath)) =
          pathEq(path1, path2) orelse pathDepends path1 subpath

    fun pathMetric ROOTPATH = 0
      | pathMetric (RECORDPATH paths) =
          fold (fn (a, b) => pathMetric a + b) paths 1
      | pathMetric (PIPATH(_, subpath)) =
          1 + pathMetric subpath
      | pathMetric (VPIPATH(_,_,subpath)) =
          1 + pathMetric subpath
      | pathMetric (DELTAPATH(_,_,subpath)) =
          1 + pathMetric subpath
      | pathMetric (VLENPATH subpath) =
          1 + pathMetric subpath

    fun pathsetMember path pathset = 
          doPathsetMember(path, pathMetric path, pathset)
    
    fun addPathToPathset(path, pathset) =
          doAddElementToPathset(path, pathMetric path, pathset) 


    fun genpath (RECORDPATH paths, env) =
           RECORD (map (fn path => VAR(lookup (path, env))) paths)
      | genpath (PIPATH(n, path), env) = SELECT(n, VAR(lookup(path, env)))
      | genpath (DELTAPATH(k,t,path), env) = 
           DECON''(k,t,VAR(lookup(path, env)))
      | genpath (VPIPATH(n, t, path), env) =
           let val lt = Transtypes.transTyLty(t)
               val header = Transtypes.unwrapOp(RBOXEDty,lt)
               val subscriptTy = ARROWty(RECORDty[BOXEDty,INTty],RBOXEDty)
            in header(APP(PRIM(P.SUBSCRIPTV,subscriptTy),
                      RECORD[VAR(lookup(path, env)),INT n]))
           end
      | genpath (VLENPATH path, env) = 
           APP(PRIM(P.LENGTH,ARROWty(BOXEDty,INTty)),
               VAR(lookup(path, env)))
      | genpath (ROOTPATH, env) = VAR(lookup(ROOTPATH, env))


    fun doDoBindings(nil, rhs) = rhs
      | doDoBindings(path::rest, rhs) = BIND(path, doDoBindings(rest, rhs))

    fun doBindings (nil, rhs) = rhs
      | doBindings ((n,paths)::morepaths, rhs) = 
	  doDoBindings(paths, doBindings(morepaths, rhs))

    fun subPaths ROOTPATH = [(0, [ROOTPATH])]
	      | subPaths (path as RECORDPATH paths) =
	  fold unitePathsets (map subPaths paths) [(pathMetric path, [path])]
      | subPaths (path as (VLENPATH subpath)) =
	  (subPaths subpath)@[(pathMetric path, [path])]
      | subPaths (path as VPIPATH (n,_,subpath)) =
	  (subPaths subpath)@[(pathMetric path, [path])]
      | subPaths (path as PIPATH (n, subpath)) =
	  (subPaths subpath)@[(pathMetric path, [path])]
      | subPaths (path as DELTAPATH (n,_,subpath)) =
	  (subPaths subpath)@[(pathMetric path, [path])]

    fun rhsbindings (n, ruleDesc) = 
          let val (_, paths, _) = nth(ruleDesc, n)
          in fold unitePathsets (map subPaths paths) nil
          end

    fun pass2rhs (n, env, ruleDesc) = 
          case nth(ruleDesc, n)
            of (_, [path], fname) => APP(VAR fname, VAR(lookup(path, env)))
             | (_, paths, fname) =>
                 APP(VAR fname, 
		     RECORD (map (fn path => VAR(lookup(path, env))) paths))

    fun pass1cases((con,t,subtree)::rest, envin, SOME envout, rhs, path) =
          let val (subtree', myEnvout) = pass1(subtree, envin, rhs)
              val (mustBindHere, otherBindings) =
                    dividePathset(pathDepends(DELTAPATH(con,t,path)),myEnvout)
              val envoutSoFar = intersectPathsets(envout, otherBindings)
              val (rest', envout') = 
                    pass1cases(rest, envin, SOME envoutSoFar, rhs, path)
              val iBind2 = differencePathsets(otherBindings, envout')
              val subtree'' = 
                    doBindings(unitePathsets(mustBindHere, iBind2), subtree')
           in 
             ((con,t,subtree'')::rest', envout')
           end
      | pass1cases((con,t,subtree)::rest, envin, NONE, rhs, path) =
          let val (subtree', myEnvout) = pass1(subtree, envin, rhs)
              val (mustBindHere, otherBindings) =
                    dividePathset(pathDepends(DELTAPATH(con,t,path)),myEnvout)
              val (rest', envout') = 
                    pass1cases(rest, envin, SOME otherBindings, rhs, path)
              val iBind2 = differencePathsets(otherBindings, envout')
              val subtree'' = 
                    doBindings(unitePathsets(mustBindHere, iBind2), subtree')
           in 
             ((con,t,subtree'')::rest', envout')
           end
      | pass1cases(nil, envin, SOME envout, rhs, path) =
          (nil, unitePathsets(envin, envout))
      | pass1cases(nil, envin, NONE, rhs, path) =
          impossible "pass1cases bad"


  and pass1(RHS n, envin, rhs) = (RHS n, rhsbindings(n, rhs))
    | pass1(CASETEST(path, sign, cases, NONE), envin, rhs) = 	
        let val (cases', envout') =
	         pass1cases(cases, unitePathsets(envin, subPaths path), 
                            NONE, rhs, path)
         in (CASETEST(path, sign, cases', NONE), envout')
        end
    | pass1(CASETEST(path, sign, cases, SOME subtree), envin, rhs) = 	
        let val newenv = unitePathsets(envin, subPaths path)
            val (subtree', subEnvout) = pass1(subtree, newenv, rhs)
            val (cases', envout') =
	          pass1cases(cases, newenv, SOME subEnvout, rhs, path)
            val subbindings = differencePathsets(subEnvout, envout')
            val subtree'' = doBindings(subbindings, subtree')
         in (CASETEST(path, sign, cases', SOME subtree''), envout')
        end
    | pass1 (ABSTEST0(path, con, subtree1, subtree2), envin, rhs) =
        let val newenv = unitePathsets(envin, subPaths path)
            val (subtree1', subEnvout1) = pass1(subtree1, newenv, rhs)
            val (subtree2', subEnvout2) = pass1(subtree2, newenv, rhs)
	    val envout =
                 unitePathsets(newenv,intersectPathsets(subEnvout1,subEnvout2))
            val bind1 = differencePathsets(subEnvout1, envout)
            val bind2 = differencePathsets(subEnvout2, envout)
            val subtree1'' = doBindings(bind1, subtree1')
            val subtree2'' = doBindings(bind2, subtree2')
         in (ABSTEST0(path, con, subtree1'', subtree2''), envout)
        end
    | pass1 (ABSTEST1(path, con, subtree1, subtree2), envin, rhs) =
        let val newenv = unitePathsets(envin, subPaths path)
            val yesenv =
	          if isAnException con then newenv
                  else addPathToPathset(mkDELTAPATH(con,path), envin)
            val (subtree1', subEnvout1) = pass1(subtree1, yesenv, rhs)
            val (subtree2', subEnvout2) = pass1(subtree2, newenv, rhs)
	    val envout = 
                  unitePathsets(newenv,
                                intersectPathsets(subEnvout1,subEnvout2))
            val bind1 = differencePathsets(subEnvout1, envout)
            val bind2 = differencePathsets(subEnvout2, envout)
            val subtree1'' = doBindings(bind1, subtree1')
            val subtree2'' = doBindings(bind2, subtree2')
         in (ABSTEST1(path, con, subtree1'', subtree2''), envout)
        end
     | pass1 _ = impossible "pass1 bad"


    fun pass2 (BIND(path, subtree), env, rhs) =
          let val newvar = mkLvar()
	      val subcode = pass2(subtree, (path, newvar)::env, rhs)
	   in APP(FN(newvar,BOGUSty,subcode), genpath(path, env))
          end
      | pass2 (CASETEST(path, sign, cases, NONE), env, rhs) = 
          SWITCH(VAR(lookup(path, env)), sign, 
                 pass2cases(cases,env,rhs), NONE)
      | pass2 (CASETEST(path, sign, cases, SOME subtree), env, rhs) = 
          SWITCH(VAR(lookup(path, env)), sign, 
                 pass2cases(cases,env,rhs),SOME(pass2(subtree,env,rhs)))
      | pass2 (ABSTEST0(path, con, yes, no), env, rhs) =
          if isAnException con 
          then SWITCH(VAR(lookup(path, env)), nil, 
                      [(mkDATAcon1 con,  pass2(yes, env, rhs))],
                      SOME(pass2(no, env, rhs)))
          else abstest0(path, con, pass2(yes,env,rhs), pass2(no,env,rhs)) 
      | pass2 (ABSTEST1(path, con, yes, no), env, rhs) =
          if isAnException con 
          then SWITCH(VAR(lookup(path, env)), nil, 
                      [(mkDATAcon1 con,  pass2(yes, env, rhs))],
                      SOME(pass2(no, env, rhs)))
          else abstest1(path, con, pass2(yes,env,rhs), pass2(no,env,rhs)) 
      | pass2 (RHS n, env, rhs) = pass2rhs(n, env, rhs)  

    and pass2cases(nil, env, rhs) = nil
      | pass2cases((con,t,subtree)::rest, env, rhs) = 
	  ((con,pass2(subtree, env, rhs))::(pass2cases(rest,env,rhs)))

    (* given a decision tree for a match, a list of ?? and the name of 
       the variable bound to the value to be matched, produce code for the 
       match 
     *)
    fun generate (dt, matchRep, rootVar) =
	let val (subtree, envout) = pass1(dt, [(0, [ROOTPATH])], matchRep)
         in case doBindings(envout, subtree)
             of BIND(ROOTPATH, subtree') => 
                  pass2(subtree', [(ROOTPATH, rootVar)], matchRep)
              | _ => pass2(subtree, [], matchRep)
        end
     
    fun doMatchCompile(rules,lt) = 
          let val matchReps = map preProcessPat rules
              val (matchRep,rhsRep) = 
                     fold (fn ((a,b),(c,d)) => (a@c,b::d)) matchReps (nil,nil)
              val allRules = mkAllRules(matchRep,0)
              val flattened = flattenAndors(makeAndor(matchRep,0),allRules)
              val ready = fireConstraint(ROOTPATH,flattened,nil,nil)
              val dt = genDecisionTree(ready,allRules)
	      val numRules = length matchRep
              val rawUnusedRules = complement(0,numRules,rulesUsed dt)
              val unusedRules = rev(fixupUnused(rawUnusedRules,matchReps,0,0,nil))
		   (* the rev fixes bug 670 *)
              val exhaustive = isthere(numRules-1,unusedRules)
              val rootvar = mkLvar()
	      fun funBind((fname, fbody), body) = 
                    APP(FN(fname,BOGUSty,body),fbody)
              val code = fold funBind rhsRep (generate(dt,matchRep,rootvar))
              val redundantF = redundant(unusedRules, length rules - 1)

           in (FN(rootvar,lt,code), unusedRules, redundantF, exhaustive)
          end

    (*
      Test pat, the guard pattern of the first match rule of a match,
      for the occurence of variables (including layering variables) 
      or wildcards.  Return true if any are present, false otherwise.
     *)      
    fun noVarsIn ((pat,_)::_) =
          let 
            fun var WILDpat = true (* might want to flag this *)
              | var (VARpat _) = true
              | var (LAYEREDpat _) = true
              | var (CONSTRAINTpat(p,_)) = var p
              | var (APPpat(_,_,p)) = var p
              | var (RECORDpat{pats=ref patlist,...}) = exists var patlist
              | var (VECTORpat(patlist,_)) = exists var patlist
              | var (ORpat (pat1,pat2)) = var pat1 orelse var pat2
              | var _ = false
          in 
            not(var pat)
          end
      | noVarsIn _ = impossible "noVarsIn in mc"

  (* 
    The three entry points for the match compiler.
    They take as arguments an environment (env); a match represented
    as a list of pattern--lambda expression pairs (match); and a 
    function to use in printing warning messages (warn).
 
    env and warn are only used in the printing of diagnostic information.
 
    If the control flag System.Control.MC.printArgs is
    set, they print match.  
  
    They call doMatchCompile to actually compile match.
    This returns a 4-tuple (code, unused, redundant, exhaustive).
    code is lambda code that implements match.  unused
    is a list of the indices of the unused rules.  redundant 
    and exhaustive are boolean flags which are set if 
    match is redundant or exhaustive respectively.
 
    They print warning messages as appropriate, as described below.
    If the control flag System.Control.MC.printRet is
    set, they print code.
 
    They return code.
 
    They assume that match has one element for each rule of the match 
    to be compiled, in order, plus a single, additional, final element.
    This element must have a pattern that is always matched 
    (in practice, it is either a variable or wildcard), and a
    lambda expression that implements the appropriate behavior 
    for argument values that satisfy none of the guard patterns.
    A pattern is exhaustive if this dummy rule is never used,
    and is irredundant if all of the other rules are used.
  *)   

    (* make various control flags visible *)
    open System.Control.MC 
    
    (* 
      entry point for compiling matches induced by val declarations
      (e.g., val listHead::listTail = list).  match is a two 
      element list.  If the control flag System.Control.MC.bindExhaustive 
      is set, and match is inexhaustive a warning is printed.  If the control
      flag System.Control.MC.bindContainsVar is set, and the first pattern
      (i.e., the only non-dummy pattern) of match contains no variables or 
      wildcards, a warning is printed.    Arguably, a pattern containing no 
      variables, but one or more wildcards, should also trigger a warning, 
      but this would cause warnings on constructions like
      val _ = <exp>  and  val _:<ty> = <exp>.
    *)
    fun bindCompile env (rules, lt, err) =
          let
            val _ = 
              if !printArgs 
                then (say "MC called with:"; MCprint.printMatch env rules)
                else ()
            val (code, _, _, exhaustive) = doMatchCompile(rules,lt)
            val inexhaustiveF = !bindExhaustive andalso not exhaustive
            val noVarsF = !bindContainsVar andalso noVarsIn rules
          in
            if inexhaustiveF
	    then err WARN "binding not exhaustive" (bindPrint(env,rules))
            else if noVarsF
	    then err WARN "binding contains no variables" (bindPrint(env,rules))
	    else ();
            if !printRet then 
              (say "MC:  returns with\n"; MCprint.printLexp code)
            else ();
            code
          end

    (* 
      entry point for compiling matches induced by exception handlers.
      (e.g., handle Bind => Foo).  If the control flag 
      System.Control.MC.matchRedundant is set, and match is redundant, 
      a warning is printed.   
    *)
    fun matchCompileHandler env (rules, lt, err) =
        let val _ = 
              if !printArgs 
              then (say "MC called with: "; MCprint.printMatch env rules)
              else ()
            val (code, unused, redundant, _) = doMatchCompile(rules,lt)
            val  redundantF= !matchRedundant andalso redundant
         in if redundantF 
            then err WARN "redundant patterns in match"
                     (matchPrint(env,rules,unused))
            else ();
            if !printRet 
            then (say "MC:  returns with\n"; MCprint.printLexp code)
            else ();
            code
        end
    
    (* 
      Entry point for compiling matches induced by function expressions
      (and thus case expression, if-then-else expressions, while expressions
      and fun declarations) (e.g., fn (x::y) => ([x],y)). If the control flag 
      System.Control.MC.matchRedundant is set, and match is redundant, 
      a warning is printed.   If the control flag 
      System.Control.MC.matchExhaustive is set, and match is inexhaustive, 
      a warning is printed.   
    *)
    fun matchCompile env (rules, lt, err) =
       let val _ =
	     if !printArgs 
             then (say "MC called with: "; MCprint.printMatch env rules)
             else ()
           val (code, unused, redundant, exhaustive) = doMatchCompile(rules,lt)
           val inexhaustiveF = !matchExhaustive andalso not exhaustive
           val redundantF = !matchRedundant andalso redundant
        in case (inexhaustiveF,redundantF)
	    of (true,true) =>
	         err WARN "match redundant and nonexhaustive"
	           (matchPrint(env,rules,unused))
	     | (true,_) =>
	         err WARN "match nonexhaustive" (matchPrint(env,rules,unused))
	     | (_,true) =>
	         err WARN "match redundant"
                   (matchPrint(env,rules,unused))
             | _ => ();
           if !printRet 
           then (say "MC:  returns with\n"; MCprint.printLexp code)
           else ();
           code
       end

  end (* struct MC *)




