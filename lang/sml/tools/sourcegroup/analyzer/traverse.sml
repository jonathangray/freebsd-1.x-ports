(* Copyright (c) 1992 by Carnegie Mellon University *)

functor TraverseFun (ModuleDecls :MODULE_DECLS) = struct

open System.Ast ModuleDecls
structure MN = ModuleDecls.MN
structure MD = ModuleDecls

val structPath  = MN.structPath 
val sigPath     = MN.sigPath 
val functorPath = MN.functorPath 
val fsigPath    = MN.fsigPath 
val modulePath  = MN.modulePath 

type binder = {name:MN.moduleName, def:modExp, constraint:modExp option}

fun allButLast lst =
  case lst of
     [] => []
   | [last] => []
   | (head::(tail as (_::_))) => (head::(allButLast tail))

fun element'of (element, []) = false
  | element'of (element, (head::tail)) =
      if element = head then true else element'of (element, tail)

fun include'element (element, lst) =
  if element'of (element, lst) then lst else element::lst

fun union (set1, set2) = fold include'element set1 set2

fun modref (path:symbol list, accum) :MN.moduleName list =
  case path of [] => accum
   | (_::_) => include'element (modulePath path, accum)

fun declRef (path:symbol list, accum) :decl list =
  case path of [] => accum
   | (modpath as (_::_)) =>
       let val modName = modulePath modpath in
         case accum of [] => [DeclRef [modName]]
          | ((DeclRef otherRefs)::tail) =>
              (DeclRef (include'element (modName, otherRefs)))::tail
          | _ => (DeclRef [modName])::accum
       end

fun modRefList ([]:MN.moduleName list, accum) :decl list = accum
  | modRefList (modNames, accum) =
 (case accum of [] => [DeclRef modNames]
   | ((DeclRef otherRefs)::tail) =>
       (DeclRef (union (modNames, otherRefs)))::tail
   | _ => (DeclRef modNames)::accum)

fun localDec ((bind, body), accum) :decl list =
 (case (bind,body) of
     ([],[]) => accum
   | ([],[DeclRef names]) => modRefList (names, accum)
   | ([DeclRef names], []) => modRefList (names, accum)
   | ([DeclRef names1], [DeclRef names2]) =>
       modRefList (names1 @ names2, accum)
   | args => (LocalDecl (SeqDecl bind, SeqDecl body))::accum)

exception Compare

fun compare'decs (dec1:dec, dec2:dec) =
  let val pos1 = case dec1 of (MarkDec (_,p,_)) => p | _ => raise Compare
      val pos2 = case dec2 of (MarkDec (_,p,_)) => p | _ => raise Compare
  in
    pos1 <= pos2
  end

fun sort'decs (decList:dec list) :dec list =
  (ListSort.sort compare'decs decList) handle Compare => decList

fun f_dec (ast:dec) :decl =
 (case do_dec (ast, []) of
     [] => (DeclRef [])
   | [decl] => decl
   | declList => (SeqDecl declList)
 )
and do_dec (ast:dec, accum:decl list) :decl list =
 (case ast of
     (ValDec (arg:vb list)) => fold f_vb arg accum
   | (ValrecDec (arg:rvb list)) => fold f_rvb arg accum
   | (FunDec (arg:fb list)) => fold f_fb arg accum
   | (TypeDec (arg:tb list)) => modRefList (fold f_tb arg [], accum)
   | (DatatypeDec {datatycs: db list, withtycs: tb list}) =>
       modRefList (fold f_db datatycs (fold f_tb withtycs []), accum)
   | (AbstypeDec {abstycs: db list, withtycs: tb list, body: dec}) =>
       (* body is syntactically restricted to ldecs, no module scoping here *)
       modRefList (fold f_db abstycs (fold f_tb withtycs []),
                    (f_dec body)::accum)
   | (ExceptionDec (arg:eb list)) => modRefList (fold f_eb arg [], accum)
   | (StrDec (arg:strb list)) => (ModDecl (fold f_strb arg []))::accum
   | (AbsDec (arg:strb list)) => (ModDecl (fold f_strb arg []))::accum
   | (FctDec (arg:fctb list)) => (ModDecl (fold f_fctb arg []))::accum
   | (SigDec (arg:sigb list))   => (ModDecl (fold f_sigb  arg []))::accum
   | (FsigDec (arg:fsigb list)) => (ModDecl (fold f_fsigb arg []))::accum
   | (LocalDec (bindingDec:dec, bodyDec:dec)) =>
       localDec ((do_dec(bindingDec,[]), do_dec(bodyDec,[])), accum)
   | (SeqDec (arg:dec list)) => fold do_dec (sort'decs arg) accum
   | (OpenDec (arg:path list)) => (OpenDecl (map modulePath arg))::accum
   | (OvldDec arg) => accum
   | (FixDec arg) => accum
   | (ImportDec arg) => accum
   | (MarkDec (arg,_,_)) => do_dec(arg,accum)
 )
and f_strb (ast:strb, accum) :binder list =
 (case ast of
     (Strb {name:symbol,def:strexp, constraint:sigexp option}) =>
       ({name=structPath[name], def=f_strexp def,
         constraint=sigexpOpt constraint})::accum
   | (MarkStrb (arg,_,_)) => f_strb (arg, accum)
 )
and f_fctb (ast:fctb, accum) :binder list =
 (case ast of
     (Fctb {name:symbol, def:fctexp}) => 
       let val (def_exp, constrain_exp) = f_fctexp def in
        {name=functorPath[name], def=def_exp, constraint=constrain_exp}::accum
       end
   | (MarkFctb (arg,_,_)) => f_fctb (arg, accum)
 )
and f_sigb (ast:sigb, accum) :binder list =
 (case ast of
     (Sigb {name:symbol, def:sigexp}) =>
       {name=sigPath[name], def=f_sigexp def, constraint=NONE}::accum
   | (MarkSigb (arg,_,_)) => f_sigb (arg, accum)
 )
and f_fsigb (ast:fsigb, accum) :binder list =
 (case ast of
     (Fsigb {name:symbol, def:fsigexp}) =>
       {name=fsigPath[name], def=f_fsigexp def, constraint=NONE}::accum
   | (MarkFsigb (arg,_,_)) => f_fsigb (arg, accum)
 )
and f_strexp (ast:strexp) :modExp =
 (case ast of
     (VarStr (path:path)) => (VarModExp (structPath path))
   | (StructStr (dec:dec)) => (StructModExp (f_dec dec))
   | (AppStr (path:path, argList:(strexp*bool) list)) =>
       (AppModExp (functorPath path, map (fn(se,b)=>(f_strexp se,b)) argList))
   | (LetStr (bindings:dec, body:strexp)) =>
       (LetModExp (f_dec bindings, f_strexp body))
   | (MarkStr (strexp,_,_)) => f_strexp strexp
 )
and f_fctexp (ast:fctexp) :modExp * modExp option=
 (case ast of
     (VarFct (path:path, constraint:fsigexp option)) =>
       (VarModExp (functorPath path), fsigexpOpt constraint)
   | (FctFct {params :(symbol option * sigexp) list, body :strexp,
              constraint :sigexp option}) =>
       (FctModExp {params = map functorParams params, body = f_strexp body},
        sigexpOpt constraint)
   | (AppFct(path:path,argList:(strexp*bool)list,constraint:fsigexp option)) =>
       (AppModExp (functorPath path, map (fn(se,b)=>(f_strexp se,b)) argList),
        fsigexpOpt constraint)
   | (LetFct (bindings:dec, body:fctexp)) =>
       let val (def,constraint) = f_fctexp body in
         (LetModExp (f_dec bindings, def), constraint)
       end
   | (MarkFct (arg,_,_)) => f_fctexp arg
 )
and functorParams (symOpt:symbol option, constraint:sigexp) =
 let val c = f_sigexp constraint in
   case symOpt of
      NONE => (NONE,c)
    | (SOME sym) => (SOME (modulePath[sym]), c)
 end
and sigexpOpt (seOption:sigexp option) :modExp option =
 (case seOption of
     NONE => NONE
   | (SOME sigexp) => (SOME (f_sigexp sigexp))
 )
and f_sigexp (ast:sigexp) :modExp =
 (case ast of
     (VarSig symbol) => (VarModExp (sigPath[symbol]))
   | (SigSig (specList:spec list)) =>
       (StructModExp (SeqDecl (fold f_spec specList [])))
   | (MarkSig (arg,_,_)) => f_sigexp arg
 )
and fsigexpOpt (arg:fsigexp option) :modExp option =
 (case arg of
     NONE => NONE
   | (SOME fsigexp) => (SOME (f_fsigexp fsigexp))
 )
and f_fsigexp (ast:fsigexp) :modExp =
 (case ast of
     (VarFsig symbol) => (VarModExp (fsigPath[symbol]))
   | (FsigFsig {param: (symbol option * sigexp) list, def:sigexp}) =>
       (FctModExp {params = map functorParams param, body = f_sigexp def})
   | (MarkFsig (arg,_,_)) => f_fsigexp arg
 )
and modspec (moduleName:MN.moduleName, expr:modExp) =
  {name=moduleName, def=expr, constraint=(NONE:modExp option)}
and f_spec (ast:spec, accum) :decl list =
 (case ast of
     (StrSpec (arg:(symbol * sigexp) list)) =>
       let val specs = map (fn(sy,se)=>(structPath[sy],f_sigexp se)) arg in
         (ModDecl (map modspec specs))::accum end
   | (TycSpec (arg:(symbol * tyvar list) list, _:bool)) => accum
   | (FctSpec (arg:(symbol * fsigexp) list)) =>
       let val specs = map (fn(sy,fse)=>(functorPath[sy],f_fsigexp fse)) arg in
         (ModDecl (map modspec specs))::accum end
   | (ValSpec (arg:(symbol * ty) list)) =>
       let val mod'ref'list = fold f_ty (map #2 arg) [] in
         modRefList (mod'ref'list, accum) end
   | (DataSpec (arg:db list)) => modRefList (fold f_db arg [], accum)
   | (ExceSpec (arg:(symbol * ty option) list)) =>
       let val mod'ref'list = fold tyoption (map #2 arg) [] in
         modRefList (mod'ref'list, accum) end
   | (FixSpec {fixity: fixity, ops: symbol list}) => accum
   | (ShareSpec (arg:path list)) => accum
   | (ShatycSpec (arg:path list)) => accum
   | (LocalSpec (bind:spec list, body:spec list)) =>
       (case (fold f_spec bind [], fold f_spec body []) of
           ([],[]) => accum
         | ([], [DeclRef names]) => modRefList (names, accum)
         | ([DeclRef names], []) => modRefList (names, accum)
         | ([DeclRef names1], [DeclRef names2]) =>
             modRefList (names1 @ names2, accum)
         | (bind',body') => (LocalDecl (SeqDecl bind', SeqDecl body'))::accum)
   | (IncludeSpec (symbol:symbol)) => (OpenDecl [sigPath[symbol]])::accum
   | (OpenSpec (arg:path list)) => (OpenDecl (map structPath arg))::accum
   | (MarkSpec (arg,_,_)) => f_spec (arg, accum)
 )
and f_vb (ast:vb, accum:decl list) :decl list =
 (case ast of
     (Vb {pat,exp}) => modRefList (f_pat(pat,[]), f_exp(exp,accum))
   | (MarkVb (arg,_,_)) => f_vb (arg, accum)
 )
and f_rvb (ast:rvb, accum) :decl list =
 (case ast of
     (Rvb {var:symbol, exp:exp, resultty: ty option}) =>
       modRefList (tyoption(resultty,[]), f_exp(exp,accum))
   | (MarkRvb (arg,_,_)) => f_rvb (arg, accum)
 )
and f_fb (ast:fb, accum) :decl list =
 (case ast of
     (Fb {var:symbol, clauses:clause list}) => fold f_clause clauses accum
   | (MarkFb (arg,_,_)) => f_fb (arg, accum)
 )
and f_clause (ast:clause, accum) :decl list =
 (case ast of
     (Clause {pats: pat list, resultty: ty option, exp:exp}) =>
       modRefList (fold f_pat pats (tyoption(resultty,[])), f_exp(exp,accum))
 )
and f_tb (ast:tb, accum) :MN.moduleName list =
 (case ast of
     (Tb {tyc:symbol, def:ty, tyvars:tyvar list}) => f_ty (def, accum)
   | (MarkTb (arg,_,_)) => f_tb (arg, accum)
 )
and f_db (ast:db, accum) :MN.moduleName list =
 (case ast of
     (Db {tyc :symbol, tyvars :tyvar list, def :(symbol * ty option) list}) =>
       fold tyoption (map #2 def) accum
   | (MarkDb (arg,_,_)) => f_db(arg, accum)
 )
and f_eb (ast:eb, accum) :MN.moduleName list  =
 (case ast of
     (EbGen {exn: symbol, etype: ty option}) => tyoption (etype, accum)
   | (EbDef {exn: symbol, edef: path}) => modref (allButLast edef, accum)
   | (MarkEb (arg,_,_)) => f_eb (arg, accum)
 )
and f_exp (ast:exp, accum) :decl list =
 (case ast of
     (VarExp (path:path)) => declRef (allButLast path, accum)
   | (FnExp (arg:rule list)) => fold f_rule arg accum
   | (AppExp {function:exp,argument:exp}) =>
       f_exp (function, f_exp (argument, accum))
   | (CaseExp {expr:exp,rules:rule list}) =>
       f_exp (expr, fold f_rule rules accum)
   | (LetExp {dec:dec, expr:exp}) =>
       (* syntactically only ldecs; no module scoping here *)
       localDec ((do_dec(dec,[]), f_exp(expr,[])), accum)
   | (SeqExp (arg:exp list)) => fold f_exp arg accum
   | (IntExp arg) => accum
   | (RealExp arg) => accum
   | (StringExp arg) => accum
   | (RecordExp (arg:(symbol * exp) list)) => fold f_exp (map #2 arg) accum
   | (TupleExp (arg:exp list)) => fold f_exp arg accum
   | (SelectorExp symbol) => accum
   | (ConstraintExp {expr:exp,constraint:ty}) =>
       f_exp (expr, modRefList(f_ty(constraint,[]),accum))
   | (HandleExp {expr:exp, rules:rule list}) =>
       f_exp (expr, fold f_rule rules accum)
   | (RaiseExp expr) => f_exp (expr, accum)
   | (IfExp {test:exp, thenCase:exp, elseCase:exp}) =>
       f_exp (test, f_exp (thenCase, f_exp (elseCase, accum)))
   | (AndalsoExp (expr1,expr2)) => f_exp (expr1, f_exp (expr2, accum))
   | (OrelseExp (expr1,expr2)) => f_exp (expr1, f_exp (expr2, accum))
   | (WhileExp {test:exp,expr:exp}) => f_exp (test, f_exp (expr, accum))
   | (MarkExp (arg,_,_)) => f_exp (arg, accum)
   | (VectorExp (arg:exp list)) => fold f_exp arg accum
 )
and f_rule (ast:rule, accum) :decl list =
 (case ast of
     (Rule {pat,exp}) => modRefList (f_pat(pat,[]), f_exp(exp,accum))
 )
and f_pat (ast:pat, accum) :MN.moduleName list =
 (case ast of
     (WildPat) => accum
   | (VarPat path) => modref (allButLast path, accum)
   | (IntPat arg) => accum
   | (RealPat arg) => accum
   | (StringPat arg) => accum
   | (RecordPat {def:(symbol*pat)list,...}) => fold f_pat (map #2 def) accum
   | (TuplePat (arg:pat list)) => fold f_pat arg accum
   | (AppPat {constr:path,argument:pat}) =>
       (modref (allButLast constr, f_pat (argument, accum)))
   | (ConstraintPat {pattern:pat,constraint:ty}) =>
       f_pat (pattern, f_ty (constraint, accum))
   | (LayeredPat {varPat:pat,expPat:pat}) => f_pat (varPat,f_pat(expPat,accum))
   | (VectorPat (arg:pat list)) => fold f_pat arg accum
   | (MarkPat (arg,_,_)) => f_pat (arg, accum)
 )
and f_ty (ast:ty, accum) :MN.moduleName list =
 (case ast of
     (VarTy arg) => accum
   | (ConTy (consName:symbol list, args:ty list)) =>
       modref (allButLast consName, fold f_ty args accum)
   | (RecordTy (arg:(symbol*ty) list)) => fold f_ty (map #2 arg) accum
   | (TupleTy arg) => fold f_ty arg accum
   | (MarkTy (arg,_,_)) => f_ty (arg, accum)
 )
and tyoption (arg:ty option, accum) :MN.moduleName list =
 (case arg of NONE => accum | (SOME ty) => f_ty (ty, accum))

fun traverse (ast:dec) = f_dec ast

end
