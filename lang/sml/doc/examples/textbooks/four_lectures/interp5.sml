(* interp5.sml : the complete type checker and evaluator
                 based on version 2 *)

signature INTERPRETER=
   sig
      val interpret: string -> string
      val eval: bool ref
      and tc  : bool ref
   end;

                  (* syntax *)

signature EXPRESSION =
   sig
      datatype Expression =
         SUMexpr of Expression * Expression   |
         DIFFexpr of Expression * Expression   |
         PRODexpr of Expression * Expression   |
         BOOLexpr of bool   |
         EQexpr of Expression * Expression   |
         CONDexpr of Expression * Expression * Expression   |
         CONSexpr of Expression * Expression   |
         LISTexpr of Expression list   |
         DECLexpr of string * Expression * Expression   |
         RECDECLexpr of string * Expression * Expression   |
         IDENTexpr of string   |
         LAMBDAexpr of string * Expression   |
         APPLexpr of Expression * Expression   |
         NUMBERexpr of int


      val prExp: int -> Expression -> string
   end;


              (* parsing *)

signature PARSER =
   sig
      structure E: sig type Expression end

      exception Lexical of string
      exception Syntax of string

      val parse: string -> E.Expression
   end


                        (* environments *)

signature ENVIRONMENT =
   sig
      type 'object Environment

      exception Retrieve of string

      val emptyEnv: 'object Environment
      val declare: string * 'object * 'object Environment -> 'object Environment
      val retrieve: string * 'object Environment -> 'object
      val fold: (('object * 'result) -> 'result) -> 'result -> 
                   'object Environment -> 'result
      val map: ('object -> 'newobject) -> 'object Environment -> 
                 'newobject Environment
  
      val plus: 'obj Environment * 'obj Environment -> 'obj Environment

   end;

                        (* evaluation *)
signature VALUE =
   sig
      structure Environment : sig type 'a Environment end
      type Value and Env and Exp
      exception Value

      val mkValueNumber: int -> Value
          and unValueNumber: Value -> int

      val mkValueBool: bool -> Value
          and unValueBool: Value -> bool

      val ValueNil: Value
      val mkValueCons: Value * Value -> Value
          and unValueHead: Value -> Value
          and unValueTail: Value -> Value

      val mkValueClos: string * Exp * Env * Env -> Value
          and unValueClos: Value -> string * Exp * Env * Env

      val mkEnv: Value Environment.Environment -> Env
          and unEnv: Env -> Value Environment.Environment

      val Rec: Env -> Env

      exception EqValue
      val eqValue: Value * Value -> bool
      val printValue: Value -> string
   end;


signature EVALUATOR =
   sig
      structure Exp: sig type Expression end
      structure Val: sig type Exp and Value end
            sharing type Val.Exp = Exp.Expression
      exception Unimplemented
      exception RuntimeError of string
      val evaluate: Exp.Expression -> Val.Value
   end;

                  (* type checking *)
signature TYPE =
   sig
      eqtype tyvar
      val freshTyvar: unit -> tyvar
      type Type 
      type TypeScheme
  
      val tyvarsTy: Type -> tyvar list
      and tyvarsTySch: TypeScheme -> tyvar list

      val instance: TypeScheme -> Type

	(*constructors and decstructors*)
      exception Type
      val mkTypeInt: unit -> Type
          and unTypeInt: Type -> unit

      val mkTypeBool: unit -> Type
          and unTypeBool: Type -> unit

      val mkTypeList: Type -> Type
          and unTypeList: Type -> Type

      val mkTypeArrow:  Type * Type -> Type
          and unTypeArrow: Type -> Type * Type

      val mkTypeTyvar: tyvar -> Type
          and unTypeTyvar: Type -> tyvar

      val mkTypeScheme: tyvar list * Type -> TypeScheme
          and unTypeScheme: TypeScheme -> tyvar list * Type

      type subst
      val Id: subst                     (* the identify substitution;   *)
      val mkSubst: tyvar*Type -> subst 	(* make singleton substitution; *)
      val on : subst * Type -> Type     (* application;                 *)
      val onScheme: subst * TypeScheme -> TypeScheme
	
      val oo : subst * subst -> subst   (* composition *)

      val prType: Type->string          (* printing *)
   end

signature TYPEENV=
 sig
  structure Type : sig type Type and TypeScheme and subst end
  type typeenv
  exception Retrieve of string
  val emptyEnv: typeenv
  val declare: string * Type.TypeScheme * typeenv -> typeenv
  val retrieve: string * typeenv -> Type.TypeScheme
  val close: typeenv * Type.Type -> Type.TypeScheme
  val onTE: Type.subst * typeenv -> typeenv
 end
  


signature TYPECHECKER =
   sig
      structure Exp: sig type Expression end
      structure Type: sig type Type end
      exception NotImplemented of string
      val typecheck: Exp.Expression -> Type.Type * bool
   end;

                  (* the interpreter*)
functor Interpreter
   (structure Ty: TYPE
    structure Value : VALUE
    structure Parser: PARSER
    structure TyCh: TYPECHECKER
    structure Evaluator:EVALUATOR
      sharing Parser.E = TyCh.Exp = Evaluator.Exp 
          and type Value.Exp = Parser.E.Expression
          and TyCh.Type = Ty
          and Evaluator.Val = Value
   ): INTERPRETER=

struct
  val eval= ref true    (* toggle for evaluation *)
  and tc  = ref true    (* toggle for type checking *)
  fun interpret'(str)=
    let val abstsyn= Parser.parse str
        val (typestr,ok)= 
                     if !tc then 
                       let val (ty, ok) = TyCh.typecheck abstsyn
                        in (Ty.prType(ty),ok)
                       end
                     else ("(disabled)",false)
        val valuestr= if !eval andalso ok then 
                         Value.printValue(Evaluator.evaluate abstsyn)
                      else "(disabled)"
             
    in  valuestr ^ " : " ^ typestr 
    end
    handle Evaluator.Unimplemented => "Evaluator not fully implemented"
         | TyCh.NotImplemented msg => "Type Checker not fully implemented " ^ msg
         | Value.Value   => "Run-time error"
         | Evaluator.RuntimeError msg => "Run-time error: " ^ msg
         | Parser.Syntax msg => "Syntax Error: " ^ msg
         | Parser.Lexical msg=> "Lexical Error: " ^ msg

  fun interpret(str) = 
     let val separator = "\n--------------------------------\n"
         val result = interpret' str
      in output(std_out, separator);
         output(std_out, result);
         output(std_out, separator ^ "\n");
         ""
     end
end;

                    (* the evaluator *)
functor Evaluator
  (structure Expression: EXPRESSION
   structure Env: ENVIRONMENT
   structure Value: VALUE
             sharing type Value.Exp = Expression.Expression
             sharing Value.Environment = Env
  ):EVALUATOR=

   struct
      structure Exp= Expression
      structure Val= Value
      type Env = Val.Value Env.Environment

      exception Unimplemented
      exception RuntimeError of string
      local
         open Expression Value
         fun evaluate(E, exp) =
            case exp
              of BOOLexpr b => mkValueBool b
               | NUMBERexpr i => mkValueNumber i
               | SUMexpr(e1, e2) =>
                    let val e1' = evaluate(E, e1)
                        val e2' = evaluate(E, e2)
                    in
                       mkValueNumber(unValueNumber e1' + unValueNumber e2')
                    end

               | DIFFexpr(e1, e2) =>
                    let val e1' = evaluate(E, e1)
                        val e2' = evaluate(E, e2)
                    in
                       mkValueNumber(unValueNumber e1' - unValueNumber e2')
                    end

               | PRODexpr(e1, e2) =>
                    let val e1' = evaluate(E, e1)
                        val e2' = evaluate(E, e2)
                    in
                       mkValueNumber(unValueNumber e1' * unValueNumber e2')
                    end

               | EQexpr(e1,e2)=> 
                    let val v1 = evaluate(E,e1)
                        val v2 = evaluate(E,e2)
                     in mkValueBool(eqValue(v1,v2))
                    end
               | CONDexpr(e1,e2,e3)=> 
                    let val v1 = evaluate(E, e1)
                     in if eqValue(v1,mkValueBool true) then evaluate(E,e2)
                        else evaluate(E, e3)
                    end
               | CONSexpr(e1, e2) =>
                    let val v1 = evaluate(E, e1)
                        val v2 = evaluate(E, e2)
                     in mkValueCons(v1,v2)
                    end
               | LISTexpr [] => ValueNil
               | LISTexpr (hd::tl)=> 
                    evaluate(E, CONSexpr(hd, LISTexpr tl))
               | DECLexpr(id,e1,e2) => 
                    let val v1 = evaluate(E,e1)
                        val E' = Env.declare(id,v1,E)
                     in evaluate(E', e2)
                    end
               | RECDECLexpr(f,e1,e2) => 
                    let val v1 = evaluate(E, e1)
                        val ? = unValueClos v1
                                handle Value=> raise RuntimeError(
                           "recursively  defined value is not a function")
                        val Env0 = mkEnv(Env.declare(f,v1,Env.emptyEnv))
                        val recE0 = Rec Env0
                        val newE = Env.plus(E, unEnv recE0)
                     in evaluate(newE,e2)
                    end

               | IDENTexpr id=> 
                    Env.retrieve(id,E)
               | APPLexpr(e1,e2)=> 
                   let val v1 = evaluate(E,e1)
                       val v2 = evaluate(E,e2)
                       val (id',exp',Env',Env'')= unValueClos v1
                       val recE'= Env.plus(unEnv Env', unEnv(Rec Env''))
                    in evaluate(Env.declare(id',v2,recE'), exp')
                   end
               | LAMBDAexpr(x,e) => 
                   mkValueClos(x,e,mkEnv E, mkEnv Env.emptyEnv)
                   


      in
         val evaluate = fn(e) => evaluate(Env.emptyEnv,e)
      end
   end;

                        (* the type checker *)   
signature UNIFY=
   sig
      structure Type: sig type Type and subst end
      exception NotImplemented of string
      exception Unify
      val unify: Type.Type * Type.Type -> Type.subst
   end;

functor TypeCheckerRecovery(structure Ex: EXPRESSION
                            structure Ty: TYPE
                            structure List: LISTUTIL): 
  sig 
   val report: Ex.Expression * int * Ty.subst * string list ->
               Ty.subst * Ty.Type * bool
  end=

struct
  exception Recovery of int
  val messages= [
(1, fn[t2]=> 
"expected the second operand to cons to be of list type" ^
"\n   found :   " ^  t2
|        _ => raise Recovery 1),

(2, fn[t1,t2]=>
"the type of the first list element differs from the type of the others " ^
"\n   first element  :   " ^  t1 ^
"\n   other elements :   " ^  t2
|        _ => raise Recovery 2),

(3, fn[t1,t2]=>
"left and right hand sides of = have different types" ^
"\n  left-hand side  :  " ^  t1 ^
"\n  right-hand side :  " ^  t2
|        _ => raise Recovery 3),

(4, fn[t1]=>
"expected boolean expression between `if' and `then';" ^
"\n  found:  " ^  t1
|        _ => raise Recovery 4),

(5, fn[t2,t3]=>
"`then' and `else' branches have different types" ^
"\n  `then' branch :  " ^  t2 ^
"\n  `else' branch :  " ^  t3
|        _ => raise Recovery 5),

(6, fn[t1,t2]=>
"the domain type of the function differs from the type of the argument " ^
"\nto which it is applied" ^
"\n  function domain type : " ^  t1 ^
"\n  argument        type : " ^  t2
|        _ => raise Recovery 6),

(7, fn[t1]=>
"I expected this expression, which is an argument " ^
"\nto a numeric operator, to have type int; but I " ^
"\nfound : " ^  t1
|        _ => raise Recovery 7),

(8, fn [x] =>
"the identifier " ^ x ^ " has not been declared"
|        _ => raise Recovery 8),

(9, fn [t] =>
"although the above expression occurs in " ^
"\napplication position, I have found it to " ^
"\nhave type :  " ^ t
|        _ => raise Recovery 9)



]

  fun report(exp, i, S, stringlist) =
      let val msgf = List.lookup messages i
                    handle List.Lookup => raise Recovery(i)
          val sep = "\n----------------\n"
          val freshty = Ty.mkTypeTyvar (Ty.freshTyvar())
          val msg   = "Type Error in expression:\n   " ^ Ex.prExp 60 exp ^
                      "\nClue: " ^  msgf stringlist ^ "\n"

       in 
          output(std_out, sep);
          output(std_out, msg);
          (S,freshty,false)
      end
end;



functor TypeChecker
  (structure Ex: EXPRESSION
   structure Ty: TYPE
   structure TyEnv: TYPEENV
   structure Unify: UNIFY 
      sharing Unify.Type = Ty = TyEnv.Type
   structure List: LISTUTIL
  ): TYPECHECKER=
struct
  infixr on         val (op on) = Ty.on
  infixr onscheme   val op onscheme = Ty.onScheme
  infixr onTE       val op onTE = TyEnv.onTE
  infixr oo         val op oo = Ty.oo

  structure Exp = Ex
  structure Type = Ty
  structure Recovery= TypeCheckerRecovery(
    structure Ex = Ex
    structure Ty = Ty
    structure List = List)

  exception NotImplemented of string
  exception Recover of Ex.Expression * int * Ty.subst * string list ;

  fun tc (TE: TyEnv.typeenv, exp: Ex.Expression): 
        Ty.subst *Ty.Type * bool =
   (case exp of
      Ex.BOOLexpr b => (Ty.Id,Ty.mkTypeBool(),true)
    | Ex.NUMBERexpr _ => (Ty.Id,Ty.mkTypeInt(),true)
    | Ex.SUMexpr(e1,e2)  => checkIntBin(TE,e1,e2)
    | Ex.DIFFexpr(e1,e2) => checkIntBin(TE,e1,e2)
    | Ex.PRODexpr(e1,e2) => checkIntBin(TE,e1,e2)
    | Ex.LISTexpr [] =>
         let val new = Ty.freshTyvar ()
          in (Ty.Id,Ty.mkTypeList(Ty.mkTypeTyvar  new),true)
         end
    | Ex.LISTexpr(e::es) => tc (TE, Ex.CONSexpr(e,Ex.LISTexpr es))
    | Ex.CONSexpr(e1,e2) =>
       (let val (S1,t1,ok1) = tc(TE, e1)
            val (S2,t2,ok2) = tc(S1 onTE TE, e2)
            val new = Ty.freshTyvar ()
            val newt= Ty.mkTypeTyvar new
            val t2' = Ty.mkTypeList newt
            val S3 = Unify.unify(t2, t2')
                     handle Unify.Unify=> 
                     raise Recover(e2, 1, (S2 oo S1), [Ty.prType t2])
            val S4 = Unify.unify(S3 on newt,S3 oo S2 on t1)
                     handle Unify.Unify=>
                     raise Recover(exp, 2, (S3 oo S2 oo S1), 
                       [Ty.prType (S3 oo S2 on t1), Ty.prType (S3 on newt)])
         in (S4 oo S3 oo S2 oo S1, S4 oo S3 on t2, ok1 andalso ok2)
        end handle Recover q => Recovery.report q)
    | Ex.EQexpr(e1,e2)=> 
       (let val (S1,t1,ok1) = tc(TE,e1)
            val (S2,t2,ok2) = tc(S1 onTE TE, e2)
            val S3 = Unify.unify(S2 on t1, t2)
                     handle Unify.Unify=>
                     raise Recover(exp, 3, (S2 oo S1), [Ty.prType (S2 on t1), 
                          Ty.prType t2])
         in (S3 oo S2 oo S1, Ty.mkTypeBool(), ok1 andalso ok2)
        end  handle Recover q=> Recovery.report q)
    | Ex.CONDexpr(b,e1,e2)=> 
        (let val (S1,t1,ok1) = tc(TE,b)
             val S1' = Unify.unify(t1,Ty.mkTypeBool())
                       handle Unify.Unify=>
                       raise Recover(exp, 4, S1, [Ty.prType t1])
             val (S2,t2,ok2) = tc(S1 onTE TE, e1)
             val (S3,t3,ok3) = tc(S2 oo S1 onTE TE, e2)
             val S3' = Unify.unify(S3 on t2,t3)
                       handle Unify.Unify=>
                       raise Recover(exp, 5, (S3 oo S2 oo S1' oo S1), 
                       [Ty.prType (S3 on t2), Ty.prType t3])
          in (S3' oo S3 oo S2 oo S1' oo S1, 
              S3' oo S3 on t2,
              ok1 andalso ok2 andalso ok3)
         end handle Recover q=> Recovery.report q)
    | Ex.DECLexpr(x,e1,e2) => 
         let val (S1,t1,ok1) = tc(TE,e1);
             val typeScheme = TyEnv.close(S1 onTE TE,t1)
             val (S2,t2,ok2) = tc(TyEnv.declare(x,typeScheme,TE), e2)
          in (S2 oo S1, t2, ok1 andalso ok2)
         end
    | Ex.RECDECLexpr(fid,e1,e2)=>
         let val new = Ty.mkTypeScheme([],
                         Ty.mkTypeTyvar(Ty.freshTyvar()))
             val TE' = TyEnv.declare(fid,new,TE)
             val (S1,t1, ok1) = tc(TE',e1)
             val (S2,t2, ok2) = tc(S1 onTE TE', e2)
          in
             (S2 oo S1, t2, ok1 andalso ok2)
         end
    | Ex.IDENTexpr x   => 
         ((Ty.Id, Ty.instance(TyEnv.retrieve(x,TE)), true)
         handle TyEnv.Retrieve _=> 
          Recovery.report(exp,8,Ty.Id,[x]))
    | Ex.LAMBDAexpr(x,e)=>
         let val newty = Ty.mkTypeTyvar(Ty.freshTyvar())
             val new_scheme = Ty.mkTypeScheme([], newty)
             val TE' = TyEnv.declare(x,new_scheme,TE)
             val (S1,t1,ok1) = tc(TE', e)
          in (S1, Ty.mkTypeArrow(S1 on newty,t1), ok1)
         end

    | Ex.APPLexpr(e1,e2)=>
        (let val (S1,t1,ok1) = tc(TE, e1)
             val new =  Ty.mkTypeTyvar(Ty.freshTyvar())
             val new' = Ty.mkTypeTyvar(Ty.freshTyvar())
             val arrow1=Ty.mkTypeArrow(new,new')
             val S1' = Unify.unify(arrow1,t1)
                 handle Unify.Unify=>
                 raise Recover(e1,9,S1,[Ty.prType t1])
             val (S2,t2,ok2) = tc((S1' oo S1) onTE TE, e2)
             val new2 = Ty.mkTypeTyvar(Ty.freshTyvar())
             val arrow2 = Ty.mkTypeArrow(t2,new2) 
             val S3 = Unify.unify(arrow2, (S2 oo S1') on t1)
                 handle Unify.Unify=> 
                 raise Recover(exp, 6, (S2 oo S1' oo S1 ), 
                    [Ty.prType ((S2 oo S1') on new),Ty.prType  t2])
          in (S3 oo S2 oo S1' oo S1,
              S3 on new2, ok1 andalso ok2)
         end  handle Recover q=> Recovery.report q)

   )handle Unify.NotImplemented msg => raise NotImplemented msg
       
  and checkIntBin(TE,e1,e2) =
   (let val (S1,t1,ok1) = tc(TE,e1)
        val S1'  = Unify.unify(t1, Ty.mkTypeInt())
                 handle Unify.Unify=> 
                 raise Recover(e1, 7, S1, [Ty.prType t1])
        val (S2,t2,ok2) = tc((S1' oo S1) onTE TE,e2)
        val S2' =  Unify.unify(t2, Ty.mkTypeInt())
                   handle Unify.Unify=> 
                   raise Recover(e2, 7, (S2 oo S1' oo S1), [Ty.prType t2])
     in (S2' oo S2 oo S1' oo S1, Ty.mkTypeInt(), ok1 andalso ok2)
    end handle Recover q=> Recovery.report q);
 
  fun typecheck(e) = let val (_,ty,ok) =
                          tc(TyEnv.emptyEnv,e)
                      in (ty,ok)
                     end

end; (*TypeChecker*)


functor Unify(Ty:TYPE):UNIFY=
struct
   structure Type = Ty
   exception NotImplemented of string
   exception Unify
 
   infix on 
   val op on = Ty.on
   infix oo
   val op oo = Ty.oo
   fun occurs(tv:Ty.tyvar,t:Ty.Type):bool=
     (Ty.unTypeInt t; false)              handle Ty.Type=>
     (Ty.unTypeBool t; false)             handle Ty.Type=>
     let val tv' = Ty.unTypeTyvar t
     in  tv=tv'
     end                                  handle Ty.Type=>
     let val t'  = Ty.unTypeList t
     in  occurs(tv,t')
     end                                  handle Ty.Type=>
     let val (t1,t2)= Ty.unTypeArrow t
     in occurs(tv, t1) orelse occurs(tv, t2)
     end                                  handle Ty.Type=>
   raise NotImplemented "(the occur check)"


   fun unify(t,t')=
   let val tv = Ty.unTypeTyvar t
    in let val tv' = Ty.unTypeTyvar t'
        in Ty.mkSubst(tv,t')
       end                            	handle Ty.Type=>
       if occurs(tv,t') then raise Unify
       else Ty.mkSubst(tv,t')
   end                                  handle Ty.Type=>
   let val tv' = Ty.unTypeTyvar t'
    in if occurs(tv',t) then raise Unify
       else Ty.mkSubst(tv',t)
   end                   		handle Ty.Type=>
   let val ? = Ty.unTypeInt t
    in let val ? = Ty.unTypeInt t'
        in Ty.Id
       end handle Ty.Type=> raise Unify
   end					handle Ty.Type =>
   let val ? = Ty.unTypeBool t
    in let val ? = Ty.unTypeBool t'
        in Ty.Id
       end handle Ty.Type=> raise Unify
   end					handle Ty.Type=>
   let val t = Ty.unTypeList t
    in let val t' = Ty.unTypeList t'
        in unify(t,t')
       end handle Ty.Type => raise Unify
   end 					handle Ty.Type=>
   let val (t1,t2)= Ty.unTypeArrow(t)
    in let val (t1',t2') = Ty.unTypeArrow(t')
        in let val S1 = unify(t1,t1')
               val S2 = unify(S1 on t2, S1 on t2')
            in S2 oo S1
           end
       end handle Ty.Type => raise Unify
   end 					handle Ty.Type=>
   raise NotImplemented "(unify)"     

end; (*Unify*)
  
                     (* the basics -- nullary functors *)


functor Type(structure List:LISTUTIL
             structure Print: PRINTUTIL) :TYPE =
struct
  type tyvar = int
  val freshTyvar =
      let val r= ref 0 in fn()=>(r:= !r +1; !r) end
  datatype Type = INT
                | BOOL
                | LIST of Type
                | ARROW of Type * Type 
                | TYVAR of tyvar  

  datatype TypeScheme = FORALL of tyvar list * Type

  fun tyvarsTy INT = []
    | tyvarsTy BOOL = []
    | tyvarsTy (LIST ty) = tyvarsTy ty
    | tyvarsTy (ARROW(ty,ty')) = List.union(tyvarsTy ty, tyvarsTy ty')
    | tyvarsTy (TYVAR tyvar) = [tyvar];

  fun tyvarsTySch(FORALL(tyvarlist, ty))= List.minus(tyvarsTy ty, tyvarlist)


  fun instance(FORALL(tyvars,ty))=
  let val old_to_new_tyvars = map (fn tv=>(tv,freshTyvar())) tyvars
      exception Find;
      fun find(tv,[]:(tyvar*tyvar)list)= raise Find
      |   find(tv,(tv',new_tv)::rest)=
          if tv=tv' then new_tv else find(tv,rest)
      fun ty_instance INT = INT
      |   ty_instance BOOL = BOOL
      |   ty_instance (LIST t) = LIST(ty_instance t)
      |   ty_instance (ARROW(t,t'))= ARROW(ty_instance t, ty_instance t')
      |   ty_instance (TYVAR tv) = 
             TYVAR(find(tv,old_to_new_tyvars)
                   handle Find=> tv)

  in 
     ty_instance ty
  end
             

  exception Type

  fun mkTypeInt() = INT
  and unTypeInt(INT)=()
    | unTypeInt(_)= raise Type

  fun mkTypeBool() = BOOL
  and unTypeBool(BOOL)=()
    | unTypeBool(_)= raise Type

  fun mkTypeList(t)=LIST t
  and unTypeList(LIST t)= t
    | unTypeList(_)= raise Type

  fun mkTypeArrow(t,t')= ARROW(t,t')
  and unTypeArrow(ARROW(t,t'))= (t,t')
    | unTypeArrow(_) = raise Type

  fun mkTypeTyvar tv = TYVAR tv
  and unTypeTyvar(TYVAR tv) = tv
    | unTypeTyvar _ = raise Type
  
  fun mkTypeScheme(l,ty)= FORALL(l,ty)
  and unTypeScheme(FORALL(l,ty))= (l,ty)

  type subst = Type -> Type

  fun Id x = x
  fun mkSubst(tv,ty)=
     let fun su(TYVAR tv')= if tv=tv' then ty else TYVAR tv'
         |   su(INT) = INT
         |   su(BOOL)= BOOL
         |   su(LIST ty') = LIST (su ty')
         |   su(ARROW (ty,ty'))= ARROW(su ty, su ty')
      in su
     end


  fun on(S,t)= S(t)
  infixr on
  fun onScheme(S,FORALL(bounds,ty)) = 
      let val fv = tyvarsTy ty
          val fvrange= 
           List.fold(fn (tv,res)=>
                      List.union(tyvarsTy(S(TYVAR tv)),res))
                    []
                    fv
          val criticals= List.intersect(bounds, fvrange)
          val criticals'= map (freshTyvar o (fn _=>())) 
                          criticals
          val renlist = List.zip(criticals,criticals')
          fun renaming(TYVAR tv) =TYVAR(List.lookup renlist tv
                                        handle List.Lookup=>tv)
            | renaming(INT)=INT
            | renaming(BOOL)=BOOL
            | renaming(LIST ty)= (LIST (renaming ty))
            | renaming(ARROW(ty,ty'))= ARROW(renaming ty, renaming ty')

          val bounds'= List.map (fn tv=> List.lookup renlist tv
                                         handle List.Lookup => tv)
                                bounds
                            
          val ty'= S on renaming on ty
       in FORALL(bounds',ty')
      end

  val oo = op o; (* composition of substitutions is just
                    function composision *)



  fun prType INT = "int"
  |   prType BOOL= "bool"
  |   prType (LIST ty) = "(" ^ prType ty ^ ")list"
  |   prType (ARROW(ty,ty'))= "(" ^ prType ty ^ "->" ^ prType ty' ^ ")"
  |   prType (TYVAR tv) = "a" ^ Print.intToString tv
end;

functor Environment():ENVIRONMENT =
struct
   type 'a Environment = (string *  'a )list

   exception Retrieve of string;
   
   val emptyEnv = [];

   fun declare(s:string,obj:'a,e:'a Environment)=
       (s,obj)::e

   fun retrieve(s,[])= raise Retrieve(s)
   |   retrieve(s,(s',obj)::rest) =
           if s=s' then obj else retrieve(s,rest)

   fun map f [] = []
     | map f ((hd as (key,obj))::tl)= (key, f(obj)) :: map f tl

   fun fold f r [] = r
     | fold f r ((hd as (key,obj))::tl)= f(obj,fold f r tl)

   fun plus(E1,E2) = E2 @ E1


end;


functor TypeEnv(structure Type: TYPE 
                structure E: ENVIRONMENT 
                structure List: LISTUTIL): TYPEENV=
struct
  structure Type = Type
  structure E = Environment()
  open E
  type typeenv = Type.TypeScheme Environment
  
  fun close(TE, ty)= 
      let fun f(tyscheme, tyvars)= List.union(Type.tyvarsTySch tyscheme,
                                              tyvars)
          val tyvarsTE = E.fold f [] TE
          val bound = List.minus(Type.tyvarsTy ty, tyvarsTE)
       in Type.mkTypeScheme(bound,ty)
      end;
 
  fun onTE(S,TE)=
      E.map(fn(scheme)=> Type.onScheme(S,scheme)) TE
end;

functor Expression(structure List: LISTUTIL
                   structure Print: PRINTUTIL): EXPRESSION =
   struct
      type 'a pair = 'a * 'a

      datatype Expression =
         SUMexpr of Expression pair   |
         DIFFexpr of Expression pair   |
         PRODexpr of Expression pair   |
         BOOLexpr of bool   |
         EQexpr of Expression pair   |
         CONDexpr of Expression * Expression * Expression   |
         CONSexpr of Expression pair   |
         LISTexpr of Expression list   |
         DECLexpr of string * Expression * Expression   |
         RECDECLexpr of string * Expression * Expression   |
         IDENTexpr of string   |
         LAMBDAexpr of string * Expression   |
         APPLexpr of Expression * Expression   |
         NUMBERexpr of int

      fun pr(SUMexpr p) = printPair "+" p
        | pr(DIFFexpr p) = printPair "-" p
        | pr(PRODexpr p) = printPair "*" p
        | pr(BOOLexpr true) = " true"
        | pr(BOOLexpr false) = " false"
        | pr(EQexpr p) = printPair "=" p
        | pr(CONDexpr(e1,e2,e3))=
           " if" ^ pr(e1) ^ " then" ^ pr(e2) ^
           " else" ^ pr(e3)
        | pr(CONSexpr p) = printPair "::" p
        | pr(LISTexpr l) = prList l
        | pr(DECLexpr(f,e1,e2))=
           " let " ^ f ^ "=" ^ pr(e1) ^
           " in" ^ pr e2 ^ " end"
        | pr(RECDECLexpr(f,e1,e2))=
           " let rec " ^ f ^ "=" ^ pr(e1) ^
           " in" ^ pr e2 ^ " end"
        | pr(IDENTexpr f)= " " ^ f
        | pr(LAMBDAexpr(x,e))= " fn " ^ x ^ "=>" ^ pr(e)
        | pr(APPLexpr(e1,e2))= pr e1 ^ pr e2
        | pr(NUMBERexpr i)= " " ^ Print.intToString i
      and printPair operator (e1,e2) = pr e1 ^ " " ^ operator ^
            pr e2
      and prList l = "[" ^ prList' l ^ "]"
      and prList' [] = ""
        | prList' [e] = pr e
        | prList'(hd::tl)= pr hd ^ "," ^ prList' tl


      fun prExp n e =
          let val s = pr e
              val Size = size s
           in if Size <= n then s
              else
                 let val slist = explode s
                     val half = (n-3)div 2
                     val initial = List.prefix(slist,half)
                     val final = rev(List.prefix(rev slist,half))
                  in implode(initial @ (explode "...") @ final)
                 end
          end
   end;


functor Value(structure Env: ENVIRONMENT
              structure Exp : EXPRESSION
              structure Print: PRINTUTIL): VALUE =
   struct
      type 'a pair = 'a * 'a

      type Exp = Exp.Expression
      structure Environment= Env

      datatype Value = NUMBERvalue of int   |
                       BOOLvalue of bool   |
                       NILvalue   |
                       CONSvalue of Value pair |
		       CLOS of string
                            *  Exp
                            *  Env * Env
           and Env   = ENV of Value Env.Environment

      exception Value

      val mkValueNumber = NUMBERvalue
      val mkValueBool = BOOLvalue

      val ValueNil = NILvalue
      val mkValueCons = CONSvalue

      val mkValueClos = CLOS
      val mkEnv       = ENV


      fun unValueNumber(NUMBERvalue(i)) = i   |
          unValueNumber(_) = raise Value

      fun unValueBool(BOOLvalue(b)) = b   |
          unValueBool(_) = raise Value

      fun unValueHead(CONSvalue(c, _)) = c   |
          unValueHead(_) = raise Value

      fun unValueTail(CONSvalue(_, c)) = c   |
          unValueTail(_) = raise Value

      fun unValueClos(CLOS q) = q |
          unValueClos(_) = raise Value

      fun unEnv(ENV e) = e
        
     
      exception EqValue

      fun eqValue(NUMBERvalue v,NUMBERvalue v') = v=v' |
          eqValue(BOOLvalue v, BOOLvalue v') = v=v' |
          eqValue(NILvalue,NILvalue) = true |
          eqValue(CONSvalue(v1,v2),CONSvalue(v1',v2'))= 
             eqValue(v1,v1') andalso eqValue(v2,v2') |
          eqValue(CLOS _, _) = raise EqValue |
          eqValue(_, CLOS _) = raise EqValue |
          eqValue (_,_) = false
          

          
                 

      (* unfolding of environments for recursion*)

      fun Rec(E as (ENV env))=
          let fun unfold(CLOS(id',exp',E',E'')) = CLOS(id',exp',E',E)
                | unfold (v) = v
           in ENV(Env.map unfold env)
          end
          


				(* Pretty-printing *)
      fun printValue(NUMBERvalue(i)) = " " ^ Print.intToString(i)   |
          printValue(BOOLvalue(true)) = " true"   |
          printValue(BOOLvalue(false)) = " false"   |
          printValue(NILvalue) = "[]"   |
          printValue(CONSvalue(cons)) = "[" ^ printValueList(cons) ^ "]" |
          printValue(CLOS(id,_,_,_)) = "<" ^ id ^ ",_,_,_>"
          
          and printValueList(hd, NILvalue) = printValue(hd)   |
              printValueList(hd, CONSvalue(tl)) =
                 printValue(hd) ^ ", " ^ printValueList(tl)   |
              printValueList(_) = raise Value
          
   end;


