(* interp4.sml : adding identifiers and let 
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
   end


              (* parsing *)

signature PARSER =
   sig
      structure E: EXPRESSION

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
   end

                        (* evaluation *)
signature VALUE =
   sig
      type Value
      exception Value

      val mkValueNumber: int -> Value
          and unValueNumber: Value -> int

      val mkValueBool: bool -> Value
          and unValueBool: Value -> bool

      val ValueNil: Value
      val mkValueCons: Value * Value -> Value
          and unValueHead: Value -> Value
          and unValueTail: Value -> Value

      val eqValue: Value * Value -> bool
      val printValue: Value -> string
   end


signature EVALUATOR =
   sig
      structure Exp: EXPRESSION
      structure Val: VALUE
      exception Unimplemented
      val evaluate: Exp.Expression -> Val.Value
   end

                  (* type checking *)
signature TYPE =
   sig
      eqtype tyvar
      val freshTyvar: unit -> tyvar
      type Type 
      type TypeScheme
  
      val instance: TypeScheme -> Type
      val close: Type -> TypeScheme
	(*constructors and decstructors*)
      exception Type
      val mkTypeInt: unit -> Type
          and unTypeInt: Type -> unit

      val mkTypeBool: unit -> Type
          and unTypeBool: Type -> unit

      val mkTypeList: Type -> Type
          and unTypeList: Type -> Type

      val mkTypeTyvar: tyvar -> Type
          and unTypeTyvar: Type -> tyvar

      type subst
      val Id: subst                     (* the identify substitution;   *)
      val mkSubst: tyvar*Type -> subst 	(* make singleton substitution; *)
      val on : subst * Type -> Type     (* application;                 *)

	
      val prType: Type->string          (* printing *)
   end



signature TYPECHECKER =
   sig
      structure Exp: EXPRESSION
      structure Type: TYPE
      exception NotImplemented of string
      exception TypeError of Exp.Expression * string
      val typecheck: Exp.Expression -> Type.Type
   end;

                  (* the interpreter*)

functor Interpreter
   (structure Ty: TYPE
    structure Value : VALUE
    structure Parser: PARSER
    structure TyCh: TYPECHECKER
    structure Evaluator:EVALUATOR
      sharing Parser.E = TyCh.Exp = Evaluator.Exp
          and TyCh.Type = Ty
          and Evaluator.Val = Value
   ): INTERPRETER=

struct
  val eval= ref true    (* toggle for evaluation *)
  and tc  = ref true    (* toggle for type checking *)
  fun interpret(str)=
    let val abstsyn= Parser.parse str
        val typestr= if !tc then Ty.prType(TyCh.typecheck abstsyn)
                     else "(disabled)"
        val valuestr= if !eval then 
                         Value.printValue(Evaluator.evaluate abstsyn)
                      else "(disabled)"
             
    in  valuestr ^ " : " ^ typestr 
    end
    handle Evaluator.Unimplemented => "Evaluator not fully implemented"
         | TyCh.NotImplemented msg => "Type Checker not fully implemented " ^ msg
         | Value.Value   => "Run-time error"
         | Parser.Syntax msg => "Syntax Error: " ^ msg
         | Parser.Lexical msg=> "Lexical Error: " ^ msg
         | TyCh.TypeError(_,msg)=> "Type Error: " ^ msg
end;
               
                    (* the evaluator *)

functor Evaluator
  (structure Expression: EXPRESSION
   structure Value: VALUE):EVALUATOR=

   struct
      structure Exp= Expression
      structure Val= Value
      exception Unimplemented

      local
         open Expression Value
         fun evaluate exp =
            case exp
              of BOOLexpr b => mkValueBool b
               | NUMBERexpr i => mkValueNumber i
               | SUMexpr(e1, e2) =>
                    let val e1' = evaluate e1
                        val e2' = evaluate e2
                    in
                       mkValueNumber(unValueNumber e1' + unValueNumber e2')
                    end

               | DIFFexpr(e1, e2) =>
                    let val e1' = evaluate e1
                        val e2' = evaluate e2
                    in
                       mkValueNumber(unValueNumber e1' - unValueNumber e2')
                    end

               | PRODexpr(e1, e2) =>
                    let val e1' = evaluate e1
                        val e2' = evaluate e2
                    in
                       mkValueNumber(unValueNumber e1' * unValueNumber e2')
                    end

               | EQexpr _ => raise Unimplemented
               | CONDexpr _ => raise Unimplemented
               | CONSexpr _ => raise Unimplemented
               | LISTexpr _ => raise Unimplemented
               | DECLexpr _ => raise Unimplemented
               | RECDECLexpr _ => raise Unimplemented
               | IDENTexpr _ => raise Unimplemented
               | LAMBDAexpr _ => raise Unimplemented
               | APPLexpr _ => raise Unimplemented

      in
         val evaluate = evaluate
      end
   end;

                        (* the type checker *)   
signature UNIFY=
   sig
      structure Type: TYPE
      exception NotImplemented of string
      exception Unify
      val unify: Type.Type * Type.Type -> Type.subst
   end;

functor TypeChecker
  (structure Ex: EXPRESSION
   structure Ty: TYPE
   structure Unify: UNIFY 
      sharing Unify.Type = Ty
   structure TE: ENVIRONMENT
  )=
struct
  infix on 
  val (op on) = Ty.on
  structure Exp = Ex
  structure Type = Ty
  exception NotImplemented of string
  exception TypeError of Ex.Expression * string

  fun tc (TE: Ty.TypeScheme TE.Environment, exp: Ex.Expression): Ty.Type =
   (case exp of
      Ex.BOOLexpr b => Ty.mkTypeBool()
    | Ex.NUMBERexpr _ => Ty.mkTypeInt()
    | Ex.SUMexpr(e1,e2)  => checkIntBin(TE,e1,e2)
    | Ex.DIFFexpr(e1,e2) => checkIntBin(TE,e1,e2)
    | Ex.PRODexpr(e1,e2) => checkIntBin(TE,e1,e2)
    | Ex.LISTexpr [] =>
         let val new = Ty.freshTyvar ()
          in Ty.mkTypeList(Ty.mkTypeTyvar  new)
         end
    | Ex.LISTexpr(e::es) => tc (TE, Ex.CONSexpr(e,Ex.LISTexpr es))
    | Ex.CONSexpr(e1,e2) =>
        let val t1 = tc(TE, e1)
            val t2 = tc(TE, e2)
            val new = Ty.freshTyvar ()
            val newt= Ty.mkTypeTyvar new
            val t2' = Ty.mkTypeList newt
            val S1 = Unify.unify(t2, t2')
                     handle Unify.Unify=> 
                     raise TypeError(e2,"expected list type")

            val S2 = Unify.unify(S1 on newt,S1 on t1)
                     handle Unify.Unify=>
                     raise TypeError(exp,"element and list have different types")
         in S2 on (S1 on t2)
        end
    | Ex.EQexpr _ => raise NotImplemented "(equality)"
    | Ex.CONDexpr _ => raise NotImplemented "(conditional)"
    | Ex.DECLexpr(x,e1,e2) => 
         let val t1 = tc(TE,e1);
             val typeScheme = Ty.close(t1)
          in tc(TE.declare(x,typeScheme,TE), e2)
         end
    | Ex.RECDECLexpr _ => raise NotImplemented "(rec decl)"
    | Ex.IDENTexpr x   => 
         (Ty.instance(TE.retrieve(x,TE))
         handle TE.Retrieve _ => 
          raise TypeError(exp,"identifier " ^ x ^ " not declared"))
    | Ex.LAMBDAexpr _  => raise NotImplemented "(function)"
    | Ex.APPLexpr _ => raise NotImplemented    "(application)"

   )handle Unify.NotImplemented msg => raise NotImplemented msg
       
  and checkIntBin(TE,e1,e2) =
    let val t1 = tc(TE,e1)
        val _  = Ty.unTypeInt t1
                 handle Ty.Type=> raise TypeError(e1,"expected int")
        val t2 = tc(TE,e2)
        val _  = Ty.unTypeInt t2
                 handle Ty.Type=> raise TypeError(e2,"expected int")
     in Ty.mkTypeInt()
    end;
 
  fun typecheck(e) = tc(TE.emptyEnv,e)

end; (*TypeChecker*)





functor Unify(Ty:TYPE):UNIFY=
struct
   structure Type = Ty
   exception NotImplemented of string
   exception Unify
 
   fun occurs(tv:Ty.tyvar,t:Ty.Type):bool=
     (Ty.unTypeInt t; false)              handle Ty.Type=>
     (Ty.unTypeBool t; false)             handle Ty.Type=>
     let val tv' = Ty.unTypeTyvar t
     in  tv=tv'
     end                                  handle Ty.Type=>
     let val t'  = Ty.unTypeList t
     in  occurs(tv,t')
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
   let val _ = Ty.unTypeInt t
    in let val _ = Ty.unTypeInt t'
        in Ty.Id
       end handle Ty.Type=> raise Unify
   end					handle Ty.Type =>
   let val _ = Ty.unTypeBool t
    in let val _ = Ty.unTypeBool t'
        in Ty.Id
       end handle Ty.Type=> raise Unify
   end					handle Ty.Type=>
   let val t = Ty.unTypeList t
    in let val t' = Ty.unTypeList t'
        in unify(t,t')
       end handle Ty.Type => raise Unify
   end 					handle Ty.Type=>
   raise NotImplemented "(unify)"     

end; (*Unify*)
  
                     (* the basics -- nullary functors *)

functor Type():TYPE =
struct
  type tyvar = int
  val freshTyvar =
      let val r= ref 0 in fn()=>(r:= !r +1; !r) end
  datatype Type = INT
                | BOOL
                | LIST of Type
                | TYVAR of tyvar  

  datatype TypeScheme = FORALL of tyvar list * Type

  fun instance(FORALL(tyvars,ty))=
  let val old_to_new_tyvars = map (fn tv=>(tv,freshTyvar())) tyvars
      exception Find;
      fun find(tv,[])= raise Find
      |   find(tv,(tv',new_tv)::rest)=
          if tv=tv' then new_tv else find(tv,rest)
      fun ty_instance INT = INT
      |   ty_instance BOOL = BOOL
      |   ty_instance (LIST t) = LIST(ty_instance t)
      |   ty_instance (TYVAR tv) = 
             TYVAR(find(tv,old_to_new_tyvars)
                   handle Find=> tv)

  in 
     ty_instance ty
  end
             
  fun close(ty)=
  let fun fv(INT) = []
      |   fv(BOOL)= []
      |   fv(LIST t) = fv(t)
      |   fv(TYVAR tv) = [tv]
   in FORALL(fv ty,ty)
  end;

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

  fun mkTypeTyvar tv = TYVAR tv
  and unTypeTyvar(TYVAR tv) = tv
    | unTypeTyvar _ = raise Type
  
  type subst = Type -> Type

  fun Id x = x
  fun mkSubst(tv,ty)=
     let fun su(TYVAR tv')= if tv=tv' then ty else TYVAR tv'
         |   su(INT) = INT
         |   su(BOOL)= BOOL
         |   su(LIST ty') = LIST (su ty')
      in su
     end


  fun on(S,t)= S(t)

  fun intToString(i:int)=  (if i<0 then " -" else "")^ natToString (abs i)
  and natToString(n:int)=
      let val d = n div 10 in
        if d = 0 then chr(ord"0" + n)
        else natToString(d)^ chr(ord"0" + (n mod 10))
      end

  fun prType INT = "int"
  |   prType BOOL= "bool"
  |   prType (LIST ty) = "(" ^ prType ty ^ ")list"
  |   prType (TYVAR tv) = "a" ^ intToString tv
end;



functor Environment() =
struct
   type 'a Environment = (string *  'a )list

   exception Retrieve of string;
   
   val emptyEnv = [];

   fun declare(s:string,obj:'a,e:'a Environment)=
       (s,obj)::e

   fun retrieve(s,[])= raise Retrieve(s)
   |   retrieve(s,(s',obj)::rest) =
           if s=s' then obj else retrieve(s,rest)

end;

functor Expression(): EXPRESSION =
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
   end;

functor Value(): VALUE =
   struct
      type 'a pair = 'a * 'a

      datatype Value = NUMBERvalue of int   |
                      BOOLvalue of bool   |
                      NILvalue   |
                      CONSvalue of Value pair

      exception Value

      val mkValueNumber = NUMBERvalue
      val mkValueBool = BOOLvalue

      val ValueNil = NILvalue
      val mkValueCons = CONSvalue

      fun unValueNumber(NUMBERvalue(i)) = i   |
          unValueNumber(_) = raise Value

      fun unValueBool(BOOLvalue(b)) = b   |
          unValueBool(_) = raise Value

      fun unValueHead(CONSvalue(c, _)) = c   |
          unValueHead(_) = raise Value

      fun unValueTail(CONSvalue(_, c)) = c   |
          unValueTail(_) = raise Value

      fun eqValue(c1, c2) = (c1 = c2)

				(* Pretty-printing *)
      fun intToString(i:int)=  (if i<0 then " -" else "")^ natToString (abs i)
      and natToString(n:int)=
          let val d = n div 10 in
            if d = 0 then chr(ord"0" + n)
            else natToString(d)^ chr(ord"0" + (n mod 10))
          end
      fun printValue(NUMBERvalue(i)) = intToString(i)   |
          printValue(BOOLvalue(true)) = "true"   |
          printValue(BOOLvalue(false)) = "false"   |
          printValue(NILvalue) = "[]"   |
          printValue(CONSvalue(cons)) = "[" ^ printValueList(cons) ^ "]"
          and printValueList(hd, NILvalue) = printValue(hd)   |
              printValueList(hd, CONSvalue(tl)) =
                 printValue(hd) ^ ", " ^ printValueList(tl)   |
              printValueList(_) = raise Value
   end;






 
