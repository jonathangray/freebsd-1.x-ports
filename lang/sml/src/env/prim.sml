(* Copyright 1989 by AT&T Bell Laboratories *)
structure Prim : sig val primEnv : Modules.env
		     val inLineName : Access.primop -> string
		     val pure : Access.primop -> bool
		     val mayRaise : Access.primop -> bool
		     val special : Access.access -> bool
                     val assignVar : Variables.var
		     val fixOpEnv : Modules.env
		 end = 
struct
   open Access Modules Variables Types BasicTypes Symbol Fixity

(* primTypes structure *)

   val env =
       (fold (fn ((s, t), e) => Env.bind(tycSymbol s, TYCbind t, e))
	     [("bool", boolTycon),
	      ("list", listTycon),
	      ("ref", refTycon),
	      ("unit", unitTycon),
	      ("int", intTycon),
	      ("real", realTycon),
	      ("cont", contTycon),
	      ("array", arrayTycon),
	      ("vector", vectorTycon),
	      ("string", stringTycon),
	      ("exn", exnTycon),
	      ("option", optionTycon),
              ("frag", fragTycon),
	      ("symbol", symbolTycon),
	      ("environment", environmentTycon),
	      ("staticEnv", staticEnvTycon),
	      ("source", sourceTycon),
	      ("codeUnit", codeUnitTycon)]
	     Env.empty)

   val env =
       (fold (fn ((s,c),e) => Env.bind(varSymbol s, CONbind c, e))
	     [("true", trueDcon),
	      ("false", falseDcon),
	      ("::", consDcon),
	      ("nil", nilDcon),
	      ("ref", refDcon),
	      ("SOME", SOMEDcon),
	      ("NONE", NONEDcon),
              ("QUOTE", QUOTEDcon),
              ("ANTIQUOTE", ANTIQUOTEDcon)]
	     env)


   val primTypesFix = 
       let val fixcons = fixSymbol "::"
        in Env.bind(fixcons,FIXbind(FIXvar{name=fixcons,
					   binding=infixright 5}),
		    Env.empty)
       end

   val fixOpEnv =
        fold
          (fn ((name,fixity,isleft),env) =>
             let val name = Symbol.fixSymbol name
             in
             Env.bind(name,
                      FIXbind(FIXvar{
                        name = name,
                        binding = (if isleft then infixleft else infixright)
                                  fixity}),
                      env)
             end)
          [(":=",3,true),
           ("::",5,false),
           ("@",5,false),
           ("^",6,true),
           (">",4,true),
           ("<",4,true),
           (">=",4,true),
           ("<=",4,true),
           ("=",4,true),
           ("<>",4,true),
           ("+",6,true),
           ("-",6,true),
           ("*",7,true),
           ("/",7,true),
           ("quot",7,true),
           ("mod",7,true),
           ("rem",7,true),
           ("div",7,true),
           ("before",5,true),
           ("o",3,true)]
        Env.empty


   val primTypes = ModuleUtil.mkStructure(Env.consolidate env,[])

(* inLine structure *)

   val bottom = POLYty{sign=[{weakness=infinity,eq=false}], abs=0,
		       tyfun=TYFUN{arity=1,body=IBOUND 0}}

   val primopNames = [
        ("capture",P.CAPTURE),
        ("callcc",P.CALLCC),
        ("throw",P.THROW),
	("!",P.DEREF),
	("*",P.IMUL ),
	("+",P.IADD),
	("-",P.ISUB),
	(":=",P.ASSIGN),
	("<",P.ILT),
	("<=",P.ILE),
	(">",P.IGT),
	(">=",P.IGE),
  	("lessu",P.LESSU),
  	("gequ",P.GEQU),
	("boxed",P.BOXED),
	("unboxed",P.UNBOXED),
	("div",P.IDIV),
	("orb",P.ORB),
	("andb",P.ANDB),
	("xorb",P.XORB),
	("lshift",P.LSHIFT),
	("rshift",P.RSHIFT),
	("notb",P.NOTB),
	("cast",P.CAST),
	("=",P.POLYEQL),
	("fadd",P.FADDd),
  	("floor",P.FLOOR),
  	("round",P.ROUND),
  	("real",P.REAL),
  	("subscriptf",P.FSUBSCRIPTd),
  	("updatef",P.FUPDATEd),
  	("inlsubscriptf",P.INLFSUBSCRIPTd),
  	("inlupdatef",P.INLFUPDATEd),
  	("subscriptv",P.SUBSCRIPTV),
  	("inlsubscriptv",P.INLSUBSCRIPTV),
	("fdiv",P.FDIVd),
	("feql",P.FEQLd),
	("fge",P.FGEd),
	("fgt",P.FGTd),
	("fle",P.FLEd),
	("flt",P.FLTd),
	("fmul",P.FMULd),
	("fneq",P.FNEQd),
	("fsub",P.FSUBd),
	("fnegd",P.FNEGd),
	("fabsd",P.FABSd),
	("gethdlr",P.GETHDLR),
	("ieql",P.IEQL),
	("ineq",P.INEQ),
	("<>",P.POLYNEQ),
	("makeref",P.MAKEREF),
	("ordof",P.ORDOF),
	("sethdlr",P.SETHDLR),
	("length",P.LENGTH),
	("objlength",P.OBJLENGTH),
	("store",P.STORE),
	("subscript",P.SUBSCRIPT),
	("boxedupdate",P.BOXEDUPDATE),
	("unboxedupdate",P.UNBOXEDUPDATE),
	("update",P.UPDATE),
        ("inlsubscript",P.INLSUBSCRIPT),
        ("inlupdate",P.INLUPDATE),
        ("inlbyteof",P.INLBYTEOF),
        ("inlstore",P.INLSTORE),
        ("inlordof",P.INLORDOF),
  	("~",P.INEG),
  	("getvar",P.GETVAR),
  	("setvar",P.SETVAR),
	("gettag", P.GETTAG),
	("mkspecial", P.MKSPECIAL),
	("getspecial", P.GETSPECIAL),
	("setspecial", P.SETSPECIAL),
  	("uselvar",P.USELVAR),
  	("deflvar",P.DEFLVAR)]

   fun enter((s : string, p : primop), env) =
       let val name = varSymbol s
        in Env.bind(name,
		    VARbind(VALvar{access=INLINE p, name=[name],
				   typ= ref bottom}),
		    env)
       end

   val assignVar =
         VALvar{access=INLINE P.ASSIGN,
                name=[Symbol.varSymbol ":="],
                typ= ref bottom}

   val inLine =
      ModuleUtil.mkStructure(Env.consolidate(fold enter primopNames
					     Env.empty),[])

  (* priming structures: PrimTypes and InLine *)
   val nameofPT = Symbol.strSymbol "PrimTypes"
   val varofPT = STRvar{name=nameofPT,access=PATH[0],binding=primTypes}
   val nameofIL = Symbol.strSymbol "InLine"
   val varofIL = STRvar{name=nameofIL,access=PATH[0],binding=inLine}

   val primEnv =
         Env.bind(nameofIL,STRbind varofIL, 
	   Env.bind(nameofPT,STRbind varofPT,
	     Env.atop(primTypesFix,Env.atop(fixOpEnv,
		       ModuleUtil.openStructureVar (Env.empty,varofPT)))))


   fun inLineName p =
       let fun find [] = ErrorMsg.impossible "Prim.inLineName - bad primop name"
	     | find ((s,p1)::rest) = if p1=p then s else find rest
        in find primopNames
       end

   val pure =
     fn P.DEREF => false
      | P.ASSIGN => false (* this should probably should never be called on ASSIGN *)
      | P.SUBSCRIPT => false
      | P.STORE => false
      | P.BOXEDUPDATE => false
      | P.UNBOXEDUPDATE => false
      | P.UPDATE => false
      | P.CAPTURE => false
      | P.CALLCC => false
      | P.INEG => false (* these must be here because they may raise *)
      | P.IADD => false
      | P.ISUB => false
      | P.IMUL => false
      | P.IDIV => false
      | P.FADDd => false
      | P.FSUBd => false
      | P.FMULd => false
      | P.FDIVd => false
      | P.LSHIFT => false
      | P.FSUBSCRIPTd => false
      | P.FUPDATEd => false
      | P.GETSPECIAL => false
      | P.SETSPECIAL => false
      | _ => true
  
   val mayRaise =
     fn P.INEG => true
      | P.IADD => true
      | P.ISUB => true
      | P.IMUL => true
      | P.IDIV => true
      | P.FADDd => true
      | P.FSUBd => true
      | P.FMULd => true
      | P.FDIVd => true
      | P.FLOOR => true
      | P.ROUND => true
      | _ => false

   fun special (INLINE P.POLYEQL) = true
     | special (INLINE P.POLYNEQ) = true
     | special (INLINE P.ASSIGN) = true
     | special (INLINE P.UPDATE) = true
     | special (INLINE P.INLUPDATE) = true
     | special (INLINE P.INLSUBSCRIPT) = true
     | special (INLINE P.INLSUBSCRIPTV) = true
     | special (INLINE P.INLBYTEOF) = true
     | special (INLINE P.INLSTORE) = true
     | special (INLINE P.INLORDOF) = true
     | special (INLINE P.INLFSUBSCRIPTd) = true
     | special (INLINE P.INLFUPDATEd) = true
     | special _ = false

end (* structure Prim *)

