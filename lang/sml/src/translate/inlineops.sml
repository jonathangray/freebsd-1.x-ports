structure InlineOps : INLINE_OPS =
   struct
      open Access Absyn Lambda Types BasicTypes Transtypes

      fun transDcon(DATACON{name,rep,typ,...}) = (name,rep,transTyLty typ)
      val trueDcon' = transDcon trueDcon
      val falseDcon' = transDcon falseDcon

      val unitLexp = RECORD[]
      val conToLexp =
         fn (d as DATACON{const=true,...}) => CON'(transDcon d,unitLexp)
          | (d as DATACON{const=false,typ=ty,name=name,rep=rep,...}) => 
              let val v = mkLvar() 
                  val t = transTyLty ty
                  val (t1,t2) = checkArrowTy(t) 
               in FN(v,t1,CON'((name,rep,t),VAR v)) 
              end

      val COND = fn (a : lexp,b : lexp, c : lexp) => 
              SWITCH(a,boolsign,
                    [(DATAcon(trueDcon'),b),
                     (DATAcon(falseDcon'),c)],NONE)

      val LET = fn (v : lvar, e : lexp, b : lexp) => APP(FN(v,BOGUSty,b),e)

      val predTy = ARROWty(RECORDty[INTty,INTty],BOOLty)
      val lengthTy = ARROWty(BOXEDty,INTty)
      val subscriptTy = ARROWty(RECORDty[BOXEDty,INTty],RBOXEDty)

(* Functions to generate lambda language expressions for the following
   ML functions *)

(*  val op sub : 'a array * int -> 'a =
	  fn (a,i) =>
	     if lessu(i, alength a) then subscript(a, i) else raise Subscript
*)


  
      val inlsubscript = fn lty =>
        let val p = mkLvar()
            val a = mkLvar()
            val i = mkLvar()
            val vp = VAR p
            val va = VAR a
            val vi = VAR i
            val argt = RECORDty[BOXEDty,INTty]
            val (_,res) = checkArrowTy(lty)
            val header = unwrapOp(subscriptTy,lty)
         in FN(p,argt,
             LET(a,SELECT(0,vp),
              LET(i,SELECT(1,vp),
                 COND(APP(PRIM(P.LESSU,predTy),
                          RECORD[vi,APP(PRIM(P.LENGTH,lengthTy), va)]),
                      APP(header(PRIM(P.SUBSCRIPT,subscriptTy)),
                          RECORD[va,vi]),
                      RAISE(conToLexp(!CoreInfo.exnSubscript),res)))))
        end

      val inlsubscriptv = fn lty =>
        let val p = mkLvar()
            val a = mkLvar()
            val i = mkLvar()
            val vp = VAR p
            val va = VAR a
            val vi = VAR i
            val argt = RECORDty[BOXEDty,INTty]
            val (_,res) = checkArrowTy(lty)
            val header = unwrapOp(subscriptTy,lty)
         in FN(p,argt,
             LET(a,SELECT(0,vp),
              LET(i,SELECT(1,vp),
                 COND(APP(PRIM(P.LESSU,predTy),
                     RECORD[vi,APP(PRIM(P.LENGTH,lengthTy), va)]),
                     APP(header(PRIM(P.SUBSCRIPTV,subscriptTy)),
                         RECORD[va,vi]),
                     RAISE(conToLexp(!CoreInfo.exnSubscript),res)))))
        end

(*  val update : 'a array * int * 'a -> unit =
	  fn (a,i,v) => 
	     if lessu(i, alength a) then update(a, i, v) else raise Subscript
*)

      val inlupdate = fn(lty,oper) =>
        let val t = mkLvar()
            val a = mkLvar()
            val i = mkLvar()
            val v = mkLvar()
            val vt = VAR t
            val va = VAR a
            val vi = VAR i
            val vv = VAR v
            val (argt,res) = checkArrowTy(lty)
         in FN(t,argt,
               LET(a,SELECT(0,vt),
                LET(i,SELECT(1,vt),
                 LET(v,SELECT(2,vt),
                  COND(APP(PRIM(P.LESSU,predTy),
                           RECORD[vi,APP(PRIM(P.LENGTH,lengthTy),va)]),
                       APP(oper,RECORD[va,vi,vv]),
                       RAISE(conToLexp(!CoreInfo.exnSubscript),res))))))
        end

(* Bytearray subscripting:

    val op sub = fn (s, i) =>
          if lessu(i, blength s) then byteof(s, i) else raise Subscript
*)

    val inlbyteof = fn () =>
        let val p = mkLvar()
            val s = mkLvar()
            val i = mkLvar()
            val vp = VAR p
            val vs = VAR s
            val vi = VAR i
            val argt = RECORDty[BOXEDty,INTty]
            val ordofty = ARROWty(argt,INTty)
         in FN(p,argt,
             LET(s,SELECT(0,vp),
              LET(i,SELECT(1,vp),
                 COND(APP(PRIM(P.LESSU,predTy),
                          RECORD[vi,APP(PRIM(P.LENGTH,lengthTy),vs)]),
                      APP(PRIM(P.ORDOF,ordofty),RECORD[vs,vi]),
                      RAISE(conToLexp(!CoreInfo.exnOrd),INTty)))))
        end
 
(* Bytearray store:

   fun update(arg as (s,i,c)) =
	if i<0 orelse i >= blength s then raise Subscript
	else if c<0 orelse c>255 then raise Range
	else InLine.store arg
*)

      val inlstore = fn () =>
        let val t = mkLvar()
            val s = mkLvar()
            val i = mkLvar()
            val c = mkLvar()
            val vt = VAR t
            val vs = VAR s
            val vi = VAR i
            val vc = VAR c
            val argt = RECORDty[BOXEDty,INTty,INTty]
            val storety = ARROWty(argt,INTty)
         in FN(t,argt,
              LET(s,SELECT(0,vt),
               LET(i,SELECT(1,vt),
                LET(c,SELECT(2,vt),
                 COND(APP(PRIM(P.LESSU,predTy),
                          RECORD[vi,APP(PRIM(P.LENGTH,lengthTy),vs)]),
                      COND(APP(PRIM(P.LESSU,predTy),RECORD[vc,INT 256]),
                           APP(PRIM(P.STORE,storety),RECORD[vs,vi,vc]),
                           RAISE(conToLexp(!CoreInfo.exnRange),INTty)),
                      RAISE(conToLexp(!CoreInfo.exnOrd),INTty))))))
        end

(*  String ordof:

    val ordof = fn (s,i) =>
	  if boxed s
            then if lessu(i, slength s) then ordof(s, i) else raise Ord
	    else if ieql(i,0) then cast s else raise Ord
*)

      val inlordof = fn () =>
        let val p = mkLvar()
            val s = mkLvar()
            val i = mkLvar()
            val vp = VAR p
            val vs = VAR s
            val vi = VAR i
            val argt = RECORDty[BOXEDty,INTty]
            val ordofty = ARROWty(argt,INTty)
            val boxedty = ARROWty(BOXEDty,BOOLty)
         in FN(p,argt,
               LET(s,SELECT(0,vp),
                LET(i,SELECT(1,vp),
                 COND(APP(PRIM(P.BOXED,boxedty),vs),
                      COND(APP(PRIM(P.LESSU,predTy),
                               RECORD[vi,APP(PRIM(P.LENGTH,lengthTy),vs)]),
                           APP(PRIM(P.ORDOF,ordofty),RECORD[vs,vi]),
                           RAISE(conToLexp(!CoreInfo.exnOrd),INTty)),
                      COND(APP(PRIM(P.IEQL,predTy),RECORD[vi,INT 0]),
                           UNWRAP(INTty,vs),
                           RAISE(conToLexp(!CoreInfo.exnOrd),INTty))))))
        end


(* 
 * inlsubscriptf = fn(a,i)=> if lessu(i, slength a) then subscriptf(a,i)
 *			     else raise Vector.Subscript
 *)
      val inlsubscriptf = fn () => 
        let val p = mkLvar()
            val a = mkLvar()
            val i = mkLvar()
            val vp = VAR p
            val va = VAR a
            val vi = VAR i
            val argt = RECORDty[BOXEDty,INTty]
            val fsubty = ARROWty(argt,REALty)
         in FN(p,argt,
  	      LET(a,SELECT(0,vp),
               LET(i,SELECT(1,vp),
	        COND(APP(PRIM(P.LESSU,predTy), 
                         RECORD[vi,APP(PRIM(P.LENGTH,lengthTy),va)]),
		     APP(PRIM(P.FSUBSCRIPTd,fsubty),RECORD[va,vi]),
		     RAISE(conToLexp(!CoreInfo.exnRealSubscript),REALty)))))
	end
	      
(*
 * val inlupdatef = fn(a,i,v)=> if lessu(i, slength a) then updatef(a,i,v) 
 *				else raise RealSubscript
 *)
      val inlupdatef = fn () =>
	let val t = mkLvar()
            val a = mkLvar()
            val i = mkLvar()
            val v = mkLvar()
            val vt = VAR t
            val va = VAR a
            val vi = VAR i
            val vv = VAR v
            val argt = RECORDty[BOXEDty,INTty,REALty]
            val fupdty = ARROWty(argt,INTty)
         in FN(t,argt,
	      LET(a,SELECT(0,vt),
               LET(i,SELECT(1,vt),
                LET(v,SELECT(2,vt),
                 COND(APP(PRIM(P.LESSU,predTy),
                          RECORD[vi,APP(PRIM(P.LENGTH,lengthTy),va)]),
	              APP(PRIM(P.FUPDATEd,fupdty),RECORD[va,vi,vv]),
	              RAISE(conToLexp(!CoreInfo.exnRealSubscript),INTty))))))
        end
   end

