(****************************************************************************
 *                                                                          *
 * This module implements the pair of unwrapOp and wrapOp operations.       *
 * Also the ML types (Types.ty) are transformed into the lambda language    *
 * types(lty).  (zsh)                                                       *
 *                                                                          *
 ****************************************************************************)

signature TRANSTYPES = sig
   val transTyLty : Types.ty -> Lambda.lty
   val lengthOf : Lambda.lty -> int
   val unwrapOp : Lambda.lty * Lambda.lty -> Lambda.lexp -> Lambda.lexp
   val wrapOp : Lambda.lty * Lambda.lty -> Lambda.lexp -> Lambda.lexp
   val checkTypes : Lambda.lty * Lambda.lty -> bool
   val checkArrowTy : Lambda.lty -> Lambda.lty * Lambda.lty
   val checkContTy : Lambda.lty -> Lambda.lty
   val checkRecordTy : Lambda.lty -> Lambda.lty list
end

structure Transtypes : TRANSTYPES  = 

struct

open Access Types BasicTypes Lambda

(****************************************************************************
 *                  UTILITY FUNCTIONS AND CONSTANTS                         * 
 ****************************************************************************) 
val impossible = ErrorMsg.impossible
val eqTycon = TypesUtil.eqTycon
fun ident le = le

fun merge(h1::t1,h2::t2) = (h1,h2)::(merge(t1,t2))
  | merge(nil,nil) = nil
  | merge _ = impossible "type conflicts in transtypes-merge"

fun merge2(h1::t1,h2::t2,i) = (h1,h2,i)::(merge2(t1,t2,i+1))
  | merge2(nil,nil,i) = nil
  | merge2 _ = impossible "type conflicts in transtypes-merge2"

fun mkmod(RECORDty l) = RECORDty (map (fn x => RBOXEDty) l)
  | mkmod(ARROWty _) = ARROWty(RBOXEDty,RBOXEDty)
  | mkmod(CONTty _) = CONTty(RBOXEDty)
  | mkmod _ = BOXEDty

(****************************************************************************
 * Translate a ML type (Types.ty) to the lambda type (Lambda.lty)           *
 *               transTyLty : Types.ty -> Lambda.lty                        *
 ****************************************************************************)
fun transTyLty ty = 
  let fun transty(POLYty{tyfun=TYFUN{body,arity},...},_) = 
             let val f = TypesUtil.getRecTyvarMap(arity,body)
              in transty(body,f)
             end
        | transty(VARty(ref (INSTANTIATED ty)),f) = transty(ty,f)
        | transty(CONty(RECORDtyc label,args),f) = 
             let val args' = map (fn y => transty(y,f)) args
              in RECORDty args'
             end   (*** I am assuming all record types are sorted ***)
        | transty(CONty(tycon,args),f) =
           if eqTycon(tycon,intTycon) then INTty
           else if eqTycon(tycon,boolTycon) then BOOLty
                else if eqTycon(tycon,realTycon) then REALty
                     else if eqTycon(tycon,arrowTycon) then
                             (let val t1 = transty(nth(args,0),f)
                                  val t2 = transty(nth(args,1),f)
                               in ARROWty(t1,t2)
                              end)
                          else if eqTycon(tycon,contTycon) then
                                    (CONTty(transty(nth(args,0),f)))
                               else if eqTycon(tycon,unitTycon) then INTty
                                    else BOXEDty
        | transty(IBOUND i, f) = if (f i) then RBOXEDty else BOXEDty
        | transty _ = BOXEDty
   in transty (ty,fn i => false)
  end

(****************************************************************************
 * lengthOf : Lambda.lty -> int                                             *
 *     In the future, lengthOf(REALty) and lengthOf(ARROWty) perhaps        *
 *     should be 2 instead.                                                 *
 ****************************************************************************)
fun lengthOf(RECORDty args) = fold (op +) (map lengthOf args) 0
  | lengthOf _ = 1


(****************************************************************************
 *            checkTypes : Lambda.lty * Lambda.lty -> bool                  *
 *          checkArrowTy : Lambda.lty -> (Lambda.lty * Lambda.lty)          *
 *           checkContTy : Lambda.lty -> Lambda.lty                         *
 *         checkRecordTy : Lambda.lty -> Lambda.lty list                    *
 ****************************************************************************)
fun checkTypes(BOXEDty,BOXEDty) = true
  | checkTypes(INTty,INTty) = true
  | checkTypes(REALty,REALty) = true
  | checkTypes(BOOLty,BOOLty) = true
  | checkTypes(RECORDty args,RECORDty args') = 
        let fun f(a,b) = if a then b else false
	 in fold f (map checkTypes (merge(args,args'))) true
        end
  | checkTypes(ARROWty(lt1,lt2),ARROWty(rt1,rt2)) =
        (checkTypes(lt1,rt1) andalso checkTypes(lt2,rt2))
  | checkTypes(CONTty args, CONTty args') = checkTypes(args,args') 
  | checkTypes(RBOXEDty,RBOXEDty) = true
  | checkTypes(RECORDty nil, INTty) = true
  | checkTypes(INTty, RECORDty nil) = true
  | checkTypes _ = impossible "checkTypes in translate-transTyLty"

fun checkArrowTy(ARROWty(ty1,ty2)) = (ty1,ty2)
  | checkArrowTy _ = impossible "transtypes.sml checkArrowTy 324"

fun checkContTy(CONTty lt) = lt
  | checkContTy _ = impossible "transtypes.sml checkContTy 301"

fun checkRecordTy(RECORDty lt) = lt
  | checkRecordTy _ = impossible "transtypes.sml checkRecordTy 212"


(****************************************************************************
 * unwrapOp : Lambda.lty * Lambda.lty -> Lambda.lexp -> Lambda.lexp         *
 *   wrapOp : Lambda.lty * Lambda.lty -> Lambda.lexp -> Lambda.lexp         *
 ****************************************************************************)

fun unwrapOp(BOXEDty,BOXEDty) = ident
  | unwrapOp(RBOXEDty,RBOXEDty) = ident
  | unwrapOp(INTty,INTty) = ident
  | unwrapOp(REALty,REALty) = ident
  | unwrapOp(BOOLty,BOOLty) = ident
  | unwrapOp(RECORDty nil,INTty) = ident
  | unwrapOp(INTty,RECORDty nil) = ident
  | unwrapOp(RECORDty nil, RECORDty nil) = ident
  | unwrapOp(RECORDty args,RECORDty args') =
      let val v = mkLvar()
          val args2 =  merge2(args,args',0)
          val result = 
            map (fn (a,a',n) => ((unwrapOp(a,a'))(SELECT(n,VAR v)))) args2 
       in fn l => (APP(FN(v,BOGUSty, RECORD result),l))
      end
  | unwrapOp(ARROWty(lt1,lt2),ARROWty(rt1,rt2)) =
      let val v = mkLvar()
          val bigS = unwrapOp(lt2,rt2)
          val bigG = wrapOp(lt1,rt1)
       in fn l => FN(v,rt1,bigS(APP(l,bigG(VAR v))))
      end
  | unwrapOp(BOXEDty,lty) = 
      if (lty = RBOXEDty) then ident
      else (fn l => UNWRAP(lty,l))
  | unwrapOp(RBOXEDty,lty) = 
      if (lty = BOXEDty) then ident
      else (let val m = mkmod(lty)
             in unwrapOp(m,lty)         
            end)
  | unwrapOp(CONTty args,CONTty args') = 
      let val v = mkLvar()
          val bigG = wrapOp(args,args')
       in (fn l => FN(v,args',APP(l,bigG(VAR v))))
      end
  | unwrapOp _ = impossible "type conflicts happened in transtypes-unwrapOp2"
 
and wrapOp(BOXEDty,BOXEDty) = ident
  | wrapOp(RBOXEDty,RBOXEDty) = ident
  | wrapOp(INTty,INTty) = ident
  | wrapOp(REALty,REALty) = ident
  | wrapOp(BOOLty,BOOLty) = ident
  | wrapOp(RECORDty nil,INTty) = ident
  | wrapOp(INTty,RECORDty nil) = ident
  | wrapOp(RECORDty nil,RECORDty nil) = ident
  | wrapOp(RECORDty args,RECORDty args') =
      let val v = mkLvar()
          val args2 =  merge2(args,args',0)
          val result = 
              map (fn (a,a',n) => ((wrapOp(a,a'))(SELECT(n,VAR v)))) args2 
       in fn l => (APP(FN(v,BOGUSty,RECORD result),l))
      end
  | wrapOp(ARROWty(lt1,lt2),ARROWty(rt1,rt2)) =
      let val v = mkLvar()
          val bigG = wrapOp(lt2,rt2)
          val bigS = unwrapOp(lt1,rt1)
       in fn l => FN(v,lt1,bigG(APP(l,bigS(VAR v))))
      end
  | wrapOp(BOXEDty,lty) = 
      if (lty = RBOXEDty) then ident
      else (fn l => WRAP(lty,l))
  | wrapOp(RBOXEDty,lty) = 
      if (lty = BOXEDty) then ident
      else (let val m = mkmod(lty)
             in wrapOp(m,lty)
            end)
  | wrapOp(CONTty args,CONTty args') = 
      let val v = mkLvar()
          val bigS = unwrapOp(args,args')
       in fn l => FN(v,args,(APP(l,bigS(VAR v))))
      end
  | wrapOp _ = impossible "type conflicts happened in transtypes-wrapOp2"


(****************************************************************************
 *  Turn off all effects if !System.Control.CG.representations is false     * 
 ****************************************************************************)
  val flag = !System.Control.CG.representations
  val transTyLty = if flag then transTyLty else (fn _ => BOGUSty) 
  val lengthOf = if flag then lengthOf else (fn _ => 30000)
  val checkTypes = if flag then checkTypes else (fn _ => true)
  val checkArrowTy = if flag then checkArrowTy else (fn _ => (BOGUSty,BOGUSty))
  val checkContTy = if flag then checkContTy else (fn _ => BOGUSty)
  val checkRecordTy = if flag then checkRecordTy else (fn _ => [BOGUSty])
  val unwrapOp = if flag then unwrapOp else (fn _ => ident)
  val wrapOp = if flag then unwrapOp else (fn _ => ident)

end
