(**********************************************************************
 * CONVENTIONS on the input to ContMap module:                        *
 *   The cexp into the contmap function is directly from the cpsopt   *
 *   module. We assume that every escaped functions have either one   *
 *   or two arguments and the one-arguments excaped functions are     *
 *   definitely continuations.                                        *
 **********************************************************************)
signature CONTMAP = sig
   val contmap : CPS.function -> CPS.function * (CPS.lvar -> bool) 
                                 * (CPS.lvar -> bool * CPS.lvar list)
end

structure ContMap : CONTMAP = 
struct
    
open CPS Access SortedList

(**********************************************************************
 *               UTILITY FUNCTIONS AND CONSTANTS                      *
 **********************************************************************)
val error = ErrorMsg.impossible

fun clean l =
    let fun vars(l, VAR x :: rest) = vars(x::l, rest)
          | vars(l, _::rest) = vars(l,rest)
          | vars(l, nil) = rev l
     in vars(nil,l)
    end

fun sublist test =
    let fun subl(a::r) = if test a then a::(subl r) else subl r
          | subl [] = []
     in  subl
    end

fun divlist test =
    let fun divl(a::r) = let val (t1,t0) = divl r
                          in if (test a) then (a::t1,t0)
                             else (t1,a::t0)
                         end
          | divl [] = ([],[])
     in divl
    end

fun mixer(v::vl,t::tl) = (v::t)::mixer(vl,tl)
  | mixer(nil,nil) = nil
  | mixer _ = error "grouping known fun info with diff # of args"

fun grouping nil = nil 
  | grouping (vl::nil) = map (fn x => [x]) vl
  | grouping (vl::tl) =  mixer(vl,grouping tl)
                        
fun lookup(v,nil) = NONE
  | lookup(v,(a,b)::tl) = if v = a then (SOME b) else lookup(v,tl)

fun clookup(v,env) = (case lookup(v,env) of NONE => v
                                          | SOME v' => v')

fun glookup(VAR v,env) = VAR (clookup(v,env))
  | glookup(x,env) = x

fun fix(nil,e) = e
  | fix(fl,e) = FIX(fl,e)
    

(**********************************************************************
 *  contmap : CPS.function -> CPS.function * (CPS.lvar -> bool)       *
 *                            * (CPS.lvar -> bool * CPS.lvar list)    *
 *                                                                    *
 * The function contmap eliminates all strange continuation variables *
 * and generates accurate continuation variable information, the      *
 * postion information and the free variable information              *
 **********************************************************************)
fun contmap(fvar,fargs,cexp) = 
let val (_,_,known) = FreeMap.freemapClose(cexp)
    val escapes = not o known

    exception Mcont
    val t : (value list list) ref Intmap.intmap = Intmap.new(32,Mcont)
    fun find v = (!(Intmap.map t v)) handle Mcont => nil
    fun add(v,vl) = (let val a = Intmap.map t v
                         val _ = (a := (vl::(!a)))
                      in ()
                     end) handle Mcont => Intmap.add t (v,ref([vl]))

    val contset = Intset.new()
    fun iscont v = Intset.mem contset v
    fun contM v = Intset.add contset v

    fun iscontv (VAR v) = iscont v 
      | iscontv _ = false 


(* cexp -> lvar list, i.e. known function name list. The side-effect is to 
 * find all continuation variables and to put them into a hash table.
 *)
    val rec incinfo = fn (e,l) => (contvars e)@l
    and contvars = 
      fn RECORD(_,vl,w,e) => contvars e
       | SELECT(i,v,w,e) => contvars e
       | APP(f as (VAR g),vl) => 
           if (escapes g) then 
             (let val k = length(vl)
               in if (k > 2) orelse (k < 1) 
		  then (error "escaped funs have > 2 or < 1 arguments z";
		        nil)    
		  else if k = 1 then (contM g ; nil)
		       else let val c = case vl 
                                  of (_::(VAR x)::_) => x
                                   | _ => error "contmap contvars 124"
                                (* the 2nd arg must be a VAR *)
	 		     in (contM c ; nil)
                            end
              end)        
           else (add(g,vl);[g])
       | SWITCH(v,c,el) => (fold incinfo el) nil
       | BRANCH(_,_,_,e1,e2) => contvars e1 @ contvars e2
       | SETTER(_,_,e) => contvars e
       | LOOKER(_,_,_,e) => contvars e
       | ARITH(_,_,_,e) => contvars e
       | PURE(_,_,_,e) => contvars e
       | FIX(fl,e) => fold contvars' fl (contvars e)
       | _ => (error "contmap contvars 125")
    and contvars' = 
      fn ((fv,fa,ce),a) =>
         if escapes fv 
         then let val k = length(fa)
                  val kl = contvars ce
               in if (k > 2) orelse (k < 1) 
		  then (error "escaped funs have > 2, < 1 args z"; a)
		  else if k = 1 then (contM fv ; (kl@a))
                       else (contM (nth(fa,1)) ; (kl@a))
              end
         else let val kl = contvars ce
               in add(fv,(map VAR fa)); fv::(kl@a)
              end

    val knownlabs = contvars'((fvar,fargs,cexp), [])

    val knownlabs = uniq knownlabs
          
(* run the stupid loop to gather all known functions' continuation variable 
 * information . It's expected to be rewritten in the future .
 *)
    local val clicked = ref 0
          fun click () = (inc clicked) 
          fun cpass v = 
           let val infolist = (find v) 
               val newl = grouping infolist
               fun proc vl =
	             case divlist iscont (clean vl)                   
                      of (nil,_) => ()
                       | (_,nil) => ()
                       | (_,vl0) => (click(); (app contM vl0))
            in (app proc newl) 
           end
          fun loop () = let val _ = (app cpass knownlabs) 
                            val k = (!clicked) before (clicked := 0)
                         in if k = 0 then ()
                            else loop ()
                        end
       in val _ = loop ()
      end 

    fun substin(v,(env,fl)) = 
         case lookup(v,env) of 
            NONE => (let val v' = dupLvar v
		         val x = mkLvar()
                         val c = mkLvar()
                         val _ = contM(c) 
                         val tmp = (v',[x,c],APP(VAR v,[VAR x]))
     	              in ((v,v')::env,tmp::fl)
                     end)
          | SOME v' => (env,fl)

(* This is a very tricky function. (v,v') is added to env because 
 * when v is stored back somewhere, we want to use v' again.
 *)
    fun substout(v,(env,fl)) = 
         case lookup(v,env) of 
            NONE => (let val v' = dupLvar v
		         val x = mkLvar()
                         val tmp = (v,[x],APP(VAR v',
                                           [(VAR x),(INT 0)]))
		      in ((v,v')::env,tmp::fl)
                     end)
          | SOME v' => (env,fl)

    exception EB 
    val ebtable : (bool * lvar list) Intmap.intmap = Intmap.new(32,EB)
    fun ebinfo v = (Intmap.map ebtable v) 
                     handle EB => (true,nil)
    fun ebadd(v,info) = Intmap.add ebtable (v,info)
    fun enterv(VAR v,l) = enter(v,l)
      | enterv(_,l) = l (* error "contmap enterv 123" *)

    fun transform(RECORD(k,vl,w,e),env) = 
         let val cl = uniq(sublist iscont (clean (map #1 vl)))
             val (env',fl) = (fold substin cl) (env,nil)
             val vl' = map (fn (x,p) => (glookup(x,env'),p)) vl
             val (e',eb,free) = transform(e,env')
             val eb' = if (cl=nil) then eb else true
             val free' = merge(uniq(clean(map #1 vl)),rmv(w,free))
             val free'' = remove(uniq(map #1 fl),free')  
          in (fix(fl,RECORD(k,vl',w,e')),eb',free'')
         end

      | transform(SELECT(i,v,w,e),env) =
         if (iscont w) then 
           (let val (env',fl) = substout(w,(env,nil))
                val w' = clookup(w,env')
                val (e',_,free) = transform(e,env')
                val free' = enterv(v,rmv(w',rmv(w,free)))
             in (SELECT(i,v,w',fix(fl,e')),true,free')
            end)
         else (let val (e',eb,free) = transform(e,env)
                   val free' = enterv(v,rmv(w,free))
                in (SELECT(i,v,w,e'),eb,free')
               end)

      | transform(APP(VAR f,vl),env) = 
         let val cl = sublist iscont (clean vl)
             val k = if iscont f then (length cl) else (length cl)-1  
          in if (k < 1) 
             then (APP(VAR f,vl),false,enter(f,uniq(clean(vl))))
             else (let fun sep(nil) = (nil,nil)
                         | sep(hd::tl) = 
                             if iscontv hd then ([hd],tl)
                             else (let val (a,b) = sep tl
                                    in (hd::a,b)
                                   end)
                       val (vl1,vl2) = if iscont f then (nil,vl) 
                                       else sep(List.rev vl)
                       val (vl1,vl2) = (List.rev vl1 , List.rev vl2)

                       (*** cl must not be empty because k > 0 here ***) 
                       val cl = uniq(sublist iscont (clean vl2))
                       val (env',fl) = (fold substin cl) (env,nil)
                       val vl' = (map (fn x => glookup(x,env')) vl2)@vl1
                       val free = enter(f,uniq(clean vl))
                       val free' = remove(uniq(map #1 fl),free)
                    in (fix(fl,APP(VAR f,vl')),true,free')
                   end)
         end
      
      | transform(SWITCH(v,c,el),env) = 
         let fun f(e,(el,eb,free)) =
               let val (e',eb',free') = transform(e,env)
                in (e'::el,eb orelse eb',merge(free,free'))
               end
             val (el',eb',free') = fold f el (nil,false,nil)
          in (SWITCH(v,c,el'),eb',enterv(v,free'))
         end

      | transform(SETTER(i,vl,e),env) =
         let val cl = sublist iscont (clean vl)
             val (env',fl) = (fold substin cl) (env,nil)
             val vl' = map (fn x => glookup(x,env')) vl
             val (e',eb,free) = transform(e,env')
             val free' = merge(uniq(clean vl),free)
             val (eb',free'') = if (cl=nil) then (eb,free')
                                else (true,remove(uniq(map #1 fl),free'))
          in (fix(fl,SETTER(i,vl',e')),eb',free'')
         end

      | transform(LOOKER(i,vl,w,e),env) =
         let val cl1 = sublist iscont (clean vl)
             val (env',fl) = (fold substin cl1) (env,nil)
             val cl2 = sublist iscont [w]
             val (env'',fl') = (fold substout cl2) (env',nil)
             val vl' = map (fn x => glookup(x,env'')) vl
             val w' = clookup(w,env'')

             val (e',eb,free) = transform(e,env'')
             val eb' = if ((cl1=nil) andalso (cl2=nil)) then eb else true
             val free' = merge(uniq(clean vl),rmv(w,free))
             val free'' = remove(uniq(map #1 (fl@fl')),free')

          in (fix(fl,LOOKER(i,vl',w',fix(fl',e'))),eb',free'')
         end

      | transform(ARITH(i,vl,w,e),env) =
         let val cl1 = sublist iscont (clean vl)
             val (env',fl) = (fold substin cl1) (env,nil)
             val cl2 = sublist iscont [w]
             val (env'',fl') = (fold substout cl2) (env',nil)
             val vl' = map (fn x => glookup(x,env'')) vl
             val w' = clookup(w,env'')

             val (e',eb,free) = transform(e,env'')
             val eb' = if ((cl1=nil) andalso (cl2=nil)) then eb else true
             val free' = merge(uniq(clean vl),rmv(w,free))
             val free'' = remove(uniq(map #1 (fl@fl')),free')

          in (fix(fl,ARITH(i,vl',w',fix(fl',e'))),eb',free'')
         end

      | transform(PURE(i,vl,w,e),env) =
         let val cl1 = sublist iscont (clean vl)
             val (env',fl) = (fold substin cl1) (env,nil)
             val cl2 = sublist iscont [w]
             val (env'',fl') = (fold substout cl2) (env',nil)
             val vl' = map (fn x => glookup(x,env'')) vl
             val w' = clookup(w,env'')

             val (e',eb,free) = transform(e,env'')
             val eb' = if ((cl1=nil) andalso (cl2=nil)) then eb else true
             val free' = merge(uniq(clean vl),rmv(w,free))
             val free'' = remove(uniq(map #1 (fl@fl')),free')
          in (fix(fl,PURE(i,vl',w',fix(fl',e'))),eb',free'')
         end

      | transform(BRANCH(i,vl,c,e1,e2),env) =
         let val cl = sublist iscont (clean vl)
             val (env',fl) = (fold substin cl) (env,nil)
             val vl' = map (fn x => glookup(x,env')) vl

             val (e1',eb1,free1) = transform(e1,env')
             val (e2',eb2,free2) = transform(e2,env')
             val eb = if (cl=nil) then (eb1 orelse eb2) else true
             val free = merge(uniq(clean vl),merge(free1,free2))
             val free' = remove(uniq(map #1 fl),free)
	  in (fix(fl,BRANCH(i,vl',c,e1',e2')),eb,free')
         end
          
      | transform(FIX(l,e),env) =
         let fun g(fe,(el,eb,free)) =
               let val (fe',eb',free') = transfunc(fe,env)
                in (fe'::el,eb orelse eb',merge(free,free'))
               end
             val (e0,eb0,free0) = transform(e,env)
             val (l',eb',free') = fold g l (nil,eb0,free0)
             val free'' = remove(uniq(map #1 l'),free')
          in (FIX(l',e0),eb',free'')
         end

      | transform(_,env) =
         error "illformed cexp in the contmap transforming process"

    and transfunc((fv,fa,ce),env) = 
         let fun transf(fv,fa,ce) = 
                   let val cl = sublist iscont fa
                       val k = if iscont fv then length(cl) 
                               else length(cl)-1 
                       val cl' = if k < 1 then nil 
                                 else if iscont fv then cl
                                      else (List.tl(List.rev cl))
                       val (env',fl) = (fold substout cl') (env,nil)
                       val fa' = map (fn x => clookup(x,env')) fa 

                       val (ce',eb,free) = transform(ce,env')
                       val eb' = if (cl'=nil) then eb else true
                       val free' = remove(uniq(map #1 fl),free)
                       val free'' = rmv(fv,remove(uniq(fa),free'))
                       val _ = ebadd(fv,(eb',free''))

                    in ((fv,fa',fix(fl,ce')),eb',free'')
                   end
          in transf(fv,fa,ce)
         end

    val ((fvar',fargs',cexp'),_,_) = transfunc((fvar,fargs,cexp),nil)

 in ((fvar',fargs',cexp'),iscont,ebinfo)
end (* contmap *)

end (* ContMap *)
