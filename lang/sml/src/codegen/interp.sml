(* Copyright 1989 by AT&T Bell Laboratories *)
structure Interp : sig val interp : Lambda.lexp -> 'a end = struct

open Array List Access Types Lambda ErrorMsg
infix 9 sub
structure U = System.Unsafe
val cast = U.cast
datatype 'a env = BIND of 'a env * lvar * 'a

val MTENV : 'a env = cast()

fun realc s =
  let val (sign,s) = case explode s of "~"::rest => (~1.0,rest) | s => (1.0,s)
      fun j(exp,d::dl,mant) = j(exp,dl,mant * 0.1 + real(d))
        | j(0,nil,mant) = mant*sign
        | j(exp,nil,mant) = if exp>0 then j(exp-1,nil,mant*10.0)
				     else j(exp+1,nil,mant*0.1)
      fun h(esign,wholedigits,diglist,exp,nil) = 
			j(esign*exp+wholedigits-1,diglist,0.0)
        | h(es,fd,dl,exp,d::s) = h(es,fd,dl,exp*10+(ord d - ord "0"),s)
      fun g(i,r,"E"::"~"::s)=h(~1,i,r,0,s)
	| g(i,r,"E"::s)=h(1,i,r,0,s)
	| g(i,r,d::s) = g(i, (ord d - ord "0")::r, s)
	| g(i,r,nil) = h(1,i,r,0,nil)
      fun f(i,r,"."::s)=g(i,r,s)
        | f(i,r,s as "E"::_)=g(i,r,s)
        | f(i,r,d::s) = f(i+1,(ord(d)-ord("0"))::r,s)
   in f(0,nil,s)
  end

fun look v =
 let fun f(BIND(e,w,x)) = if v=w then x else f e
  in f
 end

val upd = BIND

 val rec M = 
   fn APP(FN(v,_,b),a) => let val a' = M a and b' = M b
                           in fn r => cast(b' (upd(r,v, cast (a' r))))
                          end
    | APP(a,b) => cast let val a' = cast(M a) and b' = M b
                   in fn r => cast((a' r) (b' r))
                  end
    | FN(v,_,b) => let val b' = M b
                    in fn r => cast (fn x =>  b' (upd(r,v,x)))
                   end
    | DECON((_,UNTAGGED,_),b) => let val b' = cast(M b)
	                          in fn r => let val {x} = (cast(b' r))
	        		              in x
					     end
				  end
    | DECON((_,TAGGED _,_),b) => let val b' = cast(M b)
	                          in fn r => cast(U.subscript(b' r, 1))
				 end
    | DECON((_,TAGGEDREC(_,1),_),b) => 
            let val b' = M b
	     in fn r => let val (i,x) = cast(b' r)
	    	         in cast{x=x} 
			end
	    end
    | DECON((_,TAGGEDREC(_,2),_),b) => 
            let val b' = M b
	     in fn r => let val (i,x,y) = cast(b' r)
	                 in cast(x,y)
			end
	    end
    | DECON((_,TAGGEDREC(_,3),_),b) => 
            let val b' = M b
	     in fn r => let val (i,x,y,z) = cast(b' r)
			 in cast(x,y,z)
	        	end
	    end
    | DECON((_,TAGGEDREC(_,4),_),b) => 
            let val b' = M b
	     in fn r => let val (i,w,x,y,z) = cast(b' r)
		 	 in cast(w,x,y,z)
			end
	    end
    | DECON((_,TAGGEDREC(_,5),_),b) => 
            let val b' = M b
	     in fn r => let val (i,v,w,x,y,z) = cast(b' r)
			 in cast(v,w,x,y,z)
			end
	    end
    | DECON((_,TAGGEDREC(_,j),_),b) => 
            let val b' = M b
	        fun f(z,n) = if n=j then nil
		             else U.subscript(z,j+1)::f(z,n+1)
	     in fn r => cast(Vector.vector(f(cast(b' r),0)))
	    end
    | DECON((_,UNTAGGEDREC _,_),b) => M b
    | DECON((_,VARIABLE _,_),b) => let val b' = cast(M b)
	                            in fn r => cast(U.subscript(b' r, 1))
			  	   end
    | DECON((_, TRANSPARENT,_),b) => M b
    | CON((_,UNTAGGED,_),b) => let val b' = M b
	                        in fn r => cast {a= b' r}
			       end
    | CON((_,CONSTANT i,_),b) => (fn r => cast i)
    | CON((_,TAGGED i,_),b) => let val b' = M b
	                        in fn r => cast (i, b' r)
			       end
    | CON((_,VARIABLE(PATH p),_),b) => 
	                 let val b' = M b
			     fun g [v] = VAR v
			       | g (i::r) = SELECT(i, g r)
			     val gp = M(g p)
			  in fn r => cast (gp r, b' r)
			 end
    | CON((_,VARIABLEc(PATH p),_),_) => 
			 let fun g [v] = VAR v
			       | g (i::r) = SELECT(i, g r)
			  in M(g p)
			 end
    | CON((_,TRANSPARENT,_),b) => M b
    | CON((_,TAGGEDREC(i,1),_),b) => 
            let val b' = M b
	     in fn r => let val {1=x} = cast(b' r)
       		         in cast(i,x)
		        end        
            end
    | CON((_,TAGGEDREC(i,2),_),b) => 
            let val b' = M b
	     in fn r => let val (x,y) = cast(b' r)
			 in cast(i,x,y)
			end         
            end
    | CON((_,TAGGEDREC(i,3),_),b) => 
            let val b' = M b
	     in fn r => let val (x,y,z) = cast(b' r)
			 in cast(i,x,y,z)
			end        
            end
    | CON((_,TAGGEDREC(i,4),_),b) => 
            let val b' = M b
	     in fn r => let val (w,x,y,z) = cast(b' r)
			 in cast(i,w,x,y,z)
			end         
            end
    | CON((_,TAGGEDREC(i,5),_),b) => 
            let val b' = M b
	     in fn r => let val (v,w,x,y,z) = cast(b' r)
	    	         in cast(i,v,w,x,y,z)
			end         
            end
    | CON((_,TAGGEDREC(i,j),_),b) => 
            let val b' = M b
	        fun f(z,n) = if n=j then nil
			     else U.subscript(z,n)::f(z,n+1)
	     in fn r => cast(Vector.vector(i::f(cast(b' r),0)))
	    end
    | CON((_,UNTAGGEDREC _,_),b) => M b
    | SELECT(i,a) => let val a' = cast(M a) 
		      in fn r => cast(U.subscript(a' r, i))
		     end
    | RECORD [] => (fn r => cast())
    | RECORD [a,b] => let val a' = M a and b' = M b
		       in fn r => cast (a' r, b' r)
		      end
    | RECORD [a,b,c] => let val a' = M a and b' = M b and c' = M c
		         in fn r => cast (a' r, b' r, c' r)
		        end
    | RECORD [a,b,c,d] => let val a' = M a and b' = M b
			      and c' = M c and d' = M d
		           in fn r => cast (a' r, b' r, c' r, d' r)
		          end
    | RECORD [a,b,c,d,e] => let val a' = M a and b' = M b
			        and c' = M c and d' = M d
			        and e' = M e
		             in fn r => cast (a' r, b' r, c' r, d' r, e' r)
		            end
    | RECORD [a,b,c,d,e,f] => let val a' = M a and b' = M b
			          and c' = M c and d' = M d
			          and e' = M e and f' = M f
		               in fn r => cast (a' r, b' r, c' r, d' r, 
                                                e' r, f' r)
		              end
    | RECORD l => let val l' = map M l
		   in fn r => cast(Vector.vector(map (fn x => x r) l'))
		  end
    | VECTOR [] => (fn r => cast())
    | VECTOR [a,b] => let val a' = M a and b' = M b
		       in fn r => cast(Vector.vector[a' r, b' r])
		      end
    | VECTOR [a,b,c] => let val a' = M a and b' = M b and c' = M c
		         in fn r => cast (a' r, b' r, c' r)
		        end
    | VECTOR [a,b,c,d] => let val a' = M a and b' = M b
			      and c' = M c and d' = M d
		           in fn r => cast (a' r, b' r, c' r, d' r)
		          end
    | VECTOR [a,b,c,d,e] => let val a' = M a and b' = M b
			        and c' = M c and d' = M d
			        and e' = M e
		             in fn r => cast (a' r, b' r, c' r, d' r, e' r)
		            end
    | VECTOR [a,b,c,d,e,f] => let val a' = M a and b' = M b
			          and c' = M c and d' = M d
			          and e' = M e and f' = M f
		               in fn r => cast (a' r, b' r, c' r, d' r, 
                                                e' r, f' r)
		              end
    | VECTOR l => let val l' = map M l
		   in fn r => cast(Vector.vector(map (fn x => x r) l'))
		  end

    | INT i => (fn r => cast i)
    | STRING s => (fn r => cast s)
    | REAL s => let val x = realc s in fn r => cast x end
    | PRIM(P.CAST,_) => (fn r => cast(fn x => x))
    | PRIM(P.IADD,_) => (fn r => cast Integer.+)  
    | PRIM(P.ISUB,_) => (fn r => cast Integer.-)
    | PRIM(P.IMUL,_) => (fn r => cast Integer.* )
    | PRIM(P.IDIV,_) => (fn r => cast Integer.div)
    | PRIM(P.ORB,_) => (fn r => cast Bits.orb)
    | PRIM(P.ANDB,_) => (fn r => cast Bits.andb)
    | PRIM(P.XORB,_) => (fn r => cast Bits.xorb)
    | PRIM(P.NOTB,_) => (fn r => cast Bits.notb)
    | PRIM(P.RSHIFT,_) => (fn r => cast Bits.rshift)
    | PRIM(P.LSHIFT,_) => (fn r => cast Bits.lshift)
    | PRIM(P.DEREF,_) => (fn r => cast !)
    | PRIM(P.MAKEREF,_) => (fn r => cast ref)
    | PRIM(P.INEG,_) => (fn r => cast Integer.~)
    | PRIM(P.IEQL,_) => (fn r => cast (fn(a:int,b) => a=b))
    | PRIM(P.INEQ,_) => (fn r => cast (fn(a:int,b) => a<>b))
    | PRIM(P.IGT,_) => (fn r => cast Integer.>)
    | PRIM(P.ILT,_) => (fn r => cast Integer.<)
    | PRIM(P.IGE,_) => (fn r => cast Integer.>=)
    | PRIM(P.ILE,_) => (fn r => cast Integer.<=)
    | PRIM(P.LESSU,_) =>  (* this is buggy if b<0 *)
	(fn r => cast (fn (a, b) => ((0 <= a) andalso (a < b))))
    | PRIM(P.GEQU,_) => (* this is buggy if b<0 *)
	(fn r => cast (fn (a, b) => ((0 > a) orelse (a >= b))))
    | PRIM(P.SUBSCRIPT,_) => (fn r => cast U.subscript)
    | PRIM(P.UPDATE,_) => (fn r => cast U.update)
    | PRIM(P.BOXEDUPDATE,_) => (fn r => cast U.update)
    | PRIM(P.UNBOXEDUPDATE,_) => (fn r => cast U.update)
    | PRIM(P.SUBSCRIPTV,_) => (fn r => cast U.subscriptv)
    | PRIM(P.FSUBSCRIPTd,_) => (fn r => cast U.subscriptf)
    | PRIM(P.FUPDATEd,_) => (fn r => cast U.updatef)
    | PRIM(P.LENGTH,_) => (fn r => cast Array.length)
    | PRIM(P.OBJLENGTH,_) => (fn r => cast U.objLength)
    | PRIM(P.STORE,_) => (fn r => cast U.store)
    | PRIM(P.ORDOF,_) => (fn r => cast U.ordof)
    | PRIM(P.FADDd,_) => (fn r => cast Real.+)
    | PRIM(P.FDIVd,_) => (fn r => cast Real./)
    | PRIM(P.FMULd,_) => (fn r => cast Real.* )
    | PRIM(P.FSUBd,_) => (fn r => cast Real.-)
    | PRIM(P.FNEGd,_) => (fn r => cast Real.~)
    | PRIM(P.FABSd,_) => (fn r => cast Real.abs)
    | PRIM(P.REAL,_) => (fn r => cast Real.real)
    | PRIM(P.FEQLd,_) => (fn r => cast (fn(a:real,b)=>a=b))
    | PRIM(P.FNEQd,_) => (fn r => cast (fn(a:real,b)=>a<>b))
    | PRIM(P.FGTd,_) => (fn r => cast Real.>)
    | PRIM(P.FGEd,_) => (fn r => cast Real.>=)
    | PRIM(P.FLEd,_) => (fn r => cast Real.<=)
    | PRIM(P.FLTd,_) => (fn r => cast Real.<)
    | PRIM(P.BOXED,_) => (fn r => cast U.boxed)
    | PRIM(P.UNBOXED,_) => (fn r => cast (not o U.boxed))
    | PRIM(P.CALLCC,_) => (fn r => cast callcc)
    | PRIM(P.CAPTURE,_) => (fn r => cast U.PolyCont.capture)
    | PRIM(P.THROW,_) => (fn r => cast throw)
    | PRIM(P.GETVAR,_) => (fn r => cast U.getvar)
    | PRIM(P.GETTAG,_) => (fn r => cast U.getObjTag)
    | PRIM(P.MKSPECIAL,_) => (fn r => cast U.special)
    | PRIM(P.SETSPECIAL,_) => (fn r => cast U.setSpecial)
    | PRIM(P.GETSPECIAL,_) => (fn r => cast U.getSpecial)
    | PRIM(P.SETVAR,_) => (fn r => cast U.setvar)
    | PRIM(P.GETHDLR,_) => (fn r => cast U.gethdlr)
    | PRIM(P.SETHDLR,_) => (fn r => cast U.sethdlr)
    | PRIM(p,_) => impossible(implode["bad primop ", 
                              P.pr_primop p, " in interp"])
    | VAR v => look v
    | HANDLE(a,b) => let val a' = cast (M a) and b' = cast(M b)
                      in fn r => (a' r handle e => b' r e)
                     end
    | RAISE(a,_) => let val a' = cast (M a) in fn r => raise(a' r) end
    | FIX(nl,_,fl,b) => 
         let fun g(n::nl,f::fl) = let val f' = M f
			              val fl' = g(nl,fl)
                                   in fn rr => cast (upd(fl' rr,n, 
						fn x => cast(f'(!rr)) x))
                                  end
                | g(nil,_) = cast(fn rr => !rr)
             val l = g(nl,fl)
             val b' = cast(M b)
          in fn r => cast (let val rr = ref (cast r)
		            in rr := l (cast rr); b'(!rr)
                           end)
         end
   | SWITCH(e,_,l as (DATAcon(_,VARIABLE _,_), _)::_, SOME d) => exnswitch(e,l,d)
   | SWITCH(e,_,l as (DATAcon(_,VARIABLEc _,_),_)::_, SOME d) => exnswitch(e,l,d)
   | SWITCH(e,_,l as (REALcon _, _)::_, SOME d) =>
     let fun trans(REALcon i, a)= (realc i, M a)
         val cases = map trans l and d' = M d and e' = M e
     in fn r => cast (let val e'':real = cast(e' r)
		    fun find ((i, answer)::rest) =
			 if i=e'' then answer r else find rest
	              | find nil = d' r
		 in find cases
		end)
     end
   | SWITCH(e,_,l as (INTcon _, _)::_, SOME d) =>
     let fun trans(INTcon i, a)= (i, M a)
         val cases = map trans l and d' = M d and e' = M e
     in fn r => cast (let val e'':int = cast(e' r)
		    fun find ((i, answer)::rest) =
			 if i=e'' then answer r else find rest
	              | find nil = d' r
		 in find cases
		end)
     end
   | SWITCH(e,_,l as (STRINGcon _, _)::_, SOME d) =>
     let fun trans(STRINGcon i, a)= (i, M a)
         val cases = map trans l and d' = M d and e' = M e
     in fn r => cast(let val e'':string = cast(e' r)
		    fun find ((i, answer)::rest) =
			 if i=e'' then answer r else find rest
	              | find nil = d' r
		 in find cases
		end)
     end
   | SWITCH(e,_, l as (DATAcon _, _)::_, d) =>
     let val d' = case d of SOME d0 => M d0
                          | NONE => fn r =>  impossible "no default in interp"
         val e' = M e
         fun f((DATAcon(_,CONSTANT i,_),ans)::rest) =
		let val rest' = f rest
		    val ans' = M ans
	         in fn x => if x=i then ans' else rest' x
		end
           | f((DATAcon(_,TAGGED i,_),ans)::rest) =
		let val rest' = f rest
		    val ans' = M ans
	         in fn x => if U.boxed x andalso U.subscript(cast x,0)=i 
				then ans' else rest' x
		end
           | f((DATAcon(_,TAGGEDREC(i,_),_),ans)::rest) =
		let val rest' = f rest
		    val ans' = M ans
	         in fn x => if U.boxed x andalso U.subscript(cast x,0)=i 
				then ans' else rest' x
		end
           | f((DATAcon(_,UNTAGGED,_),ans)::rest) =
		let val rest' = f rest
		    val ans' = M ans
	         in fn x => if U.boxed x then ans' else rest' x
		end
           | f((DATAcon(_,UNTAGGEDREC _,_),ans)::rest) =
		let val rest' = f rest
		    val ans' = M ans
	         in fn x => if U.boxed x then ans' else rest' x
		end
           | f((DATAcon(_,TRANSPARENT,_),ans)::rest) =
		let val rest' = f rest
		    val ans' = M ans
	         in fn x => if U.boxed x
				then ans' else rest' x
		end
           | f nil = fn x => d'
	 val cases = f l
     in fn r => cases(e' r) r
     end
   | WRAP(t,e) => M e
   | UNWRAP(t,e) => M e
   | _ => impossible "bad lexp in interp"

 and exnswitch = fn (e,l,d) =>
     let fun trans(DATAcon(_,VARIABLEc(PATH p),_), a)=
		(rev(0::p), M a)
           | trans(DATAcon(_,VARIABLE(PATH p),_), a)=
		(rev p, M a)
         val cases = map trans l and d' = M d and e' = M e
     in fn r => cast(let val e'' : int = U.subscript(cast(e' r),0)
		    fun select(x,i::rest) = select(U.subscript(cast x,i),rest)
		      | select(x,nil) = cast x
		    fun find ((v::path, answer)::rest) =
			 if select(look v r,path)=e'' then answer r
			    else find rest
	              | find nil = d' r
		 in find cases
		end)
     end

 fun interp lexp = cast(M lexp MTENV)

end (* structure Interp *)
