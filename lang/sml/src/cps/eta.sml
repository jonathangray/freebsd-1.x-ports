(* Copyright 1989 by AT&T Bell Laboratories *)
functor Eta(val maxfree : int) :
	sig val eta : {function: CPS.function,
		       afterClosure: bool}
	              -> CPS.function
        end =
struct

 open Access CPS
 structure CG = System.Control.CG
 fun map1 f (a,b) = (f a, b)

 fun member(i : int, a::b) = i=a orelse member(i,b)
   | member(i,[]) = false

 fun last0[x]=x | last0(a::b)=last0 b | last0 _ = 0
			  
 fun sameName(x,VAR y) = Access.sameName(x,y) 
   | sameName(x,LABEL y) = Access.sameName(x,y) 
   | sameName _ = ()

 fun freein v = 
  let fun try(VAR w) = v=w
	| try(LABEL w) = v=w
	| try _ = false

      fun any(w :: rest) = try w orelse any rest
	| any nil = false
      
      fun any1((w,_)::rest) = try w orelse any1 rest
	| any1 nil = false

      val rec g =
	 fn APP(f,args) => try f orelse any args
	  | SWITCH(v,c,l) => try v orelse exists g l
	  | RECORD(_,l,w,ce) => any1 l orelse g ce
	  | SELECT(_,v,w,ce) => try v orelse g ce
	  | OFFSET(_,v,w,ce) => try v orelse g ce
	  | SETTER(_,vl,e) => any vl orelse g e
	  | LOOKER(_,vl,w,e) => any vl orelse g e
	  | ARITH(_,vl,w,e) => any vl orelse g e
	  | PURE(_,vl,w,e) => any vl orelse g e
	  | BRANCH(_,vl,c,e1,e2) => any vl orelse g e1 orelse g e2
	  | FIX(fl, e) => exists (g o #3) fl  orelse  g e
  in g
 end

fun eta {function=(fvar,fargs,cexp), afterClosure} =
let
 val debug = !CG.misc1 (* false *)
 fun debugprint s = if debug then System.Print.say s else ()
 fun debugflush() = if debug then System.Print.flush() else ()

 (* Note that maxfree has already been reduced by 1 (in CPScomp)
    on most machines to allow for an arithtemp *)
 val maxregs = min(!CG.maxregs, maxfree)- !CG.calleesaves

 val label = if afterClosure then LABEL else VAR

 exception M2
 val m : value Intmap.intmap = Intmap.new(32, M2)
 val name = Intmap.map m
 fun rename(v0 as VAR v) = (rename(name v) handle M2 => v0)
   | rename(v0 as LABEL v) = (rename(name v) handle M2 => v0)
   | rename x = x
 fun newname x = (sameName x; Intmap.add m x)
 val rec reduce = 
   fn RECORD(k,vl,w,e) => RECORD(k, map (map1 rename) vl, w, reduce e)
    | SELECT(i,v,w,e) => SELECT(i, v, w, reduce e)
    | OFFSET(i,v,w,e) => OFFSET(i, v, w, reduce e)
    | APP(f,vl) => APP(rename f, map rename vl)
    | SWITCH(v,c,el) => SWITCH(v, c,map reduce el)
    | BRANCH(i,vl,c,e1,e2) =>
          BRANCH(i, map rename vl, c, reduce e1, reduce e2)
    | LOOKER(i,vl,w,e) => LOOKER(i,map rename vl, w, reduce e)
    | ARITH(i,vl,w,e) => ARITH(i,map rename vl, w, reduce e)
    | PURE(i,vl,w,e) => PURE(i,map rename vl, w, reduce e)
    | SETTER(i,vl,e) => SETTER(i,map rename vl, reduce e)
    | FIX(l,e) =>
        let fun split1(f,vl,body,rest) =
   		if !CG.etasplit andalso not afterClosure
   		 then let val vl' = map dupLvar vl 
   		          and f' = dupLvar f
   	               in (f,vl',APP(label f', map VAR vl'))
   			  ::(f',vl,body)::rest
   		      end
   		 else (f,vl,body) :: rest
   	 fun split(f,vl,body as FIX([(g,[b,k],body2)],
   				    APP(VAR c,[VAR g'])),
   		   rest) =
   	       if last0 vl = c andalso g=g' 
   		     andalso !System.Control.CG.uncurry
		     andalso not afterClosure
		     (* it's not desirable to uncurry if g is recursive *)
		     andalso not (freein g body2)
   		     (* this next test is overly crude, as
   		        perhaps k' is not free in body2; no big deal *)
   		     andalso length vl + 2 <= maxregs-1
   		then let val b'=dupLvar b
   		         and vl' = map dupLvar vl
   			 and k'=dupLvar k
   			 and g'=dupLvar g
   			 and f'=dupLvar f
   		     in debugprint "u";
			 (f,vl',FIX([(g',[b',k'],
                                          APP(VAR f',
   				           map VAR vl' @ 
   					   [VAR b',VAR k']))],
   				        APP(VAR(last0 vl'),[VAR g'])))
   		         ::split(f',vl@[b,k],body2,rest)
   		     end
   		 else split1(f,vl,body,rest)
   	   | split(f,vl,body,rest) = split1(f,vl,body,rest)

   	  fun same(v::vl, (VAR w)::wl) = v=w andalso same(vl,wl)
               | same(nil,nil) = true
   	    | same _ = false
   	  fun h((f,vl,body as APP(VAR g, wl))::r) =
   	     if same(vl,wl) andalso not (member(g, f::vl)) andalso !CG.eta
   	     	then (newname(f,rename(VAR g)); h r)
   	     	else (f,vl,body):: h r
   	   | h((f,vl,body as APP(LABEL g, wl))::r) =
   	     if same(vl,wl) andalso not (member(g, f::vl)) andalso !CG.eta
   	     	then (newname(f,rename(LABEL g)); h r)
   	     	else (f,vl,body):: h r
   	   | h((f,vl,body)::r) = split(f,vl,body, h r)
   	   | h [] = []
         in case h l of
   	  [] => reduce e
   	| l' => FIX(map (fn(f,vl,e)=>(f,vl,reduce e)) l', reduce e)
        end
 in (fvar, fargs, reduce cexp)
end

end
