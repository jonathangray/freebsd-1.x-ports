(* Copyright 1989 by AT&T Bell Laboratories *)
signature GLOBALFIX =
  sig val globalfix : CPS.function -> CPS.function list
  end

structure GlobalFix : GLOBALFIX =
struct
open CPS
fun globalfix(f,vl,cexp) =
let
fun gfix ce =
  case ce of
    FIX(fl,c) =>
    let val (n,c') = gfix c
	val l' =
	revfold (fn((v,a,c),m) => let val (l,d) = gfix c in (v,a,d)::l@m end) fl n
    in (l',c')
    end
  | APP _ => ([],ce)
  | SWITCH(v,c0,l) =>
    let val (f,l') =
	fold (fn(c,(fl,cl)) => let val (f,d) = gfix c in (f@fl,d::cl) end) l ([],[])
    in  (f,SWITCH(v,c0,l'))
    end
  | RECORD(k,l,v,c) => let val (f,c') = gfix c in (f,RECORD(k,l,v,c')) end
  | SELECT(i,v,w,c) => let val (f,c') = gfix c in (f,SELECT(i,v,w,c')) end
  | OFFSET(i,v,w,c) => let val (f,c') = gfix c in (f,OFFSET(i,v,w,c')) end
  | SETTER(i,vl,c) => let val (f,c') = gfix c in (f,SETTER(i,vl,c')) end
  | LOOKER(i,vl,w,c) => let val (f,c') = gfix c in (f,LOOKER(i,vl,w,c')) end
  | ARITH(i,vl,w,c) => let val (f,c') = gfix c in (f,ARITH(i,vl,w,c')) end
  | PURE(i,vl,w,c) => let val (f,c') = gfix c in (f,PURE(i,vl,w,c')) end
  | BRANCH(i,args,c,e1,e2) =>
	let val (f1,e1') = gfix e1
            val (f2,e2') = gfix e2
         in (f1@f2, BRANCH(i,args,c,e1',e2'))
	end
val (l,body) = gfix cexp
in  (f,vl,body) :: l
end
end (* structure GlobalFix *)
