(*$Memo : MEMO Array General *)

loadSig "MEMO";

structure Memo: MEMO =

(* MEMO FUNCTION PACKAGE

Created by:	K. Mitchell, LFCS, University of Edinburgh
Date:	        April 1989.

Maintenance:	Author

RCS LOG

$Log: Memo.sml,v $
Revision 1.1  1994/02/08 00:23:17  jkh
Initial revision

Revision 1.5  91/07/02  15:12:18  15:12:18  db (Dave Berry)
Minor tidying up.

Revision 1.4  91/01/26  15:13:55  15:13:55  db (Dave Berry)
Renamed RefVector and REF_VECTOR to Array and ARRAY, respectively.

Revision 1.3  91/01/25  20:17:30  20:17:30  db (Dave Berry)
Changed signature names to all upper case.
Amended tag declarations to match above change.

Revision 1.2  91/01/24  17:21:36  17:21:36  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/20  15:00:48  15:00:48  db (Dave Berry)
Initial revision


*)

struct


(* MEMO FUNCTIONS *)

local
  exception Uninitialised;

  fun extend(a,newmax,v) =         (* extends an array with more elements *)
      let val a' = Array.create newmax  v
       in (Array.copy a 0 (Array.size a) a' 0; a') end

  fun xupdate(r as ref a, i, v, expfn, nullvalue) =
      (Array.update (a, i, v); a)
        handle Array.Update _ => 
          if i < 0 then raise General.Nat ("memo", i)
          else let val newsize = Int.max (i+1) (expfn(i+1))  in
         (r := extend(a,newsize,nullvalue); Array.update (!r, i, v); !r) end;

  fun xsub(ref a, i) =                
      Array.sub(a, i) 
        handle Array.Subscript _  => 
          if i < 0 then raise General.Nat ("memo", i) else raise Uninitialised;
in
  fun memo (expfn: Nat -> Nat) (inj: 'a -> Nat) f =
      let val answers = ref(Array.create 0 None)
          fun compute answers i =
              ( case xsub(answers, inj i) of
                  Some v => v 
                | None => raise Uninitialised )
              handle 
                Uninitialised =>
                  let val v = f (compute answers) i 
                   in ( xupdate(answers, inj i, Some v, expfn, None); 
                        v ) end
       in ( fn n => compute (ref(Array.create (inj n) None)) n,
            compute answers ) end;
end; (* local *)


fun memo2 expfn injx injy f = 
    memo  expfn injx (fn tf => fn x => #2(memo expfn injy (fn _ => f tf x)));

fun memo3 expfn injx injy injz f = 
    memo  expfn injx (fn tf => fn x => #2(memo expfn injy 
                     (fn _  => fn y => #2(memo expfn injz 
                     (fn _ => f tf x y)))));

end
