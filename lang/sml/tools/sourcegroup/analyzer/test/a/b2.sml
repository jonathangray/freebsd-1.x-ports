signature A = sig end
structure A:A = struct end


(* source sml "b1.sml"
     import structure C, B, D, A
     import signature A
     export functor F
     export structure F;
   source sml "bx.sml"
     import structure F, B
     export structure S
*)
