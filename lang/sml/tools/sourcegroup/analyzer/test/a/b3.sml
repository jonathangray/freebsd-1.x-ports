signature A = sig
  structure B :sig val x:int end
end

structure A:A = struct
  structure B = struct val x=4 end
end

(* source sml "b1.sml"
     import structure C, D, A
     import signature A
     export functor F
     export structure F;
   source sml "bx.sml"
     import structure F
     export structure S
*)
