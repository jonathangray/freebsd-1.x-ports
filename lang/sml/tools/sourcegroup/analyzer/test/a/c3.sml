signature A = sig
  structure B :sig type x end
end

(* source sml "c1.sml"
     import structure C, D
     import signature A
     export signature F;
*)
