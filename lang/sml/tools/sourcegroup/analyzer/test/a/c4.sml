signature A = sig
  structure B :sig type x end
  structure D :sig type y end
end

(* source sml "c1.sml"
     import structure C
     import signature A
     export signature F;
*)
