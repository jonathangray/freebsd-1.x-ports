signature A = sig
  structure B :sig val x:int end
end

(* {dst B}*(dsi A [dst B]) *)


signature C = A

(* rsi A dsi C *)

(* (rsi A)*(dsi C [dst B]) *)
