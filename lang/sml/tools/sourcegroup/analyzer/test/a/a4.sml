signature A = sig
  structure B :sig val x:int end
  structure D :sig val y:int end
end


(* {dst B dst D}*(dsi A [dst B dst D]) *)
