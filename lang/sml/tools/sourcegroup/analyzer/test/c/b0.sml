signature A = sig
  type t
  structure M :sig val x:t end
end

functor F (A:A) = struct open A end

structure B = F(struct type t=int structure M = struct val x:t=4 end end)
(* B.M *)


functor G (structure D:A) = struct open D end

structure W = struct
  type t=bool
  structure M = struct val x:t=false end
end

structure K = G (structure D = W) 
(* K.M *)

structure L = F (W)
(* L.M *)
