functor F (A:A) = struct
  val c = C.z
  open A
  val b = B.x
  val d = D.y
end

(* {rsi A dst A:A rst C
    (ost A [rst B rst D])}
   dfu F
*)
