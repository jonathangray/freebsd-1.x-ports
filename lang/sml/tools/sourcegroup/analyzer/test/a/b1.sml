functor F (A:A) = struct
  val c = C.z
  open A
  val b = B.x
  val d = D.y
end

structure F = F(A)
