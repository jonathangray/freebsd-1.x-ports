signature F  = sig
  val c :C.z
  include A         (* open A' *)
  val b :B.x
  val d :D.y
end
