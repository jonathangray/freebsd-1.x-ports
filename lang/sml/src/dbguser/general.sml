structure DebugGeneral (* : GENERAL *) =
  struct
    open General
    fun f o g = fn x => f(g x)
    fun a before b = a
    (* eventually should do something about callcc/throw here. *)
  end (* structure General *)
