(* Copyright (c) 1992 by Carnegie Mellon University *)

functor InterfaceFun () : INTERFACE = struct

type pos = int
val line = ref 0
fun initLine () = (line := 1)
fun nextLine () = (line := !line + 1)
fun makeString (line:pos) = makestring line

end
