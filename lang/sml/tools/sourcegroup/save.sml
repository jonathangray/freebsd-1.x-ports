(* Copyright (c) 1992 by Carnegie Mellon University *)
val buildDate :string ref = ref "";
fun export () =
  (exportML "sml-sg";
   print (System.version ^
          "\n  with SourceGroup " ^ (makestring SourceGroup.version) ^
          " built on " ^ (!buildDate) ^ "\n"));
buildDate := Date.date();
