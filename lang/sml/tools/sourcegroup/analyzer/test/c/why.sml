(* You need to apply functors to analyze sml programs *)
structure A = struct
  structure B = F()
  open B
end
