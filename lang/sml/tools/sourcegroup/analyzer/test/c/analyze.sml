structure Traverse = TraverseFun (Scopes)

structure a = struct
  fun a filename =
    Scopes.prScope (Traverse.traverse (Parse.parseSource filename))
end

