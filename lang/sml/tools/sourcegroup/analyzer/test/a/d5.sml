signature A = sig
  structure B :sig
    val x:int
    structure D :sig val y:int end
  end
end
