structure A =
  let structure B = struct val x = 3 end
      structure C = struct val y = B.x end
  in
    struct
      val c = C.y
    end
  end
