structure B = struct
  local structure C = struct val z = A.x end
        structure A = struct val x = 4 val q = 7 end
  in
    val y = A.q
  end
end
