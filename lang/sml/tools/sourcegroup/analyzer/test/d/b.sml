local structure A = struct val x = B.y end in
  structure H = struct val y = A.x end
end
