structure A = struct val x=4 val c = B1.y end

and B = struct val y=5 val b = A1.x end

structure C = struct val x = A.c + B.b end
