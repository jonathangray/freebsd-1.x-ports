signature B = sig type z structure A:A val b:A.t end

and A = sig type t structure B:B val c:B.z end
