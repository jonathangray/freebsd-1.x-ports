structure B = struct

fun b () =
  let val home = GetEnv.getenv "HOME" in
    print ("User "^(GetEnv.getenv "USER")^"\n");
    A.a home
  end
end

