signature HASHENV = sig val hash : StaticEnv.staticEnv -> string end

structure HashEnv : HASHENV = 
struct

 open ModuleUtil


 fun hash env =
  let val key = HashKey.new()
      fun add i = HashKey.add(key,i)
      val bindings = sortEnvBindings (Env.consolidate env)

      fun h_bind(