(* Copyright (c) 1992 by Carnegie Mellon University *)

structure Parse :PARSE = struct

val makeSource  = Pervasives.makeSource
val closeSource = Pervasives.closeSource
val parse       = Pervasives.parse
val staticPart  = Pervasives.staticPart

fun withSource (sourceName:string)
      (action :source -> 'a -> 'b) (argument:'a) :'b =
  let val sourceStream = open_in sourceName
      val source = makeSource (sourceName, 1, sourceStream, false, 
                               {linewidth = !System.Print.linewidth,
				flush = System.Print.flush,
				consumer = System.Print.say})
      val result = action source argument
                     handle exn => (closeSource source; raise exn)
  in closeSource source; result end

val start = staticPart (!System.Env.pervasiveEnvRef);

fun parseSource (sourceName:string) :System.Compile.Ast.dec =
  let fun comp source () :System.Compile.Ast.dec * staticEnv =
        parse (source, start)
      val (ast, _) = withSource sourceName comp ()
  in
    ast
  end

end
