(* Copyright (c) 1992 by Carnegie Mellon University *)

structure GetWorkingDirectory :sig val getwd :unit -> string end = struct

fun withInOutStreams (inS :instream, outS :outstream)
      (action :instream * outstream -> 'a -> 'b) (argument:'a) :'b =
  let val result = action (inS, outS) argument
                     handle exn => (close_in inS; close_out outS; raise exn)
  in
    close_out outS; close_in inS;
    result
  end

local
    open System.Unsafe.CInterface
    val wait = (c_function "wait")
	handle CFunNotFound _  => 
	    (fn () => syscall (84,[])
	     handle SysError _ => 0)
in
    fun waitForProcess () = while (wait()) > 0 do ()
end

fun firstLine (program:string, args :string list) =
  let fun strip_newline str =
            let val len = size str in
              if len > 0 then substring (str, 0, len - 1)
                         else str end
      fun first_line (inS,outS) () = strip_newline (input_line inS)
      val result =
        withInOutStreams (execute (program, args)) first_line ()
  in
    waitForProcess(); result
  end


fun getwd () = firstLine ("/bin/pwd", [])

end
