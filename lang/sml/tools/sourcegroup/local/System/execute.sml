(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins+@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ, Pittsburgh, PA *)

structure Execute :EXECUTE = struct

exception NoExecuteOutput

local
    open System.Unsafe.CInterface
    val wait = (c_function "wait")
	handle CFunNotFound _  => 
	    (fn () => syscall (84,[])
	     handle SysError _ => 0)
in
    fun waitForProcess () = while (wait()) > 0 do ()
end

fun runAndPrint (program:string, args :string list) =
  let fun read'all (inS,outS) () =
        if end_of_stream inS then ()
          else (print (input_line inS); read'all (inS,outS) ())
  in
    IO_Stream.withInOutStreams (execute (program, args)) read'all ();
    waitForProcess()
  end

fun firstLine (program:string, args :string list) =
  let fun strip_newline str =
            let val len = size str in
              if len > 0 then substring (str, 0, len - 1)
                         else str end
      fun first_line (inS,outS) () = strip_newline (input_line inS)
      val result =
        IO_Stream.withInOutStreams (execute (program, args)) first_line ()
  in
    waitForProcess(); result
  end

(* Examples:
 val cwd = firstLine ("/bin/pwd", [])
 runAndPrint ("/usr/cs/bin/ls", ["-FC"])
*)

end 
