(* Copyright (c) 1992 by Carnegie Mellon University *)
structure Date :sig val date :unit -> string end = struct

fun withInOutStreams (inS :instream, outS :outstream)
      (action :instream * outstream -> 'a -> 'b) (argument:'a) :'b =
  let val result = action (inS, outS) argument
                     handle exn => (close_in inS; close_out outS; raise exn)
  in
    close_out outS; close_in inS;
    result
  end

val SYS_wait = 84

fun waitForProcess () =
 (while (((System.Unsafe.CInterface.syscall (SYS_wait, System.Unsafe.cast 0))
    handle System.Unsafe.CInterface.SysError _ => 0) > 0) do ())

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

fun date () = firstLine ("/bin/date", [])

end
