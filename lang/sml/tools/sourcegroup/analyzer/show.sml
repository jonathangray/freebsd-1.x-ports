(* Copyright (c) 1992 by Carnegie Mellon University *)

structure Show = struct

fun show'off strm () =
  if end_of_stream strm then ()
    else (print (input_line strm); show'off strm ())

fun show directory =
  let fun scanner d f t =
        let val e = Pathname.extension f
            val pathname = Pathname.mergePathnames[d,f]
        in
          if not ((e="sml") orelse (e="sig")) then ()
            else (print ("--------------- "^ pathname ^" ---------------\n");
                  IO_Stream.withInStream (open_in pathname) show'off ())
        end
  in
    DirFile.scan scanner [DirFile.ALPHA] directory
  end

fun show'out directory =
  let fun scanner d f t =
        let val e = Pathname.extension f
            val pathname = Pathname.mergePathnames[d,f]
        in
          if not (e="out") then ()
            else (print ("--------------- "^ pathname ^" ---------------\n");
                  IO_Stream.withInStream (open_in pathname) show'off ())
        end
  in
    DirFile.scan scanner [DirFile.ALPHA] directory
  end

end
