(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins+@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ, Pittsburgh, PA *)

structure IO_Stream :IO_STREAM = struct

fun withInStream (stream :instream)
      (action :instream -> 'a -> 'b) (argument:'a) :'b =
  let val result = action stream argument
                     handle exn => (close_in stream; raise exn)
  in close_in stream; result end

fun withOutStream (stream :outstream)
      (action :outstream -> 'a -> 'b) (argument:'a) :'b =
  let val result = action stream argument
                     handle exn => (close_out stream; raise exn)
  in close_out stream; result end

fun withInOutStreams (inS :instream, outS :outstream)
      (action :instream * outstream -> 'a -> 'b) (argument:'a) :'b =
  let val result = action (inS, outS) argument
                     handle exn => (close_in inS; close_out outS; raise exn)
  in
    close_out outS; close_in inS;
    result
  end

end
