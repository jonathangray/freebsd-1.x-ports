(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins+@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ, Pittsburgh, PA *)

signature IO_STREAM = sig
  val withInStream :instream  -> (instream -> 'a -> 'b) -> 'a -> 'b
  val withOutStream :outstream -> (outstream -> 'a -> 'b) -> 'a -> 'b
  val withInOutStreams :instream * outstream ->
                          (instream * outstream -> 'a -> 'b) -> 'a -> 'b
 end
