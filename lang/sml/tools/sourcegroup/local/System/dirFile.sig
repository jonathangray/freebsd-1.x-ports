(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins+@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ, Pittsburgh, PA *)

signature DIRFILE = sig
  datatype mapOptions = FOLLOWDIRS | FOLLOWFILES | RECURSIVE | ALPHA
  datatype fileType = FILE | DIR | SYMLINK
  val scan :(string->string->fileType->unit)->mapOptions list->string->unit
  val fold :(string->string->fileType->'a->'a)
                 ->mapOptions list->string->'a->'a
  val checkFile :string->string->bool->bool->((string*string)*fileType*bool)
  val listFiles :string -> mapOptions list -> unit
  val fileExists :string -> bool

  val timeModified :string -> System.Timer.time
  val timeModifiedInSeconds :string -> int
  val isZeroTime :System.Timer.time -> bool
end
