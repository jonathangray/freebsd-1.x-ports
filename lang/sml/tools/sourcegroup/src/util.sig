(* Copyright (c) 1992 by Carnegie Mellon University *)

signature UTIL = sig
  structure Data :DATA

  val stringEqual :string * string -> bool
  val printSep :string -> string list -> unit
  val issueWarnings :bool ref
  val warn :string list -> unit
  val err :string list -> 'a

  val zeroTime :System.Timer.time
  val isZeroTime :System.Timer.time -> bool
  val currentTime :unit -> System.Timer.time
  val seconds :System.Timer.time -> int
  val microSeconds :System.Timer.time -> int
  val newer :System.Timer.time * System.Timer.time -> bool
  val newAs :System.Timer.time * System.Timer.time -> bool

  val trim :string -> string

  val remDuplicates :string list -> string list -> string list * string list

  val toolIsDefined :(string * int -> bool) ref
end
